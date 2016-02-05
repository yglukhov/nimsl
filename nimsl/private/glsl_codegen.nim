import macros, strutils

type
    glslbuiltin_t* = object
    glslinfix_t* = object

type ShaderKind* = enum
    skVertexShader
    skFragmentShader

type GLSLCompilerContext* = object
    procNode: NimNode
    isMainProc: bool
    globalDefs*: seq[string]
    definedSyms: seq[string]
    shaderKind*: ShaderKind
    mainProcName*: string

proc newCtx*(): GLSLCompilerContext =
    result.definedSyms = newSeq[string]()
    result.globalDefs = newSeq[string]()
    result.mainProcName = "main"

proc gen(ctx: var GLSLCompilerContext, n: NimNode, r: var string)

template genSemicolon(r: var string) =
    let rl = r.len - 1
    if rl > 0 and r[rl] != ';' and r[rl] != '}' and r[rl] != '{':
            r &= ";"

proc isRange(n: NimNode, rangeLen: int = -1): bool =
    if n.kind == nnkBracketExpr and $(n[0]) == "range":
        if rangeLen == -1:
            result = true
        elif n[2].intVal - n[1].intVal + 1 == rangeLen:
            result = true

proc hasTag(n: NimNode, tag: string): bool =
    case n.kind
    of nnkProcDef:
        for i in n.pragma:
            if i.kind == nnkPragma and i[0].kind == nnkExprColonExpr and $(i[0][0]) == "tags":
                for j in i[0][1]:
                    if $j == tag: return true
    of nnkSym:
        result = n.symbol.getImpl().hasTag(tag)
    else:
        result = false

proc isGLSLBuiltin(n: NimNode): bool = n.hasTag("glslbuiltin_t")

proc getTypeName(ctx: var GLSLCompilerContext, t: NimNode, skipVar = false): string =
    case t.kind
    of nnkBracketExpr:
        if $(t[0]) == "array" and t[1].isRange(2) and $(t[2]) == "float32":
            result = "vec2"
        elif $(t[0]) == "array" and t[1].isRange(3) and $(t[2]) == "float32":
            result = "vec3"
        elif $(t[0]) == "array" and t[1].isRange(4) and $(t[2]) == "float32":
            result = "vec4"
        elif $(t[0]) == "distinct":
            result = getTypeName(ctx, t[1], skipVar)
    of nnkSym:
        case $t
        of "vecBase": result = getTypeName(ctx, getType(t), skipVar)
        of "float32": result = "float"
        else:
            result = $t
    of nnkVarTy:
        result = getTypeName(ctx, t[0])
        if not skipVar:
            result = "inout " & result
    else:
        echo "UNKNOWN TYPE: ", treeRepr(t)
        assert(false, "Unknown type")

proc genLetSection(ctx: var GLSLCompilerContext, n: NimNode, r: var string) =
    for i in n:
        let s = i[0]
        r &= getTypeName(ctx, getType(s))
        r &= " "
        r &= $s
        if i[2].kind != nnkEmpty:
            r &= "="
            gen(ctx, i[2], r)
        r &= ";"

proc genStmtList(ctx: var GLSLCompilerContext, n: NimNode, r: var string) =
    for i in n:
        gen(ctx, i, r)
        genSemicolon(r)

proc isSystemSym(s: NimNode): bool =
    let ln = s.symbol.getImpl().lineinfo
    if ln.find("lib/system.nim(") != -1 or ln.find("lib\\system.nim(") != -1:
        result = true

proc genSystemCall(ctx: var GLSLCompilerContext, n: NimNode, r: var string) =
    let pn = $(n[0])
    case pn
    of "inc":
        if n[2].kind == nnkIntLit and n[2].intVal == 1:
            r &= "++"
            gen(ctx, n[1], r)
        else:
            gen(ctx, n[1], r)
            r &= "+="
            gen(ctx, n[2], r)
    of "dec":
        if n[2].kind == nnkIntLit and n[2].intVal == 1:
            r &= "--"
            gen(ctx, n[1], r)
        else:
            gen(ctx, n[1], r)
            r &= "-="
            gen(ctx, n[2], r)
    of "<=", ">=", "<", ">", "+", "-", "*", "/", "*=", "/=", "+=", "-=":
        if n.kind == nnkInfix:
            gen(ctx, n[1], r)
            r &= pn
            gen(ctx, n[2], r)
        elif n.kind == nnkPrefix:
            r &= pn
            gen(ctx, n[1], r)
        else:
            assert(false)
    of "max", "min":
        r &= pn
        r &= "("
        gen(ctx, n[1], r)
        r &= ","
        gen(ctx, n[2], r)
        r &= ")"
    else:
        echo "UNKNOWN SYSTEM CALL: ", treeRepr(n)

proc genCall(ctx: var GLSLCompilerContext, n: NimNode, r: var string) =
    if n[0].hasTag("glslinfix_t"):
        if $(n[0]) == ".":
            # This is a property
            gen(ctx, n[1], r)
            r &= "."
            r &= $(n[2])
        else:
            gen(ctx, n[1], r)
            r &= "."
            r &= $(n[0])
    else:
        if n[0].isSystemSym:
            genSystemCall(ctx, n, r)
        else:
            gen(ctx, n[0], r)
            r &= "("
            for i in 1 ..< n.len:
                if i != 1: r &= ","
                gen(ctx, n[i], r)
            r &= ")"

proc genInfixCall(ctx: var GLSLCompilerContext, n: NimNode, r: var string) =
    if isGLSLBuiltin(n[0]):
        r &= "("
        gen(ctx, n[1], r)
        r &= $(n[0])
        gen(ctx, n[2], r)
        r &= ")"
    else:
        genCall(ctx, n, r)

proc genPrefixCall(ctx: var GLSLCompilerContext, n: NimNode, r: var string) =
    if isGLSLBuiltin(n[0]):
        r &= $(n[0])
        gen(ctx, n[1], r)
    else:
        genCall(ctx, n, r)

proc genAsgn(ctx: var GLSLCompilerContext, n: NimNode, r: var string) =
    gen(ctx, n[0], r)
    r &= "="
    gen(ctx, n[1], r)

proc genReturnStmt(ctx: var GLSLCompilerContext, n: NimNode, r: var string) =
    if ctx.isMainProc:
        if n[0].kind == nnkEmpty:
            r &= "return"
        else:
            gen(ctx, genSym(nskVar, "result"), r)
            r &= "="
            gen(ctx, n[0][1], r)
            r &= ";return"
    else:
        r &= "return"
        if n[0].kind == nnkEmpty:
            if ctx.procNode.params[0].kind != nnkEmpty:
                r &= " result"
        else:
            r &= " "
            gen(ctx, n[0][1], r)

proc genGLSLBuiltinSym(n: NimNode): string =
    let pn = $n
    case pn
    of "newVec2": result = "vec2"
    of "newVec3": result = "vec3"
    of "newVec4": result = "vec4"
    else: result = pn

proc genSym(ctx: var GLSLCompilerContext, n: NimNode, r: var string) =
    let i = getImpl(n.symbol)
    case i.kind
    of nnkProcDef:
        if isGLSLBuiltin(i):
            r &= genGLSLBuiltinSym(n)
        else:
            #echo "SYMBOL: ", treeRepr(i)
            if not ($(n.symbol) in ctx.definedSyms):
                ctx.definedSyms.add($(n.symbol))
                gen(ctx, i, r)
            r &= $n
    else:
        r &= $n

iterator paramsAndTypes*(procNode: NimNode): tuple[name, typ: NimNode] =
    for i in 1 ..< procNode.params.len:
        for j in 0 .. procNode.params[i].len - 3:
            yield(procNode.params[i][j], procNode.params[i][^2])

proc genGlobals(ctx: var GLSLCompilerContext, n: NimNode) =
    # n is the main proc def. collect uniforms, varyings and attributes
    var globals = ""
    if ctx.shaderKind == skFragmentShader:
        globals = """
#ifdef GL_ES
#extension GL_OES_standard_derivatives : enable
precision mediump float;
#endif
"""
    for param in n.paramsAndTypes:
        let paramName = $(param.name)
        if paramName.startsWith("v"):
            globals &= "varying "
        elif paramName.startsWith("a"):
            globals &= "attribute "
        else:
            globals &= "uniform "

        globals &= getTypeName(ctx, param.typ, true)
        globals &= " "
        globals &= paramName
        globals &= ";"

    if globals.len > 0:
        ctx.globalDefs.add(globals)

proc genProcDef*(ctx: var GLSLCompilerContext, n: NimNode, main = false) =
    let oldNode = ctx.procNode
    let oldMain = ctx.isMainProc
    ctx.procNode = n
    ctx.isMainProc = main

    var retType = "void"
    if n.params[0].kind != nnkEmpty:
        retType = getTypeName(ctx, n.params[0])

    var r = if main: "void" else: retType
    r &= " "
    r &= (if main: ctx.mainProcName else: $(n[0]))
    r &= "("

    if main:
        genGlobals(ctx, n)
    else:
        var first = true
        for i in 1 ..< n.params.len:
            for j in 0 .. n.params[i].len - 3:
                if first:
                    first = false
                else:
                    r &= ","
                r &= getTypeName(ctx, n.params[i][^2])
                r &= " "
                r &= $(n.params[i][j])
    r &= "){"
    if main:
        r &= retType
        r &= " result"
        r &= "=vec4(0.0);"
    elif n.params[0].kind != nnkEmpty:
        r &= retType
        r &= " result;"
    gen(ctx, n.body, r)
    genSemicolon(r)
    if n.params[0].kind != nnkEmpty and not main:
        r &= "return result;"
    elif main:
        case ctx.shaderKind
        of skVertexShader:
            r &= "gl_Position=result;"
        of skFragmentShader:
            r &= "gl_FragColor=result;"
    r &= "}"
    ctx.globalDefs.add(r)
    ctx.procNode = oldNode
    ctx.isMainProc = oldMain

proc genBlockStmt(ctx: var GLSLCompilerContext, n: NimNode, r: var string) =
    r &= "{"
    gen(ctx, n[1], r)
    r &= "}"

proc genWhileStmt(ctx: var GLSLCompilerContext, n: NimNode, r: var string) =
    r &= "while("
    gen(ctx, n[0], r)
    r &= "){"
    gen(ctx, n[1], r)
    r &= "}"

proc genConv(ctx: var GLSLCompilerContext, n: NimNode, r: var string) =
    gen(ctx, n[1], r)

proc genBracketExpr(ctx: var GLSLCompilerContext, n: NimNode, r: var string) =
    let indexVal = n[1].intVal
    gen(ctx, n[0], r)
    r &= "."
    case indexVal
    of 0: r &= "x"
    of 1: r &= "y"
    of 2: r &= "z"
    of 3: r &= "w"
    else: assert(false)

proc genIfStmt(ctx: var GLSLCompilerContext, n: NimNode, r: var string) =
    var first = true
    for c in n:
        if c.kind == nnkElifBranch:
            if first:
                r &= "if"
                first = false
            else:
                r &= "else if"
            r &= "("
            gen(ctx, c[0], r)
            r &= "){"
            gen(ctx, c[1], r)
        elif c.kind == nnkElse:
            r &= "else{"
            gen(ctx, c[0], r)
        else:
            echo "UNEXPECTED IF BRANCH: ", treeRepr(c)
            assert(false)
        genSemicolon(r)
        r &= "}"

proc gen(ctx: var GLSLCompilerContext, n: NimNode, r: var string) =
    case n.kind:
    of nnkLetSection, nnkVarSection: genLetSection(ctx, n, r)
    of nnkStmtList: genStmtList(ctx, n, r)
    of nnkCall: genCall(ctx, n, r)
    of nnkInfix: genInfixCall(ctx, n, r)
    of nnkPrefix: genPrefixCall(ctx, n, r)
    of nnkFloatLit, nnkFloat64Lit: r &= $(n.floatVal)
    of nnkIntLit: r &= $(n.intVal)
    of nnkAsgn, nnkFastAsgn: genAsgn(ctx, n, r)
    of nnkSym: genSym(ctx, n, r)
    of nnkReturnStmt: genReturnStmt(ctx, n, r)
    of nnkProcDef: genProcDef(ctx, n)
    of nnkBlockStmt: genBlockStmt(ctx, n, r)
    of nnkIfStmt: genIfStmt(ctx, n, r)
    of nnkWhileStmt: genWhileStmt(ctx, n, r)
    of nnkConv, nnkHiddenStdConv: genConv(ctx, n, r)
    of nnkHiddenDeref, nnkHiddenAddr: gen(ctx, n[0], r)
    of nnkBracketExpr: genBracketExpr(ctx, n, r)
    of nnkDiscardStmt: gen(ctx, n[0], r)
    of nnkEmpty, nnkCommentStmt: discard
    else:
        echo "UNKNOWN NODE:"
        echo treeRepr(n)
