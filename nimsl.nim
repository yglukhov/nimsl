import macros, math, strutils

type
    glslbuiltin_t = object
    glslinfix_t = object

{.pragma: glslbuiltin, tags: [glslbuiltin_t].}
{.pragma: glsltype.}
{.pragma: glslinfix, tags: [glslinfix_t].}

type ShaderKind = enum
    skVertexShader
    skFragmentShader

type GLSLCompilerContext = object
    procNode: NimNode
    isMainProc: bool
    globalDefs: seq[string]
    definedSyms: seq[string]
    shaderKind: ShaderKind

proc newCtx(): GLSLCompilerContext =
    result.definedSyms = newSeq[string]()
    result.globalDefs = newSeq[string]()

proc gen(ctx: var GLSLCompilerContext, n: NimNode, r: var string)

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

proc skipTypes(n: NimNode, s: TNimTypeKinds): NimNode =
    result = n
    while result.typeKind in s:
        result = getType(result)

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
        if r[^1] != ';' and r[^1] != '}' and r[^1] != '{':
            r &= ";"

proc isSystemSym(s: NimNode): bool =
    let ln = s.symbol.getImpl().lineinfo
    if ln.find("lib/system.nim(") != -1:
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
    elif $(n) == "result":
        if ctx.isMainProc:
            if ctx.shaderKind == skVertexShader:
                r &= "gl_Position"
            elif ctx.shaderKind == skFragmentShader:
                r &= "gl_FragColor"
            else:
                assert(false, "Not Implemented")
        else:
            r &= "result"
    else:
        r &= $n

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
    for i in 1 ..< n.params.len:
        for j in 0 .. n.params[i].len - 3:
            let paramName = $(n.params[i][j])
            if paramName.startsWith("v"):
                globals &= "varying "
            elif paramName.startsWith("a"):
                globals &= "attribute "
            else:
                globals &= "uniform "

            globals &= getTypeName(ctx, n.params[i][^2], true)
            globals &= " "
            globals &= paramName
            globals &= ";\L"

    if globals.len > 0:
        ctx.globalDefs.add(globals)

proc genProcDef(ctx: var GLSLCompilerContext, n: NimNode, main = false) =
    let oldNode = ctx.procNode
    let oldMain = ctx.isMainProc
    ctx.procNode = n
    ctx.isMainProc = main

    var retType = "void"
    if n.params[0].kind != nnkEmpty:
        retType = getTypeName(ctx, n.params[0])

    var r = if main: "void" else: retType
    r &= " "
    r &= (if main: "main" else: $(n[0]))
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
        gen(ctx, genSym(nskVar, "result"), r)
        r &= "=vec4(0.0);"
    elif n.params[0].kind != nnkEmpty:
        r &= retType
        r &= " result;"
    gen(ctx, n.body, r)
    if n.params[0].kind != nnkEmpty and not main:
        r &= "return result;"
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
    of nnkWhileStmt: genWhileStmt(ctx, n, r)
    of nnkConv, nnkHiddenStdConv: genConv(ctx, n, r)
    of nnkHiddenDeref, nnkHiddenAddr: gen(ctx, n[0], r)
    of nnkBracketExpr: genBracketExpr(ctx, n, r)
    of nnkDiscardStmt: gen(ctx, n[0], r)
    of nnkEmpty, nnkCommentStmt: discard
    else:
        echo "UNKNOWN NODE:"
        echo treeRepr(n)

proc getShaderCode(s: NimNode, k: ShaderKind): string =
    var ctx = newCtx()
    ctx.shaderKind = k
    genProcDef(ctx, getImpl(s.symbol), true)
    result = ""
    for i in ctx.globalDefs:
        result &= i
    echo "SHADER CODE: ", result

macro getGLSLFragmentShader*(s: typed): expr =
    result = newLit(getShaderCode(s, skFragmentShader))

macro getGLSLVertexShader*(s: typed): expr =
    result = newLit(getShaderCode(s, skVertexShader))

type
    vecBase[I: static[int], T] = distinct array[I, T]
    matBase[I: static[int], T] = distinct array[I * I, T]

    mat3* = array[9, float32]
    mat4* = array[16, float32]

    vec2* {.glsltype.} = vecBase[2, float32]
    vec3* {.glsltype.} = vecBase[3, float32]
    vec4* {.glsltype.} = vecBase[4, float32]

    ivec2* = vecBase[2, int32]
    ivec3* = vecBase[3, int32]
    ivec4* = vecBase[4, int32]

    uvec2* = vecBase[2, uint32]
    uvec3* = vecBase[3, uint32]
    uvec4* = vecBase[4, uint32]

    dvec2* = vecBase[2, float64]
    dvec3* = vecBase[3, float64]
    dvec4* = vecBase[4, float64]

    bvec2* = vecBase[2, bool]
    bvec3* = vecBase[3, bool]
    bvec4* = vecBase[4, bool]

template defineBorrowsForVec(i: int, t: typedesc): stmt =
    template `[]`*(v: vecBase[i, t], index: int): auto =
        array[i, t](v)[index]
    template `[]=`*(v: var vecBase[i, t], index: int, val: t) =
        array[i, t](v)[index] = val

template defineBorrowsForVecs(t: typedesc): stmt =
    defineBorrowsForVec(2, t)
    defineBorrowsForVec(3, t)
    defineBorrowsForVec(4, t)

defineBorrowsForVecs(float32)
defineBorrowsForVecs(int32)
defineBorrowsForVecs(uint32)
#defineBorrowsForVecs(float64)
defineBorrowsForVecs(bool)

proc newVec2*(x, y: float32): vec2 {.glslbuiltin.} = [x, y].vec2
proc newVec3*(x, y, z: float32): vec3 {.glslbuiltin.} = [x, y, z].vec3
proc newVec4*(x, y, z, w: float32): vec4 {.glslbuiltin.} = [x, y, z, w].vec4

proc newVec2*(x: float32): vec2 {.glslbuiltin.} = [x, x].vec2
proc newVec3*(x: float32): vec3 {.glslbuiltin.} = [x, x, x].vec3
proc newVec4*(x: float32): vec4 {.glslbuiltin.} = [x, x, x, x].vec4

proc newVec3*(x, y: float32): vec3 {.glslbuiltin.} = [x, y, y].vec3
proc newVec4*(x, y: float32): vec4 {.glslbuiltin.} = [x, y, y, y].vec4

proc newVec4*(x, y, z: float32): vec4 {.glslbuiltin.} = [x, y, z, z].vec4
proc newVec4*(v: vec2, z, w: float32): vec4 {.glslbuiltin.} = [v[0], v[1], z, w].vec4
proc newVec4*(u, v: vec2): vec4 {.glslbuiltin.} = [u[0], u[1], v[0], v[1]].vec4

proc `+`*(v: vec2, s: float32): vec2 {.glslbuiltin.} = [v[0] + s, v[1] + s].vec2
proc `-`*(v: vec2, s: float32): vec2 {.glslbuiltin.} = [v[0] - s, v[1] - s].vec2
proc `*`*(v: vec2, s: float32): vec2 {.glslbuiltin.} = [v[0] * s, v[1] * s].vec2
proc `/`*(v: vec2, s: float32): vec2 {.glslbuiltin.} = [v[0] / s, v[1] / s].vec2

proc `/`*(s: float32, v: vec2): vec2 {.glslbuiltin.} = [s / v[0], s / v[1]].vec2


proc `+`*(u, v: vec2): vec2 {.glslbuiltin.} = [u[0] + v[0], u[1] + v[1]].vec2
proc `-`*(u, v: vec2): vec2 {.glslbuiltin.} = [u[0] - v[0], u[1] - v[1]].vec2
proc `*`*(u, v: vec2): vec2 {.glslbuiltin.} = [u[0] * v[0], u[1] * v[1]].vec2
proc `/`*(u, v: vec2): vec2 {.glslbuiltin.} = [u[0] / v[0], u[1] / v[1]].vec2

proc `-`*(v: vec2): vec2 {.glslbuiltin.} = [-v[0], -v[1]].vec2


proc sin*(v: float32 | float64): auto {.glslbuiltin.} = math.sin(v)
proc abs*(v: float32 | float64): auto {.glslbuiltin.} = (if v < 0: -v else: v)
proc abs*(v: vec2): vec2 {.glslbuiltin.} = [abs(v[0]), abs(v[1])].vec2
proc max*(u, v: float32): float32 {.glslbuiltin.} = (if u > v: u else: v)
proc max*(v: vec2, s: float32): vec2 {.glslbuiltin.} = [max(v[0], s), max(v[1], s)].vec2
proc min*(u, v: float32): float32 {.glslbuiltin.} = (if u > v: v else: u)
proc min*(v: vec2, s: float32): vec2 {.glslbuiltin.} = [min(v[0], s), min(v[1], s)].vec2
proc length*(v: vec2): float32 {.glslbuiltin.} = sqrt(v[0] * v[0] + v[1] * v[1])

proc smoothstep*(edge0, edge1, x: float32): float32 {.glslbuiltin.} =
    let c = clamp((x - edge0)/(edge1 - edge0), 0.0, 1.0)
    result = c*c*(3 - 2*c)

proc fwidth*(v: float32): float32 {.glslbuiltin.} = 0
proc mix*(x, y, a: vec4): vec4 {.glslbuiltin.} =
    assert(false, "Not implemented")
proc mix*(x, y: vec4, a: float32): vec4 {.glslbuiltin.} =
    assert(false, "Not implemented")

proc dot*[I: static[int], T](v1, v2: vecBase[I, T]): T {.glslbuiltin.} =
    for i in 0 ..< I: result += v1[i] * v2[i]


template x*[I: static[int], T](v: vecBase[I, T]): T = v[0]
template y*[I: static[int], T](v: vecBase[I, T]): T = v[1]
template z*[I: static[int], T](v: vecBase[I, T]): T = v[2]
template w*[I: static[int], T](v: vecBase[I, T]): T = v[3]

template `x=`*[I: static[int], T](v: var vecBase[I, T], val: T) = v[0] = val
template `y=`*[I: static[int], T](v: var vecBase[I, T], val: T) = v[1] = val
template `z=`*[I: static[int], T](v: var vecBase[I, T], val: T) = v[2] = val
template `w=`*[I: static[int], T](v: var vecBase[I, T], val: T) = v[3] = val

proc `*`*(m: mat4, v: vec4): vec4 {.glslbuiltin.} =
    assert(false, "Not implemented")
    discard

proc `.`*[T](v: vecBase[2, T], f: static[string]): vecBase[f.len, T] {.glslinfix.} =
    for i, c in f:
        case c
            of 'x', 'r': result[i] = v.x
            of 'y', 'g': result[i] = v.y
            else: assert(false, "Unknown field: " & $c)

proc `.`*[T](v: vecBase[3, T], f: static[string]): vecBase[f.len, T] {.glslinfix.} =
    for i, c in f:
        case c
            of 'x', 'r': result[i] = v.x
            of 'y', 'g': result[i] = v.y
            of 'z', 'b': result[i] = v.z
            else: assert(false, "Unknown field: " & $c)

proc `.`*[T](v: vecBase[4, T], f: static[string]): vecBase[f.len, T] {.glslinfix.} =
    for i, c in f:
        case c
            of 'x', 'r': result[i] = v.x
            of 'y', 'g': result[i] = v.y
            of 'z', 'b': result[i] = v.z
            of 'w', 'a': result[i] = v.w
            else: assert(false, "Unknown field: " & $c)

when isMainModule:
    proc myVertexShader(uModelViewProjectionMatrix: mat4, aPos: vec2, vPos: var vec2): vec4 =
        vPos = aPos
        result = uModelViewProjectionMatrix * newVec4(aPos, 0, 1)

    proc testProc(): float32 = 1.0

    proc myShader(vPos, someP: vec2): vec4 =
        var a = -newVec2(50)
        let b = -50.0

    echo "VERTEX SHADER: "
    echo getGLSLVertexShader(myVertexShader)

    echo "FRAGMENT SHADER: "
    echo getGLSLFragmentShader(myShader)
