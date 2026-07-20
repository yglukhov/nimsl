import std/[macros, strutils, tables, hashes]
import ./[common, lower_exprs, codegen_common]

proc hash(n: NimNode): Hash = hash($n)

type
  ShaderLanguage* = enum
    slGLSL
    slWGSL

  ShaderKind* = enum
    skVertexShader
    skFragmentShader

  CompilerContext* = object of CompilerContextBase
    shaderLanguage*: ShaderLanguage
    isMainProc: bool
    shaderKind*: ShaderKind
    mainProcName*: string

  GLSLCompilerContext* = CompilerContext

proc newCtx*(shaderLanguage = slGLSL): CompilerContext =
  result.shaderLanguage = shaderLanguage
  result.mainProcName = "main"

proc slSel[T](ctx: CompilerContext, glsl, wgsl: T): T =
  case ctx.shaderLanguage
  of slGLSL: glsl
  of slWGSL: wgsl

proc isGLSL(c: CompilerContext): bool = c.shaderLanguage == slGLSL
proc isWGSL(c: CompilerContext): bool = c.shaderLanguage == slWGSL

proc gen(ctx: var CompilerContext, n: NimNode, r: var string)
proc genStmtList(ctx: var CompilerContext, n: NimNode, r: var string)
proc getTypeName(ctx: var CompilerContext, t: NimNode, skipVar = false): string

proc rangeLen(n: NimNode): int =
  if n.kind == nnkBracketExpr and $(n[0]) == "range":
    (n[2].intVal - n[1].intVal + 1).int
  elif n.kind == nnkInfix and n.len == 3 and n[0].isIdent("..") and n[1].kind == nnkIntLit and n[2].kind == nnkIntLit:
    (n[2].intVal - n[1].intVal + 1).int
  elif n.kind == nnkIntLit:
    n.intVal.int
  else:
    -1

proc varAttrs(pragmas: NimNode): seq[string] =
  for p in pragmas:
    case p.kind
    of nnkSym:
      if p.isIdent("storage"): result.add("storage")
      elif p.isIdent("readWrite"): result.add("read_write")
      elif p.isIdent("read"): result.add("read")
      elif p.isIdent("write"): result.add("write")
      elif p.isIdent("uniform"): result.add("uniform")
      elif p.isIdent("private"): result.add("private")
      elif p.isIdent("workgroup"): result.add("workgroup")
    else:
      discard

proc genPragmas(ctx: var CompilerContext, pragmas: NimNode, r: var string) =
  if ctx.shaderLanguage != slWGSL:
    return
  for p in pragmas:
    case p.kind
    of nnkSym, nnkIdent:
      if p.isIdent("compute"):
        r &= "@compute "
      elif p.isIdent("fragment"):
        r &= "@fragment "
      elif p.isIdent("vertex"):
        r &= "@vertex "
    of nnkCall:
      if p.len > 0 and p[0].kind == nnkSym:
        let id = $p[0]
        if cmpIgnoreStyle(id, "workgroupSize") == 0:
          r &= "@workgroup_size("
          for i in 1 ..< p.len:
            if i != 1:
              r &= ","
              ctx.space(r)
            r &= $p[i].intVal
          r &= ") "
        if cmpIgnoreStyle(id, "location") == 0:
          r &= "@location("
          for i in 1 ..< p.len:
            if i != 1:
              r &= ","
              ctx.space(r)
            r &= $p[i].intVal
          r &= ") "
        elif cmpIgnoreStyle(id, "builtin") == 0:
          r &= "@builtin("
          let a = $p[1]
          if cmpIgnoreCase(a, "globalInvocationId") == 0:
            r &= "global_invocation_id"
          elif cmpIgnoreCase(a, "localInvocationIndex") == 0:
            r &= "local_invocation_index"
          elif cmpIgnoreCase(a, "vertexIndex") == 0:
            r &= "vertex_index"
          elif cmpIgnoreCase(a, "instanceIndex") == 0:
            r &= "instance_index"
          elif cmpIgnoreCase(a, "position") == 0:
            r &= "position"
          elif cmpIgnoreCase(a, "fragDepth") == 0:
            r &= "frag_depth"
          r &= ") "
        elif cmpIgnoreStyle(id, "group") == 0:
          r &= "@group("
          for i in 1 ..< p.len:
            if i != 1:
              r &= ","
              ctx.space(r)
            r &= $p[i].intVal
          r &= ") "
        elif cmpIgnoreStyle(id, "binding") == 0:
          r &= "@binding("
          for i in 1 ..< p.len:
            if i != 1:
              r &= ","
              ctx.space(r)
            r &= $p[i].intVal
          r &= ") "
    else:
      discard

proc genType(c: var CompilerContext, n: NimNode) =
  let i = getImpl(n)
  i.expectKind(nnkTypeDef)
  resetPropertyInScope(c.indent)

  let o = i[2]
  case o.kind
  of nnkObjectTy:
    let rec = o[2]
    rec.expectKind(nnkRecList)
    var r = "struct "
    let name = c.globalSymName(n)
    r &= name
    c.space(r)
    r &= "{"
    c.nl(r)
    inc c.indent

    var first = true
    for i in rec:
      for j in 0 .. i.len - 3:
        if first: first = false
        else:
          r &= ","
          c.nl(r)
        c.indent(r)

        var name = i[j]
        let typ = i[^2]
        if name.kind == nnkPragmaExpr:
          genPragmas(c, name[1], r)
          name = name[0]
        if name.kind == nnkPostfix:
          name = name[1]
        r &= $name
        r &= ":"
        c.space(r)
        r &= getTypeName(c, typ)

    dec c.indent

    c.nl(r)
    r &= "}"
    c.nl(r)
    c.nl(r)
    c.globalDefs &= r
    c.globalSyms[n] = name
  of nnkEnumTy:
    c.globalSyms[n] = "i32"
  else:
    echo "Unexpected type kind: ", o.kind
    doAssert(false)

proc getTypeName(ctx: var CompilerContext, t: NimNode, skipVar = false): string =
  case t.kind
  of nnkBracketExpr:
    let t0 = $t[0]
    if t0 == "array" and t[1].rangeLen == 2 and $t[2] == "float32":
      result = ctx.slSel(wgsl = "vec2f", glsl = "vec2")
    elif t0 == "array" and t[1].rangeLen == 3 and $t[2] == "float32":
      result = ctx.slSel(wgsl = "vec3f", glsl = "vec3")
    elif t0 == "array" and t[1].rangeLen == 4 and $t[2] == "float32":
      result = ctx.slSel(wgsl = "vec4f", glsl = "vec4")
    elif t0 == "array":
      if ctx.shaderLanguage == slWGSL:
        result = "array<" & getTypeName(ctx, t[2], skipVar) & "," & $(t[1].rangeLen) & ">"
      else:
        echo "Unknown type: ", treeRepr(t)
        assert(false, "Unknown type")
    elif t0 == "distinct":
      result = getTypeName(ctx, t[1], skipVar)
    elif t0 == "seq" and ctx.shaderLanguage == slWGSL:
      result = "array<"
      result &= getTypeName(ctx, t[1], skipVar)
      result &= ">"
    elif t0 == "Texture2D" and ctx.shaderLanguage == slWGSL:
      result = "texture_2d<"
      result &= getTypeName(ctx, t[1], skipVar)
      result &= ">"
    elif t0 == "TextureStorage2D" and ctx.shaderLanguage == slWGSL:
      result = "texture_storage_2d<"
      result &= "r8uint"
      # result &= getTypeName(ctx, t[1], skipVar)
      result &= ", read_write"
      result &= ">"
    else:
      echo "Unknown type: ", treeRepr(t)
      assert(false, "Unknown type")
  of nnkSym:
    case $t
    of "VecBase", "Texture2D": result = getTypeName(ctx, getType(t), skipVar)
    of "float32": result = ctx.slSel(wgsl = "f32", glsl = "float")
    of "int32": result = ctx.slSel(wgsl = "i32", glsl = "int")
    of "uint32": result = ctx.slSel(wgsl = "u32", glsl = "uint")
    of "bool": result = "bool"
    of "Vec2", "vec2": result = ctx.slSel(wgsl = "vec2f", glsl = "vec2")
    of "Vec3", "vec3": result = ctx.slSel(wgsl = "vec3f", glsl = "vec3")
    of "Vec4", "vec4": result = ctx.slSel(wgsl = "vec4f", glsl = "vec4")
    of "Vec2i", "ivec2": result = ctx.slSel(wgsl = "vec2i", glsl = "ivec2")
    of "Vec3i", "ivec3": result = ctx.slSel(wgsl = "vec3i", glsl = "ivec3")
    of "Vec4i", "ivec4": result = ctx.slSel(wgsl = "vec4i", glsl = "ivec4")
    of "Vec2u", "uvec2": result = ctx.slSel(wgsl = "vec2u", glsl = "uvec2")
    of "Vec3u", "uvec3": result = ctx.slSel(wgsl = "vec3u", glsl = "uvec3")
    of "Vec4u", "uvec4": result = ctx.slSel(wgsl = "vec4u", glsl = "uvec4")
    of "Vec2d", "dvec2": result = "dvec2"
    of "Vec3d", "dvec3": result = "dvec3"
    of "Vec4d", "dvec4": result = "dvec4"
    of "Vec2b", "bvec2": result = ctx.slSel(wgsl = "vec2<bool>", glsl = "bvec2")
    of "Vec3b", "bvec3": result = ctx.slSel(wgsl = "vec3<bool>", glsl = "bvec3")
    of "Vec4b", "bvec4": result = ctx.slSel(wgsl = "vec4<bool>", glsl = "bvec4")
    of "Mat3", "mat3": result = ctx.slSel(wgsl = "mat3x3f", glsl = "mat3")
    of "Mat4", "mat4": result = ctx.slSel(wgsl = "mat4x4f", glsl = "mat4")
    of "Sampler": result = "sampler"
    else:
      result = ctx.globalSyms.getOrDefault(t)
      if result == "":
        genType(ctx, t)
        result = ctx.globalSyms[t]
  of nnkVarTy:
    result = getTypeName(ctx, t[0])
    if not skipVar:
      result = case ctx.shaderLanguage
      of slGLSL: "inout " & result
      of slWGSL: "ptr<function, " & result & ">"
  else:
    echo "UNKNOWN TYPE: ", treeRepr(t)
    assert(false, "Unknown type")

proc genWGSLLetSection(ctx: var CompilerContext, n: NimNode, r: var string) =
  for i in n:
    let s = skipPragma(i[0])
    r &= (if n.kind == nnkLetSection: "let " else: "var ")
    let name = mangleSym(s)
    r &= name
    ctx.localSyms[s] = name
    if i[2].kind != nnkEmpty:
      ctx.space(r)
      r &= "="
      ctx.space(r)
      gen(ctx, i[2], r)
    else:
      r &= ":"
      ctx.space(r)
      r &= getTypeName(ctx, i[1])

proc genGLSLLetSection(ctx: var CompilerContext, n: NimNode, r: var string) =
  for i in n:
    let s = skipPragma(i[0])
    if i[^2].kind != nnkEmpty:
      r &= getTypeName(ctx, i[^2])
    else:
      r &= getTypeName(ctx, getType(s))
    r &= " "
    let name = $s
    r &= name
    ctx.localSyms[s] = name
    if i[2].kind != nnkEmpty:
      r &= "="
      gen(ctx, i[2], r)

proc genLetSection(ctx: var CompilerContext, n: NimNode, r: var string) =
  case ctx.shaderLanguage
  of slWGSL: genWGSLLetSection(ctx, n, r)
  of slGLSL: genGLSLLetSection(ctx, n, r)

proc genStmt(ctx: var CompilerContext, n: NimNode, r: var string) =
  case n.kind
  of nnkStmtList:
    # Nested stmtlists don't need extra indent and semicolon
    gen(ctx, n, r)
  else:
    if (n.kind == nnkDiscardStmt and n[0].kind == nnkEmpty) or n.kind == nnkConstSection:
      return
    ctx.indent(r)
    gen(ctx, n, r)
    if r.len > 0 and r[^1] != '}':
      r &= ";"
    ctx.nl(r)

proc genStmtList(ctx: var CompilerContext, n: NimNode, r: var string) =
  if n.kind == nnkStmtList:
    for i in n:
      genStmt(ctx, i, r)
  else:
    genStmt(ctx, n, r)

proc genStmtListExpr(ctx: var CompilerContext, n: NimNode, r: var string) =
  for i in n:
    gen(ctx, i, r)

proc skipAddr(n: NimNode): NimNode =
  result = n
  while result.kind in {nnkHiddenAddr}:
    result = result[^1]

proc genSystemCall(ctx: var CompilerContext, n: NimNode, r: var string) =
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
      ctx.space(r)
      r &= "-="
      ctx.space(r)
      gen(ctx, n[2], r)
  of "max", "min", "abs", "clamp":
    r &= $(n[0])
    r &= "("
    for i in 1 ..< n.len:
      if i != 1:
        r &= ","
        ctx.space(r)
      gen(ctx, n[i], r)
    r &= ")"
  else:
    echo "UNKNOWN SYSTEM CALL: ", treeRepr(n)

proc precedance(n: NimNode): int =
  result = 100
  if n.kind in {nnkCall, nnkInfix, nnkPrefix}:
    if n[0].kind == nnkSym:
      let name = $n[0]
      let unary = n.len == 2
      let bin = n.len == 3
      result =
        if unary and name in ["not", "-"]: 80
        elif bin and name in ["*", "/", "mod", "div"]: 70
        elif bin and name in ["+", "-"]: 60
        elif bin and name in ["shl", "shr"]: 50
        elif bin and name in ["<", "<=", ">", ">="]: 40
        elif bin and name in ["==", "!="]: 35
        elif bin and name in ["and"]: 30
        elif bin and name in ["xor"]: 29
        elif bin and name in ["or"]: 28
        elif bin and name in ["+=", "-=", "*=", "/="]: 5
        else: 100

proc isUndistinctConv(n: NimNode): bool =
  if n.kind != nnkConv:
    return false
  let srcType = getType(n[1]).getTypeImpl
  srcType.kind == nnkDistinctTy and sameType(n[0], srcType[0])

proc isTransparentConv(n: NimNode): bool =
  n.kind == nnkHiddenStdConv or n.isUndistinctConv()

proc skipConv(n: NimNode): NimNode =
  result = n
  while result.isTransparentConv():
    result = result[^1]

proc needsParens(parentPrec: int, parent, child: NimNode, sideLeft: bool): bool =
  let child = skipConv(child)
  let prec = precedance(child)
  if prec < parentPrec: return true
  if prec > parentPrec: return false
  if child.len == 3 and child[0].kind == nnkSym and $child[0] in ["+", "-", "*", "/", "div", "mod", "shl", "shr", "<", ">", "<=", ">=", "==", "!=", "and", "or", "xor", "+=", "-=", "*=", "/="]:
    discard
  else:
    return false
  if $parent[0] in ["=", "+=", "-=", "*=", "/="]:
    return sideLeft
  return not sideLeft

proc genBinaryOpCall(ctx: var CompilerContext, op: string, n: NimNode, r: var string) =
  let prec = precedance(n)
  let aNeedsParentheses = needsParens(prec, n, n[1], true)
  if aNeedsParentheses: r &= "("
  gen(ctx, skipAddr(n[1]), r)
  if aNeedsParentheses: r &= ")"
  ctx.space(r)
  r &= op
  ctx.space(r)
  let bNeedsParentheses = needsParens(prec, n, n[2], false)
  if bNeedsParentheses: r &= "("
  gen(ctx, n[2], r)
  if bNeedsParentheses: r &= ")"

proc genUnaryOpCall(ctx: var CompilerContext, op: string, n: NimNode, r: var string) =
  let prec = precedance(n)
  let aNeedsParentheses = precedance(n[1]) < prec
  r &= op
  if aNeedsParentheses: r &= "("
  gen(ctx, skipAddr(n[1]), r)
  if aNeedsParentheses: r &= ")"

proc genPostfixReceiver(ctx: var CompilerContext, a: NimNode, r: var string) =
  let postfixPrec = 90
  let aNeedsParentheses = precedance(skipConv(a)) < postfixPrec
  if aNeedsParentheses: r &= "("
  gen(ctx, a, r)
  if aNeedsParentheses: r &= ")"

proc genDotExpr(ctx: var CompilerContext, a: NimNode, b: string, r: var string) =
  ctx.genPostfixReceiver(a, r)
  r &= "."
  r &= b

proc genDotExpr(ctx: var CompilerContext, n: NimNode, r: var string) =
  # let indexVal = n[1].skipConv.intVal
  ctx.genDotExpr(n[0], $n[1], r)

proc genCall(ctx: var CompilerContext, n: NimNode, r: var string) =
  let symIsMagic = n[0].isMagic()
  var name = ""
  if n[0].kind == nnkSym:
    name = $n[0]

  if symIsMagic and name in [".", "nimsl_deriveVectorWithComponents"]:
    # This is a property
    ctx.genDotExpr(n[1], $n[2], r)
  else:
    let symIsSystem = isSystemSym(n[0])
    if (symIsMagic or symIsSystem) and n.len == 3 and name in ["+=", "-=", "*=", "/=", "+", "-", "*", "/", "<=", ">=", "<", ">", "==", "!="]:
      ctx.genBinaryOpCall(name, n, r)
    elif (symIsMagic or symIsSystem) and n.len == 3 and name == "div":
      ctx.genBinaryOpCall("/", n, r)
    elif (symIsMagic or symIsSystem) and n.len == 3 and name == "mod":
      ctx.genBinaryOpCall("%", n, r)
    elif (symIsMagic or symIsSystem) and n.len == 3 and name == "shl":
      ctx.genBinaryOpCall("<<", n, r)
    elif (symIsMagic or symIsSystem) and n.len == 3 and name == "shr":
      ctx.genBinaryOpCall(">>", n, r)
    elif (symIsMagic or symIsSystem) and n.len == 3 and name == "and":
      if getType(n).isIdent("bool"):
        ctx.genBinaryOpCall("&&", n, r)
      else:
        ctx.genBinaryOpCall("&", n, r)
    elif (symIsMagic or symIsSystem) and n.len == 3 and name == "or":
      if getType(n).isIdent("bool"):
        ctx.genBinaryOpCall("||", n, r)
      else:
        ctx.genBinaryOpCall("|", n, r)
    elif (symIsMagic or symIsSystem) and n.len == 3 and name == "xor":
      if getType(n).isIdent("bool"):
        ctx.genBinaryOpCall("^^", n, r)
      else:
        ctx.genBinaryOpCall("^", n, r)
    elif (symIsMagic or symIsSystem) and n.len == 2 and name == "-":
      ctx.genUnaryOpCall(name, n, r)
    elif (symIsMagic or symIsSystem) and n.len == 2 and name == "not":
      if getType(n).isIdent("bool"):
        ctx.genUnaryOpCall("!", n, r)
      else:
        ctx.genUnaryOpCall("~", n, r)
    elif symIsSystem:
      genSystemCall(ctx, n, r)
    elif symIsMagic:
      if name in ["x", "y", "z", "w", "r", "g", "b", "a"]:
        ctx.genDotExpr(skipAddr(n[1]), name, r)
      else:
        gen(ctx, n[0], r)
        r &= "("
        for i in 1 ..< n.len:
          if i != 1:
            r &= ","
            ctx.space(r)
          gen(ctx, n[i], r)
        r &= ")"
    else:
      gen(ctx, n[0], r)
      r &= "("
      for i in 1 ..< n.len:
        if i != 1:
          r &= ","
          ctx.space(r)
        gen(ctx, n[i], r)
      r &= ")"

proc genObjConstr(ctx: var CompilerContext, n: NimNode, r: var string) =
  # TODO: This is a dummy stub
  r &= getTypeName(ctx, n[0])
  r &= "("
  for i in 1 ..< n.len:
    if i > 1: r &= ","
    let c = n[i]
    c.expectKind(nnkExprColonExpr)
    gen(ctx, c[1], r)
  r &= ")"

proc genBracket(ctx: var CompilerContext, n: NimNode, r: var string) =
  r &= "array("
  for i in 0 ..< n.len:
    if i > 0:
      r &= ","
      ctx.space(r)
    gen(ctx, n[i], r)
  r &= ")"

proc genAsgn(ctx: var CompilerContext, n: NimNode, r: var string) =
  gen(ctx, n[0], r)
  ctx.space(r)
  r &= "="
  ctx.space(r)
  gen(ctx, n[1], r)

proc genReturnStmt(ctx: var CompilerContext, n: NimNode, r: var string) =
  if ctx.isGLSL and ctx.isMainProc:
    if ctx.procNode.params[0].kind == nnkEmpty:
      r &= "return"
    else:
      if ctx.shaderKind == skVertexShader:
        r &= "gl_Position"
      else:
        r &= "gl_FragColor"
      r &= "="
      if n[0].kind == nnkEmpty:
        r &= "result"
      else:
        n[0].expectKind(nnkAsgn)
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

proc genWGSLMagicSym(n: NimNode): string =
  let pn = $n
  case pn
  of "vec2": result = "vec2f"
  of "vec3": result = "vec3f"
  of "vec4": result = "vec4f"
  else: result = pn

proc genGLSLMagicSym(n: NimNode): string =
  let pn = $n
  case pn
  of "newVec2": result = "vec2"
  of "newVec3": result = "vec3"
  of "newVec4": result = "vec4"
  else: result = pn

proc genMagicSym(ctx: var CompilerContext, n: NimNode): string =
  case ctx.shaderLanguage
  of slWGSL: genWGSLMagicSym(n)
  of slGLSL: genGLSLMagicSym(n)

proc globalVarAttrs(pragmas: NimNode, hasAssignment: bool): seq[string] =
  const addressSpaces = ["function", "private", "workgroup", "uniform", "storage", "handle"]
  result = varAttrs(pragmas)
  if hasAssignment:
    var addressSpaceDefined = false
    for a in addressSpaces:
      if a in result:
        addressSpaceDefined = true
        break
    if not addressSpaceDefined:
      result.add("private")

proc genWGSLGlobalVar(ctx: var CompilerContext, n, idDefs: NimNode) =
  var r = ""
  var name = idDefs[0]
  let val = idDefs[^1]
  var pragmas: NimNode
  if name.kind == nnkPragmaExpr:
    pragmas = name[1]
    genPragmas(ctx, pragmas, r)
    name = name[0]
  let namestr = ctx.globalSymName(name)
  ctx.globalSyms[name] = namestr
  r &= "var"
  let varAttrs = globalVarAttrs(pragmas, hasAssignment = val.kind != nnkEmpty)
  if varAttrs.len != 0:
    r &= "<"
    r &= varAttrs.join(if ctx.pretty: ", " else: ",")
    r &= ">"
    ctx.space(r)
  else:
    r &= " "
  r &= namestr
  r &= ":"
  ctx.space(r)

  var nimType = getType(n)
  if nimType.kind == nnkObjectTy:
    nimType = getTypeInst(name)

  r &= getTypeName(ctx, nimType)
  if val.kind != nnkEmpty:
    ctx.space(r)
    r &= "="
    ctx.space(r)
    gen(ctx, val, r)
  r &= ";"
  ctx.nl(r)
  ctx.nl(r)
  ctx.globalDefs &= r

proc genGLSLGlobalVar(ctx: var CompilerContext, n, idDefs: NimNode) =
  echo repr n
  doAssert(false, "Not implemented")

proc genGlobalVar(ctx: var CompilerContext, n, idDefs: NimNode) =
  case ctx.shaderLanguage
  of slWGSL: genWGSLGlobalVar(ctx, n, idDefs)
  of slGLSL: genGLSLGlobalVar(ctx, n, idDefs)

proc genSym(ctx: var CompilerContext, n: NimNode, r: var string) =
  let i = getImpl(n)
  case i.kind
  of nnkProcDef:
    if isMagic(i):
      r &= genMagicSym(ctx, n)
    else:
      # echo "PROCDEF ", n
      var s = ctx.globalSyms.getOrDefault(n)
      if s == "":
        s = ctx.globalSymName(n)
        ctx.globalSyms[n] = s
        gen(ctx, i, r)
      r &= s
  elif n.isIdent("true"):
    r &= "true"
  elif n.isIdent("false"):
    r &= "false"
  elif n.symKind == nskEnumField:
    r &= $n.intVal
  else:
    if i.kind == nnkIdentDefs:
      var name = ctx.localSyms.getOrDefault(n)
      if name == "":
        name = ctx.globalSyms.getOrDefault(n)
        if name == "":
          genGlobalVar(ctx, n, i)
          name = ctx.globalSyms[n]
      r &= name
    else:
      r &= mangleSym(n)

iterator paramsAndTypes*(procNode: NimNode): tuple[name, typ: NimNode] =
  for i in 1 ..< procNode.params.len:
    for j in 0 .. procNode.params[i].len - 3:
      yield(procNode.params[i][j], procNode.params[i][^2])

proc genGLSLGlobals(ctx: var CompilerContext, n: NimNode) =
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
    let paramName = $param.name
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

type
  ProcDefFlag* = enum
    forceVertex
    forceFragment
    forceCompute

proc genGLSLProcDef(ctx: var CompilerContext, n: NimNode, flags: set[ProcDefFlag], main: bool) =
  var retType = "void"
  if n.params[0].kind != nnkEmpty:
    retType = getTypeName(ctx, n.params[0])

  let hasResult = n.params[0].kind != nnkEmpty

  var r = if main: "void" else: retType
  r &= " "

  var name = if main: ctx.mainProcName else: ctx.globalSyms.getOrDefault(n[0])
  if name == "":
    name = $(n[0])
    ctx.globalSyms[n[0]] = name

  r &= name
  r &= "("

  if main:
    genGLSLGlobals(ctx, n)
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
  if hasResult:
    if main:
      r &= retType
      r &= " result"
      r &= "=vec4(0.0);"
    else:
      r &= retType
      r &= " result;"

  let body = lowerExprs(n.body)
  genStmtList(ctx, body, r)

  if hasResult:
    if main:
      case ctx.shaderKind
      of skVertexShader:
        r &= "gl_Position=result;"
      of skFragmentShader:
        r &= "gl_FragColor=result;"
    else:
      r &= "return result;"
  r &= "}"
  ctx.globalDefs.add(r)

proc genWGSLProcDef(ctx: var CompilerContext, n: NimNode, flags: set[ProcDefFlag], main: bool) =
  var retType = "void"
  if n.params[0].kind != nnkEmpty:
    retType = getTypeName(ctx, n.params[0])

  let hasResult = n.params[0].kind != nnkEmpty

  var r = ""
  if forceCompute in flags: r &= "@compute "
  if forceVertex in flags: r &= "@vertex "
  if forceFragment in flags: r &= "@fragment "

  genPragmas(ctx, n.pragma, r)
  ctx.nl(r)
  r &= "fn "
  var name = ctx.globalSyms.getOrDefault(n[0])
  if name == "":
    name = $(n[0])
    ctx.globalSyms[n[0]] = name
  r &= name
  r &= "("

  var first = true
  for i in 1 ..< n.params.len:
    for j in 0 .. n.params[i].len - 3:
      if first:
        first = false
      else:
        r &= ","
        ctx.space(r)
      r &= $(n.params[i][j])
      r &= ":"
      ctx.space(r)
      r &= getTypeName(ctx, n.params[i][^2])
  r &= ")"
  ctx.space(r)
  if retType != "void":
    r &= "->"
    ctx.space(r)
    r &= retType
    ctx.space(r)
  r &= "{"
  ctx.nl(r)
  inc ctx.indent
  let body = lowerExprs(n.body)

  if hasResult and body.kind == nnkAsgn and body[0].isIdent("result"):
    ctx.indent(r)
    r &= "return "
    gen(ctx, body[^1], r)
    r &= ";"
    ctx.nl(r)
  else:
    if hasResult:
      ctx.indent(r)
      r &= "var result:"
      ctx.space(r)
      r &= retType
      r &= ";"
      ctx.nl(r)
    genStmtList(ctx, body, r)
    if hasResult:
      ctx.indent(r)
      r &= "return result;"
      ctx.nl(r)
  dec ctx.indent
  r &= "}"
  ctx.nl(r)
  ctx.nl(r)
  ctx.globalDefs.add(r)

proc genProcDef*(ctx: var CompilerContext, n: NimNode, flags: set[ProcDefFlag] = {}, main = false) =
  # echo "PROCDEF: ", treeRepr n
  resetPropertyInScope(ctx.procNode, n)
  resetPropertyInScope(ctx.indent)
  resetPropertyInScope(ctx.localSyms)
  resetPropertyInScope(ctx.isMainProc, main)

  case ctx.shaderLanguage
  of slWGSL: genWGSLProcDef(ctx, n, flags, main)
  of slGLSL: genGLSLProcDef(ctx, n, flags, main)

proc genBlockStmt(ctx: var CompilerContext, n: NimNode, r: var string) =
  r &= "{"
  gen(ctx, n[1], r)
  r &= "}"

proc genWhileStmt(ctx: var CompilerContext, n: NimNode, r: var string) =
  r &= "while("
  gen(ctx, n[0], r)
  r &= ")"
  ctx.space(r)
  r &= "{"
  ctx.nl(r)
  gen(ctx, n[1], r)
  r &= "}"

proc genForStmt(ctx: var CompilerContext, n: NimNode, r: var string) =
  echo treerepr n
  let s = mangleSym(n[0])
  var rangeStart, rangeEnd, rangeStep: NimNode
  var dirNegative = false
  var inclusive = true
  let rn = n[1]
  if rn.kind == nnkCall and rn[0].isIdent("countdown"):
    rangeStart = rn[1]
    rangeEnd = rn[2]
    rangeStep = rn[3]
    dirNegative = true
  elif rn.kind == nnkCall and rn[0].isIdent("countup"):
    rangeStart = rn[1]
    rangeEnd = rn[2]
    rangeStep = rn[3]
  elif rn.kind == nnkInfix and $rn[0] == "..<":
    rangeStart = rn[1]
    rangeEnd = rn[2]
    rangeStep = newLit(1)
    inclusive = false
  elif rn.kind == nnkInfix and rn[0].isIdent(".."):
    rangeStart = rn[1]
    rangeEnd = rn[2]
    rangeStep = newLit(1)
  else:
    echo repr n
    error("invalid for range")

  r &= "for"
  ctx.space(r)
  r &= "("
  case ctx.shaderLanguage
  of slGLSL:
    r &= ctx.getTypeName(n[0])
    r &= " "
  of slWGSL:
    r &= "var "
  r &= s
  ctx.space(r)
  r &= "="
  ctx.space(r)
  gen(ctx, rangeStart, r)
  r &= ";"
  ctx.space(r)
  r &= s
  ctx.space(r)
  r &= (if dirNegative:
         if inclusive: ">="
         else: ">"
       else:
         if inclusive: "<="
         else: "<")
  ctx.space(r)
  gen(ctx, rangeEnd, r)
  r &= ";"
  ctx.space(r)
  r &= s
  if rangeStep.kind in {nnkIntLit, nnkUint32Lit, nnkInt32Lit} and rangeStep.intVal == 1:
    r &= (if dirNegative: "--"
         else: "++")
  else:
    ctx.space(r)
    r &= (if dirNegative: "-="
        else: "+=")
    ctx.space(r)
    gen(ctx, rangeStep, r)
  r &= ")"
  ctx.space(r)
  r &= "{"
  ctx.nl(r)
  inc ctx.indent
  genStmtList(ctx, n[2], r)
  dec ctx.indent
  ctx.indent(r)
  r &= "}"

proc genConvWithT(ctx: var CompilerContext, n: NimNode, t: NimNode, r: var string) =
  # let isLit = n.kind in {nnkIntLit, nnkInt32Lit, nnkUInt32Lit, nnkFloatLit, nnkFloat32Lit}
  # TODO: Prettier literals. Currently produces e.g. u32(0) instead of 0u
  # echo "CONV: ", repr n, " ", n.kind

  if t.isIdent("uint32"):
    if n.kind in {nnkIntLit, nnkInt32Lit}:
      gen(ctx, n, r)
      r &= "u"
    else:
      r &= "u32("
      gen(ctx, n, r)
      r &= ")"
  elif t.isIdent("int32"):
    if n.kind in {nnkIntLit, nnkInt32Lit}:
      gen(ctx, n, r)
    else:
      r &= "i32("
      gen(ctx, n, r)
      r &= ")"
  elif t.isIdent("float32"):
    if n.kind in {nnkIntLit, nnkInt32Lit}:
      gen(ctx, n, r)
      r &= ".0"
    elif n.kind in {nnkFloatLit, nnkFloat32Lit}:
      gen(ctx, n, r)
    else:
      r &= "f32("
      gen(ctx, n, r)
      r &= ")"
  elif t.kind == nnkSym and getType(t).kind == nnkEnumTy: # Enums are int32 in wgsl
    r &= "i32("
    gen(ctx, n, r)
    r &= ")"
  else:
    gen(ctx, n, r)

proc genWGSLConv(ctx: var CompilerContext, n: NimNode, r: var string) =
  if n[0].kind != nnkEmpty:
    genConvWithT(ctx, n[1], n[0], r)
  elif n.kind == nnkHiddenStdConv and n[1].kind in {
      nnkIntLit, nnkInt8Lit, nnkInt16Lit, nnkInt32Lit, nnkInt64Lit,
      nnkUIntLit, nnkUInt8Lit, nnkUInt16Lit, nnkUInt32Lit, nnkUInt64Lit,
      nnkFloatLit, nnkFloat32Lit, nnkFloat64Lit, nnkFloat128Lit}:
    # Literals need target type info for proper suffix (e.g. IntLit→u32 emits 'u', IntLit→f32 emits '.0')
    genConvWithT(ctx, n[1], getTypeInst(n), r)
  else:
    # Complex expressions (Infix, Call, etc.) may lack type info on the HiddenStdConv.
    # These are implicit float32↔float64 promotions which are no-ops in WGSL (all f32).
    gen(ctx, n[1], r)

proc genGLSLConv(ctx: var CompilerContext, n: NimNode, r: var string) =
  gen(ctx, n[1], r)

proc genConv(ctx: var CompilerContext, n: NimNode, r: var string) =
  case ctx.shaderLanguage
  of slWGSL: genWGSLConv(ctx, n, r)
  of slGLSL: genGLSLConv(ctx, n, r)

proc genBracketExpr(ctx: var CompilerContext, n: NimNode, r: var string) =
  ctx.genPostfixReceiver(n[0], r)
  r &= "["
  gen(ctx, n[1], r)
  r &= "]"


proc genIfStmt(ctx: var CompilerContext, n: NimNode, r: var string) =
  var first = true
  for c in n:
    if c.kind == nnkElifBranch:
      if first:
        r &= "if"
        first = false
      else:
        ctx.nl(r)
        ctx.indent(r)
        r &= "else if"
      case ctx.shaderLanguage
      of slGLSL:
        ctx.space(r)
        r &= "("
      of slWGSL:
        r &= " "
      gen(ctx, c[0], r)
      case ctx.shaderLanguage
      of slGLSL: r &= ")"
      of slWGSL: discard
      ctx.space(r)
      r &= "{"
      ctx.nl(r)
      inc ctx.indent
      genStmtList(ctx, c[1], r)
    elif c.kind == nnkElse:
      ctx.nl(r)
      ctx.indent(r)
      r &= "else"
      ctx.space(r)
      r &= "{"
      ctx.nl(r)
      inc ctx.indent
      genStmtList(ctx, c[0], r)
    else:
      echo "UNEXPECTED IF BRANCH: ", treeRepr(c)
      assert(false)
    dec ctx.indent
    ctx.indent(r)
    r &= "}"

proc genCaseStmt(ctx: var CompilerContext, n: NimNode, r: var string) =
  r &= "switch "
  gen(ctx, n[0], r)
  ctx.space(r)
  r &= "{"
  ctx.nl(r)
  inc ctx.indent

  let t = getType(n[0])

  for i in 1 ..< n.len:
    let c = n[i]
    ctx.indent(r)
    if c.kind == nnkOfBranch:
      r &= "case "
      for j in 0 ..< c.len - 1:
        if j > 0:
          r &= ","
          ctx.space(r)
        gen(ctx, c[j], r)
      r &= ":"
    elif c.kind == nnkElse:
      r &= "default:"
    else:
      echo "UNEXPECTED IF BRANCH: ", treeRepr(c)
      assert(false)
    ctx.space(r)
    r &= "{"
    ctx.nl(r)
    inc ctx.indent
    genStmtList(ctx, c[^1], r)
    dec ctx.indent
    ctx.indent(r)
    r &= "}"
    ctx.nl(r)

  dec ctx.indent
  ctx.indent(r)
  r &= "}"

proc genWGSLIntLit(ctx: var CompilerContext, n: NimNode, r: var string) =
  let t = n.getTypeInst()
  r &= $n.intVal
  if t.isIdent("uint32"):
    r &= "u"

proc genGLSLIntLit(ctx: var CompilerContext, n: NimNode, r: var string) =
  r &= $n.intVal

proc genIntLit(ctx: var CompilerContext, n: NimNode, r: var string) =
  case ctx.shaderLanguage
  of slWGSL: genWGSLIntLit(ctx, n, r)
  of slGLSL: genGLSLIntLit(ctx, n, r)

proc genWGSLUInt32Lit(ctx: var CompilerContext, n: NimNode, r: var string) =
  r &= $n.intVal & "u"

proc genGLSLUInt32Lit(ctx: var CompilerContext, n: NimNode, r: var string) =
  r &= $n.intVal

proc genUInt32Lit(ctx: var CompilerContext, n: NimNode, r: var string) =
  case ctx.shaderLanguage
  of slWGSL: genWGSLUInt32Lit(ctx, n, r)
  of slGLSL: genGLSLUInt32Lit(ctx, n, r)

proc genWGSLHiddenDeref(ctx: var CompilerContext, n: NimNode, r: var string) =
  if n[^1].kind in {nnkIdent, nnkSym}:
    r &= "(*"
    gen(ctx, n[0], r)
    r &= ")"
  else:
    gen(ctx, n[0], r)

proc genGLSLHiddenDeref(ctx: var CompilerContext, n: NimNode, r: var string) =
  gen(ctx, n[0], r)

proc genHiddenDeref(ctx: var CompilerContext, n: NimNode, r: var string) =
  case ctx.shaderLanguage
  of slWGSL: genWGSLHiddenDeref(ctx, n, r)
  of slGLSL: genGLSLHiddenDeref(ctx, n, r)

proc genWGSLHiddenAddr(ctx: var CompilerContext, n: NimNode, r: var string) =
  r &= "(&"
  gen(ctx, n[0], r)
  r &= ")"

proc genGLSLHiddenAddr(ctx: var CompilerContext, n: NimNode, r: var string) =
  gen(ctx, n[0], r)

proc genHiddenAddr(ctx: var CompilerContext, n: NimNode, r: var string) =
  case ctx.shaderLanguage
  of slWGSL: genWGSLHiddenAddr(ctx, n, r)
  of slGLSL: genGLSLHiddenAddr(ctx, n, r)

proc gen(ctx: var CompilerContext, n: NimNode, r: var string) =
  case n.kind:
  of nnkLetSection, nnkVarSection: genLetSection(ctx, n, r)
  of nnkStmtList: genStmtList(ctx, n, r)
  of nnkStmtListExpr: genStmtListExpr(ctx, n, r)
  of nnkCall, nnkInfix, nnkPrefix: genCall(ctx, n, r)
  of nnkObjConstr: genObjConstr(ctx, n, r)
  of nnkBracket: genBracket(ctx, n, r)
  # of nnkInfix: genInfixCall(ctx, n, r)
  # of nnkPrefix: genPrefixCall(ctx, n, r)
  of nnkFloatLit, nnkFloat64Lit, nnkFloat32Lit: r &= $n.floatVal
  of nnkIntLit, nnkInt32Lit: genIntLit(ctx, n, r)
  of nnkUInt32Lit: genUInt32Lit(ctx, n, r)
  of nnkAsgn, nnkFastAsgn: genAsgn(ctx, n, r)
  of nnkSym: genSym(ctx, n, r)
  of nnkReturnStmt: genReturnStmt(ctx, n, r)
  of nnkProcDef: genProcDef(ctx, n)
  of nnkBlockStmt: genBlockStmt(ctx, n, r)
  of nnkIfStmt: genIfStmt(ctx, n, r)
  of nnkCaseStmt: genCaseStmt(ctx, n, r)
  of nnkWhileStmt: genWhileStmt(ctx, n, r)
  of nnkForStmt: genForStmt(ctx, n, r)
  of nnkConv, nnkHiddenStdConv: genConv(ctx, n, r)
  of nnkHiddenDeref: genHiddenDeref(ctx, n, r)
  of nnkHiddenAddr: genHiddenAddr(ctx, n, r)
  of nnkBracketExpr: genBracketExpr(ctx, n, r)
  of nnkDotExpr: genDotExpr(ctx, n, r)
  of nnkBreakStmt: r &= "break"
  of nnkDiscardStmt: gen(ctx, n[0], r)
  of nnkEmpty: discard
  of nnkCommentStmt:
    if ctx.pretty:
      r &= "// " & $n
  else:
    echo "UNKNOWN NODE:"
    echo treeRepr(n)

proc genShader(syms: NimNode): string =
  var c = newCtx(slWGSL)
  c.pretty = true
  when defined(nimslTests):
    c.localMangling = true

  for s in syms:
    genProcDef(c, getImpl(s))
    # echo repr getImpl(s)
  for i in c.globalDefs:
    result &= i


  # echo repr syms


macro wgslShader*(syms: varargs[typed]): untyped =
  newLit(genShader(syms))

when wgslOutputPath != "":
  var c {.compileTime.} = CompilerContext(shaderLanguage: slWGSL, pretty: not defined(release), localMangling: false)

  proc flushDefs(since: int) =
    writeFile(wgslOutputPath, c.globalDefs.join())

  proc singleVertexShader*(n: NimNode): NimNode =
    let i = c.globalDefs.len
    genProcDef(c, n, {forceVertex})
    flushDefs(i)
    return n

  proc singleFragmentShader*(n: NimNode): NimNode =
    let i = c.globalDefs.len
    genProcDef(c, n, {forceFragment})
    flushDefs(i)
    return n

  proc singleComputeShader*(n: NimNode): NimNode =
    let i = c.globalDefs.len
    genProcDef(c, n, {forceCompute})
    flushDefs(i)
    return n
