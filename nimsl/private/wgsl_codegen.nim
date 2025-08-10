import std/[macros, strutils, tables, hashes]
import ./[common, lower_exprs, codegen_common]

proc hash(n: NimNode): Hash = hash($n)

type CompilerContext* = object of CompilerContextBase
  globalIdCounter: int
  localSyms: Table[NimNode, string]
  globalSyms: Table[NimNode, string]

proc mangleSym(n: NimNode): string =
  n.expectKind({nnkIdent, nnkSym})
  result = $n
  result = result.replace('`', '_')

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
        # else:
        #   echo "PRAGMA: ", treerepr p
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
    # echo treeRepr(i)
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
      result = "vec2f"
    elif t0 == "array" and t[1].rangeLen == 3 and $t[2] == "float32":
      result = "vec3f"
    elif t0 == "array" and t[1].rangeLen == 4 and $t[2] == "float32":
      result = "vec4f"
    elif t0 == "array":
      result = "array<" & getTypeName(ctx, t[2], skipVar) & "," & $(t[1].rangeLen) & ">"
    elif t0 == "distinct":
      result = getTypeName(ctx, t[1], skipVar)
    elif t0 == "seq":
      result = "array<"
      result &= getTypeName(ctx, t[1], skipVar)
      result &= ">"
    elif t0 == "Texture2D":
      result = "texture_2d<"
      result &= getTypeName(ctx, t[1], skipVar)
      result &= ">"
    elif t0 == "TextureStorage2D":
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
    of "VecBase": result = getTypeName(ctx, getType(t), skipVar)
    of "Texture2D": result = getTypeName(ctx, getType(t), skipVar)
    of "float32": result = "f32"
    of "int32": result = "i32"
    of "uint32": result = "u32"
    of "bool": result = "bool"
    of "Vec2", "vec2": result = "vec2f"
    of "Vec3", "vec3": result = "vec3f"
    of "Vec4", "vec4": result = "vec4f"
    of "Vec2i", "ivec2": result = "vec2i"
    of "Vec3i", "ivec3": result = "vec3i"
    of "Vec4i", "ivec4": result = "vec4i"
    of "Vec2u", "uvec2": result = "vec2u"
    of "Vec3u", "uvec3": result = "vec3u"
    of "Vec4u", "uvec4": result = "vec4u"
    of "Vec2b", "bvec2": result = "vec2<bool>"
    of "Vec3b", "bvec3": result = "vec3<bool>"
    of "Vec4b", "bvec4": result = "vec4<bool>"
    of "Mat3", "mat3": result = "mat3f"
    of "Mat4", "mat4": result = "mat4f"
    of "Sampler": result = "sampler"
    else:
      result = ctx.globalSyms.getOrDefault(t)
      if result == "":
        genType(ctx, t)
        result = ctx.globalSyms[t]
  of nnkVarTy:
    result = getTypeName(ctx, t[0])
    if not skipVar:
      result = "ptr<function, " & result & ">"
  else:
    echo "UNKNOWN TYPE: ", treeRepr(t)
    assert(false, "Unknown type")

proc skipPragma(n: NimNode): NimNode =
  result = n
  if n.kind == nnkPragmaExpr:
    result = n[0]

proc genLetSection(ctx: var CompilerContext, n: NimNode, r: var string) =
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
      # if s.kind == nnkIdent:
      r &= getTypeName(ctx, i[1])
      # else:
      #   r &= getTypeName(ctx, getType(s))

proc genStmt(ctx: var CompilerContext, n: NimNode, r: var string) =
  case n.kind
  of nnkStmtList:
    # Nested stmtlists don't need extra indent and semicolon
    gen(ctx, n, r)
  elif (n.kind != nnkDiscardStmt or n[0].kind != nnkEmpty) and n.kind != nnkConstSection:
    ctx.indent(r)
    gen(ctx, n, r)
    if r[^1] != '}':
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

proc genSystemRegularFunctionCall(ctx: var CompilerContext, n: NimNode, r: var string) =
  r &= $(n[0])
  r &= "("
  for i in 1 ..< n.len:
    if i != 1:
      r &= ","
      ctx.space(r)
    gen(ctx, n[i], r)
  r &= ")"

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
  of "<=", ">=", "<", ">", "+", "-", "*", "/", "*=", "/=", "+=", "-=", "==", "!=":
    if n.kind == nnkInfix:
      r &= "("
      gen(ctx, n[1], r)
      ctx.space(r)
      r &= pn
      ctx.space(r)
      gen(ctx, n[2], r)
      r &= ")"
    elif n.kind == nnkPrefix:
      r &= pn
      gen(ctx, n[1], r)
    else:
      assert(false)
  of "and":
    r &= "("
    gen(ctx, n[1], r)
    ctx.space(r)
    if getType(n).isIdent("bool"):
      r &= "&&"
    else:
      r &= "&"
    ctx.space(r)
    gen(ctx, n[2], r)
    r &= ")"
  of "or":
    r &= "("
    gen(ctx, n[1], r)
    ctx.space(r)
    if getType(n).isIdent("bool"):
      r &= "||"
    else:
      r &= "|"
    ctx.space(r)
    gen(ctx, n[2], r)
    r &= ")"
  of "xor":
    r &= "("
    gen(ctx, n[1], r)
    ctx.space(r)
    if getType(n).isIdent("bool"):
      r &= "^"
    else:
      r &= "^"
    ctx.space(r)
    gen(ctx, n[2], r)
    r &= ")"
  of "shl":
    r &= "("
    gen(ctx, n[1], r)
    ctx.space(r)
    r &= "<<"
    ctx.space(r)
    gen(ctx, n[2], r)
    r &= ")"
  of "shr":
    r &= "("
    gen(ctx, n[1], r)
    ctx.space(r)
    r &= ">>"
    ctx.space(r)
    gen(ctx, n[2], r)
    r &= ")"
  of "div":
    gen(ctx, n[1], r)
    ctx.space(r)
    r &= "/"
    ctx.space(r)
    gen(ctx, n[2], r)
  of "mod":
    gen(ctx, n[1], r)
    ctx.space(r)
    r &= "%"
    ctx.space(r)
    gen(ctx, n[2], r)
  of "not":
    if getType(n).isIdent("bool"):
      r &= "!("
    else:
      r &= "~("
    gen(ctx, n[1], r)
    r &= ")"
  of "max", "min", "abs":
    genSystemRegularFunctionCall(ctx, n, r)
  else:
    echo "UNKNOWN SYSTEM CALL: ", treeRepr(n)

proc skipAddr(n: NimNode): NimNode =
  result = n
  while result.kind in {nnkHiddenAddr}:
    result = result[^1]

proc genCall(ctx: var CompilerContext, n: NimNode, r: var string) =
  if n[0].isMagic() and $n[0] in [".", "nimsl_deriveVectorWithComponents"]:
    # This is a property
    gen(ctx, n[1], r)
    r &= "."
    r &= $(n[2])
  else:
    if n[0].kind == nnkSym and n[0].isSystemSym:
      genSystemCall(ctx, n, r)
    elif isMagic(n[0]):
      let name = $n[0]
      if name in ["x", "y", "z", "w", "r", "g", "b", "a"]:
        gen(ctx, skipAddr(n[1]), r)
        r &= "."
        r &= name
      # elif name == "fract":
      #   r &= "modf("
      #   gen(ctx, n[1], r)
      #   r &= ").fract"
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

proc genInfixCall(ctx: var CompilerContext, n: NimNode, r: var string) =
  # echo "INFIX: ", repr n
  if isMagic(n[0]):
    r &= "("
    gen(ctx, n[1], r)
    r &= $(n[0])
    gen(ctx, n[2], r)
    r &= ")"
  else:
    genCall(ctx, n, r)

proc genPrefixCall(ctx: var CompilerContext, n: NimNode, r: var string) =
  # echo "PREFIX: ", repr n
  if isMagic(n[0]):
    r &= $(n[0])
    gen(ctx, n[1], r)
  else:
    genCall(ctx, n, r)

proc genAsgn(ctx: var CompilerContext, n: NimNode, r: var string) =
  gen(ctx, n[0], r)
  ctx.space(r)
  r &= "="
  ctx.space(r)
  gen(ctx, n[1], r)

proc genReturnStmt(ctx: var CompilerContext, n: NimNode, r: var string) =
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
  of "newVec2", "vec2": result = "vec2f"
  of "newVec3", "vec3": result = "vec3f"
  of "newVec4", "vec4": result = "vec4f"
  else: result = pn

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

proc genGlobalVar(ctx: var CompilerContext, n, idDefs: NimNode) =
  var r = ""
  var name = idDefs[0]
  let typ = idDefs[^2]
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

proc genSym(ctx: var CompilerContext, n: NimNode, r: var string) =
  let i = getImpl(n)
  case i.kind
  of nnkProcDef:
    if isMagic(i):
      r &= genGLSLBuiltinSym(n)
    else:
      # echo "PROCDEF ", n
      var s = ctx.globalSyms.getOrDefault(n)
      if s == "":
        s = ctx.globalSymName(n)
        # s = $n & $ctx.globalIdCounter
        # inc ctx.globalIdCounter
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

type
  ProcDefFlag = enum
    forceVertex
    forceFragment
    forceCompute

proc genProcDef*(ctx: var CompilerContext, n: NimNode, flags: set[ProcDefFlag] = {}) =
  # echo "PROCDEF: ", treeRepr n
  resetPropertyInScope(ctx.procNode, n)
  resetPropertyInScope(ctx.indent)
  resetPropertyInScope(ctx.localSyms)

  var retType = "void"
  if n.params[0].kind != nnkEmpty:
    retType = getTypeName(ctx, n.params[0])

  var r: string
  if forceCompute in flags: r &= "@compute "
  if forceVertex in flags: r &= "@vertex "
  if forceFragment in flags: r &= "@fragment "

  genPragmas(ctx, n.pragma, r)
  ctx.nl(r)
  r &= "fn "
  var s = ctx.globalSyms.getOrDefault(n[0])
  if s == "":
    s = $(n[0])
    ctx.globalSyms[n[0]] = s
  r &= s
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

  let hasResult = n.params[0].kind != nnkEmpty

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
    if hasResult: # and not main:
      ctx.indent(r)
      r &= "return result;"
      ctx.nl(r)
  dec ctx.indent
  r &= "}"
  ctx.nl(r)
  ctx.nl(r)
  ctx.globalDefs.add(r)

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
  r &= "(var "
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

proc genConv(ctx: var CompilerContext, n: NimNode, r: var string) =
  # echo "CONV: ",  treeRepr(n)
  if n[0].kind != nnkEmpty:
    genConvWithT(ctx, n[1], n[0], r)
  elif n.kind == nnkHiddenStdConv:
    let t = getTypeInst(n)
    genConvWithT(ctx, n[1], t, r)
  else:
    gen(ctx, n[1], r)

proc skipConv(n: NimNode): NimNode =
  result = n
  while result.kind == nnkHiddenStdConv:
    result = result[^1]

proc genBracketExpr(ctx: var CompilerContext, n: NimNode, r: var string) =
  # let indexVal = n[1].skipConv.intVal
  gen(ctx, n[0], r)
  r &= "["
  gen(ctx, n[1], r)
  r &= "]"

proc genDotExpr(ctx: var CompilerContext, n: NimNode, r: var string) =
  # let indexVal = n[1].skipConv.intVal
  gen(ctx, n[0], r)
  r &= "."
  gen(ctx, n[1], r)

proc genIfStmt(ctx: var CompilerContext, n: NimNode, r: var string) =
  var first = true
  for c in n:
    if c.kind == nnkElifBranch:
      if first:
        r &= "if "
        first = false
      else:
        ctx.nl(r)
        ctx.indent(r)
        r &= "else if "
      gen(ctx, c[0], r)
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

proc genIntLit(ctx: var CompilerContext, n: NimNode, r: var string) =
  let t = n.getTypeInst()
  r &= $(n.intVal)
  if t.isIdent("uint32"):
    r &= "u"

proc genHiddenDeref(ctx: var CompilerContext, n: NimNode, r: var string) =
  if n[^1].kind in {nnkIdent, nnkSym}:
    r &= "(*"
    gen(ctx, n[0], r)
    r &= ")"
  else:
    gen(ctx, n[0], r)

proc genHiddenAddr(ctx: var CompilerContext, n: NimNode, r: var string) =
  r &= "(&"
  gen(ctx, n[0], r)
  r &= ")"

proc gen(ctx: var CompilerContext, n: NimNode, r: var string) =
  case n.kind:
  of nnkLetSection, nnkVarSection: genLetSection(ctx, n, r)
  of nnkStmtList: genStmtList(ctx, n, r)
  of nnkStmtListExpr: genStmtListExpr(ctx, n, r)
  of nnkCall: genCall(ctx, n, r)
  of nnkObjConstr: genObjConstr(ctx, n, r)
  of nnkBracket: genBracket(ctx, n, r)
  of nnkInfix: genInfixCall(ctx, n, r)
  of nnkPrefix: genPrefixCall(ctx, n, r)
  of nnkFloatLit, nnkFloat64Lit, nnkFloat32Lit: r &= $(n.floatVal)
  of nnkIntLit, nnkInt32Lit: genIntLit(ctx, n, r)
  of nnkUInt32Lit: r &= $(n.intVal) & "u"
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
  var c: CompilerContext
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
  var c {.compileTime.} = CompilerContext(pretty: not defined(release), localMangling: false)

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
