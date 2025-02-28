import std/[macros]

type
  Context = object
    tmpId: int

  Lowering = tuple
    stmt, res: NimNode

proc lowerExpr(ctx: var Context, n: NimNode): Lowering
proc lowerStmt(ctx: var Context, n: NimNode): NimNode

proc addOutVar(ctx: var Context, stmtList, typ: NimNode): NimNode =
  echo "OUTVAR: ", treeRepr(typ)
  result = genSym(nskVar, "tmp" & $ctx.tmpId)
  # result = ident("tmp" & $ctx.tmpId)
  inc ctx.tmpId
  stmtList.add newTree(nnkVarSection, newIdentDefs(result, typ))

proc asgn(dst, src: NimNode): NimNode =
  result = newTree(nnkAsgn, dst, src)
  # echo "ASGN: ", treeRepr(result)

proc exprToStmtListWithAsgn(ctx: var Context, n, outVar: NimNode): NimNode =
  let (st, res) = lowerExpr(ctx, n)
  result = newTree(nnkStmtList)
  if st != nil: result.add(st)
  result.add(asgn(outVar, res))

proc lowerIfExpr(ctx: var Context, n: NimNode): Lowering =
  var lowerings: seq[Lowering]

  let s = newTree(nnkStmtList)
  let outVar = addOutVar(ctx, s, n.getTypeInst())

  var p = s

  var haveElse = false
  for c in n:
    case c.kind
    of nnkElifExpr, nnkElifBranch:
      lowerings.add(lowerExpr(ctx, c[0]))
    else:
      c.expectKind({nnkElseExpr, nnkElse})
      haveElse = true

  var i = 0
  for (st, res) in lowerings:
    let elifBr = newTree(nnkElifBranch, res)
    if st == nil:
      if p.kind != nnkIfStmt:
        let i = newTree(nnkIfStmt)
        p.add(i)
        p = i
      p.add(elifBr)
    else:
      echo "BR: ", repr n[i]
      echo "ST: ", treeRepr(st)
      assert(false)

    elifBr.add(exprToStmtListWithAsgn(ctx, n[i][^1], outVar))
    inc i

  if haveElse:
    p.expectKind(nnkIfStmt)
    let elseBr = newTree(nnkElse, exprToStmtListWithAsgn(ctx, n[^1][^1], outVar))
    p.add(elseBr)

  (s, outVar)

proc lowerCaseExpr(ctx: var Context, n: NimNode): Lowering =
  let s = newTree(nnkStmtList)
  let outVar = addOutVar(ctx, s, n.getTypeInst())
  let caseS = newTree(nnkCaseStmt, n[0])
  s.add(caseS)

  var haveElse = false
  for i in 1 ..< n.len:
    let c = n[i]
    case c.kind
    of nnkOfBranch:
      let br = newTree(nnkOfBranch)
      for i in 0 ..< c.len - 1:
        br.add(c[i])
      br.add exprToStmtListWithAsgn(ctx, c[^1], outVar)
      caseS.add(br)
    of nnkElse:
      let br = newTree(nnkElse)
      br.add exprToStmtListWithAsgn(ctx, c[^1], outVar)
      caseS.add(br)
    else:
      assert(false)
  (s, outVar)

proc lowerStmtListExpr(ctx: var Context, n: NimNode): Lowering =
  var s = newTree(nnkStmtList)
  for i in 0 ..< n.len - 1:
    let ss = lowerStmt(ctx, n[i])
    if ss.kind != nnkEmpty:
      s.add(ss)

  let (ss, res) = lowerExpr(ctx, n[^1])
  if ss != nil:
    s.add(ss)
  if s.len == 0: s = nil
  (s, res)

proc lowerLastExprInExpr(ctx: var Context, n: NimNode): Lowering =
  var s = newTree(n.kind)
  for i in 0 ..< n.len - 1:
    s.add(n[i])
  let (ss, res) = lowerExpr(ctx, n[^1])
  s.add(res)
  (ss, s)

proc lowerExpr(ctx: var Context, n: NimNode): Lowering =
  case n.kind
  of nnkIfExpr:
    lowerIfExpr(ctx, n)
  of nnkCaseStmt:
    lowerCaseExpr(ctx, n)
  of nnkStmtListExpr:
    lowerStmtListExpr(ctx, n)
  of nnkConv, nnkHiddenStdConv:
    lowerLastExprInExpr(ctx, n)
  else:
    (nil, n)

proc lowerAsgn(ctx: var Context, n: NimNode): NimNode =
  let (s, res) = lowerExpr(ctx, n[1])
  if s == nil:
    n
  else:
    newTree(nnkStmtList, s, newTree(nnkAsgn, n[0], res))
  # echo "ASGN: ", treeRepr(result)

proc lowerStmtList(ctx: var Context, n: NimNode): NimNode =
  result = newTree(nnkStmtList)
  for i in 0 ..< n.len:
    result.add lowerStmt(ctx, n[i])

proc lowerReturnStmt(ctx: var Context, n: NimNode): NimNode =
  if n[0].kind != nnkEmpty:
    n[0].expectKind(nnkAsgn)
    let (s, r) = lowerExpr(ctx, n[0][1])
    n[0][1] = r
    if s != nil:
      echo "SSSSS: ", treeRepr s
      newTree(nnkStmtList, s, n)
    else:
      n
  else:
    n

proc lowerLetSection(ctx: var Context, n: NimNode): NimNode =
  result = newTree(nnkStmtList)
  for c in n:
    let (ss, res) = lowerExpr(ctx, c[^1])
    if ss == nil:
      result.add(newTree(n.kind, c))
    else:
      result.add(ss)
      result.add(newTree(n.kind, newIdentDefs(c[0], c[1], res)))
  # echo "LET: ", repr result

  # let (s, res) = lowerExpr(ctx, n[1])
  # result = newTree(nnkAsgn, n[0], res)
  # if s != nil:
  #   result = newTree(nnkStmtList, s, result)

proc lowerLastStmtInStmt(ctx: var Context, n: NimNode): NimNode =
  result = newTree(n.kind)
  for i in 0 ..< n.len - 1:
    result.add(n[i])
  result.add(lowerStmt(ctx, n[^1]))

proc lowerIfStmt(ctx: var Context, n: NimNode): NimNode =
  result = newTree(n.kind)
  for i in 0 ..< n.len:
    result.add(lowerLastStmtInStmt(ctx, n[i]))

proc lowerStmt(ctx: var Context, n: NimNode): NimNode =
  result = case n.kind
  of nnkIfStmt:
    lowerIfStmt(ctx, n)
  of nnkAsgn:
    lowerAsgn(ctx, n)
  of nnkStmtList:
    lowerStmtList(ctx, n)
  of nnkReturnStmt:
    lowerReturnStmt(ctx, n)
  of nnkLetSection, nnkVarSection:
    lowerLetSection(ctx, n)
  of nnkForStmt, nnkWhileStmt:
    lowerLastStmtInStmt(ctx, n)
  else:
    n

proc lowerExprs*(n: NimNode): NimNode =
  var ctx: Context
  # echo "EXPRS: ", treerepr n
  lowerStmt(ctx, n)
  # case n.kind
  # of nnkStmtList:
  #   lowerStmtList(ctx, n)
  # of nnkReturnStmt:
  #   lowerReturnStmt(ctx, n)
  # else:
  #   n
