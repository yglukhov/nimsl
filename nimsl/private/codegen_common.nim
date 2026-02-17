import std/[macros, strutils, sequtils, tables, hashes]

type
  CompilerContextBase* = object of RootObj
    procNode*: NimNode
    globalDefs*: seq[string]
    globalSyms*: Table[NimNode, string]
    localSyms*: Table[NimNode, string]
    manglingContext*: ManglingContext
    indent*: int
    pretty*: bool
    localMangling*: bool

  ManglingContext = object
    globalSyms: Table[NimNode, string]
    globalSymCounter: int

proc hash(n: NimNode): Hash = hash($n)

var globalManglingContext {.compileTime.}: ManglingContext

proc indent*(c: CompilerContextBase, r: var string) =
  if c.pretty:
    for i in 0 ..< c.indent:
      r &= "  "

proc nl*(c: CompilerContextBase, r: var string) =
  if c.pretty:
    r &= "\n"

proc space*(c: CompilerContextBase, r: var string) =
  if c.pretty:
    r &= " "

proc isIdent*(n: NimNode, s: string): bool =
  n.kind in {nnkIdent, nnkSym} and cmpIgnoreStyle($n, s) == 0

proc hasMagicPragma(n: NimNode): bool =
  for p in n.pragma:
    if isIdent(p, "nimslmagic"):
      return true
    elif p.kind == nnkCall and p.len == 1 and isIdent(p[0], "nimslmagic"):
      return true

proc isMagic*(n: NimNode): bool =
  case n.kind
  of nnkProcDef:
    result = hasMagicPragma(n)
  of nnkSym:
    result = isMagic(getImpl(n))
  else: discard

proc isRange*(n: NimNode, rangeLen: int = -1): bool =
  if n.kind == nnkBracketExpr and $(n[0]) == "range":
    if rangeLen == -1:
      result = true
    elif n[2].intVal - n[1].intVal + 1 == rangeLen:
      result = true

proc isSystemSym*(s: NimNode): bool =
  let ln = s.getImpl().lineinfo
  if ln.find("/system.nim(") != -1 or
     ln.find("\\system.nim(") != -1 or
     ln.find("\\system\\") != -1 or
     ln.find("/system/") != -1 or
      ln.find("/pure/math.nim(") != -1 or
     ln.find("\\pure\\math.nim(") != -1:
    result = true

proc genIdent*(a: int): string =
  const
    firstChars = {'a' .. 'z' }.toSeq() & { 'A' .. 'Z' }.toSeq() & '_'
    restChars = firstChars & { '0' .. '9' }.toSeq()
    firstBase = firstChars.len
    restBase = restChars.len

  # Determine the number of characters needed
  var x = a
  var length = 1
  var maxCount = firstBase
  var mult = 1

  while x >= maxCount:
    x -= maxCount
    mult *= restBase
    maxCount = mult * firstBase
    inc(length)

  # Now decode the number using the correct base and length
  # First character (most significant)
  result.add firstChars[x div mult]

  # Remaining characters (if any)
  var rem = x mod mult
  for i in 1 ..< length:
    mult = mult div restBase
    result.add restChars[rem div mult]
    rem = rem mod mult

proc globalSymName(c: var ManglingContext, sym: NimNode, pretty: bool): string =
  result = c.globalSyms.getOrDefault(sym)
  if result == "":
    if pretty:
      result = $sym & $c.globalSymCounter
    else:
      result = genIdent(c.globalSymCounter)
    c.globalSyms[sym] = result
    inc c.globalSymCounter

proc globalSymName*(c: var CompilerContextBase, sym: NimNode): string =
  if c.localMangling:
    globalSymName(c.manglingContext, sym, c.pretty)
  else:
    globalSymName(globalManglingContext, sym, c.pretty)

template resetPropertyInScope*(property: untyped, value: untyped) =
  let tmpProp = property
  property = value
  defer: property = tmpProp

template resetPropertyInScope*(property: untyped) =
  var tmpProp: typeof(property)
  swap(tmpProp, property)
  defer: swap(property, tmpProp)

proc mangleSym*(n: NimNode): string =
  n.expectKind({nnkIdent, nnkSym})
  result = $n
  result = result.replace('`', '_')

proc skipPragma*(n: NimNode): NimNode =
  result = n
  if n.kind == nnkPragmaExpr:
    result = n[0]

when isMainModule:
  static:
    doAssert(genIdent(0) == "a")
    doAssert(genIdent(10100) == "bRE")

  doAssert(genIdent(0) == "a")
  doAssert(genIdent(10100) == "bRE")
