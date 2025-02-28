import std/[macros, strutils]

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
