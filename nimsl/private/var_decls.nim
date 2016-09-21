import macros, tables, variant

var varTab: TableRef[TypeID, RootRef]

proc varNotFound(name: string) =
    discard
    #raise newException(Exception, "Var " & name & " not set")

proc getVarAddr[T](name: string): ptr T =
    if varTab.isNil:
        varNotFound(name)
        varTab = newTable[TypeID, RootRef]()
    const tid = getTypeId(T)
    var t = varTab.getOrDefault(tid)
    var tt : TableRef[string, T]
    if t.isNil:
        varNotFound(name)
        tt = newTable[string, T]()
        varTab[tid] = cast[RootRef](tt)
    else:
        tt = cast[TableRef[string, T]](t)
    if name notin tt:
        varNotFound(name)
        var v: T
        tt[name] = v
    result = addr tt[name]

proc setVar[T](name: string, v: T) =
    if varTab.isNil:
        varTab = newTable[TypeID, RootRef]()
    const tid = getTypeId(T)
    var t = varTab.getOrDefault(tid)
    var tt : TableRef[string, T]
    if t.isNil:
        tt = newTable[string, T]()
        varTab[tid] = cast[RootRef](tt)
    else:
        tt = cast[TableRef[string, T]](t)
    tt[name] = v

proc nimslUniformAddr[T](name: string): ptr T {.inline.} = getVarAddr[T](name)
proc nimslVaryingAddr[T](name: string): ptr T {.inline.} = getVarAddr[T](name)
proc nimslAttributeAddr[T](name: string): ptr T {.inline.} = getVarAddr[T](name)

template uniformDecl(sym: untyped, typ: typedesc): untyped =
    let `sym nimslu` {.inject.} = nimslUniformAddr[typ](astToStr(sym))
    template sym: typ = `sym nimslu`[]

template varyingDecl(sym: untyped, typ: typedesc): untyped =
    let `sym nimslv` {.inject.} = nimslVaryingAddr[typ](astToStr(sym))
    template sym: typ = `sym nimslv`[]

template attributeDecl(sym: untyped, typ: typedesc): untyped =
    let `sym nimsla` {.inject.} = nimslVaryingAddr[typ](astToStr(sym))
    template sym: typ = `sym nimsla`[]

proc createVarDecls(n: NimNode, declTemplate: NimNode): NimNode =
    result = newStmtList()
    for section in n:
        section.expectKind({nnkLetSection, nnkVarSection})
        for identDefs in section:
            let typ = identDefs[^2]
            for i in 0 ..< identDefs.len - 2:
                result.add(newCall(declTemplate, identDefs[i], typ))

macro uniforms*(n: untyped): untyped = createVarDecls(n, bindSym "uniformDecl")
macro varyings*(n: untyped): untyped = createVarDecls(n, bindSym "varyingDecl")
macro attributes*(n: untyped): untyped = createVarDecls(n, bindSym "attributeDecl")

when isMainModule:
    setVar("a", 5)
    setVar("b", 10)
    setVar("v", 0)
    uniforms:
        let a, b: int
    varyings:
        var v: int
    doAssert(a == 5)
    doAssert(b == 10)
    v = 15
    doAssert(getVarAddr[int]("v")[] == 15)
