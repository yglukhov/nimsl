import macros, tables

type
  VarValue[T] = ref object of RootRef
    v: T

var varTab: TableRef[string, RootRef]

proc getVarAddr[T](name: string): ptr T =
    if varTab.isNil:
        varTab = newTable[string, RootRef]()
    var vv = VarValue[T](varTab.getOrDefault(name))
    if vv.isNil:
        vv = VarValue[T]()
        varTab[name] = vv
    addr vv.v

proc setVar[T](name: string, v: T) =
    if varTab.isNil:
        varTab = newTable[string, RootRef]()
    var vv = VarValue[T](varTab.getOrDefault(name))
    if vv.isNil:
        vv = VarValue[T](v: v)
        varTab[name] = vv
    else:
        vv.v = v

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
