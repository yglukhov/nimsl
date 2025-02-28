
{.push stackTrace: off.}
proc glslbuiltin*() {.inline.} = discard
proc glslinfix*() {.inline.} = discard
{.pop.}

type
  BuiltinKind* = enum
    globalInvocationId
    localInvocationIndex
    vertexIndex
    instanceIndex
    position

template storage* {.pragma.}
template uniform* {.pragma.}
template readWrite* {.pragma.}
template read* {.pragma.}
template write* {.pragma.}
template private* {.pragma.}
template workgroup* {.pragma.}


template group*(a: int) {.pragma.}
template binding*(a: int) {.pragma.}
template builtin*(a: BuiltinKind) {.pragma.}
template workgroupSize*(x, y, z: int) {.pragma.}
template location*(x: int) {.pragma.}



# Private, don't use

template nimslmagic* {.pragma.}

const wgslOutputPath* {.strdefine: "nimslWgslOutputPath".} = ""
const glslOutputPath* {.strdefine: "nimslGlslOutputPath".} = ""
