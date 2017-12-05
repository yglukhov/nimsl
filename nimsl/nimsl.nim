import macros, math, strutils
import private.glsl_codegen

proc getShaderCode(s: NimNode, k: ShaderKind, mainProcName: string): string =
    var ctx = newCtx()
    ctx.mainProcName = mainProcName
    ctx.shaderKind = k
    genProcDef(ctx, getImpl(s.symbol), true)
    result = ""
    for i in ctx.globalDefs:
        result &= i

macro getGLSLFragmentShader*(s: typed, mainProcName: string = "main"): string =
    result = newLit(getShaderCode(s, skFragmentShader, mainProcName.strVal))

macro getGLSLVertexShader*(s: typed): string =
    result = newLit(getShaderCode(s, skVertexShader, "main"))

type
    vecBase[I: static[int], T] = distinct array[I, T]
    matBase[I: static[int], T] = distinct array[I, T]

    mat3* = matBase[9, float32]
    mat4* = matBase[16, float32]

    vec2* = vecBase[2, float32]
    vec3* = vecBase[3, float32]
    vec4* = vecBase[4, float32]

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

template defineBorrowsForVec(i: int, t: typedesc) =
    template `[]`*(v: vecBase[i, t], index: int): auto =
        array[i, t](v)[index]
    template `[]=`*(v: var vecBase[i, t], index: int, val: t) =
        array[i, t](v)[index] = val

template defineBorrowsForMat(i: int, t: typedesc) =
    template `[]`*(v: matBase[i, t], index: int): auto =
        array[i, t](v)[index]
    template `[]=`*(v: var matBase[i, t], index: int, val: t) =
        array[i, t](v)[index] = val

template defineBorrowsForVecs(t: typedesc) =
    defineBorrowsForVec(2, t)
    defineBorrowsForVec(3, t)
    defineBorrowsForVec(4, t)

defineBorrowsForVecs(float32)
defineBorrowsForVecs(int32)
defineBorrowsForVecs(uint32)
#defineBorrowsForVecs(float64)
defineBorrowsForVecs(bool)

defineBorrowsForMat(9, float32)
defineBorrowsForMat(16, float32)

proc newVec2*(x, y: float32): vec2 = glslbuiltin(); return [x, y].vec2
proc newVec3*(x, y, z: float32): vec3 = glslbuiltin(); return [x, y, z].vec3
proc newVec4*(x, y, z, w: float32): vec4 = glslbuiltin(); return [x, y, z, w].vec4

proc newVec2*(x: float32): vec2 = glslbuiltin(); return [x, x].vec2
proc newVec3*(x: float32): vec3 = glslbuiltin(); return [x, x, x].vec3
proc newVec4*(x: float32): vec4 = glslbuiltin(); return [x, x, x, x].vec4

proc newVec3*(x, y: float32): vec3 = glslbuiltin(); return [x, y, y].vec3

proc newVec3*(v: vec2, z: float32): vec3 = glslbuiltin(); return [v[0], v[1], z].vec3

proc newVec4*(x, y: float32): vec4 = glslbuiltin(); return [x, y, y, y].vec4

proc newVec4*(x, y, z: float32): vec4 = glslbuiltin(); return [x, y, z, z].vec4
proc newVec4*(v: vec2, z, w: float32): vec4 = glslbuiltin(); return [v[0], v[1], z, w].vec4
proc newVec4*(u, v: vec2): vec4 = glslbuiltin(); return [u[0], u[1], v[0], v[1]].vec4

proc newIdentityMat4*(): mat4 =
    result[0] = 1
    result[5] = 1
    result[10] = 1
    result[15] = 1

# vec2
proc `+`*(v: vec2, s: float32): vec2 = glslbuiltin(); return [v[0] + s, v[1] + s].vec2
proc `-`*(v: vec2, s: float32): vec2 = glslbuiltin(); return [v[0] - s, v[1] - s].vec2
proc `*`*(v: vec2, s: float32): vec2 = glslbuiltin(); return [v[0] * s, v[1] * s].vec2
proc `/`*(v: vec2, s: float32): vec2 = glslbuiltin(); return [v[0] / s, v[1] / s].vec2

proc `/`*(s: float32, v: vec2): vec2 = glslbuiltin(); return [s / v[0], s / v[1]].vec2


proc `+`*(u, v: vec2): vec2 = glslbuiltin(); return [u[0] + v[0], u[1] + v[1]].vec2
proc `-`*(u, v: vec2): vec2 = glslbuiltin(); return [u[0] - v[0], u[1] - v[1]].vec2
proc `*`*(u, v: vec2): vec2 = glslbuiltin(); return [u[0] * v[0], u[1] * v[1]].vec2
proc `/`*(u, v: vec2): vec2 = glslbuiltin(); return [u[0] / v[0], u[1] / v[1]].vec2

proc `-`*(v: vec2): vec2 = glslbuiltin(); return [-v[0], -v[1]].vec2

# vec3
proc `+`*(v: vec3, s: float32): vec3 = glslbuiltin(); return [v[0] + s, v[1] + s, v[2] + s].vec3
proc `-`*(v: vec3, s: float32): vec3 = glslbuiltin(); return [v[0] - s, v[1] - s, v[2] - s].vec3
proc `*`*(v: vec3, s: float32): vec3 = glslbuiltin(); return [v[0] * s, v[1] * s, v[2] * s].vec3
proc `/`*(v: vec3, s: float32): vec3 = glslbuiltin(); return [v[0] / s, v[1] / s, v[2] / s].vec3

proc `/`*(s: float32, v: vec3): vec3 = glslbuiltin(); return [s / v[0], s / v[1], s / v[2]].vec3


proc `+`*(u, v: vec3): vec3 = glslbuiltin(); return [u[0] + v[0], u[1] + v[1], u[2] + v[2]].vec3
proc `-`*(u, v: vec3): vec3 = glslbuiltin(); return [u[0] - v[0], u[1] - v[1], u[2] - v[2]].vec3
proc `*`*(u, v: vec3): vec3 = glslbuiltin(); return [u[0] * v[0], u[1] * v[1], u[2] * v[2]].vec3
proc `/`*(u, v: vec3): vec3 = glslbuiltin(); return [u[0] / v[0], u[1] / v[1], u[2] / v[2]].vec3

proc `-`*(v: vec3): vec3 = glslbuiltin(); return [-v[0], -v[1], -v[2]].vec3

# vec4
proc `+`*(v: vec4, s: float32): vec4 = glslbuiltin(); return [v[0] + s, v[1] + s, v[2] + s, v[3] + s].vec4
proc `-`*(v: vec4, s: float32): vec4 = glslbuiltin(); return [v[0] - s, v[1] - s, v[2] - s, v[3] - s].vec4
proc `*`*(v: vec4, s: float32): vec4 = glslbuiltin(); return [v[0] * s, v[1] * s, v[2] * s, v[3] * s].vec4
proc `/`*(v: vec4, s: float32): vec4 = glslbuiltin(); return [v[0] / s, v[1] / s, v[2] / s, v[3] / s].vec4

proc `/`*(s: float32, v: vec4): vec4 = glslbuiltin(); return [s / v[0], s / v[1], s / v[2], s / v[3]].vec4


proc `+`*(u, v: vec4): vec4 = glslbuiltin(); return [u[0] + v[0], u[1] + v[1], u[2] + v[2], u[3] + v[3]].vec4
proc `-`*(u, v: vec4): vec4 = glslbuiltin(); return [u[0] - v[0], u[1] - v[1], u[2] - v[2], u[3] - v[3]].vec4
proc `*`*(u, v: vec4): vec4 = glslbuiltin(); return [u[0] * v[0], u[1] * v[1], u[2] * v[2], u[3] * v[3]].vec4
proc `/`*(u, v: vec4): vec4 = glslbuiltin(); return [u[0] / v[0], u[1] / v[1], u[2] / v[2], u[3] / v[3]].vec4

proc `-`*(v: vec4): vec4 = glslbuiltin(); return [-v[0], -v[1], -v[2], -v[3]].vec4


proc sin*(v: float32 | float64): auto = glslbuiltin(); return math.sin(v)
proc abs*(v: float32 | float64): auto = glslbuiltin(); return (if v < 0: -v else: v)
proc abs*(v: vec2): vec2 = glslbuiltin(); return [abs(v[0]), abs(v[1])].vec2
proc max*(u, v: float32): float32 = glslbuiltin(); return (if u > v: u else: v)
proc max*(v: vec2, s: float32): vec2 = glslbuiltin(); return [max(v[0], s), max(v[1], s)].vec2
proc min*(u, v: float32): float32 = glslbuiltin(); return (if u > v: v else: u)
proc min*(v: vec2, s: float32): vec2 = glslbuiltin(); return [min(v[0], s), min(v[1], s)].vec2
proc length*(v: vec2): float32 = glslbuiltin(); return sqrt(v[0] * v[0] + v[1] * v[1])

proc smoothstep*(edge0, edge1, x: float32): float32 =
    glslbuiltin()
    let c = clamp((x - edge0)/(edge1 - edge0), 0.0, 1.0)
    result = c*c*(3 - 2*c)

proc fwidth*(v: float32): float32 = glslbuiltin(); return 0
proc mix*(x, y, a: vec4): vec4 =
    glslbuiltin();
    assert(false, "Not implemented")
proc mix*(x, y: vec4, a: float32): vec4 = glslbuiltin(); return x * (1.0 - a) + y * (a)

proc dot*[I: static[int], T](v1, v2: vecBase[I, T]): T =
    glslbuiltin()
    for i in 0 ..< I: result += v1[i] * v2[i]

proc cross*[T](a, b: vecBase[3, T]): vecBase[3, T] =
    glslbuiltin()
    return vecBase[3, T]([a[1]*b[2] - a[2]*b[1], a[2]*b[0] - a[0]*b[2], a[0]*b[1] - a[1]*b[0]])

proc length*(v: vec3): float32 = glslbuiltin(); return sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2])

template x*[I: static[int], T](v: vecBase[I, T]): T = v[0]
template y*[I: static[int], T](v: vecBase[I, T]): T = v[1]
template z*[I: static[int], T](v: vecBase[I, T]): T = v[2]
template w*[I: static[int], T](v: vecBase[I, T]): T = v[3]

template `x=`*[I: static[int], T](v: var vecBase[I, T], val: T) = v[0] = val
template `y=`*[I: static[int], T](v: var vecBase[I, T], val: T) = v[1] = val
template `z=`*[I: static[int], T](v: var vecBase[I, T], val: T) = v[2] = val
template `w=`*[I: static[int], T](v: var vecBase[I, T], val: T) = v[3] = val

proc `*`*(m: mat4, v: vec4): vec4 =
    glslbuiltin()
    let (x, y, z, w) = (v[0], v[1], v[2], v[3])
    result[0] = m[0] * x + m[4] * y + m[8] * z + m[12] * w
    result[1] = m[1] * x + m[5] * y + m[9] * z + m[13] * w
    result[2] = m[2] * x + m[6] * y + m[10] * z + m[14] * w
    result[3] = m[3] * x + m[7] * y + m[11] * z + m[15] * w

proc validateAttrs(s: string, a: set[char]) {.compileTime.} =
    for c in s:
        assert(c in a, "Invalid vector component: " & $c)

proc nimsl_deriveVectorWithComponents[T](v: vecBase[2, T], f: static[string]): vecBase[f.len, T] =
    glslinfix()
    static: validateAttrs(f, {'x', 'r', 'y', 'g'})
    for i, c in f:
        case c
            of 'x', 'r': result[i] = v.x
            of 'y', 'g': result[i] = v.y
            else: discard # Should be prevented by validateAttrs

proc nimsl_deriveVectorWithComponents[T](v: vecBase[3, T], f: static[string]): vecBase[f.len, T] =
    glslinfix()
    static: validateAttrs(f, {'x', 'r', 'y', 'g', 'z', 'b'})
    for i, c in f:
        case c
            of 'x', 'r': result[i] = v.x
            of 'y', 'g': result[i] = v.y
            of 'z', 'b': result[i] = v.z
            else: discard # Should be prevented by validateAttrs

proc nimsl_deriveVectorWithComponents[T](v: vecBase[4, T], f: static[string]): vecBase[f.len, T] =
    glslinfix()
    static: validateAttrs(f, {'x', 'r', 'y', 'g', 'z', 'b', 'w', 'a'})
    for i, c in f:
        case c
            of 'x', 'r': result[i] = v.x
            of 'y', 'g': result[i] = v.y
            of 'z', 'b': result[i] = v.z
            of 'w', 'a': result[i] = v.w
            else: discard # Should be prevented by validateAttrs

when defined(nimNewDot):
    template `.`*[I, T](v: vecBase[I, T], f: untyped): auto =
        nimsl_deriveVectorWithComponents(v, astToStr(f))
else:
    proc `.`*[I, T](v: vecBase[I, T], f: static[string]): vecBase[f.len, T] {.inline.} =
        glslinfix()
        result = nimsl_deriveVectorWithComponents(v, f)
