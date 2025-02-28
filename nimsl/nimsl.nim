import std/[macros, math]
import private/common
from private/glsl_codegen import nil
import private/wgsl_codegen as wgsl


when wgslOutputPath != "" or glslOutputPath != "":
  macro vertex*(u: typed): untyped =
    when wgslOutputPath != "":
      wgsl.singleVertexShader(u)

  macro fragment*(u: typed): untyped =
    when wgslOutputPath != "":
      wgsl.singleFragmentShader(u)

  macro compute*(u: typed): untyped =
    when wgslOutputPath != "":
      wgsl.singleComputeShader(u)

else:
  template compute* {.pragma.}
  template vertex* {.pragma.}
  template fragment* {.pragma.}

export common
export wgsl.wgslShader

type
  Texture2D*[T] = object
  TextureStorage2D*[T] = object
  Sampler* = object

proc getGLSLShaderCode(s: NimNode, k: glsl_codegen.ShaderKind, mainProcName: string): string =
  var ctx = glsl_codegen.newCtx()
  ctx.mainProcName = mainProcName
  ctx.shaderKind = k
  glsl_codegen.genProcDef(ctx, getImpl(s), true)
  result = ""
  for i in ctx.globalDefs:
    result &= i

macro getGLSLFragmentShader*(s: typed{nkSym}, mainProcName: string = "main"): string =
  result = newLit(getGLSLShaderCode(s, glsl_codegen.skFragmentShader, mainProcName.strVal))

macro getGLSLVertexShader*(s: typed{nkSym}): string =
  result = newLit(getGLSLShaderCode(s, glsl_codegen.skVertexShader, "main"))

type
  VecBase[I: static[int], T] = distinct array[I, T]
  MatBase[I: static[int], T] = distinct array[I, T]

  Mat3* = MatBase[9, float32]
  Mat4* = MatBase[16, float32]

  Vec2* = VecBase[2, float32]
  Vec3* = VecBase[3, float32]
  Vec4* = VecBase[4, float32]

  Vec2i* = VecBase[2, int32]
  Vec3i* = VecBase[3, int32]
  Vec4i* = VecBase[4, int32]

  Vec2u* = VecBase[2, uint32]
  Vec3u* = VecBase[3, uint32]
  Vec4u* = VecBase[4, uint32]

  Vec2d* = VecBase[2, float64]
  Vec3d* = VecBase[3, float64]
  Vec4d* = VecBase[4, float64]

  Vec2b* = VecBase[2, bool]
  Vec3b* = VecBase[3, bool]
  Vec4b* = VecBase[4, bool]

template defineBorrowsForVec(i: int, t: typedesc) =
  template `[]`*(v: VecBase[i, t], index: int): auto =
    array[i, t](v)[index]
  template `[]=`*(v: var VecBase[i, t], index: int, val: t) =
    array[i, t](v)[index] = val

template defineBorrowsForMat(i: int, t: typedesc) =
  template `[]`*(v: MatBase[i, t], index: int): auto =
    array[i, t](v)[index]
  template `[]=`*(v: var MatBase[i, t], index: int, val: t) =
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

template defineConstructors(suffix: untyped, elemType: untyped) =
  proc `vec2 suffix`*(x: elemType): `Vec2 suffix` {.nimslmagic, inline.} = [x, default(elemType)].`Vec2 suffix`
  proc `vec2 suffix`*(x, y: elemType): `Vec2 suffix` {.nimslmagic, inline.} = [x, y].`Vec2 suffix`

  proc `vec3 suffix`*(x: elemType): `Vec3 suffix` {.nimslmagic, inline.} = [x, default(elemType), default(elemType)].`Vec3 suffix`
  proc `vec3 suffix`*(x, y: elemType): `Vec3 suffix` {.nimslmagic, inline.} = [x, y, default(elemType)].`Vec3 suffix`
  proc `vec3 suffix`*(x, y, z: elemType): `Vec3 suffix` {.nimslmagic, inline.} = [x, y, z].`Vec3 suffix`

  proc `vec3 suffix`*(xy: `Vec2 suffix`): `Vec3 suffix` {.nimslmagic, inline.} = [xy[0], xy[1], default(elemType)].`Vec3 suffix`
  proc `vec3 suffix`*(xy: `Vec2 suffix`, z: elemType): `Vec3 suffix` {.nimslmagic, inline.} = [xy[0], xy[1], z].`Vec3 suffix`
  proc `vec3 suffix`*(x: elemType, yz: `Vec2 suffix`): `Vec3 suffix` {.nimslmagic, inline.} = [x, yz[0], yz[1]].`Vec3 suffix`

  proc `vec4 suffix`*(x: elemType): `Vec4 suffix` {.nimslmagic, inline.} = [x, default(elemType), default(elemType), default(elemType)].`Vec4 suffix`
  proc `vec4 suffix`*(x, y: elemType): `Vec4 suffix` {.nimslmagic, inline.} = [x, y, default(elemType), default(elemType)].`Vec4 suffix`
  proc `vec4 suffix`*(x, y, z: elemType): `Vec4 suffix` {.nimslmagic, inline.} = [x, y, z, default(elemType)].`Vec4 suffix`
  proc `vec4 suffix`*(x, y, z, w: elemType): `Vec4 suffix` {.nimslmagic, inline.} = [x, y, z, w].`Vec4 suffix`

  proc `vec4 suffix`*(xy: `Vec2 suffix`): `Vec4 suffix` {.nimslmagic, inline.} = [xy[0], xy[1], default(elemType), default(elemType)].`Vec4 suffix`
  proc `vec4 suffix`*(xy: `Vec2 suffix`, z: elemType): `Vec4 suffix` {.nimslmagic, inline.} = [xy[0], xy[1], z, default(elemType)].`Vec4 suffix`
  proc `vec4 suffix`*(xy: `Vec2 suffix`, z, w: elemType): `Vec4 suffix` {.nimslmagic, inline.} = [xy[0], xy[1], z, w].`Vec4 suffix`

  proc `vec4 suffix`*(xy, zw: `Vec2 suffix`): `Vec4 suffix` {.nimslmagic, inline.} = [xy[0], xy[1], zw[0], zw[1]].`Vec4 suffix`
  proc `vec4 suffix`*(xyz: `Vec3 suffix`): `Vec4 suffix` {.nimslmagic, inline.} = [xyz[0], xyz[1], xyz[2], default(elemType)].`Vec4 suffix`
  proc `vec4 suffix`*(xyz: `Vec3 suffix`, w: elemType): `Vec4 suffix` {.nimslmagic, inline.} = [xyz[0], xyz[1], xyz[2], w].`Vec4 suffix`
  

defineConstructors(u, uint32)
defineConstructors(i, int32)

proc `$`*[I, T](v: VecBase[I, T]): string =
  result = "("
  for i in 0 ..< I:
    if i != 0: result &= ", "
    result &= $v[i]
  result &= ")"


proc vec2*(x: float32): Vec2 {.nimslmagic, inline.} = [x, default(float32)].Vec2
proc vec2*(x, y: float32): Vec2 {.nimslmagic, inline.} = [x, y].Vec2

proc vec3*(x: float32): Vec3 {.nimslmagic, inline.} = [x, default(float32), default(float32)].Vec3
proc vec3*(x, y: float32): Vec3 {.nimslmagic, inline.} = [x, y, default(float32)].Vec3
proc vec3*(x, y, z: float32): Vec3 {.nimslmagic, inline.} = [x, y, z].Vec3

proc vec3*(xy: Vec2): Vec3 {.nimslmagic, inline.} = [xy[0], xy[1], default(float32)].Vec3
proc vec3*(xy: Vec2, z: float32): Vec3 {.nimslmagic, inline.} = [xy[0], xy[1], z].Vec3
proc vec3*(x: float32, yz: Vec2): Vec3 {.nimslmagic, inline.} = [x, yz[0], yz[1]].Vec3

proc vec4*(x: float32): Vec4 {.nimslmagic, inline.} = [x, default(float32), default(float32), default(float32)].Vec4
proc vec4*(x, y: float32): Vec4 {.nimslmagic, inline.} = [x, y, default(float32), default(float32)].Vec4
proc vec4*(x, y, z: float32): Vec4 {.nimslmagic, inline.} = [x, y, z, default(float32)].Vec4
proc vec4*(x, y, z, w: float32): Vec4 {.nimslmagic, inline.} = [x, y, z, w].Vec4

proc vec4*(xy: Vec2): Vec4 {.nimslmagic, inline.} = [xy[0], xy[1], default(float32), default(float32)].Vec4
proc vec4*(xy: Vec2, z: float32): Vec4 {.nimslmagic, inline.} = [xy[0], xy[1], z, default(float32)].Vec4
proc vec4*(xy: Vec2, z, w: float32): Vec4 {.nimslmagic, inline.} = [xy[0], xy[1], z, w].Vec4

proc vec4*(xy, zw: Vec2): Vec4 {.nimslmagic, inline.} = [xy[0], xy[1], zw[0], zw[1]].Vec4
proc vec4*(xyz: Vec3): Vec4 {.nimslmagic, inline.} = [xyz[0], xyz[1], xyz[2], default(float32)].Vec4
proc vec4*(xyz: Vec3, w: float32): Vec4 {.nimslmagic, inline.} = [xyz[0], xyz[1], xyz[2], w].Vec4


proc newVec2*(x, y: float32): Vec2 {.nimslmagic.} = [x, y].Vec2
proc newVec3*(x, y, z: float32): Vec3 {.nimslmagic.} = [x, y, z].Vec3
proc newVec4*(x, y, z, w: float32): Vec4 {.nimslmagic.} = [x, y, z, w].Vec4

proc newVec2*(x: float32): Vec2 {.nimslmagic.} = [x, x].Vec2
proc newVec3*(x: float32): Vec3 {.nimslmagic.} = [x, x, x].Vec3
proc newVec4*(x: float32): Vec4 {.nimslmagic.} = [x, x, x, x].Vec4

proc newVec3*(x, y: float32): Vec3 {.nimslmagic.} = [x, y, y].Vec3

proc newVec3*(v: Vec2, z: float32): Vec3 {.nimslmagic.} = [v[0], v[1], z].Vec3

proc newVec4*(x, y: float32): Vec4 {.nimslmagic.} = [x, y, y, y].Vec4

proc newVec4*(x, y, z: float32): Vec4 {.nimslmagic.} = [x, y, z, z].Vec4
proc newVec4*(v: Vec2, z, w: float32): Vec4 {.nimslmagic.} = [v[0], v[1], z, w].Vec4
proc newVec4*(u, v: Vec2): Vec4 {.nimslmagic.} = [u[0], u[1], v[0], v[1]].Vec4

proc newIdentityMat4*(): Mat4 =
  result[0] = 1
  result[5] = 1
  result[10] = 1
  result[15] = 1

template forEachComponentI(num: int, body: untyped) =
  when num > 0:
    const i {.inject.} = 0
    body

    when num > 1:
      block:
        const i {.inject.} = 1
        body

        when num > 2:
          block:
            const i {.inject.} = 2
            body

            when num > 3:
              block:
                const i {.inject.} = 3
                body

template defineArithVectorScalarOp(op: untyped) =
  proc op*[I, T](v: VecBase[I, T], s: T): VecBase[I, T] {.nimslmagic.} =
    forEachComponentI(I):
      result[i] = `op`(v[i], s)

defineArithVectorScalarOp(`+`)
defineArithVectorScalarOp(`-`)
defineArithVectorScalarOp(`*`)
defineArithVectorScalarOp(`/`)
defineArithVectorScalarOp(`div`)

template defineArithVectorVectorOp(op: untyped) =
  proc op*[I, T](a, b: VecBase[I, T]): VecBase[I, T] {.nimslmagic.} =
    forEachComponentI(I):
      result[i] = `op`(a[i], b[i])

defineArithVectorVectorOp(`+`)
defineArithVectorVectorOp(`-`)
defineArithVectorVectorOp(`*`)
defineArithVectorVectorOp(`/`)
defineArithVectorVectorOp(`div`)

template defineArithScalarVectorOp(op: untyped) =
  proc op*[I, T](s: T, v: VecBase[I, T]): VecBase[I, T] {.nimslmagic.} =
    forEachComponentI(I):
      result[i] = `op`(s, v[i])

defineArithScalarVectorOp(`+`)
defineArithScalarVectorOp(`-`)
defineArithScalarVectorOp(`*`)
defineArithScalarVectorOp(`/`)
defineArithScalarVectorOp(`div`)

template defineFloatScalarFloatVectoOp(op: untyped) =
  proc op*[I](s: float32, v: VecBase[I, float32]): VecBase[I, float32] {.nimslmagic.} =
    forEachComponentI(I):
      result[i] = `op`(s, v[i])

defineFloatScalarFloatVectoOp(`+`)
defineFloatScalarFloatVectoOp(`-`)
defineFloatScalarFloatVectoOp(`*`)
defineFloatScalarFloatVectoOp(`/`)

template defineUnaryVectorOp(op: untyped) =
  proc op*[I, T](v: VecBase[I, T]): VecBase[I, T] {.nimslmagic.} =
    forEachComponentI(I):
      result[i] = `op`(v[i])

defineUnaryVectorOp(`-`)


proc sin*(v: float32 | float64): auto {.nimslmagic.} = math.sin(v)
proc abs*(v: float32 | float64): auto {.nimslmagic.} = (if v < 0: -v else: v)

defineUnaryVectorOp(sin)
defineUnaryVectorOp(cos)
defineUnaryVectorOp(abs)

proc `==`*[I, T](a, b: VecBase[I, T]): bool {.nimslmagic, inline.} =
  for i in 0 ..< I:
    if a[i] != b[i]: return false
  return true

proc max*(v: Vec2, s: float32): Vec2 {.nimslmagic.} = [max(v[0], s), max(v[1], s)].Vec2
proc min*(v: Vec2, s: float32): Vec2 {.nimslmagic.} = [min(v[0], s), min(v[1], s)].Vec2
proc length*(v: Vec2): float32 {.nimslmagic.} = sqrt(v[0] * v[0] + v[1] * v[1])

proc smoothstep*(edge0, edge1, x: float32): float32 {.nimslmagic.} =
  let c = clamp((x - edge0)/(edge1 - edge0), 0.0, 1.0)
  result = c*c*(3 - 2*c)

proc fwidth*(v: float32): float32 {.nimslmagic.} = 0
proc mix*(x, y, a: Vec4): Vec4 {.nimslmagic.} =
  assert(false, "Not implemented")
proc mix*[N: static[int]](x, y: VecBase[N, float32], a: float32): VecBase[N, float32] {.nimslmagic.} = x * (1.0 - a) + y * (a)

proc dot*[I: static[int], T](v1, v2: VecBase[I, T]): T {.nimslmagic.} =
  for i in 0 ..< I: result += v1[i] * v2[i]

proc cross*[T](a, b: VecBase[3, T]): VecBase[3, T] {.nimslmagic.} =
  VecBase[3, T]([a[1]*b[2] - a[2]*b[1], a[2]*b[0] - a[0]*b[2], a[0]*b[1] - a[1]*b[0]])

proc cross*[T](a, b: VecBase[2, T]): T {.nimslmagic.} =
  a.x * b.y - a.y * b.x

proc sign*[I, T](a: VecBase[I, T]): VecBase[I, T] {.nimslmagic.} =
  for i in 0 ..< I:
    result[i] = T(cmp(a[i], 0))

proc storageBarrier*() {.nimslmagic, inline.} = discard
proc workgroupBarrier*() {.nimslmagic, inline.} = discard

proc fract*(a: float32): float32 {.nimslmagic.} = a mod 1
proc fract*[I, T](v: VecBase[I, T]): VecBase[I, T] {.nimslmagic.} =
  for i in 0 ..< I:
    result[i] = v[i] mod 1
proc pack4x8snorm*(a: Vec4): uint32 {.nimslmagic.} = discard # TODO: Implement me
proc unpack4x8snorm*(a: uint32): Vec4 {.nimslmagic.} = discard # TODO: Implement me

proc length*(v: Vec3): float32 {.nimslmagic.} = sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2])

proc normalize*[I, T](v: VecBase[I, T]): VecBase[I, T] =
  let len = length(v)
  for i in 0 ..< I:
    result[i] = v[i] / len

proc x*[I: static[int], T](v: VecBase[I, T]): T {.inline, stackTrace: off, nimslmagic.} = v[0]
proc y*[I: static[int], T](v: VecBase[I, T]): T {.inline, stackTrace: off, nimslmagic.} = v[1]
proc z*[I: static[int], T](v: VecBase[I, T]): T {.inline, stackTrace: off, nimslmagic.} = v[2]
proc w*[I: static[int], T](v: VecBase[I, T]): T {.inline, stackTrace: off, nimslmagic.} = v[3]

proc x*[I: static[int], T](v: var VecBase[I, T]): var T {.inline, stackTrace: off, nimslmagic.} = v[0]
proc y*[I: static[int], T](v: var VecBase[I, T]): var T {.inline, stackTrace: off, nimslmagic.} = v[1]
proc z*[I: static[int], T](v: var VecBase[I, T]): var T {.inline, stackTrace: off, nimslmagic.} = v[2]
proc w*[I: static[int], T](v: var VecBase[I, T]): var T {.inline, stackTrace: off, nimslmagic.} = v[3]

template `x=`*[I: static[int], T](v: var VecBase[I, T], val: T) = v[0] = val
template `y=`*[I: static[int], T](v: var VecBase[I, T], val: T) = v[1] = val
template `z=`*[I: static[int], T](v: var VecBase[I, T], val: T) = v[2] = val
template `w=`*[I: static[int], T](v: var VecBase[I, T], val: T) = v[3] = val

proc `*`*(m: Mat4, v: Vec4): Vec4 {.nimslmagic.} =
  let (x, y, z, w) = (v[0], v[1], v[2], v[3])
  result[0] = m[0] * x + m[4] * y + m[8] * z + m[12] * w
  result[1] = m[1] * x + m[5] * y + m[9] * z + m[13] * w
  result[2] = m[2] * x + m[6] * y + m[10] * z + m[14] * w
  result[3] = m[3] * x + m[7] * y + m[11] * z + m[15] * w

proc validateAttrs(s: string, sourceLen: int) {.compileTime.} =
  var validChars: set[char]
  if sourceLen > 0:
    validChars.incl({'x', 'r'})
    if sourceLen > 1:
      validChars.incl({'y', 'g'})
      if sourceLen > 2:
        validChars.incl({'z', 'b'})
        if sourceLen > 3:
          validChars.incl({'w', 'a'})

  for c in s:
    assert(c in validChars, "Invalid vector component: " & $c)

proc nimsl_deriveVectorWithComponents[I, T](v: VecBase[I, T], f: static[string]): VecBase[f.len, T] {.nimslmagic, inline.} =
  static: validateAttrs(f, I)

  template appendToResult(i: int) =
    when f[i] == 'x' or f[i] == 'r':
      result[i] = v.x
    elif f[i] == 'y' or f[i] == 'g':
      result[i] = v.y
    elif f[i] == 'z' or f[i] == 'b':
      result[i] = v.z
    elif f[i] == 'w' or f[i] == 'a':
      result[i] = v.w

    when i < f.len - 1:
      appendToResult(i + 1)

  appendToResult(0)

template `.`*[I, T](v: VecBase[I, T], f: untyped): auto =
  nimsl_deriveVectorWithComponents(v, astToStr(f))

proc textureLoad*[T, C, L](t: Texture2D[T], coords: VecBase[2, C], level: L): VecBase[4, T] {.nimslmagic.} = discard
proc textureLoad*[T, C](t: TextureStorage2D[T], coords: VecBase[2, C]): VecBase[4, T] {.nimslmagic.} = discard

proc textureStore*[T, C](t: TextureStorage2D[T], coords: VecBase[2, C], v: VecBase[4, T]) {.nimslmagic.} = discard

   # fn textureSample(t: texture_2d<f32>, s: sampler, coords: vec2<f32>) -> vec4<f32>
proc textureSample*[T](t: Texture2D[T], s: Sampler, coords: Vec2): Vec4 {.nimslmagic.} = discard
# when isMainModule:
#   import ./private/compiler
#   compiler.main()
