import std/[macros, math]
import private/common
from private/glsl_codegen import nil

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

proc newVec2*(x, y: float32): Vec2 = glslbuiltin(); return [x, y].Vec2
proc newVec3*(x, y, z: float32): Vec3 = glslbuiltin(); return [x, y, z].Vec3
proc newVec4*(x, y, z, w: float32): Vec4 = glslbuiltin(); return [x, y, z, w].Vec4

proc newVec2*(x: float32): Vec2 = glslbuiltin(); return [x, x].Vec2
proc newVec3*(x: float32): Vec3 = glslbuiltin(); return [x, x, x].Vec3
proc newVec4*(x: float32): Vec4 = glslbuiltin(); return [x, x, x, x].Vec4

proc newVec3*(x, y: float32): Vec3 = glslbuiltin(); return [x, y, y].Vec3

proc newVec3*(v: Vec2, z: float32): Vec3 = glslbuiltin(); return [v[0], v[1], z].Vec3

proc newVec4*(x, y: float32): Vec4 = glslbuiltin(); return [x, y, y, y].Vec4

proc newVec4*(x, y, z: float32): Vec4 = glslbuiltin(); return [x, y, z, z].Vec4
proc newVec4*(v: Vec2, z, w: float32): Vec4 =
  glslbuiltin()
  return [v[0], v[1], z, w].Vec4
proc newVec4*(u, v: Vec2): Vec4 = glslbuiltin(); return [u[0], u[1], v[0], v[1]].Vec4

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
  proc op*[I, T](v: VecBase[I, T], s: T): VecBase[I, T] =
    glslbuiltin()
    forEachComponentI(I):
      result[i] = `op`(v[i], s)

defineArithVectorScalarOp(`+`)
defineArithVectorScalarOp(`-`)
defineArithVectorScalarOp(`*`)
defineArithVectorScalarOp(`/`)

template defineArithVectorVectorOp(op: untyped) =
  proc op*[I, T](a, b: VecBase[I, T]): VecBase[I, T] =
    glslbuiltin()
    forEachComponentI(I):
      result[i] = `op`(a[i], b[i])

defineArithVectorVectorOp(`+`)
defineArithVectorVectorOp(`-`)
defineArithVectorVectorOp(`*`)
defineArithVectorVectorOp(`/`)

template defineArithScalarVectorOp(op: untyped) =
  proc op*[I, T](s: T, v: VecBase[I, T]): VecBase[I, T] =
    glslbuiltin()
    forEachComponentI(I):
      result[i] = `op`(s, v[i])

defineArithScalarVectorOp(`+`)
defineArithScalarVectorOp(`-`)
defineArithScalarVectorOp(`*`)
defineArithScalarVectorOp(`/`)

template defineFloatScalarFloatVectoOp(op: untyped) =
  proc op*[I](s: float32, v: VecBase[I, float32]): VecBase[I, float32] =
    glslbuiltin()
    forEachComponentI(I):
      result[i] = `op`(s, v[i])

defineFloatScalarFloatVectoOp(`+`)
defineFloatScalarFloatVectoOp(`-`)
defineFloatScalarFloatVectoOp(`*`)
defineFloatScalarFloatVectoOp(`/`)

template defineUnaryVectorOp(op: untyped) =
  proc op*[I](v: VecBase[I, float32]): VecBase[I, float32] =
    glslbuiltin()
    forEachComponentI(I):
      result[i] = `op`(v[i])

defineUnaryVectorOp(`-`)


proc sin*(v: float32 | float64): auto = glslbuiltin(); return math.sin(v)
proc abs*(v: float32 | float64): auto = glslbuiltin(); return (if v < 0: -v else: v)

defineUnaryVectorOp(sin)
defineUnaryVectorOp(cos)
defineUnaryVectorOp(abs)

# proc abs*(v: Vec2): Vec2 = glslbuiltin(); return [abs(v[0]), abs(v[1])].Vec2
proc max*(v: Vec2, s: float32): Vec2 = glslbuiltin(); return [max(v[0], s), max(v[1], s)].Vec2
proc min*(v: Vec2, s: float32): Vec2 = glslbuiltin(); return [min(v[0], s), min(v[1], s)].Vec2
proc length*(v: Vec2): float32 = glslbuiltin(); return sqrt(v[0] * v[0] + v[1] * v[1])

proc smoothstep*(edge0, edge1, x: float32): float32 =
  glslbuiltin()
  let c = clamp((x - edge0)/(edge1 - edge0), 0.0, 1.0)
  result = c*c*(3 - 2*c)

proc fwidth*(v: float32): float32 = glslbuiltin(); return 0
proc mix*(x, y, a: Vec4): Vec4 =
  glslbuiltin();
  assert(false, "Not implemented")
proc mix*(x, y: Vec4, a: float32): Vec4 = glslbuiltin(); return x * (1.0 - a) + y * (a)

proc dot*[I: static[int], T](v1, v2: VecBase[I, T]): T =
  glslbuiltin()
  for i in 0 ..< I: result += v1[i] * v2[i]

proc cross*[T](a, b: VecBase[3, T]): VecBase[3, T] =
  glslbuiltin()
  return VecBase[3, T]([a[1]*b[2] - a[2]*b[1], a[2]*b[0] - a[0]*b[2], a[0]*b[1] - a[1]*b[0]])

proc length*(v: Vec3): float32 = glslbuiltin(); return sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2])

template x*[I: static[int], T](v: VecBase[I, T]): T = v[0]
template y*[I: static[int], T](v: VecBase[I, T]): T = v[1]
template z*[I: static[int], T](v: VecBase[I, T]): T = v[2]
template w*[I: static[int], T](v: VecBase[I, T]): T = v[3]

template `x=`*[I: static[int], T](v: var VecBase[I, T], val: T) = v[0] = val
template `y=`*[I: static[int], T](v: var VecBase[I, T], val: T) = v[1] = val
template `z=`*[I: static[int], T](v: var VecBase[I, T], val: T) = v[2] = val
template `w=`*[I: static[int], T](v: var VecBase[I, T], val: T) = v[3] = val

proc `*`*(m: Mat4, v: Vec4): Vec4 =
  glslbuiltin()
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

proc nimsl_deriveVectorWithComponents[I, T](v: VecBase[I, T], f: static[string]): VecBase[f.len, T] {.inline.} =
  glslinfix()
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

when defined(nimNewDot):
  template `.`*[I, T](v: VecBase[I, T], f: untyped): auto =
    nimsl_deriveVectorWithComponents(v, astToStr(f))
else:
  proc `.`*[I, T](v: VecBase[I, T], f: static[string]): VecBase[f.len, T] {.inline.} =
    glslinfix()
    result = nimsl_deriveVectorWithComponents(v, f)
