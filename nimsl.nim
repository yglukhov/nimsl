import std/[macros, math]
import ./nimsl/private/common
import ./nimsl/private/codegen


when wgslOutputPath != "" or glslOutputPath != "":
  macro vertex*(u: typed): untyped =
    when wgslOutputPath != "":
      codegen.singleVertexShader(u)

  macro fragment*(u: typed): untyped =
    when wgslOutputPath != "":
      codegen.singleFragmentShader(u)

  macro compute*(u: typed): untyped =
    when wgslOutputPath != "":
      codegen.singleComputeShader(u)

else:
  template compute* {.pragma.}
  template vertex* {.pragma.}
  template fragment* {.pragma.}

export common
export codegen.wgslShader

type
  Texture2D*[T] = object
  TextureStorage2D*[T] = object
  Sampler* = object

proc getGLSLShaderCode(s: NimNode, k: ShaderKind, mainProcName: string): string =
  var ctx = newCtx(slGLSL)
  ctx.mainProcName = mainProcName
  ctx.shaderKind = k
  genProcDef(ctx, getImpl(s), main = true)
  result = ""
  for i in ctx.globalDefs:
    result &= i

macro getGLSLFragmentShader*(s: typed{nkSym}, mainProcName: static[string] = "main"): string =
  result = newLit(getGLSLShaderCode(s, skFragmentShader, mainProcName))

macro getGLSLVertexShader*(s: typed{nkSym}): string =
  result = newLit(getGLSLShaderCode(s, skVertexShader, "main"))

import ./nimsl/vmath
export vmath

proc newIdentityMat4*(): Mat4 =
  result[0] = 1
  result[5] = 1
  result[10] = 1
  result[15] = 1

proc storageBarrier*() {.nimslmagic, inline.} = discard
proc workgroupBarrier*() {.nimslmagic, inline.} = discard

proc pack4x8snorm*(a: Vec4): uint32 {.nimslmagic.} = discard # TODO: Implement me
proc unpack4x8snorm*(a: uint32): Vec4 {.nimslmagic.} = discard # TODO: Implement me

proc textureLoad*[T, C, L](t: Texture2D[T], coords: VecBase[2, C], level: L): VecBase[4, T] {.nimslmagic.} = discard
proc textureLoad*[T, C](t: TextureStorage2D[T], coords: VecBase[2, C]): VecBase[4, T] {.nimslmagic.} = discard

proc textureStore*[T, C](t: TextureStorage2D[T], coords: VecBase[2, C], v: VecBase[4, T]) {.nimslmagic.} = discard

   # fn textureSample(t: texture_2d<f32>, s: sampler, coords: vec2<f32>) -> vec4<f32>
proc textureSample*[T](t: Texture2D[T], s: Sampler, coords: Vec2): Vec4 {.nimslmagic.} = discard
proc textureSampleGrad*[T](t: Texture2D[T], s: Sampler, coords, ddx, ddy: Vec2): Vec4 {.nimslmagic.} = discard
# when isMainModule:
#   import ./private/compiler
#   compiler.main()
