import std/[unittest, times, math]
import nimsl
import nimsl/emulation

proc fillAlpha(dist: float32): float32 =
  let d = fwidth(dist)
  result = 1.0 - smoothstep(-d, d, dist)
  #  return 1.0 - step(0.0, dist); // No antialiasing

proc drawShape(res: var Vec4, dist: float32, color: Vec4) =
  res = mix(res, color, fillAlpha(dist))

proc sdEllipseInRect(pos: Vec2, rect: Vec4): float32 =
  let ab = rect.zw / 2.0
  let center = rect.xy + ab
  let p = pos - center
  result = dot(p * p, 1.0 / (ab * ab)) - 1.0
  result *= min(ab.x, ab.y)

proc insetRect(r: Vec4, by: float32): Vec4 = vec4(r.xy + by, r.zw - by * 2.0)

proc myVertexShader(mvp: Mat4, a: Vec2, vPos: var Vec2): Vec4 =
  vPos = a
  result = mvp * vec4(a, 0, 1)

proc myFragmentShader(vPos: Vec2): Vec4 =
  let bounds = vec4(0, 0, 200, 100)
  let uStrokeColor = vec4(0, 0, 0, 1)
  let uFillColor = vec4(1, 0, 0, 1)
  let uStrokeWidth = 3.0
  result.drawShape(sdEllipseInRect(vPos, bounds), uStrokeColor);
  result.drawShape(sdEllipseInRect(vPos, insetRect(bounds, uStrokeWidth)), uFillColor);

proc myVertexShader2(a: Vec4): Vec3 =
  result = a.xxz

suite "types":
  test "vectors":
    let v = vec2(1, 2)
    let x = v.yxxy
    check x.x == 2
    check x.y == 1
    check x.z == 1
    check x.w == 2

    let s = sin(vec3(PI, 0.0, PI))
    check abs(s.x) < 0.001

# import nimx/write_image_impl

proc testShaderOnCPU() =
  const screenWidth = 500
  const screenHeight = 500
  const colorComponents = 4

  var pseudoScreenBuffer = newSeq[uint8](screenWidth * screenHeight * colorComponents)
  let vertices = [vec2(0, 0), vec2(screenWidth, 0),
    vec2(screenWidth, screenHeight), vec2(0, screenHeight)]

  var mvp = newIdentityMat4()

  let startTime = epochTime()
  # Draw first triangle
  cpuTest(myVertexShader, myFragmentShader, {
      mvp : mvp,
      a: [vertices[0], vertices[1], vertices[2]]
    },
    pseudoScreenBuffer, screenWidth)
  # Draw second triangle
  cpuTest(myVertexShader, myFragmentShader, {
      mvp : mvp,
      a: [vertices[0], vertices[2], vertices[3]]
    },
    pseudoScreenBuffer, screenWidth)
  let endTime = epochTime()
  echo "Rasterization took: ", endTime - startTime, " seconds"
  # discard stbi_write_png("output.png", screenWidth, screenHeight, colorComponents, addr pseudoScreenBuffer[0], 0)

testShaderOnCPU()

