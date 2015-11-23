import nimsl.nimsl
import nimsl.emulation
import unittest, times

proc fillAlpha(dist: float32): float32 =
    let d = fwidth(dist)
    result = 1.0 - smoothstep(-d, d, dist)
    #    return 1.0 - step(0.0, dist); // No antialiasing

proc drawShape(res: var vec4, dist: float32, color: vec4) =
    res = mix(res, color, fillAlpha(dist))

proc sdEllipseInRect(pos: vec2, rect: vec4): float32 =
    let ab = rect.zw / 2.0
    let center = rect.xy + ab
    let p = pos - center
    result = dot(p * p, 1.0 / (ab * ab)) - 1.0
    result *= min(ab.x, ab.y)

proc insetRect(r: vec4, by: float32): vec4 = newVec4(r.xy + by, r.zw - by * 2.0)

proc myVertexShader(mvp: mat4, a: vec2, vPos: var vec2): vec4 =
    vPos = a
    result = mvp * newVec4(a, 0, 1)

proc myFragmentShader(vPos: vec2): vec4 =
    let bounds = newVec4(0, 0, 200, 100)
    let uStrokeColor = newVec4(0, 0, 0, 1)
    let uFillColor = newVec4(1, 0, 0, 1)
    let uStrokeWidth = 3.0
    result.drawShape(sdEllipseInRect(vPos, bounds), uStrokeColor);
    result.drawShape(sdEllipseInRect(vPos, insetRect(bounds, uStrokeWidth)), uFillColor);


suite "codegen":
    test "vs":
        check(getGLSLVertexShader(myVertexShader) ==
            "uniform mat4 mvp;attribute vec2 a;varying vec2 vPos;void main(){gl_Position=vec4(0.0);vPos=a;gl_Position=(mvp*vec4(a,0.0,1.0));}")

# import nimx.write_image_impl

proc testShaderOnCPU() =
    const screenWidth = 500
    const screenHeight = 500
    const colorComponents = 4

    var pseudoScreenBuffer = newSeq[uint8](screenWidth * screenHeight * colorComponents)
    let vertices = [newVec2(0, 0), newVec2(screenWidth, 0),
        newVec2(screenWidth, screenHeight), newVec2(0, screenHeight)]

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
#    discard stbi_write_png("output.png", screenWidth, screenHeight, colorComponents, addr pseudoScreenBuffer[0], 0)

testShaderOnCPU()
