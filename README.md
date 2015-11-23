# nimsl [![Build Status](https://semaphoreci.com/api/v1/projects/a58e296c-b761-42d7-aedc-9fdbcf5280f4/611106/badge.svg)](https://semaphoreci.com/yglukhov/nimsl)
Shaders in Nim language.

Ever wanted to use metaprogramming and unittests for your shader functions? Now you have it! Of course you also have other nice Nim features, such as type inference, template, generics, etc. Since shader code is valid Nim code, you can compile it to native/js target and debug it with your regular debugging tools.

```nim
import nimsl.nimsl

proc myVertexShader(uModelViewProjectionMatrix: mat4, aPos: vec2, vPos: var vec2): vec4 =
    vPos = aPos
    result = uModelViewProjectionMatrix * newVec4(aPos, 0, 1)

proc myFragmentShader(vPos: vec2): vec4 =
    if vPos.x < 50:
        result = newVec4(1, 0, 0, 1) # red
    else:
        result = newVec4(0, 1, 0, 1) # green

const shaderCodeInGLSL = getGLSLVertexShader(myVertexShader)
echo shaderCodeInGLSL
```
Outputs:
```glsl
uniform mat4 uModelViewProjectionMatrix;
attribute vec2 aPos;
varying vec2 vPos;
void main(){gl_Position=vec4(0.0);vPos=aPos;gl_Position=(uModelViewProjectionMatrix*vec4(aPos,0.0,1.0));}
```

Running shader on the CPU:
```nim
import nimsl.emulation

const screenWidth = 500
const screenHeight = 500
const colorComponents = 4

var pseudoScreenBuffer = newSeq[uint8](screenWidth * screenHeight * colorComponents)
let vertices = [newVec2(0, 0), newVec2(screenWidth, 0),
    newVec2(screenWidth, screenHeight), newVec2(0, screenHeight)]

var mvp = newIdentityMat4()

cpuTest(myVertexShader, myFragmentShader,
    {
        uModelViewProjectionMatrix : mvp, # Uniforms
        aPos: [vertices[0], vertices[1], vertices[2]] }, # Attributes
    pseudoScreenBuffer, screenWidth)
# Color data (RGBA) is written to pseudoScreenBuffer
```
