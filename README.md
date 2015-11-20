# nimsl
Shaders in Nim language.

Ever wanted to use metaprogramming and unittests for your shader functions? Now you have it! Of course you also have other nice Nim features, such as type inference, template, generics, etc.

```nim
import nimsl

proc myVertexShader(uModelViewProjectionMatrix: mat4, aPos: vec2, vPos: var vec2): vec4 =
    vPos = aPos
    result = uModelViewProjectionMatrix * newVec4(aPos, 0, 1)

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
