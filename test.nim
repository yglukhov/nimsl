import nimsl
import unittest

proc myVertexShader(m: mat4, a: vec2, v: var vec2): vec4 =
    v = a
    result = m * newVec4(a, 0, 1)

suite "codegen":
    test "vs":
        check(getGLSLVertexShader(myVertexShader) ==
            "uniform mat4 m;attribute vec2 a;varying vec2 v;void main(){gl_Position=vec4(0.0);v=a;gl_Position=(m*vec4(a,0.0,1.0));}")
