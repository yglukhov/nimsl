import std/strutils
import nimsl/nimsl

proc removeTrailingSpaces(s: string): string =
  for l in s.splitLines():
    result &= l.strip(leading = false)
    result &= "\n"

proc chk(actual, expected: static[string]) =
  let a = actual.removeTrailingSpaces().strip()
  let e = expected.strip()
  if a != e:
    writeFile("actual", a)
    writeFile("expected", e)
    echo "EXPEC: ", e
    echo "ATUAL: ", a
    doAssert(a == e)


block:
  proc myVertexShader(a: Vec4): Vec3 =
    newVec3(1, 0, 0)

  chk(getGLSLVertexShader(myVertexShader), """
attribute vec4 a;void main(){vec3 result=vec4(0.0);result=vec3(1,0,0);gl_Position=result;}
""")

block:
  proc myVertexShader(a: Vec4): Vec3 =
    if true:
      return vec3(1, 0, 0)
    vec3(0, 1, 0)

  chk(getGLSLVertexShader(myVertexShader), """
attribute vec4 a;void main(){vec3 result=vec4(0.0);if(true){gl_Position=vec3(1,0,0);return;}result=vec3(0,1,0);gl_Position=result;}
""")

block:
  proc myVertexShader(): Vec3 =
    if true:
      vec3(1, 0, 0)
    else:
      vec3(0, 1, 0)

  chk(getGLSLVertexShader(myVertexShader), """
void main(){vec3 result=vec4(0.0);vec3 tmp0;if(true){tmp0=vec3(1,0,0);}else{tmp0=vec3(0,1,0);}result=tmp0;gl_Position=result;}
""")

block:
  proc s(mvp: Mat4, a: Vec2, vPos: var Vec2): Vec4 =
    vPos = a
    result = mvp * vec4(a, 0, 1)
  
  chk(getGLSLVertexShader(s), """
uniform mat4 mvp;attribute vec2 a;varying vec2 vPos;void main(){vec4 result=vec4(0.0);vPos=a;result=(mvp*vec4(a,0,1));gl_Position=result;}
""")

block:
  proc s(a: Vec4): Vec3 =
    result = a.xxz

  chk(getGLSLVertexShader(s), """
attribute vec4 a;void main(){vec3 result=vec4(0.0);result=a.xxz;gl_Position=result;}
""")

block:
  proc s() =
    var a = vec3(1)
    a.x = 4

  chk(getGLSLVertexShader(s), """
void main(){vec3 a=vec3(1);a.x=4.0;}
""")

echo "GLSL Codegen: OK"
