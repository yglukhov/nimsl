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
  type
    VertexInput = object
      idx {.builtin(vertexIndex).}: uint32
    VertexOutput = object
      position {.builtin(position).}: Vec4


  proc setVelocity(x, y: uint32, v: Vec2) = discard
  proc vsMain(input: VertexInput): VertexOutput {.vertex.} =
    setVelocity(0, 0, vec2(0, 0))

  chk(wgslShader(vsMain), """
struct VertexOutput {
  @builtin(position) position: vec4f
}

struct VertexInput {
  @builtin(vertex_index) idx: u32
}


fn setVelocity0(x: u32, y: u32, v: vec2f) {
  ;
}

@vertex
fn vsMain(input: VertexInput) -> VertexOutput {
  var result: VertexOutput;
  setVelocity0(u32(0), u32(0), vec2f(f32(0), f32(0)));
  return result;
}
""")

block:
  type
    VertexInput = object
      idx {.builtin(vertexIndex).}: uint32
    VertexOutput = object
      position {.builtin(position).}: Vec3

  var someConsts = [vec3(1, 2, 3), vec3(5, 5, 6)]

  proc vsMain(input: VertexInput): VertexOutput {.vertex.} =
    VertexOutput(position: someConsts[1])

  chk(wgslShader(vsMain), """
struct VertexOutput {
  @builtin(position) position: vec3f
}

struct VertexInput {
  @builtin(vertex_index) idx: u32
}

var<private> someConsts: array<vec3f,2> = array(vec3f(f32(1), f32(2), f32(3)), vec3f(f32(5), f32(5), f32(6)));

@vertex
fn vsMain(input: VertexInput) -> VertexOutput {
  return VertexOutput(someConsts[1]);
}
""")

block: # enums
  type
    SomeEnum = enum
      seValue1
      seValue2

  proc someFunc(e: SomeEnum) = discard

  proc vsMain() {.vertex.} =
    let k = seValue1
    someFunc(seValue1)
    someFunc(seValue2)
    var ppp = 1'u32
    let p = SomeEnum(ppp)

  chk(wgslShader(vsMain), """
fn someFunc0(e: i32) {
  ;
}

@vertex
fn vsMain() {
  let k = 0;
  someFunc0(0);
  someFunc0(1);
  var ppp = 1u;
  let p = i32(ppp);
}
""")

block: # array brackets
  var someArray {.private.}: array[20, bool]

  proc vsMain() {.vertex.} =
    let b = someArray[1]
    someArray[2] = b
    if not someArray[1]:
      someArray[3] = someArray[2]

  chk(wgslShader(vsMain), """
var<private> someArray: array<bool,20>;

@vertex
fn vsMain() {
  let b = someArray[1];
  someArray[2] = b;
  if !(someArray[1]) {
    someArray[3] = someArray[2];
  }
}
""")

block: # case in let
  proc vsMain() {.vertex.} =
    let a = 5'u32
    if true:
      let b: uint32 = case a
              of 1: 2
              of 2: 3
              else: 5

  chk(wgslShader(vsMain), """
@vertex
fn vsMain() {
  let a = 5u;
  if true {
    var tmp0: u32;
    switch a {
      case 1u: {
        tmp0 = 2u;
      }
      case 2u: {
        tmp0 = 3u;
      }
      default: {
        tmp0 = 5u;
      }
    }
    let b = tmp0;
  }
}
""")


import std/[math] # for mod
block: # ops
  proc vsMain() {.vertex.} =
    let a = 5'u32
    let b = not a
    let c = false
    let d = not c
    let
      e = 5'f32
      f = e mod 2'f32

  chk(wgslShader(vsMain), """
@vertex
fn vsMain() {
  let a = 5u;
  let b = ~(a);
  let c = false;
  let d = !(c);
  let e = 5.0;
  let f = e % 2.0;
}
""")

block: # typs
  type
    Uniforms = object
      scale: float32
  const sz = 512
  var pa {.workgroup.}: array[sz, Vec2]
  var pu {.uniform.}: Uniforms
  proc vsMain() {.vertex.} =
    let p = pa[0]
    let b = pu.scale

  chk(wgslShader(vsMain), """
var<workgroup> pa: array<vec2f,512>;

struct Uniforms {
  scale: f32
}

var<uniform> pu: Uniforms;

@vertex
fn vsMain() {
  let p = pa[0];
  let b = pu.scale;
}
""")

echo "WGSL Codegen: OK"
