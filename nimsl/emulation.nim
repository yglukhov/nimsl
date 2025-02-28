import macros, strutils
import nimsl
import private/glsl_codegen

proc pointInTriangle(p, p0, p1, p2: Vec2): bool =
  var s = p0.y * p2.x - p0.x * p2.y + (p2.y - p0.y) * p.x + (p0.x - p2.x) * p.y
  var t = p0.x * p1.y - p0.y * p1.x + (p0.y - p1.y) * p.x + (p1.x - p0.x) * p.y

  if (s < 0) != (t < 0):
    return false

  var A = -p1.y * p2.x + p0.y * (p2.x - p1.x) + p0.x * (p1.y - p2.y) + p1.x * p2.y
  if A < 0.0:
    s = -s
    t = -t
    A = -A
  return s > 0 and t > 0 and (s + t) < A

proc getValueByNameInTableConstr(tableConstr: NimNode, name: string): NimNode =
  for c in tableConstr:
    if $c[0] == name:
      return c[1]

macro cpuTest*(vertexShader, fragmentShader: typed{nkSym}, attributesAndUniforms: untyped, screenBuffer: var openarray[uint8], screenWidth: int): untyped =
  var vsCalls = [newCall(vertexShader), newCall(vertexShader), newCall(vertexShader)]
  var fsCall = newCall(fragmentShader)
  var varyingDefs = newStmtList()

  var varyingInterpolations = newStmtList()

  let a0 = genSym(nskLet, "a0")
  let a1 = genSym(nskLet, "a1")
  let a2 = genSym(nskLet, "a2")

  var globalVarSyms = newSeq[NimNode]()

  for param in getImpl(vertexShader).paramsAndTypes:
    let paramName = $(param.name)
    if paramName.startsWith("v"):
      let varSym = genSym(nskLet, paramName)
      var varSyms : array[3, NimNode]
      for i in 0 .. 2:
        varSyms[i] = genSym(nskVar, paramName & $i)
        varyingDefs.add(
          newNimNode(nnkVarSection).add(
            newNimNode(nnkIdentDefs).add(varSyms[i], param.typ[0], newEmptyNode())))
        vsCalls[i].add(varSyms[i])

      globalVarSyms.add(varSym)
      let vs0 = varSyms[0]
      let vs1 = varSyms[1]
      let vs2 = varSyms[2]
      let q = quote do:
        let `varSym` = `vs0` * `a0` + `vs1` * `a1` + `vs2` * `a2`
      varyingInterpolations.add q
    elif paramName.startsWith("a"):
      for i in 0 .. 2:
        vsCalls[i].add(getValueByNameInTableConstr(attributesAndUniforms, paramName)[i])
    else:
      # uniforms
      for i in 0 .. 2:
        vsCalls[i].add(getValueByNameInTableConstr(attributesAndUniforms, paramName))

  for param in getImpl(fragmentShader).paramsAndTypes:
    let paramName = $(param.name)
    if paramName.startsWith("v"):
      for vs in globalVarSyms:
        if $vs == paramName:
          fsCall.add(vs)
          break
    elif paramName.startsWith("a"):
      # No attributes allowed in fragment shader
      assert(false)
    else:
      # uniforms
      fsCall.add(getValueByNameInTableConstr(attributesAndUniforms, paramName))

  let vsc0 = vsCalls[0]
  let vsc1 = vsCalls[1]
  let vsc2 = vsCalls[2]

  result = quote do:
    let screenHeight = int(`screenBuffer`.len / 4 / screenWidth)

    `varyingDefs`

    let p1 = `vsc0`
    let p2 = `vsc1`
    let p3 = `vsc2`

    let fp1 = vec3(p1.xy)
    let fp2 = vec3(p2.xy)
    let fp3 = vec3(p3.xy)

    # Rasterization loop
    for x in 0 ..< screenWidth:
      for y in 0 ..< screenHeight:
        let f = vec3(x.float32, y.float32)
        if pointInTriangle(f.xy, fp1.xy, fp2.xy, fp3.xy):
          let f1 = fp1-f
          let f2 = fp2-f
          let f3 = fp3-f
          # calculate the areas and factors (order of parameters doesn't matter):
          let a = cross(fp1-fp2, fp1-fp3).length() # main triangle area a
          let `a0` = cross(f2, f3).length() / a # p1's triangle area / a
          let `a1` = cross(f3, f1).length() / a # p2's triangle area / a
          let `a2` = cross(f1, f2).length() / a # p3's triangle area / a
          # find the uv corresponding to point f (uv1/uv2/uv3 are associated to p1/p2/p3):
          #var uv: Vector2 = uv1 * a1 + uv2 * a2 + uv3 * a3;
          `varyingInterpolations`
          #let vPos = vPos0 * a1 + vPos1 * a2 + vPos2 * a3
          let output = `fsCall`

          let bufferOffset = (y * screenWidth + x) * colorComponents
          for i in 0 .. 3:
            `screenBuffer`[bufferOffset + i] = uint8(output[i] * 255)
