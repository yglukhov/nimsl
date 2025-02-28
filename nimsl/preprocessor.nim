
proc preprocess*(s: var string, defs: set[char]) =
  ## Preprocess string s by finding the following
  ## sequence:
  ## `aSOMESTRING``
  ## The start of the sequence is backtick, followed
  ## by flag (some character). If this flag is not
  ## in `defs`, all the sequense is be replaced with
  ## spaces. The end of the sequence is marked with
  ## two subsequent backticks.
  ## The sequences can be chained:
  ## `aFOO`bBAR`cBAZ``
  const escapeChar = '`'
  var erase = false
  var i = 0
  let sz = s.len
  while i < sz:
    if s[i] == escapeChar:
      let flag = s[i + 1]
      erase = flag != escapeChar and flag notin defs
      s[i] = ' '
      inc i
      s[i] = ' '
    elif erase:
      s[i] = ' '
    inc i

when isMainModule:
  var s = """
  foo
  `a
  bar
  `b
  yo
  ``
  """
  preprocess(s, {'b', 'a'})
  echo s
