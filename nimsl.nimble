# Package
version = "0.2.1"
author = "Yuriy Glukhov"
description = "Shaders in Nim"
license = "MIT"

# Dependencies
requires "variant"

# Tests
const allTests = ["test", "nimsl/private/var_decls"]

task tests, "Run tests":
    exec "nimble jsTests"
    exec "nimble cTests"

task jsTests, "":
    for t in allTests:
        exec("nim js -r -d:nodejs " & t)

task cTests, "":
    for t in allTests:
        exec("nim c -r " & t)
