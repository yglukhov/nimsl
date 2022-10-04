# Package
version = "0.3.3"
author = "Yuriy Glukhov"
description = "Shaders in Nim"
license = "MIT"

# Tests
const allTests = ["test", "nimsl/private/var_decls"]

task test, "Run tests":
    exec "nimble jsTests"
    exec "nimble cTests"

task jsTests, "":
    for t in allTests:
        exec("nim js -r -d:nodejs " & t)

task cTests, "":
    for t in allTests:
        exec("nim c -r " & t)
