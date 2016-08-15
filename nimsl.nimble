# Package
version = "0.2"
author = "Yuriy Glukhov"
description = "Shaders in Nim"
license = "MIT"

task tests, "Run tests":
    exec "nimble jsTests"
    exec "nimble cTests"

task jsTests, "":
    setCommand "js", "-d:nodejs -r test"

task cTests, "":
    setCommand "c", "-r test"
