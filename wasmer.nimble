# Package

version       = "0.1.0"
author        = "Yuriy Glukhov"
description   = "Embed wasm in nim"
license       = "MIT"


# Dependencies

requires "nim >= 1.7.3"

proc compileWasm(name: string) =
  let f = "tests/wasm/" & name
  exec "nim c --out:" & f & ".unoptimized.wasm " & f
  if findExe("wasm-opt") != "":
    exec "wasm-opt -Oz --zero-filled-memory --strip-producers " & f & ".unoptimized.wasm -o " & f & ".wasm"
  else:
    echo "wasm-opt not found"

proc runTest(name: string) =
  let f = "tests/" & name
  exec "nim c -r " & f

task test, "Run tests":
  compileWasm "t_wasm1"

  runTest "test1"
