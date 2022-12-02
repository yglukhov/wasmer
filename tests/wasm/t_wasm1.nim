import macros

macro exportwasm*(p: untyped): untyped =
  expectKind(p, nnkProcDef)
  result = p
  result.addPragma(newIdentNode("exportc"))
  let cgenDecl = when defined(cpp):
                   "extern \"C\" __attribute__ ((visibility (\"default\"))) $# $#$#"
                 else:
                   "__attribute__ ((visibility (\"default\"))) $# $#$#"

  result.addPragma(newColonExpr(newIdentNode("codegenDecl"), newLit(cgenDecl)))

proc importedFunc(a: int): int {.importc.}

proc sum(a, b: int): int {.exportwasm.} =
  a + importedFunc(b)
