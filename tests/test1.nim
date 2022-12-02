from os import getAppDir
import wasmer

proc importedFunc(a: int32): int32 =
  a + 1

proc test() =
  const watString = """
    (module
      (type $sum_t (func (param i32 i32) (result i32)))
      (func $sum_f (type $sum_t) (param $x i32) (param $y i32) (result i32)
        local.get $x
        local.get $y
        i32.add)
      (export "sum" (func $sum_f)))
  """

  # let wasmBytes = wat2wasm(watString)
  let wasmBytes = newVec(readFile(getAppDir() & "/wasm/t_wasm1.wasm"))
  let e = newEngine()
  let store = newStore(e)
  let module = newModule(store, wasmBytes)
  wasmBytes.delete()
  if module.isNil:
    raise newException(ValueError, "Error compiling wasm")

  let imports = module.imports
  var importObject = newSeq[Extern](imports.len)
  for i, im in imports:
    let n = im.name
    case n
    of "importedFunc":
      importObject[i] = newFunc(store, importedFunc).asExtern
    else:
      raise newException(ValueError, "Unexpected import: " & n)

  let instance = newInstance(store, module, importObject)
  if instance.isNil:
    raise newException(ValueError, "Error instantiating module")

  let exports = instance.exports()
  let sumFunc = exports[1].asFunc()
  if sumFunc.isNil:
    raise newException(ValueError, "Error getting sum func")

  var result: Val
  if sumFunc.call([vali32(1), vali32(6)], result) != nil:
    raise newException(ValueError, "Error calling sum function")

  doAssert(result.i32 == 8)

test()
