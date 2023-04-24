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
  let wasmBytes = readFile(getAppDir() & "/wasm/t_wasm1.wasm")
  let engine = newEngine()
  let store = engine.newStore()
  let module = newModule(store, wasmBytes)
  if module.isNil:
    raise newException(ValueError, "Error compiling wasm")

  var a = 1'i32
  proc someClosure(b: int32): int32 =
    a + b

  let im = makeImports(store, {
    "importedFunc": importedFunc,
    "importedFunc2": someClosure,
  })

  let instance = newInstance(store, module, im)
  if instance.isNil:
    raise newException(ValueError, "Error instantiating module")

  let sumFuncTyped = instance.getExport(module, "sum", proc(a, b: int32): int32)
  if sumFuncTyped.isNil:
    raise newException(ValueError, "Error getting sum func")
  doAssert(sumFuncTyped(1, 6) == 15)

  let sumFunc = instance.getExport(module, "sum").asFunc()
  if sumFunc.isNil:
    raise newException(ValueError, "Error getting sum func")

  var result: Val
  if sumFunc.call([vali32(1), vali32(6)], result) != nil:
    raise newException(ValueError, "Error calling sum function")
  doAssert(result.i32 == 15)

  a = 5
  if sumFunc.call([vali32(1), vali32(6)], result) != nil:
    raise newException(ValueError, "Error calling sum function")
  doAssert(result.i32 == 19)
  doAssert(sumFuncTyped(1, 6) == 19)

test()
