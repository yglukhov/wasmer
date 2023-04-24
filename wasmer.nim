import macros, tables

{.pragma: lib, importc, dynlib: "libwasmer.so".}

type
  Module* = ptr object
  Vec*[T] = object
    size: csize_t
    data: ptr UncheckedArray[T]

  Store* = ptr object
  Engine* = ptr object
  Config* = ptr object
  Extern* = ptr object
  Instance* = ptr object
  Trap* = ptr object
  Func* = ptr object
  ImportType* = ptr object
  ExportType* = ptr object
  ExternType* = ptr object
  FuncType* = ptr object
  ValType* = ptr object

  FuncCallback* = proc(args, results: ptr Vec[Val]): Trap {.cdecl.}
  FuncCallbackWithEnv* = proc(env: pointer, args, results: ptr Vec[Val]): Trap {.cdecl.}

  ExternKind* = enum
    kFunc
    kGlobal
    kTable
    kMemory

  ValKind* = enum
    i32
    i64
    f32
    f64
    anyRef = 128
    funcRef

  Val* = object
    case kind*: ValKind
    of i32:
      i32*: int32
    of i64:
      i64*: int64
    of f32:
      f32*: float32
    of f64:
      f64*: float64
    of anyRef, funcRef:
      vref*: pointer

proc vali32*(v: int32): Val {.inline.} = Val(kind: i32, i32: v)
proc vali64*(v: int64): Val {.inline.} = Val(kind: i64, i64: v)
proc valf32*(v: float32): Val {.inline.} = Val(kind: f32, f32: v)
proc valf64*(v: float64): Val {.inline.} = Val(kind: f64, f64: v)
proc valInit*(): Val {.inline.} = Val(kind: anyRef)
proc `$`*(v: Val): string =
  case v.kind
  of i32: $v.i32
  of i64: $v.i64
  of f32: $v.f32
  of f64: $v.f64
  of anyRef: "<ref " & $cast[int](v.vref) & ">"
  of funcRef: "<func " & $cast[int](v.vref) & ">"

proc len*(v: Vec): int {.inline.} = v.size.int
proc `[]`*[T](v: Vec[T], i: int): T {.inline.} = v.data[i]
proc `[]=`*[T](v: Vec[T], i: int, d: T) {.inline.} = v.data[i] = d
iterator items*[T](v: Vec[T]): T =
  block:
    let vv = v
    for i in 0 ..< vv.size.int:
      yield vv.data[i]

iterator pairs*[T](v: Vec[T]): (int, T) =
  block:
    let vv = v
    for i in 0 ..< vv.size.int:
      yield (i, vv.data[i])

template declareArray(nimType: untyped, name: string) =
  proc wasm_vec_new(v: var Vec[nimType], sz: csize_t, data: ptr nimType) {.lib, importc: "wasm_" & name & "_vec_new".}
  proc wasm_vec_new_empty(v: var Vec[nimType]) {.lib, importc: "wasm_" & name & "_vec_new_empty".}
  proc wasm_vec_delete(v: ptr Vec[nimType]) {.lib, importc: "wasm_" & name & "_vec_delete".}

proc wasmer_version_major(): uint8 {.lib.}
proc wasmer_version_minor(): uint8 {.lib.}
proc wasmer_version_patch(): uint8 {.lib.}

proc kind*(t: ExternType): ExternKind {.lib, importc: "wasm_externtype_kind".}
proc asFuncType*(t: ExternType): FuncType {.lib, importc: "wasm_externtype_as_functype".}

proc wasm_importtype_module(i: ImportType): ptr Vec[byte] {.lib.}
proc wasm_importtype_name(i: ImportType): ptr Vec[byte] {.lib.}
proc typ*(i: ImportType): ExternType {.lib, importc: "wasm_importtype_type".}

proc wasm_exporttype_name(i: ExportType): ptr Vec[byte] {.lib.}
proc typ*(i: ExportType): ExternType {.lib, importc: "wasm_exporttype_type".}

proc toString(b: Vec[byte]): string =
  let sz = b.size.int
  if sz > 0:
    result.setLen(sz)
    copyMem(addr result[0], b.data, sz)

proc name*(i: ImportType): string = wasm_importtype_name(i)[].toString()
proc module*(i: ImportType): string = wasm_importtype_module(i)[].toString()

proc name*(i: ExportType): string = wasm_exporttype_name(i)[].toString()

declareArray(byte, "byte")
declareArray(Extern, "extern")
declareArray(Val, "val")
declareArray(ImportType, "importtype")
declareArray(ExportType, "exporttype")
declareArray(ValType, "valtype")

proc delete*(v: Vec) {.inline.} = wasm_vec_delete(addr v)
proc delete*(e: Extern) {.lib, importc: "wasm_extern_delete".}

proc version*(): (int, int, int) =
  (wasmer_version_major().int, wasmer_version_minor().int, wasmer_version_patch().int)

proc new*(v: var Vec) {.inline.} = wasm_vec_new_empty(v)
proc new*[T](v: var Vec[T], d: openarray[T]) {.inline.} =
  if d.len == 0: v.new() else: wasm_vec_new(v, d.len.csize_t, addr d[0])

proc newVec*[T](v: openarray[T]): Vec[T] {.inline.} = result.new(v)
proc newVec*(v: string): Vec[byte] {.inline.} = result.new(cast[seq[byte]](v))
proc unsafeVec[T](o: openarray[T]): Vec[T] {.inline.} =
  Vec[T](size: o.len.csize_t, data: cast[ptr UncheckedArray[T]](addr o))

proc wasm_functype_new(p, r: ptr Vec[ValType]): FuncType {.lib.}
proc wasm_functype_params(f: FuncType): ptr Vec[ValType] {.lib.}
proc wasm_functype_results(f: FuncType): ptr Vec[ValType] {.lib.}
proc delete*(f: FuncType) {.lib, importc: "wasm_functype_delete".}


proc newFuncType*(p, r: Vec[ValType]): FuncType {.inline.} = wasm_functype_new(addr p, addr r)
proc newFuncType*(p, r: openarray[ValType]): FuncType =
  newFuncType(newVec(p), newVec(r))

proc params*(f: FuncType): Vec[ValType] {.inline.} = wasm_functype_params(f)[]
proc results*(f: FuncType): Vec[ValType] {.inline.} = wasm_functype_results(f)[]

proc toValKind*(t: typedesc): ValKind {.inline.} =
  when t is int32: i32
  elif t is int64: i64
  elif t is float32: f32
  elif t is float64: f64
  else: {.error: "Unexpected type".}

proc newValType*(k: ValKind): ValType {.lib, importc: "wasm_valtype_new".}
proc newValType*(t: typedesc): ValType {.inline.} = newValType(toValKind(t))
proc kind*(v: ValType): ValKind {.lib, importc: "wasm_valtype_kind".}

proc `$`*(f: FuncType): string =
  result &= "("
  for i, p in f.params:
    if i > 0:
      result &= ", "
    result &= $p.kind
  result &= "): ("
  for i, p in f.results:
    if i > 0:
      result &= ", "
    result &= $p.kind
  result &= ")"

proc wasm_module_new(s: Store, b: ptr Vec[byte]): Module {.lib.}

proc newModule*(s: Store, v: Vec[byte]): Module {.inline.} = wasm_module_new(s, addr v)
proc newModule*(s: Store, v: openarray[byte]): Module {.inline.} = newModule(s, unsafeVec(v))

proc wasm_module_validate(s: Store, b: ptr Vec[byte]): bool {.lib.}
proc validateModule*(s: Store, b: Vec[byte]): bool {.inline.} =
  wasm_module_validate(s, addr b)


proc wasm_module_imports(m: Module, r: var Vec[ImportType]) {.lib.}
proc imports*(m: Module): Vec[ImportType] {.inline.} =
  wasm_module_imports(m, result)
proc wasm_module_exports(m: Module, r: var Vec[ExportType]) {.lib.}
proc exports*(m: Module): Vec[ExportType] {.inline.} =
  wasm_module_exports(m, result)
# WASM_API_EXTERN void wasm_module_exports(const wasm_module_t*, own wasm_exporttype_vec_t* out);

proc wasm_module_serialize(m: Module, b: var Vec[byte]) {.lib.}
proc wasm_module_deserialize(s: Store, b: ptr Vec[byte]): Module {.lib.}

proc serialize*(m: Module, b: var Vec[byte]) {.inline.} = wasm_module_serialize(m, b)
proc deserializeModule*(s: Store, b: Vec[byte]): Module {.inline.} = wasm_module_deserialize(s, addr b)

proc wat2wasm(wat: ptr Vec[byte], wasm: var Vec[byte]) {.lib.}
proc wat2wasm*(wat: Vec[byte]): Vec[byte] {.inline.} = wat2wasm(addr wat, result)
proc wat2wasm*(wat: string): Vec[byte] {.inline.} =
  var v = Vec[byte](size: wat.len.csize_t, data: cast[ptr UncheckedArray[byte]](addr wat[0]))
  wat2wasm(addr v, result)

proc wasm_config_new(): Config {.lib.}
proc newConfig*(): Config {.inline.} = wasm_config_new()

proc newEngine*(c: Config): Engine {.lib, importc: "wasm_engine_new_with_config".}
proc newEngine*(): Engine {.inline.} = newEngine(newConfig())

proc wasm_store_new(e: Engine): Store {.lib.}
proc newStore*(e: Engine): Store {.inline.} = wasm_store_new(e)

proc wasm_instance_new(s: Store, m: Module, imports: ptr Vec[Extern], t: ptr Trap): Instance {.lib.}
proc newInstance*(s: Store, m: Module, imports: Vec[Extern], t: ptr Trap = nil): Instance {.inline.} =
  wasm_instance_new(s, m, addr imports, t)

proc newInstance*(s: Store, m: Module, imports: openarray[Extern], t: ptr Trap = nil): Instance {.inline.} =
  var v = unsafeVec(imports)
  wasm_instance_new(s, m, addr v, t)

proc newInstance*(s: Store, m: Module, imports: TableRef[string, Extern], t: ptr Trap = nil): Instance {.inline.} =
  let importedObjects = m.imports
  var importsArray = newSeq[Extern](importedObjects.len)
  for i, im in importedObjects:
    importsArray[i] = imports[im.name]
  newInstance(s, m, importsArray, t)

proc wasm_instance_exports(i: Instance, e: var Vec[Extern]) {.lib.}
proc exports*(i: Instance): Vec[Extern] {.inline.} =
  wasm_instance_exports(i, result)

proc wasm_extern_as_func(e: Extern): Func {.lib.}
proc asFunc*(e: Extern): Func {.inline.} = wasm_extern_as_func(e)

proc newFunc*(s: Store, typ: FuncType, c: FuncCallback): Func {.lib, importc: "wasm_func_new".}
proc newFunc*(s: Store, typ: FuncType, c: FuncCallbackWithEnv, env: pointer, f: proc(p: pointer){.cdecl.}): Func {.lib, importc: "wasm_func_new_with_env".}
proc newFuncConsumingType(s: Store, typ: FuncType, c: FuncCallback): Func =
  result = newFunc(s, typ, c)
  typ.delete()

proc typ*(f: Func): FuncType {.lib, importc: "wasm_func_type".}

proc wasm_func_param_arity(f: Func): csize_t {.lib.}
proc wasm_func_result_arity(f: Func): csize_t {.lib.}
proc paramArity*(f: Func): int {.inline.} = wasm_func_param_arity(f).int
proc resultArity*(f: Func): int {.inline.} = wasm_func_result_arity(f).int

proc wasm_func_call(f: Func, args: ptr Vec[Val], results: var Vec[Val]): Trap {.lib.}
proc call*(f: Func, args: Vec[Val], results: var Vec[Val]): Trap {.inline.} =
  wasm_func_call(f, addr args, results)

proc call*(f: Func, args: openarray[Val], results: var openarray[Val]): Trap {.inline.} =
  var argss = unsafeVec(args)
  var resultss = unsafeVec(results)
  wasm_func_call(f, addr argss, resultss)

proc call*(f: Func, args: openarray[Val], results: var Val): Trap {.inline.} =
  var argss = unsafeVec(args)
  var resultss = Vec[Val](size: 1, data: cast[ptr UncheckedArray[Val]](addr results))
  wasm_func_call(f, addr argss, resultss)

iterator arguments(formalParams: NimNode): tuple[idx: int, name, typ, default: NimNode] =
  proc stripSinkFromArgType(t: NimNode): NimNode =
    result = t
    if result.kind == nnkBracketExpr and result.len == 2 and result[0].kind == nnkSym and $result[0] == "sink":
      result = result[1]

  formalParams.expectKind(nnkFormalParams)
  var iParam = 0
  for i in 1 ..< formalParams.len:
    let pp = formalParams[i]
    for j in 0 .. pp.len - 3:
      yield (iParam, pp[j], copyNimTree(stripSinkFromArgType(pp[^2])), pp[^1])
      inc iParam

proc get(v: Val, t: typedesc): auto =
  when t is int32:
    v.i32
  elif t is int64:
    v.i64
  elif t is float32:
    v.f32
  elif t is float64:
    v.f64
  else:
    {.error: "Unknown param type".}

proc set[T](v: var Val, d: T) =
  when T is int32:
    v = vali32(d)
  elif T is int64:
    v = vali64(d)
  elif T is float32:
    v = valf32(d)
  elif (T is float64) or (T is float):
    v = valf64(d)
  else:
    {.error: "Unknown param type".}

template getThunkArg(params: ptr Vec[Val], i: int, t: typedesc): untyped =
  get(params[][i], t)

template thunkBody(call: untyped, results: ptr Vec[Val]) =
  when typeof(call) is void:
    call
  else:
    block:
      let r = call
      if results[].len > 0:
        set(results.data[0], r)

# macro newFuncWithoutEnvAux(s: Store, c: proc): untyped =
#   let t = getType(c)
#   let thunkName = genSym(nskProc, "thunk")

#   let paramsIdent = ident"params"
#   let call = newCall(c)
#   let paramTypeArray = newNimNode(nnkBracket)
#   for i in 2 ..< t.len:
#     let tt = newCall("typeof", t[i])
#     call.add(newCall(bindSym"getThunkArg", paramsIdent, newLit(i - 2), tt))
#     paramTypeArray.add(newCall(bindSym"newValType", tt))

#   let retTypeArray = newNimNode(nnkBracket)
#   let retType = t[1]
#   if $retType != "void":
#     retTypeArray.add(newCall(bindSym"newValType", newCall("typeof", retType)))

#   result = quote do:
#     proc `thunkName`(`paramsIdent`, results: ptr Vec[Val]): Trap {.cdecl.} =
#       thunkBody(`call`, results)

#     newFuncConsumingType(`s`, newFuncType(`paramTypeArray`, `retTypeArray`), `thunkName`)

type
  ClosureWrapper[T] = ref object
    c: T

proc finalizeCW[T](e: pointer) {.cdecl.} =
  let e = cast[ClosureWrapper[T]](e)
  GC_unref(e)

proc newFuncConsumingType[T](s: Store, typ: FuncType, c: FuncCallbackWithEnv, p: T): Func =
  let cw = ClosureWrapper[T](c: p)
  GC_ref(cw)
  result = newFunc(s, typ, c, cast[pointer](cw), finalizeCW[T])
  typ.delete()

macro newFuncWithEnvAux(s: Store, c: proc): untyped =
  let t = getType(c)
  let thunkName = genSym(nskProc, "thunk")

  let envIdent = ident"env"
  let paramsIdent = ident"params"
  let call = newCall(newDotExpr(envIdent, ident"c"))
  let paramTypeArray = newNimNode(nnkBracket)
  for i in 2 ..< t.len:
    let tt = newCall("typeof", t[i])
    call.add(newCall(bindSym"getThunkArg", paramsIdent, newLit(i - 2), tt))
    paramTypeArray.add(newCall(bindSym"newValType", tt))

  let retTypeArray = newNimNode(nnkBracket)
  let retType = t[1]
  if $retType != "void":
    retTypeArray.add(newCall(bindSym"newValType", newCall("typeof", retType)))

  result = quote do:
    type CBType = typeof(`c`)
    proc `thunkName`(`envIdent`: pointer, `paramsIdent`, results: ptr Vec[Val]): Trap {.cdecl.} =
      let `envIdent` = cast[ClosureWrapper[CBType]](`envIdent`)
      thunkBody(`call`, results)

    newFuncConsumingType(`s`, newFuncType(`paramTypeArray`, `retTypeArray`), `thunkName`, `c`)

proc newFunc*(s: Store, c: proc): Func =
  newFuncWithEnvAux(s, c)

proc asExtern*(f: Func): Extern {.lib, importc: "wasm_func_as_extern"}

template makeImportsAux(imports: varargs[(string, Extern)]): TableRef[string, Extern] =
  imports.newTable()

proc makeImportFuncAux(store: Store, name: string, cb: proc): (string, Extern) =
  (name, newFunc(store, cb).asExtern)

macro makeImports*(store: Store, imports: untyped{nkTableConstr}): untyped =
  result = newCall(bindSym"makeImportsAux")
  for n in imports:
    n.expectKind(nnkExprColonExpr)
    result.add(newCall(bindSym"makeImportFuncAux", `store`, n[0], n[1]))

proc getExport*(instance: Instance, module: Module, name: string): Extern =
  let et = module.exports()
  for i, e in et:
    if e.name == name:
      return instance.exports()[i]

proc toArg[T](v: T): Val {.inline.} =
  result.set(v)

template setResult(v: untyped) =
  when result isnot (proc):
    result = get(v, typeof(result))

proc raiseTrap(t: Trap) =
  raise newException(ValueError, "Wasm trap")

macro makeExportWrapper(procType: typedesc, f: untyped): untyped =
  let argsArray = newNimNode(nnkBracket)
  let ty = getTypeImpl(procType)
  ty.expectKind(nnkBracketExpr)
  ty[1].expectKind(nnkProcTy)
  let srcParams = ty[1][0]
  var params: seq[NimNode]
  params.add(srcParams[0])
  for i, name, typ, _ in arguments(srcParams):
    let id = ident("arg" & $i)
    params.add newIdentDefs(id, typ)
    argsArray.add(newCall(bindSym"toArg", id))

  let body = quote do:
    var r: Val
    let t = `f`.call(`argsArray`, r)
    if not t.isNil:
      raiseTrap(t)
    setResult(r)

  result = newProc(params = params, procType = nnkLambda, body = body)

proc getExport*(instance: Instance, module: Module, name: string, procType: typedesc): procType =
  let e = instance.getExport(module, name)
  if not e.isNil:
    let f = e.asFunc()
    result = makeExportWrapper(procType, f)
