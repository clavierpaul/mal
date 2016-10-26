" types module

function ObjNewWithMeta(obj_type, obj_val, obj_meta)
  return {"type": a:obj_type, "val": a:obj_val, "meta": a:obj_meta}
endfunction

function ObjNew(obj_type, obj_val)
  return {"type": a:obj_type, "val": a:obj_val}
endfunction

function ObjType(obj)
  return a:obj["type"]
endfunction

function ObjHasMeta(obj)
  return ObjQ(a:obj) && has_key(a:obj, "meta")
endfunction

function ObjMeta(obj)
  return ObjHasMeta(a:obj) ? a:obj["meta"] : g:MalNil
endfunction

function ObjSetValue(obj, newval)
  let a:obj["val"] = a:newval
  return a:newval
endfunction

function ObjSetMeta(obj, newmeta)
  let a:obj["meta"] = a:newmeta
  return a:newmeta
endfunction

function ObjQ(obj)
  return type(a:obj) == type({})
endfunction

function SymbolQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "symbol"
endfunction

function StringQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "string"
endfunction

function KeywordQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "keyword"
endfunction

function AtomQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "atom"
endfunction

function NilQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "nil"
endfunction

function TrueQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "true"
endfunction

function FalseQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "false"
endfunction

function IntegerQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "integer"
endfunction

function FloatQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "float"
endfunction

function ListQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "list"
endfunction

function VectorQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "vector"
endfunction

function SequentialQ(obj)
  return ObjQ(a:obj) && ListQ(a:obj) || VectorQ(a:obj)
endfunction

function HashQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "hash"
endfunction

function FunctionQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "function" && !a:obj.val.is_macro
endfunction

function MacroQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "function" && a:obj.val.is_macro
endfunction

function NativeFunctionQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "nativefunction"
endfunction

function NilNew()
  return ObjNew("nil", "")
endfunction

function TrueNew()
  return ObjNew("true", "")
endfunction

function FalseNew()
  return ObjNew("false", "")
endfunction

function BoolNew(bool)
  return a:bool ? g:MalTrue : g:MalFalse
endfunction

function KeywordNew(val)
  return ObjNew("keyword", a:val)
endfunction

function AtomNew(val)
  return ObjNewWithMeta("atom", a:val, g:MalNil)
endfunction

function SymbolNew(val)
  return ObjNew("symbol", a:val)
endfunction

function StringNew(val)
  return ObjNew("string", a:val)
endfunction

function IntegerNew(val)
  return ObjNew("integer", a:val)
endfunction

function FloatNew(val)
  return ObjNew("float", a:val)
endfunction

function ListNew(val)
  return ObjNewWithMeta("list", a:val, g:MalNil)
endfunction

function VectorNew(val)
  return ObjNewWithMeta("vector", a:val, g:MalNil)
endfunction

function HashNew(val)
  return ObjNewWithMeta("hash", a:val, g:MalNil)
endfunction

function HashMakeKey(obj)
  if !StringQ(a:obj) && !KeywordQ(a:obj)
    throw "expected hash-map key string, got: " . ObjType(a:obj));
  endif
  return ObjType(a:obj) . "#" . a:obj.val
endfunction

function HashParseKey(str)
  if a:str =~ "^string#"
    return StringNew(a:str[7:])
  elseif a:str =~ "^keyword#"
    return KeywordNew(a:str[8:])
  endif
endfunction

function HashBuild(elements)
  if (len(a:elements) % 2) != 0
    throw "Odd number of hash-map arguments"
  endif
  let i = 0
  let hash = {}
  while i < len(a:elements)
    let key = a:elements[i]
    let val = a:elements[i + 1]
    let keystring = HashMakeKey(key)
    let hash[keystring] = val
    let i = i + 2
  endwhile
  return HashNew(hash)
endfunction

function HashEqualQ(x, y)
  if len(a:x.val) != len(a:y.val)
    return 0
  endif
  for k in keys(a:x.val)
    let vx = a:x.val[k]
    let vy = a:y.val[k]
    if empty(vy) || !EqualQ(vx, vy)
      return 0
    endif
  endfor
  return 1
endfunction

function SequentialEqualQ(x, y)
  if len(a:x.val) != len(a:y.val)
    return 0
  endif
  let i = 0
  while i < len(a:x.val)
    let ex = a:x.val[i]
    let ey = a:y.val[i]
    if !EqualQ(ex, ey)
      return 0
    endif
    let i = i +1
  endwhile
  return 1
endfunction

function EqualQ(x, y)
  if SequentialQ(a:x) && SequentialQ(a:y)
    return SequentialEqualQ(a:x, a:y)
  elseif HashQ(a:x) && HashQ(a:y)
    return HashEqualQ(a:x, a:y)
  elseif ObjType(a:x) != ObjType(a:y)
    return 0
  else
    return a:x.val == a:y.val
  endif
endfunction

function EmptyQ(list)
  return empty(a:list.val)
endfunction

function ListCount(list)
  return len(a:list.val)
endfunction

function ListNth(list, index)
  if a:index >= len(a:list.val)
    throw "nth: index out of range"
  endif
  return a:list.val[a:index]
endfunction

function ListFirst(list)
  return get(a:list.val, 0, g:MalNil)
endfunction

function ListDrop(list, drop_elements)
  return ListNew(a:list.val[a:drop_elements :])
endfunction

function ListRest(list)
  return ListDrop(a:list, 1)
endfunction

function FuncInvoke(funcobj, args)
  let fn = a:funcobj.val
  let funcenv = NewEnvWithBinds(fn.env, fn.params, a:args)
  return EVAL(fn.ast, funcenv)
endfunction

function NativeFuncInvoke(funcobj, argslist)
  let fn = a:funcobj.val
  return fn.Func(a:argslist.val)
endfunction

function MarkAsMacro(funcobj)
  let fn = a:funcobj.val
  let fn.is_macro = 1
  return a:funcobj
endfunction

function NewFn(ast, env, params)
  let fn = {"ast": a:ast, "env": a:env, "params": a:params, "is_macro": 0}
  return ObjNewWithMeta("function", fn, g:MalNil)
endfunction

function NewNativeFn(funcname)
  let fn = {"Func": function(a:funcname), "name": a:funcname}
  return ObjNewWithMeta("nativefunction", fn, g:MalNil)
endfunction

function NewNativeFnLambda(lambdaexpr)
  let fn = {"Func": a:lambdaexpr, "name": "inline"}
  return ObjNewWithMeta("nativefunction", fn, g:MalNil)
endfunction

let g:MalNil = NilNew()
let g:MalTrue = TrueNew()
let g:MalFalse = FalseNew()
