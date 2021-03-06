source readline.vim
source types.vim
source reader.vim
source printer.vim
source env.vim
source core.vim

let MalExceptionObj = ""

function READ(str)
  return ReadStr(a:str)
endfunction

function StartsWith(ast, sym)
  if EmptyQ(a:ast)
     return 0
  endif
  let fst = ListFirst(a:ast)
  return SymbolQ(fst) && fst.val == a:sym
endfunction

function QuasiquoteLoop(xs)
  let revlist = reverse(copy(a:xs))
  let acc = ListNew([])
  for elt in revlist
     if ListQ(elt) && StartsWith(elt, "splice-unquote")
       let acc = ListNew([SymbolNew("concat"), ListNth(elt, 1), acc])
     else
       let acc = ListNew([SymbolNew("cons"), Quasiquote(elt), acc])
     endif
   endfor
   return acc
 endfunction

function Quasiquote(ast)
  if VectorQ(a:ast)
    return ListNew([SymbolNew("vec"), QuasiquoteLoop(a:ast.val)])
  elseif SymbolQ(a:ast) || HashQ(a:ast)
    return ListNew([SymbolNew("quote"), a:ast])
  elseif !ListQ(a:ast)
    return a:ast
  elseif StartsWith(a:ast, "unquote")
    return ListNth(a:ast, 1)
  else
    return QuasiquoteLoop(a:ast.val)
  endif
endfunction

function IsMacroCall(ast, env)
  if !ListQ(a:ast)
    return 0
  endif
  let a0 = ListFirst(a:ast)
  if !SymbolQ(a0)
    return 0
  endif
  let macroname = a0.val
  if empty(a:env.find(macroname))
    return 0
  endif
  return MacroQ(a:env.get(macroname))
endfunction

function MacroExpand(ast, env)
  let ast = a:ast
  while IsMacroCall(ast, a:env)
    let macroobj = a:env.get(ListFirst(ast).val)
    let macroargs = ListRest(ast)
    let ast = FuncInvoke(macroobj, macroargs)
  endwhile
  return ast
endfunction

function EvalAst(ast, env)
  if SymbolQ(a:ast)
    let varname = a:ast.val
    return a:env.get(varname)
  elseif ListQ(a:ast)
    return ListNew(map(copy(a:ast.val), {_, e -> EVAL(e, a:env)}))
  elseif VectorQ(a:ast)
    return VectorNew(map(copy(a:ast.val), {_, e -> EVAL(e, a:env)}))
  elseif HashQ(a:ast)
    let ret = {}
    for [k,v] in items(a:ast.val)
      let newval = EVAL(v, a:env)
      let ret[k] = newval
    endfor
    return HashNew(ret)
  else
    return a:ast
  end
endfunction

function GetCatchClause(ast)
  if ListCount(a:ast) < 3
    return ""
  end
  let catch_clause = ListNth(a:ast, 2)
  if ListFirst(catch_clause) == SymbolNew("catch*")
    return catch_clause
  else
    return ""
  end
endfunction

function EVAL(ast, env)
  let ast = a:ast
  let env = a:env

  while 1
    if !ListQ(ast)
      return EvalAst(ast, env)
    end

    let ast = MacroExpand(ast, env)
    if !ListQ(ast)
      return EvalAst(ast, env)
    end
    if EmptyQ(ast)
      return ast
    endif

    let first = ListFirst(ast)
    let first_symbol = SymbolQ(first) ? first.val : ""
    if first_symbol == "def!"
      let a1 = ast.val[1]
      let a2 = ast.val[2]
      return env.set(a1.val, EVAL(a2, env))
    elseif first_symbol == "let*"
      let a1 = ast.val[1]
      let a2 = ast.val[2]
      let env = NewEnv(env)
      let let_binds = a1.val
      let i = 0
      while i < len(let_binds)
        call env.set(let_binds[i].val, EVAL(let_binds[i+1], env))
        let i = i + 2
      endwhile
      let ast = a2
      " TCO
    elseif first_symbol == "quote"
      return ListNth(ast, 1)
    elseif first_symbol == "quasiquoteexpand"
      return Quasiquote(ListNth(ast, 1))
    elseif first_symbol == "quasiquote"
      let ast = Quasiquote(ListNth(ast, 1))
      " TCO
    elseif first_symbol == "defmacro!"
      let a1 = ListNth(ast, 1)
      let a2 = ListNth(ast, 2)
      let macro = MarkAsMacro(EVAL(a2, env))
      return env.set(a1.val, macro)
    elseif first_symbol == "macroexpand"
      return MacroExpand(ListNth(ast, 1), env)
    elseif first_symbol == "if"
      let condvalue = EVAL(ast.val[1], env)
      if FalseQ(condvalue) || NilQ(condvalue)
        if len(ast.val) < 4
          return g:MalNil
        else
          let ast = ast.val[3]
        endif
      else
        let ast = ast.val[2]
      endif
      " TCO
    elseif first_symbol == "try*"
      try
        return EVAL(ListNth(ast, 1), env)
      catch
        let catch_clause = GetCatchClause(ast)
        if empty(catch_clause)
          throw v:exception
        endif

        let exc_var = ListNth(catch_clause, 1).val
        if v:exception == "__MalException__"
          let exc_value = g:MalExceptionObj
        else
          let exc_value = StringNew(v:exception)
        endif
        let catch_env = NewEnvWithBinds(env, ListNew([SymbolNew(exc_var)]), ListNew([exc_value]))
        return EVAL(ListNth(catch_clause, 2), catch_env)
      endtry
    elseif first_symbol == "do"
      let astlist = ast.val
      call EvalAst(ListNew(astlist[1:-2]), env)
      let ast = astlist[-1]
      " TCO
    elseif first_symbol == "fn*"
      let fn = NewFn(ListNth(ast, 2), env, ListNth(ast, 1))
      return fn
    elseif first_symbol == "eval"
      let ast = EVAL(ListNth(ast, 1), env)
      let env = env.root()
      " TCO
    else
      " apply list
      let el = EvalAst(ast, env)
      let funcobj = ListFirst(el)
      let args = ListRest(el)
      if NativeFunctionQ(funcobj)
        return NativeFuncInvoke(funcobj, args)
      elseif FunctionQ(funcobj)
        let fn = funcobj.val
        let ast = fn.ast
        let env = NewEnvWithBinds(fn.env, fn.params, args)
        " TCO
      else
        throw "Not a function"
      endif
    endif
  endwhile
endfunction

function PRINT(exp)
  return PrStr(a:exp, 1)
endfunction

function RE(str, env)
  return EVAL(READ(a:str), a:env)
endfunction

function REP(str, env)
  return PRINT(EVAL(READ(a:str), a:env))
endfunction

function GetArgvList()
  return ListNew(map(copy(argv()[1:]), {_, arg -> StringNew(arg)}))
endfunction

set maxfuncdepth=10000
let repl_env = NewEnv("")

for [k, v] in items(CoreNs)
  call repl_env.set(k, v)
endfor

call repl_env.set("*ARGV*", GetArgvList())

call RE("(def! not (fn* (a) (if a false true)))", repl_env)
call RE("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))", repl_env)
call RE("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))", repl_env)

if !empty(argv())
  try
    call RE('(load-file "' . argv(0) . '")', repl_env)
  catch
    call PrintLn("Error: " . v:exception)
  endtry
  qall!
endif

while 1
  let [eof, line] = Readline("user> ")
  if eof
    break
  endif
  if line == ""
    continue
  endif
  try
    call PrintLn(REP(line, repl_env))
  catch
    if v:exception == "__MalException__"
      call PrintLn("Error: " . PrStr(g:MalExceptionObj, 1))
    else
      call PrintLn("Error: " . v:exception)
    end
  endtry
endwhile
qall!
