import rdstdin, tables, sequtils, os, types, reader, printer, env, core

proc read(str: string): MalType = str.read_str

proc quasiquote(ast: MalType): MalType

proc quasiquote_loop(xs: seq[MalType]): MalType =
  result = list()
  for i in countdown(xs.high, 0):
    var elt = xs[i]
    if elt.kind == List and 0 < elt.list.len and elt.list[0] == symbol "splice-unquote":
      result = list(symbol "concat", elt.list[1], result)
    else:
      result = list(symbol "cons", quasiquote(elt), result)

proc quasiquote(ast: MalType): MalType =
  case ast.kind
  of List:
    if ast.list.len == 2 and ast.list[0] == symbol "unquote":
      result = ast.list[1]
    else:
      result = quasiquote_loop(ast.list)
  of Vector:
    result = list(symbol "vec", quasiquote_loop(ast.list))
  of Symbol:
    result = list(symbol "quote", ast)
  of HashMap:
    result = list(symbol "quote", ast)
  else:
    result = ast

proc is_macro_call(ast: MalType, env: Env): bool =
  ast.kind == List and ast.list.len > 0 and ast.list[0].kind == Symbol and
    env.find(ast.list[0].str) != nil and env.get(ast.list[0].str).fun_is_macro

proc macroexpand(ast: MalType, env: Env): MalType =
  result = ast
  while result.is_macro_call(env):
    let mac = env.get(result.list[0].str)
    result = mac.malfun.fn(result.list[1 .. ^1]).macroexpand(env)

proc eval(ast: MalType, env: Env): MalType

proc eval_ast(ast: MalType, env: var Env): MalType =
  case ast.kind
  of Symbol:
    result = env.get(ast.str)
  of List:
    result = list ast.list.mapIt(it.eval(env))
  of Vector:
    result = vector ast.list.mapIt(it.eval(env))
  of HashMap:
    result = hash_map()
    for k, v in ast.hash_map.pairs:
      result.hash_map[k] = v.eval(env)
  else:
    result = ast

proc eval(ast: MalType, env: Env): MalType =
  var ast = ast
  var env = env

  template defaultApply =
    let el = ast.eval_ast(env)
    let f = el.list[0]
    case f.kind
    of MalFun:
      ast = f.malfun.ast
      env = initEnv(f.malfun.env, f.malfun.params, list(el.list[1 .. ^1]))
    else:
      return f.fun(el.list[1 .. ^1])

  while true:
    if ast.kind != List: return ast.eval_ast(env)

    ast = ast.macroexpand(env)
    if ast.kind != List: return ast.eval_ast(env)
    if ast.list.len == 0: return ast

    let a0 = ast.list[0]
    case a0.kind
    of Symbol:
      case a0.str
      of "def!":
        let
          a1 = ast.list[1]
          a2 = ast.list[2]
          res = a2.eval(env)
        return env.set(a1.str, res)

      of "let*":
        let
          a1 = ast.list[1]
          a2 = ast.list[2]
        var let_env = initEnv(env)
        case a1.kind
        of List, Vector:
          for i in countup(0, a1.list.high, 2):
            let_env.set(a1.list[i].str, a1.list[i+1].eval(let_env))
        else: raise newException(ValueError, "Illegal kind in let*")
        ast = a2
        env = let_env
        # Continue loop (TCO)

      of "quote":
        return ast.list[1]

      of "quasiquoteexpand":
        return ast.list[1].quasiquote

      of "quasiquote":
        ast = ast.list[1].quasiquote
        # Continue loop (TCO)

      of "defmacro!":
        var fun = ast.list[2].eval(env)
        fun = malfun(fun.malfun.fn, fun.malfun.ast, fun.malfun.params, fun.malfun.env, true)
        return env.set(ast.list[1].str, fun)

      of "macroexpand":
        return ast.list[1].macroexpand(env)

      of "try*":
        let a1 = ast.list[1]
        if ast.list.len <= 2:
            return a1.eval(env)
        let a2 = ast.list[2]
        if a2.list[0].str == "catch*":
          try:
            return a1.eval(env)
          except MalError:
            let exc = (ref MalError) getCurrentException()
            var catchEnv = initEnv(env, list a2.list[1], exc.t)
            return a2.list[2].eval(catchEnv)
          except:
            let exc = getCurrentExceptionMsg()
            var catchEnv = initEnv(env, list a2.list[1], list str(exc))
            return a2.list[2].eval(catchEnv)
        else:
          return a1.eval(env)

      of "do":
        let last = ast.list.high
        discard (list ast.list[1 ..< last]).eval_ast(env)
        ast = ast.list[last]
        # Continue loop (TCO)

      of "if":
        let
          a1 = ast.list[1]
          a2 = ast.list[2]
          cond = a1.eval(env)

        if cond.kind in {Nil, False}:
          if ast.list.len > 3: ast = ast.list[3]
          else: ast = nilObj
        else: ast = a2

      of "fn*":
        let
          a1 = ast.list[1]
          a2 = ast.list[2]
        var env2 = env
        let fn = proc(a: varargs[MalType]): MalType =
          var newEnv = initEnv(env2, a1, list(a))
          a2.eval(newEnv)
        return malfun(fn, a2, a1, env)

      else: defaultApply()

    else: defaultApply()

proc print(exp: MalType): string = exp.pr_str

var repl_env = initEnv()

for k, v in ns.items:
  repl_env.set(k, v)
repl_env.set("eval", fun(proc(xs: varargs[MalType]): MalType = eval(xs[0], repl_env)))
var ps = commandLineParams()
repl_env.set("*ARGV*", list((if paramCount() > 1: ps[1..ps.high] else: @[]).map(str)))


# core.nim: defined using nim
proc rep(str: string): string {.discardable.} =
  str.read.eval(repl_env).print

# core.mal: defined using mal itself
rep "(def! not (fn* (a) (if a false true)))"
rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))"
rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"
rep "(def! *host-language* \"nim\")"

if paramCount() >= 1:
  rep "(load-file \"" & paramStr(1) & "\")"
  quit()

rep "(println (str \"Mal [\" *host-language* \"]\"))"

while true:
  try:
    let line = readLineFromStdin("user> ")
    echo line.rep
  except Blank: discard
  except IOError: quit()
  except MalError:
    let exc = (ref MalError) getCurrentException()
    echo "Error: " & exc.t.list[0].pr_str
  except:
    stdout.write "Error: "
    echo getCurrentExceptionMsg()
    echo getCurrentException().getStackTrace()
