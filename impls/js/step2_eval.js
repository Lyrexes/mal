if (typeof module !== 'undefined') {
    var types = require('./types');
    var readline = require('./node_readline');
    var reader = require('./reader');
    var printer = require('./printer');
}

// read
function READ(str) {
    return reader.read_str(str);
}

// eval
function eval_ast(ast, env) {
    if (types._symbol_Q(ast)) {
        if (ast in env) {
            return env[ast];
        } else {
            throw new Error("'" + ast.value + "' not found");
        }
    } else if (types._list_Q(ast)) {
        return ast.map(function(a) { return EVAL(a, env); });
    } else if (types._vector_Q(ast)) {
        var v = ast.map(function(a) { return EVAL(a, env); });
        v.__isvector__ = true;
        return v;
    } else if (types._hash_map_Q(ast)) {
        var new_hm = {};
        for (k in ast) {
            new_hm[k] = EVAL(ast[k], env);
        }
        return new_hm;
    } else {
        return ast;
    }
}

function _EVAL(ast, env) {
    //printer.println("EVAL:", printer._pr_str(ast, true));
    if (!types._list_Q(ast)) {
        return eval_ast(ast, env);
    }
    if (ast.length === 0) {
        return ast;
    }

    // apply list
    var el = eval_ast(ast, env), f = el[0];
    return f.apply(f, el.slice(1));
}

function EVAL(ast, env) {
    var result = _EVAL(ast, env);
    return (typeof result !== "undefined") ? result : null;
}

// print
function PRINT(exp) {
    return printer._pr_str(exp, true);
}

// repl
repl_env = {};
var rep = function(str) { return PRINT(EVAL(READ(str), repl_env)); };

repl_env['+'] = function(a,b){return a+b;};
repl_env['-'] = function(a,b){return a-b;};
repl_env['*'] = function(a,b){return a*b;};
repl_env['/'] = function(a,b){return a/b;};

// repl loop
if (typeof require !== 'undefined' && require.main === module) {
    // Synchronous node.js commandline mode
    while (true) {
        var line = readline.readline("user> ");
        if (line === null) { break; }
        try {
            if (line) { printer.println(rep(line)); }
        } catch (exc) {
            if (exc instanceof reader.BlankException) { continue }
            if (exc instanceof Error) { console.warn(exc.stack) }
            else { console.warn("Error: " + printer._pr_str(exc, true)) }
        }
    }
}
