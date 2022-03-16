#include <bits/enable_special_members.h>
#include <iostream>
#include <readline/readline.h>
#include <readline/history.h>
#include <optional>
#include "env.hpp"
#include "reader.hpp"
#include "printer.hpp"
#include "types.hpp"
#include "eval.hpp"
#include "core.hpp"
// #define NDEBUG

using namespace Types;
using Env = Environment;
using EnvPtr = Env::EnvPtr;
std::optional<std::string> INPUT(std::string_view prompt);
MalType READ(std::string input, const char* regex);
MalType EVAL(MalType ast, EnvPtr env);
std::string PRINT(const MalType& result, bool readably);
std::string REP(std::string arg, const char* regex, bool readably, EnvPtr env);
EnvPtr init_repl_env();
bool handle_args(int argc, char* argv[], EnvPtr env);

int main(int argc, char *argv[]) {
    auto repl_env = init_repl_env();
    if(handle_args(argc, argv, repl_env))
       return 0;
    while (auto input = INPUT("user> ")) {
        try {
            std::cout << REP(std::move(*input), Core::regex, true, repl_env) << '\n';
        } catch (const std::exception &e) {
            std::cout << "EOF" << " | Error: " << e.what() << '\n';
        }
    }
    return 0;
}

std::string REP(std::string arg, const char* regex, bool readably, EnvPtr env) {
    return PRINT(EVAL(READ(std::move(arg), regex), env), readably);
}

MalType READ(std::string input, const char* regex) {
    return Parser::read_str(std::move(input), regex);
}

std::optional<std::string> INPUT(std::string_view prompt) {
    const auto line = readline(prompt.data());
    std::optional<std::string> result = {};
    if(line != nullptr) 
        result = std::string(line);
    free(line);
    if (result)
        add_history(result.value().c_str());
    return result;
}

std::string PRINT(const MalType &result, bool readably) {
    return Printer::pr_str(result, readably);
}

MalType EVAL(MalType ast, EnvPtr env) {
    return Eval::eval(ast, env);
}

EnvPtr init_repl_env() {
    auto repl_env = std::make_shared<Env>(Env::Maybe<EnvPtr>{}, Core::core_namespace());
    repl_env->set("eval", Builtin([repl_env](Core::Args args, EnvPtr env) {
        return Eval::eval(args[0], repl_env);
    }));
    auto load_file = "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))";
    auto not_code = "(def! not (fn* (a) (if a false true)))";
    auto cond_code = std::string("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs)")
    + " (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\"))"
    + " (cons 'cond (rest (rest xs)))))))";
    REP(not_code, Core::regex, true, repl_env);
    REP(load_file, Core::regex, true, repl_env);
    REP("(def! *ARGV* (list))", Core::regex, true, repl_env);
    REP(cond_code, Core::regex, true, repl_env);
    return repl_env;
}

bool handle_args(int argc, char* argv[], EnvPtr env) {
    if(argc <= 1)
        return false;
    auto args = std::string{"(list "};
    for(int i = 2; i < argc; ++i)
        args += "\"" + std::string(argv[i]) + "\"" + " ";
    args.back() = ')';
    try {
        if(args.size() <= 1)
            REP("(def! *ARGV* (list))", Core::regex, true, env);
        else
            REP("(def! *ARGV* " + args + ")", Core::regex, true, env);
        REP(std::string("(load-file ") + "\"" + argv[1] + "\"" + ")", Core::regex, true, env);
    } catch(const std::exception& e) {
        std::cout << "EOF" << " | Error in file: " << e.what() << '\n';
    }
    return true;
}