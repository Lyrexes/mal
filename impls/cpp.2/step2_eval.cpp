#include <iostream>
#include <readline/readline.h>
#include <readline/history.h>
#include <optional>
#include "reader.hpp"
#include "printer.hpp"
#include "types.hpp"
#include "env.hpp"
#include "eval.hpp"

using MalType = Types::MalType;
using Env = Environment;
std::optional<std::string> INPUT(std::string_view prompt);
Types::MalType READ(std::string input, const char* regex);
Types::MalType EVAL(Types::MalType ast, Env& env);
std::string PRINT(const Types::MalType& result, bool readably);
std::string REP(std::string arg, const char* regex, bool readably, Env& env);
MalType eval_ast(const Types::MalType& t, Env& env);

int main() {
    auto regex = R"([\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*))";
    auto readably = true;
    auto repl_env = Environment({}, {});
    repl_env.set("+", Builtin(Types::add_num));
    repl_env.set("-", Builtin(Types::sub_num));
    repl_env.set("*", Builtin(Types::mul_num));
    repl_env.set("/", Builtin(Types::div_num));
    while (auto input = INPUT("user> ")) {
        try {
            std::cout << REP(std::move(*input), regex, readably, repl_env) << '\n';
        } catch (const std::exception &e) {
            std::cout << "EOF" << " | Error: " << e.what() << '\n';
        } catch (const char* e) {} //comment exception
    }
    return 0;
}

std::string REP(std::string arg, const char* regex, bool readably, Env& env) {
    return PRINT(EVAL(READ(std::move(arg), regex), env), readably);
}

Types::MalType READ(std::string input, const char* regex) {
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

std::string PRINT(const Types::MalType &result, bool readably) {
    return Printer::pr_str(result, readably);
}

Types::MalType EVAL(Types::MalType ast, Env& env) {
    return Eval::eval(ast, env);
}
