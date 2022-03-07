#include <iostream>
#include <readline/readline.h>
#include <readline/history.h>
#include <optional>
#include "reader.hpp"
#include "printer.hpp"

std::optional<std::string> INPUT(std::string_view prompt);
Types::MalType READ(std::string input, const char* regex);
Types::MalType EVAL(Types::MalType ast);
std::string PRINT(const Types::MalType& result, bool readably);
std::string REP(std::string arg, const char* regex, bool readably);

int main() {
    auto regex = R"([\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*))";
    auto readably = true;
    while (auto input = INPUT("user> ")) {
        try {
            std::cout << REP(std::move(*input), regex, readably) << '\n';
        } catch (const std::runtime_error &e) {
            std::cout << "EOF" << " | Error: " << e.what() << '\n';
        }
        catch (const char* e) {} //comment exception
    }
    return 0;
}

std::string REP(std::string arg, const char* regex, bool readably) {
    return PRINT(EVAL(READ(std::move(arg), regex)), readably);
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

Types::MalType EVAL(Types::MalType ast) {
    return ast;
}

std::string PRINT(const Types::MalType &result, bool readably) {
    return Printer::pr_str(result, readably);
}
