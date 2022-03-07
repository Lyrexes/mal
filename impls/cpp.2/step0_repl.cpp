#include <iostream>
#include <readline/readline.h>
#include <readline/history.h>
#include <optional>

std::optional<std::string> READ(std::string_view prompt);
std::string EVAL (std::string ast);
std::string PRINT (std::string arg);
std::string REP(std::string arg);

int main(void) {
    while (auto input = READ("user> ")) {
        std::cout << REP(*input) << std::endl;
    }
    return 0;
}

std::string REP(std::string arg) {
    return PRINT(EVAL(arg));
}

std::optional<std::string> READ(std::string_view prompt) {
    const auto line = readline(prompt.data());
    std::optional<std::string> result = {};
    if(line != nullptr) 
        result = std::string(line);
    free(line);
    if (result)
        add_history(result.value().c_str());
    return result;
}

std::string EVAL (std::string arg) {
    return arg;
}

std::string PRINT (std::string arg) {
    return arg;
}
