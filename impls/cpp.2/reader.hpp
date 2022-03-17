#ifndef READER_HPP
#define READER_HPP
#include <string>
#include <span>
#include <iostream>
#include <regex>
#include <vector>
#include <exception>
#include <stdexcept>
#include <optional>

class MalType;
enum class Type;

namespace Parser {
    using Tokens  = std::vector<std::string>;
    using Table  = std::map<std::string, std::string>;
    class Reader {
        public:
            explicit Reader(Tokens &&tokens);
            void pop();
            [[nodiscard]] std::string next();
            [[nodiscard]] std::string_view peek() const;
            [[nodiscard]] bool eof() const noexcept { return tokens.empty(); }
        public:
            const static Table macros;
        private:
            Tokens tokens;
    };

    Tokens tokenize(std::string str, const char* regex_str);
    MalType read_str(std::string str, const char* regex_str);
    MalType read_comment(Reader &tokens);
    MalType read_string(Reader &tokens);
    MalType read_form(Reader &tokens);
    MalType read_container(Reader &tokens, Type type);
    MalType read_hashmap(Reader &tokens);
    MalType read_atom(Reader &tokens);
    MalType read_keyword(Reader &tokens);
    MalType read_macro(Reader &tokens);
    MalType read_meta(Reader &tokens);

    std::optional<char> read_escape_seq(char code);

    std::optional<std::string> input(std::string_view prompt);

    bool is_valid_float(std::string_view str);
    bool is_valid_int(std::string_view str);
    bool is_valid_nil(std::string_view str);
    bool is_valid_bool(std::string_view str);
}
#endif