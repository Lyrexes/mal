#include "reader.hpp"
#include "types.hpp"
#include <readline/readline.h>
#include <readline/history.h>

namespace Parser {

    const Table Reader::macros = {
        {"'", "quote"},
        {"`", "quasiquote"},
        {"~", "unquote"},
        {"~@", "splice-unquote"},
        {"@", "deref"},
    };

    MalType read_str(std::string str, const char* regex_str) { 
        Reader reader(move(tokenize(str, regex_str)));
        return read_form(reader);
    }
    
    MalType read_form(Reader &tokens) { 
        if(tokens.eof())
            return Types::Nil();
        switch(tokens.peek().front()) {
            case '(' :
                return read_container(tokens, Type::LIST);
            case '[': 
                return read_container(tokens, Type::VECTOR);
            case '{':
                return read_hashmap(tokens);
            case '"':   
                return read_string(tokens);
            case ':':
                return read_keyword(tokens);
            case '^': 
                return read_meta(tokens);
            case ';':
               return read_comment(tokens);
            default:  
                return read_atom(tokens);
        }
    }

    MalType read_comment(Reader &tokens) {
        tokens.pop();
        return read_form(tokens);
    }

    MalType read_hashmap(Reader &tokens) {
        Map_t map{};
        tokens.pop(); // pops delim '{'
        while(tokens.peek() != "}") {
            if(tokens.eof())
                throw std::runtime_error("EOF: No value for key!");
            auto key = read_form(tokens);
            if(tokens.eof())
                throw std::runtime_error("EOF: No value for value!");
            auto val = read_form(tokens);
            map.insert({key, val});
        }
        tokens.pop(); // pops delim '}'
        return Types::Map(std::move(map));
    }

    MalType read_keyword(Reader &tokens) {
        auto str = std::move(tokens.next());
        str.erase(str.begin());
        return Types::Keyword(std::move(str));
    }

    MalType read_atom(Reader &tokens)  {
        auto curr_token = tokens.peek();
        if (is_valid_int(curr_token))
            return Types::Int(std::stol(tokens.next()));
        else if(Reader::macros.contains(std::string{curr_token}))
            return read_macro(tokens);
        else if (is_valid_float(curr_token))
            return Types::Float(std::stod(tokens.next()));
        else if (is_valid_bool(curr_token))
            return Types::Bool(tokens.next() == "true");
        else if (is_valid_nil(curr_token)) {
            tokens.pop();
            return Types::Nil();
        }
        else
            return Types::Symbol(std::move(tokens.next()));
    }

    MalType read_macro(Reader& tokens) {
        auto sym = Types::Symbol(Reader::macros.at((tokens.next())));
        return Types::List(Container{sym, read_form(tokens)});
    }
    
    MalType read_meta(Reader& tokens) {
        auto sym = Types::Symbol("with-meta");
        tokens.pop();
        auto meta = read_form(tokens);
        auto value = read_form(tokens);
        return Types::List(Container{sym, value, meta});
    }

    MalType read_container(Reader &tokens, Type type) {
        Container mal_container;
        std::string end_delim;

        if(type == Type::LIST)
            end_delim = ")";
        else 
            end_delim = "]";
        tokens.pop(); //pops start delim
        while(tokens.peek() != end_delim) {
            if(tokens.eof())
                throw std::runtime_error("EOF: missing symbol'" + end_delim + "'!");
            mal_container.push_back(read_form(tokens));
        }
        tokens.pop(); // pops ending delim
        return MalType(type, std::move(mal_container));
    }

    MalType read_string(Reader &tokens) {
        std::string mal_str;
        auto input_str = std::move(tokens.next());
        if(!input_str.ends_with('"') || input_str.size() <= 1) 
            throw std::runtime_error("EOF: expected Symbol '\"'! ");
        input_str = input_str.substr(1, input_str.size() - 2);//remove quotes
        for(unsigned int i = 0; i < input_str.size(); i++) {
            auto c = input_str[i];
            if(input_str[i] == '\\') {
                if(i + 1 >= input_str.size())
                    throw std::runtime_error("EOF: expected Symbol '\"'! ");
                if(auto esc = read_escape_seq(input_str[i+1])) 
                    c = *esc;
                else
                    throw std::runtime_error("EOF: unexpected escape sequence!");
                i++;
            }
            mal_str += c;
        }
        return Types::String(std::move(mal_str));
    }

    std::optional<char> read_escape_seq(char code) { 
        switch(code) {
            case '\\':return '\\';
            case 'a': return '\a';
            case 'b': return '\b';
            case 'f': return '\f';
            case 'n': return '\n';
            case 'r': return '\r';
            case 't': return '\t';
            case 'v': return '\v';
            case '"': return '"';
            default: return {};
        }
    }
    
    Tokens tokenize(std::string str, const char* regex_str) {
        auto reg = std::regex(regex_str, std::regex::ECMAScript);
        std::sregex_iterator current_match(str.begin(), str.end(), reg);
        std::sregex_iterator last_match;
        Tokens tokens;
        while(current_match != last_match) {
            std::smatch match = *current_match;
            if(!match.str(1).empty())
                tokens.push_back(move(match.str(1)));
            current_match++;
        }
        return tokens;
    }

    Reader::Reader(Tokens &&tokens) :  tokens(tokens) {}

    std::string Reader::next() { 
        auto result = move(tokens.front());
        tokens.erase(tokens.begin());
        return result;
    }

    std::string_view Reader::peek() const { 
        if(tokens.empty())
            return "";
        return tokens.front();
    }

    void Reader::pop() { 
        tokens.erase(tokens.begin());
    }

    bool is_valid_float(std::string_view str) {
        auto number = str;
        auto decimal_point = false;
        std::size_t current_size = 1;
        if(str.size() > 0 && (str.starts_with('+') || str.starts_with('-')))
            number = std::string_view(number.begin()+1, number.end());
        for(const auto& c : number) {
            if(c == '.' && !decimal_point) {
                decimal_point = true;
                continue; 
            }
            if(!std::isdigit(c))
                break;
            current_size++;
        }
        return current_size == number.size() && decimal_point;
    }

    bool is_valid_int(std::string_view str) {
        if(str.empty())
            return false;
        auto number = str;
        if(str.size() > 1 && (str.starts_with('+') || str.starts_with('-')))
            number = std::string_view(number.begin()+1, number.end());
        return std::all_of(number.begin(), number.end(),[](const auto &c){ return std::isdigit(c); });
    }

    bool is_valid_nil(std::string_view str) {
        return str == "nil";
    }

    bool is_valid_bool(std::string_view str) {
        return str == "false" || str == "true";
    }

    std::optional<std::string> input(std::string_view prompt) {
        const auto line = readline(prompt.data());
        std::optional<std::string> result = {};
        if(line != nullptr) 
            result = std::string(line);
        free(line);
        if (result)
            add_history(result.value().c_str());
        return result;
    }
}