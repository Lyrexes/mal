#include "types.hpp"
#include <iostream>

namespace Types {
    std::string to_string(const MalType& type, bool readably) {
        switch (type.id) {
            case Type::INT:
                return std::to_string(get_int(type));
            case Type::FLOAT:
                return to_string(get_float(type));
            case Type::SYMBOL:
                return std::get<std::string>(type.val);
            case Type::MAP:
                return to_string(std::get<Map_t>(type.val), readably);
            case Type::LIST:
            case Type::VECTOR:
                return to_string(get_seq_view(type), type.id, readably);
            case Type::BOOL:
                return get_bool(type)? "true" : "false";
            case Type::STRING:
                return to_string(get_str(type), readably);
            case Type::NIL:
                return "nil";
            case Type::KEYWORD:
                return std::string(":") + std::get<std::string>(type.val);
            case Type::LAMBDA:
                return "#<function>";
            case Type::BUILTIN:
                return "#<builtin>";
            case Type::ATOM:
                return to_string(std::get<Atom_t>(type.val), readably);
            default:
                return "";
        }
    }
    
    std::string to_string(std::span<const MalType> container, Type type, bool readably) {
            std::string delim;
            std::string container_str;
            if(type == Type::LIST)
                delim = "()";
            else
                delim = "[]";
            if(container.empty())
                return delim;
            container_str = {delim.front()};
            for(auto const &el: container) {
                container_str += to_string(el, readably) + " ";
            }
            container_str.pop_back();
            container_str += {delim.back()};
            return container_str;
    }

    std::string to_string(const Atom_t &atom, bool readably) {
        return "(atom " + to_string(*atom.ref, readably) + ")";
    }

    std::string to_string(const Map_t& map, bool readably) {
        std::string container_str = "{";
        if(map.empty())
            return "{}";
        for(const auto& [key, value] : map) {
            container_str += to_string(key, readably) + " " + to_string(value, readably) + " ";
        }
        container_str.pop_back();
        container_str += "}";
        return container_str;
    }

    std::string unescape_char(char c) { 
        switch(c) {
            case '\\': return  "\\\\";
            case '\a' : return "\\a";
            case '\b' : return "\\b";
            case '\f' : return "\\f";
            case '\n' : return "\\n";
            case '\r' : return "\\r";
            case '\t' : return "\\t";
            case '\v' : return "\\v";
            case '\"' : return "\\\"";
            default : return {c};
        }       
    }

    std::string to_string(std::string_view mal_str, bool readably) {
        auto ret_str = std::string{mal_str};
        if(readably) {
            ret_str = std::accumulate(mal_str.begin(), mal_str.end(), std::string(),
                [](std::string&& acc, char c){ return std::move(acc) + unescape_char(c); });
            return std::string("\"") + ret_str + std::string("\"");
        }
        return ret_str;
    }

    std::string to_string(double number) {
        auto str = std::to_string(number);
        str.erase(str.find_last_not_of('0') + 1, std::string::npos);
        return str;
    }

    Type get_number_type(double val) {
        int val_int = std::abs(val);
        return val == val_int ? Type::INT : Type::FLOAT;
    }

    MalType String(std::string string) {
        return MalType(Type::STRING, std::move(string));
    }

    MalType Symbol(std::string symbol) {
        return MalType(Type::SYMBOL, std::move(symbol));
    }

    MalType Float(double num) {
        return MalType(Type::FLOAT, num);
    }

    MalType Int(long num) {
        return MalType(Type::INT, static_cast<double>(num));
    }

    MalType Bool(bool val) {
        return MalType(Type::BOOL, val);
    }

    MalType List(Container list) {
        return MalType(Type::LIST,std::move(list));
    }

    MalType Vector(Container vec) {
        return MalType(Type::VECTOR,std::move(vec));
    }

    MalType Map(Map_t map) {
        return MalType(Type::MAP, std::move(map));
    }

    MalType Nil() { 
        return MalType(Type::NIL);
    }

    MalType Keyword(std::string kw) {
        return MalType(Type::KEYWORD, std::move(kw));
    }

    MalType Builtin(Builtin_t builtin) {
        return MalType(Type::BUILTIN, std::move(builtin));
    }

    MalType Lambda(std::vector<std::string> params, Container body,
     std::shared_ptr<Environment> env, MaybeVariadic is_variadic) {
        return MalType(Type::LAMBDA, Lambda_t(std::move(params), std::move(body), env, is_variadic));
    }

    MalType Atom(MalType mal_val, Maybe<std::string> var) { 
        return MalType(Type::ATOM, 
        Atom_t(std::make_shared<MalType>(std::move(mal_val)), std::move(var)));
    }

    void type_error(std::string&& expected, std::string&& got) {
        throw std::runtime_error("TypeError: expected type: " + expected + ", got: " + got);
    }

    long get_int(const MalType& type) {
        if(type.id != Type::INT)
            type_error("int", to_string(type, true));
        return std::roundl(std::get<double>(type.val));
    }

    double get_float(const MalType& type) {
        if(type.id != Type::FLOAT)
            type_error("float", to_string(type, true));
        return std::get<double>(type.val);
    }

    bool get_bool(const MalType& type) {
        if(type.id != Type::BOOL)
            type_error("bool", to_string(type, true));
        return std::get<bool>(type.val);
    }

    std::string_view get_str(const MalType& type) {
        if(type.id != Type::STRING && type.id != Type::KEYWORD
         &&type.id != Type::SYMBOL)
            type_error("string-type", to_string(type, true));
        return std::get<std::string>(type.val);
    }

    std::span<const MalType> get_seq_view(const MalType& type) {
        if(type.id != Type::LIST && type.id != Type::VECTOR )
            type_error("container-type", to_string(type, true));
        return std::get<Container>(type.val);
    }

    std::span<MalType> get_seq(MalType& type) {
        if(type.id != Type::LIST && type.id != Type::VECTOR )
            type_error("container-type", to_string(type, true));
        return std::get<Container>(type.val);
    }

    MalType& fst(MalType& type) {
        if(get_seq(type).empty())
            throw std::invalid_argument("fst: cannot call fst on empty seq!");
        return get_seq(type)[0];
    }

    MalType& nth_elem(std::size_t index, MalType& type) {
        auto seq = get_seq(type);
        if(seq.size() <= index)
            throw std::invalid_argument("get_nth: index out of bounds!");
        return seq[index];
    }

    bool empty(MalType& seq) {
        return get_seq(seq).empty();
    }

    bool is_type(const MalType& mal, Type type) {
        return mal.id == type;
    }
}