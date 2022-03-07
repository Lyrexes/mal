#include "types.hpp"

namespace Types {
    std::string to_string(const MalType& type, bool readably) {
        switch (type.id) {
            case TypeID::INT:
                return std::to_string(get_int(type));
            case TypeID::FLOAT:
                return to_string(get_float(type));
            case TypeID::SYMBOL:
                return std::get<std::string>(type.val);
            case TypeID::MAP:
                return to_string(std::get<Map_t>(type.val), readably);
            case TypeID::LIST:
            case TypeID::VECTOR:
                return to_string(get_container_view(type), type.id, readably);
            case TypeID::BOOL:
                return get_bool(type)? "true" : "false";
            case TypeID::STRING:
                return to_string(get_string_view(type), readably);
            case TypeID::NIL:
                return "nil";
            case TypeID::KEYWORD:
                return std::string(":") + std::get<std::string>(type.val);
            default:
                return "";
        }
    }
    
    std::string to_string(std::span<const MalType> container, TypeID type, bool readably) {
            std::string delim;
            std::string container_str;
            if(type == TypeID::LIST)
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
        if(readably) {
            mal_str = std::accumulate(mal_str.begin(), mal_str.end(), std::string(),
                [](std::string acc, char c){
                    return std::move(acc) + unescape_char(c);
                });
        }
        return std::string("\"") + std::string{mal_str} + std::string("\"");
    }

    std::string to_string(double number) {
        auto str = std::to_string(number);
        str.erase(str.find_last_not_of('0') + 1, std::string::npos);
        return str;
    }

    TypeID get_number_type(double val) {
        int val_int = std::abs(val);
        return val == val_int ? TypeID::INT : TypeID::FLOAT;
    }

    void validate_args(std::span<const MalType> args, std::size_t num, std::string op) {
        if(args.size() != num)
            throw std::runtime_error("too many arguments for function call: '" + op + "' !");
    }

    MalType add_num(std::span<const MalType> args) {
        validate_args(args, 2, "+");
        return apply_num_op(std::plus<>{}, args[0], args[1]);
    }
    MalType mul_num(std::span<const MalType> args) {
        validate_args(args, 2, "*");
        return apply_num_op(std::multiplies<>{}, args[0], args[1]);
    }
    MalType sub_num(std::span<const MalType> args) {
        validate_args(args, 2, "-");
        return apply_num_op(std::minus<>{}, args[0], args[1]);
    }
    MalType div_num(std::span<const MalType> args){
        validate_args(args, 2, "/");
        return apply_num_op(std::divides<>{}, args[0], args[1]);
    }

    MalType String(std::string string) {
        return MalType(TypeID::STRING, std::move(string));
    }

    MalType Symbol(std::string symbol) {
        return MalType(TypeID::SYMBOL, std::move(symbol));
    }

    MalType Float(double num) {
        return MalType(TypeID::FLOAT, num);
    }

    MalType Int(long num) {
        return MalType(TypeID::INT, static_cast<double>(num));
    }

    MalType Bool(bool val) {
        return MalType(TypeID::BOOL, val);
    }

    MalType List(Container list) {
        return MalType(TypeID::LIST,std::move(list));
    }

    MalType Vector(Container vec) {
        return MalType(TypeID::VECTOR,std::move(vec));
    }

    MalType Map(Map_t map) {
        return MalType(TypeID::MAP, std::move(map));
    }

    MalType Nil() { 
        return MalType(TypeID::NIL);
    }

    MalType Keyword(std::string kw) {
        return MalType(TypeID::KEYWORD, std::move(kw));
    }

    MalType Builtin(Builtin_t builtin) {
        return MalType(builtin);
    }

    void type_error(std::string&& expected, std::string&& got) {
        throw new std::runtime_error("TypeError: expected type: " + expected + ", got: " + got);
    }

    long get_int(const MalType& type) {
        if(type.id != TypeID::INT)
            type_error("int", to_string(type, true));
        return std::roundl(std::get<double>(type.val));
    }

    double get_float(const MalType& type) {
        if(type.id != TypeID::FLOAT)
            type_error("float", to_string(type, true));
        return std::get<double>(type.val);
    }

    bool get_bool(const MalType& type) {
        if(type.id != TypeID::BOOL)
            type_error("bool", to_string(type, true));
        return std::get<bool>(type.val);
    }

    std::string_view get_string_view(const MalType& type) {
        if(type.id != TypeID::STRING && type.id != TypeID::KEYWORD
         &&type.id != TypeID::SYMBOL)
            type_error("string-type", to_string(type, true));
        return {std::get<std::string>(type.val).begin(),
                std::get<std::string>(type.val).end() };
    }

    std::span<const MalType> get_container_view(const MalType& type) {
        if(type.id != TypeID::LIST && type.id != TypeID::VECTOR )
            type_error("container-type", to_string(type, true));
        return { std::get<Container>(type.val).begin(),
                 std::get<Container>(type.val).end()};
    }

    std::span<MalType> get_container_ref(MalType& type) {
        if(type.id != TypeID::LIST && type.id != TypeID::VECTOR )
            type_error("container-type", to_string(type, true));
        return { std::get<Container>(type.val).begin(),
                 std::get<Container>(type.val).end()};
    }
}