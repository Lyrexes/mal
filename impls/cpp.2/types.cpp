#include "types.hpp"
#include <cstddef>
#include <iostream>

namespace Types {
    std::string to_string(const MalType& type, bool readably) {
        switch (type.id()) {
            case Type::INT:
                return std::to_string(type.intv());
            case Type::FLOAT:
                return to_string(type.floatv());
            case Type::SYMBOL:
                return std::get<std::string>(type.val());
            case Type::MAP:
                return to_string(std::get<Map_t>(type.val()), readably);
            case Type::LIST:
            case Type::VECTOR:
                return to_string(type.seq_view(), type.id(), readably);
            case Type::BOOL:
                return type.boolv() ? "true" : "false";
            case Type::STRING:
                return to_string(type.str(), readably);
            case Type::NIL:
                return "nil";
            case Type::KEYWORD:
                return std::string(":") + std::get<std::string>(type.val());
            case Type::LAMBDA:
                return "#<function>";
            case Type::BUILTIN:
                return "#<builtin>";
            case Type::ATOM:
                return to_string(std::get<Atom_t>(type.val()), readably);
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
     std::shared_ptr<Environment> env, MaybeVariadic is_variadic, bool is_macro) {
        return MalType(Type::LAMBDA, 
         Lambda_t(std::move(params), std::move(body), env, is_variadic, is_macro));
    }

    MalType Atom(MalType mal_val, Maybe<std::string> var) { 
        return MalType(Type::ATOM, 
        Atom_t(std::make_shared<MalType>(std::move(mal_val)), std::move(var)));
    }

    void type_error(std::string&& expected, std::string&& got) {
        throw std::runtime_error("TypeError: expected type: " + expected + ", got: " + got);
    }
/**
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

    bool is_macro(const MalType& mal) {
        if(!is_type(mal, Type::LAMBDA))
            throw std::runtime_error("is_macro t: expected lambda!");
        return std::get<Lambda_t>(mal.func).is_macro;
    }

    std::span<MalType> get_sub_seq(MalType& type, std::size_t offset, std::size_t count) {
        if(get_seq(type).size() <= offset + count)
            throw std::invalid_argument("get_sub_seq: index out of bounds!");
        return get_seq(type).subspan(offset, count);
    }

    std::span<MalType> get_sub_seq(MalType& type, std::size_t offset) {
        if(get_seq(type).size() <= offset)
            throw std::invalid_argument("get_sub_seq: index out of bounds!");
        return get_seq(type).subspan(offset);
    }
    **/
}

long MalType::intv() const {
    if(m_id != Type::INT)
        Types::type_error("int", Types::to_string(*this, true));
    return std::roundl(std::get<double>(m_val));
}

double MalType::floatv() const {
    if(m_id != Type::FLOAT)
        Types::type_error("float", Types::to_string(*this, true));
    return std::get<double>(m_val);
}

bool MalType::boolv() const {
    if(m_id != Type::BOOL)
        Types::type_error("bool", Types::to_string(*this, true));
    return std::get<bool>(m_val);
}

std::string_view MalType::str() const {
    if(m_id != Type::STRING && m_id != Type::KEYWORD
     &&m_id != Type::SYMBOL)
        Types::type_error("string-type", Types::to_string(*this, true));
    return std::get<std::string>(m_val);
}

std::span<const MalType> MalType::seq_view() const {
    if(m_id != Type::LIST && m_id != Type::VECTOR)
        Types::type_error("sequence-type", Types::to_string(*this, true));
    return std::get<Container>(m_val);
}

std::span<MalType> MalType::seq()  {
    if(m_id != Type::LIST && m_id != Type::VECTOR)
        Types::type_error("sequence-type", Types::to_string(*this, true));
    return std::get<Container>(m_val);
}

std::span<MalType> MalType::sub_seq(std::size_t offset, std::size_t count) {
    if(this->seq().size() <= offset + count)
        throw std::invalid_argument("get_sub_seq: index out of bounds!");
    return this->seq().subspan(offset, count);
}

std::span<MalType> MalType::sub_seq(std::size_t offset) {
    if(this->seq().size() <= offset)
        throw std::invalid_argument("get_sub_seq: index out of bounds!");
    return this->seq().subspan(offset);
}

MalType& MalType::fst() {
    if(this->empty())
        throw std::invalid_argument("fst: cannot call fst on empty seq!");
    return this->seq()[0];
}

MalType& MalType::nth(std::size_t index) {
    if(this->seq().size() <= index)
        throw std::invalid_argument("get_nth: index out of bounds!");
    return this->seq()[index];
}

bool MalType::empty() const {
    return this->seq_view().empty();
}

bool MalType::type(Type type) const {
    return type == m_id;
}

bool MalType::is_macro() const {
    if(!this->type(Type::LAMBDA))
        throw std::runtime_error("is_macro t: expected lambda!");
    return std::get<Lambda_t>(m_func).is_macro;
}

const DataType& MalType::val() const {
    return m_val;
}

const Functor& MalType::func() const {
    return m_func;
}

DataType& MalType::val() {
    return m_val;
}

Functor& MalType::func() {
    return m_func;
}

Type MalType::id() const { return m_id; }