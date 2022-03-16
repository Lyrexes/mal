#ifndef TYPES_HPP
#define TYPES_HPP
#include <cstddef>
#include <utility>
#include <variant>
#include <vector>
#include <string>
#include <numeric>
#include <span>
#include <algorithm>
#include <functional>
#include <optional>
#include <map>
#include <cmath>
#include <memory>
#include <stdexcept>

struct MalType;
struct Environment;
struct Lambda_t;

enum class Type {
    FLOAT,
    INT,
    BOOL, 
    LIST,
    VECTOR,
    MAP,
    NIL,
    SYMBOL,
    STRING,
    KEYWORD,
    BUILTIN,
    LAMBDA,
    ATOM,
};

using MalRef = std::shared_ptr<MalType>;
template <class T>
using Maybe = std::optional<T>;

struct Atom_t {
    MalRef ref;
    Maybe<std::string> var;
    Atom_t(MalRef ref, Maybe<std::string> var) : ref(ref), var(var) {}
    bool operator ==(const Atom_t& rhs) const { return ref == rhs.ref; }
    bool operator <(const Atom_t& rhs) const { return ref < rhs.ref; }
};

using Null = std::monostate;
using Container = std::vector<MalType>;
using Map_t = std::map<MalType, MalType>;
using Builtin_t = std::function<MalType(std::span<MalType> args, std::shared_ptr<Environment> env)>;
using MaybeVariadic = std::optional<std::size_t>;
using DataType = std::variant<double, bool, Container, Map_t, std::string, Atom_t,Null>;
using Functor  = std::variant<Builtin_t, Lambda_t, Null>;

struct Lambda_t {
    std::vector<std::string> params{};
    Container body{};
    std::shared_ptr<Environment> env{};
    std::optional<std::size_t> varidic_index{};
    bool is_macro = false;
    Lambda_t(std::vector<std::string> parameter, Container body,
     std::shared_ptr<Environment> env, MaybeVariadic is_variadic)
        :  params(parameter), body(body), env(env) , varidic_index(is_variadic){}
    Lambda_t(std::vector<std::string> parameter, Container body,
     std::shared_ptr<Environment> env, MaybeVariadic is_variadic, bool is_macro)
        :  params(parameter), body(body), env(env) , varidic_index(is_variadic),
         is_macro(is_macro){}
};

struct MalType {
    MalType() : m_id(Type::NIL) {}
    explicit MalType(Type id) : m_id(id) {}
    MalType(Type id, Functor func) 
        : m_id(id), m_func(std::move(func)) {}
    MalType(Type id, DataType val)
        : m_id(id), m_val(std::move(val)) {}
    bool operator ==(const MalType& rhs) const { return m_val == rhs.m_val; }
    bool operator <(const MalType& rhs) const { return m_val < rhs.m_val; }

    long intv() const;
    double floatv() const;
    bool boolv() const;
    std::string_view str() const;
    std::span<const MalType> seq_view() const;
    std::span<MalType> seq();
    std::span<MalType> sub_seq(std::size_t offset, std::size_t count);
    std::span<MalType> sub_seq(std::size_t offset);
    MalType& fst();
    MalType& nth(std::size_t);
    bool empty() const;
    bool type(Type type) const;
    bool is_macro() const;
    Type id() const;
    const DataType& val() const;
    const Functor& func() const;
    DataType& val();
    Functor& func();
private:
    Type m_id{};
    DataType m_val{};
    Functor m_func{};
};

struct EvalPair {
    MalType ast{};
    std::shared_ptr<Environment> env{};
    EvalPair() {}
    EvalPair(MalType ast, std::shared_ptr<Environment> env)
     : ast(std::move(ast)), env(std::move(env)) {} 
};
namespace Types {
    using namespace std;
    string to_string(const MalType& type, bool readably);
    string to_string(std::span<const MalType> container, Type type, bool readably);
    string to_string(const Map_t& hashmap, bool readably);
    string to_string(string_view mal_str, bool readably);
    string to_string(double number);
    std::string to_string(const Atom_t &atom, bool readably);

    std::string unescape_char(char c);
    Type get_number_type(double val);

    MalType Atom(MalType mal_val, std::optional<std::string> var);
    MalType String(std::string string);
    MalType Symbol(std::string symbol);
    MalType Float(double num);
    MalType Int(long num);
    MalType Bool(bool val);
    MalType List(Container list);
    MalType Vector(Container vec);
    MalType Map(Map_t map);
    MalType Nil();
    MalType Keyword(std::string kw);
    MalType Builtin(Builtin_t builtin);
    MalType Lambda(std::vector<std::string> params, Container body, std::shared_ptr<Environment> env,
     MaybeVariadic is_variadic, bool is_macro = false);

    template<typename L, typename R, typename Ret, typename Func>
    Ret apply_binary(Func func, const MalType& lhs, const MalType& rhs) {
        return func(std::get<L>(lhs.val()), std::get<R>(rhs.val()));
    }

    template<typename Func>
    MalType apply_num_op(Func func, const MalType& lhs, const MalType& rhs){
        auto num = func(std::get<double>(lhs.val()), std::get<double>(rhs.val()));
        return MalType(get_number_type(num), num);
    }

    template<typename Func>
    MalType apply_num_bool_op(Func func, const MalType& lhs, const MalType& rhs){
        auto res = func(std::get<double>(lhs.val()), std::get<double>(rhs.val()));
        return Bool(res);
    }

    void type_error(std::string&& expected, std::string&& got);
}


#endif