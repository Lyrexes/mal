#ifndef TYPES_HPP
#define TYPES_HPP
#include <utility>
#include <variant>
#include <vector>
#include <string>
#include <numeric>
#include <span>
#include <functional>
#include <optional>
#include <map>
#include <cmath>
#include <stdexcept>


namespace Types {
    using namespace std;
    enum class TypeID {
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
        VOID
    };
    struct MalType;
    using Null = std::monostate;
    using Container = std::vector<MalType>;
    using Map_t = std::map<MalType, MalType>;
    using Builtin_t = std::function<MalType(std::span<const MalType> args)>;
    template <class T>
    using Maybe = std::optional<T>;
    using Type = std::variant<double, bool, Container, Map_t, string, Null>;
    

    struct MalType {
        TypeID id;
        Type val{};
        Maybe<Builtin_t> builtin{};
        MalType() : id(TypeID::VOID) {}
        explicit MalType(TypeID id) : id(id) {}
        MalType(TypeID id, Type&& val)
            : id(id), val(std::move(val)) {}
        explicit MalType(Builtin_t val)
            : id(TypeID::BUILTIN), builtin(std::move(val)) {}
        bool operator <(const MalType& rhs) const { return val < rhs.val; }
    };

    string to_string(const MalType& type, bool readably);
    string to_string(std::span<const MalType> container, TypeID type, bool readably);
    string to_string(const Map_t& hashmap, bool readably);
    string to_string(string_view mal_str, bool readably);
    string to_string(double number);

    std::string unescape_char(char c);
    TypeID get_number_type(double val);

    template<typename L, typename R, typename Ret, typename Func>
    Ret apply_binary(Func func, const MalType& lhs, const MalType& rhs) {
        return func(std::get<L>(lhs.val), std::get<R>(rhs.val));
    }

    template<typename Func>
    MalType apply_num_op(Func func, const MalType& lhs, const MalType& rhs){
        auto num = func(std::get<double>(lhs.val), std::get<double>(rhs.val));
        return MalType(get_number_type(num), num);
    }

    void validate_args(std::span<const MalType> args, std::size_t num, std::string op);

    MalType add_num(std::span<const MalType> args);
    MalType mul_num(std::span<const MalType> args);
    MalType sub_num(std::span<const MalType> args);
    MalType div_num(std::span<const MalType> args);

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

    void type_error(std::string&& expected, std::string&& got);

    long get_int(const MalType& type);
    double get_float(const MalType& type);
    bool get_bool(const MalType& type);
    std::string_view get_string_view(const MalType& type);
    std::span<const MalType> get_container_view(const MalType& type);
    std::span<MalType> get_container_ref(MalType& type);
}




#endif