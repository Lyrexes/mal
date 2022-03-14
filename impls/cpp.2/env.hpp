#ifndef ENV_HPP
#define ENV_HPP
#include <memory>
#include <optional>
#include <map>
#include <vector>

class MalType;
class Atom_t;
class Environment :  public std::enable_shared_from_this<Environment>{
    public:
        template <typename T>
        using Maybe = std::optional<T>;
        using ConstEnvPtr = std::shared_ptr<const Environment>;
        using EnvPtr = std::shared_ptr<Environment>;
        using Pair_t = std::pair<std::string, MalType>;
        template <typename T>
        using vec = std::vector<T>;
        using str = std::string;
    public:
        Environment(Maybe<EnvPtr> outer={}, const vec<str>& binds={}, const vec<MalType>& exprs={});
        Environment(Maybe<EnvPtr> outer, std::map<std::string, MalType> table);
        void set(std::string key, MalType value);
        void set(const std::map<std::string, MalType>& table);
        void def_atom(std::string key, MalType atom);
        void change_atom(std::string key, MalType value);
        EnvPtr get_root_env();
        Maybe<EnvPtr> find(std::string_view name);
        MalType get(std::string_view name);
        Maybe<ConstEnvPtr> find(std::string_view name) const;
        MalType get(std::string_view name) const;
        std::string to_string() const;
        MalType at(std::string_view name) const;
    private:
        Maybe<EnvPtr> outer;
        std::map<std::string, MalType> data{};
};













#endif