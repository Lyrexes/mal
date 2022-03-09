#ifndef ENV_HPP
#define ENV_HPP
#include <memory>
#include <optional>
#include <map>
#include <vector>

class MalType;

class Environment {
    public:
        template <typename T>
        using Maybe = std::optional<T>;
        using Pair_t = std::pair<std::string, MalType>;
    public:
        Environment(Maybe<const Environment*> outer={}, const std::vector<Pair_t>& pairs={});
        void set(std::string key, MalType value);
        Maybe<const Environment*> find(std::string_view name) const;
        MalType get(std::string_view name) const;
        std::string to_string() const;
    private:
        Maybe<const Environment*> outer;
        std::map<std::string, MalType> data{};
};













#endif