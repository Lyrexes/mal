#ifndef ENV_HPP
#define ENV_HPP
#include <memory>
#include <optional>
#include "types.hpp"

class Environment {
    using MalType = Types::MalType;
    template <typename T>
    using Maybe = std::optional<T>;
    using Pair_t = std::pair<std::string, MalType>;
    public:
        Environment(Maybe<const Environment*> outer, std::span<Pair_t> pairs);
        void set(std::string key, MalType value);
        Maybe<const Environment*> find(std::string_view name) const;
        MalType get(std::string_view name) const;
    private:
        Maybe<const Environment*> outer;
        std::map<std::string, MalType> data{};
};













#endif