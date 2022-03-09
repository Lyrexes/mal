#include "env.hpp"
#include "types.hpp"
#include <iostream>


Environment::Environment(Maybe<const Environment*> outer, const std::vector<Pair_t>& pairs)
: outer(outer) , data(pairs.begin(), pairs.end()){}

void Environment::set(std::string key, MalType value) {
    data.insert_or_assign(key, value);
}

Environment::Maybe<const Environment*> Environment::find(std::string_view name) const {
    if(data.contains(name.data()))
        return {this};
    if(outer)
        return outer.value()->find(name);
    return {};
}

MalType Environment::get(std::string_view name) const {
    if(auto env = find(name)) {
        if(env.value() == this)
            return data.at(name.data());
        return env.value()->get(name);
    }
    throw std::runtime_error("variable : '" + std::string{name} + "' not found");
}

std::string Environment::to_string() const {
    std::string env_str{"{"};

    for(auto &[key, value] : data) {
        env_str += "(" + key + ", " + Types::to_string(value, true) + ")";
    }
    env_str += "} | ";
    auto cur_env = outer;
    while(auto out = cur_env) {
        env_str += (*out)->to_string();
        cur_env = (*out)->outer;
    }
    return env_str;
}