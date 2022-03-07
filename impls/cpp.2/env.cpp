#include "env.hpp"
#include "types.hpp"


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

Environment::MalType Environment::get(std::string_view name) const {
    if(auto env = find(name)) {
        if(env.value() == this)
            return data.at(name.data());
        return env.value()->get(name);
    }
    throw std::runtime_error("variable : '" + std::string{name} + "' not found");
}