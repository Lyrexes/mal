#include "env.hpp"
#include "types.hpp"
#include "eval.hpp"
#include <iostream>
#include <cassert>



Environment::Environment(Maybe<EnvPtr> outer, std::span<std::string> binds, std::span<MalType> exprs):
    outer(outer) {
        assert(("key and values are diffrent size!", binds.size() == exprs.size()));
        for(std::size_t i = 0; i < binds.size(); i++) 
            data.insert_or_assign(binds[i], exprs[i]);
        //std::cout << to_string()<< std::endl;
}

Environment::Environment(Maybe<EnvPtr> outer, const vec<str>& binds, const vec<MalType>& exprs) 
    : outer(outer){
        for(std::size_t i = 0; i < binds.size(); i++) 
            data.insert_or_assign(binds[i], exprs[i]);
}

Environment::Environment(Maybe<EnvPtr> outer, std::map<std::string, MalType> table)
 : outer(outer), data(std::move(table)){}

void Environment::set(std::string key, MalType value) {
    data.insert_or_assign(key, value);
}

Environment::EnvPtr Environment::get_root_env() {
    if(!outer)
        return shared_from_this();
    auto current_outer = outer;
    auto root = *outer;
    while(current_outer) {
        root = *current_outer;
        current_outer = (*current_outer)->outer;
    }
    return root;
}

Maybe<Environment::EnvPtr> Environment::find(std::string_view name) {
    if(data.contains(name.data()))
        return shared_from_this();
    if(outer)
        return (*outer)->find(name);
    return {};
}

MalType Environment::get(std::string_view name) {
    if(auto env = find(name)) 
        return (*env)->at(name.data());
    throw std::runtime_error("'" + std::string{name} + "' not found");
}

bool Environment::exists(std::string_view key) {
    return find(key).has_value();
}
void Environment::set(const std::map<std::string, MalType>& table) {
    for(const auto&[key, value] : table)
        data.insert_or_assign(key, value);
}

Environment::Maybe<Environment::ConstEnvPtr> Environment::find(std::string_view name) const {
    if(data.contains(name.data()))
        return shared_from_this();
    if(outer)
        return (*outer)->find(name);
    return {};
}

MalType Environment::get(std::string_view name) const {
    if(auto env = find(name)) 
        return (*env)->at(name.data());
    throw std::runtime_error("'" + std::string{name} + "' not found");
}

MalType Environment::at(std::string_view name) const { 
    return data.at(name.data());
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