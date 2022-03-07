#include "eval.hpp"
#include "types.hpp"
#include <cstddef>

namespace Eval {

    MalType eval(const MalType& ast, Environment& env) {
        if(ast.id == TypeID::LIST) {
            auto list = get_container_view(ast);
            if(list.empty())
                return ast;
            auto first_sym = get_string_view(list.front());
            if(first_sym == "def!") 
                return apply_def(list, env); 
            if(first_sym == "let*")
                return apply_let(list, env); 
            return apply_builtin(get_container_view(eval_ast(ast, env)));
        }
        return eval_ast(ast, env);
    }

    MalType eval_ast(const Types::MalType& ast, Environment& env) {
        switch (ast.id) {
            case TypeID::SYMBOL:
                return env.get(get_string_view(ast));
            case TypeID::LIST:
            case TypeID::VECTOR:
                return eval_container(ast, env);
            case TypeID::MAP:
                return eval_map(ast, env);
            default:
                return ast;
        }
    }

    MalType eval_container(const Types::MalType& ast, Environment& env) {
        Container seq{};
        for(const auto& el : get_container_view(ast))
            seq.push_back(eval(el, env));
        return MalType(ast.id, std::move(seq));
    }

    MalType eval_map(const Types::MalType& ast, Environment& env) {
        Map_t map{};
        for(const auto& [key, value] : std::get<Map_t>(ast.val))
            map.insert({key, eval(value, env)});
        return Map(std::move(map));
    }

    MalType apply_builtin(std::span<const MalType> args) {
        auto builtin = *args.front().builtin;
        return builtin({args.begin()+1, args.end()});
    }

    MalType apply_def(std::span<const MalType> args, Environment& env) {
        Types::validate_args(args, 3, "def!");
        auto key = std::get<std::string>(args[1].val);
        auto value = eval(args[2], env);
        env.set(std::move(key), value);
        return value;
    }

    MalType apply_let(std::span<const MalType> args, Environment& env) {
        if(args[1].id != TypeID::LIST && args[1].id != TypeID::VECTOR)
            throw std::runtime_error("expected list in let! statement! Got: "
             + to_string(args[1], true));
        auto bind_list = get_container_view(args[1]);
        if(bind_list.size() % 2 != 0)
            throw std::runtime_error("expected even number of arguments in let binding list!");
        auto let_env = Environment(&env, {});
        for(std::size_t i = 0; i < bind_list.size(); i+=2) {
            auto value = eval(bind_list[i+1], let_env);
            let_env.set(std::get<std::string>(bind_list[i].val), value);
        }
        return eval(args[2], let_env);
    }   
}