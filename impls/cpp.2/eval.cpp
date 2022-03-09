#include "eval.hpp"
#include "types.hpp"
#include "env.hpp"
#include <cstddef>
#include <iostream>

using namespace Types;
namespace Eval {

    MalType eval(const MalType& ast, Environment& env) {
        std::cout << "eval: " << Types::to_string(ast, true) << std::endl;
        if(ast.id == TypeID::LIST) {
            auto list = get_container_view(ast);
            if(list.empty())
                return ast;
            /**
            if(list.front().id == TypeID::LIST) {
                auto result =  eval(list.front(), env);
                if(result.id != TypeID::LAMBDA)
                    throw std::runtime_error("expected a lambda call!");
                std::vector<MalType> new_list{list.begin(), list.end()};
                new_list[0] = result;
                return apply_lambda({new_list.begin(), new_list.end()}, env);
            }
            **/
            if(list.front().id == TypeID::SYMBOL) {
                auto first_sym = get_string_view(list.front());
                if(first_sym == "def!") 
                    return apply_def(list, env); 
                if(first_sym == "let*")
                    return apply_let(list, env); 
                if(first_sym == "do*")
                    return apply_do(list, env); 
                if(first_sym == "if")
                    return apply_if(list, env); 
                if(first_sym == "fn*")
                    return create_lambda(list, env); 
            }
            return apply(get_container_view(eval_ast(ast, env)), env); //REFACTOR 
        }
        std::cout << "eval_ast called \n";
        return eval_ast(ast, env);
    }

    MalType apply(std::span<const MalType> args, Environment& env) {
        std::cout << "apply is called" << std::endl;
        auto arguments = args;
        if(arguments.front().id == TypeID::LIST) {
            auto new_first = eval(arguments.front(), env);
            auto new_arguments = std::vector{args.begin()+1, args.end()};
            new_arguments.insert(new_arguments.begin(), new_first);
            arguments = {new_arguments};
        }
        if(arguments.front().id == TypeID::BUILTIN) {
            auto builtin = std::get<Builtin_t>(arguments.front().func);
            return builtin({arguments.begin()+1, arguments.end()});
        }
        if(arguments.front().id == TypeID::LAMBDA)
            return apply_lambda(arguments, env);
        throw std::runtime_error("undefined symbol cant evaluate : " + Types::to_string(arguments.front(), true));
    }

    MalType create_lambda(std::span<const MalType> args, Environment& env) {
        std::cout << "create_lambda called" << std::endl;
        if(args.size() != 3) 
            throw std::runtime_error("Lambda fn* needs 2 arguments: parameters and body!");
        auto arguments = std::span{args.begin()+1, args.end()};
        auto params = std::get<Container>(std::move(arguments[0].val));
        auto body = Container{};
        if (arguments[1].id == TypeID::LIST) 
            body = std::get<Container>(std::move(arguments[1].val));
        else
            body = {std::move(arguments[1])};
        std::cout << "curr env: " << env.to_string() << std::endl;
        return Lambda(std::move(params), std::move(body), &env);
    }

    MalType apply_do(std::span<const MalType> args, Environment& env) {
        auto evaluated_list = eval_ast(List({args.begin()+1, args.end()}), env);
        return get_container_view(evaluated_list).back(); 
    }

    MalType apply_if(std::span<const MalType> args, Environment& env) {
        auto params = std::span{args.begin()+1, args.end()};
        if(params.size() < 2 || params.size() > 3) {
            throw std::runtime_error("if needs at least two and max three arguments!");
        }
        auto condition = eval(params[0], env);
        if(condition.id != TypeID::NIL && condition.id != TypeID::BOOL) {
            return eval(params[1], env);
        }
        if(condition.id == TypeID::BOOL && std::get<bool>(condition.val))
            return eval(params[1], env);
        if(params.size() == 3)
            return eval(params[2], env);
        return Nil();
    }

    MalType eval_ast(const MalType& ast, Environment& env) {
        switch (ast.id) {
            case TypeID::SYMBOL:
                std::cout << "symbol called \n";
                return env.get(get_string_view(ast));
            case TypeID::LIST:
                std::cout << "container called \n";
                return eval_container(ast, env);
            case TypeID::VECTOR:
                return eval_container(ast, env);
            case TypeID::MAP:
                return eval_map(ast, env);
            default:
                return ast;
        }
    }

    MalType eval_container(const MalType& ast, Environment& env) {
        Container seq{};
        for(const auto& el : get_container_view(ast))
            seq.push_back(eval(el, env));
        return MalType(ast.id, std::move(seq));
    }

    MalType eval_map(const MalType& ast, Environment& env) {
        Map_t map{};
        for(const auto& [key, value] : std::get<Map_t>(ast.val))
            map.insert({key, eval(value, env)});
        return Map(std::move(map));
    }


    MalType apply_lambda(std::span<const MalType> args, Environment& env) { 
        std::cout << "apply_lambda is called" << std::endl;
        auto lambda = std::get<Lambda_t>(args.front().func);
        auto arguments = std::span{args.begin()+1, args.end()};
        auto params = std::span{lambda.params.cbegin(), lambda.params.cend()};
        std::vector<std::pair<std::string, MalType>> key_value_pair{};

        if(arguments.size() != lambda.params.size())
            throw std::runtime_error("invalid amount of arguments to call lambda!");
        std::transform(arguments.begin(), arguments.end(), params.begin(), 
            std::back_inserter(key_value_pair),
            [&](const MalType& arg, const MalType& param) {
                auto val =  eval(arg, env);
                return std::pair{std::get<std::string>(param.val), eval(arg, env)};
            });
        std::cout << "params: " << Types::to_string(List(lambda.params), true)
            << " body: " << Types::to_string(List(lambda.body), true) << std::endl;
        std::cout << "cuurent env: " << lambda.env->to_string() << std::endl;
        Environment lambda_env(lambda.env, key_value_pair);
        MalType body{};
        if(lambda.body.size() == 1)
            body = lambda.body[0];
        else
            body = List(lambda.body);
        std::cout << "lambda_env: " << lambda_env.to_string() << std::endl;
        return eval(body, lambda_env);
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
        auto let_env = Environment(&env);
        for(std::size_t i = 0; i < bind_list.size(); i+=2) {
            auto value = eval(bind_list[i+1], let_env);
            let_env.set(std::get<std::string>(bind_list[i].val), value);
        }
        return eval(args[2], let_env);
    }   
}