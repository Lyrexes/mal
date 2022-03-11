#include "eval.hpp"
#include "types.hpp"
#include "env.hpp"
#include "core.hpp"
#include <cstddef>
#include <iostream>

using namespace Types;
using EnvPtr = Environment::EnvPtr;
namespace Eval {

    void called_with(std::string calle, const MalType& ast) {
        std::cout << "called '" + calle + "' with " << Types::to_string(ast, true) << std::endl;
    }

    void called_with(std::string calle, std::span<const MalType> ast) {
        auto val = List({ast.begin(), ast.end()});
        std::cout << "called '" + calle + "' with " << Types::to_string(val, true) << std::endl;
    }

    MalType eval(const MalType& ast, EnvPtr env) {
        if(ast.id == TypeID::LIST) {
            auto list = get_container_view(ast);
            if(list.empty())
                return ast;
            if(list.front().id == TypeID::SYMBOL) {
                auto first_sym = get_string_view(list.front());
                if(first_sym == "def!") 
                    return apply_def(list, env); 
                if(first_sym == "let*")
                    return apply_let(list, env); 
                if(first_sym == "do")
                    return apply_do(list, env); 
                if(first_sym == "if")
                    return apply_if(list, env); 
                if(first_sym == "fn*")
                    return create_lambda(list, env); 
            }
            return apply(get_container_view(eval_ast(ast, env)), env); 
        }
        return eval_ast(ast, env);
    }

    MalType apply(std::span<const MalType> args, EnvPtr env) {
        auto arguments = args;
        if(arguments.front().id == TypeID::BUILTIN) {
            auto builtin = std::get<Builtin_t>(arguments.front().func);
            return builtin({arguments.begin()+1, arguments.end()});
        }
        if(arguments.front().id == TypeID::LAMBDA)
            return apply_lambda(arguments, env);
        throw std::runtime_error("undefined symbol cant evaluate : " + Types::to_string(arguments.front(), true));
    }

    MalType create_lambda(std::span<const MalType> args, EnvPtr env) {
        if(args.size() != 3) 
            throw std::runtime_error("Lambda fn* needs 2 arguments: parameters and body!");
        auto arguments = std::span{args.begin()+1, args.end()};
        auto [params, is_variadic] =  validate_lambda_params(arguments[0]);
        auto body = Container{};
        if (arguments[1].id == TypeID::LIST) 
            body = std::get<Container>(std::move(arguments[1].val));
        else
            body = {std::move(arguments[1])};
        return Lambda(std::move(params), std::move(body), env, is_variadic);
    }

    std::pair<std::vector<std::string>,MaybeVariadic> validate_lambda_params(const MalType& params) {
        if(params.id != TypeID::LIST && params.id != TypeID::VECTOR)
            throw std::runtime_error("lamda parameter must be a list or vector!");
        auto param_list = std::vector<std::string>{};
        param_list.reserve(Types::get_container_view(params).size());
        auto params_view = get_container_view(params);
        auto is_variadic = MaybeVariadic{};
        auto current_index = std::size_t{0};
        for(auto& param : params_view) {
            if(param.id != TypeID::SYMBOL)
                throw std::runtime_error("Each lamda parameter must be a Symbol! got: " 
                + Types::to_string(param, true));
            auto sym = get_string_view(param);
            if(sym == "&") {
                is_variadic = current_index;
                continue;
            }
            param_list.push_back(std::string{sym});
            current_index++;
        }
        return {param_list, is_variadic};
    }

    MalType apply_do(std::span<const MalType> args, EnvPtr env) {
        auto evaluated_list = eval_ast(List({args.begin()+1, args.end()}), env);
        return get_container_view(evaluated_list).back(); 
    }

    MalType apply_if(std::span<const MalType> args, EnvPtr env) {
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

    MalType eval_ast(const MalType& ast, EnvPtr env) {
        switch (ast.id) {
            case TypeID::SYMBOL:
                return env->get(get_string_view(ast));
            case TypeID::LIST:
            case TypeID::VECTOR:
                return eval_container(ast, env);
            case TypeID::MAP:
                return eval_map(ast, env);
            default:
                return ast;
        }
    }

    MalType eval_container(const MalType& ast, EnvPtr env) {
        Container seq{};
        for(const auto& el : get_container_view(ast))
            seq.push_back(eval(el, env));
        return MalType(ast.id, std::move(seq));
    }

    MalType eval_map(const MalType& ast, EnvPtr env) {
        Map_t map{};
        for(const auto& [key, value] : std::get<Map_t>(ast.val))
            map.insert({key, eval(value, env)});
        return Map(std::move(map));
    }

    MalType apply_lambda(std::span<const MalType> args, EnvPtr env) { 
        auto lambda = std::get<Lambda_t>(args.front().func);
        auto arguments = std::vector<MalType>{args.begin()+1, args.end()};
        auto body = MalType{};
        auto lambda_env = std::shared_ptr<Environment>{};
        if(lambda.varidic_index) {
            auto v_index = *lambda.varidic_index;
            auto non_variadic_args = std::vector<MalType>{arguments.begin(), arguments.begin()+v_index};
            auto non_variadic_params = std::vector<std::string>{lambda.params.begin(), lambda.params.begin()+v_index};
            auto variadic_args = std::vector<MalType>{arguments.begin()+v_index, arguments.end()};
            lambda_env = std::make_shared<Environment>(lambda.env,  non_variadic_params, non_variadic_args);
            lambda_env->set(lambda.params[v_index], Types::List(variadic_args));
        } else {
            if(arguments.size() != lambda.params.size())
                throw std::runtime_error("invalid amount of arguments to call lambda!");
            lambda_env = std::make_shared<Environment>(lambda.env, lambda.params, arguments);
        }
       
        if(lambda.body.size() == 1)
            body = lambda.body[0];
        else
            body = List(lambda.body);
        return eval(body, lambda_env);
    }

    MalType apply_def(std::span<const MalType> args, EnvPtr env) {
        Core::validate_args(args, 3, "def!");
        auto key = std::get<std::string>(args[1].val);
        auto value = eval(args[2], env);
        env->set(std::move(key), value);
        return value;
    }

    MalType apply_let(std::span<const MalType> args, EnvPtr env) {
        if(args[1].id != TypeID::LIST && args[1].id != TypeID::VECTOR)
            throw std::runtime_error("expected list in let! statement! Got: "
             + to_string(args[1], true));
        auto bind_list = get_container_view(args[1]);
        if(bind_list.size() % 2 != 0)
            throw std::runtime_error("expected even number of arguments in let binding list!");
        auto let_env = std::make_shared<Environment>(env);
        for(std::size_t i = 0; i < bind_list.size(); i+=2) {
            auto value = eval(bind_list[i+1], let_env);
            let_env->set(std::get<std::string>(bind_list[i].val), value);
        }
        return eval(args[2], let_env);
    }   
}