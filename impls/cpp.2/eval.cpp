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
        auto curr_pair = EvalPair{ast, env};
        while (true) {
            if(curr_pair.ast.id == TypeID::LIST) {
                auto list = get_container_view(curr_pair.ast);
                if(list.empty())
                    return curr_pair.ast;
            
                if(list.front().id == TypeID::SYMBOL) {
                    auto first_sym = get_string_view(list.front());
                    if(first_sym == "def!") 
                        return apply_def(list, curr_pair.env); 
                    if(first_sym == "let*") {
                        curr_pair = apply_let(list, curr_pair.env); 
                        continue;
                    }
                    if(first_sym == "do") {
                        curr_pair = apply_do(list, curr_pair.env); 
                        continue;
                    }
                    if(first_sym == "if") {
                        curr_pair =  apply_if(list, curr_pair.env); 
                        continue;
                    }
                    if(first_sym == "fn*")
                        return create_lambda(list, curr_pair.env); 
                }
                auto evaluated_ast = eval_ast(curr_pair);
                auto eval_list = get_container_view(evaluated_ast);
                if(eval_list.front().id == TypeID::BUILTIN)
                    return apply(eval_list, curr_pair.env); 
                if(eval_list.front().id == TypeID::LAMBDA) {
                    curr_pair = apply_lambda(eval_list, curr_pair.env);
                    continue;
                }
                throw std::runtime_error("undefined symbol cant evaluate : "
                 + Types::to_string(List(std::get<Container>(evaluated_ast.val)), true));
            }
            return eval_ast(curr_pair);
        }
    }

    MalType apply(std::span<const MalType> args, EnvPtr env) {
        auto builtin = std::get<Builtin_t>(args.front().func);
        return builtin({args.begin()+1, args.end()});
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

        for(auto param  = params_view.begin(); param != params_view.end(); ++param) {
            if(param->id != TypeID::SYMBOL)
                throw std::runtime_error("Each lamda parameter must be a Symbol! got: " 
                + Types::to_string(*param, true));
            if(get_string_view(*param) == "&") {
                is_variadic = std::distance(params_view.begin(), param);
                continue;
            }
            param_list.push_back(std::get<std::string>(std::move(param->val)));
        }
        return {param_list, is_variadic};
    }

    EvalPair apply_do(std::span<const MalType> args, EnvPtr env) {
        Core::validate_args_at_least(args, 2, "do");
        if(args.size() == 2)
            return EvalPair(args[1], env);
        auto arguments = args.subspan(1, args.size() - 2);
        auto evaluated_list = eval_ast(EvalPair(
            List(std::vector<MalType>{arguments.begin(),
             arguments.end()}),env));
        return EvalPair(args.back(), env); 
    }

    EvalPair apply_if(std::span<const MalType> args, EnvPtr env) {
        auto params = std::span{args.begin()+1, args.end()};
        auto has_else = params.size() == 3;

        if(params.size() < 2 || params.size() > 3) 
            throw std::runtime_error("if needs at least two and max three arguments!");

        auto condition = eval(params[0], env);

        switch(condition.id){
            case TypeID::NIL:
                if(has_else)
                    return EvalPair(params[2], env);
                return EvalPair(Nil(), env);
            case TypeID::BOOL:
                if(get_bool(condition))
                    return EvalPair(params[1], env);
                else if(has_else)
                    return EvalPair(params[2], env);
                return EvalPair(Nil(), env);
            default:
                return EvalPair(params[1], env);
        }
    }

    MalType eval_ast(const EvalPair& pair) {
        switch (pair.ast.id) {
            case TypeID::SYMBOL:
                return pair.env->get(get_string_view(pair.ast));
            case TypeID::LIST:
            case TypeID::VECTOR:
                return eval_container(pair.ast, pair.env);
            case TypeID::MAP:
                return eval_map(pair.ast, pair.env);
            default:
                return pair.ast;
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

    EvalPair apply_lambda(std::span<const MalType> args, EnvPtr env) { 
        auto lambda = std::get<Lambda_t>(args.front().func);
        auto arguments = std::vector<MalType>{args.begin()+1, args.end()};
        auto body = MalType{};
        auto lambda_env = std::shared_ptr<Environment>{};

        if(lambda.varidic_index)
            lambda_env = get_env_with_variadic(lambda, std::move(arguments));
        else 
            lambda_env = get_env_lambda(lambda, std::move(arguments));
       
        if(lambda.body.size() == 1)
            body = lambda.body[0];
        else
            body = List(lambda.body);

        return EvalPair(body, lambda_env);
    }

    std::shared_ptr<Environment> get_env_with_variadic(const Lambda_t& lambda, std::vector<MalType> args) {
        auto v_index = *lambda.varidic_index;
        auto non_variadic_params = std::vector<std::string>{lambda.params.begin(), lambda.params.begin()+v_index};
        auto variadic_args = std::vector<MalType>{};
        auto non_variadic_args = std::vector<MalType>{};

        std::move(args.begin(), args.begin()+v_index, std::back_inserter(non_variadic_args));
        std::move(args.begin()+v_index, args.end(), std::back_inserter(variadic_args));

        auto lambda_env = std::make_shared<Environment>(lambda.env,  non_variadic_params, non_variadic_args);
        lambda_env->set(lambda.params[v_index], Types::List(variadic_args));

        return lambda_env;
    }

    std::shared_ptr<Environment> get_env_lambda(const Lambda_t& lambda, std::vector<MalType> args) {
        if(args.size() != lambda.params.size())
                throw std::runtime_error("invalid amount of arguments to call lambda!");
        return std::make_shared<Environment>(lambda.env, lambda.params, std::move(args));
    }

    MalType apply_def(std::span<const MalType> args, EnvPtr env) {
        Core::validate_args(args, 3, "def!");
        auto key = std::get<std::string>(std::move(args[1].val));
        auto value = eval(args[2], env);
        env->set(std::move(key), value);
        return value;
    }

    EvalPair apply_let(std::span<const MalType> args, EnvPtr env) {
        if(args[1].id != TypeID::LIST && args[1].id != TypeID::VECTOR)
            throw std::runtime_error("expected list or vector in let! statement! Got: "
             + to_string(args[1], true));

        auto bind_list = get_container_view(args[1]);
        if(bind_list.size() % 2 != 0)
            throw std::runtime_error("expected even number of arguments in let binding list!");

        auto let_env = std::make_shared<Environment>(env);
        for(std::size_t i = 0; i < bind_list.size(); i+=2) {
            auto value = eval(bind_list[i+1], let_env);
            let_env->set(std::get<std::string>(bind_list[i].val), value);
        }
        return {args[2], let_env};
    }   
}