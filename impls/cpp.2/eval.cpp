#include "eval.hpp"
#include "types.hpp"
#include "env.hpp"
#include "core.hpp"
#include "MalException.hpp"
#include <cstddef>
#include <exception>
#include <iostream>
#include <ranges>

using namespace Types;
using EnvPtr = Environment::EnvPtr;

namespace Eval {

    MalType eval(const MalType& ast, EnvPtr env) {
        auto curr_pair = EvalPair{ast, env};
        while (true) {
            if(!curr_pair.ast.type(Type::LIST))
                return eval_ast(curr_pair);

            curr_pair.ast = macro_expand_(curr_pair.ast, curr_pair.env);

            if(curr_pair.ast.type(Type::LIST)) {

                auto list = curr_pair.ast.seq();

                if(list.empty())
                    return curr_pair.ast;
            
                if(list.front().type(Type::SYMBOL)) {

                    auto first_sym = list.front().str();

                    if(first_sym == "def!") 
                        return apply_def(list, curr_pair.env); 
                    if(first_sym == "defmacro!") 
                        return apply_defmacro(list, curr_pair.env); 
                    if(first_sym == "fn*")
                        return create_lambda(list, curr_pair.env); 
                    if(first_sym == "quote")
                        return apply_quote(list);
                    if(first_sym == "quasiquoteexpand")
                        return apply_quasiquote(list);
                    if(first_sym == "macroexpand")
                        return apply_macro_expand(list, curr_pair.env);
                    if(first_sym == "try*")
                        return apply_try_catch(list, curr_pair.env);
                    if(first_sym == "quasiquote") {
                        curr_pair.ast = apply_quasiquote(list);
                        continue;
                    }
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
                }

                auto evaluated_ast = eval_ast(curr_pair);
                auto eval_list = evaluated_ast.seq();

                if(eval_list.front().type(Type::BUILTIN))
                    return apply(eval_list, curr_pair.env); 
                
                if(eval_list.front().type(Type::LAMBDA)) {
                    curr_pair = apply_lambda(eval_list);
                    continue;
                }
                throw std::runtime_error("EOF: undefined symbol cant evaluate : "
                 + Types::to_string(List(std::get<Container>(evaluated_ast.val())), true));
            }
            return eval_ast(curr_pair);
        }
    }

    bool is_catch_statement_valid(std::span<MalType> args) {
        return args.size() == 3 && args[2].type(Type::LIST) && !args[2].empty()
         && args[2].fst().str() == "catch*" && args[2].nth(1).type(Type::SYMBOL);
    }

    MalType apply_try_catch(std::span<MalType> args, std::shared_ptr<Environment> env) {
        auto result = Nil();
        try {
            result = eval(args[1], env);
        } catch (const MalException& e) {
                if(!is_catch_statement_valid(args))
                    throw MalException(e.get());
                auto except_env = std::make_shared<Environment>(env);
                except_env->set(std::get<std::string>(args[2].nth(1).val()), e.get());
                return eval(args[2].nth(2), except_env);

        } catch (const std::runtime_error& e) {
                if(!is_catch_statement_valid(args))
                    throw e;
                auto except_env = std::make_shared<Environment>(env);
                except_env->set(std::get<std::string>(args[2].nth(1).val()), String(e.what()));
                return eval(args[2].nth(2), except_env);
        }
        return result;
    }

    MalType apply_quasiquote(std::span<MalType> args) {
        Core::validate_args(args, 2, "quasiquote");
        return recur_quasiquote(args[1]);
    }

    MalType recur_quasiquote(MalType& ast) {
        if(ast.type(Type::LIST) || ast.type(Type::VECTOR)) {
            if(!ast.empty() && ast.fst().type(Type::SYMBOL)
                && !ast.type(Type::VECTOR) && ast.fst().str() == "unquote") {
                Core::validate_args(ast.seq(), 2, "unquote");
                return ast.nth(1);
            }

            auto ast_list = ast.seq();
            auto result = std::vector<MalType>{};
            auto temp_list = std::vector<MalType>{3};

            for (auto it = ast_list.rbegin(); it != ast_list.rend(); ++it) {
                auto &elt = *it;
                if(elt.type(Type::LIST) && !empty(elt)
                 && elt.fst().type(Type::SYMBOL)
                 && elt.fst().str() == "splice-unquote") {
                    auto elt_list = elt.seq();
                    Core::validate_args(elt_list, 2, "splice-unquote");
                    temp_list[0] = Symbol("concat"); 
                    temp_list[1] = elt_list[1]; 
                } else {
                    temp_list[0] = Symbol("cons"); 
                    temp_list[1] = recur_quasiquote(elt);
                }
                temp_list[2] = List(result);
                result = temp_list;
            }
            if(ast.type(Type::VECTOR))
                return List({Symbol("vec"), List(result)});
            return List(result);
        }

        if(ast.type(Type::SYMBOL) || ast.type(Type::MAP))
            return List({Symbol("quote"), ast});
        return ast;
    }
    
    MalType apply_quote(std::span<MalType> args) {
        Core::validate_args(args, 2, "quote");
        return args[1];
    }

    MalType apply(std::span<MalType> args, EnvPtr env) {
        auto builtin = std::get<Builtin_t>(args.front().func());
        return builtin({args.begin()+1, args.end()}, env);
    }

    MalType create_lambda(std::span<MalType> args, EnvPtr env) {
        if(args.size() != 3) 
            throw std::runtime_error("EOF: Lambda fn* needs 2 arguments: parameters and body!");
        auto arguments = std::span{args.begin()+1, args.end()};
        auto [params, is_variadic] =  validate_lambda_params(arguments[0]);
        auto body = Container{};
        if (arguments[1].type(Type::LIST))
            body = std::get<Container>(std::move(arguments[1].val()));
        else
            body = {std::move(arguments[1])};
        return Lambda(std::move(params), std::move(body), env, is_variadic);
    }

    std::pair<std::vector<std::string>,MaybeVariadic> validate_lambda_params(const MalType& params) {
        if(params.type(Type::LIST) && params.type(Type::VECTOR))
            throw std::runtime_error("EOF: lamda parameter must be a list or vector!");

        auto param_list = std::vector<std::string>{};
        param_list.reserve(params.seq_view().size());
        auto params_view = params.seq_view();
        auto is_variadic = MaybeVariadic{};

        for(auto param  = params_view.begin(); param != params_view.end(); ++param) {
            if(!param->type(Type::SYMBOL))
                throw std::runtime_error("EOF: Each lamda parameter must be a Symbol! got: " 
                + Types::to_string(*param, true));
            if(param->str() == "&") {
                is_variadic = std::distance(params_view.begin(), param);
                continue;
            }
            param_list.push_back(std::get<std::string>(std::move(param->val())));
        }
        return {param_list, is_variadic};
    }

    EvalPair apply_do(std::span<MalType> args, EnvPtr env) {
        Core::validate_args_at_least(args, 2, "do");
        if(args.size() == 2)
            return EvalPair(args[1], env);
        auto arguments = args.subspan(1, args.size() - 2);
        auto evaluated_list = eval_ast(EvalPair(
            List(std::vector<MalType>{arguments.begin(),
             arguments.end()}),env));
        return EvalPair(args.back(), env); 
    }

    EvalPair apply_if(std::span<MalType> args, EnvPtr env) {
        auto params = std::span{args.begin()+1, args.end()};
        auto has_else = params.size() == 3;

        if(params.size() < 2 || params.size() > 3) 
            throw std::runtime_error("EOF: if needs at least two and max three arguments!");

        auto condition = eval(params[0], env);

        switch(condition.id()){
            case Type::NIL:
                if(has_else)
                    return EvalPair(params[2], env);
                return EvalPair(Nil(), env);
            case Type::BOOL:
                if(condition.boolv())
                    return EvalPair(params[1], env);
                else if(has_else)
                    return EvalPair(params[2], env);
                return EvalPair(Nil(), env);
            default:
                return EvalPair(params[1], env);
        }
    }

    MalType eval_ast(const EvalPair& pair) {
        switch (pair.ast.id()) {
            case Type::SYMBOL:
                return pair.env->get(pair.ast.str());
            case Type::LIST:
            case Type::VECTOR:
                return eval_container(pair.ast, pair.env);
            case Type::MAP:
                return eval_map(pair.ast, pair.env);
            default:
                return pair.ast;
        }
    }

    MalType eval_container(const MalType& ast, EnvPtr env) {
        Container seq{};
        for(const auto& el : ast.seq_view())
            seq.push_back(eval(el, env));
        return MalType(ast.id(), std::move(seq));
    }

    MalType eval_map(const MalType& ast, EnvPtr env) {
        Map_t map{};
        for(const auto& [key, value] : std::get<Map_t>(ast.val()))
            map.insert({key, eval(value, env)});
        return Map(std::move(map));
    }

    EvalPair apply_lambda(std::span<MalType> args) { 
        auto const &lambda = std::get<Lambda_t>(args.front().func());
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
                throw std::runtime_error("EOF: invalid amount of arguments to call lambda!");
        return std::make_shared<Environment>(lambda.env, lambda.params, std::move(args));
    }

    MalType apply_defmacro(std::span<MalType> args, std::shared_ptr<Environment> env) {
        Core::validate_args(args, 3, "def!");
        auto value = eval(args[2], env);
        auto key = std::get<std::string>(std::move(args[1].val()));
        if(!value.type(Type::LAMBDA))
            throw std::runtime_error("EOF: macro has to be a function type!");
        auto lambda = std::get<Lambda_t>(std::move(value.func()));
        lambda.is_macro = true;
        env->set(std::move(key), MalType(Type::LAMBDA, lambda));
        return value;
    }

    bool is_macro_call(MalType& ast, std::shared_ptr<Environment> env) {
        if(ast.type(Type::LIST) && !empty(ast) && ast.fst().type(Type::SYMBOL)) {
            if(!env->exists(ast.fst().str()))
                return false;
            auto lam = env->get(ast.fst().str());
            if(lam.type(Type::LAMBDA))
                return lam.is_macro();
        }
        return false;
    }

    MalType apply_macro_expand(std::span<MalType> args, std::shared_ptr<Environment> env) {
        Core::validate_args(args, 2, "macroexpand");
        auto ast = args[1];
        return macro_expand_(ast, env);
    }

    MalType macro_expand(MalType& ast, std::shared_ptr<Environment> env) {
        auto lam = MalType{};
        auto params = std::vector<MalType>{};
        auto eval_pair = EvalPair{};
        while(is_macro_call(ast, env)) {
            lam = env->get(ast.fst().str());
            params = std::vector<MalType>{ast.seq().begin()+1, ast.seq().end()};
            params.insert(params.begin(), lam);
            eval_pair = apply_lambda(params);
            ast = eval(eval_pair.ast, eval_pair.env);
        }
        return ast;
    }

    MalType macro_expand_(MalType& ast, std::shared_ptr<Environment> env) {
        auto lam = MalType{};
        auto eval_pair = EvalPair{};
        auto params = std::span<MalType>{};
        while(is_macro_call(ast, env)) {
            params = ast.seq();
            params[0] = env->get(ast.fst().str());
            eval_pair = apply_lambda(params);
            ast = eval(eval_pair.ast, eval_pair.env);
        }
        return ast;
    }
    MalType apply_def(std::span<MalType> args, EnvPtr env) {
        Core::validate_args(args, 3, "def!");
        auto key = std::get<std::string>(std::move(args[1].val()));
        auto value = eval(args[2], env);
        env->set(std::move(key), value);
        return value;
    }

    EvalPair apply_let(std::span<MalType> args, EnvPtr env) {
        if(args[1].type(Type::LIST) && args[1].type(Type::VECTOR))
            throw std::runtime_error("EOF: expected list or vector in let! statement! Got: "
             + to_string(args[1], true));

        auto bind_list = args[1].seq_view();
        if(bind_list.size() % 2 != 0)
            throw std::runtime_error("EOF: expected even number of arguments in let binding list!");

        auto let_env = std::make_shared<Environment>(env);
        for(std::size_t i = 0; i < bind_list.size(); i+=2) {
            auto value = eval(bind_list[i+1], let_env);
            let_env->set(std::get<std::string>(bind_list[i].val()), value);
        }
        return {args[2], let_env};
    }   
}