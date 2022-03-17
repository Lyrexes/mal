#ifndef EVAL_HPP
#define EVAL_HPP
#include <cstddef>
#include <set>
#include <span>
#include <vector>
#include <string>
#include <memory>
#include <optional>

class MalType;
class Environment;
class Lambda_t;
class EvalPair;
namespace Eval {
    using strVec = std::vector<std::string>;
    MalType eval(const MalType& ast, std::shared_ptr<Environment> env);
    MalType eval_ast(const EvalPair& pair);
    MalType eval_container(const MalType& ast, std::shared_ptr<Environment> env);
    MalType eval_map(const MalType& ast, std::shared_ptr<Environment> env);
    MalType apply_def(std::span<MalType> args, std::shared_ptr<Environment> env);
    MalType apply_defmacro(std::span<MalType> args, std::shared_ptr<Environment> env);
    MalType apply_quote(std::span<MalType> args);
    MalType apply_quasiquote(std::span<MalType> args);
    MalType apply_try_catch(std::span<MalType> args, std::shared_ptr<Environment> env);
    MalType recur_quasiquote(MalType& ast);
    MalType create_lambda(std::span<MalType> args, std::shared_ptr<Environment> env);
    MalType apply_macro_expand(std::span<MalType> args, std::shared_ptr<Environment> env);
    MalType macro_expand(MalType& ast, std::shared_ptr<Environment> env);
    MalType macro_expand_(MalType& ast, std::shared_ptr<Environment> env);
    EvalPair apply_lambda(std::span<MalType> args);
    EvalPair apply_do(std::span<MalType> args, std::shared_ptr<Environment> env);
    EvalPair apply_if(std::span<MalType> args, std::shared_ptr<Environment> env);
    EvalPair apply_let(std::span<MalType> args, std::shared_ptr<Environment> env);
    MalType apply(std::span<MalType> args, std::shared_ptr<Environment> env);
    std::pair<strVec, std::optional<std::size_t>> validate_lambda_params(const MalType& params);
    std::shared_ptr<Environment> get_env_with_variadic(const Lambda_t& lambda, std::vector<MalType> args);
    std::shared_ptr<Environment> get_env_lambda(const Lambda_t& lambda, std::vector<MalType> args);
    bool is_macro_call(MalType& ast, std::shared_ptr<Environment> env);
    bool is_catch_statement_valid(std::span<MalType> args);
    
}

#endif