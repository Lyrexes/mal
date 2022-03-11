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
    MalType apply_def(std::span<const MalType> args, std::shared_ptr<Environment> env);
    MalType create_lambda(std::span<const MalType> args, std::shared_ptr<Environment> env);
    EvalPair apply_lambda(std::span<const MalType> args, std::shared_ptr<Environment> env);
    EvalPair apply_do(std::span<const MalType> args, std::shared_ptr<Environment> env);
    EvalPair apply_if(std::span<const MalType> args, std::shared_ptr<Environment> env);
    EvalPair apply_let(std::span<const MalType> args, std::shared_ptr<Environment> env);
    MalType apply(std::span<const MalType> args, std::shared_ptr<Environment> env);
    std::pair<strVec, std::optional<std::size_t>> validate_lambda_params(const MalType& params);
    std::shared_ptr<Environment> get_env_with_variadic(const Lambda_t& lambda, std::vector<MalType> args);
    std::shared_ptr<Environment> get_env_lambda(const Lambda_t& lambda, std::vector<MalType> args);
    void called_with(std::string calle, const MalType& ast);
    void called_with(std::string calle, std::span<const MalType> ast);
}

#endif