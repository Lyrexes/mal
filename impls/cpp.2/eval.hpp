#ifndef EVAL_HPP
#define EVAL_HPP
#include <set>
#include <span>

class MalType;
class Environment;
namespace Eval {
    MalType eval(const MalType& ast, Environment& env);
    MalType eval_ast(const MalType& ast,  Environment& env);
    MalType eval_container(const MalType& ast, Environment& env);
    MalType eval_map(const MalType& ast, Environment& env);
    MalType apply_def(std::span<const MalType> args, Environment& env);
    MalType apply_lambda(std::span<const MalType> args, Environment& env);
    MalType create_lambda(std::span<const MalType> args, Environment& env);
    MalType apply_do(std::span<const MalType> args, Environment& env);
    MalType apply_if(std::span<const MalType> args, Environment& env);
    MalType apply_let(std::span<const MalType> args, Environment& env);
    MalType apply_builtin(std::span<const MalType> args, Environment& env);
}

#endif