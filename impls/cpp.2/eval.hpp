#ifndef EVAL_HPP
#define EVAL_HPP
#include "types.hpp"
#include "env.hpp"

namespace Eval {
    using namespace Types;
    MalType eval(const MalType& ast, Environment& env);
    MalType eval_ast(const MalType& ast,  Environment& env);
    MalType eval_container(const MalType& ast, Environment& env);
    MalType eval_map(const MalType& ast, Environment& env);
    MalType apply_def(std::span<const MalType> args, Environment& env);
    MalType apply_let(std::span<const MalType> args, Environment& env);
    MalType apply_builtin(std::span<const MalType> args);
}

#endif