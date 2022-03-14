#ifndef CORE_HPP
#define CORE_HPP
#include <map>
#include <string>
#include <span>
#include <memory>

class MalType;
class Environment;

namespace Core {
    using Table = std::map<std::string, MalType>;
    using Args = std::span<MalType>;
    using EnvPtr = std::shared_ptr<Environment>;

    const static char *regex = R"([\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*))";

    Table core_namespace();

    void validate_args(Args args,std::size_t num, std::string op);
    void validate_args_at_least(Args args, std::size_t num, std::string op);

    MalType reset(Args args, EnvPtr env);
    MalType swap(Args args, EnvPtr env);
    MalType deref(Args args, EnvPtr env);
    MalType is_atom(Args args, EnvPtr env);
    MalType atom(Args args, EnvPtr env);
    MalType add_num(Args args, EnvPtr env);
    MalType mul_num(Args args, EnvPtr env);
    MalType sub_num(Args args, EnvPtr env);
    MalType div_num(Args args, EnvPtr env);
    MalType prn(Args args, EnvPtr env);
    MalType pr_str(Args args, EnvPtr env);
    MalType str(Args args, EnvPtr env);
    MalType println(Args args, EnvPtr env);
    MalType list(Args args, EnvPtr env);
    MalType vec(Args args, EnvPtr env);
    MalType is_list(Args args, EnvPtr env);
    MalType is_empty(Args args, EnvPtr env);
    MalType count(Args args, EnvPtr env);
    MalType equals(Args args, EnvPtr env);
    MalType gt(Args args, EnvPtr env);
    MalType lt(Args args, EnvPtr env);
    MalType gt_or_eq(Args args, EnvPtr env);
    MalType lt_or_eq(Args args, EnvPtr env);
    MalType read_str(Args args, EnvPtr env);
    MalType slurp(Args args, EnvPtr env);
    MalType cons(Args args, EnvPtr env);
    MalType concat(Args args, EnvPtr env);
}



#endif