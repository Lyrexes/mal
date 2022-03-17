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

    MalType is_macro(Args, EnvPtr env);
    MalType time_ms(Args args, EnvPtr env);
    MalType meta(Args args, EnvPtr env);
    MalType with_meta(Args args, EnvPtr env);
    MalType is_fn(Args args, EnvPtr env);
    MalType is_string(Args args, EnvPtr env);
    MalType is_num(Args args, EnvPtr env);
    MalType seq(Args args, EnvPtr env);
    MalType conj(Args args, EnvPtr env);
    MalType read_line(Args args, EnvPtr env);
    MalType apply (Args args, EnvPtr env);
    MalType map (Args args, EnvPtr env);
    MalType throw_ (Args args, EnvPtr env);
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
    MalType nth(Args args, EnvPtr env);
    MalType first(Args args, EnvPtr env);
    MalType rest(Args args, EnvPtr env);
    MalType list(Args args, EnvPtr env);
    MalType vec(Args args, EnvPtr env);
    MalType symbol(Args args, EnvPtr env);
    MalType keyword(Args args, EnvPtr env);
    MalType is_keyword(Args args, EnvPtr env);
    MalType vector(Args args, EnvPtr env);
    MalType is_vector(Args args, EnvPtr env);
    MalType is_sequential(Args args, EnvPtr env);
    MalType hash_map(Args args, EnvPtr env);
    MalType is_map(Args args, EnvPtr env);
    MalType assoc(Args args, EnvPtr env);
    MalType dissoc(Args args, EnvPtr env);
    MalType get(Args args, EnvPtr env);
    MalType contains(Args args, EnvPtr env);
    MalType keys(Args args, EnvPtr env);
    MalType vals(Args args, EnvPtr env);
    MalType is_list(Args args, EnvPtr env);
    MalType is_nil(Args args, EnvPtr env);
    MalType is_true(Args args, EnvPtr env);
    MalType is_false(Args args, EnvPtr env);
    MalType is_symbol(Args args, EnvPtr env);
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