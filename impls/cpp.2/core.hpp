#ifndef CORE_HPP
#define CORE_HPP
#include <map>
#include <string>
#include <span>

class MalType;
namespace Core {
    using Table = std::map<std::string, MalType>;
    using Args = std::span<const MalType>;

    Table core_namespace();

    void validate_args(Args args, std::size_t num, std::string op);
    void validate_args_at_least(Args args, std::size_t num, std::string op);

    MalType add_num(Args args);
    MalType mul_num(Args args);
    MalType sub_num(Args args);
    MalType div_num(Args args);
    MalType prn(Args args);
    MalType pr_str(Args args);
    MalType str(Args args);
    MalType println(Args args);
    MalType list(Args args);
    MalType is_list(Args args);
    MalType is_empty(Args args);
    MalType count(Args args);
    MalType equals(Args args);
    MalType gt(Args args);
    MalType lt(Args args);
    MalType gt_or_eq(Args args);
    MalType lt_or_eq(Args args);
}



#endif