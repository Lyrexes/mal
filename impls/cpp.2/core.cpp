#include "core.hpp"
#include "types.hpp"
#include "printer.hpp"
#include <functional>
#include <iostream>

namespace Core {

    Table core_namespace() {
        return Table {
            {"+", Types::Builtin(add_num)},
            {"-", Types::Builtin(sub_num)},
            {"*", Types::Builtin(mul_num)},
            {"/", Types::Builtin(div_num)},
            {"pr-str", Types::Builtin(pr_str)},
            {"str", Types::Builtin(str)},
            {"prn", Types::Builtin(prn)},
            {"println", Types::Builtin(println)},
            {"list", Types::Builtin(list)},
            {"list?", Types::Builtin(is_list)},
            {"empty?", Types::Builtin(is_empty)},
            {"count", Types::Builtin(count)},
            {"=", Types::Builtin(equals)},
            {"<", Types::Builtin(lt)},
            {">", Types::Builtin(gt)},
            {"<=", Types::Builtin(lt_or_eq)},
            {">=", Types::Builtin(gt_or_eq)},
        };
    }

     MalType pr_str(Args args) {
        auto str = std::accumulate(args.begin(), args.end(),
         std::string{}, [](auto&& acc, const auto& val) {
            return std::move(acc) + Types::to_string(val, true) + " "; 
        });
        if(!str.empty())
            str.pop_back();
        return Types::String(str);
    }
    
    MalType str(Args args) {
        auto str = std::accumulate(args.begin(), args.end(),
         std::string{}, [](auto&& acc, const auto& val) {
            return std::move(acc) + Types::to_string(val, false); 
        });
        return Types::String(str);
    }

    MalType println(Args args) {
        auto str = std::accumulate(args.begin(), args.end(),
         std::string{}, [](auto&& acc, const auto& val) {
            return std::move(acc) + Types::to_string(val, false) + " "; 
        });
        if(!str.empty())
            str.pop_back();
        std::cout << str << std::endl;
        return Types::Nil();
    }

    MalType prn(Args args) {
        auto str = std::accumulate(args.begin(), args.end(),
         std::string{}, [](auto&& acc, const auto& val) {
            return std::move(acc) + Types::to_string(val, true) + " "; 
        });
        if(!str.empty())
            str.pop_back();
        std::cout << str << std::endl;
        return Types::Nil();
    }

    MalType add_num(Args args) {
        validate_args(args, 2, "+");
        return Types::apply_num_op(std::plus<>{}, args[0], args[1]);
    }
    MalType mul_num(Args args) {
        validate_args(args, 2, "*");
        return Types::apply_num_op(std::multiplies<>{}, args[0], args[1]);
    }
    MalType sub_num(Args args) {
        validate_args(args, 2, "-");
        return Types::apply_num_op(std::minus<>{}, args[0], args[1]);
    }
    MalType div_num(Args args){
        validate_args(args, 2, "/");
        return Types::apply_num_op(std::divides<>{}, args[0], args[1]);
    }

    MalType list(Args args) {
        return Types::List({args.begin(), args.end()});
    }

    MalType is_list(Args args) {
        validate_args(args, 1, "list?");
        return Types::Bool(args[0].id == TypeID::LIST);
    }

    MalType is_empty(Args args) {
        validate_args(args, 1, "empty?");
        return Types::Bool(Types::get_container_view(args[0]).empty());
    }

    MalType count(Args args) {
        validate_args(args, 1, "count");
        if(args[0].id == TypeID::LIST || args[0].id == TypeID::VECTOR)
            return Types::Int(Types::get_container_view(args[0]).size());
        return Types::Int(0);
    }

    MalType equals(Args args) {
        validate_args(args, 2, "=");
        auto type = args[0].id;
        if(type == TypeID::BUILTIN || type == TypeID::LAMBDA)
            throw std::runtime_error("cant compare these types: " + Types::to_string(args[0], true)
             + " = " + Types::to_string(args[1], true));
        return Types::Bool(args[0].val == args[1].val);
    }

    MalType gt(Args args) {
        validate_args(args, 2, ">");
        if(args[0].val.index() != 0 || args[1].val.index() != 0)
            throw std::runtime_error("Only numbers can be compared got: " 
             + Types::to_string(args[0], true) +  " > " + Types::to_string(args[1], true));
        return Types::apply_num_bool_op(std::greater<>{}, args[0], args[1]);
    }

    MalType lt(Args args) {
        validate_args(args, 2, "<");
        if(args[0].val.index() != 0 || args[1].val.index() != 0)
            throw std::runtime_error("Only numbers can be compared got: " 
             + Types::to_string(args[0], true) +  " < " + Types::to_string(args[1], true));
        return Types::apply_num_bool_op(std::less<>{}, args[0], args[1]);
    }

    MalType gt_or_eq(Args args) {
        validate_args(args, 2, ">=");
        if(args[0].val.index() != 0 || args[1].val.index() != 0)
            throw std::runtime_error("Only numbers can be compared got: " 
             + Types::to_string(args[0], true) +  " >= " + Types::to_string(args[1], true));
        return Types::apply_num_bool_op(std::greater_equal<>{}, args[0], args[1]);
    }

    MalType lt_or_eq(Args args) {
        validate_args(args, 2, "<=");
        if(args[0].val.index() != 0 || args[1].val.index() != 0)
            throw std::runtime_error("Only numbers can be compared got: " 
             + Types::to_string(args[0], true) +  " <= " + Types::to_string(args[1], true));
        return Types::apply_num_bool_op(std::less_equal<>{}, args[0], args[1]);
    }

    void validate_args(Args args, std::size_t num, std::string op) {
        if(args.size() != num)
            throw std::runtime_error("expected : "+ std::to_string(num)
             + " arguments to call fucntion: '" + op + "' got: " 
             + std::to_string(args.size()));
    }

    void validate_args_at_least(Args args, std::size_t num, std::string op) {
        if(args.size() < num)
            throw std::runtime_error("not enough arguments for function call: '" + op + "' !");
    }
}