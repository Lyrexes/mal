#include "core.hpp"
#include "eval.hpp"
#include "types.hpp"
#include "printer.hpp"
#include "reader.hpp"
#include "env.hpp"
#include <functional>
#include <iostream>
#include <fstream>
#include <array>

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
            {"vec", Types::Builtin(vec)},
            {"list", Types::Builtin(list)},
            {"list?", Types::Builtin(is_list)},
            {"empty?", Types::Builtin(is_empty)},
            {"count", Types::Builtin(count)},
            {"read-string", Types::Builtin(read_str)},
            {"slurp", Types::Builtin(slurp)},
            {"reset!", Types::Builtin(reset)},
            {"atom", Types::Builtin(atom)},
            {"atom?", Types::Builtin(is_atom)},
            {"swap!", Types::Builtin(swap)},
            {"deref", Types::Builtin(deref)},
            {"cons", Types::Builtin(cons)},
            {"concat", Types::Builtin(concat)},
            {"nth", Types::Builtin(nth)},
            {"first", Types::Builtin(first)},
            {"rest", Types::Builtin(rest)},
            {"=", Types::Builtin(equals)},
            {"<", Types::Builtin(lt)},
            {">", Types::Builtin(gt)},
            {"<=", Types::Builtin(lt_or_eq)},
            {">=", Types::Builtin(gt_or_eq)},
        };
    }

    MalType nth(Args args, EnvPtr env) {
       validate_args(args, 2, "nth");
        return args[0].nth(args[1].intv());
    }

    MalType first(Args args, EnvPtr env) {
        validate_args(args, 1, "first");
        if(args[0].type(Type::NIL))
            return Types::Nil();
        if(args[0].type(Type::LIST) || args[0].type(Type::VECTOR)) {
            if(args[0].empty())
                return Types::Nil();
            return args[0].fst();
        }
        throw std::runtime_error("first: needs a list or vector as argument!");
    }

    MalType rest(Args args, EnvPtr env) {
        validate_args(args, 1, "rest");
        if(args[0].type(Type::NIL))
            return Types::List({});
        if(args[0].type(Type::LIST) || args[0].type(Type::VECTOR)) {
            if(args[0].seq().size() <= 1)
                return Types::List({});
            auto new_seq = args[0].sub_seq(1);
            return Types::List({new_seq.begin(), new_seq.end()});
        }
        throw std::runtime_error("first: needs a list or vector as argument!");
    }

    MalType vec(Args args, EnvPtr env) {
       validate_args(args, 1, "vec");
        if(!args[0].type(Type::LIST) && !args[0].type(Type::VECTOR))
            throw std::runtime_error("vec needs a list or vector as argument! got : "
                + Types::to_string(args[0],true));
        return Types::Vector(std::get<Container>(args[0].val()));
    }

    MalType cons(Args args, EnvPtr env) {
       validate_args(args, 2, "cons");
        if(!args[1].type(Type::LIST) && !args[1].type(Type::VECTOR))
            throw std::runtime_error("cons needs a list at its second argument! got: "
             + Types::to_string(args[1], true));
        auto vec = std::get<std::vector<MalType>>(args[1].val());
        vec.insert(vec.begin(), std::move(args[0]));
        return Types::List(std::move(vec));
    }

    MalType concat(Args args, EnvPtr env) {
        if(args.size() == 1) {
            if(!args[0].type(Type::LIST) && !args[0].type(Type::VECTOR))
                throw std::runtime_error("can only concat lists or vector! got:"
                 + Types::to_string(args[0], true));
            return Types::List(std::get<Container>(args[0].val()));
        }
        auto new_list = std::vector<MalType>{};
        for(auto&  list : args) {
            if(!list.type(Type::LIST) && !list.type(Type::VECTOR))
                throw std::runtime_error("can only concat lists or vector! got:"
                 + Types::to_string(list, true));
            auto &curr_list = std::get<Container>(list.val());
            new_list.insert(new_list.end(), curr_list.begin(), curr_list.end());
        }
        return Types::List(new_list);
    }

    MalType reset(Args args, EnvPtr env) {
       validate_args(args, 2, "reset!");
        if (!args[0].type(Type::ATOM))
            throw std::runtime_error("reset needs a atom to change itsval()ue!");
        auto atom = std::get<Atom_t>(args[0].val());
        if(atom.var) 
            env->set(*atom.var, Types::Atom(args[1], *atom.var));
        return args[1];
    }

    MalType swap(Args args, EnvPtr env) {
       validate_args_at_least(args, 2, "swap!");
        if(!args[0].type(Type::ATOM) ||
         (!args[1].type(Type::LAMBDA) && !args[1].type(Type::BUILTIN)))
            throw std::runtime_error("swap! expects a atom and a function as arguments");
        auto atom = args[0];
        auto func = args[1];
        auto lam_args = args;
        auto atom_val = std::get<Atom_t>(atom.val());
        lam_args[0] = func;
        lam_args[1] = *atom_val.ref;
        auto end_val = Types::Nil();
        if(func.type(Type::LAMBDA)) {
            auto res = Eval::apply_lambda(lam_args);
            end_val = Eval::eval(res.ast, res.env);
        } else {
            end_val = Eval::apply(lam_args, env);
        }
        if(atom_val.var) 
            env->change_atom(*atom_val.var, Types::Atom(end_val, atom_val.var));
        return end_val;
    }

    MalType deref(Args args, EnvPtr env) {
       validate_args(args, 1, "atom");
        if(!args[0].type(Type::ATOM))
            throw std::runtime_error("Only can derefrence atoms!");
        return *std::get<Atom_t>(args[0].val()).ref;  
    }

    MalType atom(Args args, EnvPtr env) {
       validate_args(args, 1, "atom");
        return Types::Atom(args[0], {});
    }   

    MalType is_atom(Args args, EnvPtr env) {
       validate_args(args, 1, "is_atom");
        return Types::Bool(args[0].type(Type::ATOM));
    }

    MalType pr_str(Args args, EnvPtr env) {
        auto str = std::accumulate(args.begin(), args.end(),
         std::string{}, [](auto&& acc, const auto& val) {
            return std::move(acc) + Types::to_string(val, true) + " "; 
        });
        if(!str.empty())
            str.pop_back();
        return Types::String(str);
    }
    
    MalType str(Args args, EnvPtr env) {
        auto str = std::accumulate(args.begin(), args.end(),
         std::string{}, [](auto&& acc, const auto& val) {
            return std::move(acc) + Types::to_string(val, false); 
        });
        return Types::String(str);
    }

    MalType println(Args args, EnvPtr env) {
        auto str = std::accumulate(args.begin(), args.end(),
         std::string{}, [](auto&& acc, const auto& val) {
            return std::move(acc) + Types::to_string(val, false) + " "; 
        });
        if(!str.empty())
            str.pop_back();
        std::cout << str << std::endl;
        return Types::Nil();
    }

    MalType prn(Args args, EnvPtr env) {
        auto str = std::accumulate(args.begin(), args.end(),
         std::string{}, [](auto&& acc, const auto& val) {
            return std::move(acc) + Types::to_string(val, true) + " "; 
        });
        if(!str.empty())
            str.pop_back();
        std::cout << str << std::endl;
        return Types::Nil();
    }

    MalType add_num(Args args, EnvPtr env) {
       validate_args(args, 2, "+");
        return Types::apply_num_op(std::plus<>{}, args[0], args[1]);
    }
    MalType mul_num(Args args, EnvPtr env) {
       validate_args(args, 2, "*");
        return Types::apply_num_op(std::multiplies<>{}, args[0], args[1]);
    }
    MalType sub_num(Args args, EnvPtr env) {
       validate_args(args, 2, "-");
        return Types::apply_num_op(std::minus<>{}, args[0], args[1]);
    }
    MalType div_num(Args args, EnvPtr env){
       validate_args(args, 2, "/");
        return Types::apply_num_op(std::divides<>{}, args[0], args[1]);
    }

    MalType list(Args args, EnvPtr env) {
        return Types::List({args.begin(), args.end()});
    }

    MalType is_list(Args args, EnvPtr env) {
       validate_args(args, 1, "list?");
        return Types::Bool(args[0].type(Type::LIST));
    }

    MalType is_empty(Args args, EnvPtr env) {
       validate_args(args, 1, "empty?");
        if(!args[0].type(Type::LIST) && !args[0].type(Type::VECTOR))
            throw std::runtime_error("Only sequences can be empty!");
        return Types::Bool(args[0].empty());
    }

    MalType count(Args args, EnvPtr env) {
       validate_args(args, 1, "count");
        if(args[0].type(Type::LIST) || args[0].type(Type::VECTOR))
            return Types::Int(args[0].seq().size());
        return Types::Int(0);
    }

    MalType equals(Args args, EnvPtr env) {
       validate_args(args, 2, "=");
        auto type = args[0].id();
        if(type == Type::BUILTIN || type == Type::LAMBDA)
            throw std::runtime_error("cant compare these types: " + Types::to_string(args[0], true)
             + " = " + Types::to_string(args[1], true));
        if((args[0].type(Type::LIST) || args[0].type(Type::VECTOR))
         &&(args[1].type(Type::LIST) || args[1].type(Type::VECTOR)))
            return Types::Bool(args[0] == args[1]);
        return Types::Bool(args[0] == args[1] && args[0].id() == args[1].id());
    }

    MalType gt(Args args, EnvPtr env) {
       validate_args(args, 2, ">");
        if(args[0].val().index() != 0 || args[1].val().index() != 0)
            throw std::runtime_error("Only numbers can be compared got: " 
             + Types::to_string(args[0], true) +  " > " + Types::to_string(args[1], true));
        return Types::apply_num_bool_op(std::greater<>{}, args[0], args[1]);
    }

    MalType lt(Args args, EnvPtr env) {
       validate_args(args, 2, "<");
        if(args[0].val().index() != 0 || args[1].val().index() != 0)
            throw std::runtime_error("Only numbers can be compared got: " 
             + Types::to_string(args[0], true) +  " < " + Types::to_string(args[1], true));
        return Types::apply_num_bool_op(std::less<>{}, args[0], args[1]);
    }

    MalType gt_or_eq(Args args, EnvPtr env) {
       validate_args(args, 2, ">=");
        if(args[0].val().index() != 0 || args[1].val().index() != 0)
            throw std::runtime_error("Only numbers can be compared got: " 
             + Types::to_string(args[0], true) +  " >= " + Types::to_string(args[1], true));
        return Types::apply_num_bool_op(std::greater_equal<>{}, args[0], args[1]);
    }

    MalType lt_or_eq(Args args, EnvPtr env) {
       validate_args(args, 2, "<=");
        if(args[0].val().index() != 0 || args[1].val().index() != 0)
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

    MalType read_str(Args args, EnvPtr env) {
       validate_args(args, 1, "read-string");
        return Parser::read_str(std::get<std::string>(args[0].val()), regex);
    }
    
    MalType slurp(Args args, EnvPtr env) {
       validate_args(args, 1, "slurp");
        auto line = std::string{};
        auto acc = std::string{};
        std::ifstream rfile;
        rfile.open(std::get<std::string>(args[0].val()));
        if(!rfile.is_open())
            throw std::runtime_error("Could not open file");
        while (std::getline(rfile, line)) {
            if(!line.empty()) 
                acc += line + "\n";
        }
        rfile.close();
        return Types::String(acc);
    }
}