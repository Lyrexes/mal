#include "core.hpp"
#include "eval.hpp"
#include "types.hpp"
#include "printer.hpp"
#include "reader.hpp"
#include "env.hpp"
#include "MalException.hpp"
#include <functional>
#include <iostream>
#include <fstream>
#include <array>
#include <chrono>

namespace Core {

    Table core_namespace() {
        return Table {
            {"+", Types::Builtin(add_num)},
            {"-", Types::Builtin(sub_num)},
            {"*", Types::Builtin(mul_num)},
            {"/", Types::Builtin(div_num)},
            {"time-ms", Types::Builtin(time_ms)},
            {"meta", Types::Builtin(meta)},
            {"with-meta", Types::Builtin(with_meta)},
            {"fn?", Types::Builtin(is_fn)},
            {"string?", Types::Builtin(is_string)},
            {"number?", Types::Builtin(is_num)},
            {"seq", Types::Builtin(seq)},
            {"conj", Types::Builtin(conj)},
            {"readline", Types::Builtin(read_line)},
            {"pr-str", Types::Builtin(pr_str)},
            {"str", Types::Builtin(str)},
            {"prn", Types::Builtin(prn)},
            {"println", Types::Builtin(println)},
            {"throw", Types::Builtin(throw_)},
            {"apply", Types::Builtin(apply)},
            {"map", Types::Builtin(map)},
            {"vec", Types::Builtin(vec)},
            {"symbol", Types::Builtin(symbol)},
            {"keyword", Types::Builtin(keyword)},
            {"keyword?", Types::Builtin(is_keyword)},
            {"vector", Types::Builtin(vector)},
            {"vector?", Types::Builtin(is_vector)},
            {"sequential?", Types::Builtin(is_sequential)},
            {"hash-map", Types::Builtin(hash_map)},
            {"map?", Types::Builtin(is_map)},
            {"assoc", Types::Builtin(assoc)},
            {"dissoc", Types::Builtin(dissoc)},
            {"get", Types::Builtin(get)},
            {"contains?", Types::Builtin(contains)},
            {"keys", Types::Builtin(keys)},
            {"vals", Types::Builtin(vals)},
            {"list", Types::Builtin(list)},
            {"list?", Types::Builtin(is_list)},
            {"nil?", Types::Builtin(is_nil)},
            {"true?", Types::Builtin(is_true)},
            {"false?", Types::Builtin(is_false)},
            {"symbol?", Types::Builtin(is_symbol)},
            {"empty?", Types::Builtin(is_empty)},
            {"macro?", Types::Builtin(is_macro)},
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

    MalType is_macro(Args args, EnvPtr env) {
        validate_args(args, 1, "time-ms");
        return Types::Bool(args[0].type(Type::LAMBDA) && args[0].is_macro());
    }

    MalType time_ms(Args args, EnvPtr env) {
        validate_args(args, 0, "time-ms");
        const auto clock = std::chrono::system_clock::now();
        auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
                   clock.time_since_epoch()).count();
        return Types::Int(ms);
    }

    MalType meta(Args args, EnvPtr env) {
        validate_args(args, 1, "meta");
        if(!args[0].type(Type::LIST) && !args[0].type(Type::VECTOR)
            && !args[0].type(Type::MAP) && !args[0].type(Type::LAMBDA)
            && !args[0].type(Type::BUILTIN))
            throw std::runtime_error("EOF: meta needs a list, hashmap, function or vector as argument!");
        if(args[0].getMeta())
            return *args[0].getMeta();
        else
            return Types::Nil();
    }

    MalType with_meta(Args args, EnvPtr env){
        validate_args(args, 2, "with-meta");
        if(!args[0].type(Type::LIST) && !args[0].type(Type::VECTOR)
            && !args[0].type(Type::MAP) && !args[0].type(Type::LAMBDA)
            && !args[0].type(Type::BUILTIN))
            throw std::runtime_error("EOF: with-meta needs a list, hashmap, function or vector as argument!");
        args[0].setMeta(std::make_shared<MalType>(args[1]));
        return args[0];
    }

    MalType is_fn(Args args, EnvPtr env) {
        validate_args(args, 1, "fn?");
        if(args[0].type(Type::LAMBDA))
            return Types::Bool(!args[0].is_macro());
        return Types::Bool(args[0].type(Type::BUILTIN));
    }

    MalType is_string(Args args, EnvPtr env) {
        validate_args(args, 1, "string?");
        return Types::Bool(args[0].type(Type::STRING));
    }

    MalType is_num(Args args, EnvPtr env) {
        validate_args(args, 1, "number?");
        return Types::Bool(args[0].type(Type::INT) || args[0].type(Type::FLOAT));
    }

    MalType seq(Args args, EnvPtr env) {
        validate_args(args, 1, "seq");
        if(args[0].type(Type::NIL))
            return args[0];
        if(!args[0].type(Type::LIST) && !args[0].type(Type::VECTOR)
            && !args[0].type(Type::STRING))
            throw std::runtime_error("EOF: seq needs a list, string or vector as argument!");


        if(args[0].type(Type::STRING)) {
            if(args[0].str().empty())
                return Types::Nil();
            auto char_list = std::vector<MalType>{};
            char_list.reserve(args[0].str().size());
            for(auto& el : args[0].str()) 
                char_list.push_back(Types::String({el}));
            return Types::List(char_list);
        } 

        if(args[0].empty())
            return Types::Nil();

        if(args[0].type(Type::LIST))
            return args[0];

        return Types::List(std::get<Container>(args[0].val()));
    }

    MalType conj(Args args, EnvPtr env) {
        validate_args_at_least(args, 1, "conj");
        if(!args[0].type(Type::LIST) && !args[0].type(Type::VECTOR))
            throw std::runtime_error("EOF: conj needs a list or vector as argument!");
        auto items = args.subspan(1);
        auto old_seq = args[0].seq();
        auto new_seq = std::vector<MalType>{old_seq.begin(), old_seq.end()};
        new_seq.reserve(items.size());
        if(args[0].type(Type::LIST)) {
            for(auto &el : items) 
                new_seq.insert(new_seq.begin(), el);
        } else {
            for(auto &el : items) 
                new_seq.insert(new_seq.end(), el);
        }
        return MalType(args[0].id(), new_seq);
    }

    MalType read_line(Args args, EnvPtr env) {
        validate_args(args, 1, "readline");
        if(!args[0].type(Type::STRING))
            throw std::runtime_error("EOF: readline requires a string to prompt the user!");
        if(auto str = Parser::input(args[0].str()))
            return Types::String(*str);
        else
            return Types::Nil();
    }

    MalType symbol(Args args, EnvPtr env) {
        validate_args(args, 1, "symbol");
        if(!args[0].type(Type::STRING))
            throw std::runtime_error("EOF: symbol must be a string! (symbol \"str\") ");
        return Types::Symbol(std::get<std::string>(args[0].val()));
    }

    MalType keyword(Args args, EnvPtr env) {
        validate_args(args, 1, "keyword");
        if(!args[0].type(Type::STRING) && !args[0].type(Type::KEYWORD))
            throw std::runtime_error("EOF: keyword must be a string! (keyword \"str\") ");
        return Types::Keyword(std::get<std::string>(args[0].val()));
    }

    MalType is_keyword(Args args, EnvPtr env) {
        validate_args(args, 1, "keyword");
        return Types::Bool(args[0].type(Type::KEYWORD));
    }

    MalType vector(Args args, EnvPtr env) {
        return Types::Vector(std::vector<MalType>(args.begin(), args.end()));
    }

    MalType is_vector(Args args, EnvPtr env) {
        validate_args(args, 1, "vector?");
        return Types::Bool(args[0].type(Type::VECTOR));
    }

    MalType is_sequential(Args args, EnvPtr env) {
        validate_args(args, 1, "sequential?");
        return Types::Bool(args[0].type(Type::VECTOR) || args[0].type(Type::LIST));
    }

    MalType hash_map(Args args, EnvPtr env) {
        if(args.size() % 2 != 0)
            throw std::runtime_error("EOF: hash-map: expected a even amount arguments!");
        Map_t map{};
        for(std::size_t i = 0; i < args.size(); i += 2) 
            map.insert({args[i], args[i + 1]});
        return Types::Map(map);
    }

    MalType is_map(Args args, EnvPtr env){
        validate_args(args, 1, "map?");
        return Types::Bool(args[0].type(Type::MAP));
    }

    MalType assoc(Args args, EnvPtr env) {
        if(args[0].type(Type::NIL))
            return Types::Nil();
        if(!args[0].type(Type::MAP))
            throw std::runtime_error("EOF: assoc: a Map as its first argument!");
        if(args.size() % 2 == 0)
            throw std::runtime_error("EOF: assoc: key value pairs must be even in size!");
        
        Map_t map = std::get<Map_t>(args[0].val());
        for(std::size_t i = 1; i < args.size(); i += 2) 
            map.insert_or_assign(args[i], args[i + 1]);
        return Types::Map(map);
    }

    MalType dissoc(Args args, EnvPtr env) {

        if(args[0].type(Type::NIL))
            return Types::Nil();

        if(!args[0].type(Type::MAP))
            throw std::runtime_error("EOF: assoc: a Map as its first argument!");

        auto keys = args.subspan(1);
        Map_t map = std::get<Map_t>(args[0].val());

        for(auto& key : keys) 
            map.erase(key);
        return Types::Map(map);
    }

    MalType get(Args args, EnvPtr env) {
        validate_args(args, 2, "get");
        if(args[0].type(Type::NIL))
            return Types::Nil();
        if(!args[0].type(Type::MAP))
            throw std::runtime_error("EOF: get: Needs a Map as its first argument");
        auto& map = std::get<Map_t>(args[0].val());
        if(map.contains(args[1]))
            return map.at(args[1]);
        else
            return Types::Nil();
    }

    MalType contains(Args args, EnvPtr env) {
        validate_args(args, 2, "contains");

        if(args[0].type(Type::NIL))
            return Types::Nil();
        if(!args[0].type(Type::MAP))
            throw std::runtime_error("EOF: contains: Needs a Map as its first argument");
        auto& map = std::get<Map_t>(args[0].val());
        return Types::Bool(map.contains(args[1]));
    }

    MalType keys(Args args, EnvPtr env) {
        validate_args(args, 1, "keys");

        if(args[0].type(Type::NIL))
            return Types::Nil();
        if(!args[0].type(Type::MAP))
            throw std::runtime_error("EOF: keys: Needs a Map as its first argument");

        auto& map = std::get<Map_t>(args[0].val());
        auto list = std::vector<MalType>{};
        list.reserve(map.size());

        for (const auto& [key, _] : map)
            list.push_back(key);

        return Types::List(list);
    }

    MalType vals(Args args, EnvPtr env) {
        validate_args(args, 1, "vals");
        if(args[0].type(Type::NIL))
            return Types::Nil();
        if(!args[0].type(Type::MAP))
            throw std::runtime_error("EOF: vals: Needs a Map as its first argument");

        auto& map = std::get<Map_t>(args[0].val());
        auto list = std::vector<MalType>{};
        list.reserve(map.size());

        for (const auto& [_, value] : map)
            list.push_back(value);

        return Types::List(list);
    }

    MalType map (Args args, EnvPtr env) {
        validate_args(args, 2, "map");
        if((!args[1].type(Type::LIST) && !args[1].type(Type::VECTOR))
         ||(!args[0].type(Type::LAMBDA) && !args[0].type(Type::BUILTIN)))
            throw std::runtime_error("EOF: map needs a fucntion and a sequence! '(map func seq)'");

        auto result = std::vector<MalType>{};
        result.reserve(args[1].seq().size());

        auto temp_param = std::array<MalType ,2>{};
        temp_param[0] = args[0];

        auto ast_env = EvalPair{};

        for(auto &el: args[1].seq()) {
            temp_param[1] = el;
            if(args[0].type(Type::BUILTIN)) {
                result.push_back(Eval::apply(temp_param, env));
            } else {
                ast_env = Eval::apply_lambda(temp_param);
                result.push_back(Eval::eval(ast_env.ast, ast_env.env));
            }
        }
        return Types::List(result);
    }

    MalType apply (Args args, EnvPtr env) {
        validate_args_at_least(args, 2, "apply");
        if(!args[0].type(Type::LAMBDA) && !args[0].type(Type::BUILTIN))
            throw std::runtime_error("EOF: apply needs a function to apply arguments! got: " + 
            Types::to_string(args[0], true));
        if(!args.back().type(Type::LIST) && !args.back().type(Type::VECTOR))
            throw std::runtime_error("EOF: apply needs a sequence of arguments! got: " + 
            Types::to_string(args[1], true));

        auto params = std::vector<MalType>{args.back().seq().begin(), args.back().seq().end()};
        if(args.size() > 2) {
            auto arguments = args.subspan(1, args.size()-2);
            params.insert(params.begin(), arguments.begin(), arguments.end());
        }
        params.insert(params.begin(), args[0]);
        if(args[0].type(Type::BUILTIN))
            return Eval::apply(params, env);
        auto ast_env = Eval::apply_lambda(params);
        return Eval::eval(ast_env.ast, ast_env.env);
    }

    MalType throw_ (Args args, EnvPtr env) {
        validate_args(args, 1, "throw");
        throw MalException(args[0]);
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
        throw std::runtime_error("EOF: first: needs a list or vector as argument!");
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
        throw std::runtime_error("EOF: first: needs a list or vector as argument!");
    }

    MalType vec(Args args, EnvPtr env) {
       validate_args(args, 1, "vec");
        if(!args[0].type(Type::LIST) && !args[0].type(Type::VECTOR))
            throw std::runtime_error("EOF: vec needs a list or vector as argument! got : "
                + Types::to_string(args[0],true));
        return Types::Vector(std::get<Container>(args[0].val()));
    }

    MalType cons(Args args, EnvPtr env) {
       validate_args(args, 2, "cons");
        if(!args[1].type(Type::LIST) && !args[1].type(Type::VECTOR))
            throw std::runtime_error("EOF: cons needs a list at its second argument! got: "
             + Types::to_string(args[1], true));
        auto vec = std::get<std::vector<MalType>>(args[1].val());
        vec.insert(vec.begin(), std::move(args[0]));
        return Types::List(std::move(vec));
    }

    MalType concat(Args args, EnvPtr env) {
        if(args.size() == 1) {
            if(!args[0].type(Type::LIST) && !args[0].type(Type::VECTOR))
                throw std::runtime_error("EOF: can only concat lists or vector! got:"
                 + Types::to_string(args[0], true));
            return Types::List(std::get<Container>(args[0].val()));
        }
        auto new_list = std::vector<MalType>{};
        for(auto&  list : args) {
            if(!list.type(Type::LIST) && !list.type(Type::VECTOR))
                throw std::runtime_error("EOF: can only concat lists or vector! got:"
                 + Types::to_string(list, true));
            auto &curr_list = std::get<Container>(list.val());
            new_list.insert(new_list.end(), curr_list.begin(), curr_list.end());
        }
        return Types::List(new_list);
    }

    MalType reset(Args args, EnvPtr env) {
       validate_args(args, 2, "reset!");
        if (!args[0].type(Type::ATOM))
            throw std::runtime_error("EOF: reset needs a atom to change itsval()ue!");
        auto atom = std::get<Atom_t>(args[0].val());
        *atom.ref = args[1];
        return args[1];
    }

    MalType swap(Args args, EnvPtr env) {
       validate_args_at_least(args, 2, "swap!");
        if(!args[0].type(Type::ATOM) ||
         (!args[1].type(Type::LAMBDA) && !args[1].type(Type::BUILTIN)))
            throw std::runtime_error("EOF: swap! expects a atom and a function as arguments");
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
        *atom_val.ref = end_val;
        return end_val;
    }

    MalType deref(Args args, EnvPtr env) {
       validate_args(args, 1, "atom");
        if(!args[0].type(Type::ATOM))
            throw std::runtime_error("EOF: Only can derefrence atoms!");
        return *std::get<Atom_t>(args[0].val()).ref;  
    }

    MalType atom(Args args, EnvPtr env) {
        validate_args(args, 1, "atom");
        return Types::Atom(args[0]);
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

    MalType is_nil(Args args, EnvPtr env) {
        validate_args(args, 1, "nil?");
        return Types::Bool(args[0].type(Type::NIL));
    }

    MalType is_true(Args args, EnvPtr env) {
        validate_args(args, 1, "true?");
        return Types::Bool(args[0].type(Type::BOOL) && args[0].boolv());
    }

    MalType is_false(Args args, EnvPtr env) {
        validate_args(args, 1, "false?");
        return Types::Bool(args[0].type(Type::BOOL) && !args[0].boolv());
    }

    MalType is_symbol(Args args, EnvPtr env) {
        validate_args(args, 1, "symbol?");
        return Types::Bool(args[0].type(Type::SYMBOL));
    }

    MalType is_empty(Args args, EnvPtr env) {
       validate_args(args, 1, "empty?");
        if(!args[0].type(Type::LIST) && !args[0].type(Type::VECTOR))
            throw std::runtime_error("EOF: Only sequences can be empty!");
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
            throw std::runtime_error("EOF: cant compare these types: " + Types::to_string(args[0], true)
             + " = " + Types::to_string(args[1], true));
        if((args[0].type(Type::LIST) || args[0].type(Type::VECTOR))
         &&(args[1].type(Type::LIST) || args[1].type(Type::VECTOR)))
            return Types::Bool(args[0] == args[1]);
        return Types::Bool(args[0] == args[1] && args[0].id() == args[1].id());
    }

    MalType gt(Args args, EnvPtr env) {
       validate_args(args, 2, ">");
        if(args[0].val().index() != 0 || args[1].val().index() != 0)
            throw std::runtime_error("EOF: Only numbers can be compared got: " 
             + Types::to_string(args[0], true) +  " > " + Types::to_string(args[1], true));
        return Types::apply_num_bool_op(std::greater<>{}, args[0], args[1]);
    }

    MalType lt(Args args, EnvPtr env) {
       validate_args(args, 2, "<");
        if(args[0].val().index() != 0 || args[1].val().index() != 0)
            throw std::runtime_error("EOF: Only numbers can be compared got: " 
             + Types::to_string(args[0], true) +  " < " + Types::to_string(args[1], true));
        return Types::apply_num_bool_op(std::less<>{}, args[0], args[1]);
    }

    MalType gt_or_eq(Args args, EnvPtr env) {
       validate_args(args, 2, ">=");
        if(args[0].val().index() != 0 || args[1].val().index() != 0)
            throw std::runtime_error("EOF: Only numbers can be compared got: " 
             + Types::to_string(args[0], true) +  " >= " + Types::to_string(args[1], true));
        return Types::apply_num_bool_op(std::greater_equal<>{}, args[0], args[1]);
    }

    MalType lt_or_eq(Args args, EnvPtr env) {
       validate_args(args, 2, "<=");
        if(args[0].val().index() != 0 || args[1].val().index() != 0)
            throw std::runtime_error("EOF: Only numbers can be compared got: " 
             + Types::to_string(args[0], true) +  " <= " + Types::to_string(args[1], true));
        return Types::apply_num_bool_op(std::less_equal<>{}, args[0], args[1]);
    }

    void validate_args(Args args, std::size_t num, std::string op) {
        if(args.size() != num)
            throw std::runtime_error("EOF: expected : "+ std::to_string(num)
             + " arguments to call fucntion: '" + op + "' got: " 
             + std::to_string(args.size()));
    }

    void validate_args_at_least(Args args, std::size_t num, std::string op) {
        if(args.size() < num)
            throw std::runtime_error("EOF: not enough arguments for function call: '" + op + "' !");
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
            throw std::runtime_error("EOF: Could not open file");
        while (std::getline(rfile, line)) {
            if(!line.empty()) 
                acc += line + "\n";
        }
        rfile.close();
        return Types::String(acc);
    }
}