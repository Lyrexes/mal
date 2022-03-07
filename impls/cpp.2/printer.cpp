#include "printer.hpp"

namespace Printer {
    std::string pr_str(const MalType& val, bool readably) {
        return Types::to_string(val, readably);
    }
}
