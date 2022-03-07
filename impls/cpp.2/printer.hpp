#ifndef PRINTER_HPP
#define PRINTER_HPP
#include <string>
#include "types.hpp"

namespace Printer {
    using MalType = Types::MalType;

    std::string pr_str(const MalType& val, bool readably);
}

#endif //PRINTER_HPP
