#ifndef PRINTER_HPP
#define PRINTER_HPP
#include <string>

class MalType;
namespace Printer {
    std::string pr_str(const MalType& val, bool readably);
}

#endif //PRINTER_HPP
