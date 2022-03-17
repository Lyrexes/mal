#include "MalException.hpp"
#include "types.hpp"


MalException::MalException(MalType type) 
: m_type(std::make_unique<MalType>(type)){
    m_msg = "MalException: " + Types::to_string(type, true);
}

const char* MalException::what() const noexcept { 
    return m_msg.c_str();
}

MalType MalException::get() const noexcept {
    return *m_type;
}