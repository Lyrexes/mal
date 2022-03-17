#ifndef MALEXCEPTION_HPP
#define MALEXCEPTION_HPP
#include <memory>
#include <exception>
struct MalType;

class MalException : public std::exception {
public:
    MalException(MalType type);
    virtual const char* what() const noexcept override;
    MalType get() const noexcept;
private:
    std::unique_ptr<MalType> m_type;
    std::string m_msg;
};








#endif