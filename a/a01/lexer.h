#ifndef LEXER_H
#define LEXER_H

#include <string>
#include "token.h"

struct Lexer
{
  public:
    std::string src;
    size_t      pos;

    Lexer(const std::string &s);

    Token next();
    Token peek();

  private:
    void skipSpaces();
};

#endif
