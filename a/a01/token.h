#ifndef TOKEN_H
#define TOKEN_H

#include <string>

enum TokenType
{
  TOK_INT,
  TOK_IDENT,
  TOK_PLUS,
  TOK_MINUS,
  TOK_STAR,
  TOK_SLASH,
  TOK_PERCENT,
  TOK_EOF,
  TOK_ERROR
};

struct Token
{
    TokenType type;
    int intVal;
    std::string strVal;
};

#endif
