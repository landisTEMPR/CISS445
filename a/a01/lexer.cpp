#include <cctype>
#include <cstdlib>
#include "lexer.h"

Lexer::Lexer(const std::string &s) : src(s), pos(0) {}

void Lexer::skipSpaces()
{
    while (pos < src.size() && std::isspace((unsigned char)src[pos]))
    {
      pos++;
    }
}

Token Lexer::next()
{
    skipSpaces();

    Token t;

    if (pos >= src.size())
    {
      t.type = TOK_EOF;
      return t;
    }

    char c = src[pos];

    if (c == '+') { pos++; t.type = TOK_PLUS; return t; }
    if (c == '-') { pos++; t.type = TOK_MINUS; return t; }
    if (c == '*') { pos++; t.type = TOK_STAR; return t; }
    if (c == '/') { pos++; t.type = TOK_SLASH; return t; }
    if (c == '%') { pos++; t.type = TOK_PERCENT; return t; }

    if (std::isdigit((unsigned char)c))
    {
        std::string num;
        while (pos < src.size() && std::isdigit((unsigned char)src[pos]))
        {
          num += src[pos++];
        }
        if (num.size() > 1 && num[0] == '0')
        {
          t.type = TOK_ERROR;
          t.strVal = "OCTAL";
          return t;
        }
        t.type = TOK_INT;
        t.intVal = std::atoi(num.c_str());
        return t;
    }

    if (std::isalpha((unsigned char)c) || c == '_')
    {
        std::string ident;
        while (pos < src.size() && (std::isalnum((unsigned char)src[pos]) || src[pos] == '_'))
        { 
          ident += src[pos++];
        }
        t.type = TOK_IDENT;
        t.strVal = ident;
        return t;
    }

    t.type = TOK_ERROR;
    t.strVal = std::string(1, c);
    pos++;
    return t;
}

Token Lexer::peek()
{
    size_t savedPos = pos;
    Token t = next();
    pos = savedPos;
    return t;
}
