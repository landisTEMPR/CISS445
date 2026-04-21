#ifndef PARSER_H
#define PARSER_H

#include <string>
#include "lexer.h"

struct Parser
{
    Lexer lex;
    std::string errMsg;
    std::string lastBinOp;

    Parser(const std::string &s);

    bool parseExpr(int &result);

private:
    bool parseTerm(int &result);
    bool parseUnary(int &result);
    bool parsePrimary(int &result);
};

#endif
