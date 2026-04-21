#include <map>
#include <string>
#include "parser.h"

// defined in main.cpp
extern std::map<std::string, int> symtable;

Parser::Parser(const std::string &s) : lex(s) {}

bool Parser::parseExpr(int &result)
{
    int left;
    if (!parseTerm(left))
    {
    return false;
    }

    while (true)
    {
      Token t = lex.peek();
      if (t.type != TOK_PLUS && t.type != TOK_MINUS)
      {
        break;
      }

      lex.next();
      std::string op = (t.type == TOK_PLUS) ? "+" : "-";
      lastBinOp      = op;

      if (lex.peek().type == TOK_EOF)
      {
        errMsg = "ERROR: right operand for " + op + " is missing";
        return false;
      }

      int right;
      if (!parseTerm(right))
      {
        return false;
      }

      if (op == "+") { left += right; }
      else { left -= right; }
    }

    Token t = lex.peek();
    if (t.type != TOK_EOF)
    {
      if (t.type == TOK_IDENT)
      {
        errMsg = "ERROR: \"" + t.strVal + "\" is neither a value nor found in the symtable";
      }
      else
      {
        errMsg = "ERROR: unexpected token";
      }
      return false;
    }

    result = left;
    return true;
}

bool Parser::parseTerm(int &result)
{
  int left;
  if (!parseUnary(left))
  {
    return false;
  }

  while (true)
  {
    Token t = lex.peek();
    if (t.type != TOK_STAR && t.type != TOK_SLASH && t.type != TOK_PERCENT)
    {
      break;
    }

      lex.next();
      std::string op = (t.type == TOK_STAR) ? "*" : (t.type == TOK_SLASH ? "/" : "%");
      lastBinOp      = op;

      if (lex.peek().type == TOK_EOF)
      {
        errMsg = "ERROR: right operand for " + op + " is missing";
        return false;
      }

      int right;
      if (!parseUnary(right))
      {
        return false;
      }

      if (op == "*")
      {
        left *= right;
      }
      else if (op == "/")
      {
        if (right == 0) { errMsg = "ERROR: division by zero"; return false; }
        left /= right;
      }
      else
      {
        if (right == 0) { errMsg = "ERROR: division by zero"; return false; }
        left %= right;
      }
    }

    result = left;
    return true;
}

bool Parser::parseUnary(int &result)
{
  Token t = lex.peek();

  if (t.type == TOK_PLUS)
  {
    lex.next();
    int val;
    if (!parseUnary(val))
    {
      return false;
    }
      result = val;
      return true;
    }

  if (t.type == TOK_MINUS)
  {
    lex.next();
    int val;
    if (!parseUnary(val))
    {
      return false;
    }
    result = -val;
    return true;
  }

  return parsePrimary(result);
}

bool Parser::parsePrimary(int &result)
{
  Token t = lex.next();
  if (t.type == TOK_EOF)
  {
    errMsg = "ERROR: missing operand";
    return false;
  }
  if (t.type == TOK_ERROR)
  {
    if (t.strVal == "OCTAL")
    {
      errMsg = "ERROR: I don't understand octals yet";
    }
    else
    {
      errMsg = "ERROR: unexpected character '" + t.strVal + "'";
    }
    return false;
  }
  if (t.type == TOK_INT)
  {
    result = t.intVal;
    return true;
  }
  if (t.type == TOK_IDENT)
  {
    auto it = symtable.find(t.strVal);
    if (it == symtable.end())
    {
      errMsg = "ERROR: \"" + t.strVal + "\" is neither a value nor found in the symtable";
      return false;
    }
    result = it->second;
    return true;
  }
  errMsg = "ERROR: unexpected token";
  return false;
}
