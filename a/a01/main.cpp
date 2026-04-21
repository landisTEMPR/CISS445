// File: main.cpp
// Author : Brysen Landis

#include <iostream>
#include <limits>
#include <string>
#include <map>
#include <cctype>
#include "parser.h"

const int MAX_BUF = 1024;

std::map<std::string, int> symtable;

std::string trim(const std::string &s)
{
    int start = 0;
    int end   = (int)s.size() - 1;
    while (start <= end && std::isspace((unsigned char)s[start])) { start++; }
    while (end >= start && std::isspace((unsigned char)s[end])) { end--; }
    if (start > end)
    {
        return "";
    }
    return s.substr(start, end - start + 1);
}

// alphanumeric + underscore, first char non-numeric
bool isValidIdentifier(const std::string &s)
{
    if (s.empty())
    {
        return false;
    }
    if (!std::isalpha((unsigned char)s[0]) && s[0] != '_')
    {
        return false;
    }
    for (size_t i = 1; i < s.size(); i++)
    {
        if (!std::isalnum((unsigned char)s[i]) && s[i] != '_')
        {
            return false;
        }
    }
    return true;
}

// leading zero followed by more digits
bool isOctalLike(const std::string &s)
{
    size_t i = 0;
    if (i < s.size() && (s[i] == '+' || s[i] == '-'))
    {
        i++;
    }
    if (i < s.size() && s[i] == '0' && i + 1 < s.size() && std::isdigit((unsigned char)s[i + 1]))
    {
        return true;
    }
    return false;
}

void processLine(const std::string &rawLine, bool interactive)
{
    std::string line = trim(rawLine);
    if (line.empty())
    {
        return;
    }

    // check for assignment
    size_t eqPos = std::string::npos;
    for (size_t i = 0; i < line.size(); i++)
    {
        if (line[i] == '=')
        {
            eqPos = i;
            break;
        }
    }

    if (eqPos != std::string::npos)
    {
        std::string lhs = trim(line.substr(0, eqPos));
        std::string rhs = trim(line.substr(eqPos + 1));

        if (!isValidIdentifier(lhs))
        {
            std::cout << "ERROR: \"" << lhs << "\" is not a variable name" << std::endl;
            return;
        }

        Parser p(rhs);
        int    value;
        if (!p.parseExpr(value))
        {
            std::cout << p.errMsg << std::endl;
            return;
        }

        symtable[lhs] = value;
        return;
    }

    // catch bare octal before parsing
    if (isOctalLike(line))
    {
        size_t i = 0;
        if (!line.empty() && (line[i] == '+' || line[i] == '-'))
        {
            i++;
        }
        bool allDigits = true;
        for (size_t j = i; j < line.size(); j++)
        {
            if (!std::isdigit((unsigned char)line[j]))
            {
                allDigits = false;
                break;
            }
        }
        if (allDigits)
        {
            std::cout << "ERROR: I don't understand octals yet" << std::endl;
            return;
        }
    }

    Parser p(line);
    int    value;
    if (!p.parseExpr(value))
    {
        std::cout << p.errMsg << std::endl;
        return;
    }

    if (interactive)
    {
        std::cout << value << std::endl;
    }
}

int main(int argc, char *argv[])
{
    bool interactive = true;

    while (true)
    {
        if (interactive)
        {
            std::cout << ">>> " << std::flush;
        }

        char buf[MAX_BUF];
        std::cin.getline(buf, MAX_BUF);

        if (std::cin.eof())
        {
            break;
        }
        if (std::cin.fail() || std::cin.bad())
        {
            std::cin.clear();
            std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
            continue;
        }

        // strip trailing \r
        std::string line(buf);
        while (!line.empty() && (line.back() == '\r' || line.back() == '\n'))
        {
            line.pop_back();
        }

        processLine(line, interactive);
    }

    return 0;
}
