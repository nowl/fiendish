#pragma once

#include <string>
#include <vector>

#include "color.hpp"

enum class TextTokenType {
    CHAR,
    COMMAND
};

struct TextToken {
    TextTokenType token;
    int start;
    int end;
    int toNextBreak;
};

struct TextCommand {
    const std::string str;
    std::vector<TextToken> tokens;
};

TextCommand parse_text_command(const std::string& str);
void print_text_command(const TextCommand& cmd);
