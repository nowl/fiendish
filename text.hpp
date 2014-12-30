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
    std::string str;
    std::vector<TextToken> tokens;
};

TextCommand parse_text_command(const std::string& str);
void print_text_command(const TextCommand& cmd);

// Draws text command to the screen starting at location line_start,
// min_col and continuing to max_col. Lines are split and continued on
// the subsequent line up to max_lines. The number of lines drawn is
// returned.
int draw_tcmd_fill(const TextCommand& tcmd,
                   int line_start,
                   int min_col,
                   int max_col,
                   int max_lines,
                   bool do_draw = true);
