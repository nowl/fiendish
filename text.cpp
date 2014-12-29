#include "text.hpp"
#include "color.hpp"
#include "globals.hpp"

static void
fill_breaks(std::vector<TextToken>& tokens,
            int init_break_count)
{                                              
    auto iter = tokens.end();
    int breakCount = init_break_count;
    for(;;) {
        --iter;
        
        if (iter->toNextBreak != -1)
            break;
        
        iter->toNextBreak = breakCount;

        if(iter->token == TextTokenType::CHAR)
            breakCount++;
                        
        if (iter == tokens.begin())
            break;
    }
}

// parses commands as {[color name]} and {[color_reset]}
TextCommand
parse_text_command(const std::string& str)
{
    TextCommand command{str};

    const char *s = str.c_str();
    int i=0, cmd_start = 0;
    bool in_command = false;
    for (i=0; i<(int)str.size(); i++) {
        if (!in_command) {
            if (s[i] != '{' && s[i+1] != '[') {
                command.tokens.push_back(TextToken{TextTokenType::CHAR, i, i+1, -1});

                // process breaks
                if (s[i] == ' ' || s[i] == '\n' || s[i] == '\t')
                    fill_breaks(command.tokens, 0);
            } else {
                in_command = true;
                cmd_start = i;
            }
        } else {
            if (s[i] == ']' && s[i+1] == '}') {
                command.tokens.push_back(TextToken{TextTokenType::COMMAND, cmd_start, i+2, -1});
                in_command = false;
                i += 2-1;
            }
        }
    }
        
    fill_breaks(command.tokens, 1);

    return command;
}

void print_text_command(const TextCommand& cmd)
{
    printf("token length = %lu\n", cmd.tokens.size());
    for(unsigned int i=0; i<cmd.tokens.size(); i++) {
        const TextToken& tkn = cmd.tokens[i];
        printf("%u: token_type: %d, start: %d, end: %d, toNextBreak: %d\n",
               i, static_cast<int>(tkn.token), tkn.start, tkn.end, tkn.toNextBreak);
               
    }
}

static Color draw_fg_color;
static Color draw_bg_color;
static bool delayed_break_draw;
static char delayed_break_char;
static int delayed_break_row;
static int delayed_break_col;

int
draw_tcmd_fill(const TextCommand& tcmd,
               int line_start,
               int min_col,
               int max_col,
               int max_lines)
{
    draw_fg_color = Color(1, 1, 1);
    draw_bg_color = Color(0, 0, 0);
    delayed_break_draw = false;

    int draw_col = min_col;
    int draw_row = line_start;
    auto iter = tcmd.tokens.begin();
    
    for (; iter != tcmd.tokens.end(); iter++) {
        switch (iter->token) {
        case TextTokenType::CHAR:
        {
            int cols_left = max_col - draw_col + 1;
            if (iter->toNextBreak > cols_left) {
                delayed_break_draw = false;
                draw_col = min_col;
                draw_row++;
                
                // make sure we haven't exceeded max_lines
                if (draw_row - line_start + 1 > max_lines) {
                    return max_lines;
                }
            } else if (iter->toNextBreak == 0) {
                delayed_break_char = tcmd.str[iter->start];
                delayed_break_draw = true;
                delayed_break_col = draw_col;
                delayed_break_row = draw_row;
            } else if (delayed_break_draw) {
                delayed_break_draw = false;
                putchar(delayed_break_col, delayed_break_row,
                        delayed_break_char,
                        draw_fg_color,
                        draw_bg_color);
            }
            
            if (!delayed_break_draw) {
                putchar(draw_col, draw_row, tcmd.str[iter->start],
                        draw_fg_color,
                        draw_bg_color);
            }

            draw_col++;
            
            break;
        }
        case TextTokenType::COMMAND:
            if (tcmd.str.substr(iter->start+2, 11) == "color_reset")
            {
                draw_fg_color = Color(1, 1, 1);
                draw_bg_color = Color(0, 0, 0);
            }
            else if (tcmd.str.substr(iter->start+2, 6) == "color ")
            {
                int start = iter->start+8;
                int len = iter->end - (iter->start+8) - 2;
                auto color_name = tcmd.str.substr(start, len);
                assert(ColorByName.find(color_name) != ColorByName.end());
                draw_fg_color = ColorByName[color_name];
            }
            else if (tcmd.str.substr(iter->start+2, 9) == "colorhex ")
            {
                int start = iter->start+2+9;
                int len = 6;
                const char *color_hex = tcmd.str.substr(start, len).c_str();
                int r, g, b;
                int vals_assigned = sscanf(color_hex, "%2x%2x%2x", &r, &g, &b);
                assert(vals_assigned == 3);
                draw_fg_color = Color(r/256.0f, g/256.0f, b/256.0f);
            }
            break;
        }
    }

    return draw_row - line_start + 1;
}
