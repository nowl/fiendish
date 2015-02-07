#include "fiendish.h"

enum {
    TEXT_TOKEN_CHAR,
    TEXT_TOKEN_COLOR,
};

struct text_token {
    int type;
    union {
        char chr;
        struct color clr;
    } data;
    int to_next_break;
};

static void
fill_breaks(struct mem_buf* mb,
            int init_break_count)
{
    struct text_token *tokens = mb->mem;
    int num_tokens = mb->usage / sizeof(struct text_token);
    int i;
    
    int breakCount = init_break_count;
    for(i=num_tokens-1; i>=0; i--) {
        if (tokens[i].to_next_break != -1)
            break;
        
        tokens[i].to_next_break = breakCount;
        
        if (tokens[i].type == TEXT_TOKEN_CHAR)
            breakCount++;
    }
}

static void
compile_command(char *s, int start, int end, struct text_token *token)
{
    if (substr_compare(s, start+2, start+2+6, "creset")) {
        token->type = TEXT_TOKEN_COLOR;
        token->data.clr.r = 0;
        token->data.clr.g = 0;
        token->data.clr.b = 0;
    } else if (substr_compare(s, start+2, start+2+5, "color")) {
        token->type = TEXT_TOKEN_COLOR;

        int result = sscanf(&s[start+2+7], "%f %f %f",
                            &(token->data.clr.r),
                            &(token->data.clr.g),
                            &(token->data.clr.b));
        assert(result == 3);
    } else if (substr_compare(s, start+2, start+2+4, "chsv")) {
        token->type = TEXT_TOKEN_COLOR;

        float hue, sat, val;
        int result = sscanf(&s[start+2+5], "%f %f %f", &hue, &sat, &val);
        assert(result == 3);
        struct color col = hsv_to_col(hue, sat, val);
        token->data.clr = col;
        
        printf("%f, %f, %f\n", 
               token->data.clr.r,
               token->data.clr.g,
               token->data.clr.b);
    } else {
        assert(0);
    }
    token->to_next_break = -1;
}

// parses commands as {[color name]} and {[color_reset]}
struct mem_buf *
compile_text_command(char *s)
{
    int slen = strlen(s);
    size_t text_token_size = sizeof(struct text_token);
    struct mem_buf *mb = mem_buf_size(NULL, text_token_size, 0);
    int num_tokens = 0;
    
    int i=0, cmd_start = 0;
    char in_command = 0;
    for (i=0; i<slen; i++) {
        if (!in_command) {
            if (s[i] != '{' && s[i+1] != '[') {         
                num_tokens++;
                mb = mem_buf_size(mb, text_token_size * num_tokens, 1);
                struct text_token *token = &mb->mem[(num_tokens-1) * text_token_size];
                token->type = TEXT_TOKEN_CHAR;
                token->data.chr = s[i];
                token->to_next_break = -1;

                // process breaks
                if (s[i] == ' ' || s[i] == '\n' || s[i] == '\t')
                    fill_breaks(mb, 0);
            } else {
                in_command = 1;
                cmd_start = i;
            }
        } else {
            if (s[i] == ']' && s[i+1] == '}') {
                num_tokens++;
                mb = mem_buf_size(mb, text_token_size * num_tokens, 1);
                struct text_token *token = &mb->mem[(num_tokens-1) * text_token_size];
                compile_command(s, cmd_start, i+2, token);
                
                in_command = 0;
                i += 2-1;
            }
        }
    }
        
    fill_breaks(mb, 1);

    return mb;
}

#if 0

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
               int max_lines,
               bool do_draw)
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
                if (do_draw)
                    putchar(delayed_break_col, delayed_break_row,
                            delayed_break_char,
                            draw_fg_color,
                            draw_bg_color);
            }
            
            if (do_draw)
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

int draw_string_fill(const std::string& str,
                     int line_start,
                     int min_col,
                     int max_col,
                     int max_lines,
                     bool do_draw)
{
    return draw_tcmd_fill(parse_text_command(str),
                          line_start, min_col, max_col,
                          max_lines, do_draw);
}
#endif
