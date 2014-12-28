#include "text.hpp"
#include "color.hpp"

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
