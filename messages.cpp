#include <deque>

#include "messages.hpp"
#include "globals.hpp"

static std::deque<TextCommand> messages;

void add_message(const TextCommand& tcmd)
{
    messages.push_back(tcmd);
}

void add_message(const std::string& str)
{
    add_message(parse_text_command(str.c_str()));
}

void draw_messages(int row_min, int row_max, int col_min, int col_max)
{
    if (messages.empty())
        return;

    auto iter = messages.end() - 1;
    
    // count how many messages to display
    int max_lines = row_max - row_min + 1;
    int total_rows = 0;
    int count = 0;
    for(;;) {
        TextCommand& message = *iter;
                
        int num_rows = draw_tcmd_fill(message,
                                      row_min,
                                      col_min,
                                      col_max,
                                      1000,
                                      false);
        total_rows += num_rows;
        if (total_rows > max_lines)
            break;

        ++count;

        if(iter == messages.begin())
            break;

        --iter;
    }

    // now do the actual draw
    int start_row = row_min;
    for(int i=count; i>0; i--)
    {
        TextCommand& message = messages[messages.size()-i];
                
        int num_rows = draw_tcmd_fill(message,
                                      start_row,
                                      col_min,
                                      col_max,
                                      100);
        start_row += num_rows;
    }
}

