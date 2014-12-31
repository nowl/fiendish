#include <deque>

#include "messages.hpp"
#include "globals.hpp"

static std::deque<TextCommand> messages;

void add_message(const TextCommand& tcmd)
{
    messages.push_back(tcmd);
}

void draw_messages()
{
    if (messages.empty())
        return;

    auto iter = messages.end() - 1;
    
    // count how many messages to display
    int max_lines = MESSAGE_BOX_ROW_MAX - MESSAGE_BOX_ROW_MIN + 1;
    int total_rows = 0;
    int count = 0;
    for(;;) {
        TextCommand& message = *iter;
                
        int num_rows = draw_tcmd_fill(message,
                                      MESSAGE_BOX_ROW_MIN,
                                      MESSAGE_BOX_COL_MIN,
                                      MESSAGE_BOX_COL_MAX,
                                      100,
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
    int start_row = MESSAGE_BOX_ROW_MIN;
    for(int i=count; i>0; i--)
    {
        TextCommand& message = messages[messages.size()-i];
                
        int num_rows = draw_tcmd_fill(message,
                                      start_row,
                                      MESSAGE_BOX_COL_MIN,
                                      MESSAGE_BOX_COL_MAX,
                                      100);
        start_row += num_rows;
    }
}

