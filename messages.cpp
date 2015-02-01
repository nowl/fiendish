#include "fiendish.hpp"

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

void draw_border(const std::string& str) {
    auto fg_color = Color::fromHSV(0, 0, .75);
    auto bg_color = ColorByName["BLACK"];

    for (int y=1; y<CELLS_VERT-1; y++) {
        putchar(0, y, SYM_VERT_BAR,
                fg_color, bg_color);
        putchar(CELLS_HORIZ-1, y, SYM_VERT_BAR,
                fg_color, bg_color);
    }
    
    putchar(0, 0, SYM_UL_BAR, fg_color, bg_color);
    putchar(0, CELLS_VERT-1, SYM_LL_BAR, fg_color, bg_color);
    putchar(CELLS_HORIZ-1, 0, SYM_UR_BAR, fg_color, bg_color);
    putchar(CELLS_HORIZ-1, CELLS_VERT-1, SYM_LR_BAR, fg_color, bg_color);

    for (int x=1; x<CELLS_HORIZ-1; x++)
        putchar(x, CELLS_VERT-1, SYM_HORZ_BAR,
                fg_color, bg_color);

    // find midpoint where to place overlay text
    int str_len = str.size();
    assert(str_len <= CELLS_HORIZ-6);
    int fill_cells = (CELLS_HORIZ - 6 - str_len) / 2;

    for (int x=1; x<fill_cells+1; x++)
        putchar(x, 0, SYM_HORZ_BAR, fg_color, bg_color);
    putchar(fill_cells+1, 0, SYM_HORZ_STOP_LEFT, fg_color, bg_color);
    draw_string_fill(str, 0, 3+fill_cells, 1000, 1000);
    putchar(fill_cells+1+str_len+3, 0, SYM_HORZ_STOP_RIGHT, fg_color, bg_color);
    for (int x=fill_cells+1+str_len+3+1; x<CELLS_HORIZ-1; x++)
        putchar(x, 0, SYM_HORZ_BAR, fg_color, bg_color);
}
