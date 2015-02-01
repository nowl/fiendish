#include "fiendish.hpp"

void MessageState::handle_events()
{
    key_event key;
    while (unhandled_key(key)) {

        if (key.scancode == SDL_SCANCODE_ESCAPE || key.scancode == SDL_SCANCODE_Q)
            g->GlobState = parentState;
    }
}

void MessageState::update() {
}

void MessageState::render() {
    for(int x=0; x<CELLS_HORIZ; x++)
        for(int y=0; y<CELLS_VERT; y++)
            putchar(x, y, ' ', ColorByName["BLACK"], ColorByName["BLACK"]);
    
    draw_border("Messages - Scroll (PgUp/PgDn/space) - Quit (q)");;

    draw_messages(1, 36, 1, 118);
}
