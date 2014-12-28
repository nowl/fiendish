#include "globals.hpp"

struct CellTypeInfo CellTypeInfo[] = {
    {false, "BEDROCK"},
    {true, "HALL"},
    {true, "ROOM"},
};

struct DirectionInfo DirectionInfo[] = {
    "NORTH", "SOUTH", "EAST", "WEST"
};

std::unique_ptr<Game> g(new Game());
std::unique_ptr<Player> p(new Player());

int pressed(int scancode) {
    if (g->kb().is_pressed(scancode))
        return g->kb().ms_pressed(scancode);
    else
        return -1;
}

void handle_key(int scancode) {
    g->kb().handle(scancode);
}

bool unhandled_key(key_event& key) {
    return g->kb().next_unhandled_key(key);
}

void putchar(int x, int y, char c, Color fg, Color bg) {
    g->sdl().putChar(x, y, c, fg, bg);
}
