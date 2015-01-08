#include "globals.hpp"

struct CellTypeCatalog CellTypeCatalog[] = {
    {false, "BEDROCK", {240, 240, 0.5, 0.5, 1, 1}},
    {true, "HALL", {0, 60, 0, 0.25, 0.5, 0.75}},
    {true, "ROOM", {0, 60, 0, 0.25, 0.5, 0.75}},
};

struct DirectionInfo DirectionInfo[] = {
    "NORTH", "SOUTH", "EAST", "WEST"
};

std::unique_ptr<Game> g(new Game());
std::unique_ptr<Player> p(new Player());

bool key_held(int scancode) {
    return g->kb().held(scancode);
}

bool unhandled_key(key_event& key) {
    return g->kb().next_unhandled_key(key);
}

void putchar(int x, int y, char c, Color fg, Color bg) {
    g->sdl().putChar(x, y, c, fg, bg);
}
