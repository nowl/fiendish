#pragma once

#include <memory>

#include "game.hpp"
#include "player.hpp"
#include "dungeon.hpp"

extern std::unique_ptr<Game> g;
extern std::unique_ptr<Player> p;

extern CellTypeInfo CellTypeInfo[];
extern DirectionInfo DirectionInfo[];

const Point PlayerScreenPosition = Point{CELLS_HORIZ/2, CELLS_VERT/2};

// keyboard handling functions

// is key pressed? -1 if not, otherwise returns ms (ticks) it's been
// pressed for.
int pressed(int scancode);
void handle_key(int scancode);
bool unhandled_key(key_event& key);


// drawing functions

void putchar(int x, int y, char c, Color fg, Color bg);

extern const char *string_blocked;

// positions

#define MESSAGE_BOX_ROW_MIN 35
#define MESSAGE_BOX_ROW_MAX 37
#define MESSAGE_BOX_COL_MIN 1
#define MESSAGE_BOX_COL_MAX 118
