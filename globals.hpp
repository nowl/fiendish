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

bool key_held(int scancode);
bool unhandled_key(key_event& key);

// drawing functions

void putchar(int x, int y, char c, Color fg, Color bg);

#define STRING_BLOCKED "{[colorhex c0c000]}blocked{[color_reset]} to the %s, can't enter {[colorhex 0080f0]}%s{[color_reset]}"

// positions

#define MESSAGE_BOX_ROW_MIN 35
#define MESSAGE_BOX_ROW_MAX 37
#define MESSAGE_BOX_COL_MIN 1
#define MESSAGE_BOX_COL_MAX 118
