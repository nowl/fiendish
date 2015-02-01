#pragma once

extern std::unique_ptr<Game> g;
extern std::unique_ptr<Player> p;

extern CellTypeCatalog CellTypeCatalog[];
extern DirectionInfo DirectionInfo[];

const Point PlayerScreenPosition = Point{CELLS_HORIZ/2, CELLS_VERT/2};

// keyboard handling functions

// test if a key is held down (for use in update)
bool key_held(int scancode);

// test for an initial keypress (for use in handle_events)
bool unhandled_key(key_event& key);

// drawing functions

void putchar(int x, int y, char c, Color fg, Color bg);

#define STRING_BLOCKED "{[colorhex c0c000]}blocked{[color_reset]} to the %s, can't enter {[colorhex 0080f0]}%s{[color_reset]}"

// positions

#define MESSAGE_BOX_ROW_MIN 35
#define MESSAGE_BOX_ROW_MAX 37
#define MESSAGE_BOX_COL_MIN 1
#define MESSAGE_BOX_COL_MAX 118

// symbols

#define SYM_VERT_BAR 179
#define SYM_HORZ_BAR 196
#define SYM_LL_BAR 192
#define SYM_UR_BAR 191
#define SYM_UL_BAR 218
#define SYM_LR_BAR 217
#define SYM_HORZ_STOP_RIGHT 195
#define SYM_HORZ_STOP_LEFT 180
