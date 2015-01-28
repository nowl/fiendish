#include "fiendish.h"

#include <lua.h>
#include <lauxlib.h>

static lua_State *L;
static int func_ref_event_handler;
static int func_ref_update;

static int lua_rand_double(lua_State *L) {
    lua_pushnumber(L, rand_double());
    return 1;
}

static int lua_getticks(lua_State *L) {
    lua_pushnumber(L, sdl_getticks());
    return 1;
}

static int lua_register_event_handler(lua_State *L) {
    luaL_checktype(L, -1, LUA_TFUNCTION);
    func_ref_event_handler = luaL_ref(L, LUA_REGISTRYINDEX);
    return 0;
}

static int lua_register_update_handler(lua_State *L) {
    luaL_checktype(L, -1, LUA_TFUNCTION);
    func_ref_update = luaL_ref(L, LUA_REGISTRYINDEX);
    return 0;
}

static int lua_putchar(lua_State *L) {
    int x = luaL_checkint(L, 1);
    int y = luaL_checkint(L, 2);
    int c = luaL_checkint(L, 3);
    lua_Number fgr = luaL_checknumber(L, 4);
    lua_Number fgg = luaL_checknumber(L, 5);
    lua_Number fgb = luaL_checknumber(L, 6);
    lua_Number bgr = luaL_optnumber(L, 7, 0);
    lua_Number bgg = luaL_optnumber(L, 8, 0);
    lua_Number bgb = luaL_optnumber(L, 9, 0);

    struct color fg = {fgr, fgg, fgb};
    struct color bg = {bgr, bgg, bgb};
    sdl_putchar(x, y, c, fg, bg);
    return 0;
}

static int lua_end_game(lua_State* L) {
    GameRunning = 0;
    return 0;
}

const luaL_Reg funcs[] = {
    {"rand_double", lua_rand_double},
    {"getticks", lua_getticks},
    {"register_event_handler", lua_register_event_handler},
    {"register_update_handler", lua_register_update_handler},
    {"putchar", lua_putchar},
    {"end_game", lua_end_game},
    {NULL, NULL}
};

#define SET_CONSTANT(name)  {              \
        lua_pushinteger( L, name );        \
        lua_setfield( L, -2, #name );      \
    }

static void register_constants(void) {
    lua_getglobal(L, "core");

    /* sdl event types we're interested in */

    SET_CONSTANT(SDL_KEYUP);
    SET_CONSTANT(SDL_KEYDOWN);

    /* sdl keycodes */

    SET_CONSTANT(SDLK_UNKNOWN);
                 
    SET_CONSTANT(SDLK_RETURN);
    SET_CONSTANT(SDLK_ESCAPE);
    SET_CONSTANT(SDLK_BACKSPACE);
    SET_CONSTANT(SDLK_TAB);
    SET_CONSTANT(SDLK_SPACE);
    SET_CONSTANT(SDLK_EXCLAIM);
    SET_CONSTANT(SDLK_QUOTEDBL);
    SET_CONSTANT(SDLK_HASH);
    SET_CONSTANT(SDLK_PERCENT);
    SET_CONSTANT(SDLK_DOLLAR);
    SET_CONSTANT(SDLK_AMPERSAND);
    SET_CONSTANT(SDLK_QUOTE);
    SET_CONSTANT(SDLK_LEFTPAREN);
    SET_CONSTANT(SDLK_RIGHTPAREN);
    SET_CONSTANT(SDLK_ASTERISK);
    SET_CONSTANT(SDLK_PLUS);
    SET_CONSTANT(SDLK_COMMA);
    SET_CONSTANT(SDLK_MINUS);
    SET_CONSTANT(SDLK_PERIOD);
    SET_CONSTANT(SDLK_SLASH);
    SET_CONSTANT(SDLK_0);
    SET_CONSTANT(SDLK_1);
    SET_CONSTANT(SDLK_2);
    SET_CONSTANT(SDLK_3);
    SET_CONSTANT(SDLK_4);
    SET_CONSTANT(SDLK_5);
    SET_CONSTANT(SDLK_6);
    SET_CONSTANT(SDLK_7);
    SET_CONSTANT(SDLK_8);
    SET_CONSTANT(SDLK_9);
    SET_CONSTANT(SDLK_COLON);
    SET_CONSTANT(SDLK_SEMICOLON);
    SET_CONSTANT(SDLK_LESS);
    SET_CONSTANT(SDLK_EQUALS);
    SET_CONSTANT(SDLK_GREATER);
    SET_CONSTANT(SDLK_QUESTION);
    SET_CONSTANT(SDLK_AT);
    
    SET_CONSTANT(SDLK_LEFTBRACKET);
    SET_CONSTANT(SDLK_BACKSLASH);
    SET_CONSTANT(SDLK_RIGHTBRACKET);
    SET_CONSTANT(SDLK_CARET);
    SET_CONSTANT(SDLK_UNDERSCORE);
    SET_CONSTANT(SDLK_BACKQUOTE);
    SET_CONSTANT(SDLK_a);
    SET_CONSTANT(SDLK_b);
    SET_CONSTANT(SDLK_c);
    SET_CONSTANT(SDLK_d);
    SET_CONSTANT(SDLK_e);
    SET_CONSTANT(SDLK_f);
    SET_CONSTANT(SDLK_g);
    SET_CONSTANT(SDLK_h);
    SET_CONSTANT(SDLK_i);
    SET_CONSTANT(SDLK_j);
    SET_CONSTANT(SDLK_k);
    SET_CONSTANT(SDLK_l);
    SET_CONSTANT(SDLK_m);
    SET_CONSTANT(SDLK_n);
    SET_CONSTANT(SDLK_o);
    SET_CONSTANT(SDLK_p);
    SET_CONSTANT(SDLK_q);
    SET_CONSTANT(SDLK_r);
    SET_CONSTANT(SDLK_s);
    SET_CONSTANT(SDLK_t);
    SET_CONSTANT(SDLK_u);
    SET_CONSTANT(SDLK_v);
    SET_CONSTANT(SDLK_w);
    SET_CONSTANT(SDLK_x);
    SET_CONSTANT(SDLK_y);
    SET_CONSTANT(SDLK_z);
    
    SET_CONSTANT(SDLK_CAPSLOCK);
    
    SET_CONSTANT(SDLK_F1);
    SET_CONSTANT(SDLK_F2);
    SET_CONSTANT(SDLK_F3);
    SET_CONSTANT(SDLK_F4);
    SET_CONSTANT(SDLK_F5);
    SET_CONSTANT(SDLK_F6);
    SET_CONSTANT(SDLK_F7);
    SET_CONSTANT(SDLK_F8);
    SET_CONSTANT(SDLK_F9);
    SET_CONSTANT(SDLK_F10);
    SET_CONSTANT(SDLK_F11);
    SET_CONSTANT(SDLK_F12);
    
    SET_CONSTANT(SDLK_PRINTSCREEN);
    SET_CONSTANT(SDLK_SCROLLLOCK);
    SET_CONSTANT(SDLK_PAUSE);
    SET_CONSTANT(SDLK_INSERT);
    SET_CONSTANT(SDLK_HOME);
    SET_CONSTANT(SDLK_PAGEUP);
    SET_CONSTANT(SDLK_DELETE);
    SET_CONSTANT(SDLK_END);
    SET_CONSTANT(SDLK_PAGEDOWN);
    SET_CONSTANT(SDLK_RIGHT);
    SET_CONSTANT(SDLK_LEFT);
    SET_CONSTANT(SDLK_DOWN);
    SET_CONSTANT(SDLK_UP);
    
    SET_CONSTANT(SDLK_NUMLOCKCLEAR);
    SET_CONSTANT(SDLK_KP_DIVIDE);
    SET_CONSTANT(SDLK_KP_MULTIPLY);
    SET_CONSTANT(SDLK_KP_MINUS);
    SET_CONSTANT(SDLK_KP_PLUS);
    SET_CONSTANT(SDLK_KP_ENTER);
    SET_CONSTANT(SDLK_KP_1);
    SET_CONSTANT(SDLK_KP_2);
    SET_CONSTANT(SDLK_KP_3);
    SET_CONSTANT(SDLK_KP_4);
    SET_CONSTANT(SDLK_KP_5);
    SET_CONSTANT(SDLK_KP_6);
    SET_CONSTANT(SDLK_KP_7);
    SET_CONSTANT(SDLK_KP_8);
    SET_CONSTANT(SDLK_KP_9);
    SET_CONSTANT(SDLK_KP_0);
    SET_CONSTANT(SDLK_KP_PERIOD);
    
    SET_CONSTANT(SDLK_APPLICATION);
    SET_CONSTANT(SDLK_POWER);
    SET_CONSTANT(SDLK_KP_EQUALS);
    SET_CONSTANT(SDLK_F13);
    SET_CONSTANT(SDLK_F14);
    SET_CONSTANT(SDLK_F15);
    SET_CONSTANT(SDLK_F16);
    SET_CONSTANT(SDLK_F17);
    SET_CONSTANT(SDLK_F18);
    SET_CONSTANT(SDLK_F19);
    SET_CONSTANT(SDLK_F20);
    SET_CONSTANT(SDLK_F21);
    SET_CONSTANT(SDLK_F22);
    SET_CONSTANT(SDLK_F23);
    SET_CONSTANT(SDLK_F24);
    SET_CONSTANT(SDLK_EXECUTE);
    SET_CONSTANT(SDLK_HELP);
    SET_CONSTANT(SDLK_MENU);
    SET_CONSTANT(SDLK_SELECT);
    SET_CONSTANT(SDLK_STOP);
    SET_CONSTANT(SDLK_AGAIN);
    SET_CONSTANT(SDLK_UNDO);
    SET_CONSTANT(SDLK_CUT);
    SET_CONSTANT(SDLK_COPY);
    SET_CONSTANT(SDLK_PASTE);
    SET_CONSTANT(SDLK_FIND);
    SET_CONSTANT(SDLK_MUTE);
    SET_CONSTANT(SDLK_VOLUMEUP);
    SET_CONSTANT(SDLK_VOLUMEDOWN);
    SET_CONSTANT(SDLK_KP_COMMA);
    SET_CONSTANT(SDLK_KP_EQUALSAS400);
    
    SET_CONSTANT(SDLK_ALTERASE);
    SET_CONSTANT(SDLK_SYSREQ);
    SET_CONSTANT(SDLK_CANCEL);
    SET_CONSTANT(SDLK_CLEAR);
    SET_CONSTANT(SDLK_PRIOR);
    SET_CONSTANT(SDLK_RETURN2);
    SET_CONSTANT(SDLK_SEPARATOR);
    SET_CONSTANT(SDLK_OUT);
    SET_CONSTANT(SDLK_OPER);
    SET_CONSTANT(SDLK_CLEARAGAIN);
    SET_CONSTANT(SDLK_CRSEL);
    SET_CONSTANT(SDLK_EXSEL);
    
    SET_CONSTANT(SDLK_KP_00);
    SET_CONSTANT(SDLK_KP_000);
    SET_CONSTANT(SDLK_THOUSANDSSEPARATOR);
    SET_CONSTANT(SDLK_DECIMALSEPARATOR);
    SET_CONSTANT(SDLK_CURRENCYUNIT);
    SET_CONSTANT(SDLK_CURRENCYSUBUNIT);
    SET_CONSTANT(SDLK_KP_LEFTPAREN);
    SET_CONSTANT(SDLK_KP_RIGHTPAREN);
    SET_CONSTANT(SDLK_KP_LEFTBRACE);
    SET_CONSTANT(SDLK_KP_RIGHTBRACE);
    SET_CONSTANT(SDLK_KP_TAB);
    SET_CONSTANT(SDLK_KP_BACKSPACE);
    SET_CONSTANT(SDLK_KP_A);
    SET_CONSTANT(SDLK_KP_B);
    SET_CONSTANT(SDLK_KP_C);
    SET_CONSTANT(SDLK_KP_D);
    SET_CONSTANT(SDLK_KP_E);
    SET_CONSTANT(SDLK_KP_F);
    SET_CONSTANT(SDLK_KP_XOR);
    SET_CONSTANT(SDLK_KP_POWER);
    SET_CONSTANT(SDLK_KP_PERCENT);
    SET_CONSTANT(SDLK_KP_LESS);
    SET_CONSTANT(SDLK_KP_GREATER);
    SET_CONSTANT(SDLK_KP_AMPERSAND);
    SET_CONSTANT(SDLK_KP_DBLAMPERSAND);
    SET_CONSTANT(SDLK_KP_VERTICALBAR);
    SET_CONSTANT(SDLK_KP_DBLVERTICALBAR);
    SET_CONSTANT(SDLK_KP_COLON);
    SET_CONSTANT(SDLK_KP_HASH);
    SET_CONSTANT(SDLK_KP_SPACE);
    SET_CONSTANT(SDLK_KP_AT);
    SET_CONSTANT(SDLK_KP_EXCLAM);
    SET_CONSTANT(SDLK_KP_MEMSTORE);
    SET_CONSTANT(SDLK_KP_MEMRECALL);
    SET_CONSTANT(SDLK_KP_MEMCLEAR);
    SET_CONSTANT(SDLK_KP_MEMADD);
    SET_CONSTANT(SDLK_KP_MEMSUBTRACT);
    SET_CONSTANT(SDLK_KP_MEMMULTIPLY);
    SET_CONSTANT(SDLK_KP_MEMDIVIDE);
    SET_CONSTANT(SDLK_KP_PLUSMINUS);
    SET_CONSTANT(SDLK_KP_CLEAR);
    SET_CONSTANT(SDLK_KP_CLEARENTRY);
    SET_CONSTANT(SDLK_KP_BINARY);
    SET_CONSTANT(SDLK_KP_OCTAL);
    SET_CONSTANT(SDLK_KP_DECIMAL);
    SET_CONSTANT(SDLK_KP_HEXADECIMAL);
    
    SET_CONSTANT(SDLK_LCTRL);
    SET_CONSTANT(SDLK_LSHIFT);
    SET_CONSTANT(SDLK_LALT);
    SET_CONSTANT(SDLK_LGUI);
    SET_CONSTANT(SDLK_RCTRL);
    SET_CONSTANT(SDLK_RSHIFT);
    SET_CONSTANT(SDLK_RALT);
    SET_CONSTANT(SDLK_RGUI);
    
    SET_CONSTANT(SDLK_MODE);
    
    SET_CONSTANT(SDLK_AUDIONEXT);
    SET_CONSTANT(SDLK_AUDIOPREV);
    SET_CONSTANT(SDLK_AUDIOSTOP);
    SET_CONSTANT(SDLK_AUDIOPLAY);
    SET_CONSTANT(SDLK_AUDIOMUTE);
    SET_CONSTANT(SDLK_MEDIASELECT);
    SET_CONSTANT(SDLK_WWW);
    SET_CONSTANT(SDLK_MAIL);
    SET_CONSTANT(SDLK_CALCULATOR);
    SET_CONSTANT(SDLK_COMPUTER);
    SET_CONSTANT(SDLK_AC_SEARCH);
    SET_CONSTANT(SDLK_AC_HOME);
    SET_CONSTANT(SDLK_AC_BACK);
    SET_CONSTANT(SDLK_AC_FORWARD);
    SET_CONSTANT(SDLK_AC_STOP);
    SET_CONSTANT(SDLK_AC_REFRESH);
    SET_CONSTANT(SDLK_AC_BOOKMARKS);
    
    SET_CONSTANT(SDLK_BRIGHTNESSDOWN);
    SET_CONSTANT(SDLK_BRIGHTNESSUP);
    SET_CONSTANT(SDLK_DISPLAYSWITCH);
    SET_CONSTANT(SDLK_KBDILLUMTOGGLE);
    SET_CONSTANT(SDLK_KBDILLUMDOWN);
    SET_CONSTANT(SDLK_KBDILLUMUP);
    SET_CONSTANT(SDLK_EJECT);
    SET_CONSTANT(SDLK_SLEEP);

    /* modifier keys */

    SET_CONSTANT(KMOD_NONE);
    SET_CONSTANT(KMOD_LSHIFT);
    SET_CONSTANT(KMOD_RSHIFT);
    SET_CONSTANT(KMOD_LCTRL);
    SET_CONSTANT(KMOD_RCTRL);
    SET_CONSTANT(KMOD_LALT);
    SET_CONSTANT(KMOD_RALT);
    SET_CONSTANT(KMOD_LGUI);
    SET_CONSTANT(KMOD_RGUI);
    SET_CONSTANT(KMOD_NUM);
    SET_CONSTANT(KMOD_CAPS);
    SET_CONSTANT(KMOD_MODE);
    SET_CONSTANT(KMOD_RESERVED);

    SET_CONSTANT(KMOD_CTRL);
    SET_CONSTANT(KMOD_SHIFT);
    SET_CONSTANT(KMOD_ALT);
    SET_CONSTANT(KMOD_GUI);
}

int lua_init(void) {
    L = luaL_newstate();
    assert(L);
    luaL_openlibs(L);

    luaL_register(L, "core", funcs);

    lua_pop(L, 1);

    register_constants();

    return 0;
}
void lua_dofile(const char *filename) {
    int exec = luaL_dofile(L, filename);
    if (exec != 0) {
        const char *err = luaL_checkstring(L, -1);
        printf("error in lua: %s\n", err);
        exit(1);
    }
}

void lua_destroy(void) {
    lua_close(L);
}

void lua_handle_event(int result, int32_t keycode, uint16_t keymod)
{
    lua_rawgeti(L, LUA_REGISTRYINDEX, func_ref_event_handler);
    lua_pushinteger(L, result);
    lua_pushinteger(L, keycode);
    lua_pushinteger(L, keymod);
    int exec = lua_pcall(L, 3, 0, 0);
    if (exec != 0) {
        const char *err = luaL_checkstring(L, -1);
        printf("error while calling 'handle_event': %s\n", err);
        exit(1);
    }
}

void lua_update(void)
{
    lua_rawgeti(L, LUA_REGISTRYINDEX, func_ref_update);
    int exec = lua_pcall(L, 0, 0, 0);
    if (exec != 0) {
        const char *err = luaL_checkstring(L, -1);
        printf("error while calling 'lua_update': %s\n", err);
        exit(1);
    }
}
