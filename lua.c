#include "fiendish.h"

#include <lua.h>
#include <lauxlib.h>

int cmwc(lua_State *L) {
    lua_pushnumber(L, 4);
    return 1;
}

int init(lua_State *L) {
    lua_pushnumber(L, 5);
    return 1;
}

const luaL_Reg funcs[] = {
    {"cmwc", cmwc},
    {"init", init},
    {NULL, NULL}
};

int lua_init() {
    lua_State *L = luaL_newstate();
    assert(L);
    luaL_openlibs(L);

    //lua_newtable(L);
    //luaL_newlib(L, funcs);

    //lua_pushstring(L, "cmwc");
    //lua_pushcfunction(L, cmwc);
    //lua_settable(L, -3);

    //luaL_setfuncs(L, funcs, 0);

    //lua_setglobal(L, "happy");

    luaL_register(L, "happy", funcs);


    //lua_newtable(
    //lua_register(L, "cmwc", cmwc);

    int exec = luaL_dofile(L, "test.lua");
    //if (exec != LUA_OK) {
    if (exec != 0) {
        const char *err = luaL_checkstring(L, -1);
        printf("error in lua: %s\n", err);
               
    }

    lua_close(L);

    return 0;
}
