#include "fiendish.h"

#define TICKS_PER_SEC     15
#define MS_PER_TICK       (1000.0 / TICKS_PER_SEC)
#define MAX_DRAW_SKIPS    5

int main(int argc, char *argv[]) {
    //rand_seed_good();
    rand_init(123);
    sdl_init();
    lua_init();

    lua_dofile("main.lua");

    int frames = 0;
    int ticks = 0;

    int32_t keycode;
    uint16_t keymod;

    int next_tick = sdl_getticks();
    int game_start_tick = next_tick;
    
    while(GameRunning) {
        int result;
        while( (result = sdl_pollevent(&keycode, &keymod)) != 0) {
            /* reload lua */
            if(result == SDL_KEYDOWN && keycode == SDLK_F12 && keymod == 0) {
                lua_dofile("main.lua");
            }
            
            lua_handle_event(result, keycode, keymod);
        }
        
        int current_time = sdl_getticks();
        while (current_time >= next_tick) {
            lua_update();
            ticks++;
            next_tick += MS_PER_TICK;
        }

        sdl_draw();
        frames++;        
    }

	int game_end_tick = sdl_getticks();
	
    lua_destroy();
    sdl_destroy();
    
    printf("frames = %d\n", frames);
    printf("ticks = %d\n", ticks);
    printf("total average frames/sec = %.3f\n",
		   (double)frames/(game_end_tick - game_start_tick)*1000);
    printf("total average ticks/sec = %.3f\n",
		   (double)ticks/(game_end_tick - game_start_tick)*1000);
}
