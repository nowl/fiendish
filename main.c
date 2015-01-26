#include "fiendish.h"

#define TICKS_PER_SEC     60
#define MS_PER_TICK       (1000.0 / TICKS_PER_SEC)
#define MAX_DRAW_SKIPS    5

int main(int argc, char *argv[]) {
    //rand_seed_good();
    rand_init(123);
    sdl_init();
    lua_init();

    lua_dofile("test.lua");

    int frames_drawn = 0;

    int32_t keycode;
    uint16_t keymod;
    
    int running = 1;
    while(running) {
        struct color bg = {0, 0, 0};
        
        int result;
        while( (result = sdl_pollevent(&keycode, &keymod)) != 0) {
            lua_handle_event(result, keycode, keymod);

            if (keycode == SDLK_ESCAPE)
                running = 0;
            if (keycode == SDLK_d) {
                int i;
                for(i=0; i<100; i++) {
                    struct color fg = {rand_float(),
                                       rand_float(),
                                       rand_float()};
                    int x = (rand_normal() * 5) + 60;
                    int y = (rand_normal() * 2.5) + 20;
                    sdl_putchar(x,//rand_max_inc(CELLS_HORIZ-1),
                                y,//rand_max_inc(CELLS_VERT-1),
                                rand_max_inc(255),//'@',
                                fg,
                                bg);
                }
            }
        }
        
        frames_drawn++;
        
        sdl_draw();
    }

    lua_destroy();
    sdl_destroy();
    
    printf("frames = %d\n", frames_drawn);

    /*
    init_colors();
    
    rng::seed_good();

    int frames = 0;

    for(int x=0; x<120; x++)
        for(int y=0; y<38; y++)
            SDLMan.putChar(x, y, 255 - y - x, Color(x/240.0 + .5, 0, 0), ColorByName["BLACK"]);
    for(int x=0; x<120; x++)
        SDLMan.putChar(x, 4, '@', ColorByName["WHITE"], ColorByName["BLACK"]);

    GlobTimer.Reset();
    
    auto play_state = std::unique_ptr<State>(new PlayState());
    GlobState = play_state.get();

    double next_update = SDLMan.getTicks() + MS_PER_TICK;
    GameRunning = true;
    while(GameRunning) {
        SDLMan.draw();
        frames++;

        GlobKeyboard.poll_events();
        GlobState->handle_events();

        int update_loops = 0;
        int cur_tick = SDLMan.getTicks();
        while (cur_tick >= next_update && update_loops < MAX_DRAW_SKIPS) {
            GlobState->update();
            next_update += MS_PER_TICK;
            update_loops++;
        }

        GlobState->render();
    }
    float elapsedTime = GlobTimer.Elapsed();

    printf("elapsed time = %f seconds\n", elapsedTime);
    printf("fps = %f\n", frames / elapsedTime);
    */
}
