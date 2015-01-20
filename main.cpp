#include <cstdio>
#include <memory>

#include "sdl.hpp"
#include "timer.hpp"
#include "rng.hpp"
#include "keyboard.hpp"
#include "playstate.hpp"
#include "globals.hpp"

#define TICKS_PER_SEC     60
#define MS_PER_TICK       (1000.0 / TICKS_PER_SEC)
#define MAX_DRAW_SKIPS    5

int main(int argc, char *argv[]) {
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
}
