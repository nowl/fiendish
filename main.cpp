#include "fiendish.hpp"

#define TICKS_PER_SEC     60
#define MS_PER_TICK       (1000.0 / TICKS_PER_SEC)
#define MAX_DRAW_SKIPS    5

int main(int argc, char *argv[]) {
    init_colors();
    
    rng::seed_good();

    int frames = 0;

    for(int x=0; x<120; x++)
        for(int y=0; y<38; y++)
            g->SDLMan.putChar(x, y, 255 - y - x, Color(x/240.0 + .5, 0, 0), ColorByName["BLACK"]);
    for(int x=0; x<120; x++)
        g->SDLMan.putChar(x, 4, '@', ColorByName["WHITE"], ColorByName["BLACK"]);

    g->GlobTimer.Reset();
    
    auto play_state = std::unique_ptr<State>(new PlayState());
    g->GlobState = play_state.get();

    double next_update = g->SDLMan.getTicks() + MS_PER_TICK;
    g->GameRunning = true;
    while(g->GameRunning) {
        g->SDLMan.draw();
        frames++;

        g->GlobKeyboard.poll_events();
        g->GlobState->handle_events();

        int update_loops = 0;
        int cur_tick = g->SDLMan.getTicks();
        while (cur_tick >= next_update && update_loops < MAX_DRAW_SKIPS) {
            g->GlobState->update();
            next_update += MS_PER_TICK;
            update_loops++;
        }

        g->GlobState->render();
    }
    float elapsedTime = g->GlobTimer.Elapsed();

    printf("elapsed time = %f seconds\n", elapsedTime);
    printf("fps = %f\n", frames / elapsedTime);
}
