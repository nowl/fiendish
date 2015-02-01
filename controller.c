#include "fiendish.h"

void new_input(int key_up_down, int32_t keycode, uint16_t keymod)
{
    if(key_up_down == SDL_KEYDOWN && keycode == SDLK_ESCAPE && keymod == 0)
    {
        GameRunning = 0;
    }

    if(key_up_down == SDL_KEYDOWN && keycode == SDLK_l && keymod == 0)
        Player.x++;
    else if(key_up_down == SDL_KEYDOWN && keycode == SDLK_h && keymod == 0)
        Player.x--;
    else if(key_up_down == SDL_KEYDOWN && keycode == SDLK_j && keymod == 0)
        Player.y++;
    else if(key_up_down == SDL_KEYDOWN && keycode == SDLK_k && keymod == 0)
        Player.y--;
}
