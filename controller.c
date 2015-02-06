#include "fiendish.h"

#define INITIAL_DELAY      250
#define REPEAT_DELAY        40

#define MAX_KEYPRESSES      10

struct keypress {
    char pressed;
    int32_t key;
    uint16_t keymod;
    char in_repeat;
    int last_handled;
    char handled;
};

static struct keypress keyvec[MAX_KEYPRESSES];

static void handle_keypress(int32_t keycode, uint16_t keymod) {
    if(keycode == SDLK_ESCAPE && keymod == 0)
    {
        GameRunning = 0;
    }

    if(keycode == SDLK_l && keymod == 0)
        Player.x++;
    else if(keycode == SDLK_h && keymod == 0)
        Player.x--;
    else if(keycode == SDLK_j && keymod == 0)
        Player.y++;
    else if(keycode == SDLK_k && keymod == 0)
        Player.y--;
}

void new_input(int key_up_down, int32_t keycode, uint16_t keymod)
{
    int i;
    if(key_up_down == SDL_KEYDOWN) {
        char already_pressed = 0;
        /* first check if already pressed */
        for(i=0; i<MAX_KEYPRESSES; i++)
            if (keyvec[i].key == keycode && keyvec[i].pressed == 1) {
                already_pressed = 1;
                break;
            }
        if (!already_pressed) {
            /* find open keyvec */
            for(i=0; i<MAX_KEYPRESSES; i++)
                if (!keyvec[i].pressed)
                    break;
            assert(i<MAX_KEYPRESSES);
            
            keyvec[i].pressed = 1;
            keyvec[i].key = keycode;
            keyvec[i].keymod = keymod;
            keyvec[i].in_repeat = 0;
            keyvec[i].handled = 0;
            keyvec[i].last_handled = sdl_getticks();
        }
    } else if (key_up_down == SDL_KEYUP) {
        /* find keypress */
        for(i=0; i<MAX_KEYPRESSES; i++)
            if (keyvec[i].key == keycode)
                break;
        assert(i<MAX_KEYPRESSES);
        keyvec[i].pressed = 0;
    }

    /* newly pressed */
    for(i=0; i<MAX_KEYPRESSES; i++) {
        if(keyvec[i].pressed == 1 && keyvec[i].handled == 0) {
            handle_keypress(keyvec[i].key, keyvec[i].keymod);
            keyvec[i].handled = 1;
        }
    }
}

static void update_key_repeat(void) {
    int i;
    /* repeated presses */
    for(i=0; i<MAX_KEYPRESSES; i++) {
        if(keyvec[i].pressed == 1 && keyvec[i].handled == 1) {
            int ctime = sdl_getticks();
            if(keyvec[i].in_repeat) {
                if (ctime - keyvec[i].last_handled > REPEAT_DELAY) {
                    handle_keypress(keyvec[i].key, keyvec[i].keymod);
                    keyvec[i].last_handled = ctime;
                }
            } else {
                if (ctime - keyvec[i].last_handled > INITIAL_DELAY) {
                    keyvec[i].in_repeat = 1;
                    handle_keypress(keyvec[i].key, keyvec[i].keymod);
                    keyvec[i].last_handled = ctime;
                }
            }
        }
    }
}

void update(void) {
    update_key_repeat();
}
