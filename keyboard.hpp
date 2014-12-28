#pragma once

#include <deque>

#include <SDL.h>

#include "sdlman.hpp"

#define REPEAT_RATE_MS     100

struct key_event {
    // the tick this key was pressed
    int tick_pressed;
    // if the key has been handled by a receiver yet
    bool handled;
    // if the key is currently pressed
    bool pressed;
    // what key this actually is
    int scancode;
};

class keyboard {
public:
    keyboard(SDLMan& sdl) : sdl(sdl) {
        reset_key_states();
    }

    // clears the keyboard state
    void reset_key_states();
   
    bool is_pressed(int scancode) {
        return key_events[scancode].pressed;
    }

    int ms_pressed(int scancode) {
        return sdl.getTicks() - key_events[scancode].tick_pressed;
    }

    // returns the next unhandled key, sets the handled flag or
    // returns false if no more unhandled
    bool next_unhandled_key(key_event &key);

    void poll_events();

    // "handles" an already pressed key by resetting it's time pressed
    void handle(int scancode) {
        key_events[scancode].tick_pressed = sdl.getTicks();
    }

private:
    key_event key_events[SDL_NUM_SCANCODES];
    SDLMan &sdl;
    std::deque<int> unhandled_events;

    void handle_incoming_event();
};
