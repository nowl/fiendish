#pragma once

#include <deque>

#include <SDL.h>

#include "sdlman.hpp"

#define INITIAL_DELAY_MS     250
#define REPEAT_RATE_MS       60

struct key_event {
    // the tick this key was pressed
    int tick_pressed;
    // if the key has been handled by a receiver yet
    bool handled;
    // if the key is currently pressed
    bool pressed;
    // what key this actually is
    int scancode;
    // next valid hold ms. when this goes positive, a hold is valid
    int next_valid_hold_ms;
};

class keyboard {
public:
    keyboard(SDLMan& sdl) : sdl(sdl) {
        reset_key_states();
    }

    // clears the keyboard state
    void reset_key_states();

    // returns true if a key has been "held" and is valid for handling
    bool held(int scancode);

    // returns the next unhandled key, sets the handled flag or
    // returns false if no more unhandled
    bool next_unhandled_key(key_event &key);

    void poll_events();

private:
    key_event key_events[SDL_NUM_SCANCODES];
    SDLMan &sdl;
    std::deque<int> unhandled_events;

    void handle_incoming_event();
};
