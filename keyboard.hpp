#pragma once

#include "sdl.hpp"

#include <deque>

#define INITIAL_DELAY_MS     250
#define REPEAT_RATE_MS       60

struct key_event {
    // the tick this key was pressed
    int tick_pressed;
    // if the key is currently pressed
    bool pressed;
    // what key this actually is
    int scancode;
    // used to keep track of a held key, determines next valid time
    // for a repeated event
    int next_valid_hold_ms;
};

class keyboard {
public:
    keyboard(SDL& sdl) : sdl(sdl) {
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
    SDL &sdl;
    std::deque<int> unhandled_events;

    void handle_incoming_event();
};
