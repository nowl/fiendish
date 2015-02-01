#include "fiendish.hpp"

void keyboard::poll_events() {
    while(sdl.pollEvent())
        handle_incoming_event();
}

void keyboard::handle_incoming_event() {
    SDL_Event& event = sdl.getCurrentEvent();
            
    if (event.type == SDL_KEYDOWN) {
        int sc = event.key.keysym.scancode;
        // if this key is already in the pressed state then ignore
        // this event
        if (key_events[sc].pressed == false) {
            key_events[sc].pressed = true;
            key_events[sc].tick_pressed = sdl.getTicks();
            key_events[sc].next_valid_hold_ms = -INITIAL_DELAY_MS;
            unhandled_events.push_back(sc);
        }
    }
    else if(event.type == SDL_KEYUP)
        key_events[event.key.keysym.scancode].pressed = false;
}

bool keyboard::next_unhandled_key(key_event &key) {
    if (unhandled_events.size() > 0) {
        key = key_events[unhandled_events.front()];
        unhandled_events.pop_front();
        return true;
    }

    return false;
}

void keyboard::reset_key_states() {
    for (int i=0; i<SDL_NUM_SCANCODES; i++) {
        key_events[i].pressed = false;
        key_events[i].scancode = i;
        key_events[i].next_valid_hold_ms = -INITIAL_DELAY_MS;
    }
    unhandled_events.clear();
}

bool keyboard::held(int scancode) {
    if (!key_events[scancode].pressed)
        return false;
    
    // hold valid
    int hold_valid = sdl.getTicks() - key_events[scancode].tick_pressed + key_events[scancode].next_valid_hold_ms;
    if (hold_valid >= 0) {
        key_events[scancode].next_valid_hold_ms -= REPEAT_RATE_MS;
        return true;
    }
    
    return false;
}
