#include "message_state.hpp"
#include "globals.hpp"

void MessageState::handle_events()
{
    key_event key;
    while (unhandled_key(key)) {

        if (key.scancode == SDL_SCANCODE_ESCAPE || key.scancode == SDL_SCANCODE_Q)
            g->state = parentState;
    }
}

void MessageState::update() {
}

void MessageState::render() {
    parentState->render();
}
