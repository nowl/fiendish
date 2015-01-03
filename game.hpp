#pragma once

#include "timer.hpp"
#include "sdl.hpp"
#include "keyboard.hpp"
#include "state.hpp"

class Game {
public:
    Game() : state(NULL), running_(false), keyboard_(man_) {}

    SDL& sdl() { return man_; }
    keyboard& kb() { return keyboard_; }
    Timer& timer() { return timer_; }
    bool is_running() const { return running_; }
    void set_running(bool state) { running_ = state; }

    State* state;

private:
    SDL man_;
    Timer timer_;
    bool running_;
    keyboard keyboard_;
};
