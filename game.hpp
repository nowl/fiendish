#pragma once

#include "timer.hpp"
#include "sdlman.hpp"
#include "keyboard.hpp"

class Game {
public:
    Game() : running_(false), keyboard_(man_) {}

    SDLMan& sdl() { return man_; }
    keyboard& kb() { return keyboard_; }
    Timer& timer() { return timer_; }
    bool is_running() const { return running_; }
    void set_running(bool state) { running_ = state; }

private:
    SDLMan man_;
    Timer timer_;
    bool running_;
    keyboard keyboard_;
};
