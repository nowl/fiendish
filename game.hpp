#pragma once

#include "timer.hpp"
#include "sdl.hpp"
#include "keyboard.hpp"

class State;

struct Game {
	Game();
	
	Timer GlobTimer;
	State *GlobState;
	SDL SDLMan;
	bool GameRunning;
	keyboard GlobKeyboard;
};
