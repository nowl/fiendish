struct Game {
	Game();
	
	Timer GlobTimer;
	State *GlobState;
	SDL SDLMan;
	bool GameRunning;
	keyboard GlobKeyboard;
};
