#pragma once

#include <memory>
#include <SDL.h>

#include "color.hpp"

#define PROGRAM_NAME "Mordell Engine"

// this is the number of cells across and down the screen
#define CELLS_HORIZ    120
#define CELLS_VERT     38

// this is the logical size of the screen
#define WIDTH   (CELLS_HORIZ*9)
#define HEIGHT  (CELLS_VERT*16)

#define FALSE 0
#define TRUE  1

// this is the actual size of the screen in pixels
#define SCREENWIDTH   1920
#define SCREENHEIGHT  (SCREENWIDTH * 9 / 16)

// this is the size of the codepage437 texture
#define TEXTURE_WIDTH  304
#define TEXTURE_HEIGHT 144

class SDLMan {
public:
	SDLMan();
	virtual ~SDLMan();

	void draw();
    void putChar(int x, int y, char c, const Color& fg, const Color& bg);
    bool pollEvent();
    int getTicks();
    SDL_Event& getCurrentEvent();

private:
    // TODO move all of this back into the class instead of having a pimpl
    struct impl;
    std::unique_ptr<impl> p;

    void setupwindow();
    void setupGL();
};
