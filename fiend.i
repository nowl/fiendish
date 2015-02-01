%module fiend
%include "typemaps.i"

%{
#include "fiendish.h"
%}

%include "fiendish.h"
%include "SDL_keycode.h"

int sdl_pollevent(int *OUTPUT, unsigned short *OUTPUT);

