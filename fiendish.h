#pragma once

#ifndef __FIENDISH_H__
#define __FIENDISH_H__

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include <memory.h>
#include <assert.h>
#include <math.h>

#include <SDL.h>

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

// globals

extern int GameRunning;

// structs

struct color {
    float r, g, b;
};

// sdl

void sdl_init(void);
void sdl_destroy(void);
void sdl_draw(void);
void sdl_putchar(int x, int y, unsigned char c, struct color fg, struct color bg);
int sdl_pollevent(int32_t *keycode, uint16_t *keymod);
int sdl_getticks(void);

// rng

void rand_init(uint32_t x);
uint32_t rand_cmwc(void);
void rand_seed_good(void);
int rand_int(void);
int rand_max_inc(int max);
int rand_min_max_inc(int min, int max);
float rand_float(void);
float rand_float_min_max(float min, float max);
double rand_double(void);
double rand_normal(void);

// lua

void lua_dofile(const char *filename);
int lua_init(void);
void lua_destroy(void);
void lua_handle_event(int result, int32_t keycode, uint16_t keymod);
void lua_update(void);

#endif  /* __FIENDISH_H__ */
