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

// data structures

struct color {
    float r, g, b;
};

struct player {
    int x, y;
};

struct mem_buf {
    void *mem;                  /* malloced buffer */
    size_t usage;               /* in bytes */
    size_t cap;                 /* in bytes */
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

// drawing
void render_world(void);
struct color hsv_to_col(float h, float s, float v);

// controller
void new_input(int key_up_down, int32_t keycode, uint16_t keymod);
void update(void);

// text

// utils

/* Double memory buffer until it will fit "size" bytes. Also returns
 * the same buffer. NULL can be passed as the buffer and it will be
 * instatiated. */
struct mem_buf *mem_buf_size(struct mem_buf *buf, size_t size, char update_usage);
void mem_buf_free(struct mem_buf *buf);

/* Compare two strings with the first string "s" being a substring
 * defined by start and end. Assumes "t" is of length end-start.  */
char substr_compare(char *s, int start, int end, char *t);

// globals

void game_init(void);

extern int GameRunning;
extern struct player Player;

#endif  /* __FIENDISH_H__ */
