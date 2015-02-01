#include "fiendish.h"

#define put sdl_putchar

struct color
hsv_to_col(float h, float s, float v)
{
    while (h < 0) h += 360;
    //while (h > 360) h -= 360;
    
    float c = v * s;
    float x = c * (1 - fabs(fmod(h / 60, 2) - 1));
    float m = v - c;
    
    switch( (int)(h / 60) ) {
    case 0: {struct color t = {c+m,x+m,m}; return t;}
    case 1: {struct color t = {x+m,c+m,m}; return t;}
    case 2: {struct color t = {m,c+m,x+m}; return t;}
    case 3: {struct color t = {m,x+m,c+m}; return t;}
    case 4: {struct color t = {x+m,m,c+m}; return t;}
    case 5: {struct color t = {c+m,m,x+m}; return t;}
    }

    struct color t = {0,0,0};
    return t;
}

void render_world(void)
{
    double abberation = rand_normal();
    struct color fg = hsv_to_col(35, abberation, .7);
    struct color bg = {0, 0, 0};
    put(Player.x, Player.y, '@', fg, bg);
}
