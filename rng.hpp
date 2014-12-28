#pragma once

#include <climits>
#include <cstdio>
#include <cassert>
#include <stdint.h>

class rng {
private:
    static uint32_t Q[4096];
    static uint32_t c, it;
    static uint32_t rand_cmwc();
    static void init_rand(uint32_t x);

public:
    static void seed_good();
    static int i();
    static int i_max_inc(int max);
    static int i_min_max_inc(int min, int max);
    static float f();
    static double d();
};
