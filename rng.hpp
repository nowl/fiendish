#pragma once

#include <cstdint>

class rng {
private:
    static uint32_t Q[4096];
    static uint32_t c, it;
    static uint32_t rand_cmwc();

    static double other_norm;
    static bool other_norm_available;

public:
    static void init_rand(uint32_t x);
    static void seed_good();
    static int i();
    static int i_max_inc(int max);
    static int i_min_max_inc(int min, int max);
    static float f();
    static float f_min_max(float min, float max);
    static double d();
    static double normal();
};
