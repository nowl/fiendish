/* adapted from Marsaglia's Complementay Multipy With Carry PRNG:
 *    http://en.wikipedia.org/wiki/Multiply-with-carry
 */

#include "fiendish.hpp"

#define PI 3.14159265

#define PHI 0x9e3779b9

uint32_t rng::c = 362436;
uint32_t rng::Q[4096];
uint32_t rng::it = 4095;

double rng::other_norm;
bool rng::other_norm_available = false;
 
void rng::init_rand(uint32_t x)
{
	int i;
 
	Q[0] = x;
	Q[1] = x + PHI;
	Q[2] = x + PHI + PHI;
 
	for (i = 3; i < 4096; i++)
		Q[i] = Q[i - 3] ^ Q[i - 2] ^ PHI ^ i;
}
 
uint32_t rng::rand_cmwc()
{
	uint64_t t;
	it = (it + 1) & 4095;
	t = (18705ULL * Q[it]) + c;
	c = t >> 32;
	Q[it] = 0xfffffffe - t;
 
	return Q[it];
}

void rng::seed_good() {
    uint32_t x;
    FILE *f = fopen("/dev/urandom", "rb");
    assert( fread(&x, sizeof(x), 1, f) == 1 );
    fclose(f);

    init_rand(x);
}

int rng::i() {
    return rand_cmwc();
}

int rng::i_max_inc(int max) {
    return rand_cmwc() % (max + 1);
}

int rng::i_min_max_inc(int min, int max) {
    return rand_cmwc() % (max - min + 1) + min;
}

float rng::f() {
    return static_cast<float>(rand_cmwc()) / UINT_MAX;
}

float rng::f_min_max(float min, float max) {
    float range = max - min;
    float val = f() * range;
    return val + min;
}

double rng::d() {
    return static_cast<double>(rand_cmwc()) / UINT_MAX;
}

double rng::normal() {
    if (other_norm_available) {
        other_norm_available = false;
        return other_norm;
    }
    
    double u1 = d();
    double u2 = d();
    double R = std::sqrt(-2*std::log(u1));
    double first_norm = R * cos(2*PI*u2);
    
    other_norm_available = true;
    other_norm = R * sin(2*PI*u2);

    return first_norm;
}
