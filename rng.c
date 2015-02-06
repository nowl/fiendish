/* adapted from Marsaglia's Complementay Multipy With Carry PRNG:
 *    http://en.wikipedia.org/wiki/Multiply-with-carry
 */

#include "fiendish.h"

#define PI 3.14159265

#define PHI 0x9e3779b9

static uint32_t c = 362436;
static uint32_t Q[4096];

static double other_norm;
static int other_norm_available = 0;
 
void rand_init(uint32_t x)
{
	int i;
 
	Q[0] = x;
	Q[1] = x + PHI;
	Q[2] = x + PHI + PHI;
 
	for (i = 3; i < 4096; i++)
		Q[i] = Q[i - 3] ^ Q[i - 2] ^ PHI ^ i;
    
    /* warm up */
    for(i=0; i<4096; i++)
        rand_cmwc();
}
 
uint32_t rand_cmwc(void)
{
	static uint32_t i = 4095;
	uint64_t t;
 
	i = (i + 1) & 4095;
	t = (18705ULL * Q[i]) + c;
	c = t >> 32;
	Q[i] = 0xfffffffe - t;
 
	return Q[i];
}

void rand_seed_good(void) {
    uint32_t x;
    int i;
    FILE *f = fopen("/dev/urandom", "rb");
    for (i=0; i<4096; i++) {
        assert( fread(&x, sizeof(x), 1, f) == 1 );
        Q[i] = x;
    }
    fclose(f);
}

/*
void rand_seed_best(void) {
    uint32_t x;
    FILE *f = fopen("/dev/urandom", "rb");
    assert( fread(&x, sizeof(x), 1, f) == 1 );
    fclose(f);

    rand_init(x);
}
*/

int rand_int(void) {
    return rand_cmwc();
}

int rand_max_inc(int max) {
    return rand_cmwc() % (max + 1);
}

int rand_min_max_inc(int min, int max) {
    return rand_cmwc() % (max - min + 1) + min;
}

float rand_float(void) {
    return (float)(rand_cmwc()) / UINT_MAX;
}

float rand_float_min_max(float min, float max) {
    float range = max - min;
    float val = rand_float() * range;
    return val + min;
}

double rand_double(void) {
    return (double)(rand_cmwc()) / UINT_MAX;
}

double rand_normal(void) {
    if (other_norm_available) {
        other_norm_available = 0;
        return other_norm;
    }
    
    double u1 = rand_double();
    double u2 = rand_double();
    double R = sqrt(-2*log(u1));
    double first_norm = R * cos(2*PI*u2);
    
    other_norm_available = 1;
    other_norm = R * sin(2*PI*u2);

    return first_norm;
}