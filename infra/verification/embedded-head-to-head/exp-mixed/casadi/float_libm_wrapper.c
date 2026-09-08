/* CasADi emits bare libm names even when casadi_real is float. Force the same
 * Binary32 transcendental operations used by the Rumoca artifact. */
#include <math.h>
#define sqrt(x) sqrtf(x)
#define cos(x) cosf(x)
#define sin(x) sinf(x)
#include "casadi_exp_mixed.c"
