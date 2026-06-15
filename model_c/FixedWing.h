#pragma once

#include <stddef.h>

typedef double real_t;

enum {
    FIXEDWING_Y_LEN = 248,
    FIXEDWING_P_LEN = 163,
    FIXEDWING_STATE_LEN = 18,
    FIXEDWING_DERIVATIVE_LEN = 18,
    FIXEDWING_EVENT_INDICATOR_LEN = 27
};

typedef struct {
    real_t time;
    real_t y[248];
    real_t p[163];
    real_t event_indicators[27];
    real_t event_indicators_prev[27];
} FixedWing_t;

void startup(FixedWing_t *m);
void dostep(FixedWing_t *m, real_t dt);
void recalibrate(FixedWing_t *m);