#include "api-common.h"

#define SCALARS (2 * WIDTH)
#if WIDTH == 1
static const double seed[SCALARS] = {3, 5};
static const double expected[SCALARS] = {2, 6};
static const double input_expected[SCALARS] = {5, 0};
#if FMI_VERSION == 3
static const double adj_seed[SCALARS] = {7, 11};
static const double adj_expected[SCALARS] = {15, 7};
#endif
#else
static const double seed[SCALARS] = {3, 5, 7, 11};
static const double expected[SCALARS] = {4, 1, 39, -2};
static const double input_expected[SCALARS] = {7, 11, 33, 0};
#if FMI_VERSION == 3
static const double adj_seed[SCALARS] = {2, 3, 5, 7};
static const double adj_expected[SCALARS] = {15, -13, 2, 18};
#endif
#endif

static void successful_requests(int phase) {
    Instance current = instance(phase);
    ModelInstance before = *(ModelInstance*)current;
    double output[SCALARS];
    int full = phase < 2 || (FMI_VERSION == 3 && phase == 2);
    const Reference* known = full ? knowns : knowns + INPUT_REFS;
    size_t nk = full ? REFS : INPUT_REFS;
    const double* input = full ? seed : seed + WIDTH;
    size_t ni = full ? SCALARS : WIDTH;
    CHECK(forward(current, unknowns, REFS, known, nk, input, ni, output, SCALARS) == OK);
    CHECK(close_values(output, full ? expected : input_expected, SCALARS));
#if FMI_VERSION == 3
    double transpose[SCALARS];
    CHECK(fmi3GetAdjointDerivative(current, unknowns, REFS, known, nk, adj_seed,
        SCALARS, transpose, ni) == OK);
    CHECK(close_values(transpose, full ? adj_expected : adj_expected + WIDTH, ni));
    double left = 0, right = 0;
    for (size_t k = 0; k < SCALARS; ++k) left += adj_seed[k] * output[k];
    for (size_t k = 0; k < ni; ++k) right += input[k] * transpose[k];
    CHECK(fabs(left - right) < 1e-10);
#endif
    Reference duplicates[2 * INPUT_REFS];
    double repeated[SCALARS], doubled[SCALARS];
    for (size_t k = 0; k < 2 * INPUT_REFS; ++k) duplicates[k] = knowns[INPUT_REFS + k % INPUT_REFS];
    for (size_t k = 0; k < SCALARS; ++k) {
        repeated[k] = seed[WIDTH + k % WIDTH];
        doubled[k] = 2 * input_expected[k];
    }
    CHECK(forward(current, unknowns, REFS, duplicates, 2 * INPUT_REFS,
        repeated, SCALARS, output, SCALARS) == OK);
    CHECK(close_values(output, doubled, SCALARS));
    fail_after = 0;
    CHECK(forward(current, NULL, 0, NULL, 0, NULL, 0, NULL, 0) == OK);
    fail_after = -1;
    CHECK(primal_unchanged(current, &before));
    FREE_INSTANCE(current);
}

static void rejected_request(int which) {
    Instance current = instance(1);
    ModelInstance before = *(ModelInstance*)current;
    double output[SCALARS], bad_seed[SCALARS];
    Reference invalid[REFS];
    for (size_t k = 0; k < SCALARS; ++k) { output[k] = 1234567 + (double)k; bad_seed[k] = seed[k]; }
    memcpy(invalid, unknowns, sizeof invalid);
    Status status;
    switch (which) {
    case 0:
        invalid[REFS - 1] = UINT32_MAX;
        status = forward(current, invalid, REFS, knowns, REFS, seed, SCALARS, output, SCALARS);
        break;
    case 1:
        memcpy(invalid, knowns, sizeof invalid); invalid[REFS - 1] = UINT32_MAX;
        status = forward(current, unknowns, REFS, invalid, REFS, seed, SCALARS, output, SCALARS);
        break;
    case 2:
        invalid[REFS - 1] = NONREAL_VR;
        status = forward(current, invalid, REFS, knowns, REFS, seed, SCALARS, output, SCALARS);
        break;
    case 3:
        bad_seed[SCALARS - 1] = NAN;
        status = forward(current, unknowns, REFS, knowns, REFS, bad_seed, SCALARS, output, SCALARS);
        break;
    case 4:
#if FMI_VERSION == 3
        status = forward(current, unknowns, REFS, knowns, REFS, seed, SCALARS - 1, output, SCALARS);
#else
        status = forward(current, NULL, REFS, knowns, REFS, seed, SCALARS, output, SCALARS);
#endif
        break;
    case 5:
        status = forward(current, unknowns, REFS, knowns, REFS, NULL, SCALARS, output, SCALARS);
        break;
    default:
        fail_after = which - 6;
        status = forward(current, unknowns, REFS, knowns, REFS, seed, SCALARS, output, SCALARS);
        fail_after = -1;
        break;
    }
    CHECK(status == ERROR);
    for (size_t k = 0; k < SCALARS; ++k) CHECK(output[k] == 1234567 + (double)k);
    CHECK(primal_unchanged(current, &before));
    CHECK(((ModelInstance*)current)->state == ERROR_STATE);
    CHECK(RESET(current) == OK);
    initialize(current, 0, 1);
    CHECK(forward(current, unknowns, REFS, knowns, REFS, seed, SCALARS, output, SCALARS) == OK);
    CHECK(close_values(output, expected, SCALARS));
    FREE_INSTANCE(current);
}

int main(void) {
    for (int phase = 0; phase < 4; ++phase) successful_requests(phase);
    for (int which = 0; which < 10; ++which) rejected_request(which);
    puts("directional API: all mode, shape, transpose, duplicate, refusal and Reset controls passed");
    return 0;
}
