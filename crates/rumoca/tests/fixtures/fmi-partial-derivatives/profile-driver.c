#include "api-common.h"

static void require_direction(Instance current, const Reference* unknown, size_t nu,
                              const Reference* known, size_t nk, const double* seed,
                              const double* expected) {
    ModelInstance before = *(ModelInstance*)current;
    double output[3] = {12345, 12345, 12345};
    CHECK(forward(current, unknown, nu, known, nk, seed, nk, output, nu) == OK);
    CHECK(close_values(output, expected, nu));
    CHECK(primal_unchanged(current, &before));
}

#if PROFILE == 1
static void parameter_profile(int phase) {
    Instance current = instance(phase);
    const Reference initial_known[] = {GAIN_VR, INPUT_VR, STATE_VR};
    const Reference initial_unknown[] = {OFFSET_VR, DERIVATIVE_VR, OUTPUT_VR};
    const Reference later_known[] = {INPUT_VR, STATE_VR};
    const Reference later_unknown[] = {DERIVATIVE_VR, OUTPUT_VR};
    const double initial_seed[] = {5, 7, 11};
    const double initial_expected[] = {22, 2, 95};
    const double later_seed[] = {7, 11};
    const double later_expected[] = {-15, 73};
    const double input_expected[] = {7, 7};
    const int initializing = phase == 0;
    const int states_allowed = phase == 1 || (FMI_VERSION == 3 && phase == 2);
    const Reference* unknown = initializing ? initial_unknown : later_unknown;
    const Reference* known = initializing ? initial_known : later_known;
    const size_t nu = initializing ? 3 : 2;
    const size_t nk = initializing ? 3 : (states_allowed ? 2 : 1);
    require_direction(current, unknown, nu, known, nk,
        initializing ? initial_seed : later_seed,
        initializing ? initial_expected : (states_allowed ? later_expected : input_expected));
    if (!initializing) {
        const Reference offset = OFFSET_VR;
        double value = NAN;
#if FMI_VERSION == 2
        CHECK(fmi2GetReal(current, &offset, 1, &value) == OK);
#else
        CHECK(fmi3GetFloat64(current, &offset, 1, &value, 1) == OK);
#endif
        CHECK(value == 6);
    }
#if FMI_VERSION == 3
    ModelInstance before = *(ModelInstance*)current;
    const double initial_adjoint_seed[] = {2, 3, 5};
    const double initial_adjoint[] = {27, 18, 24};
    const double later_adjoint_seed[] = {2, 3};
    const double later_adjoint[] = {5, 14};
    double result[3] = {12345, 12345, 12345};
    CHECK(fmi3GetAdjointDerivative(current, unknown, nu, known, nk,
        initializing ? initial_adjoint_seed : later_adjoint_seed, nu, result, nk) == OK);
    CHECK(close_values(result, initializing ? initial_adjoint : later_adjoint, nk));
    CHECK(primal_unchanged(current, &before));
#endif
    FREE_INSTANCE(current);
}
#else
static void set_time(Instance current) {
#if FMI_VERSION == 2
    CHECK(fmi2SetTime(current, 2) == OK);
#else
    CHECK(fmi3SetTime(current, 2) == OK);
#endif
}

static void time_profile(Instance current) {
    const Reference known[] = {0, STATE_VR, INPUT_VR};
    const Reference unknown[] = {DERIVATIVE_VR, OUTPUT_VR};
    const double seed[] = {5, 7, 11};
    const double expected[] = {30, 29};
    set_time(current);
    require_direction(current, unknown, 2, known, 3, seed, expected);
#if FMI_VERSION == 3
    ModelInstance before = *(ModelInstance*)current;
    const double adjoint_seed[] = {2, 3};
    const double expected_adjoint[] = {11, 10, 2};
    double result[3] = {12345, 12345, 12345};
    CHECK(fmi3GetAdjointDerivative(current, unknown, 2, known, 3,
        adjoint_seed, 2, result, 3) == OK);
    CHECK(close_values(result, expected_adjoint, 3));
    CHECK(primal_unchanged(current, &before));
#endif
}

static void refuse_time(int phase) {
    Instance current = instance(phase);
    ModelInstance before = *(ModelInstance*)current;
    const Reference time = 0, unknown = DERIVATIVE_VR;
    const double seed = 5;
    double result = 12345;
    CHECK(forward(current, &unknown, 1, &time, 1, &seed, 1, &result, 1) == ERROR);
    CHECK(result == 12345 && primal_unchanged(current, &before));
    CHECK(((ModelInstance*)current)->state == ERROR_STATE);
    CHECK(RESET(current) == OK);
    initialize(current, phase == 3, phase);
    // A refused time seed remains refused after Reset in the same mode.
    CHECK(forward(current, &unknown, 1, &time, 1, &seed, 1, &result, 1) == ERROR);
    CHECK(result == 12345);
    FREE_INSTANCE(current);
}
#endif

int main(void) {
#if PROFILE == 1
    for (int phase = 0; phase < 4; ++phase) parameter_profile(phase);
    puts("initialization and frozen parameter API controls passed");
#else
    Instance current = instance(1);
    time_profile(current);
    FREE_INSTANCE(current);
    for (int phase = 0; phase < 4; ++phase)
        if (phase != 1) refuse_time(phase);
    puts("explicit time forward, transpose and mode refusal API controls passed");
#endif
    return 0;
}
