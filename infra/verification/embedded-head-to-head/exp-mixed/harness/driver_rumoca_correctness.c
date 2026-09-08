#include <stdint.h>

#include "ExpMixedStep.h"
#include "expected_output.h"
#include "inputs.h"
#include "trace_markers.h"
#include "trace_output.h"

static ExpMixedStepState state;

static void copy_float(float *destination,
                       const volatile float *source,
                       uint32_t count) {
    uint32_t index;

    for (index = 0; index < count; index += 1) {
        destination[index] = source[index];
    }
}

static void load_case(uint32_t case_index) {
    const volatile ExpMixedInput *input = &INPUT_CASES[case_index];

    state = (ExpMixedStepState){0};
    ExpMixedStep_startup(&state);
    copy_float(state.X0, input->X0, 10);
    copy_float(state.l, input->l, 9);
    copy_float(state.r, input->r, 9);
    copy_float(&state.B[0][0], &input->B[0][0], 4);
}

static int report_case(uint32_t case_index) {
    trace_report_case_output("rumoca_exp_mixed",
                             CORRECTNESS_CASE_NAMES[case_index],
                             state.X1,
                             10);
    return trace_output_matches(state.X1,
                                EXPECTED_OUTPUT_BITS[case_index],
                                10) &&
           state.rumoca_galec_error_signal_status == UINT32_C(0);
}

int main(void) {
    uint32_t case_index;
    int accepted;

    load_case(0);
    TRACE_BEGIN();
    ExpMixedStep_dostep(&state); /* MEASURED_CALL */
    TRACE_END();
    accepted = report_case(0);

    for (case_index = 1; case_index < CORRECTNESS_CASE_COUNT; case_index += 1) {
        load_case(case_index);
        ExpMixedStep_dostep(&state); /* COHORT_CALL */
        accepted &= report_case(case_index);
    }
    return accepted ? 0 : 1;
}
