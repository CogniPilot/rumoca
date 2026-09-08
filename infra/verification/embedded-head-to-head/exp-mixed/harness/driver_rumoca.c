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

/* The embedded step is a per-cycle hot path; apply the same hint to both
 * measured columns so GCC does not classify the root entry as cold. */
int __attribute__((hot)) main(void) {
    const volatile ExpMixedInput *input = &INPUT_CASES[0];

    ExpMixedStep_startup(&state);
    copy_float(state.X0, input->X0, 10);
    copy_float(state.l, input->l, 9);
    copy_float(state.r, input->r, 9);
    copy_float(&state.B[0][0], &input->B[0][0], 4);
    TRACE_BEGIN();
    ExpMixedStep_dostep(&state); /* MEASURED_CALL */
    TRACE_END();

    trace_report_case_output("rumoca_exp_mixed",
                             CORRECTNESS_CASE_NAMES[0],
                             state.X1,
                             10);
    return trace_output_matches(state.X1,
                                EXPECTED_OUTPUT_BITS[0],
                                10) &&
                   state.rumoca_galec_error_signal_status == UINT32_C(0)
               ? 0
               : 1;
}
