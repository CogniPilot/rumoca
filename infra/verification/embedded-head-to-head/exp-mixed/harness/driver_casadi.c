#include <stdint.h>

#include "casadi_exp_mixed.h"
#include "expected_output.h"
#include "inputs.h"
#include "trace_markers.h"
#include "trace_output.h"

static float output[10];
static float input_state[10];
static float input_left[9];
static float input_right[9];
static float input_coupling_column_major[4];

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
    const casadi_real *arguments[4];
    casadi_real *results[1];
    int operation_status;

    copy_float(input_state, input->X0, 10);
    copy_float(input_left, input->l, 9);
    copy_float(input_right, input->r, 9);
    input_coupling_column_major[0] = input->B[0][0];
    input_coupling_column_major[1] = input->B[1][0];
    input_coupling_column_major[2] = input->B[0][1];
    input_coupling_column_major[3] = input->B[1][1];
    arguments[0] = input_state;
    arguments[1] = input_left;
    arguments[2] = input_right;
    arguments[3] = input_coupling_column_major;
    results[0] = output;

    TRACE_BEGIN();
    operation_status = exp_mixed_full(arguments,
                                      results,
                                      0,
                                      0,
                                      0); /* MEASURED_CALL */
    TRACE_END();

    trace_report_case_output("casadi_exp_mixed",
                             CORRECTNESS_CASE_NAMES[0],
                             output,
                             10);
    return trace_output_matches(output, EXPECTED_OUTPUT_BITS[0], 10) &&
                   operation_status == 0
               ? 0
               : 1;
}
