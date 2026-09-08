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

static void clear_float(float *values, uint32_t count) {
    uint32_t index;

    for (index = 0; index < count; index += 1) {
        values[index] = 0.0f;
    }
}

static void load_case(uint32_t case_index) {
    const volatile ExpMixedInput *input = &INPUT_CASES[case_index];

    copy_float(input_state, input->X0, 10);
    copy_float(input_left, input->l, 9);
    copy_float(input_right, input->r, 9);
    input_coupling_column_major[0] = input->B[0][0];
    input_coupling_column_major[1] = input->B[1][0]; /* COLUMN_MAJOR_1 */
    input_coupling_column_major[2] = input->B[0][1]; /* COLUMN_MAJOR_2 */
    input_coupling_column_major[3] = input->B[1][1];
    clear_float(output, 10);
}

static int report_case(uint32_t case_index, int operation_status) {
    trace_report_case_output("casadi_exp_mixed",
                             CORRECTNESS_CASE_NAMES[case_index],
                             output,
                             10);
    return trace_output_matches(output,
                                EXPECTED_OUTPUT_BITS[case_index],
                                10) &&
           operation_status == 0;
}

int main(void) {
    const casadi_real *arguments[4];
    casadi_real *results[1];
    uint32_t case_index;
    int accepted;

    arguments[0] = input_state;
    arguments[1] = input_left;
    arguments[2] = input_right;
    arguments[3] = input_coupling_column_major;
    results[0] = output;

    load_case(0);
    TRACE_BEGIN();
    accepted = exp_mixed_full(arguments, results, 0, 0, 0); /* MEASURED_CALL */
    TRACE_END();
    accepted = report_case(0, accepted);

    for (case_index = 1; case_index < CORRECTNESS_CASE_COUNT; case_index += 1) {
        int operation_status;

        load_case(case_index);
        operation_status = exp_mixed_full(arguments,
                                          results,
                                          0,
                                          0,
                                          0); /* COHORT_CALL */
        accepted &= report_case(case_index, operation_status);
    }
    return accepted ? 0 : 1;
}
