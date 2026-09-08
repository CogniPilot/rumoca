#ifndef RUMOCA_HEAD_TO_HEAD_TRACE_OUTPUT_H
#define RUMOCA_HEAD_TO_HEAD_TRACE_OUTPUT_H

#include <stdint.h>

void trace_report_output(const char *name, const float *values, uint32_t count);
void trace_report_case_output(const char *name,
                              const char *case_name,
                              const float *values,
                              uint32_t count);
int trace_output_matches(const float *values,
                         const uint32_t *expected_bits,
                         uint32_t count);

#endif
