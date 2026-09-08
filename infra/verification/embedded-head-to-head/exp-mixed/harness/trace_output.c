#include "trace_output.h"

typedef union {
    float value;
    uint32_t bits;
} FloatBits;

static char *append_text(char *destination, const char *source) {
    while (*source != '\0') {
        *destination++ = *source++;
    }
    return destination;
}

static char *append_hex32(char *destination, uint32_t value) {
    static const char digits[] = "0123456789abcdef";
    int shift;

    for (shift = 28; shift >= 0; shift -= 4) {
        *destination++ = digits[(value >> shift) & 0xfu];
    }
    return destination;
}

static void semihost_write_zero_terminated(const char *text) {
    register uint32_t operation __asm__("r0") = 4;
    register const char *argument __asm__("r1") = text;

    __asm__ volatile("bkpt 0xab"
                     : "+r"(operation), "+r"(argument)
                     :
                     : "memory");
}

void trace_report_output(const char *name, const float *values, uint32_t count) {
    char buffer[160];
    char *cursor = buffer;
    uint32_t index;

    cursor = append_text(cursor, "OUTPUT name=");
    cursor = append_text(cursor, name);
    cursor = append_text(cursor, " bits=");
    for (index = 0; index < count; index += 1) {
        FloatBits encoded;

        encoded.value = values[index];
        cursor = append_hex32(cursor, encoded.bits);
        if (index + 1 < count) {
            *cursor++ = ',';
        }
    }
    *cursor++ = '\n';
    *cursor = '\0';
    semihost_write_zero_terminated(buffer);
}

void trace_report_case_output(const char *name,
                              const char *case_name,
                              const float *values,
                              uint32_t count) {
    char buffer[192];
    char *cursor = buffer;
    uint32_t index;

    cursor = append_text(cursor, "OUTPUT name=");
    cursor = append_text(cursor, name);
    cursor = append_text(cursor, " case=");
    cursor = append_text(cursor, case_name);
    cursor = append_text(cursor, " bits=");
    for (index = 0; index < count; index += 1) {
        FloatBits encoded;

        encoded.value = values[index];
        cursor = append_hex32(cursor, encoded.bits);
        if (index + 1 < count) {
            *cursor++ = ',';
        }
    }
    *cursor++ = '\n';
    *cursor = '\0';
    semihost_write_zero_terminated(buffer);
}

int trace_output_matches(const float *values,
                         const uint32_t *expected_bits,
                         uint32_t count) {
    uint32_t index;

    for (index = 0; index < count; index += 1) {
        FloatBits encoded;

        encoded.value = values[index];
        if (encoded.bits != expected_bits[index]) {
            return 0;
        }
    }
    return 1;
}
