// Native memory semantics witness for the device helper, including unaligned inputs.
#include <assert.h>
#include <stddef.h>
#include <string.h>

void rmc_cuda_copy(unsigned char *, const unsigned char *, size_t);

int main(void) {
    _Alignas(16) unsigned char source[320], actual[320], expected[320];
    for (size_t i = 0; i < sizeof(source); ++i) source[i] = (unsigned char)(i * 37);
    for (size_t from = 0; from < 16; ++from) {
        for (size_t to = 0; to < 16; ++to) {
            for (size_t size = 0; size <= 256; ++size) {
                memset(expected, 0xab, sizeof(expected));
                memcpy(actual, expected, sizeof(actual));
                memcpy(expected + to, source + from, size);
                rmc_cuda_copy(actual + to, source + from, size);
                rmc_cuda_copy(actual + to, actual + to, size);
                assert(memcmp(actual, expected, sizeof(actual)) == 0);
            }
        }
    }
    return 0;
}
