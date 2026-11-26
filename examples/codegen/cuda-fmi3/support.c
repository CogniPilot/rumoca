#include <stdlib.h>
#include <string.h>

void *calloc(size_t count, size_t size) {
    if (count == 0 || size > (size_t)-1 / count) return NULL;
    size_t bytes = count * size;
    void *result = malloc(bytes);
    if (result) memset(result, 0, bytes);
    return result;
}

int memcmp(const void *left, const void *right, size_t size) {
    const unsigned char *a = left, *b = right;
    for (size_t i = 0; i < size; ++i) {
        if (a[i] != b[i]) return (int)a[i] - (int)b[i];
    }
    return 0;
}

/* The numeric device ABI excludes host logger callbacks and formatted text. */
int snprintf(char *destination, size_t size, const char *format, ...) {
    __builtin_trap();
}
