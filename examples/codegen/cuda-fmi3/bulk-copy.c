// Called for nonvolatile, nonoverlapping LLVM memcpy intrinsics in generic address space.
// Some generated FMI arrays carry byte alignment in IR but are aligned at runtime.
#include <stdint.h>
#include <stddef.h>

typedef uint64_t Word __attribute__((may_alias));
typedef uint64_t Pair __attribute__((vector_size(16), may_alias));

#ifdef RMC_CUDA_BLOCK_COPIES
int rmc_block_copy(unsigned char *, const unsigned char *, size_t);
#endif

void rmc_cuda_copy(unsigned char *destination, const unsigned char *source, size_t count) {
    if (destination == source) return;
#ifdef RMC_CUDA_BLOCK_COPIES
    if (rmc_block_copy(destination, source, count)) return;
#endif
    uintptr_t addresses = (uintptr_t)destination | (uintptr_t)source;
    if ((addresses & 7) == 0) {
        if ((addresses & 15) == 0) {
            while (count >= 64) {
                const Pair *from = (const Pair *)source;
                Pair *to = (Pair *)destination;
                for (size_t i = 0; i < 4; ++i) to[i] = from[i];
                destination += 64;
                source += 64;
                count -= 64;
            }
        }
        while (count >= 8) {
            *(Word *)destination = *(const Word *)source;
            destination += 8;
            source += 8;
            count -= 8;
        }
    }
    while (count--) *destination++ = *source++;
}
