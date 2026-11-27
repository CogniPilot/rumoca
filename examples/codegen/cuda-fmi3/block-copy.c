// A block owns one component. Threads 0..31 copy global arrays while thread 32
// runs the scalar solver. The two barriers publish a request and acknowledge
// completion before the solver can reuse either buffer.
#include <stddef.h>
#include <stdint.h>
typedef uint64_t Word __attribute__((may_alias));
typedef uint64_t Pair __attribute__((vector_size(16), may_alias));

#define SHARED __attribute__((address_space(3)))
static unsigned char *volatile SHARED destination;
static const unsigned char *volatile SHARED source;
static volatile size_t SHARED length;

static inline void rendezvous(void) {
    __asm__ volatile("bar.sync 0;" ::: "memory");
}

static inline int global_address(const void *pointer) {
    int result;
    __asm__ ("{ .reg .pred p; isspacep.global p, %1; selp.u32 %0,1,0,p; }"
             : "=r"(result) : "l"(pointer));
    return result;
}

int rmc_block_copy(unsigned char *to, const unsigned char *from, size_t bytes) {
    if (bytes < 512 || (((uintptr_t)to | (uintptr_t)from) & 7) ||
        !global_address(to) || !global_address(from)) return 0;
    destination = to;
    source = from;
    length = bytes;
    rendezvous();
    rendezvous();
    return 1;
}

void rmc_copy_workers_finish(void) {
    length = 0;
    rendezvous();
}

void rmc_copy_worker(unsigned thread) {
    for (;;) {
        rendezvous();
        size_t bytes = length;
        if (!bytes) return;
        unsigned char *to = destination;
        const unsigned char *from = source;
        size_t width;
        if ((((uintptr_t)to | (uintptr_t)from) & 15) == 0) {
            width = sizeof(Pair);
            for (size_t i = thread; i < bytes / width; i += 32)
                ((Pair *)to)[i] = ((const Pair *)from)[i];
        } else {
            width = sizeof(Word);
            for (size_t i = thread; i < bytes / width; i += 32)
                ((Word *)to)[i] = ((const Word *)from)[i];
        }
        if (thread < bytes % width) {
            size_t index = bytes - bytes % width + thread;
            to[index] = from[index];
        }
        rendezvous();
    }
}
