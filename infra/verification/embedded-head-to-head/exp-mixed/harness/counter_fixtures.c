#include "trace_markers.h"

#if defined(FIXTURE_STRAIGHT)
__attribute__((naked, noinline)) static void fixture_operation(void) {
    __asm__ volatile("movs r0, #1\n"
                     "adds r0, r0, #2\n"
                     "eors r0, r0, r0\n"
                     "bx lr\n");
}
#elif defined(FIXTURE_CALLED_LEAF)
__attribute__((naked, noinline, used)) static void fixture_leaf(void) {
    __asm__ volatile("movs r0, #3\n"
                     "adds r0, r0, #4\n"
                     "bx lr\n");
}

__attribute__((naked, noinline)) static void fixture_operation(void) {
    __asm__ volatile("push {lr}\n"
                     "bl fixture_leaf\n"
                     "pop {pc}\n");
}
#else
#error "select exactly one counter fixture"
#endif

int main(void) {
    TRACE_BEGIN();
    fixture_operation();
    TRACE_END();

#if defined(FIXTURE_RETURN_FAILURE)
    return 7;
#else
    return 0;
#endif
}
