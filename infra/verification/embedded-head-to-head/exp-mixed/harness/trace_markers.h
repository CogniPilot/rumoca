#ifndef RUMOCA_HEAD_TO_HEAD_TRACE_MARKERS_H
#define RUMOCA_HEAD_TO_HEAD_TRACE_MARKERS_H

#define TRACE_BEGIN()                                                         \
    __asm__ volatile(".global trace_begin\n"                                  \
                     ".type trace_begin, %%function\n"                       \
                     "trace_begin:\n"                                        \
                     "nop\n"                                                 \
                     ::: "memory")

#define TRACE_END()                                                           \
    __asm__ volatile(".global trace_end\n"                                    \
                     ".type trace_end, %%function\n"                         \
                     "trace_end:\n"                                          \
                     "nop\n"                                                 \
                     ::: "memory")

#endif
