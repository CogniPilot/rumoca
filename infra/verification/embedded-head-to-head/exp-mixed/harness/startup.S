.syntax unified
.cpu cortex-m7
.fpu fpv5-d16
.thumb

.section .isr_vector, "a", %progbits
.align 2
.global vector_table
vector_table:
    .word __stack_top
    .word Reset_Handler + 1
    .rept 14
    .word Default_Handler + 1
    .endr

.section .text.Reset_Handler, "ax", %progbits
.align 2
.thumb_func
.global Reset_Handler
.type Reset_Handler, %function
Reset_Handler:
    ldr r0, =__data_load
    ldr r1, =__data_start
    ldr r2, =__data_end
1:
    cmp r1, r2
    bcs 2f
    ldr r3, [r0], #4
    str r3, [r1], #4
    b 1b
2:
    ldr r1, =__bss_start
    ldr r2, =__bss_end
    movs r3, #0
3:
    cmp r1, r2
    bcs 4f
    str r3, [r1], #4
    b 3b
4:
    ldr r0, =0xe000ed88
    ldr r1, [r0]
    orr r1, r1, #0x00f00000
    str r1, [r0]
    dsb
    isb
    movs r2, #0
    vmsr fpscr, r2
    bl main

    sub sp, sp, #8
    ldr r1, =0x00020026
    str r1, [sp]
    str r0, [sp, #4]
    mov r1, sp
    movs r0, #0x20
    bkpt 0xab
5:
    b 5b
.size Reset_Handler, . - Reset_Handler

.thumb_func
.global Default_Handler
.type Default_Handler, %function
Default_Handler:
    movs r0, #1
    b Reset_Handler
.size Default_Handler, . - Default_Handler
