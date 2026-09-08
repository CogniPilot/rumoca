ENTRY(Reset_Handler)

MEMORY
{
    FLASH (rx)  : ORIGIN = 0x00000000, LENGTH = 4M
    RAM   (rwx) : ORIGIN = 0x20000000, LENGTH = 4M
}

__stack_top = ORIGIN(RAM) + LENGTH(RAM);

SECTIONS
{
    .isr_vector : { KEEP(*(.isr_vector)) } > FLASH
    .text : { *(.text*) *(.rodata*) KEEP(*(.init)) KEEP(*(.fini)) } > FLASH
    .ARM.exidx : { *(.ARM.exidx*) *(.ARM.extab*) } > FLASH
    .data :
    {
        . = ALIGN(4); __data_start = .; *(.data*)
        . = ALIGN(4); __data_end = .;
    } > RAM AT > FLASH
    __data_load = LOADADDR(.data);
    .bss (NOLOAD) :
    {
        . = ALIGN(4); __bss_start = .; *(.bss*) *(COMMON)
        . = ALIGN(4); __bss_end = .;
    } > RAM
    /DISCARD/ : { *(.comment) *(.note*) }
}
