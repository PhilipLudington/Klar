/* Test linker script for bare-metal ARM64 */
ENTRY(_start)
SECTIONS {
    . = 0x80000;
    .text : { *(.text*) }
    .rodata : { *(.rodata*) }
    .data : { *(.data*) }
    .bss : { *(.bss*) }
}
