// Helper C library for testing custom C code linking
// This file demonstrates that Klar can link with user-provided C code.

#include <stdint.h>

// Simple function that adds two numbers
int32_t add_numbers(int32_t a, int32_t b) {
    return a + b;
}

// Function that returns a constant (for simple verification)
int32_t get_magic_number(void) {
    return 42;
}

// Function that modifies a value through a pointer (out parameter pattern)
void double_value(int32_t *value) {
    *value = *value * 2;
}

// Struct for testing sized extern type passing
typedef struct {
    int32_t x;
    int32_t y;
} Point;

// Function that takes a sized struct by value
int32_t point_sum(Point p) {
    return p.x + p.y;
}

// Function that returns a sized struct by value
Point make_point(int32_t x, int32_t y) {
    Point p = {x, y};
    return p;
}

// Function that writes to an out parameter and returns a status
int32_t get_value(int32_t key, int32_t *out_value) {
    // Simulate looking up a value by key
    if (key == 42) {
        *out_value = 100;
        return 1;  // Success
    } else {
        *out_value = 0;
        return 0;  // Not found
    }
}

// Function with multiple out parameters
int32_t divide_with_remainder(int32_t dividend, int32_t divisor, int32_t *out_quotient, int32_t *out_remainder) {
    if (divisor == 0) {
        return 0;  // Error
    }
    *out_quotient = dividend / divisor;
    *out_remainder = dividend % divisor;
    return 1;  // Success
}
