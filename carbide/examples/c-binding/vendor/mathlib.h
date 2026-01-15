/**
 * Example C library header for demonstrating Zig C interop.
 */

#ifndef MATHLIB_H
#define MATHLIB_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Error codes
#define MATHLIB_OK          0
#define MATHLIB_ERR_NULL   -1
#define MATHLIB_ERR_RANGE  -2
#define MATHLIB_ERR_ZERO   -3

// Simple math operations
int32_t mathlib_add(int32_t a, int32_t b);
int32_t mathlib_multiply(int32_t a, int32_t b);

// Operations that can fail
int mathlib_divide(int32_t a, int32_t b, int32_t* result);
int mathlib_sqrt(int32_t n, int32_t* result);

// Array operations
int mathlib_sum_array(const int32_t* arr, size_t len, int64_t* result);
int mathlib_find_max(const int32_t* arr, size_t len, int32_t* result);

// String operations (demonstrates null-terminated strings)
size_t mathlib_strlen(const char* str);
int mathlib_parse_int(const char* str, int32_t* result);

// Struct handling
typedef struct {
    int32_t x;
    int32_t y;
} Point;

typedef struct {
    Point top_left;
    Point bottom_right;
} Rectangle;

int32_t mathlib_point_distance_squared(const Point* a, const Point* b);
int32_t mathlib_rectangle_area(const Rectangle* rect);

// Callback example
typedef void (*mathlib_callback_fn)(void* user_data, int32_t value);
void mathlib_foreach(const int32_t* arr, size_t len, mathlib_callback_fn callback, void* user_data);

// Memory allocation (caller must free)
char* mathlib_format_point(const Point* p);
void mathlib_free_string(char* str);

#ifdef __cplusplus
}
#endif

#endif // MATHLIB_H
