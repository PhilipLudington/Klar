/**
 * Example C library implementation for demonstrating Zig C interop.
 */

#include "mathlib.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

int32_t mathlib_add(int32_t a, int32_t b) {
    return a + b;
}

int32_t mathlib_multiply(int32_t a, int32_t b) {
    return a * b;
}

int mathlib_divide(int32_t a, int32_t b, int32_t* result) {
    if (result == NULL) {
        return MATHLIB_ERR_NULL;
    }
    if (b == 0) {
        return MATHLIB_ERR_ZERO;
    }
    *result = a / b;
    return MATHLIB_OK;
}

int mathlib_sqrt(int32_t n, int32_t* result) {
    if (result == NULL) {
        return MATHLIB_ERR_NULL;
    }
    if (n < 0) {
        return MATHLIB_ERR_RANGE;
    }

    // Integer square root using Newton's method
    if (n == 0) {
        *result = 0;
        return MATHLIB_OK;
    }

    int32_t x = n;
    int32_t y = (x + 1) / 2;
    while (y < x) {
        x = y;
        y = (x + n / x) / 2;
    }
    *result = x;
    return MATHLIB_OK;
}

int mathlib_sum_array(const int32_t* arr, size_t len, int64_t* result) {
    if (arr == NULL || result == NULL) {
        return MATHLIB_ERR_NULL;
    }

    int64_t sum = 0;
    for (size_t i = 0; i < len; i++) {
        sum += arr[i];
    }
    *result = sum;
    return MATHLIB_OK;
}

int mathlib_find_max(const int32_t* arr, size_t len, int32_t* result) {
    if (arr == NULL || result == NULL) {
        return MATHLIB_ERR_NULL;
    }
    if (len == 0) {
        return MATHLIB_ERR_RANGE;
    }

    int32_t max = arr[0];
    for (size_t i = 1; i < len; i++) {
        if (arr[i] > max) {
            max = arr[i];
        }
    }
    *result = max;
    return MATHLIB_OK;
}

size_t mathlib_strlen(const char* str) {
    if (str == NULL) {
        return 0;
    }
    return strlen(str);
}

int mathlib_parse_int(const char* str, int32_t* result) {
    if (str == NULL || result == NULL) {
        return MATHLIB_ERR_NULL;
    }

    char* endptr;
    errno = 0;
    long val = strtol(str, &endptr, 10);

    if (errno != 0 || endptr == str || *endptr != '\0') {
        return MATHLIB_ERR_RANGE;
    }

    if (val < INT32_MIN || val > INT32_MAX) {
        return MATHLIB_ERR_RANGE;
    }

    *result = (int32_t)val;
    return MATHLIB_OK;
}

int32_t mathlib_point_distance_squared(const Point* a, const Point* b) {
    if (a == NULL || b == NULL) {
        return -1;
    }
    int32_t dx = b->x - a->x;
    int32_t dy = b->y - a->y;
    return dx * dx + dy * dy;
}

int32_t mathlib_rectangle_area(const Rectangle* rect) {
    if (rect == NULL) {
        return -1;
    }
    int32_t width = rect->bottom_right.x - rect->top_left.x;
    int32_t height = rect->bottom_right.y - rect->top_left.y;
    if (width < 0) width = -width;
    if (height < 0) height = -height;
    return width * height;
}

void mathlib_foreach(const int32_t* arr, size_t len, mathlib_callback_fn callback, void* user_data) {
    if (arr == NULL || callback == NULL) {
        return;
    }
    for (size_t i = 0; i < len; i++) {
        callback(user_data, arr[i]);
    }
}

char* mathlib_format_point(const Point* p) {
    if (p == NULL) {
        return NULL;
    }

    char* buffer = malloc(64);
    if (buffer == NULL) {
        return NULL;
    }

    snprintf(buffer, 64, "Point(%d, %d)", p->x, p->y);
    return buffer;
}

void mathlib_free_string(char* str) {
    free(str);
}
