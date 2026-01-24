// ============================================================================
// Klar Runtime Library
// ============================================================================
//
// This module provides runtime support for Klar native executables.
// It includes reference counting (Rc/Arc), memory allocation, and panic handling.

const std = @import("std");

pub const rc = @import("rc.zig");
pub const arc = @import("arc.zig");
pub const alloc = @import("alloc.zig");
pub const string = @import("string.zig");
pub const string_heap = @import("string_heap.zig");
pub const list = @import("list.zig");
pub const args = @import("args.zig");

// Re-export commonly used functions for C-compatible FFI

// Rc (non-thread-safe reference counting)
pub const klar_rc_alloc = rc.klar_rc_alloc;
pub const klar_rc_clone = rc.klar_rc_clone;
pub const klar_rc_drop = rc.klar_rc_drop;
pub const klar_weak_clone = rc.klar_weak_clone;
pub const klar_weak_drop = rc.klar_weak_drop;
pub const klar_weak_upgrade = rc.klar_weak_upgrade;
pub const klar_rc_downgrade = rc.klar_rc_downgrade;

// Arc (thread-safe atomic reference counting)
pub const klar_arc_alloc = arc.klar_arc_alloc;
pub const klar_arc_clone = arc.klar_arc_clone;
pub const klar_arc_drop = arc.klar_arc_drop;
pub const klar_weak_arc_clone = arc.klar_weak_arc_clone;
pub const klar_weak_arc_drop = arc.klar_weak_arc_drop;
pub const klar_weak_arc_upgrade = arc.klar_weak_arc_upgrade;
pub const klar_arc_downgrade = arc.klar_arc_downgrade;

// String literal operations
pub const klar_string_trim = string.klar_string_trim;
pub const klar_string_to_uppercase = string.klar_string_to_uppercase;
pub const klar_string_to_lowercase = string.klar_string_to_lowercase;
pub const klar_string_chars = string.klar_string_chars;

// Heap-allocated String operations
pub const klar_string_new = string_heap.klar_string_new;
pub const klar_string_with_capacity = string_heap.klar_string_with_capacity;
pub const klar_string_from = string_heap.klar_string_from;
pub const klar_string_clone_heap = string_heap.klar_string_clone_heap;
pub const klar_string_len_heap = string_heap.klar_string_len_heap;
pub const klar_string_is_empty_heap = string_heap.klar_string_is_empty_heap;
pub const klar_string_capacity_heap = string_heap.klar_string_capacity;
pub const klar_string_push_byte = string_heap.klar_string_push_byte;
pub const klar_string_push_char = string_heap.klar_string_push_char;
pub const klar_string_append = string_heap.klar_string_append;
pub const klar_string_concat = string_heap.klar_string_concat;
pub const klar_string_as_ptr = string_heap.klar_string_as_ptr;
pub const klar_string_clear_heap = string_heap.klar_string_clear;
pub const klar_string_drop_heap = string_heap.klar_string_drop;
pub const klar_string_eq_heap = string_heap.klar_string_eq;
pub const klar_string_eq_literal = string_heap.klar_string_eq_literal;
pub const klar_string_hash_heap = string_heap.klar_string_hash_heap;

// List operations
pub const klar_list_new = list.klar_list_new;
pub const klar_list_with_capacity = list.klar_list_with_capacity;
pub const klar_list_clone = list.klar_list_clone;
pub const klar_list_push = list.klar_list_push;
pub const klar_list_pop = list.klar_list_pop;
pub const klar_list_get = list.klar_list_get;
pub const klar_list_set = list.klar_list_set;
pub const klar_list_len = list.klar_list_len;
pub const klar_list_is_empty = list.klar_list_is_empty;
pub const klar_list_capacity = list.klar_list_capacity;
pub const klar_list_clear = list.klar_list_clear;
pub const klar_list_first = list.klar_list_first;
pub const klar_list_last = list.klar_list_last;
pub const klar_list_drop = list.klar_list_drop;

// Command-line arguments
pub const _klar_args_from_argv = args._klar_args_from_argv;
pub const _klar_args_free = args._klar_args_free;

test {
    _ = rc;
    _ = arc;
    _ = alloc;
    _ = string;
    _ = string_heap;
    _ = list;
    _ = args;
}
