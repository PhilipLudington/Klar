const std = @import("std");
const Allocator = std.mem.Allocator;

pub const TaskId = u64;

pub const TaskOutcome = enum {
    completed,
    failed,
    cancelled,
};

pub const TaskFn = *const fn (ctx: *anyopaque) anyerror!void;

pub const Task = struct {
    id: TaskId,
    ctx: *anyopaque,
    run: TaskFn,
};

pub const ExecutionRecord = struct {
    task_id: TaskId,
    outcome: TaskOutcome,
};

pub const RunStats = struct {
    executed: usize,
    failed: usize,
    // Number of cancellations observed since the previous successful runUntilIdle call.
    cancelled: usize,
};

/// Single-threaded cooperative executor with deterministic FIFO scheduling.
pub const CooperativeExecutor = struct {
    allocator: Allocator,
    queue: std.ArrayListUnmanaged(Task) = .{},
    queue_head: usize = 0,
    history: std.ArrayListUnmanaged(ExecutionRecord) = .{},
    next_task_id: TaskId = 1,
    executed_count: usize = 0,
    failed_count: usize = 0,
    cancelled_count: usize = 0,
    pending_cancelled_count: usize = 0,

    pub fn init(allocator: Allocator) CooperativeExecutor {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *CooperativeExecutor) void {
        self.queue.deinit(self.allocator);
        self.history.deinit(self.allocator);
    }

    pub fn schedule(self: *CooperativeExecutor, ctx: *anyopaque, run: TaskFn) !TaskId {
        const id = self.next_task_id;
        self.next_task_id += 1;
        try self.queue.append(self.allocator, .{
            .id = id,
            .ctx = ctx,
            .run = run,
        });
        return id;
    }

    fn recordHistory(self: *CooperativeExecutor, task_id: TaskId, outcome: TaskOutcome) !void {
        try self.history.append(self.allocator, .{
            .task_id = task_id,
            .outcome = outcome,
        });
    }

    fn compactQueueIfIdle(self: *CooperativeExecutor) void {
        if (self.queue_head >= self.queue.items.len) {
            self.queue.clearRetainingCapacity();
            self.queue_head = 0;
        }
    }

    pub fn cancel(self: *CooperativeExecutor, task_id: TaskId) !bool {
        var idx = self.queue_head;
        while (idx < self.queue.items.len) : (idx += 1) {
            const task = self.queue.items[idx];
            if (task.id == task_id) {
                try self.recordHistory(task_id, .cancelled);
                _ = self.queue.orderedRemove(idx);
                self.cancelled_count += 1;
                self.pending_cancelled_count += 1;
                self.compactQueueIfIdle();
                return true;
            }
        }
        return false;
    }

    pub fn hasPending(self: *const CooperativeExecutor) bool {
        return self.queue_head < self.queue.items.len;
    }

    pub fn runUntilIdle(self: *CooperativeExecutor) !RunStats {
        const start_executed = self.executed_count;
        const start_failed = self.failed_count;

        while (self.queue_head < self.queue.items.len) {
            const task = self.queue.items[self.queue_head];
            self.queue_head += 1;
            task.run(task.ctx) catch {
                self.failed_count += 1;
                self.executed_count += 1;
                try self.recordHistory(task.id, .failed);
                continue;
            };

            self.executed_count += 1;
            try self.recordHistory(task.id, .completed);
        }
        self.compactQueueIfIdle();
        const cancelled_for_run = self.pending_cancelled_count;
        self.pending_cancelled_count = 0;

        return .{
            .executed = self.executed_count - start_executed,
            .failed = self.failed_count - start_failed,
            .cancelled = cancelled_for_run,
        };
    }
};

const TestCtx = struct {
    allocator: Allocator,
    log: std.ArrayListUnmanaged(u8) = .{},
    fail_byte: ?u8 = null,

    fn deinit(self: *TestCtx) void {
        self.log.deinit(self.allocator);
    }
};

fn appendByteTask(ctx_ptr: *anyopaque) anyerror!void {
    const ctx: *TestCtx = @ptrCast(@alignCast(ctx_ptr));
    const byte = ctx.fail_byte orelse return error.InvalidContext;
    if (byte == '!') return error.SimulatedTaskFailure;
    try ctx.log.append(ctx.allocator, byte);
}

test "cooperative executor runs tasks in FIFO order" {
    const testing = std.testing;

    var exec = CooperativeExecutor.init(testing.allocator);
    defer exec.deinit();

    var a = TestCtx{ .allocator = testing.allocator, .fail_byte = 'a' };
    defer a.deinit();
    var b = TestCtx{ .allocator = testing.allocator, .fail_byte = 'b' };
    defer b.deinit();
    var c = TestCtx{ .allocator = testing.allocator, .fail_byte = 'c' };
    defer c.deinit();

    const a_id = try exec.schedule(&a, appendByteTask);
    const b_id = try exec.schedule(&b, appendByteTask);
    const c_id = try exec.schedule(&c, appendByteTask);

    const stats = try exec.runUntilIdle();
    try testing.expectEqual(@as(usize, 3), stats.executed);
    try testing.expectEqual(@as(usize, 0), stats.failed);
    try testing.expectEqual(@as(usize, 0), stats.cancelled);
    try testing.expect(!exec.hasPending());

    try testing.expectEqual(@as(usize, 3), exec.history.items.len);
    try testing.expectEqual(a_id, exec.history.items[0].task_id);
    try testing.expectEqual(b_id, exec.history.items[1].task_id);
    try testing.expectEqual(c_id, exec.history.items[2].task_id);
    try testing.expectEqual(TaskOutcome.completed, exec.history.items[0].outcome);
    try testing.expectEqual(TaskOutcome.completed, exec.history.items[1].outcome);
    try testing.expectEqual(TaskOutcome.completed, exec.history.items[2].outcome);
}

test "cooperative executor isolates task failures" {
    const testing = std.testing;

    var exec = CooperativeExecutor.init(testing.allocator);
    defer exec.deinit();

    var good = TestCtx{ .allocator = testing.allocator, .fail_byte = 'g' };
    defer good.deinit();
    var bad = TestCtx{ .allocator = testing.allocator, .fail_byte = '!' };
    defer bad.deinit();
    var good2 = TestCtx{ .allocator = testing.allocator, .fail_byte = 'h' };
    defer good2.deinit();

    _ = try exec.schedule(&good, appendByteTask);
    _ = try exec.schedule(&bad, appendByteTask);
    _ = try exec.schedule(&good2, appendByteTask);

    const stats = try exec.runUntilIdle();
    try testing.expectEqual(@as(usize, 3), stats.executed);
    try testing.expectEqual(@as(usize, 1), stats.failed);
    try testing.expectEqual(TaskOutcome.failed, exec.history.items[1].outcome);
}

test "cooperative executor supports deterministic cancellation" {
    const testing = std.testing;

    var exec = CooperativeExecutor.init(testing.allocator);
    defer exec.deinit();

    var first = TestCtx{ .allocator = testing.allocator, .fail_byte = 'x' };
    defer first.deinit();
    var second = TestCtx{ .allocator = testing.allocator, .fail_byte = 'y' };
    defer second.deinit();

    _ = try exec.schedule(&first, appendByteTask);
    const second_id = try exec.schedule(&second, appendByteTask);
    try testing.expect(try exec.cancel(second_id));

    const stats = try exec.runUntilIdle();
    try testing.expectEqual(@as(usize, 1), stats.executed);
    try testing.expectEqual(@as(usize, 0), stats.failed);
    try testing.expectEqual(@as(usize, 1), stats.cancelled);
    try testing.expectEqual(@as(usize, 1), exec.cancelled_count);
    try testing.expectEqual(TaskOutcome.cancelled, exec.history.items[0].outcome);
    try testing.expectEqual(TaskOutcome.completed, exec.history.items[1].outcome);

    const next_stats = try exec.runUntilIdle();
    try testing.expectEqual(@as(usize, 0), next_stats.cancelled);
}

test "cooperative executor keeps task queued when cancellation history append fails" {
    const testing = std.testing;

    var backing_buffer: [256]u8 = undefined;
    var backing_alloc = std.heap.FixedBufferAllocator.init(&backing_buffer);
    var failing_alloc = std.testing.FailingAllocator.init(backing_alloc.allocator(), .{
        .fail_index = 1,
    });

    var exec = CooperativeExecutor.init(failing_alloc.allocator());
    defer exec.deinit();

    var ctx = TestCtx{ .allocator = testing.allocator, .fail_byte = 'z' };
    defer ctx.deinit();

    const task_id = try exec.schedule(&ctx, appendByteTask);
    try testing.expectError(error.OutOfMemory, exec.cancel(task_id));
    try testing.expect(exec.hasPending());
    try testing.expectEqual(@as(usize, 0), exec.cancelled_count);
    try testing.expectEqual(@as(usize, 1), exec.queue.items.len - exec.queue_head);
    try testing.expectEqual(task_id, exec.queue.items[exec.queue_head].id);
}
