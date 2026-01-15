const std = @import("std");
const Token = @import("token.zig").Token;

/// Source location span for error reporting
pub const Span = struct {
    start: usize,
    end: usize,
    line: u32,
    column: u32,

    pub fn from(token: Token) Span {
        return .{
            .start = token.loc.start,
            .end = token.loc.end,
            .line = token.loc.line,
            .column = token.loc.column,
        };
    }

    pub fn merge(a: Span, b: Span) Span {
        return .{
            .start = @min(a.start, b.start),
            .end = @max(a.end, b.end),
            .line = a.line,
            .column = a.column,
        };
    }
};

// ============================================================================
// Expressions
// ============================================================================

pub const Expr = union(enum) {
    literal: Literal,
    identifier: Identifier,
    binary: *Binary,
    unary: *Unary,
    postfix: *Postfix,
    call: *Call,
    index: *Index,
    field: *Field,
    method_call: *MethodCall,
    if_expr: *IfExpr,
    match_expr: *MatchExpr,
    block: *Block,
    closure: *Closure,
    range: *Range,
    struct_literal: *StructLiteral,
    array_literal: *ArrayLiteral,
    tuple_literal: *TupleLiteral,
    type_cast: *TypeCast,
    grouped: *Grouped,

    pub fn span(self: Expr) Span {
        return switch (self) {
            .literal => |l| l.span,
            .identifier => |i| i.span,
            .binary => |b| b.span,
            .unary => |u| u.span,
            .postfix => |p| p.span,
            .call => |c| c.span,
            .index => |i| i.span,
            .field => |f| f.span,
            .method_call => |m| m.span,
            .if_expr => |i| i.span,
            .match_expr => |m| m.span,
            .block => |b| b.span,
            .closure => |c| c.span,
            .range => |r| r.span,
            .struct_literal => |s| s.span,
            .array_literal => |a| a.span,
            .tuple_literal => |t| t.span,
            .type_cast => |t| t.span,
            .grouped => |g| g.span,
        };
    }
};

pub const Literal = struct {
    kind: Kind,
    span: Span,

    pub const Kind = union(enum) {
        int: i128,
        float: f64,
        string: []const u8,
        char: u21,
        bool_: bool,
    };
};

pub const Identifier = struct {
    name: []const u8,
    span: Span,
};

pub const Binary = struct {
    left: Expr,
    op: BinaryOp,
    right: Expr,
    span: Span,
};

pub const BinaryOp = enum {
    // Arithmetic
    add,
    sub,
    mul,
    div,
    mod,
    // Wrapping arithmetic
    add_wrap,
    sub_wrap,
    mul_wrap,
    // Saturating arithmetic
    add_sat,
    sub_sat,
    mul_sat,
    // Comparison
    eq,
    not_eq,
    lt,
    gt,
    lt_eq,
    gt_eq,
    is,
    // Logical
    and_,
    or_,
    // Bitwise
    bit_and,
    bit_or,
    bit_xor,
    shl,
    shr,
    // Null coalescing
    null_coalesce,
    // Assignment
    assign,
    add_assign,
    sub_assign,
    mul_assign,
    div_assign,
    mod_assign,

    pub fn fromToken(kind: Token.Kind) ?BinaryOp {
        return switch (kind) {
            .plus => .add,
            .minus => .sub,
            .star => .mul,
            .slash => .div,
            .percent => .mod,
            .plus_wrap => .add_wrap,
            .minus_wrap => .sub_wrap,
            .star_wrap => .mul_wrap,
            .plus_sat => .add_sat,
            .minus_sat => .sub_sat,
            .star_sat => .mul_sat,
            .eq_eq => .eq,
            .not_eq => .not_eq,
            .lt => .lt,
            .gt => .gt,
            .lt_eq => .lt_eq,
            .gt_eq => .gt_eq,
            .is => .is,
            .and_ => .and_,
            .or_ => .or_,
            .amp => .bit_and,
            .pipe => .bit_or,
            .caret => .bit_xor,
            .lt_lt => .shl,
            .gt_gt => .shr,
            .question_question => .null_coalesce,
            .eq => .assign,
            .plus_eq => .add_assign,
            .minus_eq => .sub_assign,
            .star_eq => .mul_assign,
            .slash_eq => .div_assign,
            .percent_eq => .mod_assign,
            else => null,
        };
    }
};

pub const Unary = struct {
    op: UnaryOp,
    operand: Expr,
    span: Span,
};

pub const UnaryOp = enum {
    negate,
    not,
    ref,
    ref_mut,
    deref,

    pub fn fromToken(kind: Token.Kind) ?UnaryOp {
        return switch (kind) {
            .minus => .negate,
            .not => .not,
            .amp => .ref,
            .star => .deref,
            else => null,
        };
    }
};

pub const Postfix = struct {
    operand: Expr,
    op: PostfixOp,
    span: Span,
};

pub const PostfixOp = enum {
    unwrap, // ?
    force_unwrap, // !
};

pub const Call = struct {
    callee: Expr,
    args: []const Expr,
    span: Span,
};

pub const Index = struct {
    object: Expr,
    index: Expr,
    span: Span,
};

pub const Field = struct {
    object: Expr,
    field_name: []const u8,
    span: Span,
};

pub const MethodCall = struct {
    object: Expr,
    method_name: []const u8,
    type_args: ?[]const TypeExpr,
    args: []const Expr,
    span: Span,
};

pub const IfExpr = struct {
    condition: Expr,
    then_branch: Expr,
    else_branch: ?Expr,
    span: Span,
};

pub const MatchExpr = struct {
    subject: Expr,
    arms: []const MatchArm,
    span: Span,
};

pub const MatchArm = struct {
    pattern: Pattern,
    guard: ?Expr,
    body: Expr,
    span: Span,
};

pub const Block = struct {
    statements: []const Stmt,
    final_expr: ?Expr,
    span: Span,
};

pub const Closure = struct {
    params: []const ClosureParam,
    return_type: ?TypeExpr,
    body: Expr,
    span: Span,
};

pub const ClosureParam = struct {
    name: []const u8,
    type_: ?TypeExpr,
    span: Span,
};

pub const Range = struct {
    start: ?Expr,
    end: ?Expr,
    inclusive: bool,
    span: Span,
};

pub const StructLiteral = struct {
    type_name: ?TypeExpr,
    fields: []const StructFieldInit,
    spread: ?Expr,
    span: Span,
};

pub const StructFieldInit = struct {
    name: []const u8,
    value: Expr,
    span: Span,
};

pub const ArrayLiteral = struct {
    elements: []const Expr,
    span: Span,
};

pub const TupleLiteral = struct {
    elements: []const Expr,
    span: Span,
};

pub const TypeCast = struct {
    expr: Expr,
    cast_kind: CastKind,
    target_type: TypeExpr,
    span: Span,
};

pub const CastKind = enum {
    as, // safe widening
    to, // checked narrowing
    trunc, // truncating
};

pub const Grouped = struct {
    expr: Expr,
    span: Span,
};

// ============================================================================
// Patterns
// ============================================================================

pub const Pattern = union(enum) {
    wildcard: Wildcard,
    literal: PatternLiteral,
    binding: Binding,
    variant: *VariantPattern,
    struct_pattern: *StructPattern,
    tuple_pattern: *TuplePattern,
    or_pattern: *OrPattern,
    guarded: *GuardedPattern,

    pub fn span(self: Pattern) Span {
        return switch (self) {
            .wildcard => |w| w.span,
            .literal => |l| l.span,
            .binding => |b| b.span,
            .variant => |v| v.span,
            .struct_pattern => |s| s.span,
            .tuple_pattern => |t| t.span,
            .or_pattern => |o| o.span,
            .guarded => |g| g.span,
        };
    }
};

pub const Wildcard = struct {
    span: Span,
};

pub const PatternLiteral = struct {
    kind: Literal.Kind,
    span: Span,
};

pub const Binding = struct {
    name: []const u8,
    mutable: bool,
    span: Span,
};

pub const VariantPattern = struct {
    type_name: ?[]const u8,
    variant_name: []const u8,
    payload: ?Pattern,
    span: Span,
};

pub const StructPattern = struct {
    type_name: ?[]const u8,
    fields: []const StructFieldPattern,
    span: Span,
};

pub const StructFieldPattern = struct {
    name: []const u8,
    pattern: ?Pattern,
    span: Span,
};

pub const TuplePattern = struct {
    elements: []const Pattern,
    span: Span,
};

pub const OrPattern = struct {
    alternatives: []const Pattern,
    span: Span,
};

pub const GuardedPattern = struct {
    pattern: Pattern,
    guard: Expr,
    span: Span,
};

// ============================================================================
// Statements
// ============================================================================

pub const Stmt = union(enum) {
    let_decl: *LetDecl,
    var_decl: *VarDecl,
    assignment: *Assignment,
    expr_stmt: *ExprStmt,
    return_stmt: *ReturnStmt,
    break_stmt: *BreakStmt,
    continue_stmt: *ContinueStmt,
    for_loop: *ForLoop,
    while_loop: *WhileLoop,
    loop_stmt: *LoopStmt,

    pub fn span(self: Stmt) Span {
        return switch (self) {
            .let_decl => |l| l.span,
            .var_decl => |v| v.span,
            .assignment => |a| a.span,
            .expr_stmt => |e| e.span,
            .return_stmt => |r| r.span,
            .break_stmt => |b| b.span,
            .continue_stmt => |c| c.span,
            .for_loop => |f| f.span,
            .while_loop => |w| w.span,
            .loop_stmt => |l| l.span,
        };
    }
};

pub const LetDecl = struct {
    name: []const u8,
    type_: ?TypeExpr,
    value: Expr,
    span: Span,
};

pub const VarDecl = struct {
    name: []const u8,
    type_: ?TypeExpr,
    value: Expr,
    span: Span,
};

pub const Assignment = struct {
    target: Expr,
    op: BinaryOp,
    value: Expr,
    span: Span,
};

pub const ExprStmt = struct {
    expr: Expr,
    span: Span,
};

pub const ReturnStmt = struct {
    value: ?Expr,
    span: Span,
};

pub const BreakStmt = struct {
    value: ?Expr,
    span: Span,
};

pub const ContinueStmt = struct {
    span: Span,
};

pub const ForLoop = struct {
    pattern: Pattern,
    iterable: Expr,
    body: *Block,
    span: Span,
};

pub const WhileLoop = struct {
    condition: Expr,
    body: *Block,
    span: Span,
};

pub const LoopStmt = struct {
    body: *Block,
    span: Span,
};

// ============================================================================
// Declarations
// ============================================================================

pub const Decl = union(enum) {
    function: *FunctionDecl,
    struct_decl: *StructDecl,
    enum_decl: *EnumDecl,
    trait_decl: *TraitDecl,
    impl_decl: *ImplDecl,
    type_alias: *TypeAlias,
    const_decl: *ConstDecl,
    import_decl: *ImportDecl,
    module_decl: *ModuleDecl,

    pub fn span(self: Decl) Span {
        return switch (self) {
            .function => |f| f.span,
            .struct_decl => |s| s.span,
            .enum_decl => |e| e.span,
            .trait_decl => |t| t.span,
            .impl_decl => |i| i.span,
            .type_alias => |t| t.span,
            .const_decl => |c| c.span,
            .import_decl => |i| i.span,
            .module_decl => |m| m.span,
        };
    }
};

pub const FunctionDecl = struct {
    name: []const u8,
    type_params: []const TypeParam,
    params: []const FunctionParam,
    return_type: ?TypeExpr,
    where_clause: ?[]const WhereConstraint,
    body: ?*Block,
    is_pub: bool,
    is_async: bool,
    span: Span,
};

pub const FunctionParam = struct {
    name: []const u8,
    type_: TypeExpr,
    default_value: ?Expr,
    span: Span,
};

pub const TypeParam = struct {
    name: []const u8,
    bounds: []const TypeExpr,
    span: Span,
};

pub const WhereConstraint = struct {
    type_param: []const u8,
    bounds: []const TypeExpr,
    span: Span,
};

pub const StructDecl = struct {
    name: []const u8,
    type_params: []const TypeParam,
    fields: []const StructField,
    traits: []const TypeExpr,
    is_pub: bool,
    span: Span,
};

pub const StructField = struct {
    name: []const u8,
    type_: TypeExpr,
    is_pub: bool,
    span: Span,
};

pub const EnumDecl = struct {
    name: []const u8,
    type_params: []const TypeParam,
    variants: []const EnumVariant,
    is_pub: bool,
    span: Span,
};

pub const EnumVariant = struct {
    name: []const u8,
    payload: ?VariantPayload,
    span: Span,
};

pub const VariantPayload = union(enum) {
    tuple: []const TypeExpr,
    struct_: []const StructField,
};

pub const TraitDecl = struct {
    name: []const u8,
    type_params: []const TypeParam,
    methods: []const FunctionDecl,
    is_pub: bool,
    span: Span,
};

pub const ImplDecl = struct {
    type_params: []const TypeParam,
    target_type: TypeExpr,
    trait_type: ?TypeExpr,
    where_clause: ?[]const WhereConstraint,
    methods: []const FunctionDecl,
    span: Span,
};

pub const TypeAlias = struct {
    name: []const u8,
    type_params: []const TypeParam,
    target: TypeExpr,
    is_pub: bool,
    span: Span,
};

pub const ConstDecl = struct {
    name: []const u8,
    type_: ?TypeExpr,
    value: Expr,
    is_pub: bool,
    span: Span,
};

pub const ImportDecl = struct {
    path: []const []const u8,
    items: ?ImportItems,
    alias: ?[]const u8,
    span: Span,
};

pub const ImportItems = union(enum) {
    all, // import path.*
    specific: []const ImportItem,
};

pub const ImportItem = struct {
    name: []const u8,
    alias: ?[]const u8,
    span: Span,
};

pub const ModuleDecl = struct {
    path: []const []const u8,
    span: Span,
};

// ============================================================================
// Types
// ============================================================================

pub const TypeExpr = union(enum) {
    named: NamedType,
    array: *ArrayType,
    slice: *SliceType,
    tuple: *TupleType,
    optional: *OptionalType,
    result: *ResultType,
    function: *FunctionType,
    reference: *ReferenceType,
    generic_apply: *GenericApply,

    pub fn span(self: TypeExpr) Span {
        return switch (self) {
            .named => |n| n.span,
            .array => |a| a.span,
            .slice => |s| s.span,
            .tuple => |t| t.span,
            .optional => |o| o.span,
            .result => |r| r.span,
            .function => |f| f.span,
            .reference => |r| r.span,
            .generic_apply => |g| g.span,
        };
    }
};

pub const NamedType = struct {
    name: []const u8,
    span: Span,
};

pub const ArrayType = struct {
    element: TypeExpr,
    size: Expr,
    span: Span,
};

pub const SliceType = struct {
    element: TypeExpr,
    span: Span,
};

pub const TupleType = struct {
    elements: []const TypeExpr,
    span: Span,
};

pub const OptionalType = struct {
    inner: TypeExpr,
    span: Span,
};

pub const ResultType = struct {
    ok_type: TypeExpr,
    err_type: TypeExpr,
    span: Span,
};

pub const FunctionType = struct {
    params: []const TypeExpr,
    return_type: TypeExpr,
    span: Span,
};

pub const ReferenceType = struct {
    inner: TypeExpr,
    mutable: bool,
    span: Span,
};

pub const GenericApply = struct {
    base: TypeExpr,
    args: []const TypeExpr,
    span: Span,
};

// ============================================================================
// Top Level
// ============================================================================

pub const Module = struct {
    module_decl: ?ModuleDecl,
    imports: []const ImportDecl,
    declarations: []const Decl,
};

// ============================================================================
// Tests
// ============================================================================

test "Span merge" {
    const a = Span{ .start = 10, .end = 20, .line = 1, .column = 10 };
    const b = Span{ .start = 25, .end = 35, .line = 1, .column = 25 };
    const merged = Span.merge(a, b);

    try std.testing.expectEqual(@as(usize, 10), merged.start);
    try std.testing.expectEqual(@as(usize, 35), merged.end);
}

test "BinaryOp fromToken" {
    try std.testing.expectEqual(BinaryOp.add, BinaryOp.fromToken(.plus).?);
    try std.testing.expectEqual(BinaryOp.sub, BinaryOp.fromToken(.minus).?);
    try std.testing.expectEqual(BinaryOp.add_wrap, BinaryOp.fromToken(.plus_wrap).?);
    try std.testing.expect(BinaryOp.fromToken(.l_paren) == null);
}

test "UnaryOp fromToken" {
    try std.testing.expectEqual(UnaryOp.negate, UnaryOp.fromToken(.minus).?);
    try std.testing.expectEqual(UnaryOp.not, UnaryOp.fromToken(.not).?);
    try std.testing.expect(UnaryOp.fromToken(.plus) == null);
}
