/// Internal async Future state tag mapping shared by runtime and codegen.
/// This is not a stable public ABI.
pub const pending_tag: u8 = 0;
pub const completed_tag: u8 = 1;
pub const failed_tag: u8 = 2;
pub const cancelled_tag: u8 = 3;
