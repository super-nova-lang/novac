use std::time::{SystemTime, UNIX_EPOCH};

/// Very small logging helpers to centralize formatting for source-aware errors
/// and simple progress/info/warning messages.

pub fn init() {
    // Placeholder for future initialization (tracing subscriber, RUST_LOG, etc.)
    // Print a subtle hint to indicate logging is active.
}

pub fn error_span(file: &str, line: usize, column: usize, kind: &str, msg: &str) {
    eprintln!("{}:{}:{} [error.{}] {}", file, line, column, kind, msg);
}

pub fn error_simple(file: &str, kind: &str, msg: &str) {
    eprintln!("{}:0:0 [error.{}] {}", file, kind, msg);
}

pub fn warn_simple(file: &str, kind: &str, msg: &str) {
    eprintln!("{}:0:0 [warning.{}] {}", file, kind, msg);
}

pub fn info(msg: &str) {
    println!("{}", msg);
}

pub fn progress(msg: &str) {
    println!("[progress] {}", msg);
}

/// Helper to emit a small build time hint (prints a human-friendly ms value).
pub fn build_time_hint(ms: u128) {
    println!("[hint] build time: {} ms", ms);
}
