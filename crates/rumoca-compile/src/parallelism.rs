//! Shared worker sizing policy for compiler-owned parallel stages.

use std::sync::OnceLock;

/// Logical CPUs reserved for the OS, desktop, and foreground tooling on larger
/// hosts.
pub const DEFAULT_RESERVED_CPUS: usize = 4;

/// Process-wide override for the compiler worker-thread count, set once from the
/// CLI (`--jobs`) instead of an environment variable. Spawned single-purpose
/// workers pass `--jobs 1` because the parent already parallelizes across them.
static PARALLELISM_OVERRIDE: OnceLock<usize> = OnceLock::new();

/// Set the compiler parallelism for this process. Call once at startup, before
/// any parallel compile stage runs. A zero or repeated value is ignored.
pub fn set_compiler_parallelism(threads: usize) {
    if threads > 0 {
        let _ = PARALLELISM_OVERRIDE.set(threads);
    }
}

/// Default worker count for a host with `auto_threads` logical CPUs.
///
/// Small CI runners keep all available cores. Larger developer machines reserve
/// fixed headroom so MSL gates and compile-heavy workflows do not monopolize the
/// system.
pub fn default_worker_threads_for(auto_threads: usize) -> usize {
    if auto_threads <= DEFAULT_RESERVED_CPUS {
        auto_threads.max(1)
    } else {
        auto_threads.saturating_sub(DEFAULT_RESERVED_CPUS).max(1)
    }
}

/// Default worker count for the current host.
pub fn default_worker_threads() -> usize {
    if let Some(threads) = compiler_parallelism_override() {
        return threads;
    }
    std::thread::available_parallelism()
        .map(|threads| default_worker_threads_for(threads.get()))
        .unwrap_or(1)
}

fn compiler_parallelism_override() -> Option<usize> {
    PARALLELISM_OVERRIDE.get().copied()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_worker_threads_uses_all_cores_on_small_hosts() {
        assert_eq!(default_worker_threads_for(1), 1);
        assert_eq!(default_worker_threads_for(2), 2);
        assert_eq!(default_worker_threads_for(4), 4);
    }

    #[test]
    fn default_worker_threads_reserves_headroom_on_large_hosts() {
        assert_eq!(default_worker_threads_for(8), 4);
        assert_eq!(default_worker_threads_for(32), 28);
    }
}
