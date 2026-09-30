//! Process global allocator for Rumoca executables.
//!
//! Every Rumoca binary installs [`ProcessAllocator`] as its
//! `#[global_allocator]` (SPEC_0041 §6). It is mimalloc with one fixed
//! startup configuration that bounds the address space the process reserves
//! but never uses, so a run under an address-space limit (`ulimit -v`,
//! `RLIMIT_AS`) is limited by the memory the simulation needs, not by the host's
//! core count.
//!
//! On Linux, where the kernel charges reserved mappings against `RLIMIT_AS`,
//! the configuration is applied inside the allocator before its first
//! allocation. The Rust runtime allocates before `main` and before any thread
//! exists, so the configuration precedes every mimalloc arena reservation and
//! every glibc arena, with no dependence on constructor or call ordering. The
//! constants are fixed: they do not depend on environment variables (the
//! explicit option set overrides `MIMALLOC_*`) or on the core count. A
//! configuration that cannot be applied aborts the process with a typed
//! diagnostic; there is no fallback to the defaults.
//!
//! On other platforms reserved address space is not charged against a limit,
//! and the allocator is plain mimalloc with its defaults.

use std::alloc::{GlobalAlloc, Layout};
use std::fmt;

use mimalloc::MiMalloc;

/// Size of each mimalloc arena reservation, in KiB (64 MiB).
///
/// mimalloc's default is 1 GiB, reserved by the first allocation of the
/// process. 64 MiB is the smallest reservation mimalloc makes for an ordinary
/// page request anyway (the request is padded by one 32 MiB chunk and rounded
/// to 32 MiB), so a smaller value would not shrink the first arena. Later
/// arenas keep mimalloc's geometric growth (the size doubles every eight
/// arenas), so large heaps do not fragment into many small arenas.
pub const MIMALLOC_ARENA_RESERVE_KIB: usize = 64 * 1024;

/// Upper bound on the number of glibc malloc arenas.
///
/// Rust heap allocations go to mimalloc; glibc malloc is reached only by libc
/// and standard-library internals, notably `pthread_getattr_np` in the start
/// of every spawned thread. With glibc's default bound (eight per core) each
/// new thread creates its own arena and reserves a 64 MiB heap for a few
/// bytes, so a thread pool reserves 64 MiB per worker. One arena serves those
/// internal allocations for all threads.
pub const GLIBC_MALLOC_ARENA_MAX: i32 = 1;

/// The global allocator of every Rumoca executable: mimalloc with the fixed
/// startup configuration of [`MIMALLOC_ARENA_RESERVE_KIB`] and
/// [`GLIBC_MALLOC_ARENA_MAX`].
///
/// ```
/// #[global_allocator]
/// static GLOBAL: rumoca_allocator::ProcessAllocator = rumoca_allocator::ProcessAllocator;
///
/// fn main() {
///     let state = vec![0.0_f64; 400];
///     assert_eq!(state.len(), 400);
/// }
/// ```
pub struct ProcessAllocator;

/// A startup allocator configuration that the platform did not accept.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessAllocatorError {
    /// mimalloc did not report [`MIMALLOC_ARENA_RESERVE_KIB`] after it was set.
    ArenaReserveNotApplied,
    /// glibc `mallopt(M_ARENA_MAX, GLIBC_MALLOC_ARENA_MAX)` reported failure.
    MallocArenaMaxRejected,
}

impl ProcessAllocatorError {
    /// Complete diagnostic line, static so it can be written without allocating.
    pub const fn diagnostic(self) -> &'static str {
        match self {
            Self::ArenaReserveNotApplied => {
                "rumoca: process allocator configuration failed: \
mimalloc did not apply the fixed arena reserve\n"
            }
            Self::MallocArenaMaxRejected => {
                "rumoca: process allocator configuration failed: \
glibc rejected mallopt(M_ARENA_MAX)\n"
            }
        }
    }
}

impl fmt::Display for ProcessAllocatorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.diagnostic().trim_end())
    }
}

impl std::error::Error for ProcessAllocatorError {}

// SAFETY: every method forwards to `MiMalloc` with the caller's arguments
// unchanged, so the `GlobalAlloc` contract is `MiMalloc`'s. The startup
// configuration runs before the first allocation and does not allocate.
unsafe impl GlobalAlloc for ProcessAllocator {
    #[inline]
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        #[cfg(target_os = "linux")]
        startup::ensure_configured();
        // SAFETY: forwarded unchanged; the caller upholds `alloc`'s contract.
        unsafe { MiMalloc.alloc(layout) }
    }

    #[inline]
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        #[cfg(target_os = "linux")]
        startup::ensure_configured();
        // SAFETY: forwarded unchanged; the caller upholds `alloc_zeroed`'s contract.
        unsafe { MiMalloc.alloc_zeroed(layout) }
    }

    // `dealloc` and `realloc` receive memory an earlier `alloc` or
    // `alloc_zeroed` returned, so the configuration has already run.
    #[inline]
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        // SAFETY: forwarded unchanged; the caller upholds `dealloc`'s contract.
        unsafe { MiMalloc.dealloc(ptr, layout) }
    }

    #[inline]
    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        // SAFETY: forwarded unchanged; the caller upholds `realloc`'s contract.
        unsafe { MiMalloc.realloc(ptr, layout, new_size) }
    }
}

#[cfg(target_os = "linux")]
mod startup {
    use std::sync::Once;

    use libmimalloc_sys::{mi_option_get, mi_option_set, mi_option_t};

    use super::{MIMALLOC_ARENA_RESERVE_KIB, ProcessAllocatorError};

    /// Position of `mi_option_arena_reserve` in mimalloc v3's `mi_option_t`
    /// (`mimalloc.h`); `libmimalloc-sys` does not name it.
    pub(crate) const MI_OPTION_ARENA_RESERVE: mi_option_t = 23;

    /// Length of mimalloc v3's `mi_option_t`, the enum the index above was
    /// read from. A mimalloc with a different option list fails to build here
    /// instead of setting an unrelated option.
    const MI_OPTION_COUNT: mi_option_t = 47;
    const _: () = assert!(libmimalloc_sys::_mi_option_last == MI_OPTION_COUNT);

    static CONFIGURED: Once = Once::new();

    /// Applies the configuration exactly once; aborts with the typed
    /// diagnostic if the platform rejects it. Every allocation pays one
    /// atomic load; the one-time work stays out of line so the allocation
    /// path keeps mimalloc's size.
    #[inline(always)]
    pub(super) fn ensure_configured() {
        if !CONFIGURED.is_completed() {
            configure_once();
        }
    }

    #[cold]
    #[inline(never)]
    fn configure_once() {
        CONFIGURED.call_once(|| {
            if let Err(error) = configure() {
                abort_with(error);
            }
        });
    }

    /// Sets both fixed options and verifies that each took effect.
    pub(crate) fn configure() -> Result<(), ProcessAllocatorError> {
        let reserve_kib = std::ffi::c_long::try_from(MIMALLOC_ARENA_RESERVE_KIB)
            .map_err(|_| ProcessAllocatorError::ArenaReserveNotApplied)?;
        // SAFETY: option setters take plain integers and do not allocate; this
        // runs before any other thread exists (first allocation of the process)
        // or under `Once`, which serializes it.
        let applied = unsafe {
            mi_option_set(MI_OPTION_ARENA_RESERVE, reserve_kib);
            mi_option_get(MI_OPTION_ARENA_RESERVE)
        };
        if applied != reserve_kib {
            return Err(ProcessAllocatorError::ArenaReserveNotApplied);
        }
        bound_glibc_arenas()
    }

    #[cfg(target_env = "gnu")]
    fn bound_glibc_arenas() -> Result<(), ProcessAllocatorError> {
        // SAFETY: `mallopt` takes plain integers; glibc serializes it internally.
        let accepted = unsafe { libc::mallopt(libc::M_ARENA_MAX, super::GLIBC_MALLOC_ARENA_MAX) };
        if accepted == 1 {
            Ok(())
        } else {
            Err(ProcessAllocatorError::MallocArenaMaxRejected)
        }
    }

    /// Only glibc keeps per-thread malloc arenas.
    #[cfg(not(target_env = "gnu"))]
    fn bound_glibc_arenas() -> Result<(), ProcessAllocatorError> {
        Ok(())
    }

    fn abort_with(error: ProcessAllocatorError) -> ! {
        let diagnostic = error.diagnostic();
        // SAFETY: writes a static buffer to stderr without allocating. The
        // result is not inspected: the process aborts either way.
        unsafe {
            libc::write(
                libc::STDERR_FILENO,
                diagnostic.as_ptr().cast(),
                diagnostic.len(),
            );
        }
        std::process::abort()
    }
}

#[cfg(all(test, target_os = "linux"))]
mod tests {
    use libmimalloc_sys::mi_option_get;

    use super::MIMALLOC_ARENA_RESERVE_KIB;
    use super::startup::{MI_OPTION_ARENA_RESERVE, configure};

    /// mimalloc v3's built-in 64-bit arena reserve, in KiB (`options.c`).
    const MIMALLOC_DEFAULT_ARENA_RESERVE_KIB: std::ffi::c_long = 1024 * 1024;

    /// The test harness does not install `ProcessAllocator`, so mimalloc here
    /// still has its defaults: the index must name the 1 GiB arena reserve
    /// before `configure` and the fixed reserve after it.
    #[cfg(target_pointer_width = "64")]
    #[test]
    fn the_arena_reserve_index_names_mimallocs_arena_reserve_and_configure_sets_it() {
        assert!(
            std::env::var_os("MIMALLOC_ARENA_RESERVE").is_none(),
            "run without MIMALLOC_ARENA_RESERVE so mimalloc's built-in default is observable"
        );
        // SAFETY: reads an option; no other test in this crate touches options.
        let default = unsafe { mi_option_get(MI_OPTION_ARENA_RESERVE) };
        assert_eq!(default, MIMALLOC_DEFAULT_ARENA_RESERVE_KIB);

        configure().expect("the fixed allocator configuration applies on Linux");

        // SAFETY: as above.
        let applied = unsafe { mi_option_get(MI_OPTION_ARENA_RESERVE) };
        assert_eq!(applied, MIMALLOC_ARENA_RESERVE_KIB as std::ffi::c_long);
    }
}
