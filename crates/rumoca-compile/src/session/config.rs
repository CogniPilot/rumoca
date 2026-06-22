use rumoca_phase_instantiate::{DEFAULT_INSTANTIATION_DEPTH_LIMIT, InstantiateOptions};
use std::sync::Once;

static RAYON_INIT: Once = Once::new();

/// Configuration for a compilation session.
#[derive(Debug, Clone)]
pub struct SessionConfig {
    /// Enable parallel compilation.
    pub parallel: bool,
    /// Strictness toggle for ER070 Evaluate-scope diagnostics.
    pub evaluate_scope_is_error: bool,
    /// Strictness toggle for ER053 when single-assignment diagnostics.
    pub when_single_assign_is_error: bool,
    /// Maximum concrete class/component nesting allowed during instantiation.
    pub instantiation_depth_limit: usize,
}

impl Default for SessionConfig {
    fn default() -> Self {
        Self {
            parallel: false,
            evaluate_scope_is_error: true,
            when_single_assign_is_error: true,
            instantiation_depth_limit: DEFAULT_INSTANTIATION_DEPTH_LIMIT,
        }
    }
}

impl SessionConfig {
    pub(super) fn instantiate_options(&self) -> InstantiateOptions {
        InstantiateOptions {
            depth_limit: self.instantiation_depth_limit,
            ..InstantiateOptions::default()
        }
    }
}

/// Initialize rayon thread pool with shared compiler worker sizing and 16MB stack per thread.
///
/// The large stack size is needed for deep MSL class hierarchies.
pub(super) fn init_rayon_pool() {
    RAYON_INIT.call_once(|| {
        let num_threads = crate::parallelism::default_worker_threads();
        rayon::ThreadPoolBuilder::new()
            .num_threads(num_threads)
            .stack_size(16 * 1024 * 1024) // 16 MB per thread for deep class hierarchies
            .build_global()
            .ok(); // Ignore error if pool already initialized
    });
}
