//! Diffsol selection of the shared FMI 3 Model Exchange runtime.
//!
//! This module is intentionally only interface wiring. Lifecycle, event, and
//! model semantics live in `rumoca-solver::fmi_me::MeRuntimeHost`.

use rumoca_solver::{
    SimOptions,
    fmi_me::{
        MeExecutionBackend, MeInstanceConfig, MeModelSource, MeNumericsProfile, MeRootProfile,
        MeRuntimeHost,
    },
};

use crate::SimError;

#[cfg(test)]
pub(crate) use rumoca_solver::fmi_me::{
    MeRuntimeInitialState as MeInitialState, MeRuntimePostEventState as MePostEventState,
};
pub(crate) type DiffsolMeHost = MeRuntimeHost;

pub(crate) fn instantiate(
    source: MeModelSource<'_>,
    opts: &SimOptions,
) -> Result<DiffsolMeHost, SimError> {
    instantiate_with_execution_backend(source, opts, None)
}

/// Instantiate the shared ME runtime with a host-supplied compiled execution
/// backend.
///
/// The backend stays the opaque [`MeExecutionBackend`] handle end to end: this
/// crate can only pass it on to the generic ME host, which unwraps it inside
/// the `rumoca-solver` contract boundary (SPEC_0038 §Internal Solver
/// Boundary).
pub(crate) fn instantiate_with_execution_backend(
    source: MeModelSource<'_>,
    opts: &SimOptions,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<DiffsolMeHost, SimError> {
    let config = MeInstanceConfig {
        instance_name: "bdf",
        tolerance: opts.atol.max(1.0e-10),
        start_time: opts.t_start,
        stop_time: opts.t_end,
        root_profile: MeRootProfile::DiffsolFrozen,
        numerics_profile: MeNumericsProfile::DiffsolFrozen,
    };
    MeRuntimeHost::instantiate_with_execution_backend(source, &config, execution_backend)
        .map_err(Into::into)
}
