//! Diffsol selection of the shared FMI 3 Model Exchange runtime.
//!
//! This module is intentionally only interface wiring. Lifecycle, event, and
//! model semantics live in `rumoca-solver::fmi_me::MeRuntimeHost`.

use rumoca_solver::{
    SimOptions,
    fmi_me::{MeInstanceConfig, MeModelSource, MeNumericsProfile, MeRootProfile, MeRuntimeHost},
};

use crate::SimError;

pub(crate) use rumoca_solver::fmi_me::{
    MeRuntimeInitialState as MeInitialState, MeRuntimePostEventState as MePostEventState,
};
pub(crate) type DiffsolMeHost = MeRuntimeHost;

pub(crate) fn instantiate(
    source: MeModelSource<'_>,
    opts: &SimOptions,
) -> Result<DiffsolMeHost, SimError> {
    let config = MeInstanceConfig {
        instance_name: "bdf",
        tolerance: opts.atol.max(1.0e-10),
        start_time: opts.t_start,
        stop_time: opts.t_end,
        root_profile: MeRootProfile::DiffsolFrozen,
        numerics_profile: MeNumericsProfile::DiffsolFrozen,
    };
    MeRuntimeHost::instantiate(source, &config).map_err(Into::into)
}
