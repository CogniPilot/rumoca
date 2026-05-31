//! Trace-target gates for the DAE evaluator's debug diagnostics. These replace
//! the former `RUMOCA_SIM_*` environment variables: the diagnostics now activate
//! when the matching `--trace` target is enabled (in a tracing-enabled build),
//! e.g. `rumoca sim model.mo --trace=rumoca_eval_dae::sim`.

/// `rumoca_eval_dae::sim` — general DAE simulation evaluation tracing.
pub(crate) fn sim_enabled() -> bool {
    tracing::enabled!(target: "rumoca_eval_dae::sim", tracing::Level::DEBUG)
}

/// `rumoca_eval_dae::introspect` — fine-grained value introspection (pre-seed,
/// algorithm assignments).
pub(crate) fn introspect_enabled() -> bool {
    tracing::enabled!(target: "rumoca_eval_dae::introspect", tracing::Level::DEBUG)
}

/// Either of the two general DAE-eval trace targets (several diagnostics fire
/// for both).
pub(crate) fn sim_or_introspect_enabled() -> bool {
    sim_enabled() || introspect_enabled()
}

/// `rumoca_eval_dae::function_inputs` — log function-call input values.
pub(crate) fn function_inputs_enabled() -> bool {
    tracing::enabled!(target: "rumoca_eval_dae::function_inputs", tracing::Level::DEBUG)
}

/// `rumoca_eval_dae::function_match` — trace user/library function calls. (The
/// former env var took a comma-separated name filter; the trace target is a
/// boolean, so enabling it traces all matched calls.)
pub(crate) fn function_match_enabled() -> bool {
    tracing::enabled!(target: "rumoca_eval_dae::function_match", tracing::Level::DEBUG)
}
