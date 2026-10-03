//! Debug tracing filters chosen by the `--trace` diagnostics flags.

use super::*;

pub(crate) fn init_debug_tracing(diagnostics: &DiagnosticsArgs) -> Result<()> {
    let Some(filter) = trace_filter_from_diagnostics(diagnostics) else {
        return Ok(());
    };

    #[cfg(feature = "tracing")]
    {
        use tracing_subscriber::EnvFilter;
        let filter = EnvFilter::try_new(&filter)
            .map_err(|error| anyhow::anyhow!("invalid trace filter `{filter}`: {error}"))?;
        tracing_subscriber::fmt()
            .with_env_filter(filter)
            .with_target(true)
            .with_level(true)
            .init();
    }

    #[cfg(not(feature = "tracing"))]
    {
        let _ = filter;
        eprintln!("Warning: tracing flags require --features tracing");
        eprintln!("Rebuild with: cargo build --features tracing");
    }

    Ok(())
}

pub(super) fn trace_filter_from_diagnostics(diagnostics: &DiagnosticsArgs) -> Option<String> {
    let mut filters = Vec::new();
    // `--trace` present with no value (Some("")) selects the default phase
    // filter; `--trace=<FILTER>` supplies a custom filter (with short phase
    // aliases expanded to their `rumoca_phase_*` targets).
    match diagnostics.trace.as_deref() {
        Some(filter) if !filter.is_empty() => filters.push(expand_trace_filter(filter)),
        Some(_) => filters.push(DEFAULT_DEBUG_TRACE_FILTER.to_string()),
        None => {}
    }
    if diagnostics.trace_profile {
        filters.push(PROFILE_TRACE_FILTER.to_string());
    }
    if filters.is_empty() {
        None
    } else {
        Some(filters.join(","))
    }
}

/// Whether the `--trace` filter names the `viewer` subsystem (e.g.
/// `--trace=viewer`). The browser viewer is just another trace subsystem:
/// naming it enables the viewer's debug overlay/logging (this replaced the
/// separate `--viewer-debug` flag).
pub(super) fn trace_requests_viewer(diagnostics: &DiagnosticsArgs) -> bool {
    diagnostics.trace.as_deref().is_some_and(|spec| {
        spec.split(',').map(str::trim).any(|token| {
            let name = token.split_once(':').map_or(token, |(name, _)| name.trim());
            name == "viewer"
        })
    })
}

/// Short aliases accepted in a `--trace` filter, mapping to tracing targets so
/// `--trace=dae:debug` is shorthand for `--trace=rumoca_phase_dae=debug`. Covers
/// the compiler phases plus a few high-traffic runtime diagnostic targets.
const TRACE_PHASE_ALIASES: &[(&str, &str)] = &[
    ("parse", "rumoca_phase_parse"),
    ("resolve", "rumoca_phase_resolve"),
    ("instantiate", "rumoca_phase_instantiate"),
    ("typecheck", "rumoca_phase_typecheck"),
    ("flatten", "rumoca_phase_flatten"),
    ("dae", "rumoca_phase_dae"),
    ("structural", "rumoca_phase_structural"),
    ("solve", "rumoca_phase_solve"),
    ("codegen", "rumoca_phase_codegen"),
    // Runtime diagnostic shortcuts (see TRACE_LONG_HELP for the full target set).
    ("bdf", "rumoca_solver_diffsol::bdf"),
    ("rk45", "rumoca_solver_rk45::eval"),
    ("hotpath", "rumoca_solver::hotpath"),
    ("driver", "rumoca_solver::driver"),
];

/// Expand short phase aliases in a `--trace` filter. Each comma-separated token
/// of the form `<phase>` or `<phase>:<level>` (e.g. `dae:debug`) expands to
/// `rumoca_phase_<phase>=<level>` (default level `debug`); any token that is not
/// a known alias passes through unchanged, so full tracing EnvFilter directives
/// (e.g. `rumoca_phase_dae::profile=debug`) still work.
pub(super) fn expand_trace_filter(spec: &str) -> String {
    spec.split(',')
        .map(str::trim)
        .filter(|token| !token.is_empty())
        .map(|token| {
            let (name, level) = match token.split_once(':') {
                Some((name, level)) => (name.trim(), level.trim()),
                None => (token, "debug"),
            };
            match TRACE_PHASE_ALIASES.iter().find(|(alias, _)| *alias == name) {
                Some((_, target)) => format!("{target}={level}"),
                None => token.to_string(),
            }
        })
        .collect::<Vec<_>>()
        .join(",")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn diagnostics(trace: Option<&str>) -> DiagnosticsArgs {
        DiagnosticsArgs {
            verbose: false,
            trace: trace.map(str::to_string),
            trace_profile: false,
        }
    }

    #[test]
    fn the_viewer_subsystem_is_requested_by_name_with_or_without_a_level() {
        assert!(trace_requests_viewer(&diagnostics(Some("viewer"))));
        assert!(trace_requests_viewer(&diagnostics(Some(
            "dae:debug, viewer:trace"
        ))));
        assert!(!trace_requests_viewer(&diagnostics(Some("dae,viewers"))));
        assert!(!trace_requests_viewer(&diagnostics(Some(""))));
        assert!(!trace_requests_viewer(&diagnostics(None)));
    }
}
