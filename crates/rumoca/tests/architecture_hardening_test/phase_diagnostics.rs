//! Architecture checks for phase diagnostics and debug output.

use super::{collect_rs_files, find_banned_source_lines, workspace_root};
use std::fs;

#[test]
fn test_phase_debug_output_uses_tracing_not_env_stderr() {
    let root = workspace_root();
    let checks: &[(&str, &[&str])] = &[
        (
            "crates/rumoca-phase-structural/src",
            &["RUMOCA_SIM_TRACE", "RUMOCA_SIM_INTROSPECT", "eprintln!"],
        ),
        (
            "crates/rumoca-phase-dae/src",
            &[
                "eprintln!",
                "RUMOCA_DEBUG_TODAE",
                "RUMOCA_DEBUG_EQ_FILTER",
                "RUMOCA_TODAE_PROFILE",
                "RUMOCA_DEBUG_FM_CANON",
                "RUMOCA_DAE_CLOCK_DEBUG",
            ],
        ),
        (
            "crates/rumoca-phase-instantiate/src",
            &["eprintln!", "RUMOCA_DEBUG_CONNECTION_PARAMS"],
        ),
    ];

    let mut offenders = Vec::new();
    for (src, banned) in checks {
        let mut rs_files = Vec::new();
        collect_rs_files(&root.join(src), &mut rs_files);
        for path in rs_files {
            let content = fs::read_to_string(&path).expect("read phase source");
            offenders.extend(find_banned_source_lines(
                &path, &content, banned, "contains",
            ));
        }
    }

    assert!(
        offenders.is_empty(),
        "phase debug output must use the tracing feature instead of \
stderr writes or phase-level debug environment variables: {offenders:?}"
    );
}

#[test]
fn test_phase_typecheck_errors_go_through_phase_error_type() {
    let root = workspace_root();
    let typecheck_src = root.join("crates/rumoca-phase-typecheck/src");
    let allowed_error_module = typecheck_src.join("lib.rs");
    let mut rs_files = Vec::new();
    collect_rs_files(&typecheck_src, &mut rs_files);

    let mut offenders = Vec::new();
    for path in rs_files {
        if path == allowed_error_module {
            continue;
        }
        let content = fs::read_to_string(&path).expect("read phase-typecheck source");
        offenders.extend(find_banned_source_lines(
            &path,
            &content,
            &[
                "CommonDiagnostic::error(",
                "rumoca_core::Diagnostic::error(",
            ],
            "constructs",
        ));
    }

    assert!(
        offenders.is_empty(),
        "phase-typecheck fatal diagnostics must go through TypeCheckError/PhaseError \
instead of constructing CommonDiagnostic::error in helper modules: {offenders:?}"
    );
}
