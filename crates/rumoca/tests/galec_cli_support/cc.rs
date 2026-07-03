//! Shared C-compiler helper for the suites that compile-check generated C
//! (`cli_target_embedded_c_galec.rs`, `cli_target_galec_production.rs`).
//!
//! Included per suite via `#[path = "galec_cli_support/cc.rs"]` — see
//! `galec_cli_support/cli.rs` for the include-pattern rationale.

use std::process::Command;

/// A missing C compiler is a hard failure (GAL-012: the compile check is
/// mandatory and never silently skipped), exactly like the galec suites'
/// xmllint requirement.
pub(super) fn cc() -> Command {
    let probe = Command::new("cc").arg("--version").output();
    assert!(
        probe.is_ok_and(|output| output.status.success()),
        "`cc` must be installed: the generated-C compile check is a hard \
         CI dependency and never skips (SPEC_0034 GAL-012/GAL-024)"
    );
    Command::new("cc")
}
