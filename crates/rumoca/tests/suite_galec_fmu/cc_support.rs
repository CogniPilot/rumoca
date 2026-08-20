//! Shared C-compiler helper for the suites that compile-check generated C
//! (`cli_target_embedded_c_galec.rs`, `cli_target_galec_production.rs`).
//!
//! Declared once by `suite_galec_fmu/main.rs` as the normal `cc_support` module and
//! reached by sibling suites as `super::cc_support`.

use std::process::Command;

/// File name of the shared array-kernel library both C targets emit beside
/// every model unit.
///
/// The generated model source is NOT self-contained: its whole-array moves,
/// fills and inner products are calls into this library, which is
/// model-independent and byte-identical for every block one compiler build
/// emits. Every link of a generated model unit — here and in an integrator's
/// build — must include it, so the name is spelled once.
pub(super) const GALEC_KERNEL_LIBRARY: &str = "rumoca_galec_kernels.c";

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

/// Strict, portable C99 preflight used for every generated GALEC C product.
///
/// These warnings catch useful coding-profile defects, but are deliberately
/// not called a MISRA compliance check (SPEC_0034 GAL-029/GAL-031).
pub(super) fn assurance_c99_cc() -> Command {
    let mut command = cc();
    command.args([
        "-std=c99",
        "-pedantic",
        "-Wall",
        "-Wextra",
        "-Wconversion",
        "-Wsign-conversion",
        "-Wshadow",
        "-Wundef",
        "-Wcast-qual",
        "-Wstrict-prototypes",
        "-Wmissing-prototypes",
        "-Werror",
    ]);
    command
}
