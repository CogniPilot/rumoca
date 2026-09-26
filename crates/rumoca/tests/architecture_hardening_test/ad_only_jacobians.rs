//! Algorithmic differentiation is the only Jacobian source (SPEC_0043 AD-only
//! Jacobian row).
//!
//! Every Jacobian, tangent, and sensitivity the compiler, the linked kernel,
//! and the generated C form comes from a forward-mode JVP or its reverse; no
//! code differences a residual. This scan fails when a Rust source or a code
//! template under `crates/` names a difference quotient, a perturbation step,
//! or a finite difference. The only files that may are the AD verification
//! batteries listed in `VERIFICATION_BATTERIES`, which check the lowered
//! derivatives against differences taken independently (in this compiler's
//! primal and under OpenModelica), and the one projection test double that
//! predates the rule.

use std::fs;
use std::path::{Path, PathBuf};

use crate::architecture_hardening_support::workspace_root;

/// Files that compare AD against an independent difference, with the reason
/// each may name one. Paths are relative to the workspace root.
const VERIFICATION_BATTERIES: &[(&str, &str)] = &[
    (
        "crates/rumoca/tests/suite_core/jacobian_admission_battery.rs",
        "admission battery: every admitted AD pair against central differences",
    ),
    (
        "crates/rumoca/tests/suite_core/jacobian_finite_difference.rs",
        "synthesized Jacobians against central differences",
    ),
    (
        "crates/rumoca/tests/suite_core/jacobian_standard_modelica.rs",
        "the expanded battery against differences under OpenModelica",
    ),
    (
        "crates/rumoca/tests/suite_core/forward_param_jacobian_test.rs",
        "forward parameter Jacobians against differences",
    ),
    (
        "crates/rumoca/tests/suite_core/main.rs",
        "declares the batteries above",
    ),
    (
        "crates/rumoca-phase-autodiff/src/admission.rs",
        "the admission table the batteries are generated from",
    ),
    (
        "crates/rumoca-phase-autodiff/src/admission/rows.rs",
        "the admission table the batteries are generated from",
    ),
    (
        "crates/rumoca-phase-autodiff/src/tests/admission.rs",
        "the admission table the batteries are generated from",
    ),
    (
        "crates/rumoca-phase-autodiff/src/builtins.rs",
        "names which builtins the batteries can check",
    ),
    (
        "crates/rumoca-solver/src/runtime/projection/tests/order_robustness.rs",
        "a projection test double's residual JVP",
    ),
];

/// The names and phrases of a difference quotient, lowercase. Built from
/// parts so this scan does not name them itself.
fn needles() -> Vec<String> {
    let finite = ["finite", " ", "difference"].concat();
    vec![
        ["pert", "urbation"].concat(),
        ["fd", "_step"].concat(),
        finite.clone(),
        finite.replace(' ', "-"),
        finite.replace(' ', "_"),
        ["difference", " ", "quotient"].concat(),
        ["difference", "_", "quotient"].concat(),
        ["central", " ", "difference"].concat(),
        ["central", "_", "difference"].concat(),
        ["symmetric", " ", "difference"].concat(),
    ]
}

fn collect_sources(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = fs::read_dir(dir)
        .unwrap_or_else(|error| panic!("failed to read {}: {error}", dir.display()));
    for entry in entries {
        let path = entry.expect("directory entry").path();
        if path.is_dir() {
            if path.file_name().is_some_and(|name| name != "target") {
                collect_sources(&path, out);
            }
            continue;
        }
        if path
            .extension()
            .is_some_and(|extension| extension == "rs" || extension == "jinja")
        {
            out.push(path);
        }
    }
}

#[test]
fn no_source_differences_a_residual() {
    let root = workspace_root();
    let mut files = Vec::new();
    collect_sources(&root.join("crates"), &mut files);
    files.sort();
    let needles = needles();
    let this_file = Path::new(file!());
    let mut offenders = Vec::new();
    for path in files {
        let relative = path.strip_prefix(&root).unwrap_or(&path);
        let relative_text = relative.to_string_lossy().replace('\\', "/");
        if this_file.ends_with(relative)
            || VERIFICATION_BATTERIES
                .iter()
                .any(|(allowed, _)| relative_text == *allowed)
        {
            continue;
        }
        let source = fs::read_to_string(&path).expect("read a source for the quotient scan");
        for (line_index, line) in source.lines().enumerate() {
            let lower = line.to_ascii_lowercase();
            if let Some(needle) = needles
                .iter()
                .find(|needle| lower.contains(needle.as_str()))
            {
                offenders.push(format!(
                    "{relative_text}:{}: `{needle}` in `{}`",
                    line_index + 1,
                    line.trim()
                ));
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "Jacobians come from algorithmic differentiation alone; these lines name a \
         difference quotient:\n{}",
        offenders.join("\n")
    );
}

#[test]
fn every_verification_battery_still_names_its_difference() {
    let root = workspace_root();
    let needles = needles();
    for (path, reason) in VERIFICATION_BATTERIES {
        let source = fs::read_to_string(root.join(path))
            .unwrap_or_else(|error| panic!("{path} ({reason}) is readable: {error}"));
        let lower = source.to_ascii_lowercase();
        assert!(
            needles.iter().any(|needle| lower.contains(needle.as_str())),
            "{path} ({reason}) no longer names a difference; remove it from the list"
        );
    }
}
