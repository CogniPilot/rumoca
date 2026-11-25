//! The `rust-fixed-ode` event runtime locates state events with the same
//! bracket arithmetic and location plan as every other executor.

use rumoca_ir_solve::fmi::RootLocationPlan;
use rumoca_phase_codegen::templates::builtin_target;

fn event_runtime() -> &'static str {
    builtin_target("rust-fixed-ode")
        .and_then(|target| target.template_source("events.rs"))
        .expect("rust-fixed-ode bundles its event runtime")
}

/// The runtime carries `RootBracket` verbatim; only `open` is public there,
/// because the generated module may be a crate root.
#[test]
fn the_event_runtime_carries_the_shared_root_bracket_verbatim() {
    let shared = include_str!("../../rumoca-ir-solve/src/fmi/root_location/bracket.rs");
    let start = shared
        .find("/// Which end of a bracket")
        .expect("bracket.rs opens its items with BracketEnd");
    let end = shared
        .find("#[cfg(test)]")
        .expect("bracket.rs ends its items before its tests");
    let items = shared[start..end]
        .trim_end()
        .replace("    pub(super) fn open(", "    pub fn open(");
    assert!(
        event_runtime().contains(&items),
        "events.rs must carry bracket.rs's items verbatim"
    );
}

/// The runtime's location constants are the standard plan's.
#[test]
fn the_event_runtime_reads_the_standard_location_plan() {
    let plan = RootLocationPlan::STANDARD;
    let runtime = event_runtime();
    assert!(runtime.contains(&format!(
        "const MINMAX_SLACK: u32 = {};",
        plan.minmax_slack()
    )));
    assert!(runtime.contains(&format!(
        "const REFINEMENT_CAP: usize = {};",
        plan.refinement_iteration_cap()
    )));
    assert!(runtime.contains("const LOCATION_EPSILONS: f64 = 100.0;"));
    assert!(runtime.contains(&format!(
        "const SLIDING_STREAK: u32 = {};",
        plan.sliding_streak()
    )));
    assert!(runtime.contains("const SLIDING_GAP: f64 = 1.0e-5 * 1.0e-3;"));
    assert_eq!(plan.sliding_gap(plan.scan_resolution(0.0)), 1.0e-5 * 1.0e-3);
    assert_eq!(plan.interval_roundoff(1.0, 0.0), 100.0 * f64::EPSILON);
}
