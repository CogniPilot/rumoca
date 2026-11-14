//! The default (browser) product's bytes, pinned as the unchanged emitters
//! printed them. A saved product is bound by re-emitting and comparing bytes, so
//! no byte of this profile may move when the linked profile is added.
use super::fixtures;
use crate::{
    compile_exact_assignment_schedule_wasm, compile_fmi_derivative_operation_wasm,
    compile_issued_assignment_rows_wasm, compile_private_programs_wasm,
};

fn fnv1a(bytes: &[u8]) -> u64 {
    bytes.iter().fold(0xcbf2_9ce4_8422_2325, |hash, byte| {
        (hash ^ u64::from(*byte)).wrapping_mul(0x0100_0000_01b3)
    })
}

/// Every module of every fixture, labelled, in the default profile.
pub(super) fn default_modules() -> Vec<(String, Vec<u8>)> {
    let mut modules = Vec::new();
    for fixture in [fixtures::CAUSAL, fixtures::MIXED] {
        let component = fixtures::component(fixture);
        let root = component.runtime_view().retain_assertion_root();
        let operation = root
            .try_derivative_operation()
            .unwrap()
            .try_causal_operation()
            .unwrap();
        let code = compile_fmi_derivative_operation_wasm(&operation).unwrap();
        let name = fixture.0;
        for (role, part) in [
            ("static_refresh", code.static_program()),
            ("dynamic_refresh", code.dynamic_program()),
            ("values", Some(code.values())),
            ("kinematic_values", code.kinematic()),
        ] {
            if let Some(part) = part {
                modules.push((format!("{name}.{role}"), part.module_bytes().to_vec()));
            }
        }
        modules.push((format!("{name}.control"), code.module_bytes().to_vec()));
    }
    let model = fixtures::exact_model();
    let exact = compile_exact_assignment_schedule_wasm(
        &model.problem.continuous.implicit_rhs,
        &model.problem.continuous.refresh_owners,
        fixtures::exact_schedule(&model),
        &model.problem.layout,
        &model.pure_calls,
    )
    .unwrap();
    modules.push(("ExactWire.exact".into(), exact.module_bytes().to_vec()));
    let issued = compile_issued_assignment_rows_wasm(
        &fixtures::issued_rows(),
        &fixtures::issued_layout(),
        &fixtures::empty_calls(),
    )
    .unwrap();
    modules.push(("rows.issued".into(), issued.module_bytes().to_vec()));
    let blocks = fixtures::private_blocks();
    let requests = blocks.iter().map(|block| (block, None)).collect::<Vec<_>>();
    let private = compile_private_programs_wasm(
        &requests,
        &fixtures::private_layout(),
        &fixtures::empty_calls(),
    )
    .unwrap();
    assert!(
        private
            .windows(2)
            .all(|pair| pair[0].module_bytes() == pair[1].module_bytes()),
        "the fixture's private entries share one module"
    );
    modules.push(("private.batch".into(), private[0].module_bytes().to_vec()));
    let event = fixtures::event_program();
    let layout = event.source().model().problem.layout.clone();
    let (event, _, _) =
        crate::emit::emit_event_program_module(&event, &layout, crate::emit::Linkage::Browser)
            .unwrap();
    modules.push(("event.observation".into(), event.module_bytes().to_vec()));
    modules
}

/// Recorded from the emitters of the base before the linked profile existed.
const PINNED: &[(&str, usize, u64)] = &[
    ("CausalEntry.static_refresh", 383, 0x02750e2784f6445b),
    ("CausalEntry.dynamic_refresh", 379, 0xf0978aa6c09a1906),
    ("CausalEntry.values", 436, 0x398bacc122627155),
    ("CausalEntry.control", 1370, 0x284ad02c47e4022a),
    ("Mixed.dynamic_refresh", 379, 0x52edee9f8211568f),
    ("Mixed.values", 441, 0xbb7f585dfa0bac00),
    ("Mixed.kinematic_values", 398, 0xdbd7a7e30e2fc0b8),
    ("Mixed.control", 1158, 0x1e3869fc3dc6021b),
    ("ExactWire.exact", 383, 0xee054d544dc52e25),
    ("rows.issued", 451, 0x24e06e10d98a125e),
    ("private.batch", 1112, 0x388192b03dd6b637),
    ("event.observation", 1006, 0x2309ef69d75a7b6d),
];

#[test]
fn the_default_profile_prints_the_pinned_bytes_of_every_fixture_module() {
    let actual = default_modules()
        .into_iter()
        .map(|(label, bytes)| (label, bytes.len(), fnv1a(&bytes)))
        .collect::<Vec<_>>();
    let printed = actual
        .iter()
        .map(|(label, len, hash)| format!("    (\"{label}\", {len}, 0x{hash:016x}),"))
        .collect::<Vec<_>>()
        .join("\n");
    let pinned = PINNED
        .iter()
        .map(|(label, len, hash)| (label.to_string(), *len, *hash))
        .collect::<Vec<_>>();
    assert_eq!(actual, pinned, "actual default modules:\n{printed}");
}
