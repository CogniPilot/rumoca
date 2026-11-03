//! The total-tangent consumer runs exactly the issued occurrences, in issued
//! order: the template iterates them and keeps no order of its own.

use super::codegen_test_support::builtin_template;

const PARTIALS: [(&str, &str); 7] = [
    ("fmi-c-kernel.jinja", "scalar_kernel.jinja"),
    ("fmi-me-projection.jinja", "me_projection.jinja"),
    ("fmi-typed-functions.jinja", "typed_functions.jinja"),
    ("fmi-typed-arithmetic.jinja", "typed_arithmetic.jinja"),
    ("fmi-typed-tensors.jinja", "typed_tensors.jinja"),
    ("fmi-scalar-events.jinja", "scalar_events.jinja"),
    ("fmi-total-tangent.jinja", "total_tangent.jinja"),
];

/// The printed marker of each occurrence kind.
fn marker(kind: &str) -> &'static str {
    match kind {
        "implicit_solve" => "rmc_implicit_tangent(m, seed",
        "assignment" => "seed[7] = scratch[0];",
        "implicit_verification" => "&implicit_check) != 0",
        "consistency" => "&source_check) != 0",
        other => panic!("no marker for {other}"),
    }
}

fn render(occurrences: &[&str]) -> String {
    let mut environment = super::create_environment();
    for (name, file) in PARTIALS {
        environment
            .add_template(name, builtin_template("fmi3", file))
            .expect("fmi3 partial");
    }
    let issued: Vec<_> = occurrences
        .iter()
        .map(|kind| match *kind {
            "implicit_solve" => serde_json::json!({ "kind": kind, "unknowns": 2 }),
            "assignment" => serde_json::json!({ "kind": kind, "segment": 0, "targets": 2 }),
            "implicit_verification" => serde_json::json!({ "kind": kind, "unknowns": 2 }),
            "consistency" => serde_json::json!({ "kind": kind, "rows": 1 }),
            _ => serde_json::json!({ "kind": kind }),
        })
        .collect();
    let tangent = serde_json::json!({
        "available": true,
        "implicit": {
            "present": false, "n": 2, "rows": [4, 5], "subtract_targets": [0, 1],
            "tolerance": 1e-9, "plan": { "programs": [] }, "matrix_len": 4
        },
        "consistency": {
            "present": false, "rows": [6], "tolerance": 1e-8, "plan": { "programs": [] }
        },
        "assignments": [{ "targets": [7, 8], "plan": { "programs": [] } }],
        "rhs": { "programs": [] },
        "seed_len": 9, "time_seed": 8, "scratch_len": 3, "y_len": 6, "p_len": 2, "state_len": 2,
        "uses": { "seed": true, "states": true, "scratch": true, "private": true, "pivots": true },
        "occurrences": issued,
    });
    environment
        .render_str(
            "{% from 'fmi-total-tangent.jinja' import functions %}{{ functions(tangent, 'rmc') }}",
            minijinja::context! { tangent => tangent },
        )
        .expect("total tangent renders")
}

/// Positions of each occurrence's marker in the printed consumer.
fn printed_order(text: &str, occurrences: &[&str]) -> Vec<usize> {
    occurrences
        .iter()
        .filter(|kind| **kind != "rhs")
        .map(|kind| {
            let marker = marker(kind);
            assert_eq!(text.matches(marker).count(), 1, "{kind} printed once");
            text.find(marker).expect("marker")
        })
        .collect()
}

#[test]
fn the_consumer_runs_the_issued_occurrences_in_issued_order() {
    let issued = [
        "implicit_solve",
        "assignment",
        "implicit_verification",
        "consistency",
        "rhs",
    ];
    let positions = printed_order(&render(&issued), &issued);
    assert!(
        positions.windows(2).all(|pair| pair[0] < pair[1]),
        "{positions:?}"
    );
}

/// A template with an order of its own would print this permutation in that
/// order instead; the consumer must follow whatever order is issued.
#[test]
fn a_permuted_issue_order_is_printed_in_that_order() {
    let issued = [
        "consistency",
        "implicit_verification",
        "assignment",
        "implicit_solve",
        "rhs",
    ];
    let positions = printed_order(&render(&issued), &issued);
    assert!(
        positions.windows(2).all(|pair| pair[0] < pair[1]),
        "{positions:?}"
    );
}

/// An occurrence that is not issued is not printed.
#[test]
fn an_unissued_occurrence_is_not_printed() {
    let text = render(&["assignment", "rhs"]);
    for kind in ["implicit_solve", "implicit_verification", "consistency"] {
        assert!(!text.contains(marker(kind)), "{kind} must not be printed");
    }
}
