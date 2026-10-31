//! MLS §7.2: a symbolic modifier keeps the occurrence where it was written.
use super::*;
use rumoca_core::{BuiltinFunction, Expression, VarName};

fn assert_reference(expression: &Expression, expected: &str) {
    match expression {
        Expression::VarRef { name, .. } => assert_eq!(name.as_str(), expected),
        other => panic!("expected reference to {expected}, got {other:?}"),
    }
}

#[test]
fn nested_epoch_modifier_preserves_local_reference_and_outer_pre_operand() {
    let mut session = Session::default();
    session
        .add_document(
            "EpochRecord.mo",
            include_str!("fixtures/symbolic_epoch_scope.mo"),
        )
        .expect("epoch fixture parses");
    let cases: [(&str, &[&str]); 2] = [
        ("EpochRecord.Test", &[""]),
        ("EpochRecord.Pair", &["first.", "second."]),
    ];
    for (target, prefixes) in cases {
        let flat = session
            .compile_model_flat_strict_reachable_uncached_with_recovery(target)
            .unwrap_or_else(|error| panic!("{target} must flatten: {error}"));
        for prefix in prefixes {
            let nested = &flat.variables[&VarName::new(format!("{prefix}avionics.task.epoch"))];
            assert_reference(
                nested.binding.as_ref().expect("nested input binding"),
                &format!("{prefix}avionics.navigationEpoch"),
            );
            let outer = &flat.variables[&VarName::new(format!("{prefix}avionics.navigationEpoch"))];
            match outer.binding.as_ref().expect("outer input binding") {
                Expression::BuiltinCall {
                    function: BuiltinFunction::Pre,
                    args,
                    ..
                } => {
                    assert_eq!(args.len(), 1);
                    assert_reference(&args[0], &format!("{prefix}estimator.status.reseedCount"));
                }
                other => panic!("epoch guard must keep pre(): {other:?}"),
            }
        }
    }
}
