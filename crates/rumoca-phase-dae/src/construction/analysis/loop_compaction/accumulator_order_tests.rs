//! Floating accumulators require their source association, not a tensor sum.
use super::preservation_corpus::*;
use super::*;
use rumoca_core::{EffectiveType, OpBinary, TypeId};

fn shapes() -> ShapeEnvironment {
    let mut shapes = ShapeEnvironment::default();
    shapes.insert(VarName::new("y"), vec![]);
    shapes.insert(VarName::new("r"), vec![14400]);
    shapes.insert(VarName::new("k"), vec![]);
    shapes
}

fn accumulation(target: &str, op: OpBinary, seed: Expression) -> Vec<rumoca_core::Statement> {
    vec![
        assign(target, seed),
        for_loop(
            "i",
            14400,
            vec![assign(
                target,
                binary(op, var(target), element("r", var("i"))),
            )],
        ),
    ]
}

#[test]
fn real_accumulators_retain_full_source_domain_seed_and_operation_spans() {
    let ordered = HashSet::from([VarName::new("y")]);
    for op in [OpBinary::Add, OpBinary::Sub, OpBinary::Mul] {
        for seed in [var("u"), real(-0.0), real(f64::MAX)] {
            let source = accumulation("y", op.clone(), seed);
            let result = compact_accumulator_loops(&source, &shapes(), &ordered);
            assert_eq!(result, source);
        }
    }
}

#[test]
fn real_accumulator_protection_reaches_nested_and_standalone_loops() {
    let ordered = HashSet::from([VarName::new("y")]);
    let source = accumulation("y", OpBinary::Add, var("u"));
    let standalone = vec![source[1].clone()];
    let nested = vec![branch(
        vec![(boolean(true), source)],
        Some(standalone.clone()),
    )];
    assert_eq!(
        compact_accumulator_loops(&standalone, &shapes(), &ordered),
        standalone
    );
    assert_eq!(
        compact_accumulator_loops(&nested, &shapes(), &ordered),
        nested
    );
    let integer = accumulation("k", OpBinary::Add, integer(0));
    assert_eq!(
        compact_accumulator_loops(&integer, &shapes(), &ordered).len(),
        1
    );
}

#[test]
fn real_alias_and_tensor_types_are_classified_by_resolved_type() {
    let mut function = harness_function(vec![]);
    function.outputs[0] = rumoca_core::FunctionParam::new(
        "y",
        "RealAlias",
        EffectiveType::new(TypeId(100), TypeId(1), vec![]).unwrap(),
        span(),
    );
    let flat = flat::Model {
        predefined_types: flat::PredefinedTypeIds {
            real: TypeId(1),
            integer: TypeId(2),
            boolean: TypeId(3),
            string: TypeId(4),
            clock: TypeId(5),
        },
        ..flat::Model::default()
    };
    let names = ordered_accumulators(&function, &flat);
    assert!(names.contains(&VarName::new("y")));
    assert!(names.contains(&VarName::new("w")));
    assert!(names.contains(&VarName::new("t")));
    assert!(names.contains(&VarName::new("k")));
    assert!(!names.contains(&VarName::new("f")));
}

#[test]
fn integer_accumulators_retain_seeded_nested_and_standalone_source_order() {
    let ordered = HashSet::from([VarName::new("k")]);
    for op in [OpBinary::Add, OpBinary::Sub, OpBinary::Mul] {
        let source = accumulation("k", op, integer(-i64::MAX));
        let standalone = vec![source[1].clone()];
        let nested = vec![branch(vec![(boolean(true), source.clone())], None)];
        assert_eq!(
            compact_accumulator_loops(&source, &shapes(), &ordered),
            source
        );
        assert_eq!(
            compact_accumulator_loops(&standalone, &shapes(), &ordered),
            standalone
        );
        assert_eq!(
            compact_accumulator_loops(&nested, &shapes(), &ordered),
            nested
        );
    }
}

#[test]
fn resolved_integer_aliases_and_arrays_also_require_source_order() {
    let mut function = harness_function(vec![]);
    function.locals.push(rumoca_core::FunctionParam::new(
        "counter",
        "CounterAlias",
        EffectiveType::new(TypeId(101), TypeId(2), vec![14400]).unwrap(),
        span(),
    ));
    let flat = flat::Model {
        predefined_types: flat::PredefinedTypeIds {
            real: TypeId(1),
            integer: TypeId(2),
            boolean: TypeId(3),
            string: TypeId(4),
            clock: TypeId(5),
        },
        ..flat::Model::default()
    };
    assert!(ordered_accumulators(&function, &flat).contains(&VarName::new("counter")));
}
