mod fixture;

use super::preservation_interpreter::interpret_with_limits;
use super::preservation_values::{Environment, Refusal, Value};
use super::*;
use fixture::*;
use rumoca_core::{DefId, EffectiveType, Function, FunctionParam, OpBinary, TypeId};

fn rectangularize(source: &[rumoca_core::Statement]) -> Vec<rumoca_core::Statement> {
    let mut shapes = ShapeEnvironment::default();
    crate::construction::function_shapes::infer_finite_for_counter_bounds(
        source,
        &mut shapes,
        &[(VarName::new("count"), COUNTER)],
        &[],
    );
    assert_eq!(shapes.proven_integer_bounds(&count()), Some((0, 14400)));
    assert_eq!(shapes.proven_extent(&count()), None);
    rectangularize_dependent_loops(source, &HashMap::new(), &shapes, &HashSet::new())
        .expect("proved whole-domain envelope")
}

fn run(source: &[rumoca_core::Statement]) -> Result<Environment, Refusal> {
    // Fuel charges expressions as well as statements: the largest guarded
    // fixture needs fewer than 375000 visits; both domains remain 14400 cells.
    interpret_with_limits(source, &entry(), 400000, 14400)
}

#[test]
fn full_14400_same_domain_reference_preserves_count_order_and_lexical_binders() {
    let i = variable("i", DefId::new(8111));
    let even = Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Mod,
        args: vec![i.clone(), integer(2)],
        span: span(),
    };
    let cases = [
        (binary(OpBinary::Eq, integer(1), integer(1)), 14400),
        (binary(OpBinary::Eq, integer(1), integer(0)), 0),
        (binary(OpBinary::Eq, even, integer(0)), 7200),
        (binary(OpBinary::Eq, i, integer(14400)), 1),
    ];
    for (predicate, expected_count) in cases {
        let original = source(predicate);
        let rewritten = rectangularize(&original);
        assert_ne!(
            original, rewritten,
            "the comparison must execute the actual guarded envelope"
        );
        let before = run(&original).expect("canonical original");
        let after = run(&rewritten).expect("canonical rewrite");
        assert_eq!(
            before, after,
            "every output/local/input and binder restoration"
        );
        assert_eq!(
            after[&VarName::new("count")],
            Value::Integer(expected_count)
        );
        assert_eq!(after[&VarName::new("i")], Value::Integer(80000));
        assert_eq!(after[&VarName::new("j")], Value::Integer(90000));
    }
}

#[test]
fn same_domain_reference_preserves_active_fault_and_skips_poisoned_inactive_body() {
    for (accepted, expected) in [
        (false, Ok(entry_after_empty())),
        (true, Err(Refusal::IntegerOverflow)),
    ] {
        let predicate = binary(OpBinary::Eq, integer(1), integer(i64::from(accepted)));
        let mut original = source(predicate);
        let rumoca_core::Statement::For { equations, .. } = &mut original[3] else {
            panic!("suffix For")
        };
        equations.push(assign(
            "sum",
            DefId::new(8108),
            binary(OpBinary::Add, integer(i64::MAX), integer(1)),
        ));
        let rewritten = rectangularize(&original);
        assert_eq!(run(&original), expected);
        assert_eq!(run(&rewritten), expected);
    }
}

fn entry_after_empty() -> Environment {
    let mut env = entry();
    env.insert(VarName::new("count"), Value::Integer(0));
    env.insert(VarName::new("sum"), Value::Integer(0));
    env
}

fn declarations(
    canonical: TypeId,
    dimensions: Vec<i64>,
    id: Option<DefId>,
) -> (Function, flat::Model) {
    let integer = TypeId(21);
    let flat = flat::Model {
        predefined_types: flat::PredefinedTypeIds {
            real: TypeId(20),
            integer,
            boolean: TypeId(22),
            string: TypeId(23),
            clock: TypeId(24),
        },
        ..flat::Model::default()
    };
    let effective =
        EffectiveType::new(canonical, canonical, dimensions).expect("checked predefined type");
    let mut local = FunctionParam::new("count", "Integer", effective, span());
    local.def_id = id;
    let mut function = Function::new("generic_counter", span());
    function.locals = vec![local];
    (function, flat)
}

#[test]
fn integration_admits_only_resolved_scalar_canonical_integer_locals() {
    let original = source(binary(OpBinary::Eq, integer(1), integer(1)));
    for (canonical, dimensions, id, expected) in [
        (TypeId(21), vec![], Some(COUNTER), Some((0, 14400))),
        (TypeId(20), vec![], Some(COUNTER), None),
        (TypeId(21), vec![1], Some(COUNTER), None),
        (TypeId(21), vec![], None, None),
        (TypeId(21), vec![], Some(DefId::new(999)), None),
    ] {
        let (function, flat) = declarations(canonical, dimensions, id);
        let mut shapes = ShapeEnvironment::default();
        infer_declared_finite_counters(&original, &mut shapes, &function, &flat);
        assert_eq!(shapes.proven_integer_bounds(&count()), expected);
    }
}

#[test]
fn declined_overflow_proof_keeps_original_canonical_fault() {
    let mut original = source(binary(OpBinary::Eq, integer(1), integer(1)));
    original[0] = assign("count", COUNTER, integer(i64::MAX));
    let mut shapes = ShapeEnvironment::default();
    crate::construction::function_shapes::infer_finite_for_counter_bounds(
        &original,
        &mut shapes,
        &[(VarName::new("count"), COUNTER)],
        &[],
    );
    assert_eq!(shapes.proven_integer_bounds(&count()), None);
    assert_eq!(run(&original), Err(Refusal::IntegerOverflow));
}
