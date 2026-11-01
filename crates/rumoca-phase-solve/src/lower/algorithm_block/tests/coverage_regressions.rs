//! Behavioral witnesses for paths omitted by the original #533 workspace run.

use super::*;

/// Compare a lifecycle transition against the independent GALEC evaluator,
/// including every authored block slot, then check the numerical witness.
fn compare_step(block: Block, inputs: &[(&str, f64)], expected: &[(&str, f64)]) {
    use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
    use rumoca_ir_solve::SolveBlockMethodKind;

    let package = package(block);
    let solve = lower_algorithm_block(&package, binary64()).expect("the transition refines");
    let checked =
        rumoca_ir_galec::package::CheckedAlgorithmBlock::construct(package.block().clone())
            .unwrap();
    let mut oracle = Evaluator::new(&checked, IntegerDomain::signed_32()).unwrap();
    let mut state = vec![None; solve.slots().len()];
    for &(name, value) in inputs {
        oracle.set_state(name, Value::Real(value)).unwrap();
        let index = solve
            .slots()
            .iter()
            .position(|slot| slot.source().qualified_name() == name)
            .expect("the input slot exists");
        state[index] = Some(real64(value));
    }
    oracle.startup().unwrap();
    rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::Startup, &mut state)
        .unwrap();
    oracle.do_step().unwrap();
    rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::DoStep, &mut state)
        .unwrap();
    let authored = package.block().interface.len() + package.block().protected.len();
    for (index, slot) in solve.slots().iter().take(authored).enumerate() {
        let name = slot.source().qualified_name();
        assert_eq!(
            solve_bits(state[index].as_ref()),
            oracle_bits(oracle.state(name).unwrap()),
            "{name}"
        );
    }
    for &(name, value) in expected {
        assert_eq!(
            oracle_bits(oracle.state(name).unwrap()),
            Bits::Real(value.to_bits())
        );
    }
}

#[test]
fn saturation_inside_a_conditional_publishes_its_clamped_state() {
    // The region's limit reads and writes x; its clamped value must become
    // the value published to the surrounding block, for both target forms.
    for target in [LimitTarget::Reference(state("x")), LimitTarget::SelfState] {
        let mut block = controller();
        block.do_step.locals.clear();
        block.do_step.statements = vec![at(
            Statement::If(IfStatement {
                branches: vec![IfBranch {
                    condition: Condition::Expression(sref("first")),
                    body: vec![
                        assign(state("x"), r(250.0), 41),
                        at(Statement::Limit(vec![target]), 42),
                        assign(state("y"), sref("x"), 43),
                    ],
                    span: span(40),
                }],
                else_body: None,
            }),
            40,
        )];
        compare_step(
            block,
            &[("u", 0.0), ("gain", 2.5), ("period", 0.005)],
            &[("x", 100.0)],
        );
    }
}

#[test]
fn a_user_call_inside_a_builtin_keeps_its_declared_result_type() {
    let mut block = function_caller();
    let Statement::Assignment { value, .. } = &mut block.do_step.statements[0].node else {
        panic!("the caller assigns its first output");
    };
    *value = builtin_call("absolute", vec![value.clone()]);
    respan(&mut block);
    compare_step(
        block,
        &[("u", 1.0), ("v", -1.0), ("period", 0.01)],
        &[("y", 1.0), ("s", 3.0), ("d", -2.0)],
    );
}

#[test]
fn missing_callee_summaries_refuse_a_region_until_the_effects_are_known() {
    let package = package(function_caller());
    let scope = block_scope(package.block(), binary64()).unwrap();
    let method = method_scope(&scope, Method::DoStep, &package.block().do_step).unwrap();
    // Signatures alone cannot prove that a call preserves fault-visible
    // state. This must fail closed before the callee-first pass populates it.
    let error = require_fault_visible_state(&method, &package.block().do_step.statements)
        .expect_err("an unresolved callee may fault after an unpublished effect");
    assert_eq!(error.code(), EL007_UNSUPPORTED_ALGORITHM_CODE);
    assert!(error.to_string().contains("fault after"), "{error}");
    lower_functions(package.block(), &scope).expect("callee-first bodies are checked");
    require_fault_visible_state(&method, &package.block().do_step.statements)
        .expect("the known function effects admit the same caller");
}
