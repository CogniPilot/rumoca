use super::*;
use crate::typed_program::{InvocationCompletion, RecursionChain, eval_owner};
use rumoca_ir_solve::SolveAssertionLevel;

pub(super) fn forwarded_table() -> SolvePureCallTable {
    let (mut table, child) =
        super::assertions::assertion_then_fault_builder(SolveAssertionLevel::Error, false);
    let site = table.call_site(child).unwrap();
    table
        .add_owner(
            identity(950),
            site.inputs().to_vec(),
            site.outputs().to_vec(),
            span(950),
            |builder, inputs, outputs| {
                let argument = builder.load(inputs[0], span(951))?;
                let call = builder.emit_call(child, &[argument], span(952))?;
                let parent = builder.assertion_output(1, span(953))?;
                builder.forward_assertion(&call, 1, parent, span(954))?;
                for (output, value) in outputs.iter().zip(call.registers()) {
                    builder.store(*output, *value, span(955))?;
                }
                Ok(())
            },
        )
        .unwrap();
    table.finish()
}

#[test]
fn forwarded_stop_keeps_child_origin_and_actual_arguments_without_partial_tuple() {
    let table = forwarded_table();
    let owner = &table.owners()[1];
    assert!(owner.assertion_flow().is_some());
    let arguments = [TypedValue::scalar(&SolveValue::boolean(false))];
    let InvocationCompletion::Stopped(stop) =
        eval_owner(&table, owner, &arguments, RecursionChain::ROOT).unwrap()
    else {
        panic!("forwarded call published missing child results");
    };
    let observed = stop.observations.last().unwrap();
    assert_eq!(observed.observation.owner, table.owners()[0].id());
    assert_eq!(observed.observation.provenance, span(4));
    assert_eq!(
        observed.observation.captures[0].1.elements(),
        [SolveValueKind::Integer(17)]
    );
    for frame in &observed.invocation_path {
        let invocation = frame.invocation.as_ref().unwrap();
        assert_eq!(invocation.arguments.as_ref(), &arguments);
        assert_eq!(invocation.owner.id(), frame.owner);
        assert!(std::ptr::eq(
            invocation.owner,
            table.owner(frame.owner).unwrap()
        ));
    }
}

#[test]
fn forwarded_passed_assertion_keeps_the_later_numerical_fault() {
    let table = forwarded_table();
    let result = eval_pure_call(
        &table,
        table.owners()[1].id(),
        &[TypedValue::scalar(&SolveValue::boolean(true))],
    );
    assert!(
        matches!(result, Err(TypedProgramEvalError::IntegerArithmetic { provenance, .. }) if provenance == span(11))
    );
}
