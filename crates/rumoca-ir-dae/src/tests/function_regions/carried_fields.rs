//! A field of a returned record carries its executed-path state to the read.

use super::*;

/// What the body of `make` does with the two partial field locals.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Use {
    /// `r := R(a, b)` with both states carried: the accepted form.
    ReturnedRecord,
    /// `spare := R(a, b)`: a carried record assigned to a local.
    LocalRecord,
    /// `r := R(a + a, b)`: arithmetic on a value that may be absent.
    Arithmetic,
    /// `r := R(a, b); spare := r`: the returned record read back.
    ReadBack,
}

struct Made<'dae> {
    make: FunctionId<'dae>,
    real: ValueTypeId<'dae>,
}

/// `function make input Real x; output R r; protected Real a, b;` with `a`
/// written in the first arm and `b` in the other.
fn make_function<'dae>(
    dae: &mut DaeConstruction<'dae>,
    source: &TestSource,
    body_use: Use,
) -> Result<Made<'dae>, DaeConstructionError> {
    let at = source.source("function make", 0);
    let (real, record) = dae.types(|types| {
        let real = types.derived(ValueType::scalar(ScalarType::Real), at)?;
        let record = types.record(
            VarName::new("R"),
            [(VarName::new("a"), real), (VarName::new("b"), real)],
            at,
        )?;
        Ok((real, record))
    })?;
    let (make, ()) = dae.function(
        FunctionSignature::new(VarName::new("make"), [real], [record], at),
        |dae, reservation| {
            let (x, output, spare, a, b) = dae.functions(|functions| {
                Ok((
                    functions.parameter(&reservation, VarName::new("x"), 0, at)?,
                    functions.output(&reservation, VarName::new("r"), 0, at)?,
                    functions.local(&reservation, VarName::new("spare"), record, at)?,
                    functions.local(&reservation, VarName::new("a"), real, at)?,
                    functions.local(&reservation, VarName::new("b"), real, at)?,
                ))
            })?;
            let (x, positive) = dae.expressions(|expressions| {
                let x = expressions.at(at).function_parameter(x)?;
                let zero = expressions.at(at).literal(DaeLiteral::Real(0.0))?;
                let positive = expressions
                    .at(at)
                    .binary(BinaryOperator::Greater, x, zero)?;
                Ok((x, positive))
            })?;
            let body = dae.functions(|functions| functions.begin(reservation, at))?;
            let (mut child, finish) =
                dae.functions(|functions| functions.begin_branch(body, at))?;
            dae.functions(|functions| functions.assign(&mut child, a, x, at))?;
            let (body, branch) =
                dae.functions(|functions| functions.finish_branch(finish, child))?;
            let (mut child, finish) =
                dae.functions(|functions| functions.begin_branch(body, at))?;
            dae.functions(|functions| functions.assign(&mut child, b, x, at))?;
            let (mut body, fallback) =
                dae.functions(|functions| functions.finish_branch(finish, child))?;
            dae.functions(|functions| {
                functions.finish_conditional(&mut body, vec![(positive, branch)], fallback, at)
            })?;
            let (a, b) = dae.functions(|functions| {
                functions.carry_assigned(&mut body, a, at)?;
                functions.carry_assigned(&mut body, b, at)?;
                Ok((functions.read(&body, a, at)?, functions.read(&body, b, at)?))
            })?;
            let value = dae.expressions(|expressions| {
                let first = match body_use {
                    Use::Arithmetic => expressions.at(at).binary(BinaryOperator::Add, a, a)?,
                    Use::ReturnedRecord | Use::LocalRecord | Use::ReadBack => a,
                };
                expressions.at(at).record(record, [first, b])
            })?;
            let target = if body_use == Use::LocalRecord {
                spare
            } else {
                output
            };
            dae.functions(|functions| {
                functions.assign(&mut body, target, value, at)?;
                if body_use == Use::ReadBack {
                    let returned = functions.read(&body, output, at)?;
                    functions.assign(&mut body, spare, returned, at)?;
                }
                functions.define(body, at)
            })
        },
    )?;
    Ok(Made { make, real })
}

/// `function pick input Real x, ra, rb; output Real y := ra;` whose last two
/// parameters accept absence when `accepting` holds.
fn pick_function<'dae>(
    dae: &mut DaeConstruction<'dae>,
    source: &TestSource,
    real: ValueTypeId<'dae>,
    accepting: bool,
    forward_plain_parameter: bool,
) -> Result<FunctionId<'dae>, DaeConstructionError> {
    let at = source.source("function pick", 0);
    let signature = FunctionSignature::new(VarName::new("pick"), [real, real, real], [real], at)
        .with_absent_parameters(if accepting { vec![1, 2] } else { Vec::new() });
    dae.function(signature, |dae, reservation| {
        let (x, ra, output) = dae.functions(|functions| {
            let x = functions.parameter(&reservation, VarName::new("x"), 0, at)?;
            let ra = functions.parameter(&reservation, VarName::new("ra"), 1, at)?;
            functions.parameter(&reservation, VarName::new("rb"), 2, at)?;
            let output = functions.output(&reservation, VarName::new("y"), 0, at)?;
            Ok((x, ra, output))
        })?;
        let value = dae.expressions(|expressions| {
            if forward_plain_parameter {
                // `x` does not accept absence, so it has no state to forward.
                expressions.at(at).assigned_parameter(x, false)?;
            }
            if accepting {
                expressions.at(at).assigned_parameter(ra, true)
            } else {
                expressions.at(at).function_parameter(ra)
            }
        })?;
        let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
        dae.functions(|functions| {
            functions.assign(&mut body, output, value, at)?;
            functions.define(body, at)
        })
    })
    .map(|(function, ())| function)
}

fn source() -> TestSource {
    TestSource::new("function make ... end make; function pick ... end pick; y = pick(1, make(1));")
}

/// The whole accepted shape: `y = pick(1, make(1).a, make(1).b)` with both
/// field states forwarded.
fn passed_on(accepting: bool, forwarded: bool) -> Result<Dae, DaeConstructionError> {
    let source = source();
    let call_at = source.source("y = pick(1, make(1));", 0);
    Dae::construct(source.map.clone(), |dae| {
        let Made { make, real } = make_function(dae, &source, Use::ReturnedRecord)?;
        let pick = pick_function(dae, &source, real, accepting, false)?;
        dae.expressions(|expressions| {
            let one = expressions.at(call_at).literal(DaeLiteral::Real(1.0))?;
            let made = expressions.at(call_at).call(make, 0, [one])?;
            let mut fields = Vec::new();
            for field in 0..2 {
                let read = expressions.at(call_at).field(made, field)?;
                let read = match forwarded {
                    true => expressions.at(call_at).pass_assigned(read)?,
                    false => read,
                };
                fields.push(read);
            }
            expressions
                .at(call_at)
                .call(pick, 0, [one, fields[0], fields[1]])?;
            Ok(())
        })
    })
}

fn refused(result: Result<Dae, DaeConstructionError>) -> bool {
    matches!(
        result,
        Err(DaeConstructionError::InvalidAssignedStatePass { .. })
    )
}

fn check_passed_on(view: DaeView<'_>) {
    let make = view.function(view.function_id(0).unwrap()).unwrap();
    let statements = make.statements().collect::<Vec<_>>();
    let [
        FunctionStatementView::Conditional { conditional },
        FunctionStatementView::CarryAssigned {
            conditional: first, ..
        },
        FunctionStatementView::CarryAssigned {
            conditional: second,
            ..
        },
        FunctionStatementView::Assignment { .. },
    ] = &statements[..]
    else {
        panic!("the two field states are carried, not checked, before the record is returned");
    };
    assert_eq!(first.id(), conditional.id());
    assert_eq!(second.id(), conditional.id());
    assert!(make.absent_parameters().is_empty());
    let pick = view.function(view.function_id(1).unwrap()).unwrap();
    assert_eq!(pick.absent_parameters(), &[1, 2]);
}

#[test]
fn a_returned_record_carries_its_field_states_to_an_accepting_call() {
    passed_on(true, true)
        .expect("carried fields reach parameters that accept absence")
        .inspect(check_passed_on);
}

#[test]
fn the_carried_form_replays_through_both_wires() {
    let dae = passed_on(true, true).unwrap();
    let bytes = bincode::serialize(&dae).unwrap();
    let replayed: Dae = bincode::deserialize(&bytes).unwrap();
    replayed.inspect(check_passed_on);
    assert_eq!(bincode::serialize(&replayed).unwrap(), bytes);
    let json = serde_json::to_value(&dae).unwrap();
    let replayed: Dae = serde_json::from_value(json.clone()).unwrap();
    replayed.inspect(check_passed_on);
    assert_eq!(serde_json::to_value(&replayed).unwrap(), json);
}

#[test]
fn a_plain_read_of_a_result_field_needs_no_accepting_parameter() {
    // The read is the checked read; it carries nothing into the call.
    passed_on(false, false).expect("checked reads are ordinary arguments");
    passed_on(true, false).expect("an accepting parameter also takes a present value");
}

#[test]
fn a_forwarded_state_is_refused_at_a_parameter_that_does_not_accept_absence() {
    assert!(refused(passed_on(false, true)));
}

#[test]
fn a_carried_value_is_refused_outside_the_returned_record() {
    for body_use in [Use::LocalRecord, Use::Arithmetic, Use::ReadBack] {
        let source = source();
        let result = Dae::construct(source.map.clone(), |dae| {
            make_function(dae, &source, body_use).map(|_| ())
        });
        assert!(refused(result));
    }
}

#[test]
fn only_a_result_field_or_an_accepting_parameter_has_a_state_to_forward() {
    let source = source();
    let result = Dae::construct(source.map.clone(), |dae| {
        let real = dae.types(|types| {
            types.derived(
                ValueType::scalar(ScalarType::Real),
                source.source("function pick", 0),
            )
        })?;
        pick_function(dae, &source, real, true, true).map(|_| ())
    });
    assert!(refused(result));
}

#[test]
fn a_read_of_a_parameter_that_may_arrive_absent_is_the_checked_read() {
    let source = source();
    let at = source.source("function pick", 0);
    let result = Dae::construct(source.map.clone(), |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        let signature = FunctionSignature::new(VarName::new("pick"), [real], [real], at)
            .with_absent_parameters([0]);
        dae.function(signature, |dae, reservation| {
            let (ra, output) = dae.functions(|functions| {
                Ok((
                    functions.parameter(&reservation, VarName::new("ra"), 0, at)?,
                    functions.output(&reservation, VarName::new("y"), 0, at)?,
                ))
            })?;
            let value = dae.expressions(|expressions| {
                let plain = expressions.at(at).function_parameter(ra)?;
                let named = expressions
                    .at(at)
                    .coordinate(CoordinateInput::FunctionParameter(ra))?;
                let checked = expressions.at(at).assigned_parameter(ra, true)?;
                // Each spelling of the read constructs the checked node.
                expressions.at(at).array([plain, named, checked])?;
                Ok(plain)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
            dae.functions(|functions| {
                functions.assign(&mut body, output, value, at)?;
                functions.define(body, at)
            })
        })
        .map(|_| ())
    })
    .expect("the checked read is an ordinary value");
    result.inspect(|view| {
        let checked = (0..view.expression_count())
            .filter_map(|index| view.expression(view.expression_id(index)?))
            .filter(|node| {
                matches!(
                    node.operation(),
                    ExpressionOperation::AssignedParameter { checked: true, .. }
                )
            })
            .count();
        let plain = (0..view.expression_count())
            .filter_map(|index| view.expression(view.expression_id(index)?))
            .filter(|node| matches!(node.operation(), ExpressionOperation::Coordinate(_)))
            .count();
        assert_eq!((checked, plain), (3, 0));
    });
}
