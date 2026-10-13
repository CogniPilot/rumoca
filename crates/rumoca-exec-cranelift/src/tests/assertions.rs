use super::*;
use rumoca_eval_solve::TypedValue;
use rumoca_ir_solve::{
    SolveArithmeticProfile, SolveAssertionLevel, SolveBinaryOperator, SolveIntegerDomain,
    SolvePureCallIdentity, SolvePureCallOutput, SolvePureCallTable, SolveRealFormat,
    SolveScalarType, SolveValue, SolveValueKind, SolveValueType,
};

fn arithmetic() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL)
}

fn real_type() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::real(arithmetic()))
}

fn boolean_type() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::Boolean)
}

fn identity(value: u64) -> SolvePureCallIdentity {
    SolvePureCallIdentity::issued(NonZeroU64::new(value).unwrap())
}

fn child_outputs(level: SolveAssertionLevel) -> Vec<SolvePureCallOutput> {
    vec![
        SolvePureCallOutput::result(real_type()),
        SolvePureCallOutput::assertion_predicate_at_level(level),
        SolvePureCallOutput::assertion_message_value(real_type(), 1),
    ]
}

/// Owners: 0 asserts `ok` with the input captured in its failed message,
/// 1 forwards that assertion to its own predicate, 2 asserts only inside the
/// selected branch of an `if` (the shape of a guarded flow-model function).
fn assertion_table(level: SolveAssertionLevel) -> SolvePureCallTable {
    let span = fixture_span();
    let inputs = vec![real_type(), boolean_type()];
    SolvePureCallTable::construct(arithmetic(), |table| {
        let child = table.add_owner(
            identity(1),
            inputs.clone(),
            child_outputs(level),
            span,
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span)?;
                let ok = b.load(inputs[1], span)?;
                let predicate = b.assertion_output(1, span)?;
                let message =
                    b.check_assertion(predicate, ok, &[x], span, |m, captures, out| {
                        let value = m.load(captures[0], span)?;
                        m.store(out[0], value, span)
                    })?;
                let two = b.constant(SolveValue::real(arithmetic(), 2.0), span)?;
                let doubled = b.binary(SolveBinaryOperator::Multiply, x, two, span)?;
                b.store(outputs[0], doubled, span)?;
                b.store(outputs[1], ok, span)?;
                b.store(outputs[2], message[0], span)
            },
        )?;
        let site = table.call_site(child).unwrap();
        table.add_owner(
            identity(2),
            site.inputs().to_vec(),
            site.outputs().to_vec(),
            span,
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span)?;
                let ok = b.load(inputs[1], span)?;
                let call = b.emit_call(child, &[x, ok], span)?;
                let parent = b.assertion_output(1, span)?;
                b.forward_assertion(&call, 1, parent, span)?;
                for (output, value) in outputs.iter().zip(call.registers()) {
                    b.store(*output, *value, span)?;
                }
                Ok(())
            },
        )?;
        table.add_owner(
            identity(3),
            site.inputs().to_vec(),
            site.outputs().to_vec(),
            span,
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span)?;
                let guarded = b.load(inputs[1], span)?;
                let yes = b.constant(SolveValue::boolean(true), span)?;
                let inactive = b.constant(
                    SolveValue::inactive_assertion_message(SolveScalarType::real(arithmetic())),
                    span,
                )?;
                let result = b.conditional(
                    guarded,
                    &[x],
                    vec![real_type()],
                    span,
                    |region, captures, out| {
                        let x = region.load(captures[0], span)?;
                        let invalid = region.constant(SolveValue::boolean(false), span)?;
                        let call = region.emit_call(child, &[x, invalid], span)?;
                        let parent = region.assertion_output(1, span)?;
                        region.forward_assertion(&call, 1, parent, span)?;
                        region.store(out[0], call.registers()[0], span)
                    },
                    |region, captures, out| {
                        let x = region.load(captures[0], span)?;
                        region.store(out[0], x, span)
                    },
                )?;
                b.store(outputs[0], result[0], span)?;
                b.store(outputs[1], yes, span)?;
                b.store(outputs[2], inactive, span)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn flatten(values: &[TypedValue]) -> Vec<f64> {
    values
        .iter()
        .flat_map(|value| value.elements().iter().copied())
        .map(|kind| match kind {
            SolveValueKind::Real32(bits) => f64::from(f32::from_bits(bits)),
            SolveValueKind::Real64(bits) => f64::from_bits(bits),
            SolveValueKind::Integer(value) => value as f64,
            SolveValueKind::Boolean(value) => f64::from(value),
        })
        .collect()
}

/// The typed interpreter value row: published cells at every assertion level.
fn interpreted(table: &SolvePureCallTable, owner: usize, x: f64, ok: bool) -> Vec<f64> {
    let arguments = [
        TypedValue::construct(real_type(), vec![SolveValueKind::Real64(x.to_bits())]).unwrap(),
        TypedValue::construct(boolean_type(), vec![SolveValueKind::Boolean(ok)]).unwrap(),
    ];
    let values = rumoca_eval_solve::eval_pure_call(table, table.owners()[owner].id(), &arguments)
        .expect("a value row never stops on an assertion");
    flatten(&values)
}

fn native(
    compiled: &CompiledPureCallTable,
    table: &SolvePureCallTable,
    owner: usize,
    x: f64,
    ok: bool,
) -> Result<Vec<f64>, CompileError> {
    let site = table.owners()[owner].call_site();
    let mut output = vec![f64::NAN; site.output_scalar_count().unwrap()];
    compiled.call_scalar_payload(
        rumoca_eval_solve::PureCallInvocation::Primal(&site),
        &[x, f64::from(ok)],
        &mut output,
        &mut Vec::new(),
        &mut Vec::new(),
    )?;
    Ok(output)
}

#[test]
fn native_assertions_match_the_typed_interpreter_for_every_owner_and_level() {
    for level in [SolveAssertionLevel::Error, SolveAssertionLevel::Warning] {
        let table = assertion_table(level);
        let compiled = compile_pure_call_table(&table).unwrap();
        for owner in 0..table.owners().len() {
            for (x, ok) in [(3.5, true), (3.5, false), (-1.0, true), (-1.0, false)] {
                let expected = interpreted(&table, owner, x, ok);
                let cells = native(&compiled, &table, owner, x, ok)
                    .expect("a native value row never stops on an assertion");
                assert_eq!(
                    cells, expected,
                    "owner {owner} level {level:?} x {x} ok {ok}"
                );
            }
        }
    }
}

#[test]
fn a_failing_error_assertion_publishes_the_row_values_natively() {
    let table = assertion_table(SolveAssertionLevel::Error);
    let compiled = compile_pure_call_table(&table).unwrap();
    let cells = native(&compiled, &table, 0, 2.0, false).unwrap();
    assert_eq!(cells[0], 4.0);
    assert_eq!(cells[1], 0.0);
}
