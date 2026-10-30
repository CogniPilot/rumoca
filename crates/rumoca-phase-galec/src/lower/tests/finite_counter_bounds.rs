//! Finite carried indices, through the existing checked function projection.
use super::*;
use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};

#[derive(Clone, Copy)]
struct Counter {
    extent: u32,
    initial: i64,
    step: Option<i64>,
    reset_outside: bool,
}

fn fixture(case: Counter) -> dae::Dae {
    fixture_with_guard(case, None)
}

fn fixture_with_guard(case: Counter, guard: Option<bool>) -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "function copy input Real rhs[:]; input Integer step; output Real x[size(rhs,1)]; \
                protected Integer k; algorithm x := rhs; k := 0; \
                for i in 1:size(rhs,1) loop k := k + 1; x[k] := rhs[1]; end for; end copy;";
    let source = sources.add("finite-counter.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |dae| {
        let (vector, integer) = dae.types(|types| {
            Ok((
                types.derived(
                    dae::ValueType::array(dae::ScalarType::Real, [case.extent]),
                    at,
                )?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
            ))
        })?;
        let signature =
            dae::FunctionSignature::new(VarName::new("copy"), [vector, integer], [vector], at);
        dae.function(signature, |dae, reservation| {
            let (rhs, hint, x, k) = dae.functions(|f| {
                Ok((
                    f.parameter(&reservation, VarName::new("rhs"), 0, at)?,
                    f.parameter(&reservation, VarName::new("step"), 1, at)?,
                    f.output(&reservation, VarName::new("x"), 0, at)?,
                    f.local(&reservation, VarName::new("k"), integer, at)?,
                ))
            })?;
            let initial =
                dae.expressions(|e| e.at(at).literal(dae::DaeLiteral::Integer(case.initial)))?;
            let input = dae.expressions(|e| e.at(at).function_parameter(rhs))?;
            let mut body = dae.functions(|f| f.begin(reservation, at))?;
            dae.functions(|f| {
                f.assign(&mut body, x, input, at)?;
                f.assign(&mut body, k, initial, at)
            })?;
            let domain = counter_domain(dae, case.extent, guard.is_some(), at)?;
            let mut loop_body = dae.functions(|f| f.begin_loop(body, domain, [x, k], at))?;
            if let Some(unknown) = guard {
                loop_body = guarded_updates(dae, loop_body, [x, k], input, hint, unknown, at)?;
                let body = dae.functions(|f| f.finish_loop(loop_body, at))?;
                return dae.functions(|f| f.define(body, at));
            }
            let current = dae.functions(|f| f.read(loop_body.body(), k, at))?;
            let step = dae.expressions(|e| match case.step {
                Some(step) => e.at(at).literal(dae::DaeLiteral::Integer(step)),
                None => e.at(at).function_parameter(hint),
            })?;
            let value =
                dae.expressions(|e| e.at(at).binary(dae::BinaryOperator::Add, current, step))?;
            dae.functions(|f| f.assign_loop(&mut loop_body, k, value, at))?;
            if case.reset_outside {
                let outside = integer_literal(dae, i64::from(case.extent) + 1, at)?;
                dae.functions(|f| f.assign_loop(&mut loop_body, k, outside, at))?;
            }
            store(dae, &mut loop_body, x, k, input, at)?;
            let body = dae.functions(|f| f.finish_loop(loop_body, at))?;
            dae.functions(|f| f.define(body, at))
        })
        .map(|_| ())
    })
    .unwrap()
}

fn store<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    body: &mut dae::FunctionLoop<'dae>,
    output: dae::FunctionValueId<'dae>,
    counter: dae::FunctionValueId<'dae>,
    input: dae::ExprId<'dae>,
    at: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let (index, current) = dae.functions(|f| {
        Ok((
            f.read(body.body(), counter, at)?,
            f.read(body.body(), output, at)?,
        ))
    })?;
    let updated = dae.expressions(|e| {
        let one = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
        let first = e.at(at).index(
            input,
            [dae::Subscript::Index {
                expression: one,
                provenance: at,
            }],
        )?;
        e.at(at).array_update(
            current,
            first,
            [dae::Subscript::Index {
                expression: index,
                provenance: at,
            }],
        )
    })?;
    dae.functions(|f| f.assign_loop(body, output, updated, at))
}

fn lower(case: Counter) -> Result<Vec<gast::UserFunction>, String> {
    lower_dae(fixture(case))
}

fn lower_dae(dae: dae::Dae) -> Result<Vec<gast::UserFunction>, String> {
    dae.inspect(|view| {
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        user_functions::lower_reachable(
            view,
            &definitions,
            HashSet::from([0]),
            EmissionFacts::structured(),
        )
        .map_err(|error| format!("{error:?}"))
    })
}

#[test]
fn a_finite_carried_counter_is_in_range_at_every_executed_write() {
    for extent in [1, 3, 15, 120] {
        let functions = lower(Counter {
            extent,
            initial: 0,
            step: Some(1),
            reset_outside: false,
        })
        .expect("a checked finite counter bounds every write without unrolling the emitted loop");
        assert_eq!(functions.len(), 1);
        assert!(
            functions[0]
                .statements
                .iter()
                .any(|statement| matches!(statement.node, gast::Statement::For(_)))
        );
    }
}

#[test]
fn a_descending_carried_counter_is_also_bounded() {
    lower(Counter {
        extent: 15,
        initial: 16,
        step: Some(-1),
        reset_outside: false,
    })
    .unwrap();
}

#[test]
fn an_off_by_one_start_is_not_admitted() {
    assert!(
        lower(Counter {
            extent: 3,
            initial: 1,
            step: Some(1),
            reset_outside: false
        })
        .is_err()
    );
}

#[test]
fn an_unknown_step_does_not_inherit_an_optimistic_range() {
    assert!(
        lower(Counter {
            extent: 3,
            initial: 0,
            step: None,
            reset_outside: false
        })
        .is_err()
    );
}

#[test]
fn a_second_write_invalidates_the_counter_proof() {
    assert!(
        lower(Counter {
            extent: 3,
            initial: 0,
            step: Some(1),
            reset_outside: true
        })
        .is_err()
    );
}

#[test]
fn overflow_is_not_a_wrapping_counter_bound() {
    assert!(
        lower(Counter {
            extent: 3,
            initial: i64::MAX,
            step: Some(1),
            reset_outside: false
        })
        .is_err()
    );
}

#[test]
fn a_stride_that_overshoots_the_extent_is_refused() {
    assert!(
        lower(Counter {
            extent: 3,
            initial: 0,
            step: Some(2),
            reset_outside: false
        })
        .is_err()
    );
}

#[test]
fn a_counter_crossing_zero_is_refused() {
    assert!(
        lower(Counter {
            extent: 3,
            initial: -1,
            step: Some(1),
            reset_outside: false
        })
        .is_err()
    );
}

#[test]
fn an_exhausted_coordinate_proof_budget_does_not_admit_an_index() {
    assert!(
        lower(Counter {
            extent: 65_537,
            initial: 0,
            step: Some(1),
            reset_outside: false
        })
        .is_err()
    );
}

fn integer_literal<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    value: i64,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    dae.expressions(|e| e.at(at).literal(dae::DaeLiteral::Integer(value)))
}

fn counter_domain<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    extent: u32,
    guarded: bool,
    at: dae::DaeProvenance,
) -> Result<dae::DomainId<'dae>, dae::DaeConstructionError> {
    let limits = if guarded {
        vec![2, 4]
    } else {
        vec![i64::from(extent)]
    };
    let binders = limits
        .into_iter()
        .enumerate()
        .map(|(i, upper)| StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: i,
            display_name: format!("i{i}"),
            lower: 1,
            upper,
            step: 1,
        })
        .collect();
    dae.domains(|d| d.structured(StructuredIndexDomain { binders }, at))
}

fn guarded_updates<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    body: dae::FunctionLoop<'dae>,
    targets: [dae::FunctionValueId<'dae>; 2],
    input: dae::ExprId<'dae>,
    hint: dae::FunctionParameterId<'dae>,
    unknown: bool,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionLoop<'dae>, dae::DaeConstructionError> {
    let [x, k] = targets;
    let condition = guard_expression(dae, &body, hint, unknown, at)?;
    let (mut branch, finish) = dae.functions(|f| f.begin_branch_loop(body, at))?;
    let current = dae.functions(|f| f.read(&branch, k, at))?;
    let value = dae.expressions(|e| {
        let one = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
        e.at(at).binary(dae::BinaryOperator::Add, current, one)
    })?;
    dae.functions(|f| f.assign(&mut branch, k, value, at))?;
    let (index, current) =
        dae.functions(|f| Ok((f.read(&branch, k, at)?, f.read(&branch, x, at)?)))?;
    let updated = dae.expressions(|e| {
        let one = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
        let value = e.at(at).index(
            input,
            [dae::Subscript::Index {
                expression: one,
                provenance: at,
            }],
        )?;
        e.at(at).array_update(
            current,
            value,
            [dae::Subscript::Index {
                expression: index,
                provenance: at,
            }],
        )
    })?;
    dae.functions(|f| f.assign(&mut branch, x, updated, at))?;
    let (body, branch) = dae.functions(|f| f.finish_branch_loop(finish, branch))?;
    let (empty, finish) = dae.functions(|f| f.begin_branch_loop(body, at))?;
    let (mut body, fallback) = dae.functions(|f| f.finish_branch_loop(finish, empty))?;
    dae.functions(|f| {
        f.finish_conditional_loop(&mut body, vec![(condition, branch)], fallback, at)
    })?;
    Ok(body)
}

fn guard_expression<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionLoop<'dae>,
    hint: dae::FunctionParameterId<'dae>,
    unknown: bool,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let domain = body.domain();
    let (i, j) = dae.domains(|d| Ok((d.binder(domain, 0, at)?, d.binder(domain, 1, at)?)))?;
    dae.expressions(|e| {
        let i = e.at(at).binder(i)?;
        let j = if unknown {
            e.at(at).function_parameter(hint)?
        } else {
            e.at(at).binder(j)?
        };
        e.at(at).binary(dae::BinaryOperator::GreaterEqual, j, i)
    })
}

fn lower_guarded(extent: u32, unknown: bool) -> Result<Vec<gast::UserFunction>, String> {
    lower_dae(fixture_with_guard(
        Counter {
            extent,
            initial: 0,
            step: Some(1),
            reset_outside: false,
        },
        Some(unknown),
    ))
}

#[test]
fn asymmetric_binder_guard_counts_only_the_seven_executed_updates() {
    lower_guarded(7, false).unwrap();
}

#[test]
fn guarded_counter_cannot_hide_an_out_of_range_seventh_write() {
    assert!(lower_guarded(3, false).is_err());
}

#[test]
fn an_input_dependent_guard_cannot_issue_a_counter_fact() {
    assert!(lower_guarded(7, true).is_err());
}
