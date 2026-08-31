use rumoca_core::{OperationContractKey, SourceMap, Span, VarName};
use rumoca_ir_dae::{
    Dae, DaeConstruction, DaeConstructionError, DaeLiteral, DaeProvenance, FunctionId,
    FunctionReservation, FunctionSignature, ScalarType, ValueType,
};

use crate::{CallableOperationSourceView, CallablePlan, PlanConstructionError};

#[derive(Clone, Copy)]
enum CallMutation {
    None,
    PermutedProjectionList,
    WrongOutputMetadata,
    ForeignScope,
}

/// `function triple output Real a, b, c; algorithm a := 1.0; ... end triple;`
fn define_triple<'function, 'dae>(
    dae: &mut DaeConstruction<'dae>,
    reservation: FunctionReservation<'function, 'dae>,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let destinations = (0..3)
        .map(|ordinal| {
            dae.functions(|functions| {
                functions.output(
                    &reservation,
                    VarName::new(format!("r{ordinal}")),
                    ordinal,
                    at,
                )
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let values = [1.0, 2.0, 3.0]
        .into_iter()
        .map(|value| {
            dae.expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(value)))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
    for (destination, value) in destinations.into_iter().zip(values) {
        dae.functions(|functions| functions.assign(&mut body, destination, value, at))?;
    }
    dae.functions(|functions| functions.define(body, at))?;
    Ok(())
}

/// One caller that stores exactly the requested `triple` output projections.
fn define_selected_output_caller<'function, 'dae>(
    dae: &mut DaeConstruction<'dae>,
    reservation: FunctionReservation<'function, 'dae>,
    triple: FunctionId<'dae>,
    outputs: &[usize],
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let destinations = (0..outputs.len())
        .map(|ordinal| {
            dae.functions(|functions| {
                functions.output(
                    &reservation,
                    VarName::new(format!("y{ordinal}")),
                    ordinal,
                    at,
                )
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let projections = dae.expressions(|expressions| {
        expressions
            .at(at)
            .call_results(triple, outputs.iter().copied(), [])
    })?;
    let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
    for (destination, projection) in destinations.into_iter().zip(projections) {
        dae.functions(|functions| functions.assign(&mut body, destination, projection, at))?;
    }
    dae.functions(|functions| functions.define(body, at))?;
    Ok(())
}

fn selected_output_call(outputs: &[usize]) -> Dae {
    let mut source_map = SourceMap::new();
    let source = source_map.add(
        "callable-selected-call.mo",
        "function triple output Real a; output Real b; output Real c; end triple; function caller output Real y; end caller;",
    );
    let span = Span::from_offsets(source, 0, 1);
    let at = DaeProvenance::source(span).expect("test span is source-backed");
    Dae::construct(source_map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        let (triple, ()) = dae.function(
            FunctionSignature::new(VarName::new("triple"), [], [real, real, real], at),
            |dae, reservation| define_triple(dae, reservation, at),
        )?;
        dae.function(
            FunctionSignature::new(
                VarName::new("caller"),
                [],
                std::iter::repeat_n(real, outputs.len()),
                at,
            ),
            |dae, reservation| define_selected_output_caller(dae, reservation, triple, outputs, at),
        )?;
        Ok(())
    })
    .expect("selected-output call fixture is construction-valid")
}

fn construct_selected_call(
    outputs: &[usize],
    mutation: CallMutation,
) -> Result<CallablePlan, PlanConstructionError> {
    let encoded = serde_json::to_string(&selected_output_call(outputs))
        .expect("selected-output call fixture encodes");
    let dae = serde_json::from_str(&encoded).expect("current replay reissues call inventory");
    CallablePlan::construct::<PlanConstructionError, _>(dae, |dae, construction| {
        let functions = construction.functions().collect::<Vec<_>>();
        let triple = functions
            .iter()
            .copied()
            .find(|source| dae.exact_function(source.function()).name().as_str() == "triple")
            .expect("triple source");
        let caller = functions
            .iter()
            .copied()
            .find(|source| dae.exact_function(source.function()).name().as_str() == "caller")
            .expect("caller source");
        let definitions = construction.definitions().collect::<Vec<_>>();
        let expressions = construction.expressions().collect::<Vec<_>>();
        let call = construction.calls().next().expect("one call occurrence");
        let triple_owner = construction.add_owner(triple)?;
        let caller_owner = construction.add_owner(caller)?;
        let triple_scope = construction.root_scope(triple_owner);
        let caller_scope = construction.root_scope(caller_owner);

        for definition in definitions
            .iter()
            .copied()
            .filter(|definition| definition.function() == triple.function())
        {
            let rhs = dae.exact_function_definition(definition.definition()).rhs();
            let source = expressions
                .iter()
                .copied()
                .find(|source| source.function() == triple.function() && source.expression() == rhs)
                .expect("triple result literal");
            let value = construction.add_value_operation(
                triple_scope,
                source,
                OperationContractKey::Constant,
                &[],
            )?;
            construction.add_store(triple_scope, definition, value)?;
        }

        match mutation {
            CallMutation::PermutedProjectionList => {
                construction.calls[0].projections.swap(0, 1);
            }
            CallMutation::WrongOutputMetadata => {
                construction.calls[0].projections[1].output =
                    construction.calls[0].projections[0].output;
            }
            CallMutation::ForeignScope => {
                construction.add_call(triple_scope, call, &[])?;
                return Ok(());
            }
            CallMutation::None => {}
        }

        let projection_sources = construction.call_projections(call)?.collect::<Vec<_>>();
        let results = construction.add_call(caller_scope, call, &[])?;
        for (projection, value) in projection_sources.into_iter().zip(results) {
            let definition = definitions
                .iter()
                .copied()
                .find(|definition| {
                    definition.function() == caller.function()
                        && dae.exact_function_definition(definition.definition()).rhs()
                            == projection.expression()
                })
                .expect("caller result definition");
            construction.add_store(caller_scope, definition, value)?;
        }
        Ok(())
    })
}

fn assert_retained_outputs(plan: &CallablePlan, expected: &[u32]) {
    assert_eq!(plan.counters().source_calls(), 1);
    plan.inspect(|view| {
        let call = view
            .operations()
            .find(|operation| operation.contract() == OperationContractKey::Call)
            .expect("one call operation");
        let CallableOperationSourceView::CallOccurrence(source) = call.source() else {
            panic!("call operation retains its issued occurrence")
        };
        assert_eq!(
            source
                .projections()
                .map(|projection| projection.output())
                .collect::<Vec<_>>(),
            expected
        );
        assert_eq!(call.results().len(), expected.len());
        assert_eq!(source.call().ordinal(), 0);
    });
}

#[test]
fn discarded_output_zero_and_sparse_requested_outputs_remain_exact() {
    let output_one =
        construct_selected_call(&[1], CallMutation::None).expect("a call may discard output zero");
    assert_retained_outputs(&output_one, &[1]);

    let sparse = construct_selected_call(&[0, 2], CallMutation::None)
        .expect("a call may request a sparse ordered result tuple");
    assert_retained_outputs(&sparse, &[0, 2]);

    let repeated = construct_selected_call(&[1, 1], CallMutation::None)
        .expect("two distinct projections may request the same output ordinal");
    assert_retained_outputs(&repeated, &[1, 1]);
}

#[test]
fn permuted_wrong_output_and_foreign_call_occurrences_refuse() {
    for mutation in [
        CallMutation::PermutedProjectionList,
        CallMutation::WrongOutputMetadata,
        CallMutation::ForeignScope,
    ] {
        assert!(matches!(
            construct_selected_call(&[0, 2], mutation),
            Err(PlanConstructionError::InvalidOperation { .. }
                | PlanConstructionError::InvalidSourceOccurrence { .. })
        ));
    }
}
