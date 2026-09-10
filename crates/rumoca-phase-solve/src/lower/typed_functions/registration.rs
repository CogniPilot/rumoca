//! Construct one acyclic call-owner graph before exposing any projection.

use super::*;

pub(super) fn register_call<'dae>(
    table: &mut solve::SolvePureCallTableBuilder,
    view: dae::DaeView<'dae>,
    call: dae::ExprId<'dae>,
    identity: solve::SolvePureCallIdentity,
    arithmetic: solve::SolveArithmeticProfile,
    identities: &mut NestedIdentityIssuer,
    active: &mut Vec<dae::FunctionId<'dae>>,
) -> Result<RegisteredCall<'dae>, solve::SolveProgramConstructionError> {
    let node = view
        .expression(call)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    let dae::ExpressionOperation::Call { function, .. } = node.operation() else {
        return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
            provenance: node.provenance().span(),
        });
    };
    // SOLVE-C51 admits finite acyclic owners, independently of runtime arguments.
    if active.contains(&function) {
        return Err(solve::SolveProgramConstructionError::RecursiveCall {
            provenance: node.provenance().span(),
        });
    }
    active.push(function);
    let result = register_call_body(table, view, call, identity, arithmetic, identities, active);
    active.pop();
    result
}

// SPEC_0021: Exception - top-level typed-call owner construction entry point.
#[allow(clippy::too_many_lines)]
fn register_call_body<'dae>(
    table: &mut solve::SolvePureCallTableBuilder,
    view: dae::DaeView<'dae>,
    call: dae::ExprId<'dae>,
    identity: solve::SolvePureCallIdentity,
    arithmetic: solve::SolveArithmeticProfile,
    identities: &mut NestedIdentityIssuer,
    active: &mut Vec<dae::FunctionId<'dae>>,
) -> Result<RegisteredCall<'dae>, solve::SolveProgramConstructionError> {
    let call_node = view
        .expression(call)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    let dae::ExpressionOperation::Call { function, .. } = call_node.operation() else {
        return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
            provenance: call_node.provenance().span(),
        });
    };
    let function = view
        .function(function)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    if function.is_external() {
        return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
            provenance: call_node.provenance().span(),
        });
    }
    let assertions = assertion_conditions(view, function)?;
    let conditional_groups = conditional_definition_groups(function.statements())?;
    let nested_call_ids = nested_calls(view, function, &assertions);
    let mut callees = HashMap::new();
    for nested_call in nested_call_ids.iter().copied() {
        let nested_node = view
            .expression(nested_call)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        let nested_identity = identities.issue(nested_node.provenance().span())?;
        let registered = register_call(
            table,
            view,
            nested_call,
            nested_identity,
            arithmetic,
            identities,
            active,
        )?;
        callees.insert(nested_call, registered);
    }
    let mut predicate_ranges = HashMap::new();
    let mut next_predicate = assertions.len();
    for nested_call in &nested_call_ids {
        let call = callees.get(nested_call).ok_or(
            solve::SolveProgramConstructionError::UnknownCallOwner {
                provenance: call_node.provenance().span(),
            },
        )?;
        let end = next_predicate.checked_add(call.assertion_count).ok_or(
            solve::SolveProgramConstructionError::IdentityOverflow {
                provenance: call_node.provenance().span(),
            },
        )?;
        predicate_ranges.insert(*nested_call, next_predicate..end);
        next_predicate = end;
    }
    let parameter_types = function.parameter_types().iter().collect::<Vec<_>>();
    let mut inputs = Vec::new();
    let mut parameter_ranges = Vec::with_capacity(parameter_types.len());
    for value_type in &parameter_types {
        let start = inputs.len();
        inputs.extend(lower_value_type_leaves(view, *value_type, arithmetic)?);
        parameter_ranges.push(start..inputs.len());
    }
    let result_types = function.result_types().iter().collect::<Vec<_>>();
    let mut result_leaf_types = Vec::new();
    let mut result_ranges = Vec::with_capacity(result_types.len());
    for value_type in &result_types {
        let start = result_leaf_types.len();
        result_leaf_types.extend(lower_value_type_leaves(view, *value_type, arithmetic)?);
        result_ranges.push(start..result_leaf_types.len());
    }
    let result_leaf_count = result_leaf_types.len();
    let assertion_count = callees
        .values()
        .try_fold(assertions.len(), |count, call| {
            count.checked_add(call.assertion_count)
        })
        .ok_or(solve::SolveProgramConstructionError::IdentityOverflow {
            provenance: call_node.provenance().span(),
        })?;
    let mut registered_assertions = assertions
        .iter()
        .enumerate()
        .map(|(index, assertion)| RegisteredAssertion {
            predicate_output: result_leaf_count + index,
            message: assertion.message,
            provenance: assertion.provenance,
        })
        .collect::<Vec<_>>();
    let mut parent_predicate = assertions.len();
    for nested_call in &nested_call_ids {
        let nested = callees.get(nested_call).ok_or(
            solve::SolveProgramConstructionError::UnknownCallOwner {
                provenance: call_node.provenance().span(),
            },
        )?;
        for assertion in nested.assertions.iter() {
            registered_assertions.push(RegisteredAssertion {
                predicate_output: result_leaf_count + parent_predicate,
                message: assertion.message,
                provenance: assertion.provenance,
            });
            parent_predicate += 1;
        }
    }
    let mut outputs = result_leaf_types
        .iter()
        .cloned()
        .map(solve::SolvePureCallOutput::result)
        .collect::<Vec<_>>();
    outputs.extend((0..assertion_count).map(|_| solve::SolvePureCallOutput::assertion_predicate()));
    let provenance = call_node.provenance().span();
    let owner = table.add_owner(
        identity,
        inputs,
        outputs,
        provenance,
        |builder, inputs, outputs| {
            let mut parameters = HashMap::new();
            for ((parameter, value_type), range) in function
                .parameters()
                .zip(parameter_types.iter().copied())
                .zip(&parameter_ranges)
            {
                let leaves = inputs[range.clone()]
                    .iter()
                    .map(|input| builder.load(*input, provenance))
                    .collect::<Result<Vec<_>, _>>()?;
                parameters.insert(parameter.id(), LoweredValue { value_type, leaves });
            }
            let mut lowerer = ExpressionLowerer {
                view,
                builder,
                model_coordinates: HashMap::new(),
                parameters,
                function_values: HashMap::new(),
                conditional_groups,
                fold_parameters: HashMap::new(),
                fold_values: HashMap::new(),
                binders: HashMap::new(),
                callees,
                predicate_ranges,
                cache: HashMap::new(),
                call_values: HashMap::new(),
                predicate_values: vec![None; assertion_count],
                next_direct_assertion: 0,
                direct_assertion_count: assertions.len(),
            };
            lowerer.statements(function.statements())?;
            for ((definition, value_type), range) in function
                .result_values()
                .iter()
                .zip(result_types.iter().copied())
                .zip(&result_ranges)
            {
                let value = lowerer
                    .function_values
                    .get(&definition.id())
                    .cloned()
                    .ok_or(solve::SolveProgramConstructionError::InvalidCallOutput {
                        provenance: definition.provenance().span(),
                    })?;
                if value.value_type != value_type || value.leaves.len() != range.len() {
                    return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                        provenance: definition.provenance().span(),
                    });
                }
                for (output, source) in outputs[range.clone()].iter().zip(value.leaves) {
                    lowerer
                        .builder
                        .store(*output, source, definition.provenance().span())?;
                }
            }
            if lowerer.predicate_values.iter().any(Option::is_none) {
                return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
            }
            for (predicate, output) in lowerer
                .predicate_values
                .into_iter()
                .map(Option::unwrap)
                .zip(&outputs[result_leaf_count..])
            {
                lowerer.builder.store(*output, predicate, provenance)?;
            }
            Ok(())
        },
    )?;
    let site = table
        .call_site(owner)
        .ok_or(solve::SolveProgramConstructionError::UnknownCallOwner { provenance })?;
    Ok(RegisteredCall {
        owner,
        site,
        result_ranges: result_ranges.into_boxed_slice(),
        result_leaf_count,
        assertion_count,
        assertions: registered_assertions.into_boxed_slice(),
    })
}
