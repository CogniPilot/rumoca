//! Call owners for external functions with a compiler-defined body.
//!
//! The DAE proves an external interface against its SPEC_0040 DAE-C30 catalog
//! row once and records the argument each catalog input reads and the result
//! each catalog output writes. The owner program is that binding: it lowers
//! the arguments over the loaded parameters, issues one `Native` operation,
//! and stores its outputs, so no foreign code runs and nothing is re-checked.

use super::*;

pub(super) fn register_native_call<'dae>(
    table: &mut solve::SolvePureCallTableBuilder,
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
    binding: dae::NativeBodyView<'dae>,
    identity: solve::SolvePureCallIdentity,
    arithmetic: solve::SolveArithmeticProfile,
    provenance: rumoca_core::Span,
) -> Result<RegisteredCall<'dae>, solve::SolveProgramConstructionError> {
    let parameter_types = function.parameter_types().iter().collect::<Vec<_>>();
    let (inputs, parameter_ranges) = leaf_layout(view, &parameter_types, arithmetic)?;
    let result_types = function.result_types().iter().collect::<Vec<_>>();
    let (results, result_ranges) = leaf_layout(view, &result_types, arithmetic)?;
    let result_leaf_count = results.len();
    let outputs = results
        .into_iter()
        .map(solve::SolvePureCallOutput::result)
        .collect();
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
                conditional_groups: HashMap::new(),
                fold_parameters: HashMap::new(),
                fold_values: HashMap::new(),
                binders: HashMap::new(),
                callees: HashMap::new(),
                predicate_ranges: HashMap::new(),
                cache: HashMap::new(),
                call_values: HashMap::new(),
                predicate_values: Vec::new(),
                assertion_slots: std::sync::Arc::from(Vec::new()),
                next_direct_assertion: 0,
                direct_assertion_count: 0,
                totality: HashMap::new(),
            };
            let operands = binding
                .inputs()
                .map(|input| lowerer.expression(input)?.only_register(provenance))
                .collect::<Result<Vec<_>, _>>()?;
            let values = lowerer
                .builder
                .native(binding.body(), &operands, provenance)?;
            for (value, result) in values.into_iter().zip(binding.results()) {
                let leaf = result_ranges
                    .get(*result as usize)
                    .filter(|range| range.len() == 1)
                    .map(|range| range.start)
                    .ok_or(solve::SolveProgramConstructionError::InvalidCallInterface {
                        provenance,
                    })?;
                lowerer.builder.store(outputs[leaf], value, provenance)?;
            }
            Ok(())
        },
    )?;
    let site = table
        .call_site(owner)
        .ok_or(solve::SolveProgramConstructionError::UnknownCallOwner { provenance })?;
    Ok(RegisteredCall {
        callee: CalleeInterface {
            owner,
            result_ranges: result_ranges.into_boxed_slice(),
            result_leaf_count,
            assertion_slots: std::sync::Arc::from(Vec::new()),
            assertions: Box::new([]),
            recursive: false,
        },
        site,
    })
}

type LeafLayout = (Vec<solve::SolveValueType>, Vec<Range<usize>>);

pub(super) fn leaf_layout<'dae>(
    view: dae::DaeView<'dae>,
    types: &[dae::ValueTypeId<'dae>],
    arithmetic: solve::SolveArithmeticProfile,
) -> Result<LeafLayout, solve::SolveProgramConstructionError> {
    let mut leaves = Vec::new();
    let mut ranges = Vec::new();
    for value_type in types {
        let start = leaves.len();
        leaves.extend(lower_value_type_leaves(view, *value_type, arithmetic)?);
        ranges.push(start..leaves.len());
    }
    Ok((leaves, ranges))
}
