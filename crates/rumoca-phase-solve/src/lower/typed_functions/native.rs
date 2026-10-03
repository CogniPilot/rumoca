//! Call owners for external functions with a compiler-defined body.
//!
//! The DAE proves an external interface against its SPEC_0040 DAE-C30 catalog
//! row once and records which parameter each catalog input reads and which
//! result each catalog output writes. The owner program is that binding: it
//! loads the parameters, issues one `Native` operation, and stores its
//! outputs, so no foreign code runs and nothing is re-checked here.

use super::*;

pub(super) fn register_native_call<'dae>(
    table: &mut solve::SolvePureCallTableBuilder,
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
    binding: &dae::NativeBodyBinding,
    identity: solve::SolvePureCallIdentity,
    arithmetic: solve::SolveArithmeticProfile,
    provenance: rumoca_core::Span,
) -> Result<RegisteredCall<'dae>, solve::SolveProgramConstructionError> {
    let (inputs, parameter_ranges) = leaf_layout(view, function.parameter_types(), arithmetic)?;
    let (results, result_ranges) = leaf_layout(view, function.result_types(), arithmetic)?;
    let result_leaf_count = results.len();
    let outputs = results
        .into_iter()
        .map(solve::SolvePureCallOutput::result)
        .collect();
    let single = |ranges: &[Range<usize>], position: u32| {
        ranges
            .get(position as usize)
            .filter(|range| range.len() == 1)
            .map(|range| range.start)
            .ok_or(solve::SolveProgramConstructionError::InvalidCallInterface { provenance })
    };
    let owner = table.add_owner(
        identity,
        inputs,
        outputs,
        provenance,
        |builder, inputs, outputs| {
            let operands = binding
                .parameters()
                .iter()
                .map(|parameter| {
                    let leaf = single(&parameter_ranges, *parameter)?;
                    builder.load(inputs[leaf], provenance)
                })
                .collect::<Result<Vec<_>, _>>()?;
            let values = builder.native(binding.body(), &operands, provenance)?;
            for (value, result) in values.into_iter().zip(binding.results()) {
                let leaf = single(&result_ranges, *result)?;
                builder.store(outputs[leaf], value, provenance)?;
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
        assertion_slots: std::sync::Arc::from(Vec::new()),
        assertions: Box::new([]),
    })
}

fn leaf_layout<'dae>(
    view: dae::DaeView<'dae>,
    types: dae::ValueTypeOperands<'dae>,
    arithmetic: solve::SolveArithmeticProfile,
) -> Result<(Vec<solve::SolveValueType>, Vec<Range<usize>>), solve::SolveProgramConstructionError> {
    let mut leaves = Vec::new();
    let mut ranges = Vec::new();
    for value_type in types.iter() {
        let start = leaves.len();
        leaves.extend(lower_value_type_leaves(view, value_type, arithmetic)?);
        ranges.push(start..leaves.len());
    }
    Ok((leaves, ranges))
}
