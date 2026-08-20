//! Typed structured-region capture and reconstruction.

use std::{collections::HashMap, ops::Range};

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{
    ConditionalDefinitionGroup, ExpressionLowerer, LoweredValue, ModelCoordinateKey, RegisteredCall,
};

#[derive(Clone)]
pub(super) struct EnvironmentLayout<'dae> {
    pub(super) model_coordinates: Vec<(
        ModelCoordinateKey<'dae>,
        dae::ValueTypeId<'dae>,
        Range<usize>,
    )>,
    pub(super) parameters: Vec<(
        dae::FunctionParameterId<'dae>,
        dae::ValueTypeId<'dae>,
        Range<usize>,
    )>,
    pub(super) values: Vec<(
        dae::FunctionDefinitionId<'dae>,
        dae::ValueTypeId<'dae>,
        Range<usize>,
    )>,
    pub(super) fold_parameters: Vec<(
        dae::FunctionFoldId<'dae>,
        u32,
        dae::ValueTypeId<'dae>,
        Range<usize>,
    )>,
    pub(super) binders: Vec<((u32, u32), Range<usize>)>,
}

#[derive(Clone)]
pub(super) struct RegionContext<'dae> {
    pub(super) view: dae::DaeView<'dae>,
    pub(super) callees: HashMap<dae::ExprId<'dae>, RegisteredCall<'dae>>,
    pub(super) predicate_ranges: HashMap<dae::ExprId<'dae>, Range<usize>>,
    pub(super) conditional_groups:
        HashMap<dae::FunctionDefinitionId<'dae>, ConditionalDefinitionGroup<'dae>>,
    pub(super) predicate_count: usize,
    pub(super) direct_assertion_count: usize,
}

/// One value a region publishes, paired with the type the target it lands on
/// declares.
///
/// The region's output slots are typed from those declarations, so a branch
/// value only becomes a region output after it is taken to that type. Carrying
/// the pair as one value is what makes the two impossible to separate: there is
/// no expression here that some caller could publish without its target type.
pub(super) type RegionOutput<'dae> = (dae::ValueTypeId<'dae>, dae::ExprId<'dae>);

pub(super) struct RegionValues<'borrow, 'dae> {
    pub(super) results: &'borrow [RegionOutput<'dae>],
    pub(super) pending_predicates: &'borrow [usize],
    pub(super) provenance: rumoca_core::Span,
}

pub(super) struct RegionConditional<'dae> {
    pub(super) value_type: dae::ValueTypeId<'dae>,
    pub(super) operands: Vec<dae::ExprId<'dae>>,
    pub(super) pending: Vec<usize>,
    pub(super) provenance: rumoca_core::Span,
}

pub(super) fn function_value_type<'dae>(
    view: dae::DaeView<'dae>,
    value: dae::FunctionValueId<'dae>,
    provenance: rumoca_core::Span,
) -> Result<dae::ValueTypeId<'dae>, solve::SolveProgramConstructionError> {
    view.function(value.function())
        .and_then(|function| {
            function
                .values()
                .find(|candidate| candidate.id() == value)
                .map(|candidate| candidate.value_type())
        })
        .ok_or(solve::SolveProgramConstructionError::InvalidCallInterface { provenance })
}

pub(super) fn lower_region_values<'program, 'dae>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    inputs: &[solve::ProgramSlot<'program>],
    outputs: &[solve::ProgramSlot<'program>],
    environment: &EnvironmentLayout<'dae>,
    context: &RegionContext<'dae>,
    region: RegionValues<'_, 'dae>,
) -> Result<(), solve::SolveProgramConstructionError> {
    let RegionValues {
        results,
        pending_predicates,
        provenance,
    } = region;
    let mut lowerer = load_region_lowerer(builder, inputs, environment, context, provenance)?;
    let mut values = Vec::new();
    for (value_type, expression) in results {
        // Publishing at the target's declared type is the same rule the
        // in-order statement path applies; a correlated group's branch is just
        // that definition's right-hand side evaluated under a condition. The
        // region's output slot was typed from that declaration, so the branch
        // value has to reach it as that type - an Integer branch under a Real
        // target converts here, exactly as the ordinary statement would.
        let value = lowerer.expression(*expression)?;
        let value = lowerer.coerce_value(value, *value_type, provenance)?;
        values.extend(value.leaves);
    }
    for slot in pending_predicates {
        let predicate = match lowerer.predicate_values.get(*slot).copied().flatten() {
            Some(predicate) => predicate,
            None => lowerer
                .builder
                .constant(solve::SolveValue::boolean(true), provenance)?,
        };
        values.push(predicate);
    }
    if values.len() != outputs.len() {
        return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
    }
    for (output, value) in outputs.iter().zip(values) {
        lowerer.builder.store(*output, value, provenance)?;
    }
    Ok(())
}

pub(super) fn load_region_lowerer<'builder, 'program, 'dae>(
    builder: &'builder mut solve::TypedProgramBuilder<'program>,
    inputs: &[solve::ProgramSlot<'program>],
    environment: &EnvironmentLayout<'dae>,
    context: &RegionContext<'dae>,
    provenance: rumoca_core::Span,
) -> Result<ExpressionLowerer<'builder, 'program, 'dae>, solve::SolveProgramConstructionError> {
    let loaded = inputs
        .iter()
        .map(|input| builder.load(*input, provenance))
        .collect::<Result<Vec<_>, _>>()?;
    let parameters = environment
        .parameters
        .iter()
        .map(|(id, value_type, range)| {
            (
                *id,
                LoweredValue {
                    value_type: *value_type,
                    leaves: loaded[range.clone()].to_vec(),
                },
            )
        })
        .collect();
    let model_coordinates = environment
        .model_coordinates
        .iter()
        .map(|(key, value_type, range)| {
            (
                *key,
                LoweredValue {
                    value_type: *value_type,
                    leaves: loaded[range.clone()].to_vec(),
                },
            )
        })
        .collect();
    let function_values = environment
        .values
        .iter()
        .map(|(id, value_type, range)| {
            (
                *id,
                LoweredValue {
                    value_type: *value_type,
                    leaves: loaded[range.clone()].to_vec(),
                },
            )
        })
        .collect();
    let fold_parameters = environment
        .fold_parameters
        .iter()
        .map(|(fold, carried, value_type, range)| {
            (
                (*fold, *carried),
                LoweredValue {
                    value_type: *value_type,
                    leaves: loaded[range.clone()].to_vec(),
                },
            )
        })
        .collect();
    let binders = environment
        .binders
        .iter()
        .map(|(binder, range)| {
            let [register] = &loaded[range.clone()] else {
                unreachable!("checked binder capture owns one scalar register")
            };
            (*binder, *register)
        })
        .collect();
    Ok(ExpressionLowerer {
        view: context.view,
        builder,
        model_coordinates,
        parameters,
        function_values,
        conditional_groups: context.conditional_groups.clone(),
        fold_parameters,
        fold_values: HashMap::new(),
        binders,
        callees: context.callees.clone(),
        predicate_ranges: context.predicate_ranges.clone(),
        cache: HashMap::new(),
        call_values: HashMap::new(),
        predicate_values: vec![None; context.predicate_count],
        next_direct_assertion: 0,
        direct_assertion_count: context.direct_assertion_count,
    })
}

pub(super) fn lower_region_conditional<'program, 'dae>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    inputs: &[solve::ProgramSlot<'program>],
    outputs: &[solve::ProgramSlot<'program>],
    environment: &EnvironmentLayout<'dae>,
    context: &RegionContext<'dae>,
    region: RegionConditional<'dae>,
) -> Result<(), solve::SolveProgramConstructionError> {
    let RegionConditional {
        value_type,
        operands,
        pending,
        provenance,
    } = region;
    let mut lowerer = load_region_lowerer(builder, inputs, environment, context, provenance)?;
    let value = if let [fallback] = operands.as_slice() {
        lowerer.expression(*fallback)?
    } else {
        lowerer.conditional(value_type, &operands, provenance)?
    };
    let value = lowerer.coerce_value(value, value_type, provenance)?;
    let mut values = value.leaves;
    for slot in pending {
        let predicate = match lowerer.predicate_values.get(slot).copied().flatten() {
            Some(predicate) => predicate,
            None => lowerer
                .builder
                .constant(solve::SolveValue::boolean(true), provenance)?,
        };
        values.push(predicate);
    }
    if values.len() != outputs.len() {
        return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
    }
    for (output, source) in outputs.iter().zip(values) {
        lowerer.builder.store(*output, source, provenance)?;
    }
    Ok(())
}

// SPEC_0021: Exception - this is the explicit checked structured-region ABI;
// each argument is a distinct construction proof input rather than optional
// behavioral configuration.
// SPEC_0021: Exception - validated boundary keeps proof-relevant inputs explicit.
#[allow(clippy::too_many_arguments)]
pub(super) fn lower_region_assignment_chain<'program, 'dae>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    inputs: &[solve::ProgramSlot<'program>],
    outputs: &[solve::ProgramSlot<'program>],
    environment: &EnvironmentLayout<'dae>,
    context: &RegionContext<'dae>,
    value_types: Vec<dae::ValueTypeId<'dae>>,
    conditions: Vec<dae::ExprId<'dae>>,
    branches: Vec<Vec<dae::ExprId<'dae>>>,
    fallback: Vec<dae::ExprId<'dae>>,
    pending: Vec<usize>,
    provenance: rumoca_core::Span,
) -> Result<(), solve::SolveProgramConstructionError> {
    if conditions.is_empty() {
        if value_types.len() != fallback.len() {
            return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
        }
        let results = value_types
            .into_iter()
            .zip(fallback)
            .collect::<Vec<RegionOutput<'dae>>>();
        return lower_region_values(
            builder,
            inputs,
            outputs,
            environment,
            context,
            RegionValues {
                results: &results,
                pending_predicates: &pending,
                provenance,
            },
        );
    }
    let mut lowerer = load_region_lowerer(builder, inputs, environment, context, provenance)?;
    let values = lowerer.assignment_conditional_chain(
        &value_types,
        &conditions,
        &branches,
        &fallback,
        &pending,
        provenance,
    )?;
    if values.len() != outputs.len() {
        return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
    }
    for (output, value) in outputs.iter().zip(values) {
        lowerer.builder.store(*output, value, provenance)?;
    }
    Ok(())
}
