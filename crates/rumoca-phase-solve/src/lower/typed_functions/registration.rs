//! Construct one owner graph before exposing any projection: acyclic except
//! for SOLVE-C62 recursive owner groups.

use super::*;

pub(super) fn register_call<'dae>(
    table: &mut solve::SolvePureCallTableBuilder,
    view: dae::DaeView<'dae>,
    call: dae::ExprId<'dae>,
    identity: Option<solve::SolvePureCallIdentity>,
    arithmetic: solve::SolveArithmeticProfile,
    identities: &mut CallRegistration<'dae>,
    active: &mut Vec<dae::FunctionId<'dae>>,
) -> Result<RegisteredCall<'dae>, solve::SolveProgramConstructionError> {
    let node = view
        .expression(call)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    let dae::ExpressionOperation::Call {
        function, owner, ..
    } = node.operation()
    else {
        return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
            provenance: node.provenance().span(),
        });
    };
    if let Some(registered) = identities.calls.get(&owner) {
        return Ok(registered.clone());
    }
    // SOLVE-C51 admits finite owners, independently of runtime arguments: an
    // owner under construction is re-entered only through a SOLVE-C62 group,
    // whose members are issued before this point.
    if active.contains(&function) {
        return Err(solve::SolveProgramConstructionError::RecursiveCall {
            provenance: node.provenance().span(),
        });
    }
    recursive_groups::register_group_of(table, view, function, arithmetic, identities, active)?;
    if let Some(registered) = identities.calls.get(&owner) {
        return Ok(registered.clone());
    }
    let identity = match identity {
        Some(identity) => identity,
        None => identities.issue(node.provenance().span())?,
    };
    active.push(function);
    let result = register_call_body(table, view, owner, identity, arithmetic, identities, active);
    active.pop();
    if let Ok(registered) = &result {
        identities.calls.insert(owner, registered.clone());
    }
    result
}

fn register_call_body<'dae>(
    table: &mut solve::SolvePureCallTableBuilder,
    view: dae::DaeView<'dae>,
    call: dae::ExprId<'dae>,
    identity: solve::SolvePureCallIdentity,
    arithmetic: solve::SolveArithmeticProfile,
    identities: &mut CallRegistration<'dae>,
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
    let provenance = call_node.provenance().span();
    if let Some(external) = function.external() {
        // SPEC_0040 DAE-C30: only a proven native body gives a foreign body
        // an executable meaning; every other external interface is refused.
        let binding = external
            .native_body()
            .ok_or(solve::SolveProgramConstructionError::InvalidCallInterface { provenance })?;
        return native::register_native_call(
            table, view, function, binding, identity, arithmetic, provenance,
        );
    }
    let assertions = assertion_conditions(view, function)?;
    let nested_call_ids = nested_calls(view, function, &assertions);
    let mut callees = HashMap::new();
    for nested_call in nested_call_ids.iter().copied() {
        let registered = register_call(
            table,
            view,
            nested_call,
            None,
            arithmetic,
            identities,
            active,
        )?;
        callees.insert(nested_call, registered.callee);
    }
    let interface = FunctionInterface::lower(view, function, arithmetic)?;
    let layout = assertion_layout(
        view,
        &assertions,
        &nested_call_ids,
        &callees,
        interface.result_leaf_count(),
        arithmetic,
    )?;
    let body = OwnerBody {
        function,
        interface: &interface,
        callees,
        assertion_count: assertions.len(),
        layout,
    };
    let owner = table.add_owner(
        identity,
        body.interface.inputs.clone(),
        body.outputs(),
        provenance,
        |builder, inputs, outputs| body.lower(view, builder, inputs, outputs, provenance),
    )?;
    let site = table
        .call_site(owner)
        .ok_or(solve::SolveProgramConstructionError::UnknownCallOwner { provenance })?;
    Ok(RegisteredCall {
        callee: body.callee(owner),
        site,
    })
}

/// The typed leaf interface of one Modelica function.
pub(super) struct FunctionInterface<'dae> {
    parameter_types: Vec<dae::ValueTypeId<'dae>>,
    pub(super) inputs: Vec<solve::SolveValueType>,
    parameter_ranges: Vec<Range<usize>>,
    result_types: Vec<dae::ValueTypeId<'dae>>,
    pub(super) results: Vec<solve::SolveValueType>,
    pub(super) result_ranges: Vec<Range<usize>>,
}

impl<'dae> FunctionInterface<'dae> {
    pub(super) fn lower(
        view: dae::DaeView<'dae>,
        function: dae::FunctionView<'dae>,
        arithmetic: solve::SolveArithmeticProfile,
    ) -> Result<Self, solve::SolveProgramConstructionError> {
        let parameter_types = function.parameter_types().iter().collect::<Vec<_>>();
        let (inputs, parameter_ranges) = native::leaf_layout(view, &parameter_types, arithmetic)?;
        let result_types = function.result_types().iter().collect::<Vec<_>>();
        let (results, result_ranges) = native::leaf_layout(view, &result_types, arithmetic)?;
        Ok(Self {
            parameter_types,
            inputs,
            parameter_ranges,
            result_types,
            results,
            result_ranges,
        })
    }

    pub(super) fn result_leaf_count(&self) -> usize {
        self.results.len()
    }
}

/// Everything one owner body lowers from: the function, its typed interface,
/// the owners its nested calls name, and its assertion output layout.
pub(super) struct OwnerBody<'a, 'dae> {
    pub(super) function: dae::FunctionView<'dae>,
    pub(super) interface: &'a FunctionInterface<'dae>,
    pub(super) callees: HashMap<dae::ExprId<'dae>, CalleeInterface<'dae>>,
    pub(super) assertion_count: usize,
    pub(super) layout: AssertionLayout<'dae>,
}

impl<'dae> OwnerBody<'_, 'dae> {
    /// Result leaves, then every assertion slot, in owner output order.
    pub(super) fn outputs(&self) -> Vec<solve::SolvePureCallOutput> {
        self.interface
            .results
            .iter()
            .cloned()
            .map(solve::SolvePureCallOutput::result)
            .chain(self.layout.slots.iter().map(AssertionSlot::output))
            .collect()
    }

    pub(super) fn callee(&self, owner: solve::SolvePureCallOwnerId) -> CalleeInterface<'dae> {
        CalleeInterface {
            owner,
            result_ranges: self.interface.result_ranges.clone().into_boxed_slice(),
            result_leaf_count: self.interface.result_leaf_count(),
            assertion_slots: std::sync::Arc::from(self.layout.slots.clone()),
            assertions: self.layout.registered.clone().into_boxed_slice(),
            recursive: self.callees.values().any(|callee| callee.recursive),
        }
    }

    pub(super) fn lower<'program>(
        &self,
        view: dae::DaeView<'dae>,
        builder: &mut solve::TypedProgramBuilder<'program>,
        inputs: &[solve::ProgramSlot<'program>],
        outputs: &[solve::ProgramSlot<'program>],
        provenance: rumoca_core::Span,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let function = self.function;
        let interface = self.interface;
        let mut parameters = HashMap::new();
        for ((parameter, value_type), range) in function
            .parameters()
            .zip(interface.parameter_types.iter().copied())
            .zip(&interface.parameter_ranges)
        {
            let leaves = inputs[range.clone()]
                .iter()
                .map(|input| builder.load(*input, provenance))
                .collect::<Result<Vec<_>, _>>()?;
            parameters.insert(parameter.id(), LoweredValue { value_type, leaves });
        }
        let assertion_slots = std::sync::Arc::<[AssertionSlot]>::from(self.layout.slots.clone());
        let mut lowerer = ExpressionLowerer {
            view,
            builder,
            model_coordinates: HashMap::new(),
            parameters,
            function_values: HashMap::new(),
            conditional_groups: conditional_definition_groups(function.statements())?,
            fold_parameters: HashMap::new(),
            fold_values: HashMap::new(),
            binders: HashMap::new(),
            callees: self.callees.clone(),
            predicate_ranges: self.layout.predicate_ranges.clone(),
            cache: HashMap::new(),
            call_values: HashMap::new(),
            predicate_values: vec![None; assertion_slots.len()],
            assertion_slots,
            next_direct_assertion: 0,
            direct_assertion_count: self.assertion_count,
            totality: HashMap::new(),
        };
        lowerer.statements(function.statements())?;
        for ((definition, value_type), range) in function
            .result_values()
            .iter()
            .zip(interface.result_types.iter().copied())
            .zip(&interface.result_ranges)
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
        for &(value, slot) in &self.layout.direct_message_values {
            let at = view
                .expression(value)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .provenance()
                .span();
            let register = lowerer.expression(value)?.only_register(at)?;
            if lowerer.predicate_values[slot].replace(register).is_some() {
                return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                    provenance: at,
                });
            }
        }
        if lowerer.predicate_values.iter().any(Option::is_none) {
            return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
        }
        let result_leaf_count = interface.result_leaf_count();
        for (predicate, output) in lowerer
            .predicate_values
            .into_iter()
            .map(Option::unwrap)
            .zip(&outputs[result_leaf_count..])
        {
            lowerer.builder.store(*output, predicate, provenance)?;
        }
        Ok(())
    }
}

/// Assertion outputs of one owner after its result leaves.
pub(super) struct AssertionLayout<'dae> {
    slots: Vec<AssertionSlot>,
    predicate_ranges: HashMap<dae::ExprId<'dae>, Range<usize>>,
    /// Each message value of an assertion this function declares, with the
    /// slot its frame publishes it in.
    direct_message_values: Vec<(dae::ExprId<'dae>, usize)>,
    registered: Vec<RegisteredAssertion<'dae>>,
}

/// Lay out the owner's own predicates, then its own message values, then each
/// nested call's complete slot tuple, so every nested assertion keeps the
/// predicate and message values its callee's frame produced.
pub(super) fn assertion_layout<'dae>(
    view: dae::DaeView<'dae>,
    assertions: &[assertions::FunctionAssertion<'dae>],
    nested_call_ids: &[dae::ExprId<'dae>],
    callees: &HashMap<dae::ExprId<'dae>, CalleeInterface<'dae>>,
    result_leaf_count: usize,
    arithmetic: solve::SolveArithmeticProfile,
) -> Result<AssertionLayout<'dae>, solve::SolveProgramConstructionError> {
    let mut slots = vec![AssertionSlot::Predicate; assertions.len()];
    let mut direct_message_values = Vec::new();
    let mut registered = Vec::with_capacity(assertions.len());
    for (index, assertion) in assertions.iter().enumerate() {
        let mut message_values = Vec::new();
        for value in assertions::message_values(view, assertion) {
            let Some(value_type) = message_value_type(view, value, arithmetic)? else {
                continue;
            };
            direct_message_values.push((value, slots.len()));
            message_values.push((value, result_leaf_count + slots.len()));
            let predicate = slots.len() - index;
            slots.push(AssertionSlot::MessageValue {
                value_type,
                predicate,
            });
        }
        registered.push(RegisteredAssertion {
            predicate_output: result_leaf_count + index,
            message: assertion.message,
            message_values: message_values.into_boxed_slice(),
            level: assertion.level,
            provenance: assertion.provenance,
        });
    }
    let mut predicate_ranges = HashMap::new();
    for nested_call in nested_call_ids {
        let nested = callees
            .get(nested_call)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        let start = slots.len();
        slots.extend(nested.assertion_slots.iter().cloned());
        predicate_ranges.insert(*nested_call, start..slots.len());
        let shift = |output: usize| result_leaf_count + start + output - nested.result_leaf_count;
        registered.extend(nested.assertions.iter().map(|assertion| {
            RegisteredAssertion {
                predicate_output: shift(assertion.predicate_output),
                message: assertion.message,
                message_values: assertion
                    .message_values
                    .iter()
                    .map(|&(value, output)| (value, shift(output)))
                    .collect(),
                level: assertion.level,
                provenance: assertion.provenance,
            }
        }));
    }
    Ok(AssertionLayout {
        slots,
        predicate_ranges,
        direct_message_values,
        registered,
    })
}

/// The scalar output type of one converted message value, or `None` for a
/// value no String conversion renders (MLS §3.7.2 converts Real, Integer and
/// Boolean scalars here).
fn message_value_type<'dae>(
    view: dae::DaeView<'dae>,
    value: dae::ExprId<'dae>,
    arithmetic: solve::SolveArithmeticProfile,
) -> Result<Option<solve::SolveValueType>, solve::SolveProgramConstructionError> {
    let node = view
        .expression(value)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    if !matches!(
        node.value_type().scalar_type(),
        dae::ScalarType::Real | dae::ScalarType::Integer | dae::ScalarType::Boolean
    ) {
        return Ok(None);
    }
    let leaves = lower_value_type_leaves(view, node.value_type_id(), arithmetic)?;
    Ok(match leaves.as_slice() {
        [leaf] if leaf.dimensions().is_empty() => Some(leaf.clone()),
        _ => None,
    })
}
