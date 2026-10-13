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
    let function_id = function;
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
    let facts = FunctionFacts::of(view, function_id, arithmetic, identities)?;
    let (assertions, nested_call_ids) = (&facts.assertions.assertions, &facts.nested_calls);
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
    let interface = &facts.interface;
    let refused = assertions::refused_message_values(view, assertions, &callees);
    let kept_calls = if refused.is_empty() {
        nested_call_ids.clone()
    } else {
        assertions::nested_calls_outside(view, function, assertions, &refused)
    };
    let layout = assertion_layout(
        view,
        (assertions, &refused),
        &kept_calls,
        &callees,
        interface.result_leaf_count(),
        arithmetic,
    )?;
    let scalar_storable =
        facts.body_scalar_storable && callees.values().all(|callee| callee.scalar_storable);
    let body = OwnerBody {
        function,
        interface,
        callees: callees.into(),
        inventory: &facts.assertions,
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
        callee: body.callee(owner, scalar_storable),
        site,
    })
}

/// What a Modelica function body fixes for every owner issued from it: its
/// assertions, the owners its body calls, and its typed interface. A function
/// called from many sites is read once, not once per owner.
pub(super) struct FunctionFacts<'dae> {
    pub(super) assertions: assertions::FunctionAssertionInventory<'dae>,
    pub(super) nested_calls: Vec<dae::ExprId<'dae>>,
    /// The function each nested call names, in the order of `nested_calls`.
    pub(super) callees: std::rc::Rc<[dae::FunctionId<'dae>]>,
    pub(super) interface: FunctionInterface<'dae>,
    /// Whether this body alone is scalar storable, without its callees.
    pub(super) body_scalar_storable: bool,
}

impl<'dae> FunctionFacts<'dae> {
    pub(super) fn of(
        view: dae::DaeView<'dae>,
        key: dae::FunctionId<'dae>,
        arithmetic: solve::SolveArithmeticProfile,
        identities: &mut CallRegistration<'dae>,
    ) -> Result<std::rc::Rc<Self>, solve::SolveProgramConstructionError> {
        if let Some(facts) = identities.function_facts.get(&key) {
            return Ok(facts.clone());
        }
        let function = view
            .function(key)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        let assertions = assertions::FunctionAssertionInventory::discover(view, function)?;
        let nested_calls = nested_calls(view, function, &assertions.assertions);
        let callees = nested_calls
            .iter()
            .map(|&call| super::recursive_groups::callee(view, call))
            .collect::<Result<std::rc::Rc<[_]>, _>>()?;
        let interface = FunctionInterface::lower(view, function, arithmetic)?;
        let facts = std::rc::Rc::new(Self {
            assertions,
            nested_calls,
            callees,
            interface,
            body_scalar_storable: scalar_expansion::body_is_scalar_storable(view, function),
        });
        identities.function_facts.insert(key, facts.clone());
        Ok(facts)
    }
}

/// The typed leaf interface of one Modelica function.
pub(super) struct FunctionInterface<'dae> {
    pub(super) result_layout: std::sync::Arc<FunctionResultsLayout<'dae>>,
    parameter_types: Vec<dae::ValueTypeId<'dae>>,
    pub(super) inputs: Vec<solve::SolveValueType>,
    parameter_ranges: Vec<Range<usize>>,
}

impl<'dae> FunctionInterface<'dae> {
    pub(super) fn lower(
        view: dae::DaeView<'dae>,
        function: dae::FunctionView<'dae>,
        arithmetic: solve::SolveArithmeticProfile,
    ) -> Result<Self, solve::SolveProgramConstructionError> {
        let parameter_types = function.parameter_types().iter().collect::<Vec<_>>();
        let (inputs, parameter_ranges) = native::leaf_layout(view, &parameter_types, arithmetic)?;
        let result_layout =
            std::sync::Arc::new(FunctionResultsLayout::lower(view, function, arithmetic)?);
        Ok(Self {
            result_layout,
            parameter_types,
            inputs,
            parameter_ranges,
        })
    }

    pub(super) fn result_leaf_count(&self) -> usize {
        self.result_layout.leaves.len()
    }
}

/// Everything one owner body lowers from: the function, its typed interface,
/// the owners its nested calls name, and its assertion output layout.
pub(super) struct OwnerBody<'a, 'dae> {
    pub(super) function: dae::FunctionView<'dae>,
    pub(super) interface: &'a FunctionInterface<'dae>,
    pub(super) callees: std::sync::Arc<HashMap<dae::ExprId<'dae>, CalleeInterface<'dae>>>,
    /// The statement inventory of the function body: its assertions and the
    /// loop and conditional statement maps every region lowers from.
    pub(super) inventory: &'a assertions::FunctionAssertionInventory<'dae>,
    pub(super) layout: AssertionLayout<'dae>,
}

impl<'dae> OwnerBody<'_, 'dae> {
    /// Result leaves, then every assertion slot, in owner output order.
    pub(super) fn outputs(&self) -> Vec<solve::SolvePureCallOutput> {
        self.interface
            .result_layout
            .leaves
            .iter()
            .cloned()
            .map(solve::SolvePureCallOutput::result)
            .chain(self.layout.slots.iter().map(AssertionSlot::output))
            .collect()
    }

    pub(super) fn callee(
        &self,
        owner: solve::SolvePureCallOwnerId,
        scalar_storable: bool,
    ) -> CalleeInterface<'dae> {
        CalleeInterface {
            owner,
            result_layout: self.interface.result_layout.clone(),
            result_leaf_count: self.interface.result_leaf_count(),
            assertion_slots: std::sync::Arc::from(self.layout.slots.clone()),
            assertions: self.layout.registered.clone().into(),
            recursive: self.callees.values().any(|callee| callee.recursive),
            scalar_storable,
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
            direct_assertion_count: self.inventory.assertions.len(),
            direct_assertions: std::sync::Arc::from(
                self.layout.registered[..self.inventory.assertions.len()].to_vec(),
            ),
            assertion_output_base: interface.result_leaf_count(),
            loop_statements: self.inventory.loops.clone(),
            conditional_statements: self.inventory.conditionals.clone(),
            assigned_values: HashMap::new(),
            totality: HashMap::new(),
            implied_guards: Vec::new(),
            eager: EagerScope::default(),
        };
        lowerer.statements(function.statements())?;
        for ((definition, value_type), range) in function
            .result_values()
            .iter()
            .zip(interface.result_layout.signature.iter().copied())
            .zip(&interface.result_layout.ranges)
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
        for slot in 0..lowerer.predicate_values.len() {
            lowerer.predicate_values[slot] = Some(lowerer.published_slot(slot, provenance)?);
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
    predicate_ranges: std::sync::Arc<HashMap<dae::ExprId<'dae>, Range<usize>>>,
    registered: Vec<RegisteredAssertion<'dae>>,
}

/// Lay out the owner's own predicates, then its own message values, then each
/// nested call's complete slot tuple, so every nested assertion keeps the
/// predicate and message values its callee's frame produced.
pub(super) fn assertion_layout<'dae>(
    view: dae::DaeView<'dae>,
    (assertions, refused): (
        &[assertions::FunctionAssertion<'dae>],
        &std::collections::HashSet<dae::ExprId<'dae>>,
    ),
    nested_call_ids: &[dae::ExprId<'dae>],
    callees: &HashMap<dae::ExprId<'dae>, CalleeInterface<'dae>>,
    result_leaf_count: usize,
    arithmetic: solve::SolveArithmeticProfile,
) -> Result<AssertionLayout<'dae>, solve::SolveProgramConstructionError> {
    let mut slots = assertions
        .iter()
        .map(|assertion| AssertionSlot::Predicate {
            level: assertion.level,
        })
        .collect::<Vec<_>>();
    let mut registered = Vec::with_capacity(assertions.len());
    for (index, assertion) in assertions.iter().enumerate() {
        let mut message_values = Vec::new();
        for value in assertions::message_values(view, assertion)
            .into_iter()
            .filter(|value| !refused.contains(value))
        {
            let Some(value_type) = message_value_type(view, value, arithmetic)? else {
                continue;
            };
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
            message_values: message_values.into(),
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
        let target_start = result_leaf_count
            .checked_add(start)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        for assertion in nested.assertions.iter() {
            registered.push(assertion.project_outputs(nested.result_leaf_count, target_start)?);
        }
    }
    Ok(AssertionLayout {
        slots,
        predicate_ranges: predicate_ranges.into(),
        registered,
    })
}

impl<'dae> RegisteredAssertion<'dae> {
    /// Project the same predicate/message tuple into its enclosing owner.
    pub(super) fn project_outputs(
        &self,
        source_result_count: usize,
        target_slot_start: usize,
    ) -> Result<Self, solve::SolveProgramConstructionError> {
        let shift = |output: usize| {
            output
                .checked_sub(source_result_count)
                .and_then(|slot| target_slot_start.checked_add(slot))
                .ok_or(solve::SolveProgramConstructionError::InvalidCallOutput {
                    provenance: self.provenance.span(),
                })
        };
        Ok(Self {
            predicate_output: shift(self.predicate_output)?,
            message: self.message,
            message_values: self
                .message_values
                .iter()
                .map(|&(value, output)| shift(output).map(|output| (value, output)))
                .collect::<Result<_, _>>()?,
            level: self.level,
            provenance: self.provenance,
        })
    }
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
