//! Single-source DAE pure-function lowering into the checked typed vocabulary.

mod assertion_statements;
mod assertions;
mod captures;
mod eager_scope;
use eager_scope::EagerScope;
mod family_owner;
mod folds;
pub(crate) mod formal_stages;
mod indexed_slices;
mod indexed_values;
mod model_calls;
mod model_coordinates;
pub(in crate::lower) mod model_events;
mod native;
mod record_fills;
mod recursive_groups;
mod regions;
mod registration;
mod tensor;
mod total_conditionals;
mod value_types;
use value_types::FunctionResultsLayout;

use assertions::{assertion_conditions, nested_calls};
pub(crate) use family_owner::AlgebraicFamilyForm;
use indexed_values::leading_index_axes;
use model_coordinates::ModelCoordinateKey;
pub(super) use model_events::lower_model_event_transactions;
use regions::{
    EnvironmentLayout, RegionAssignmentChain, RegionConditional, RegionContext, RegionValues,
    function_value_type, load_region_lowerer, lower_region_assignment_chain,
    lower_region_conditional, lower_region_values,
};
use registration::register_call;
pub(crate) use value_types::is_text_value;
use value_types::{lower_primitive_type, lower_value_type_leaves, record_field_leaf_range};

use std::{
    collections::{BTreeSet, HashMap},
    num::NonZeroU64,
    ops::Range,
};

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

/// Construction-owned inventory of exact pure DAE call frames demanded by
/// Solve consumers.
///
/// Registration happens at the DAE expression boundary, before a consumer can
/// emit a projection. It never scans, hashes, or reconstructs an already
/// lowered scalar program. Nested owners are issued before their caller, so
/// the resulting table is finite and topological by construction, except
/// that the members of one SOLVE-C62 recursive group are issued together.
pub(crate) struct PureCallRegistry<'dae> {
    table: solve::SolvePureCallTableBuilder,
    identities: CallRegistration<'dae>,
}

impl<'dae> PureCallRegistry<'dae> {
    pub(crate) fn new() -> Self {
        let arithmetic = arithmetic_profile();
        Self {
            table: solve::SolvePureCallTable::builder(arithmetic),
            identities: CallRegistration::new(None),
        }
    }

    pub(crate) fn register_root(
        &mut self,
        view: dae::DaeView<'dae>,
        call: dae::ExprId<'dae>,
    ) -> Result<RegisteredCall<'dae>, solve::SolveProgramConstructionError> {
        register_call(
            &mut self.table,
            view,
            call,
            None,
            arithmetic_profile(),
            &mut self.identities,
            &mut Vec::new(),
        )
    }

    pub(crate) fn finish(&mut self) -> solve::SolvePureCallTable {
        let replacement = solve::SolvePureCallTable::builder(arithmetic_profile());
        self.identities = CallRegistration::new(None);
        std::mem::replace(&mut self.table, replacement).finish()
    }
}

/// Lower one exact pure Modelica call occurrence and its finite nested graph.
///
/// This is the first cutover slice of SOLVE-C51/C52. Structured statements,
/// records, and the remaining tensor families extend this implementation; no
/// consumer may introduce a second DAE function lowerer while those
/// capabilities are added here.
#[cfg(test)]
pub(super) fn lower_exact_call<'dae>(
    view: dae::DaeView<'dae>,
    call: dae::ExprId<'dae>,
    identity: solve::SolvePureCallIdentity,
) -> Result<solve::SolvePureCallTable, solve::SolveProgramConstructionError> {
    let arithmetic = arithmetic_profile();
    let mut table = solve::SolvePureCallTable::builder(arithmetic);
    let mut identities = CallRegistration::new(Some(identity));
    register_call(
        &mut table,
        view,
        call,
        Some(identity),
        arithmetic,
        &mut identities,
        &mut Vec::new(),
    )?;
    Ok(table.finish())
}

/// One issued pure-call owner together with its checked call site.
#[derive(Clone)]
pub(crate) struct RegisteredCall<'dae> {
    pub(crate) callee: CalleeInterface<'dae>,
    pub(crate) site: solve::SolvePureCallSite,
}

/// The owner interface a caller body lowers a call against. A SOLVE-C62
/// recursive group member has it before the group, and so its site, exists.
#[derive(Clone)]
pub(crate) struct CalleeInterface<'dae> {
    pub(crate) owner: solve::SolvePureCallOwnerId,
    pub(crate) result_layout: std::sync::Arc<FunctionResultsLayout<'dae>>,
    pub(crate) result_leaf_count: usize,
    /// Every call-scoped assertion output after the result leaves, in owner
    /// output order: the owner's own predicates, its own message values, then
    /// each nested call's complete slot tuple.
    pub(crate) assertion_slots: std::sync::Arc<[AssertionSlot]>,
    pub(crate) assertions: Box<[RegisteredAssertion<'dae>]>,
    /// Whether an invocation can enter a SOLVE-C62 recursive group, whose
    /// body no caller may expand in place.
    pub(crate) recursive: bool,
}

/// One call-scoped assertion output of a pure-call owner.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum AssertionSlot {
    /// The Boolean condition of one assertion reached by the invocation.
    Predicate { level: dae::AssertionLevel },
    /// One scalar an assertion message converts to text, evaluated in the
    /// frame of the function that declares that assertion. `predicate` is
    /// the backward distance to that assertion's predicate slot, which slot
    /// tuples keep when they are concatenated.
    MessageValue {
        value_type: solve::SolveValueType,
        predicate: usize,
    },
}

impl AssertionSlot {
    fn value_type(&self) -> solve::SolveValueType {
        match self {
            Self::Predicate { .. } => {
                solve::SolveValueType::scalar(solve::SolveScalarType::Boolean)
            }
            Self::MessageValue { value_type, .. } => value_type.clone(),
        }
    }

    fn output(&self) -> solve::SolvePureCallOutput {
        match self {
            Self::Predicate { level } => {
                solve::SolvePureCallOutput::assertion_predicate_at_level(match level {
                    dae::AssertionLevel::Error => solve::SolveAssertionLevel::Error,
                    dae::AssertionLevel::Warning => solve::SolveAssertionLevel::Warning,
                })
            }
            Self::MessageValue {
                value_type,
                predicate,
            } => {
                solve::SolvePureCallOutput::assertion_message_value(value_type.clone(), *predicate)
            }
        }
    }

    /// The value a slot holds when its assertion lies in a branch the
    /// invocation does not select: a satisfied predicate, and a message value
    /// that is never rendered because that predicate cannot fail.
    fn unselected<'program>(
        &self,
        builder: &mut solve::TypedProgramBuilder<'program>,
        provenance: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let value = match self {
            Self::Predicate { .. } => solve::SolveValue::boolean(true),
            Self::MessageValue { value_type, .. } => match value_type.element_type() {
                solve::SolveScalarType::Real { .. } => {
                    solve::SolveValue::real(arithmetic_profile(), 0.0)
                }
                solve::SolveScalarType::Integer(_) => {
                    solve::SolveValue::integer(arithmetic_profile(), 0).map_err(|_| {
                        solve::SolveProgramConstructionError::InvalidCallOutput { provenance }
                    })?
                }
                solve::SolveScalarType::Boolean => solve::SolveValue::boolean(false),
            },
        };
        builder.constant(value, provenance)
    }
}

/// Construction-issued correlation shared by every definition one function
/// conditional publishes.
///
/// A read can demand a definition before the statement walk reaches its
/// assignment group (most notably from a fold's compact update tuple). Keeping
/// this exact DAE-owned association lets that demand lower the whole correlated
/// tuple once instead of lowering each joined scalar expression independently.
#[derive(Clone, Copy)]
struct ConditionalDefinitionGroup<'dae> {
    definitions: dae::FunctionDefinitionValues<'dae>,
    conditional: dae::FunctionConditionalView<'dae>,
}

fn conditional_definition_groups<'dae>(
    statements: dae::FunctionStatements<'dae>,
) -> Result<
    HashMap<dae::FunctionDefinitionId<'dae>, ConditionalDefinitionGroup<'dae>>,
    solve::SolveProgramConstructionError,
> {
    fn collect<'dae>(
        statements: dae::FunctionStatements<'dae>,
        groups: &mut HashMap<dae::FunctionDefinitionId<'dae>, ConditionalDefinitionGroup<'dae>>,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        for statement in statements {
            match statement {
                dae::FunctionStatementView::AssignmentGroup {
                    definitions,
                    conditional: Some(conditional),
                } => {
                    let group = ConditionalDefinitionGroup {
                        definitions,
                        conditional,
                    };
                    insert_conditional_definition_group(groups, definitions, group)?;
                }
                dae::FunctionStatementView::For { statements, .. } => {
                    collect(statements, groups)?;
                }
                dae::FunctionStatementView::Assignment { .. }
                | dae::FunctionStatementView::AssignmentGroup {
                    conditional: None, ..
                }
                | dae::FunctionStatementView::Assertion { .. } => {}
            }
        }
        Ok(())
    }

    let mut groups = HashMap::new();
    collect(statements, &mut groups)?;
    Ok(groups)
}

fn insert_conditional_definition_group<'dae>(
    groups: &mut HashMap<dae::FunctionDefinitionId<'dae>, ConditionalDefinitionGroup<'dae>>,
    definitions: dae::FunctionDefinitionValues<'dae>,
    group: ConditionalDefinitionGroup<'dae>,
) -> Result<(), solve::SolveProgramConstructionError> {
    for definition in definitions.iter() {
        if groups.insert(definition.id(), group).is_some() {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: definition.provenance().span(),
            });
        }
    }
    Ok(())
}

#[derive(Clone)]
pub(crate) struct RegisteredAssertion<'dae> {
    pub(crate) predicate_output: usize,
    pub(crate) message: dae::ExprId<'dae>,
    /// Owner output carrying each converted message value, keyed by the value
    /// expression the message converts.
    pub(crate) message_values: Box<[(dae::ExprId<'dae>, usize)]>,
    pub(crate) level: dae::AssertionLevel,
    pub(crate) provenance: dae::DaeProvenance,
}

#[derive(Clone)]
struct LoweredValue<'program, 'dae> {
    value_type: dae::ValueTypeId<'dae>,
    leaves: Vec<solve::ProgramRegister<'program>>,
}

impl<'program, 'dae> LoweredValue<'program, 'dae> {
    /// A value with a zero extent: it holds no scalar, so it holds no leaf
    /// (the rule [`lower_value_type_leaves`] applies to interfaces).
    fn empty(value_type: dae::ValueTypeId<'dae>) -> Self {
        Self {
            value_type,
            leaves: Vec::new(),
        }
    }

    fn scalar(
        value_type: dae::ValueTypeId<'dae>,
        register: solve::ProgramRegister<'program>,
    ) -> Self {
        Self {
            value_type,
            leaves: vec![register],
        }
    }

    fn only_register(
        &self,
        provenance: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let [register] = self.leaves.as_slice() else {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface { provenance });
        };
        Ok(*register)
    }
}

struct CallRegistration<'dae> {
    reserved: Option<solve::SolvePureCallIdentity>,
    next: u64,
    calls: HashMap<dae::ExprId<'dae>, RegisteredCall<'dae>>,
    /// SOLVE-C70 owners, keyed by the DAE family they lower.
    families: HashMap<dae::ContinuousFamilyId<'dae>, solve::SolvePureCallSite>,
    /// Functions whose call SCC was already examined for a SOLVE-C62 group.
    examined_functions: std::collections::HashSet<dae::FunctionId<'dae>>,
    /// What each function contributes to every owner of it, derived once.
    function_facts: HashMap<dae::FunctionId<'dae>, std::rc::Rc<registration::FunctionFacts<'dae>>>,
}

impl CallRegistration<'_> {
    fn new(reserved: Option<solve::SolvePureCallIdentity>) -> Self {
        Self {
            reserved,
            next: 1,
            calls: HashMap::new(),
            families: HashMap::new(),
            examined_functions: std::collections::HashSet::new(),
            function_facts: HashMap::new(),
        }
    }

    fn issue(
        &mut self,
        provenance: rumoca_core::Span,
    ) -> Result<solve::SolvePureCallIdentity, solve::SolveProgramConstructionError> {
        loop {
            let value = NonZeroU64::new(self.next)
                .ok_or(solve::SolveProgramConstructionError::IdentityOverflow { provenance })?;
            self.next = self
                .next
                .checked_add(1)
                .ok_or(solve::SolveProgramConstructionError::IdentityOverflow { provenance })?;
            let identity = solve::SolvePureCallIdentity::issued(value);
            if Some(identity) != self.reserved {
                return Ok(identity);
            }
        }
    }
}

fn arithmetic_profile() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::construct(i64::MIN, i64::MAX)
            .expect("the full i64 domain is nonempty"),
    )
}

struct ExpressionLowerer<'builder, 'program, 'dae> {
    view: dae::DaeView<'dae>,
    builder: &'builder mut solve::TypedProgramBuilder<'program>,
    model_coordinates: HashMap<ModelCoordinateKey<'dae>, LoweredValue<'program, 'dae>>,
    parameters: HashMap<dae::FunctionParameterId<'dae>, LoweredValue<'program, 'dae>>,
    /// Values of the function's construction-issued SSA definitions.
    ///
    /// The key is the definition identity the DAE issued, not the assigned
    /// value alone: one function value owns a distinct definition for every
    /// redefinition, for a loop's entry parameter, and for a loop's output.
    /// Keying by definition is what lets a read name the exact reaching
    /// definition, so a demand that arrives before its defining statement was
    /// lowered resolves that definition instead of an unrelated live value.
    function_values: HashMap<dae::FunctionDefinitionId<'dae>, LoweredValue<'program, 'dae>>,
    /// Exact DAE assignment-group membership for demand-ordered definitions.
    conditional_groups:
        std::sync::Arc<HashMap<dae::FunctionDefinitionId<'dae>, ConditionalDefinitionGroup<'dae>>>,
    fold_parameters: HashMap<(dae::FunctionFoldId<'dae>, u32), LoweredValue<'program, 'dae>>,
    fold_values: HashMap<dae::FunctionFoldId<'dae>, Vec<LoweredValue<'program, 'dae>>>,
    binders: HashMap<(u32, u32), solve::ProgramRegister<'program>>,
    callees: std::sync::Arc<HashMap<dae::ExprId<'dae>, CalleeInterface<'dae>>>,
    predicate_ranges: std::sync::Arc<HashMap<dae::ExprId<'dae>, Range<usize>>>,
    cache: HashMap<dae::ExprId<'dae>, LoweredValue<'program, 'dae>>,
    call_values: HashMap<dae::ExprId<'dae>, Vec<solve::ProgramRegister<'program>>>,
    predicate_values: Vec<Option<solve::ProgramRegister<'program>>>,
    /// Kind of each entry of `predicate_values`, in owner output order.
    assertion_slots: std::sync::Arc<[AssertionSlot]>,
    next_direct_assertion: usize,
    direct_assertion_count: usize,
    direct_assertions: std::sync::Arc<[RegisteredAssertion<'dae>]>,
    assertion_output_base: usize,
    loop_statements:
        std::sync::Arc<HashMap<dae::FunctionFoldId<'dae>, assertions::LoopStatements<'dae>>>,
    /// Whether each expression evaluates without failure or effect (see
    /// `total_conditionals`).
    totality: HashMap<dae::ExprId<'dae>, bool>,
    /// Eagerly demanded calls and correlated conditionals not yet issued.
    eager: EagerScope<'dae>,
}

impl<'builder, 'program, 'dae> ExpressionLowerer<'builder, 'program, 'dae> {
    /// A lowerer for an owner body over captured model coordinates alone: no
    /// function parameters, definitions, folds, or nested calls.
    fn for_model_coordinates(
        view: dae::DaeView<'dae>,
        builder: &'builder mut solve::TypedProgramBuilder<'program>,
        model_coordinates: HashMap<ModelCoordinateKey<'dae>, LoweredValue<'program, 'dae>>,
    ) -> Self {
        Self {
            view,
            builder,
            model_coordinates,
            parameters: HashMap::new(),
            function_values: HashMap::new(),
            conditional_groups: Default::default(),
            fold_parameters: HashMap::new(),
            fold_values: HashMap::new(),
            binders: HashMap::new(),
            callees: Default::default(),
            predicate_ranges: Default::default(),
            cache: HashMap::new(),
            call_values: HashMap::new(),
            predicate_values: Vec::new(),
            assertion_slots: std::sync::Arc::from([]),
            next_direct_assertion: 0,
            direct_assertion_count: 0,
            direct_assertions: std::sync::Arc::from([]),
            assertion_output_base: 0,
            loop_statements: Default::default(),
            totality: HashMap::new(),
            eager: EagerScope::default(),
        }
    }
}

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    /// Value one definition holds, at the type its target declares.
    ///
    /// DAE assignment compatibility admits an Integer right-hand side under a
    /// Real target, so the right-hand side's own type is not the type of the
    /// definition. Consumers read the definition through the target's declared
    /// type, which makes the declared type the only correct type to store: this
    /// is the single place that turns a right-hand side into a definition
    /// value, so a demand-ordered capture and an in-order statement always
    /// agree.
    ///
    /// Every demand for a definition's value reaches it through the one
    /// memoizing resolver, `function_definition_value`: the in-order
    /// `Assignment` statement arm, the reverse-demand capture path, and a
    /// fold's entry value all call that resolver, so one definition owns one
    /// value at one type no matter which demand arrives first. A correlated
    /// conditional group applies the same rule at its region-output boundary,
    /// where a branch value is taken to its target's declared type before it
    /// becomes a region output. Each of those four demands is pinned by its own
    /// regression in `tests.rs` - the ordinary-statement, reverse-demand
    /// capture, fold-carry, and correlated-branch coercion tests - so bypassing
    /// this rule at any one of them turns a test red on its own.
    fn definition_value(
        &mut self,
        definition: dae::FunctionDefinitionView<'dae>,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        let value = self.expression(definition.rhs())?;
        let target_type = function_value_type(
            self.view,
            definition.target(),
            definition.provenance().span(),
        )?;
        self.coerce_value(value, target_type, definition.provenance().span())
    }

    // SPEC_0021: Exception - exhaustive dispatch over function statement variants.
    #[allow(clippy::excessive_nesting)]
    fn statements(
        &mut self,
        statements: dae::FunctionStatements<'dae>,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        for statement in statements {
            match statement {
                dae::FunctionStatementView::Assignment { definition } => {
                    self.function_definition_value(definition)?;
                }
                dae::FunctionStatementView::AssignmentGroup {
                    definitions,
                    conditional: None,
                } => {
                    for definition in definitions.iter() {
                        self.function_definition_value(definition)?;
                    }
                }
                dae::FunctionStatementView::Assertion {
                    condition,
                    provenance,
                    ..
                } => {
                    self.assertion_statement(condition, provenance.span())?;
                }
                dae::FunctionStatementView::AssignmentGroup {
                    definitions,
                    conditional: Some(conditional),
                } => {
                    self.conditional_assignment(definitions, conditional)?;
                }
                dae::FunctionStatementView::For {
                    fold, provenance, ..
                } => {
                    let values = self.function_fold(fold, provenance.span())?;
                    if let Some(source) = self.loop_statements.get(&fold) {
                        if self.next_direct_assertion != source.assertions.start {
                            return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                                provenance: provenance.span(),
                            });
                        }
                        self.next_direct_assertion = source.assertions.end;
                    }
                    let fold = self
                        .view
                        .function_fold(fold)
                        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
                    if fold.output_values().len() != values.len() {
                        return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                            provenance: provenance.span(),
                        });
                    }
                    // The loop's exit value of each carried target belongs to
                    // the output definition the fold issued, not to the
                    // in-body definitions that produced it.
                    self.function_values.extend(
                        fold.output_values()
                            .iter()
                            .map(|definition| definition.id())
                            .zip(values),
                    );
                }
            }
        }
        Ok(())
    }

    fn conditional(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        operands: &[dae::ExprId<'dae>],
        provenance: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        if operands.len() < 3 || operands.len().is_multiple_of(2) {
            return Err(solve::SolveProgramConstructionError::InvalidRegion { provenance });
        }
        if let Some(value) = self.total_conditional(value_type, operands, provenance)? {
            return Ok(value);
        }
        let condition = self.expression(operands[0])?.only_register(provenance)?;
        let pending = self.pending_predicates(operands[1..].iter().copied())?;
        let mut output_types =
            lower_value_type_leaves(self.view, value_type, arithmetic_profile())?;
        let value_leaf_count = output_types.len();
        output_types.extend(self.pending_slot_types(&pending));
        let (captures, environment) =
            self.capture_environment_for(operands[1..].iter().copied())?;
        let context = self.region_context();
        let true_environment = environment.clone();
        let true_context = context.clone();
        let true_operands = vec![operands[1]];
        let false_operands = operands[2..].to_vec();
        let true_pending = pending.clone();
        let false_pending = pending.clone();
        let destinations = self.builder.conditional(
            condition,
            &captures,
            output_types,
            provenance,
            move |builder, inputs, outputs| {
                lower_region_conditional(
                    builder,
                    inputs,
                    outputs,
                    &true_environment,
                    &true_context,
                    RegionConditional {
                        value_type,
                        operands: true_operands,
                        pending: true_pending,
                        provenance,
                    },
                )
            },
            move |builder, inputs, outputs| {
                lower_region_conditional(
                    builder,
                    inputs,
                    outputs,
                    &environment,
                    &context,
                    RegionConditional {
                        value_type,
                        operands: false_operands,
                        pending: false_pending,
                        provenance,
                    },
                )
            },
        )?;
        for slot in pending {
            let predicate = self.inactive_slot(slot, provenance)?;
            let value = self
                .predicate_values
                .get_mut(slot)
                .ok_or(solve::SolveProgramConstructionError::InvalidCallOutput { provenance })?;
            if value.replace(predicate).is_some() {
                return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
            }
        }
        Ok(LoweredValue {
            value_type,
            leaves: destinations[..value_leaf_count].to_vec(),
        })
    }

    fn coerce_value(
        &mut self,
        mut value: LoweredValue<'program, 'dae>,
        target: dae::ValueTypeId<'dae>,
        provenance: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        if value.value_type == target {
            return Ok(value);
        }
        let source_type = self
            .view
            .value_type(value.value_type)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        let target_type = self
            .view
            .value_type(target)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        if source_type == target_type {
            value.value_type = target;
            return Ok(value);
        }
        if !source_type.is_record()
            && !target_type.is_record()
            && source_type.dimensions() == target_type.dimensions()
            && target_type.scalar_type() == dae::ScalarType::Real
            && matches!(
                source_type.scalar_type(),
                dae::ScalarType::Integer | dae::ScalarType::Enumeration
            )
        {
            // A zero-size array holds no scalar to convert (MLS 3.7 §10.4).
            if source_type.dimensions().contains(&0) {
                return Ok(LoweredValue::empty(target));
            }
            let register = value.only_register(provenance)?;
            value.leaves = vec![self.builder.convert(
                solve::SolveConversionOperator::IntegerToReal,
                register,
                provenance,
            )?];
            value.value_type = target;
            return Ok(value);
        }
        Err(solve::SolveProgramConstructionError::TypeMismatch { provenance })
    }

    /// Output types of the pending assertion slots a region publishes.
    fn pending_slot_types(&self, pending: &[usize]) -> Vec<solve::SolveValueType> {
        pending
            .iter()
            .map(|&slot| self.assertion_slots[slot].value_type())
            .collect()
    }

    /// The value a region publishes for one pending assertion slot: the value
    /// its branch produced, or the unselected value when the branch reaches
    /// no call that owns the slot.
    fn published_slot(
        &mut self,
        slot: usize,
        provenance: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        if let Some(value) = self.predicate_values.get(slot).copied().flatten() {
            return Ok(value);
        }
        self.inactive_slot(slot, provenance)
    }

    /// Nested diagnostics travel through the invocation observer. A region's
    /// normal result carries only the construction-issued inactive cells.
    fn inactive_slot(
        &mut self,
        slot: usize,
        provenance: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let kind = self
            .assertion_slots
            .get(slot)
            .cloned()
            .ok_or(solve::SolveProgramConstructionError::InvalidCallOutput { provenance })?;
        kind.unselected(self.builder, provenance)
    }

    /// The assertion slots `expressions` would settle that no value in this
    /// scope settled yet, once the demanded calls among them are issued here
    /// (a region over `expressions` publishes only the slots still open).
    fn pending_predicates(
        &mut self,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>> + Clone,
    ) -> Result<Vec<usize>, solve::SolveProgramConstructionError> {
        self.issue_demanded_calls(&mut dae::ExpressionTraversal::new(), expressions.clone())?;
        Ok(self.unissued_assertion_slots(expressions))
    }

    /// Source statement discovery gathers slots without evaluating a call.
    fn unissued_assertion_slots(
        &self,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>>,
    ) -> Vec<usize> {
        let mut pending = BTreeSet::new();
        for root in expressions {
            dae::for_each_expression(self.view, root, |_, node| {
                self.collect_pending_call_predicates(node, &mut pending);
            });
        }
        pending.into_iter().collect()
    }

    fn collect_pending_call_predicates(
        &self,
        node: dae::ExpressionView<'dae>,
        pending: &mut BTreeSet<usize>,
    ) {
        let dae::ExpressionOperation::Call { owner, .. } = node.operation() else {
            return;
        };
        let Some(range) = self.predicate_ranges.get(&owner) else {
            return;
        };
        pending.extend(
            range
                .clone()
                .filter(|&slot| self.predicate_values.get(slot).is_some_and(Option::is_none)),
        );
    }

    fn assignment_conditional_chain(
        &mut self,
        value_types: &[dae::ValueTypeId<'dae>],
        conditions: &[dae::ExprId<'dae>],
        branches: &[Vec<dae::ExprId<'dae>>],
        fallback: &[dae::ExprId<'dae>],
        pending: &[usize],
        provenance: rumoca_core::Span,
    ) -> Result<Vec<solve::ProgramRegister<'program>>, solve::SolveProgramConstructionError> {
        let (Some(condition_expression), Some(branch)) = (conditions.first(), branches.first())
        else {
            return Err(solve::SolveProgramConstructionError::InvalidRegion { provenance });
        };
        // Every arm of the chain publishes the same correlated tuple, so each
        // arm owes exactly one value per declared target. Proving that here
        // makes the value/target pairing below total for the whole recursion.
        if branches
            .iter()
            .any(|branch| branch.len() != value_types.len())
            || fallback.len() != value_types.len()
        {
            return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
        }
        let condition = self
            .expression(*condition_expression)?
            .only_register(provenance)?;
        // The calls the condition reaches run in this scope before either arm,
        // so the assertion slots they settle are published from here; the arms
        // publish only the slots still pending.
        let all_pending = pending;
        let pending = &self.still_pending(all_pending);
        let mut output_types = Vec::new();
        for value_type in value_types {
            output_types.extend(lower_value_type_leaves(
                self.view,
                *value_type,
                arithmetic_profile(),
            )?);
        }
        output_types.extend(self.pending_slot_types(pending));
        let capture_roots = conditions[1..]
            .iter()
            .copied()
            .chain(branches.iter().flatten().copied())
            .chain(fallback.iter().copied());
        let (captures, environment) = self.capture_environment_for(capture_roots)?;
        let context = self.region_context();
        let true_environment = environment.clone();
        let true_context = context.clone();
        let true_branch = value_types
            .iter()
            .copied()
            .zip(branch.iter().copied())
            .collect::<Vec<_>>();
        let true_pending = pending.to_vec();
        let false_pending = pending.to_vec();
        let false_value_types = value_types.to_vec();
        let false_conditions = conditions[1..].to_vec();
        let false_branches = branches[1..].to_vec();
        let false_fallback = fallback.to_vec();
        let destinations = self.builder.conditional(
            condition,
            &captures,
            output_types,
            provenance,
            move |builder, inputs, outputs| {
                lower_region_values(
                    builder,
                    inputs,
                    outputs,
                    &true_environment,
                    &true_context,
                    RegionValues {
                        results: &true_branch,
                        pending_predicates: &true_pending,
                        provenance,
                    },
                )
            },
            move |builder, inputs, outputs| {
                lower_region_assignment_chain(
                    builder,
                    inputs,
                    outputs,
                    &environment,
                    &context,
                    RegionAssignmentChain {
                        value_types: false_value_types,
                        conditions: false_conditions,
                        branches: false_branches,
                        fallback: false_fallback,
                        pending: false_pending,
                        provenance,
                    },
                )
            },
        )?;
        self.publish_chain_slots(&destinations, all_pending, pending, provenance)
    }

    /// The slots of `pending` no call of this scope has settled yet.
    fn still_pending(&self, pending: &[usize]) -> Vec<usize> {
        pending
            .iter()
            .copied()
            .filter(|slot| {
                self.predicate_values
                    .get(*slot)
                    .is_some_and(Option::is_none)
            })
            .collect()
    }

    /// A chain's values followed by every slot of `all_pending`: the arms'
    /// value for a slot they published, and this scope's value for a slot its
    /// condition settled.
    fn publish_chain_slots(
        &mut self,
        destinations: &[solve::ProgramRegister<'program>],
        all_pending: &[usize],
        regional_slots: &[usize],
        provenance: rumoca_core::Span,
    ) -> Result<Vec<solve::ProgramRegister<'program>>, solve::SolveProgramConstructionError> {
        let value_count = destinations.len() - regional_slots.len();
        let mut published = destinations[..value_count].to_vec();
        for slot in all_pending {
            let value = if regional_slots.contains(slot) {
                Some(self.inactive_slot(*slot, provenance)?)
            } else {
                self.predicate_values.get(*slot).copied().flatten()
            };
            published.push(
                value.ok_or(solve::SolveProgramConstructionError::InvalidCallOutput {
                    provenance,
                })?,
            );
        }
        Ok(published)
    }

    fn conditional_assignment(
        &mut self,
        definitions: dae::FunctionDefinitionValues<'dae>,
        conditional: dae::FunctionConditionalView<'dae>,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let at = definitions
            .get(0)
            .expect("checked function conditional defines a nonempty target tuple")
            .provenance()
            .span();
        if conditional.branch_count() == 0
            || conditional.branch_count() != conditional.conditions().len()
        {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: at,
            });
        }
        let definitions = definitions.iter().collect::<Vec<_>>();
        let conditions = conditional.conditions().collect::<Vec<_>>();
        let branches = (0..conditional.branch_count())
            .map(|ordinal| {
                conditional
                    .branch(ordinal)
                    .ok_or(solve::SolveProgramConstructionError::WireMismatch)
                    .map(Iterator::collect::<Vec<_>>)
            })
            .collect::<Result<Vec<_>, _>>()?;
        let fallback = conditional.fallback().collect::<Vec<_>>();
        if branches
            .iter()
            .any(|branch| branch.len() != definitions.len())
            || fallback.len() != definitions.len()
        {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: at,
            });
        }
        let mut value_types = Vec::with_capacity(definitions.len());
        for definition in &definitions {
            value_types.push(function_value_type(self.view, definition.target(), at)?);
        }
        let values = self.correlated_conditional_values(
            &value_types,
            &conditions,
            &branches,
            &fallback,
            at,
        )?;
        for (definition, value) in definitions.iter().zip(values) {
            self.function_values.insert(definition.id(), value);
        }
        Ok(())
    }

    /// The values of one correlated conditional tuple, lowered as one region
    /// pair: every target shares the conditions, and so shares the values its
    /// arms compute once.
    ///
    /// A correlated group of assignments and a set of sibling conditional
    /// expressions with the same conditions (`lower_fused_conditionals`) both
    /// arrive here, so the tuple has one owner whichever way its targets were
    /// demanded.
    fn correlated_conditional_values(
        &mut self,
        value_types: &[dae::ValueTypeId<'dae>],
        conditions: &[dae::ExprId<'dae>],
        branches: &[Vec<dae::ExprId<'dae>>],
        fallback: &[dae::ExprId<'dae>],
        at: rumoca_core::Span,
    ) -> Result<Vec<LoweredValue<'program, 'dae>>, solve::SolveProgramConstructionError> {
        let pending = self.pending_predicates(
            conditions
                .iter()
                .copied()
                .chain(branches.iter().flatten().copied())
                .chain(fallback.iter().copied()),
        )?;
        let mut value_ranges = Vec::with_capacity(value_types.len());
        let mut value_leaf_count = 0;
        for value_type in value_types {
            let leaves = lower_value_type_leaves(self.view, *value_type, arithmetic_profile())?;
            value_ranges.push((
                *value_type,
                value_leaf_count..value_leaf_count + leaves.len(),
            ));
            value_leaf_count += leaves.len();
        }
        let destinations = self.assignment_conditional_chain(
            value_types,
            conditions,
            branches,
            fallback,
            &pending,
            at,
        )?;
        for (slot, predicate) in pending
            .into_iter()
            .zip(destinations[value_leaf_count..].iter().copied())
        {
            let value = self.predicate_values.get_mut(slot).ok_or(
                solve::SolveProgramConstructionError::InvalidCallOutput { provenance: at },
            )?;
            // A slot the chain's first condition settled in this scope comes
            // back as the value this scope already holds.
            match value {
                Some(existing) if *existing == predicate => {}
                Some(_) => {
                    return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                        provenance: at,
                    });
                }
                None => *value = Some(predicate),
            }
        }
        Ok(value_ranges
            .into_iter()
            .map(|(value_type, range)| LoweredValue {
                value_type,
                leaves: destinations[range].to_vec(),
            })
            .collect())
    }

    // SPEC_0021: Exception - exhaustive typed dispatch over expression operation variants.
    #[allow(clippy::too_many_lines)]
    fn expression(
        &mut self,
        expression: dae::ExprId<'dae>,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        if let Some(value) = self.cache.get(&expression).cloned() {
            return Ok(value);
        }
        let node = self
            .view
            .expression(expression)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        let at = node.provenance().span();
        let value = match node.operation() {
            dae::ExpressionOperation::Literal(value) => {
                self.literal(value, node.value_type_id(), at)?
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => self.parameters.get(&parameter).cloned().ok_or(
                solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at },
            )?,
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) => {
                let register = self
                    .binders
                    .get(&(binder.domain().index(), binder.ordinal()))
                    .copied()
                    .ok_or(solve::SolveProgramConstructionError::InvalidCallInterface {
                        provenance: at,
                    })?;
                LoweredValue::scalar(node.value_type_id(), register)
            }
            dae::ExpressionOperation::Coordinate(coordinate) => {
                let key = ModelCoordinateKey::from_view(coordinate).ok_or(
                    solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at },
                )?;
                self.model_coordinates.get(&key).cloned().ok_or(
                    solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at },
                )?
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                let operand = self.expression(operand)?;
                if operand.leaves.is_empty() && self.is_zero_size(node.value_type_id())? {
                    return Ok(LoweredValue::empty(node.value_type_id()));
                }
                let register = operand.only_register(at)?;
                let result = match operator {
                    dae::UnaryOperator::Plus => register,
                    dae::UnaryOperator::Negate => {
                        self.builder
                            .unary(solve::SolveUnaryOperator::Negate, register, at)?
                    }
                    dae::UnaryOperator::Not => {
                        self.builder
                            .unary(solve::SolveUnaryOperator::Not, register, at)?
                    }
                };
                LoweredValue::scalar(node.value_type_id(), result)
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                self.binary(node.value_type_id(), operator, lhs, rhs, at)?
            }
            dae::ExpressionOperation::Conditional(operands) => {
                self.lower_group_of(expression)?;
                if let Some(value) = self.cache.get(&expression).cloned() {
                    return Ok(value);
                }
                self.conditional(
                    node.value_type_id(),
                    &operands.iter().collect::<Vec<_>>(),
                    at,
                )?
            }
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                self.builtin(node.value_type_id(), builtin, arguments, at)?
            }
            dae::ExpressionOperation::Call {
                owner,
                output,
                arguments,
                ..
            } => self.call(owner, expression, output, arguments, at)?,
            dae::ExpressionOperation::Record(arguments) => {
                self.record(node.value_type_id(), arguments, at)?
            }
            dae::ExpressionOperation::Array(arguments) => {
                self.array(node.value_type_id(), arguments, at)?
            }
            dae::ExpressionOperation::Field { base, field } => {
                self.field(node.value_type_id(), base, field, at)?
            }
            dae::ExpressionOperation::Comprehension { domain, body } => {
                self.comprehension(node.value_type_id(), domain, body, at)?
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => self.array_update(node.value_type_id(), base, value, subscripts, at)?,
            dae::ExpressionOperation::Index { base, subscripts } => {
                if self.is_zero_size(node.value_type_id())? {
                    // A zero-size slice holds no leaf; its base is still
                    // lowered so a call inside it keeps its evaluation.
                    self.expression(base)?;
                    LoweredValue::empty(node.value_type_id())
                } else {
                    self.index(node.value_type_id(), base, subscripts, at)?
                }
            }
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.function_definition_value(definition)?
            }
            dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. } => {
                self.fold_parameters.get(&(fold, carried)).cloned().ok_or(
                    solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at },
                )?
            }
            dae::ExpressionOperation::FunctionFoldOutput { fold, carried, .. } => self
                .function_fold(fold, at)?
                .get(carried as usize)
                .cloned()
                .ok_or(solve::SolveProgramConstructionError::InvalidCallOutput {
                    provenance: at,
                })?,
            _ => {
                return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                    provenance: at,
                });
            }
        };
        self.cache.insert(expression, value.clone());
        Ok(value)
    }

    /// Whether `value_type` has a zero extent (MLS 3.7 §10.1), so a value of it
    /// holds no scalar and lowers to no leaf.
    fn is_zero_size(
        &self,
        value_type: dae::ValueTypeId<'dae>,
    ) -> Result<bool, solve::SolveProgramConstructionError> {
        Ok(self
            .view
            .value_type(value_type)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
            .dimensions()
            .contains(&0))
    }

    fn record(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        arguments: dae::ExpressionOperands<'dae>,
        at: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        let mut leaves = Vec::new();
        let record = self
            .view
            .value_type(value_type)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        if !record.is_record() || record.record_field_count() != arguments.len() {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: at,
            });
        }
        for (ordinal, argument) in arguments.iter().enumerate() {
            let (_, field_type) = self.view.record_field(value_type, ordinal).ok_or(
                solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at },
            )?;
            if is_text_value(
                self.view
                    .value_type(field_type)
                    .ok_or(solve::SolveProgramConstructionError::WireMismatch)?,
            ) {
                // A text field occupies no leaf, so its value is not lowered.
                continue;
            }
            let field = self.expression(argument)?;
            leaves.extend(self.coerce_value(field, field_type, at)?.leaves);
        }
        let expected = lower_value_type_leaves(self.view, value_type, arithmetic_profile())?;
        if leaves.len() != expected.len() {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: at,
            });
        }
        Ok(LoweredValue { value_type, leaves })
    }

    fn array(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        arguments: dae::ExpressionOperands<'dae>,
        at: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        let dimensions = self
            .view
            .value_type(value_type)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
            .dimensions()
            .to_vec();
        // MLS 3.6 §10.4: a zero-size array constructor holds no scalars, so it
        // holds no leaf either. It is the value `lower_value_type_leaves`
        // already types as empty; constructing an aggregate for it would demand
        // an element that does not exist.
        if dimensions.contains(&0) {
            return Ok(LoweredValue {
                value_type,
                leaves: Vec::new(),
            });
        }
        let elements = arguments
            .iter()
            .map(|argument| self.expression(argument))
            .collect::<Result<Vec<_>, _>>()?;
        let Some(first) = elements.first() else {
            return Err(solve::SolveProgramConstructionError::InvalidAggregate { provenance: at });
        };
        if elements
            .iter()
            .any(|element| element.leaves.len() != first.leaves.len())
        {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: at,
            });
        }
        let expected = lower_value_type_leaves(self.view, value_type, arithmetic_profile())?;
        if expected.len() != first.leaves.len() {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: at,
            });
        }
        let mut leaves = Vec::with_capacity(expected.len());
        for (ordinal, leaf_type) in expected.iter().enumerate() {
            let element_dimensions = leaf_type
                .dimensions()
                .get(1..)
                .ok_or(solve::SolveProgramConstructionError::InvalidAggregate { provenance: at })?;
            let element_type = if element_dimensions.is_empty() {
                solve::SolveValueType::scalar(leaf_type.element_type())
            } else {
                solve::SolveValueType::tensor(leaf_type.element_type(), element_dimensions.to_vec())
                    .map_err(|_| solve::SolveProgramConstructionError::InvalidAggregate {
                        provenance: at,
                    })?
            };
            let field_elements = elements
                .iter()
                .map(|element| {
                    self.builder
                        .coerce_to(element.leaves[ordinal], &element_type, at)
                })
                .collect::<Result<Vec<_>, _>>()?;
            leaves.push(self.builder.construct_aggregate(
                &field_elements,
                leaf_type.dimensions().to_vec(),
                at,
            )?);
        }
        Ok(LoweredValue { value_type, leaves })
    }

    fn field(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        base: dae::ExprId<'dae>,
        field: u32,
        at: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        let base_value = self.expression(base)?;
        let range = record_field_leaf_range(
            self.view,
            base_value.value_type,
            field as usize,
            arithmetic_profile(),
        )?;
        let leaves = base_value
            .leaves
            .get(range)
            .ok_or(solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at })?
            .to_vec();
        let expected = lower_value_type_leaves(self.view, value_type, arithmetic_profile())?;
        if leaves.len() != expected.len() {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: at,
            });
        }
        Ok(LoweredValue { value_type, leaves })
    }

    fn comprehension(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        domain_id: dae::DomainId<'dae>,
        body: dae::ExprId<'dae>,
        at: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        // A zero-size comprehension holds no scalar, so it holds no leaf.
        if self.is_zero_size(value_type)? {
            return Ok(LoweredValue::empty(value_type));
        }
        let body_node = self
            .view
            .expression(body)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        let domain = self
            .view
            .domain(domain_id)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
            .structured()
            .clone();
        if body_node.binder_domain().is_some() {
            let leaves = self.mapped_leaves(body, domain_id, domain, at)?;
            return Ok(LoweredValue { value_type, leaves });
        }
        let body_value = self.expression(body)?;
        if body_value.leaves.len() != 1 {
            return self.filled_record_comprehension(
                (value_type, body_node.value_type_id()),
                &domain,
                body_value,
                at,
            );
        }
        let value = body_value.only_register(at)?;
        if !body_node.value_type().dimensions().is_empty() {
            let body_type =
                lower_primitive_type(self.view, body_node.value_type_id(), arithmetic_profile())?;
            let result = self.builder.map(
                domain,
                &[value],
                body_type,
                at,
                |builder, captures, _, output| {
                    let value = builder.load(captures[0], at)?;
                    builder.store(output, value, at)
                },
            )?;
            return Ok(LoweredValue::scalar(value_type, result));
        }
        let dimensions = self
            .view
            .value_type(value_type)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
            .dimensions()
            .to_vec();
        let result = self.builder.fill(value, dimensions, at)?;
        Ok(LoweredValue::scalar(value_type, result))
    }

    fn load_domain_binders(
        &mut self,
        domain: dae::DomainId<'dae>,
        binders: &[solve::ProgramSlot<'program>],
        at: rumoca_core::Span,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        for (ordinal, &binder) in binders.iter().enumerate() {
            let register = self.builder.load(binder, at)?;
            let ordinal = u32::try_from(ordinal).map_err(|_| {
                solve::SolveProgramConstructionError::IdentityOverflow { provenance: at }
            })?;
            self.binders.insert((domain.index(), ordinal), register);
        }
        Ok(())
    }

    fn mapped_expression(
        &mut self,
        body: dae::ExprId<'dae>,
        domain_id: dae::DomainId<'dae>,
        domain: rumoca_core::StructuredIndexDomain,
        at: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let [leaf] = self
            .mapped_leaves(body, domain_id, domain, at)?
            .try_into()
            .map_err(|_| solve::SolveProgramConstructionError::InvalidMap { provenance: at })?;
        Ok(leaf)
    }

    /// One compact map per leaf of the body: a record-valued comprehension
    /// is the struct of arrays of its fields, each leaf a map over the
    /// domain that evaluates only the body field holding that leaf.
    fn mapped_leaves(
        &mut self,
        body: dae::ExprId<'dae>,
        domain_id: dae::DomainId<'dae>,
        domain: rumoca_core::StructuredIndexDomain,
        at: rumoca_core::Span,
    ) -> Result<Vec<solve::ProgramRegister<'program>>, solve::SolveProgramConstructionError> {
        if !self.pending_predicates([body])?.is_empty() {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: at,
            });
        }
        let body_types = lower_value_type_leaves(
            self.view,
            self.view
                .expression(body)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .value_type_id(),
            arithmetic_profile(),
        )?;
        if body_types.is_empty() {
            return Err(solve::SolveProgramConstructionError::InvalidMap { provenance: at });
        }
        let (captures, environment) = self.capture_environment_for([body])?;
        let environment = std::rc::Rc::new(environment);
        let mut leaves = Vec::with_capacity(body_types.len());
        for (leaf, body_type) in body_types.into_iter().enumerate() {
            let context = self.region_context();
            let environment = std::rc::Rc::clone(&environment);
            let domain = domain.clone();
            leaves.push(self.builder.map(
                domain,
                &captures,
                body_type,
                at,
                move |builder, captures, binders, output| {
                    let mut lowerer =
                        load_region_lowerer(builder, captures, &environment, &context, at)?;
                    lowerer.load_domain_binders(domain_id, binders, at)?;
                    let value = lowerer.expression_leaf(body, leaf, at)?;
                    lowerer.builder.store(output, value, at)
                },
            )?);
        }
        Ok(leaves)
    }

    /// Leaf `leaf` of a value: a record constructor lowers, and coerces to
    /// its declared field type, only the field that holds the leaf.
    fn expression_leaf(
        &mut self,
        expression: dae::ExprId<'dae>,
        leaf: usize,
        at: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let node = self
            .view
            .expression(expression)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        let value = match node.operation() {
            dae::ExpressionOperation::Record(arguments) => {
                let value_type = node.value_type_id();
                let mut selected = None;
                for (ordinal, argument) in arguments.iter().enumerate() {
                    let range = record_field_leaf_range(
                        self.view,
                        value_type,
                        ordinal,
                        arithmetic_profile(),
                    )?;
                    selected = selected.or(range.contains(&leaf).then_some((
                        ordinal,
                        argument,
                        range.start,
                    )));
                }
                let (ordinal, argument, start) =
                    selected.ok_or(solve::SolveProgramConstructionError::InvalidCallInterface {
                        provenance: at,
                    })?;
                let (_, field_type) = self.view.record_field(value_type, ordinal).ok_or(
                    solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at },
                )?;
                let field = self.expression(argument)?;
                let value = self.coerce_value(field, field_type, at)?;
                return value.leaves.get(leaf - start).copied().ok_or(
                    solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at },
                );
            }
            _ => self.expression(expression)?,
        };
        value
            .leaves
            .get(leaf)
            .copied()
            .ok_or(solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at })
    }

    fn array_update(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        base: dae::ExprId<'dae>,
        value: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        at: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        let base_value = self.expression(base)?;
        let base = base_value.only_register(at)?;
        let value = self.expression(value)?;
        let mut value_register = value.only_register(at)?;
        let base_scalar = self
            .view
            .value_type(base_value.value_type)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
            .scalar_type();
        let value_scalar = self
            .view
            .value_type(value.value_type)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
            .scalar_type();
        if base_scalar == dae::ScalarType::Real
            && matches!(
                value_scalar,
                dae::ScalarType::Integer | dae::ScalarType::Enumeration
            )
        {
            value_register = self.builder.convert(
                solve::SolveConversionOperator::IntegerToReal,
                value_register,
                at,
            )?;
        }
        let result = if self.needs_indexed_slice(subscripts) {
            let dimensions = self
                .view
                .value_type(value.value_type)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .dimensions()
                .to_vec();
            self.scatter_slice(base, value_register, &dimensions, subscripts, at)?
        } else if subscripts.iter().all(|subscript| {
            matches!(
                subscript,
                dae::SubscriptView::Whole { .. } | dae::SubscriptView::Slice { .. }
            )
        }) {
            let origin = self.contiguous_slice_origin(subscripts, at)?;
            self.builder
                .update_slice(base, value_register, origin, at)?
        } else if subscripts.iter().any(|subscript| {
            matches!(
                subscript,
                dae::SubscriptView::Whole { .. } | dae::SubscriptView::Slice { .. }
            )
        }) {
            let dimensions = self
                .view
                .value_type(value.value_type)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .dimensions()
                .to_vec();
            let axes = self.tensor_view_axes(subscripts, &dimensions, at)?;
            self.builder.update_view(base, value_register, &axes, at)?
        } else {
            let indices = subscripts
                .iter()
                .map(|subscript| match subscript {
                    dae::SubscriptView::Index { expression, .. } => {
                        self.expression(expression)?.only_register(at)
                    }
                    dae::SubscriptView::Whole { .. } | dae::SubscriptView::Slice { .. } => {
                        Err(solve::SolveProgramConstructionError::InvalidTensorAlgebra {
                            provenance: at,
                        })
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;
            let base_dimensions = self
                .view
                .value_type(base_value.value_type)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .dimensions();
            if indices.len() < base_dimensions.len() {
                // MLS 10.5.3: the unsubscripted trailing axes are whole, so the
                // update replaces the sub-array the leading indices select.
                let axes = leading_index_axes(&indices, &base_dimensions[indices.len()..]);
                self.builder.update_view(base, value_register, &axes, at)?
            } else {
                self.builder
                    .update_element(base, value_register, &indices, at)?
            }
        };
        Ok(LoweredValue::scalar(value_type, result))
    }

    // SPEC_0021: Exception - exhaustive checked typed-call interface lowering.
    #[allow(clippy::excessive_nesting)]
    fn call(
        &mut self,
        owner: dae::ExprId<'dae>,
        expression: dae::ExprId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        at: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        let call = self
            .callees
            .get(&owner)
            .cloned()
            .ok_or(solve::SolveProgramConstructionError::UnknownCallOwner { provenance: at })?;
        let Some(range) = call.result_layout.ranges.get(output as usize).cloned() else {
            return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance: at });
        };
        let values = match self.call_values.get(&owner).cloned() {
            Some(values) => values,
            None => {
                let lowered_arguments = self.call_arguments(expression, arguments, at)?;
                let issued = self.builder.emit_call(call.owner, &lowered_arguments, at)?;
                if issued.registers().len() != call.result_leaf_count + call.assertion_slots.len() {
                    return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                        provenance: at,
                    });
                }
                let predicate_range = self.predicate_ranges.get(&owner).cloned().ok_or(
                    solve::SolveProgramConstructionError::InvalidCallOutput { provenance: at },
                )?;
                if predicate_range.len() != call.assertion_slots.len() {
                    return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                        provenance: at,
                    });
                }
                for (offset, kind) in call.assertion_slots.iter().enumerate() {
                    if matches!(kind, AssertionSlot::Predicate { .. }) {
                        let parent = self.builder.assertion_output(
                            self.assertion_output_base + predicate_range.start + offset,
                            at,
                        )?;
                        self.builder.forward_assertion(
                            &issued,
                            call.result_leaf_count + offset,
                            parent,
                            at,
                        )?;
                    }
                }
                let values = issued.into_registers();
                for (slot, predicate) in
                    predicate_range.zip(values[call.result_leaf_count..].iter().copied())
                {
                    let destination = self.predicate_values.get_mut(slot).ok_or(
                        solve::SolveProgramConstructionError::InvalidCallOutput { provenance: at },
                    )?;
                    // A slot already holds a value only when a conditional
                    // region of this lowerer evaluated the same call owner
                    // and published its predicate (a region's call values
                    // stay inside the region). This evaluation is the same
                    // pure call in the same environment, unconditional here,
                    // so its predicate is exact on every path and the
                    // region's selected-or-vacuous value is subsumed by it.
                    *destination = Some(predicate);
                }
                self.call_values.insert(owner, values.clone());
                values
            }
        };
        let value_type = self
            .view
            .expression(expression)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
            .value_type_id();
        Ok(LoweredValue {
            value_type,
            leaves: values[range].to_vec(),
        })
    }

    fn call_arguments(
        &mut self,
        expression: dae::ExprId<'dae>,
        arguments: dae::ExpressionOperands<'dae>,
        at: rumoca_core::Span,
    ) -> Result<Vec<solve::ProgramRegister<'program>>, solve::SolveProgramConstructionError> {
        let node = self
            .view
            .expression(expression)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        let dae::ExpressionOperation::Call { function, .. } = node.operation() else {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: at,
            });
        };
        let function = self
            .view
            .function(function)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        let parameter_types = function.parameter_types();
        if parameter_types.len() != arguments.len() {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: at,
            });
        }
        let mut lowered = Vec::new();
        for (argument, target) in arguments.iter().zip(parameter_types.iter()) {
            if is_text_value(
                self.view
                    .value_type(target)
                    .ok_or(solve::SolveProgramConstructionError::WireMismatch)?,
            ) {
                // A text input occupies no leaf (a decomposed record's `name`
                // field, for example), so its argument is not lowered.
                continue;
            }
            let value = self.expression(argument)?;
            lowered.extend(self.coerce_value(value, target, at)?.leaves);
        }
        Ok(lowered)
    }

    fn literal(
        &mut self,
        literal: &dae::DaeLiteral,
        value_type: dae::ValueTypeId<'dae>,
        at: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        let value = match literal {
            dae::DaeLiteral::Real(value) => solve::SolveValue::real(arithmetic_profile(), *value),
            dae::DaeLiteral::Integer(value) | dae::DaeLiteral::Enumeration(value) => {
                solve::SolveValue::integer(arithmetic_profile(), *value).map_err(|_| {
                    solve::SolveProgramConstructionError::ProfileMismatch { provenance: at }
                })?
            }
            dae::DaeLiteral::Boolean(value) => solve::SolveValue::boolean(*value),
            dae::DaeLiteral::String(_) => {
                return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                    provenance: at,
                });
            }
        };
        let register = self.builder.constant(value, at)?;
        Ok(LoweredValue::scalar(value_type, register))
    }
}

#[cfg(test)]
mod tests;
