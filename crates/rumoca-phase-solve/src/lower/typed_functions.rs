//! Single-source DAE pure-function lowering into the checked typed vocabulary.

mod assertions;
mod captures;
mod folds;
pub(in crate::lower) mod model_events;
mod regions;
mod tensor;

use assertions::{assertion_conditions, assertion_is_map_independent, nested_calls};
use model_events::ModelCoordinateKey;
pub(super) use model_events::lower_model_event_transactions;
use regions::{
    EnvironmentLayout, RegionConditional, RegionContext, RegionValues, function_value_type,
    load_region_lowerer, lower_region_assignment_chain, lower_region_conditional,
    lower_region_values,
};

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
/// the resulting table is finite and topological by construction.
pub(crate) struct PureCallRegistry<'dae> {
    table: solve::SolvePureCallTableBuilder,
    identities: NestedIdentityIssuer,
    roots: HashMap<dae::ExprId<'dae>, RegisteredCall<'dae>>,
}

impl<'dae> PureCallRegistry<'dae> {
    pub(crate) fn new() -> Self {
        let arithmetic = arithmetic_profile();
        Self {
            table: solve::SolvePureCallTable::builder(arithmetic),
            identities: NestedIdentityIssuer::new(None),
            roots: HashMap::new(),
        }
    }

    pub(crate) fn register_root(
        &mut self,
        view: dae::DaeView<'dae>,
        call: dae::ExprId<'dae>,
    ) -> Result<RegisteredCall<'dae>, solve::SolveProgramConstructionError> {
        let call_node = view
            .expression(call)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        let dae::ExpressionOperation::Call { owner, .. } = call_node.operation() else {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: call_node.provenance().span(),
            });
        };
        if let Some(registered) = self.roots.get(&owner) {
            return Ok(registered.clone());
        }
        let provenance = view
            .expression(owner)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
            .provenance()
            .span();
        let identity = self.identities.issue(provenance)?;
        let registered = register_call(
            &mut self.table,
            view,
            owner,
            identity,
            arithmetic_profile(),
            &mut self.identities,
        )?;
        self.roots.insert(owner, registered.clone());
        Ok(registered)
    }

    pub(crate) fn finish(&mut self) -> solve::SolvePureCallTable {
        let replacement = solve::SolvePureCallTable::builder(arithmetic_profile());
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
    let mut identities = NestedIdentityIssuer::new(Some(identity));
    register_call(
        &mut table,
        view,
        call,
        identity,
        arithmetic,
        &mut identities,
    )?;
    Ok(table.finish())
}

#[derive(Clone)]
pub(crate) struct RegisteredCall<'dae> {
    pub(crate) owner: solve::SolvePureCallOwnerId,
    pub(crate) site: solve::SolvePureCallSite,
    pub(crate) result_ranges: Box<[Range<usize>]>,
    pub(crate) result_leaf_count: usize,
    pub(crate) assertion_count: usize,
    pub(crate) assertions: Box<[RegisteredAssertion<'dae>]>,
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
    pub(crate) provenance: dae::DaeProvenance,
}

#[derive(Clone)]
struct LoweredValue<'program, 'dae> {
    value_type: dae::ValueTypeId<'dae>,
    leaves: Vec<solve::ProgramRegister<'program>>,
}

impl<'program, 'dae> LoweredValue<'program, 'dae> {
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

struct NestedIdentityIssuer {
    reserved: Option<solve::SolvePureCallIdentity>,
    next: u64,
}

impl NestedIdentityIssuer {
    const fn new(reserved: Option<solve::SolvePureCallIdentity>) -> Self {
        Self { reserved, next: 1 }
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

// SPEC_0021: Exception - top-level typed-call owner construction entry point.
#[allow(clippy::too_many_lines)]
fn register_call<'dae>(
    table: &mut solve::SolvePureCallTableBuilder,
    view: dae::DaeView<'dae>,
    call: dae::ExprId<'dae>,
    identity: solve::SolvePureCallIdentity,
    arithmetic: solve::SolveArithmeticProfile,
    identities: &mut NestedIdentityIssuer,
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

fn boolean_map_type<'dae>(
    view: dae::DaeView<'dae>,
    domains: &[dae::DomainId<'dae>],
    arithmetic: solve::SolveArithmeticProfile,
    provenance: rumoca_core::Span,
) -> Result<solve::SolveValueType, solve::SolveProgramConstructionError> {
    if domains.is_empty() {
        return Ok(solve::SolveValueType::scalar(
            solve::SolveScalarType::Boolean,
        ));
    }
    let mut dimensions = Vec::new();
    for domain in domains {
        dimensions.extend(
            view.domain(*domain)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .structured()
                .extents()
                .map_err(|_| solve::SolveProgramConstructionError::InvalidMap { provenance })?
                .into_iter()
                .map(|extent| {
                    u32::try_from(extent).map_err(|_| {
                        solve::SolveProgramConstructionError::InvalidMap { provenance }
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
        );
    }
    let value_type = solve::SolveValueType::tensor(solve::SolveScalarType::Boolean, dimensions)
        .map_err(|_| solve::SolveProgramConstructionError::InvalidMap { provenance })?;
    if !value_type.belongs_to(arithmetic) {
        return Err(solve::SolveProgramConstructionError::InvalidMap { provenance });
    }
    Ok(value_type)
}

fn arithmetic_profile() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveRoundingMode::NearestTiesToEven,
        solve::SolveIntegerDomain::construct(i64::MIN, i64::MAX)
            .expect("the full i64 domain is nonempty"),
    )
}

/// Typed leaves one DAE value type occupies in a pure-call interface.
///
/// A leaf holds scalars, so a value type holds exactly as many leaves as it
/// takes to hold its scalars. MLS 3.6 §10.3.1 admits a zero-size array
/// dimension, and such a value holds no scalars at all: it occupies no leaf.
/// That is the same rule the record arm applies field by field, so a record
/// with a zero-size field is narrower than its field count by construction and
/// every consumer that walks leaves by width - the interface, the call site's
/// packing, and record-field projection - agrees without a second convention.
fn lower_value_type_leaves<'dae>(
    view: dae::DaeView<'dae>,
    id: dae::ValueTypeId<'dae>,
    arithmetic: solve::SolveArithmeticProfile,
) -> Result<Vec<solve::SolveValueType>, solve::SolveProgramConstructionError> {
    let value_type = view
        .value_type(id)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    if value_type.dimensions().contains(&0) {
        return Ok(Vec::new());
    }
    if !value_type.is_record() {
        return Ok(vec![lower_primitive_type(view, id, arithmetic)?]);
    }
    // A record the DAE resolved names its fields. A record that names none is
    // an unresolved type, not an empty one, so it never reaches an interface.
    if value_type.record_field_count() == 0 {
        return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
            provenance: value_type_provenance(view, id),
        });
    }
    let mut leaves = Vec::new();
    for ordinal in 0..value_type.record_field_count() {
        let (_, field_type) = view.record_field(id, ordinal).ok_or(
            solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: value_type_provenance(view, id),
            },
        )?;
        for field_leaf in lower_value_type_leaves(view, field_type, arithmetic)? {
            if value_type.dimensions().is_empty() {
                leaves.push(field_leaf);
                continue;
            }
            let mut dimensions = value_type.dimensions().to_vec();
            dimensions.extend_from_slice(field_leaf.dimensions());
            leaves.push(
                solve::SolveValueType::tensor(field_leaf.element_type(), dimensions).map_err(
                    |_| solve::SolveProgramConstructionError::InvalidCallInterface {
                        provenance: value_type_provenance(view, id),
                    },
                )?,
            );
        }
    }
    Ok(leaves)
}

fn value_type_provenance<'dae>(
    view: dae::DaeView<'dae>,
    id: dae::ValueTypeId<'dae>,
) -> rumoca_core::Span {
    view.value_type_provenance(id)
        .expect("final DAE value type carries provenance")
        .span()
}

fn record_field_leaf_range<'dae>(
    view: dae::DaeView<'dae>,
    record: dae::ValueTypeId<'dae>,
    field: usize,
    arithmetic: solve::SolveArithmeticProfile,
) -> Result<Range<usize>, solve::SolveProgramConstructionError> {
    let value_type = view
        .value_type(record)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    if !value_type.is_record() {
        return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
            provenance: value_type_provenance(view, record),
        });
    }
    let mut start = 0usize;
    for ordinal in 0..value_type.record_field_count() {
        let (_, field_type) = view.record_field(record, ordinal).ok_or(
            solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: value_type_provenance(view, record),
            },
        )?;
        let width = lower_value_type_leaves(view, field_type, arithmetic)?.len();
        if ordinal == field {
            return Ok(start..start + width);
        }
        start = start.checked_add(width).ok_or(
            solve::SolveProgramConstructionError::IdentityOverflow {
                provenance: value_type_provenance(view, record),
            },
        )?;
    }
    Err(solve::SolveProgramConstructionError::InvalidCallInterface {
        provenance: value_type_provenance(view, record),
    })
}

fn lower_primitive_type<'dae>(
    view: dae::DaeView<'dae>,
    id: dae::ValueTypeId<'dae>,
    arithmetic: solve::SolveArithmeticProfile,
) -> Result<solve::SolveValueType, solve::SolveProgramConstructionError> {
    let value_type = view
        .value_type(id)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    let scalar = match value_type.scalar_type() {
        dae::ScalarType::Real => solve::SolveScalarType::real(arithmetic),
        dae::ScalarType::Integer | dae::ScalarType::Enumeration => {
            solve::SolveScalarType::integer(arithmetic)
        }
        dae::ScalarType::Boolean => solve::SolveScalarType::Boolean,
        dae::ScalarType::String | dae::ScalarType::Record => {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: value_type_provenance(view, id),
            });
        }
    };
    if value_type.dimensions().is_empty() {
        Ok(solve::SolveValueType::scalar(scalar))
    } else {
        solve::SolveValueType::tensor(scalar, value_type.dimensions().to_vec()).map_err(|_| {
            solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: value_type_provenance(view, id),
            }
        })
    }
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
    conditional_groups: HashMap<dae::FunctionDefinitionId<'dae>, ConditionalDefinitionGroup<'dae>>,
    fold_parameters: HashMap<(dae::FunctionFoldId<'dae>, u32), LoweredValue<'program, 'dae>>,
    fold_values: HashMap<dae::FunctionFoldId<'dae>, Vec<LoweredValue<'program, 'dae>>>,
    binders: HashMap<(u32, u32), solve::ProgramRegister<'program>>,
    callees: HashMap<dae::ExprId<'dae>, RegisteredCall<'dae>>,
    predicate_ranges: HashMap<dae::ExprId<'dae>, Range<usize>>,
    cache: HashMap<dae::ExprId<'dae>, LoweredValue<'program, 'dae>>,
    call_values: HashMap<dae::ExprId<'dae>, Vec<solve::ProgramRegister<'program>>>,
    predicate_values: Vec<Option<solve::ProgramRegister<'program>>>,
    next_direct_assertion: usize,
    direct_assertion_count: usize,
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
                    let predicate = self
                        .expression(condition)?
                        .only_register(provenance.span())?;
                    self.record_assertion_predicate(predicate, provenance.span())?;
                }
                dae::FunctionStatementView::AssignmentGroup {
                    definitions,
                    conditional: Some(conditional),
                } => {
                    self.conditional_assignment(definitions, conditional)?;
                }
                dae::FunctionStatementView::For {
                    fold,
                    statements,
                    provenance,
                } => {
                    let values = self.function_fold(fold, provenance.span())?;
                    let fold = self
                        .view
                        .function_fold(fold)
                        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
                    if fold.output_values().len() != values.len() {
                        return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                            provenance: provenance.span(),
                        });
                    }
                    let domain = fold.domain();
                    // The loop's exit value of each carried target belongs to
                    // the output definition the fold issued, not to the
                    // in-body definitions that produced it.
                    self.function_values.extend(
                        fold.output_values()
                            .iter()
                            .map(|definition| definition.id())
                            .zip(values),
                    );
                    self.loop_assertions(statements, &mut vec![domain], provenance.span())?;
                }
            }
        }
        Ok(())
    }

    fn record_assertion_predicate(
        &mut self,
        predicate: solve::ProgramRegister<'program>,
        provenance: rumoca_core::Span,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        if self.next_direct_assertion >= self.direct_assertion_count {
            return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
        }
        self.predicate_values[self.next_direct_assertion] = Some(predicate);
        self.next_direct_assertion += 1;
        Ok(())
    }

    // SPEC_0021: Exception - exhaustive dispatch over loop statement variants.
    #[allow(clippy::excessive_nesting)]
    fn loop_assertions(
        &mut self,
        statements: dae::FunctionStatements<'dae>,
        domains: &mut Vec<dae::DomainId<'dae>>,
        provenance: rumoca_core::Span,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        for statement in statements {
            match statement {
                dae::FunctionStatementView::Assertion {
                    condition,
                    provenance,
                    ..
                } => {
                    if !assertion_is_map_independent(self.view, condition)
                        || !self.pending_predicates([condition]).is_empty()
                    {
                        return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                            provenance: provenance.span(),
                        });
                    }
                    let predicate =
                        self.map_assertion_predicate(condition, domains, provenance.span())?;
                    let predicate = self.builder.reduce(
                        solve::SolveReductionOperator::All,
                        predicate,
                        provenance.span(),
                    )?;
                    self.record_assertion_predicate(predicate, provenance.span())?;
                }
                dae::FunctionStatementView::For {
                    fold,
                    statements,
                    provenance: nested_provenance,
                } => {
                    let domain = self
                        .view
                        .function_fold(fold)
                        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                        .domain();
                    domains.push(domain);
                    self.loop_assertions(statements, domains, nested_provenance.span())?;
                    domains.pop();
                }
                dae::FunctionStatementView::Assignment { .. }
                | dae::FunctionStatementView::AssignmentGroup { .. } => {}
            }
        }
        if domains.is_empty() {
            return Err(solve::SolveProgramConstructionError::InvalidMap { provenance });
        }
        Ok(())
    }

    // SPEC_0021: Exception - exhaustive assertion-map statement dispatch.
    #[allow(clippy::excessive_nesting)]
    fn map_assertion_predicate(
        &mut self,
        condition: dae::ExprId<'dae>,
        domains: &[dae::DomainId<'dae>],
        provenance: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let Some((&domain_id, remaining)) = domains.split_first() else {
            return self.expression(condition)?.only_register(provenance);
        };
        let domain = self
            .view
            .domain(domain_id)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
            .structured()
            .clone();
        let body_type = boolean_map_type(self.view, remaining, arithmetic_profile(), provenance)?;
        let (captures, environment) = self.capture_environment_for([condition])?;
        let context = RegionContext {
            view: self.view,
            callees: self.callees.clone(),
            predicate_ranges: self.predicate_ranges.clone(),
            conditional_groups: self.conditional_groups.clone(),
            predicate_count: self.predicate_values.len(),
            direct_assertion_count: self.direct_assertion_count,
        };
        let remaining = remaining.to_vec();
        self.builder.map(
            domain,
            &captures,
            body_type,
            provenance,
            move |builder, captures, binders, output| {
                let mut lowerer =
                    load_region_lowerer(builder, captures, &environment, &context, provenance)?;
                for (ordinal, binder) in binders.iter().enumerate() {
                    let register = lowerer.builder.load(*binder, provenance)?;
                    let ordinal = u32::try_from(ordinal).map_err(|_| {
                        solve::SolveProgramConstructionError::IdentityOverflow { provenance }
                    })?;
                    lowerer
                        .binders
                        .insert((domain_id.index(), ordinal), register);
                }
                let predicate =
                    lowerer.map_assertion_predicate(condition, &remaining, provenance)?;
                lowerer.builder.store(output, predicate, provenance)
            },
        )
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
        let condition = self.expression(operands[0])?.only_register(provenance)?;
        let pending = self.pending_predicates(operands[1..].iter().copied());
        let mut output_types =
            lower_value_type_leaves(self.view, value_type, arithmetic_profile())?;
        let value_leaf_count = output_types.len();
        output_types.extend(std::iter::repeat_n(
            solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
            pending.len(),
        ));
        let (captures, environment) =
            self.capture_environment_for(operands[1..].iter().copied())?;
        let context = RegionContext {
            view: self.view,
            callees: self.callees.clone(),
            predicate_ranges: self.predicate_ranges.clone(),
            conditional_groups: self.conditional_groups.clone(),
            predicate_count: self.predicate_values.len(),
            direct_assertion_count: self.direct_assertion_count,
        };
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
        for (slot, predicate) in pending
            .into_iter()
            .zip(destinations[value_leaf_count..].iter().copied())
        {
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
        if !source_type.is_record()
            && !target_type.is_record()
            && source_type.dimensions() == target_type.dimensions()
            && target_type.scalar_type() == dae::ScalarType::Real
            && matches!(
                source_type.scalar_type(),
                dae::ScalarType::Integer | dae::ScalarType::Enumeration
            )
        {
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

    fn pending_predicates(
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
        if !self
            .pending_predicates(std::iter::once(*condition_expression))
            .is_empty()
        {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface { provenance });
        }
        let condition = self
            .expression(*condition_expression)?
            .only_register(provenance)?;
        let mut output_types = Vec::new();
        for value_type in value_types {
            output_types.extend(lower_value_type_leaves(
                self.view,
                *value_type,
                arithmetic_profile(),
            )?);
        }
        output_types.extend(std::iter::repeat_n(
            solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
            pending.len(),
        ));
        let capture_roots = conditions[1..]
            .iter()
            .copied()
            .chain(branches.iter().flatten().copied())
            .chain(fallback.iter().copied());
        let (captures, environment) = self.capture_environment_for(capture_roots)?;
        let context = RegionContext {
            view: self.view,
            callees: self.callees.clone(),
            predicate_ranges: self.predicate_ranges.clone(),
            conditional_groups: self.conditional_groups.clone(),
            predicate_count: self.predicate_values.len(),
            direct_assertion_count: self.direct_assertion_count,
        };
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
        self.builder.conditional(
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
                    false_value_types,
                    false_conditions,
                    false_branches,
                    false_fallback,
                    false_pending,
                    provenance,
                )
            },
        )
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
        let pending = self.pending_predicates(
            conditions
                .iter()
                .copied()
                .chain(branches.iter().flatten().copied())
                .chain(fallback.iter().copied()),
        );
        let mut output_types = Vec::new();
        let mut value_types = Vec::with_capacity(definitions.len());
        let mut value_ranges = Vec::with_capacity(definitions.len());
        for definition in &definitions {
            let value_type = function_value_type(self.view, definition.target(), at)?;
            value_types.push(value_type);
            let start = output_types.len();
            output_types.extend(lower_value_type_leaves(
                self.view,
                value_type,
                arithmetic_profile(),
            )?);
            value_ranges.push((value_type, start..output_types.len()));
        }
        let value_leaf_count = output_types.len();
        output_types.extend(std::iter::repeat_n(
            solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
            pending.len(),
        ));
        let destinations = self.assignment_conditional_chain(
            &value_types,
            &conditions,
            &branches,
            &fallback,
            &pending,
            at,
        )?;
        for (definition, (value_type, range)) in definitions.iter().zip(value_ranges) {
            self.function_values.insert(
                definition.id(),
                LoweredValue {
                    value_type,
                    leaves: destinations[range].to_vec(),
                },
            );
        }
        for (slot, predicate) in pending
            .into_iter()
            .zip(destinations[value_leaf_count..].iter().copied())
        {
            let value = self.predicate_values.get_mut(slot).ok_or(
                solve::SolveProgramConstructionError::InvalidCallOutput { provenance: at },
            )?;
            if value.replace(predicate).is_some() {
                return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                    provenance: at,
                });
            }
        }
        Ok(())
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
            dae::ExpressionOperation::Conditional(operands) => self.conditional(
                node.value_type_id(),
                &operands.iter().collect::<Vec<_>>(),
                at,
            )?,
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
            dae::ExpressionOperation::Comprehension { body, .. } => {
                self.comprehension(node.value_type_id(), body, at)?
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => self.array_update(node.value_type_id(), base, value, subscripts, at)?,
            dae::ExpressionOperation::Index { base, subscripts } => {
                self.index(node.value_type_id(), base, subscripts, at)?
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
            let field_elements = elements
                .iter()
                .map(|element| element.leaves[ordinal])
                .collect::<Vec<_>>();
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

    // SPEC_0021: Exception - exhaustive checked tensor-comprehension lowering.
    #[allow(clippy::excessive_nesting)]
    fn comprehension(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        body: dae::ExprId<'dae>,
        at: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        let body_node = self
            .view
            .expression(body)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        if let Some(domain_id) = body_node.binder_domain() {
            if !self.pending_predicates([body]).is_empty() {
                return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                    provenance: at,
                });
            }
            let domain = self
                .view
                .domain(domain_id)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .structured()
                .clone();
            let body_types = lower_value_type_leaves(
                self.view,
                body_node.value_type_id(),
                arithmetic_profile(),
            )?;
            let [body_type] = body_types.as_slice() else {
                return Err(solve::SolveProgramConstructionError::InvalidMap { provenance: at });
            };
            let (captures, environment) = self.capture_environment_for([body])?;
            let context = RegionContext {
                view: self.view,
                callees: self.callees.clone(),
                predicate_ranges: self.predicate_ranges.clone(),
                conditional_groups: self.conditional_groups.clone(),
                predicate_count: self.predicate_values.len(),
                direct_assertion_count: self.direct_assertion_count,
            };
            let result = self.builder.map(
                domain,
                &captures,
                body_type.clone(),
                at,
                move |builder, captures, binders, output| {
                    let mut lowerer =
                        load_region_lowerer(builder, captures, &environment, &context, at)?;
                    for (ordinal, binder) in binders.iter().enumerate() {
                        let register = lowerer.builder.load(*binder, at)?;
                        let ordinal = u32::try_from(ordinal).map_err(|_| {
                            solve::SolveProgramConstructionError::IdentityOverflow {
                                provenance: at,
                            }
                        })?;
                        lowerer
                            .binders
                            .insert((domain_id.index(), ordinal), register);
                    }
                    let value = lowerer.expression(body)?.only_register(at)?;
                    lowerer.builder.store(output, value, at)
                },
            )?;
            return Ok(LoweredValue::scalar(value_type, result));
        }
        let value = self.expression(body)?.only_register(at)?;
        let dimensions = self
            .view
            .value_type(value_type)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
            .dimensions()
            .to_vec();
        let result = self.builder.fill(value, dimensions, at)?;
        Ok(LoweredValue::scalar(value_type, result))
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
        let result = if subscripts.iter().all(|subscript| {
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
            self.builder
                .update_element(base, value_register, &indices, at)?
        };
        Ok(LoweredValue::scalar(value_type, result))
    }

    fn index(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        at: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        let base = self.expression(base)?;
        let base_types = lower_value_type_leaves(self.view, base.value_type, arithmetic_profile())?;
        let result_types = lower_value_type_leaves(self.view, value_type, arithmetic_profile())?;
        if base.leaves.len() != base_types.len() || base.leaves.len() != result_types.len() {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: at,
            });
        }
        if subscripts.iter().all(|subscript| {
            matches!(
                subscript,
                dae::SubscriptView::Whole { .. } | dae::SubscriptView::Slice { .. }
            )
        }) {
            let outer_origin = self.contiguous_slice_origin(subscripts, at)?;
            let leaves = base
                .leaves
                .iter()
                .zip(&base_types)
                .zip(&result_types)
                .map(|((&register, base_type), result_type)| {
                    let mut origin = outer_origin.clone();
                    origin.resize(base_type.dimensions().len(), 0);
                    self.builder.project_slice(
                        register,
                        origin,
                        result_type.dimensions().to_vec(),
                        at,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;
            return Ok(LoweredValue { value_type, leaves });
        }
        if subscripts.iter().any(|subscript| {
            matches!(
                subscript,
                dae::SubscriptView::Whole { .. } | dae::SubscriptView::Slice { .. }
            )
        }) {
            if base.leaves.len() != 1 {
                return Err(solve::SolveProgramConstructionError::InvalidProjection {
                    provenance: at,
                });
            }
            let axes = self.tensor_view_axes(subscripts, result_types[0].dimensions(), at)?;
            let result = self.builder.project_view(base.leaves[0], &axes, at)?;
            return Ok(LoweredValue::scalar(value_type, result));
        }
        let index_expressions = subscripts
            .iter()
            .map(|subscript| {
                let dae::SubscriptView::Index { expression, .. } = subscript else {
                    return Err(solve::SolveProgramConstructionError::InvalidProjection {
                        provenance: at,
                    });
                };
                Ok(expression)
            })
            .collect::<Result<Vec<_>, _>>()?;
        let static_indices = index_expressions
            .iter()
            .map(|expression| {
                let node = self.view.expression(*expression)?;
                let dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(index)) =
                    node.operation()
                else {
                    return None;
                };
                index
                    .checked_sub(1)
                    .and_then(|index| u32::try_from(index).ok())
            })
            .collect::<Option<Vec<_>>>();
        let dynamic_indices = static_indices.is_none().then(|| {
            index_expressions
                .iter()
                .map(|expression| self.expression(*expression)?.only_register(at))
                .collect::<Result<Vec<_>, _>>()
        });
        let dynamic_indices = dynamic_indices.transpose()?;
        let mut leaves = Vec::with_capacity(base.leaves.len());
        for ((&register, base_type), result_type) in
            base.leaves.iter().zip(&base_types).zip(&result_types)
        {
            let result = self.project_indexed_leaf(
                register,
                base_type.dimensions().len(),
                result_type.dimensions(),
                static_indices.as_deref(),
                dynamic_indices.as_deref(),
                at,
            )?;
            leaves.push(result);
        }
        Ok(LoweredValue { value_type, leaves })
    }

    fn project_indexed_leaf(
        &mut self,
        register: solve::ProgramRegister<'program>,
        base_rank: usize,
        result_dimensions: &[u32],
        static_indices: Option<&[u32]>,
        dynamic_indices: Option<&[solve::ProgramRegister<'program>]>,
        at: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let index_count = static_indices.map_or_else(
            || dynamic_indices.map_or(0, |indices| indices.len()),
            |indices| indices.len(),
        );
        if base_rank == index_count {
            if let Some(indices) = static_indices {
                return self.builder.project_element(register, indices.to_vec(), at);
            }
            let indices =
                dynamic_indices.ok_or(solve::SolveProgramConstructionError::InvalidProjection {
                    provenance: at,
                })?;
            return self.builder.project_element_dynamic(register, indices, at);
        }
        let indices = dynamic_indices
            .ok_or(solve::SolveProgramConstructionError::InvalidProjection { provenance: at })?;
        let mut axes = indices
            .iter()
            .copied()
            .map(solve::ProgramTensorViewAxis::Index)
            .collect::<Vec<_>>();
        axes.extend(
            result_dimensions
                .iter()
                .copied()
                .map(|extent| solve::ProgramTensorViewAxis::Span { origin: 0, extent }),
        );
        self.builder.project_view(register, &axes, at)
    }

    fn contiguous_slice_origin(
        &self,
        subscripts: dae::SubscriptsView<'dae>,
        at: rumoca_core::Span,
    ) -> Result<Vec<u32>, solve::SolveProgramConstructionError> {
        subscripts
            .iter()
            .map(|subscript| match subscript {
                dae::SubscriptView::Whole { .. } => Ok(0),
                dae::SubscriptView::Slice { expression, .. } => {
                    let range = self
                        .view
                        .expression(expression)
                        .and_then(|node| match node.operation() {
                            dae::ExpressionOperation::Range(range) => Some(range),
                            _ => None,
                        })
                        .ok_or(solve::SolveProgramConstructionError::InvalidProjection {
                            provenance: at,
                        })?;
                    if range.effective_step() != 1 {
                        return Err(solve::SolveProgramConstructionError::InvalidProjection {
                            provenance: at,
                        });
                    }
                    range
                        .start()
                        .value()
                        .checked_sub(1)
                        .and_then(|index| u32::try_from(index).ok())
                        .ok_or(solve::SolveProgramConstructionError::InvalidProjection {
                            provenance: at,
                        })
                }
                dae::SubscriptView::Index { .. } => {
                    Err(solve::SolveProgramConstructionError::InvalidProjection { provenance: at })
                }
            })
            .collect()
    }

    fn tensor_view_axes(
        &mut self,
        subscripts: dae::SubscriptsView<'dae>,
        result_dimensions: &[u32],
        at: rumoca_core::Span,
    ) -> Result<Vec<solve::ProgramTensorViewAxis<'program>>, solve::SolveProgramConstructionError>
    {
        let mut retained = result_dimensions.iter().copied();
        let axes = subscripts
            .iter()
            .map(|subscript| match subscript {
                dae::SubscriptView::Index { expression, .. } => self
                    .expression(expression)?
                    .only_register(at)
                    .map(solve::ProgramTensorViewAxis::Index),
                dae::SubscriptView::Whole { .. } => retained
                    .next()
                    .map(|extent| solve::ProgramTensorViewAxis::Span { origin: 0, extent })
                    .ok_or(solve::SolveProgramConstructionError::InvalidProjection {
                        provenance: at,
                    }),
                dae::SubscriptView::Slice { expression, .. } => {
                    let range = self
                        .view
                        .expression(expression)
                        .and_then(|node| match node.operation() {
                            dae::ExpressionOperation::Range(range) => Some(range),
                            _ => None,
                        })
                        .ok_or(solve::SolveProgramConstructionError::InvalidProjection {
                            provenance: at,
                        })?;
                    if range.effective_step() != 1 {
                        return Err(solve::SolveProgramConstructionError::InvalidProjection {
                            provenance: at,
                        });
                    }
                    let origin = range
                        .start()
                        .value()
                        .checked_sub(1)
                        .and_then(|index| u32::try_from(index).ok())
                        .ok_or(solve::SolveProgramConstructionError::InvalidProjection {
                            provenance: at,
                        })?;
                    let extent = retained.next().ok_or(
                        solve::SolveProgramConstructionError::InvalidProjection { provenance: at },
                    )?;
                    Ok(solve::ProgramTensorViewAxis::Span { origin, extent })
                }
            })
            .collect::<Result<Vec<_>, _>>()?;
        if retained.next().is_some() {
            return Err(solve::SolveProgramConstructionError::InvalidProjection { provenance: at });
        }
        Ok(axes)
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
        let Some(range) = call.result_ranges.get(output as usize).cloned() else {
            return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance: at });
        };
        let values = match self.call_values.get(&owner).cloned() {
            Some(values) => values,
            None => {
                let mut lowered_arguments = Vec::new();
                for argument in arguments.iter() {
                    lowered_arguments.extend(self.expression(argument)?.leaves);
                }
                let values = self.builder.call(call.owner, &lowered_arguments, at)?;
                if values.len() != call.result_leaf_count + call.assertion_count {
                    return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                        provenance: at,
                    });
                }
                let predicate_range = self.predicate_ranges.get(&owner).cloned().ok_or(
                    solve::SolveProgramConstructionError::InvalidCallOutput { provenance: at },
                )?;
                if predicate_range.len() != call.assertion_count {
                    return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                        provenance: at,
                    });
                }
                for (slot, predicate) in
                    predicate_range.zip(values[call.result_leaf_count..].iter().copied())
                {
                    let destination = self.predicate_values.get_mut(slot).ok_or(
                        solve::SolveProgramConstructionError::InvalidCallOutput { provenance: at },
                    )?;
                    if destination.replace(predicate).is_some() {
                        return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                            provenance: at,
                        });
                    }
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
