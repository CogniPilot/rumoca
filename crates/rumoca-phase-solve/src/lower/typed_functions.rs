//! Single-source DAE pure-function lowering into the checked typed vocabulary.

mod assertions;
mod model_events;
mod regions;
mod tensor;

use assertions::{assertion_conditions, assertion_is_map_independent, nested_calls};
use model_events::ModelCoordinateKey;
pub(super) use model_events::lower_model_event_transactions;
use regions::{
    EnvironmentLayout, RegionContext, function_value_type, load_region_lowerer,
    lower_region_assignment_chain, lower_region_conditional, lower_region_values,
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
                    .get(&definition.target())
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
    if std::env::var_os("RUMOCA_PROFILE_IR").is_some() {
        eprintln!(
            "rumoca-ir-profile kind=pure-call-owner owner={} function={} source={} start={} end={}",
            owner.index(),
            function.name(),
            provenance.source.0,
            provenance.start.0,
            provenance.end.0,
        );
    }
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

fn lower_value_type_leaves<'dae>(
    view: dae::DaeView<'dae>,
    id: dae::ValueTypeId<'dae>,
    arithmetic: solve::SolveArithmeticProfile,
) -> Result<Vec<solve::SolveValueType>, solve::SolveProgramConstructionError> {
    let value_type = view
        .value_type(id)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    if !value_type.is_record() {
        return Ok(vec![lower_primitive_type(view, id, arithmetic)?]);
    }
    if !value_type.dimensions().is_empty() {
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
        leaves.extend(lower_value_type_leaves(view, field_type, arithmetic)?);
    }
    if leaves.is_empty() {
        return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
            provenance: value_type_provenance(view, id),
        });
    }
    Ok(leaves)
}

fn value_type_provenance<'dae>(
    view: dae::DaeView<'dae>,
    id: dae::ValueTypeId<'dae>,
) -> rumoca_core::Span {
    view.value_type_provenance(id)
        .map_or(rumoca_core::Span::DUMMY, dae::DaeProvenance::span)
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
    if !value_type.is_record() || !value_type.dimensions().is_empty() {
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
    function_values: HashMap<dae::FunctionValueId<'dae>, LoweredValue<'program, 'dae>>,
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
    fn statements(
        &mut self,
        statements: dae::FunctionStatements<'dae>,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        for statement in statements {
            match statement {
                dae::FunctionStatementView::Assignment { definition } => {
                    let value = self.expression(definition.rhs())?;
                    let target_type = function_value_type(
                        self.view,
                        definition.target(),
                        definition.provenance().span(),
                    )?;
                    let value =
                        self.coerce_value(value, target_type, definition.provenance().span())?;
                    self.function_values.insert(definition.target(), value);
                }
                dae::FunctionStatementView::AssignmentGroup {
                    definitions,
                    conditional: None,
                } => {
                    let values = definitions
                        .iter()
                        .map(|definition| {
                            let value = self.expression(definition.rhs())?;
                            let target_type = function_value_type(
                                self.view,
                                definition.target(),
                                definition.provenance().span(),
                            )?;
                            self.coerce_value(value, target_type, definition.provenance().span())
                                .map(|value| (definition.target(), value))
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    self.function_values.extend(values);
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
                    if fold.targets().len() != values.len() {
                        return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                            provenance: provenance.span(),
                        });
                    }
                    let domain = fold.domain();
                    self.function_values.extend(fold.targets().zip(values));
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
        let (captures, environment) = self.capture_environment();
        let context = RegionContext {
            view: self.view,
            callees: self.callees.clone(),
            predicate_ranges: self.predicate_ranges.clone(),
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

    fn function_fold(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        provenance: rumoca_core::Span,
    ) -> Result<Vec<LoweredValue<'program, 'dae>>, solve::SolveProgramConstructionError> {
        if let Some(values) = self.fold_values.get(&fold) {
            return Ok(values.clone());
        }
        let fold_view = self
            .view
            .function_fold(fold)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        let initial_expressions = fold_view.initial_values().rhs_iter().collect::<Vec<_>>();
        let update_expressions = fold_view.update_values().rhs_iter().collect::<Vec<_>>();
        let carried_targets = fold_view.targets().collect::<Vec<_>>();
        if initial_expressions.is_empty()
            && update_expressions.is_empty()
            && carried_targets.is_empty()
        {
            self.fold_values.insert(fold, Vec::new());
            return Ok(Vec::new());
        }
        if initial_expressions.is_empty() || initial_expressions.len() != update_expressions.len() {
            return Err(solve::SolveProgramConstructionError::InvalidFold { provenance });
        }
        if carried_targets.len() != initial_expressions.len() {
            return Err(solve::SolveProgramConstructionError::InvalidFold { provenance });
        }
        if !self
            .pending_predicates(update_expressions.iter().copied())
            .is_empty()
        {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface { provenance });
        }
        let mut initial = Vec::with_capacity(initial_expressions.len());
        let mut initial_flat = Vec::new();
        let mut carried_layout = Vec::with_capacity(initial_expressions.len());
        for (ordinal, expression) in initial_expressions.iter().copied().enumerate() {
            let value = self.expression(expression)?;
            let start = initial_flat.len();
            initial_flat.extend(value.leaves.iter().copied());
            let carried = u32::try_from(ordinal).map_err(|_| {
                solve::SolveProgramConstructionError::IdentityOverflow { provenance }
            })?;
            carried_layout.push((carried, value.value_type, start..initial_flat.len()));
            initial.push(value);
        }
        let domain_id = fold_view.domain();
        let domain = self
            .view
            .domain(domain_id)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
            .structured()
            .clone();
        let (captures, environment) = self.capture_environment();
        let context = RegionContext {
            view: self.view,
            callees: self.callees.clone(),
            predicate_ranges: self.predicate_ranges.clone(),
            predicate_count: self.predicate_values.len(),
            direct_assertion_count: self.direct_assertion_count,
        };
        let transition_layout = carried_layout.clone();
        let destinations = self.builder.fold(
            domain,
            &initial_flat,
            &captures,
            provenance,
            move |builder, carried, captures, binders, outputs| {
                let loaded_captures = captures
                    .iter()
                    .map(|capture| builder.load(*capture, provenance))
                    .collect::<Result<Vec<_>, _>>()?;
                let parameters = environment
                    .parameters
                    .iter()
                    .map(|(id, value_type, range)| {
                        (
                            *id,
                            LoweredValue {
                                value_type: *value_type,
                                leaves: loaded_captures[range.clone()].to_vec(),
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
                                leaves: loaded_captures[range.clone()].to_vec(),
                            },
                        )
                    })
                    .collect();
                let mut function_values = environment
                    .values
                    .iter()
                    .map(|(id, value_type, range)| {
                        (
                            *id,
                            LoweredValue {
                                value_type: *value_type,
                                leaves: loaded_captures[range.clone()].to_vec(),
                            },
                        )
                    })
                    .collect::<HashMap<_, _>>();
                let mut fold_parameters = environment
                    .fold_parameters
                    .iter()
                    .map(|(fold, carried, value_type, range)| {
                        (
                            (*fold, *carried),
                            LoweredValue {
                                value_type: *value_type,
                                leaves: loaded_captures[range.clone()].to_vec(),
                            },
                        )
                    })
                    .collect::<HashMap<_, _>>();
                let mut loaded_carried = Vec::with_capacity(carried.len());
                for slot in carried {
                    loaded_carried.push(builder.load(*slot, provenance)?);
                }
                for (ordinal, value_type, range) in &transition_layout {
                    let value = LoweredValue {
                        value_type: *value_type,
                        leaves: loaded_carried[range.clone()].to_vec(),
                    };
                    fold_parameters.insert((fold, *ordinal), value.clone());
                    let target = carried_targets
                        .get(*ordinal as usize)
                        .ok_or(solve::SolveProgramConstructionError::InvalidFold { provenance })?;
                    function_values.insert(*target, value);
                }
                let mut binder_values = environment
                    .binders
                    .iter()
                    .map(|(binder, range)| {
                        let [register] = &loaded_captures[range.clone()] else {
                            unreachable!("checked binder capture owns one scalar register")
                        };
                        (*binder, *register)
                    })
                    .collect::<HashMap<_, _>>();
                for (ordinal, slot) in binders.iter().enumerate() {
                    let register = builder.load(*slot, provenance)?;
                    let ordinal = u32::try_from(ordinal).map_err(|_| {
                        solve::SolveProgramConstructionError::IdentityOverflow { provenance }
                    })?;
                    binder_values.insert((domain_id.index(), ordinal), register);
                }
                let mut lowerer = ExpressionLowerer {
                    view: context.view,
                    builder,
                    model_coordinates,
                    parameters,
                    function_values,
                    fold_parameters,
                    fold_values: HashMap::new(),
                    binders: binder_values,
                    callees: context.callees,
                    predicate_ranges: context.predicate_ranges,
                    cache: HashMap::new(),
                    call_values: HashMap::new(),
                    predicate_values: vec![None; context.predicate_count],
                    next_direct_assertion: 0,
                    direct_assertion_count: context.direct_assertion_count,
                };
                let mut updated = Vec::new();
                for (expression, (ordinal, value_type, _)) in
                    update_expressions.iter().zip(&transition_layout)
                {
                    let value = lowerer.expression(*expression)?;
                    let value = lowerer.coerce_value(value, *value_type, provenance)?;
                    let target = carried_targets
                        .get(*ordinal as usize)
                        .ok_or(solve::SolveProgramConstructionError::InvalidFold { provenance })?;
                    // The DAE fold tuple is ordered by the function's
                    // sequential redefinitions. A later tuple member reads
                    // the completed value of every preceding target, while a
                    // FunctionFoldParameter continues to name the iteration's
                    // entry value. Preserve that issued order directly.
                    lowerer.function_values.insert(*target, value.clone());
                    updated.extend(value.leaves);
                }
                if updated.len() != outputs.len() {
                    return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                        provenance,
                    });
                }
                for (output, value) in outputs.iter().zip(updated) {
                    lowerer.builder.store(*output, value, provenance)?;
                }
                Ok(())
            },
        )?;
        let values = carried_layout
            .iter()
            .map(|(_, value_type, range)| LoweredValue {
                value_type: *value_type,
                leaves: destinations[range.clone()].to_vec(),
            })
            .collect::<Vec<_>>();
        self.fold_values.insert(fold, values.clone());
        Ok(values)
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
        let (captures, environment) = self.capture_environment();
        let context = RegionContext {
            view: self.view,
            callees: self.callees.clone(),
            predicate_ranges: self.predicate_ranges.clone(),
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
                    value_type,
                    true_operands,
                    true_pending,
                    provenance,
                )
            },
            move |builder, inputs, outputs| {
                lower_region_conditional(
                    builder,
                    inputs,
                    outputs,
                    &environment,
                    &context,
                    value_type,
                    false_operands,
                    false_pending,
                    provenance,
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

    fn capture_environment(
        &self,
    ) -> (
        Vec<solve::ProgramRegister<'program>>,
        EnvironmentLayout<'dae>,
    ) {
        let mut captures = Vec::new();
        let mut model_coordinates = self.model_coordinates.iter().collect::<Vec<_>>();
        model_coordinates.sort_by_key(|(key, _)| key.stable_key());
        let model_coordinates = model_coordinates
            .into_iter()
            .map(|(key, value)| {
                let start = captures.len();
                captures.extend(value.leaves.iter().copied());
                (*key, value.value_type, start..captures.len())
            })
            .collect();
        let mut parameters = self.parameters.iter().collect::<Vec<_>>();
        parameters.sort_by_key(|(id, _)| **id);
        let parameters = parameters
            .into_iter()
            .map(|(id, value)| {
                let start = captures.len();
                captures.extend(value.leaves.iter().copied());
                (*id, value.value_type, start..captures.len())
            })
            .collect();
        let mut values = self.function_values.iter().collect::<Vec<_>>();
        values.sort_by_key(|(id, _)| **id);
        let values = values
            .into_iter()
            .map(|(id, value)| {
                let start = captures.len();
                captures.extend(value.leaves.iter().copied());
                (*id, value.value_type, start..captures.len())
            })
            .collect();
        let mut fold_parameters = self.fold_parameters.iter().collect::<Vec<_>>();
        fold_parameters.sort_by_key(|((fold, carried), _)| {
            (fold.function().index(), fold.ordinal(), *carried)
        });
        let fold_parameters = fold_parameters
            .into_iter()
            .map(|((fold, carried), value)| {
                let start = captures.len();
                captures.extend(value.leaves.iter().copied());
                (*fold, *carried, value.value_type, start..captures.len())
            })
            .collect();
        let mut binders = self.binders.iter().collect::<Vec<_>>();
        binders.sort_by_key(|(id, _)| **id);
        let binders = binders
            .into_iter()
            .map(|(id, register)| {
                let start = captures.len();
                captures.push(*register);
                (*id, start..captures.len())
            })
            .collect();
        (
            captures,
            EnvironmentLayout {
                model_coordinates,
                parameters,
                values,
                fold_parameters,
                binders,
            },
        )
    }

    fn pending_predicates(
        &self,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>>,
    ) -> Vec<usize> {
        let mut pending = BTreeSet::new();
        for root in expressions {
            dae::for_each_expression(self.view, root, |_, node| {
                if let dae::ExpressionOperation::Call { owner, .. } = node.operation() {
                    if let Some(range) = self.predicate_ranges.get(&owner) {
                        for slot in range.clone() {
                            if self.predicate_values.get(slot).is_some_and(Option::is_none) {
                                pending.insert(slot);
                            }
                        }
                    }
                }
            });
        }
        pending.into_iter().collect()
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
        let (captures, environment) = self.capture_environment();
        let context = RegionContext {
            view: self.view,
            callees: self.callees.clone(),
            predicate_ranges: self.predicate_ranges.clone(),
            predicate_count: self.predicate_values.len(),
            direct_assertion_count: self.direct_assertion_count,
        };
        let true_environment = environment.clone();
        let true_context = context.clone();
        let true_branch = branch.clone();
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
                    &true_branch,
                    &true_pending,
                    provenance,
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
            .map_or(rumoca_core::Span::DUMMY, |value| value.provenance().span());
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
                definition.target(),
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
            dae::ExpressionOperation::FunctionValue { value, definition } => self
                .function_values
                .get(&value)
                .cloned()
                .map_or_else(|| self.expression(definition.rhs()), Ok)?,
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
        for argument in arguments.iter() {
            leaves.extend(self.expression(argument)?.leaves);
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
        let elements = arguments
            .iter()
            .map(|argument| self.expression(argument)?.only_register(at))
            .collect::<Result<Vec<_>, _>>()?;
        let dimensions = self
            .view
            .value_type(value_type)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
            .dimensions()
            .to_vec();
        let register = self
            .builder
            .construct_aggregate(&elements, dimensions, at)?;
        Ok(LoweredValue::scalar(value_type, register))
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
            let (captures, environment) = self.capture_environment();
            let context = RegionContext {
                view: self.view,
                callees: self.callees.clone(),
                predicate_ranges: self.predicate_ranges.clone(),
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
        let base = self.expression(base)?.only_register(at)?;
        if subscripts.iter().all(|subscript| {
            matches!(
                subscript,
                dae::SubscriptView::Whole { .. } | dae::SubscriptView::Slice { .. }
            )
        }) {
            let origin = self.contiguous_slice_origin(subscripts, at)?;
            let dimensions = self
                .view
                .value_type(value_type)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .dimensions()
                .to_vec();
            let result = self.builder.project_slice(base, origin, dimensions, at)?;
            return Ok(LoweredValue::scalar(value_type, result));
        }
        if subscripts.iter().any(|subscript| {
            matches!(
                subscript,
                dae::SubscriptView::Whole { .. } | dae::SubscriptView::Slice { .. }
            )
        }) {
            let dimensions = self
                .view
                .value_type(value_type)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .dimensions()
                .to_vec();
            let axes = self.tensor_view_axes(subscripts, &dimensions, at)?;
            let result = self.builder.project_view(base, &axes, at)?;
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
        let result = if let Some(indices) = static_indices {
            self.builder.project_element(base, indices, at)?
        } else {
            let indices = index_expressions
                .into_iter()
                .map(|expression| self.expression(expression)?.only_register(at))
                .collect::<Result<Vec<_>, _>>()?;
            self.builder.project_element_dynamic(base, &indices, at)?
        };
        Ok(LoweredValue::scalar(value_type, result))
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
