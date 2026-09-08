use super::*;

pub(super) struct EquationRows<'scope, 'dae> {
    pub(super) source: EquationSource<'scope>,
    pub(super) excluded: &'scope HashSet<usize>,
    pub(super) records: &'scope HashMap<usize, RecordEquationPlan>,
    pub(super) multi_output: &'scope HashMap<usize, MultiOutputEquationPlan>,
    pub(super) topology: &'scope DiscreteValueTopologyPlan,
    pub(super) clocked_owners: &'scope HashMap<usize, ClockPlan>,
    pub(super) clocks: &'scope LoweredClocks<'dae>,
    /// MLS §3.7.4.5 Rule 1 / Rule 2 replacements proven by analysis. A row
    /// listed here is lowered from the rule's residual instead of the source
    /// one; the row count, its owner, and its balance contribution are
    /// unchanged, which is why the rule needs no separate equation identity.
    pub(super) semi_linear: &'scope SemiLinearRules,
}

pub(super) enum EquationSource<'scope> {
    Model(ModelEquationSequence<'scope>),
    Initialization(&'scope [flat::Equation]),
}

struct OrdinaryEquationRow<'input, 'scope, 'dae> {
    input: &'input EquationRowEnvironment<'scope, 'dae>,
    index: usize,
    equation: &'scope flat::Equation,
    owner: dae::DaeProvenance,
    generation: Option<dae::DaeGeneration>,
    owner_clock: Option<dae::PeriodicClockId<'dae>>,
}

/// A model row's common data together with the partition it was issued with.
///
/// They travel as one value so the ordinary model helper cannot be handed a
/// partition that belongs to a different row.
struct OrdinaryModelEquationRow<'input, 'scope, 'dae> {
    common: OrdinaryEquationRow<'input, 'scope, 'dae>,
    partition: EquationPartition<'scope>,
}

struct EquationLoweringContext<'borrow, 'scope, 'dae> {
    construction: &'borrow mut dae::DaeConstruction<'dae>,
    discrete_values: &'borrow mut DiscreteValueStaging<'dae>,
    coordinates: &'borrow HashMap<VarName, Coordinate<'dae>>,
    coordinate_instances: &'borrow HashMap<rumoca_core::InstanceId, Coordinate<'dae>>,
    functions: &'borrow FunctionRegistry<'scope, 'dae>,
    input: &'borrow EquationRowEnvironment<'scope, 'dae>,
}

struct EquationRowEnvironment<'scope, 'dae> {
    excluded: &'scope HashSet<usize>,
    records: &'scope HashMap<usize, RecordEquationPlan>,
    multi_output: &'scope HashMap<usize, MultiOutputEquationPlan>,
    topology: &'scope DiscreteValueTopologyPlan,
    clocked_owners: &'scope HashMap<usize, ClockPlan>,
    clocks: &'scope LoweredClocks<'dae>,
    semi_linear: &'scope SemiLinearRules,
}

pub(super) struct DeferredModelEquationRows<'flat> {
    rows: std::collections::BTreeMap<usize, ModelEquationRow<'flat>>,
}

impl<'flat> DeferredModelEquationRows<'flat> {
    fn intentionally_empty() -> Self {
        Self {
            rows: std::collections::BTreeMap::new(),
        }
    }

    pub(super) fn take_partition(
        &mut self,
        index: usize,
        span: Span,
    ) -> Result<EquationPartition<'flat>, dae::DaeConstructionError> {
        self.rows
            .remove(&index)
            .map(|row| row.into_parts().2)
            .ok_or(dae::DaeConstructionError::InvalidExpressionForm { span })
    }

    pub(super) fn consume_non_structured_claims(
        &mut self,
        structured: &HashSet<usize>,
        claims: impl IntoIterator<Item = usize>,
    ) {
        for index in claims {
            if !structured.contains(&index) {
                self.rows.remove(&index);
            }
        }
    }

    pub(super) fn finish(self) -> Result<(), dae::DaeConstructionError> {
        if let Some(row) = self.rows.into_values().next() {
            return Err(dae::DaeConstructionError::InvalidExpressionForm {
                span: row.equation().span,
            });
        }
        Ok(())
    }
}

pub(super) fn lower_equations<'scope, 'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    coordinate_instances: &HashMap<rumoca_core::InstanceId, Coordinate<'dae>>,
    functions: &FunctionRegistry<'scope, 'dae>,
    input: EquationRows<'scope, 'dae>,
) -> Result<DeferredModelEquationRows<'scope>, dae::DaeConstructionError> {
    let EquationRows {
        source,
        excluded,
        records,
        multi_output,
        topology,
        clocked_owners,
        clocks,
        semi_linear,
    } = input;
    let environment = EquationRowEnvironment {
        excluded,
        records,
        multi_output,
        topology,
        clocked_owners,
        clocks,
        semi_linear,
    };
    let mut deferred = DeferredModelEquationRows::intentionally_empty();
    let mut context = EquationLoweringContext {
        construction,
        discrete_values,
        coordinates,
        coordinate_instances,
        functions,
        input: &environment,
    };
    match source {
        EquationSource::Model(sequence) => {
            for row in sequence.into_rows() {
                if environment.excluded.contains(&row.index()) {
                    deferred.rows.insert(row.index(), row);
                    continue;
                }
                lower_model_equation_row(&mut context, row)?;
            }
        }
        EquationSource::Initialization(equations) => {
            for (index, equation) in equations.iter().enumerate() {
                if environment.excluded.contains(&index) {
                    continue;
                }
                lower_initialization_equation_row(&mut context, index, equation)?;
            }
        }
    }
    Ok(deferred)
}

/// Shared preparation for both phases: one provenance issuance, then the
/// multi-output and record branches in their established priority.
///
/// `Ok(None)` means a higher-priority branch **positively lowered** this row and
/// succeeded: the multi-output or record call ran, its `?` propagated any error,
/// and only then is `None` returned. It does not mean "no checker objected", and
/// it is not the removed phase tag wearing an `Option`; the partition never
/// travels through here. The
/// phase distinction reaches this function as `is_initialization` because the
/// multi-output lowering has always taken it as a flag; it is never stored
/// beside the partition, so no row can carry a phase that disagrees with the
/// function lowering it.
fn prepare_equation_row<'borrow, 'scope, 'dae>(
    context: &mut EquationLoweringContext<'borrow, 'scope, 'dae>,
    index: usize,
    equation: &'scope flat::Equation,
    is_initialization: bool,
) -> Result<Option<OrdinaryEquationRow<'borrow, 'scope, 'dae>>, dae::DaeConstructionError> {
    let input = context.input;
    let owner = equation_owner_provenance(&equation.origin, equation.span)?;
    let generation = equation_generation(&equation.origin);
    let owner_clock = input
        .clocked_owners
        .get(&index)
        .map(|plan| input.clocks.id(plan, equation.span))
        .transpose()?;
    if let Some(plan) = input.multi_output.get(&index) {
        lower_multi_output_equation(
            context.construction,
            context.coordinates,
            context.functions,
            equation,
            plan,
            owner,
            is_initialization,
        )?;
        return Ok(None);
    }
    if let Some(plan) = input.records.get(&index) {
        lower_record_equation(
            &mut RecordEquationLowering {
                construction: context.construction,
                discrete_values: context.discrete_values,
                coordinates: context.coordinates,
                coordinate_instances: context.coordinate_instances,
                functions: context.functions,
                topology: input.topology,
            },
            equation,
            plan,
            owner,
        )?;
        return Ok(None);
    }
    Ok(Some(OrdinaryEquationRow {
        input,
        index,
        equation,
        owner,
        generation,
        owner_clock,
    }))
}

/// Lower one model equation, consuming its row so the index, equation and
/// partition cannot travel apart.
fn lower_model_equation_row<'scope, 'dae>(
    context: &mut EquationLoweringContext<'_, 'scope, 'dae>,
    row: ModelEquationRow<'scope>,
) -> Result<(), dae::DaeConstructionError> {
    let (index, equation, partition) = row.into_parts();
    let Some(ordinary) = prepare_equation_row(context, index, equation, false)? else {
        return Ok(());
    };
    lower_ordinary_model_equation(
        context.construction,
        context.discrete_values,
        context.coordinates,
        context.functions,
        OrdinaryModelEquationRow {
            common: ordinary,
            partition,
        },
    )
}

/// Lower one initial equation. MLS 3.7.4.5's rules are stated for the model
/// equations, so an initial equation carries no partition and is lowered
/// exactly as written.
fn lower_initialization_equation_row<'scope, 'dae>(
    context: &mut EquationLoweringContext<'_, 'scope, 'dae>,
    index: usize,
    equation: &'scope flat::Equation,
) -> Result<(), dae::DaeConstructionError> {
    let Some(ordinary) = prepare_equation_row(context, index, equation, true)? else {
        return Ok(());
    };
    lower_ordinary_initialization_equation(
        context.construction,
        context.coordinates,
        context.functions,
        ordinary,
    )
}

fn lower_ordinary_initialization_equation<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    row: OrdinaryEquationRow<'_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let residual = lower_expression(
        construction,
        coordinates,
        functions,
        &row.equation.residual,
        row.generation,
    )?;
    construction.initialization(|system| system.value_equation(row.owner, residual))?;
    Ok(())
}

fn lower_ordinary_model_equation<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    row: OrdinaryModelEquationRow<'_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let OrdinaryModelEquationRow { common, partition } = row;
    let OrdinaryEquationRow {
        input,
        index,
        equation,
        owner,
        generation,
        owner_clock,
    } = common;
    match partition {
        EquationPartition::Continuous => {
            let (source, generation) = match input.semi_linear.residual(index) {
                Some(replacement) => (replacement, Some(dae::DaeGeneration::SemiLinearLowering)),
                None => (&equation.residual, generation),
            };
            let residual = lower_equation_expression(
                construction,
                coordinates,
                functions,
                owner_clock,
                source,
                generation,
            )?;
            construction.continuous(|system| system.value_equation(owner, residual))?;
        }
        EquationPartition::DiscreteReal { .. } => {
            let residual = lower_equation_expression(
                construction,
                coordinates,
                functions,
                owner_clock,
                &equation.residual,
                generation,
            )?;
            construction.discrete(|system| {
                system.real_equation(owner, |equation| equation.residual(residual))
            })?;
        }
        EquationPartition::DiscreteValue(plan) => {
            let generation = if plan.generated {
                Some(dae::DaeGeneration::DiscreteUpdate)
            } else {
                generation
            };
            let value = lower_equation_expression(
                construction,
                coordinates,
                functions,
                owner_clock,
                plan.value.as_ref(),
                generation,
            )?;
            let Some(Coordinate::DiscreteValue(target)) = coordinates.get(&plan.target).copied()
            else {
                return Err(dae::DaeConstructionError::InvalidVariableRole {
                    name: plan.target.clone(),
                    span: equation.span,
                });
            };
            let semantic_owner = discrete_values
                .owner(owner, [plan.target.clone()], coordinates, input.topology)?
                .ok_or(dae::DaeConstructionError::InvalidDiscreteTopologyPlan {
                    target: target.index(),
                    span: equation.span,
                })?;
            discrete_values.always(
                semantic_owner,
                target,
                value,
                owner,
                dae::DaeProvenance::source(equation.span)?,
            )?;
        }
        EquationPartition::ConsumedDiscreteValue => {}
    }
    Ok(())
}

pub(super) fn equation_generation(origin: &flat::EquationOrigin) -> Option<dae::DaeGeneration> {
    match origin {
        flat::EquationOrigin::ComponentEquation { .. } => None,
        flat::EquationOrigin::Connection { .. }
        | flat::EquationOrigin::OutsideStream { .. }
        | flat::EquationOrigin::EqualityConstraint { .. } => {
            Some(dae::DaeGeneration::ConnectionEquation)
        }
        flat::EquationOrigin::FlowSum { .. } | flat::EquationOrigin::UnconnectedFlow { .. } => {
            Some(dae::DaeGeneration::FlowBalanceEquation)
        }
        flat::EquationOrigin::Algorithm { .. } => Some(dae::DaeGeneration::AlgorithmEquation),
        flat::EquationOrigin::Reinit { .. } => Some(dae::DaeGeneration::EventActionLowering),
        flat::EquationOrigin::WhenAssignment { .. } => Some(dae::DaeGeneration::DiscreteUpdate),
        flat::EquationOrigin::Binding { .. } => Some(dae::DaeGeneration::BindingEquation),
    }
}

pub(super) fn equation_owner_provenance(
    origin: &flat::EquationOrigin,
    span: Span,
) -> Result<dae::DaeProvenance, dae::DaeConstructionError> {
    match equation_generation(origin) {
        Some(generation) => dae::DaeProvenance::generated(generation, span),
        None => dae::DaeProvenance::source(span),
    }
}
