use super::*;

mod clock_syntax;

#[cfg(test)]
pub(in crate::construction) use clock_syntax::expression_preorder_ordinal;
use clock_syntax::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(in crate::construction) enum ClockOwnerId {
    Coordinate(InstanceId),
    Definition {
        coordinate: InstanceId,
        occurrence: u32,
    },
    TransferSource(u32),
    TransferTarget(u32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ClockLineage {
    Periodic,
    Conversion {
        source: ClockOwnerId,
        kind: dae::ClockTransferKind,
    },
}

#[derive(Clone, Copy, Debug)]
pub(in crate::construction) struct ClockPlan {
    pub(in crate::construction) owner: ClockOwnerId,
    pub(in crate::construction) lattice: ClockLattice,
    pub(in crate::construction) constructor_span: Span,
    lineage: ClockLineage,
}

impl ClockPlan {
    pub(in crate::construction) fn periodic(
        owner: ClockOwnerId,
        lattice: ClockLattice,
        constructor_span: Span,
    ) -> Self {
        Self {
            owner,
            lattice,
            constructor_span,
            lineage: ClockLineage::Periodic,
        }
    }

    pub(in crate::construction) fn order_key(self) -> (u8, u32, u32) {
        match self.owner {
            ClockOwnerId::Coordinate(instance) => (0, instance.index(), 0),
            ClockOwnerId::Definition {
                coordinate,
                occurrence,
            } => (1, coordinate.index(), occurrence),
            ClockOwnerId::TransferSource(identity) => (2, identity, 0),
            ClockOwnerId::TransferTarget(identity) => (3, identity, 0),
        }
    }

    pub(in crate::construction) fn matches_exactly(self, other: Self) -> bool {
        self.owner == other.owner
            && self.lattice == other.lattice
            && self.constructor_span == other.constructor_span
            && self.lineage == other.lineage
    }
}

/// Analysis-issued source and target clocks for one value-clock conversion.
///
/// Lowering consumes this exact relationship instead of rediscovering clock
/// semantics from an expression tree after domain analysis has completed.
#[derive(Clone, Copy)]
pub(in crate::construction) struct ClockTransferPlan {
    pub(in crate::construction) source: ClockPlan,
    pub(in crate::construction) target: ClockPlan,
    pub(in crate::construction) kind: dae::ClockTransferKind,
}

impl ClockTransferPlan {
    fn matches_exactly(self, other: Self) -> bool {
        self.source.matches_exactly(other.source)
            && self.target.matches_exactly(other.target)
            && self.kind == other.kind
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(in crate::construction) enum ClockExpressionRoot {
    Equation(u32),
    Binding(InstanceId),
    When {
        chain: u32,
        branch: u32,
        expression: u32,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(in crate::construction) struct WhenOccurrenceId {
    pub(in crate::construction) chain: u32,
    pub(in crate::construction) branch: u32,
}

pub(in crate::construction) struct WhenExpressionRoots<'expression> {
    roots: Vec<(&'expression Expression, ClockExpressionRoot)>,
}

impl<'expression> WhenExpressionRoots<'expression> {
    pub(in crate::construction) fn collect(
        occurrence: WhenOccurrenceId,
        equations: &'expression [flat::WhenEquation],
    ) -> Result<Self, ToDaeError> {
        let mut roots = Vec::new();
        collect_when_expression_roots(occurrence, equations, &mut roots)?;
        Ok(Self { roots })
    }

    pub(in crate::construction) fn iter(
        &self,
    ) -> impl Iterator<Item = (&'expression Expression, ClockExpressionRoot)> + '_ {
        self.roots.iter().copied()
    }
}

fn collect_when_expression_roots<'expression>(
    occurrence: WhenOccurrenceId,
    equations: &'expression [flat::WhenEquation],
    roots: &mut Vec<(&'expression Expression, ClockExpressionRoot)>,
) -> Result<(), ToDaeError> {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                push_when_expression_root(occurrence, value, roots)?;
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                push_when_expression_root(occurrence, condition, roots)?;
                push_when_expression_root(occurrence, message, roots)?;
                if let Some(level) = level {
                    push_when_expression_root(occurrence, level, roots)?;
                }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                push_when_expression_root(occurrence, message, roots)?;
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, equations) in branches {
                    push_when_expression_root(occurrence, condition, roots)?;
                    collect_when_expression_roots(occurrence, equations, roots)?;
                }
                if let Some(equations) = else_branch {
                    collect_when_expression_roots(occurrence, equations, roots)?;
                }
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                push_when_expression_root(occurrence, function, roots)?;
            }
        }
    }
    Ok(())
}

fn push_when_expression_root<'expression>(
    occurrence: WhenOccurrenceId,
    expression: &'expression Expression,
    roots: &mut Vec<(&'expression Expression, ClockExpressionRoot)>,
) -> Result<(), ToDaeError> {
    let expression_id = u32::try_from(roots.len())
        .map_err(|_| ToDaeError::internal("when expression identity exceeds u32"))?;
    roots.push((
        expression,
        ClockExpressionRoot::When {
            chain: occurrence.chain,
            branch: occurrence.branch,
            expression: expression_id,
        },
    ));
    Ok(())
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ExpressionOccurrenceId {
    root: ClockExpressionRoot,
    path: Vec<u32>,
}

#[derive(Default)]
pub(in crate::construction) struct ClockTransferPlans {
    by_occurrence: HashMap<ExpressionOccurrenceId, ClockTransferPlan>,
    /// Ephemeral address index into the immutable Flat model borrowed for one
    /// construction. The value, not the address, is the semantic identity:
    /// lowering uses this only to recover the exact analysis-issued occurrence
    /// in O(1), and never derives ownership from pointer order or equality.
    occurrence_by_expression: HashMap<usize, ExpressionOccurrenceId>,
}

impl ClockTransferPlans {
    pub(in crate::construction) fn plan(
        &self,
        expression: &Expression,
    ) -> Option<ClockTransferPlan> {
        let occurrence = self
            .occurrence_by_expression
            .get(&expression_address(expression))?;
        self.by_occurrence.get(occurrence).copied()
    }

    pub(in crate::construction) fn values(&self) -> impl Iterator<Item = &ClockTransferPlan> {
        self.by_occurrence.values()
    }

    fn insert(
        &mut self,
        expression_address: usize,
        occurrence: ExpressionOccurrenceId,
        plan: ClockTransferPlan,
        span: Span,
    ) -> Result<(), ToDaeError> {
        if self
            .by_occurrence
            .get(&occurrence)
            .is_some_and(|established| !established.matches_exactly(plan))
        {
            return Err(ToDaeError::unsupported_flat(
                "clocked value conversion ownership proof",
                "one expression occurrence has conflicting clock-transfer plans",
                span,
            ));
        }
        if self
            .occurrence_by_expression
            .get(&expression_address)
            .is_some_and(|established| established != &occurrence)
        {
            return Err(ToDaeError::internal(
                "one Flat expression address names multiple clock-transfer occurrences",
            ));
        }
        self.by_occurrence.insert(occurrence.clone(), plan);
        self.occurrence_by_expression
            .insert(expression_address, occurrence);
        Ok(())
    }
}

fn expression_address(expression: &Expression) -> usize {
    std::ptr::from_ref(expression).addr()
}

#[derive(Clone, Copy)]
pub(in crate::construction) struct ClockedValuePlan {
    pub(in crate::construction) clock: ClockPlan,
    pub(in crate::construction) ownership_span: Span,
    pub(in crate::construction) sampled: bool,
}

/// One `sample(u)` / `sample(u, c)` coordinate definition.
///
/// `clock` is populated only by the MLS §16.3 two-operand form, where the model
/// names the owning clock itself; the one-operand form leaves the owner to the
/// §16.5.1 inference in [`infer_sampled_clock_owners`].
#[derive(Clone, Copy)]
pub(super) struct SampledTarget {
    pub(super) span: Span,
    pub(super) clock: Option<ClockPlan>,
}

pub(super) struct ClockAnalysis {
    pub(super) plans: HashMap<InstanceId, ClockPlan>,
    pub(super) equation_rows: HashSet<usize>,
    pub(super) sampled_targets: HashMap<InstanceId, SampledTarget>,
}

pub(super) struct ClockDomainAnalysis {
    pub(super) equation_owners: HashMap<usize, ClockPlan>,
    pub(super) value_owners: HashMap<InstanceId, ClockedValuePlan>,
    /// Owning clock of every `when Clock()` branch, keyed by its exact Flat
    /// container occurrence. Source spans remain provenance only.
    pub(super) when_owners: HashMap<WhenOccurrenceId, ClockPlan>,
    /// Owning clock of every runtime coordinate that belongs to a clocked
    /// partition, whatever role the coordinate was planned with. Declaration
    /// bindings resolve their clock through this map, since a binding is not an
    /// equation row and therefore has no `equation_owners` entry.
    pub(super) coordinate_owners: HashMap<InstanceId, ClockPlan>,
    /// Exact source/target relationship of every value-clock conversion.
    pub(super) transfers: ClockTransferPlans,
}

#[derive(Clone, Copy)]
enum ContinuousClockConflict {
    State,
    Input,
}

impl ContinuousClockConflict {
    const fn description(self) -> &'static str {
        match self {
            Self::State => "continuous state",
            Self::Input => "continuous input",
        }
    }
}

#[derive(Clone, Copy)]
struct ContinuousClockOccurrence {
    member: usize,
    span: Span,
    conflict: ContinuousClockConflict,
}

/// The exact `Clock` coordinates of one Flat model.
///
/// A Flat expression occurrence names its coordinate through the model's
/// variable catalog: `Reference::instance_id` carries the *enclosing* class
/// occurrence, not the referenced declaration, so clock ownership resolves
/// through the same catalog every other construction path uses and then keys
/// its plans on the resolved variable's own occurrence identity.
struct ClockCoordinates<'flat> {
    ordered: Vec<&'flat flat::Variable>,
    by_name: HashMap<&'flat VarName, &'flat flat::Variable>,
}

impl<'flat> ClockCoordinates<'flat> {
    fn resolve(&self, name: &VarName) -> Option<&'flat flat::Variable> {
        self.by_name.get(name).copied()
    }

    fn contains(&self, name: &VarName) -> bool {
        self.by_name.contains_key(name)
    }

    fn len(&self) -> usize {
        self.ordered.len()
    }
}

pub(super) fn analyze_clocks(
    flat: &flat::Model,
    constants: &EvalContext,
) -> Result<ClockAnalysis, ToDaeError> {
    let clocks = exact_clock_coordinates(flat)?;
    let mut plans = HashMap::new();
    let mut aliases = Vec::new();
    let mut derived = Vec::new();
    let mut equation_rows = HashSet::new();
    derive_bound_clock_plans(constants, &clocks, &mut plans)?;
    for (row, equation) in flat.equations.iter().enumerate() {
        let residual = static_clock_branch(&equation.residual, constants, &clocks)?;
        let Some((lhs, rhs)) = subtraction_operands(residual) else {
            if expression_mentions_clock(residual, &clocks) {
                return Err(unsupported_clock_equation(equation));
            }
            continue;
        };
        let lhs_clock = whole_clock_reference(lhs, &clocks);
        let rhs_clock = whole_clock_reference(rhs, &clocks);
        let constructor = lhs_clock
            .map(|target| {
                periodic_constructor(rhs, constants, ClockOwnerId::Coordinate(target.instance_id))
            })
            .transpose()?
            .flatten();
        match (lhs_clock, rhs_clock, constructor) {
            (Some(target), None, Some(plan)) => {
                insert_plan(&mut plans, target, plan, equation.span)?;
                equation_rows.insert(row);
            }
            (Some(lhs), Some(rhs), None) => {
                aliases.push((lhs, rhs, equation.span));
                equation_rows.insert(row);
            }
            (Some(target), None, None) => {
                derived.push((target, rhs, equation.span));
                equation_rows.insert(row);
            }
            _ if expression_mentions_clock(residual, &clocks) => {
                return Err(unsupported_clock_equation(equation));
            }
            _ => {}
        }
    }

    resolve_clock_definitions(&mut plans, &derived, &aliases, constants, &clocks)?;
    for variable in &clocks.ordered {
        if !plans.contains_key(&variable.instance_id) {
            return Err(ToDaeError::unresolved_clock_schedule(
                variable.name.as_str(),
                "no unique constructor reaches this clock coordinate through its aliases",
                variable.source_span,
            ));
        }
    }
    let sampled_targets = analyze_sampled_targets(flat, &clocks, &plans, &equation_rows)?;
    Ok(ClockAnalysis {
        plans,
        equation_rows,
        sampled_targets,
    })
}

/// The single branch a parameter `if` selects, or `None` when a condition is
/// not parameter-evaluable.
fn statically_selected_branch<'expression>(
    branches: &'expression [(Expression, Expression)],
    else_branch: &'expression Expression,
    constants: &EvalContext,
) -> Option<&'expression Expression> {
    for (condition, value) in branches {
        if eval_expr(condition, constants).ok()?.as_bool()? {
            return Some(value);
        }
    }
    Some(else_branch)
}

fn exact_clock_coordinates(flat: &flat::Model) -> Result<ClockCoordinates<'_>, ToDaeError> {
    let clock_type = flat.predefined_types.clock;
    let mut ordered = Vec::new();
    let mut by_name = HashMap::new();
    for (name, variable) in &flat.variables {
        if !is_predefined_clock_variable(flat, variable)? {
            continue;
        }
        ordered.push(variable);
        by_name.insert(name, variable);
    }
    debug_assert!(
        ordered
            .iter()
            .all(|variable| flat.effective_types[&variable.type_id].canonical_type() == clock_type)
    );
    Ok(ClockCoordinates { ordered, by_name })
}

fn derive_bound_clock_plans(
    constants: &EvalContext,
    clocks: &ClockCoordinates<'_>,
    plans: &mut HashMap<InstanceId, ClockPlan>,
) -> Result<(), ToDaeError> {
    for _ in 0..clocks.len() {
        let mut progress = false;
        for variable in &clocks.ordered {
            if plans.contains_key(&variable.instance_id) {
                continue;
            }
            let Some(binding) = variable.binding.as_ref() else {
                continue;
            };
            let Some(plan) =
                bound_clock_plan(binding, constants, clocks, plans, variable.instance_id)?
            else {
                continue;
            };
            insert_plan(plans, variable, plan, expression_span(binding)?)?;
            progress = true;
        }
        if !progress {
            break;
        }
    }
    Ok(())
}

fn analyze_sampled_targets(
    flat: &flat::Model,
    clocks: &ClockCoordinates<'_>,
    plans: &HashMap<InstanceId, ClockPlan>,
    clock_equation_rows: &HashSet<usize>,
) -> Result<HashMap<InstanceId, SampledTarget>, ToDaeError> {
    let mut sampled = HashMap::new();
    for (row, equation) in flat.equations.iter().enumerate() {
        if clock_equation_rows.contains(&row) {
            continue;
        }
        if let Some((target, sample_span, clock)) =
            sampled_value_target(&equation.residual, flat, clocks)
        {
            let clock = clock
                .map(|clock| named_sample_clock_plan(clock, plans, sample_span))
                .transpose()?;
            if sampled
                .insert(
                    target.instance_id,
                    SampledTarget {
                        span: sample_span,
                        clock,
                    },
                )
                .is_some()
            {
                return Err(ToDaeError::unsupported_flat(
                    "clocked sample ownership proof",
                    format!(
                        "sampled coordinate `{}` has more than one definition",
                        target.name
                    ),
                    equation.span,
                ));
            }
        } else if expression_mentions_value_sample(&equation.residual, clocks) {
            return Err(ToDaeError::unsupported_flat(
                "clocked sample ownership proof",
                "sample(value) must be the complete right-hand side of one coordinate definition",
                equation.span,
            ));
        }
    }
    Ok(sampled)
}

pub(super) fn analyze_clock_domains(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    plans: &HashMap<InstanceId, ClockPlan>,
    clock_equation_rows: &HashSet<usize>,
    sampled_targets: &HashMap<InstanceId, SampledTarget>,
    constants: &EvalContext,
) -> Result<ClockDomainAnalysis, ToDaeError> {
    reject_initial_clock_semantics(flat)?;
    let ordinals = runtime_clock_ordinals(flat, roles);
    let mut domains = ClockDomainArena::new(ordinals.len());
    let mut equation_members = vec![Vec::new(); flat.equations.len()];
    let mut conversion_drafts = Vec::new();
    let mut continuous_occurrences = Vec::new();
    {
        let mut graph = ClockDomainGraph {
            flat,
            roles,
            ordinals: &ordinals,
            domains: &mut domains,
            constants,
            conversion_edges: &mut conversion_drafts,
            continuous_occurrences: &mut continuous_occurrences,
        };
        for (row, equation) in flat.equations.iter().enumerate() {
            if clock_equation_rows.contains(&row) {
                continue;
            }
            let row_id = u32::try_from(row)
                .map_err(|_| ToDaeError::internal("equation occurrence identity exceeds u32"))?;
            equation_members[row] =
                graph.collect_root(&equation.residual, ClockExpressionRoot::Equation(row_id))?;
        }
        for (name, variable) in &flat.variables {
            let Some(binding) = variable.binding.as_ref() else {
                continue;
            };
            let binding_span = expression_span(binding)?;
            let mut members = graph.coordinate_member(name, binding_span)?;
            if members.is_empty() {
                continue;
            }
            members.extend(
                graph.collect_root(binding, ClockExpressionRoot::Binding(variable.instance_id))?,
            );
            join_domain_members(graph.domains, members);
        }
    }
    let WhenClockSeeds { seeds, inferred } = clocked_when_seeds(
        ClockDomainGraph {
            flat,
            roles,
            ordinals: &ordinals,
            domains: &mut domains,
            constants,
            conversion_edges: &mut conversion_drafts,
            continuous_occurrences: &mut continuous_occurrences,
        },
        plans,
    )?;
    let conversion_edges = issue_clock_conversion_edges(conversion_drafts)?;
    let mut owners = assign_domain_owners(&mut domains, seeds)?;
    own_named_sample_clocks(flat, sampled_targets, &ordinals, &mut domains, &mut owners)?;
    infer_sampled_clock_owners(
        flat,
        plans,
        sampled_targets,
        &ordinals,
        &mut domains,
        &mut owners,
    )?;
    propagate_clock_conversion_owners(&conversion_edges, &mut domains, &mut owners)?;
    reject_clocked_continuous_occurrences(&continuous_occurrences, &mut domains, &owners)?;
    let transfers = resolve_clock_transfer_plans(&conversion_edges, &mut domains, &owners)?;
    let equation_owners = assign_equation_owners(
        flat,
        &equation_members,
        &mut domains,
        &owners,
        sampled_targets,
    )?;
    let when_owners = resolve_inferred_when_owners(&inferred, &mut domains, &owners)?;
    let coordinate_owners = ordinals
        .iter()
        .filter_map(|(&instance, &ordinal)| {
            let root = domains.find(ordinal);
            owners.get(&root).map(|(clock, _)| (instance, *clock))
        })
        .collect::<HashMap<_, _>>();
    validate_clocked_value_samples(flat, plans, &equation_owners)?;
    let value_owners = assign_value_owners(
        flat,
        roles,
        &ordinals,
        &mut domains,
        &owners,
        sampled_targets,
    )?;
    Ok(ClockDomainAnalysis {
        equation_owners,
        value_owners,
        when_owners,
        coordinate_owners,
        transfers,
    })
}

fn reject_initial_clock_semantics(flat: &flat::Model) -> Result<(), ToDaeError> {
    let no_sampled_targets = HashMap::new();
    for equation in &flat.initial_equations {
        if let Some(span) = required_clock_owner_span(&equation.residual, flat, &no_sampled_targets)
        {
            return Err(ToDaeError::unsupported_flat(
                "initial clocked equation ownership",
                "clock-owned sample/previous semantics are not valid in an initial equation",
                span,
            ));
        }
    }
    Ok(())
}

fn runtime_clock_ordinals(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
) -> HashMap<InstanceId, usize> {
    flat.variables
        .iter()
        .filter(|(name, _)| {
            roles
                .get(*name)
                .is_some_and(|role| is_clock_runtime_role(*role))
        })
        .enumerate()
        .map(|(ordinal, (_, variable))| (variable.instance_id, ordinal))
        .collect()
}

/// MLS §16.5.1 `when Clock() then`: the branch declares that its equations form
/// a clocked partition whose clock is *inferred*, so the owner is whichever
/// clock the connected partition already proves. A branch that reaches no such
/// owner has no schedule and is reported rather than defaulted.
fn resolve_inferred_when_owners(
    inferred: &[InferredWhenBranch],
    domains: &mut ClockDomainArena,
    owners: &HashMap<usize, (ClockPlan, Span)>,
) -> Result<HashMap<WhenOccurrenceId, ClockPlan>, ToDaeError> {
    let mut when_owners = HashMap::with_capacity(inferred.len());
    for branch in inferred {
        let owner = branch
            .member
            .map(|member| domains.find(member))
            .and_then(|root| owners.get(&root))
            .map(|(clock, _)| *clock)
            .ok_or_else(|| {
                ToDaeError::unresolved_clock_schedule(
                    "when Clock()",
                    "clock inference reaches no exact clock constructor from this partition",
                    branch.span,
                )
            })?;
        when_owners.insert(branch.occurrence, owner);
    }
    Ok(when_owners)
}

fn is_clock_runtime_role(role: PlannedRole) -> bool {
    !matches!(
        role,
        PlannedRole::UnusedExpandable
            | PlannedRole::Parameter
            | PlannedRole::Constant
            | PlannedRole::Clock
            | PlannedRole::EnumerationLiteral
            | PlannedRole::Aggregate
    )
}

/// Build the exact clock-domain graph for one expression occurrence.
///
/// Ordinary expression structure joins its clocked operands. A clock transfer
/// instead creates a fresh domain member and a directed source-to-target edge;
/// its source is deliberately not joined to its result. This recursive graph is
/// the single owner used later by lowering, including when a transfer is nested
/// below another expression.
struct ClockDomainGraph<'analysis> {
    flat: &'analysis flat::Model,
    roles: &'analysis HashMap<VarName, PlannedRole>,
    ordinals: &'analysis HashMap<InstanceId, usize>,
    domains: &'analysis mut ClockDomainArena,
    constants: &'analysis EvalContext,
    conversion_edges: &'analysis mut Vec<ClockConversionDraft>,
    continuous_occurrences: &'analysis mut Vec<ContinuousClockOccurrence>,
}

impl ClockDomainGraph<'_> {
    fn collect_root(
        &mut self,
        expression: &Expression,
        root: ClockExpressionRoot,
    ) -> Result<Vec<usize>, ToDaeError> {
        self.collect_at(expression, root, &mut Vec::new(), false)
    }

    fn collect_at(
        &mut self,
        expression: &Expression,
        root: ClockExpressionRoot,
        path: &mut Vec<u32>,
        transfer_source: bool,
    ) -> Result<Vec<usize>, ToDaeError> {
        if let Expression::VarRef { name, span, .. } = expression {
            let mut members = Vec::new();
            if let Some(role) = self.roles.get(name.var_name()).copied()
                && is_clock_runtime_role(role)
                && let Some(variable) = self.flat.variables.get(name.var_name())
                && let Some(&ordinal) = self.ordinals.get(&variable.instance_id)
            {
                self.record_continuous_occurrence(role, ordinal, *span, transfer_source)?;
                self.domains.record_provenance(ordinal, *span)?;
                members.push(ordinal);
            }
            members.extend(self.collect_children(expression, root, path, transfer_source)?);
            return Ok(join_domain_members(self.domains, members));
        }

        if let Expression::BuiltinCall {
            function:
                BuiltinFunction::SubSample
                | BuiltinFunction::SuperSample
                | BuiltinFunction::ShiftSample
                | BuiltinFunction::BackSample,
            ..
        } = expression
        {
            return self.collect_transfer(expression, root, path);
        }

        let children = match expression {
            Expression::BuiltinCall {
                function:
                    BuiltinFunction::Sample
                    | BuiltinFunction::Clock
                    | BuiltinFunction::Hold
                    | BuiltinFunction::NoClock,
                args,
                ..
            } => args.iter().enumerate().skip(1).collect::<Vec<_>>(),
            _ => expression_children(expression)
                .into_iter()
                .enumerate()
                .collect(),
        };
        let mut members = Vec::new();
        for (index, child) in children {
            push_expression_child(path, index)?;
            let child_members = self.collect_at(child, root, path, transfer_source);
            path.pop();
            members.extend(child_members?);
        }
        Ok(join_domain_members(self.domains, members))
    }

    fn collect_children(
        &mut self,
        expression: &Expression,
        root: ClockExpressionRoot,
        path: &mut Vec<u32>,
        transfer_source: bool,
    ) -> Result<Vec<usize>, ToDaeError> {
        let mut members = Vec::new();
        for (index, child) in expression_children(expression).into_iter().enumerate() {
            push_expression_child(path, index)?;
            let child_members = self.collect_at(child, root, path, transfer_source);
            path.pop();
            members.extend(child_members?);
        }
        Ok(members)
    }

    fn collect_transfer(
        &mut self,
        expression: &Expression,
        root: ClockExpressionRoot,
        path: &mut Vec<u32>,
    ) -> Result<Vec<usize>, ToDaeError> {
        let conversion = value_clock_conversion(expression, self.constants)?.ok_or_else(|| {
            ToDaeError::internal("clock-transfer recognizer lost a selected transfer expression")
        })?;
        push_expression_child(path, 0)?;
        let source_members = self.collect_at(conversion.source, root, path, true);
        path.pop();
        let source_members = source_members?;
        let [source] = source_members.as_slice() else {
            return Err(ToDaeError::unsupported_flat(
                "clocked value conversion ownership proof",
                "a clock conversion source must belong to one proven clock partition",
                conversion.span,
            ));
        };
        let target = self.domains.push(Some(conversion.span));
        self.conversion_edges.push(ClockConversionDraft {
            source: *source,
            target,
            kind: conversion.kind,
            span: conversion.span,
            occurrence: ExpressionOccurrenceId {
                root,
                path: path.clone(),
            },
            expression_address: expression_address(expression),
        });
        Ok(vec![target])
    }

    fn collect_when_equations(
        &mut self,
        occurrence: WhenOccurrenceId,
        equations: &[flat::WhenEquation],
    ) -> Result<Vec<usize>, ToDaeError> {
        let mut members = Vec::new();
        let roots = WhenExpressionRoots::collect(occurrence, equations)?;
        for (expression, root) in roots.iter() {
            members.extend(self.collect_root(expression, root)?);
        }
        members.extend(self.collect_when_targets(equations)?);
        Ok(join_domain_members(self.domains, members))
    }

    fn collect_when_targets(
        &mut self,
        equations: &[flat::WhenEquation],
    ) -> Result<Vec<usize>, ToDaeError> {
        let mut members = Vec::new();
        for equation in equations {
            self.collect_when_target(equation, &mut members)?;
        }
        Ok(members)
    }

    fn collect_when_target(
        &mut self,
        equation: &flat::WhenEquation,
        members: &mut Vec<usize>,
    ) -> Result<(), ToDaeError> {
        match equation {
            flat::WhenEquation::Assign { target, span, .. }
            | flat::WhenEquation::Reinit {
                state: target,
                span,
                ..
            } => members.extend(self.coordinate_member(target, *span)?),
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                self.collect_conditional_when_targets(branches, else_branch.as_deref(), members)?
            }
            flat::WhenEquation::FunctionCallOutputs { outputs, span, .. } => {
                self.collect_function_output_targets(outputs, *span, members)?;
            }
            flat::WhenEquation::Assert { .. } | flat::WhenEquation::Terminate { .. } => {}
        }
        Ok(())
    }

    fn collect_conditional_when_targets(
        &mut self,
        branches: &[(Expression, Vec<flat::WhenEquation>)],
        else_branch: Option<&[flat::WhenEquation]>,
        members: &mut Vec<usize>,
    ) -> Result<(), ToDaeError> {
        for (_, equations) in branches {
            members.extend(self.collect_when_targets(equations)?);
        }
        if let Some(equations) = else_branch {
            members.extend(self.collect_when_targets(equations)?);
        }
        Ok(())
    }

    fn collect_function_output_targets(
        &mut self,
        outputs: &[VarName],
        span: Span,
        members: &mut Vec<usize>,
    ) -> Result<(), ToDaeError> {
        for output in outputs {
            members.extend(self.coordinate_member(output, span)?);
        }
        Ok(())
    }

    fn coordinate_member(&mut self, name: &VarName, span: Span) -> Result<Vec<usize>, ToDaeError> {
        let variable = self
            .flat
            .variables
            .get(name)
            .ok_or_else(|| ToDaeError::unresolved_reference(name.as_str(), span))?;
        let Some(role) = self.roles.get(name).copied() else {
            return Ok(Vec::new());
        };
        if !is_clock_runtime_role(role) {
            return Ok(Vec::new());
        }
        let ordinal = self
            .ordinals
            .get(&variable.instance_id)
            .copied()
            .ok_or_else(|| {
                ToDaeError::internal(format!(
                    "runtime coordinate `{name}` has no clock-domain ordinal"
                ))
            })?;
        self.record_continuous_occurrence(role, ordinal, span, false)?;
        self.domains.record_provenance(ordinal, span)?;
        Ok(vec![ordinal])
    }

    fn record_continuous_occurrence(
        &mut self,
        role: PlannedRole,
        member: usize,
        span: Span,
        transfer_source: bool,
    ) -> Result<(), ToDaeError> {
        let Some(conflict) = continuous_clock_conflict(role) else {
            return Ok(());
        };
        if transfer_source {
            return Err(continuous_clock_error(conflict, span));
        }
        self.continuous_occurrences.push(ContinuousClockOccurrence {
            member,
            span,
            conflict,
        });
        Ok(())
    }
}

const fn continuous_clock_conflict(role: PlannedRole) -> Option<ContinuousClockConflict> {
    match role {
        PlannedRole::State => Some(ContinuousClockConflict::State),
        PlannedRole::Input => Some(ContinuousClockConflict::Input),
        _ => None,
    }
}

fn continuous_clock_error(conflict: ContinuousClockConflict, span: Span) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "clocked continuous coordinate",
        format!(
            "a {} cannot participate directly in a clocked partition; sample it at the clock boundary",
            conflict.description()
        ),
        span,
    )
}

fn reject_clocked_continuous_occurrences(
    occurrences: &[ContinuousClockOccurrence],
    domains: &mut ClockDomainArena,
    owners: &HashMap<usize, (ClockPlan, Span)>,
) -> Result<(), ToDaeError> {
    for occurrence in occurrences {
        if owners.contains_key(&domains.find(occurrence.member)) {
            return Err(continuous_clock_error(occurrence.conflict, occurrence.span));
        }
    }
    Ok(())
}

fn push_expression_child(path: &mut Vec<u32>, index: usize) -> Result<(), ToDaeError> {
    path.push(
        u32::try_from(index)
            .map_err(|_| ToDaeError::internal("expression child identity exceeds u32"))?,
    );
    Ok(())
}

fn join_domain_members(domains: &mut ClockDomainArena, members: Vec<usize>) -> Vec<usize> {
    let Some((&first, rest)) = members.split_first() else {
        return Vec::new();
    };
    for &member in rest {
        domains.union(first, member);
    }
    vec![first]
}

#[derive(Clone, Copy)]
struct ClockDomainSeed {
    member: usize,
    clock: ClockPlan,
    span: Span,
}

struct ClockConversionDraft {
    source: usize,
    target: usize,
    kind: dae::ClockTransferKind,
    span: Span,
    occurrence: ExpressionOccurrenceId,
    expression_address: usize,
}

#[derive(Clone)]
struct ClockConversionEdge {
    source: usize,
    target: usize,
    kind: dae::ClockTransferKind,
    span: Span,
    occurrence: ExpressionOccurrenceId,
    expression_address: usize,
    identity: u32,
}

impl ClockConversionEdge {
    fn source_owner(&self) -> ClockOwnerId {
        ClockOwnerId::TransferSource(self.identity)
    }

    fn target_owner(&self) -> ClockOwnerId {
        ClockOwnerId::TransferTarget(self.identity)
    }
}

fn issue_clock_conversion_edges(
    drafts: Vec<ClockConversionDraft>,
) -> Result<Vec<ClockConversionEdge>, ToDaeError> {
    let mut occurrences = drafts
        .iter()
        .map(|draft| draft.occurrence.clone())
        .collect::<Vec<_>>();
    occurrences.sort_by(compare_expression_occurrences);
    occurrences.dedup();
    let identities = occurrences
        .into_iter()
        .enumerate()
        .map(|(index, occurrence)| {
            let identity = u32::try_from(index)
                .map_err(|_| ToDaeError::internal("clock transfer identity exceeds u32"))?;
            Ok((occurrence, identity))
        })
        .collect::<Result<HashMap<_, _>, ToDaeError>>()?;
    drafts
        .into_iter()
        .map(|draft| {
            let identity = identities.get(&draft.occurrence).copied().ok_or_else(|| {
                ToDaeError::internal("issued clock-transfer occurrence lost its identity")
            })?;
            Ok(ClockConversionEdge {
                source: draft.source,
                target: draft.target,
                kind: draft.kind,
                span: draft.span,
                occurrence: draft.occurrence,
                expression_address: draft.expression_address,
                identity,
            })
        })
        .collect()
}

fn compare_expression_occurrences(
    left: &ExpressionOccurrenceId,
    right: &ExpressionOccurrenceId,
) -> std::cmp::Ordering {
    clock_expression_root_key(left.root)
        .cmp(&clock_expression_root_key(right.root))
        .then_with(|| left.path.cmp(&right.path))
}

fn clock_expression_root_key(root: ClockExpressionRoot) -> (u8, u32, u32, u32) {
    match root {
        ClockExpressionRoot::Equation(row) => (0, row, 0, 0),
        ClockExpressionRoot::Binding(instance) => (1, instance.index(), 0, 0),
        ClockExpressionRoot::When {
            chain,
            branch,
            expression,
        } => (2, chain, branch, expression),
    }
}

struct ClockConversionExpression<'expression> {
    source: &'expression Expression,
    kind: dae::ClockTransferKind,
    span: Span,
}

fn value_clock_conversion<'expression>(
    expression: &'expression Expression,
    constants: &EvalContext,
) -> Result<Option<ClockConversionExpression<'expression>>, ToDaeError> {
    let Expression::BuiltinCall {
        function,
        args,
        span,
        ..
    } = expression
    else {
        return Ok(None);
    };
    let kind = match (function, args.as_slice()) {
        (BuiltinFunction::SubSample, [_, factor]) => dae::ClockTransferKind::SubSample {
            factor: clock_positive(factor, constants, function.name())?,
        },
        (BuiltinFunction::SuperSample, [_, factor]) => dae::ClockTransferKind::SuperSample {
            factor: clock_positive(factor, constants, function.name())?,
        },
        (BuiltinFunction::ShiftSample, [_, counter]) => dae::ClockTransferKind::ShiftSample {
            counter: clock_nonnegative(counter, constants, function.name())?,
            resolution: 1,
        },
        (BuiltinFunction::ShiftSample, [_, counter, resolution]) => {
            dae::ClockTransferKind::ShiftSample {
                counter: clock_nonnegative(counter, constants, function.name())?,
                resolution: clock_positive(resolution, constants, function.name())?,
            }
        }
        (BuiltinFunction::BackSample, [_, counter]) => dae::ClockTransferKind::BackSample {
            counter: clock_nonnegative(counter, constants, function.name())?,
            resolution: 1,
        },
        (BuiltinFunction::BackSample, [_, counter, resolution]) => {
            dae::ClockTransferKind::BackSample {
                counter: clock_nonnegative(counter, constants, function.name())?,
                resolution: clock_positive(resolution, constants, function.name())?,
            }
        }
        (BuiltinFunction::NoClock, [_]) => {
            return Err(invalid_clock_operator(
                function.name(),
                "has no exact periodic lattice for checked value transfer",
                *span,
            ));
        }
        (
            BuiltinFunction::SubSample
            | BuiltinFunction::SuperSample
            | BuiltinFunction::ShiftSample
            | BuiltinFunction::BackSample
            | BuiltinFunction::NoClock,
            _,
        ) => {
            return Err(invalid_clock_operator(
                function.name(),
                "has invalid clocked value conversion arity",
                *span,
            ));
        }
        _ => return Ok(None),
    };
    Ok(Some(ClockConversionExpression {
        source: &args[0],
        kind,
        span: *span,
    }))
}

fn propagate_clock_conversion_owners(
    edges: &[ClockConversionEdge],
    domains: &mut ClockDomainArena,
    owners: &mut HashMap<usize, (ClockPlan, Span)>,
) -> Result<(), ToDaeError> {
    loop {
        let mut progress = false;
        for edge in edges {
            let source_root = domains.find(edge.source);
            let target_root = domains.find(edge.target);
            let source = owners.get(&source_root).copied();
            let target = owners.get(&target_root).copied();
            match (source, target) {
                (Some((source, _)), Some((target, _))) => {
                    require_conversion_relationship(edge, source, target)?;
                }
                (Some((source, _)), None) => {
                    let lattice = conversion_target_lattice(edge, source.lattice)?;
                    owners.insert(
                        target_root,
                        (
                            ClockPlan {
                                owner: edge.target_owner(),
                                lattice,
                                constructor_span: edge.span,
                                lineage: ClockLineage::Conversion {
                                    source: source.owner,
                                    kind: edge.kind,
                                },
                            },
                            edge.span,
                        ),
                    );
                    progress = true;
                }
                (None, Some((target, _))) => {
                    let lattice = conversion_source_lattice(edge, target.lattice)?;
                    owners.insert(
                        source_root,
                        (
                            ClockPlan {
                                owner: edge.source_owner(),
                                lattice,
                                constructor_span: edge.span,
                                lineage: ClockLineage::Conversion {
                                    source: target.owner,
                                    kind: inverse_transfer_kind(edge.kind),
                                },
                            },
                            edge.span,
                        ),
                    );
                    progress = true;
                }
                (None, None) => {}
            }
        }
        if !progress {
            return Ok(());
        }
    }
}

fn resolve_clock_transfer_plans(
    edges: &[ClockConversionEdge],
    domains: &mut ClockDomainArena,
    owners: &HashMap<usize, (ClockPlan, Span)>,
) -> Result<ClockTransferPlans, ToDaeError> {
    let mut transfers = ClockTransferPlans::default();
    for edge in edges {
        let source = owners
            .get(&domains.find(edge.source))
            .map(|(plan, _)| *plan)
            .ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "clocked value conversion ownership proof",
                    "a clock conversion source has no exact issued clock owner",
                    edge.span,
                )
            })?;
        let target = owners
            .get(&domains.find(edge.target))
            .map(|(plan, _)| *plan)
            .ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "clocked value conversion ownership proof",
                    "a clock conversion target has no exact issued clock owner",
                    edge.span,
                )
            })?;
        require_conversion_relationship(edge, source, target)?;
        let plan = ClockTransferPlan {
            source,
            target,
            kind: edge.kind,
        };
        transfers.insert(
            edge.expression_address,
            edge.occurrence.clone(),
            plan,
            edge.span,
        )?;
    }
    Ok(transfers)
}

fn require_conversion_lattice(
    edge: &ClockConversionEdge,
    source: ClockLattice,
    target: ClockLattice,
) -> Result<(), ToDaeError> {
    if conversion_target_lattice(edge, source)? == target {
        Ok(())
    } else {
        Err(ToDaeError::unsupported_flat(
            "clocked value conversion ownership proof",
            "the source and target partitions conflict with the exact clock conversion",
            edge.span,
        ))
    }
}

fn require_conversion_relationship(
    edge: &ClockConversionEdge,
    source: ClockPlan,
    target: ClockPlan,
) -> Result<(), ToDaeError> {
    require_conversion_lattice(edge, source.lattice, target.lattice)?;
    let target_matches = matches!(
        target.lineage,
        ClockLineage::Conversion {
            source: owner,
            kind,
        } if owner == source.owner && kind == edge.kind
    );
    let inverse = inverse_transfer_kind(edge.kind);
    let source_matches = matches!(
        source.lineage,
        ClockLineage::Conversion {
            source: owner,
            kind,
        } if owner == target.owner && kind == inverse
    );
    if target_matches || source_matches {
        return Ok(());
    }
    Err(ToDaeError::unsupported_flat(
        "clocked value conversion ownership proof",
        "the source and target partitions have compatible schedules but no exact clock lineage",
        edge.span,
    ))
}

fn inverse_transfer_kind(kind: dae::ClockTransferKind) -> dae::ClockTransferKind {
    match kind {
        dae::ClockTransferKind::SubSample { factor } => {
            dae::ClockTransferKind::SuperSample { factor }
        }
        dae::ClockTransferKind::SuperSample { factor } => {
            dae::ClockTransferKind::SubSample { factor }
        }
        dae::ClockTransferKind::ShiftSample {
            counter,
            resolution,
        } => dae::ClockTransferKind::BackSample {
            counter,
            resolution,
        },
        dae::ClockTransferKind::BackSample {
            counter,
            resolution,
        } => dae::ClockTransferKind::ShiftSample {
            counter,
            resolution,
        },
    }
}

fn conversion_target_lattice(
    edge: &ClockConversionEdge,
    source: ClockLattice,
) -> Result<ClockLattice, ToDaeError> {
    transfer_target_lattice(edge.kind, source).map_err(|source| {
        ToDaeError::from(dae::DaeConstructionError::InvalidClockLattice {
            source,
            span: edge.span,
        })
    })
}

fn transfer_target_lattice(
    kind: dae::ClockTransferKind,
    source: ClockLattice,
) -> Result<ClockLattice, rumoca_core::ClockLatticeErrorKind> {
    match kind {
        dae::ClockTransferKind::SubSample { factor } => source.sub_sample(factor),
        dae::ClockTransferKind::SuperSample { factor } => source.super_sample(factor),
        dae::ClockTransferKind::ShiftSample {
            counter,
            resolution,
        } => source.shift_sample(counter, resolution),
        dae::ClockTransferKind::BackSample {
            counter,
            resolution,
        } => source.back_sample(counter, resolution),
    }
}

fn conversion_source_lattice(
    edge: &ClockConversionEdge,
    target: ClockLattice,
) -> Result<ClockLattice, ToDaeError> {
    let result = match edge.kind {
        dae::ClockTransferKind::SubSample { factor } => target.super_sample(factor),
        dae::ClockTransferKind::SuperSample { factor } => target.sub_sample(factor),
        dae::ClockTransferKind::ShiftSample {
            counter,
            resolution,
        } => target.back_sample(counter, resolution),
        dae::ClockTransferKind::BackSample {
            counter,
            resolution,
        } => target.shift_sample(counter, resolution),
    };
    result.map_err(|source| {
        ToDaeError::from(dae::DaeConstructionError::InvalidClockLattice {
            source,
            span: edge.span,
        })
    })
}

/// One `when Clock() then` branch and the partition member it joins.
#[derive(Clone, Copy)]
struct InferredWhenBranch {
    member: Option<usize>,
    span: Span,
    occurrence: WhenOccurrenceId,
}

struct WhenClockSeeds {
    seeds: Vec<ClockDomainSeed>,
    inferred: Vec<InferredWhenBranch>,
}

fn clocked_when_seeds(
    mut graph: ClockDomainGraph<'_>,
    plans: &HashMap<InstanceId, ClockPlan>,
) -> Result<WhenClockSeeds, ToDaeError> {
    let mut seeds = Vec::new();
    let mut inferred = Vec::new();
    for (chain_index, chain) in graph.flat.when_chains.iter().enumerate() {
        let chain_id = u32::try_from(chain_index)
            .map_err(|_| ToDaeError::internal("when-chain identity exceeds u32"))?;
        for (branch_index, branch) in chain.branches().enumerate() {
            let occurrence = WhenOccurrenceId {
                chain: chain_id,
                branch: u32::try_from(branch_index)
                    .map_err(|_| ToDaeError::internal("when-branch identity exceeds u32"))?,
            };
            let clock = clock_condition_plan(&branch.condition, graph.flat, plans);
            if clock.is_none() && !is_inferred_clock_condition(&branch.condition) {
                continue;
            }
            let members = graph.collect_when_equations(occurrence, &branch.equations)?;
            let member = members.first().copied();
            match (clock, member) {
                (Some(clock), Some(member)) => seeds.push(ClockDomainSeed {
                    member,
                    clock,
                    span: branch.span,
                }),
                (Some(_), None) => {}
                (None, member) => inferred.push(InferredWhenBranch {
                    member,
                    span: branch.span,
                    occurrence,
                }),
            }
        }
    }
    Ok(WhenClockSeeds { seeds, inferred })
}

/// MLS §16.3 Operator 16.2 `Clock()`: the inferred-clock constructor, which in
/// a `when` condition marks a clocked partition whose clock the model does not
/// name.
pub(in crate::construction) fn is_inferred_clock_condition(condition: &Expression) -> bool {
    matches!(
        condition,
        Expression::BuiltinCall {
            function: BuiltinFunction::Clock,
            args,
            ..
        } if args.is_empty()
    )
}

fn clock_condition_plan(
    condition: &Expression,
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
) -> Option<ClockPlan> {
    let Expression::VarRef {
        name, subscripts, ..
    } = condition
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let variable = flat.variables.get(name.var_name())?;
    plans.get(&variable.instance_id).copied()
}

fn assign_domain_owners(
    domains: &mut ClockDomainArena,
    seeds: Vec<ClockDomainSeed>,
) -> Result<HashMap<usize, (ClockPlan, Span)>, ToDaeError> {
    let mut owners: HashMap<usize, (ClockPlan, Span)> = HashMap::new();
    for seed in seeds {
        let root = domains.find(seed.member);
        match owners.get(&root) {
            Some((clock, _)) if !clock.matches_exactly(seed.clock) => {
                let attempted = domains.provenance(seed.member)?.ok_or_else(|| {
                    ToDaeError::MissingProvenance {
                        owner: "conflicting clock-domain operand occurrence".to_string(),
                    }
                })?;
                return Err(ToDaeError::unsupported_flat(
                    "clocked equation ownership proof",
                    "one equation component is connected to distinct clock owners",
                    attempted,
                ));
            }
            Some(_) => {}
            None => {
                owners.insert(root, (seed.clock, seed.span));
            }
        }
    }
    Ok(owners)
}

/// MLS §16.3 `sample(u, c)`: the model names the owning clock itself, so the
/// partition the sampled coordinate belongs to is proven, not inferred.
///
/// Two named clocks reaching one partition is the same conflict a clocked
/// `when` seed reports, so it is routed through [`assign_domain_owners`].
fn own_named_sample_clocks(
    flat: &flat::Model,
    sampled_targets: &HashMap<InstanceId, SampledTarget>,
    ordinals: &HashMap<InstanceId, usize>,
    domains: &mut ClockDomainArena,
    owners: &mut HashMap<usize, (ClockPlan, Span)>,
) -> Result<(), ToDaeError> {
    let mut seeds = Vec::new();
    for variable in flat.variables.values() {
        let Some(target) = sampled_targets.get(&variable.instance_id) else {
            continue;
        };
        let Some(clock) = target.clock else {
            continue;
        };
        let Some(&ordinal) = ordinals.get(&variable.instance_id) else {
            continue;
        };
        seeds.push(ClockDomainSeed {
            member: ordinal,
            clock,
            span: target.span,
        });
    }
    seeds.sort_by_key(|seed| (seed.member, seed.span.start));
    for seed in seeds {
        let root = domains.find(seed.member);
        match owners.get(&root) {
            Some((clock, _)) if !clock.matches_exactly(seed.clock) => {
                return Err(ToDaeError::unsupported_flat(
                    "clocked sample ownership proof",
                    "sample(value, clock) names a clock that conflicts with its partition owner",
                    seed.span,
                ));
            }
            Some(_) => {}
            None => {
                owners.insert(root, (seed.clock, seed.span));
            }
        }
    }
    Ok(())
}

/// MLS §16.5.1 clock inference for `sample(u)`.
///
/// A value sample that is not connected to a clocked `when` partition still
/// belongs to exactly one clock partition. The partition is proven only when
/// the model owns exactly one clock constructor; two independent constructors
/// leave the sample owner ambiguous even when their lattices coincide, so the
/// inference fails instead of picking one.
fn infer_sampled_clock_owners(
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
    sampled_targets: &HashMap<InstanceId, SampledTarget>,
    ordinals: &HashMap<InstanceId, usize>,
    domains: &mut ClockDomainArena,
    owners: &mut HashMap<usize, (ClockPlan, Span)>,
) -> Result<(), ToDaeError> {
    for variable in flat.variables.values() {
        let Some(span) = sampled_targets
            .get(&variable.instance_id)
            .map(|target| target.span)
        else {
            continue;
        };
        let Some(&ordinal) = ordinals.get(&variable.instance_id) else {
            continue;
        };
        let root = domains.find(ordinal);
        if owners.contains_key(&root) {
            continue;
        }
        let plan = unique_clock_plan(plans, span)?;
        owners.insert(root, (plan, span));
    }
    Ok(())
}

fn unique_clock_plan(
    plans: &HashMap<InstanceId, ClockPlan>,
    span: Span,
) -> Result<ClockPlan, ToDaeError> {
    let mut unique: Option<ClockPlan> = None;
    for plan in plans.values() {
        match unique {
            Some(existing) if existing.matches_exactly(*plan) => {}
            Some(_) => {
                return Err(ToDaeError::unsupported_flat(
                    "clocked sample ownership proof",
                    "sample(value) has more than one possible inferred clock",
                    span,
                ));
            }
            None => unique = Some(*plan),
        }
    }
    unique.ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "clocked sample ownership proof",
            "sample(value) has no inferred clock constructor",
            span,
        )
    })
}

/// MLS §16.3 `sample(u, c)`: the explicit clock operand must be the clock that
/// owns the partition the sample occurs in.
///
/// Lowering keeps only the sampled value, so an operand naming a different
/// clock would silently change the sampling rate. Proving the match here is
/// what lets [`is_whole_clock_coordinate`] accept the two-operand form at the
/// lowering boundary.
fn validate_clocked_value_samples(
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
    equation_owners: &HashMap<usize, ClockPlan>,
) -> Result<(), ToDaeError> {
    for (row, equation) in flat.equations.iter().enumerate() {
        validate_value_sample_operands(
            &equation.residual,
            flat,
            plans,
            equation_owners.get(&row).copied(),
        )?;
    }
    for equation in &flat.initial_equations {
        validate_value_sample_operands(&equation.residual, flat, plans, None)?;
    }
    for chain in &flat.when_chains {
        for branch in chain.branches() {
            let owner = clock_condition_plan(&branch.condition, flat, plans);
            validate_when_value_sample_operands(&branch.equations, flat, plans, owner)?;
        }
    }
    Ok(())
}

fn validate_value_sample_operands(
    expression: &Expression,
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
    owner: Option<ClockPlan>,
) -> Result<(), ToDaeError> {
    if let Expression::BuiltinCall {
        function: BuiltinFunction::Sample,
        args,
        span,
    } = expression
        && let [value, clock] = args.as_slice()
        && let Some(named) = clock_condition_plan(clock, flat, plans)
    {
        if !owner.is_some_and(|owner| owner.matches_exactly(named)) {
            return Err(ToDaeError::unsupported_flat(
                "clocked sample ownership proof",
                "sample(value, clock) must name the clock that owns its partition",
                *span,
            ));
        }
        return validate_value_sample_operands(value, flat, plans, owner);
    }
    for child in expression_children(expression) {
        validate_value_sample_operands(child, flat, plans, owner)?;
    }
    Ok(())
}

fn validate_when_value_sample_operands(
    equations: &[flat::WhenEquation],
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
    owner: Option<ClockPlan>,
) -> Result<(), ToDaeError> {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                validate_value_sample_operands(value, flat, plans, owner)?;
            }
            flat::WhenEquation::Assert { message, level, .. } => {
                validate_value_sample_operands(message, flat, plans, owner)?;
                if let Some(level) = level {
                    validate_value_sample_operands(level, flat, plans, owner)?;
                }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                validate_value_sample_operands(message, flat, plans, owner)?;
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (_, equations) in branches {
                    validate_when_value_sample_operands(equations, flat, plans, owner)?;
                }
                if let Some(equations) = else_branch {
                    validate_when_value_sample_operands(equations, flat, plans, owner)?;
                }
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                validate_value_sample_operands(function, flat, plans, owner)?;
            }
        }
    }
    Ok(())
}

/// True when `expression` names one whole predefined `Clock` coordinate.
pub(in crate::construction) fn is_whole_clock_coordinate(
    flat: &flat::Model,
    expression: &Expression,
) -> bool {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return false;
    };
    subscripts.is_empty()
        && flat.variables.get(name.var_name()).is_some_and(|variable| {
            flat.effective_types
                .get(&variable.type_id)
                .is_some_and(|effective| effective.canonical_type() == flat.predefined_types.clock)
        })
}

fn assign_equation_owners(
    flat: &flat::Model,
    equation_members: &[Vec<usize>],
    domains: &mut ClockDomainArena,
    owners: &HashMap<usize, (ClockPlan, Span)>,
    sampled_targets: &HashMap<InstanceId, SampledTarget>,
) -> Result<HashMap<usize, ClockPlan>, ToDaeError> {
    let mut equation_owners = HashMap::new();
    for (row, members) in equation_members.iter().enumerate() {
        let owner = members
            .first()
            .map(|&member| domains.find(member))
            .and_then(|root| owners.get(&root))
            .map(|(clock, _)| *clock);
        if let Some(owner) = owner {
            equation_owners.insert(row, owner);
            continue;
        }
        let equation = &flat.equations[row];
        if let Some(span) = required_clock_owner_span(&equation.residual, flat, sampled_targets) {
            return Err(ToDaeError::unsupported_flat(
                "clocked equation ownership proof",
                "clocked expression has no exact connected clock owner",
                span,
            ));
        }
    }
    Ok(equation_owners)
}

fn required_clock_owner_span(
    expression: &Expression,
    flat: &flat::Model,
    sampled_targets: &HashMap<InstanceId, SampledTarget>,
) -> Option<Span> {
    match expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Previous | BuiltinFunction::Interval,
            span,
            ..
        } => Some(*span),
        Expression::Binary { lhs, .. } => {
            if let Expression::VarRef {
                name, subscripts, ..
            } = lhs.as_ref()
                && subscripts.is_empty()
                && let Some(variable) = flat.variables.get(name.var_name())
                && let Some(target) = sampled_targets.get(&variable.instance_id)
            {
                return Some(target.span);
            }
            expression_children(expression)
                .into_iter()
                .find_map(|child| required_clock_owner_span(child, flat, sampled_targets))
        }
        _ => expression_children(expression)
            .into_iter()
            .find_map(|child| required_clock_owner_span(child, flat, sampled_targets)),
    }
}

fn assign_value_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    ordinals: &HashMap<InstanceId, usize>,
    domains: &mut ClockDomainArena,
    owners: &HashMap<usize, (ClockPlan, Span)>,
    sampled_targets: &HashMap<InstanceId, SampledTarget>,
) -> Result<HashMap<InstanceId, ClockedValuePlan>, ToDaeError> {
    let mut value_owners = HashMap::new();
    for (name, variable) in &flat.variables {
        let instance = variable.instance_id;
        let Some(&ordinal) = ordinals.get(&instance) else {
            continue;
        };
        if !roles.get(name).is_some_and(|role| {
            matches!(role, PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        }) {
            continue;
        }
        let Some((clock, owner_span)) = owners.get(&domains.find(ordinal)).copied() else {
            continue;
        };
        let ownership_span = sampled_targets
            .get(&instance)
            .map(|target| target.span)
            .or(domains.provenance(ordinal)?)
            .ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "clocked value ownership proof",
                    format!(
                        "clocked coordinate `{name}` has an owner but no exact source occurrence"
                    ),
                    owner_span,
                )
            })?;
        require_span(
            ownership_span,
            format!("clock ownership occurrence for `{name}`"),
        )?;
        value_owners.insert(
            instance,
            ClockedValuePlan {
                clock,
                ownership_span,
                sampled: sampled_targets.contains_key(&instance),
            },
        );
    }
    for (name, variable) in &flat.variables {
        let Some(target) = sampled_targets.get(&variable.instance_id) else {
            continue;
        };
        if !value_owners.contains_key(&variable.instance_id) {
            return Err(ToDaeError::unsupported_flat(
                "clocked sample ownership proof",
                format!("sampled coordinate `{name}` has no exact connected clock owner"),
                target.span,
            ));
        }
    }
    Ok(value_owners)
}

/// Union-find members and their exact source provenance share one arena.
/// A member cannot be allocated without its matching provenance slot, which
/// makes the parallel-length drift that used to panic on transfer targets
/// unrepresentable.
struct ClockDomainArena {
    domains: DisjointDomains,
    provenance: Vec<Option<Span>>,
}

impl ClockDomainArena {
    fn new(len: usize) -> Self {
        Self {
            domains: DisjointDomains::new(len),
            provenance: vec![None; len],
        }
    }

    fn find(&mut self, member: usize) -> usize {
        self.domains.find(member)
    }

    fn union(&mut self, lhs: usize, rhs: usize) {
        self.domains.union(lhs, rhs);
    }

    fn push(&mut self, provenance: Option<Span>) -> usize {
        let member = self.domains.push();
        self.provenance.push(provenance);
        member
    }

    fn record_provenance(&mut self, member: usize, span: Span) -> Result<(), ToDaeError> {
        let provenance = self
            .provenance
            .get_mut(member)
            .ok_or_else(|| ToDaeError::internal("clock-domain member has no provenance storage"))?;
        provenance.get_or_insert(span);
        Ok(())
    }

    fn provenance(&self, member: usize) -> Result<Option<Span>, ToDaeError> {
        self.provenance
            .get(member)
            .copied()
            .ok_or_else(|| ToDaeError::internal("clock-domain member is outside its owner arena"))
    }
}

struct DisjointDomains {
    parent: Vec<usize>,
}

impl DisjointDomains {
    fn new(len: usize) -> Self {
        Self {
            parent: (0..len).collect(),
        }
    }

    fn find(&mut self, member: usize) -> usize {
        let mut root = member;
        while self.parent[root] != root {
            root = self.parent[root];
        }
        let mut cursor = member;
        while self.parent[cursor] != cursor {
            let next = self.parent[cursor];
            self.parent[cursor] = root;
            cursor = next;
        }
        root
    }

    fn push(&mut self) -> usize {
        let member = self.parent.len();
        self.parent.push(member);
        member
    }

    fn union(&mut self, lhs: usize, rhs: usize) {
        let lhs = self.find(lhs);
        let rhs = self.find(rhs);
        if lhs != rhs {
            self.parent[rhs] = lhs;
        }
    }
}
