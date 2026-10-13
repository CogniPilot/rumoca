//! MLS §16.5.2 clock conversions: the relations `subSample`, `superSample`,
//! `shiftSample`, and `backSample` state between clock partitions, and the
//! proof that every relation agrees with the partitions' owning clocks.

use super::*;

/// The MLS §16.5.2 relation one clock conversion states between its source and
/// target partitions.
#[derive(Clone, Copy)]
pub(super) enum ConversionKind {
    /// Every argument is given, so the relation fixes one lattice from the
    /// other.
    Exact(dae::ClockTransferKind),
    /// `subSample(u)` / `superSample(u)` without `factor`: the factor is
    /// inferred from the two partitions' own clocks, so the relation fixes
    /// neither lattice and is proven once both are known.
    Inferred(InferredFactor),
}

#[derive(Clone, Copy)]
pub(super) enum InferredFactor {
    SubSample,
    SuperSample,
}

#[derive(Clone, Copy)]
pub(super) struct ClockConversionEdge {
    source: usize,
    target: usize,
    kind: ConversionKind,
    span: Span,
}

impl ClockConversionEdge {
    pub(super) fn new(
        target_members: &[usize],
        source_members: &[usize],
        kind: ConversionKind,
        span: Span,
    ) -> Result<Self, ToDaeError> {
        let [target] = target_members else {
            return Err(ToDaeError::unsupported_flat(
                "clocked value conversion ownership proof",
                "a clock conversion must define exactly one target clock partition",
                span,
            ));
        };
        let Some(&source) = source_members.first() else {
            return Err(ToDaeError::unsupported_flat(
                "clocked value conversion ownership proof",
                "a clock conversion source must belong to a proven clock partition",
                span,
            ));
        };
        Ok(Self {
            source,
            target: *target,
            kind,
            span,
        })
    }
}

pub(super) struct ClockConversionExpression<'expression> {
    pub(super) source: &'expression Expression,
    pub(super) kind: ConversionKind,
    pub(super) span: Span,
}

pub(super) fn value_clock_conversion_equation<'expression>(
    equation: &'expression flat::Equation,
    constants: &EvalContext,
) -> Result<Option<ClockConversionExpression<'expression>>, ToDaeError> {
    let Some((lhs, rhs)) = subtraction_operands(&equation.residual) else {
        return Ok(None);
    };
    match (
        value_clock_conversion(lhs, constants)?,
        value_clock_conversion(rhs, constants)?,
    ) {
        (None, Some(conversion)) | (Some(conversion), None) => Ok(Some(conversion)),
        (None, None) => Ok(None),
        (Some(_), Some(_)) => Err(ToDaeError::unsupported_flat(
            "clocked value conversion ownership proof",
            "one equation cannot define two cross-clock value transfers",
            equation.span,
        )),
    }
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
        (BuiltinFunction::SubSample, [_]) => ConversionKind::Inferred(InferredFactor::SubSample),
        (BuiltinFunction::SuperSample, [_]) => {
            ConversionKind::Inferred(InferredFactor::SuperSample)
        }
        _ => return exact_value_clock_conversion(expression, constants),
    };
    Ok(Some(ClockConversionExpression {
        source: &args[0],
        kind,
        span: *span,
    }))
}

fn exact_value_clock_conversion<'expression>(
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
            factor: clock_integer(factor, constants, function.name(), *span)?,
        },
        (BuiltinFunction::SuperSample, [_, factor]) => dae::ClockTransferKind::SuperSample {
            factor: clock_integer(factor, constants, function.name(), *span)?,
        },
        (BuiltinFunction::ShiftSample, [_, counter]) => dae::ClockTransferKind::ShiftSample {
            counter: clock_integer(counter, constants, function.name(), *span)?,
            resolution: 1,
        },
        (BuiltinFunction::ShiftSample, [_, counter, resolution]) => {
            dae::ClockTransferKind::ShiftSample {
                counter: clock_integer(counter, constants, function.name(), *span)?,
                resolution: clock_integer(resolution, constants, function.name(), *span)?,
            }
        }
        (BuiltinFunction::BackSample, [_, counter]) => dae::ClockTransferKind::BackSample {
            counter: clock_integer(counter, constants, function.name(), *span)?,
            resolution: 1,
        },
        (BuiltinFunction::BackSample, [_, counter, resolution]) => {
            dae::ClockTransferKind::BackSample {
                counter: clock_integer(counter, constants, function.name(), *span)?,
                resolution: clock_integer(resolution, constants, function.name(), *span)?,
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
        kind: ConversionKind::Exact(kind),
        span: *span,
    }))
}

/// The lattice a clock conversion reads; an event clock has none.
fn conversion_lattice(plan: ClockPlan, span: Span) -> Result<ClockLattice, ToDaeError> {
    match plan.lattice() {
        Some(lattice) => Ok(lattice),
        None => Err(ToDaeError::unsupported_runtime_operator(
            "clock conversion",
            "an event clock has no periodic lattice to convert",
            span,
        )),
    }
}

pub(super) fn propagate_clock_conversion_owners(
    edges: &[ClockConversionEdge],
    domains: &mut DisjointDomains,
    owners: &mut HashMap<usize, (ClockPlan, Span)>,
) -> Result<(), ToDaeError> {
    loop {
        let mut progress = false;
        for edge in edges {
            let ConversionKind::Exact(kind) = edge.kind else {
                continue;
            };
            let source_root = domains.find(edge.source);
            let target_root = domains.find(edge.target);
            let source = owners.get(&source_root).copied();
            let target = owners.get(&target_root).copied();
            let lattice = |plan: ClockPlan| conversion_lattice(plan, edge.span);
            if let Some(fixed) =
                propagate_event_edge(edge, kind, (source_root, target_root), owners)?
            {
                progress |= fixed;
                continue;
            }
            match (source, target) {
                (Some((source, _)), Some((target, _))) => {
                    require_conversion_lattice(
                        kind,
                        edge.span,
                        lattice(source)?,
                        lattice(target)?,
                    )?;
                }
                (Some((source, _)), None) => {
                    let converted = conversion_target_lattice(kind, edge.span, lattice(source)?)?;
                    owners.insert(
                        target_root,
                        (ClockPlan::periodic(converted, edge.span), edge.span),
                    );
                    progress = true;
                }
                (None, Some((target, _))) => {
                    let converted = conversion_source_lattice(kind, edge.span, lattice(target)?)?;
                    owners.insert(
                        source_root,
                        (ClockPlan::periodic(converted, edge.span), edge.span),
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

/// Propagate one conversion edge that has an event clock at either end;
/// `None` when neither end is an event clock, otherwise whether the edge fixed
/// the owner of its other end.
fn propagate_event_edge(
    edge: &ClockConversionEdge,
    kind: dae::ClockTransferKind,
    (source_root, target_root): (usize, usize),
    owners: &mut HashMap<usize, (ClockPlan, Span)>,
) -> Result<Option<bool>, ToDaeError> {
    let source = owners.get(&source_root).copied();
    let target = owners.get(&target_root).copied();
    let is_event =
        |plan: Option<(ClockPlan, Span)>| plan.is_some_and(|(plan, _)| plan.event().is_some());
    if !is_event(source) && !is_event(target) {
        return Ok(None);
    }
    let Some(owner) = event_conversion_owner(kind, edge.span, source, target)? else {
        return Ok(Some(false));
    };
    let root = if source.is_some() {
        target_root
    } else {
        source_root
    };
    owners.insert(root, (owner, edge.span));
    Ok(Some(true))
}

/// MLS §16.5.2 on an event clock, which has no lattice: the only conversion is
/// `shiftSample(u, counter)` by whole ticks, relating the event clock that
/// skips `skip` ticks to the one that skips `skip + counter`. Called when one
/// end is an event clock; returns the owner this edge fixes for its unowned
/// end, or `None` when both ends are owned (after proving they agree).
fn event_conversion_owner(
    kind: dae::ClockTransferKind,
    span: Span,
    source: Option<(ClockPlan, Span)>,
    target: Option<(ClockPlan, Span)>,
) -> Result<Option<ClockPlan>, ToDaeError> {
    let dae::ClockTransferKind::ShiftSample {
        counter,
        resolution,
    } = kind
    else {
        return Err(event_clock_conversion("clock conversion", span));
    };
    match (source, target) {
        (Some((source, _)), Some((target, _))) => {
            let shifted = match source.shift_event(counter, resolution, span) {
                Some(shifted) => shifted?,
                None => return Err(event_clock_conversion("shiftSample", span)),
            };
            if shifted.schedule != target.schedule {
                return Err(ToDaeError::unsupported_flat(
                    "clocked value conversion ownership proof",
                    "a shiftSample of an event clock must skip exactly its counter of ticks",
                    span,
                ));
            }
            Ok(None)
        }
        (Some((source, _)), None) => source.shift_event(counter, resolution, span).transpose(),
        (None, Some((target, _))) => match unshift_event_clock(target, counter, resolution) {
            Some(unshifted) => Ok(Some(unshifted)),
            None => Err(event_clock_conversion("shiftSample", span)),
        },
        (None, None) => Ok(None),
    }
}

/// MLS §16.5.2: a `subSample(u)` or `superSample(u)` without `factor` has its
/// factor inferred from the clocks of `u` and of the result. Both partitions
/// must already be owned; the inferred factor is the exact integer ratio of
/// their periods, and the lattice that factor produces from the source must be
/// the target's own. Lowering rebuilds that exact transfer from the same two
/// owners ([`inferred_clock_transfer`]).
pub(super) fn validate_inferred_conversions(
    edges: &[ClockConversionEdge],
    domains: &mut DisjointDomains,
    owners: &HashMap<usize, (ClockPlan, Span)>,
) -> Result<(), ToDaeError> {
    for edge in edges {
        let ConversionKind::Inferred(factor) = edge.kind else {
            continue;
        };
        let owner = |member| owners.get(&member).and_then(|(clock, _)| clock.lattice());
        let (Some(source), Some(target)) = (
            owner(domains.find(edge.source)),
            owner(domains.find(edge.target)),
        ) else {
            return Err(ToDaeError::unresolved_clock_schedule(
                "clock conversion",
                "an inferred sub-sampling factor needs both the source and the target clock",
                edge.span,
            ));
        };
        if inferred_transfer(factor, source, target).is_none() {
            return Err(ToDaeError::unsupported_flat(
                "clocked value conversion ownership proof",
                "the source and target clocks are not related by an integer sampling factor",
                edge.span,
            ));
        }
    }
    Ok(())
}

/// The exact transfer an inferred-factor `subSample(u)` or `superSample(u)`
/// states from `source` to `target` (MLS §16.5.2); `None` for any other
/// operator or when no positive integer factor relates the two clocks.
pub(in crate::construction) fn inferred_clock_transfer(
    function: BuiltinFunction,
    source: ClockLattice,
    target: ClockLattice,
) -> Option<dae::ClockTransferKind> {
    let factor = match function {
        BuiltinFunction::SubSample => InferredFactor::SubSample,
        BuiltinFunction::SuperSample => InferredFactor::SuperSample,
        _ => return None,
    };
    inferred_transfer(factor, source, target)
}

/// The exact transfer an inferred-factor conversion states between `source`
/// and `target`, or `None` when no positive integer factor relates them.
fn inferred_transfer(
    factor: InferredFactor,
    source: ClockLattice,
    target: ClockLattice,
) -> Option<dae::ClockTransferKind> {
    let (coarse, fine) = match factor {
        InferredFactor::SubSample => (target, source),
        InferredFactor::SuperSample => (source, target),
    };
    let ratio = coarse.period().checked_div(fine.period()).ok()?;
    if ratio.denominator() != 1 || ratio.numerator() < 1 {
        return None;
    }
    let factor_value = i64::try_from(ratio.numerator()).ok()?;
    let (kind, lattice) = match factor {
        InferredFactor::SubSample => (
            dae::ClockTransferKind::SubSample {
                factor: factor_value,
            },
            source.sub_sample(factor_value),
        ),
        InferredFactor::SuperSample => (
            dae::ClockTransferKind::SuperSample {
                factor: factor_value,
            },
            source.super_sample(factor_value),
        ),
    };
    (lattice.ok()? == target).then_some(kind)
}

fn require_conversion_lattice(
    kind: dae::ClockTransferKind,
    span: Span,
    source: ClockLattice,
    target: ClockLattice,
) -> Result<(), ToDaeError> {
    if conversion_target_lattice(kind, span, source)? == target {
        Ok(())
    } else {
        Err(ToDaeError::unsupported_flat(
            "clocked value conversion ownership proof",
            "the source and target partitions conflict with the exact clock conversion",
            span,
        ))
    }
}

fn conversion_target_lattice(
    kind: dae::ClockTransferKind,
    span: Span,
    source: ClockLattice,
) -> Result<ClockLattice, ToDaeError> {
    let result = match kind {
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
    };
    match result {
        Ok(lattice) => Ok(lattice),
        Err(error) => Err(ToDaeError::unsupported_runtime_operator(
            "clocked value conversion",
            error.to_string(),
            span,
        )),
    }
}

fn conversion_source_lattice(
    kind: dae::ClockTransferKind,
    span: Span,
    target: ClockLattice,
) -> Result<ClockLattice, ToDaeError> {
    let result = match kind {
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
    match result {
        Ok(lattice) => Ok(lattice),
        Err(error) => Err(ToDaeError::unsupported_runtime_operator(
            "clocked value conversion",
            error.to_string(),
            span,
        )),
    }
}

/// The partition incidence and clock conversions of declaration bindings.
///
/// A binding `v = expr` of a runtime coordinate is the equation `v = expr`
/// (MLS §4.4.2.1), so it joins `v` to the partition of every clocked coordinate
/// `expr` reads, and a binding that is itself a clock conversion (MLS §16.5.2)
/// relates `v`'s partition to its source's the way the equation would.
pub(super) fn clocked_binding_conversion_edges(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    constants: &EvalContext,
    ordinals: &HashMap<InstanceId, usize>,
    occurrences: &mut [Option<Span>],
    domains: &mut DisjointDomains,
) -> Result<Vec<ClockConversionEdge>, ToDaeError> {
    let mut edges = Vec::new();
    for (name, variable) in &flat.variables {
        let Some(binding) = variable.binding.as_ref() else {
            continue;
        };
        if !roles
            .get(name)
            .is_some_and(|role| is_clock_runtime_role(*role))
        {
            continue;
        }
        let mut incidence = ClockIncidence::default();
        incidence.insert(variable.instance_id, variable.source_span);
        let target_members = register_incidence(&incidence, ordinals, occurrences, domains);
        let binding_incidence = expression_clock_incidence(binding, flat, roles);
        let binding_members =
            register_incidence(&binding_incidence, ordinals, occurrences, domains);
        if let (Some(&target), Some(&member)) = (target_members.first(), binding_members.first()) {
            domains.union(target, member);
        }
        let Some(conversion) = value_clock_conversion(binding, constants)? else {
            continue;
        };
        let source_incidence = expression_clock_incidence(conversion.source, flat, roles);
        let source_members = register_incidence(&source_incidence, ordinals, occurrences, domains);
        edges.push(ClockConversionEdge::new(
            &target_members,
            &source_members,
            conversion.kind,
            conversion.span,
        )?);
    }
    Ok(edges)
}

/// Where the conversion edges of clocked `when` bodies are registered.
pub(super) struct WhenConversionScope<'scope> {
    pub(super) flat: &'scope flat::Model,
    pub(super) roles: &'scope HashMap<VarName, PlannedRole>,
    pub(super) constants: &'scope EvalContext,
    pub(super) ordinals: &'scope HashMap<InstanceId, usize>,
}

/// The clock conversions a clocked `when` body states (MLS §16.5.2), one edge
/// per `target = conversion(source, ...)` definition.
///
/// A conditional whose guard the parameter values decide contributes only the
/// branch they select: clock partitioning is a static property of the model
/// (MLS §16.7), so the conversions of an unselected branch state no relation.
pub(super) fn clocked_when_conversion_edges(
    scope: &WhenConversionScope<'_>,
    plans: &HashMap<InstanceId, ClockPlan>,
    occurrences: &mut [Option<Span>],
    domains: &mut DisjointDomains,
) -> Result<Vec<ClockConversionEdge>, ToDaeError> {
    let mut edges = Vec::new();
    for chain in &scope.flat.when_chains {
        for branch in chain.branches() {
            if clock_condition_plan(&branch.condition, scope.flat, plans).is_none()
                && !is_inferred_clock_condition(&branch.condition)
            {
                continue;
            }
            collect_when_conversion_edges(
                scope,
                &branch.equations,
                occurrences,
                domains,
                &mut edges,
            )?;
        }
    }
    Ok(edges)
}

fn collect_when_conversion_edges(
    scope: &WhenConversionScope<'_>,
    equations: &[flat::WhenEquation],
    occurrences: &mut [Option<Span>],
    domains: &mut DisjointDomains,
    edges: &mut Vec<ClockConversionEdge>,
) -> Result<(), ToDaeError> {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign {
                target,
                value,
                span,
                ..
            } => {
                let Some(conversion) = value_clock_conversion(value, scope.constants)? else {
                    continue;
                };
                let mut target_incidence = ClockIncidence::default();
                register_runtime_coordinate(
                    scope.flat,
                    target,
                    *span,
                    scope.roles,
                    &mut target_incidence,
                );
                let target_members =
                    register_incidence(&target_incidence, scope.ordinals, occurrences, domains);
                let source_incidence =
                    expression_clock_incidence(conversion.source, scope.flat, scope.roles);
                let source_members =
                    register_incidence(&source_incidence, scope.ordinals, occurrences, domains);
                edges.push(ClockConversionEdge::new(
                    &target_members,
                    &source_members,
                    conversion.kind,
                    conversion.span,
                )?);
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                let selected = statically_selected_when_branch(branches, else_branch, scope);
                let reached: Vec<&[flat::WhenEquation]> = match selected {
                    Some(selected) => selected.into_iter().collect(),
                    None => branches
                        .iter()
                        .map(|(_, equations)| equations.as_slice())
                        .chain(else_branch.as_deref())
                        .collect(),
                };
                for equations in reached {
                    collect_when_conversion_edges(scope, equations, occurrences, domains, edges)?;
                }
            }
            _ => {}
        }
    }
    Ok(())
}

/// `Some(branch)` when the conditional selects clock structure and the
/// parameter values decide it (`Some(None)` for a decided conditional without
/// an else part), `None` otherwise.
fn statically_selected_when_branch<'equations>(
    branches: &'equations [(Expression, Vec<flat::WhenEquation>)],
    else_branch: &'equations Option<Vec<flat::WhenEquation>>,
    scope: &WhenConversionScope<'_>,
) -> Option<Option<&'equations [flat::WhenEquation]>> {
    if !when_conditional_selects_clock_structure(branches, else_branch.as_deref()) {
        return None;
    }
    for (condition, equations) in branches {
        if eval_expr(condition, scope.constants).ok()?.as_bool()? {
            return Some(Some(equations));
        }
    }
    Some(else_branch.as_deref())
}

/// Whether a conditional in a clocked `when` body selects clock structure: an
/// arm states an MLS §16.5.2 clock conversion. Clock partitioning is static
/// (MLS §16.7), so such a conditional is decided by its parameter values at
/// translation and its guard parameters are structural (MLS §4.5).
pub(in crate::construction) fn when_conditional_selects_clock_structure(
    branches: &[(Expression, Vec<flat::WhenEquation>)],
    else_branch: Option<&[flat::WhenEquation]>,
) -> bool {
    branches
        .iter()
        .map(|(_, equations)| equations.as_slice())
        .chain(else_branch)
        .flatten()
        .any(|equation| match equation {
            flat::WhenEquation::Assign { value, .. }
            | flat::WhenEquation::AssignElement { value, .. } => mentions_clock_conversion(value),
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => when_conditional_selects_clock_structure(branches, else_branch.as_deref()),
            _ => false,
        })
}

fn mentions_clock_conversion(expression: &Expression) -> bool {
    matches!(
        expression,
        Expression::BuiltinCall {
            function: BuiltinFunction::SubSample
                | BuiltinFunction::SuperSample
                | BuiltinFunction::ShiftSample
                | BuiltinFunction::BackSample
                | BuiltinFunction::NoClock,
            ..
        }
    ) || expression_children(expression)
        .into_iter()
        .any(mentions_clock_conversion)
}

/// The event clock a shifted event clock was derived from, when the shift was
/// by whole ticks at resolution one and skipped at least `counter` ticks.
fn unshift_event_clock(target: ClockPlan, counter: i64, resolution: i64) -> Option<ClockPlan> {
    let (source, skip) = target.event()?;
    if resolution != 1 {
        return None;
    }
    let counter = u32::try_from(counter).ok()?;
    Some(ClockPlan {
        schedule: ClockSchedule::Event {
            source,
            skip: skip.checked_sub(counter)?,
        },
        constructor_span: target.constructor_span,
    })
}
