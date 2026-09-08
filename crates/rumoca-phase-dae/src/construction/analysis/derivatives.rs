use rumoca_core::FallibleExpressionVisitor;
use rumoca_ir_flat::FallibleStatementVisitor;

use super::reference_identity::reference_has_exact_identity;
use super::*;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct DerivativeKey {
    span: Span,
    target: rumoca_core::SourceOccurrenceId,
    subscripts: u64,
}

#[derive(Clone)]
struct DerivativeCandidate {
    target: rumoca_core::SourceOccurrenceId,
    subscripts: Box<[Subscript]>,
    dimensions: Box<[i64]>,
    coverage: DerivativeCoverage,
}

#[derive(Clone)]
enum DerivativeCoverage {
    Whole,
    Axes(Box<[DerivativeAxisCoverage]>),
}

#[derive(Clone, Copy)]
enum DerivativeAxisCoverage {
    Full,
    Point(i64),
    Range {
        binder: rumoca_core::StructuredIndexBinderId,
        lower: i64,
        upper: i64,
        step: i64,
    },
    Unknown,
}

pub(super) struct DerivativeCandidates {
    plans: Vec<(DerivativeKey, DerivativeCandidate)>,
}

pub(in crate::construction) struct DerivativePlan {
    target: rumoca_core::SourceOccurrenceId,
    subscripts: Box<[Subscript]>,
}

impl DerivativePlan {
    pub(in crate::construction) fn target(&self) -> rumoca_core::SourceOccurrenceId {
        self.target
    }

    pub(in crate::construction) fn subscripts(&self) -> &[Subscript] {
        &self.subscripts
    }
}

pub(in crate::construction) struct DerivativePlans {
    plans: HashMap<DerivativeKey, Vec<DerivativePlan>>,
}

impl DerivativePlans {
    pub(in crate::construction) fn certificate(
        &self,
        expression: &Expression,
    ) -> Option<&DerivativePlan> {
        let Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            span,
        } = expression
        else {
            return None;
        };
        let [argument] = args.as_slice() else {
            return None;
        };
        let (reference, subscripts) = derivative_reference(argument)?;
        let target = rumoca_core::SourceOccurrenceId::try_from(reference.instance_id()?).ok()?;
        let key = DerivativeKey {
            span: *span,
            target,
            subscripts: rumoca_core::subscripts_semantic_fingerprint(subscripts),
        };
        self.plans
            .get(&key)?
            .iter()
            .find(|plan| rumoca_core::subscripts_semantically_equal(&plan.subscripts, subscripts))
    }
}

impl DerivativeCandidates {
    fn state_targets(&self) -> Result<StateTargets, ToDaeError> {
        struct CoverageGroup<'candidate> {
            target: rumoca_core::SourceOccurrenceId,
            span: Span,
            dimensions: &'candidate [i64],
            candidates: Vec<&'candidate DerivativeCandidate>,
        }

        let mut group_indices = HashMap::<rumoca_core::SourceOccurrenceId, usize>::new();
        let mut groups = Vec::<CoverageGroup<'_>>::new();
        for (key, candidate) in &self.plans {
            if let Some(index) = group_indices.get(&candidate.target).copied() {
                groups[index].candidates.push(candidate);
            } else {
                group_indices.insert(candidate.target, groups.len());
                groups.push(CoverageGroup {
                    target: candidate.target,
                    span: key.span,
                    dimensions: &candidate.dimensions,
                    candidates: vec![candidate],
                });
            }
        }
        let mut occurrences = HashSet::with_capacity(groups.len());
        for group in groups {
            if !derivative_coverage_is_complete(group.dimensions, &group.candidates) {
                return Err(ToDaeError::unsupported_flat(
                    "partial tensor derivative state",
                    "the current DAE variable role is tensor-wide, so indexed der(...) operands must prove complete declared-domain coverage",
                    group.span,
                ));
            }
            occurrences.insert(group.target);
        }
        Ok(StateTargets { occurrences })
    }

    fn admit(
        self,
        roles: &HashMap<rumoca_core::SourceOccurrenceId, PlannedRole>,
    ) -> Result<DerivativePlans, ToDaeError> {
        let mut plans = HashMap::with_capacity(self.plans.len());
        for (key, candidate) in self.plans {
            if !matches!(roles.get(&candidate.target), Some(PlannedRole::State)) {
                return Err(ToDaeError::unsupported_flat(
                    "derivative expression",
                    "der(...) must name a primitive continuous Real state coordinate",
                    key.span,
                ));
            }
            plans
                .entry(key)
                .or_insert_with(Vec::new)
                .push(DerivativePlan {
                    target: candidate.target,
                    subscripts: candidate.subscripts,
                });
        }
        Ok(DerivativePlans { plans })
    }
}

pub(super) struct StateTargets {
    occurrences: HashSet<rumoca_core::SourceOccurrenceId>,
}

impl StateTargets {
    pub(super) fn contains(&self, occurrence: rumoca_core::SourceOccurrenceId) -> bool {
        self.occurrences.contains(&occurrence)
    }
}

pub(super) fn analyze_derivatives(
    owners: &StructuredEquationOwners<'_>,
) -> Result<(StateTargets, DerivativeCandidates), ToDaeError> {
    let flat = owners.model();
    let mut analyzer = DerivativeAnalyzer {
        variables: ModelVariableIndex::from_validated_flat(flat),
        candidates: DerivativeCandidates { plans: Vec::new() },
        seen: HashMap::new(),
        in_function: false,
        structured_binders: None,
    };
    analyzer.visit_model_owners(owners)?;
    let states = analyzer.candidates.state_targets()?;
    Ok((states, analyzer.candidates))
}

pub(super) fn admit_derivative_roles(
    candidates: DerivativeCandidates,
    roles: &HashMap<rumoca_core::SourceOccurrenceId, PlannedRole>,
) -> Result<DerivativePlans, ToDaeError> {
    candidates.admit(roles)
}

struct ModelVariableIndex<'flat> {
    by_instance: HashMap<InstanceId, (&'flat VarName, &'flat flat::Variable)>,
}

impl<'flat> ModelVariableIndex<'flat> {
    /// Build the projection after `validate_source_model` has admitted Flat.
    /// That single boundary already proves allocated, unique instance IDs.
    fn from_validated_flat(flat: &'flat flat::Model) -> Self {
        Self {
            by_instance: flat
                .variables
                .iter()
                .map(|(name, variable)| (variable.instance_id, (name, variable)))
                .collect(),
        }
    }

    fn resolve(
        &self,
        reference: &rumoca_core::Reference,
        span: Span,
    ) -> Result<(rumoca_core::SourceOccurrenceId, &'flat flat::Variable), ToDaeError> {
        let instance = reference.instance_id().ok_or_else(|| {
            invalid_derivative_identity(
                "the operand has no allocated Flat occurrence identity",
                span,
            )
        })?;
        let target = rumoca_core::SourceOccurrenceId::try_from(instance).map_err(|_| {
            invalid_derivative_identity(
                "the operand has no allocated Flat occurrence identity",
                span,
            )
        })?;
        let (name, variable) = self.by_instance.get(&instance).copied().ok_or_else(|| {
            invalid_derivative_identity(
                "the operand occurrence is not present in this Flat model",
                span,
            )
        })?;
        let component_matches = variable
            .component_ref
            .as_ref()
            .is_some_and(|target| reference_has_exact_identity(reference, target));
        if reference.var_name() != name || !component_matches {
            return Err(invalid_derivative_identity(
                "the operand's cached name or component reference contradicts its occurrence identity",
                span,
            ));
        }
        Ok((target, variable))
    }
}

struct DerivativeAnalyzer<'flat> {
    variables: ModelVariableIndex<'flat>,
    candidates: DerivativeCandidates,
    seen: HashMap<DerivativeKey, Vec<DerivativeCandidate>>,
    in_function: bool,
    structured_binders: Option<HashMap<rumoca_core::StructuredIndexBinderId, BinderRange>>,
}

#[derive(Clone, Copy)]
struct BinderRange {
    lower: i64,
    upper: i64,
    step: i64,
}

impl DerivativeAnalyzer<'_> {
    fn plan(
        &self,
        arguments: &[Expression],
        span: Span,
    ) -> Result<(DerivativeKey, DerivativeCandidate), ToDaeError> {
        require_span(span, "derivative expression")?;
        if self.in_function {
            return Err(ToDaeError::unsupported_runtime_operator(
                "der",
                "der is prohibited in a function semantic owner by SPEC_0022 FUNC-010",
                span,
            ));
        }
        let [argument] = arguments else {
            return Err(invalid_derivative_expression(span));
        };
        let Some((reference, subscripts)) = derivative_reference(argument) else {
            return Err(invalid_derivative_expression(span));
        };
        let (target, variable) = self.variables.resolve(reference, span)?;
        argument
            .span()
            .ok_or_else(|| ToDaeError::MissingProvenance {
                owner: "derivative operand".to_owned(),
            })?;
        Ok((
            DerivativeKey {
                span,
                target,
                subscripts: rumoca_core::subscripts_semantic_fingerprint(subscripts),
            },
            DerivativeCandidate {
                target,
                subscripts: subscripts.to_vec().into_boxed_slice(),
                dimensions: variable.dims.clone().into_boxed_slice(),
                coverage: derivative_coverage(subscripts, self.structured_binders.as_ref()),
            },
        ))
    }
}

impl ModelExpressionOwnerVisitor for DerivativeAnalyzer<'_> {
    fn enter_function_owners(&mut self) -> Result<(), Self::Error> {
        self.in_function = true;
        Ok(())
    }

    fn enter_structured_family(
        &mut self,
        family: &flat::StructuredEquationFamily,
    ) -> Result<(), Self::Error> {
        let binders = family
            .domain
            .binders
            .iter()
            .map(|binder| {
                (
                    binder.id,
                    BinderRange {
                        lower: binder.lower,
                        upper: binder.upper,
                        step: binder.step,
                    },
                )
            })
            .collect();
        self.structured_binders = Some(binders);
        Ok(())
    }

    fn leave_structured_family(&mut self) -> Result<(), Self::Error> {
        self.structured_binders = None;
        Ok(())
    }
}

impl FallibleExpressionVisitor for DerivativeAnalyzer<'_> {
    type Error = ToDaeError;

    fn visit_expression(&mut self, expression: &Expression) -> Result<(), Self::Error> {
        if let Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            span,
        } = expression
        {
            let (key, plan) = self.plan(args, *span)?;
            let bucket = self.seen.entry(key.clone()).or_default();
            if !bucket
                .iter()
                .any(|existing| same_candidate(existing, &plan))
            {
                bucket.push(plan.clone());
                self.candidates.plans.push((key, plan));
            }
        }
        self.walk_expression(expression)
    }
}

impl FallibleStatementVisitor for DerivativeAnalyzer<'_> {}

fn invalid_derivative_expression(span: Span) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "derivative expression",
        "der(...) must have exactly one resolved variable-reference operand",
        span,
    )
}

fn invalid_derivative_identity(detail: &'static str, span: Span) -> ToDaeError {
    ToDaeError::unsupported_flat("derivative target identity", detail, span)
}

fn same_candidate(left: &DerivativeCandidate, right: &DerivativeCandidate) -> bool {
    left.target == right.target
        && rumoca_core::subscripts_semantically_equal(&left.subscripts, &right.subscripts)
}

fn derivative_coverage(
    subscripts: &[Subscript],
    binders: Option<&HashMap<rumoca_core::StructuredIndexBinderId, BinderRange>>,
) -> DerivativeCoverage {
    if subscripts.is_empty() {
        return DerivativeCoverage::Whole;
    }
    DerivativeCoverage::Axes(
        subscripts
            .iter()
            .map(|subscript| derivative_axis_coverage(subscript, binders))
            .collect(),
    )
}

fn derivative_axis_coverage(
    subscript: &Subscript,
    binders: Option<&HashMap<rumoca_core::StructuredIndexBinderId, BinderRange>>,
) -> DerivativeAxisCoverage {
    match subscript {
        Subscript::Index { value, .. } => DerivativeAxisCoverage::Point(*value),
        Subscript::Colon { .. } => DerivativeAxisCoverage::Full,
        Subscript::Expr { expr, .. } => match expr.as_ref() {
            Expression::Literal {
                value: rumoca_core::Literal::Integer(value),
                ..
            } => DerivativeAxisCoverage::Point(*value),
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => {
                let Some(binder) = name.structured_binder() else {
                    return DerivativeAxisCoverage::Unknown;
                };
                let Some(range) = binders.and_then(|binders| binders.get(&binder)) else {
                    return DerivativeAxisCoverage::Unknown;
                };
                DerivativeAxisCoverage::Range {
                    binder,
                    lower: range.lower,
                    upper: range.upper,
                    step: range.step,
                }
            }
            _ => DerivativeAxisCoverage::Unknown,
        },
    }
}

fn derivative_coverage_is_complete(
    dimensions: &[i64],
    candidates: &[&DerivativeCandidate],
) -> bool {
    if dimensions.is_empty() {
        return candidates
            .iter()
            .any(|candidate| matches!(candidate.coverage, DerivativeCoverage::Whole));
    }
    if candidates
        .iter()
        .any(|candidate| coverage_spans_whole_shape(dimensions, &candidate.coverage))
    {
        return true;
    }
    dimensions.len() == 1 && one_dimensional_coverage_is_complete(dimensions[0], candidates)
}

fn coverage_spans_whole_shape(dimensions: &[i64], coverage: &DerivativeCoverage) -> bool {
    match coverage {
        DerivativeCoverage::Whole => true,
        DerivativeCoverage::Axes(axes) if axes.len() == dimensions.len() => {
            let mut used_binders = HashSet::new();
            axes.iter()
                .zip(dimensions)
                .all(|(axis, dimension)| match axis {
                    DerivativeAxisCoverage::Full => true,
                    DerivativeAxisCoverage::Range {
                        binder,
                        lower,
                        upper,
                        step,
                    } => {
                        used_binders.insert(*binder)
                            && ((*lower == 1 && *upper == *dimension && *step == 1)
                                || (*lower == *dimension && *upper == 1 && *step == -1))
                    }
                    DerivativeAxisCoverage::Point(_) | DerivativeAxisCoverage::Unknown => false,
                })
        }
        DerivativeCoverage::Axes(_) => false,
    }
}

fn one_dimensional_coverage_is_complete(
    dimension: i64,
    candidates: &[&DerivativeCandidate],
) -> bool {
    if dimension == 0 {
        return true;
    }
    let mut intervals = Vec::with_capacity(candidates.len());
    for candidate in candidates {
        let DerivativeCoverage::Axes(axes) = &candidate.coverage else {
            return true;
        };
        let [axis] = axes.as_ref() else {
            return false;
        };
        let interval = match axis {
            DerivativeAxisCoverage::Full => (1, dimension),
            DerivativeAxisCoverage::Point(index) => (*index, *index),
            DerivativeAxisCoverage::Range {
                lower, upper, step, ..
            } if *step == 1 => (*lower, *upper),
            DerivativeAxisCoverage::Range {
                lower, upper, step, ..
            } if *step == -1 => (*upper, *lower),
            DerivativeAxisCoverage::Range { .. } | DerivativeAxisCoverage::Unknown => return false,
        };
        if interval.0 < 1 || interval.1 > dimension || interval.0 > interval.1 {
            return false;
        }
        intervals.push(interval);
    }
    intervals.sort_unstable();
    let mut covered_through = 0i64;
    for (lower, upper) in intervals {
        if lower > covered_through.saturating_add(1) {
            return false;
        }
        covered_through = covered_through.max(upper);
    }
    covered_through == dimension
}
