use super::*;
use rumoca_core::FallibleExpressionVisitor;

/// MLS §12.4.4 definedness certificate for the mutable values of one function.
///
/// A function output or local has no language-defined initial value, so the
/// checked DAE may only read a value the algorithm already wrote. A whole
/// assignment owns every element at once. An element write starts from a
/// generated aggregate seed, which stays a proven-dead value exactly while the
/// algorithm writes every declared element before anything reads the value;
/// this certificate carries that proof.
#[derive(Clone)]
pub(super) struct FunctionDefinitions {
    values: HashMap<VarName, ValueCoverage>,
    /// Values only some conditional branches define, keyed to the conditional
    /// that left them without a total owner.
    branch_only: HashMap<VarName, BranchOnlyCoverage>,
}

#[derive(Clone)]
struct BranchOnlyCoverage {
    span: Span,
    guard: Option<Expression>,
    coverage: Option<ValueCoverage>,
}

#[derive(Clone)]
enum ValueCoverage {
    /// A whole-value assignment owns every element.
    Whole,
    /// Element writes over a generated seed, with the exact indices proven so
    /// far. `proven` drops once a write covers indices analysis cannot name.
    Elements {
        covered: HashSet<Vec<i64>>,
        scalars: usize,
        proven: bool,
    },
}

impl ValueCoverage {
    fn is_total(&self) -> bool {
        match self {
            Self::Whole => true,
            Self::Elements {
                covered,
                scalars,
                proven,
            } => *proven && covered.len() == *scalars,
        }
    }

    /// Keep only the elements both paths define.
    fn meet(self, other: &Self) -> Self {
        match (self, other) {
            (Self::Whole, Self::Whole) => Self::Whole,
            (Self::Whole, other) => other.clone(),
            (owned, Self::Whole) => owned,
            (
                Self::Elements {
                    covered,
                    scalars,
                    proven,
                },
                Self::Elements {
                    covered: other_covered,
                    scalars: other_scalars,
                    proven: other_proven,
                },
            ) => Self::Elements {
                covered: covered
                    .into_iter()
                    .filter(|index| other_covered.contains(index))
                    .collect(),
                scalars: scalars.max(*other_scalars),
                proven: proven && *other_proven,
            },
        }
    }
}

/// Exact type tree of a generated aggregate seed whose value is proven dead.
pub(in crate::construction) enum FunctionValueSeed {
    Scalar {
        dimensions: Vec<u32>,
        scalar: dae::ScalarType,
    },
    Record {
        name: VarName,
        dimensions: Vec<u32>,
        fields: Vec<(VarName, FunctionValueSeed)>,
    },
}

impl FunctionDefinitions {
    /// Start from the values MLS §12.4.4 gives a declared binding.
    pub(super) fn new(function: &rumoca_core::Function) -> Self {
        let values = function
            .outputs
            .iter()
            .chain(&function.locals)
            .filter(|value| value.default.is_some())
            .map(|value| (VarName::new(&value.name), ValueCoverage::Whole))
            .collect();
        Self {
            values,
            branch_only: HashMap::new(),
        }
    }

    pub(super) fn is_defined(&self, name: &VarName) -> bool {
        self.values.contains_key(name)
    }

    pub(super) fn has_total_guarded_definition(&self, name: &VarName) -> bool {
        self.branch_only
            .get(name)
            .and_then(|definition| definition.coverage.as_ref())
            .is_some_and(ValueCoverage::is_total)
    }

    pub(super) fn enter_guard(
        &mut self,
        condition: &Expression,
        context: FunctionValidationContext<'_>,
    ) {
        let admitted = self
            .branch_only
            .iter()
            .filter_map(|(name, definition)| {
                let guard = definition.guard.as_ref()?;
                let coverage = definition.coverage.clone()?;
                condition_implies_guard(condition, guard, context, 0)
                    .then(|| (name.clone(), coverage))
            })
            .collect::<Vec<_>>();
        for (name, coverage) in admitted {
            self.branch_only.remove(&name);
            self.values.insert(name, coverage);
        }
    }

    pub(super) fn remember_guarded_branch(
        &mut self,
        condition: &Expression,
        branch: &Self,
        targets: &[VarName],
        span: Span,
    ) {
        for target in targets {
            if self.values.contains_key(target) {
                continue;
            }
            let Some(coverage) = branch.values.get(target) else {
                continue;
            };
            self.branch_only.insert(
                target.clone(),
                BranchOnlyCoverage {
                    span,
                    guard: Some(condition.clone()),
                    coverage: Some(coverage.clone()),
                },
            );
        }
    }

    pub(super) fn forget_varying_guard_paths(
        &mut self,
        generated: &[function_returns::GeneratedBooleanDefinition],
    ) {
        for definition in self.branch_only.values_mut() {
            let Some(guard) = &definition.guard else {
                continue;
            };
            let mut references = Vec::new();
            guard.collect_var_refs(&mut references);
            let invariant = references.iter().all(|target| {
                generated
                    .iter()
                    .any(|generated| &generated.target == target)
            });
            if !invariant {
                definition.guard = None;
                definition.coverage = None;
            }
        }
    }

    pub(super) fn is_total(&self, name: &VarName) -> bool {
        self.values.get(name).is_some_and(ValueCoverage::is_total)
    }

    pub(super) fn define_whole(&mut self, name: &VarName) {
        self.branch_only.remove(name);
        self.values.insert(name.clone(), ValueCoverage::Whole);
    }

    /// Install the output totality already proven by a disjoint early-return
    /// path certificate. The normalized guard sequence uses these as its join
    /// bases; it does not infer totality from these synthetic definitions.
    pub(super) fn assume_certified_outputs(&mut self, function: &rumoca_core::Function) {
        for output in &function.outputs {
            self.define_whole(&VarName::new(&output.name));
        }
    }

    /// Describe the dead initial slot a loop-carried value needs when its first
    /// iteration defines the whole value before any read.
    pub(super) fn whole_loop_seed(
        &self,
        target: &VarName,
        context: FunctionValidationContext<'_>,
        span: Span,
    ) -> Result<FunctionValueSeed, ToDaeError> {
        let declaration = declared_value(target, context).ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "function aggregate seed",
                format!(
                    "`{}.{target}` is not a declared mutable function value",
                    context.function.name
                ),
                span,
            )
        })?;
        function_value_seed(
            declaration,
            declared_dimensions(target, context, span)?,
            context,
            span,
        )
    }

    /// Record one element write and report the seed it needs, if any.
    pub(super) fn write_elements(
        &mut self,
        target: &VarName,
        subscripts: &[Subscript],
        context: FunctionValidationContext<'_>,
        span: Span,
    ) -> Result<Option<FunctionValueSeed>, ToDaeError> {
        let dimensions = declared_dimensions(target, context, span)?;
        let written = written_indices(subscripts, &dimensions, context);
        match self.values.get_mut(target) {
            Some(ValueCoverage::Whole) => Ok(None),
            Some(ValueCoverage::Elements {
                covered, proven, ..
            }) => {
                match written {
                    Some(indices) => covered.extend(indices),
                    None => *proven = false,
                }
                Ok(None)
            }
            None => {
                let Some(indices) = written else {
                    return Err(ToDaeError::unsupported_flat(
                        "function element assignment",
                        format!(
                            "`{}` writes `{target}` through an index analysis cannot name before `{target}` has a value",
                            context.function.name
                        ),
                        span,
                    ));
                };
                let declaration = declared_value(target, context).ok_or_else(|| {
                    ToDaeError::unsupported_flat(
                        "function aggregate seed",
                        format!(
                            "`{}.{target}` is not a declared mutable function value",
                            context.function.name
                        ),
                        span,
                    )
                })?;
                let scalars = declared_scalar_count(&dimensions, target, context, span)?;
                self.branch_only.remove(target);
                self.values.insert(
                    target.clone(),
                    ValueCoverage::Elements {
                        covered: indices,
                        scalars,
                        proven: true,
                    },
                );
                Ok(Some(function_value_seed(
                    declaration,
                    dimensions,
                    context,
                    span,
                )?))
            }
        }
    }

    /// Reject a read of a value whose elements do not all have a definition.
    pub(super) fn require_readable(
        &self,
        expression: &Expression,
        context: FunctionValidationContext<'_>,
        span: Span,
    ) -> Result<(), ToDaeError> {
        DefinedValueReadChecker {
            definitions: self,
            context,
            span,
        }
        .visit_expression(expression)
    }

    fn require_reference_readable(
        &self,
        reference: &rumoca_core::Reference,
        subscripts: &[Subscript],
        context: FunctionValidationContext<'_>,
        span: Span,
    ) -> Result<(), ToDaeError> {
        let name = reference.var_name();
        if let Some(conditional) = self.branch_only.get(name) {
            return Err(ToDaeError::unsupported_flat(
                "function conditional",
                format!(
                    "`{}` reads `{name}`, which only some branches of the conditional at byte {} define",
                    context.function.name, conditional.span.start.0
                ),
                span,
            ));
        }
        let Some(coverage @ ValueCoverage::Elements { covered, .. }) = self.values.get(name) else {
            return Ok(());
        };
        if coverage.is_total() {
            return Ok(());
        }
        let dimensions = declared_dimensions(name, context, span)?;
        if !subscripts.is_empty()
            && let Some(read) = written_indices(subscripts, &dimensions, context)
            && read.iter().all(|index| covered.contains(index))
        {
            return Ok(());
        }
        Err(ToDaeError::unsupported_flat(
            "function element assignment",
            format!(
                "`{}` reads elements of `{name}` that do not all have a definition",
                context.function.name
            ),
            span,
        ))
    }

    /// Join the branch certificates of one conditional onto this one.
    ///
    /// A value survives the join when this state already defines it, or when
    /// every branch defines it and the conditional has an else branch. Any
    /// other value keeps no owner past the conditional, which the read check
    /// reports exactly where the algorithm would need it.
    pub(super) fn join_branches(
        &mut self,
        branches: &[Self],
        exhaustive: bool,
        ordered_targets: &[VarName],
        context: FunctionValidationContext<'_>,
        span: Span,
    ) -> Result<Vec<VarName>, ToDaeError> {
        let mut joined = Vec::with_capacity(ordered_targets.len());
        for target in ordered_targets {
            let defines_everywhere =
                exhaustive && branches.iter().all(|branch| branch.is_defined(target));
            if !self.is_defined(target) && !defines_everywhere {
                require_definable_branch_local(target, context, span)?;
                self.values.remove(target);
                self.branch_only.insert(
                    target.clone(),
                    BranchOnlyCoverage {
                        span,
                        guard: None,
                        coverage: None,
                    },
                );
                continue;
            }
            let prior = self.values.get(target).cloned();
            let mut coverage: Option<ValueCoverage> = None;
            for branch in branches {
                let branch_coverage = branch
                    .values
                    .get(target)
                    .cloned()
                    .or_else(|| prior.clone())
                    .expect("a joined value has a definition on every branch");
                coverage = Some(match coverage {
                    Some(found) => found.meet(&branch_coverage),
                    None => branch_coverage,
                });
            }
            if !exhaustive {
                let fallthrough = prior.expect("a joined value without an else branch is defined");
                coverage = Some(match coverage {
                    Some(found) => found.meet(&fallthrough),
                    None => fallthrough,
                });
            }
            let coverage = coverage.expect("a conditional has at least one branch");
            self.branch_only.remove(target);
            self.values.insert(target.clone(), coverage);
            joined.push(target.clone());
        }
        Ok(joined)
    }
}

fn condition_implies_guard(
    condition: &Expression,
    guard: &Expression,
    context: FunctionValidationContext<'_>,
    depth: usize,
) -> bool {
    if rumoca_core::expressions_semantically_equal(condition, guard) {
        return true;
    }
    if depth >= 16 {
        return false;
    }
    if let Some(expanded) = generated_boolean_value(condition, context) {
        return condition_implies_guard(expanded, guard, context, depth + 1);
    }
    if let Some(expanded) = generated_boolean_value(guard, context) {
        return condition_implies_guard(condition, expanded, context, depth + 1);
    }
    match condition {
        Expression::Binary {
            op: OpBinary::And,
            lhs,
            rhs,
            ..
        } => {
            condition_implies_guard(lhs, guard, context, depth + 1)
                || condition_implies_guard(rhs, guard, context, depth + 1)
                || matches!(
                    guard,
                    Expression::Binary {
                        op: OpBinary::And,
                        lhs: guard_lhs,
                        rhs: guard_rhs,
                        ..
                    } if condition_implies_guard(condition, guard_lhs, context, depth + 1)
                        && condition_implies_guard(condition, guard_rhs, context, depth + 1)
                )
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } if is_boolean_false(else_branch) => branches.iter().all(|(branch_condition, _)| {
            condition_implies_guard(branch_condition, guard, context, depth + 1)
        }),
        _ => false,
    }
}

fn is_boolean_false(expression: &Expression) -> bool {
    matches!(
        expression,
        Expression::Literal {
            value: Literal::Boolean(false),
            ..
        }
    )
}

fn generated_boolean_value<'expression>(
    expression: &Expression,
    context: FunctionValidationContext<'expression>,
) -> Option<&'expression Expression> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    context
        .generated_booleans
        .iter()
        .find(|definition| definition.target == *name.var_name())
        .map(|definition| &definition.value)
}

struct DefinedValueReadChecker<'scope> {
    definitions: &'scope FunctionDefinitions,
    context: FunctionValidationContext<'scope>,
    span: Span,
}

impl FallibleExpressionVisitor for DefinedValueReadChecker<'_> {
    type Error = ToDaeError;

    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[Subscript],
    ) -> Result<(), Self::Error> {
        for subscript in subscripts {
            self.visit_subscript(subscript)?;
        }
        self.definitions
            .require_reference_readable(name, subscripts, self.context, self.span)
    }

    fn visit_index(
        &mut self,
        base: &Expression,
        subscripts: &[Subscript],
    ) -> Result<(), Self::Error> {
        let mut selected = Vec::new();
        if let Some(name) = indexed_reference(base, &mut selected) {
            selected.extend_from_slice(subscripts);
            for subscript in &selected {
                self.visit_subscript(subscript)?;
            }
            return self.definitions.require_reference_readable(
                name,
                &selected,
                self.context,
                self.span,
            );
        }
        self.visit_expression(base)?;
        for subscript in subscripts {
            self.visit_subscript(subscript)?;
        }
        Ok(())
    }
}

fn indexed_reference<'expression>(
    expression: &'expression Expression,
    subscripts: &mut Vec<Subscript>,
) -> Option<&'expression rumoca_core::Reference> {
    match expression {
        Expression::VarRef {
            name,
            subscripts: direct,
            ..
        } => {
            subscripts.extend_from_slice(direct);
            Some(name)
        }
        Expression::Index {
            base,
            subscripts: selected,
            ..
        } => {
            let name = indexed_reference(base, subscripts)?;
            subscripts.extend_from_slice(selected);
            Some(name)
        }
        _ => None,
    }
}

/// An output the conditional cannot define on every path has no owner at all,
/// because MLS §12.4.4 leaves the unwritten path undefined.
fn require_definable_branch_local(
    target: &VarName,
    context: FunctionValidationContext<'_>,
    span: Span,
) -> Result<(), ToDaeError> {
    if !context
        .function
        .outputs
        .iter()
        .any(|output| output.name == target.as_str())
    {
        return Ok(());
    }
    Err(ToDaeError::unsupported_flat(
        "function conditional",
        format!(
            "`{}` leaves output `{target}` without a definition on some branch",
            context.function.name
        ),
        span,
    ))
}

fn declared_value<'scope>(
    target: &VarName,
    context: FunctionValidationContext<'scope>,
) -> Option<&'scope rumoca_core::FunctionParam> {
    context
        .function
        .outputs
        .iter()
        .chain(&context.function.locals)
        .find(|value| value.name == target.as_str())
}

fn declared_dimensions(
    target: &VarName,
    context: FunctionValidationContext<'_>,
    span: Span,
) -> Result<Vec<u32>, ToDaeError> {
    context.shapes.get(target).cloned().ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "function element assignment",
            format!(
                "`{}` writes elements of `{target}`, which has no proven shape",
                context.function.name
            ),
            span,
        )
    })
}

fn declared_scalar_count(
    dimensions: &[u32],
    target: &VarName,
    context: FunctionValidationContext<'_>,
    span: Span,
) -> Result<usize, ToDaeError> {
    if dimensions.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "function element assignment",
            format!(
                "`{}` subscripts scalar value `{target}`",
                context.function.name
            ),
            span,
        ));
    }
    dimensions
        .iter()
        .try_fold(1usize, |count, extent| {
            count.checked_mul(usize::try_from(*extent).ok()?)
        })
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "function element assignment",
                format!(
                    "`{}` declares `{target}` with more elements than analysis can count",
                    context.function.name
                ),
                span,
            )
        })
}

fn function_value_seed(
    declaration: &rumoca_core::FunctionParam,
    dimensions: Vec<u32>,
    context: FunctionValidationContext<'_>,
    span: Span,
) -> Result<FunctionValueSeed, ToDaeError> {
    if declaration.type_class != Some(rumoca_core::ClassType::Record) {
        let scalar =
            effective_function_scalar_type(context.flat, declaration).ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "function aggregate seed",
                    format!(
                        "`{}.{}` has no checked scalar seed type",
                        context.function.name, declaration.name
                    ),
                    span,
                )
            })?;
        return Ok(FunctionValueSeed::Scalar { dimensions, scalar });
    }
    let type_id = declaration.type_def_id.ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "function aggregate seed",
            format!(
                "`{}.{}` has no exact record type identity",
                context.function.name, declaration.name
            ),
            span,
        )
    })?;
    let constructor = record_constructor(declaration, context)?;
    let mut seen = HashSet::new();
    seen.insert(type_id);
    function_record_seed(declaration, dimensions, constructor, context, &mut seen)
}

fn function_record_seed(
    declaration: &rumoca_core::FunctionParam,
    dimensions: Vec<u32>,
    constructor: &rumoca_core::Function,
    context: FunctionValidationContext<'_>,
    seen: &mut HashSet<rumoca_core::DefId>,
) -> Result<FunctionValueSeed, ToDaeError> {
    let mut fields = Vec::with_capacity(constructor.inputs.len());
    for field in &constructor.inputs {
        let field_dimensions = field
            .dimensions()
            .iter()
            .map(|extent| {
                u32::try_from(*extent).map_err(|_| {
                    ToDaeError::unsupported_flat(
                        "function aggregate seed",
                        format!(
                            "record field `{}` has invalid extent `{extent}`",
                            field.name
                        ),
                        field.span,
                    )
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let seed = if field.type_class == Some(rumoca_core::ClassType::Record) {
            let type_id = field.type_def_id.ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "function aggregate seed",
                    format!("record field `{}` has no exact type identity", field.name),
                    field.span,
                )
            })?;
            if !seen.insert(type_id) {
                return Err(ToDaeError::unsupported_flat(
                    "function aggregate seed",
                    format!("record field `{}` has a recursive layout", field.name),
                    field.span,
                ));
            }
            let nested = record_constructor(field, context)?;
            let seed = function_record_seed(field, field_dimensions, nested, context, seen)?;
            seen.remove(&type_id);
            seed
        } else {
            function_value_seed(field, field_dimensions, context, field.span)?
        };
        fields.push((VarName::new(&field.name), seed));
    }
    Ok(FunctionValueSeed::Record {
        name: VarName::new(&declaration.type_name),
        dimensions,
        fields,
    })
}

/// Enumerate the exact element indices one subscripted write covers.
///
/// Returns `None` when a subscript denotes indices analysis cannot name, which
/// keeps the certificate honest instead of guessing a bound.
fn written_indices(
    subscripts: &[Subscript],
    dimensions: &[u32],
    context: FunctionValidationContext<'_>,
) -> Option<HashSet<Vec<i64>>> {
    if subscripts.len() > dimensions.len() {
        return None;
    }
    let mut per_dimension = Vec::with_capacity(dimensions.len());
    for (ordinal, extent) in dimensions.iter().enumerate() {
        let extent = i64::from(*extent);
        let selected = match subscripts.get(ordinal) {
            None | Some(Subscript::Colon { .. }) => (1..=extent).collect::<Vec<_>>(),
            Some(Subscript::Index { value, .. }) => vec![*value],
            Some(Subscript::Expr { expr, .. }) => static_subscript_indices(expr, context)?,
        };
        if selected.iter().any(|index| *index < 1 || *index > extent) {
            return None;
        }
        per_dimension.push(selected);
    }
    let mut covered: HashSet<Vec<i64>> = HashSet::from([Vec::new()]);
    for selected in per_dimension {
        let mut extended = HashSet::with_capacity(covered.len() * selected.len());
        for prefix in &covered {
            for index in &selected {
                let mut tuple = prefix.clone();
                tuple.push(*index);
                extended.insert(tuple);
            }
        }
        covered = extended;
    }
    Some(covered)
}

fn static_subscript_indices(
    expression: &Expression,
    context: FunctionValidationContext<'_>,
) -> Option<Vec<i64>> {
    if let Expression::Range {
        start, step, end, ..
    } = expression
    {
        let lower = settled_subscript_integer(start, context)?;
        let upper = settled_subscript_integer(end, context)?;
        let stride = match step {
            Some(step) => settled_subscript_integer(step, context)?,
            None => 1,
        };
        if stride <= 0 {
            return None;
        }
        let mut indices = Vec::new();
        let mut index = lower;
        while index <= upper {
            indices.push(index);
            index = index.checked_add(stride)?;
        }
        return Some(indices);
    }
    settled_subscript_integer(expression, context).map(|index| vec![index])
}

fn settled_subscript_integer(
    expression: &Expression,
    context: FunctionValidationContext<'_>,
) -> Option<i64> {
    static_shape_integer_expression(expression, context.static_integers, context.shapes)
        .ok()
        .flatten()
}
