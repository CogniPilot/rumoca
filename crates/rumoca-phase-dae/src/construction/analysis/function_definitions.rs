use super::function_ranges::static_integer_expression;
use super::*;

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
    branch_only: HashMap<VarName, Span>,
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

/// Shape and scalar type of the generated aggregate seed one element write needs.
pub(in crate::construction) struct FunctionValueSeed {
    pub(in crate::construction) dimensions: Vec<u32>,
    pub(in crate::construction) scalar: dae::ScalarType,
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

    pub(super) fn is_total(&self, name: &VarName) -> bool {
        self.values.get(name).is_some_and(ValueCoverage::is_total)
    }

    pub(super) fn define_whole(&mut self, name: &VarName) {
        self.branch_only.remove(name);
        self.values.insert(name.clone(), ValueCoverage::Whole);
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
                let scalar = seed_scalar_type(target, context, span)?;
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
                Ok(Some(FunctionValueSeed { dimensions, scalar }))
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
        let mut references = Vec::new();
        expression.collect_var_refs(&mut references);
        for reference in references {
            if let Some(conditional) = self.branch_only.get(&reference) {
                return Err(ToDaeError::unsupported_flat(
                    "function conditional",
                    format!(
                        "`{}` reads `{reference}`, which only some branches of the conditional at byte {} define",
                        context.function.name, conditional.start.0
                    ),
                    span,
                ));
            }
            if self
                .values
                .get(&reference)
                .is_some_and(|coverage| !coverage.is_total())
            {
                return Err(ToDaeError::unsupported_flat(
                    "function element assignment",
                    format!(
                        "`{}` reads `{reference}` before every element of `{reference}` has a definition",
                        context.function.name
                    ),
                    span,
                ));
            }
        }
        Ok(())
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
                self.branch_only.insert(target.clone(), span);
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

fn seed_scalar_type(
    target: &VarName,
    context: FunctionValidationContext<'_>,
    span: Span,
) -> Result<dae::ScalarType, ToDaeError> {
    let declaration = declared_value(target, context).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "function element assignment",
            format!(
                "`{}.{target}` is not a declared mutable function value",
                context.function.name
            ),
            span,
        )
    })?;
    match effective_function_scalar_type(context.flat, declaration) {
        // A numeric or Boolean seed is a total, finite aggregate whose every
        // element the totality certificate proves the algorithm overwrites.
        Some(
            scalar @ (dae::ScalarType::Real | dae::ScalarType::Integer | dae::ScalarType::Boolean),
        ) => Ok(scalar),
        _ => Err(ToDaeError::unsupported_flat(
            "function element assignment",
            format!(
                "`{}` writes elements of `{target}`, whose value type has no checked aggregate seed",
                context.function.name
            ),
            span,
        )),
    }
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
        let lower = static_integer_expression(start, context.static_integers)?;
        let upper = static_integer_expression(end, context.static_integers)?;
        let stride = match step {
            Some(step) => static_integer_expression(step, context.static_integers)?,
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
    static_integer_expression(expression, context.static_integers).map(|index| vec![index])
}
