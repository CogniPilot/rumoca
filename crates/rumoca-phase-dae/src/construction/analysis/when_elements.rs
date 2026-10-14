use super::*;

/// Source ownership is a declaration or an exact array coordinate of it.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(in crate::construction) enum DiscreteTargetKey {
    Whole(InstanceId),
    Element {
        instance: InstanceId,
        indices: Box<[i64]>,
    },
}

impl DiscreteTargetKey {
    pub(super) fn instance(&self) -> InstanceId {
        match self {
            Self::Whole(instance) | Self::Element { instance, .. } => *instance,
        }
    }

    pub(super) fn from_reference(
        flat: &flat::Model,
        name: &rumoca_core::Reference,
        expression: &Expression,
    ) -> Self {
        if let Expression::VarRef { subscripts, .. } | Expression::Index { subscripts, .. } =
            expression
            && !subscripts.is_empty()
            && let Some(instance) = name.instance_id()
            && let Some(indices) = literal_indices(subscripts)
            && flat.variables.get(name.var_name()).is_some_and(|variable| {
                indices.len() == variable.dims.len()
                    && indices
                        .iter()
                        .zip(&variable.dims)
                        .all(|(&index, &extent)| index > 0 && index <= extent)
            })
        {
            return Self::Element { instance, indices };
        }
        Self::Whole(flat.variables[name.var_name()].instance_id)
    }
}

#[derive(Clone, Debug)]
pub(in crate::construction) struct SelectedWhenTarget {
    pub(in crate::construction) name: VarName,
    pub(in crate::construction) instance: InstanceId,
    pub(in crate::construction) indices: Box<[i64]>,
    pub(in crate::construction) span: Span,
}

impl SelectedWhenTarget {
    pub(in crate::construction) fn key(&self) -> DiscreteTargetKey {
        DiscreteTargetKey::Element {
            instance: self.instance,
            indices: self.indices.clone(),
        }
    }
}

pub(super) fn parameter_indices(
    flat: &flat::Model,
    subscripts: &[Subscript],
) -> Option<Box<[i64]>> {
    subscripts
        .iter()
        .map(|subscript| match subscript {
            Subscript::Index { value, .. } => Some(*value),
            Subscript::Expr { expr, .. } => {
                rumoca_eval_flat::flat_int::try_eval_flat_expr_i64(expr, flat, 0)
            }
            _ => None,
        })
        .collect()
}

fn literal_indices(subscripts: &[Subscript]) -> Option<Box<[i64]>> {
    subscripts
        .iter()
        .map(|subscript| match subscript {
            Subscript::Index { value, .. } => Some(*value),
            Subscript::Expr { expr, .. } => match expr.as_ref() {
                Expression::Literal {
                    value: Literal::Integer(value),
                    ..
                } => Some(*value),
                _ => None,
            },
            _ => None,
        })
        .collect()
}

pub(super) fn validate_chain(
    flat: &flat::Model,
    chain: &flat::WhenChain,
    calls: &HashMap<WhenFunctionCallKey, ModelEventFunctionCallPlan>,
) -> Result<(), ToDaeError> {
    let mut expected = None;
    for branch in chain.branches() {
        let targets = branch_targets(flat, &branch.equations, calls)?;
        if expected
            .as_ref()
            .is_some_and(|expected| expected != &targets)
        {
            return Err(ToDaeError::discrete_solved_form_violation(
                "when/elsewhen branches must define the same selected coordinates (MLS §8.3.5)",
                branch.span,
            ));
        }
        expected = Some(targets);
    }
    Ok(())
}

#[derive(Default)]
struct CoordinateOwners {
    whole: Option<usize>,
    elements: HashMap<Box<[i64]>, usize>,
}

impl CoordinateOwners {
    fn claim(
        &mut self,
        selection: Option<Box<[i64]>>,
        owner: usize,
        span: Span,
        name: &VarName,
    ) -> Result<(), ToDaeError> {
        let conflict = self.whole.is_some_and(|previous| previous != owner)
            || match &selection {
                Some(indices) => self
                    .elements
                    .get(indices)
                    .is_some_and(|&previous| previous != owner),
                None => self.elements.values().any(|&previous| previous != owner),
            };
        if conflict {
            return Err(ToDaeError::discrete_solved_form_violation(
                format!(
                    "independent when-equations have overlapping coordinate definitions of `{name}` (MLS §8.3.5.4)"
                ),
                span,
            ));
        }
        match selection {
            Some(indices) => {
                self.elements.insert(indices, owner);
            }
            None => {
                self.whole = Some(owner);
            }
        }
        Ok(())
    }
}

/// MLS §8.3.5.4 applies to `z` and `m`, including partial array definitions.
pub(super) fn validate_model_owners(
    flat: &flat::Model,
    calls: &HashMap<WhenFunctionCallKey, ModelEventFunctionCallPlan>,
) -> Result<(), ToDaeError> {
    let mut owners = HashMap::new();
    let names = flat
        .variables
        .values()
        .map(|variable| (variable.instance_id, &variable.name))
        .collect::<HashMap<_, _>>();
    for (owner, chain) in flat.when_chains.iter().enumerate() {
        for branch in chain.branches() {
            claim_equation_targets(flat, &branch.equations, owner, &mut owners, calls, &names)?;
        }
    }
    Ok(())
}

fn claim_equation_targets(
    flat: &flat::Model,
    equations: &[flat::WhenEquation],
    owner: usize,
    owners: &mut HashMap<InstanceId, CoordinateOwners>,
    calls: &HashMap<WhenFunctionCallKey, ModelEventFunctionCallPlan>,
    names: &HashMap<InstanceId, &VarName>,
) -> Result<(), ToDaeError> {
    for equation in equations {
        for instance in whole_target_instances(flat, equation, calls)? {
            owners.entry(instance).or_default().claim(
                None,
                owner,
                equation.span(),
                names[&instance],
            )?;
        }
        let target = match equation {
            flat::WhenEquation::AssignElement {
                target,
                subscripts,
                span,
                ..
            } => {
                let target = selected_target(flat, target, subscripts, *span)?;
                Some((target.instance, Some(target.indices)))
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for equations in branches
                    .iter()
                    .map(|(_, equations)| equations.as_slice())
                    .chain(else_branch.iter().map(Vec::as_slice))
                {
                    claim_equation_targets(flat, equations, owner, owners, calls, names)?;
                }
                None
            }
            _ => None,
        };
        if let Some((instance, selection)) = target {
            owners.entry(instance).or_default().claim(
                selection,
                owner,
                equation.span(),
                names[&instance],
            )?;
        }
    }
    Ok(())
}

fn branch_targets(
    flat: &flat::Model,
    equations: &[flat::WhenEquation],
    calls: &HashMap<WhenFunctionCallKey, ModelEventFunctionCallPlan>,
) -> Result<HashSet<DiscreteTargetKey>, ToDaeError> {
    let mut targets = HashSet::new();
    let mut whole = HashSet::new();
    collect_whole_targets(flat, equations, &mut whole, calls)?;
    for equation in equations {
        let new = match equation {
            flat::WhenEquation::AssignElement {
                target,
                subscripts,
                span,
                ..
            } => HashSet::from([selected_target(flat, target, subscripts, *span)?.key()]),
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                span,
                ..
            } => conditional_targets(flat, branches, else_branch.as_deref(), *span, calls)?,
            _ => HashSet::new(),
        };
        for key in new {
            if let DiscreteTargetKey::Element { instance, .. } = &key
                && whole.contains(instance)
            {
                return Err(ToDaeError::discrete_solved_form_violation(
                    "one when branch overlaps a whole declaration and a selected coordinate",
                    equation.span(),
                ));
            }
            if !targets.insert(key) {
                return Err(ToDaeError::discrete_solved_form_violation(
                    "one when branch defines the same selected coordinate more than once",
                    equation.span(),
                ));
            }
        }
    }
    Ok(targets)
}

fn collect_whole_targets(
    flat: &flat::Model,
    equations: &[flat::WhenEquation],
    whole: &mut HashSet<InstanceId>,
    calls: &HashMap<WhenFunctionCallKey, ModelEventFunctionCallPlan>,
) -> Result<(), ToDaeError> {
    for equation in equations {
        whole.extend(whole_target_instances(flat, equation, calls)?);
        collect_nested_whole_targets(flat, equation, whole, calls)?;
    }
    Ok(())
}

fn whole_target_instances(
    flat: &flat::Model,
    equation: &flat::WhenEquation,
    calls: &HashMap<WhenFunctionCallKey, ModelEventFunctionCallPlan>,
) -> Result<Vec<InstanceId>, ToDaeError> {
    let names = match equation {
        flat::WhenEquation::Assign { target, .. } => {
            return Ok(flat
                .variables
                .get(target)
                .map(|variable| vec![variable.instance_id])
                .unwrap_or_default());
        }
        flat::WhenEquation::FunctionCallOutputs { outputs, span, .. } => {
            let invalid = || {
                ToDaeError::unsupported_flat(
                    "when function-call receivers",
                    "requires an issued exact receiver plan",
                    *span,
                )
            };
            let key = when_function_call_key(flat, outputs, *span)?;
            calls
                .get(&key)
                .ok_or_else(invalid)?
                .outputs
                .iter()
                .flatten()
                .flat_map(|output| output.receivers().into_iter().map(|(target, _)| target))
                .collect()
        }
        _ => Vec::new(),
    };
    Ok(names
        .into_iter()
        .map(|name| flat.variables[name].instance_id)
        .collect())
}

fn collect_nested_whole_targets(
    flat: &flat::Model,
    equation: &flat::WhenEquation,
    whole: &mut HashSet<InstanceId>,
    calls: &HashMap<WhenFunctionCallKey, ModelEventFunctionCallPlan>,
) -> Result<(), ToDaeError> {
    if let flat::WhenEquation::Conditional {
        branches,
        else_branch,
        ..
    } = equation
    {
        for nested in branches
            .iter()
            .map(|(_, nested)| nested.as_slice())
            .chain(else_branch.as_deref())
        {
            collect_whole_targets(flat, nested, whole, calls)?;
        }
    }
    Ok(())
}

pub(in crate::construction) fn selected_target(
    flat: &flat::Model,
    target: &rumoca_core::Reference,
    subscripts: &[Subscript],
    span: Span,
) -> Result<SelectedWhenTarget, ToDaeError> {
    let invalid = || {
        ToDaeError::unsupported_flat(
            "when element assignment",
            "requires a resolved, in-bounds full-rank parameter coordinate",
            span,
        )
    };
    let instance = target.instance_id().ok_or_else(invalid)?;
    let variable = flat
        .variables
        .get(target.var_name())
        .filter(|variable| variable.instance_id == instance)
        .ok_or_else(invalid)?;
    let indices = parameter_indices(flat, subscripts).ok_or_else(invalid)?;
    if indices.len() != variable.dims.len()
        || indices
            .iter()
            .zip(&variable.dims)
            .any(|(&index, &extent)| index < 1 || index > extent)
    {
        return Err(invalid());
    }
    Ok(SelectedWhenTarget {
        name: variable.name.clone(),
        instance,
        indices,
        span,
    })
}

fn conditional_targets(
    flat: &flat::Model,
    branches: &[(Expression, Vec<flat::WhenEquation>)],
    else_branch: Option<&[flat::WhenEquation]>,
    span: Span,
    calls: &HashMap<WhenFunctionCallKey, ModelEventFunctionCallPlan>,
) -> Result<HashSet<DiscreteTargetKey>, ToDaeError> {
    let mut alternatives = branches
        .iter()
        .map(|(_, equations)| equations.as_slice())
        .chain(else_branch);
    let first = alternatives
        .next()
        .map(|equations| branch_targets(flat, equations, calls))
        .transpose()?
        .unwrap_or_default();
    for equations in alternatives {
        if branch_targets(flat, equations, calls)? != first {
            return Err(ToDaeError::discrete_solved_form_violation(
                "if-equation branches must define the same selected coordinates (MLS §8.3.5)",
                span,
            ));
        }
    }
    Ok(first)
}
