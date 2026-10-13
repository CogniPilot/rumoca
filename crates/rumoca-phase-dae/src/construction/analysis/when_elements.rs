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
    pub(super) fn from_reference(
        flat: &flat::Model,
        name: &rumoca_core::Reference,
        expression: &Expression,
    ) -> Self {
        if let Expression::VarRef { subscripts, .. } = expression
            && !subscripts.is_empty()
            && let Some(instance) = name.instance_id()
            && let Some(indices) = literal_indices(subscripts)
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

pub(super) fn literal_indices(subscripts: &[Subscript]) -> Option<Box<[i64]>> {
    subscripts
        .iter()
        .map(|subscript| match subscript {
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
) -> Result<(), ToDaeError> {
    let mut expected = None;
    for branch in chain.branches() {
        let targets = branch_targets(flat, &branch.equations)?;
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

/// MLS §8.3.5.4 applies to `z` and `m`, including partial array definitions.
pub(super) fn validate_model_owners(flat: &flat::Model) -> Result<(), ToDaeError> {
    let mut owners: HashMap<InstanceId, Vec<(Option<Box<[i64]>>, usize)>> = HashMap::new();
    for (owner, chain) in flat.when_chains.iter().enumerate() {
        for branch in chain.branches() {
            claim_equation_targets(flat, &branch.equations, owner, &mut owners)?;
        }
    }
    Ok(())
}

fn claim_equation_targets(
    flat: &flat::Model,
    equations: &[flat::WhenEquation],
    owner: usize,
    owners: &mut HashMap<InstanceId, Vec<(Option<Box<[i64]>>, usize)>>,
) -> Result<(), ToDaeError> {
    for equation in equations {
        let target = match equation {
            flat::WhenEquation::Assign { target, .. } => flat
                .variables
                .get(target)
                .map(|variable| (variable.instance_id, None)),
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
                for (_, equations) in branches {
                    claim_equation_targets(flat, equations, owner, owners)?;
                }
                if let Some(equations) = else_branch {
                    claim_equation_targets(flat, equations, owner, owners)?;
                }
                None
            }
            _ => None,
        };
        if let Some((instance, selection)) = target {
            let previous = owners.entry(instance).or_default();
            if previous.iter().any(|(other, source)| {
                *source != owner && (selection.is_none() || other.is_none() || other == &selection)
            }) {
                return Err(ToDaeError::discrete_solved_form_violation(
                    "independent when-equations have overlapping coordinate definitions (MLS §8.3.5.4)",
                    equation.span(),
                ));
            }
            if !previous.contains(&(selection.clone(), owner)) {
                previous.push((selection, owner));
            }
        }
    }
    Ok(())
}

fn branch_targets(
    flat: &flat::Model,
    equations: &[flat::WhenEquation],
) -> Result<HashSet<DiscreteTargetKey>, ToDaeError> {
    let mut targets = HashSet::new();
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
            } => {
                let mut alternatives = branches
                    .iter()
                    .map(|(_, equations)| equations.as_slice())
                    .chain(else_branch.iter().map(Vec::as_slice));
                let first = alternatives
                    .next()
                    .map(|equations| branch_targets(flat, equations))
                    .transpose()?
                    .unwrap_or_default();
                for equations in alternatives {
                    if branch_targets(flat, equations)? != first {
                        return Err(ToDaeError::discrete_solved_form_violation(
                            "if-equation branches must define the same selected coordinates (MLS §8.3.5)",
                            *span,
                        ));
                    }
                }
                first
            }
            _ => HashSet::new(),
        };
        for key in new {
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

pub(in crate::construction) fn selected_target(
    flat: &flat::Model,
    target: &rumoca_core::Reference,
    subscripts: &[Subscript],
    span: Span,
) -> Result<SelectedWhenTarget, ToDaeError> {
    let invalid = || {
        ToDaeError::unsupported_flat(
            "when element assignment",
            "requires a resolved, in-bounds full-rank literal coordinate",
            span,
        )
    };
    let instance = target.instance_id().ok_or_else(invalid)?;
    let variable = flat
        .variables
        .get(target.var_name())
        .filter(|variable| variable.instance_id == instance)
        .ok_or_else(invalid)?;
    let indices = subscripts
        .iter()
        .map(|subscript| match subscript {
            Subscript::Expr { expr, .. } => {
                rumoca_eval_flat::flat_int::try_eval_flat_expr_i64(expr, flat, 0)
            }
            _ => None,
        })
        .collect::<Option<Box<[i64]>>>()
        .ok_or_else(invalid)?;
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
