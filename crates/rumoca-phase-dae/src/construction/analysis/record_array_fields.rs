#[cfg(test)]
mod tests;

use super::*;

#[derive(Clone, PartialEq)]
pub(in crate::construction) enum RecordArrayFieldPlan {
    MaterializedCoordinate {
        coordinate: rumoca_core::InstanceId,
        target: rumoca_core::DefId,
        value_type: rumoca_core::TypeId,
    },
    Projection {
        coordinates: Box<[rumoca_core::InstanceId]>,
        target: rumoca_core::DefId,
        value_type: rumoca_core::TypeId,
        shape: Box<[i64]>,
        subscripts: Box<[Subscript]>,
    },
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum RecordArrayFieldPlanKey {
    Materialized {
        occurrence: Span,
        scope: rumoca_core::InstanceId,
        root: rumoca_core::DefId,
        target: rumoca_core::DefId,
        declarations: Box<[rumoca_core::DefId]>,
        indices: Box<[i64]>,
        index_ranks: Box<[u32]>,
    },
    Projection {
        occurrence: Span,
        scope: rumoca_core::InstanceId,
        root: rumoca_core::DefId,
        target: rumoca_core::DefId,
        declarations: Box<[rumoca_core::DefId]>,
        rank: usize,
    },
}

pub(in crate::construction) struct RecordArrayFieldPlans {
    by_occurrence: HashMap<RecordArrayFieldPlanKey, RecordArrayFieldPlan>,
}

struct CoordinateCandidates<'flat> {
    by_declarations: HashMap<Box<[rumoca_core::DefId]>, Vec<&'flat flat::Variable>>,
}

impl<'flat> CoordinateCandidates<'flat> {
    fn new(flat: &'flat flat::Model) -> Self {
        let mut by_declarations = HashMap::<_, Vec<_>>::new();
        for variable in flat.variables.values() {
            let Some(reference) = variable.component_ref.as_ref() else {
                continue;
            };
            by_declarations
                .entry(reference_declarations(reference))
                .or_default()
                .push(variable);
        }
        Self { by_declarations }
    }

    fn get(&self, declarations: &[rumoca_core::DefId]) -> &[&'flat flat::Variable] {
        self.by_declarations
            .get(declarations)
            .map_or(&[], Vec::as_slice)
    }
}

impl RecordArrayFieldPlans {
    pub(in crate::construction) fn get(
        &self,
        expression: &Expression,
    ) -> Option<&RecordArrayFieldPlan> {
        self.by_occurrence.get(&field_access_key(expression)?)
    }
}

pub(super) fn analyze_record_array_fields<'expression>(
    flat: &flat::Model,
    expressions: impl IntoIterator<Item = &'expression Expression>,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<RecordArrayFieldPlans, ToDaeError> {
    let candidates = CoordinateCandidates::new(flat);
    let mut plans = HashMap::new();
    for expression in expressions {
        collect_plans(flat, &candidates, roles, expression, &mut plans)?;
    }
    Ok(RecordArrayFieldPlans {
        by_occurrence: plans,
    })
}

fn collect_plans(
    flat: &flat::Model,
    candidates: &CoordinateCandidates<'_>,
    roles: &HashMap<VarName, PlannedRole>,
    expression: &Expression,
    plans: &mut HashMap<RecordArrayFieldPlanKey, RecordArrayFieldPlan>,
) -> Result<(), ToDaeError> {
    if let Some((key, plan)) = plan_field_access(flat, candidates, roles, expression)? {
        if let Some(previous) = plans.insert(key, plan.clone())
            && previous != plan
        {
            return Err(ToDaeError::unsupported_flat(
                "record-array member slice",
                "one semantic occurrence produced incompatible projection certificates",
                expression_span(expression)?,
            ));
        }
        return Ok(());
    }
    for child in expression_children(expression) {
        collect_plans(flat, candidates, roles, child, plans)?;
    }
    Ok(())
}

fn plan_field_access(
    flat: &flat::Model,
    candidates: &CoordinateCandidates<'_>,
    roles: &HashMap<VarName, PlannedRole>,
    expression: &Expression,
) -> Result<Option<(RecordArrayFieldPlanKey, RecordArrayFieldPlan)>, ToDaeError> {
    let Expression::FieldAccess { span, .. } = expression else {
        return Ok(None);
    };
    require_span(*span, "record-array member slice")?;
    if let Some(plan) = plan_materialized_coordinate(flat, candidates, roles, expression, *span)? {
        return Ok(Some(plan));
    }
    plan_projection(flat, candidates, roles, expression, *span)
}

fn plan_materialized_coordinate(
    flat: &flat::Model,
    candidates: &CoordinateCandidates<'_>,
    roles: &HashMap<VarName, PlannedRole>,
    expression: &Expression,
    span: Span,
) -> Result<Option<(RecordArrayFieldPlanKey, RecordArrayFieldPlan)>, ToDaeError> {
    let Some(reference) = concrete_component_reference(expression) else {
        return Ok(None);
    };
    let semantic = expression_reference(expression)
        .ok_or_else(|| ToDaeError::unresolved_reference(reference.to_var_name().as_str(), span))?;
    let scope = semantic
        .instance_id()
        .ok_or_else(|| ToDaeError::unresolved_reference(semantic.as_str(), span))?;
    let root = reference.root_def_id();
    let target = reference.target_def_id();
    let exact_indices = reference_indices(&reference).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "record-array member slice",
            "materialized coordinate identity contains a non-literal index",
            span,
        )
    })?;
    let Some(variable) = materialized_variable(
        flat,
        candidates.get(&reference_declarations(&reference)),
        scope,
        root,
        target,
        &exact_indices,
        span,
    )?
    else {
        return Ok(None);
    };
    require_runtime_coordinate(variable, roles, span)?;
    Ok(Some((
        RecordArrayFieldPlanKey::Materialized {
            occurrence: span,
            scope,
            root,
            target,
            declarations: reference_declarations(&reference),
            indices: exact_indices.values,
            index_ranks: exact_indices.ranks,
        },
        RecordArrayFieldPlan::MaterializedCoordinate {
            coordinate: variable.instance_id,
            target,
            value_type: variable.type_id,
        },
    )))
}

struct ProjectedElement {
    indices: Vec<i64>,
    coordinate: rumoca_core::InstanceId,
    value_type: rumoca_core::TypeId,
    shape: Vec<i64>,
}

fn plan_projection(
    flat: &flat::Model,
    candidates: &CoordinateCandidates<'_>,
    roles: &HashMap<VarName, PlannedRole>,
    expression: &Expression,
    span: Span,
) -> Result<Option<(RecordArrayFieldPlanKey, RecordArrayFieldPlan)>, ToDaeError> {
    let Some(pattern) = projection_pattern(expression) else {
        return Ok(None);
    };
    let root = pattern.reference.root_def_id();
    let target = pattern.reference.target_def_id();
    let rank = pattern.subscripts.len();
    let declarations = reference_declarations(&pattern.reference);
    let mut elements = candidates
        .get(&declarations)
        .iter()
        .filter_map(|&variable| {
            match projected_variable_indices(flat, pattern.base, variable, rank, span) {
                Ok(Some(indices)) => {
                    Some(require_runtime_coordinate(variable, roles, span).map(|()| {
                        ProjectedElement {
                            indices,
                            coordinate: variable.instance_id,
                            value_type: variable.type_id,
                            shape: variable.dims.clone(),
                        }
                    }))
                }
                Ok(None) => None,
                Err(error) => Some(Err(error)),
            }
        })
        .collect::<Result<Vec<_>, ToDaeError>>()?;
    if elements.is_empty() {
        return Ok(None);
    }
    elements.sort_by(|lhs, rhs| lhs.indices.cmp(&rhs.indices));
    validate_rectangular_elements(&elements, rank, span)?;
    let value_type = elements[0].value_type;
    let shape = elements[0].shape.clone().into_boxed_slice();
    Ok(Some((
        RecordArrayFieldPlanKey::Projection {
            occurrence: span,
            scope: pattern
                .base
                .instance_id()
                .ok_or_else(|| ToDaeError::unresolved_reference(pattern.base.as_str(), span))?,
            root,
            target,
            declarations,
            rank,
        },
        RecordArrayFieldPlan::Projection {
            coordinates: elements
                .into_iter()
                .map(|element| element.coordinate)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            target,
            value_type,
            shape,
            subscripts: pattern.subscripts.to_vec().into_boxed_slice(),
        },
    )))
}

struct ProjectionPattern<'expression> {
    base: &'expression rumoca_core::Reference,
    reference: rumoca_core::ComponentReference,
    subscripts: &'expression [Subscript],
}

fn projection_pattern(expression: &Expression) -> Option<ProjectionPattern<'_>> {
    let mut members = Vec::new();
    let mut current = expression;
    loop {
        let Expression::FieldAccess {
            base,
            field,
            field_def_id,
            span,
        } = current
        else {
            return None;
        };
        members.push(rumoca_core::ComponentRefPart {
            ident: field.clone(),
            span: *span,
            subs: Vec::new(),
            def_id: *field_def_id,
        });
        match base.as_ref() {
            Expression::FieldAccess { .. } => current = base,
            Expression::Index {
                base, subscripts, ..
            } => {
                let Expression::VarRef {
                    name,
                    subscripts: base_subscripts,
                    ..
                } = base.as_ref()
                else {
                    return None;
                };
                if !base_subscripts.is_empty() || subscripts.is_empty() {
                    return None;
                }
                let mut reference = name.component_ref()?.clone();
                members.reverse();
                let mut parts = reference.parts().to_vec();
                parts.extend(members);
                reference = rumoca_core::ComponentReference::construct(
                    reference.local(),
                    reference.span(),
                    parts,
                )
                .ok()?;
                return Some(ProjectionPattern {
                    base: name,
                    reference,
                    subscripts,
                });
            }
            _ => return None,
        }
    }
}

fn field_access_key(expression: &Expression) -> Option<RecordArrayFieldPlanKey> {
    let Expression::FieldAccess { span, .. } = expression else {
        return None;
    };
    if let Some(reference) = concrete_component_reference(expression) {
        let exact_indices = reference_indices(&reference)?;
        return Some(RecordArrayFieldPlanKey::Materialized {
            occurrence: *span,
            scope: expression_reference(expression)?.instance_id()?,
            root: reference.root_def_id(),
            target: reference.target_def_id(),
            declarations: reference_declarations(&reference),
            indices: exact_indices.values,
            index_ranks: exact_indices.ranks,
        });
    }
    let pattern = projection_pattern(expression)?;
    Some(RecordArrayFieldPlanKey::Projection {
        occurrence: *span,
        scope: pattern.base.instance_id()?,
        root: pattern.reference.root_def_id(),
        target: pattern.reference.target_def_id(),
        declarations: reference_declarations(&pattern.reference),
        rank: pattern.subscripts.len(),
    })
}

fn concrete_component_reference(
    expression: &Expression,
) -> Option<rumoca_core::ComponentReference> {
    match expression {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            let mut reference = name.component_ref()?.clone();
            append_literal_subscripts(&mut reference, subscripts)?;
            Some(reference)
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            let mut reference = concrete_component_reference(base)?;
            append_literal_subscripts(&mut reference, subscripts)?;
            Some(reference)
        }
        Expression::FieldAccess {
            base,
            field,
            field_def_id,
            span,
        } => {
            let reference = concrete_component_reference(base)?;
            let mut parts = reference.parts().to_vec();
            parts.push(rumoca_core::ComponentRefPart {
                ident: field.clone(),
                span: *span,
                subs: Vec::new(),
                def_id: *field_def_id,
            });
            rumoca_core::ComponentReference::construct(reference.local(), reference.span(), parts)
                .ok()
        }
        _ => None,
    }
}

fn append_literal_subscripts(
    reference: &mut rumoca_core::ComponentReference,
    subscripts: &[Subscript],
) -> Option<()> {
    let mut parts = reference.parts().to_vec();
    let part = parts.last_mut()?;
    for subscript in subscripts {
        let (value, span) = match subscript {
            Subscript::Index { value, span } => (*value, *span),
            Subscript::Expr { expr, span } => match expr.as_ref() {
                Expression::Literal {
                    value: Literal::Integer(value),
                    ..
                } => (*value, *span),
                _ => return None,
            },
            Subscript::Colon { .. } => return None,
        };
        part.subs.push(Subscript::Index { value, span });
    }
    *reference = reference
        .with_replaced_parts(parts)
        .expect("literal subscripts preserve every exact part identity");
    Some(())
}

fn expression_reference(expression: &Expression) -> Option<&rumoca_core::Reference> {
    match expression {
        Expression::VarRef { name, .. } => Some(name),
        Expression::Index { base, .. } | Expression::FieldAccess { base, .. } => {
            expression_reference(base)
        }
        _ => None,
    }
}

struct ExactReferenceIndices {
    values: Box<[i64]>,
    ranks: Box<[u32]>,
}

fn materialized_variable<'flat>(
    flat: &'flat flat::Model,
    candidates: &[&'flat flat::Variable],
    scope: rumoca_core::InstanceId,
    root: rumoca_core::DefId,
    target: rumoca_core::DefId,
    exact_indices: &ExactReferenceIndices,
    span: Span,
) -> Result<Option<&'flat flat::Variable>, ToDaeError> {
    let mut matched = None;
    for &variable in candidates {
        let Some(reference) = variable.component_ref.as_ref() else {
            continue;
        };
        if reference.target_def_id() != target {
            continue;
        }
        let Some(candidate_indices) = reference_indices(reference) else {
            continue;
        };
        if candidate_indices.values != exact_indices.values
            || candidate_indices.ranks != exact_indices.ranks
        {
            continue;
        }
        let Some(owner_indices) =
            projection_owner_indices(flat, scope, root, variable.instance_id, span)?
        else {
            continue;
        };
        if !exact_indices.values.starts_with(&owner_indices) {
            continue;
        }
        if matched.replace(variable).is_some() {
            return Err(ToDaeError::unsupported_flat(
                "record-array member slice",
                "exact materialized identity selects more than one Flat coordinate",
                span,
            ));
        }
    }
    Ok(matched)
}

fn reference_declarations(
    reference: &rumoca_core::ComponentReference,
) -> Box<[rumoca_core::DefId]> {
    reference
        .parts()
        .iter()
        .map(|part| part.def_id)
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn require_runtime_coordinate(
    variable: &flat::Variable,
    roles: &HashMap<VarName, PlannedRole>,
    span: Span,
) -> Result<(), ToDaeError> {
    match roles.get(&variable.name) {
        Some(
            PlannedRole::Parameter
            | PlannedRole::Constant
            | PlannedRole::Input
            | PlannedRole::State
            | PlannedRole::Algebraic
            | PlannedRole::Output
            | PlannedRole::DiscreteReal
            | PlannedRole::DiscreteValue,
        ) => Ok(()),
        Some(PlannedRole::Clock | PlannedRole::EnumerationLiteral | PlannedRole::Aggregate)
        | None => Err(ToDaeError::unsupported_flat(
            "record-array member slice",
            format!(
                "exact coordinate instance {:?} has no runtime DAE coordinate",
                variable.instance_id
            ),
            span,
        )),
    }
}

fn reference_indices(reference: &rumoca_core::ComponentReference) -> Option<ExactReferenceIndices> {
    let mut indices = Vec::new();
    let mut ranks = Vec::with_capacity(reference.parts().len());
    for part in reference.parts() {
        ranks.push(u32::try_from(part.subs.len()).ok()?);
        indices.extend(
            part.subs
                .iter()
                .map(|subscript| match subscript {
                    Subscript::Index { value, .. } => Some(*value),
                    Subscript::Expr { .. } | Subscript::Colon { .. } => None,
                })
                .collect::<Option<Vec<_>>>()?,
        );
    }
    Some(ExactReferenceIndices {
        values: indices.into_boxed_slice(),
        ranks: ranks.into_boxed_slice(),
    })
}

fn projected_variable_indices(
    flat: &flat::Model,
    base: &rumoca_core::Reference,
    variable: &flat::Variable,
    rank: usize,
    span: Span,
) -> Result<Option<Vec<i64>>, ToDaeError> {
    if variable.component_ref.is_none() {
        return Ok(None);
    }
    let scope = base
        .instance_id()
        .ok_or_else(|| ToDaeError::unresolved_reference(base.as_str(), span))?;
    let root_declaration = base
        .root_def_id()
        .ok_or_else(|| ToDaeError::unresolved_reference(base.as_str(), span))?;
    let indices =
        projection_owner_indices(flat, scope, root_declaration, variable.instance_id, span)?;
    let Some(indices) = indices else {
        return Ok(None);
    };
    if indices.len() != rank {
        return Ok(None);
    }
    Ok(Some(indices))
}

fn projection_owner_indices(
    flat: &flat::Model,
    scope: rumoca_core::InstanceId,
    root_declaration: rumoca_core::DefId,
    candidate: rumoca_core::InstanceId,
    span: Span,
) -> Result<Option<Vec<i64>>, ToDaeError> {
    let scope_relation = flat.instance_relations.get(&scope).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "record-array member slice",
            "reference scope is absent from the Flat instance graph",
            span,
        )
    })?;
    if scope_relation.kind != flat::InstanceKind::Class {
        return Err(ToDaeError::unsupported_flat(
            "record-array member slice",
            "reference scope is not an instantiated class occurrence",
            span,
        ));
    }
    let mut current = Some(candidate);
    for _ in 0..flat.instance_relations.len() {
        let Some(instance) = current else {
            return Ok(None);
        };
        let relation = flat.instance_relations.get(&instance).ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "record-array member slice",
                "materialized coordinate is absent from the Flat instance graph",
                span,
            )
        })?;
        if relation.owner == Some(scope)
            && relation.declaration == Some(root_declaration)
            && relation.kind == flat::InstanceKind::Aggregate
        {
            return Ok(Some(relation.indices.to_vec()));
        }
        current = relation.owner;
    }
    if current.is_none() {
        return Ok(None);
    }
    Err(ToDaeError::unsupported_flat(
        "record-array member slice",
        "Flat instance ownership contains a cycle",
        span,
    ))
}

fn validate_rectangular_elements(
    elements: &[ProjectedElement],
    rank: usize,
    span: Span,
) -> Result<(), ToDaeError> {
    let first_type = elements[0].value_type;
    let first_shape = &elements[0].shape;
    if elements
        .iter()
        .any(|element| element.value_type != first_type || &element.shape != first_shape)
    {
        return Err(ToDaeError::unsupported_flat(
            "record-array member slice",
            "projected fields do not share one exact declaration, value type, and shape",
            span,
        ));
    }
    let mut extents = vec![0i64; rank];
    for element in elements {
        for (ordinal, index) in element.indices.iter().copied().enumerate() {
            extents[ordinal] = extents[ordinal].max(index);
        }
    }
    let scalar_count = extents.iter().try_fold(1usize, |count, extent| {
        usize::try_from(*extent)
            .ok()
            .and_then(|extent| count.checked_mul(extent))
    });
    if scalar_count != Some(elements.len())
        || elements
            .windows(2)
            .any(|pair| pair[0].indices == pair[1].indices)
    {
        return Err(ToDaeError::unsupported_flat(
            "record-array member slice",
            "materialized record elements do not form one dense rectangular domain",
            span,
        ));
    }
    Ok(())
}
