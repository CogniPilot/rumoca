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
        /// Ordinal of the path part the subscripts belong to (MLS §10.5).
        sliced_part: usize,
        rank: usize,
    },
}

pub(in crate::construction) struct RecordArrayFieldPlans {
    by_occurrence: HashMap<RecordArrayFieldPlanKey, RecordArrayFieldPlan>,
}

/// Flat coordinates indexed by the declaration they materialize.
///
/// A Flat coordinate carries its whole path from the model root, while a
/// reference written inside a class names only the part of that path visible
/// from its own scope (MLS §5.3.1) — `ac.pin[1].v` inside `Probe` materializes
/// as `probe.ac.pin[1].v`. Indexing on the target declaration alone therefore
/// keeps every coordinate a written path could name; which of them it actually
/// names is settled by the occurrence graph, never by the two spellings.
struct CoordinateCandidates<'flat> {
    by_target: HashMap<rumoca_core::DefId, Vec<&'flat flat::Variable>>,
}

impl<'flat> CoordinateCandidates<'flat> {
    fn new(flat: &'flat flat::Model) -> Self {
        let mut by_target = HashMap::<_, Vec<_>>::new();
        for variable in flat.variables.values() {
            let Some(reference) = variable.component_ref.as_ref() else {
                continue;
            };
            by_target
                .entry(reference.target_def_id())
                .or_default()
                .push(variable);
        }
        Self { by_target }
    }

    /// The coordinates whose path ends in exactly the written declarations.
    fn ending_in(&self, declarations: &[rumoca_core::DefId]) -> Vec<&'flat flat::Variable> {
        let Some(target) = declarations.last() else {
            return Vec::new();
        };
        self.by_target
            .get(target)
            .map(Vec::as_slice)
            .unwrap_or_default()
            .iter()
            .copied()
            .filter(|variable| {
                variable.component_ref.as_ref().is_some_and(|reference| {
                    reference_declarations(reference).ends_with(declarations)
                })
            })
            .collect()
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
        &candidates.ending_in(&reference_declarations(&reference)),
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
    // The certificate describes one linear run of coordinates, and the lowering
    // realizes it as a rank-one array indexed by the written subscripts. A
    // multi-dimensional written slice (`ac.pin[:, :].v`, MLS §10.5) denotes a
    // rank-`n` member array that this shape cannot carry: accepting it would
    // mint a certificate the lowering then mis-indexes against a flattened
    // element order. Abstain here, by name, rather than downstream.
    if rank != 1 {
        return Err(ToDaeError::unsupported_flat(
            "record-array member slice",
            "a multi-dimensional member slice has no rank-preserving canonical projection; \
             this certificate describes exactly one subscripted dimension",
            span,
        ));
    }
    let declarations = reference_declarations(&pattern.reference);
    let mut elements = candidates
        .ending_in(&declarations)
        .iter()
        .filter_map(|&variable| {
            match projected_variable_indices(
                flat,
                pattern.base,
                &declarations,
                pattern.sliced_part,
                variable,
                rank,
                span,
            ) {
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
            sliced_part: pattern.sliced_part,
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
    /// Ordinal within `reference` of the part the subscripts belong to.
    ///
    /// MLS §10.5 binds a subscript to the component array named by the part it
    /// is written on, so `a.b[e].c` slices `b` — declared inside `a` — and not
    /// the head of the path. Flat lowering splits a reference at its first
    /// subscripted part, which puts that part last in `base`.
    sliced_part: usize,
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
                let sliced_part = parts.len().checked_sub(1)?;
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
                    sliced_part,
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
        sliced_part: pattern.sliced_part,
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

/// Prove the slice extent one Flat coordinate contributes to a member
/// projection, reading it off the part the subscripts were written on.
///
/// MLS §10.5 binds an array subscript to the component array named by the part
/// carrying it: in `a.b[e].c` the sliced array is `b`, declared inside `a`, and
/// the expression denotes the member `c` of element `e`. The proof therefore
/// matches the whole declaration chain the Flat occurrence graph records for a
/// coordinate against the reference's parts and reports the indices of the
/// subscripted part alone — anchoring on the head of the path would only ever
/// admit `b[e].c`, never `a.b[e].c`.
fn projected_variable_indices(
    flat: &flat::Model,
    base: &rumoca_core::Reference,
    declarations: &[rumoca_core::DefId],
    sliced_part: usize,
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
    let Some(chain) = component_ancestry(flat, scope, variable.instance_id, span)? else {
        return Ok(None);
    };
    if chain.len() != declarations.len()
        || chain
            .iter()
            .zip(declarations)
            .any(|(occurrence, declaration)| occurrence.declaration != *declaration)
    {
        return Ok(None);
    }
    let Some(sliced) = chain.get(sliced_part) else {
        return Ok(None);
    };
    if sliced.kind != flat::InstanceKind::Aggregate || sliced.indices.len() != rank {
        return Ok(None);
    }
    // One `Index` node carries one part's subscripts, so exactly one part of
    // this path may be an array occurrence. A second array part would make the
    // expression denote a higher-rank array (MLS §10.5) that this certificate
    // cannot describe, and silently projecting one of the two would fabricate a
    // shape the model never wrote.
    if chain
        .iter()
        .enumerate()
        .any(|(ordinal, occurrence)| ordinal != sliced_part && !occurrence.indices.is_empty())
    {
        return Err(ToDaeError::unsupported_flat(
            "record-array member slice",
            "a member slice subscripts one component array, but another part of this path is \
             itself an array occurrence",
            span,
        ));
    }
    Ok(Some(sliced.indices.to_vec()))
}

fn projection_owner_indices(
    flat: &flat::Model,
    scope: rumoca_core::InstanceId,
    root_declaration: rumoca_core::DefId,
    candidate: rumoca_core::InstanceId,
    span: Span,
) -> Result<Option<Vec<i64>>, ToDaeError> {
    let Some(chain) = component_ancestry(flat, scope, candidate, span)? else {
        return Ok(None);
    };
    let Some(root) = chain.first() else {
        return Ok(None);
    };
    if root.declaration != root_declaration || root.kind != flat::InstanceKind::Aggregate {
        return Ok(None);
    }
    Ok(Some(root.indices.to_vec()))
}

/// One component occurrence on the path between an instantiated class scope and
/// a Flat coordinate.
struct AncestorOccurrence {
    declaration: rumoca_core::DefId,
    indices: Box<[i64]>,
    kind: flat::InstanceKind,
}

/// The component occurrences the Flat graph proves between `scope` and
/// `candidate`, outermost first.
///
/// Only components name a part of a component reference — an `extends` adds
/// class occurrences that no part spells (MLS §7.1) — so class occurrences are
/// stepped over and never enter the chain. `None` means `candidate` is not
/// owned by `scope`, which is how a same-spelled coordinate belonging to a
/// sibling instance is excluded without consulting a rendered name.
fn component_ancestry(
    flat: &flat::Model,
    scope: rumoca_core::InstanceId,
    candidate: rumoca_core::InstanceId,
    span: Span,
) -> Result<Option<Vec<AncestorOccurrence>>, ToDaeError> {
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
    let mut chain = Vec::new();
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
        if relation.kind != flat::InstanceKind::Class {
            let Some(declaration) = relation.declaration else {
                return Ok(None);
            };
            chain.push(AncestorOccurrence {
                declaration,
                indices: relation.indices.clone(),
                kind: relation.kind,
            });
        }
        if relation.owner == Some(scope) {
            chain.reverse();
            return Ok(Some(chain));
        }
        current = relation.owner;
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
