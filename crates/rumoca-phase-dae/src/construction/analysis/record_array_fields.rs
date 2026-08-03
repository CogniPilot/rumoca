#[cfg(test)]
mod tests;

use super::*;

#[derive(Clone, Debug, PartialEq)]
pub(in crate::construction) enum RecordArrayFieldPlan {
    MaterializedCoordinate {
        coordinate: rumoca_core::InstanceId,
        target: rumoca_core::DefId,
        value_type: rumoca_core::TypeId,
        shape: Box<[i64]>,
    },
    Projection {
        coordinates: Box<[rumoca_core::InstanceId]>,
        target: rumoca_core::DefId,
        value_type: rumoca_core::TypeId,
        shape: Box<[i64]>,
        subscripts: Box<[Subscript]>,
    },
}

/// Exact Flat declaration evidence for an ordinary `record_expression.field`.
///
/// Unlike [`RecordArrayFieldPlan`], this certificate does not materialize a
/// coordinate run. Its base remains an expression that DAE construction must
/// lower and type-check before selecting the retained field ordinal.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::construction) struct StructuralRecordFieldPlan {
    pub(in crate::construction) owners: Box<[rumoca_core::DefId]>,
    pub(in crate::construction) field: rumoca_core::DefId,
    pub(in crate::construction) ordinal: usize,
    pub(in crate::construction) name: VarName,
    shape: Box<[i64]>,
    /// Ordered field declarations of the complete record layout. More than one
    /// retained record owner may name the same underlying record type (for
    /// example a unit type alias), but only an identical declaration layout
    /// may join this certificate.
    layout: Box<[rumoca_core::DefId]>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct StructuralRecordFieldKey {
    occurrence: Span,
    field: rumoca_core::DefId,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(in crate::construction) enum RecordArrayFieldPlanKey {
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

impl RecordArrayFieldPlanKey {
    fn occurrence(&self) -> Span {
        match self {
            Self::Materialized { occurrence, .. } | Self::Projection { occurrence, .. } => {
                *occurrence
            }
        }
    }
}

#[derive(Debug, Default)]
pub(in crate::construction) struct RecordArrayFieldPlans {
    // Flattening may specialize one source occurrence more than once in the
    // same instance scope. The retained span and declaration path identify the
    // occurrence, while the specialized subscript identifies which checked
    // projection it denotes. Keep those selector-distinct certificates under
    // one occurrence key instead of making the source span a false uniqueness
    // proof.
    by_occurrence: HashMap<RecordArrayFieldPlanKey, Vec<RecordArrayFieldPlan>>,
    /// Structural projections keyed by their exact source occurrence and
    /// declaration identity. A field spelling is checked against the retained
    /// record layout, but never acts as semantic identity.
    structural_by_occurrence: HashMap<StructuralRecordFieldKey, StructuralRecordFieldPlan>,
    /// Declared trailing shape of every structurally accessible record field.
    ///
    /// `DefId` is the field identity; the dimensions come from Flat's retained
    /// record layout.  This lets function-shape proof handle ordinary field
    /// access (for example `(a - b).re`) without recovering a declaration from
    /// a rendered field name.
    field_shapes: HashMap<rumoca_core::DefId, Box<[i64]>>,
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
        self.by_occurrence
            .get(&field_access_key(expression)?)?
            .iter()
            .find(|plan| plan_selects_expression(plan, expression))
    }

    pub(in crate::construction) fn field_shape(&self, field: rumoca_core::DefId) -> Option<&[i64]> {
        self.field_shapes.get(&field).map(Box::as_ref)
    }

    pub(in crate::construction) fn structural(
        &self,
        expression: &Expression,
    ) -> Option<&StructuralRecordFieldPlan> {
        let Expression::FieldAccess {
            field,
            field_def_id,
            span,
            ..
        } = expression
        else {
            return None;
        };
        let plan = self
            .structural_by_occurrence
            .get(&StructuralRecordFieldKey {
                occurrence: *span,
                field: *field_def_id,
            })?;
        (plan.name.as_str() == field).then_some(plan)
    }
}

pub(super) fn analyze_record_array_fields<'expression>(
    flat: &flat::Model,
    expressions: impl IntoIterator<Item = &'expression Expression>,
) -> Result<RecordArrayFieldPlans, ToDaeError> {
    let candidates = CoordinateCandidates::new(flat);
    let declared_fields = declared_record_fields(flat)?;
    let field_shapes = declared_fields
        .iter()
        .map(|(field, plan)| (*field, plan.shape.clone()))
        .collect();
    let mut plans = HashMap::new();
    let mut structural_plans = HashMap::new();
    for expression in expressions {
        collect_plans(
            flat,
            &candidates,
            &declared_fields,
            expression,
            &mut plans,
            &mut structural_plans,
        )?;
    }
    Ok(RecordArrayFieldPlans {
        by_occurrence: plans,
        structural_by_occurrence: structural_plans,
        field_shapes,
    })
}

fn declared_record_fields(
    flat: &flat::Model,
) -> Result<HashMap<rumoca_core::DefId, StructuralRecordFieldPlan>, ToDaeError> {
    let mut fields = HashMap::new();
    for (owner, record) in &flat.record_types {
        let layout = record
            .fields
            .iter()
            .map(|field| field.def_id)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        for (ordinal, field) in record.fields.iter().enumerate() {
            let plan = StructuralRecordFieldPlan {
                owners: vec![*owner].into_boxed_slice(),
                field: field.def_id,
                ordinal,
                name: VarName::new(&field.name),
                shape: field.dims.clone().into_boxed_slice(),
                layout: layout.clone(),
            };
            match fields.entry(field.def_id) {
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(plan);
                }
                std::collections::hash_map::Entry::Occupied(mut entry)
                    if same_structural_layout(entry.get(), &plan) =>
                {
                    merge_structural_owner(entry.get_mut(), *owner);
                }
                std::collections::hash_map::Entry::Occupied(_) => {
                    return Err(ToDaeError::MissingSemanticIdentity {
                        identity: format!(
                            "record field declaration {} has one exact retained layout",
                            field.def_id.index()
                        ),
                    });
                }
            }
        }
    }
    Ok(fields)
}

fn merge_structural_owner(plan: &mut StructuralRecordFieldPlan, owner: rumoca_core::DefId) {
    if plan.owners.contains(&owner) {
        return;
    }
    let mut owners = plan.owners.to_vec();
    owners.push(owner);
    owners.sort_by_key(|owner| owner.index());
    plan.owners = owners.into_boxed_slice();
}

fn same_structural_layout(
    lhs: &StructuralRecordFieldPlan,
    rhs: &StructuralRecordFieldPlan,
) -> bool {
    lhs.field == rhs.field
        && lhs.ordinal == rhs.ordinal
        && lhs.name == rhs.name
        && lhs.shape == rhs.shape
        && lhs.layout == rhs.layout
}

fn collect_plans(
    flat: &flat::Model,
    candidates: &CoordinateCandidates<'_>,
    declared_fields: &HashMap<rumoca_core::DefId, StructuralRecordFieldPlan>,
    expression: &Expression,
    plans: &mut HashMap<RecordArrayFieldPlanKey, Vec<RecordArrayFieldPlan>>,
    structural_plans: &mut HashMap<StructuralRecordFieldKey, StructuralRecordFieldPlan>,
) -> Result<(), ToDaeError> {
    if let Some((key, plan)) = plan_field_access(flat, candidates, expression)? {
        insert_plan(plans, key, plan, expression_span(expression)?)?;
        return Ok(());
    }
    if let Some((key, plan)) = plan_structural_field(declared_fields, expression)? {
        let incompatible = structural_plans
            .insert(key, plan.clone())
            .is_some_and(|previous| previous != plan);
        if incompatible {
            return Err(ToDaeError::unsupported_flat(
                "aggregate expression",
                "one semantic occurrence produced incompatible record-field owners",
                key.occurrence,
            ));
        }
    }
    for child in expression_children(expression) {
        collect_plans(
            flat,
            candidates,
            declared_fields,
            child,
            plans,
            structural_plans,
        )?;
    }
    Ok(())
}

fn plan_structural_field(
    declared_fields: &HashMap<rumoca_core::DefId, StructuralRecordFieldPlan>,
    expression: &Expression,
) -> Result<Option<(StructuralRecordFieldKey, StructuralRecordFieldPlan)>, ToDaeError> {
    let Expression::FieldAccess {
        field,
        field_def_id,
        span,
        ..
    } = expression
    else {
        return Ok(None);
    };
    let Some(plan) = declared_fields.get(field_def_id) else {
        return Ok(None);
    };
    require_span(*span, "aggregate expression")?;
    if plan.name.as_str() != field {
        return Err(ToDaeError::unsupported_flat(
            "aggregate expression",
            "the field occurrence spelling disagrees with its retained declaration identity",
            *span,
        ));
    }
    Ok(Some((
        StructuralRecordFieldKey {
            occurrence: *span,
            field: *field_def_id,
        },
        plan.clone(),
    )))
}

fn insert_plan(
    plans: &mut HashMap<RecordArrayFieldPlanKey, Vec<RecordArrayFieldPlan>>,
    key: RecordArrayFieldPlanKey,
    plan: RecordArrayFieldPlan,
    span: Span,
) -> Result<(), ToDaeError> {
    let alternatives = plans.entry(key).or_default();
    if alternatives.contains(&plan) {
        return Ok(());
    }
    if alternatives
        .iter()
        .any(|previous| same_projection_selector(previous, &plan))
    {
        return Err(ToDaeError::unsupported_flat(
            "record-array member slice",
            "one semantic occurrence and selector produced incompatible projection certificates",
            span,
        ));
    }
    alternatives.push(plan);
    Ok(())
}

fn plan_selects_expression(plan: &RecordArrayFieldPlan, expression: &Expression) -> bool {
    match plan {
        RecordArrayFieldPlan::MaterializedCoordinate { .. } => {
            concrete_component_reference(expression).is_some()
        }
        RecordArrayFieldPlan::Projection { subscripts, .. } => projection_pattern(expression)
            .is_some_and(|pattern| pattern.subscripts == subscripts.as_ref()),
    }
}

fn same_projection_selector(lhs: &RecordArrayFieldPlan, rhs: &RecordArrayFieldPlan) -> bool {
    match (lhs, rhs) {
        (
            RecordArrayFieldPlan::Projection {
                subscripts: lhs, ..
            },
            RecordArrayFieldPlan::Projection {
                subscripts: rhs, ..
            },
        ) => lhs == rhs,
        // A materialized key already includes every literal index, so two
        // values under that key necessarily claim the same selector.
        (
            RecordArrayFieldPlan::MaterializedCoordinate { .. },
            RecordArrayFieldPlan::MaterializedCoordinate { .. },
        ) => true,
        _ => false,
    }
}

fn plan_field_access(
    flat: &flat::Model,
    candidates: &CoordinateCandidates<'_>,
    expression: &Expression,
) -> Result<Option<(RecordArrayFieldPlanKey, RecordArrayFieldPlan)>, ToDaeError> {
    let Expression::FieldAccess { span, .. } = expression else {
        return Ok(None);
    };
    require_span(*span, "record-array member slice")?;
    if let Some(plan) = plan_materialized_coordinate(flat, candidates, expression, *span)? {
        return Ok(Some(plan));
    }
    plan_projection(flat, candidates, expression, *span)
}

fn plan_materialized_coordinate(
    flat: &flat::Model,
    candidates: &CoordinateCandidates<'_>,
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
            shape: variable.dims.clone().into_boxed_slice(),
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
                Ok(Some(indices)) => Some(Ok(ProjectedElement {
                    indices,
                    coordinate: variable.instance_id,
                    value_type: variable.type_id,
                    shape: variable.dims.clone(),
                })),
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

/// Complete the projection certificate by proving that every selected Flat
/// instance becomes a runtime DAE coordinate.
///
/// Shape specialization runs before model-role planning, but it may consume
/// only the declaration/instance/type/range facts above. Construction receives
/// the plan only after this second, role-owned transition succeeds, so no
/// aggregate or otherwise non-runtime Flat value can enter DAE lowering.
pub(super) fn validate_record_array_field_runtime_coordinates(
    flat: &flat::Model,
    plans: &RecordArrayFieldPlans,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<(), ToDaeError> {
    let variables = flat
        .variables
        .values()
        .map(|variable| (variable.instance_id, variable))
        .collect::<HashMap<_, _>>();
    let mut ordered = plans
        .by_occurrence
        .iter()
        .flat_map(|(key, alternatives)| alternatives.iter().map(move |plan| (key, plan)))
        .collect::<Vec<_>>();
    ordered.sort_by_key(|(key, _)| {
        let span = key.occurrence();
        (span.source.0, span.start.0, span.end.0)
    });
    for (key, plan) in ordered {
        let occurrence = key.occurrence();
        let coordinates: &[rumoca_core::InstanceId] = match plan {
            RecordArrayFieldPlan::MaterializedCoordinate { coordinate, .. } => {
                std::slice::from_ref(coordinate)
            }
            RecordArrayFieldPlan::Projection { coordinates, .. } => coordinates,
        };
        for coordinate in coordinates {
            let variable = variables.get(coordinate).copied().ok_or_else(|| {
                ToDaeError::MissingSemanticIdentity {
                    identity: format!(
                        "record-array projection coordinate instance {}",
                        coordinate.index()
                    ),
                }
            })?;
            require_runtime_coordinate(variable, roles, occurrence)?;
        }
    }
    Ok(())
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
        Some(
            PlannedRole::UnusedExpandable
            | PlannedRole::Clock
            | PlannedRole::EnumerationLiteral
            | PlannedRole::Aggregate,
        )
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
