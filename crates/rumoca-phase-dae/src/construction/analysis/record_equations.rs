use super::*;
use rumoca_core::{DefId, EffectiveType, InstanceId};

use super::discrete_values::current_discrete_dependencies;
use super::reference_identity::reference_has_exact_identity;

mod call_layout;
use call_layout::validate_call_layout;
mod identity;
use identity::*;

pub(super) fn analyze_record_equations(
    flat: &flat::Model,
    equations: &[flat::Equation],
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<HashMap<usize, RecordEquationPlan>, ToDaeError> {
    let index = ModelRecordIndex::new(flat)?;
    let mut plans = HashMap::new();
    for (row, equation) in equations.iter().enumerate() {
        if let Some(plan) = analyze_record_equation(&index, equation, roles)? {
            plans.insert(row, plan);
        }
    }
    Ok(plans)
}

pub(super) fn reject_initial_record_equations(flat: &flat::Model) -> Result<(), ToDaeError> {
    let index = ModelRecordIndex::new(flat)?;
    for equation in &flat.initial_equations {
        if record_operands(&index, equation)?.is_some() {
            return Err(ToDaeError::unsupported_flat(
                "initial record equation",
                "whole-record initialization requires a checked initialization aggregate owner",
                equation.span,
            ));
        }
    }
    Ok(())
}

pub(super) fn reject_structured_record_equations(flat: &flat::Model) -> Result<(), ToDaeError> {
    let index = ModelRecordIndex::new(flat)?;
    for family in flat
        .structured_equations
        .iter()
        .chain(&flat.initial_structured_equations)
    {
        let Some(template) = &family.template else {
            continue;
        };
        for residual in &template.body {
            if residual_operands(&index, residual, family.span)?.is_some() {
                return Err(ToDaeError::unsupported_flat(
                    "structured record equation",
                    "whole-record structured families require a checked aggregate family owner",
                    family.span,
                ));
            }
        }
    }
    Ok(())
}

pub(super) fn reject_record_family_rows(
    plans: &HashMap<usize, RecordEquationPlan>,
    family_rows: &HashSet<usize>,
    equations: &[flat::Equation],
) -> Result<(), ToDaeError> {
    if let Some(row) = plans.keys().find(|row| family_rows.contains(row)) {
        let span = equations
            .get(*row)
            .map(|equation| equation.span)
            .ok_or_else(|| ToDaeError::MissingSemanticIdentity {
                identity: format!("record family certificate row {row} exists in Flat"),
            })?;
        return Err(ToDaeError::unsupported_flat(
            "structured record equation",
            "a materialized whole-record row cannot bypass an aggregate family owner",
            span,
        ));
    }
    Ok(())
}

fn analyze_record_equation(
    index: &ModelRecordIndex<'_>,
    equation: &flat::Equation,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<Option<RecordEquationPlan>, ToDaeError> {
    let Some(operands) = record_operands(index, equation)? else {
        return Ok(None);
    };
    match operands {
        RecordOperands::Records { left, right } => {
            direct_record_plan(index, equation, left, right, roles).map(Some)
        }
        RecordOperands::Aggregate {
            record,
            call,
            aggregate,
        } => aggregate_record_plan(index, equation, record, call, aggregate, roles).map(Some),
        RecordOperands::Unsupported => Err(ToDaeError::unsupported_flat(
            "record equation",
            "whole-record equality requires another exact record occurrence or one exact record-valued call",
            equation.span,
        )),
    }
}

fn direct_record_plan(
    index: &ModelRecordIndex<'_>,
    equation: &flat::Equation,
    left: &flat::RecordInstance,
    right: &flat::RecordInstance,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<RecordEquationPlan, ToDaeError> {
    require_scalar_same_record(left, right, equation.span)?;
    let left = index.record_leaves(left, equation.span)?;
    let right = index.record_leaves(right, equation.span)?;
    require_same_leaf_layout(index.flat, &left, &right, equation.span)?;
    let fields = left
        .into_iter()
        .zip(right)
        .map(|(left, right)| plan_coordinate_pair(index.flat, left, right, roles, equation))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(RecordEquationPlan {
        fields,
        aggregate: None,
    })
}

fn aggregate_record_plan(
    index: &ModelRecordIndex<'_>,
    equation: &flat::Equation,
    record: &flat::RecordInstance,
    call: RecordCall<'_>,
    aggregate: RecordEquationAggregateSide,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<RecordEquationPlan, ToDaeError> {
    require_scalar_record(record, equation.span)?;
    let leaves = index.record_leaves(record, equation.span)?;
    validate_call_layout(index.flat, record, call, &leaves, equation.span)?;
    let dependencies = current_discrete_dependencies(call.expression, roles);
    let fields = leaves
        .into_iter()
        .map(|leaf| {
            let projection = leaf.projection.clone();
            plan_target(
                index.flat,
                leaf,
                RecordEquationFieldValue::AggregateProjection(projection),
                dependencies.clone(),
                roles,
                equation.span,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(RecordEquationPlan {
        fields,
        aggregate: Some(aggregate),
    })
}

fn plan_coordinate_pair(
    flat: &flat::Model,
    left: RecordLeaf<'_>,
    right: RecordLeaf<'_>,
    roles: &HashMap<VarName, PlannedRole>,
    equation: &flat::Equation,
) -> Result<RecordEquationFieldPlan, ToDaeError> {
    let span = equation.span;
    let left_role = roles
        .get(&left.variable.name)
        .copied()
        .ok_or_else(|| record_layout_error("left record leaf has no checked runtime role", span))?;
    let right_role = roles.get(&right.variable.name).copied().ok_or_else(|| {
        record_layout_error("right record leaf has no checked runtime role", span)
    })?;
    let left_partition = definable_record_leaf_partition(flat, &left, left_role);
    let right_partition = definable_record_leaf_partition(flat, &right, right_role);
    if matches!(left_partition, Some(RecordLeafPartition::DiscreteValue))
        && matches!(right_partition, Some(RecordLeafPartition::DiscreteValue))
        && !super::equation_partitions::ordinary_equation_owns_solved_lhs(equation)
    {
        return Err(ToDaeError::unsupported_flat(
            "record equation target",
            "two discrete-value record leaves require a checked causal orientation",
            span,
        ));
    }
    let (target, value, partition, value_role) = if let Some(partition) = left_partition {
        (left, right, partition, right_role)
    } else if let Some(partition) = right_partition {
        (right, left, partition, left_role)
    } else {
        return Err(ToDaeError::unsupported_flat(
            "record equation target",
            "record equality has no state, algebraic, output, or discrete leaf to define",
            span,
        ));
    };
    let mut dependencies = HashSet::new();
    if matches!(
        roles.get(&value.variable.name),
        Some(PlannedRole::DiscreteValue)
    ) {
        dependencies.insert(value.variable.name.clone());
    }
    let source = RecordEquationFieldValue::Coordinate(value.coordinate(
        span,
        matches!(
            value_role,
            PlannedRole::DiscreteReal | PlannedRole::DiscreteValue
        ),
    )?);
    construct_record_field_plan(target, source, dependencies, partition, span)
}

fn plan_target(
    flat: &flat::Model,
    target: RecordLeaf<'_>,
    value: RecordEquationFieldValue,
    dependencies: HashSet<VarName>,
    roles: &HashMap<VarName, PlannedRole>,
    span: Span,
) -> Result<RecordEquationFieldPlan, ToDaeError> {
    let name = target.variable.name.clone();
    let role = roles.get(&name).copied().ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "record equation coordinate",
            format!("record leaf `{name}` has no checked runtime role"),
            span,
        )
    })?;
    let partition = definable_record_leaf_partition(flat, &target, role).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "record equation target",
            format!("record leaf `{name}` has no legal Appendix-B definition partition"),
            span,
        )
    })?;
    construct_record_field_plan(target, value, dependencies, partition, span)
}

#[derive(Clone, Copy)]
enum RecordLeafPartition {
    ContinuousReal,
    DiscreteReal,
    DiscreteValue,
}

fn definable_record_leaf_partition(
    flat: &flat::Model,
    leaf: &RecordLeaf<'_>,
    role: PlannedRole,
) -> Option<RecordLeafPartition> {
    let scalar = effective_variable_scalar_type(flat, leaf.variable)?;
    match (role, scalar) {
        (
            PlannedRole::State | PlannedRole::Algebraic | PlannedRole::Output,
            dae::ScalarType::Real,
        ) => Some(RecordLeafPartition::ContinuousReal),
        (PlannedRole::DiscreteReal, dae::ScalarType::Real) => {
            Some(RecordLeafPartition::DiscreteReal)
        }
        (
            PlannedRole::DiscreteValue,
            dae::ScalarType::Integer
            | dae::ScalarType::Boolean
            | dae::ScalarType::String
            | dae::ScalarType::Enumeration,
        ) => Some(RecordLeafPartition::DiscreteValue),
        _ => None,
    }
}

fn construct_record_field_plan(
    target: RecordLeaf<'_>,
    value: RecordEquationFieldValue,
    dependencies: HashSet<VarName>,
    partition: RecordLeafPartition,
    span: Span,
) -> Result<RecordEquationFieldPlan, ToDaeError> {
    let discrete_unknown = matches!(
        partition,
        RecordLeafPartition::DiscreteReal | RecordLeafPartition::DiscreteValue
    );
    let target = target.coordinate(span, discrete_unknown)?;
    Ok(match partition {
        RecordLeafPartition::ContinuousReal => {
            RecordEquationFieldPlan::ContinuousRealResidual { target, value }
        }
        RecordLeafPartition::DiscreteReal => {
            RecordEquationFieldPlan::DiscreteRealResidual { target, value }
        }
        RecordLeafPartition::DiscreteValue => RecordEquationFieldPlan::DiscreteValueDefinition {
            target,
            value,
            dependencies,
        },
    })
}

fn require_scalar_record(record: &flat::RecordInstance, span: Span) -> Result<(), ToDaeError> {
    if record.dims.is_empty() {
        return Ok(());
    }
    Err(ToDaeError::unsupported_flat(
        "record equation",
        "arrays of records require a compact aggregate equation owner",
        span,
    ))
}

fn require_scalar_same_record(
    left: &flat::RecordInstance,
    right: &flat::RecordInstance,
    span: Span,
) -> Result<(), ToDaeError> {
    require_scalar_record(left, span)?;
    require_scalar_record(right, span)?;
    if left.type_def_id == right.type_def_id && left.effective_type_id == right.effective_type_id {
        return Ok(());
    }
    Err(ToDaeError::unsupported_flat(
        "record equation",
        "record equality operands have distinct exact nominal type identities",
        span,
    ))
}

fn require_same_leaf_layout(
    flat: &flat::Model,
    left: &[RecordLeaf<'_>],
    right: &[RecordLeaf<'_>],
    span: Span,
) -> Result<(), ToDaeError> {
    let same = left.len() == right.len()
        && left.iter().zip(right).all(|(left, right)| {
            left.projection == right.projection
                && same_direct_leaf_identity(&left.identity, &right.identity)
                && left.variable.dims == right.variable.dims
                && variable_effective_type(flat, left.variable)
                    == variable_effective_type(flat, right.variable)
        });
    if same {
        Ok(())
    } else {
        Err(record_layout_error(
            "record equality operands do not have one complete exact field layout",
            span,
        ))
    }
}

fn same_direct_leaf_identity(
    left: &RecordEquationLeafIdentity,
    right: &RecordEquationLeafIdentity,
) -> bool {
    left.declarations == right.declarations
        && left.records.len() == right.records.len()
        && left
            .records
            .iter()
            .zip(&right.records)
            .all(|(left, right)| {
                left.type_def_id == right.type_def_id
                    && left.effective_type_id == right.effective_type_id
            })
}

fn variable_effective_type<'flat>(
    flat: &'flat flat::Model,
    variable: &flat::Variable,
) -> Option<&'flat EffectiveType> {
    flat.effective_types.get(&variable.type_id)
}

struct RecordLeaf<'flat> {
    variable: &'flat flat::Variable,
    projection: Box<[usize]>,
    identity: RecordEquationLeafIdentity,
}

struct RecordLeafCollector<'flat> {
    projection: Vec<usize>,
    declarations: Vec<DefId>,
    records: Vec<RecordEquationNominalIdentity>,
    active: HashSet<DefId>,
    leaves: Vec<RecordLeaf<'flat>>,
    span: Span,
}

impl<'flat> RecordLeafCollector<'flat> {
    fn new(record: &flat::RecordInstance, span: Span) -> Self {
        Self {
            projection: Vec::new(),
            declarations: Vec::new(),
            records: vec![nominal_identity(record)],
            active: HashSet::new(),
            leaves: Vec::new(),
            span,
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
struct RecordEquationLeafIdentity {
    declarations: Box<[DefId]>,
    records: Box<[RecordEquationNominalIdentity]>,
}

#[derive(Clone, Copy, Eq, PartialEq)]
struct RecordEquationNominalIdentity {
    type_def_id: DefId,
    effective_type_id: rumoca_core::TypeId,
}

#[derive(Clone, Copy)]
struct CheckedRecordOccurrence<'flat> {
    record: &'flat flat::RecordInstance,
    class: InstanceId,
}

impl RecordLeaf<'_> {
    fn coordinate(
        &self,
        span: Span,
        discrete_unknown: bool,
    ) -> Result<RecordEquationCoordinate, ToDaeError> {
        let scalar_count = self.variable.shape_size().map_err(|_| {
            record_layout_error(
                format!(
                    "record leaf `{}` has an unrepresentable scalar cardinality",
                    self.variable.name
                ),
                span,
            )
        })?;
        Ok(RecordEquationCoordinate {
            name: self.variable.name.clone(),
            instance_id: self.variable.instance_id,
            scalar_count,
            discrete_unknown,
        })
    }
}

struct ModelRecordIndex<'flat> {
    flat: &'flat flat::Model,
    children_by_owner: HashMap<InstanceId, Vec<InstanceId>>,
    children_by_declaration: HashMap<(InstanceId, DefId), Vec<InstanceId>>,
    record_target_declarations: HashSet<DefId>,
    variables: HashMap<InstanceId, &'flat flat::Variable>,
    records: HashMap<InstanceId, &'flat flat::RecordInstance>,
}

impl<'flat> ModelRecordIndex<'flat> {
    fn new(flat: &'flat flat::Model) -> Result<Self, ToDaeError> {
        let mut children_by_owner: HashMap<InstanceId, Vec<InstanceId>> = HashMap::new();
        let mut children_by_declaration: HashMap<(InstanceId, DefId), Vec<InstanceId>> =
            HashMap::new();
        let mut exact_occurrences = HashSet::new();
        for (instance, relation) in &flat.instance_relations {
            let Some(owner) = relation.owner else {
                continue;
            };
            children_by_owner.entry(owner).or_default().push(*instance);
            let Some(declaration) = relation.declaration else {
                continue;
            };
            let exact = (owner, declaration, relation.indices.clone());
            if !exact_occurrences.insert(exact) {
                return Err(ToDaeError::MissingSemanticIdentity {
                    identity: format!(
                        "Flat owner {} has two children for declaration {} at the same exact occurrence indices",
                        owner.index(),
                        declaration.index()
                    ),
                });
            }
            children_by_declaration
                .entry((owner, declaration))
                .or_default()
                .push(*instance);
        }
        let records = unique_instances(
            flat.record_instances.values(),
            |value| value.instance_id,
            |value| value.component_ref.to_string(),
            |value| value.source_span,
            "record occurrence identity",
        )?;
        Ok(Self {
            flat,
            children_by_owner,
            children_by_declaration,
            record_target_declarations: flat
                .record_instances
                .values()
                .map(|record| record.component_ref.target_def_id())
                .collect(),
            variables: unique_instances(
                flat.variables.values(),
                |value| value.instance_id,
                |value| value.name.to_string(),
                |value| value.source_span,
                "runtime coordinate identity",
            )?,
            records,
        })
    }

    fn record_by_reference(
        &self,
        reference: &rumoca_core::Reference,
        span: Span,
    ) -> Result<Option<&'flat flat::RecordInstance>, ToDaeError> {
        let component_ref = reference
            .component_ref()
            .ok_or_else(|| missing_record_identity(reference, span))?;
        let Some(instance) = reference.instance_id() else {
            return if self
                .record_target_declarations
                .contains(&component_ref.target_def_id())
            {
                Err(missing_record_identity(reference, span))
            } else {
                Ok(None)
            };
        };
        let relation = self
            .flat
            .instance_relations
            .get(&instance)
            .ok_or_else(|| missing_record_identity(reference, span))?;
        if relation.kind != flat::InstanceKind::Aggregate {
            if self.is_record_array_family(instance, component_ref.target_def_id()) {
                return Err(ToDaeError::unsupported_flat(
                    "record equation",
                    "arrays of records require a compact record-family owner",
                    span,
                ));
            }
            return if self
                .record_target_declarations
                .contains(&component_ref.target_def_id())
                || self.records.contains_key(&instance)
            {
                Err(missing_record_identity(reference, span))
            } else {
                Ok(None)
            };
        };
        let record = self
            .records
            .get(&instance)
            .copied()
            .ok_or_else(|| missing_record_identity(reference, span))?;
        if !reference_has_exact_identity(reference, &record.component_ref)
            || relation.declaration != Some(component_ref.target_def_id())
        {
            return Err(missing_record_identity(reference, span));
        }
        Ok(Some(record))
    }

    fn is_record_array_family(&self, owner: InstanceId, declaration: DefId) -> bool {
        let Some(children) = self.children_by_declaration.get(&(owner, declaration)) else {
            return false;
        };
        !children.is_empty()
            && children
                .iter()
                .all(|child| self.is_indexed_record_occurrence(*child))
    }

    fn is_indexed_record_occurrence(&self, child: InstanceId) -> bool {
        let Some(relation) = self.flat.instance_relations.get(&child) else {
            return false;
        };
        relation.kind == flat::InstanceKind::Aggregate
            && !relation.indices.is_empty()
            && self.records.contains_key(&child)
    }

    fn record_leaves(
        &self,
        record: &'flat flat::RecordInstance,
        span: Span,
    ) -> Result<Vec<RecordLeaf<'flat>>, ToDaeError> {
        require_scalar_record(record, span)?;
        let occurrence = self.check_record_occurrence(record, span)?;
        let mut collector = RecordLeafCollector::new(record, span);
        self.collect_record_leaves(occurrence, &mut collector)?;
        Ok(collector.leaves)
    }

    fn collect_record_leaves(
        &self,
        occurrence: CheckedRecordOccurrence<'flat>,
        collector: &mut RecordLeafCollector<'flat>,
    ) -> Result<(), ToDaeError> {
        let record = occurrence.record;
        let class = occurrence.class;
        if !collector.active.insert(record.type_def_id) {
            return Err(record_layout_error(
                "record field layout is recursive",
                collector.span,
            ));
        }
        let layout = self
            .flat
            .record_types
            .get(&record.type_def_id)
            .ok_or_else(|| {
                record_layout_error(
                    "record has no exact retained Flat field layout",
                    collector.span,
                )
            })?;
        if layout
            .fields
            .iter()
            .any(|field| self.is_nested_record_array_field(class, field))
        {
            return Err(record_layout_error(
                "nested record arrays require a compact aggregate equation owner",
                collector.span,
            ));
        }
        let child_count = self.children_by_owner.get(&class).map_or(0, Vec::len);
        if child_count != layout.fields.len() {
            return Err(record_layout_error(
                format!(
                    "record class occurrence has {child_count} children for {} exact fields",
                    layout.fields.len()
                ),
                collector.span,
            ));
        }
        for (ordinal, field) in layout.fields.iter().enumerate() {
            let child = self.unique_declared_child(class, field.def_id, collector.span)?;
            collector.projection.push(ordinal);
            collector.declarations.push(field.def_id);
            let result = self.collect_record_field(record, child, field, collector);
            collector.declarations.pop();
            collector.projection.pop();
            result?;
        }
        collector.active.remove(&record.type_def_id);
        Ok(())
    }

    fn is_nested_record_array_field(&self, owner: InstanceId, field: &flat::RecordField) -> bool {
        if field.dims.is_empty() {
            return false;
        }
        let Some(children) = self.children_by_declaration.get(&(owner, field.def_id)) else {
            return false;
        };
        children.len() > 1
            && children
                .iter()
                .all(|child| self.is_record_occurrence(*child))
    }

    fn is_record_occurrence(&self, child: InstanceId) -> bool {
        let Some(relation) = self.flat.instance_relations.get(&child) else {
            return false;
        };
        relation.kind == flat::InstanceKind::Aggregate && self.records.contains_key(&child)
    }

    fn collect_record_field(
        &self,
        owner: &flat::RecordInstance,
        child: InstanceId,
        field: &flat::RecordField,
        collector: &mut RecordLeafCollector<'flat>,
    ) -> Result<(), ToDaeError> {
        let kind = self
            .flat
            .instance_relations
            .get(&child)
            .ok_or_else(|| {
                record_layout_error("record field occurrence is absent", collector.span)
            })?
            .kind;
        match kind {
            flat::InstanceKind::Materialized => {
                let variable = self.variables.get(&child).copied().ok_or_else(|| {
                    record_layout_error(
                        "materialized record field has no exact coordinate",
                        collector.span,
                    )
                })?;
                require_materialized_field(self.flat, owner, variable, field, collector.span)?;
                collector.leaves.push(RecordLeaf {
                    variable,
                    projection: collector.projection.clone().into_boxed_slice(),
                    identity: RecordEquationLeafIdentity {
                        declarations: collector.declarations.clone().into_boxed_slice(),
                        records: collector.records.clone().into_boxed_slice(),
                    },
                });
                Ok(())
            }
            flat::InstanceKind::Aggregate => {
                let nested = self.records.get(&child).copied().ok_or_else(|| {
                    record_layout_error(
                        "aggregate record field has no exact record occurrence",
                        collector.span,
                    )
                })?;
                if !declared_effective_type(self.flat, field)
                    || nested.type_def_id != field.type_def_id
                    || !exact_effective_type(
                        self.flat,
                        nested.effective_type_id,
                        &field.effective_type,
                    )
                    || !nested.dims.is_empty()
                    || nested.dims != field.dims
                    || !extends_record_reference(&owner.component_ref, &nested.component_ref, field)
                {
                    return Err(record_layout_error(
                        "nested record arrays require a compact aggregate equation owner",
                        collector.span,
                    ));
                }
                let occurrence = self.check_record_occurrence(nested, collector.span)?;
                collector.records.push(nominal_identity(nested));
                let result = self.collect_record_leaves(occurrence, collector);
                collector.records.pop();
                result
            }
            flat::InstanceKind::Class => Err(record_layout_error(
                "record field declaration resolves to an unowned class occurrence",
                collector.span,
            )),
        }
    }

    fn unique_declared_child(
        &self,
        owner: InstanceId,
        declaration: DefId,
        span: Span,
    ) -> Result<InstanceId, ToDaeError> {
        let children = self
            .children_by_declaration
            .get(&(owner, declaration))
            .map(Vec::as_slice)
            .unwrap_or(&[]);
        let [child] = children else {
            return Err(record_layout_error(
                format!(
                    "field declaration {} has {} concrete occurrences instead of one exact scalar occurrence",
                    declaration.index(),
                    children.len()
                ),
                span,
            ));
        };
        Ok(*child)
    }

    fn check_record_occurrence(
        &self,
        record: &'flat flat::RecordInstance,
        span: Span,
    ) -> Result<CheckedRecordOccurrence<'flat>, ToDaeError> {
        let relation = self.flat.instance_relations.get(&record.instance_id);
        let effective = self.flat.effective_types.get(&record.effective_type_id);
        let declared_type = self.flat.type_ids_by_def_id.get(&record.type_def_id);
        let layout = self.flat.record_types.get(&record.type_def_id);
        let component_declaration = record.component_ref.target_def_id();
        let children = self
            .children_by_owner
            .get(&record.instance_id)
            .map(Vec::as_slice)
            .unwrap_or(&[]);
        let class = match children {
            [class] => Some(*class),
            _ => None,
        };
        let class_relation = class.and_then(|class| self.flat.instance_relations.get(&class));
        let class_declared_type = class_relation
            .and_then(|relation| relation.declaration)
            .and_then(|declaration| self.flat.type_ids_by_def_id.get(&declaration));
        let exact = relation.is_some_and(|relation| {
            relation.kind == flat::InstanceKind::Aggregate
                && relation.declaration == Some(component_declaration)
                && layout.is_some_and(|layout| layout.name == record.type_name)
        }) && effective.is_some_and(|effective| {
            effective.dimensions() == record.dims
                && declared_type == Some(&effective.canonical_type())
                && self.flat.type_roots.get(&record.effective_type_id)
                    == Some(&effective.canonical_type())
                && self.flat.type_roots.get(&effective.nominal_type())
                    == Some(&effective.canonical_type())
                && self.flat.type_roots.get(&effective.canonical_type())
                    == Some(&effective.canonical_type())
                && class_declared_type == Some(&effective.nominal_type())
                && class_relation.is_some_and(|relation| {
                    relation.kind == flat::InstanceKind::Class && relation.indices.is_empty()
                })
        });
        if exact && let Some(class) = class {
            Ok(CheckedRecordOccurrence { record, class })
        } else {
            Err(record_layout_error(
                "record occurrence conflicts with its exact aggregate, class, or effective type identity",
                span,
            ))
        }
    }
}

fn nominal_identity(record: &flat::RecordInstance) -> RecordEquationNominalIdentity {
    RecordEquationNominalIdentity {
        type_def_id: record.type_def_id,
        effective_type_id: record.effective_type_id,
    }
}

fn record_layout_error(detail: impl Into<String>, span: Span) -> ToDaeError {
    ToDaeError::unsupported_flat("record equation layout", detail, span)
}

#[derive(Clone, Copy)]
struct RecordCall<'scope> {
    expression: &'scope Expression,
    name: &'scope rumoca_core::Reference,
    span: Span,
    is_constructor: bool,
}

enum RecordCallClassification {
    NotRecord,
    Record(DefId),
    Invalid(ToDaeError),
}

enum RecordOperands<'flat, 'expression> {
    Records {
        left: &'flat flat::RecordInstance,
        right: &'flat flat::RecordInstance,
    },
    Aggregate {
        record: &'flat flat::RecordInstance,
        call: RecordCall<'expression>,
        aggregate: RecordEquationAggregateSide,
    },
    Unsupported,
}

fn record_operands<'flat, 'expression>(
    index: &ModelRecordIndex<'flat>,
    equation: &'expression flat::Equation,
) -> Result<Option<RecordOperands<'flat, 'expression>>, ToDaeError> {
    residual_operands(index, &equation.residual, equation.span)
}

fn residual_operands<'flat, 'expression>(
    index: &ModelRecordIndex<'flat>,
    residual: &'expression Expression,
    span: Span,
) -> Result<Option<RecordOperands<'flat, 'expression>>, ToDaeError> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = residual
    else {
        return Ok(None);
    };
    let left = record_operand(index, lhs, span)?;
    let right = record_operand(index, rhs, span)?;
    let left_call = record_call(lhs);
    let right_call = record_call(rhs);
    let left_call_type = classify_optional_record_call(index.flat, left_call)?;
    let right_call_type = classify_optional_record_call(index.flat, right_call)?;
    match (
        left,
        right,
        left_call,
        right_call,
        left_call_type,
        right_call_type,
    ) {
        (Some(left), Some(right), _, _, _, _) => Ok(Some(RecordOperands::Records { left, right })),
        (
            Some(record),
            None,
            _,
            Some(call),
            _,
            Some(RecordCallClassification::Record(call_record)),
        ) if call_record == record.type_def_id => Ok(Some(RecordOperands::Aggregate {
            record,
            call,
            aggregate: RecordEquationAggregateSide::Right,
        })),
        (
            None,
            Some(record),
            Some(call),
            _,
            Some(RecordCallClassification::Record(call_record)),
            _,
        ) if call_record == record.type_def_id => Ok(Some(RecordOperands::Aggregate {
            record,
            call,
            aggregate: RecordEquationAggregateSide::Left,
        })),
        (Some(_), None, _, _, _, _) | (None, Some(_), _, _, _, _) => {
            Ok(Some(RecordOperands::Unsupported))
        }
        (None, None, _, _, Some(RecordCallClassification::Record(_)), _)
        | (None, None, _, _, _, Some(RecordCallClassification::Record(_))) => {
            Ok(Some(RecordOperands::Unsupported))
        }
        (None, None, _, _, _, _) => Ok(None),
    }
}

fn classify_optional_record_call(
    flat: &flat::Model,
    call: Option<RecordCall<'_>>,
) -> Result<Option<RecordCallClassification>, ToDaeError> {
    match call.map(|call| record_call_type(flat, call)) {
        Some(RecordCallClassification::Invalid(error)) => Err(error),
        classification => Ok(classification),
    }
}

fn record_call_type(flat: &flat::Model, call: RecordCall<'_>) -> RecordCallClassification {
    let Some(resolved) = call.name.resolved_function() else {
        return RecordCallClassification::Invalid(ToDaeError::unsupported_flat(
            "record equation call identity",
            format!(
                "`{}` has no exact function-instance identity",
                call.name.as_str()
            ),
            call.span,
        ));
    };
    let function =
        match rumoca_core::resolve_function_instance(flat.functions.values(), resolved.instance_id)
        {
            Ok(function) => function,
            Err(error) => {
                return RecordCallClassification::Invalid(ToDaeError::unsupported_flat(
                    "record equation call identity",
                    error.to_string(),
                    call.span,
                ));
            }
        };
    if call.is_constructor != function.is_constructor {
        return RecordCallClassification::Invalid(ToDaeError::unsupported_flat(
            "record equation call identity",
            "call constructor kind conflicts with its exact function instance",
            call.span,
        ));
    }
    if function.is_constructor {
        return match function
            .def_id
            .filter(|record| flat.record_types.contains_key(record))
        {
            Some(record) => RecordCallClassification::Record(record),
            None => RecordCallClassification::Invalid(ToDaeError::unsupported_flat(
                "record equation call identity",
                "record constructor has no exact retained record declaration",
                call.span,
            )),
        };
    }
    let Some(result) = function.outputs.first() else {
        return RecordCallClassification::Invalid(ToDaeError::unsupported_flat(
            "record equation call identity",
            "value call has no leading result slot",
            call.span,
        ));
    };
    let claims_record = result.type_class == Some(rumoca_core::ClassType::Record)
        || result
            .type_def_id
            .is_some_and(|record| flat.record_types.contains_key(&record));
    if !claims_record {
        return RecordCallClassification::NotRecord;
    }
    let exact_record = result.type_def_id.filter(|record| {
        flat.record_types.contains_key(record)
            && flat.type_ids_by_def_id.get(record) == Some(&result.effective_type.nominal_type())
            && flat.type_roots.get(&result.effective_type.nominal_type())
                == Some(&result.effective_type.canonical_type())
    });
    match exact_record {
        Some(record) if result.type_class == Some(rumoca_core::ClassType::Record) => {
            RecordCallClassification::Record(record)
        }
        _ => RecordCallClassification::Invalid(ToDaeError::unsupported_flat(
            "record equation call identity",
            "leading record result has contradictory class, declaration, or effective identity",
            call.span,
        )),
    }
}

fn record_operand<'flat>(
    index: &ModelRecordIndex<'flat>,
    expression: &Expression,
    span: Span,
) -> Result<Option<&'flat flat::RecordInstance>, ToDaeError> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return Ok(None);
    };
    if !subscripts.is_empty() {
        return if index.record_by_reference(name, span)?.is_some() {
            Err(ToDaeError::unsupported_flat(
                "record equation",
                "subscripted record equality requires a compact aggregate owner",
                span,
            ))
        } else {
            Ok(None)
        };
    }
    index.record_by_reference(name, span)
}

fn record_call(expression: &Expression) -> Option<RecordCall<'_>> {
    let Expression::FunctionCall {
        name,
        is_constructor,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span,
        ..
    } = expression
    else {
        return None;
    };
    Some(RecordCall {
        expression,
        name,
        span: *span,
        is_constructor: *is_constructor,
    })
}

fn equation_constructor<'flat>(
    flat: &'flat flat::Model,
    record: &flat::RecordInstance,
    call: RecordCall<'_>,
    equation_span: Span,
) -> Result<&'flat rumoca_core::Function, ToDaeError> {
    let resolved = call.name.resolved_function().ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "record equation call identity",
            format!(
                "`{}` has no exact function-instance identity",
                call.name.as_str()
            ),
            call.span,
        )
    })?;
    let function =
        rumoca_core::resolve_function_instance(flat.functions.values(), resolved.instance_id)
            .map_err(|error| {
                ToDaeError::unsupported_flat(
                    "record equation call identity",
                    error.to_string(),
                    call.span,
                )
            })?;
    if call.is_constructor {
        if function.is_constructor && function.def_id == Some(record.type_def_id) {
            return Ok(function);
        }
        return Err(record_layout_error(
            "constructor call does not own the target record's exact nominal identity",
            call.span,
        ));
    }
    require_record_result(flat, record, function, equation_span)?;
    rumoca_core::resolve_record_constructor(
        flat.functions.values(),
        &record.type_name,
        record.type_def_id,
    )
    .map_err(|error| record_layout_error(error.to_string(), equation_span))
}

fn require_record_result(
    flat: &flat::Model,
    record: &flat::RecordInstance,
    function: &rumoca_core::Function,
    span: Span,
) -> Result<(), ToDaeError> {
    let Some(result) = function.outputs.first() else {
        return Err(record_layout_error(
            "whole-record equality requires a leading function result",
            span,
        ));
    };
    let exact_type = flat.effective_types.get(&record.effective_type_id);
    if !function.is_constructor
        && result.type_class == Some(rumoca_core::ClassType::Record)
        && result.type_def_id == Some(record.type_def_id)
        && exact_type == Some(&result.effective_type)
        && result.effective_type.dimensions() == record.dims
    {
        return Ok(());
    }
    Err(record_layout_error(
        "function result and record occurrence have distinct exact nominal type or shape",
        span,
    ))
}
