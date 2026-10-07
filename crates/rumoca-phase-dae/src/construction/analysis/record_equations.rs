use super::record_equation_elements::{RecordElement, record_array_operand_elements};
use super::*;
use rumoca_core::DefId;

/// The system that owns one field equation of a record equation.
///
/// A record equation is the set of its field equations (MLS 3.7 §8.3.1
/// expands an equality of records member-wise), and each field equation belongs to the
/// system its target's role selects: an Integer, Boolean, or enumeration
/// field such as `state.phase` of `Modelica.Media.Water` is a discrete-valued
/// assignment (Appendix B), a discrete Real field a discrete Real equation,
/// and every other field a continuous residual.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::construction) enum RecordFieldSystem {
    Continuous,
    DiscreteReal,
    DiscreteValue,
}

impl RecordEquationPlan {
    /// The system each field equation belongs to, in field order.
    pub(in crate::construction) fn field_systems<'plan>(
        &'plan self,
        roles: &'plan HashMap<VarName, PlannedRole>,
    ) -> impl Iterator<Item = (&'plan RecordEquationFieldPlan, RecordFieldSystem)> + 'plan {
        self.fields.iter().map(move |field| {
            let system = match roles.get(&field.target) {
                Some(PlannedRole::DiscreteValue) => RecordFieldSystem::DiscreteValue,
                Some(PlannedRole::DiscreteReal) => RecordFieldSystem::DiscreteReal,
                _ => RecordFieldSystem::Continuous,
            };
            (field, system)
        })
    }

    /// The discrete-valued field targets, which one B.1c owner defines.
    pub(in crate::construction) fn discrete_value_targets<'plan>(
        &'plan self,
        roles: &'plan HashMap<VarName, PlannedRole>,
    ) -> impl Iterator<Item = &'plan VarName> + 'plan {
        self.field_systems(roles)
            .filter(|(_, system)| *system == RecordFieldSystem::DiscreteValue)
            .map(|(field, _)| &field.target)
    }
}

pub(super) fn analyze_record_equations(
    flat: &flat::Model,
    equations: &[flat::Equation],
    values: &ShapeEnvironment,
) -> Result<HashMap<usize, RecordEquationPlan>, ToDaeError> {
    let mut plans = HashMap::new();
    for (row, equation) in equations.iter().enumerate() {
        if let Some(plan) = analyze_record_equation(flat, equation, values)? {
            plans.insert(row, plan);
        }
    }
    Ok(plans)
}

fn analyze_record_equation(
    flat: &flat::Model,
    equation: &flat::Equation,
    values: &ShapeEnvironment,
) -> Result<Option<RecordEquationPlan>, ToDaeError> {
    let Some((target, target_name, value)) = record_equation(flat, equation) else {
        return analyze_record_array_equation(flat, equation, values);
    };
    if !target.dims.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            "arrays of records require a compact record-family owner",
            equation.span,
        ));
    }
    let fields = match value {
        RecordEquationValue::Aggregate(call) => {
            let target_leaves = record_leaves(flat, target, target_name.var_name(), equation.span)?;
            validate_constructor_layout(flat, target, call, equation.span)?;
            target_leaves
                .into_iter()
                .map(|leaf| RecordEquationFieldPlan {
                    target: leaf.coordinate,
                    value: RecordEquationFieldValue::AggregateProjection(leaf.projection),
                })
                .collect()
        }
        RecordEquationValue::Record(source, source_name) => record_pair_fields(
            flat,
            &RecordElement {
                record: target,
                name: target_name.var_name().clone(),
            },
            &RecordElement {
                record: source,
                name: source_name.var_name().clone(),
            },
            equation.span,
        )?,
    };
    Ok(Some(RecordEquationPlan { fields }))
}

/// MLS §10.6.1: an equation between two arrays of records is the element-wise
/// whole-record equality of each element pair, in order.
fn analyze_record_array_equation(
    flat: &flat::Model,
    equation: &flat::Equation,
    values: &ShapeEnvironment,
) -> Result<Option<RecordEquationPlan>, ToDaeError> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.residual
    else {
        return Ok(None);
    };

    let (Some(targets), Some(sources)) = (
        record_array_operand_elements(flat, lhs, values),
        record_array_operand_elements(flat, rhs, values),
    ) else {
        return Ok(None);
    };
    if targets.len() != sources.len() {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            format!(
                "record array equality operands have {} and {} elements",
                targets.len(),
                sources.len()
            ),
            equation.span,
        ));
    }
    let mut fields = Vec::new();
    for (target, source) in targets.iter().zip(&sources) {
        if !target.record.dims.is_empty() {
            return Err(ToDaeError::unsupported_flat(
                "record equation",
                "arrays of records require a compact record-family owner",
                equation.span,
            ));
        }
        fields.extend(record_pair_fields(flat, target, source, equation.span)?);
    }
    Ok(Some(RecordEquationPlan { fields }))
}

/// Leaf owners of the whole-record equality `target = source`.
fn record_pair_fields(
    flat: &flat::Model,
    target: &RecordElement<'_>,
    source: &RecordElement<'_>,
    span: Span,
) -> Result<Vec<RecordEquationFieldPlan>, ToDaeError> {
    if !source.record.dims.is_empty() || source.record.type_def_id != target.record.type_def_id {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            "record equality operands have distinct resolved type identities or shapes",
            span,
        ));
    }
    let target_leaves = record_leaves(flat, target.record, &target.name, span)?;
    let source_leaves = record_leaves(flat, source.record, &source.name, span)?;
    if target_leaves.len() != source_leaves.len()
        || target_leaves
            .iter()
            .zip(&source_leaves)
            .any(|(target, source)| {
                target.projection != source.projection
                    || flat.variables[&target.coordinate].dims
                        != flat.variables[&source.coordinate].dims
            })
    {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            "record equality operands have distinct resolved field layouts",
            span,
        ));
    }
    Ok(target_leaves
        .into_iter()
        .zip(source_leaves)
        .map(|(target, source)| RecordEquationFieldPlan {
            target: target.coordinate,
            value: RecordEquationFieldValue::Coordinate(source.coordinate),
        })
        .collect())
}

fn validate_constructor_layout(
    flat: &flat::Model,
    record: &flat::RecordInstance,
    call: RecordCall<'_>,
    equation_span: Span,
) -> Result<(), ToDaeError> {
    let constructor = equation_constructor(flat, record, call, equation_span)?;
    validate_constructor_fields(flat, record, constructor)
}

/// The constructor's inputs are the Flat record's fields, in order, by
/// identity, name and shape.
fn validate_constructor_fields(
    flat: &flat::Model,
    record: &flat::RecordInstance,
    constructor: &rumoca_core::Function,
) -> Result<(), ToDaeError> {
    let record_type = flat.record_types.get(&record.type_def_id).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "record equation",
            format!("`{}` has no resolved Flat field layout", record.type_name),
            record.source_span,
        )
    })?;
    let same_layout = constructor.inputs.len() == record_type.fields.len()
        && constructor
            .inputs
            .iter()
            .zip(&record_type.fields)
            .all(|(input, field)| {
                input.def_id == Some(field.def_id)
                    && input.name == field.name
                    && input.dimensions() == field.dims
            });
    if !same_layout {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            "Flat record field and constructor layouts disagree",
            record.source_span,
        ));
    }
    Ok(())
}

/// The field plans of a whole record receiving one record-valued result of a
/// function: every leaf coordinate of the record reads its field projection
/// of the result (MLS 3.7 section 12.4.3 with section 8.3.1).
pub(in crate::construction) fn record_result_fields(
    flat: &flat::Model,
    record: &flat::RecordInstance,
    name: &VarName,
    result: &rumoca_core::FunctionParam,
    span: Span,
) -> Result<Vec<RecordEquationFieldPlan>, ToDaeError> {
    if result.type_class != Some(rumoca_core::ClassType::Record)
        || result.type_def_id != Some(record.type_def_id)
        || !result.dimensions().is_empty()
        || !record.dims.is_empty()
    {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            "a receiving record and its function result have distinct resolved type identities or shapes",
            span,
        ));
    }
    let constructor = rumoca_core::resolve_record_constructor(
        flat.functions.values(),
        &result.type_name,
        record.type_def_id,
    )
    .map_err(|error| {
        ToDaeError::unsupported_flat(
            "record equation",
            format!("`{}` has no constructor layout: {error}", record.type_name),
            record.source_span,
        )
    })?;
    validate_constructor_fields(flat, record, constructor)?;
    Ok(record_leaves(flat, record, name, span)?
        .into_iter()
        .map(|leaf| RecordEquationFieldPlan {
            target: leaf.coordinate,
            value: RecordEquationFieldValue::AggregateProjection(leaf.projection),
        })
        .collect())
}

struct RecordLeaf {
    coordinate: VarName,
    projection: Box<[RecordProjectionStep]>,
}

fn record_leaves(
    flat: &flat::Model,
    record: &flat::RecordInstance,
    name: &VarName,
    span: Span,
) -> Result<Vec<RecordLeaf>, ToDaeError> {
    let layout = flat.record_types.get(&record.type_def_id).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "record equation",
            format!("`{}` has no resolved Flat field layout", record.type_name),
            record.source_span,
        )
    })?;
    let mut fields = Vec::new();
    for (ordinal, field) in layout.fields.iter().enumerate() {
        collect_field_leaves(
            flat,
            (name, ordinal, field),
            (Vec::new(), &[]),
            span,
            &mut HashSet::new(),
            &mut fields,
        )?;
    }
    Ok(fields)
}

/// The leaf coordinates an unsubscripted reference reads: `name` itself when
/// it is a Flat variable, every leaf field of the record aggregate it names
/// (MLS 3.7 §8.3.1 expands a record equality member-wise), and none for any
/// other name. A malformed record layout reports at the record declaration.
pub(in crate::construction) fn reference_leaf_coordinates(
    flat: &flat::Model,
    name: &VarName,
) -> Result<Vec<VarName>, ToDaeError> {
    if flat.variables.contains_key(name) {
        return Ok(vec![name.clone()]);
    }
    let Some(record) = flat.record_instances.get(name) else {
        return Ok(Vec::new());
    };
    let mut leaves = Vec::new();
    collect_record_equation_leaves(
        flat,
        name,
        Vec::new(),
        &[],
        record.source_span,
        &mut HashSet::new(),
        &mut leaves,
    )?;
    Ok(leaves.into_iter().map(|leaf| leaf.coordinate).collect())
}

/// `prefix` holds the extents of the enclosing arrays of records: Flat holds
/// an array of records as one column per leaf field, whose leading extents
/// are the array's (struct of arrays), and a field projection of an array of
/// records is that column (MLS 3.7 §10.6.1).
fn collect_record_equation_leaves(
    flat: &flat::Model,
    name: &VarName,
    projection: Vec<RecordProjectionStep>,
    prefix: &[i64],
    span: Span,
    active: &mut HashSet<DefId>,
    fields: &mut Vec<RecordLeaf>,
) -> Result<(), ToDaeError> {
    if let Some(variable) = flat.variables.get(name) {
        if !variable.dims.starts_with(prefix) {
            return Err(ToDaeError::unsupported_flat(
                "record equation",
                format!("`{name}` is not one column of extents {prefix:?} of its array of records"),
                span,
            ));
        }
        fields.push(RecordLeaf {
            coordinate: name.clone(),
            projection: projection.into_boxed_slice(),
        });
        return Ok(());
    }
    let instance = flat.record_instances.get(name).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "record equation",
            format!("`{name}` has neither a leaf coordinate nor a nested record layout"),
            span,
        )
    })?;
    let prefix = [prefix, instance.dims.as_slice()].concat();
    if !active.insert(instance.type_def_id) {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            format!("nested record layout `{name}` is recursive"),
            span,
        ));
    }
    let layout = flat
        .record_types
        .get(&instance.type_def_id)
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "record equation",
                format!("nested record `{name}` has no resolved Flat field layout"),
                span,
            )
        })?;
    for (ordinal, field) in layout.fields.iter().enumerate() {
        collect_field_leaves(
            flat,
            (name, ordinal, field),
            (projection.clone(), &prefix),
            span,
            active,
            fields,
        )?;
    }
    active.remove(&instance.type_def_id);
    Ok(())
}

/// The leaves of field `ordinal` of the record `parent`. Flat holds an array
/// of records declared inside a record as one instance per element
/// (`parent.field[i]`, row-major), so such a field is read element by element:
/// each element is one `Element` step of the projection, then its own fields.
fn collect_field_leaves(
    flat: &flat::Model,
    (parent, ordinal, field): (&VarName, usize, &flat::RecordField),
    (mut projection, prefix): (Vec<RecordProjectionStep>, &[i64]),
    span: Span,
    active: &mut HashSet<DefId>,
    fields: &mut Vec<RecordLeaf>,
) -> Result<(), ToDaeError> {
    let child = VarName::new(format!("{parent}.{}", field.name));
    projection.push(RecordProjectionStep::Field(ordinal));
    let held_whole =
        flat.variables.contains_key(&child) || flat.record_instances.contains_key(&child);
    if held_whole || field.dims.is_empty() {
        return collect_record_equation_leaves(
            flat, &child, projection, prefix, span, active, fields,
        );
    }
    for subscripts in element_subscripts(&field.dims) {
        let label = subscripts
            .iter()
            .map(i64::to_string)
            .collect::<Vec<_>>()
            .join(",");
        let element = VarName::new(format!("{child}[{label}]"));
        let mut element_projection = projection.clone();
        element_projection.push(RecordProjectionStep::Element(subscripts.into_boxed_slice()));
        collect_record_equation_leaves(
            flat,
            &element,
            element_projection,
            prefix,
            span,
            active,
            fields,
        )?;
    }
    Ok(())
}

/// Every 1-based subscript tuple of an array of extents `dims`, row-major.
fn element_subscripts(dims: &[i64]) -> Vec<Vec<i64>> {
    dims.iter().fold(vec![Vec::new()], |tuples, &extent| {
        tuples
            .into_iter()
            .flat_map(|tuple| {
                (1..=extent).map(move |index| {
                    let mut next = tuple.clone();
                    next.push(index);
                    next
                })
            })
            .collect()
    })
}

fn equation_constructor<'flat>(
    flat: &'flat flat::Model,
    record: &flat::RecordInstance,
    call: RecordCall<'_>,
    equation_span: Span,
) -> Result<&'flat rumoca_core::Function, ToDaeError> {
    let function = flat
        .functions
        .get(call.name.var_name())
        .ok_or_else(|| ToDaeError::unresolved_reference(call.name.as_str(), call.span))?;
    if call.is_constructor {
        if function.is_constructor {
            return Ok(function);
        }
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            format!("`{}` is not constructor metadata", function.name),
            call.span,
        ));
    }
    let [result] = function.outputs.as_slice() else {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            format!(
                "`{}` must have exactly one record result for whole-record equality",
                function.name
            ),
            call.span,
        ));
    };
    if result.type_class != Some(rumoca_core::ClassType::Record)
        || result.type_def_id != Some(record.type_def_id)
    {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            "record equality operands have distinct resolved type identities",
            equation_span,
        ));
    }
    rumoca_core::resolve_record_constructor(
        flat.functions.values(),
        &result.type_name,
        record.type_def_id,
    )
    .map_err(|error| {
        ToDaeError::unsupported_flat(
            "record equation",
            format!("`{}` has no constructor layout: {error}", record.type_name),
            record.source_span,
        )
    })
}

struct RecordCall<'scope> {
    name: &'scope rumoca_core::Reference,
    span: Span,
    is_constructor: bool,
}

enum RecordEquationValue<'scope> {
    Aggregate(RecordCall<'scope>),
    Record(&'scope flat::RecordInstance, &'scope rumoca_core::Reference),
}

fn record_equation<'scope>(
    flat: &'scope flat::Model,
    equation: &'scope flat::Equation,
) -> Option<(
    &'scope flat::RecordInstance,
    &'scope rumoca_core::Reference,
    RecordEquationValue<'scope>,
)> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.residual
    else {
        return None;
    };
    let Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let record = flat.record_instances.get(name.var_name())?;
    let value = match rhs.as_ref() {
        Expression::FunctionCall {
            name,
            args: _,
            is_constructor,
            span,
        } => RecordEquationValue::Aggregate(RecordCall {
            name,
            span: *span,
            is_constructor: *is_constructor,
        }),
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            RecordEquationValue::Record(flat.record_instances.get(name.var_name())?, name)
        }
        _ => return None,
    };
    Some((record, name, value))
}
