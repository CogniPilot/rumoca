use super::*;
use rumoca_core::{ExpressionVisitor, Reference, row_major_coordinates};

/// One assignment to a path under a record result or record local.
///
/// `path` is the component path after the record root (never empty); every
/// group offset is relative to the statement slice the assembly owns.
#[derive(Clone, Copy)]
struct RecordFieldWrite<'scope> {
    offset: usize,
    path: &'scope [rumoca_core::ComponentRefPart],
    value: &'scope Expression,
    span: Span,
}

impl RecordFieldWrite<'_> {
    /// Whether this write assigns field `field` of the record at `depth`
    /// whole (or a selection of a tensor field), as opposed to a deeper path.
    fn ends_at(&self, depth: usize) -> bool {
        self.path.len() == depth + 1
    }
}

/// The record a group of field writes assembles at one nesting depth.
struct RecordLevel<'scope> {
    /// Root-relative path of this record, starting with the root name.
    path: Vec<&'scope str>,
    depth: usize,
    fields: &'scope [rumoca_core::FunctionParam],
    /// Extents of the enclosing array-of-records field, when this record is
    /// its element: every value written below it is one column of shape
    /// `prefix ++ field extents` (struct of arrays).
    prefix: Vec<u32>,
}

impl RecordLevel<'_> {
    fn name(&self) -> String {
        self.path.join(".")
    }
}

/// Root fields an assembled value may read: MLS §12.4.4 forbids reading an
/// unassigned part, so values read only root fields completed earlier.
#[derive(Clone, Copy)]
struct RecordReads<'scope> {
    root: &'scope str,
    available: &'scope [VarName],
}

fn record_field_writes<'scope>(
    statements: &'scope [rumoca_core::Statement],
    function: &'scope rumoca_core::Function,
) -> Vec<RecordFieldWrite<'scope>> {
    statements
        .iter()
        .enumerate()
        .filter_map(|(offset, statement)| {
            let rumoca_core::Statement::Assignment { value, span, .. } = statement else {
                return None;
            };
            let (_, path) = record_assignment_target(statement, function)?;
            Some(RecordFieldWrite {
                offset,
                path,
                value,
                span: *span,
            })
        })
        .collect()
}

pub(super) fn plan_staged_record_assemblies(
    statements: &[rumoca_core::Statement],
    context: FunctionValidationContext<'_>,
) -> Result<
    (
        HashMap<usize, FunctionRecordFieldAssemblyPlan>,
        HashSet<usize>,
    ),
    ToDaeError,
> {
    let mut assignments: HashMap<VarName, Vec<usize>> = HashMap::new();
    for (index, statement) in statements.iter().enumerate() {
        if let Some((target, _)) = record_assignment_target(statement, context.function) {
            assignments
                .entry(VarName::new(&target.name))
                .or_default()
                .push(index);
        }
    }
    let mut plans = HashMap::new();
    let mut members = HashSet::new();
    for (target, indices) in assignments {
        if indices.windows(2).all(|pair| pair[1] == pair[0] + 1) {
            continue;
        }
        plan_staged_record(
            statements,
            context,
            &target,
            &indices,
            &mut plans,
            &mut members,
        )?;
    }
    Ok((plans, members))
}

/// The root field a record write names (`b` for `r.b` and `r.b.c`).
fn written_root_field<'scope>(
    statement: &'scope rumoca_core::Statement,
    function: &'scope rumoca_core::Function,
) -> Option<&'scope str> {
    record_assignment_target(statement, function).map(|(_, path)| path[0].ident.as_str())
}

fn plan_staged_record(
    statements: &[rumoca_core::Statement],
    context: FunctionValidationContext<'_>,
    target: &VarName,
    indices: &[usize],
    plans: &mut HashMap<usize, FunctionRecordFieldAssemblyPlan>,
    members: &mut HashSet<usize>,
) -> Result<(), ToDaeError> {
    let declaration = context
        .function
        .outputs
        .iter()
        .chain(&context.function.locals)
        .find(|value| value.name == target.as_str())
        .expect("record assignment target resolves its declaration");
    let constructor = record_constructor(declaration, context)?;
    let field_names = constructor
        .inputs
        .iter()
        .map(|field| VarName::new(&field.name))
        .collect::<Vec<_>>();
    let final_index = *indices.last().expect("staged record has assignments");
    let writes_field = |index: &usize, field: &str| {
        written_root_field(&statements[*index], context.function) == Some(field)
    };
    for field in &constructor.inputs {
        let field_indices = indices
            .iter()
            .copied()
            .filter(|index| writes_field(index, &field.name))
            .collect::<Vec<_>>();
        let first = *field_indices.first().ok_or_else(|| {
            undefined_record_field(context.function, &[target.as_str()], field, None)
        })?;
        if field_indices.windows(2).any(|pair| pair[1] != pair[0] + 1) {
            return Err(ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "`{target}.{}` has non-contiguous partial writes whose statement-time values cannot yet be staged",
                    field.name
                ),
                field.span,
            ));
        }
        let group = field_indices
            .iter()
            .map(|index| statements[*index].clone())
            .collect::<Vec<_>>();
        let available_fields = constructor
            .inputs
            .iter()
            .filter(|candidate| {
                indices
                    .iter()
                    .copied()
                    .filter(|index| writes_field(index, &candidate.name))
                    .max()
                    .is_some_and(|last| last < first)
            })
            .map(|candidate| VarName::new(&candidate.name))
            .collect::<Vec<_>>();
        let writes = record_field_writes(&group, context.function);
        let level = RecordLevel {
            path: vec![target.as_str()],
            depth: 0,
            fields: &constructor.inputs,
            prefix: Vec::new(),
        };
        let reads = RecordReads {
            root: target.as_str(),
            available: &available_fields,
        };
        let field_plan = validate_field_assembly(&writes, &level, field, context, reads)?;
        members.extend(field_indices.iter().copied().skip(1));
        plans.insert(
            first,
            FunctionRecordFieldAssemblyPlan {
                target: target.clone(),
                statement_count: field_indices.len(),
                field: field_plan,
                available_fields,
                finalize_fields: (field_indices.last() == Some(&final_index))
                    .then(|| field_names.clone()),
            },
        );
    }
    Ok(())
}

pub(super) fn validate_record_output_assembly(
    statements: &[rumoca_core::Statement],
    start: usize,
    context: FunctionValidationContext<'_>,
) -> Result<Option<(FunctionRecordAssemblyPlan, usize)>, ToDaeError> {
    let Some((target, path)) = record_assignment_target(&statements[start], context.function)
    else {
        return Ok(None);
    };
    let staged_field = FunctionRecordFieldCoordinate {
        target: VarName::new(&target.name),
        field: VarName::new(&path[0].ident),
    };
    if context.staged_record_fields.contains(&staged_field) {
        return Ok(None);
    }
    let count = statements[start..]
        .iter()
        .take_while(|statement| {
            record_assignment_target(statement, context.function)
                .is_some_and(|(candidate, _)| candidate.name == target.name)
        })
        .count();
    let writes = record_field_writes(&statements[start..start + count], context.function);
    let constructor = record_constructor(target, context)?;
    let level = RecordLevel {
        path: vec![target.name.as_str()],
        depth: 0,
        fields: &constructor.inputs,
        prefix: Vec::new(),
    };
    require_claimed_writes(&writes, &level)?;
    let mut fields = Vec::with_capacity(constructor.inputs.len());
    for field in &constructor.inputs {
        let first = writes
            .iter()
            .position(|write| write_claims_field(write, &level, field))
            .unwrap_or(writes.len());
        let available_fields = constructor
            .inputs
            .iter()
            .filter(|candidate| {
                writes
                    .iter()
                    .enumerate()
                    .filter(|(_, write)| write_claims_field(write, &level, candidate))
                    .map(|(index, _)| index)
                    .max()
                    .is_some_and(|last| last < first)
            })
            .map(|candidate| VarName::new(&candidate.name))
            .collect::<Vec<_>>();
        let reads = RecordReads {
            root: target.name.as_str(),
            available: &available_fields,
        };
        fields.push(validate_field_assembly(
            &writes, &level, field, context, reads,
        )?);
    }
    Ok(Some((
        FunctionRecordAssemblyPlan {
            target: VarName::new(&target.name),
            statement_count: count,
            fields,
            seed: None,
        },
        count,
    )))
}

/// Whether `write` assigns (part of) constructor field `field` of `level`: a
/// record field claims every write through it; a tensor field claims a write
/// that ends at it, or one naming a decomposed record it lies in.
fn write_claims_field(
    write: &RecordFieldWrite<'_>,
    level: &RecordLevel<'_>,
    field: &rumoca_core::FunctionParam,
) -> bool {
    let part = &write.path[level.depth];
    if field.type_class == Some(rumoca_core::ClassType::Record) {
        return part.ident == field.name;
    }
    write.ends_at(level.depth)
        && assigned_field_projection(part, &field.name, level.fields).is_some()
}

/// Every write of an assembly group must define some field of the record it
/// assembles; a write no field claims would otherwise be dropped silently.
fn require_claimed_writes(
    writes: &[RecordFieldWrite<'_>],
    level: &RecordLevel<'_>,
) -> Result<(), ToDaeError> {
    let Some(unclaimed) = writes.iter().find(|write| {
        !level
            .fields
            .iter()
            .any(|field| write_claims_field(write, level, field))
    }) else {
        return Ok(());
    };
    let path = unclaimed.path[level.depth..]
        .iter()
        .map(|part| part.ident.as_str())
        .collect::<Vec<_>>()
        .join(".");
    Err(ToDaeError::unsupported_flat(
        "record output assembly",
        format!(
            "`{}.{path}` writes no field the record assembly represents",
            level.name()
        ),
        unclaimed.span,
    ))
}

/// How an assignment to `record.<part>` writes constructor field `field`:
/// `Some(None)` when it names the field itself, `Some(Some(nested))` when the
/// part names a decomposed nested record whose field `nested` is spelled
/// `<part>_<nested>` in the constructor, and `None` otherwise. A part that is
/// itself a constructor field names only that field, so `zeta1` never writes
/// a sibling such as `zeta1_at_a`.
fn assigned_field_projection(
    part: &rumoca_core::ComponentRefPart,
    field: &str,
    fields: &[rumoca_core::FunctionParam],
) -> Option<Option<VarName>> {
    if part.ident == field {
        return Some(None);
    }
    if fields.iter().any(|candidate| candidate.name == part.ident) {
        return None;
    }
    field
        .strip_prefix(part.ident.as_str())
        .and_then(|suffix| suffix.strip_prefix('_'))
        .filter(|suffix| !suffix.is_empty())
        .map(|nested| Some(VarName::new(nested)))
}

/// The record root and field path an assignment writes, when its root is a
/// record result or record local.
fn record_assignment_target<'scope>(
    statement: &'scope rumoca_core::Statement,
    function: &'scope rumoca_core::Function,
) -> Option<(
    &'scope rumoca_core::FunctionParam,
    &'scope [rumoca_core::ComponentRefPart],
)> {
    let rumoca_core::Statement::Assignment { comp, .. } = statement else {
        return None;
    };
    let [root, path @ ..] = comp.parts() else {
        return None;
    };
    if path.is_empty() {
        return None;
    }
    // MLS §12.2 gives protected locals the same declaration status as results,
    // so a record local is assembled from its field assignments exactly like a
    // record result: neither can be updated field-by-field in the checked DAE,
    // because a partial update would have to read the value's undefined fields.
    let value = function
        .outputs
        .iter()
        .chain(&function.locals)
        .find(|value| {
            value.name == root.ident && value.type_class == Some(rumoca_core::ClassType::Record)
        })?;
    Some((value, path))
}

pub(super) fn record_constructor<'scope>(
    output: &rumoca_core::FunctionParam,
    context: FunctionValidationContext<'scope>,
) -> Result<&'scope rumoca_core::Function, ToDaeError> {
    let type_def_id = output.type_def_id.ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "`{}.{}` has no resolved record type identity",
                context.function.name, output.name
            ),
            output.span,
        )
    })?;
    rumoca_core::resolve_record_constructor(
        context.flat.functions.values(),
        &output.type_name,
        type_def_id,
    )
    .map_err(|error| {
        ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "`{}.{}` has no resolved constructor layout: {error}",
                context.function.name, output.name
            ),
            output.span,
        )
    })
}

/// The declared extents of one record output field and its scalar count.
fn field_scalar_layout(
    output: &str,
    field: &rumoca_core::FunctionParam,
) -> Result<(Vec<u32>, usize), ToDaeError> {
    let dimensions = field
        .dimensions()
        .iter()
        .map(|extent| {
            u32::try_from(*extent).ok().ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "record output assembly",
                    format!("`{output}.{}` has invalid extent `{extent}`", field.name),
                    field.span,
                )
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let scalar_count = dimensions
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "`{output}.{}` exceeds the checked scalar domain",
                    field.name
                ),
                field.span,
            )
        })?;
    Ok((dimensions, scalar_count))
}

fn validate_field_assembly(
    writes: &[RecordFieldWrite<'_>],
    level: &RecordLevel<'_>,
    field: &rumoca_core::FunctionParam,
    context: FunctionValidationContext<'_>,
    reads: RecordReads<'_>,
) -> Result<FunctionRecordFieldAssembly, ToDaeError> {
    if field.type_class == Some(rumoca_core::ClassType::Record) {
        return validate_record_field_assembly(writes, level, field, context, reads);
    }
    let output = level.name();
    let (field_dimensions, _) = field_scalar_layout(&output, field)?;
    let dimensions = [level.prefix.as_slice(), field_dimensions.as_slice()].concat();
    let scalar_count = dimensions
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "`{output}.{}` exceeds the checked scalar domain",
                    field.name
                ),
                field.span,
            )
        })?;
    let scalar_type = effective_function_scalar_type(context.flat, field).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "`{output}.{}` has no scalar tensor element type",
                field.name
            ),
            field.span,
        )
    })?;
    let claims = writes
        .iter()
        .filter(|write| write_claims_field(write, level, field))
        .map(|write| validate_field_write(write, level, field, context, reads, &dimensions))
        .collect::<Result<Vec<_>, _>>()?;
    if let [claim] = claims.as_slice()
        && claim.selection.axes.iter().all(Option::is_none)
    {
        return Ok(FunctionRecordFieldAssembly {
            name: VarName::new(&field.name),
            source: FunctionRecordFieldSource::Whole {
                statement: claim.statement,
                value_field: claim.value_field.clone(),
            },
        });
    }
    let scalars = collect_field_scalar_sources(&claims, &output, field, &dimensions, scalar_count)?;
    let scalars = require_total_field_scalars(context.function, scalars, level, field)?;
    Ok(FunctionRecordFieldAssembly {
        name: VarName::new(&field.name),
        source: FunctionRecordFieldSource::Tensor {
            scalar_type,
            dimensions,
            scalars,
        },
    })
}

/// One validated write of a tensor field: the group statement, the field of
/// a decomposed record value it projects, and the elements it selects.
struct FieldWrite {
    statement: usize,
    value_field: Option<VarName>,
    selection: FieldSelection,
}

fn validate_field_write(
    write: &RecordFieldWrite<'_>,
    level: &RecordLevel<'_>,
    field: &rumoca_core::FunctionParam,
    context: FunctionValidationContext<'_>,
    reads: RecordReads<'_>,
    dimensions: &[u32],
) -> Result<FieldWrite, ToDaeError> {
    let output = level.name();
    let target = &write.path[level.depth];
    let (value, span) = (write.value, write.span);
    let value_field = assigned_field_projection(target, &field.name, level.fields).flatten();
    require_span(span, "record field assignment")?;
    if !level.prefix.is_empty() && !target.subs.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "`{output}.{}` is a column of an array of records and must be assigned whole",
                field.name
            ),
            span,
        ));
    }
    validate_function_subscripts(&target.subs, context)?;
    validate_function_expression_with_roles(value, context.roles, context.flat, context.shapes)?;
    reject_record_self_reference(value, reads, span)?;
    let selection = field_selection(dimensions, &target.subs, span)?;
    let found_shape = if let Some(value_field) = &value_field {
        let projected = Expression::FieldAccess {
            base: Box::new(value.clone()),
            field: value_field.as_str().to_string(),
            field_def_id: field.def_id.ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "record output assembly",
                    format!("`{output}.{}` has no exact field identity", field.name),
                    field.span,
                )
            })?,
            span,
        };
        context
            .shape_analysis
            .expression_shape(&projected, context.shapes)?
    } else {
        context
            .shape_analysis
            .expression_shape(value, context.shapes)?
    };
    if found_shape != selection.value_dimensions {
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "`{output}.{}` selection shape {:?} does not match value shape {:?}",
                field.name, selection.value_dimensions, found_shape
            ),
            span,
        ));
    }
    Ok(FieldWrite {
        statement: write.offset,
        value_field,
        selection,
    })
}

fn collect_field_scalar_sources(
    claims: &[FieldWrite],
    output: &str,
    field: &rumoca_core::FunctionParam,
    dimensions: &[u32],
    scalar_count: usize,
) -> Result<Vec<Option<FunctionRecordScalarSource>>, ToDaeError> {
    let mut scalars = vec![None; scalar_count];
    for claim in claims {
        for (base_scalar, scalar_source) in scalars.iter_mut().enumerate() {
            let base_coordinates = row_major_coordinates(dimensions, base_scalar)
                .expect("validated record field scalar is in range");
            let Some(value_coordinates) = claim
                .selection
                .selected_value_coordinates(&base_coordinates)
            else {
                continue;
            };
            if scalar_source
                .replace(FunctionRecordScalarSource {
                    statement_offset: claim.statement,
                    value_field: claim.value_field.clone(),
                    value_coordinates,
                })
                .is_some()
            {
                return Err(ToDaeError::unsupported_flat(
                    "record output assembly",
                    format!("`{output}.{}` is assigned more than once", field.name),
                    field.span,
                ));
            }
        }
    }
    Ok(scalars)
}

fn require_total_field_scalars(
    function: &rumoca_core::Function,
    scalars: Vec<Option<FunctionRecordScalarSource>>,
    level: &RecordLevel<'_>,
    field: &rumoca_core::FunctionParam,
) -> Result<Vec<FunctionRecordScalarSource>, ToDaeError> {
    scalars
        .into_iter()
        .enumerate()
        .map(|(scalar, source)| {
            source.ok_or_else(|| {
                undefined_record_field(function, &level.path, field, Some(scalar + 1))
            })
        })
        .collect()
}

/// A record-typed field is either assigned whole by one statement or
/// assembled from writes to its own fields, recursively; a field written both
/// ways would need a partial update of a defined record value.
fn validate_record_field_assembly(
    writes: &[RecordFieldWrite<'_>],
    level: &RecordLevel<'_>,
    field: &rumoca_core::FunctionParam,
    context: FunctionValidationContext<'_>,
    reads: RecordReads<'_>,
) -> Result<FunctionRecordFieldAssembly, ToDaeError> {
    let output = level.name();
    let field_writes = writes
        .iter()
        .copied()
        .filter(|write| write_claims_field(write, level, field))
        .collect::<Vec<_>>();
    let (whole, nested): (Vec<_>, Vec<_>) = field_writes
        .iter()
        .copied()
        .partition(|write| write.ends_at(level.depth));
    if nested.is_empty() {
        return validate_aggregate_field_assembly(&whole, level, field, context, reads);
    }
    if let Some(write) = whole.first() {
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "`{output}.{}` is assigned both whole and field by field",
                field.name
            ),
            write.span,
        ));
    }
    if let Some(write) = nested
        .iter()
        .find(|write| !write.path[level.depth].subs.is_empty())
    {
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "`{output}.{}` is a record field and its fields must be written without subscripting it",
                field.name
            ),
            write.span,
        ));
    }
    let (extents, _) = field_scalar_layout(&output, field)?;
    if !extents.is_empty() && !level.prefix.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "`{output}.{}` is an array of records inside an array of records, whose columns the record assembly does not represent",
                field.name
            ),
            field.span,
        ));
    }
    let constructor = record_constructor(field, context)?;
    let mut path = level.path.clone();
    path.push(field.name.as_str());
    let nested_level = RecordLevel {
        path,
        depth: level.depth + 1,
        fields: &constructor.inputs,
        prefix: [level.prefix.as_slice(), extents.as_slice()].concat(),
    };
    require_claimed_writes(&nested, &nested_level)?;
    let fields = constructor
        .inputs
        .iter()
        .map(|nested_field| {
            validate_field_assembly(&nested, &nested_level, nested_field, context, reads)
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(FunctionRecordFieldAssembly {
        name: VarName::new(&field.name),
        source: FunctionRecordFieldSource::Record { fields, extents },
    })
}

fn validate_aggregate_field_assembly(
    writes: &[RecordFieldWrite<'_>],
    level: &RecordLevel<'_>,
    field: &rumoca_core::FunctionParam,
    context: FunctionValidationContext<'_>,
    reads: RecordReads<'_>,
) -> Result<FunctionRecordFieldAssembly, ToDaeError> {
    let output = level.name();
    let mut source = None;
    for write in writes {
        let target = &write.path[level.depth];
        require_span(write.span, "record aggregate field assignment")?;
        if !target.subs.is_empty() {
            return Err(ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "`{output}.{}` is a record field and must be assigned whole",
                    field.name
                ),
                write.span,
            ));
        }
        validate_function_expression_with_roles(
            write.value,
            context.roles,
            context.flat,
            context.shapes,
        )?;
        reject_record_self_reference(write.value, reads, write.span)?;
        if source.replace(write.offset).is_some() {
            return Err(ToDaeError::unsupported_flat(
                "record output assembly",
                format!("`{output}.{}` is assigned more than once", field.name),
                write.span,
            ));
        }
    }
    let statement =
        source.ok_or_else(|| undefined_record_field(context.function, &level.path, field, None))?;
    Ok(FunctionRecordFieldAssembly {
        name: VarName::new(&field.name),
        source: FunctionRecordFieldSource::Aggregate { statement },
    })
}

struct FieldSelection {
    axes: Vec<Option<u32>>,
    value_dimensions: Vec<u32>,
}

impl FieldSelection {
    fn selected_value_coordinates(&self, base: &[u32]) -> Option<Vec<u32>> {
        let mut value = Vec::with_capacity(self.value_dimensions.len());
        for (coordinate, selected) in base.iter().copied().zip(&self.axes) {
            match selected {
                Some(expected) if *expected != coordinate => return None,
                Some(_) => {}
                None => value.push(coordinate),
            }
        }
        Some(value)
    }
}

fn field_selection(
    dimensions: &[u32],
    subscripts: &[Subscript],
    span: Span,
) -> Result<FieldSelection, ToDaeError> {
    if subscripts.len() > dimensions.len() {
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            "field assignment has more subscripts than its rank",
            span,
        ));
    }
    let mut axes = Vec::with_capacity(dimensions.len());
    let mut value_dimensions = Vec::new();
    for (axis, extent) in dimensions.iter().copied().enumerate() {
        let selected = match subscripts.get(axis) {
            Some(Subscript::Index { value, .. }) => Some(*value),
            Some(Subscript::Expr { expr, .. }) => match expr.as_ref() {
                Expression::Literal {
                    value: Literal::Integer(value),
                    ..
                } => Some(*value),
                _ => {
                    return Err(ToDaeError::unsupported_flat(
                        "record output assembly",
                        "field coverage requires literal Integer indices or whole axes",
                        span,
                    ));
                }
            },
            Some(Subscript::Colon { .. }) | None => None,
        };
        match selected {
            Some(index) => {
                let coordinate = u32::try_from(index)
                    .ok()
                    .and_then(|index| index.checked_sub(1))
                    .filter(|index| *index < extent)
                    .ok_or_else(|| {
                        ToDaeError::unsupported_flat(
                            "record output assembly",
                            format!("field index `{index}` exceeds axis extent `{extent}`"),
                            span,
                        )
                    })?;
                axes.push(Some(coordinate));
            }
            None => {
                axes.push(None);
                value_dimensions.push(extent);
            }
        }
    }
    Ok(FieldSelection {
        axes,
        value_dimensions,
    })
}

fn reject_record_self_reference(
    value: &Expression,
    reads: RecordReads<'_>,
    span: Span,
) -> Result<(), ToDaeError> {
    let available = reads.available.iter().cloned().collect::<HashSet<_>>();
    let mut checker = RecordSelfReadChecker {
        output: reads.root,
        available: &available,
        unavailable: None,
    };
    checker.visit_expression(value);
    if let Some(reference) = checker.unavailable {
        let available = reads
            .available
            .iter()
            .map(VarName::as_str)
            .collect::<Vec<_>>()
            .join(", ");
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "`{reference}` is read before that record field is constructed; fields proven available here: [{available}]"
            ),
            span,
        ));
    }
    Ok(())
}

struct RecordSelfReadChecker<'scope> {
    output: &'scope str,
    available: &'scope HashSet<VarName>,
    unavailable: Option<String>,
}

impl ExpressionVisitor for RecordSelfReadChecker<'_> {
    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[Subscript]) {
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
        self.check_reference(name.as_str());
    }

    fn visit_field_access(&mut self, base: &Expression, field: &str) {
        if let Some(base_path) = rumoca_core::flat_expression_component_path(base)
            && (base_path.as_str() == self.output
                || base_path.as_str().starts_with(&format!("{}.", self.output))
                || base_path.as_str().starts_with(&format!("{}[", self.output)))
        {
            self.check_reference(&format!("{base_path}.{field}"));
            return;
        }
        self.visit_expression(base);
    }
}

impl RecordSelfReadChecker<'_> {
    fn check_reference(&mut self, reference: &str) {
        if reference == self.output || reference.starts_with(&format!("{}[", self.output)) {
            self.unavailable
                .get_or_insert_with(|| reference.to_string());
            return;
        }
        let Some(field) = reference
            .strip_prefix(self.output)
            .and_then(|suffix| suffix.strip_prefix('.'))
            .and_then(|suffix| suffix.split(['.', '[']).next())
        else {
            return;
        };
        if !self.available.contains(&VarName::new(field)) {
            self.unavailable
                .get_or_insert_with(|| reference.to_string());
        }
    }
}

/// The refusal for a record field the assembly finds no value for.
///
/// MLS 3.7 §12.4.4: "it is an error to use or return an uninitialized
/// variable". Flattening already assigns every field default the algorithm
/// does not overwrite, so a field no statement anywhere in the body writes is
/// returned uninitialized: a model error. A field the body does write, only in
/// a form the assembly does not represent (inside a loop, for example), stays
/// an unsupported construct. `record` is the root-relative path of the record
/// that declares `field`.
fn undefined_record_field(
    function: &rumoca_core::Function,
    record: &[&str],
    field: &rumoca_core::FunctionParam,
    scalar: Option<usize>,
) -> ToDaeError {
    let output = record.join(".");
    let place = match scalar {
        Some(scalar) => format!("scalar {scalar} of `{output}.{}`", field.name),
        None => format!("`{output}.{}`", field.name),
    };
    if statements_write_field(&function.body, record, &field.name) {
        return ToDaeError::unsupported_flat(
            "record output assembly",
            format!("{place} has no value the record assembly represents"),
            field.span,
        );
    }
    ToDaeError::UninitializedFunctionValue {
        detail: format!(
            "{place} of `{}` is never assigned and its record field has no default",
            function.name
        ),
        span: field.span,
    }
}

/// Whether any statement may write `field` of the record at path `record`:
/// a write through the field itself, or a whole write of the field or of a
/// record enclosing it.
fn statements_write_field(
    statements: &[rumoca_core::Statement],
    record: &[&str],
    field: &str,
) -> bool {
    let writes = |comp: &rumoca_core::ComponentReference| {
        let parts = comp.parts();
        parts
            .iter()
            .zip(record)
            .all(|(part, name)| part.ident == *name)
            && parts
                .get(record.len())
                .is_none_or(|part| part_names_field(&part.ident, field))
    };
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::Assignment { comp, .. } => writes(comp),
        rumoca_core::Statement::FunctionCall { outputs, .. } => {
            outputs.iter().flatten().any(&writes)
        }
        rumoca_core::Statement::For { equations, .. } => {
            statements_write_field(equations, record, field)
        }
        rumoca_core::Statement::While { block, .. } => {
            statements_write_field(&block.stmts, record, field)
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| statements_write_field(&block.stmts, record, field))
                || else_block
                    .as_deref()
                    .is_some_and(|block| statements_write_field(block, record, field))
        }
        rumoca_core::Statement::When { blocks, .. } => blocks
            .iter()
            .any(|block| statements_write_field(&block.stmts, record, field)),
        _ => false,
    })
}

/// Whether an assigned path segment names `field` itself or a record that a
/// decomposed `field` lies in (`state` for `state_p`); a conservative test, so
/// a field it cannot rule out is never reported uninitialized.
fn part_names_field(assigned: &str, field: &str) -> bool {
    assigned == field
        || field
            .strip_prefix(assigned)
            .is_some_and(|suffix| suffix.starts_with('_'))
}
