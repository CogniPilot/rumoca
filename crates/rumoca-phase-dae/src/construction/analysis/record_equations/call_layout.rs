use super::*;

pub(super) fn validate_call_layout(
    flat: &flat::Model,
    record: &flat::RecordInstance,
    call: RecordCall<'_>,
    leaves: &[RecordLeaf<'_>],
    equation_span: Span,
) -> Result<(), ToDaeError> {
    let constructor = equation_constructor(flat, record, call, equation_span)?;
    let expected = constructor_leaves(flat, record.type_def_id, constructor, equation_span)?;
    let same = expected.len() == leaves.len()
        && expected.iter().zip(leaves).all(|(expected, actual)| {
            expected.projection == actual.projection
                && expected.declarations == actual.identity.declarations
                && same_expected_nominal_path(flat, &expected.records, &actual.identity.records)
                && flat.effective_types.get(&actual.variable.type_id) == Some(&expected.value_type)
        });
    if same {
        Ok(())
    } else {
        Err(record_layout_error(
            "record occurrence and returned aggregate do not have one complete exact field layout",
            equation_span,
        ))
    }
}

struct ExpectedLeaf {
    projection: Box<[usize]>,
    declarations: Box<[DefId]>,
    records: Box<[ExpectedNominalIdentity]>,
    value_type: EffectiveType,
}

struct ConstructorLeafCollector {
    projection: Vec<usize>,
    declarations: Vec<DefId>,
    records: Vec<ExpectedNominalIdentity>,
    active: HashSet<DefId>,
    leaves: Vec<ExpectedLeaf>,
    span: Span,
}

impl ConstructorLeafCollector {
    fn new(record: DefId, span: Span) -> Self {
        Self {
            projection: Vec::new(),
            declarations: Vec::new(),
            records: vec![ExpectedNominalIdentity {
                type_def_id: record,
                effective_type: None,
            }],
            active: HashSet::new(),
            leaves: Vec::new(),
            span,
        }
    }
}

#[derive(Clone)]
struct ExpectedNominalIdentity {
    type_def_id: DefId,
    effective_type: Option<EffectiveType>,
}

fn same_expected_nominal_path(
    flat: &flat::Model,
    expected: &[ExpectedNominalIdentity],
    actual: &[RecordEquationNominalIdentity],
) -> bool {
    expected.len() == actual.len()
        && expected.iter().zip(actual).all(|(expected, actual)| {
            expected.type_def_id == actual.type_def_id
                && expected.effective_type.as_ref().is_none_or(|expected| {
                    flat.effective_types.get(&actual.effective_type_id) == Some(expected)
                })
        })
}

fn constructor_leaves(
    flat: &flat::Model,
    record: DefId,
    constructor: &rumoca_core::Function,
    span: Span,
) -> Result<Vec<ExpectedLeaf>, ToDaeError> {
    let mut collector = ConstructorLeafCollector::new(record, span);
    collect_constructor_leaves(flat, record, constructor, &mut collector)?;
    Ok(collector.leaves)
}

fn collect_constructor_leaves(
    flat: &flat::Model,
    record: DefId,
    constructor: &rumoca_core::Function,
    collector: &mut ConstructorLeafCollector,
) -> Result<(), ToDaeError> {
    if !collector.active.insert(record) {
        return Err(record_layout_error(
            "constructor field layout is recursive",
            collector.span,
        ));
    }
    let layout = flat.record_types.get(&record).ok_or_else(|| {
        record_layout_error(
            "constructor record has no retained Flat field layout",
            collector.span,
        )
    })?;
    if constructor.inputs.len() != layout.fields.len() {
        return Err(record_layout_error(
            "record constructor arity disagrees with the retained field layout",
            collector.span,
        ));
    }
    for (ordinal, (field, input)) in layout.fields.iter().zip(&constructor.inputs).enumerate() {
        collector.projection.push(ordinal);
        collector.declarations.push(field.def_id);
        let result = collect_constructor_field(flat, field, input, collector);
        collector.declarations.pop();
        collector.projection.pop();
        result?;
    }
    collector.active.remove(&record);
    Ok(())
}

fn collect_constructor_field(
    flat: &flat::Model,
    field: &flat::RecordField,
    input: &rumoca_core::FunctionParam,
    collector: &mut ConstructorLeafCollector,
) -> Result<(), ToDaeError> {
    if input.def_id != Some(field.def_id)
        || input.type_def_id != Some(field.type_def_id)
        || input.name != field.name
        || input.effective_type != field.effective_type
        || input.effective_type.dimensions() != field.dims
    {
        return Err(record_layout_error(
            "record constructor field identity, order, or dimensions disagree with Flat",
            collector.span,
        ));
    }
    if input.type_class != Some(rumoca_core::ClassType::Record) {
        collector.leaves.push(ExpectedLeaf {
            projection: collector.projection.clone().into_boxed_slice(),
            declarations: collector.declarations.clone().into_boxed_slice(),
            records: collector.records.clone().into_boxed_slice(),
            value_type: input.effective_type.clone(),
        });
        return Ok(());
    }
    if !field.dims.is_empty() {
        return Err(record_layout_error(
            "nested record arrays require a compact aggregate equation owner",
            collector.span,
        ));
    }
    let nested = input.type_def_id.ok_or_else(|| {
        record_layout_error(
            "nested constructor field has no exact record identity",
            collector.span,
        )
    })?;
    let constructor =
        rumoca_core::resolve_record_constructor(flat.functions.values(), &input.type_name, nested)
            .map_err(|error| record_layout_error(error.to_string(), collector.span))?;
    collector.records.push(ExpectedNominalIdentity {
        type_def_id: nested,
        effective_type: Some(input.effective_type.clone()),
    });
    let result = collect_constructor_leaves(flat, nested, constructor, collector);
    collector.records.pop();
    result
}
