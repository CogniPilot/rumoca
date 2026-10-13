use super::*;

pub(super) fn validate_function_value_type(
    value: &rumoca_core::FunctionParam,
    function: &rumoca_core::Function,
    flat: &flat::Model,
    active_records: &mut HashSet<rumoca_core::DefId>,
) -> Result<(), ToDaeError> {
    if effective_function_scalar_type(flat, value).is_some() {
        return Ok(());
    }
    if let Some(type_def_id) = value.type_def_id
        && crate::construction::native_tables::native_table_family(flat, type_def_id).is_some()
    {
        // MLS §12.9.7: a native standard-library table handle is an opaque
        // ExternalObject. It carries no readable Real field, so it is accepted
        // as an opaque integer table id rather than decomposed like a record.
        return Ok(());
    }
    if value.type_class != Some(rumoca_core::ClassType::Record) {
        return Err(unsupported_type(value, function));
    }
    let type_def_id = value.type_def_id.ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "function value type",
            format!(
                "`{}.{}` has no resolved record type identity",
                function.name, value.name
            ),
            value.span,
        )
    })?;
    if !active_records.insert(type_def_id) {
        return Err(ToDaeError::unsupported_flat(
            "function value type",
            format!(
                "`{}.{}` belongs to a recursive value-record cycle",
                function.name, value.name
            ),
            value.span,
        ));
    }
    let constructor = rumoca_core::resolve_record_constructor(
        flat.functions.values(),
        &value.type_name,
        type_def_id,
    )
    .map_err(|error| {
        ToDaeError::unsupported_flat(
            "function value type",
            format!(
                "`{}.{}` has no resolved record constructor: {error}",
                function.name, value.name
            ),
            value.span,
        )
    })?;
    for field in &constructor.inputs {
        validate_function_value_type(field, function, flat, active_records)?;
    }
    active_records.remove(&type_def_id);
    Ok(())
}

fn unsupported_type(
    value: &rumoca_core::FunctionParam,
    function: &rumoca_core::Function,
) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "function value type",
        format!(
            "`{}.{}` has unsupported type `{}`",
            function.name, value.name, value.type_name
        ),
        value.span,
    )
}

/// Leaf field projections of one record-typed function value.
///
/// MLS §12.2 lets a function body read a declared field of a record value
/// (`r.x`). Flat renders that read as one reference whose `VarName` joins the
/// declared value name with the field path, so the DAE symbol tables — planned
/// roles and the shape environment — need the same joined identities the read
/// carries. The paths are derived from the record constructor's declared field
/// order, which is the same order `function_value_type` interns into the DAE
/// record layout, so a projection name and its DAE field ordinal always agree.
///
/// A value that is not a record contributes nothing. Recursion is bounded by
/// `validate_function_value_type`, which rejects recursive value records before
/// any caller reaches this function.
pub(in crate::construction) fn record_field_projections<'flat>(
    value: &rumoca_core::FunctionParam,
    flat: &'flat flat::Model,
) -> Vec<(VarName, VarName, &'flat rumoca_core::FunctionParam)> {
    let mut projections = Vec::new();
    collect_record_field_projections(value, value.name.as_str(), flat, &mut projections);
    projections
}

/// The result or local value that owns each record field path of `function`,
/// so a read of `r.x` finds `r` without re-deriving every value's projections.
/// A path two values would both project belongs to the first, in output then
/// local order.
pub(in crate::construction) fn record_field_roots(
    function: &rumoca_core::Function,
    flat: &flat::Model,
) -> HashMap<VarName, VarName> {
    let mut roots = HashMap::new();
    for value in function.outputs.iter().chain(&function.locals) {
        let root = VarName::new(&value.name);
        for (path, _, _) in record_field_projections(value, flat) {
            roots.entry(path).or_insert_with(|| root.clone());
        }
    }
    roots
}

fn collect_record_field_projections<'flat>(
    value: &rumoca_core::FunctionParam,
    path: &str,
    flat: &'flat flat::Model,
    projections: &mut Vec<(VarName, VarName, &'flat rumoca_core::FunctionParam)>,
) {
    if value.type_class != Some(rumoca_core::ClassType::Record) {
        return;
    }
    let Some(type_def_id) = value.type_def_id else {
        return;
    };
    let Ok(constructor) = rumoca_core::resolve_record_constructor(
        flat.functions.values(),
        &value.type_name,
        type_def_id,
    ) else {
        return;
    };
    for field in &constructor.inputs {
        let field_path = format!("{path}.{}", field.name);
        projections.push((VarName::new(&field_path), VarName::new(path), field));
        collect_record_field_projections(field, &field_path, flat, projections);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{ClassType, DefId, EffectiveType, FunctionParam, Span, TypeId};

    const RECORD_DEF: u32 = 7;

    fn real(name: &str) -> FunctionParam {
        let real = EffectiveType::new(TypeId(1), TypeId(1), Vec::new()).unwrap();
        FunctionParam::new(name, "Real", real, Span::DUMMY)
    }

    fn record_value(name: &str) -> FunctionParam {
        let record = EffectiveType::new(TypeId(2), TypeId(2), Vec::new()).unwrap();
        let mut value = FunctionParam::new(name, "Pkg.R", record, Span::DUMMY);
        value.type_class = Some(ClassType::Record);
        value.type_def_id = Some(DefId::new(RECORD_DEF));
        value
    }

    #[test]
    fn record_field_roots_name_the_owning_result_or_local_of_each_field_path() {
        let mut constructor = rumoca_core::Function::new("Pkg.R", Span::DUMMY);
        constructor.def_id = Some(DefId::new(RECORD_DEF));
        constructor.is_constructor = true;
        constructor.add_input(real("a"));
        constructor.add_input(real("b"));
        let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
        function.add_input(record_value("u"));
        function.add_output(record_value("r"));
        function.add_local(record_value("s"));
        let mut flat = flat::Model::default();
        flat.add_function(constructor);

        let roots = record_field_roots(&function, &flat);

        let expected = [("r.a", "r"), ("r.b", "r"), ("s.a", "s"), ("s.b", "s")];
        assert_eq!(roots.len(), expected.len());
        for (path, root) in expected {
            assert_eq!(roots[&VarName::new(path)], VarName::new(root), "{path}");
        }
        // Inputs are never written, so they own no mutable root.
        assert!(!roots.contains_key(&VarName::new("u.a")));
    }
}
