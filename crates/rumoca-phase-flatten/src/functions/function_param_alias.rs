use super::*;

pub(super) fn function_param_type_alias_dims(
    class_index: &ast::ClassDefIndex<'_>,
    component: &ast::Component,
    source_map: &rumoca_core::SourceMap,
) -> Result<Vec<i64>, FlattenError> {
    let type_name = component.type_name.to_string();
    let span = required_location_span(
        source_map,
        &component.location,
        "function parameter alias dimensions",
    )?;
    let mut current = match exact_alias_class(class_index, &type_name, component.type_name.def_id) {
        Some(class) => class,
        None if component.type_name.def_id.is_some()
            && rumoca_core::is_builtin_type(&type_name) =>
        {
            return Ok(Vec::new());
        }
        None => {
            return Err(FlattenError::missing_resolved_class_metadata(
                &component.name,
                format!("function parameter alias `{type_name}`"),
                span,
            ));
        }
    };
    let mut dims = Vec::new();
    let mut visited_defs = HashSet::new();
    let mut visited_names = HashSet::new();

    loop {
        let class_def = current;
        if let Some(def_id) = class_def.def_id {
            if !visited_defs.insert(def_id) {
                return Err(FlattenError::missing_resolved_class_metadata(
                    &component.name,
                    format!(
                        "cyclic function parameter alias dimension owner `{}`",
                        class_def.name.text
                    ),
                    span,
                ));
            }
        } else if !visited_names.insert(class_def.name.text.to_string()) {
            return Err(FlattenError::missing_resolved_class_metadata(
                &component.name,
                format!(
                    "cyclic unanchored function parameter alias dimension owner `{}`",
                    class_def.name.text
                ),
                span,
            ));
        }

        dims.extend(subscripts_to_param_dims(
            &class_def.array_subscripts,
            class_def.name.text.as_ref(),
            source_map,
        )?);

        if class_def.class_type != rumoca_core::ClassType::Type {
            break;
        }
        let base = match class_def.extends.as_slice() {
            [] => break,
            [base] => base,
            _ => {
                return Err(FlattenError::missing_resolved_class_metadata(
                    &component.name,
                    format!(
                        "function parameter alias `{}` has {} base continuations",
                        class_def.name.text,
                        class_def.extends.len()
                    ),
                    span,
                ));
            }
        };
        let base_name = base.base_name.to_string();
        current = match exact_alias_class(class_index, &base_name, base.base_def_id) {
            Some(class) => class,
            None if base.base_def_id.is_some() && rumoca_core::is_builtin_type(&base_name) => break,
            None => {
                return Err(FlattenError::missing_resolved_class_metadata(
                    &component.name,
                    format!(
                        "unresolved function parameter alias continuation `{base_name}` from `{}`",
                        class_def.name.text
                    ),
                    span,
                ));
            }
        };
    }

    Ok(dims)
}

fn exact_alias_class<'a>(
    class_index: &ast::ClassDefIndex<'a>,
    name: &str,
    def_id: Option<rumoca_core::DefId>,
) -> Option<&'a ast::ClassDef> {
    match def_id {
        Some(def_id) => class_index.get(def_id),
        None => class_index.get_by_qualified_name(name),
    }
}
