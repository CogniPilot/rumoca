use super::*;

pub(super) fn function_param_type_alias_dims(
    class_index: &ast::ClassDefIndex<'_>,
    component: &ast::Component,
    source_map: &rumoca_core::SourceMap,
) -> Result<Vec<i64>, FlattenError> {
    const MAX_DEPTH: usize = 16;
    let type_name = component.type_name.to_string();
    let mut current = class_by_name_or_def_id(class_index, &type_name, component.type_name.def_id);
    let mut dims = Vec::new();
    let mut visited_defs = HashSet::new();
    let mut visited_names = HashSet::new();

    for _ in 0..MAX_DEPTH {
        let Some(class_def) = current else {
            break;
        };
        if let Some(def_id) = class_def.def_id {
            if !visited_defs.insert(def_id) {
                break;
            }
        } else if !visited_names.insert(class_def.name.text.to_string()) {
            break;
        }

        dims.extend(subscripts_to_param_dims(
            &class_def.array_subscripts,
            class_def.name.text.as_ref(),
            source_map,
        )?);

        let Some(base) = class_def.extends.first() else {
            break;
        };
        let base_name = base.base_name.to_string();
        if rumoca_core::is_builtin_type(&base_name) {
            break;
        }
        current = class_by_name_or_def_id(class_index, &base_name, base.base_def_id);
    }

    Ok(dims)
}
