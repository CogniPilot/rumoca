use rumoca_ir_ast as ast;
use rustc_hash::FxHashMap;

pub(crate) fn lookup_local_integer(
    comp_ref: &ast::ComponentReference,
    local_values: &FxHashMap<String, i64>,
) -> Option<i64> {
    if comp_ref.parts.iter().any(|part| part.subs.is_some()) {
        return None;
    }
    let dotted = comp_ref
        .parts
        .iter()
        .map(|p| p.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    if let Some(value) = local_values.get(&dotted) {
        return Some(*value);
    }
    if comp_ref.parts.len() == 1 {
        let name = comp_ref.parts[0].ident.text.as_ref();
        return local_values.get(name).copied();
    }
    None
}

pub(crate) fn lookup_local_bool(
    comp_ref: &ast::ComponentReference,
    local_values: &FxHashMap<String, bool>,
) -> Option<bool> {
    if comp_ref.parts.iter().any(|part| part.subs.is_some()) {
        return None;
    }
    let dotted = comp_ref
        .parts
        .iter()
        .map(|p| p.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    if let Some(value) = local_values.get(&dotted) {
        return Some(*value);
    }
    if comp_ref.parts.len() == 1 {
        let name = comp_ref.parts[0].ident.text.as_ref();
        return local_values.get(name).copied();
    }
    None
}
