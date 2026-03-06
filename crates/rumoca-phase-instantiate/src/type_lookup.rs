use rumoca_core::{DefId, TypeId};
use rumoca_ir_ast as ast;

use super::inheritance;
use super::type_overrides::resolve_redeclare_value_def_id;
use super::{find_class_in_tree, is_type_subtype, type_names_match};

/// Type information for a component, resolved from the class tree.
pub(super) struct TypeInfo<'a> {
    pub(super) class_def: Option<&'a ast::ClassDef>,
    pub(super) is_primitive: bool,
    pub(super) is_discrete: bool,
}

/// Map a builtin primitive name to its TypeId in the class tree.
fn builtin_type_id(tree: &ast::ClassTree, name: &str) -> Option<TypeId> {
    let simple = name.rsplit('.').next().unwrap_or(name);
    match simple {
        "Real" => Some(tree.type_table.real()),
        "Integer" => Some(tree.type_table.integer()),
        "Boolean" => Some(tree.type_table.boolean()),
        "String" => Some(tree.type_table.string()),
        "Clock" => tree.type_table.lookup("Clock"),
        _ => None,
    }
}

/// Resolve the primitive base TypeId for a component type when available.
///
/// For direct builtins this returns the corresponding TypeId. For type aliases
/// and short class definitions, this follows a single-inheritance chain until
/// a builtin primitive is found, or returns UNKNOWN when unresolved.
pub(super) fn resolve_primitive_type_id(
    tree: &ast::ClassTree,
    type_name: &str,
    class_def: Option<&ast::ClassDef>,
) -> TypeId {
    if let Some(id) = builtin_type_id(tree, type_name) {
        return id;
    }

    const MAX_DEPTH: usize = 10;
    let mut current = class_def;

    for _ in 0..MAX_DEPTH {
        let Some(class) = current else {
            return TypeId::UNKNOWN;
        };
        if class.extends.len() != 1 {
            return TypeId::UNKNOWN;
        }

        let ext = &class.extends[0];
        let base_name = ext.base_name.to_string();
        if let Some(id) = builtin_type_id(tree, &base_name) {
            return id;
        }

        current = ext
            .base_def_id
            .and_then(|def_id| tree.get_class_by_def_id(def_id))
            .or_else(|| find_class_in_tree(tree, &base_name));
    }

    TypeId::UNKNOWN
}

/// Find a nested type member within a class, following the extends chain.
///
/// When `Medium.AbsolutePressure` resolves to the `Medium` package alias,
/// we need to find `AbsolutePressure` by looking in the package's own classes
/// and then in its base classes (via extends). This handles replaceable packages
/// like `package Medium = PartialMedium` where types are defined in the base.
pub(super) fn find_member_type_in_class<'a>(
    tree: &'a ast::ClassTree,
    class: &'a ast::ClassDef,
    member_name: &str,
) -> Option<&'a ast::ClassDef> {
    // MLS §7.3: extends-modification redeclarations override inherited
    // replaceable declarations in this class context.
    if let Some(redeclared) = find_extends_redeclared_member_type(tree, class, member_name) {
        return Some(redeclared);
    }

    // Then check direct nested classes.
    if let Some(member) = class.classes.get(member_name) {
        return Some(member);
    }

    // Follow extends chain to find the member in base classes.
    // Also follow ALL extends (not just single), since packages can have
    // multiple inheritance-like extends clauses.
    const MAX_DEPTH: usize = 15;
    let mut visited = Vec::new();
    let mut to_visit: Vec<Option<&ast::ClassDef>> = class
        .extends
        .iter()
        .map(|ext| {
            let base_name = ext.base_name.to_string();
            ext.base_def_id
                .and_then(|def_id| tree.get_class_by_def_id(def_id))
                .or_else(|| find_class_in_tree(tree, &base_name))
        })
        .collect();

    for _ in 0..MAX_DEPTH {
        if to_visit.is_empty() {
            break;
        }
        let mut next_visit = Vec::new();
        for current in to_visit.drain(..) {
            let Some(bc) = current else { continue };
            // Avoid revisiting
            let bc_name = bc.name.text.as_ref();
            if visited.contains(&bc_name) {
                continue;
            }
            visited.push(bc_name);

            if let Some(member) = find_extends_redeclared_member_type(tree, bc, member_name) {
                return Some(member);
            }
            if let Some(member) = bc.classes.get(member_name) {
                return Some(member);
            }
            // Queue base classes for next level
            for ext in &bc.extends {
                let next_name = ext.base_name.to_string();
                next_visit.push(
                    ext.base_def_id
                        .and_then(|def_id| tree.get_class_by_def_id(def_id))
                        .or_else(|| find_class_in_tree(tree, &next_name)),
                );
            }
        }
        to_visit = next_visit;
    }

    None
}

fn find_extends_redeclared_member_type<'a>(
    tree: &'a ast::ClassTree,
    class: &ast::ClassDef,
    member_name: &str,
) -> Option<&'a ast::ClassDef> {
    for ext in &class.extends {
        for ext_mod in &ext.modifications {
            if !ext_mod.redeclare {
                continue;
            }
            let ast::Expression::Modification { target, value } = &ext_mod.expr else {
                continue;
            };
            let Some(first_target) = target.parts.first() else {
                continue;
            };
            if first_target.ident.text.as_ref() != member_name {
                continue;
            }
            let Some(redeclared_def_id) = resolve_redeclare_value_def_id(tree, value, None) else {
                continue;
            };
            if let Some(redeclared_class) = tree.get_class_by_def_id(redeclared_def_id) {
                return Some(redeclared_class);
            }
        }
    }
    None
}

/// Resolve a dotted member path against a class, following the extends chain
/// at each segment via [`find_member_type_in_class`].
pub(super) fn find_member_type_path_in_class<'a>(
    tree: &'a ast::ClassTree,
    class: &'a ast::ClassDef,
    member_path: &str,
) -> Option<&'a ast::ClassDef> {
    let mut current = class;
    for segment in member_path.split('.') {
        current = find_member_type_in_class(tree, current, segment)?;
    }
    Some(current)
}

/// Look up type information for a component.
/// Uses type_def_id for O(1) lookup when available, falling back to name lookup.
pub(super) fn lookup_type_info<'a>(
    tree: &'a ast::ClassTree,
    comp: &ast::Component,
    type_name: &str,
) -> TypeInfo<'a> {
    let mut class_def = comp
        .type_def_id
        .or(comp.type_name.def_id)
        .and_then(|def_id| tree.get_class_by_def_id(def_id))
        .or_else(|| find_class_in_tree(tree, type_name));

    // When a dotted type name like "Medium.AbsolutePressure",
    // "Medium.ThermodynamicState", or "Medium.BaseProperties" resolves to the
    // package ("Medium") rather than the nested member type, drill down into
    // the package member.
    //
    // Components must instantiate the referenced member class, not the
    // containing package. This is required for package-member model components
    // such as `Medium.BaseProperties medium`.
    if let Some(cd) = class_def
        && matches!(cd.class_type, ast::ClassType::Package)
        && let Some(last_dot) = type_name.rfind('.')
    {
        let member_name = &type_name[last_dot + 1..];
        if let Some(member) = find_member_type_in_class(tree, cd, member_name) {
            class_def = Some(member);
        }
    }

    // is_effectively_primitive_transitive follows inheritance chains to detect:
    // - Type aliases to Real/Integer/Boolean/String (primitive)
    // - Records like Complex with components .re/.im (not primitive)
    // - Operator records like SI.ComplexVoltage extending Complex (not primitive)
    let is_primitive = class_def.is_none()
        || class_def
            .map(|c| inheritance::is_effectively_primitive_transitive(tree, c))
            .unwrap_or(false);

    let is_discrete = inheritance::is_discrete_by_type(tree, type_name, class_def);

    TypeInfo {
        class_def,
        is_primitive,
        is_discrete,
    }
}

/// Check if inner and outer types are compatible using DefIds when available.
///
/// MLS §5.4: The inner declaration's type must be a subtype of the outer's type.
/// This version uses DefId comparison first (O(1), alias-aware), then falls back
/// to string-based subtype checking.
///
/// Handles cases where type names differ in qualification level:
/// - outer uses short form: "Interfaces.CompositeStepState"
/// - inner uses qualified form: "StateGraph.Interfaces.CompositeStepState"
pub(super) fn is_type_compatible_with_def_id(
    tree: &ast::ClassTree,
    outer_type: &str,
    outer_def_id: Option<DefId>,
    inner_type: &str,
    inner_def_id: Option<DefId>,
) -> bool {
    // Fast path: If both have DefIds and they match, types are the same
    if let (Some(outer_id), Some(inner_id)) = (outer_def_id, inner_def_id)
        && outer_id == inner_id
    {
        return true;
    }

    // Check if one name is a suffix of the other (short vs qualified name)
    // e.g., "Interfaces.CompositeStepState" matches "StateGraph.Interfaces.CompositeStepState"
    if type_names_match(tree, outer_type, inner_type) {
        return true;
    }

    // Fallback to string-based subtype checking (handles inheritance)
    is_type_subtype(tree, inner_type, outer_type)
}

/// Check if inner type is compatible with outer type (for tests and simple cases).
/// Inner must be a subtype of outer for compatibility.
#[cfg(test)]
pub(super) fn is_type_compatible(
    tree: &ast::ClassTree,
    outer_type: &str,
    inner_type: &str,
) -> bool {
    is_type_subtype(tree, inner_type, outer_type)
}
