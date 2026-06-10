//! MLS §6.4-§6.6 plug-compatibility comparator for redeclarations.
//!
//! Sibling-based subtype acceptance (two classes sharing a base) is only
//! sound when the interfaces are actually compatible; this module holds the
//! member-wise and class-flag comparisons used by `is_type_subtype_cached`.

use crate::inheritance::find_class_in_tree;
use rumoca_ir_ast as ast;

/// Class-level interface flags that must match for a redeclaration regardless
/// of how subtype acceptance was established (MLS §6.4):
/// - operator record base kind (TYPE-006)
/// - expandable connectors (TYPE-016)
/// - purity: an impure replacement needs an impure constraint (TYPE-021)
/// - finality: a final constraint requires a final replacement (TYPE-026)
/// - transitively non-replaceable constraints require a transitively
///   non-replaceable replacement (TYPE-022)
pub(crate) fn class_flags_compatible(
    subtype: &ast::ClassDef,
    supertype: Option<&ast::ClassDef>,
) -> bool {
    let Some(supertype) = supertype else {
        return true;
    };
    if subtype.operator_record != supertype.operator_record {
        return false;
    }
    // MLS §6.4 / TYPE-007: ExternalObject-derived classes are only
    // compatible with the identical class.
    let sub_external = derives_external_object(subtype);
    let super_external = derives_external_object(supertype);
    if sub_external != super_external {
        return false;
    }
    if sub_external && subtype.def_id != supertype.def_id {
        return false;
    }
    if subtype.expandable != supertype.expandable {
        return false;
    }
    if subtype.class_type == rumoca_core::ClassType::Function
        && supertype.class_type == rumoca_core::ClassType::Function
        && !subtype.pure
        && supertype.pure
    {
        return false;
    }
    if supertype.is_final && !subtype.is_final {
        return false;
    }
    if is_transitively_non_replaceable(supertype) && !is_transitively_non_replaceable(subtype) {
        return false;
    }
    true
}

/// MLS §6.4: a class is transitively non-replaceable when neither it nor any
/// of its elements are replaceable (shallow walk over local elements; extends
/// targets are not followed because the resolved tree is not available here —
/// this is the conservative direction for a constraint check).
fn is_transitively_non_replaceable(class: &ast::ClassDef) -> bool {
    if class.is_replaceable {
        return false;
    }
    if class.components.iter().any(|(_, comp)| comp.is_replaceable) {
        return false;
    }
    !class
        .classes
        .iter()
        .any(|(_, nested)| nested.is_replaceable)
}

/// MLS §6.5 plug compatibility for sibling-based acceptance: every public
/// component of the constraining type must exist in the replacement with the
/// same causality, flow/stream prefix, and conditionality. Component *types*
/// are deliberately not compared: sibling stacks legitimately differ in
/// nested replaceable members (e.g. Media), and a type comparison here would
/// reject valid MSL redeclarations.
pub(crate) fn members_plug_compatible(
    tree: &ast::ClassTree,
    subtype: &ast::ClassDef,
    supertype: &ast::ClassDef,
) -> bool {
    let sub_members = collect_public_members(tree, subtype);
    let super_members = collect_public_members(tree, supertype);
    for (name, b_comp) in &super_members {
        let Some(a_comp) = sub_members.get(name) else {
            return false;
        };
        if std::mem::discriminant(&a_comp.causality) != std::mem::discriminant(&b_comp.causality) {
            return false;
        }
        if std::mem::discriminant(&a_comp.connection) != std::mem::discriminant(&b_comp.connection)
        {
            return false;
        }
        if a_comp.condition.is_some() != b_comp.condition.is_some() {
            return false;
        }
        if a_comp.inner != b_comp.inner || a_comp.outer != b_comp.outer {
            return false;
        }
    }
    if supertype.class_type == rumoca_core::ClassType::Function {
        return function_signatures_plug_compatible(&sub_members, &super_members);
    }
    // MLS §6.4 / TYPE-023: when the constraining type is transitively
    // non-replaceable, the replacement interface shall not contain any other
    // elements.
    if is_transitively_non_replaceable(supertype)
        && sub_members
            .keys()
            .any(|name| !super_members.contains_key(name))
    {
        return false;
    }
    // MLS §6.5 / TYPE-003: additional public components of the replacement
    // must be default-connectable. Plain variables are defined by the
    // replacement's own equations; the genuinely dangling case is an extra
    // *input* without a default, which nothing in the constrained usage will
    // ever bind.
    for (name, member) in &sub_members {
        if super_members.contains_key(name) {
            continue;
        }
        let is_input = matches!(member.causality, rumoca_core::Causality::Input(_));
        let has_default =
            member.binding.is_some() || !matches!(member.start, ast::Expression::Empty { .. });
        if is_input && !has_default {
            return false;
        }
    }
    true
}

/// True when the class (or its single-extends chain) derives from the
/// builtin ExternalObject.
fn derives_external_object(class: &ast::ClassDef) -> bool {
    class
        .extends
        .iter()
        .any(|ext| ext.base_name.to_string() == "ExternalObject")
}

/// MLS §6.6 / TYPE-018..020: inputs of the constraining function must appear
/// by name and in the same relative order in the replacement; replacement
/// inputs not present in the constraint need defaults; outputs must match by
/// name in order.
fn function_signatures_plug_compatible(
    sub_members: &indexmap::IndexMap<String, ast::Component>,
    super_members: &indexmap::IndexMap<String, ast::Component>,
) -> bool {
    let inputs = |members: &indexmap::IndexMap<String, ast::Component>| -> Vec<String> {
        members
            .iter()
            .filter(|(_, c)| matches!(c.causality, rumoca_core::Causality::Input(_)))
            .map(|(name, _)| name.clone())
            .collect()
    };
    let outputs = |members: &indexmap::IndexMap<String, ast::Component>| -> Vec<String> {
        members
            .iter()
            .filter(|(_, c)| matches!(c.causality, rumoca_core::Causality::Output(_)))
            .map(|(name, _)| name.clone())
            .collect()
    };

    let b_inputs = inputs(super_members);
    let a_inputs = inputs(sub_members);
    // B's inputs must appear in A preserving relative order.
    let mut b_iter = b_inputs.iter().peekable();
    for name in &a_inputs {
        if b_iter.peek().is_some_and(|next| *next == name) {
            b_iter.next();
        }
    }
    if b_iter.peek().is_some() {
        return false;
    }
    // A's extra inputs need defaults.
    for name in &a_inputs {
        if !b_inputs.contains(name)
            && sub_members
                .get(name)
                .is_some_and(|comp| comp.binding.is_none())
        {
            return false;
        }
    }
    outputs(super_members) == outputs(sub_members)
}

/// Public components of a class including inherited ones (depth-limited walk
/// over the extends chain; later declarations win on name clashes).
fn collect_public_members(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
) -> indexmap::IndexMap<String, ast::Component> {
    fn collect_into(
        tree: &ast::ClassTree,
        class: &ast::ClassDef,
        depth: usize,
        out: &mut indexmap::IndexMap<String, ast::Component>,
    ) {
        if depth == 0 {
            return;
        }
        for ext in &class.extends {
            let base = ext
                .base_def_id
                .and_then(|id| tree.get_class_by_def_id(id))
                .or_else(|| find_class_in_tree(tree, &ext.base_name.to_string()));
            if let Some(base) = base {
                collect_into(tree, base, depth - 1, out);
            }
        }
        for (name, comp) in &class.components {
            if !comp.is_protected {
                out.insert(name.clone(), comp.clone());
            }
        }
    }
    let mut out = indexmap::IndexMap::new();
    collect_into(tree, class, 8, &mut out);
    out
}
