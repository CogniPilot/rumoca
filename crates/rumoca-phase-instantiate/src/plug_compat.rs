//! MLS §6.4-§6.6 plug-compatibility comparator for redeclarations.
//!
//! Sibling-based subtype acceptance (two classes sharing a base) is only
//! sound when the interfaces are actually compatible; this module holds the
//! member-wise and class-flag comparisons used by `is_type_subtype_cached`.

use crate::inheritance::find_class_in_tree;
use rumoca_core::{ComponentPath, DefId};
use rumoca_ir_ast as ast;
use rustc_hash::FxHashSet;

/// Class-level interface flags that must match for a redeclaration regardless
/// of how subtype acceptance was established (MLS §6.4):
/// - specialized class kind (TYPE-028)
/// - operator record base kind (TYPE-006)
/// - expandable connectors (TYPE-016)
/// - purity: an impure replacement needs an impure constraint (TYPE-021)
/// - finality: a final constraint requires a final replacement (TYPE-026)
/// - transitively non-replaceable constraints require a transitively
///   non-replaceable replacement (TYPE-022)
pub(crate) fn class_flags_compatible(
    tree: &ast::ClassTree,
    subtype: &ast::ClassDef,
    supertype: Option<&ast::ClassDef>,
) -> bool {
    let Some(supertype) = supertype else {
        return true;
    };
    if subtype.class_type != supertype.class_type {
        return false;
    }
    if subtype.operator_record != supertype.operator_record {
        return false;
    }
    // MLS §6.4 / TYPE-007: ExternalObject-derived classes are only
    // compatible with the identical class.
    if !external_object_flags_compatible(tree, subtype, supertype) {
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
/// same compatible interface. Type spelling alone is not compared because
/// sibling stacks legitimately use distinct nested replaceable names (e.g.
/// Media), but primitive bases, array shape, variability, causality, and
/// flow/stream prefixes are compared whenever the AST proves them.
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
        if !component_interfaces_compatible(tree, a_comp, b_comp) {
            return false;
        }
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
    // MLS §6.4's transitively-non-replaceable "no other elements" rule
    // (TYPE-023) is deliberately not enforced: idiomatic MSL redeclarations
    // add public members to sibling replacements (e.g. RobotR3 GearType1 for
    // GearType2, Batteries CellRCStack for CellStack) and every major tool
    // accepts them under plug-compatibility.
    // MLS §6.5 / TYPE-003: additional public components of the replacement
    // must be default-connectable. Plain variables are defined by the
    // replacement's own equations; the genuinely dangling case is an extra
    // *input* without a default, which nothing in the constrained usage will
    // ever bind.
    for (name, member) in &sub_members {
        if super_members.contains_key(name) {
            continue;
        }
        // A default means a declaration equation (binding); `start` is not a
        // binding and is auto-populated for builtin types, so it cannot count.
        let is_input = matches!(member.causality, rumoca_core::Causality::Input(_));
        if is_input && member.binding.is_none() {
            return false;
        }
    }
    true
}

/// Compare only interface properties whose incompatibility is provable from
/// the current AST. In particular, unresolved or non-primitive sibling type
/// names are not rejected merely for differing spelling.
fn component_interfaces_compatible(
    tree: &ast::ClassTree,
    subtype: &ast::Component,
    supertype: &ast::Component,
) -> bool {
    if let (Some(sub_base), Some(super_base)) = (
        primitive_base(tree, subtype),
        primitive_base(tree, supertype),
    ) && sub_base != super_base
    {
        return false;
    }

    let sub_rank = component_rank(subtype);
    let super_rank = component_rank(supertype);
    if sub_rank != super_rank {
        return false;
    }
    if sub_rank > 0
        && !subtype.shape.is_empty()
        && !supertype.shape.is_empty()
        && subtype.shape != supertype.shape
    {
        return false;
    }

    if let (Some(sub_variability), Some(super_variability)) = (
        explicit_variability_rank(&subtype.variability),
        explicit_variability_rank(&supertype.variability),
    ) && sub_variability > super_variability
    {
        return false;
    }

    true
}

fn component_rank(component: &ast::Component) -> usize {
    if component.shape_expr.is_empty() {
        component.shape.len()
    } else {
        component.shape_expr.len()
    }
}

/// MLS §4.5/§6.4 orders variability from constant through continuous. `Empty`
/// is left unknown here because its effective default depends on the resolved
/// component type.
fn explicit_variability_rank(variability: &rumoca_core::Variability) -> Option<u8> {
    match variability {
        rumoca_core::Variability::Constant(_) => Some(0),
        rumoca_core::Variability::Parameter(_) => Some(1),
        rumoca_core::Variability::Discrete(_) => Some(2),
        rumoca_core::Variability::Continuous(_) => Some(3),
        rumoca_core::Variability::Empty => None,
    }
}

fn primitive_base(tree: &ast::ClassTree, component: &ast::Component) -> Option<&'static str> {
    if let Some(type_id) = component.type_id
        && let Some(base) = primitive_base_from_type_id(tree, type_id, 0)
    {
        return Some(base);
    }
    primitive_base_from_type(
        tree,
        &component.type_name.to_string(),
        component.type_def_id,
        0,
    )
}

fn primitive_base_from_type_id(
    tree: &ast::ClassTree,
    type_id: rumoca_core::TypeId,
    depth: usize,
) -> Option<&'static str> {
    if type_id.is_unknown() || depth >= 10 {
        return None;
    }
    match tree.type_table.get(type_id)? {
        ast::Type::Builtin(ast::BuiltinType::Real) => Some("Real"),
        ast::Type::Builtin(ast::BuiltinType::Integer) => Some("Integer"),
        ast::Type::Builtin(ast::BuiltinType::Boolean) => Some("Boolean"),
        ast::Type::Builtin(ast::BuiltinType::String) => Some("String"),
        ast::Type::Builtin(ast::BuiltinType::Clock) => Some("Clock"),
        ast::Type::Alias(alias) => primitive_base_from_type_id(tree, alias.aliased, depth + 1),
        ast::Type::Array(array) => primitive_base_from_type_id(tree, array.element, depth + 1),
        ast::Type::Class(_)
        | ast::Type::Enumeration(_)
        | ast::Type::Function(_)
        | ast::Type::Unknown => None,
    }
}

fn primitive_base_from_type(
    tree: &ast::ClassTree,
    type_name: &str,
    type_def_id: Option<rumoca_core::DefId>,
    depth: usize,
) -> Option<&'static str> {
    let builtin = match type_name {
        "Real" => Some("Real"),
        "Integer" => Some("Integer"),
        "Boolean" => Some("Boolean"),
        "String" => Some("String"),
        "Clock" => Some("Clock"),
        _ => None,
    };
    if builtin.is_some() || depth >= 10 {
        return builtin;
    }

    let class = type_def_id
        .and_then(|def_id| tree.get_class_by_def_id(def_id))
        .or_else(|| find_class_in_tree(tree, type_name))?;
    if class.extends.len() != 1 {
        return None;
    }
    let base = &class.extends[0];
    primitive_base_from_type(
        tree,
        &base.base_name.to_string(),
        base.base_def_id.or(base.base_name.def_id),
        depth + 1,
    )
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ExternalObjectAncestry {
    Ordinary,
    Direct(DefId),
    Invalid,
}

fn external_object_flags_compatible(
    tree: &ast::ClassTree,
    subtype: &ast::ClassDef,
    supertype: &ast::ClassDef,
) -> bool {
    let subtype = external_object_ancestry(tree, subtype);
    let supertype = external_object_ancestry(tree, supertype);
    match (subtype, supertype) {
        (ExternalObjectAncestry::Ordinary, ExternalObjectAncestry::Ordinary) => true,
        (
            ExternalObjectAncestry::Direct(subtype_def_id),
            ExternalObjectAncestry::Direct(supertype_def_id),
        ) => subtype_def_id == supertype_def_id,
        _ => false,
    }
}

fn external_object_ancestry(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
) -> ExternalObjectAncestry {
    let external_object = tree
        .scope_tree
        .predefined_member(&ComponentPath::from_flat_path("ExternalObject"));
    let Some(external_object) = external_object else {
        return ExternalObjectAncestry::Invalid;
    };
    external_object_ancestry_inner(tree, class, external_object, &mut FxHashSet::default())
}

fn external_object_ancestry_inner(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    external_object: DefId,
    visiting: &mut FxHashSet<DefId>,
) -> ExternalObjectAncestry {
    let Some(class_def_id) = class.def_id else {
        return ExternalObjectAncestry::Invalid;
    };
    if !visiting.insert(class_def_id) {
        return ExternalObjectAncestry::Invalid;
    }

    let mut direct = false;
    let mut inherited = false;
    for extend in &class.extends {
        let Some(base_def_id) = extend.base_def_id.or(extend.base_name.def_id) else {
            visiting.remove(&class_def_id);
            return ExternalObjectAncestry::Invalid;
        };
        if base_def_id == external_object {
            direct = true;
            continue;
        }
        let Some(base_class) = tree.get_class_by_def_id(base_def_id) else {
            continue;
        };
        match external_object_ancestry_inner(tree, base_class, external_object, visiting) {
            ExternalObjectAncestry::Ordinary => {}
            ExternalObjectAncestry::Direct(_) => inherited = true,
            ExternalObjectAncestry::Invalid => {
                visiting.remove(&class_def_id);
                return ExternalObjectAncestry::Invalid;
            }
        }
    }
    visiting.remove(&class_def_id);

    if direct {
        if class.extends.len() == 1 && !inherited {
            ExternalObjectAncestry::Direct(class_def_id)
        } else {
            ExternalObjectAncestry::Invalid
        }
    } else if inherited {
        ExternalObjectAncestry::Invalid
    } else {
        ExternalObjectAncestry::Ordinary
    }
}

/// MLS §6.6 / TYPE-018..020: constrained inputs and outputs must be leading
/// prefixes in the replacement; additional inputs need defaults.
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
    if !a_inputs.starts_with(&b_inputs) {
        return false;
    }
    for name in &b_inputs {
        if super_members
            .get(name)
            .is_some_and(|component| component.binding.is_some())
            && sub_members
                .get(name)
                .is_some_and(|component| component.binding.is_none())
        {
            return false;
        }
    }
    if a_inputs[b_inputs.len()..].iter().any(|name| {
        sub_members
            .get(name)
            .is_some_and(|component| component.binding.is_none())
    }) {
        return false;
    }

    let b_outputs = outputs(super_members);
    let a_outputs = outputs(sub_members);
    a_outputs.starts_with(&b_outputs)
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(text),
            location: rumoca_core::Location::default(),
            token_number: 0,
            token_type: 0,
        }
    }

    fn component(
        name: &str,
        type_name: &str,
        variability: rumoca_core::Variability,
        causality: rumoca_core::Causality,
    ) -> ast::Component {
        ast::Component {
            name: name.to_string(),
            name_token: token(name),
            type_name: ast::Name::from_string(type_name),
            variability,
            causality,
            ..ast::Component::empty_with_span(rumoca_core::Span::DUMMY)
        }
    }

    fn with_shape(mut component: ast::Component, shape: &[usize]) -> ast::Component {
        component.shape = shape.to_vec();
        component.shape_expr = shape
            .iter()
            .map(|size| {
                ast::Subscript::Expression(ast::Expression::Terminal {
                    terminal_type: ast::TerminalType::UnsignedInteger,
                    token: token(&size.to_string()),
                    span: rumoca_core::Span::DUMMY,
                })
            })
            .collect();
        component
    }

    fn with_binding(mut component: ast::Component) -> ast::Component {
        component.binding = Some(ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: token("1"),
            span: rumoca_core::Span::DUMMY,
        });
        component
    }

    fn class(class_type: rumoca_core::ClassType, members: Vec<ast::Component>) -> ast::ClassDef {
        ast::ClassDef {
            class_type,
            components: members
                .into_iter()
                .map(|member| (member.name.clone(), member))
                .collect(),
            ..Default::default()
        }
    }

    fn class_extending(name: &str, def_id: DefId, base_def_id: DefId) -> ast::ClassDef {
        ast::ClassDef {
            name: token(name),
            def_id: Some(def_id),
            class_type: rumoca_core::ClassType::Model,
            extends: vec![ast::Extend {
                base_name: ast::Name::from_string("display-name-is-not-identity"),
                base_def_id: Some(base_def_id),
                ..Default::default()
            }],
            ..Default::default()
        }
    }

    fn continuous() -> rumoca_core::Variability {
        rumoca_core::Variability::Continuous(token("continuous"))
    }

    fn parameter() -> rumoca_core::Variability {
        rumoca_core::Variability::Parameter(token("parameter"))
    }

    fn input() -> rumoca_core::Causality {
        rumoca_core::Causality::Input(token("input"))
    }

    fn output() -> rumoca_core::Causality {
        rumoca_core::Causality::Output(token("output"))
    }

    #[test]
    fn rejects_provably_incompatible_builtin_member_types() {
        let tree = ast::ClassTree::new();
        let constraint = class(
            rumoca_core::ClassType::Package,
            vec![component(
                "x",
                "Real",
                continuous(),
                rumoca_core::Causality::Empty,
            )],
        );
        let replacement = class(
            rumoca_core::ClassType::Package,
            vec![component(
                "x",
                "Integer",
                continuous(),
                rumoca_core::Causality::Empty,
            )],
        );

        assert!(!members_plug_compatible(&tree, &replacement, &constraint));
    }

    #[test]
    fn rejects_array_rank_and_known_size_mismatches() {
        let tree = ast::ClassTree::new();
        let constraint = class(
            rumoca_core::ClassType::Package,
            vec![with_shape(
                component("x", "Real", continuous(), rumoca_core::Causality::Empty),
                &[2],
            )],
        );
        let rank_mismatch = class(
            rumoca_core::ClassType::Package,
            vec![with_shape(
                component("x", "Real", continuous(), rumoca_core::Causality::Empty),
                &[2, 1],
            )],
        );
        let size_mismatch = class(
            rumoca_core::ClassType::Package,
            vec![with_shape(
                component("x", "Real", continuous(), rumoca_core::Causality::Empty),
                &[3],
            )],
        );

        assert!(!members_plug_compatible(&tree, &rank_mismatch, &constraint));
        assert!(!members_plug_compatible(&tree, &size_mismatch, &constraint));
    }

    #[test]
    fn enforces_variability_ordering_and_direction() {
        let tree = ast::ClassTree::new();
        let constraint = class(
            rumoca_core::ClassType::Package,
            vec![component("x", "Real", parameter(), input())],
        );
        let higher_variability = class(
            rumoca_core::ClassType::Package,
            vec![component("x", "Real", continuous(), input())],
        );
        let wrong_direction = class(
            rumoca_core::ClassType::Package,
            vec![component("x", "Real", parameter(), output())],
        );
        let lower_variability = class(
            rumoca_core::ClassType::Package,
            vec![component(
                "x",
                "Real",
                rumoca_core::Variability::Constant(token("constant")),
                input(),
            )],
        );

        assert!(!members_plug_compatible(
            &tree,
            &higher_variability,
            &constraint
        ));
        assert!(!members_plug_compatible(
            &tree,
            &wrong_direction,
            &constraint
        ));
        assert!(members_plug_compatible(
            &tree,
            &lower_variability,
            &constraint
        ));
    }

    #[test]
    fn function_signatures_compare_input_types_and_output_shapes() {
        let tree = ast::ClassTree::new();
        let constraint = class(
            rumoca_core::ClassType::Function,
            vec![
                component("u", "Real", continuous(), input()),
                with_shape(component("y", "Real", continuous(), output()), &[2]),
            ],
        );
        let wrong_input_type = class(
            rumoca_core::ClassType::Function,
            vec![
                component("u", "Integer", continuous(), input()),
                with_shape(component("y", "Real", continuous(), output()), &[2]),
            ],
        );
        let wrong_output_shape = class(
            rumoca_core::ClassType::Function,
            vec![
                component("u", "Real", continuous(), input()),
                with_shape(component("y", "Real", continuous(), output()), &[3]),
            ],
        );

        assert!(!members_plug_compatible(
            &tree,
            &wrong_input_type,
            &constraint
        ));
        assert!(!members_plug_compatible(
            &tree,
            &wrong_output_shape,
            &constraint
        ));
    }

    #[test]
    fn function_signature_requires_constrained_inputs_before_defaulted_extras() {
        let constrained = indexmap::IndexMap::from([
            (
                "u".to_string(),
                component("u", "Real", continuous(), input()),
            ),
            (
                "v".to_string(),
                component("v", "Real", continuous(), input()),
            ),
        ]);
        let interleaved = indexmap::IndexMap::from([
            (
                "u".to_string(),
                component("u", "Real", continuous(), input()),
            ),
            (
                "extra".to_string(),
                with_binding(component("extra", "Real", continuous(), input())),
            ),
            (
                "v".to_string(),
                component("v", "Real", continuous(), input()),
            ),
        ]);
        let trailing = indexmap::IndexMap::from([
            (
                "u".to_string(),
                component("u", "Real", continuous(), input()),
            ),
            (
                "v".to_string(),
                component("v", "Real", continuous(), input()),
            ),
            (
                "extra".to_string(),
                with_binding(component("extra", "Real", continuous(), input())),
            ),
        ]);

        assert!(!function_signatures_plug_compatible(
            &interleaved,
            &constrained
        ));
        assert!(function_signatures_plug_compatible(&trailing, &constrained));
    }

    #[test]
    fn function_signature_preserves_required_input_bindings_and_allows_trailing_outputs() {
        let constrained = indexmap::IndexMap::from([
            (
                "u".to_string(),
                with_binding(component("u", "Real", continuous(), input())),
            ),
            (
                "y".to_string(),
                component("y", "Real", continuous(), output()),
            ),
        ]);
        let missing_binding = indexmap::IndexMap::from([
            (
                "u".to_string(),
                component("u", "Real", continuous(), input()),
            ),
            (
                "y".to_string(),
                component("y", "Real", continuous(), output()),
            ),
        ]);
        let compatible = indexmap::IndexMap::from([
            (
                "u".to_string(),
                with_binding(component("u", "Real", continuous(), input())),
            ),
            (
                "y".to_string(),
                component("y", "Real", continuous(), output()),
            ),
            (
                "diagnostic".to_string(),
                component("diagnostic", "Real", continuous(), output()),
            ),
        ]);

        assert!(!function_signatures_plug_compatible(
            &missing_binding,
            &constrained
        ));
        assert!(function_signatures_plug_compatible(
            &compatible,
            &constrained
        ));
    }

    #[test]
    fn sibling_type_spellings_with_the_same_primitive_base_remain_compatible() {
        let mut tree = ast::ClassTree::new();
        for name in ["StateA", "StateB"] {
            tree.definitions.classes.insert(
                name.to_string(),
                ast::ClassDef {
                    name: token(name),
                    class_type: rumoca_core::ClassType::Type,
                    extends: vec![ast::Extend {
                        base_name: ast::Name::from_string("Real"),
                        ..Default::default()
                    }],
                    ..Default::default()
                },
            );
        }
        let constraint = class(
            rumoca_core::ClassType::Package,
            vec![component(
                "state",
                "StateA",
                continuous(),
                rumoca_core::Causality::Empty,
            )],
        );
        let replacement = class(
            rumoca_core::ClassType::Package,
            vec![component(
                "state",
                "StateB",
                continuous(),
                rumoca_core::Causality::Empty,
            )],
        );

        assert!(members_plug_compatible(&tree, &replacement, &constraint));
    }

    #[test]
    fn user_shadowed_external_object_is_not_the_predefined_type() {
        let source = r#"
package P
  model ExternalObject
  end ExternalObject;

  model A
    extends ExternalObject;
  end A;

  model B
    extends ExternalObject;
  end B;
end P;
"#;
        let parsed = rumoca_phase_parse::parse_to_ast(source, "shadowed_external_object.mo")
            .expect("source parses");
        let mut tree = ast::ClassTree::from_parsed(parsed);
        tree.source_map.add("shadowed_external_object.mo", source);
        let tree = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
            .expect("source resolves")
            .into_inner();

        assert!(
            crate::inheritance::is_type_subtype(&tree, "P.B", "P.A"),
            "classes extending a user declaration named ExternalObject remain ordinary siblings"
        );
    }

    #[test]
    fn indirect_external_object_ancestry_is_never_plug_compatible() {
        const EXTERNAL_OBJECT: DefId = DefId(100);
        const DIRECT_OWNER: DefId = DefId(101);
        const INDIRECT_A: DefId = DefId(102);
        const INDIRECT_B: DefId = DefId(103);

        let mut tree = ast::ClassTree::new();
        tree.scope_tree.add_predefined_member(
            ComponentPath::from_flat_path("ExternalObject"),
            EXTERNAL_OBJECT,
        );
        for (name, class) in [
            (
                "DirectOwner",
                class_extending("DirectOwner", DIRECT_OWNER, EXTERNAL_OBJECT),
            ),
            (
                "IndirectA",
                class_extending("IndirectA", INDIRECT_A, DIRECT_OWNER),
            ),
            (
                "IndirectB",
                class_extending("IndirectB", INDIRECT_B, DIRECT_OWNER),
            ),
        ] {
            let def_id = class.def_id.expect("test class has identity");
            tree.name_map.insert(name.to_string(), def_id);
            tree.def_map.insert(def_id, name.to_string());
            tree.definitions.classes.insert(name.to_string(), class);
        }

        let indirect_a = tree
            .get_class_by_def_id(INDIRECT_A)
            .expect("first indirect class exists");
        let indirect_b = tree
            .get_class_by_def_id(INDIRECT_B)
            .expect("second indirect class exists");
        assert!(
            !class_flags_compatible(&tree, indirect_a, Some(indirect_b)),
            "two invalid indirect owners must not be accepted as ordinary siblings"
        );
    }

    #[test]
    fn specialized_class_kinds_must_match() {
        let tree = ast::ClassTree::new();
        let package = class(rumoca_core::ClassType::Package, Vec::new());
        let function = class(rumoca_core::ClassType::Function, Vec::new());

        assert!(!class_flags_compatible(&tree, &function, Some(&package)));
    }
}
