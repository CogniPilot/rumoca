//! Identity of duplicate inherited elements (MLS §5.6.1.4).
//!
//! When several base classes contribute an element with the same name, the
//! elements must be identical and only the first is kept. The comparison is
//! made on the *instantiated* elements, so two bases that each declare
//! `parameter Boolean allowFlowReversal = system.allowFlowReversal` are
//! identical even though each base resolved `system` to its own `outer`
//! declaration: those `system` declarations are themselves duplicate inherited
//! elements and collapse into a single element in the deriving class.
//!
//! `rumoca_ir_ast::components_are_semantically_compatible` compares component
//! references by resolved `DefId`, which distinguishes exactly those cases.
//! This module re-runs that comparison after dropping the resolved identity of
//! references that name an element being merged, so the check reflects the
//! merged class rather than the individual bases.

use super::InheritedContent;
use rumoca_ir_ast::{self as ast, ExpressionTransformer};
use std::collections::HashSet;

/// Element names contributed by both the merged content and the incoming base.
///
/// These are exactly the names that MLS §5.6.1.4 collapses into one element,
/// so a reference to any of them denotes the same element in both bases.
pub(super) fn merged_element_names(
    target: &InheritedContent,
    base: &InheritedContent,
) -> HashSet<String> {
    names_shared_with_target(target, base.components.keys().chain(base.classes.keys()))
}

/// The same set for a base class's own declared elements (MLS §5.6.1.4).
pub(super) fn merged_declared_names(
    target: &InheritedContent,
    class: &ast::ClassDef,
) -> HashSet<String> {
    names_shared_with_target(target, class.components.keys().chain(class.classes.keys()))
}

fn names_shared_with_target<'a>(
    target: &InheritedContent,
    candidates: impl Iterator<Item = &'a String>,
) -> HashSet<String> {
    candidates
        .filter(|name| target.components.contains_key(*name) || target.classes.contains_key(*name))
        .cloned()
        .collect()
}

/// Whether two duplicate inherited components denote the same element.
///
/// MLS §5.6.1.4: duplicate inherited elements must be identical, and only the
/// first is kept. Identity is decided on the elements as they appear in the
/// deriving class, so references to co-merged elements are compared by name.
pub(super) fn inherited_components_are_identical(
    existing: &ast::Component,
    incoming: &ast::Component,
    merged: &HashSet<String>,
) -> bool {
    if ast::components_are_semantically_compatible(existing, incoming) {
        return true;
    }
    if merged.is_empty() {
        return false;
    }
    ast::components_are_semantically_compatible(
        &normalized_component(existing, merged),
        &normalized_component(incoming, merged),
    )
}

/// Drop the resolved identity of every reference rooted at a co-merged element.
struct MergedReferenceNormalizer<'a> {
    merged: &'a HashSet<String>,
}

impl MergedReferenceNormalizer<'_> {
    fn names_merged_element(&self, reference: &ast::ComponentReference) -> bool {
        reference
            .parts
            .first()
            .is_some_and(|part| self.merged.contains(part.ident.text.as_ref()))
    }
}

impl ExpressionTransformer for MergedReferenceNormalizer<'_> {
    fn transform_component_ref_inner(
        &mut self,
        mut reference: ast::ComponentReference,
    ) -> ast::ComponentReference {
        for part in &mut reference.parts {
            if let Some(subscripts) = &mut part.subs {
                *subscripts = subscripts
                    .drain(..)
                    .map(|subscript| self.transform_subscript(subscript))
                    .collect();
            }
        }
        if self.names_merged_element(&reference) {
            reference.set_root_def_id(None);
        }
        reference
    }
}

/// A copy of `component` whose co-merged references carry no resolved identity.
fn normalized_component(component: &ast::Component, merged: &HashSet<String>) -> ast::Component {
    let mut normalized = component.clone();
    let mut normalizer = MergedReferenceNormalizer { merged };

    normalized.start = normalizer.transform_expression(normalized.start);
    normalized.binding = normalized
        .binding
        .map(|binding| normalizer.transform_expression(binding));
    normalized.condition = normalized
        .condition
        .map(|condition| normalizer.transform_expression(condition));
    normalized.shape_expr = normalized
        .shape_expr
        .into_iter()
        .map(|subscript| normalizer.transform_subscript(subscript))
        .collect();
    normalized.modifications = normalized
        .modifications
        .into_iter()
        .map(|(name, value)| (name, normalizer.transform_expression(value)))
        .collect();

    if type_name_is_merged(&normalized.type_name, merged) {
        normalized.type_def_id = None;
    }
    normalized
}

/// Whether a component's type is reached through a co-merged element, e.g. the
/// `Medium` package each base class inherits separately.
fn type_name_is_merged(type_name: &ast::Name, merged: &HashSet<String>) -> bool {
    type_name
        .name
        .first()
        .is_some_and(|part| merged.contains(part.text.as_ref()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn component(source: &str) -> ast::Component {
        let model = format!("model Probe\n{source}\nend Probe;\n");
        let ast = rumoca_phase_parse::parse_to_ast(&model, "probe.mo").expect("probe parses");
        ast.classes["Probe"]
            .components
            .values()
            .next()
            .expect("probe declares one component")
            .clone()
    }

    #[test]
    fn references_to_merged_elements_lose_their_resolved_identity() {
        let merged = HashSet::from(["system".to_string()]);
        let mut left = component("parameter Boolean b = system.allowFlowReversal \"left\";");
        let mut right = component("parameter Boolean b = system.allowFlowReversal \"right\";");
        left.def_id = Some(rumoca_core::DefId::new(10));
        right.def_id = Some(rumoca_core::DefId::new(11));
        set_binding_root_def_id(&mut left, rumoca_core::DefId::new(20));
        set_binding_root_def_id(&mut right, rumoca_core::DefId::new(21));

        assert!(
            !ast::components_are_semantically_compatible(&left, &right),
            "the unnormalized comparison must still distinguish the two bases"
        );
        assert!(
            inherited_components_are_identical(&left, &right, &merged),
            "MLS §5.6.1.4: a reference to a co-merged element denotes one element"
        );
    }

    #[test]
    fn references_outside_the_merged_set_keep_their_resolved_identity() {
        let merged = HashSet::from(["system".to_string()]);
        let mut left = component("parameter Real x = c;");
        let mut right = component("parameter Real x = c;");
        left.def_id = Some(rumoca_core::DefId::new(10));
        right.def_id = Some(rumoca_core::DefId::new(11));
        set_binding_root_def_id(&mut left, rumoca_core::DefId::new(30));
        set_binding_root_def_id(&mut right, rumoca_core::DefId::new(31));

        assert!(
            !inherited_components_are_identical(&left, &right, &merged),
            "two different enclosing declarations must stay a conflict"
        );
    }

    fn set_binding_root_def_id(component: &mut ast::Component, def_id: rumoca_core::DefId) {
        let reference = component
            .binding
            .as_mut()
            .and_then(|binding| match binding {
                ast::Expression::ComponentReference(reference) => Some(reference),
                _ => None,
            })
            .expect("probe binding should be a component reference");
        reference.set_root_def_id(Some(def_id));
    }

    fn resolved_tree(source: &str) -> ast::ClassTree {
        let parsed =
            rumoca_phase_parse::parse_to_ast(source, "duplicates.mo").expect("source parses");
        let mut tree = ast::ClassTree::from_parsed(parsed);
        tree.source_map.add("duplicates.mo", source);
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
            .expect("source resolves")
            .into_inner()
    }

    fn merge_bases_of(source: &str, class_name: &str) -> super::super::InstantiateResult<()> {
        let tree = resolved_tree(source);
        let class = tree
            .definitions
            .classes
            .get(class_name)
            .expect("deriving class exists");
        super::super::process_extends(&tree, class).map(|_| ())
    }

    /// `Modelica.Fluid.Fittings.SimpleGenericOrifice` in miniature: both bases
    /// declare the same `allowFlowReversal`, each binding it through its own
    /// `outer system` declaration.
    const OUTER_SYSTEM_DUPLICATES: &str = r#"
model Sys
  parameter Boolean allowFlowReversal = true "System-wide default";
end Sys;
partial model TransportBase
  outer Sys system "System wide properties";
  parameter Boolean allowFlowReversal = system.allowFlowReversal
    "= true to allow flow reversal (port_a -> port_b)";
end TransportBase;
partial model LumpedFlowBase
  outer Sys system "System properties";
  parameter Boolean allowFlowReversal = system.allowFlowReversal
    "= true to allow flow reversal (m_flow >= 0)";
end LumpedFlowBase;
model Orifice
  extends TransportBase;
  extends LumpedFlowBase;
end Orifice;
"#;

    #[test]
    fn duplicate_elements_bound_through_a_merged_outer_declaration_are_identical() {
        let merged = merge_bases_of(OUTER_SYSTEM_DUPLICATES, "Orifice");
        assert!(
            merged.is_ok(),
            "MLS §5.6.1.4 compares the elements of the merged class, where both \
             bases' `system` declarations are one element: {:?}",
            merged.err()
        );
    }

    #[test]
    fn duplicate_elements_with_different_bindings_still_conflict() {
        let source = r#"
partial model LeftBase
  parameter Real k = 1.0;
  Real x = k;
end LeftBase;
partial model RightBase
  parameter Real k = 2.0;
  Real x = k;
end RightBase;
model Conflict
  extends LeftBase;
  extends RightBase;
end Conflict;
"#;
        assert!(
            merge_bases_of(source, "Conflict").is_err(),
            "bases that bind the same name to different values are not identical"
        );
    }
}
