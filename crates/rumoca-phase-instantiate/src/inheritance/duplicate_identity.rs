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
use rumoca_core::DefId;
use rumoca_ir_ast::{self as ast, AstIndexMap as IndexMap, ExpressionTransformer};
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

/// Return exact source declaration remaps for duplicate inherited components.
///
/// The result is scoped to the inherited content being merged. A caller must
/// attach it to the concrete class occurrence that owns that content before a
/// later phase uses it; this is deliberately not a global `DefId` alias map.
pub(super) fn inherited_component_def_id_remaps(
    target: &InheritedContent,
    incoming: &IndexMap<String, ast::Component>,
    merged: &HashSet<String>,
) -> IndexMap<DefId, DefId> {
    incoming
        .iter()
        .filter_map(|(name, incoming)| {
            let retained = target.components.get(name)?;
            if !inherited_components_are_identical(retained, incoming, merged) {
                return None;
            }
            let old = incoming.def_id?;
            let retained = retained.def_id?;
            (old != retained).then_some((old, retained))
        })
        .collect()
}

/// Rewrite references in one inherited component using exact declaration IDs.
pub(super) fn remap_component_references(
    component: &mut ast::Component,
    remaps: &IndexMap<DefId, DefId>,
) {
    if remaps.is_empty() {
        return;
    }
    let mut remapper = DefIdReferenceRemapper { remaps };
    component.start = remapper.transform_expression(component.start.clone());
    component.binding = component
        .binding
        .take()
        .map(|binding| remapper.transform_expression(binding));
    component.condition = component
        .condition
        .take()
        .map(|condition| remapper.transform_expression(condition));
    component.shape_expr = component
        .shape_expr
        .drain(..)
        .map(|subscript| remapper.transform_subscript(subscript))
        .collect();
    component.modifications = component
        .modifications
        .drain(..)
        .map(|(name, value)| (name, remapper.transform_expression(value)))
        .collect();
    component.annotation = component
        .annotation
        .drain(..)
        .map(|annotation| remapper.transform_expression(annotation))
        .collect();
    component.source_modifications = component
        .source_modifications
        .drain(..)
        .map(|modification| remapper.transform_expression(modification))
        .collect();
}

/// Rewrite all component declarations in inherited content. This is called
/// only while one incoming inheritance branch is being merged, so an ID is
/// never rewritten merely because it has the same spelling elsewhere.
pub(super) fn remap_inherited_components(
    components: &mut IndexMap<String, ast::Component>,
    remaps: &IndexMap<DefId, DefId>,
) {
    for component in components.values_mut() {
        remap_component_references(component, remaps);
    }
}

/// Collapse a local remap chain to its final retained declaration.
pub(super) fn normalize_def_id_remaps(remaps: &mut IndexMap<DefId, DefId>) {
    let snapshot = remaps.clone();
    let normalized = snapshot
        .keys()
        .filter_map(|&old| {
            let retained = remap_def_id(Some(old), &snapshot)?;
            (old != retained).then_some((old, retained))
        })
        .collect();
    *remaps = normalized;
}

/// Compose a cached branch's source remaps through the declarations retained
/// by the current merge. A branch may already contain `A -> B` while the
/// current class retains `A` and contributes `B -> A`; that valid diamond is
/// composed as `A -> A` (omitted) and `B -> A`, rather than treated as a
/// cycle.
pub(super) fn compose_def_id_remaps(
    branch: &IndexMap<DefId, DefId>,
    incoming: &IndexMap<DefId, DefId>,
) -> IndexMap<DefId, DefId> {
    let mut composed = IndexMap::default();
    for (&old, &retained) in branch {
        let Some(retained) = remap_def_id(Some(retained), incoming) else {
            continue;
        };
        if old != retained {
            composed.insert(old, retained);
        }
    }
    composed.extend(incoming.iter().map(|(&old, &retained)| (old, retained)));
    normalize_def_id_remaps(&mut composed);
    composed
}

/// Add one inheritance branch's exact remaps to the local merged scope.
pub(super) fn record_def_id_remaps(target: &mut InheritedContent, remaps: &IndexMap<DefId, DefId>) {
    target
        .reference_def_id_remaps
        .extend(remaps.iter().map(|(&old, &retained)| (old, retained)));
    normalize_def_id_remaps(&mut target.reference_def_id_remaps);
}

fn remap_def_id(id: Option<DefId>, remaps: &IndexMap<DefId, DefId>) -> Option<DefId> {
    let mut current = id?;
    let mut visited = HashSet::new();
    loop {
        if !visited.insert(current) {
            // A contradictory merge order can produce a cycle when a cached
            // diamond branch carries A -> B into a scope that contributes
            // B -> A. Such a map proves no canonical identity; dropping the
            // result keeps downstream identity lookup strict and Missing.
            return None;
        }
        let Some(&next) = remaps.get(&current) else {
            return Some(current);
        };
        current = next;
    }
}

struct DefIdReferenceRemapper<'a> {
    remaps: &'a IndexMap<DefId, DefId>,
}

impl ExpressionTransformer for DefIdReferenceRemapper<'_> {
    fn transform_component_ref_inner(
        &mut self,
        mut reference: ast::ComponentReference,
    ) -> ast::ComponentReference {
        // The map belongs to the current merged class scope. Only the first
        // segment is resolved in that scope; tails belong to the component or
        // class instance selected by that segment (e.g. `child.system`).
        if let Some(root) = reference.parts.first_mut() {
            root.def_id = remap_def_id(root.def_id, self.remaps);
        }
        for part in &mut reference.parts {
            if let Some(subscripts) = &mut part.subs {
                *subscripts = subscripts
                    .drain(..)
                    .map(|subscript| self.transform_subscript(subscript))
                    .collect();
            }
        }
        reference
    }
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

    fn test_token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: std::sync::Arc::from(text),
            location: rumoca_core::Location::default(),
            token_number: 0,
            token_type: 0,
        }
    }

    fn set_binding_reference_parts(component: &mut ast::Component, parts: &[(&str, DefId)]) {
        let reference = ast::ComponentReference {
            local: false,
            parts: parts
                .iter()
                .map(|(name, def_id)| ast::ComponentRefPart {
                    ident: test_token(name),
                    subs: None,
                    def_id: Some(*def_id),
                })
                .collect(),
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        };
        component.binding = Some(ast::Expression::ComponentReference(reference));
    }

    #[test]
    fn remaps_only_the_current_scope_root_of_a_component_reference() {
        let source_system = DefId::new(10);
        let retained_system = DefId::new(20);
        let child = DefId::new(30);
        let sibling = DefId::new(31);
        let system_member = DefId::new(41);
        let remaps = IndexMap::from_iter([(source_system, retained_system)]);

        let mut direct = component("parameter Real direct = 1;");
        direct.type_name.def_id = Some(source_system);
        direct.type_def_id = Some(source_system);
        set_binding_reference_parts(
            &mut direct,
            &[
                ("system", source_system),
                ("allowFlowReversal", system_member),
            ],
        );
        remap_component_references(&mut direct, &remaps);
        let ast::Expression::ComponentReference(direct_reference) = direct.binding.unwrap() else {
            panic!("direct binding should remain a component reference");
        };
        assert_eq!(direct_reference.parts[0].def_id, Some(retained_system));
        assert_eq!(direct_reference.parts[1].def_id, Some(system_member));
        assert_eq!(direct.type_name.def_id, Some(source_system));
        assert_eq!(direct.type_def_id, Some(source_system));

        for (name, owner) in [("child", child), ("sibling", sibling)] {
            let mut nested = component(&format!("parameter Real {name} = 1;"));
            set_binding_reference_parts(&mut nested, &[(name, owner), ("system", source_system)]);
            remap_component_references(&mut nested, &remaps);
            let ast::Expression::ComponentReference(reference) = nested.binding.unwrap() else {
                panic!("nested binding should remain a component reference");
            };
            assert_eq!(reference.parts[0].def_id, Some(owner));
            assert_eq!(
                reference.parts[1].def_id,
                Some(source_system),
                "a sibling instance tail is outside the merged class scope"
            );
        }
    }

    #[test]
    fn cyclic_remaps_fail_closed_during_normalization() {
        let a = DefId::new(50);
        let b = DefId::new(51);
        let cyclic = IndexMap::from_iter([(a, b), (b, a)]);
        assert_eq!(
            remap_def_id(Some(a), &cyclic),
            None,
            "a raw cyclic map must fail closed without looping"
        );
        let mut remaps = cyclic;

        normalize_def_id_remaps(&mut remaps);

        assert!(
            remaps.is_empty(),
            "cyclic identities cannot be canonicalized"
        );
        assert_eq!(remap_def_id(Some(a), &remaps), Some(a));
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
      parameter Boolean usesSystem = system.allowFlowReversal;
end LumpedFlowBase;
model Orifice
  extends TransportBase;
  extends LumpedFlowBase;
end Orifice;
"#;

    #[test]
    fn duplicate_elements_bound_through_a_merged_outer_declaration_are_identical() {
        let tree = resolved_tree(OUTER_SYSTEM_DUPLICATES);
        let class = tree
            .definitions
            .classes
            .get("Orifice")
            .expect("deriving class exists");
        let inherited = super::super::process_extends(&tree, class)
            .expect("identical inherited outer declarations should merge");
        let transport = tree
            .definitions
            .classes
            .get("TransportBase")
            .expect("transport base exists");
        let lumped = tree
            .definitions
            .classes
            .get("LumpedFlowBase")
            .expect("lumped base exists");
        let retained_system = inherited
            .components
            .get("system")
            .and_then(|component| component.def_id)
            .expect("merged system declaration has a DefId");
        let transport_system = transport
            .components
            .get("system")
            .and_then(|component| component.def_id)
            .expect("transport system declaration has a DefId");
        let lumped_system = lumped
            .components
            .get("system")
            .and_then(|component| component.def_id)
            .expect("lumped system declaration has a DefId");
        assert_eq!(
            retained_system, transport_system,
            "the first inherited declaration is retained"
        );
        assert_eq!(
            inherited.reference_def_id_remaps.get(&lumped_system),
            Some(&retained_system),
            "the duplicate source declaration maps to the retained declaration"
        );
        let uses_system = inherited
            .components
            .get("usesSystem")
            .expect("unique incoming declaration is retained");
        let uses_system_root = uses_system
            .binding
            .as_ref()
            .and_then(|binding| match binding {
                ast::Expression::ComponentReference(reference) => reference.root_def_id(),
                _ => None,
            })
            .expect("usesSystem binding has a resolved root");
        assert_eq!(
            uses_system_root, retained_system,
            "references copied from the incoming base use the retained declaration ID"
        );
        assert_ne!(
            uses_system_root, lumped_system,
            "the incoming declaration's source ID must not survive the merge"
        );
    }

    #[test]
    fn opposite_retained_order_in_a_diamond_composes_to_current_retained_identity() {
        let source = r#"
model Sys
  parameter Boolean allowFlowReversal = true;
end Sys;
partial model A
  outer Sys system;
  parameter Boolean fromA = system.allowFlowReversal;
end A;
partial model B
  outer Sys system;
  parameter Boolean fromB = system.allowFlowReversal;
end B;
partial model RightFirst
  extends B;
  extends A;
end RightFirst;
model Derived
  extends A;
  extends RightFirst;
end Derived;
model Test
  inner Sys system;
  Derived child;
end Test;
"#;
        let tree = resolved_tree(source);
        let class = tree
            .definitions
            .classes
            .get("Derived")
            .expect("derived class exists");
        let inherited = super::super::process_extends(&tree, class)
            .expect("a valid opposite-order diamond should merge");
        let a_system = tree
            .definitions
            .classes
            .get("A")
            .and_then(|class| class.components.get("system"))
            .and_then(|component| component.def_id)
            .expect("A system identity");
        let b_system = tree
            .definitions
            .classes
            .get("B")
            .and_then(|class| class.components.get("system"))
            .and_then(|component| component.def_id)
            .expect("B system identity");

        assert_eq!(
            inherited.reference_def_id_remaps.get(&b_system),
            Some(&a_system),
            "the current Derived declaration remains canonical"
        );
        assert!(!inherited.reference_def_id_remaps.contains_key(&a_system));
        for name in ["fromA", "fromB"] {
            let root = inherited
                .components
                .get(name)
                .and_then(|component| component.binding.as_ref())
                .and_then(|binding| match binding {
                    ast::Expression::ComponentReference(reference) => reference.root_def_id(),
                    _ => None,
                });
            assert_eq!(
                root,
                Some(a_system),
                "{name} uses the retained outer identity"
            );
        }
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
