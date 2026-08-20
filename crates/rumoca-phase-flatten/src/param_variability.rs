//! Prove which structured equation families are safe to lower as calculated
//! parameters.
//!
//! The proof is occurrence-scoped and declaration-identified. Display names never
//! participate in identity: two instances may contain declarations with the same
//! spelling without sharing parameter-variability or derivative-reachability facts.
//! Any unresolved reference makes a family ineligible, so failure to prove the
//! property preserves the fully materialized equation family.

use std::ops::ControlFlow;

use rumoca_core::{DefId, InstanceId, Variability};
use rumoca_ir_ast::{
    self as ast, ComponentReference, ComponentReferenceContext, Visitor, contains_function_call,
    walk_component_reference_default, walk_expression_default,
};
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct VariableOccurrence {
    owner: InstanceId,
    declaration: DefId,
}

impl VariableOccurrence {
    fn new(owner: InstanceId, declaration: DefId) -> Self {
        Self { owner, declaration }
    }
}

/// Checked evidence that an exact variable occurrence belongs to a
/// parameter-variability, derivative-reachable family.
///
/// The evidence set is private so consumers cannot manufacture membership from a
/// rendered name. Construction below is the sole authority.
#[derive(Default)]
pub(crate) struct ParameterVariabilityFamilies {
    proven: FxHashSet<VariableOccurrence>,
}

impl ParameterVariabilityFamilies {
    fn insert(&mut self, owner: InstanceId, declaration: DefId) -> bool {
        self.proven
            .insert(VariableOccurrence::new(owner, declaration))
    }

    fn contains(&self, owner: InstanceId, declaration: DefId) -> bool {
        self.proven
            .contains(&VariableOccurrence::new(owner, declaration))
    }
}

struct FamilyRefs {
    lhs: FxHashSet<DefId>,
    external: FxHashSet<DefId>,
    all_references_resolved: bool,
}

#[derive(Default)]
struct DependenceGraph {
    der_roots: FxHashSet<DefId>,
    assignment_refs: FxHashMap<DefId, FxHashSet<DefId>>,
}

/// Construct the only proof accepted by structured-family cheapening.
pub(crate) fn prove_parameter_variability_families(
    overlay: &ast::InstanceOverlay,
) -> ParameterVariabilityFamilies {
    let mut result = ParameterVariabilityFamilies::default();

    for class_data in overlay.classes.values() {
        let owner = class_data.instance_id;
        let parameter_constants = parameter_constant_declarations(overlay, owner);
        let mut families = Vec::new();
        let mut dependence = DependenceGraph::default();

        for inst_eq in &class_data.equations {
            if let Some(family) = candidate_family(&inst_eq.equation) {
                families.push(family);
            }
            collect_dependence(&inst_eq.equation, &[], &mut dependence);
        }

        let mut parameter_variability = FxHashSet::default();
        loop {
            let mut changed = false;
            for family in &families {
                changed |=
                    classify_family(family, &parameter_constants, &mut parameter_variability);
            }
            if !changed {
                break;
            }
        }

        let reachable = derivative_reachable(&dependence);
        for declaration in parameter_variability
            .into_iter()
            .filter(|declaration| reachable.contains(declaration))
        {
            result.insert(owner, declaration);
        }
    }

    result
}

fn parameter_constant_declarations(
    overlay: &ast::InstanceOverlay,
    owner: InstanceId,
) -> FxHashSet<DefId> {
    overlay
        .components
        .values()
        .filter(|component| component.owner_class_id == Some(owner))
        .filter(|component| {
            matches!(
                component.variability,
                Variability::Parameter(_) | Variability::Constant(_)
            )
        })
        .filter_map(|component| {
            component
                .component_ref
                .as_ref()
                .map(rumoca_core::ComponentReference::target_def_id)
        })
        .collect()
}

fn classify_family(
    family: &FamilyRefs,
    parameter_constants: &FxHashSet<DefId>,
    parameter_variability: &mut FxHashSet<DefId>,
) -> bool {
    if !family.all_references_resolved
        || family
            .lhs
            .iter()
            .all(|lhs| parameter_variability.contains(lhs))
    {
        return false;
    }
    if !family.external.iter().all(|reference| {
        parameter_constants.contains(reference) || parameter_variability.contains(reference)
    }) {
        return false;
    }

    let mut changed = false;
    for lhs in &family.lhs {
        changed |= parameter_variability.insert(*lhs);
    }
    changed
}

fn collect_dependence(equation: &ast::Equation, binders: &[String], graph: &mut DependenceGraph) {
    match equation {
        ast::Equation::Simple { lhs, rhs } => {
            collect_assignment_dependence(lhs, rhs, binders, graph)
        }
        ast::Equation::For { indices, equations } => {
            let mut scoped = binders.to_vec();
            scoped.extend(indices.iter().map(|index| index.ident.text.to_string()));
            for inner in equations {
                collect_dependence(inner, &scoped, graph);
            }
        }
        ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for inner in &block.eqs {
                    collect_dependence(inner, binders, graph);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    collect_dependence(inner, binders, graph);
                }
            }
        }
        _ => {}
    }
}

fn collect_assignment_dependence(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    binders: &[String],
    graph: &mut DependenceGraph,
) {
    let is_der = |expr: &ast::Expression| {
        contains_function_call(expr, |comp, _| {
            comp.parts.len() == 1 && comp.parts[0].ident.text.as_ref() == "der"
        })
    };
    let rhs_refs = collect_reference_ids(rhs, binders).resolved;

    if is_der(lhs) || is_der(rhs) {
        graph
            .der_roots
            .extend(collect_reference_ids(lhs, binders).resolved);
        graph.der_roots.extend(rhs_refs);
        return;
    }
    if let Some(lhs_declaration) = indexed_lhs_declaration(lhs) {
        graph
            .assignment_refs
            .entry(lhs_declaration)
            .or_default()
            .extend(rhs_refs);
    }
}

fn derivative_reachable(graph: &DependenceGraph) -> FxHashSet<DefId> {
    let mut reachable = FxHashSet::default();
    let mut stack: Vec<DefId> = graph.der_roots.iter().copied().collect();
    while let Some(declaration) = stack.pop() {
        if !reachable.insert(declaration) {
            continue;
        }
        if let Some(references) = graph.assignment_refs.get(&declaration) {
            stack.extend(references.iter().copied());
        }
    }
    reachable
}

pub(crate) fn is_proven_parameter_variability_assignment_body(
    owner: InstanceId,
    indices: &[ast::ForIndex],
    equations: &[ast::Equation],
    proof: &ParameterVariabilityFamilies,
) -> bool {
    candidate_family_parts(indices, equations).is_some_and(|family| {
        family.all_references_resolved
            && !family.lhs.is_empty()
            && family
                .lhs
                .iter()
                .all(|declaration| proof.contains(owner, *declaration))
    })
}

fn candidate_family(equation: &ast::Equation) -> Option<FamilyRefs> {
    let ast::Equation::For { indices, equations } = equation else {
        return None;
    };
    candidate_family_parts(indices, equations)
}

fn candidate_family_parts(
    indices: &[ast::ForIndex],
    equations: &[ast::Equation],
) -> Option<FamilyRefs> {
    let mut lhs = FxHashSet::default();
    let mut references = ReferenceIds::default();
    let mut binders = FxHashSet::default();
    if !gather_family(indices, equations, &mut binders, &mut lhs, &mut references) {
        return None;
    }
    for declaration in &lhs {
        references.resolved.remove(declaration);
    }
    Some(FamilyRefs {
        lhs,
        external: references.resolved,
        all_references_resolved: references.all_resolved,
    })
}

fn gather_family(
    indices: &[ast::ForIndex],
    equations: &[ast::Equation],
    binders: &mut FxHashSet<String>,
    lhs: &mut FxHashSet<DefId>,
    references: &mut ReferenceIds,
) -> bool {
    for index in indices {
        binders.insert(index.ident.text.to_string());
        references.merge(collect_reference_ids(
            &index.range,
            &binders.iter().cloned().collect::<Vec<_>>(),
        ));
    }
    let scoped_binders: Vec<String> = binders.iter().cloned().collect();
    for equation in equations {
        match equation {
            ast::Equation::Simple {
                lhs: lhs_expression,
                rhs,
            } => {
                let Some(lhs_declaration) = indexed_lhs_declaration(lhs_expression) else {
                    return false;
                };
                lhs.insert(lhs_declaration);
                references.merge(collect_reference_ids(lhs_expression, &scoped_binders));
                references.merge(collect_reference_ids(rhs, &scoped_binders));
            }
            ast::Equation::For {
                indices: inner_indices,
                equations: inner_equations,
            } => {
                if !gather_family(inner_indices, inner_equations, binders, lhs, references) {
                    return false;
                }
            }
            _ => return false,
        }
    }
    true
}

fn indexed_lhs_declaration(lhs: &ast::Expression) -> Option<DefId> {
    let ast::Expression::ComponentReference(reference) = lhs else {
        return None;
    };
    let [part] = reference.parts.as_slice() else {
        return None;
    };
    let indexed = part
        .subs
        .as_ref()
        .is_some_and(|subscripts| !subscripts.is_empty());
    indexed.then_some(part.def_id).flatten()
}

#[derive(Default)]
struct ReferenceIds {
    resolved: FxHashSet<DefId>,
    all_resolved: bool,
}

impl ReferenceIds {
    fn complete() -> Self {
        Self {
            resolved: FxHashSet::default(),
            all_resolved: true,
        }
    }

    fn merge(&mut self, other: Self) {
        self.resolved.extend(other.resolved);
        self.all_resolved &= other.all_resolved;
    }
}

fn collect_reference_ids(expr: &ast::Expression, binders: &[String]) -> ReferenceIds {
    let mut references = ReferenceIds::complete();
    let mut collector = ReferenceCollector {
        binders,
        references: &mut references,
    };
    let _ = collector.visit_expression(expr);
    references
}

struct ReferenceCollector<'a> {
    binders: &'a [String],
    references: &'a mut ReferenceIds,
}

impl Visitor for ReferenceCollector<'_> {
    fn visit_expression(&mut self, expr: &ast::Expression) -> ControlFlow<()> {
        if let ast::Expression::Terminal { token, .. } = expr
            && token.text.as_ref() == "time"
        {
            self.references.all_resolved = false;
        }
        walk_expression_default(self, expr)
    }

    fn visit_component_reference_ctx(
        &mut self,
        reference: &ComponentReference,
        context: ComponentReferenceContext,
    ) -> ControlFlow<()> {
        if matches!(
            context,
            ComponentReferenceContext::ExpressionFunctionCallTarget
        ) {
            return ControlFlow::Continue(());
        }
        let Some(root) = reference.parts.first() else {
            self.references.all_resolved = false;
            return ControlFlow::Continue(());
        };
        if !self
            .binders
            .iter()
            .any(|binder| binder == root.ident.text.as_ref())
        {
            if let Some(declaration) = root.def_id {
                self.references.resolved.insert(declaration);
            } else {
                self.references.all_resolved = false;
            }
        }
        walk_component_reference_default(self, reference)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unresolved_reference_cannot_create_proof() {
        let family = FamilyRefs {
            lhs: FxHashSet::from_iter([DefId::new(2)]),
            external: FxHashSet::default(),
            all_references_resolved: false,
        };
        let mut classified = FxHashSet::default();

        assert!(!classify_family(
            &family,
            &FxHashSet::default(),
            &mut classified
        ));
        assert!(classified.is_empty());
    }

    #[test]
    fn proof_membership_includes_class_occurrence() {
        let declaration = DefId::new(2);
        let mut proof = ParameterVariabilityFamilies::default();
        proof.insert(InstanceId::new(10), declaration);

        assert!(proof.contains(InstanceId::new(10), declaration));
        assert!(!proof.contains(InstanceId::new(11), declaration));
    }
}
