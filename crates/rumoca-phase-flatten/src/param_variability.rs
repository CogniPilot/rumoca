//! Identify the for-equation families flatten may cheapen: *parameter-variability*
//! families that a state derivative actually reads.
//!
//! A for-equation family `for i (,j…) loop a[i,…] = f(…); … end for;` is
//! *parameter-variability* when every model variable its bodies reference — after
//! excluding the loop binders and the family's own assigned arrays — is a
//! parameter/constant or an array assigned by another parameter-variability family.
//! Such a family's per-cell values are constant for the whole simulation.
//!
//! Cheapening is gated by a SECOND condition: the family must be *derivative-reachable*
//! (some `der(...)` equation depends on it, transitively through other algebraics).
//! This mirrors `promote_parameter_variable::derivative_reachable_algebraics`: the DAE
//! promotes exactly the parameter-variable algebraics a derivative reads, leaving
//! standalone constant arrays (`y[i] = i` with no consumer) as ordinary algebraics.
//! Cheapening a family the DAE will not promote would either freeze it at `0.0`
//! (caught by the DAE fail-early guard) or change the equation partition of trivial
//! models; gating on reachability keeps flatten's cheapening and DAE's promotion in
//! lock-step.
//!
//! Classification runs over the *unqualified* AST loop bodies before equation
//! expansion. Membership in the parameter/constant set uses base names stripped from
//! `Context::flat_parameter_constant_keys`; references this analysis cannot prove
//! parameter-variability (e.g. a member access whose base does not match a flat key)
//! simply leave the family unclassified — the family is then materialized as before,
//! never cheapened. The analysis is therefore conservative: a misclassification can
//! only *miss* a cheapening, never cheapen a state-coupled family.

use std::ops::ControlFlow;

use rumoca_ir_ast::{
    self as ast, ComponentReference, ComponentReferenceContext, Visitor, contains_function_call,
    walk_component_reference_default, walk_expression_default,
};
use rustc_hash::{FxHashMap, FxHashSet};

/// References extracted from one candidate for-equation family.
struct FamilyRefs {
    /// Base names of the arrays this family assigns (`a` in `a[i,…] = …`).
    lhs_bases: FxHashSet<String>,
    /// Base names referenced by the bodies, excluding loop binders and `lhs_bases`.
    external_refs: FxHashSet<String>,
}

/// Compute the array base-names flatten may cheapen: parameter-variability families
/// that are also derivative-reachable, across every class instance in `overlay`.
/// `parameter_constant_keys` are the flat parameter/constant variable keys (subscripts
/// are stripped here to base names).
pub(crate) fn parameter_variability_family_bases(
    overlay: &ast::InstanceOverlay,
    parameter_constant_keys: &FxHashSet<String>,
) -> FxHashSet<String> {
    let parameter_constant_bases: FxHashSet<&str> = parameter_constant_keys
        .iter()
        .map(|key| base(key))
        .collect();

    let mut families: Vec<FamilyRefs> = Vec::new();
    let mut dependence = DependenceGraph::default();
    for class_data in overlay.classes.values() {
        for inst_eq in &class_data.equations {
            if let Some(family) = candidate_family(&inst_eq.equation) {
                families.push(family);
            }
            collect_dependence(&inst_eq.equation, &[], &mut dependence);
        }
    }
    if families.is_empty() {
        return FxHashSet::default();
    }

    // Monotone fixpoint: a family becomes parameter-variability once every one of its
    // external references is a parameter/constant or the LHS of an already-classified
    // parameter-variability family. `sig` referencing `sc`/`nc`, or turkey `m_shell`
    // referencing geometry `r`, converge here regardless of source order.
    let mut param_variability: FxHashSet<String> = FxHashSet::default();
    loop {
        let mut changed = false;
        for family in &families {
            changed |= classify_family(family, &parameter_constant_bases, &mut param_variability);
        }
        if !changed {
            break;
        }
    }

    // Restrict to the families a state derivative actually reads (mirrors the DAE's
    // promotion criterion), so flatten cheapens exactly what promotion removes.
    let reachable = derivative_reachable(&dependence);
    param_variability
        .into_iter()
        .filter(|name| reachable.contains(name))
        .collect()
}

/// One fixpoint step for a single family: if every external reference is now provably
/// parameter-variability, add the family's assigned arrays to `param_variability` and
/// report whether that grew the set. An already-classified family is a no-op.
fn classify_family(
    family: &FamilyRefs,
    parameter_constant_bases: &FxHashSet<&str>,
    param_variability: &mut FxHashSet<String>,
) -> bool {
    let already_classified = family
        .lhs_bases
        .iter()
        .all(|lhs| param_variability.contains(lhs));
    if already_classified {
        return false;
    }
    let provable = family.external_refs.iter().all(|name| {
        reference_is_parameter_variability(name, parameter_constant_bases, param_variability)
    });
    if !provable {
        return false;
    }
    let mut changed = false;
    for lhs in &family.lhs_bases {
        changed |= param_variability.insert(lhs.clone());
    }
    changed
}

fn reference_is_parameter_variability(
    name: &str,
    parameter_constant_bases: &FxHashSet<&str>,
    param_variability: &FxHashSet<String>,
) -> bool {
    name != "time" && (parameter_constant_bases.contains(name) || param_variability.contains(name))
}

/// Variable-dependence over the model equations: which arrays each `der(...)` equation
/// reads (`der_roots`), and, for each plain assignment `a[…] = rhs`, the bases its
/// right-hand side reads (`assignment_refs`, `a` → refs). Together these drive the
/// derivative-reachability closure.
#[derive(Default)]
struct DependenceGraph {
    der_roots: FxHashSet<String>,
    assignment_refs: FxHashMap<String, FxHashSet<String>>,
}

/// Walk an equation (descending `for`/`if` bodies) accumulating dependence edges.
/// `binders` are the loop indices in scope, excluded from collected references.
fn collect_dependence(equation: &ast::Equation, binders: &[String], graph: &mut DependenceGraph) {
    match equation {
        ast::Equation::Simple { lhs, rhs } => {
            collect_assignment_dependence(lhs, rhs, binders, graph)
        }
        ast::Equation::For { indices, equations } => {
            let mut scoped: Vec<String> = binders.to_vec();
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

/// Record one assignment's dependence. A derivative equation (`der(...)` on either
/// side) seeds every base it reads as a reachability root; a plain assignment adds an
/// edge `lhs_base → rhs reads`.
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
    let mut rhs_refs = FxHashSet::default();
    collect_reference_bases(rhs, &mut rhs_refs);
    rhs_refs.retain(|name| !binders.contains(name));

    if is_der(lhs) || is_der(rhs) {
        let mut lhs_refs = FxHashSet::default();
        collect_reference_bases(lhs, &mut lhs_refs);
        lhs_refs.retain(|name| !binders.contains(name));
        graph.der_roots.extend(lhs_refs);
        graph.der_roots.extend(rhs_refs);
        return;
    }
    // Plain assignment: edge from the assigned base to its right-hand-side reads.
    if let ast::Expression::ComponentReference(cr) = lhs
        && let Some(part) = cr.parts.first()
    {
        graph
            .assignment_refs
            .entry(part.ident.text.to_string())
            .or_default()
            .extend(rhs_refs);
    }
}

/// Base names transitively reachable from the derivative roots through the assignment
/// edges — the algebraics a state derivative reads, directly or via other algebraics.
///
/// This is the flatten-phase, AST-level twin of the DAE's
/// `promote_parameter_variable::derivative_reachable_algebraics` (which walks the
/// lowered DAE). The two MUST agree on which families are reachable: flatten cheapens
/// only reachable families, and the DAE promotes only reachable (or already-cheapened)
/// ones. If they drift, the DAE's `enforce_cheapened_algebraic_families_promoted` guard
/// turns the mismatch into a hard error rather than a silent miscompile — keep these two
/// closures in sync when either is changed.
fn derivative_reachable(graph: &DependenceGraph) -> FxHashSet<String> {
    let mut reachable = FxHashSet::default();
    let mut stack: Vec<String> = graph.der_roots.iter().cloned().collect();
    while let Some(name) = stack.pop() {
        if !reachable.insert(name.clone()) {
            continue;
        }
        if let Some(refs) = graph.assignment_refs.get(&name) {
            stack.extend(refs.iter().cloned());
        }
    }
    reachable
}

/// True when `equations` is an elementwise assignment body whose every assigned array
/// base is a parameter-variability family (per `Context::param_variability_family_bases`).
/// Mirrors [`candidate_family`]'s shape acceptance so the cheapen gate only fires on
/// bodies this analysis actually classified.
pub(crate) fn is_parameter_variability_assignment_body(
    indices: &[ast::ForIndex],
    equations: &[ast::Equation],
    param_variability_bases: &FxHashSet<String>,
) -> bool {
    let mut lhs_bases = FxHashSet::default();
    let mut refs = FxHashSet::default();
    let mut binders = FxHashSet::default();
    if !gather_family(indices, equations, &mut binders, &mut lhs_bases, &mut refs) {
        return false;
    }
    !lhs_bases.is_empty()
        && lhs_bases
            .iter()
            .all(|lhs| param_variability_bases.contains(lhs))
}

/// Extract a candidate family from a top-level equation, or `None` if it is not an
/// elementwise for-equation (`for … loop a[i,…] = …; … end for;`, possibly nested).
fn candidate_family(equation: &ast::Equation) -> Option<FamilyRefs> {
    let ast::Equation::For { indices, equations } = equation else {
        return None;
    };
    let mut lhs_bases = FxHashSet::default();
    let mut external_refs = FxHashSet::default();
    let mut binders = FxHashSet::default();
    if !gather_family(
        indices,
        equations,
        &mut binders,
        &mut lhs_bases,
        &mut external_refs,
    ) {
        return None;
    }
    // External references are the body references minus the loop binders and the
    // family's own assigned arrays (self-references are computed by this family).
    external_refs.retain(|name| !binders.contains(name) && !lhs_bases.contains(name));
    Some(FamilyRefs {
        lhs_bases,
        external_refs,
    })
}

/// Walk a (possibly nested) for-equation body, accumulating loop binders, assigned
/// array bases, and referenced variable bases. Returns `false` for any shape that is
/// not a flat list of elementwise `a[i,…] = rhs` leaves (an `if`/`when`/connect leaf,
/// a non-indexed or derivative LHS, etc.), which rejects the whole family.
fn gather_family(
    indices: &[ast::ForIndex],
    equations: &[ast::Equation],
    binders: &mut FxHashSet<String>,
    lhs_bases: &mut FxHashSet<String>,
    refs: &mut FxHashSet<String>,
) -> bool {
    for index in indices {
        binders.insert(index.ident.text.to_string());
        collect_reference_bases(&index.range, refs);
    }
    for equation in equations {
        match equation {
            ast::Equation::Simple { lhs, rhs } => {
                let Some(lhs_base) = indexed_lhs_base(lhs) else {
                    return false;
                };
                lhs_bases.insert(lhs_base);
                collect_reference_bases(lhs, refs);
                collect_reference_bases(rhs, refs);
            }
            ast::Equation::For {
                indices: inner_indices,
                equations: inner_equations,
            } => {
                if !gather_family(inner_indices, inner_equations, binders, lhs_bases, refs) {
                    return false;
                }
            }
            _ => return false,
        }
    }
    true
}

/// Base name of an indexed left-hand side `a[i,…]`: a SINGLE-segment component
/// reference carrying subscripts, whose base is a whole-array algebraic variable.
/// `None` for a bare scalar, a `der(...)` call, or a multi-segment reference. The
/// multi-segment rejection is essential: a connector/record member element like
/// `pin[k].v` would otherwise classify under base `pin` (not a promotable variable),
/// and DAE promotion explicitly declines such members — flatten must not cheapen a
/// family the DAE cannot reconstruct (the fail-early guard would reject it).
fn indexed_lhs_base(lhs: &ast::Expression) -> Option<String> {
    let ast::Expression::ComponentReference(cr) = lhs else {
        return None;
    };
    let [part] = cr.parts.as_slice() else {
        return None;
    };
    let indexed = part.subs.as_ref().is_some_and(|subs| !subs.is_empty());
    indexed.then(|| part.ident.text.to_string())
}

/// Collect the base names of all genuine variable references in `expr` (skipping
/// function-call target names like `cos`/`tanh`), descending into subscripts.
fn collect_reference_bases(expr: &ast::Expression, refs: &mut FxHashSet<String>) {
    let mut collector = RefBaseCollector { bases: refs };
    let _ = collector.visit_expression(expr);
}

struct RefBaseCollector<'a> {
    bases: &'a mut FxHashSet<String>,
}

impl Visitor for RefBaseCollector<'_> {
    fn visit_expression(&mut self, expr: &ast::Expression) -> ControlFlow<()> {
        if let ast::Expression::Terminal { token, .. } = expr
            && token.text.as_ref() == "time"
        {
            self.bases.insert("time".to_string());
        }
        walk_expression_default(self, expr)
    }

    fn visit_component_reference_ctx(
        &mut self,
        cr: &ComponentReference,
        ctx: ComponentReferenceContext,
    ) -> ControlFlow<()> {
        // A function-call target (`cos` in `cos(x)`) is a function, not a variable.
        if matches!(ctx, ComponentReferenceContext::ExpressionFunctionCallTarget) {
            return ControlFlow::Continue(());
        }
        if let Some(part) = cr.parts.first() {
            self.bases.insert(part.ident.text.to_string());
        }
        // Descend into subscripts to capture references like `r[k]` where `k` is a
        // parameter (handled by the default component-reference walk).
        walk_component_reference_default(self, cr)
    }
}

/// Strip a trailing array subscript so `sig[1,2]` and `sig` compare equal.
fn base(name: &str) -> &str {
    name.split('[').next().unwrap_or(name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn time_blocks_parameter_variability_family() {
        let family = FamilyRefs {
            lhs_bases: FxHashSet::from_iter(["sampleX".to_string()]),
            external_refs: FxHashSet::from_iter(["time".to_string()]),
        };
        let parameter_constant_bases = FxHashSet::from_iter(["time"]);
        let mut param_variability = FxHashSet::default();

        let changed = classify_family(&family, &parameter_constant_bases, &mut param_variability);

        assert!(!changed);
        assert!(!param_variability.contains("sampleX"));
    }

    #[test]
    fn terminal_time_is_collected_as_runtime_reference() {
        let expr = ast::Expression::Terminal {
            terminal_type: ast::TerminalType::End,
            token: rumoca_core::Token {
                text: "time".into(),
                ..Default::default()
            },
            span: rumoca_core::Span::DUMMY,
        };
        let mut refs = FxHashSet::default();

        collect_reference_bases(&expr, &mut refs);

        assert!(refs.contains("time"));
    }
}
