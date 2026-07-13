//! Promote parameter-variable algebraics to derived parameters.
//!
//! An algebraic whose explicit defining equation has at most *parameter*
//! variability — every reference resolves (transitively) to a parameter,
//! constant, structural loop index, or another parameter-variable algebraic, and
//! never to a state, input, output, or discrete variable — is **constant for the
//! entire simulation**. Keeping it as a solver algebraic forces every consumer
//! (e.g. a state derivative) to either re-solve it each step or inline its full
//! defining expression; the latter duplicates large expressions (coordinate
//! transforms, immersed-boundary masks) across every kernel that uses them,
//! bloating codegen and recomputing a constant every step.
//!
//! Promoting it to a derived parameter — moving the variable into the parameter
//! partition with its defining equation(s) reconstructed into a `Variable.start`
//! binding, and dropping the now-redundant continuous equations — makes it a value
//! evaluated once at parameter-set time. Running this BEFORE condition lowering
//! also means a parameter-variable `if`-condition (e.g. `if sc < pc`) sees `sc` as
//! a parameter, so it stays directly evaluable instead of allocating an Appendix B
//! event/condition variable.
//!
//! The DAE may store array equations either as whole-array assignments or scalarized
//! into [`dae::StructuredEquationFamily`] blocks. Promotion therefore: (1) promotes
//! ordinary scalar and standalone whole-array assignments directly, and only
//! promotes structured variables whose entire family is promotable (never
//! splitting a family); (2)
//! reconstructs each promoted array variable's per-element bindings, in equation
//! order, into a flat array `start` expression; and (3) drops the emptied families
//! and shifts the surviving families' `first_equation_index` to match the compacted
//! equation vector.

use std::collections::{HashMap, HashSet};

use rumoca_core::{ExpressionRewriter, ExpressionVisitor};
use rumoca_ir_dae as dae;

use crate::errors::ToDaeError;

pub(crate) fn promote_parameter_variable_algebraics(dae: &mut dae::Dae) -> Result<(), ToDaeError> {
    // Base names of algebraic arrays whose flatten family was cheapened
    // (`interiors_materialized == false`): their per-cell interior bodies were dropped
    // to `0.0`, so promotion MUST rebuild them from the comprehension template. Captured
    // before promotion mutates the families below; the closing guard enforces that every
    // one of these was promoted via the template, never the literal-binding fallback or
    // a surviving cheapened equation.
    let cheapened_algebraic_bases = cheapened_algebraic_family_bases(dae);
    promote(dae, &cheapened_algebraic_bases)?;
    enforce_cheapened_algebraic_families_promoted(dae)?;
    Ok(())
}

fn promote(
    dae: &mut dae::Dae,
    cheapened_algebraic_bases: &HashSet<String>,
) -> Result<(), ToDaeError> {
    let promotable = parameter_variable_algebraics(dae);
    if promotable.is_empty() {
        return Ok(());
    }

    // Per-equation target/binding for promotable algebraics, by equation index. The
    // target's base name must be an *actual* algebraic variable key (a scalar or a
    // whole array), so the later variable move is guaranteed to find it. This
    // excludes record/connector member elements like `pin[1].v`, whose subscript
    // strip (`pin`) is not itself a variable — promoting those would orphan their
    // equations and leave a state derivative depending on a producerless slot.
    let is_array_variable = |target: &rumoca_core::VarName| {
        dae.variables
            .algebraics
            .contains_key(&rumoca_core::VarName::new(
                base(target.as_str()).to_string(),
            ))
    };
    // Every parameter-variable algebraic is invariant for the whole simulation.
    // Promote it even when no state derivative reaches it: visible observations and
    // assertion conditions are runtime consumers too, and leaving a large static
    // submodel in the algebraic partition makes them rebuild the same value at every
    // output point. Derived parameters preserve the value while evaluating it once.
    let mut removable: HashMap<usize, (rumoca_core::VarName, rumoca_core::Expression)> =
        HashMap::new();
    for (index, equation) in dae.continuous.equations.iter().enumerate() {
        if let Some((target, binding)) = direct_assignment(equation)
            && promotable.contains(base(target.as_str()))
            && is_array_variable(&target)
        {
            removable.insert(index, (target, binding.clone()));
        }
    }
    if removable.is_empty() {
        return Ok(());
    }

    // Family integrity: only remove equations whose entire structured family is
    // promotable. A family that is only partially promotable stays intact (those
    // variables remain algebraic) so the structured/stencil lowering is preserved.
    let family_removed = removable_indices_respecting_families(
        &removable,
        &dae.continuous.structured_equations,
        dae.continuous.equations.len(),
        dae,
    );
    if family_removed.is_empty() {
        return Ok(());
    }

    // Dependency closure: a variable can only be promoted if every algebraic its
    // binding references is *also* promoted — otherwise its parameter binding would
    // reference a value that does not exist at parameter-evaluation time (it is
    // still a solver algebraic). Iteratively drop candidates that reference a
    // non-promoted algebraic, then keep only the surviving variables' equations.
    let promotion_order = dependency_ordered_promotions(&removable, &family_removed, dae);
    let promoted_vars: HashSet<&str> = promotion_order.iter().map(String::as_str).collect();
    let removed: HashSet<usize> = family_removed
        .into_iter()
        .filter(|index| {
            removable
                .get(index)
                .is_some_and(|(target, _)| promoted_vars.contains(base(target.as_str())))
        })
        .collect();
    if removed.is_empty() {
        return Ok(());
    }

    // Collect each promoted variable's per-element bindings in equation order so we
    // can rebuild its array `start`.
    let mut grouped: indexmap::IndexMap<rumoca_core::VarName, Vec<rumoca_core::Expression>> =
        indexmap::IndexMap::new();
    let mut binding_span: HashMap<rumoca_core::VarName, rumoca_core::Span> = HashMap::new();
    for index in 0..dae.continuous.equations.len() {
        if !removed.contains(&index) {
            continue;
        }
        if let Some((target, binding)) = removable.remove(&index) {
            let key = rumoca_core::VarName::new(base(target.as_str()).to_string());
            binding_span
                .entry(key.clone())
                .or_insert(dae.continuous.equations[index].span);
            grouped.entry(key).or_default().push(binding);
        }
    }

    // Reconstruct each promoted variable's `start` as a compact comprehension from its
    // source family's symbolic template (so it stays tunable / array-native at init),
    // captured before the families are dropped and remapped below.
    let comprehension_starts =
        comprehension_starts_from_families(&dae.continuous.structured_equations, &removed);

    // Compact the equation vector and remap the structured families.
    let new_equations: Vec<dae::Equation> = std::mem::take(&mut dae.continuous.equations)
        .into_iter()
        .enumerate()
        .filter_map(|(index, eq)| (!removed.contains(&index)).then_some(eq))
        .collect();
    dae.continuous.equations = new_equations;
    remap_structured_families(&mut dae.continuous.structured_equations, &removed);

    // Move each promoted variable into the parameter partition with a reconstructed
    // array binding (a scalar variable keeps its single binding).
    for promoted_name in promotion_order {
        let key = rumoca_core::VarName::new(promoted_name);
        let Some(bindings) = grouped.shift_remove(&key) else {
            continue;
        };
        let Some((name, mut var)) = dae.variables.algebraics.shift_remove_entry(&key) else {
            continue;
        };
        let span = binding_span.get(&key).copied().unwrap_or(var.source_span);
        var.start = Some(promoted_start(
            &key,
            bindings,
            span,
            &comprehension_starts,
            cheapened_algebraic_bases,
        )?);
        var.start_span = Some(span);
        dae.variables.parameters.insert(name, var);
    }
    Ok(())
}

/// The `start` for a promoted variable: prefer the compact comprehension when it spans
/// the variable's full extent; otherwise reconstruct the per-element array literal
/// (boundary-split families, multi-binder grids, or any shape the comprehension capture
/// declined).
///
/// A *cheapened* family is the exception: its per-element bindings are the dropped `0.0`
/// interiors, so the literal fallback would freeze the variable at a mostly-zero array.
/// Erroring here (rather than `expect`/panic) keeps DAE lowering recoverable and gives
/// the caller a spanned diagnostic — see [`enforce_cheapened_algebraic_families_promoted`]
/// for why this is a contract violation rather than an internal-only invariant.
fn promoted_start(
    key: &rumoca_core::VarName,
    bindings: Vec<rumoca_core::Expression>,
    span: rumoca_core::Span,
    comprehension_starts: &HashMap<String, (rumoca_core::Expression, usize)>,
    cheapened_algebraic_bases: &HashSet<String>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    if let Some((comprehension, extent)) = comprehension_starts.get(base(key.as_str()))
        && *extent == bindings.len()
    {
        return Ok(comprehension.clone());
    }
    if cheapened_algebraic_bases.contains(base(key.as_str())) {
        let extent = comprehension_starts
            .get(base(key.as_str()))
            .map_or(0, |(_, extent)| *extent);
        return Err(ToDaeError::runtime_contract_violation_with_span(
            format!(
                "cheapened parameter-variability family `{}` was not reconstructed from its \
                 comprehension template (extent {extent} vs {} promoted cells); its dropped \
                 per-cell bodies cannot be recovered",
                base(key.as_str()),
                bindings.len(),
            ),
            span,
        ));
    }
    Ok(reconstructed_binding(bindings, span))
}

/// Each cheapened algebraic structured family as `(target_base, family_span)`: a family
/// flattened with `interiors_materialized == false` whose leaves are algebraic
/// assignments (no derivative). Its interior per-cell bodies were dropped to `0.0`, so
/// it is correct ONLY if promotion rebuilds it from the comprehension template. A
/// state-derivative family (also cheapened) carries a derivative leaf and is excluded —
/// solve rebuilds its stencil from the corners, independent of promotion.
fn cheapened_algebraic_families(dae: &dae::Dae) -> Vec<(String, rumoca_core::Span)> {
    let mut out = Vec::new();
    for family in &dae.continuous.structured_equations {
        if family.interiors_materialized {
            continue;
        }
        let total: usize = family.equation_counts.iter().sum();
        let start = family.first_equation_index;
        let Some(equations) = dae.continuous.equations.get(start..start + total) else {
            continue;
        };
        if equations.iter().any(|eq| eq.rhs.contains_der()) {
            continue;
        }
        for equation in equations {
            if let Some((target, _)) = direct_assignment(equation) {
                out.push((base(target.as_str()).to_string(), family.span));
            }
        }
    }
    out
}

/// Base names of algebraic arrays defined by a cheapened structured family (see
/// [`cheapened_algebraic_families`]). Captured before promotion so the cheapen-aware
/// reconstruction guard and `worthwhile` override can reference them.
fn cheapened_algebraic_family_bases(dae: &dae::Dae) -> HashSet<String> {
    cheapened_algebraic_families(dae)
        .into_iter()
        .map(|(target, _)| target)
        .collect()
}

/// Fail-early guard (defensive coding): after promotion, NO cheapened algebraic family
/// may survive in the structured equations. A survivor still carries its dropped `0.0`
/// interior bodies — promotion failed to rebuild it from the template (un-promotable
/// family shape, broken dependency closure, …) — which would feed silently-wrong values
/// to the solver. Reject loudly instead.
///
/// This is an internal cross-phase invariant (flatten cheapened a family the DAE then
/// could not promote), so SPEC_0008's table would permit a `panic!`/`expect`. A typed,
/// spanned `ToDaeError` is chosen deliberately: it keeps DAE lowering recoverable (the
/// session reports the failing model instead of aborting the process) and points at the
/// offending family. It should be unreachable in practice — flatten only cheapens
/// parameter-variability ∩ derivative-reachable families (see
/// `rumoca_phase_flatten`'s `param_variability::parameter_variability_family_bases`);
/// the guard exists to catch drift between those analyses before it becomes a silent
/// miscompile.
fn enforce_cheapened_algebraic_families_promoted(dae: &dae::Dae) -> Result<(), ToDaeError> {
    let survivors = cheapened_algebraic_families(dae);
    let Some(&(_, span)) = survivors.first() else {
        return Ok(());
    };
    let mut names: Vec<String> = survivors.into_iter().map(|(target, _)| target).collect();
    names.sort_unstable();
    names.dedup();
    Err(ToDaeError::runtime_contract_violation_with_span(
        format!(
            "cheapened parameter-variability algebraic famil{} ({}) survived DAE promotion with \
             dropped per-cell bodies; expected promotion to a derived parameter reconstructed \
             from the comprehension template",
            if names.len() == 1 { "y" } else { "ies" },
            names.join(", "),
        ),
        span,
    ))
}

/// Rebuild a variable's `start` from its per-element bindings: a single binding for
/// a scalar, a flat array expression for an array variable (the elements are in
/// equation order, which matches the variable's row-major scalar layout).
fn reconstructed_binding(
    mut bindings: Vec<rumoca_core::Expression>,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    if bindings.len() == 1 {
        return bindings.pop().expect("len checked");
    }
    rumoca_core::Expression::Array {
        elements: bindings,
        is_matrix: false,
        span,
    }
}

/// For each promoted variable defined by a fully-promoted regular family with a
/// captured comprehension template, the array comprehension
/// `{ rhs(i, j, …) for i in dom0, j in dom1, … }` reconstructed from the template's
/// symbolic body, paired with the family's total cell count.
///
/// Keeping the promoted parameter's `start` as a comprehension (instead of a per-cell
/// array literal) leaves it compact AND tunable: it is computed array-natively at
/// init, and the start-value constant fold — which cannot evaluate a comprehension —
/// leaves it intact rather than baking it to literals. The comprehension iterates its
/// binders outer-to-inner (binder 0 is the major axis), matching the family's
/// row-major scalar layout / equation order. Families that do not span their
/// variable's full extent (boundary-split shapes) fall back to the literal array
/// reconstruction, which the caller selects via the element-count guard.
fn comprehension_starts_from_families(
    families: &[dae::StructuredEquationFamily],
    removed: &HashSet<usize>,
) -> HashMap<String, (rumoca_core::Expression, usize)> {
    let mut result = HashMap::new();
    for family in families {
        let total: usize = family.equation_counts.iter().sum();
        let fully_removed = (family.first_equation_index..family.first_equation_index + total)
            .all(|index| removed.contains(&index));
        if !fully_removed {
            continue;
        }
        let Some(template) = family.template.as_ref() else {
            continue;
        };
        if family.domain.binders.is_empty() {
            continue;
        }
        // One comprehension index per binder (in domain order = outer-to-inner), and
        // the product of their extents = the variable's flat element count.
        let mut extent = 1usize;
        let mut indices = Vec::with_capacity(family.domain.binders.len());
        let mut degenerate = false;
        for binder in &family.domain.binders {
            let Some(binder_cells) = binder_extent(binder) else {
                degenerate = true;
                break;
            };
            let Some(next) = extent.checked_mul(binder_cells) else {
                degenerate = true;
                break;
            };
            extent = next;
            indices.push(rumoca_core::ComprehensionIndex {
                name: binder.display_name.clone(),
                range: binder_range_expr(binder, family.span),
            });
        }
        if degenerate {
            continue;
        }
        let binder_names = indices
            .iter()
            .map(|index| index.name.clone())
            .collect::<HashSet<_>>();
        for body in &template.body {
            if let Some((target, rhs)) = residual_template_target_and_rhs(body) {
                let mut rewriter = ComprehensionBinderRewriter {
                    binder_names: &binder_names,
                };
                let comprehension = rumoca_core::Expression::ArrayComprehension {
                    expr: Box::new(rewriter.rewrite_expression(rhs)),
                    indices: indices.clone(),
                    filter: None,
                    span: family.span,
                };
                result.insert(target, (comprehension, extent));
            }
        }
    }
    result
}

struct ComprehensionBinderRewriter<'a> {
    binder_names: &'a HashSet<String>,
}

impl ExpressionRewriter for ComprehensionBinderRewriter<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let binder_name = self
            .binder_names
            .iter()
            .find(|binder| name.as_str() == binder.as_str());
        let name = binder_name.map_or_else(|| name.clone(), rumoca_core::Reference::generated);
        rumoca_core::Expression::VarRef {
            name,
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }
}

/// Extract `(target_base, rhs)` from a residual template body `lhs - rhs` whose target
/// side is the indexed family variable (e.g. `m_shell[i] - rho*V_shell[i]`). Mirrors
/// [`direct_assignment`] but operates on a symbolic template expression whose target is
/// an indexed access rather than a whole-array reference.
fn residual_template_target_and_rhs(
    residual: &rumoca_core::Expression,
) -> Option<(String, &rumoca_core::Expression)> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = residual
    else {
        return None;
    };
    if let Some(target) = indexed_base_name(lhs) {
        return Some((target, rhs));
    }
    if let Some(target) = indexed_base_name(rhs) {
        return Some((target, lhs));
    }
    None
}

/// Base variable name of an indexed access `var[...]` (peeling `Index` layers) or a
/// bare `var`; `None` for any other expression shape.
fn indexed_base_name(expr: &rumoca_core::Expression) -> Option<String> {
    match expr {
        rumoca_core::Expression::Index { base, .. } => indexed_base_name(base),
        rumoca_core::Expression::VarRef { name, .. } => {
            Some(base(name.var_name().as_str()).to_string())
        }
        _ => None,
    }
}

/// Cell count of a single binder: `(upper - lower) / step + 1`, or `0` for an empty
/// range. `None` when the step is degenerate.
fn binder_extent(binder: &rumoca_core::StructuredIndexBinder) -> Option<usize> {
    if binder.step == 0 {
        return None;
    }
    let span = binder.upper - binder.lower;
    if span != 0 && span.signum() != binder.step.signum() {
        return Some(0);
    }
    usize::try_from(span / binder.step + 1).ok()
}

/// Build the `lower:step:upper` (or `lower:upper`) range expression for a comprehension
/// index over a structured binder.
fn binder_range_expr(
    binder: &rumoca_core::StructuredIndexBinder,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    let int_lit = |value: i64| {
        Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            span,
        })
    };
    rumoca_core::Expression::Range {
        start: int_lit(binder.lower),
        step: (binder.step != 1).then(|| int_lit(binder.step)),
        end: int_lit(binder.upper),
        span,
    }
}

/// Dependency order of variables that can be promoted without changing the scalar
/// equation balance, splitting a structured family, or creating cyclic parameter
/// bindings. Derived parameters are appended in this order because parameter start
/// evaluation expects every dependency to precede its consumers.
struct PromotionGraph {
    candidate_order: Vec<String>,
    algebraic_refs: HashMap<String, HashSet<String>>,
    promoted: HashSet<String>,
}

fn build_promotion_graph(
    removable: &HashMap<usize, (rumoca_core::VarName, rumoca_core::Expression)>,
    family_removed: &HashSet<usize>,
    dae: &dae::Dae,
) -> PromotionGraph {
    let algebraics: HashSet<String> = dae
        .variables
        .algebraics
        .keys()
        .map(|k| base(k.as_str()).to_string())
        .collect();
    let mut all_indices: HashMap<String, Vec<usize>> = HashMap::new();
    for (index, (target, _)) in removable {
        all_indices
            .entry(base(target.as_str()).to_string())
            .or_default()
            .push(*index);
    }

    // Per-candidate-variable: its equation scalar count and the algebraic base
    // names referenced by its reconstructed binding.
    let mut algebraic_refs: HashMap<String, HashSet<String>> = HashMap::new();
    let mut equation_scalar_counts: HashMap<String, usize> = HashMap::new();
    for index in family_removed {
        let Some((target, binding)) = removable.get(index) else {
            continue;
        };
        let Some(equation) = dae.continuous.equations.get(*index) else {
            continue;
        };
        let target_base = base(target.as_str()).to_string();
        let Some(total) = equation_scalar_counts
            .get(&target_base)
            .copied()
            .unwrap_or(0)
            .checked_add(equation.scalar_count)
        else {
            continue;
        };
        equation_scalar_counts.insert(target_base.clone(), total);
        let mut collector = ReferencedBases::default();
        collector.visit_expression(binding);
        let entry = algebraic_refs.entry(target_base).or_default();
        entry.extend(
            collector
                .bases
                .into_iter()
                .filter(|r| algebraics.contains(r)),
        );
    }
    let candidate_order = dae
        .variables
        .algebraics
        .keys()
        .map(|name| name.as_str().to_string())
        .collect::<Vec<_>>();
    let promoted = candidate_order
        .iter()
        .filter(|name| {
            promotion_candidate_is_complete(
                name,
                &algebraic_refs,
                &all_indices,
                &equation_scalar_counts,
                family_removed,
                dae,
            )
        })
        .cloned()
        .collect();
    PromotionGraph {
        candidate_order,
        algebraic_refs,
        promoted,
    }
}

fn promotion_candidate_is_complete(
    name: &str,
    algebraic_refs: &HashMap<String, HashSet<String>>,
    all_indices: &HashMap<String, Vec<usize>>,
    equation_scalar_counts: &HashMap<String, usize>,
    family_removed: &HashSet<usize>,
    dae: &dae::Dae,
) -> bool {
    let Some(variable) = dae
        .variables
        .algebraics
        .get(&rumoca_core::VarName::new(name))
    else {
        return false;
    };
    let Some(variable_size) = variable.try_size().ok() else {
        return false;
    };
    algebraic_refs.contains_key(name)
        && all_indices
            .get(name)
            .is_some_and(|indices| indices.iter().all(|index| family_removed.contains(index)))
        && equation_scalar_counts.get(name).copied() == Some(variable_size)
}

fn prune_dangling_promotions(
    promoted: &mut HashSet<String>,
    algebraic_refs: &HashMap<String, HashSet<String>>,
) {
    loop {
        let to_remove = promoted
            .iter()
            .filter(|var| promotion_has_dangling_reference(var, promoted, algebraic_refs))
            .cloned()
            .collect::<Vec<_>>();
        if to_remove.is_empty() {
            return;
        }
        for var in to_remove {
            promoted.remove(&var);
        }
    }
}

fn promotion_has_dangling_reference(
    var: &str,
    promoted: &HashSet<String>,
    algebraic_refs: &HashMap<String, HashSet<String>>,
) -> bool {
    algebraic_refs.get(var).is_some_and(|refs| {
        refs.iter()
            .any(|reference| reference == var || !promoted.contains(reference))
    })
}

fn dependency_ordered_promotions(
    removable: &HashMap<usize, (rumoca_core::VarName, rumoca_core::Expression)>,
    family_removed: &HashSet<usize>,
    dae: &dae::Dae,
) -> Vec<String> {
    let PromotionGraph {
        candidate_order,
        algebraic_refs,
        mut promoted,
    } = build_promotion_graph(removable, family_removed, dae);

    let family_targets = structured_family_targets(removable, family_removed, dae);
    loop {
        prune_dangling_promotions(&mut promoted, &algebraic_refs);

        let split_family_targets = family_targets
            .iter()
            .filter(|targets| {
                targets
                    .iter()
                    .any(|target| !promoted.contains(target.as_str()))
            })
            .flat_map(|targets| targets.iter().cloned())
            .collect::<HashSet<_>>();
        let before_family_prune = promoted.len();
        promoted.retain(|name| !split_family_targets.contains(name));
        if promoted.len() != before_family_prune {
            continue;
        }

        let order = topological_promotion_order(&candidate_order, &algebraic_refs, &promoted);
        if order.len() == promoted.len() {
            return order;
        }
        let acyclic: HashSet<&str> = order.iter().map(String::as_str).collect();
        promoted.retain(|name| acyclic.contains(name.as_str()));
    }
}

fn structured_family_targets(
    removable: &HashMap<usize, (rumoca_core::VarName, rumoca_core::Expression)>,
    family_removed: &HashSet<usize>,
    dae: &dae::Dae,
) -> Vec<HashSet<String>> {
    dae.continuous
        .structured_equations
        .iter()
        .filter_map(|family| {
            let total: usize = family.equation_counts.iter().sum();
            let indices = family.first_equation_index..family.first_equation_index + total;
            if !indices.clone().all(|index| family_removed.contains(&index)) {
                return None;
            }
            Some(
                indices
                    .filter_map(|index| removable.get(&index))
                    .map(|(target, _)| base(target.as_str()).to_string())
                    .collect(),
            )
        })
        .filter(|targets: &HashSet<String>| !targets.is_empty())
        .collect()
}

fn topological_promotion_order(
    candidate_order: &[String],
    algebraic_refs: &HashMap<String, HashSet<String>>,
    promoted: &HashSet<String>,
) -> Vec<String> {
    let mut remaining = promoted.clone();
    let mut order = Vec::with_capacity(remaining.len());
    loop {
        let next = candidate_order.iter().find(|candidate| {
            remaining.contains(candidate.as_str())
                && algebraic_refs.get(candidate.as_str()).is_none_or(|refs| {
                    refs.iter()
                        .all(|reference| !remaining.contains(reference.as_str()))
                })
        });
        let Some(next) = next else {
            break;
        };
        remaining.remove(next.as_str());
        order.push(next.clone());
    }
    order
}

/// Restrict the removable equation set so no structured family is split: an
/// equation is removed only if every equation in its family is removable. Equations
/// outside any family are removed individually.
fn removable_indices_respecting_families(
    removable: &HashMap<usize, (rumoca_core::VarName, rumoca_core::Expression)>,
    families: &[dae::StructuredEquationFamily],
    equation_count: usize,
    dae: &dae::Dae,
) -> HashSet<usize> {
    // Map each equation index to its owning family (if any).
    let mut family_of: Vec<Option<usize>> = vec![None; equation_count];
    for (family_index, family) in families.iter().enumerate() {
        let span: usize = family.equation_counts.iter().sum();
        for index in family.first_equation_index..family.first_equation_index + span {
            if let Some(slot) = family_of.get_mut(index) {
                *slot = Some(family_index);
            }
        }
    }
    // A family is fully promotable iff every one of its equations is removable.
    let mut family_all_removable: Vec<bool> = vec![true; families.len()];
    for (family_index, family) in families.iter().enumerate() {
        let span: usize = family.equation_counts.iter().sum();
        for index in family.first_equation_index..family.first_equation_index + span {
            if !removable.contains_key(&index) {
                family_all_removable[family_index] = false;
                break;
            }
        }
    }
    removable
        .keys()
        .copied()
        .filter(|index| match family_of.get(*index).copied().flatten() {
            Some(family_index) => family_all_removable[family_index],
            None => removable.get(index).is_some_and(|(target, _)| {
                let base_name = rumoca_core::VarName::new(base(target.as_str()).to_string());
                let Some(equation) = dae.continuous.equations.get(*index) else {
                    return false;
                };
                let Some(variable) = dae.variables.algebraics.get(&base_name) else {
                    return false;
                };
                target.as_str() == base(target.as_str())
                    && ((variable.dims.is_empty() && equation.origin.starts_with("equation from "))
                        || (!variable.dims.is_empty() && equation.scalar_count > 1))
            }),
        })
        .collect()
}

/// Drop families whose equations were all removed and shift the survivors'
/// `first_equation_index` down by the number of removed equations preceding them.
fn remap_structured_families(
    families: &mut Vec<dae::StructuredEquationFamily>,
    removed: &HashSet<usize>,
) {
    // Prefix count of removed equations strictly before each index.
    let max_index = families
        .iter()
        .map(|f| f.first_equation_index + f.equation_counts.iter().sum::<usize>())
        .max()
        .unwrap_or(0);
    let mut removed_before = vec![0usize; max_index + 1];
    for index in 0..max_index {
        removed_before[index + 1] = removed_before[index] + usize::from(removed.contains(&index));
    }
    families.retain(|family| {
        let span: usize = family.equation_counts.iter().sum();
        // Keep a family only if none of its equations were removed.
        !(family.first_equation_index..family.first_equation_index + span)
            .any(|index| removed.contains(&index))
    });
    for family in families.iter_mut() {
        let shift = removed_before
            .get(family.first_equation_index)
            .copied()
            .unwrap_or(0);
        family.first_equation_index -= shift;
    }
}

/// Strip a trailing array subscript so `sig[1,2]` and `sig` compare equal.
fn base(name: &str) -> &str {
    name.split('[').next().unwrap_or(name)
}

/// Base names of algebraics provably ≤parameter variability (see module docs).
fn parameter_variable_algebraics(dae: &dae::Dae) -> HashSet<String> {
    let base_names = |map: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>| {
        map.keys()
            .map(|k| base(k.as_str()).to_string())
            .collect::<HashSet<String>>()
    };
    let mut static_references = base_names(&dae.variables.parameters);
    static_references.extend(base_names(&dae.variables.constants));
    static_references.extend(
        dae.symbols
            .enum_literal_ordinals
            .keys()
            .map(|name| base(name).to_string()),
    );
    let mut disqualifying = base_names(&dae.variables.states);
    disqualifying.extend(base_names(&dae.variables.inputs));
    disqualifying.extend(base_names(&dae.variables.outputs));
    disqualifying.extend(base_names(&dae.variables.discrete_reals));
    disqualifying.extend(base_names(&dae.variables.discrete_valued));
    let algebraics = base_names(&dae.variables.algebraics);

    // Union the referenced bases across ALL of a target's element equations. An array
    // algebraic is only parameter-variable if EVERY element is -- e.g. `Q_cond` has
    // constant boundary elements (`Q_cond[1] = 0`, `Q_cond[Np1] = 0`) whose empty
    // ref-set is vacuously "provable", but its interior elements
    // (`Q_cond[i] = ... (T[i-1] - T[i]) ...`) reference the state `T`. Checking each
    // element equation independently would let the constant boundary rows promote the
    // whole variable to a parameter, freezing it at its t=0 value.
    let mut definitions: indexmap::IndexMap<String, HashSet<String>> = indexmap::IndexMap::new();
    for equation in &dae.continuous.equations {
        let Some((target, binding)) = direct_assignment(equation) else {
            continue;
        };
        let target = base(target.as_str()).to_string();
        if !algebraics.contains(&target) {
            continue;
        }
        let mut collector = ReferencedBases::default();
        collector.visit_expression(binding);
        definitions
            .entry(target)
            .or_default()
            .extend(collector.bases);
    }

    let mut parameter_variable: HashSet<String> = HashSet::new();
    loop {
        let mut changed = false;
        for (target, refs) in &definitions {
            if parameter_variable.contains(target) {
                continue;
            }
            let provable = refs.iter().all(|r| {
                reference_is_parameter_variable(
                    r,
                    target,
                    &disqualifying,
                    &algebraics,
                    &parameter_variable,
                    &static_references,
                )
            });
            if provable {
                parameter_variable.insert(target.clone());
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    parameter_variable
}

/// Whether a single reference `r` in `target`'s defining expression keeps `target`
/// classifiable as parameter-variable: a state/input/output/discrete reference
/// breaks it; another algebraic must already be proven parameter-variable (or be
/// `target` itself); every other reference must resolve to a known parameter,
/// constant, or enumeration literal. Array-comprehension indices are removed by
/// [`ReferencedBases`] as lexical locals.
fn reference_is_parameter_variable(
    r: &str,
    target: &str,
    disqualifying: &HashSet<String>,
    algebraics: &HashSet<String>,
    parameter_variable: &HashSet<String>,
    static_references: &HashSet<String>,
) -> bool {
    if r == "time" {
        return false;
    }
    if disqualifying.contains(r) {
        return false;
    }
    if algebraics.contains(r) {
        return r == target || parameter_variable.contains(r);
    }
    static_references.contains(r)
}

/// Interpret an equation as `target := binding`, handling both explicit
/// (`lhs = Some(target)`, `rhs = binding`) and residual (`lhs = None`,
/// `rhs = target - binding`) encodings. Never matches a derivative equation.
fn direct_assignment(
    equation: &dae::Equation,
) -> Option<(rumoca_core::VarName, &rumoca_core::Expression)> {
    if let Some(lhs) = equation.lhs.as_ref() {
        if !equation.rhs.contains_der() {
            return Some((lhs.var_name().clone(), &equation.rhs));
        }
        return None;
    }
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.rhs
    else {
        return None;
    };
    if let Some(target) = whole_var_ref(lhs)
        && !rhs.contains_der()
    {
        return Some((target, rhs));
    }
    if let Some(target) = whole_var_ref(rhs)
        && !lhs.contains_der()
    {
        return Some((target, lhs));
    }
    None
}

fn whole_var_ref(expr: &rumoca_core::Expression) -> Option<rumoca_core::VarName> {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => Some(name.var_name().clone()),
        _ => None,
    }
}

#[derive(Default)]
struct ReferencedBases {
    bases: HashSet<String>,
    local_names: HashMap<String, usize>,
}

impl ExpressionVisitor for ReferencedBases {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        let name = base(name.var_name().as_str());
        if !self.local_names.contains_key(name) {
            self.bases.insert(name.to_string());
        }
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }

    fn enter_scope(&mut self, scope: rumoca_core::ExpressionScope<'_>) {
        let rumoca_core::ExpressionScope::ArrayComprehension(indices) = scope;
        for index in indices {
            *self.local_names.entry(index.name.clone()).or_default() += 1;
        }
    }

    fn exit_scope(&mut self, scope: rumoca_core::ExpressionScope<'_>) {
        let rumoca_core::ExpressionScope::ArrayComprehension(indices) = scope;
        for index in indices {
            let Some(depth) = self.local_names.get_mut(index.name.as_str()) else {
                continue;
            };
            *depth -= 1;
            if *depth == 0 {
                self.local_names.remove(index.name.as_str());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("promotion_test.mo"),
            1,
            2,
        )
    }

    fn literal(value: f64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: test_span(),
        }
    }

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::generated(name),
            subscripts: Vec::new(),
            span: test_span(),
        }
    }

    fn insert_scalar_algebraic(dae_model: &mut dae::Dae, name: &str) {
        let name = rumoca_core::VarName::new(name);
        dae_model
            .variables
            .algebraics
            .insert(name.clone(), dae::Variable::new(name, test_span()));
    }

    fn push_scalar_equation(
        dae_model: &mut dae::Dae,
        target: &str,
        binding: rumoca_core::Expression,
    ) {
        dae_model.continuous.equations.push(dae::Equation::explicit(
            rumoca_core::VarName::new(target),
            binding,
            test_span(),
            "equation from promotion fixture",
        ));
    }

    #[test]
    fn time_is_not_parameter_variable_reference() {
        let disqualifying = HashSet::new();
        let algebraics = HashSet::new();
        let parameter_variable = HashSet::from(["time".to_string()]);
        let static_references = HashSet::new();

        assert!(!reference_is_parameter_variable(
            "time",
            "sampleX",
            &disqualifying,
            &algebraics,
            &parameter_variable,
            &static_references,
        ));
    }

    #[test]
    fn promoted_parameters_are_ordered_before_their_consumers() {
        let mut dae_model = dae::Dae::default();
        insert_scalar_algebraic(&mut dae_model, "dependent");
        insert_scalar_algebraic(&mut dae_model, "source");
        push_scalar_equation(&mut dae_model, "dependent", var_ref("source"));
        push_scalar_equation(&mut dae_model, "source", literal(2.0));

        promote_parameter_variable_algebraics(&mut dae_model).unwrap();

        assert_eq!(
            dae_model
                .variables
                .parameters
                .keys()
                .map(rumoca_core::VarName::as_str)
                .collect::<Vec<_>>(),
            vec!["source", "dependent"]
        );
        assert!(dae_model.continuous.equations.is_empty());
    }

    #[test]
    fn unresolved_aggregate_reference_prevents_promotion() {
        let mut dae_model = dae::Dae::default();
        insert_scalar_algebraic(&mut dae_model, "result");
        push_scalar_equation(&mut dae_model, "result", var_ref("component.array.field"));

        promote_parameter_variable_algebraics(&mut dae_model).unwrap();

        assert!(
            dae_model
                .variables
                .algebraics
                .contains_key(&rumoca_core::VarName::new("result"))
        );
        assert_eq!(dae_model.continuous.equations.len(), 1);
    }

    #[test]
    fn duplicate_scalar_definitions_do_not_change_equation_balance() {
        let mut dae_model = dae::Dae::default();
        insert_scalar_algebraic(&mut dae_model, "result");
        push_scalar_equation(&mut dae_model, "result", literal(1.0));
        push_scalar_equation(&mut dae_model, "result", literal(2.0));

        promote_parameter_variable_algebraics(&mut dae_model).unwrap();

        assert!(
            dae_model
                .variables
                .algebraics
                .contains_key(&rumoca_core::VarName::new("result"))
        );
        assert_eq!(dae_model.continuous.equations.len(), 2);
    }

    #[test]
    fn template_binder_is_rewritten_to_comprehension_local() {
        let binder_names = HashSet::from(["i".to_string()]);
        let mut rewriter = ComprehensionBinderRewriter {
            binder_names: &binder_names,
        };
        let rewritten = rewriter.rewrite_expression(&rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("i"),
            subscripts: Vec::new(),
            span: test_span(),
        });

        let rumoca_core::Expression::VarRef { name, .. } = rewritten else {
            panic!("binder reference should remain a variable reference");
        };
        assert_eq!(name.as_str(), "i");
        assert!(name.is_generated());
    }

    #[test]
    fn qualified_component_with_binder_suffix_is_not_rewritten() {
        let binder_names = HashSet::from(["i".to_string()]);
        let mut rewriter = ComprehensionBinderRewriter {
            binder_names: &binder_names,
        };
        let rewritten = rewriter.rewrite_expression(&rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("pathPlanning.path.i"),
            subscripts: Vec::new(),
            span: test_span(),
        });

        let rumoca_core::Expression::VarRef { name, .. } = rewritten else {
            panic!("component reference should remain a variable reference");
        };
        assert_eq!(name.as_str(), "pathPlanning.path.i");
        assert!(!name.is_generated());
    }

    #[test]
    fn promotes_ordinary_time_invariant_scalar_equation() {
        let span = test_span();
        let name = rumoca_core::VarName::new("staticResult");
        let mut dae_model = dae::Dae::default();
        dae_model
            .variables
            .algebraics
            .insert(name.clone(), dae::Variable::new(name.clone(), span));
        dae_model.continuous.equations.push(dae::Equation::explicit(
            name.clone(),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(3.0),
                span,
            },
            span,
            "equation from static fixture",
        ));

        promote_parameter_variable_algebraics(&mut dae_model)
            .expect("ordinary invariant scalar should promote");

        assert!(dae_model.variables.algebraics.is_empty());
        assert!(dae_model.variables.parameters.contains_key(&name));
        assert!(dae_model.continuous.equations.is_empty());
    }
}
