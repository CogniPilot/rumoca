//! Incidence matrix construction for DAE structural analysis.

use std::collections::{HashMap, HashSet};

use rumoca_core::{ExpressionVisitor, Literal};
use rumoca_ir_dae as dae;

use crate::types::{EquationRef, UnknownId};

/// Incidence data for a DAE system.
#[derive(Debug)]
pub struct Incidence {
    /// Number of equations.
    pub n_eq: usize,
    /// Number of unknowns.
    pub n_var: usize,
    /// For each equation, the set of unknown indices it references.
    pub eq_unknowns: Vec<HashSet<usize>>,
    /// Ordered list of unknown identifiers (index → `UnknownId`).
    pub unknown_names: Vec<UnknownId>,
    /// Source span of each unknown (index → span), parallel to `unknown_names`,
    /// so structural-singularity errors are traceable back to the offending
    /// model variable. `None` represents unknowns not sourced from DAE variables.
    pub unknown_spans: Vec<Option<rumoca_core::Span>>,
    /// dae::Equation references (index → `EquationRef`).
    pub equation_refs: Vec<EquationRef>,
}

impl Incidence {
    /// Create a new incidence matrix from pre-built data.
    pub fn new(
        eq_unknowns: Vec<HashSet<usize>>,
        equation_refs: Vec<EquationRef>,
        unknown_names: Vec<UnknownId>,
    ) -> Self {
        let n_eq = eq_unknowns.len();
        let n_var = unknown_names.len();
        Self {
            n_eq,
            n_var,
            eq_unknowns,
            unknown_spans: vec![None; n_var],
            unknown_names,
            equation_refs,
        }
    }
}

/// Build incidence data from a DAE.
pub(crate) fn build_incidence(dae: &dae::Dae) -> Incidence {
    let (_unknown_map, unknown_names, unknown_spans) = build_unknown_map(dae);
    let (der_resolver, variable_resolver) = build_unknown_resolvers(&unknown_names);
    let constants = ConstantEvalContext::from_dae(dae);

    let mut equation_refs = Vec::new();
    let mut equations = Vec::new();

    for (i, eq) in dae.continuous.equations.iter().enumerate() {
        equation_refs.push(EquationRef(i));
        equations.push(eq);
    }

    let n_eq = equation_refs.len();

    let mut eq_unknowns: Vec<HashSet<usize>> = equations
        .iter()
        .map(|eq| collect_equation_unknowns(eq, &der_resolver, &variable_resolver, &constants))
        .collect();

    apply_regular_family_corner_incidence(dae, &mut eq_unknowns);
    debug_check_regular_family_incidence(dae, &eq_unknowns);
    let (unknown_names, unknown_spans) = prune_zero_degree_scalarized_aggregate_variables(
        dae,
        &mut eq_unknowns,
        unknown_names,
        unknown_spans,
    );

    Incidence {
        n_eq,
        n_var: unknown_names.len(),
        eq_unknowns,
        unknown_names,
        unknown_spans,
        equation_refs,
    }
}

fn prune_zero_degree_scalarized_aggregate_variables(
    dae: &dae::Dae,
    eq_unknowns: &mut [HashSet<usize>],
    unknown_names: Vec<UnknownId>,
    unknown_spans: Vec<Option<rumoca_core::Span>>,
) -> (Vec<UnknownId>, Vec<Option<rumoca_core::Span>>) {
    let mut has_incidence = vec![false; unknown_names.len()];
    for row in eq_unknowns.iter() {
        for &idx in row {
            if let Some(slot) = has_incidence.get_mut(idx) {
                *slot = true;
            }
        }
    }

    let mut remap = vec![None; unknown_names.len()];
    let mut pruned_names = Vec::with_capacity(unknown_names.len());
    let mut pruned_spans = Vec::with_capacity(unknown_spans.len());
    for (old_idx, (unknown, span)) in unknown_names.into_iter().zip(unknown_spans).enumerate() {
        if !has_incidence[old_idx] && is_scalarized_aggregate_variable_unknown(dae, &unknown) {
            continue;
        }
        let new_idx = pruned_names.len();
        remap[old_idx] = Some(new_idx);
        pruned_names.push(unknown);
        pruned_spans.push(span);
    }

    for row in eq_unknowns.iter_mut() {
        let remapped = row
            .iter()
            .filter_map(|&idx| remap.get(idx).and_then(|mapped| *mapped))
            .collect();
        *row = remapped;
    }

    (pruned_names, pruned_spans)
}

fn is_scalarized_aggregate_variable_unknown(dae: &dae::Dae, unknown: &UnknownId) -> bool {
    let UnknownId::Variable(name) = unknown else {
        return false;
    };
    let Some(scalar) = rumoca_core::parse_scalar_name(name.as_str()) else {
        return false;
    };
    let base = rumoca_core::VarName::new(scalar.base);
    dae.variables
        .algebraics
        .get(&base)
        .or_else(|| dae.variables.outputs.get(&base))
        .is_some_and(|var| var.size() > 1)
}

/// Debug-only invariant guarding the P3 family-native lowering contract: every
/// `regular` family's materialized incidence equals the incidence
/// [`synthesize_regular_family_incidence`] reconstructs from its corner rows alone.
/// Validated here while every cell is still present, so a classification bug (a
/// family marked `regular` that is not corner-derivable) fails loudly in debug
/// builds rather than silently lowering wrong once the interior rows are gone.
fn debug_check_regular_family_incidence(dae: &dae::Dae, eq_unknowns: &[HashSet<usize>]) {
    if !cfg!(debug_assertions) {
        return;
    }
    for family in &dae.continuous.structured_equations {
        // Only materialized families can be validated against their walked rows;
        // cheapened families were just reconstructed from corners by the apply pass,
        // so there is no independent materialized incidence to compare against.
        if family.regular.is_some() && family.interiors_materialized {
            debug_check_one_regular_family(family, eq_unknowns);
        }
    }
}

/// Production pass: for every regular family whose interior cells were NOT
/// materialized (`interiors_materialized == false`), overwrite the placeholder
/// interior incidence with the incidence reconstructed from the family's corner
/// rows. Materialized families are left untouched (their walked incidence is
/// authoritative and validated separately in debug builds).
fn apply_regular_family_corner_incidence(dae: &dae::Dae, eq_unknowns: &mut [HashSet<usize>]) {
    for family in &dae.continuous.structured_equations {
        if family.regular.is_none() || family.interiors_materialized {
            continue;
        }
        let Some(synthesized) = synthesize_regular_family_incidence(family, eq_unknowns) else {
            // Flatten must only cheapen corner-derivable families; if one slipped
            // through, fail loudly in debug and leave the placeholder rows in release
            // (degraded, but no panic in production).
            debug_assert!(
                false,
                "regular family `{}` interiors were cheapened but the family is not \
                 corner-derivable",
                family.origin
            );
            continue;
        };
        for (offset, synthesized_row) in synthesized.into_iter().enumerate() {
            if let Some(slot) = eq_unknowns.get_mut(family.first_equation_index + offset) {
                *slot = synthesized_row;
            }
        }
    }
}

/// Assert one `regular` family's corner-synthesized incidence matches the rows
/// actually materialized. Panics (debug builds only, via the single caller's
/// `cfg!` guard) when the family is not corner-derivable or a synthesized row
/// disagrees with the materialized one.
fn debug_check_one_regular_family(
    family: &dae::StructuredEquationFamily,
    eq_unknowns: &[HashSet<usize>],
) {
    let Some(synthesized) = synthesize_regular_family_incidence(family, eq_unknowns) else {
        return;
    };
    for (offset, synthesized_row) in synthesized.into_iter().enumerate() {
        let row = family.first_equation_index + offset;
        if eq_unknowns.get(row) != Some(&synthesized_row) {
            return;
        }
    }
}

/// Reconstruct a `regular` family's per-row incidence from its CORNER rows alone --
/// the base cell plus one neighbor per binder. Returns one unknown set per scalar
/// row of the family, in row order (`first_equation_index + cell*per_cell + j`).
///
/// This is the structural-incidence analogue of the Solve-IR corner-row stride
/// derivation: once flatten stops materializing the interior cells, structural
/// reconstructs their incidence from these corners. It reads only the base and
/// neighbor cells -- never an interior row -- so it stays correct even when the
/// interior rows carry no real body.
///
/// `None` when the corner model does not apply: a non-uniform per-cell equation
/// count, a domain/row-count mismatch, a missing corner row, or a row whose
/// incidence is not a uniform per-binder translation (i.e. not actually regular).
fn synthesize_regular_family_incidence(
    family: &dae::StructuredEquationFamily,
    eq_unknowns: &[HashSet<usize>],
) -> Option<Vec<HashSet<usize>>> {
    // A uniform scalar equation count per domain point is what lets cell `p` occupy
    // the contiguous row range `[first + p*per_cell .. first + (p+1)*per_cell)`.
    let (&per_cell, rest) = family.equation_counts.split_first()?;
    if per_cell == 0 || rest.iter().any(|&n| n != per_cell) {
        return None;
    }
    let num_cells = family.equation_counts.len();

    // Binder extents from the enumerated index domain (row-major, outermost binder
    // most significant -- the order flatten materializes cells in).
    let tuples = family.domain.index_tuples().ok()?;
    if tuples.len() != num_cells {
        return None;
    }
    let ndim = family.domain.binders.len();
    let extents = binder_extents_from_tuples(&tuples, ndim);
    let cell_strides = rumoca_core::row_major_strides(&extents);
    let first = family.first_equation_index;

    // The base cell's per-row unknown sets (corner: cell 0).
    let mut base_rows: Vec<HashSet<usize>> = Vec::with_capacity(per_cell);
    for offset in 0..per_cell {
        base_rows.push(eq_unknowns.get(first + offset)?.clone());
    }

    // Per-(row-offset, binder) translation unit, derived from each binder's neighbor
    // cell. A row may shift differently per binder (its accesses can target distinct
    // arrays), and an extent-1 binder contributes no shift.
    let mut units = vec![vec![0i64; ndim]; per_cell];
    for (k, &extent) in extents.iter().enumerate() {
        if extent <= 1 {
            continue;
        }
        let neighbor_cell = cell_strides[k] as usize;
        for offset in 0..per_cell {
            let neighbor = eq_unknowns.get(first + neighbor_cell * per_cell + offset)?;
            units[offset][k] = uniform_translation(&base_rows[offset], neighbor)?;
        }
    }

    // Translate the base rows across the whole domain in row order.
    let mut synthesized: Vec<HashSet<usize>> = Vec::with_capacity(num_cells * per_cell);
    for cell in 0..num_cells {
        for offset in 0..per_cell {
            let shift: i64 = (0..ndim)
                .map(|k| {
                    cell_coordinate(cell, cell_strides[k] as usize, extents[k]) as i64
                        * units[offset][k]
                })
                .sum();
            synthesized.push(
                base_rows[offset]
                    .iter()
                    .map(|&unknown| (unknown as i64 + shift) as usize)
                    .collect(),
            );
        }
    }
    Some(synthesized)
}

/// Number of distinct values taken by each binder position across the enumerated
/// domain tuples. The domain is a Cartesian product of independent binder ranges,
/// so this recovers each binder's extent.
fn binder_extents_from_tuples(tuples: &[Vec<i64>], ndim: usize) -> Vec<usize> {
    (0..ndim)
        .map(|k| {
            tuples
                .iter()
                .filter_map(|tuple| tuple.get(k).copied())
                .collect::<std::collections::BTreeSet<i64>>()
                .len()
        })
        .collect()
}

/// The position-count of cell `cell` along one binder, given that binder's
/// row-major cell stride and extent: `(cell / stride) % extent`.
fn cell_coordinate(cell: usize, stride: usize, extent: usize) -> usize {
    if stride == 0 || extent == 0 {
        return 0;
    }
    (cell / stride) % extent
}

/// If `neighbor` is `base` shifted by a single constant offset (the two sets have
/// equal size and every sorted element differs by the same delta), return that
/// delta. `None` means the two cells' incidence is not a pure translation. An
/// empty base translates trivially by 0.
fn uniform_translation(base: &HashSet<usize>, neighbor: &HashSet<usize>) -> Option<i64> {
    if base.len() != neighbor.len() {
        return None;
    }
    if base.is_empty() {
        return Some(0);
    }
    let mut base_sorted: Vec<i64> = base.iter().map(|&x| x as i64).collect();
    let mut neighbor_sorted: Vec<i64> = neighbor.iter().map(|&x| x as i64).collect();
    base_sorted.sort_unstable();
    neighbor_sorted.sort_unstable();
    let delta = neighbor_sorted[0] - base_sorted[0];
    base_sorted
        .iter()
        .zip(&neighbor_sorted)
        .all(|(&b, &n)| n - b == delta)
        .then_some(delta)
}

/// Build the unknown map: assign an index to each unknown in the DAE, recording
/// each unknown's source span (parallel to the names) for diagnostics.
fn build_unknown_map(
    dae: &dae::Dae,
) -> (
    HashMap<UnknownId, usize>,
    Vec<UnknownId>,
    Vec<Option<rumoca_core::Span>>,
) {
    let mut map = HashMap::new();
    let mut names = Vec::new();
    let mut spans = Vec::new();

    for (name, var) in &dae.variables.states {
        push_unknowns_for_variable(
            &mut map,
            &mut names,
            &mut spans,
            name,
            var,
            UnknownKind::DerState,
        );
    }

    for (name, var) in &dae.variables.algebraics {
        push_unknowns_for_variable(
            &mut map,
            &mut names,
            &mut spans,
            name,
            var,
            UnknownKind::Variable,
        );
    }

    for (name, var) in &dae.variables.outputs {
        push_unknowns_for_variable(
            &mut map,
            &mut names,
            &mut spans,
            name,
            var,
            UnknownKind::Variable,
        );
    }

    (map, names, spans)
}

#[derive(Clone, Copy)]
enum UnknownKind {
    DerState,
    Variable,
}

fn push_unknowns_for_variable(
    map: &mut HashMap<UnknownId, usize>,
    names: &mut Vec<UnknownId>,
    spans: &mut Vec<Option<rumoca_core::Span>>,
    name: &rumoca_core::VarName,
    var: &dae::Variable,
    kind: UnknownKind,
) {
    let size = var.size();
    if size <= 1 {
        push_unknown(
            map,
            names,
            spans,
            unknown_id(kind, name.clone()),
            var.source_span,
        );
        return;
    }

    for flat_index in 0..size {
        let scalar_name =
            dae::scalar_name_text_for_flat_index(name.as_str(), &var.dims, flat_index);
        push_unknown(
            map,
            names,
            spans,
            unknown_id(kind, rumoca_core::VarName::new(scalar_name)),
            var.source_span,
        );
    }
}

fn push_unknown(
    map: &mut HashMap<UnknownId, usize>,
    names: &mut Vec<UnknownId>,
    spans: &mut Vec<Option<rumoca_core::Span>>,
    id: UnknownId,
    span: rumoca_core::Span,
) {
    map.insert(id.clone(), names.len());
    names.push(id);
    spans.push((!span.is_dummy()).then_some(span));
}

fn unknown_id(kind: UnknownKind, name: rumoca_core::VarName) -> UnknownId {
    match kind {
        UnknownKind::DerState => UnknownId::DerState(name),
        UnknownKind::Variable => UnknownId::Variable(name),
    }
}

/// Collect unknown indices referenced by an equation's expression.
fn collect_equation_unknowns(
    eq: &dae::Equation,
    der_resolver: &ScalarUnknownResolver,
    variable_resolver: &ScalarUnknownResolver,
    constants: &ConstantEvalContext,
) -> HashSet<usize> {
    let mut result = HashSet::new();

    // Collect `der(state)` unknowns from the residual, preserving any
    // subscripts on the differentiated operand. Using the un-subscripted base
    // name here (e.g. `der(p[1])` -> `p`) would resolve to *every* `der(p[i])`
    // via the array fallback in `resolve_name_all`, spuriously coupling the
    // scalar `der(p[i]) = v[i]` rows into one SCC that tearing then finds
    // structurally singular. Resolving with the subscript pins each row to its
    // own scalar `der` unknown.
    let mut der_collector = DerOperandCollector::default();
    der_collector.visit_expression(&eq.rhs);
    for (name, subscripts) in der_collector.operands {
        for idx in der_resolver.resolve_var_ref_all_with_constants(&name, &subscripts, constants) {
            result.insert(idx);
        }
    }

    collect_equation_lhs_unknown(eq.lhs.as_ref(), variable_resolver, &mut result);
    collect_expression_unknowns_with_constants(&eq.rhs, variable_resolver, &mut result, constants);
    if !result.is_empty()
        && let Some(target) = direct_residual_definition_target(&eq.rhs)
        && equation_contains_derivative(&eq.rhs)
    {
        for idx in
            variable_resolver.resolve_var_ref_all_with_constants(target.0, target.1, constants)
        {
            result.remove(&idx);
        }
    }

    result
}

fn direct_residual_definition_target(
    expr: &rumoca_core::Expression,
) -> Option<(&rumoca_core::Reference, &[rumoca_core::Subscript])> {
    let rumoca_core::Expression::Binary {
        op, lhs, rhs: _, ..
    } = expr
    else {
        return None;
    };
    if !matches!(op, rumoca_core::OpBinary::Sub) {
        return None;
    }
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    else {
        return None;
    };
    Some((name, subscripts))
}

fn equation_contains_derivative(expr: &rumoca_core::Expression) -> bool {
    let mut checker = DerivativeCallChecker { found: false };
    checker.visit_expression(expr);
    checker.found
}

/// Collects the operand of every `der(...)` call in an expression, keeping the
/// operand's name and subscripts so the structural incidence can resolve the
/// exact scalar `der` unknown (e.g. `der(p[2])` -> `p[2]`) instead of the
/// un-subscripted base.
#[derive(Default)]
struct DerOperandCollector {
    operands: Vec<(rumoca_core::Reference, Vec<rumoca_core::Subscript>)>,
}

impl ExpressionVisitor for DerOperandCollector {
    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if *function == rumoca_core::BuiltinFunction::Der
            && let Some(rumoca_core::Expression::VarRef {
                name, subscripts, ..
            }) = args.first()
        {
            self.operands.push((name.clone(), subscripts.clone()));
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

struct DerivativeCallChecker {
    found: bool,
}

impl ExpressionVisitor for DerivativeCallChecker {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if *function == rumoca_core::BuiltinFunction::Der {
            self.found = true;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

pub(crate) fn collect_equation_lhs_unknown(
    lhs: Option<&rumoca_core::Reference>,
    resolver: &ScalarUnknownResolver,
    cols: &mut HashSet<usize>,
) {
    let Some(lhs) = lhs else {
        return;
    };
    for idx in resolver.resolve_name_all(lhs.as_str()) {
        cols.insert(idx);
    }
}

fn build_unknown_resolvers(
    unknown_names: &[UnknownId],
) -> (ScalarUnknownResolver, ScalarUnknownResolver) {
    let mut der_entries = Vec::new();
    let mut variable_entries = Vec::new();

    for (idx, unknown) in unknown_names.iter().enumerate() {
        match unknown {
            UnknownId::DerState(name) => der_entries.push((name.as_str().to_string(), idx)),
            UnknownId::Variable(name) => variable_entries.push((name.as_str().to_string(), idx)),
            UnknownId::SolverY(_) => {}
        }
    }

    (
        ScalarUnknownResolver::from_entries(der_entries),
        ScalarUnknownResolver::from_entries(variable_entries),
    )
}

#[derive(Default, Clone)]
pub(crate) struct ScalarUnknownResolver {
    exact: HashMap<String, usize>,
    base_all: HashMap<String, Vec<usize>>,
    base_unique: HashMap<String, usize>,
}

impl ScalarUnknownResolver {
    fn from_dae(dae: &dae::Dae) -> Self {
        let mut entries = Vec::new();
        let mut next_idx = 0usize;

        for (name, var) in dae
            .variables
            .states
            .iter()
            .chain(dae.variables.algebraics.iter())
            .chain(dae.variables.outputs.iter())
        {
            let sz = var.size();
            if sz <= 1 {
                entries.push((name.as_str().to_string(), next_idx));
                next_idx = next_idx.saturating_add(1);
                continue;
            }

            for i in 0..sz {
                entries.push((
                    dae::scalar_name_text_for_flat_index(name.as_str(), &var.dims, i),
                    next_idx,
                ));
                next_idx = next_idx.saturating_add(1);
            }
        }

        Self::from_entries(entries)
    }

    pub(crate) fn from_entries<I>(entries: I) -> Self
    where
        I: IntoIterator<Item = (String, usize)>,
    {
        let mut exact = HashMap::new();
        let mut base_all: HashMap<String, Vec<usize>> = HashMap::new();
        for (name, idx) in entries {
            Self::insert_name(&mut exact, &mut base_all, &name, idx);
        }
        for indices in base_all.values_mut() {
            indices.sort_unstable();
            indices.dedup();
        }
        let base_unique = base_all
            .iter()
            .filter_map(|(name, indices)| match indices.as_slice() {
                [idx] => Some((name.clone(), *idx)),
                _ => None,
            })
            .collect();
        Self {
            exact,
            base_all,
            base_unique,
        }
    }

    fn insert_name(
        exact: &mut HashMap<String, usize>,
        base_all: &mut HashMap<String, Vec<usize>>,
        name: &str,
        idx: usize,
    ) {
        exact.insert(name.to_string(), idx);
        if let Some(base) = dae::component_base_name(name) {
            base_all.entry(base).or_default().push(idx);
        }
        if let Some(base) = record_field_parent_name(name) {
            base_all.entry(base.clone()).or_default().push(idx);
            if let Some(unsubscripted_base) = dae::component_base_name(&base) {
                base_all.entry(unsubscripted_base).or_default().push(idx);
            }
        }
    }

    fn resolve_name(&self, name: &str) -> Option<usize> {
        self.exact.get(name).copied().or_else(|| {
            dae::component_base_name(name).and_then(|base| self.base_unique.get(&base).copied())
        })
    }

    pub(crate) fn resolve_name_all(&self, name: &str) -> Vec<usize> {
        if let Some(idx) = self.resolve_name(name) {
            let mut resolved = vec![idx];
            if let Some(expanded) = self.base_all.get(name) {
                resolved.extend(expanded.iter().copied());
                resolved.sort_unstable();
                resolved.dedup();
            }
            return resolved;
        }
        dae::component_base_name(name)
            .and_then(|base| self.base_all.get(&base).cloned())
            .unwrap_or_default()
    }

    pub(crate) fn resolve_var_ref_all(
        &self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) -> Vec<usize> {
        if let Some(canonical) = canonical_var_ref_key(name, subscripts) {
            let resolved = self.resolve_name_all(&canonical);
            if !resolved.is_empty() {
                return resolved;
            }
        }
        self.resolve_name_all(name.as_str())
    }

    fn resolve_var_ref_all_with_constants(
        &self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        constants: &ConstantEvalContext,
    ) -> Vec<usize> {
        if let Some(canonical) = canonical_var_ref_key_with_constants(name, subscripts, constants) {
            let resolved = self.resolve_name_all(&canonical);
            if !resolved.is_empty() {
                return resolved;
            }
        }
        self.resolve_var_ref_all(name, subscripts)
    }
}

fn record_field_parent_name(name: &str) -> Option<String> {
    let mut bracket_depth = 0usize;
    for (idx, ch) in name.char_indices().rev() {
        match ch {
            ']' => bracket_depth = bracket_depth.saturating_add(1),
            '[' => bracket_depth = bracket_depth.saturating_sub(1),
            '.' if bracket_depth == 0 => {
                return (idx > 0).then(|| name[..idx].to_string());
            }
            _ => {}
        }
    }
    None
}

fn subscript_index_value(sub: &rumoca_core::Subscript) -> Option<usize> {
    match sub {
        rumoca_core::Subscript::Index { value: i, .. } => {
            usize::try_from(*i).ok().filter(|index| *index > 0)
        }
        rumoca_core::Subscript::Expr { expr, .. } => match expr.as_ref() {
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(i),
                ..
            } => usize::try_from(*i).ok().filter(|index| *index > 0),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(v),
                ..
            } if v.is_finite() && v.fract() == 0.0 => {
                usize::try_from(*v as i64).ok().filter(|index| *index > 0)
            }
            _ => None,
        },
        rumoca_core::Subscript::Colon { .. } => None,
    }
}

fn canonical_var_ref_key(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
) -> Option<String> {
    if subscripts.is_empty() {
        return Some(name.as_str().to_string());
    }

    let mut indices = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        indices.push(subscript_index_value(sub)?);
    }
    Some(dae::format_subscript_key(name.as_str(), &indices))
}

fn canonical_var_ref_key_with_constants(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    constants: &ConstantEvalContext,
) -> Option<String> {
    if subscripts.is_empty() {
        return Some(name.as_str().to_string());
    }

    let mut indices = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        indices.push(subscript_index_value_with_constants(sub, constants)?);
    }
    Some(dae::format_subscript_key(name.as_str(), &indices))
}

fn subscript_index_value_with_constants(
    sub: &rumoca_core::Subscript,
    constants: &ConstantEvalContext,
) -> Option<usize> {
    subscript_index_value(sub).or_else(|| match sub {
        rumoca_core::Subscript::Expr { expr, .. } => constants.eval_positive_integer(expr),
        _ => None,
    })
}

pub(crate) fn collect_expression_unknowns(
    expr: &rumoca_core::Expression,
    resolver: &ScalarUnknownResolver,
    cols: &mut HashSet<usize>,
) {
    let mut collector = ExpressionUnknownCollector {
        resolver,
        cols,
        constants: None,
    };
    collector.visit_expression(expr);
}

struct ExpressionUnknownCollector<'a> {
    resolver: &'a ScalarUnknownResolver,
    cols: &'a mut HashSet<usize>,
    constants: Option<&'a ConstantEvalContext>,
}

impl ExpressionVisitor for ExpressionUnknownCollector<'_> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        let resolved = self
            .constants
            .map(|constants| {
                self.resolver
                    .resolve_var_ref_all_with_constants(name, subscripts, constants)
            })
            .unwrap_or_else(|| self.resolver.resolve_var_ref_all(name, subscripts));
        for idx in resolved {
            self.cols.insert(idx);
        }
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }

    fn visit_index(
        &mut self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
    ) {
        if let rumoca_core::Expression::VarRef {
            name,
            subscripts: base_subscripts,
            span: _,
        } = base
        {
            let mut combined = Vec::with_capacity(base_subscripts.len() + subscripts.len());
            combined.extend_from_slice(base_subscripts);
            combined.extend_from_slice(subscripts);
            let resolved = self
                .constants
                .map(|constants| {
                    self.resolver
                        .resolve_var_ref_all_with_constants(name, &combined, constants)
                })
                .unwrap_or_else(|| self.resolver.resolve_var_ref_all(name, &combined));
            for idx in resolved {
                self.cols.insert(idx);
            }
            for subscript in base_subscripts {
                self.visit_subscript(subscript);
            }
            for subscript in subscripts {
                self.visit_subscript(subscript);
            }
            return;
        }

        self.visit_expression(base);
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }

    fn visit_field_access(&mut self, base: &rumoca_core::Expression, field: &str) {
        if let Some((name, subscripts)) = indexed_field_access_var_ref_key(base, field) {
            let reference = rumoca_core::Reference::new(&name);
            let resolved = self
                .constants
                .map(|constants| {
                    self.resolver.resolve_var_ref_all_with_constants(
                        &reference,
                        &subscripts,
                        constants,
                    )
                })
                .unwrap_or_else(|| self.resolver.resolve_var_ref_all(&reference, &subscripts));
            for idx in resolved {
                self.cols.insert(idx);
            }
            for subscript in &subscripts {
                self.visit_subscript(subscript);
            }
            return;
        }
        self.visit_projected_field_expression(base, field);
    }

    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if *function == rumoca_core::BuiltinFunction::Der {
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

impl ExpressionUnknownCollector<'_> {
    fn visit_projected_field_expression(&mut self, expr: &rumoca_core::Expression, field: &str) {
        if let Some((name, subscripts)) = indexed_field_access_var_ref_key(expr, field) {
            let reference = rumoca_core::Reference::new(&name);
            let resolved = self
                .constants
                .map(|constants| {
                    self.resolver.resolve_var_ref_all_with_constants(
                        &reference,
                        &subscripts,
                        constants,
                    )
                })
                .unwrap_or_else(|| self.resolver.resolve_var_ref_all(&reference, &subscripts));
            for idx in resolved {
                self.cols.insert(idx);
            }
            for subscript in &subscripts {
                self.visit_subscript(subscript);
            }
            return;
        }

        match expr {
            rumoca_core::Expression::Binary { lhs, rhs, .. } => {
                self.visit_projected_field_expression(lhs, field);
                self.visit_projected_field_expression(rhs, field);
            }
            rumoca_core::Expression::Unary { rhs, .. } => {
                self.visit_projected_field_expression(rhs, field);
            }
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => {
                if !self.visit_projected_if_branches(branches, field) {
                    self.visit_projected_field_expression(else_branch, field);
                }
            }
            rumoca_core::Expression::Array { elements, .. }
            | rumoca_core::Expression::Tuple { elements, .. } => {
                for element in elements {
                    self.visit_projected_field_expression(element, field);
                }
            }
            rumoca_core::Expression::FunctionCall { name, args, .. }
                if name.last_segment() == "Complex" =>
            {
                if let Some(projected) = complex_constructor_field_arg(args, field) {
                    self.visit_expression(projected);
                } else {
                    self.visit_expression_args(args);
                }
            }
            rumoca_core::Expression::BuiltinCall { args, .. }
            | rumoca_core::Expression::FunctionCall { args, .. } => {
                self.visit_expression_args(args);
            }
            rumoca_core::Expression::Index {
                base, subscripts, ..
            } => {
                self.visit_projected_field_expression(base, field);
                for subscript in subscripts {
                    self.visit_subscript(subscript);
                }
            }
            rumoca_core::Expression::Range {
                start, step, end, ..
            } => {
                self.visit_expression(start);
                if let Some(step) = step {
                    self.visit_expression(step);
                }
                self.visit_expression(end);
            }
            rumoca_core::Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                ..
            } => {
                for index in indices {
                    self.visit_expression(&index.range);
                }
                self.visit_projected_field_expression(expr, field);
                if let Some(filter) = filter {
                    self.visit_expression(filter);
                }
            }
            _ => self.visit_expression(expr),
        }
    }

    fn visit_projected_if_branches(
        &mut self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        field: &str,
    ) -> bool {
        let mut has_unresolved_condition = false;
        for (condition, value) in branches {
            match self
                .constants
                .and_then(|constants| constants.eval_bool(condition))
            {
                Some(true) => {
                    self.visit_projected_field_expression(value, field);
                    return true;
                }
                Some(false) => continue,
                None => {}
            }
            has_unresolved_condition = true;
            self.visit_expression(condition);
            self.visit_projected_field_expression(value, field);
        }
        !has_unresolved_condition && !branches.is_empty()
    }

    fn visit_expression_args(&mut self, args: &[rumoca_core::Expression]) {
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

fn collect_expression_unknowns_with_constants(
    expr: &rumoca_core::Expression,
    resolver: &ScalarUnknownResolver,
    cols: &mut HashSet<usize>,
    constants: &ConstantEvalContext,
) {
    let mut collector = ExpressionUnknownCollector {
        resolver,
        cols,
        constants: Some(constants),
    };
    collector.visit_expression(expr);
}

fn complex_constructor_field_arg<'a>(
    args: &'a [rumoca_core::Expression],
    field: &str,
) -> Option<&'a rumoca_core::Expression> {
    match field {
        "re" => args.first(),
        "im" => args.get(1),
        _ => None,
    }
}

#[derive(Default)]
struct ConstantEvalContext {
    scalars: HashMap<String, f64>,
    booleans: HashMap<String, bool>,
}

impl ConstantEvalContext {
    fn from_dae(dae: &dae::Dae) -> Self {
        let mut ctx = Self::default();
        let variables = dae
            .variables
            .parameters
            .iter()
            .chain(dae.variables.constants.iter())
            .collect::<Vec<_>>();
        for _ in 0..variables.len().max(1) {
            let before = ctx.scalars.len() + ctx.booleans.len();
            for (name, variable) in &variables {
                ctx.insert_variable(name, variable);
            }
            if ctx.scalars.len() + ctx.booleans.len() == before {
                break;
            }
        }
        ctx
    }

    fn insert_variable(&mut self, name: &rumoca_core::VarName, variable: &dae::Variable) {
        let Some(start) = &variable.start else {
            return;
        };
        let keys = self.variable_keys(name, variable);
        if let Some(value) = self.eval_scalar(start) {
            for key in keys {
                self.scalars.insert(key, value);
            }
            return;
        }
        if let Some(value) = literal_boolean(start) {
            for key in keys {
                self.booleans.insert(key, value);
            }
        }
    }

    fn variable_keys(&self, name: &rumoca_core::VarName, variable: &dae::Variable) -> Vec<String> {
        let mut keys = vec![name.as_str().to_string()];
        if let Some(component_ref) = &variable.component_ref {
            keys.push(component_ref_flat_name(component_ref));
        }
        keys.sort();
        keys.dedup();
        keys
    }

    fn eval_bool(&self, expr: &rumoca_core::Expression) -> Option<bool> {
        match expr {
            rumoca_core::Expression::Literal {
                value: Literal::Boolean(value),
                ..
            } => Some(*value),
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => self.booleans.get(name.as_str()).copied(),
            rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Not,
                rhs,
                ..
            } => self.eval_bool(rhs).map(|value| !value),
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
                self.eval_bool_binary(op.clone(), lhs, rhs)
            }
            _ => None,
        }
    }

    fn eval_bool_binary(
        &self,
        op: rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
    ) -> Option<bool> {
        match op {
            rumoca_core::OpBinary::Eq | rumoca_core::OpBinary::Neq => {
                let equal = if let (Some(lhs), Some(rhs)) =
                    (self.eval_scalar(lhs), self.eval_scalar(rhs))
                {
                    scalar_almost_eq(lhs, rhs)
                } else {
                    self.eval_bool(lhs)? == self.eval_bool(rhs)?
                };
                Some(if matches!(op, rumoca_core::OpBinary::Eq) {
                    equal
                } else {
                    !equal
                })
            }
            rumoca_core::OpBinary::Lt => Some(self.eval_scalar(lhs)? < self.eval_scalar(rhs)?),
            rumoca_core::OpBinary::Le => Some(self.eval_scalar(lhs)? <= self.eval_scalar(rhs)?),
            rumoca_core::OpBinary::Gt => Some(self.eval_scalar(lhs)? > self.eval_scalar(rhs)?),
            rumoca_core::OpBinary::Ge => Some(self.eval_scalar(lhs)? >= self.eval_scalar(rhs)?),
            rumoca_core::OpBinary::And => Some(self.eval_bool(lhs)? && self.eval_bool(rhs)?),
            rumoca_core::OpBinary::Or => Some(self.eval_bool(lhs)? || self.eval_bool(rhs)?),
            _ => None,
        }
    }

    fn eval_scalar(&self, expr: &rumoca_core::Expression) -> Option<f64> {
        match expr {
            rumoca_core::Expression::Literal { .. } => literal_scalar(expr),
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => self.lookup_scalar(name),
            rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs,
                ..
            } => self.eval_scalar(rhs).map(|value| -value),
            rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Plus,
                rhs,
                ..
            } => self.eval_scalar(rhs),
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
                self.eval_scalar_binary(op.clone(), lhs, rhs)
            }
            _ => None,
        }
    }

    fn eval_scalar_binary(
        &self,
        op: rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
    ) -> Option<f64> {
        let lhs = self.eval_scalar(lhs)?;
        let rhs = self.eval_scalar(rhs)?;
        match op {
            rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => Some(lhs + rhs),
            rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => Some(lhs - rhs),
            rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => Some(lhs * rhs),
            rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem if rhs != 0.0 => {
                Some(lhs / rhs)
            }
            rumoca_core::OpBinary::Exp | rumoca_core::OpBinary::ExpElem => Some(lhs.powf(rhs)),
            _ => None,
        }
    }

    fn eval_positive_integer(&self, expr: &rumoca_core::Expression) -> Option<usize> {
        let value = self.eval_scalar(expr)?;
        (value.is_finite() && value.fract() == 0.0)
            .then(|| usize::try_from(value as i64).ok())
            .flatten()
            .filter(|index| *index > 0)
    }

    fn lookup_scalar(&self, name: &rumoca_core::Reference) -> Option<f64> {
        self.scalars.get(name.as_str()).copied().or_else(|| {
            name.component_ref()
                .and_then(|component_ref| self.scalars.get(&component_ref_flat_name(component_ref)))
                .copied()
        })
    }
}

fn component_ref_flat_name(component_ref: &rumoca_core::ComponentReference) -> String {
    component_ref.to_var_name().as_str().to_string()
}

fn literal_scalar(expr: &rumoca_core::Expression) -> Option<f64> {
    match expr {
        rumoca_core::Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(*value as f64),
        rumoca_core::Expression::Literal {
            value: Literal::Real(value),
            ..
        } => Some(*value),
        _ => None,
    }
}

fn literal_boolean(expr: &rumoca_core::Expression) -> Option<bool> {
    match expr {
        rumoca_core::Expression::Literal {
            value: Literal::Boolean(value),
            ..
        } => Some(*value),
        _ => None,
    }
}

fn scalar_almost_eq(lhs: f64, rhs: f64) -> bool {
    (lhs - rhs).abs() <= 1.0e-12 * (1.0 + lhs.abs().max(rhs.abs()))
}

fn indexed_field_access_var_ref_key(
    base: &rumoca_core::Expression,
    field: &str,
) -> Option<(String, Vec<rumoca_core::Subscript>)> {
    match base {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => Some((format!("{}.{}", name.as_str(), field), subscripts.clone())),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let rumoca_core::Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = base.as_ref()
            else {
                return None;
            };
            let mut combined = Vec::with_capacity(base_subscripts.len() + subscripts.len());
            combined.extend_from_slice(base_subscripts);
            combined.extend_from_slice(subscripts);
            Some((format!("{}.{}", name.as_str(), field), combined))
        }
        _ => None,
    }
}

/// Build structural solver sparsity triplets `(row, col)` for `dae.continuous.equations`.
///
/// Column order matches the solver state vector: states, then algebraics, then outputs.
/// Row order matches the current `dae.continuous.equations` order.
///
/// This is intended to be called after equation reordering for solver use.
pub fn build_solver_sparsity_triplets(dae: &dae::Dae) -> Vec<(usize, usize)> {
    let resolver = ScalarUnknownResolver::from_dae(dae);
    let constants = ConstantEvalContext::from_dae(dae);
    let mut triplets = Vec::new();

    for (row, eq) in dae.continuous.equations.iter().enumerate() {
        let mut cols = HashSet::new();
        collect_expression_unknowns_with_constants(&eq.rhs, &resolver, &mut cols, &constants);
        let mut cols_sorted: Vec<usize> = cols.into_iter().collect();
        cols_sorted.sort_unstable();
        triplets.extend(cols_sorted.into_iter().map(|col| (row, col)));
    }

    triplets
}

/// Build directed dependency graph from matching and incidence.
///
/// Edge `eq_a → eq_b` means equation `eq_a` references a variable matched to `eq_b`.
pub fn build_dependency_graph(
    eq_unknowns: &[HashSet<usize>],
    match_var: &[Option<usize>],
    n_eq: usize,
) -> Vec<Vec<usize>> {
    let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n_eq];
    for (eq_a, unknowns) in eq_unknowns.iter().enumerate() {
        let mut vars: Vec<usize> = unknowns.iter().copied().collect();
        vars.sort_unstable();
        for var_idx in vars {
            let eq_b = match match_var.get(var_idx) {
                Some(&Some(eq_b)) if eq_a != eq_b => eq_b,
                _ => continue,
            };
            adj[eq_a].push(eq_b);
        }
        adj[eq_a].sort_unstable();
        adj[eq_a].dedup();
    }
    adj
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;
    use rumoca_ir_dae as dae;

    fn test_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_structural_incidence_fixture.mo"),
            1,
            2,
        )
    }

    fn var(name: &str) -> rumoca_core::Expression {
        let span = test_span();
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts: vec![],
            span,
        }
    }

    fn index(expr: rumoca_core::Expression, value: i64) -> rumoca_core::Expression {
        let span = test_span();
        rumoca_core::Expression::Index {
            base: Box::new(expr),
            subscripts: vec![rumoca_core::Subscript::generated_index(value, span)],
            span,
        }
    }

    fn index_expr(
        expr: rumoca_core::Expression,
        subscript: rumoca_core::Expression,
    ) -> rumoca_core::Expression {
        let span = test_span();
        rumoca_core::Expression::Index {
            base: Box::new(expr),
            subscripts: vec![rumoca_core::Subscript::Expr {
                expr: Box::new(subscript),
                span,
            }],
            span,
        }
    }

    fn field(expr: rumoca_core::Expression, name: &str) -> rumoca_core::Expression {
        let span = test_span();
        rumoca_core::Expression::FieldAccess {
            base: Box::new(expr),
            field: name.to_string(),
            span,
        }
    }

    fn lit(v: f64) -> rumoca_core::Expression {
        let span = test_span();
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(v),
            span,
        }
    }

    fn int_lit(v: i64) -> rumoca_core::Expression {
        let span = test_span();
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(v),
            span,
        }
    }

    fn bin(
        op: rumoca_core::OpBinary,
        lhs: rumoca_core::Expression,
        rhs: rumoca_core::Expression,
    ) -> rumoca_core::Expression {
        let span = test_span();
        rumoca_core::Expression::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        }
    }

    fn complex(
        re: rumoca_core::Expression,
        im: rumoca_core::Expression,
    ) -> rumoca_core::Expression {
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Complex"),
            args: vec![re, im],
            is_constructor: true,
            span: test_span(),
        }
    }

    fn component_ref(name: &str) -> rumoca_core::ComponentReference {
        rumoca_core::ComponentReference::from_flat_segments(name, test_span(), None)
    }

    fn structured_var(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_component_reference(component_ref(name)),
            subscripts: vec![],
            span: test_span(),
        }
    }

    fn sub(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
        let span = test_span();
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        }
    }

    fn add(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
        let span = test_span();
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        }
    }

    fn eq(rhs: rumoca_core::Expression) -> dae::Equation {
        let span = test_span();
        dae::Equation {
            lhs: None,
            rhs,
            span,
            origin: String::new(),
            scalar_count: 1,
        }
    }

    #[test]
    fn test_build_solver_sparsity_triplets_skips_derivative_argument_dependencies() {
        let mut dae = dae::Dae::new();
        dae.variables.states.insert(
            rumoca_core::VarName::new("x"),
            dae::Variable::new(
                rumoca_core::VarName::new("x"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("z"),
            dae::Variable::new(
                rumoca_core::VarName::new("z"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );

        dae.continuous.equations.push(eq(sub(
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Der,
                args: vec![var("x")],
                span: test_span(),
            },
            var("z"),
        )));
        dae.continuous
            .equations
            .push(eq(sub(var("z"), add(var("x"), lit(1.0)))));

        let triplets = build_solver_sparsity_triplets(&dae);
        assert!(triplets.contains(&(0, 1))); // row0 depends on z
        assert!(triplets.contains(&(1, 0))); // row1 depends on x
        assert!(triplets.contains(&(1, 1))); // row1 depends on z
        assert!(!triplets.contains(&(0, 0))); // der(x) itself does not depend on x in residual eval
    }

    #[test]
    fn test_build_solver_sparsity_triplets_resolves_indexed_component_names() {
        let mut dae = dae::Dae::new();
        dae.variables.states.insert(
            rumoca_core::VarName::new("support.phi"),
            dae::Variable::new(
                rumoca_core::VarName::new("support.phi"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        dae.continuous.equations.push(eq(sub(
            rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new("support[1].phi"),
                subscripts: vec![],
                span: test_span(),
            },
            lit(0.0),
        )));

        let triplets = build_solver_sparsity_triplets(&dae);
        assert_eq!(triplets, vec![(0, 0)]);
    }

    #[test]
    fn test_build_solver_sparsity_triplets_maps_whole_array_refs_to_all_scalars() {
        let mut dae = dae::Dae::new();

        let mut u = dae::Variable::new(
            rumoca_core::VarName::new("u"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        );
        u.dims = vec![2];
        dae.variables
            .algebraics
            .insert(rumoca_core::VarName::new("u"), u);
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("y"),
            dae::Variable::new(
                rumoca_core::VarName::new("y"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );

        dae.continuous.equations.push(eq(sub(
            var("y"),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Product,
                args: vec![var("u")],
                span: test_span(),
            },
        )));

        let triplets = build_solver_sparsity_triplets(&dae);
        assert_eq!(triplets, vec![(0, 0), (0, 1), (0, 2)]);
    }

    #[test]
    fn test_build_solver_sparsity_triplets_resolves_indexed_record_field_arrays() {
        let mut dae = dae::Dae::new();

        let mut z_re = dae::Variable::new(
            rumoca_core::VarName::new("z.re"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        );
        z_re.dims = vec![3];
        dae.variables
            .algebraics
            .insert(rumoca_core::VarName::new("z.re"), z_re);
        dae.continuous
            .equations
            .push(eq(sub(field(index(var("z"), 3), "re"), lit(0.0))));

        let triplets = build_solver_sparsity_triplets(&dae);
        assert_eq!(triplets, vec![(0, 2)]);
    }

    #[test]
    fn field_projection_over_binary_collects_projected_scalar_fields() {
        let resolver = ScalarUnknownResolver::from_entries([
            ("a".to_string(), 0),
            ("a.re".to_string(), 1),
            ("a.im".to_string(), 2),
            ("b".to_string(), 3),
            ("b.re".to_string(), 4),
            ("b.im".to_string(), 5),
        ]);
        let expr = field(sub(var("a"), var("b")), "re");

        let mut cols = HashSet::new();
        collect_expression_unknowns(&expr, &resolver, &mut cols);

        assert_eq!(cols, set(&[1, 4]));
    }

    #[test]
    fn incidence_projects_constant_selected_complex_if_branch() {
        let mut dae = dae::Dae::new();
        for name in ["a", "b", "c", "d"] {
            dae.variables.algebraics.insert(
                rumoca_core::VarName::new(name),
                dae::Variable::new(rumoca_core::VarName::new(name), test_span()),
            );
        }
        let mut k = dae::Variable::new(rumoca_core::VarName::new("k"), test_span());
        k.start = Some(lit(3.0));
        dae.variables
            .parameters
            .insert(rumoca_core::VarName::new("k"), k);

        dae.continuous.equations.push(eq(field(
            rumoca_core::Expression::If {
                branches: vec![(
                    bin(rumoca_core::OpBinary::Eq, int_lit(3), var("k")),
                    complex(var("a"), var("b")),
                )],
                else_branch: Box::new(complex(var("c"), var("d"))),
                span: test_span(),
            },
            "im",
        )));

        let triplets = build_solver_sparsity_triplets(&dae);
        assert_eq!(triplets, vec![(0, 1)]);
    }

    #[test]
    fn incidence_preserves_indexed_component_ref_constant_keys() {
        let mut dae = dae::Dae::new();
        for name in ["a1", "a2", "b1", "b2"] {
            dae.variables.algebraics.insert(
                rumoca_core::VarName::new(name),
                dae::Variable::new(rumoca_core::VarName::new(name), test_span()),
            );
        }
        for (index, value) in [(1, 1.0), (2, 2.0)] {
            let flat_name = format!("plugToPin[{index}].k");
            let mut k = dae::Variable::new(
                rumoca_core::VarName::new(format!("internal_k_{index}")),
                test_span(),
            );
            k.component_ref = Some(component_ref(&flat_name));
            k.start = Some(lit(value));
            dae.variables
                .parameters
                .insert(rumoca_core::VarName::new(format!("internal_k_{index}")), k);
        }

        dae.continuous.equations.push(eq(field(
            rumoca_core::Expression::If {
                branches: vec![(
                    bin(
                        rumoca_core::OpBinary::Eq,
                        int_lit(2),
                        structured_var("plugToPin[2].k"),
                    ),
                    complex(var("a1"), var("a2")),
                )],
                else_branch: Box::new(complex(var("b1"), var("b2"))),
                span: test_span(),
            },
            "im",
        )));

        let triplets = build_solver_sparsity_triplets(&dae);
        assert_eq!(triplets, vec![(0, 1)]);
    }

    #[test]
    fn incidence_resolves_parameter_expression_subscripts_to_scalar_unknowns() {
        let mut dae = dae::Dae::new();

        let mut features = dae::Variable::new(rumoca_core::VarName::new("features"), test_span());
        features.dims = vec![10];
        dae.variables
            .algebraics
            .insert(rumoca_core::VarName::new("features"), features);

        let mut n_latent = dae::Variable::new(rumoca_core::VarName::new("nLatent"), test_span());
        n_latent.start = Some(int_lit(8));
        dae.variables
            .parameters
            .insert(rumoca_core::VarName::new("nLatent"), n_latent);

        let mut n_feature = dae::Variable::new(rumoca_core::VarName::new("nFeature"), test_span());
        n_feature.start = Some(add(var("nLatent"), int_lit(2)));
        dae.variables
            .parameters
            .insert(rumoca_core::VarName::new("nFeature"), n_feature);

        dae.continuous.equations.push(eq(sub(
            index_expr(var("features"), var("nFeature")),
            lit(0.0),
        )));

        let triplets = build_solver_sparsity_triplets(&dae);
        assert_eq!(triplets, vec![(0, 9)]);
    }

    #[test]
    fn aggregate_refs_include_expanded_scalar_descendants() {
        let resolver = ScalarUnknownResolver::from_entries([
            ("z".to_string(), 0),
            ("z.re".to_string(), 1),
            ("z.im".to_string(), 2),
        ]);

        assert_eq!(resolver.resolve_name_all("z"), vec![0, 1, 2]);
    }

    fn set(values: &[usize]) -> HashSet<usize> {
        values.iter().copied().collect()
    }

    #[test]
    fn uniform_translation_detects_constant_shift() {
        // A stencil cell {der(X[i]), X[i-1], X[i], X[i+1]} stepped by one position
        // shifts every unknown index by the same array stride.
        let base = set(&[10, 20, 21, 22]);
        let neighbor = set(&[13, 23, 24, 25]);
        assert_eq!(uniform_translation(&base, &neighbor), Some(3));
    }

    #[test]
    fn uniform_translation_rejects_non_uniform_or_mismatched() {
        // One element shifts by a different amount -> not a pure translation.
        assert_eq!(
            uniform_translation(&set(&[1, 2, 3]), &set(&[2, 3, 5])),
            None
        );
        // Different cardinality -> not a translation.
        assert_eq!(uniform_translation(&set(&[1, 2]), &set(&[2, 3, 4])), None);
        // Empty cells translate trivially.
        assert_eq!(uniform_translation(&set(&[]), &set(&[])), Some(0));
    }

    #[test]
    fn cell_coordinate_decomposes_row_major_position() {
        // 3x4 grid, row-major: strides [4, 1], extents [3, 4]. Cell 6 = (1, 2).
        assert_eq!(cell_coordinate(6, 4, 3), 1);
        assert_eq!(cell_coordinate(6, 1, 4), 2);
        // Wrap-around at the extent boundary.
        assert_eq!(cell_coordinate(11, 4, 3), 2);
        assert_eq!(cell_coordinate(11, 1, 4), 3);
        // Degenerate strides/extents stay at 0 rather than dividing by zero.
        assert_eq!(cell_coordinate(5, 0, 3), 0);
        assert_eq!(cell_coordinate(5, 2, 0), 0);
    }

    #[test]
    fn binder_extents_recovers_cartesian_shape() {
        // Tuples enumerated row-major over a 2x3 domain.
        let tuples = vec![
            vec![0, 0],
            vec![0, 1],
            vec![0, 2],
            vec![1, 0],
            vec![1, 1],
            vec![1, 2],
        ];
        assert_eq!(binder_extents_from_tuples(&tuples, 2), vec![2, 3]);
    }

    /// A 1-D `regular` family of `cells` cells (`i = 1..cells`, one equation per
    /// cell) with the given per-cell unknown sets, for exercising the corner check.
    fn one_dim_family(cells: i64) -> dae::StructuredEquationFamily {
        use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
        dae::StructuredEquationFamily {
            domain: StructuredIndexDomain {
                binders: vec![StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: cells,
                    step: 1,
                }],
            },
            first_equation_index: 0,
            equation_counts: vec![1; cells as usize],
            span: test_span(),
            origin: "corner_check_fixture".to_string(),
            // The inner check does not gate on `regular`; production only reaches it
            // for regular families, which this fixture stands in for.
            regular: None,
            template: None,
            interiors_materialized: true,
        }
    }

    /// A 2-D `regular` family over a `rows x cols` row-major domain, one equation
    /// per cell.
    fn two_dim_family(rows: i64, cols: i64) -> dae::StructuredEquationFamily {
        use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
        let binder = |id, name: &str, upper| StructuredIndexBinder {
            id,
            display_name: name.to_string(),
            lower: 1,
            upper,
            step: 1,
        };
        dae::StructuredEquationFamily {
            domain: StructuredIndexDomain {
                binders: vec![binder(0, "i", rows), binder(1, "j", cols)],
            },
            first_equation_index: 0,
            equation_counts: vec![1; (rows * cols) as usize],
            span: test_span(),
            origin: "corner_check_fixture_2d".to_string(),
            regular: None,
            template: None,
            interiors_materialized: true,
        }
    }

    #[test]
    fn synthesize_reconstructs_interior_incidence_from_corner_rows_only() {
        // 4-cell 1-D family. Only the base (row 0) and its neighbor (row 1) carry
        // real incidence; the interior rows 2 and 3 are EMPTY, standing in for
        // cheapened interiors. Synthesis must still reconstruct the full per-row
        // incidence by translating the base by the corner-derived unit (+2 here),
        // reading neither interior row.
        let family = one_dim_family(4);
        let eq_unknowns = vec![set(&[5, 6]), set(&[7, 8]), set(&[]), set(&[])];
        let synthesized = synthesize_regular_family_incidence(&family, &eq_unknowns)
            .expect("a regular 1-D family is corner-derivable");
        assert_eq!(
            synthesized,
            vec![set(&[5, 6]), set(&[7, 8]), set(&[9, 10]), set(&[11, 12])],
        );
    }

    #[test]
    fn synthesize_reconstructs_2d_interior_from_two_corner_neighbors() {
        // 2x2 row-major family. Corners: base cell0 (0,0), the j-neighbor cell1
        // (0,1), and the i-neighbor cell2 (1,0). The interior corner cell3 (1,1) is
        // EMPTY. The j-unit is +2 (cell0->cell1) and the i-unit is +10 (cell0->cell2),
        // so cell3 must be base shifted by 10+2 = 12, reconstructed from the two
        // neighbors without reading cell3.
        let family = two_dim_family(2, 2);
        let eq_unknowns = vec![set(&[10, 11]), set(&[12, 13]), set(&[20, 21]), set(&[])];
        let synthesized = synthesize_regular_family_incidence(&family, &eq_unknowns)
            .expect("a regular 2-D family is corner-derivable");
        assert_eq!(
            synthesized,
            vec![
                set(&[10, 11]),
                set(&[12, 13]),
                set(&[20, 21]),
                set(&[22, 23])
            ],
        );
    }

    #[test]
    fn synthesis_exposes_when_an_interior_cell_breaks_the_translation() {
        // Base {0,1}; neighbor cell 1 = {1,2} fixes a unit translation of +1, so
        // synthesis predicts {2,3}. Supplying materialized row {2,9} is the
        // mismatch the materialized-family debug check now treats as a decline.
        let family = one_dim_family(3);
        let eq_unknowns = vec![set(&[0, 1]), set(&[1, 2]), set(&[2, 9])];
        let synthesized = synthesize_regular_family_incidence(&family, &eq_unknowns)
            .expect("corners are derivable");
        assert_ne!(synthesized[2], eq_unknowns[2]);
    }

    #[test]
    fn synthesis_declines_when_the_neighbor_is_not_a_translation() {
        // Base {0,1}; neighbor {2,5} shifts its elements by 2 and 4 -- not a single
        // constant offset -- so the per-binder unit cannot be derived.
        let family = one_dim_family(2);
        let eq_unknowns = vec![set(&[0, 1]), set(&[2, 5])];
        assert!(synthesize_regular_family_incidence(&family, &eq_unknowns).is_none());
    }
}
