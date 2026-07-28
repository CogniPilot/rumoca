//! Incidence matrix construction for DAE structural analysis.

pub(crate) mod corners;
pub mod rows;

use std::collections::{HashMap, HashSet};

use rumoca_core::ExpressionVisitor;
use rumoca_ir_dae as dae;

use crate::incidence::corners::{CornerPlan, RowSource, cell_coordinate, uniform_translation};
use crate::incidence::rows::{IncidenceRows, IncidenceRowsBuilder};
use crate::types::{EquationRef, UnknownId};

/// Incidence data for a DAE system.
#[derive(Debug)]
pub struct Incidence {
    /// Number of equations.
    pub n_eq: usize,
    /// Number of unknowns.
    pub n_var: usize,
    /// For each equation, the sorted unknown indices it references, stored as
    /// compressed sparse rows so structural matching, Tarjan and BLT read
    /// canonical `&[usize]` runs instead of per-row hash sets.
    pub eq_unknowns: IncidenceRows,
    /// Ordered list of unknown identifiers (index → `UnknownId`).
    pub unknown_names: Vec<UnknownId>,
    /// Source span of each unknown (index → span), parallel to `unknown_names`,
    /// so structural-singularity errors are traceable back to the offending
    /// model variable. `None` represents unknowns not sourced from DAE variables.
    pub unknown_spans: Vec<Option<rumoca_core::Span>>,
    /// dae::Equation references (index → `EquationRef`).
    pub equation_refs: Vec<EquationRef>,
    /// Compact, proof-derived matching candidates for regular equation
    /// families. Scalar incidence remains the compatibility view; matching
    /// consumes these affine candidates first without rediscovering family
    /// structure from anonymous rows.
    pub(crate) structured_matching: Vec<StructuredMatchingFamily>,
}

#[derive(Debug, Clone)]
pub(crate) struct StructuredMatchingFamily {
    pub(crate) first_equation_index: usize,
    pub(crate) equations_per_point: usize,
    pub(crate) point_count: usize,
    pub(crate) extents: Vec<usize>,
    pub(crate) cell_strides: Vec<usize>,
    pub(crate) base_unknowns: Vec<usize>,
    pub(crate) unknown_steps: Vec<Vec<i64>>,
    /// Span of the `for`-equation that owns the family, so a contract failure
    /// on any of its rows is reported against real source.
    pub(crate) span: rumoca_core::Span,
}

impl StructuredMatchingFamily {
    pub(crate) fn candidate(
        &self,
        point: usize,
        equation_position: usize,
    ) -> Option<(usize, usize)> {
        if point >= self.point_count || equation_position >= self.equations_per_point {
            return None;
        }
        let equation = point
            .checked_mul(self.equations_per_point)?
            .checked_add(equation_position)?
            .checked_add(self.first_equation_index)?;
        let mut unknown = i64::try_from(*self.base_unknowns.get(equation_position)?).ok()?;
        for (dimension, step) in self
            .unknown_steps
            .get(equation_position)?
            .iter()
            .enumerate()
        {
            let coordinate = cell_coordinate(
                point,
                *self.cell_strides.get(dimension)?,
                *self.extents.get(dimension)?,
            );
            unknown = unknown.checked_add(i64::try_from(coordinate).ok()?.checked_mul(*step)?)?;
        }
        Some((equation, usize::try_from(unknown).ok()?))
    }

    /// Number of scalar rows the family covers.
    pub(crate) fn row_count(&self) -> Option<usize> {
        self.point_count.checked_mul(self.equations_per_point)
    }

    /// The contiguous scalar row range the family owns.
    pub(crate) fn row_range(&self) -> Option<std::ops::Range<usize>> {
        let end = self.first_equation_index.checked_add(self.row_count()?)?;
        Some(self.first_equation_index..end)
    }
}

impl Incidence {
    /// Create a new incidence matrix from pre-built data.
    ///
    /// The `Vec<HashSet<usize>>` parameter is retained for the IC-projection and
    /// Solve-projection builders, which assemble rows from unordered sources;
    /// the rows are canonicalized into the compact CSR store on the way in.
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
            eq_unknowns: IncidenceRows::from_sets(eq_unknowns),
            unknown_spans: vec![None; n_var],
            unknown_names,
            equation_refs,
            structured_matching: Vec::new(),
        }
    }
}

/// Walks scalar equation bodies on demand, producing canonical CSR runs.
struct EquationRowWalker<'a> {
    equations: &'a [dae::Equation],
    der_resolver: &'a ScalarUnknownResolver,
    variable_resolver: &'a ScalarUnknownResolver,
}

impl EquationRowWalker<'_> {
    fn unknown_set(&self, row: usize) -> HashSet<usize> {
        match self.equations.get(row) {
            Some(equation) => {
                collect_equation_unknowns(equation, self.der_resolver, self.variable_resolver)
            }
            None => HashSet::new(),
        }
    }

    fn ordered_occurrences(&self, row: usize) -> Vec<usize> {
        let Some(equation) = self.equations.get(row) else {
            return Vec::new();
        };
        let mut occurrences = Vec::new();

        let mut der_collector = DerOperandCollector::default();
        der_collector.visit_expression(&equation.rhs);
        for (name, subscripts) in der_collector.operands {
            occurrences.extend(self.der_resolver.resolve_var_ref_all(&name, &subscripts));
        }

        if let Some(lhs) = equation.lhs.as_ref() {
            occurrences.extend(self.variable_resolver.resolve_var_name_all(lhs.var_name()));
        }
        collect_expression_unknown_occurrences(
            &equation.rhs,
            self.variable_resolver,
            &mut occurrences,
        );
        occurrences
    }
}

impl RowSource for EquationRowWalker<'_> {
    fn ordered_row(&self, row: usize) -> Vec<usize> {
        self.ordered_occurrences(row)
    }
}

/// Build incidence data from a DAE.
pub(crate) fn build_incidence(dae: &dae::Dae) -> Result<Incidence, crate::StructuralError> {
    let (_unknown_map, unknown_names, unknown_spans) = build_unknown_map(dae);
    let (der_resolver, variable_resolver) = build_unknown_resolvers(dae, &unknown_names);

    let n_eq = dae.continuous.equations.len();
    let walker = EquationRowWalker {
        equations: &dae.continuous.equations,
        der_resolver: &der_resolver,
        variable_resolver: &variable_resolver,
    };
    let plans = checked_non_materialized_family_plans(dae, n_eq)?;
    let eq_unknowns = build_incidence_rows(&plans, &walker, n_eq)?;
    let equation_refs: Vec<EquationRef> = (0..n_eq).map(EquationRef).collect();
    let structured_matching = build_structured_matching_families(dae, &eq_unknowns);

    Ok(Incidence {
        n_eq,
        n_var: unknown_names.len(),
        eq_unknowns,
        unknown_names,
        unknown_spans,
        equation_refs,
        structured_matching,
    })
}

/// Emit the scalar compatibility graph in equation order.
///
/// Rows belonging to a non-materialized regular family are produced
/// arithmetically from the family's corner rows; every other row is walked. A
/// non-materialized family whose compact proof is incomplete is rejected:
/// placeholder interiors are derived views, not authoritative equation bodies,
/// so walking them would silently lose incidence.
fn build_incidence_rows(
    plans: &[CornerPlan],
    walker: &EquationRowWalker<'_>,
    n_eq: usize,
) -> Result<IncidenceRows, crate::StructuralError> {
    let mut builder = IncidenceRowsBuilder::with_row_capacity(n_eq);
    let mut plan_cursor = 0usize;
    let mut row = 0usize;
    while row < n_eq {
        if let Some(plan) = plan_starting_at(plans, &mut plan_cursor, row) {
            row += emit_planned_family(&mut builder, plan, walker)?;
        } else {
            builder.push_unsorted(walker.unknown_set(row));
            row += 1;
        }
    }
    Ok(builder.finish())
}

/// The plan whose row block starts exactly at `row`, advancing the cursor past
/// any plan that starts earlier (already consumed or degenerate).
fn plan_starting_at<'a>(
    plans: &'a [CornerPlan],
    cursor: &mut usize,
    row: usize,
) -> Option<&'a CornerPlan> {
    while plans.get(*cursor)?.first_equation_index < row {
        *cursor += 1;
    }
    let plan = plans.get(*cursor)?;
    if plan.first_equation_index != row {
        return None;
    }
    *cursor += 1;
    Some(plan)
}

/// Emit one proven family's rows, returning how many rows were emitted.
fn emit_planned_family(
    builder: &mut IncidenceRowsBuilder,
    plan: &CornerPlan,
    walker: &EquationRowWalker<'_>,
) -> Result<usize, crate::StructuralError> {
    let row_count = plan
        .row_count()
        .ok_or_else(|| crate::StructuralError::ContractViolation {
            reason: "compact structural family row count overflows".to_string(),
            span: plan.span,
        })?;
    corners::emit_family_rows(builder, plan, walker)?;
    Ok(row_count)
}

fn checked_non_materialized_family_plans(
    dae: &dae::Dae,
    n_eq: usize,
) -> Result<Vec<CornerPlan>, crate::StructuralError> {
    let mut plans = Vec::new();
    for family in dae
        .continuous
        .structured_equations
        .iter()
        .filter(|family| !family.interiors_materialized)
    {
        if family.regular.is_none() {
            return Err(compact_family_contract(
                family,
                "non-materialized equation family has no regular affine descriptor",
            ));
        }
        let Some(template) = family.template.as_ref() else {
            return Err(compact_family_contract(
                family,
                "non-materialized equation family has no authoritative comprehension template",
            ));
        };
        if template.body.len() != family.equations_per_point {
            return Err(compact_family_contract(
                family,
                format!(
                    "comprehension template has {} body row(s), expected {}",
                    template.body.len(),
                    family.equations_per_point
                ),
            ));
        }
        if template.scalar_view == rumoca_core::ComprehensionScalarView::BinderSubstitution
            && template
                .body
                .iter()
                .any(|body| !binders_are_confined_to_subscripts(body, family))
        {
            return Err(compact_family_contract(
                family,
                "a domain binder occurs as a value outside an array subscript; corner incidence cannot prove the interior body",
            ));
        }
        let Some(plan) = CornerPlan::from_family(family) else {
            return Err(compact_family_contract(
                family,
                "compact domain does not describe a non-empty contiguous equation family",
            ));
        };
        let Some(end) = plan.end_row() else {
            return Err(compact_family_contract(
                family,
                "compact family row range overflows",
            ));
        };
        if end > n_eq {
            return Err(compact_family_contract(
                family,
                format!(
                    "compact family row range {}..{end} exceeds the {n_eq} continuous rows",
                    plan.first_equation_index
                ),
            ));
        }
        plans.push(plan);
    }
    plans.sort_by_key(|plan| plan.first_equation_index);
    for pair in plans.windows(2) {
        let previous_end =
            pair[0]
                .end_row()
                .ok_or_else(|| crate::StructuralError::ContractViolation {
                    reason: "compact structural family row range overflows".to_string(),
                    span: pair[0].span,
                })?;
        if pair[1].first_equation_index < previous_end {
            return Err(crate::StructuralError::ContractViolation {
                reason: format!(
                    "non-materialized structural families overlap at continuous row {}",
                    pair[1].first_equation_index
                ),
                span: pair[1].span,
            });
        }
    }
    Ok(plans)
}

fn compact_family_contract(
    family: &dae::StructuredEquationFamily,
    reason: impl Into<String>,
) -> crate::StructuralError {
    crate::StructuralError::ContractViolation {
        reason: reason.into(),
        span: family.span,
    }
}

fn binders_are_confined_to_subscripts(
    expression: &rumoca_core::Expression,
    family: &dae::StructuredEquationFamily,
) -> bool {
    let mut proof = BinderSubscriptUseProof {
        binders: &family.domain.binders,
        inside_subscript: false,
        valid: true,
    };
    proof.visit_expression(expression);
    proof.valid
}

struct BinderSubscriptUseProof<'a> {
    binders: &'a [rumoca_core::StructuredIndexBinder],
    inside_subscript: bool,
    valid: bool,
}

impl ExpressionVisitor for BinderSubscriptUseProof<'_> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        if !self.inside_subscript
            && self
                .binders
                .iter()
                .any(|binder| binder.display_name == name.as_str())
        {
            self.valid = false;
        }
        self.walk_var_ref(name, subscripts);
    }

    fn visit_subscript(&mut self, subscript: &rumoca_core::Subscript) {
        let was_inside = self.inside_subscript;
        self.inside_subscript = true;
        self.walk_subscript(subscript);
        self.inside_subscript = was_inside;
    }
}

fn build_structured_matching_families(
    dae: &dae::Dae,
    eq_unknowns: &IncidenceRows,
) -> Vec<StructuredMatchingFamily> {
    dae.continuous
        .structured_equations
        .iter()
        .filter(|family| family.regular.is_some())
        .filter_map(|family| structured_matching_family(family, eq_unknowns))
        .collect()
}

fn structured_matching_family(
    family: &dae::StructuredEquationFamily,
    eq_unknowns: &IncidenceRows,
) -> Option<StructuredMatchingFamily> {
    let plan = CornerPlan::from_family(family)?;
    let dimension_count = plan.extents.len();
    let mut base_unknowns = Vec::with_capacity(plan.equations_per_point);
    let mut unknown_steps = vec![vec![0i64; dimension_count]; plan.equations_per_point];

    for equation_position in 0..plan.equations_per_point {
        let row = plan.row_of(0, equation_position)?;
        // Rows are stored ascending, so the first column is the minimum.
        base_unknowns.push(*eq_unknowns.get(row)?.first()?);
    }
    for (dimension, &extent) in plan.extents.iter().enumerate() {
        if extent <= 1 {
            continue;
        }
        let neighbor_point = *plan.cell_strides.get(dimension)?;
        for (equation_position, steps) in unknown_steps.iter_mut().enumerate() {
            let base_row = eq_unknowns.get(plan.row_of(0, equation_position)?)?;
            let neighbor_row = eq_unknowns.get(plan.row_of(neighbor_point, equation_position)?)?;
            steps[dimension] = uniform_translation(base_row, neighbor_row)?;
        }
    }

    Some(StructuredMatchingFamily {
        span: plan.span,
        first_equation_index: plan.first_equation_index,
        equations_per_point: plan.equations_per_point,
        point_count: plan.point_count,
        extents: plan.extents,
        cell_strides: plan.cell_strides,
        base_unknowns,
        unknown_steps,
    })
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
    if size == 0 {
        return;
    }
    if size == 1 {
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
) -> HashSet<usize> {
    let mut result = HashSet::new();

    // Collect `der(state)` unknowns from the residual, preserving any
    // subscripts on the differentiated operand. Using the un-subscripted base
    // name here (e.g. `der(p[1])` -> `p`) would resolve to *every* `der(p[i])`
    // via the array fallback in the resolver, spuriously coupling the
    // scalar `der(p[i]) = v[i]` rows into one SCC that tearing then finds
    // structurally singular. Resolving with the subscript pins each row to its
    // own scalar `der` unknown.
    let mut der_collector = DerOperandCollector::default();
    der_collector.visit_expression(&eq.rhs);
    for (name, subscripts) in der_collector.operands {
        for idx in der_resolver.resolve_var_ref_all(&name, &subscripts) {
            result.insert(idx);
        }
    }

    collect_equation_lhs_unknown(eq.lhs.as_ref(), variable_resolver, &mut result);
    collect_expression_unknowns(&eq.rhs, variable_resolver, &mut result);

    result
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

pub(crate) fn collect_equation_lhs_unknown(
    lhs: Option<&rumoca_core::Reference>,
    resolver: &ScalarUnknownResolver,
    cols: &mut HashSet<usize>,
) {
    let Some(lhs) = lhs else {
        return;
    };
    for idx in resolver.resolve_var_name_all(lhs.var_name()) {
        cols.insert(idx);
    }
}

fn build_unknown_resolvers(
    dae: &dae::Dae,
    unknown_names: &[UnknownId],
) -> (ScalarUnknownResolver, ScalarUnknownResolver) {
    let mut der_entries = Vec::new();
    let mut variable_entries = Vec::new();

    for (idx, unknown) in unknown_names.iter().enumerate() {
        match unknown {
            UnknownId::DerState(name) => der_entries.push((name.clone(), idx)),
            UnknownId::Variable(name) => variable_entries.push((name.clone(), idx)),
            UnknownId::SolverY(_) | UnknownId::Unmatched { .. } => {}
        }
    }

    let mut der_resolver = ScalarUnknownResolver::from_var_name_entries(der_entries);
    let mut variable_resolver = ScalarUnknownResolver::from_var_name_entries(variable_entries);
    der_resolver.register_variable_shapes(dae.variables.states.iter());
    variable_resolver.register_variable_shapes(
        dae.variables
            .algebraics
            .iter()
            .chain(dae.variables.outputs.iter()),
    );
    (der_resolver, variable_resolver)
}

#[derive(Clone)]
struct ShapedUnknownBinding {
    start: usize,
    dims: Vec<usize>,
    size: usize,
}

#[derive(Default, Clone)]
pub(crate) struct ScalarUnknownResolver {
    exact: HashMap<rumoca_core::VarName, usize>,
    base_all: HashMap<rumoca_core::VarName, Vec<usize>>,
    base_unique: HashMap<rumoca_core::VarName, usize>,
    shaped: HashMap<rumoca_core::VarName, ShapedUnknownBinding>,
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
                entries.push((name.clone(), next_idx));
                next_idx = next_idx.saturating_add(1);
                continue;
            }

            for i in 0..sz {
                entries.push((
                    rumoca_core::VarName::new(dae::scalar_name_text_for_flat_index(
                        name.as_str(),
                        &var.dims,
                        i,
                    )),
                    next_idx,
                ));
                next_idx = next_idx.saturating_add(1);
            }
        }

        let mut resolver = Self::from_var_name_entries(entries);
        resolver.register_variable_shapes(
            dae.variables
                .states
                .iter()
                .chain(dae.variables.algebraics.iter())
                .chain(dae.variables.outputs.iter()),
        );
        resolver
    }

    pub(crate) fn from_entries<I>(entries: I) -> Self
    where
        I: IntoIterator<Item = (String, usize)>,
    {
        Self::from_var_name_entries(
            entries
                .into_iter()
                .map(|(name, index)| (rumoca_core::VarName::new(name), index)),
        )
    }

    fn from_var_name_entries<I>(entries: I) -> Self
    where
        I: IntoIterator<Item = (rumoca_core::VarName, usize)>,
    {
        let mut exact = HashMap::new();
        let mut base_all: HashMap<rumoca_core::VarName, Vec<usize>> = HashMap::new();
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
            shaped: HashMap::new(),
        }
    }

    fn insert_name(
        exact: &mut HashMap<rumoca_core::VarName, usize>,
        base_all: &mut HashMap<rumoca_core::VarName, Vec<usize>>,
        name: &rumoca_core::VarName,
        idx: usize,
    ) {
        exact.insert(name.clone(), idx);
        if let Some(base) = dae::component_base_name(name.as_str()) {
            base_all
                .entry(rumoca_core::VarName::new(base))
                .or_default()
                .push(idx);
        }
    }

    fn register_variable_shapes<'a, I>(&mut self, variables: I)
    where
        I: IntoIterator<Item = (&'a rumoca_core::VarName, &'a dae::Variable)>,
    {
        for (name, variable) in variables {
            let size = variable.size();
            if size == 0 {
                continue;
            }
            let Some(dims) = variable
                .dims
                .iter()
                .map(|dimension| usize::try_from(*dimension).ok())
                .collect::<Option<Vec<_>>>()
            else {
                continue;
            };
            let first_name = if size == 1 {
                name.clone()
            } else {
                rumoca_core::VarName::new(dae::scalar_name_text_for_flat_index(
                    name.as_str(),
                    &variable.dims,
                    0,
                ))
            };
            let Some(start) = self.exact.get(&first_name).copied() else {
                continue;
            };
            self.shaped
                .insert(name.clone(), ShapedUnknownBinding { start, dims, size });
        }
    }

    fn resolve_name(&self, name: &rumoca_core::VarName) -> Option<usize> {
        self.exact.get(name).copied().or_else(|| {
            dae::component_base_name(name.as_str()).and_then(|base| {
                self.base_unique
                    .get(&rumoca_core::VarName::new(base))
                    .copied()
            })
        })
    }

    fn resolve_var_name_all(&self, name: &rumoca_core::VarName) -> Vec<usize> {
        if let Some(idx) = self.resolve_name(name) {
            return vec![idx];
        }
        dae::component_base_name(name.as_str())
            .and_then(|base| self.base_all.get(&rumoca_core::VarName::new(base)).cloned())
            .unwrap_or_default()
    }

    pub(crate) fn resolve_var_ref_all(
        &self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) -> Vec<usize> {
        if subscripts.is_empty() {
            return self.resolve_var_name_all(name.var_name());
        }
        if let Some(indices) = constant_subscript_indices(subscripts)
            && self.shaped.contains_key(name.var_name())
        {
            return self
                .resolve_shaped_element(name.var_name(), &indices)
                .into_iter()
                .collect();
        }
        if let Some(canonical) = canonical_var_ref_key(name, subscripts) {
            let resolved = self.resolve_var_name_all(&rumoca_core::VarName::new(canonical));
            if !resolved.is_empty() {
                return resolved;
            }
        }
        self.resolve_var_name_all(name.var_name())
    }

    fn resolve_shaped_element(
        &self,
        name: &rumoca_core::VarName,
        indices: &[usize],
    ) -> Option<usize> {
        let binding = self.shaped.get(name)?;
        if indices.len() != binding.dims.len() {
            return None;
        }
        let mut flat_index = 0usize;
        for (&index, &extent) in indices.iter().zip(&binding.dims) {
            if index == 0 || index > extent {
                return None;
            }
            flat_index = flat_index
                .checked_mul(extent)?
                .checked_add(index.checked_sub(1)?)?;
        }
        (flat_index < binding.size)
            .then(|| binding.start.checked_add(flat_index))
            .flatten()
    }
}

fn constant_subscript_indices(subscripts: &[rumoca_core::Subscript]) -> Option<Vec<usize>> {
    subscripts.iter().map(subscript_index_value).collect()
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

pub(crate) fn collect_expression_unknowns(
    expr: &rumoca_core::Expression,
    resolver: &ScalarUnknownResolver,
    cols: &mut HashSet<usize>,
) {
    let mut collector = ExpressionUnknownCollector { resolver, cols };
    collector.visit_expression(expr);
}

fn collect_expression_unknown_occurrences(
    expr: &rumoca_core::Expression,
    resolver: &ScalarUnknownResolver,
    occurrences: &mut Vec<usize>,
) {
    let mut collector = ExpressionUnknownCollector {
        resolver,
        cols: occurrences,
    };
    collector.visit_expression(expr);
}

struct ExpressionUnknownCollector<'a, C> {
    resolver: &'a ScalarUnknownResolver,
    cols: &'a mut C,
}

impl<C: Extend<usize>> ExpressionVisitor for ExpressionUnknownCollector<'_, C> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        self.cols
            .extend(self.resolver.resolve_var_ref_all(name, subscripts));
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
            span,
        } = base
            && span.is_dummy()
        {
            let mut combined = Vec::with_capacity(base_subscripts.len() + subscripts.len());
            combined.extend_from_slice(base_subscripts);
            combined.extend_from_slice(subscripts);
            self.cols
                .extend(self.resolver.resolve_var_ref_all(name, &combined));
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
        if let Some((reference, subscripts)) = dae::indexed_field_var_ref(base, field) {
            self.cols
                .extend(self.resolver.resolve_var_ref_all(&reference, &subscripts));
            for subscript in &subscripts {
                self.visit_subscript(subscript);
            }
            return;
        }
        self.visit_expression(base);
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

/// Build structural solver sparsity triplets `(row, col)` for `dae.continuous.equations`.
///
/// Column order matches the solver state vector: states, then algebraics, then outputs.
/// Row order matches the current `dae.continuous.equations` order.
///
/// This is intended to be called after equation reordering for solver use.
pub fn build_solver_sparsity_triplets(dae: &dae::Dae) -> Vec<(usize, usize)> {
    let resolver = ScalarUnknownResolver::from_dae(dae);
    let mut triplets = Vec::new();

    for (row, eq) in dae.continuous.equations.iter().enumerate() {
        let mut cols = HashSet::new();
        collect_expression_unknowns(&eq.rhs, &resolver, &mut cols);
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
    eq_unknowns: &IncidenceRows,
    match_var: &[Option<usize>],
    n_eq: usize,
) -> Vec<Vec<usize>> {
    let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n_eq];
    for (eq_a, unknowns) in eq_unknowns.iter().enumerate().take(n_eq) {
        // CSR runs are already ascending, so the old per-row collect + sort is
        // gone. Matched owners are not sorted even when columns are, so the
        // adjacency row still needs its own canonicalization.
        for &var_idx in unknowns {
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
    fn incidence_keeps_direct_algebraic_target_in_derivative_residual() {
        let mut dae = dae::Dae::new();
        dae.variables.states.insert(
            rumoca_core::VarName::new("x"),
            dae::Variable::new(rumoca_core::VarName::new("x"), test_span()),
        );
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("v"),
            dae::Variable::new(rumoca_core::VarName::new("v"), test_span()),
        );
        dae.continuous.equations.push(eq(sub(
            var("v"),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Der,
                args: vec![var("x")],
                span: test_span(),
            },
        )));

        let incidence = build_incidence(&dae).expect("valid scalar incidence");
        let derivative = incidence
            .unknown_names
            .iter()
            .position(|unknown| *unknown == UnknownId::DerState(rumoca_core::VarName::new("x")))
            .expect("der(x) unknown");
        let voltage = incidence
            .unknown_names
            .iter()
            .position(|unknown| *unknown == UnknownId::Variable(rumoca_core::VarName::new("v")))
            .expect("v unknown");

        assert_eq!(incidence.eq_unknowns.len(), 1);
        assert!(incidence.eq_unknowns.contains(0, derivative));
        assert!(incidence.eq_unknowns.contains(0, voltage));
    }

    #[test]
    fn incidence_omits_zero_length_array_unknowns() {
        let mut dae = dae::Dae::new();
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("x"),
            dae::Variable::new(rumoca_core::VarName::new("x"), test_span()),
        );
        let mut empty = dae::Variable::new(rumoca_core::VarName::new("empty"), test_span());
        empty.dims = vec![0];
        dae.variables
            .algebraics
            .insert(rumoca_core::VarName::new("empty"), empty);
        dae.continuous.equations.push(eq(sub(
            var("x"),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sum,
                args: vec![var("empty")],
                span: test_span(),
            },
        )));

        let incidence = build_incidence(&dae).expect("valid scalar incidence");

        assert_eq!(
            incidence.unknown_names,
            [UnknownId::Variable(rumoca_core::VarName::new("x"))]
        );
        assert_eq!(
            incidence.eq_unknowns,
            IncidenceRows::from_sorted_rows(vec![vec![0]])
        );
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
    fn shaped_resolver_does_not_widen_invalid_constant_subscript_to_whole_array() {
        let mut dae = dae::Dae::new();
        let mut u = dae::Variable::new(
            rumoca_core::VarName::new("u"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        );
        u.dims = vec![2];
        dae.variables
            .algebraics
            .insert(rumoca_core::VarName::new("u"), u);
        let resolver = ScalarUnknownResolver::from_dae(&dae);

        assert!(
            resolver
                .resolve_var_ref_all(
                    &rumoca_core::Reference::new("u"),
                    &[rumoca_core::Subscript::Index {
                        value: 3,
                        span: test_span(),
                    }],
                )
                .is_empty()
        );
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
    fn compact_domain_reports_cartesian_extents() {
        let domain = rumoca_core::StructuredIndexDomain {
            binders: vec![
                rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 0,
                    upper: 1,
                    step: 1,
                },
                rumoca_core::StructuredIndexBinder {
                    id: 1,
                    display_name: "j".to_string(),
                    lower: 0,
                    upper: 2,
                    step: 1,
                },
            ],
        };
        assert_eq!(domain.extents(), Ok(vec![2, 3]));
    }

    /// A 1-D `regular` family of `cells` cells, non-materialized so
    /// `build_incidence` takes the corner path.
    fn compact_one_dim_family(cells: i64) -> dae::StructuredEquationFamily {
        let mut family = corners::tests::one_dim_family(cells);
        family.regular = Some(rumoca_core::RegularForFamily {
            binders: vec!["i".to_string()],
            accesses: Vec::new(),
        });
        family.template = Some(rumoca_core::ComprehensionTemplate {
            body: vec![lit(0.0)],
            scalar_view: rumoca_core::ComprehensionScalarView::BinderSubstitution,
        });
        family.interiors_materialized = false;
        family
    }

    #[test]
    fn structured_matching_family_keeps_affine_candidates_compact() {
        let family = corners::tests::two_dim_family(1_000, 1_000);
        // Only the base and one neighbor per dimension are needed. The backing
        // store is sparse here to prove descriptor size is independent of the
        // million-point domain.
        let mut sparse = vec![Vec::new(); 1_001];
        sparse[0] = vec![10, 20];
        sparse[1] = vec![11, 21];
        sparse[1_000] = vec![1_010, 1_020];
        let eq_unknowns = IncidenceRows::from_sorted_rows(sparse);

        let matching = structured_matching_family(&family, &eq_unknowns)
            .expect("corner translations should produce a compact matching family");

        assert_eq!(matching.base_unknowns, vec![10]);
        assert_eq!(matching.unknown_steps, vec![vec![1_000, 1]]);
        assert_eq!(matching.candidate(999_999, 0), Some((999_999, 1_000_009)));
        assert_eq!(matching.row_range(), Some(0..1_000_000));
    }

    #[test]
    fn structured_matching_family_declines_when_the_neighbor_is_not_a_translation() {
        // Base {0,1}; neighbor {2,5} shifts its elements by 2 and 4 -- not a
        // single constant offset -- so the per-binder unit cannot be derived and
        // the compact matching optimization declines without affecting scalar
        // compatibility matching.
        let family = compact_one_dim_family(2);
        let eq_unknowns = IncidenceRows::from_sorted_rows(vec![vec![0, 1], vec![2, 5]]);
        assert!(structured_matching_family(&family, &eq_unknowns).is_none());
    }

    /// A DAE holding `cells` scalar rows over one non-materialized regular
    /// family where only the base cell and its neighbor carry a real body; the
    /// interior rows are literal placeholders, exactly as flatten emits them
    /// when `interiors_materialized` is false.
    fn corner_only_family_dae(cells: usize) -> dae::Dae {
        let mut system = dae::Dae::new();
        for index in 0..cells {
            let name = rumoca_core::VarName::new(format!("x{index}").as_str());
            system
                .variables
                .algebraics
                .insert(name.clone(), dae::Variable::new(name, test_span()));
        }
        for index in 0..cells {
            let rhs = if index < 2 {
                var(&format!("x{index}"))
            } else {
                lit(0.0)
            };
            let mut equation = eq(rhs);
            equation.origin = format!("corner_family[{index}]");
            system.continuous.equations.push(equation);
        }
        system.continuous.structured_equations = vec![compact_one_dim_family(
            i64::try_from(cells).expect("cell count fits i64"),
        )];
        system
    }

    #[test]
    fn corner_derived_interiors_are_emitted_without_walking_bodies() {
        // 100_000 interior rows carry no body at all. Walking them would produce
        // empty rows; the corner model translates the base row instead, so the
        // last row must reference the last unknown.
        let cells = 100_000usize;
        let dae = corner_only_family_dae(cells);
        let incidence = build_incidence(&dae).expect("uniform corner family");

        assert_eq!(incidence.eq_unknowns.len(), cells);
        let base = incidence.eq_unknowns.row(0).to_vec();
        assert_eq!(base, vec![0], "base row references exactly x0");
        assert_eq!(
            incidence.eq_unknowns.row(cells - 1),
            &[cells - 1],
            "interior rows are the base row translated by their cell offset"
        );
        assert_eq!(incidence.structured_matching.len(), 1);
        assert_eq!(incidence.structured_matching[0].point_count, cells);
    }

    #[test]
    fn materialized_family_rows_stay_authoritative() {
        // The same DAE with `interiors_materialized = true` must keep the walked
        // (placeholder) rows: materialized incidence is authoritative and the
        // corner translation must not be applied behind its back.
        let cells = 8usize;
        let mut dae = corner_only_family_dae(cells);
        dae.continuous.structured_equations[0].interiors_materialized = true;
        let incidence = build_incidence(&dae).expect("materialized rows remain authoritative");

        assert_eq!(incidence.eq_unknowns.len(), cells);
        for row in 2..cells {
            assert!(
                incidence.eq_unknowns.row(row).is_empty(),
                "materialized row {row} keeps its walked (empty) body"
            );
        }
    }

    #[test]
    fn non_uniform_compact_family_is_rejected_instead_of_walking_placeholders() {
        // A non-materialized family whose neighbor row is not a translation of
        // the base has no authoritative interior rows to fall back to.
        let cells = 6usize;
        let mut dae = corner_only_family_dae(cells);
        dae.continuous.equations[1].rhs = add(var("x1"), var("x3"));
        let error = build_incidence(&dae).expect_err("mixed incidence strides are not proven");
        assert!(
            error
                .to_string()
                .contains("does not preserve its base-cell incidence cardinality"),
            "the failure should identify the rejected corner proof, got {error}"
        );
    }

    #[test]
    fn non_materialized_family_requires_an_authoritative_template() {
        let mut dae = corner_only_family_dae(4);
        dae.continuous.structured_equations[0].template = None;

        let error = build_incidence(&dae).expect_err("placeholder rows cannot replace a template");
        assert!(
            error.to_string().contains("comprehension template"),
            "the missing compact owner should be diagnosed, got {error}"
        );
    }

    #[test]
    fn binder_value_use_is_rejected_before_corner_extrapolation() {
        let mut dae = corner_only_family_dae(4);
        dae.continuous.structured_equations[0]
            .template
            .as_mut()
            .expect("fixture template")
            .body = vec![add(var("x0"), var("i"))];

        let error = build_incidence(&dae).expect_err("a binder-valued body is not corner-affine");
        assert!(
            error.to_string().contains("outside an array subscript"),
            "binder confinement should be diagnosed, got {error}"
        );
    }

    #[test]
    fn fixed_and_varying_incidence_keeps_affine_zero_stride() {
        let mut dae = corner_only_family_dae(4);
        let captured = rumoca_core::VarName::new("z");
        dae.variables
            .algebraics
            .insert(captured.clone(), dae::Variable::new(captured, test_span()));
        dae.continuous.equations[0].rhs = add(var("x0"), var("z"));
        dae.continuous.equations[1].rhs = add(var("x1"), var("z"));

        let incidence =
            build_incidence(&dae).expect("a captured scalar has zero affine binder stride");
        assert_eq!(incidence.eq_unknowns.row(0), &[0, 4]);
        assert_eq!(incidence.eq_unknowns.row(1), &[1, 4]);
        assert_eq!(incidence.eq_unknowns.row(2), &[2, 4]);
        assert_eq!(incidence.eq_unknowns.row(3), &[3, 4]);
    }

    #[test]
    fn dependency_graph_reads_sorted_csr_rows() {
        let eq_unknowns = IncidenceRows::from_sorted_rows(vec![vec![0], vec![0, 1], vec![1, 2]]);
        let match_var = vec![Some(0), Some(1), Some(2)];
        let adj = build_dependency_graph(&eq_unknowns, &match_var, 3);
        assert_eq!(adj, vec![Vec::new(), vec![0], vec![1]]);
    }
}
