//! Incidence matrix construction for DAE structural analysis.

use std::collections::{HashMap, HashSet};

use rumoca_core::ExpressionVisitor;
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

    let mut equation_refs = Vec::new();
    let mut equations = Vec::new();

    for (i, eq) in dae.continuous.equations.iter().enumerate() {
        equation_refs.push(EquationRef(i));
        equations.push(eq);
    }

    let n_eq = equation_refs.len();

    let eq_unknowns: Vec<HashSet<usize>> = equations
        .iter()
        .map(|eq| collect_equation_unknowns(eq, &der_resolver, &variable_resolver))
        .collect();

    Incidence {
        n_eq,
        n_var: unknown_names.len(),
        eq_unknowns,
        unknown_names,
        unknown_spans,
        equation_refs,
    }
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
        for idx in der_resolver.resolve_var_ref_all(&name, &subscripts) {
            result.insert(idx);
        }
    }

    collect_equation_lhs_unknown(eq.lhs.as_ref(), variable_resolver, &mut result);
    collect_expression_unknowns(&eq.rhs, variable_resolver, &mut result);
    if !result.is_empty()
        && let Some(target) = direct_residual_definition_target(&eq.rhs)
        && equation_contains_derivative(&eq.rhs)
    {
        for idx in variable_resolver.resolve_var_ref_all(target.0, target.1) {
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
    }

    fn resolve_name(&self, name: &str) -> Option<usize> {
        self.exact.get(name).copied().or_else(|| {
            dae::component_base_name(name).and_then(|base| self.base_unique.get(&base).copied())
        })
    }

    pub(crate) fn resolve_name_all(&self, name: &str) -> Vec<usize> {
        if let Some(idx) = self.resolve_name(name) {
            return vec![idx];
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

struct ExpressionUnknownCollector<'a> {
    resolver: &'a ScalarUnknownResolver,
    cols: &'a mut HashSet<usize>,
}

impl ExpressionVisitor for ExpressionUnknownCollector<'_> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        for idx in self.resolver.resolve_var_ref_all(name, subscripts) {
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
            span,
        } = base
            && span.is_dummy()
        {
            let mut combined = Vec::with_capacity(base_subscripts.len() + subscripts.len());
            combined.extend_from_slice(base_subscripts);
            combined.extend_from_slice(subscripts);
            for idx in self.resolver.resolve_var_ref_all(name, &combined) {
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
            for idx in self.resolver.resolve_var_ref_all(&reference, &subscripts) {
                self.cols.insert(idx);
            }
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
}
