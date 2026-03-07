//! Incidence matrix construction for DAE structural analysis.

use std::collections::{HashMap, HashSet};

use rumoca_ir_dae as dae;

use crate::types::{EquationRef, UnknownId};

/// Incidence data for a DAE system.
pub struct Incidence {
    /// Number of equations.
    pub n_eq: usize,
    /// Number of unknowns.
    pub n_var: usize,
    /// For each equation, the set of unknown indices it references.
    pub eq_unknowns: Vec<HashSet<usize>>,
    /// Ordered list of unknown identifiers (index → `UnknownId`).
    pub unknown_names: Vec<UnknownId>,
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
            unknown_names,
            equation_refs,
        }
    }
}

/// Build incidence data from a DAE.
pub(crate) fn build_incidence(dae: &dae::Dae) -> Incidence {
    let (_unknown_map, unknown_names) = build_unknown_map(dae);
    let n_var = unknown_names.len();
    let (der_resolver, variable_resolver) = build_unknown_resolvers(&unknown_names);

    let mut equation_refs = Vec::new();
    let mut equations_rhs = Vec::new();

    for (i, eq) in dae.f_x.iter().enumerate() {
        equation_refs.push(EquationRef::Continuous(i));
        equations_rhs.push(&eq.rhs);
    }

    let n_eq = equation_refs.len();

    let eq_unknowns: Vec<HashSet<usize>> = equations_rhs
        .iter()
        .map(|rhs| collect_equation_unknowns(rhs, &der_resolver, &variable_resolver))
        .collect();

    Incidence {
        n_eq,
        n_var,
        eq_unknowns,
        unknown_names,
        equation_refs,
    }
}

/// Build the unknown map: assign an index to each unknown in the DAE.
fn build_unknown_map(dae: &dae::Dae) -> (HashMap<UnknownId, usize>, Vec<UnknownId>) {
    let mut map = HashMap::new();
    let mut names = Vec::new();

    for name in dae.states.keys() {
        let id = UnknownId::DerState(name.clone());
        map.insert(id.clone(), names.len());
        names.push(id);
    }

    for name in dae.algebraics.keys() {
        let id = UnknownId::Variable(name.clone());
        map.insert(id.clone(), names.len());
        names.push(id);
    }

    for name in dae.outputs.keys() {
        let id = UnknownId::Variable(name.clone());
        map.insert(id.clone(), names.len());
        names.push(id);
    }

    (map, names)
}

/// Collect unknown indices referenced by an equation's expression.
fn collect_equation_unknowns(
    expr: &dae::Expression,
    der_resolver: &ScalarUnknownResolver,
    variable_resolver: &ScalarUnknownResolver,
) -> HashSet<usize> {
    let mut result = HashSet::new();

    let mut der_states = HashSet::new();
    expr.collect_state_variables(&mut der_states);
    for name in der_states {
        for idx in der_resolver.resolve_name_all(name.as_str()) {
            result.insert(idx);
        }
    }

    collect_expression_unknowns(expr, variable_resolver, &mut result);

    result
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
            .states
            .iter()
            .chain(dae.algebraics.iter())
            .chain(dae.outputs.iter())
        {
            let sz = var.size();
            if sz <= 1 {
                entries.push((name.as_str().to_string(), next_idx));
                next_idx = next_idx.saturating_add(1);
                continue;
            }

            for i in 1..=sz {
                entries.push((format!("{}[{i}]", name.as_str()), next_idx));
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
        name: &dae::VarName,
        subscripts: &[dae::Subscript],
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

fn subscript_index_value(sub: &dae::Subscript) -> Option<i64> {
    match sub {
        dae::Subscript::Index(i) => Some(*i),
        dae::Subscript::Expr(expr) => match expr.as_ref() {
            dae::Expression::Literal(dae::Literal::Integer(i)) => Some(*i),
            dae::Expression::Literal(dae::Literal::Real(v))
                if v.is_finite() && v.fract() == 0.0 =>
            {
                Some(*v as i64)
            }
            _ => None,
        },
        dae::Subscript::Colon => None,
    }
}

fn canonical_var_ref_key(name: &dae::VarName, subscripts: &[dae::Subscript]) -> Option<String> {
    if subscripts.is_empty() {
        return Some(name.as_str().to_string());
    }

    let mut index_parts = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        index_parts.push(subscript_index_value(sub)?.to_string());
    }
    Some(format!("{}[{}]", name.as_str(), index_parts.join(",")))
}

fn collect_subscript_unknowns(
    subscripts: &[dae::Subscript],
    resolver: &ScalarUnknownResolver,
    cols: &mut HashSet<usize>,
) {
    for sub in subscripts {
        if let dae::Subscript::Expr(expr) = sub {
            collect_expression_unknowns(expr, resolver, cols);
        }
    }
}

pub(crate) fn collect_expression_unknowns(
    expr: &dae::Expression,
    resolver: &ScalarUnknownResolver,
    cols: &mut HashSet<usize>,
) {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            for idx in resolver.resolve_var_ref_all(name, subscripts) {
                cols.insert(idx);
            }
            collect_subscript_unknowns(subscripts, resolver, cols);
        }
        dae::Expression::Index { base, subscripts } => {
            if let dae::Expression::VarRef {
                name,
                subscripts: base_subscripts,
            } = base.as_ref()
            {
                let mut combined = Vec::with_capacity(base_subscripts.len() + subscripts.len());
                combined.extend_from_slice(base_subscripts);
                combined.extend_from_slice(subscripts);
                for idx in resolver.resolve_var_ref_all(name, &combined) {
                    cols.insert(idx);
                }
                collect_subscript_unknowns(base_subscripts, resolver, cols);
                collect_subscript_unknowns(subscripts, resolver, cols);
            } else {
                collect_expression_unknowns(base, resolver, cols);
                collect_subscript_unknowns(subscripts, resolver, cols);
            }
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            collect_expression_unknowns(lhs, resolver, cols);
            collect_expression_unknowns(rhs, resolver, cols);
        }
        dae::Expression::Unary { rhs, .. } => {
            collect_expression_unknowns(rhs, resolver, cols);
        }
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            ..
        } => {}
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_expression_unknowns(arg, resolver, cols);
            }
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, value) in branches {
                collect_expression_unknowns(cond, resolver, cols);
                collect_expression_unknowns(value, resolver, cols);
            }
            collect_expression_unknowns(else_branch, resolver, cols);
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            for element in elements {
                collect_expression_unknowns(element, resolver, cols);
            }
        }
        dae::Expression::Range { start, step, end } => {
            collect_expression_unknowns(start, resolver, cols);
            if let Some(step) = step.as_deref() {
                collect_expression_unknowns(step, resolver, cols);
            }
            collect_expression_unknowns(end, resolver, cols);
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            for index in indices {
                collect_expression_unknowns(&index.range, resolver, cols);
            }
            collect_expression_unknowns(expr, resolver, cols);
            if let Some(filter) = filter.as_deref() {
                collect_expression_unknowns(filter, resolver, cols);
            }
        }
        dae::Expression::FieldAccess { base, .. } => {
            collect_expression_unknowns(base, resolver, cols);
        }
        dae::Expression::Literal(_) | dae::Expression::Empty => {}
    }
}

/// Build structural solver sparsity triplets `(row, col)` for `dae.f_x`.
///
/// Column order matches the solver state vector: states, then algebraics, then outputs.
/// Row order matches the current `dae.f_x` order.
///
/// This is intended to be called after equation reordering for solver use.
pub fn build_solver_sparsity_triplets(dae: &dae::Dae) -> Vec<(usize, usize)> {
    let resolver = ScalarUnknownResolver::from_dae(dae);
    let mut triplets = Vec::new();

    for (row, eq) in dae.f_x.iter().enumerate() {
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

    fn var(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: vec![],
        }
    }

    fn lit(v: f64) -> dae::Expression {
        dae::Expression::Literal(dae::Literal::Real(v))
    }

    fn sub(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    fn add(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(rumoca_ir_core::Token::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    fn eq(rhs: dae::Expression) -> dae::Equation {
        dae::Equation {
            lhs: None,
            rhs,
            span: Span::DUMMY,
            origin: String::new(),
            scalar_count: 1,
        }
    }

    #[test]
    fn test_build_solver_sparsity_triplets_skips_derivative_argument_dependencies() {
        let mut dae = dae::Dae::new();
        dae.states.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        dae.algebraics.insert(
            dae::VarName::new("z"),
            dae::Variable::new(dae::VarName::new("z")),
        );

        dae.f_x.push(eq(sub(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Der,
                args: vec![var("x")],
            },
            var("z"),
        )));
        dae.f_x.push(eq(sub(var("z"), add(var("x"), lit(1.0)))));

        let triplets = build_solver_sparsity_triplets(&dae);
        assert!(triplets.contains(&(0, 1))); // row0 depends on z
        assert!(triplets.contains(&(1, 0))); // row1 depends on x
        assert!(triplets.contains(&(1, 1))); // row1 depends on z
        assert!(!triplets.contains(&(0, 0))); // der(x) itself does not depend on x in residual eval
    }

    #[test]
    fn test_build_solver_sparsity_triplets_resolves_indexed_component_names() {
        let mut dae = dae::Dae::new();
        dae.states.insert(
            dae::VarName::new("support.phi"),
            dae::Variable::new(dae::VarName::new("support.phi")),
        );
        dae.f_x.push(eq(sub(
            dae::Expression::VarRef {
                name: dae::VarName::new("support[1].phi"),
                subscripts: vec![],
            },
            lit(0.0),
        )));

        let triplets = build_solver_sparsity_triplets(&dae);
        assert_eq!(triplets, vec![(0, 0)]);
    }

    #[test]
    fn test_build_solver_sparsity_triplets_maps_whole_array_refs_to_all_scalars() {
        let mut dae = dae::Dae::new();

        let mut u = dae::Variable::new(dae::VarName::new("u"));
        u.dims = vec![2];
        dae.algebraics.insert(dae::VarName::new("u"), u);
        dae.algebraics.insert(
            dae::VarName::new("y"),
            dae::Variable::new(dae::VarName::new("y")),
        );

        dae.f_x.push(eq(sub(
            var("y"),
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Product,
                args: vec![var("u")],
            },
        )));

        let triplets = build_solver_sparsity_triplets(&dae);
        assert_eq!(triplets, vec![(0, 0), (0, 1), (0, 2)]);
    }
}
