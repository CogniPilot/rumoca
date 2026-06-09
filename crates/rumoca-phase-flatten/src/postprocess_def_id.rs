use super::*;
use rumoca_core::{ExpressionRewriter, StatementRewriter};
use std::collections::{HashMap, HashSet};

pub(crate) fn canonicalize_varrefs_via_instantiated_def_ids(flat: &mut flat::Model) {
    let known_variables: HashSet<String> = flat.variables.keys().map(ToString::to_string).collect();
    let def_id_index = DefIdVarRefIndex::build(flat);
    if def_id_index.is_empty() {
        return;
    }

    for var in flat.variables.values_mut() {
        let owner = var.name.as_str();
        canonicalize_def_id_opt_expr(
            &mut var.binding,
            &def_id_index,
            &known_variables,
            Some(owner),
        );
        canonicalize_def_id_opt_expr(&mut var.start, &def_id_index, &known_variables, Some(owner));
        canonicalize_def_id_opt_expr(&mut var.min, &def_id_index, &known_variables, Some(owner));
        canonicalize_def_id_opt_expr(&mut var.max, &def_id_index, &known_variables, Some(owner));
        canonicalize_def_id_opt_expr(
            &mut var.nominal,
            &def_id_index,
            &known_variables,
            Some(owner),
        );
    }
    for equation in &mut flat.equations {
        let owner = equation_origin_owner(&equation.origin);
        canonicalize_def_id_expr(
            &mut equation.residual,
            &def_id_index,
            &known_variables,
            owner.as_deref(),
        );
    }
    for equation in &mut flat.initial_equations {
        let owner = equation_origin_owner(&equation.origin);
        canonicalize_def_id_expr(
            &mut equation.residual,
            &def_id_index,
            &known_variables,
            owner.as_deref(),
        );
    }
    for assert_eq in &mut flat.assert_equations {
        canonicalize_def_id_expr(
            &mut assert_eq.condition,
            &def_id_index,
            &known_variables,
            None,
        );
        canonicalize_def_id_expr(
            &mut assert_eq.message,
            &def_id_index,
            &known_variables,
            None,
        );
        canonicalize_def_id_opt_expr(&mut assert_eq.level, &def_id_index, &known_variables, None);
    }
    for assert_eq in &mut flat.initial_assert_equations {
        canonicalize_def_id_expr(
            &mut assert_eq.condition,
            &def_id_index,
            &known_variables,
            None,
        );
        canonicalize_def_id_expr(
            &mut assert_eq.message,
            &def_id_index,
            &known_variables,
            None,
        );
        canonicalize_def_id_opt_expr(&mut assert_eq.level, &def_id_index, &known_variables, None);
    }
    for when_clause in &mut flat.when_clauses {
        canonicalize_def_id_expr(
            &mut when_clause.condition,
            &def_id_index,
            &known_variables,
            None,
        );
        canonicalize_def_id_when_equations(
            &mut when_clause.equations,
            &def_id_index,
            &known_variables,
        );
    }
    for algorithm in &mut flat.algorithms {
        canonicalize_def_id_statements(
            &mut algorithm.statements,
            &def_id_index,
            &known_variables,
            None,
        );
    }
    for algorithm in &mut flat.initial_algorithms {
        canonicalize_def_id_statements(
            &mut algorithm.statements,
            &def_id_index,
            &known_variables,
            None,
        );
    }
}

struct DefIdVarRefIndex {
    by_def_id: HashMap<rumoca_core::DefId, Vec<IndexedVarRef>>,
    by_leaf: HashMap<String, Vec<IndexedVarRef>>,
}

#[derive(Clone)]
struct IndexedVarRef {
    name: String,
    leaf: String,
    path: rumoca_core::ComponentPath,
}

impl DefIdVarRefIndex {
    fn build(flat: &flat::Model) -> Self {
        let mut by_def_id: HashMap<rumoca_core::DefId, Vec<IndexedVarRef>> = HashMap::new();
        let mut by_leaf: HashMap<String, Vec<IndexedVarRef>> = HashMap::new();
        for (name, var) in &flat.variables {
            let indexed = indexed_var_ref(name, var);
            by_leaf
                .entry(indexed_var_leaf(name, var).to_string())
                .or_default()
                .push(indexed.clone());
            if let Some(def_id) = var.component_ref.as_ref().and_then(|comp| comp.def_id) {
                by_def_id.entry(def_id).or_default().push(indexed);
            }
        }
        Self { by_def_id, by_leaf }
    }

    fn is_empty(&self) -> bool {
        self.by_def_id.is_empty() && self.by_leaf.is_empty()
    }

    fn resolve(
        &self,
        name: &rumoca_core::Reference,
        owner: Option<&str>,
        known_variables: &HashSet<String>,
    ) -> Option<String> {
        let raw = name.as_str();
        if known_variables.contains(raw) {
            return None;
        }
        let raw_leaf = name.last_segment();
        if let Some(def_id) = name.target_def_id()
            && let Some(candidates) = self.by_def_id.get(&def_id)
        {
            return resolve_best_owner_scoped_candidate(name, raw_leaf, candidates, owner);
        }
        if name.target_def_id().is_some() {
            return None;
        }
        if is_class_qualified_reference(name) {
            let candidates = self.by_leaf.get(raw_leaf)?;
            return resolve_best_owner_scoped_candidate(name, raw_leaf, candidates, owner);
        }
        None
    }
}

fn indexed_var_ref(name: &rumoca_core::VarName, var: &flat::Variable) -> IndexedVarRef {
    let path = var
        .component_ref
        .as_ref()
        .map(rumoca_core::ComponentPath::from_component_reference)
        .unwrap_or_else(|| rumoca_core::ComponentPath::from_flat_path(name.as_str()));
    IndexedVarRef {
        name: name.as_str().to_string(),
        leaf: indexed_var_leaf(name, var).to_string(),
        path,
    }
}

fn indexed_var_leaf<'a>(name: &'a rumoca_core::VarName, var: &'a flat::Variable) -> &'a str {
    var.component_ref
        .as_ref()
        .and_then(rumoca_core::ComponentReference::last_ident)
        .unwrap_or_else(|| name.last_segment())
}

fn resolve_best_owner_scoped_candidate(
    name: &rumoca_core::Reference,
    raw_leaf: &str,
    candidates: &[IndexedVarRef],
    owner: Option<&str>,
) -> Option<String> {
    let raw = name.as_str();
    let owner_path = owner.map(rumoca_core::ComponentPath::from_flat_path);
    let raw_path = name
        .component_ref()
        .map(rumoca_core::ComponentPath::from_component_reference)
        .unwrap_or_else(|| rumoca_core::ComponentPath::from_flat_path(raw));
    let mut scored = candidates
        .iter()
        .filter(|candidate| candidate.leaf == raw_leaf)
        .filter_map(|candidate| {
            let owner_score = owner_path
                .as_ref()
                .map(|owner| owner_scope_score(owner, &candidate.path))
                .unwrap_or_else(|| (candidates.len() == 1).then_some(0))?;
            let suffix_score = path_suffix_score(&raw_path, &candidate.path);
            Some(((owner_score, suffix_score), candidate))
        })
        .collect::<Vec<_>>();
    scored.sort_by_key(|(score, _)| *score);
    let (best_score, best) = scored.last()?;
    let ambiguous = scored
        .iter()
        .rev()
        .skip(1)
        .any(|(score, _)| score == best_score);
    (!ambiguous && best.name.as_str() != raw).then(|| best.name.clone())
}

fn path_suffix_score(
    raw_path: &rumoca_core::ComponentPath,
    candidate_path: &rumoca_core::ComponentPath,
) -> usize {
    raw_path
        .parts()
        .iter()
        .rev()
        .zip(candidate_path.parts().iter().rev())
        .take_while(|(raw, candidate)| raw == candidate)
        .count()
}

fn owner_scope_score(
    owner_path: &rumoca_core::ComponentPath,
    candidate_path: &rumoca_core::ComponentPath,
) -> Option<usize> {
    let candidate_scope = candidate_path.prefix(candidate_path.len().saturating_sub(1))?;
    if owner_path.starts_with(&candidate_scope) {
        return Some(candidate_scope.len());
    }
    candidate_scope
        .starts_with(owner_path)
        .then_some(owner_path.len())
}

fn is_class_qualified_reference(name: &rumoca_core::Reference) -> bool {
    let path = name
        .component_ref()
        .map(rumoca_core::ComponentPath::from_component_reference)
        .unwrap_or_else(|| rumoca_core::ComponentPath::from_reference(name));
    path.len() >= 3
        && path
            .parts()
            .first()
            .and_then(|part| part.chars().next())
            .is_some_and(char::is_uppercase)
}

fn equation_origin_owner(origin: &flat::EquationOrigin) -> Option<String> {
    match origin {
        flat::EquationOrigin::ComponentEquation { component }
        | flat::EquationOrigin::Algorithm { component } => Some(component.clone()),
        flat::EquationOrigin::Binding { variable }
        | flat::EquationOrigin::Reinit { state: variable }
        | flat::EquationOrigin::WhenAssignment { target: variable }
        | flat::EquationOrigin::UnconnectedFlow { variable } => Some(variable.clone()),
        flat::EquationOrigin::Connection { lhs, .. } => Some(lhs.clone()),
        flat::EquationOrigin::FlowSum { .. } => None,
    }
    .filter(|owner| !owner.is_empty())
}

fn canonicalize_def_id_opt_expr(
    expr: &mut Option<rumoca_core::Expression>,
    index: &DefIdVarRefIndex,
    known_variables: &HashSet<String>,
    owner: Option<&str>,
) {
    if let Some(expr) = expr {
        canonicalize_def_id_expr(expr, index, known_variables, owner);
    }
}

fn canonicalize_def_id_expr(
    expr: &mut rumoca_core::Expression,
    index: &DefIdVarRefIndex,
    known_variables: &HashSet<String>,
    owner: Option<&str>,
) {
    let mut rewriter = DefIdVarRefCanonicalizer {
        index,
        known_variables,
        owner,
    };
    *expr = rewriter.rewrite_expression(expr);
}

fn canonicalize_def_id_statements(
    statements: &mut [rumoca_core::Statement],
    index: &DefIdVarRefIndex,
    known_variables: &HashSet<String>,
    owner: Option<&str>,
) {
    let mut rewriter = DefIdVarRefCanonicalizer {
        index,
        known_variables,
        owner,
    };
    for statement in statements {
        *statement = rewriter.rewrite_statement(statement);
    }
}

fn canonicalize_def_id_when_equations(
    equations: &mut [flat::WhenEquation],
    index: &DefIdVarRefIndex,
    known_variables: &HashSet<String>,
) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                canonicalize_def_id_expr(value, index, known_variables, None);
            }
            flat::WhenEquation::Assert {
                condition, message, ..
            } => {
                canonicalize_def_id_expr(condition, index, known_variables, None);
                canonicalize_def_id_expr(message, index, known_variables, None);
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, branch_equations) in branches {
                    canonicalize_def_id_expr(condition, index, known_variables, None);
                    canonicalize_def_id_when_equations(branch_equations, index, known_variables);
                }
                canonicalize_def_id_when_equations(else_branch, index, known_variables);
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                canonicalize_def_id_expr(function, index, known_variables, None);
            }
            flat::WhenEquation::Terminate { message, .. } => {
                canonicalize_def_id_expr(message, index, known_variables, None);
            }
        }
    }
}

struct DefIdVarRefCanonicalizer<'a> {
    index: &'a DefIdVarRefIndex,
    known_variables: &'a HashSet<String>,
    owner: Option<&'a str>,
}

impl ExpressionRewriter for DefIdVarRefCanonicalizer<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let rewritten_name = self
            .index
            .resolve(name, self.owner, self.known_variables)
            .map(|selected| name.with_var_name(rumoca_core::VarName::new(selected)))
            .unwrap_or_else(|| name.clone());
        rumoca_core::Expression::VarRef {
            name: rewritten_name,
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }
}

impl StatementRewriter for DefIdVarRefCanonicalizer<'_> {}
