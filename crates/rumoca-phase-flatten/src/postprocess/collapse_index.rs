use std::collections::HashSet;

use rumoca_core::{ExpressionRewriter, StatementRewriter};
use rumoca_ir_flat as flat;

pub(crate) fn collapse_index_refs_to_known_varrefs(flat: &mut flat::Model) {
    let known_flat_vars: HashSet<String> = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();

    for eq in &mut flat.equations {
        collapse_index_expr(&mut eq.residual, &known_flat_vars);
    }
    for eq in &mut flat.initial_equations {
        collapse_index_expr(&mut eq.residual, &known_flat_vars);
    }
    for assert_eq in &mut flat.assert_equations {
        collapse_index_expr(&mut assert_eq.condition, &known_flat_vars);
        collapse_index_expr(&mut assert_eq.message, &known_flat_vars);
        if let Some(level) = &mut assert_eq.level {
            collapse_index_expr(level, &known_flat_vars);
        }
    }
    for assert_eq in &mut flat.initial_assert_equations {
        collapse_index_expr(&mut assert_eq.condition, &known_flat_vars);
        collapse_index_expr(&mut assert_eq.message, &known_flat_vars);
        if let Some(level) = &mut assert_eq.level {
            collapse_index_expr(level, &known_flat_vars);
        }
    }

    for var in flat.variables.values_mut() {
        collapse_index_variable(var, &known_flat_vars);
    }

    for when_clause in &mut flat.when_clauses {
        collapse_index_expr(&mut when_clause.condition, &known_flat_vars);
        collapse_index_when_equations(&mut when_clause.equations, &known_flat_vars);
    }

    for algorithm in &mut flat.algorithms {
        collapse_index_statements(&mut algorithm.statements, &known_flat_vars);
    }
    for algorithm in &mut flat.initial_algorithms {
        collapse_index_statements(&mut algorithm.statements, &known_flat_vars);
    }

    for function in flat.functions.values_mut() {
        for input in &mut function.inputs {
            if let Some(default) = &mut input.default {
                collapse_index_expr(default, &known_flat_vars);
            }
        }
        for output in &mut function.outputs {
            if let Some(default) = &mut output.default {
                collapse_index_expr(default, &known_flat_vars);
            }
        }
        for local in &mut function.locals {
            if let Some(default) = &mut local.default {
                collapse_index_expr(default, &known_flat_vars);
            }
        }
        collapse_index_statements(&mut function.body, &known_flat_vars);
    }
}

fn collapse_index_variable(var: &mut flat::Variable, known_flat_vars: &HashSet<String>) {
    if let Some(binding) = &mut var.binding {
        collapse_index_expr(binding, known_flat_vars);
    }
    if let Some(start) = &mut var.start {
        collapse_index_expr(start, known_flat_vars);
    }
    if let Some(min) = &mut var.min {
        collapse_index_expr(min, known_flat_vars);
    }
    if let Some(max) = &mut var.max {
        collapse_index_expr(max, known_flat_vars);
    }
    if let Some(nominal) = &mut var.nominal {
        collapse_index_expr(nominal, known_flat_vars);
    }
}

fn collapse_index_when_equations(
    equations: &mut [flat::WhenEquation],
    known_flat_vars: &HashSet<String>,
) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                collapse_index_expr(value, known_flat_vars);
            }
            flat::WhenEquation::Assert { condition, .. } => {
                collapse_index_expr(condition, known_flat_vars);
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (cond, branch_equations) in branches {
                    collapse_index_expr(cond, known_flat_vars);
                    collapse_index_when_equations(branch_equations, known_flat_vars);
                }
                collapse_index_when_equations(else_branch, known_flat_vars);
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                collapse_index_expr(function, known_flat_vars);
            }
            flat::WhenEquation::Terminate { .. } => {}
        }
    }
}

fn collapse_index_statements(
    statements: &mut [rumoca_core::Statement],
    known_flat_vars: &HashSet<String>,
) {
    for statement in statements {
        *statement = CollapseIndexRewriter { known_flat_vars }.rewrite_statement(statement);
    }
}

fn collapse_index_expr(expr: &mut rumoca_core::Expression, known_flat_vars: &HashSet<String>) {
    *expr = CollapseIndexRewriter { known_flat_vars }.rewrite_expression(expr);
}

struct CollapseIndexRewriter<'a> {
    known_flat_vars: &'a HashSet<String>,
}

impl ExpressionRewriter for CollapseIndexRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let rumoca_core::Expression::FieldAccess { base, field, span } = expr {
            let base = self.rewrite_expression(base);
            if let Some(collapsed) =
                collapse_field_access_to_known_var(&base, field, *span, self.known_flat_vars)
            {
                return collapsed;
            }
            return rumoca_core::Expression::FieldAccess {
                base: Box::new(base),
                field: field.clone(),
                span: *span,
            };
        }
        if let rumoca_core::Expression::Index {
            base,
            subscripts,
            span,
        } = expr
        {
            let base = self.rewrite_expression(base);
            let subscripts = self.rewrite_subscripts(subscripts);
            if let rumoca_core::Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = &base
                && let Some(collapsed) = collapse_indexed_var_ref_to_known_var(
                    name,
                    base_subscripts,
                    &subscripts,
                    *span,
                    self.known_flat_vars,
                )
            {
                return collapsed;
            }
            return rumoca_core::Expression::Index {
                base: Box::new(base),
                subscripts,
                span: *span,
            };
        }
        self.walk_expression(expr)
    }
}

impl StatementRewriter for CollapseIndexRewriter<'_> {}

fn collapse_indexed_var_ref_to_known_var(
    name: &rumoca_core::Reference,
    base_subscripts: &[rumoca_core::Subscript],
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    known_flat_vars: &HashSet<String>,
) -> Option<rumoca_core::Expression> {
    let mut merged = base_subscripts.to_vec();
    merged.extend_from_slice(subscripts);
    if let Some(suffix) = subscript_suffix(&merged) {
        let candidate = format!("{}{}", name.as_str(), suffix);
        if known_flat_vars.contains(candidate.as_str()) {
            return Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(candidate),
                subscripts: vec![],
                span,
            });
        }
    }
    if known_flat_vars.contains(name.as_str()) {
        return Some(rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: merged,
            span,
        });
    }
    None
}

fn collapse_field_access_to_known_var(
    base: &rumoca_core::Expression,
    field: &str,
    span: rumoca_core::Span,
    known_flat_vars: &HashSet<String>,
) -> Option<rumoca_core::Expression> {
    if let Some(candidate) = field_access_flat_path(base, field)
        && known_flat_vars.contains(candidate.as_str())
    {
        return Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(candidate),
            subscripts: vec![],
            span,
        });
    }

    match base {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => collapse_var_field_access(name.as_str(), subscripts, field, span, known_flat_vars),
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
            let mut merged = base_subscripts.clone();
            merged.extend_from_slice(subscripts);
            collapse_var_field_access(name.as_str(), &merged, field, span, known_flat_vars)
        }
        _ => None,
    }
}

fn field_access_flat_path(base: &rumoca_core::Expression, field: &str) -> Option<String> {
    Some(format!("{}.{}", expr_flat_path(base)?, field))
}

fn expr_flat_path(expr: &rumoca_core::Expression) -> Option<String> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => Some(format!(
            "{}{}",
            name.as_str(),
            subscript_suffix(subscripts)?
        )),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => Some(format!(
            "{}{}",
            expr_flat_path(base)?,
            subscript_suffix(subscripts)?
        )),
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            Some(format!("{}.{}", expr_flat_path(base)?, field))
        }
        _ => None,
    }
}

fn collapse_var_field_access(
    base_name: &str,
    subscripts: &[rumoca_core::Subscript],
    field: &str,
    span: rumoca_core::Span,
    known_flat_vars: &HashSet<String>,
) -> Option<rumoca_core::Expression> {
    let subscript_suffix = subscript_suffix(subscripts)?;
    for candidate in field_access_index_candidates(base_name, subscript_suffix.as_str(), field) {
        if known_flat_vars.contains(candidate.as_str()) {
            return Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(candidate),
                subscripts: vec![],
                span,
            });
        }
    }
    None
}

fn field_access_index_candidates(
    base_name: &str,
    subscript_suffix: &str,
    field: &str,
) -> indexmap::IndexSet<String> {
    let path = rumoca_core::ComponentPath::from_flat_path(base_name);
    let parts = path.parts();
    let mut candidates = indexmap::IndexSet::with_capacity(parts.len() + 2);

    let mut base_index_parts = parts.to_vec();
    if let Some(last) = base_index_parts.last_mut() {
        append_subscript_suffix(last, subscript_suffix);
        base_index_parts.push(field.to_string());
        insert_component_path_candidate(&mut candidates, base_index_parts);
    }

    let mut field_index_parts = parts.to_vec();
    field_index_parts.push(format!("{field}{subscript_suffix}"));
    insert_component_path_candidate(&mut candidates, field_index_parts);

    if !subscript_suffix.is_empty() {
        for part_index in 0..parts.len().saturating_sub(1) {
            let mut projected_parts = parts.to_vec();
            append_subscript_suffix(&mut projected_parts[part_index], subscript_suffix);
            projected_parts.push(field.to_string());
            insert_component_path_candidate(&mut candidates, projected_parts);
        }
    }

    candidates
}

fn append_subscript_suffix(part: &mut String, subscript_suffix: &str) {
    part.push_str(subscript_suffix);
}

fn insert_component_path_candidate(
    candidates: &mut indexmap::IndexSet<String>,
    parts: Vec<String>,
) {
    candidates.insert(rumoca_core::ComponentPath::from_parts(parts).to_flat_string());
}

fn subscript_suffix(subscripts: &[rumoca_core::Subscript]) -> Option<String> {
    if subscripts.is_empty() {
        return Some(String::new());
    }
    let mut values = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        match subscript {
            rumoca_core::Subscript::Index { value, .. } => {
                values.push(value.to_string());
            }
            rumoca_core::Subscript::Expr { expr, .. } => {
                let rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(value),
                    ..
                } = expr.as_ref()
                else {
                    return None;
                };
                values.push(value.to_string());
            }
            rumoca_core::Subscript::Colon { .. } => return None,
        }
    }
    Some(format!("[{}]", values.join(",")))
}
