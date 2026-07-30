//! Collapse structured reference trees onto the flat variables the model owns.
//!
//! Instantiation leaves a reference such as `comp[1].port_p.Phi` as an
//! `Index`/`FieldAccess` tree, while the flat model owns one scalarized
//! variable per leaf (plus a record instance for each scalarized record base).
//! This pass rewrites every such tree to the single `VarRef` that names the
//! flat variable, folding compile-time subscripts on the way (MLS §4.5).

use super::*;
use rumoca_core::{ExpressionRewriter, StatementRewriter};

pub(crate) fn collapse_index_refs_to_known_varrefs(flat: &mut flat::Model) {
    let known_flat_vars = KnownFlatVars::build(flat);

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
        if let Some(binding) = &mut var.binding {
            collapse_index_expr(binding, &known_flat_vars);
        }
        if let Some(start) = &mut var.start {
            collapse_index_expr(start, &known_flat_vars);
        }
        if let Some(min) = &mut var.min {
            collapse_index_expr(min, &known_flat_vars);
        }
        if let Some(max) = &mut var.max {
            collapse_index_expr(max, &known_flat_vars);
        }
        if let Some(nominal) = &mut var.nominal {
            collapse_index_expr(nominal, &known_flat_vars);
        }
    }

    for chain in &mut flat.when_chains {
        for branch in chain.branches_mut() {
            collapse_index_expr(&mut branch.condition, &known_flat_vars);
            collapse_index_when_equations(&mut branch.equations, &known_flat_vars);
        }
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

fn collapse_index_when_equations(
    equations: &mut [rumoca_ir_flat::WhenEquation],
    known_flat_vars: &KnownFlatVars,
) {
    for equation in equations {
        match equation {
            rumoca_ir_flat::WhenEquation::Assign { value, .. }
            | rumoca_ir_flat::WhenEquation::Reinit { value, .. } => {
                collapse_index_expr(value, known_flat_vars);
            }
            rumoca_ir_flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                collapse_index_expr(condition, known_flat_vars);
                collapse_index_expr(message, known_flat_vars);
                if let Some(level) = level {
                    collapse_index_expr(level, known_flat_vars);
                }
            }
            rumoca_ir_flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (cond, branch_equations) in branches {
                    collapse_index_expr(cond, known_flat_vars);
                    collapse_index_when_equations(branch_equations, known_flat_vars);
                }
                if let Some(else_branch) = else_branch {
                    collapse_index_when_equations(else_branch, known_flat_vars);
                }
            }
            rumoca_ir_flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                collapse_index_expr(function, known_flat_vars);
            }
            rumoca_ir_flat::WhenEquation::Terminate { message, .. } => {
                collapse_index_expr(message, known_flat_vars)
            }
        }
    }
}

fn collapse_index_statements(
    statements: &mut [rumoca_core::Statement],
    known_flat_vars: &KnownFlatVars,
) {
    for statement in statements {
        *statement = CollapseIndexRewriter { known_flat_vars }.rewrite_statement(statement);
    }
}

fn collapse_index_expr(expr: &mut rumoca_core::Expression, known_flat_vars: &KnownFlatVars) {
    *expr = CollapseIndexRewriter { known_flat_vars }.rewrite_expression(expr);
}

/// Flat variable lookup for the collapse pass: exact names plus enough
/// structure to recover a scalarized record base (`comp[1].port_p.Phi` whose
/// only flat variables are the `.re`/`.im` leaves).
struct KnownFlatVars {
    names: rustc_hash::FxHashMap<rumoca_core::VarNameId, rumoca_core::Reference>,
    record_instances: rustc_hash::FxHashMap<rumoca_core::VarNameId, rumoca_core::Reference>,
    /// Compile-time integer values of `constant`/`parameter` variables, used to
    /// fold subscripts that are written as a symbolic reference (MLS §4.5
    /// requires array subscripts to be evaluable at compile time).
    integer_values: rustc_hash::FxHashMap<String, i64>,
}

impl KnownFlatVars {
    fn build(flat: &flat::Model) -> Self {
        let names = flat
            .variables
            .iter()
            .filter_map(|(name, var)| {
                let component_ref = var.component_ref.clone()?;
                Some((
                    name.id(),
                    rumoca_core::Reference::with_component_reference(name.as_str(), component_ref)
                        .with_instance_id(var.instance_id),
                ))
            })
            .collect();
        let record_instances = flat
            .record_instances
            .iter()
            .map(|(name, record)| {
                (
                    name.id(),
                    rumoca_core::Reference::with_component_reference(
                        name.as_str(),
                        record.component_ref.clone(),
                    )
                    .with_instance_id(record.instance_id),
                )
            })
            .collect();
        let integer_values = flat
            .variables
            .iter()
            .filter_map(|(name, var)| {
                rumoca_eval_flat::flat_int::structural_integer_value(var, flat)
                    .map(|value| (name.as_str().to_string(), value))
            })
            .collect();
        Self {
            names,
            record_instances,
            integer_values,
        }
    }

    fn expression(&self, name: &str, span: rumoca_core::Span) -> Option<rumoca_core::Expression> {
        let name = rumoca_core::VarName::new(name);
        let reference = self.names.get(&name.id())?;
        Some(rumoca_core::Expression::VarRef {
            name: reference.with_var_name(name),
            subscripts: Vec::new(),
            span,
        })
    }

    /// Compile-time integer value of a `constant`/`parameter` flat variable.
    fn integer_value(&self, name: &str) -> Option<i64> {
        self.integer_values.get(name).copied()
    }

    /// Exact structured occurrence for a scalarized record base.
    fn record_base_expression(
        &self,
        path: &str,
        span: rumoca_core::Span,
    ) -> Option<rumoca_core::Expression> {
        let path = rumoca_core::VarName::new(path);
        let reference = self.record_instances.get(&path.id())?;
        Some(rumoca_core::Expression::VarRef {
            name: reference.with_var_name(path),
            subscripts: Vec::new(),
            span,
        })
    }
}

struct CollapseIndexRewriter<'a> {
    known_flat_vars: &'a KnownFlatVars,
}

impl ExpressionRewriter for CollapseIndexRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let rumoca_core::Expression::FieldAccess {
            base,
            field,
            field_def_id,
            span,
        } = expr
        {
            let base = self.rewrite_expression(base);
            if let Some(collapsed) =
                collapse_field_access_to_known_var(&base, field, *span, self.known_flat_vars)
            {
                return collapsed;
            }
            return rumoca_core::Expression::FieldAccess {
                base: Box::new(base),
                field: field.clone(),
                field_def_id: *field_def_id,
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
            if let Some(collapsed) =
                collapse_indexed_expression(&base, &subscripts, *span, self.known_flat_vars)
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

/// Collapse `<base>[i...]` onto a known flat variable, for whichever shape the
/// already-rewritten base has.
fn collapse_indexed_expression(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    known_flat_vars: &KnownFlatVars,
) -> Option<rumoca_core::Expression> {
    if let rumoca_core::Expression::VarRef {
        name,
        subscripts: base_subscripts,
        ..
    } = base
    {
        return collapse_indexed_var_ref_to_known_var(
            name,
            base_subscripts,
            subscripts,
            span,
            known_flat_vars,
        );
    }
    collapse_indexed_field_access_to_known_var(base, subscripts, span, known_flat_vars)
}

/// Collapse `<field-access chain>[i]` onto a known flat variable.
///
/// A component that is itself an array element keeps a subscripted part in the
/// middle of its path (`plugToPins_p.plugToPin_p[1].plug_p.pin`), so flatten
/// leaves the reference as a field-access chain rather than a single dotted
/// `VarRef`. Rendering the chain to its flat path lets the same
/// known-variable/record-base collapse apply.
fn collapse_indexed_field_access_to_known_var(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    known_flat_vars: &KnownFlatVars,
) -> Option<rumoca_core::Expression> {
    let base_path = rumoca_core::flat_expression_component_path(base)?.to_flat_string();
    let candidate = format!(
        "{base_path}{}",
        subscript_suffix(subscripts, known_flat_vars)?
    );
    if let Some(expression) = known_flat_vars.expression(candidate.as_str(), span) {
        return Some(expression);
    }
    known_flat_vars.record_base_expression(&candidate, span)
}

fn collapse_indexed_var_ref_to_known_var(
    name: &rumoca_core::Reference,
    base_subscripts: &[rumoca_core::Subscript],
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    known_flat_vars: &KnownFlatVars,
) -> Option<rumoca_core::Expression> {
    let mut merged = base_subscripts.to_vec();
    merged.extend_from_slice(subscripts);
    if let Some(suffix) = subscript_suffix(&merged, known_flat_vars) {
        let candidate = format!("{}{}", name.as_str(), suffix);
        if let Some(expression) = known_flat_vars.expression(candidate.as_str(), span) {
            return Some(expression);
        }
        // Element of a scalarized record array (`r[2]` whose flat variables
        // are the field leaves `r[2].a`...): same record-base collapse as for
        // field accesses.
        if let Some(expression) = known_flat_vars.record_base_expression(&candidate, span) {
            return Some(expression);
        }
    }
    if known_flat_vars.names.contains_key(&name.var_name().id()) {
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
    known_flat_vars: &KnownFlatVars,
) -> Option<rumoca_core::Expression> {
    if let Some(candidate) = field_access_flat_path(base, field) {
        if let Some(expression) = known_flat_vars.expression(candidate.as_str(), span) {
            return Some(expression);
        }
        // Scalarized record base (`comp[1].port_p.Phi` where only the
        // `.re`/`.im` leaves exist as flat variables): collapse to a single
        // structured VarRef so downstream record-equation expansion sees the
        // record reference instead of an Index/FieldAccess tree it cannot
        // match (and shape inference does not inflate the equation to the
        // whole component array).
        if let Some(expression) = known_flat_vars.record_base_expression(&candidate, span) {
            return Some(expression);
        }
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

pub(crate) fn field_access_flat_path(
    base: &rumoca_core::Expression,
    field: &str,
) -> Option<String> {
    Some(format!(
        "{}.{field}",
        rumoca_core::flat_expression_component_path(base)?.to_flat_string()
    ))
}

fn collapse_var_field_access(
    base_name: &str,
    subscripts: &[rumoca_core::Subscript],
    field: &str,
    span: rumoca_core::Span,
    known_flat_vars: &KnownFlatVars,
) -> Option<rumoca_core::Expression> {
    let subscript_suffix = subscript_suffix(subscripts, known_flat_vars)?;
    for candidate in [
        format!("{base_name}{subscript_suffix}.{field}"),
        format!("{base_name}.{field}{subscript_suffix}"),
    ] {
        if let Some(expression) = known_flat_vars.expression(candidate.as_str(), span) {
            return Some(expression);
        }
    }
    None
}

fn subscript_suffix(
    subscripts: &[rumoca_core::Subscript],
    known_flat_vars: &KnownFlatVars,
) -> Option<String> {
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
                let value = fold_subscript_expr(expr, known_flat_vars, 0)?;
                values.push(value.to_string());
            }
            rumoca_core::Subscript::Colon { .. } => return None,
        }
    }
    Some(format!("[{}]", values.join(",")))
}

/// Maximum expression depth folded while resolving one subscript.
const MAX_SUBSCRIPT_FOLD_DEPTH: u8 = 8;

/// Fold a subscript expression to its compile-time integer value.
///
/// MLS §4.5 requires an array subscript to be evaluable at compile time, so a
/// subscript is either an integer literal, a reference to a `constant` or
/// `parameter` with a known binding, or arithmetic over those. Anything else
/// (a discrete/continuous variable, an unbound parameter, a `for` index that
/// survived expansion) yields `None`, leaving the reference untouched.
fn fold_subscript_expr(
    expr: &rumoca_core::Expression,
    known_flat_vars: &KnownFlatVars,
    depth: u8,
) -> Option<i64> {
    if depth > MAX_SUBSCRIPT_FOLD_DEPTH {
        return None;
    }
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => Some(*value),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => known_flat_vars.integer_value(name.as_str()),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = fold_subscript_expr(lhs, known_flat_vars, depth + 1)?;
            let rhs = fold_subscript_expr(rhs, known_flat_vars, depth + 1)?;
            rumoca_eval_flat::flat_int::eval_binary_op_i64(op, lhs, rhs)
        }
        _ => None,
    }
}
