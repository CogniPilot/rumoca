// SPEC_0021 file-size exception: postprocess still coordinates constant
// substitution, annotations, and scoped parameter preservation. split plan:
// move annotation substitution and scoped parameter rewrites into submodules.
use super::*;
use def_id::{aggregate_projection_needs_alias_protection, aggregate_projection_ref};
use rumoca_core::{
    ExpressionRewriter, FallibleExpressionRewriter, FallibleStatementRewriter, StatementRewriter,
};
use std::collections::HashMap;
#[path = "postprocess_record_alias.rs"]
mod record_alias;
use record_alias::*;
pub(super) fn canonicalize_varrefs_via_record_aliases(flat: &mut flat::Model, ctx: &Context) {
    let known_variables: HashSet<String> = flat.variables.keys().map(ToString::to_string).collect();
    for var in flat.variables.values_mut() {
        let owner = rumoca_core::ComponentPath::from_flat_path(var.name.as_str());
        canonicalize_record_alias_opt_expr_in_owner(
            &mut var.binding,
            ctx,
            &known_variables,
            &owner,
        );
        canonicalize_record_alias_opt_expr_in_owner(&mut var.start, ctx, &known_variables, &owner);
        canonicalize_record_alias_opt_expr_in_owner(&mut var.min, ctx, &known_variables, &owner);
        canonicalize_record_alias_opt_expr_in_owner(&mut var.max, ctx, &known_variables, &owner);
        canonicalize_record_alias_opt_expr_in_owner(
            &mut var.nominal,
            ctx,
            &known_variables,
            &owner,
        );
    }
    for equation in &mut flat.equations {
        canonicalize_record_alias_expr(&mut equation.residual, ctx, &known_variables);
    }
    for equation in &mut flat.initial_equations {
        canonicalize_record_alias_expr(&mut equation.residual, ctx, &known_variables);
    }
    for when_clause in &mut flat.when_clauses {
        canonicalize_record_alias_expr(&mut when_clause.condition, ctx, &known_variables);
        canonicalize_record_alias_when_equations(&mut when_clause.equations, ctx, &known_variables);
    }
    for algorithm in &mut flat.algorithms {
        canonicalize_record_alias_statements(&mut algorithm.statements, ctx, &known_variables);
    }
    for algorithm in &mut flat.initial_algorithms {
        canonicalize_record_alias_statements(&mut algorithm.statements, ctx, &known_variables);
    }
}
#[path = "postprocess_def_id.rs"]
mod def_id;
pub(crate) use def_id::canonicalize_varrefs_via_instantiated_def_ids;
#[path = "postprocess_field_access.rs"]
mod field_access;
pub(super) use field_access::{
    drop_invalid_field_access_bindings, resolve_nested_constructor_field_access_bindings,
};
fn record_alias_rewrite_name(
    name: &str,
    ctx: &Context,
    known_variables: &HashSet<String>,
    owner: Option<&rumoca_core::ComponentPath>,
) -> Option<String> {
    record_alias::rewrite_name(name, ctx, known_variables, owner)
}
pub(super) fn mark_record_constructor_calls(flat: &mut flat::Model, tree: &ast::ClassTree) {
    let constructor_def_ids = tree
        .def_map
        .keys()
        .copied()
        .filter(|def_id| {
            tree.get_class_by_def_id(*def_id)
                .is_some_and(|class_def| class_def.class_type == rumoca_core::ClassType::Record)
        })
        .collect::<HashSet<_>>();
    let mut constructor_names: HashSet<String> = tree
        .def_map
        .iter()
        .filter(|&(def_id, _qualified_name)| constructor_def_ids.contains(def_id))
        .map(|(_def_id, qualified_name)| qualified_name.clone())
        .collect();
    constructor_names.extend(
        flat.functions
            .values()
            .filter(|function| function.is_constructor)
            .map(|function| function.name.as_str().to_string()),
    );
    if constructor_names.is_empty() && constructor_def_ids.is_empty() {
        return;
    }
    let marker = ConstructorMarker {
        constructor_names: &constructor_names,
        constructor_def_ids: &constructor_def_ids,
    };
    for var in flat.variables.values_mut() {
        marker.mark_opt_expr(&mut var.binding);
        marker.mark_opt_expr(&mut var.start);
        marker.mark_opt_expr(&mut var.min);
        marker.mark_opt_expr(&mut var.max);
        marker.mark_opt_expr(&mut var.nominal);
    }
    for eq in &mut flat.equations {
        marker.mark_expr(&mut eq.residual);
    }
    for eq in &mut flat.initial_equations {
        marker.mark_expr(&mut eq.residual);
    }
    for assert_eq in &mut flat.assert_equations {
        marker.mark_expr(&mut assert_eq.condition);
        marker.mark_expr(&mut assert_eq.message);
        marker.mark_opt_expr(&mut assert_eq.level);
    }
    for assert_eq in &mut flat.initial_assert_equations {
        marker.mark_expr(&mut assert_eq.condition);
        marker.mark_expr(&mut assert_eq.message);
        marker.mark_opt_expr(&mut assert_eq.level);
    }
    for algorithm in &mut flat.algorithms {
        marker.mark_statements(&mut algorithm.statements);
    }
    for algorithm in &mut flat.initial_algorithms {
        marker.mark_statements(&mut algorithm.statements);
    }
    for when_clause in &mut flat.when_clauses {
        marker.mark_expr(&mut when_clause.condition);
        marker.mark_when_equations(&mut when_clause.equations);
    }
    for function in flat.functions.values_mut() {
        for input in &mut function.inputs {
            marker.mark_opt_expr(&mut input.default);
        }
        for output in &mut function.outputs {
            marker.mark_opt_expr(&mut output.default);
        }
        for local in &mut function.locals {
            marker.mark_opt_expr(&mut local.default);
        }
        marker.mark_statements(&mut function.body);
    }
}
#[derive(Clone, Copy)]
struct ConstructorMarker<'a> {
    constructor_names: &'a HashSet<String>,
    constructor_def_ids: &'a HashSet<rumoca_core::DefId>,
}
impl ConstructorMarker<'_> {
    fn mark_opt_expr(self, expr: &mut Option<rumoca_core::Expression>) {
        if let Some(expr) = expr {
            self.mark_expr(expr);
        }
    }
    fn mark_expr(mut self, expr: &mut rumoca_core::Expression) {
        *expr = self.rewrite_expression(expr);
    }
    fn mark_statements(mut self, statements: &mut [rumoca_core::Statement]) {
        for statement in statements {
            *statement = self.rewrite_statement(statement);
        }
    }
    fn mark_when_equations(self, equations: &mut [rumoca_ir_flat::WhenEquation]) {
        for equation in equations {
            match equation {
                rumoca_ir_flat::WhenEquation::Assign { value, .. }
                | rumoca_ir_flat::WhenEquation::Reinit { value, .. } => self.mark_expr(value),
                rumoca_ir_flat::WhenEquation::Assert {
                    condition, message, ..
                } => {
                    self.mark_expr(condition);
                    self.mark_expr(message);
                }
                rumoca_ir_flat::WhenEquation::Conditional {
                    branches,
                    else_branch,
                    ..
                } => self.mark_conditional_when_equation(branches, else_branch),
                rumoca_ir_flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                    self.mark_expr(function);
                }
                rumoca_ir_flat::WhenEquation::Terminate { message, .. } => self.mark_expr(message),
            }
        }
    }
    fn mark_conditional_when_equation(
        self,
        branches: &mut [(rumoca_core::Expression, Vec<rumoca_ir_flat::WhenEquation>)],
        else_branch: &mut [rumoca_ir_flat::WhenEquation],
    ) {
        for (condition, branch_equations) in branches {
            self.mark_expr(condition);
            self.mark_when_equations(branch_equations);
        }
        self.mark_when_equations(else_branch);
    }
    fn is_constructor_call(self, name: &rumoca_core::Reference) -> bool {
        name.target_def_id()
            .is_some_and(|def_id| self.constructor_def_ids.contains(&def_id))
            || self.constructor_names.contains(name.as_str())
    }
}
impl ExpressionRewriter for ConstructorMarker<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expr
        {
            return rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args: self.rewrite_expressions(args),
                is_constructor: *is_constructor || self.is_constructor_call(name),
                span: *span,
            };
        }
        self.walk_expression(expr)
    }
}
impl StatementRewriter for ConstructorMarker<'_> {}

pub(super) fn collapse_index_refs_to_known_varrefs(flat: &mut flat::Model) {
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

pub(super) fn recover_indexed_lhs_dimensions(flat: &mut flat::Model) {
    let mut recovered: Vec<(rumoca_core::VarName, Vec<i64>)> = Vec::new();
    for equation in &flat.equations {
        let Some((name, dims)) = indexed_lhs_dimensions(&equation.residual) else {
            continue;
        };
        merge_recovered_dims(&mut recovered, name, dims);
    }
    recover_whole_var_equality_dimensions(flat, &mut recovered);

    for (name, dims) in recovered {
        let Some(var) = flat.variables.get_mut(&name) else {
            continue;
        };
        if should_replace_dims(&var.dims, &dims) {
            var.dims = dims;
        }
        if matches!(
            &var.variability,
            rumoca_core::Variability::Parameter(token) if token.text.is_empty()
        ) && var.binding.is_none()
        {
            var.variability = rumoca_core::Variability::Empty;
        }
    }
}

fn indexed_lhs_dimensions(
    residual: &rumoca_core::Expression,
) -> Option<(rumoca_core::VarName, Vec<i64>)> {
    let rumoca_core::Expression::Binary { op, lhs, .. } = residual else {
        return None;
    };
    if !matches!(op, rumoca_core::OpBinary::Sub) {
        return None;
    }
    let (name, subscripts) = indexed_var_ref(lhs.as_ref())?;
    let dims = subscript_upper_bounds(subscripts)?;
    Some((name.var_name().clone(), dims))
}

fn indexed_var_ref(
    expr: &rumoca_core::Expression,
) -> Option<(&rumoca_core::Reference, &[rumoca_core::Subscript])> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if !subscripts.is_empty() => Some((name, subscripts)),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let rumoca_core::Expression::VarRef { name, .. } = base.as_ref() else {
                return None;
            };
            Some((name, subscripts))
        }
        _ => None,
    }
}

fn subscript_upper_bounds(subscripts: &[rumoca_core::Subscript]) -> Option<Vec<i64>> {
    let mut dims = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        let value = match subscript {
            rumoca_core::Subscript::Index { value, .. } => *value,
            rumoca_core::Subscript::Expr { expr, .. } => constant_integer_bound(expr)?,
            rumoca_core::Subscript::Colon { .. } => return None,
        };
        if value <= 0 {
            return None;
        }
        dims.push(value);
    }
    Some(dims)
}

fn constant_integer_bound(expr: &rumoca_core::Expression) -> Option<i64> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => Some(*value),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args,
            ..
        } => size_call_bound(args),
        _ => None,
    }
}

fn size_call_bound(args: &[rumoca_core::Expression]) -> Option<i64> {
    let [array, dim] = args else {
        return None;
    };
    if constant_integer_bound(dim)? != 1 {
        return None;
    }
    let rumoca_core::Expression::Array { elements, .. } = array else {
        return None;
    };
    i64::try_from(elements.len()).ok()
}

fn recover_whole_var_equality_dimensions(
    flat: &flat::Model,
    recovered: &mut Vec<(rumoca_core::VarName, Vec<i64>)>,
) {
    for equation in &flat.equations {
        let Some((lhs, rhs)) = whole_var_equality(&equation.residual) else {
            continue;
        };
        let scalar_count = i64::try_from(equation.scalar_count).ok();
        if let Some(dim) = scalar_count.filter(|dim| *dim > 1)
            && whole_var_can_accept_1d_recovery(flat, lhs, rhs)
        {
            merge_recovered_dims(recovered, lhs.clone(), vec![dim]);
            merge_recovered_dims(recovered, rhs.clone(), vec![dim]);
        }
    }
}

fn whole_var_equality(
    residual: &rumoca_core::Expression,
) -> Option<(&rumoca_core::VarName, &rumoca_core::VarName)> {
    let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = residual else {
        return None;
    };
    if !matches!(op, rumoca_core::OpBinary::Sub) {
        return None;
    }
    let lhs = whole_var_ref(lhs.as_ref())?;
    let rhs = whole_var_ref(rhs.as_ref())?;
    Some((lhs, rhs))
}

fn whole_var_ref(expr: &rumoca_core::Expression) -> Option<&rumoca_core::VarName> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    subscripts.is_empty().then(|| name.var_name())
}

fn whole_var_can_accept_1d_recovery(
    flat: &flat::Model,
    lhs: &rumoca_core::VarName,
    rhs: &rumoca_core::VarName,
) -> bool {
    [lhs, rhs].into_iter().all(|name| {
        flat.variables
            .get(name)
            .is_some_and(|var| var.dims.len() <= 1)
    })
}

fn merge_recovered_dims(
    recovered: &mut Vec<(rumoca_core::VarName, Vec<i64>)>,
    name: rumoca_core::VarName,
    dims: Vec<i64>,
) {
    if let Some((_, existing)) = recovered
        .iter_mut()
        .find(|(candidate, _)| candidate == &name)
    {
        for (index, dim) in dims.into_iter().enumerate() {
            if index >= existing.len() {
                existing.push(dim);
            } else {
                existing[index] = existing[index].max(dim);
            }
        }
        return;
    }
    recovered.push((name, dims));
}

fn should_replace_dims(current: &[i64], recovered: &[i64]) -> bool {
    if recovered.is_empty() {
        return false;
    }
    current.len() < recovered.len()
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
                condition, message, ..
            } => {
                collapse_index_expr(condition, known_flat_vars);
                collapse_index_expr(message, known_flat_vars);
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
                collapse_index_when_equations(else_branch, known_flat_vars);
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
    names: std::collections::BTreeMap<String, Option<rumoca_core::ComponentReference>>,
    aggregate_projection_refs: HashSet<String>,
}

impl KnownFlatVars {
    fn build(flat: &flat::Model) -> Self {
        let mut aggregate_projection_counts: HashMap<String, usize> = HashMap::new();
        let names = flat
            .variables
            .iter()
            .map(|(name, var)| {
                if let Some(projection) = aggregate_projection_ref(name.as_str()) {
                    *aggregate_projection_counts.entry(projection).or_insert(0) += 1;
                }
                (name.as_str().to_string(), var.component_ref.clone())
            })
            .collect();
        let known_variable_names = flat
            .variables
            .keys()
            .map(|name| name.as_str().to_string())
            .collect::<HashSet<_>>();
        let aggregate_projection_refs = aggregate_projection_counts
            .into_iter()
            .filter_map(|(projection, count)| {
                (count > 1
                    && aggregate_projection_needs_alias_protection(
                        &projection,
                        &known_variable_names,
                    ))
                .then_some(projection)
            })
            .collect();
        Self {
            names,
            aggregate_projection_refs,
        }
    }

    fn contains(&self, name: &str) -> bool {
        self.names.contains_key(name)
    }

    fn is_aggregate_projection_ref(&self, name: &str) -> bool {
        self.aggregate_projection_refs.contains(name)
    }

    /// Structured reference for a scalarized record base: `path` names no flat
    /// variable itself, but at least one leaf variable renders as
    /// `path.<field>...`. The base reference is recovered by truncating that
    /// leaf's component reference to the parts that render exactly `path`, so
    /// part identity (spans, subscripts) is owned by the leaf metadata rather
    /// than re-parsed from text.
    fn record_base_reference(&self, path: &str) -> Option<rumoca_core::ComponentReference> {
        let prefix = format!("{path}.");
        let (leaf_name, leaf_ref) = self.names.range(prefix.clone()..).next()?;
        if !leaf_name.starts_with(&prefix) {
            return None;
        }
        let leaf_ref = leaf_ref.as_ref()?;
        for depth in (1..leaf_ref.parts.len()).rev() {
            let truncated = rumoca_core::ComponentReference {
                local: leaf_ref.local,
                span: leaf_ref.span,
                parts: leaf_ref.parts[..depth].to_vec(),
                def_id: None,
            };
            if truncated.to_var_name().as_str() == path {
                return Some(truncated);
            }
        }
        None
    }

    fn array_base_reference(&self, path: &str) -> Option<rumoca_core::ComponentReference> {
        let prefix = format!("{path}[");
        let (leaf_name, leaf_ref) = self.names.range(prefix.clone()..).next()?;
        if !leaf_name.starts_with(&prefix) {
            return None;
        }
        let span = leaf_ref.as_ref()?.span;
        Some(rumoca_core::ComponentReference::from_flat_segments(
            path, span, None,
        ))
    }
}

struct CollapseIndexRewriter<'a> {
    known_flat_vars: &'a KnownFlatVars,
}

impl ExpressionRewriter for CollapseIndexRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } = expr
            && subscripts.is_empty()
            && !self.known_flat_vars.contains(name.as_str())
            && !self
                .known_flat_vars
                .is_aggregate_projection_ref(name.as_str())
            && let Some(collapsed) = collapse_repeated_field_tail_to_known_var(
                name.as_str(),
                *span,
                self.known_flat_vars,
            )
        {
            return collapsed;
        }
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
    known_flat_vars: &KnownFlatVars,
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
        // Element of a scalarized record array (`r[2]` whose flat variables
        // are the field leaves `r[2].a`...): same record-base collapse as for
        // field accesses.
        if let Some(reference) = known_flat_vars.record_base_reference(&candidate) {
            return Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(&candidate, reference),
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
    known_flat_vars: &KnownFlatVars,
) -> Option<rumoca_core::Expression> {
    if let Some(candidate) = field_access_flat_path(base, field) {
        if known_flat_vars.contains(candidate.as_str()) {
            return Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(candidate),
                subscripts: vec![],
                span,
            });
        }
        if let Some(collapsed) =
            collapse_repeated_field_tail_to_known_var(&candidate, span, known_flat_vars)
        {
            return Some(collapsed);
        }
        // Scalarized record base (`comp[1].port_p.Phi` where only the
        // `.re`/`.im` leaves exist as flat variables): collapse to a single
        // structured VarRef so downstream record-equation expansion sees the
        // record reference instead of an Index/FieldAccess tree it cannot
        // match (and shape inference does not inflate the equation to the
        // whole component array).
        if let Some(reference) = known_flat_vars.record_base_reference(&candidate) {
            return Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(&candidate, reference),
                subscripts: vec![],
                span,
            });
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

fn collapse_repeated_field_tail_to_known_var(
    candidate: &str,
    span: rumoca_core::Span,
    known_flat_vars: &KnownFlatVars,
) -> Option<rumoca_core::Expression> {
    let mut path = candidate;
    while let Some((prefix, field)) = rendered_path_last_segment(path) {
        if !prefix.ends_with(field) {
            return collapse_penultimate_field_to_known_var(path, span, known_flat_vars);
        }
        path = prefix;
        if known_flat_vars.contains(path) {
            return Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(path.to_string()),
                subscripts: vec![],
                span,
            });
        }
        if let Some(alternate) = alternate_array_field_path(path)
            && known_flat_vars.contains(&alternate)
        {
            return Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(alternate),
                subscripts: vec![],
                span,
            });
        }
        if let Some(reference) = known_flat_vars.record_base_reference(path) {
            return Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(path, reference),
                subscripts: vec![],
                span,
            });
        }
        if let Some(reference) = known_flat_vars.array_base_reference(path) {
            return Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(path, reference),
                subscripts: vec![],
                span,
            });
        }
        if let Some(alternate) = alternate_array_field_path(path)
            && let Some(reference) = known_flat_vars.record_base_reference(&alternate)
        {
            return Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(&alternate, reference),
                subscripts: vec![],
                span,
            });
        }
        if let Some(alternate) = alternate_array_field_path(path)
            && let Some(reference) = known_flat_vars.array_base_reference(&alternate)
        {
            return Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(&alternate, reference),
                subscripts: vec![],
                span,
            });
        }
        if let Some(collapsed) =
            collapse_penultimate_field_to_known_var(path, span, known_flat_vars)
        {
            return Some(collapsed);
        }
    }
    None
}

fn collapse_penultimate_field_to_known_var(
    path: &str,
    span: rumoca_core::Span,
    known_flat_vars: &KnownFlatVars,
) -> Option<rumoca_core::Expression> {
    let (prefix, leaf) = rendered_path_last_segment(path)?;
    let (base, _) = rendered_path_last_segment(prefix)?;
    let candidate = format!("{base}.{leaf}");
    known_path_expression(&candidate, span, known_flat_vars)
}

fn known_path_expression(
    path: &str,
    span: rumoca_core::Span,
    known_flat_vars: &KnownFlatVars,
) -> Option<rumoca_core::Expression> {
    if known_flat_vars.contains(path) {
        return Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(path.to_string()),
            subscripts: vec![],
            span,
        });
    }
    if let Some(alternate) = alternate_array_field_path(path)
        && known_flat_vars.contains(&alternate)
    {
        return Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(alternate),
            subscripts: vec![],
            span,
        });
    }
    if let Some(reference) = known_flat_vars.record_base_reference(path) {
        return Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(path, reference),
            subscripts: vec![],
            span,
        });
    }
    if let Some(reference) = known_flat_vars.array_base_reference(path) {
        return Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(path, reference),
            subscripts: vec![],
            span,
        });
    }
    if let Some(alternate) = alternate_array_field_path(path)
        && let Some(reference) = known_flat_vars.record_base_reference(&alternate)
    {
        return Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(&alternate, reference),
            subscripts: vec![],
            span,
        });
    }
    if let Some(alternate) = alternate_array_field_path(path)
        && let Some(reference) = known_flat_vars.array_base_reference(&alternate)
    {
        return Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(&alternate, reference),
            subscripts: vec![],
            span,
        });
    }
    None
}

fn alternate_array_field_path(path: &str) -> Option<String> {
    let (base, field) = rendered_path_last_segment(path)?;
    if !base.ends_with(']') {
        return None;
    }
    let bracket_start = base.rfind('[')?;
    let (array_base, suffix) = base.split_at(bracket_start);
    Some(format!("{array_base}.{field}{suffix}"))
}

fn rendered_path_last_segment(path: &str) -> Option<(&str, &str)> {
    let mut bracket_depth = 0usize;
    for (idx, byte) in path.bytes().enumerate().rev() {
        match byte {
            b']' => bracket_depth += 1,
            b'[' => bracket_depth = bracket_depth.saturating_sub(1),
            b'.' if bracket_depth == 0 => return Some((&path[..idx], &path[idx + 1..])),
            _ => {}
        }
    }
    None
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
    known_flat_vars: &KnownFlatVars,
) -> Option<rumoca_core::Expression> {
    let subscript_suffix = subscript_suffix(subscripts)?;
    for candidate in [
        format!("{base_name}{subscript_suffix}.{field}"),
        format!("{base_name}.{field}{subscript_suffix}"),
    ] {
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

pub(super) fn substitute_known_constants_in_flat(
    flat: &mut flat::Model,
    ctx: &Context,
) -> Result<(), FlattenError> {
    let live_vars: rustc_hash::FxHashSet<String> = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();
    let var_dims: rustc_hash::FxHashMap<String, Vec<i64>> = flat
        .variables
        .iter()
        .filter(|(_, var)| !var.dims.is_empty())
        .map(|(name, var)| (name.as_str().to_string(), var.dims.clone()))
        .collect();
    evaluate_static_initial_parameter_algorithms(flat, ctx)?;
    let var_values = parameter_constant_var_values(flat);
    let binding_var_values = flat_binding_var_values(flat);
    let no_locals: HashSet<String> = HashSet::new();

    for eq in &mut flat.equations {
        let scope = equation_origin_scope(&eq.origin);
        eq.residual = substitute_known_constants_expr_with_options_dims_and_values(
            eq.residual.clone(),
            ctx,
            &live_vars,
            &no_locals,
            &scope,
            true,
            &var_dims,
            &var_values,
        )?;
        reconcile_residual_constructor_extents_with_lhs_dims(&mut eq.residual, &var_dims);
    }
    for eq in &mut flat.initial_equations {
        let scope = equation_origin_scope(&eq.origin);
        eq.residual = substitute_known_constants_expr_with_options_dims_and_values(
            eq.residual.clone(),
            ctx,
            &live_vars,
            &no_locals,
            &scope,
            true,
            &var_dims,
            &var_values,
        )?;
        reconcile_residual_constructor_extents_with_lhs_dims(&mut eq.residual, &var_dims);
    }
    substitute_structured_equation_templates(
        &mut flat.structured_equations,
        ctx,
        &live_vars,
        &no_locals,
        &var_dims,
        &var_values,
    )?;
    substitute_structured_equation_templates(
        &mut flat.initial_structured_equations,
        ctx,
        &live_vars,
        &no_locals,
        &var_dims,
        &var_values,
    )?;
    recover_primitive_constructor_parameter_bindings(flat);
    substitute_assert_equations(
        &mut flat.assert_equations,
        ctx,
        &live_vars,
        &no_locals,
        &var_dims,
        &var_values,
    )?;
    substitute_assert_equations(
        &mut flat.initial_assert_equations,
        ctx,
        &live_vars,
        &no_locals,
        &var_dims,
        &var_values,
    )?;
    for when_clause in &mut flat.when_clauses {
        when_clause.condition = substitute_known_constants_expr(
            when_clause.condition.clone(),
            ctx,
            &live_vars,
            &no_locals,
            "",
        )?;
        for equation in &mut when_clause.equations {
            substitute_known_constants_when_equation(equation, ctx, &live_vars, &no_locals)?;
        }
    }
    substitute_algorithms(&mut flat.algorithms, ctx, &live_vars, &no_locals)?;
    substitute_algorithms(&mut flat.initial_algorithms, ctx, &live_vars, &no_locals)?;
    substitute_variable_annotations(
        flat,
        ctx,
        &live_vars,
        &no_locals,
        &var_dims,
        &binding_var_values,
    )?;
    substitute_function_bodies(&mut flat.functions, ctx, &live_vars)?;
    crate::zero_sized_arrays::materialize_referenced_zero_sized_array_variables(flat, ctx)?;
    Ok(())
}

fn parameter_constant_var_values(
    flat: &flat::Model,
) -> rustc_hash::FxHashMap<String, rumoca_core::Expression> {
    flat.variables
        .iter()
        .filter_map(|(name, var)| {
            let expr = var.binding.as_ref().or_else(|| {
                (var.fixed != Some(false))
                    .then_some(())
                    .and(var.start.as_ref())
            })?;
            if !parameter_value_is_structural(flat, name, var, expr) {
                return None;
            }
            Some((name.as_str().to_string(), expr.clone()))
        })
        .collect()
}

fn flat_binding_var_values(
    flat: &flat::Model,
) -> rustc_hash::FxHashMap<String, rumoca_core::Expression> {
    flat.variables
        .iter()
        .filter_map(|(name, var)| {
            let expr = var.binding.as_ref().or_else(|| {
                (var.fixed != Some(false))
                    .then_some(())
                    .and(var.start.as_ref())
            })?;
            if parameter_value_is_structural(flat, name, var, expr)
                || structural_non_real_expr(expr)
                    && !variable_type_is_real(flat.variable_type_names.get(name))
            {
                return Some((name.as_str().to_string(), expr.clone()));
            }
            None
        })
        .collect()
}

fn parameter_value_is_structural(
    flat: &flat::Model,
    name: &rumoca_core::VarName,
    var: &flat::Variable,
    expr: &rumoca_core::Expression,
) -> bool {
    if matches!(var.variability, rumoca_core::Variability::Constant(_)) {
        return true;
    }
    if !matches!(var.variability, rumoca_core::Variability::Parameter(_)) {
        return false;
    }
    var.evaluate
        || var.is_discrete_type
        || structural_non_real_expr(expr)
            && !variable_type_is_real(flat.variable_type_names.get(name))
}

fn variable_type_is_real(type_name: Option<&String>) -> bool {
    type_name.is_some_and(|name| rumoca_core::qualified_type_name_matches(name, "Real"))
}

fn structural_non_real_expr(expr: &rumoca_core::Expression) -> bool {
    match expr {
        rumoca_core::Expression::Literal { value, .. } => {
            !matches!(value, rumoca_core::Literal::Real(_))
        }
        rumoca_core::Expression::VarRef { .. } => true,
        rumoca_core::Expression::Unary { rhs, .. } => structural_non_real_expr(rhs),
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            structural_non_real_expr(lhs) && structural_non_real_expr(rhs)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().all(|(condition, value)| {
                structural_non_real_expr(condition) && structural_non_real_expr(value)
            }) && structural_non_real_expr(else_branch)
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. }
        | rumoca_core::Expression::Array { elements: args, .. } => {
            args.iter().all(structural_non_real_expr)
        }
        rumoca_core::Expression::FieldAccess { base, .. } => structural_non_real_expr(base),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            structural_non_real_expr(base)
                && subscripts.iter().all(|subscript| match subscript {
                    rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => {
                        true
                    }
                    rumoca_core::Subscript::Expr { expr, .. } => structural_non_real_expr(expr),
                })
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            structural_non_real_expr(start)
                && step
                    .as_ref()
                    .is_none_or(|step| structural_non_real_expr(step))
                && structural_non_real_expr(end)
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            structural_non_real_expr(expr)
                && indices
                    .iter()
                    .all(|index| structural_non_real_expr(&index.range))
                && filter
                    .as_ref()
                    .is_none_or(|filter| structural_non_real_expr(filter))
        }
        rumoca_core::Expression::Empty { .. } => false,
    }
}

fn evaluate_static_initial_parameter_algorithms(
    flat: &mut flat::Model,
    ctx: &Context,
) -> Result<(), FlattenError> {
    for algorithm in flat.initial_algorithms.clone() {
        let mut eval_ctx = constant_eval_context_for_flat(flat, ctx);
        let mut assignments = rustc_hash::FxHashMap::default();
        if eval_static_statement_block(&algorithm.statements, flat, &mut eval_ctx, &mut assignments)
            .is_err()
        {
            continue;
        }
        for (name, value) in assignments {
            let Some(var) = flat.variables.get_mut(&rumoca_core::VarName::new(&name)) else {
                continue;
            };
            if let Some(expr) = constant_value_to_expression(&value, var.source_span) {
                var.binding = Some(expr);
            }
        }
    }
    Ok(())
}

fn constant_eval_context_for_flat(
    flat: &flat::Model,
    ctx: &Context,
) -> rumoca_eval_flat::constant::EvalContext {
    let mut eval_ctx = rumoca_eval_flat::constant::EvalContext::with_capacity(
        ctx.parameter_values.len()
            + ctx.real_parameter_values.len()
            + ctx.boolean_parameter_values.len()
            + ctx.string_parameter_values.len()
            + ctx.constant_values.len()
            + flat.variables.len(),
        ctx.enum_parameter_values.len(),
        flat.functions.len(),
    );
    for function in flat.functions.values() {
        eval_ctx.add_function(function.clone());
    }
    for (name, value) in &ctx.parameter_values {
        eval_ctx.add_parameter(
            name.clone(),
            rumoca_eval_flat::constant::Value::Integer(*value),
        );
    }
    for (name, value) in &ctx.real_parameter_values {
        eval_ctx.add_parameter(
            name.clone(),
            rumoca_eval_flat::constant::Value::Real(*value),
        );
    }
    for (name, value) in &ctx.boolean_parameter_values {
        eval_ctx.add_parameter(
            name.clone(),
            rumoca_eval_flat::constant::Value::Bool(*value),
        );
    }
    for (name, value) in &ctx.string_parameter_values {
        eval_ctx.add_parameter(
            name.clone(),
            rumoca_eval_flat::constant::Value::String(value.clone()),
        );
    }
    for (name, value) in &ctx.enum_parameter_values {
        eval_ctx.enum_literals.insert(
            name.clone(),
            (parent_component_scope(value), value.to_string()),
        );
    }
    for (name, expr) in &ctx.constant_values {
        let Some(span) = expr.span() else {
            continue;
        };
        if let Ok(value) = rumoca_eval_flat::constant::eval_expr_with_span(expr, &eval_ctx, span) {
            eval_ctx.add_parameter(name.clone(), value);
        }
    }
    for (name, var) in &flat.variables {
        if !matches!(
            var.variability,
            rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
        ) {
            continue;
        }
        let Some(expr) = var.binding.as_ref().or_else(|| {
            (var.fixed != Some(false))
                .then_some(())
                .and(var.start.as_ref())
        }) else {
            continue;
        };
        if let Ok(value) = rumoca_eval_flat::constant::eval_expr_with_span(
            expr,
            &eval_ctx,
            expr.span().unwrap_or(var.source_span),
        ) {
            eval_ctx.add_parameter(name.as_str().to_string(), value);
        }
    }
    eval_ctx
}

fn eval_static_statement_block(
    statements: &[rumoca_core::Statement],
    flat: &flat::Model,
    eval_ctx: &mut rumoca_eval_flat::constant::EvalContext,
    assignments: &mut rustc_hash::FxHashMap<String, rumoca_eval_flat::constant::Value>,
) -> Result<(), ()> {
    for statement in statements {
        eval_static_statement(statement, flat, eval_ctx, assignments)?;
    }
    Ok(())
}

fn eval_static_statement(
    statement: &rumoca_core::Statement,
    flat: &flat::Model,
    eval_ctx: &mut rumoca_eval_flat::constant::EvalContext,
    assignments: &mut rustc_hash::FxHashMap<String, rumoca_eval_flat::constant::Value>,
) -> Result<(), ()> {
    match statement {
        rumoca_core::Statement::Empty { .. } => Ok(()),
        rumoca_core::Statement::Assignment { comp, value, span } => {
            let target = static_initial_parameter_target(flat, comp)?;
            let value = rumoca_eval_flat::constant::eval_expr_with_span(value, eval_ctx, *span)
                .map_err(|_| ())?;
            eval_ctx.add_parameter(target.clone(), value.clone());
            assignments.insert(target, value);
            Ok(())
        }
        rumoca_core::Statement::For {
            indices, equations, ..
        } => eval_static_for(indices, equations, flat, eval_ctx, assignments),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span,
        } => {
            for block in cond_blocks {
                let cond =
                    rumoca_eval_flat::constant::eval_expr_with_span(&block.cond, eval_ctx, *span)
                        .map_err(|_| ())?;
                if cond.as_bool().ok_or(())? {
                    return eval_static_statement_block(&block.stmts, flat, eval_ctx, assignments);
                }
            }
            if let Some(else_block) = else_block {
                return eval_static_statement_block(else_block, flat, eval_ctx, assignments);
            }
            Ok(())
        }
        rumoca_core::Statement::Assert {
            condition, span, ..
        } => {
            let condition =
                rumoca_eval_flat::constant::eval_expr_with_span(condition, eval_ctx, *span)
                    .map_err(|_| ())?;
            condition.as_bool().filter(|value| *value).ok_or(())?;
            Ok(())
        }
        _ => Err(()),
    }
}

fn eval_static_for(
    indices: &[rumoca_core::ForIndex],
    body: &[rumoca_core::Statement],
    flat: &flat::Model,
    eval_ctx: &mut rumoca_eval_flat::constant::EvalContext,
    assignments: &mut rustc_hash::FxHashMap<String, rumoca_eval_flat::constant::Value>,
) -> Result<(), ()> {
    let Some((index, rest)) = indices.split_first() else {
        return eval_static_statement_block(body, flat, eval_ctx, assignments);
    };
    let span = index.range.span().ok_or(())?;
    let values = rumoca_eval_flat::constant::eval_expr_with_span(&index.range, eval_ctx, span)
        .map_err(|_| ())?;
    let values = values.as_array().ok_or(())?.clone();
    let previous = eval_ctx.parameters.get(&index.ident).cloned();
    for value in values {
        eval_ctx.add_parameter(index.ident.clone(), value);
        eval_static_for(rest, body, flat, eval_ctx, assignments)?;
    }
    match previous {
        Some(value) => eval_ctx.add_parameter(index.ident.clone(), value),
        None => {
            eval_ctx.parameters.swap_remove(&index.ident);
        }
    }
    Ok(())
}

fn static_initial_parameter_target(
    flat: &flat::Model,
    comp: &rumoca_core::ComponentReference,
) -> Result<String, ()> {
    if comp.parts.iter().any(|part| !part.subs.is_empty()) {
        return Err(());
    }
    let name = comp.to_var_name();
    let Some(var) = flat.variables.get(&name) else {
        return Err(());
    };
    if !matches!(
        var.variability,
        rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
    ) || var.fixed != Some(false)
    {
        return Err(());
    }
    Ok(name.as_str().to_string())
}

fn constant_value_to_expression(
    value: &rumoca_eval_flat::constant::Value,
    span: rumoca_core::Span,
) -> Option<rumoca_core::Expression> {
    match value {
        rumoca_eval_flat::constant::Value::Integer(value) => {
            Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(*value),
                span,
            })
        }
        rumoca_eval_flat::constant::Value::Real(value) => Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(*value),
            span,
        }),
        rumoca_eval_flat::constant::Value::Bool(value) => Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(*value),
            span,
        }),
        rumoca_eval_flat::constant::Value::String(value) => {
            Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String(value.clone()),
                span,
            })
        }
        rumoca_eval_flat::constant::Value::Array(values) => {
            let elements = values
                .iter()
                .map(|value| constant_value_to_expression(value, span))
                .collect::<Option<Vec<_>>>()?;
            Some(rumoca_core::Expression::Array {
                elements,
                is_matrix: false,
                span,
            })
        }
        rumoca_eval_flat::constant::Value::Enum(type_name, literal) => {
            Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(format!("{type_name}.{literal}")),
                subscripts: vec![],
                span,
            })
        }
        rumoca_eval_flat::constant::Value::Record(_) => None,
    }
}

fn substitute_structured_equation_templates(
    families: &mut [flat::StructuredEquationFamily],
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    var_dims: &rustc_hash::FxHashMap<String, Vec<i64>>,
    var_values: &rustc_hash::FxHashMap<String, rumoca_core::Expression>,
) -> Result<(), FlattenError> {
    for family in families {
        let Some(template) = &mut family.template else {
            continue;
        };
        let scope = equation_origin_scope(&family.origin);
        for residual in &mut template.body {
            *residual = substitute_known_constants_expr_with_options_dims_and_values(
                residual.clone(),
                ctx,
                live_vars,
                locals,
                &scope,
                true,
                var_dims,
                var_values,
            )?;
        }
    }
    Ok(())
}

#[derive(Clone)]
struct PrimitiveParameterBindingCandidate {
    scope: String,
    min: Option<rumoca_core::Expression>,
    max: Option<rumoca_core::Expression>,
    binding: rumoca_core::Expression,
}

fn recover_primitive_constructor_parameter_bindings(flat: &mut flat::Model) {
    let candidates = primitive_parameter_binding_candidates(flat);
    if candidates.is_empty() {
        return;
    }

    for eq in &mut flat.equations {
        let scope = assignment_scope_from_residual(&eq.residual)
            .unwrap_or_else(|| equation_origin_scope(&eq.origin));
        let mut rewriter = PrimitiveConstructorBindingRecoverer {
            scope: &scope,
            candidates: &candidates,
        };
        eq.residual = rewriter.rewrite_expression(&eq.residual);
    }
    for eq in &mut flat.initial_equations {
        let scope = assignment_scope_from_residual(&eq.residual)
            .unwrap_or_else(|| equation_origin_scope(&eq.origin));
        let mut rewriter = PrimitiveConstructorBindingRecoverer {
            scope: &scope,
            candidates: &candidates,
        };
        eq.residual = rewriter.rewrite_expression(&eq.residual);
    }
}

fn primitive_parameter_binding_candidates(
    flat: &flat::Model,
) -> Vec<PrimitiveParameterBindingCandidate> {
    flat.variables
        .iter()
        .filter_map(|(name, var)| {
            let type_name = flat.variable_type_names.get(name)?;
            if !rumoca_core::qualified_type_name_matches(type_name, "Integer")
                || !matches!(
                    var.variability,
                    rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
                )
                || !var.binding_from_modification
            {
                return None;
            }
            let binding = var.binding.clone()?;
            Some(PrimitiveParameterBindingCandidate {
                scope: parent_component_scope(var.name.as_str()),
                min: var.min.clone(),
                max: var.max.clone(),
                binding,
            })
        })
        .collect()
}

struct PrimitiveConstructorBindingRecoverer<'a> {
    scope: &'a str,
    candidates: &'a [PrimitiveParameterBindingCandidate],
}

impl ExpressionRewriter for PrimitiveConstructorBindingRecoverer<'_> {
    fn walk_function_call_expression(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let mut rewritten_args = self.rewrite_expressions(args);
        for index in primitive_constructor_binding_arg_indices(name.as_str(), rewritten_args.len())
        {
            if let Some(arg) = rewritten_args.get_mut(*index)
                && let Some(recovered) = self.recover_integer_constructor_argument(arg, span)
            {
                *arg = recovered;
            }
        }
        rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args: rewritten_args,
            is_constructor,
            span,
        }
    }
}

fn primitive_constructor_binding_arg_indices(name: &str, arg_len: usize) -> &'static [usize] {
    match name {
        "Clock" if arg_len >= 1 => &[0],
        "subSample" | "superSample" if arg_len >= 2 => &[1],
        "shiftSample" | "backSample" if arg_len >= 3 => &[1, 2],
        _ => &[],
    }
}

impl PrimitiveConstructorBindingRecoverer<'_> {
    fn recover_integer_constructor_argument(
        &self,
        expr: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> Option<rumoca_core::Expression> {
        let bounds = primitive_integer_constructor_bounds(expr)?;
        let mut matches = self.candidates.iter().filter(|candidate| {
            candidate.scope == self.scope
                && bounds
                    .min
                    .as_ref()
                    .is_none_or(|min| candidate.min.as_ref() == Some(min))
                && bounds
                    .max
                    .as_ref()
                    .is_none_or(|max| candidate.max.as_ref() == Some(max))
        });
        let candidate = matches.next()?;
        matches
            .next()
            .is_none()
            .then(|| candidate.binding.clone().with_span(span))
    }
}

struct PrimitiveConstructorBounds {
    min: Option<rumoca_core::Expression>,
    max: Option<rumoca_core::Expression>,
}

fn primitive_integer_constructor_bounds(
    expr: &rumoca_core::Expression,
) -> Option<PrimitiveConstructorBounds> {
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor,
        ..
    } = expr
    else {
        return None;
    };
    if name.as_str() != "Integer" || !*is_constructor || args.is_empty() {
        return None;
    }

    let mut min = None;
    let mut max = None;
    for arg in args {
        let rumoca_core::Expression::FunctionCall {
            name,
            args: named_args,
            ..
        } = arg
        else {
            return None;
        };
        let attribute = name
            .as_str()
            .strip_prefix(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)?;
        let value = named_args.first()?.clone();
        if named_args.len() != 1 {
            return None;
        }
        match attribute {
            "min" => min = Some(value),
            "max" => max = Some(value),
            _ => return None,
        }
    }

    Some(PrimitiveConstructorBounds { min, max })
}

fn assignment_scope_from_residual(expr: &rumoca_core::Expression) -> Option<String> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = expr
    else {
        return None;
    };
    assignment_scope_var_ref(lhs)
        .or_else(|| assignment_scope_var_ref(rhs))
        .map(|name| parent_component_scope(name.as_str()))
        .filter(|scope| !scope.is_empty())
}

fn assignment_scope_var_ref(expr: &rumoca_core::Expression) -> Option<&rumoca_core::Reference> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name),
        _ => None,
    }
}

fn equation_origin_scope(origin: &flat::EquationOrigin) -> String {
    match origin {
        flat::EquationOrigin::ComponentEquation { component }
        | flat::EquationOrigin::Algorithm { component } => component.clone(),
        flat::EquationOrigin::Binding { variable }
        | flat::EquationOrigin::Reinit { state: variable }
        | flat::EquationOrigin::WhenAssignment { target: variable }
        | flat::EquationOrigin::UnconnectedFlow { variable } => parent_component_scope(variable),
        flat::EquationOrigin::Connection { .. } | flat::EquationOrigin::FlowSum { .. } => {
            String::new()
        }
    }
}

fn parent_component_scope(name: &str) -> String {
    rumoca_core::ComponentPath::from_flat_path(name)
        .parent()
        .unwrap_or_else(rumoca_core::ComponentPath::root)
        .to_flat_string()
}

fn substitute_assert_equations(
    equations: &mut [flat::AssertEquation],
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    var_dims: &rustc_hash::FxHashMap<String, Vec<i64>>,
    var_values: &rustc_hash::FxHashMap<String, rumoca_core::Expression>,
) -> Result<(), FlattenError> {
    for assert_eq in equations {
        let scope = equation_origin_scope(&assert_eq.origin);
        assert_eq.condition = substitute_known_constants_expr_with_options_dims_and_values(
            assert_eq.condition.clone(),
            ctx,
            live_vars,
            locals,
            &scope,
            true,
            var_dims,
            var_values,
        )?;
        assert_eq.message = substitute_known_constants_expr_with_options_dims_and_values(
            assert_eq.message.clone(),
            ctx,
            live_vars,
            locals,
            &scope,
            true,
            var_dims,
            var_values,
        )?;
        substitute_opt_expr_with_options_dims_and_values(
            &mut assert_eq.level,
            ctx,
            live_vars,
            locals,
            &scope,
            true,
            var_dims,
            var_values,
        )?;
    }
    Ok(())
}

fn substitute_algorithms(
    algorithms: &mut [flat::Algorithm],
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> Result<(), FlattenError> {
    for algorithm in algorithms {
        for statement in &mut algorithm.statements {
            substitute_known_constants_statement(statement, ctx, live_vars, locals, "")?;
        }
    }
    Ok(())
}

fn substitute_variable_annotations(
    flat: &mut flat::Model,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    var_dims: &rustc_hash::FxHashMap<String, Vec<i64>>,
    var_values: &rustc_hash::FxHashMap<String, rumoca_core::Expression>,
) -> Result<(), FlattenError> {
    for (name, var) in &mut flat.variables {
        let scope = parent_component_scope(var.name.as_str());
        if !is_runtime_parameter_modifier_binding(var)
            || binding_references_class_constant(var.binding.as_ref(), ctx)
        {
            substitute_opt_expr_with_options_dims_and_values(
                &mut var.binding,
                ctx,
                live_vars,
                locals,
                &scope,
                false,
                var_dims,
                var_values,
            )?;
        }
        substitute_opt_expr_with_options_and_dims(
            &mut var.start,
            ctx,
            live_vars,
            locals,
            &scope,
            true,
            var_dims,
        )?;
        substitute_opt_expr_with_options_and_dims(
            &mut var.min,
            ctx,
            live_vars,
            locals,
            &scope,
            true,
            var_dims,
        )?;
        substitute_opt_expr_with_options_and_dims(
            &mut var.max,
            ctx,
            live_vars,
            locals,
            &scope,
            true,
            var_dims,
        )?;
        substitute_opt_expr_with_options_and_dims(
            &mut var.nominal,
            ctx,
            live_vars,
            locals,
            &scope,
            true,
            var_dims,
        )?;
        reconcile_constructor_extents_with_declared_dims(var);
        if variable_is_string_type(var, flat.variable_type_names.get(name)) {
            recover_string_literal_opt_expr(&mut var.binding, &scope);
            recover_string_literal_opt_expr(&mut var.start, &scope);
        }
    }
    Ok(())
}

fn reconcile_constructor_extents_with_declared_dims(var: &mut flat::Variable) {
    if var.dims.is_empty()
        || !matches!(
            var.variability,
            rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
        )
    {
        return;
    }
    reconcile_constructor_extent_expr(&mut var.binding, &var.dims);
    reconcile_constructor_extent_expr(&mut var.start, &var.dims);
}

fn reconcile_residual_constructor_extents_with_lhs_dims(
    residual: &mut rumoca_core::Expression,
    var_dims: &rustc_hash::FxHashMap<String, Vec<i64>>,
) -> Option<()> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = residual
    else {
        return None;
    };
    let lhs_name = residual_lhs_var_ref_name(lhs)?;
    let dims = var_dims.get(lhs_name.as_str())?;
    reconcile_constructor_extent_in_value_expr(rhs, dims)
}

fn residual_lhs_var_ref_name(expr: &rumoca_core::Expression) -> Option<&rumoca_core::Reference> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name),
        rumoca_core::Expression::Index { base, .. } => {
            let rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } = base.as_ref()
            else {
                return None;
            };
            subscripts.is_empty().then_some(name)
        }
        _ => None,
    }
}

fn reconcile_constructor_extent_expr(
    expr: &mut Option<rumoca_core::Expression>,
    dims: &[i64],
) -> Option<()> {
    reconcile_constructor_extent_in_value_expr(expr.as_mut()?, dims)
}

fn reconcile_constructor_extent_in_value_expr(
    expr: &mut rumoca_core::Expression,
    dims: &[i64],
) -> Option<()> {
    match expr {
        rumoca_core::Expression::BuiltinCall { .. } => {
            reconcile_constructor_extent_call(expr, dims)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let mut changed = false;
            for (_condition, value) in branches {
                changed |= reconcile_constructor_extent_in_value_expr(value, dims).is_some();
            }
            changed |= reconcile_constructor_extent_in_value_expr(else_branch, dims).is_some();
            changed.then_some(())
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            let lhs_changed = reconcile_constructor_extent_in_value_expr(lhs, dims).is_some();
            let rhs_changed = reconcile_constructor_extent_in_value_expr(rhs, dims).is_some();
            (lhs_changed || rhs_changed).then_some(())
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            reconcile_constructor_extent_in_value_expr(rhs, dims)
        }
        _ => None,
    }
}

fn reconcile_constructor_extent_call(
    expr: &mut rumoca_core::Expression,
    dims: &[i64],
) -> Option<()> {
    let target_dims = dims
        .iter()
        .copied()
        .map(|dim| usize::try_from(dim).ok())
        .collect::<Option<Vec<_>>>()?;
    if target_dims.is_empty() {
        return None;
    }
    let target_len = target_dims.iter().product::<usize>();
    if target_len == 0 {
        return None;
    }
    let rumoca_core::Expression::BuiltinCall {
        function,
        args,
        span,
    } = expr
    else {
        unreachable!("constructor extent reconciliation is only called for builtin calls");
    };
    let offset = match function {
        rumoca_core::BuiltinFunction::Fill => 1,
        rumoca_core::BuiltinFunction::Zeros | rumoca_core::BuiltinFunction::Ones => 0,
        _ => return None,
    };
    if args.len() < offset + 1 {
        return None;
    }
    if let Some(current_dims) = args[offset..]
        .iter()
        .map(literal_usize)
        .collect::<Option<Vec<_>>>()
    {
        let current_len = current_dims.iter().product::<usize>();
        if current_len == target_len || current_len != 0 {
            return None;
        }
    }
    let mut rewritten = args[..offset].to_vec();
    rewritten.extend(
        target_dims
            .into_iter()
            .map(|dim| rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(dim as i64),
                span: *span,
            }),
    );
    *args = rewritten;
    Some(())
}

fn literal_usize(expr: &rumoca_core::Expression) -> Option<usize> {
    let rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        ..
    } = expr
    else {
        return None;
    };
    usize::try_from(*value).ok()
}

fn is_runtime_parameter_modifier_binding(var: &flat::Variable) -> bool {
    matches!(
        var.variability,
        rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
    ) && var.binding_from_modification
        && !var.evaluate
        && !var.is_discrete_type
        && var.binding.is_some()
}

fn binding_references_class_constant(
    binding: Option<&rumoca_core::Expression>,
    ctx: &Context,
) -> bool {
    binding.is_some_and(|expr| expr_references_class_constant(expr, ctx))
}

fn expr_references_class_constant(expr: &rumoca_core::Expression, ctx: &Context) -> bool {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            let self_is_class_constant = subscripts.is_empty()
                && (ctx.class_constant_keys.contains(name.as_str())
                    || name
                        .target_def_id()
                        .and_then(|def_id| ctx.target_def_names.get(&def_id))
                        .is_some_and(|target| ctx.class_constant_keys.contains(target)));
            self_is_class_constant || subscripts_reference_class_constant(subscripts, ctx)
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            expr_references_class_constant(lhs, ctx) || expr_references_class_constant(rhs, ctx)
        }
        rumoca_core::Expression::Unary { rhs, .. } => expr_references_class_constant(rhs, ctx),
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. }
        | rumoca_core::Expression::Array { elements: args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. } => args
            .iter()
            .any(|expr| expr_references_class_constant(expr, ctx)),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(cond, value)| {
                expr_references_class_constant(cond, ctx)
                    || expr_references_class_constant(value, ctx)
            }) || expr_references_class_constant(else_branch, ctx)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            expr_references_class_constant(start, ctx)
                || step
                    .as_ref()
                    .is_some_and(|expr| expr_references_class_constant(expr, ctx))
                || expr_references_class_constant(end, ctx)
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            expr_references_class_constant(expr, ctx)
                || indices
                    .iter()
                    .any(|index| expr_references_class_constant(&index.range, ctx))
                || filter
                    .as_ref()
                    .is_some_and(|expr| expr_references_class_constant(expr, ctx))
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            expr_references_class_constant(base, ctx)
                || subscripts_reference_class_constant(subscripts, ctx)
        }
        rumoca_core::Expression::FieldAccess { base, .. } => {
            expr_references_class_constant(base, ctx)
        }
        rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => false,
    }
}

fn subscripts_reference_class_constant(
    subscripts: &[rumoca_core::Subscript],
    ctx: &Context,
) -> bool {
    subscripts.iter().any(|subscript| {
        matches!(
            subscript,
            rumoca_core::Subscript::Expr { expr, .. }
            if expr_references_class_constant(expr, ctx)
        )
    })
}

fn variable_is_string_type(var: &flat::Variable, type_name: Option<&String>) -> bool {
    var.type_id == rumoca_core::TypeId(3)
        || type_name.is_some_and(|name| rumoca_core::qualified_type_name_matches(name, "String"))
}

fn recover_string_literal_opt_expr(expr: &mut Option<rumoca_core::Expression>, scope: &str) {
    let Some(recovered) = expr.as_ref().and_then(|expr| {
        crate::variables::recover_string_literal_from_invalid_component_expr(expr, scope)
    }) else {
        return;
    };
    *expr = Some(recovered);
}

fn substitute_function_bodies(
    functions: &mut flat::VarNameIndexMap<rumoca_core::Function>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
) -> Result<(), FlattenError> {
    for function in functions.values_mut() {
        let function_locals: HashSet<String> = function
            .inputs
            .iter()
            .chain(function.outputs.iter())
            .chain(function.locals.iter())
            .map(|param| param.name.clone())
            .collect();
        let function_scope =
            crate::path_utils::enclosing_scope(function.name.as_str()).unwrap_or("");

        for param in function
            .inputs
            .iter_mut()
            .chain(function.outputs.iter_mut())
            .chain(function.locals.iter_mut())
        {
            substitute_opt_expr(
                &mut param.default,
                ctx,
                live_vars,
                &function_locals,
                function_scope,
            )?;
        }
        for statement in &mut function.body {
            substitute_known_constants_statement(
                statement,
                ctx,
                live_vars,
                &function_locals,
                function_scope,
            )?;
        }
    }
    Ok(())
}

fn substitute_opt_expr(
    expr: &mut Option<rumoca_core::Expression>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
) -> Result<(), FlattenError> {
    substitute_opt_expr_with_options(expr, ctx, live_vars, locals, scope, false)
}

fn substitute_opt_expr_with_options(
    expr: &mut Option<rumoca_core::Expression>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
    prefer_scoped_parameters: bool,
) -> Result<(), FlattenError> {
    if let Some(expr) = expr {
        *expr = substitute_known_constants_expr_with_options(
            expr.clone(),
            ctx,
            live_vars,
            locals,
            scope,
            prefer_scoped_parameters,
        )?;
    }
    Ok(())
}

fn substitute_opt_expr_with_options_and_dims(
    expr: &mut Option<rumoca_core::Expression>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
    prefer_scoped_parameters: bool,
    var_dims: &rustc_hash::FxHashMap<String, Vec<i64>>,
) -> Result<(), FlattenError> {
    if let Some(expr) = expr {
        *expr = substitute_known_constants_expr_with_options_and_dims(
            expr.clone(),
            ctx,
            live_vars,
            locals,
            scope,
            prefer_scoped_parameters,
            Some(var_dims),
        )?;
    }
    Ok(())
}

fn substitute_opt_expr_with_options_dims_and_values(
    expr: &mut Option<rumoca_core::Expression>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
    prefer_scoped_parameters: bool,
    var_dims: &rustc_hash::FxHashMap<String, Vec<i64>>,
    var_values: &rustc_hash::FxHashMap<String, rumoca_core::Expression>,
) -> Result<(), FlattenError> {
    if let Some(expr) = expr {
        *expr = substitute_known_constants_expr_with_options_dims_and_values(
            expr.clone(),
            ctx,
            live_vars,
            locals,
            scope,
            prefer_scoped_parameters,
            var_dims,
            var_values,
        )?;
    }
    Ok(())
}

pub(crate) fn substitute_known_constants_expr(
    expr: rumoca_core::Expression,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
) -> Result<rumoca_core::Expression, FlattenError> {
    substitute_known_constants_expr_with_options(expr, ctx, live_vars, locals, scope, false)
}

fn substitute_known_constants_expr_with_options(
    expr: rumoca_core::Expression,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
    prefer_scoped_parameters: bool,
) -> Result<rumoca_core::Expression, FlattenError> {
    substitute_known_constants_expr_with_options_and_dims(
        expr,
        ctx,
        live_vars,
        locals,
        scope,
        prefer_scoped_parameters,
        None,
    )
}

fn substitute_known_constants_expr_with_options_and_dims(
    expr: rumoca_core::Expression,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
    prefer_scoped_parameters: bool,
    var_dims: Option<&rustc_hash::FxHashMap<String, Vec<i64>>>,
) -> Result<rumoca_core::Expression, FlattenError> {
    KnownConstantSubstituter {
        env: ConstantSubstitutionEnv {
            ctx,
            live_vars,
            locals,
            scope,
            prefer_scoped_parameters,
            var_dims,
            var_values: None,
            resolving_value: None,
        },
    }
    .rewrite_expression(&expr)
}

fn substitute_known_constants_expr_with_options_dims_and_values(
    expr: rumoca_core::Expression,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
    prefer_scoped_parameters: bool,
    var_dims: &rustc_hash::FxHashMap<String, Vec<i64>>,
    var_values: &rustc_hash::FxHashMap<String, rumoca_core::Expression>,
) -> Result<rumoca_core::Expression, FlattenError> {
    KnownConstantSubstituter {
        env: ConstantSubstitutionEnv {
            ctx,
            live_vars,
            locals,
            scope,
            prefer_scoped_parameters,
            var_dims: Some(var_dims),
            var_values: Some(var_values),
            resolving_value: None,
        },
    }
    .rewrite_expression(&expr)
}

struct KnownConstantSubstituter<'a> {
    env: ConstantSubstitutionEnv<'a>,
}

#[derive(Clone, Copy)]
struct ConstantSubstitutionEnv<'a> {
    ctx: &'a Context,
    live_vars: &'a rustc_hash::FxHashSet<String>,
    locals: &'a HashSet<String>,
    scope: &'a str,
    prefer_scoped_parameters: bool,
    var_dims: Option<&'a rustc_hash::FxHashMap<String, Vec<i64>>>,
    var_values: Option<&'a rustc_hash::FxHashMap<String, rumoca_core::Expression>>,
    resolving_value: Option<&'a ResolvingValue<'a>>,
}

#[derive(Clone, Copy)]
struct ResolvingValue<'a> {
    key: &'a str,
    parent: Option<&'a ResolvingValue<'a>>,
}

impl<'a> ConstantSubstitutionEnv<'a> {
    fn with_scope<'b>(&'b self, scope: &'b str) -> ConstantSubstitutionEnv<'b> {
        ConstantSubstitutionEnv {
            ctx: self.ctx,
            live_vars: self.live_vars,
            locals: self.locals,
            scope,
            prefer_scoped_parameters: self.prefer_scoped_parameters,
            var_dims: self.var_dims,
            var_values: self.var_values,
            resolving_value: self.resolving_value,
        }
    }
}

impl FallibleExpressionRewriter for KnownConstantSubstituter<'_> {
    type Error = FlattenError;

    fn rewrite_expression(
        &mut self,
        expr: &rumoca_core::Expression,
    ) -> Result<rumoca_core::Expression, Self::Error> {
        match expr {
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
            } => self.rewrite_var_ref(name, subscripts, *span),
            rumoca_core::Expression::FieldAccess { base, field, span } => {
                self.rewrite_field_access(base, field, *span)
            }
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Size,
                args,
                span,
            } => {
                if let Some(size_expr) = self.rewrite_size_from_declared_dims(args, *span) {
                    return Ok(size_expr);
                }
                self.walk_expression(expr)
            }
            other => self.walk_expression(other),
        }
    }

    fn rewrite_subscript(
        &mut self,
        subscript: &rumoca_core::Subscript,
    ) -> Result<rumoca_core::Subscript, Self::Error> {
        match subscript {
            rumoca_core::Subscript::Expr { expr, span } => Ok(rumoca_core::Subscript::Expr {
                expr: Box::new(self.rewrite_expression(expr)?),
                span: *span,
            }),
            other => Ok(other.clone()),
        }
    }
}

impl KnownConstantSubstituter<'_> {
    fn rewrite_size_from_declared_dims(
        &self,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
    ) -> Option<rumoca_core::Expression> {
        let [base, dim] = args else {
            return None;
        };
        let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = base
        else {
            return None;
        };
        if !subscripts.is_empty() {
            return None;
        }
        let dim = literal_integer(dim)?;
        let dim_idx = usize::try_from(dim.checked_sub(1)?).ok()?;
        let value = *declared_dims_for_reference(name.as_str(), self.env)?.get(dim_idx)?;
        Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            span,
        })
    }

    fn rewrite_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, FlattenError> {
        if subscripts.is_empty() {
            if let Some(replaced) = substitute_scalar_var_ref(name, span, self.env)? {
                return Ok(replaced);
            }
            return Ok(rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: vec![],
                span,
            });
        }

        let rewritten_subscripts = self.rewrite_subscripts(subscripts)?;
        if self.env.locals.contains(name.as_str()) {
            return Ok(rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: rewritten_subscripts,
                span,
            });
        }
        if let Some(replaced) =
            substitute_indexed_constant_var_ref(name, rewritten_subscripts.clone(), span, self.env)?
        {
            return Ok(replaced);
        }

        Ok(rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: rewritten_subscripts,
            span,
        })
    }

    fn rewrite_field_access(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, FlattenError> {
        let rewritten_base = self.rewrite_expression(base)?;
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            ..
        } = &rewritten_base
        {
            if let Some(named_arg) = named_constructor_arg(args, field) {
                return Ok(named_arg.clone().with_span(span));
            }
            if let Some(positional_arg) =
                positional_constructor_arg_for_field(name.as_str(), args, field, self.env.ctx)
            {
                return Ok(positional_arg.clone().with_span(span));
            }
            if args.is_empty()
                && let Some(resolved) =
                    resolve_constant_field_access(name.as_str(), field, span, self.env.ctx)
            {
                return Ok(resolved);
            }
        }
        if let rumoca_core::Expression::Index {
            base, subscripts, ..
        } = &rewritten_base
            && let Some(resolved) = resolve_indexed_constant_field_access(
                base,
                subscripts,
                field,
                span,
                self.env.ctx,
                self.env.live_vars,
            )
        {
            return self.rewrite_expression(&resolved);
        }
        if let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = &rewritten_base
            && subscripts.is_empty()
            && !self.env.live_vars.contains(name.as_str())
            && !reference_root_is_local(name, self.env.locals)
            && let Some(resolved) =
                resolve_constant_field_access(name.as_str(), field, span, self.env.ctx)
        {
            return Ok(resolved);
        }
        Ok(rumoca_core::Expression::FieldAccess {
            base: Box::new(rewritten_base),
            field: field.to_string(),
            span,
        })
    }
}

fn declared_dims_for_reference<'a>(
    key: &str,
    env: ConstantSubstitutionEnv<'a>,
) -> Option<&'a Vec<i64>> {
    let var_dims = env.var_dims?;
    if let Some(dims) = var_dims.get(key) {
        return Some(dims);
    }
    if !env.scope.is_empty() && !key.contains('.') {
        let scoped_key = format!("{}.{}", env.scope, key);
        if let Some(dims) = var_dims.get(scoped_key.as_str()) {
            return Some(dims);
        }
    }
    None
}

fn resolve_indexed_constant_field_access(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    field: &str,
    span: rumoca_core::Span,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
) -> Option<rumoca_core::Expression> {
    if let rumoca_core::Expression::VarRef { name, .. } = base
        && live_vars.contains(name.as_str())
    {
        return None;
    }
    let selected = select_constant_index(base, subscripts, span, ctx)?;
    resolve_field_on_constant_expr(&selected, field, span, ctx)
}

fn select_constant_index(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    if subscripts.is_empty() {
        return Some(base.clone().with_span(span));
    }

    let base = resolve_constant_expr_alias(base, ctx)?;
    let (first, rest) = subscripts.split_first()?;
    match first {
        rumoca_core::Subscript::Index { value, .. } => {
            select_constant_index_element(&base, *value, rest, span, ctx)
        }
        rumoca_core::Subscript::Expr { expr, .. } => {
            if let Some(index) = literal_integer(expr) {
                return select_constant_index_element(&base, index, rest, span, ctx);
            }
            select_constant_index_symbolic_element(&base, expr, rest, span, ctx)
        }
        rumoca_core::Subscript::Colon { .. } => {
            let rumoca_core::Expression::Array { elements, .. } = base else {
                return None;
            };
            let projected = elements
                .iter()
                .map(|element| select_constant_index(element, rest, span, ctx))
                .collect::<Option<Vec<_>>>()?;
            Some(rumoca_core::Expression::Array {
                elements: projected,
                is_matrix: false,
                span,
            })
        }
    }
}

fn select_constant_index_symbolic_element(
    base: &rumoca_core::Expression,
    index: &rumoca_core::Expression,
    rest: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    match base {
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => select_array_comprehension_symbolic_index_element(
            expr,
            indices,
            filter.as_deref(),
            index,
            rest,
            span,
            ctx,
        ),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            select_binary_symbolic_index_element(op.clone(), lhs, rhs, index, rest, span, ctx)
        }
        _ => None,
    }
}

fn select_constant_index_element(
    base: &rumoca_core::Expression,
    index: i64,
    rest: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    match base {
        rumoca_core::Expression::Array { elements, .. } => {
            let zero_based = usize::try_from(index.checked_sub(1)?).ok()?;
            let element = elements.get(zero_based)?;
            select_constant_index(element, rest, span, ctx)
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => select_array_comprehension_index_element(
            expr,
            indices,
            filter.as_deref(),
            index,
            rest,
            span,
            ctx,
        ),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            select_binary_index_element(op.clone(), lhs, rhs, index, rest, span, ctx)
        }
        _ => None,
    }
}

fn select_array_comprehension_index_element(
    expr: &rumoca_core::Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&rumoca_core::Expression>,
    index: i64,
    rest: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    if filter.is_some() || indices.len() != 1 {
        return None;
    }
    let value = comprehension_index_value(&indices[0].range, index)?;
    let selected = substitute_comprehension_index_literal(expr, &indices[0].name, value, span);
    select_constant_index(&selected, rest, span, ctx)
}

fn select_array_comprehension_symbolic_index_element(
    expr: &rumoca_core::Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&rumoca_core::Expression>,
    index: &rumoca_core::Expression,
    rest: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    if filter.is_some() || indices.len() != 1 {
        return None;
    }
    let selected = substitute_comprehension_index_expr(expr, &indices[0].name, index, span);
    select_constant_index(&selected, rest, span, ctx)
}

fn select_binary_index_element(
    op: rumoca_core::OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    index: i64,
    rest: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    let lhs_selected = select_constant_index_element(lhs, index, rest, span, ctx);
    let rhs_selected = select_constant_index_element(rhs, index, rest, span, ctx);
    match (lhs_selected, rhs_selected) {
        (Some(lhs), Some(rhs)) => Some(rumoca_core::Expression::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        }),
        (Some(lhs), None) => Some(rumoca_core::Expression::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs.clone().with_span(span)),
            span,
        }),
        (None, Some(rhs)) => Some(rumoca_core::Expression::Binary {
            op,
            lhs: Box::new(lhs.clone().with_span(span)),
            rhs: Box::new(rhs),
            span,
        }),
        (None, None) => None,
    }
}

fn select_binary_symbolic_index_element(
    op: rumoca_core::OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    index: &rumoca_core::Expression,
    rest: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    let lhs_selected = select_constant_index_symbolic_element(lhs, index, rest, span, ctx);
    let rhs_selected = select_constant_index_symbolic_element(rhs, index, rest, span, ctx);
    match (lhs_selected, rhs_selected) {
        (Some(lhs), Some(rhs)) => Some(rumoca_core::Expression::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        }),
        (Some(lhs), None) => Some(rumoca_core::Expression::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs.clone().with_span(span)),
            span,
        }),
        (None, Some(rhs)) => Some(rumoca_core::Expression::Binary {
            op,
            lhs: Box::new(lhs.clone().with_span(span)),
            rhs: Box::new(rhs),
            span,
        }),
        (None, None) => None,
    }
}

fn comprehension_index_value(range: &rumoca_core::Expression, one_based_index: i64) -> Option<i64> {
    let rumoca_core::Expression::Range {
        start, step, end, ..
    } = range
    else {
        return None;
    };
    let start = literal_integer(start)?;
    let step = match step.as_deref() {
        Some(step) => literal_integer(step)?,
        None => 1,
    };
    let value = start + (one_based_index.checked_sub(1)?) * step;
    let end = literal_integer(end)?;
    ((step > 0 && value <= end) || (step < 0 && value >= end) || (step == 0 && value == start))
        .then_some(value)
}

fn substitute_comprehension_index_literal(
    expr: &rumoca_core::Expression,
    name: &str,
    value: i64,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    substitute_comprehension_index_expr(
        expr,
        name,
        &rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            span,
        },
        span,
    )
}

fn substitute_comprehension_index_expr(
    expr: &rumoca_core::Expression,
    name: &str,
    replacement: &rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    struct Substituter<'a> {
        name: &'a str,
        replacement: &'a rumoca_core::Expression,
        span: rumoca_core::Span,
    }

    impl rumoca_core::ExpressionRewriter for Substituter<'_> {
        fn walk_var_ref_expression(
            &mut self,
            name: &rumoca_core::Reference,
            subscripts: &[rumoca_core::Subscript],
            span: rumoca_core::Span,
        ) -> rumoca_core::Expression {
            if name.as_str() == self.name && subscripts.is_empty() {
                return self.replacement.clone().with_span(self.span);
            }
            rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: self.rewrite_subscripts(subscripts),
                span,
            }
        }

        fn walk_array_comprehension_expression(
            &mut self,
            expr: &rumoca_core::Expression,
            indices: &[rumoca_core::ComprehensionIndex],
            filter: Option<&rumoca_core::Expression>,
            span: rumoca_core::Span,
        ) -> rumoca_core::Expression {
            if indices.iter().any(|index| index.name == self.name) {
                return rumoca_core::Expression::ArrayComprehension {
                    expr: Box::new(expr.clone()),
                    indices: indices.to_vec(),
                    filter: filter.cloned().map(Box::new),
                    span,
                };
            }
            rumoca_core::ExpressionRewriter::walk_array_comprehension_expression(
                self, expr, indices, filter, span,
            )
        }
    }

    let mut substituter = Substituter {
        name,
        replacement,
        span,
    };
    substituter.rewrite_expression(expr)
}

fn resolve_constant_expr_alias(
    expr: &rumoca_core::Expression,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => resolve_constant_value_expr_for_ref(name, ctx).cloned(),
        other => Some(other.clone()),
    }
}

fn resolve_field_on_constant_expr(
    expr: &rumoca_core::Expression,
    field: &str,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    match expr {
        rumoca_core::Expression::Array { elements, .. } => {
            let projected = elements
                .iter()
                .map(|element| resolve_field_on_constant_expr(element, field, span, ctx))
                .collect::<Option<Vec<_>>>()?;
            Some(rumoca_core::Expression::Array {
                elements: projected,
                is_matrix: false,
                span,
            })
        }
        rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            ..
        } => named_constructor_arg(args, field)
            .cloned()
            .map(|expr| expr.with_span(span))
            .or_else(|| {
                positional_constructor_arg_for_field(name.as_str(), args, field, ctx)
                    .cloned()
                    .map(|expr| expr.with_span(span))
            })
            .or_else(|| {
                args.is_empty()
                    .then(|| resolve_constant_field_access(name.as_str(), field, span, ctx))
                    .flatten()
            }),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            resolve_constant_field_access(name.as_str(), field, span, ctx)
        }
        _ => None,
    }
}

fn literal_integer(expr: &rumoca_core::Expression) -> Option<i64> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => Some(*value),
        _ => None,
    }
}

impl FallibleStatementRewriter for KnownConstantSubstituter<'_> {}

fn substitute_indexed_constant_var_ref(
    name: &rumoca_core::Reference,
    subscripts: Vec<rumoca_core::Subscript>,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'_>,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    let constant_expr = match resolve_constant_value_expr_for_ref(name, env.ctx) {
        Some(expr) => expr.clone(),
        None => return Ok(None),
    };
    if env.live_vars.contains(name.as_str()) {
        return Ok(symbolic_alias_expr(&constant_expr).map(|base| {
            rumoca_core::Expression::Index {
                base: Box::new(base.with_span(span)),
                subscripts,
                span,
            }
        }));
    }

    if let Some(selected) = select_constant_index(&constant_expr, &subscripts, span, env.ctx) {
        return Ok(Some(substitute_resolved_constant_expr(
            name.as_str(),
            &selected,
            span,
            env,
        )?));
    }

    Ok(Some(rumoca_core::Expression::Index {
        base: Box::new(constant_expr),
        subscripts,
        span,
    }))
}

fn symbolic_alias_expr(expr: &rumoca_core::Expression) -> Option<rumoca_core::Expression> {
    match expr {
        rumoca_core::Expression::VarRef { .. }
        | rumoca_core::Expression::Index { .. }
        | rumoca_core::Expression::FieldAccess { .. } => Some(expr.clone()),
        _ => None,
    }
}

fn substitute_scalar_var_ref(
    name: &rumoca_core::Reference,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'_>,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    let key = name.as_str();
    if let Some(expr) = substitute_flat_variable_value_ref(key, span, env)? {
        return Ok(Some(expr));
    }
    if env.live_vars.contains(key) {
        if parameter_is_non_structural(key, env) {
            return Ok(None);
        }
        if let Some(expr) = structured_key_value_expr(key, span, env)? {
            return Ok(Some(expr));
        }
        if reference_key_is_structured(key)
            && let Some(v) = resolve_constant_value_expr_for_ref(name, env.ctx)
        {
            return Ok(Some(substitute_resolved_constant_expr(key, v, span, env)?));
        }
        return Ok(None);
    }
    if reference_root_is_local(name, env.locals) {
        return Ok(None);
    }
    if inline_index_base_is_live_or_local(key, env.live_vars, env.locals) {
        return Ok(None);
    }
    if let Some(scoped_live_ref) = scoped_live_var_ref(key, span, env) {
        return Ok(Some(scoped_live_ref));
    }
    let has_array_shape = reference_has_array_shape(name, key, env.ctx, env.scope);
    if env.prefer_scoped_parameters
        && !env.scope.is_empty()
        && let Some(expr) = substitute_scoped_scalar_var_ref(key, span, env)?
    {
        return Ok(Some(expr));
    }
    if let Some(expr) = structured_key_value_expr(key, span, env)? {
        return Ok(Some(expr));
    }
    if let Some(v) = resolve_constant_value_expr_for_ref(name, env.ctx) {
        if has_array_shape && !constant_expr_preserves_array_shape(v) {
            return Ok(None);
        }
        if expression_is_record_constructor(v)
            && let Some(expr) = resolve_projected_constant_path(key, span, env.ctx)
        {
            return Ok(Some(substitute_known_constants_expr_with_options(
                expr,
                env.ctx,
                env.live_vars,
                env.locals,
                env.scope,
                env.prefer_scoped_parameters,
            )?));
        }
        return Ok(Some(substitute_resolved_constant_expr(key, v, span, env)?));
    }
    if !has_array_shape && let Some(literal) = scalar_parameter_literal(key, span, env.ctx) {
        return Ok(Some(literal));
    }
    if let Some(expr) = resolve_inline_indexed_constant(key, span, env.ctx)? {
        return Ok(Some(expr));
    }
    if let Some(expr) = resolve_projected_constant_path(key, span, env.ctx) {
        return Ok(Some(substitute_known_constants_expr_with_options(
            expr,
            env.ctx,
            env.live_vars,
            env.locals,
            env.scope,
            env.prefer_scoped_parameters,
        )?));
    }
    if !env.prefer_scoped_parameters
        && !env.scope.is_empty()
        && let Some(expr) = substitute_scoped_scalar_var_ref(key, span, env)?
    {
        return Ok(Some(expr));
    }

    substitute_alias_resolved_scalar_var_ref(key, span, env)
}

fn substitute_flat_variable_value_ref(
    key: &str,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'_>,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    let Some(var_values) = env.var_values else {
        return Ok(None);
    };
    for candidate in scoped_lookup_candidates(key, env.scope) {
        if resolving_value_stack_contains(env.resolving_value, &candidate) {
            continue;
        }
        let Some(value) = var_values.get(&candidate) else {
            continue;
        };
        if reference_key_has_array_shape(&candidate, env.ctx, env.scope)
            && !constant_expr_preserves_array_shape(value)
        {
            continue;
        }
        return Ok(Some(substitute_resolved_constant_expr(
            &candidate, value, span, env,
        )?));
    }
    Ok(None)
}

fn resolving_value_stack_contains(mut node: Option<&ResolvingValue<'_>>, key: &str) -> bool {
    while let Some(value) = node {
        if value.key == key {
            return true;
        }
        node = value.parent;
    }
    false
}

fn parameter_is_non_structural(key: &str, env: ConstantSubstitutionEnv<'_>) -> bool {
    env.ctx.non_structural_params.contains(key)
        || (!env.scope.is_empty()
            && !key.contains('.')
            && env
                .ctx
                .non_structural_params
                .contains(format!("{}.{}", env.scope, key).as_str()))
}

fn reference_key_is_structured(key: &str) -> bool {
    key.contains('.') || key.contains('[')
}

fn scoped_name_is_structural_value(key: &str, ctx: &Context) -> bool {
    ctx.constant_values.contains_key(key)
        || ctx.class_constant_keys.contains(key)
        || ctx.structural_params.contains(key)
        || ctx.parameter_values.contains_key(key)
        || ctx.boolean_parameter_values.contains_key(key)
        || ctx.string_parameter_values.contains_key(key)
        || ctx.enum_parameter_values.contains_key(key)
}

fn structured_key_value_expr(
    key: &str,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'_>,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    if !reference_key_is_structured(key) || !scoped_name_is_structural_value(key, env.ctx) {
        return Ok(None);
    }
    if let Some(enum_name) = env.ctx.enum_parameter_values.get(key) {
        return Ok(Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(enum_name.clone()),
            subscripts: vec![],
            span,
        }));
    }
    if let Some(v) = resolve_constant_value_expr(key, env.ctx) {
        if expression_is_record_constructor(v)
            && let Some(expr) = resolve_projected_constant_path(key, span, env.ctx)
        {
            return Ok(Some(substitute_known_constants_expr_with_options(
                expr,
                env.ctx,
                env.live_vars,
                env.locals,
                env.scope,
                env.prefer_scoped_parameters,
            )?));
        }
        return Ok(Some(substitute_resolved_constant_expr(key, v, span, env)?));
    }
    if let Some(value) = env.ctx.parameter_values.get(key) {
        return Ok(Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(*value),
            span,
        }));
    }
    if env.ctx.class_constant_keys.contains(key)
        && let Some(value) = env.ctx.real_parameter_values.get(key)
        && value.is_finite()
    {
        return Ok(Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(*value),
            span,
        }));
    }
    if let Some(value) = env.ctx.boolean_parameter_values.get(key) {
        return Ok(Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(*value),
            span,
        }));
    }
    if let Some(value) = env.ctx.string_parameter_values.get(key) {
        return Ok(Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String(value.clone()),
            span,
        }));
    }
    Ok(None)
}

fn scoped_live_var_ref(
    key: &str,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'_>,
) -> Option<rumoca_core::Expression> {
    if env.scope.is_empty() || key.contains('.') {
        return None;
    }
    let scoped_key = format!("{}.{}", env.scope, key);
    if !env.live_vars.contains(&scoped_key) {
        return None;
    }
    Some(rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(scoped_key),
        subscripts: vec![],
        span,
    })
}

fn substitute_alias_resolved_scalar_var_ref(
    key: &str,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'_>,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    let Some(resolved_key) = resolve_varref_through_constant_aliases(key, env.ctx, env.scope)
    else {
        return Ok(None);
    };
    if resolved_key == key {
        return Ok(None);
    }
    if let Some(v) = resolve_constant_value_expr(&resolved_key, env.ctx) {
        if reference_key_has_array_shape(&resolved_key, env.ctx, env.scope)
            && !constant_expr_preserves_array_shape(v)
        {
            return Ok(None);
        }
        return Ok(Some(substitute_resolved_constant_expr(
            &resolved_key,
            v,
            span,
            env,
        )?));
    }
    if !reference_key_has_array_shape(&resolved_key, env.ctx, env.scope)
        && let Some(literal) = evaluated_scalar_parameter_literal(&resolved_key, span, env.ctx)
    {
        return Ok(Some(literal));
    }
    if let Some(expr) = resolve_inline_indexed_constant(&resolved_key, span, env.ctx)? {
        return Ok(Some(expr));
    }
    Ok(Some(rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(resolved_key),
        subscripts: vec![],
        span,
    }))
}

fn substitute_resolved_constant_expr(
    key: &str,
    expr: &rumoca_core::Expression,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'_>,
) -> Result<rumoca_core::Expression, FlattenError> {
    let declaration_scope = parent_component_scope(key);
    let scope = if declaration_scope.is_empty() {
        env.scope
    } else {
        &declaration_scope
    };
    let mut expr = expr.clone().with_span(span);
    if let Some(var_dims) = env.var_dims
        && let Some(dims) = var_dims.get(key)
    {
        let mut maybe_expr = Some(expr);
        reconcile_constructor_extent_expr(&mut maybe_expr, dims);
        expr = maybe_expr.expect("reconciled expression should remain present");
    }
    let resolving_value = ResolvingValue {
        key,
        parent: env.resolving_value,
    };
    KnownConstantSubstituter {
        env: ConstantSubstitutionEnv {
            ctx: env.ctx,
            live_vars: env.live_vars,
            locals: env.locals,
            scope,
            prefer_scoped_parameters: env.prefer_scoped_parameters,
            var_dims: env.var_dims,
            var_values: env.var_values,
            resolving_value: Some(&resolving_value),
        },
    }
    .rewrite_expression(&expr)
}

fn constant_expr_preserves_array_shape(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::Array { .. }
            | rumoca_core::Expression::Tuple { .. }
            | rumoca_core::Expression::Range { .. }
            | rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Fill
                    | rumoca_core::BuiltinFunction::Zeros
                    | rumoca_core::BuiltinFunction::Ones,
                ..
            }
    )
}

fn expression_is_record_constructor(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::FunctionCall {
            is_constructor: true,
            ..
        }
    )
}

fn scalar_parameter_literal(
    key: &str,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    if let Some(v) = ctx.parameter_values.get(key) {
        return Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(*v),
            span,
        });
    }
    if let Some(v) = ctx.real_parameter_values.get(key)
        && v.is_finite()
    {
        return Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(*v),
            span,
        });
    }
    if let Some(v) = ctx.boolean_parameter_values.get(key) {
        return Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(*v),
            span,
        });
    }
    if let Some(v) = ctx.string_parameter_values.get(key) {
        return Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String(v.clone()),
            span,
        });
    }
    ctx.enum_parameter_values
        .get(key)
        .map(|v| rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(v.clone()),
            subscripts: vec![],
            span,
        })
}

fn evaluated_scalar_parameter_literal(
    key: &str,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    if let Some(v) = ctx.parameter_values.get(key) {
        return Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(*v),
            span,
        });
    }
    if let Some(v) = ctx.real_parameter_values.get(key)
        && v.is_finite()
    {
        return Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(*v),
            span,
        });
    }
    if let Some(v) = ctx.boolean_parameter_values.get(key) {
        return Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(*v),
            span,
        });
    }
    if let Some(v) = ctx.string_parameter_values.get(key) {
        return Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String(v.clone()),
            span,
        });
    }
    ctx.enum_parameter_values
        .get(key)
        .map(|v| rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(v.clone()),
            subscripts: vec![],
            span,
        })
}

fn reference_has_array_shape(
    name: &rumoca_core::Reference,
    key: &str,
    ctx: &Context,
    scope: &str,
) -> bool {
    reference_key_has_array_shape(key, ctx, scope)
        || name
            .target_def_id()
            .and_then(|def_id| ctx.target_def_names.get(&def_id))
            .is_some_and(|target_name| reference_key_has_array_shape(target_name, ctx, scope))
}

fn reference_key_has_array_shape(key: &str, ctx: &Context, scope: &str) -> bool {
    ctx.array_dimensions
        .get(key)
        .is_some_and(|dims| !dims.is_empty())
        || scoped_lookup_candidates(key, scope)
            .into_iter()
            .any(|candidate| {
                ctx.array_dimensions
                    .get(&candidate)
                    .is_some_and(|dims| !dims.is_empty())
            })
}

fn substitute_scoped_scalar_var_ref(
    key: &str,
    span: rumoca_core::Span,
    env: ConstantSubstitutionEnv<'_>,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    for (candidate, candidate_scope) in scoped_lookup_candidates_with_scope(key, env.scope) {
        if candidate == key {
            continue;
        }
        if env.live_vars.contains(&candidate)
            || inline_index_base_is_live_or_local(&candidate, env.live_vars, env.locals)
        {
            return Ok(Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(candidate),
                subscripts: vec![],
                span,
            }));
        }
        let candidate_has_array_shape =
            reference_key_has_array_shape(&candidate, env.ctx, &candidate_scope);
        if !candidate_has_array_shape
            && let Some(literal) = evaluated_scalar_parameter_literal(&candidate, span, env.ctx)
        {
            return Ok(Some(literal));
        }
        if let Some(v) = resolve_constant_value_expr(&candidate, env.ctx) {
            if candidate_has_array_shape && !constant_expr_preserves_array_shape(v) {
                continue;
            }
            let candidate_env = env.with_scope(&candidate_scope);
            if expression_is_record_constructor(v)
                && let Some(expr) = resolve_projected_constant_path(&candidate, span, env.ctx)
            {
                return Ok(Some(substitute_known_constants_expr_with_options(
                    expr,
                    env.ctx,
                    env.live_vars,
                    env.locals,
                    &candidate_scope,
                    env.prefer_scoped_parameters,
                )?));
            }
            return Ok(Some(substitute_resolved_constant_expr(
                &candidate,
                v,
                span,
                candidate_env,
            )?));
        }
        if let Some(expr) = resolve_inline_indexed_constant(&candidate, span, env.ctx)? {
            return Ok(Some(expr));
        }
    }
    Ok(None)
}

fn resolve_projected_constant_path(
    name: &str,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    let path = rumoca_core::ComponentPath::from_flat_path(name);
    let parts = path.parts();
    if parts.len() < 2 {
        return None;
    }

    for split in (1..parts.len()).rev() {
        let prefix = path.prefix(split)?.to_flat_string();
        let Some(mut expr) = resolve_constant_value_expr(&prefix, ctx).cloned() else {
            continue;
        };
        let mut resolved = true;
        for field in &parts[split..] {
            let Some(field_expr) = resolve_field_on_constant_expr(&expr, field, span, ctx) else {
                resolved = false;
                break;
            };
            expr = field_expr;
        }
        if resolved {
            return Some(expr.with_span(span));
        }
    }

    None
}

fn inline_index_base_is_live_or_local(
    name: &str,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> bool {
    let Some((base, _indices)) = split_inline_indexed_name(name) else {
        return false;
    };
    live_vars.contains(base) || locals.contains(base)
}

fn reference_root_is_local(name: &rumoca_core::Reference, locals: &HashSet<String>) -> bool {
    if let Some(component_ref) = name.component_ref()
        && let Some(root) = component_ref.parts.first()
    {
        return locals.contains(root.ident.as_str());
    }

    rumoca_core::first_path_segment_without_index(name.as_str())
        .is_some_and(|root| locals.contains(root))
}

fn resolve_constant_value_expr<'a>(
    name: &str,
    ctx: &'a Context,
) -> Option<&'a rumoca_core::Expression> {
    let mut current = name.to_string();
    let mut visited = rustc_hash::FxHashSet::default();
    loop {
        if !visited.insert(current.clone()) {
            return None;
        }
        let expr = ctx.constant_values.get(&current)?;
        let rumoca_core::Expression::VarRef {
            name: alias_name,
            subscripts,
            ..
        } = expr
        else {
            return Some(expr);
        };
        if !subscripts.is_empty() || alias_name.as_str() == current {
            return Some(expr);
        }
        let alias_scope = parent_component_scope(&current);
        let Some(resolved_key) =
            resolve_constant_key_with_scope(alias_name.as_str(), &alias_scope, ctx)
        else {
            return Some(expr);
        };
        if resolved_key == current {
            return Some(expr);
        }
        current = resolved_key;
    }
}

fn resolve_constant_value_expr_for_ref<'a>(
    name: &rumoca_core::Reference,
    ctx: &'a Context,
) -> Option<&'a rumoca_core::Expression> {
    resolve_constant_value_expr(name.as_str(), ctx).or_else(|| {
        name.target_def_id()
            .and_then(|def_id| ctx.target_def_names.get(&def_id))
            .and_then(|target_name| resolve_constant_value_expr(target_name, ctx))
    })
}

fn resolve_constant_key_with_scope(name: &str, scope: &str, ctx: &Context) -> Option<String> {
    scoped_lookup_candidates_with_scope(name, scope)
        .into_iter()
        .map(|(candidate, _candidate_scope)| candidate)
        .find(|candidate| ctx.constant_values.contains_key(candidate))
}

fn resolve_varref_through_constant_aliases(
    name: &str,
    ctx: &Context,
    scope: &str,
) -> Option<String> {
    let mut current = name.to_string();
    let mut visited = rustc_hash::FxHashSet::default();
    loop {
        if !visited.insert(current.clone()) {
            return None;
        }

        let mut replaced = false;
        for (idx, ch) in current.char_indices().rev() {
            if ch != '.' {
                continue;
            }
            let prefix = &current[..idx];
            let suffix = &current[idx..];
            let alias_key = if ctx.constant_values.contains_key(prefix) {
                Some(prefix.to_string())
            } else {
                resolve_constant_key_with_scope(prefix, scope, ctx)
            };
            let Some(alias_key) = alias_key else {
                continue;
            };
            let Some(alias_expr) = ctx.constant_values.get(&alias_key) else {
                continue;
            };
            let rumoca_core::Expression::VarRef {
                name: alias_name,
                subscripts,
                ..
            } = alias_expr
            else {
                continue;
            };
            if !subscripts.is_empty() {
                continue;
            }
            let alias_scope = parent_component_scope(&alias_key);
            let alias_target =
                resolve_alias_target_with_scope(alias_name.as_str(), &alias_scope, ctx)
                    .or_else(|| resolve_alias_target_with_scope(alias_name.as_str(), scope, ctx))
                    .unwrap_or_else(|| alias_name.as_str().to_string());
            current = format!("{alias_target}{suffix}");
            replaced = true;
            break;
        }

        if !replaced {
            return if current == name { None } else { Some(current) };
        }
    }
}

fn resolve_alias_target_with_scope(name: &str, scope: &str, ctx: &Context) -> Option<String> {
    scoped_lookup_candidates_with_scope(name, scope)
        .into_iter()
        .map(|(candidate, _candidate_scope)| candidate)
        .find(|candidate| constant_key_or_prefix_exists(candidate, ctx))
}

fn constant_key_or_prefix_exists(name: &str, ctx: &Context) -> bool {
    ctx.constant_values.contains_key(name)
        || ctx.real_parameter_values.contains_key(name)
        || ctx.parameter_values.contains_key(name)
        || ctx.boolean_parameter_values.contains_key(name)
        || ctx.string_parameter_values.contains_key(name)
        || ctx.enum_parameter_values.contains_key(name)
        || map_has_key_prefix(&ctx.constant_values, name)
        || map_has_key_prefix(&ctx.real_parameter_values, name)
        || map_has_key_prefix(&ctx.parameter_values, name)
        || map_has_key_prefix(&ctx.boolean_parameter_values, name)
        || map_has_key_prefix(&ctx.string_parameter_values, name)
        || map_has_key_prefix(&ctx.enum_parameter_values, name)
}

fn map_has_key_prefix<T>(map: &rustc_hash::FxHashMap<String, T>, prefix: &str) -> bool {
    map.keys().any(|key| {
        key.strip_prefix(prefix)
            .is_some_and(|suffix| suffix.starts_with('.'))
    })
}

fn resolve_inline_indexed_constant(
    name: &str,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    let Some((base, indices)) = split_inline_indexed_name(name) else {
        return Ok(None);
    };
    let Some(base_expr) = resolve_constant_value_expr(base, ctx).cloned() else {
        return Ok(None);
    };
    let subscripts = indices
        .into_iter()
        .map(|index| {
            rumoca_core::Subscript::try_generated_expr(
                Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(index),
                    span,
                }),
                span,
                "flatten inline indexed constant",
            )
            .map_err(|err| FlattenError::missing_source_context(err.to_string()))
        })
        .collect::<Result<Vec<_>, FlattenError>>()?;
    Ok(Some(rumoca_core::Expression::Index {
        base: Box::new(base_expr),
        subscripts,
        span,
    }))
}

fn split_inline_indexed_name(name: &str) -> Option<(&str, Vec<i64>)> {
    let scalar = rumoca_core::parse_scalar_name(name)?;
    Some((scalar.base, scalar.indices))
}

fn named_constructor_arg<'a>(
    args: &'a [rumoca_core::Expression],
    field: &str,
) -> Option<&'a rumoca_core::Expression> {
    for arg in args {
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            ..
        } = arg
            && name.as_str().strip_prefix("__rumoca_named_arg__.") == Some(field)
        {
            return args.first();
        }
    }
    None
}

fn positional_constructor_arg_for_field<'a>(
    constructor_name: &str,
    args: &'a [rumoca_core::Expression],
    field: &str,
    ctx: &Context,
) -> Option<&'a rumoca_core::Expression> {
    let function = ctx.functions.get(constructor_name)?;
    if !function.is_constructor {
        return None;
    }
    let index = function
        .inputs
        .iter()
        .position(|input| input.name == field)?;
    args.iter()
        .filter(|arg| !is_named_constructor_arg(arg))
        .nth(index)
}

fn is_named_constructor_arg(arg: &rumoca_core::Expression) -> bool {
    matches!(
        arg,
        rumoca_core::Expression::FunctionCall { name, .. }
            if name.as_str().starts_with(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)
    )
}

fn resolve_constant_field_access(
    base_name: &str,
    field: &str,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    let mut current = base_name.to_string();
    let mut visited = rustc_hash::FxHashSet::default();
    loop {
        if !visited.insert(current.clone()) {
            return None;
        }
        let key = format!("{}.{}", current, field);
        if let Some(value) = ctx.constant_values.get(&key) {
            return Some(value.clone().with_span(span));
        }
        if let Some(value) = ctx.real_parameter_values.get(&key)
            && value.is_finite()
        {
            return Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(*value),
                span,
            });
        }
        if let Some(value) = ctx.parameter_values.get(&key) {
            return Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(*value),
                span,
            });
        }
        if let Some(value) = ctx.boolean_parameter_values.get(&key) {
            return Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(*value),
                span,
            });
        }
        if let Some(value) = ctx.string_parameter_values.get(&key) {
            return Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String(value.clone()),
                span,
            });
        }
        if let Some(value) = ctx.enum_parameter_values.get(&key) {
            return Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(value.clone()),
                subscripts: vec![],
                span,
            });
        }

        let alias_expr = ctx.constant_values.get(&current)?;
        let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = alias_expr
        else {
            return None;
        };
        if !subscripts.is_empty() {
            return None;
        }
        current = name.as_str().to_string();
    }
}

fn substitute_known_constants_statement(
    statement: &mut rumoca_core::Statement,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
) -> Result<(), FlattenError> {
    *statement = KnownConstantSubstituter {
        env: ConstantSubstitutionEnv {
            ctx,
            live_vars,
            locals,
            scope,
            prefer_scoped_parameters: false,
            var_dims: None,
            var_values: None,
            resolving_value: None,
        },
    }
    .rewrite_statement(statement)?;
    Ok(())
}

fn substitute_known_constants_when_equation(
    equation: &mut flat::WhenEquation,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> Result<(), FlattenError> {
    let substitute = |expr: &mut rumoca_core::Expression| {
        *expr = substitute_known_constants_expr(expr.clone(), ctx, live_vars, locals, "")?;
        Ok::<(), FlattenError>(())
    };
    match equation {
        flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
            substitute(value)?;
        }
        flat::WhenEquation::Assert {
            condition, message, ..
        } => {
            substitute(condition)?;
            substitute(message)?;
        }
        flat::WhenEquation::Terminate { message, .. } => {
            substitute(message)?;
        }
        flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (condition, equations) in branches {
                substitute(condition)?;
                for nested in equations {
                    substitute_known_constants_when_equation(nested, ctx, live_vars, locals)?;
                }
            }
            for nested in else_branch {
                substitute_known_constants_when_equation(nested, ctx, live_vars, locals)?;
            }
        }
        flat::WhenEquation::FunctionCallOutputs { function, .. } => {
            substitute(function)?;
        }
    }
    Ok(())
}

#[cfg(test)]
#[path = "postprocess_record_alias_tests.rs"]
mod record_alias_postprocess_tests;

#[cfg(test)]
mod substitute_constant_tests;
