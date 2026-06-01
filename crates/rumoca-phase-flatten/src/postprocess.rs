use super::*;
use rumoca_core::{ExpressionRewriter, ExpressionVisitor, StatementRewriter};

#[path = "postprocess_record_alias.rs"]
mod record_alias;
use record_alias::*;

pub(super) fn canonicalize_varrefs_via_record_aliases(flat: &mut flat::Model, ctx: &Context) {
    if ctx.record_aliases.is_empty() {
        return;
    }
    let known_variables: HashSet<String> = flat.variables.keys().map(ToString::to_string).collect();
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

fn record_alias_rewrite_name(
    name: &str,
    ctx: &Context,
    known_variables: &HashSet<String>,
) -> Option<String> {
    let name_path = rumoca_core::ComponentPath::from_flat_path(name);
    ctx.record_aliases.iter().find_map(|(alias, target)| {
        if !name_path.starts_with(alias) || name_path.len() == alias.len() {
            return None;
        }
        let suffix = name_path
            .suffix_from(alias.len())
            .expect("suffix index is in range");
        let candidate = target.join(&suffix).to_flat_string();
        known_variables.contains(&candidate).then_some(candidate)
    })
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
                rumoca_ir_flat::WhenEquation::Assert { condition, .. } => {
                    self.mark_expr(condition);
                }
                rumoca_ir_flat::WhenEquation::Conditional {
                    branches,
                    else_branch,
                    ..
                } => self.mark_conditional_when_equation(branches, else_branch),
                rumoca_ir_flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                    self.mark_expr(function);
                }
                rumoca_ir_flat::WhenEquation::Terminate { .. } => {}
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
        || current
            .iter()
            .zip(recovered.iter())
            .any(|(current, recovered)| *recovered > *current)
}

fn collapse_index_when_equations(
    equations: &mut [rumoca_ir_flat::WhenEquation],
    known_flat_vars: &HashSet<String>,
) {
    for equation in equations {
        match equation {
            rumoca_ir_flat::WhenEquation::Assign { value, .. }
            | rumoca_ir_flat::WhenEquation::Reinit { value, .. } => {
                collapse_index_expr(value, known_flat_vars);
            }
            rumoca_ir_flat::WhenEquation::Assert { condition, .. } => {
                collapse_index_expr(condition, known_flat_vars);
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
            rumoca_ir_flat::WhenEquation::Terminate { .. } => {}
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

pub(super) fn substitute_known_constants_in_flat(flat: &mut flat::Model, ctx: &Context) {
    let live_vars: rustc_hash::FxHashSet<String> = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();
    let no_locals: HashSet<String> = HashSet::new();

    for eq in &mut flat.equations {
        let scope = equation_origin_scope(&eq.origin);
        eq.residual = substitute_known_constants_expr(
            eq.residual.clone(),
            ctx,
            &live_vars,
            &no_locals,
            &scope,
        );
    }
    for eq in &mut flat.initial_equations {
        let scope = equation_origin_scope(&eq.origin);
        eq.residual = substitute_known_constants_expr(
            eq.residual.clone(),
            ctx,
            &live_vars,
            &no_locals,
            &scope,
        );
    }
    substitute_assert_equations(&mut flat.assert_equations, ctx, &live_vars, &no_locals);
    substitute_assert_equations(
        &mut flat.initial_assert_equations,
        ctx,
        &live_vars,
        &no_locals,
    );
    for when_clause in &mut flat.when_clauses {
        when_clause.condition = substitute_known_constants_expr(
            when_clause.condition.clone(),
            ctx,
            &live_vars,
            &no_locals,
            "",
        );
        for equation in &mut when_clause.equations {
            substitute_known_constants_when_equation(equation, ctx, &live_vars, &no_locals);
        }
    }
    substitute_algorithms(&mut flat.algorithms, ctx, &live_vars, &no_locals);
    substitute_algorithms(&mut flat.initial_algorithms, ctx, &live_vars, &no_locals);
    substitute_variable_annotations(&mut flat.variables, ctx, &live_vars, &no_locals);
    substitute_function_bodies(&mut flat.functions, ctx, &live_vars);
    materialize_referenced_zero_sized_array_variables(flat, ctx);
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
) {
    for assert_eq in equations {
        assert_eq.condition = substitute_known_constants_expr(
            assert_eq.condition.clone(),
            ctx,
            live_vars,
            locals,
            "",
        );
        assert_eq.message =
            substitute_known_constants_expr(assert_eq.message.clone(), ctx, live_vars, locals, "");
        substitute_opt_expr(&mut assert_eq.level, ctx, live_vars, locals, "");
    }
}

fn substitute_algorithms(
    algorithms: &mut [flat::Algorithm],
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) {
    for algorithm in algorithms {
        for statement in &mut algorithm.statements {
            substitute_known_constants_statement(statement, ctx, live_vars, locals, "");
        }
    }
}

fn substitute_variable_annotations(
    variables: &mut flat::VarNameIndexMap<flat::Variable>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) {
    for var in variables.values_mut() {
        let scope = parent_component_scope(var.name.as_str());
        substitute_opt_expr(&mut var.binding, ctx, live_vars, locals, &scope);
        substitute_opt_expr(&mut var.start, ctx, live_vars, locals, &scope);
        substitute_opt_expr(&mut var.min, ctx, live_vars, locals, &scope);
        substitute_opt_expr(&mut var.max, ctx, live_vars, locals, &scope);
        substitute_opt_expr(&mut var.nominal, ctx, live_vars, locals, &scope);
    }
}

fn substitute_function_bodies(
    functions: &mut flat::VarNameIndexMap<rumoca_core::Function>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
) {
    for function in functions.values_mut() {
        let function_locals: HashSet<String> = function
            .inputs
            .iter()
            .chain(function.outputs.iter())
            .chain(function.locals.iter())
            .map(|param| param.name.clone())
            .collect();
        let function_scope = crate::path_utils::parent_scope(function.name.as_str()).unwrap_or("");

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
            );
        }
        for statement in &mut function.body {
            substitute_known_constants_statement(
                statement,
                ctx,
                live_vars,
                &function_locals,
                function_scope,
            );
        }
    }
}

fn substitute_opt_expr(
    expr: &mut Option<rumoca_core::Expression>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
) {
    if let Some(expr) = expr {
        *expr = substitute_known_constants_expr(expr.clone(), ctx, live_vars, locals, scope);
    }
}

pub(crate) fn substitute_known_constants_expr(
    expr: rumoca_core::Expression,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
) -> rumoca_core::Expression {
    KnownConstantSubstituter {
        ctx,
        live_vars,
        locals,
        scope,
    }
    .rewrite_expression(&expr)
}

struct KnownConstantSubstituter<'a> {
    ctx: &'a Context,
    live_vars: &'a rustc_hash::FxHashSet<String>,
    locals: &'a HashSet<String>,
    scope: &'a str,
}

impl ExpressionRewriter for KnownConstantSubstituter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        match expr {
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
            } => self.rewrite_var_ref(name, subscripts, *span),
            rumoca_core::Expression::FieldAccess { base, field, span } => {
                self.rewrite_field_access(base, field, *span)
            }
            other => self.walk_expression(other),
        }
    }

    fn rewrite_subscript(&mut self, subscript: &rumoca_core::Subscript) -> rumoca_core::Subscript {
        match subscript {
            rumoca_core::Subscript::Expr { expr, span } => rumoca_core::Subscript::Expr {
                expr: Box::new(self.rewrite_expression(expr)),
                span: *span,
            },
            other => other.clone(),
        }
    }
}

impl KnownConstantSubstituter<'_> {
    fn rewrite_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if subscripts.is_empty() {
            if let Some(replaced) = substitute_scalar_var_ref(
                name,
                span,
                self.ctx,
                self.live_vars,
                self.locals,
                self.scope,
            ) {
                return replaced;
            }
            return rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: vec![],
                span,
            };
        }

        let rewritten_subscripts = self.rewrite_subscripts(subscripts);
        if self.locals.contains(name.as_str()) {
            return rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: rewritten_subscripts,
                span,
            };
        }
        if let Some(replaced) = substitute_indexed_constant_var_ref(
            name,
            rewritten_subscripts.clone(),
            span,
            self.ctx,
            self.live_vars,
        ) {
            return replaced;
        }

        rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: rewritten_subscripts,
            span,
        }
    }

    fn rewrite_field_access(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let rewritten_base = self.rewrite_expression(base);
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            ..
        } = &rewritten_base
        {
            if let Some(named_arg) = named_constructor_arg(args, field) {
                return named_arg.clone().with_span(span);
            }
            if args.is_empty()
                && let Some(resolved) =
                    resolve_constant_field_access(name.as_str(), field, span, self.ctx)
            {
                return resolved;
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
                self.ctx,
                self.live_vars,
            )
        {
            return self.rewrite_expression(&resolved);
        }
        if let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = &rewritten_base
            && subscripts.is_empty()
            && !self.live_vars.contains(name.as_str())
            && !reference_root_is_local(name, self.locals)
            && let Some(resolved) =
                resolve_constant_field_access(name.as_str(), field, span, self.ctx)
        {
            return resolved;
        }
        rumoca_core::Expression::FieldAccess {
            base: Box::new(rewritten_base),
            field: field.to_string(),
            span,
        }
    }
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
            let index = literal_integer(expr)?;
            select_constant_index_element(&base, index, rest, span, ctx)
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

fn select_constant_index_element(
    base: &rumoca_core::Expression,
    index: i64,
    rest: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    let rumoca_core::Expression::Array { elements, .. } = base else {
        return None;
    };
    let zero_based = usize::try_from(index.checked_sub(1)?).ok()?;
    let element = elements.get(zero_based)?;
    select_constant_index(element, rest, span, ctx)
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

impl StatementRewriter for KnownConstantSubstituter<'_> {}

fn substitute_indexed_constant_var_ref(
    name: &rumoca_core::Reference,
    subscripts: Vec<rumoca_core::Subscript>,
    span: rumoca_core::Span,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
) -> Option<rumoca_core::Expression> {
    if live_vars.contains(name.as_str()) {
        return None;
    }

    let constant_expr = resolve_constant_value_expr_for_ref(name, ctx)?.clone();
    Some(rumoca_core::Expression::Index {
        base: Box::new(constant_expr),
        subscripts,
        span,
    })
}

fn substitute_scalar_var_ref(
    name: &rumoca_core::Reference,
    span: rumoca_core::Span,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
) -> Option<rumoca_core::Expression> {
    let key = name.as_str();
    if live_vars.contains(key) || reference_root_is_local(name, locals) {
        return None;
    }
    if inline_index_base_is_live_or_local(key, live_vars, locals) {
        return None;
    }
    let has_array_shape = reference_has_array_shape(name, key, ctx, scope);
    if let Some(v) = resolve_constant_value_expr_for_ref(name, ctx) {
        if has_array_shape && !constant_expr_preserves_array_shape(v) {
            return None;
        }
        return Some(substitute_resolved_constant_expr(
            key, v, span, ctx, live_vars, locals, scope,
        ));
    }
    if !has_array_shape && let Some(literal) = scalar_parameter_literal(key, span, ctx) {
        return Some(literal);
    }
    if let Some(expr) = resolve_inline_indexed_constant(key, span, ctx) {
        return Some(expr);
    }
    if let Some(expr) = resolve_projected_constant_path(key, span, ctx) {
        return Some(substitute_known_constants_expr(
            expr, ctx, live_vars, locals, scope,
        ));
    }
    if !scope.is_empty()
        && let Some(expr) =
            substitute_scoped_scalar_var_ref(key, span, ctx, live_vars, locals, scope)
    {
        return Some(expr);
    }

    if let Some(resolved_key) = resolve_varref_through_constant_aliases(key, ctx, scope)
        && resolved_key != key
    {
        if let Some(v) = resolve_constant_value_expr(&resolved_key, ctx) {
            if reference_key_has_array_shape(&resolved_key, ctx, scope)
                && !constant_expr_preserves_array_shape(v)
            {
                return None;
            }
            return Some(substitute_resolved_constant_expr(
                &resolved_key,
                v,
                span,
                ctx,
                live_vars,
                locals,
                scope,
            ));
        }
        if !reference_key_has_array_shape(&resolved_key, ctx, scope)
            && let Some(literal) = scalar_parameter_literal(&resolved_key, span, ctx)
        {
            return Some(literal);
        }
        if let Some(expr) = resolve_inline_indexed_constant(&resolved_key, span, ctx) {
            return Some(expr);
        }
        return Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(resolved_key),
            subscripts: vec![],
            span,
        });
    }

    None
}

fn substitute_resolved_constant_expr(
    key: &str,
    expr: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    fallback_scope: &str,
) -> rumoca_core::Expression {
    let declaration_scope = parent_component_scope(key);
    let scope = if declaration_scope.is_empty() {
        fallback_scope
    } else {
        &declaration_scope
    };
    substitute_known_constants_expr(expr.clone().with_span(span), ctx, live_vars, locals, scope)
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

fn materialize_referenced_zero_sized_array_variables(flat: &mut flat::Model, ctx: &Context) {
    let mut collector = MissingZeroSizedArrayRefCollector {
        flat,
        ctx,
        refs: Vec::new(),
    };
    collector.collect();

    for name in collector.refs {
        let Some(dims) = zero_sized_array_dims_for_ref(&name, ctx) else {
            continue;
        };
        let var_name = name.var_name().clone();
        if flat.variables.contains_key(&var_name) {
            continue;
        }
        let variable = flat::Variable {
            name: var_name.clone(),
            component_ref: name.component_ref().cloned(),
            dims,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            is_primitive: true,
            is_protected: true,
            ..Default::default()
        };
        flat.variable_type_names
            .entry(var_name.clone())
            .or_insert_with(|| "Real".to_string());
        flat.add_variable(var_name, variable);
    }
}

struct MissingZeroSizedArrayRefCollector<'a> {
    flat: &'a flat::Model,
    ctx: &'a Context,
    refs: Vec<rumoca_core::Reference>,
}

impl MissingZeroSizedArrayRefCollector<'_> {
    fn collect(&mut self) {
        for equation in &self.flat.equations {
            self.visit_expression(&equation.residual);
        }
        for equation in &self.flat.initial_equations {
            self.visit_expression(&equation.residual);
        }
        for assertion in &self.flat.assert_equations {
            self.visit_expression(&assertion.condition);
            self.visit_expression(&assertion.message);
            if let Some(level) = &assertion.level {
                self.visit_expression(level);
            }
        }
        for assertion in &self.flat.initial_assert_equations {
            self.visit_expression(&assertion.condition);
            self.visit_expression(&assertion.message);
            if let Some(level) = &assertion.level {
                self.visit_expression(level);
            }
        }
    }
}

impl rumoca_core::ExpressionVisitor for MissingZeroSizedArrayRefCollector<'_> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        if subscripts.is_empty()
            && !self.flat.variables.contains_key(name.var_name())
            && zero_sized_array_dims_for_ref(name, self.ctx).is_some()
        {
            self.refs.push(name.clone());
        }
        self.walk_var_ref(name, subscripts);
    }
}

fn zero_sized_array_dims_for_ref(name: &rumoca_core::Reference, ctx: &Context) -> Option<Vec<i64>> {
    let dims = ctx.array_dimensions.get(name.as_str()).or_else(|| {
        name.target_def_id()
            .and_then(|def_id| ctx.target_def_names.get(&def_id))
            .and_then(|target_name| ctx.array_dimensions.get(target_name))
    })?;
    (!dims.is_empty() && dims.iter().any(|dim| *dim <= 0)).then(|| dims.clone())
}

fn scalar_parameter_literal(
    key: &str,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    if let Some(v) = ctx.real_parameter_values.get(key)
        && v.is_finite()
    {
        return Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(*v),
            span,
        });
    }
    if let Some(v) = ctx.parameter_values.get(key) {
        return Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(*v),
            span,
        });
    }
    if let Some(v) = ctx.boolean_parameter_values.get(key) {
        return Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(*v),
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
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
) -> Option<rumoca_core::Expression> {
    for (candidate, candidate_scope) in scoped_lookup_candidates_with_scope(key, scope) {
        if candidate == key {
            continue;
        }
        if let Some(v) = resolve_constant_value_expr(&candidate, ctx) {
            if reference_key_has_array_shape(&candidate, ctx, &candidate_scope)
                && !constant_expr_preserves_array_shape(v)
            {
                continue;
            }
            return Some(substitute_resolved_constant_expr(
                &candidate,
                v,
                span,
                ctx,
                live_vars,
                locals,
                &candidate_scope,
            ));
        }
        if !reference_key_has_array_shape(&candidate, ctx, &candidate_scope)
            && let Some(literal) = scalar_parameter_literal(&candidate, span, ctx)
        {
            return Some(literal);
        }
        if let Some(expr) = resolve_inline_indexed_constant(&candidate, span, ctx) {
            return Some(expr);
        }
    }
    None
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
        || ctx.enum_parameter_values.contains_key(name)
        || map_has_key_prefix(&ctx.constant_values, name)
        || map_has_key_prefix(&ctx.real_parameter_values, name)
        || map_has_key_prefix(&ctx.parameter_values, name)
        || map_has_key_prefix(&ctx.boolean_parameter_values, name)
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
) -> Option<rumoca_core::Expression> {
    let (base, indices) = split_inline_indexed_name(name)?;
    let base_expr = resolve_constant_value_expr(base, ctx)?.clone();
    Some(rumoca_core::Expression::Index {
        base: Box::new(base_expr),
        subscripts: indices
            .into_iter()
            .map(|index| {
                rumoca_core::Subscript::generated_expr(Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(index),
                    span,
                }))
            })
            .collect(),
        span,
    })
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
) {
    *statement = KnownConstantSubstituter {
        ctx,
        live_vars,
        locals,
        scope,
    }
    .rewrite_statement(statement);
}

fn substitute_known_constants_when_equation(
    equation: &mut flat::WhenEquation,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) {
    match equation {
        flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
            *value = substitute_known_constants_expr(value.clone(), ctx, live_vars, locals, "");
        }
        flat::WhenEquation::Assert { condition, .. } => {
            *condition =
                substitute_known_constants_expr(condition.clone(), ctx, live_vars, locals, "");
        }
        flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (condition, equations) in branches {
                *condition =
                    substitute_known_constants_expr(condition.clone(), ctx, live_vars, locals, "");
                for nested in equations {
                    substitute_known_constants_when_equation(nested, ctx, live_vars, locals);
                }
            }
            for nested in else_branch {
                substitute_known_constants_when_equation(nested, ctx, live_vars, locals);
            }
        }
        flat::WhenEquation::FunctionCallOutputs { function, .. } => {
            *function =
                substitute_known_constants_expr(function.clone(), ctx, live_vars, locals, "");
        }
        flat::WhenEquation::Terminate { .. } => {}
    }
}

/// Drop FieldAccess bindings whose targets don't exist in the flat model.
///
/// During modifier propagation, record bindings like `x = someRecord.field` may reference
/// internal component structure that was eliminated during flattening. These dangling
/// FieldAccess bindings would cause incorrect equation generation in todae if kept.
pub(super) fn drop_invalid_field_access_bindings(flat: &mut flat::Model) {
    use std::collections::HashSet;

    // Collect the set of all variable names for lookup
    let all_names: HashSet<rumoca_core::VarName> = flat.variables.keys().cloned().collect();

    // Find variables with FieldAccess bindings pointing to non-existent targets
    let to_clear: Vec<rumoca_core::VarName> = flat
        .variables
        .iter()
        .filter_map(|(name, var)| {
            let binding = var.binding.as_ref()?;
            let target_name = field_access_target_name(binding)?;
            if all_names.contains(&target_name)
                || !field_access_targets_flat_namespace(binding, &all_names)
            {
                None
            } else {
                Some(name.clone())
            }
        })
        .collect();

    for name in &to_clear {
        if let Some(var) = flat.variables.get_mut(name) {
            var.binding = None;
        }
    }
}

fn field_access_target_name(expr: &rumoca_core::Expression) -> Option<rumoca_core::VarName> {
    let rumoca_core::Expression::FieldAccess { .. } = expr else {
        return None;
    };
    rendered_component_target(expr).map(rumoca_core::VarName::new)
}

fn rendered_component_target(expr: &rumoca_core::Expression) -> Option<String> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            let mut rendered = name.as_str().to_string();
            append_flat_subscripts(&mut rendered, subscripts)?;
            Some(rendered)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let mut rendered = rendered_component_target(base)?;
            append_flat_subscripts(&mut rendered, subscripts)?;
            Some(rendered)
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            Some(format!("{}.{}", rendered_component_target(base)?, field))
        }
        _ => None,
    }
}

fn field_access_targets_flat_namespace(
    expr: &rumoca_core::Expression,
    all_names: &HashSet<rumoca_core::VarName>,
) -> bool {
    field_access_base_target(expr)
        .into_iter()
        .chain(leftmost_reference_target(expr))
        .any(|target| flat_namespace_contains(all_names, &target))
}

fn field_access_base_target(expr: &rumoca_core::Expression) -> Option<String> {
    let rumoca_core::Expression::FieldAccess { base, .. } = expr else {
        return None;
    };
    rendered_component_target(base)
}

fn leftmost_reference_target(expr: &rumoca_core::Expression) -> Option<String> {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => Some(name.as_str().to_string()),
        rumoca_core::Expression::Index { base, .. }
        | rumoca_core::Expression::FieldAccess { base, .. } => leftmost_reference_target(base),
        _ => None,
    }
}

fn flat_namespace_contains(all_names: &HashSet<rumoca_core::VarName>, target: &str) -> bool {
    let exact = rumoca_core::VarName::new(target);
    all_names.contains(&exact)
        || all_names.iter().any(|name| {
            let name = name.as_str();
            name.strip_prefix(target)
                .is_some_and(|suffix| suffix.starts_with('.') || suffix.starts_with('['))
        })
}

fn append_flat_subscripts(
    rendered: &mut String,
    subscripts: &[rumoca_core::Subscript],
) -> Option<()> {
    for subscript in subscripts {
        match subscript {
            rumoca_core::Subscript::Index { value, .. } => {
                rendered.push('[');
                rendered.push_str(&value.to_string());
                rendered.push(']');
            }
            rumoca_core::Subscript::Expr { expr, .. } => {
                let value = constant_integer_bound(expr)?;
                rendered.push('[');
                rendered.push_str(&value.to_string());
                rendered.push(']');
            }
            rumoca_core::Subscript::Colon { .. } => return None,
        }
    }
    Some(())
}

#[cfg(test)]
#[path = "postprocess_record_alias_tests.rs"]
mod record_alias_postprocess_tests;

#[cfg(test)]
mod substitute_constant_tests;
