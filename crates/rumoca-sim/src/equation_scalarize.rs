use std::collections::HashMap;

use rumoca_ir_dae as dae;

use crate::projection_maps::{
    build_component_index_projection_map, build_function_output_projection_map, output_scalar_count,
};

type Dae = dae::Dae;
type Equation = dae::Equation;
type Expression = dae::Expression;
type Literal = dae::Literal;
type OpBinary = rumoca_ir_core::OpBinary;
type OpUnary = rumoca_ir_core::OpUnary;
type Subscript = dae::Subscript;
type VarName = dae::VarName;

/// Build output variable names in solver-vector order (states, algebraics, outputs).
///
/// Array variables are expanded to `name[1]`, `name[2]`, etc.
pub fn build_output_names(dae: &Dae) -> Vec<String> {
    let mut names = Vec::new();
    for (name, var) in dae
        .states
        .iter()
        .chain(dae.algebraics.iter())
        .chain(dae.outputs.iter())
    {
        let sz = var.size();
        if sz <= 1 {
            names.push(name.as_str().to_string());
        } else {
            for i in 1..=sz {
                names.push(format!("{}[{}]", name.as_str(), i));
            }
        }
    }
    names
}

/// Collect variable dimensions from all variable categories.
pub fn build_var_dims_map(dae: &Dae) -> HashMap<String, Vec<i64>> {
    let mut map = HashMap::new();
    for (name, var) in dae
        .states
        .iter()
        .chain(dae.algebraics.iter())
        .chain(dae.outputs.iter())
        .chain(dae.parameters.iter())
        .chain(dae.constants.iter())
        .chain(dae.inputs.iter())
    {
        if !var.dims.is_empty() {
            map.insert(name.as_str().to_string(), var.dims.clone());
        }
    }
    map
}

/// Build a map from a Complex-record base name to concrete `base.re` / `base.im`
/// scalar variable names present in the DAE.
pub fn build_complex_field_map(dae: &Dae) -> HashMap<String, [Option<String>; 2]> {
    let mut map: HashMap<String, [Option<String>; 2]> = HashMap::new();
    for (name, _) in dae
        .states
        .iter()
        .chain(dae.algebraics.iter())
        .chain(dae.outputs.iter())
        .chain(dae.parameters.iter())
        .chain(dae.constants.iter())
        .chain(dae.inputs.iter())
    {
        let raw = name.as_str();
        let Some((base, field)) = raw.rsplit_once('.') else {
            continue;
        };
        let slot = match field {
            "re" => 0,
            "im" => 1,
            _ => continue,
        };
        map.entry(base.to_string()).or_insert([None, None])[slot] = Some(raw.to_string());
    }
    map
}

/// Recursively index into an expression tree at 1-based Modelica index `i`.
///
/// - `Array { elements }` → return `elements[i-1]`
/// - `VarRef { name, subscripts: [] }` for array vars → add `Subscript::Index(i)`
/// - `FunctionCall { is_constructor: true }` → project positional constructor arg `i`
/// - `Binary/Unary/BuiltinCall/If/FunctionCall/Index` → recurse into children
/// - Scalars (Literal, etc.) → broadcast unchanged
pub struct IndexProjectionContext<'a> {
    i: usize,
    var_dims: &'a HashMap<String, Vec<i64>>,
    complex_fields: &'a HashMap<String, [Option<String>; 2]>,
    component_index_map: &'a HashMap<String, HashMap<usize, String>>,
    function_output_index_map: &'a HashMap<String, HashMap<usize, String>>,
}

impl<'a> IndexProjectionContext<'a> {
    fn map_exprs(&self, exprs: &[Expression]) -> Vec<Expression> {
        exprs.iter().map(|expr| self.project(expr)).collect()
    }

    fn project_var_ref(
        &self,
        name: &VarName,
        subscripts: &[Subscript],
        fallback: &Expression,
    ) -> Expression {
        if !subscripts.is_empty() {
            return fallback.clone();
        }

        if let Some(dims) = self.var_dims.get(name.as_str()) {
            let scalar_count = output_scalar_count(dims);
            if scalar_count > 1 && self.i <= scalar_count {
                return Expression::VarRef {
                    name: name.clone(),
                    subscripts: vec![Subscript::Index(self.i as i64)],
                };
            }
            return fallback.clone();
        }

        if let Some(fields) = self.complex_fields.get(name.as_str()) {
            let projected = match self.i {
                1 => fields[0].as_ref(),
                2 => fields[1].as_ref(),
                _ => None,
            };
            if let Some(projected_name) = projected {
                return Expression::VarRef {
                    name: VarName::new(projected_name.clone()),
                    subscripts: vec![],
                };
            }
        }

        if let Some(by_index) = self.component_index_map.get(name.as_str())
            && let Some(projected_name) = by_index.get(&self.i)
        {
            return Expression::VarRef {
                name: VarName::new(projected_name.clone()),
                subscripts: vec![],
            };
        }

        fallback.clone()
    }

    fn project(&self, expr: &Expression) -> Expression {
        match expr {
            Expression::Array {
                elements,
                is_matrix,
            } => project_array_literal_scalar(elements, *is_matrix, self.i)
                .unwrap_or_else(|| expr.clone()),
            Expression::VarRef { name, subscripts } => self.project_var_ref(name, subscripts, expr),
            Expression::Binary { op, lhs, rhs } => Expression::Binary {
                op: op.clone(),
                lhs: Box::new(self.project(lhs)),
                rhs: Box::new(self.project(rhs)),
            },
            Expression::Unary { op, rhs } => Expression::Unary {
                op: op.clone(),
                rhs: Box::new(self.project(rhs)),
            },
            Expression::BuiltinCall { function, args } => Expression::BuiltinCall {
                function: *function,
                args: self.map_exprs(args),
            },
            Expression::If {
                branches,
                else_branch,
            } => Expression::If {
                branches: branches
                    .iter()
                    .map(|(cond, val)| (cond.clone(), self.project(val)))
                    .collect(),
                else_branch: Box::new(self.project(else_branch)),
            },
            Expression::FunctionCall {
                name,
                args,
                is_constructor,
            } => {
                if *is_constructor && self.i >= 1 && self.i <= args.len() {
                    return self.project(&args[self.i - 1]);
                }
                if let Some(by_index) = self.function_output_index_map.get(name.as_str())
                    && let Some(projected_output) = by_index.get(&self.i)
                {
                    return Expression::FunctionCall {
                        name: VarName::new(format!("{}.{}", name.as_str(), projected_output)),
                        args: self.map_exprs(args),
                        is_constructor: false,
                    };
                }
                Expression::FunctionCall {
                    name: name.clone(),
                    args: self.map_exprs(args),
                    is_constructor: *is_constructor,
                }
            }
            Expression::Index { base, subscripts } => Expression::Index {
                base: Box::new(self.project(base)),
                subscripts: subscripts.clone(),
            },
            _ => expr.clone(),
        }
    }
}

fn project_array_literal_scalar(
    elements: &[Expression],
    is_matrix: bool,
    scalar_index: usize,
) -> Option<Expression> {
    if scalar_index == 0 {
        return None;
    }
    if !is_matrix {
        return elements.get(scalar_index - 1).cloned();
    }

    let first = elements.first()?;
    let Expression::Array {
        elements: first_row,
        ..
    } = first
    else {
        // MLS §10.4: a single-row matrix literal is encoded as `is_matrix=true`
        // with scalar elements. Preserve the written row order on the compiled
        // scalarization path so it matches interpreted array evaluation.
        return elements.get(scalar_index - 1).cloned();
    };
    let cols = first_row.len();
    if cols == 0 {
        return None;
    }

    let flat = scalar_index - 1;
    let row = flat / cols;
    let col = flat % cols;
    let Expression::Array {
        elements: row_elements,
        ..
    } = elements.get(row)?
    else {
        return None;
    };
    row_elements.get(col).cloned()
}

pub fn index_into_expr(
    expr: &Expression,
    i: usize,
    var_dims: &HashMap<String, Vec<i64>>,
    complex_fields: &HashMap<String, [Option<String>; 2]>,
    component_index_map: &HashMap<String, HashMap<usize, String>>,
    function_output_index_map: &HashMap<String, HashMap<usize, String>>,
) -> Expression {
    IndexProjectionContext {
        i,
        var_dims,
        complex_fields,
        component_index_map,
        function_output_index_map,
    }
    .project(expr)
}

pub struct ExpressionScalarizationContext {
    var_dims: HashMap<String, Vec<i64>>,
    complex_fields: HashMap<String, [Option<String>; 2]>,
    component_index_map: HashMap<String, HashMap<usize, String>>,
    function_output_index_map: HashMap<String, HashMap<usize, String>>,
}

pub fn build_expression_scalarization_context(dae: &Dae) -> ExpressionScalarizationContext {
    ExpressionScalarizationContext {
        var_dims: build_var_dims_map(dae),
        complex_fields: build_complex_field_map(dae),
        component_index_map: build_component_index_projection_map(dae),
        function_output_index_map: build_function_output_projection_map(dae),
    }
}

pub fn scalarize_expression_rows(
    expr: &Expression,
    output_len: usize,
    ctx: &ExpressionScalarizationContext,
) -> Vec<Expression> {
    if output_len <= 1 {
        return vec![expr.clone()];
    }

    (1..=output_len)
        .map(|index| {
            index_into_expr(
                expr,
                index,
                &ctx.var_dims,
                &ctx.complex_fields,
                &ctx.component_index_map,
                &ctx.function_output_index_map,
            )
        })
        .collect()
}

pub fn scalar_targets_for_lhs(lhs: &str, scalar_names: &[String]) -> Vec<String> {
    let dotted_prefix = format!("{lhs}.");
    let indexed_prefix = format!("{lhs}[");
    scalar_names
        .iter()
        .filter(|name| {
            let raw = name.as_str();
            raw == lhs || raw.starts_with(&dotted_prefix) || raw.starts_with(&indexed_prefix)
        })
        .cloned()
        .collect()
}

pub fn scalarization_subscript_text(sub: &Subscript) -> Option<String> {
    match sub {
        Subscript::Index(i) => Some(i.to_string()),
        Subscript::Expr(expr) => match expr.as_ref() {
            Expression::Literal(Literal::Integer(i)) => Some(i.to_string()),
            Expression::Literal(Literal::Real(v)) if v.is_finite() && v.fract() == 0.0 => {
                Some((*v as i64).to_string())
            }
            _ => None,
        },
        _ => None,
    }
}

pub fn scalarization_var_ref_name(name: &VarName, subscripts: &[Subscript]) -> Option<String> {
    if subscripts.is_empty() {
        return Some(name.as_str().to_string());
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        indices.push(scalarization_subscript_text(sub)?);
    }
    Some(format!("{}[{}]", name.as_str(), indices.join(",")))
}

pub fn residual_lhs_target_name(expr: &Expression) -> Option<String> {
    let Expression::Binary {
        op: OpBinary::Sub(_),
        lhs,
        ..
    } = expr
    else {
        return None;
    };
    if let Expression::VarRef { name, subscripts } = lhs.as_ref() {
        return scalarization_var_ref_name(name, subscripts);
    }
    None
}

pub fn parse_one_based_index(text: &str) -> Option<usize> {
    let idx = text.trim().parse::<usize>().ok()?;
    (idx > 0).then_some(idx)
}

pub fn parse_complex_field_selector(fragment: &str) -> Option<(usize, Option<usize>)> {
    let (field_name, maybe_index_text) = if let Some(open_idx) = fragment.find('[') {
        if !fragment.ends_with(']') {
            return None;
        }
        let field = fragment[..open_idx].trim();
        let index_text = &fragment[open_idx + 1..fragment.len() - 1];
        (field, Some(index_text))
    } else {
        (fragment.trim(), None)
    };

    let field_selector = match field_name {
        "re" => 1,
        "im" => 2,
        _ => return None,
    };
    let array_selector = maybe_index_text.and_then(parse_one_based_index);
    Some((field_selector, array_selector))
}

pub fn parse_scalar_target_projection(
    lhs: &str,
    target: &str,
) -> Option<(Option<usize>, Option<usize>)> {
    if target == lhs {
        return Some((None, None));
    }

    if let Some(rest) = target.strip_prefix(lhs) {
        if let Some(indexed) = rest.strip_prefix('[') {
            let close_idx = indexed.find(']')?;
            let index = parse_one_based_index(&indexed[..close_idx])?;
            let tail = &indexed[close_idx + 1..];
            if tail.is_empty() {
                return Some((Some(index), None));
            }
            if let Some(field_part) = tail.strip_prefix('.')
                && let Some((field_selector, nested_array_selector)) =
                    parse_complex_field_selector(field_part)
            {
                return Some((nested_array_selector.or(Some(index)), Some(field_selector)));
            }
        }

        if let Some(field_part) = rest.strip_prefix('.')
            && let Some((field_selector, array_selector)) = parse_complex_field_selector(field_part)
        {
            return Some((array_selector, Some(field_selector)));
        }
    }

    None
}

pub fn target_var_ref_expr(target: &str) -> Expression {
    let last_dot = target.rfind('.');
    let trailing_open = target.rfind('[');
    if let Some(open_idx) = trailing_open
        && target.ends_with(']')
        && last_dot.is_none_or(|dot_idx| open_idx > dot_idx)
    {
        let base = &target[..open_idx];
        let inner = &target[open_idx + 1..target.len() - 1];
        let subscripts = inner
            .split(',')
            .map(str::trim)
            .map(parse_one_based_index)
            .collect::<Option<Vec<_>>>();
        if let Some(indices) = subscripts {
            return Expression::VarRef {
                name: VarName::new(base.to_string()),
                subscripts: indices
                    .into_iter()
                    .map(|idx| Subscript::Index(idx as i64))
                    .collect(),
            };
        }
    }

    Expression::VarRef {
        name: VarName::new(target.to_string()),
        subscripts: Vec::new(),
    }
}

pub struct ScalarProjectionContext<'a> {
    var_dims: &'a HashMap<String, Vec<i64>>,
    complex_fields: &'a HashMap<String, [Option<String>; 2]>,
    component_index_map: &'a HashMap<String, HashMap<usize, String>>,
    function_output_index_map: &'a HashMap<String, HashMap<usize, String>>,
}

impl ScalarProjectionContext<'_> {
    fn project_index(&self, expr: &Expression, scalar_idx: usize) -> Expression {
        index_into_expr(
            expr,
            scalar_idx,
            self.var_dims,
            self.complex_fields,
            self.component_index_map,
            self.function_output_index_map,
        )
    }
}

fn complex_zero() -> Expression {
    Expression::Literal(Literal::Real(0.0))
}

fn complex_add(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Add(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn complex_sub(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn complex_mul(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Mul(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn complex_div(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Div(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn var_ref_expr(name: &VarName, subscripts: &[Subscript]) -> Expression {
    Expression::VarRef {
        name: name.clone(),
        subscripts: subscripts.to_vec(),
    }
}

fn project_complex_var_ref(
    name: &VarName,
    subscripts: &[Subscript],
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Expression {
    if name.as_str().ends_with(".re") {
        return if field_idx == 1 {
            var_ref_expr(name, subscripts)
        } else {
            complex_zero()
        };
    }
    if name.as_str().ends_with(".im") {
        return if field_idx == 2 {
            var_ref_expr(name, subscripts)
        } else {
            complex_zero()
        };
    }
    if let Some(fields) = projection.complex_fields.get(name.as_str()) {
        let projected_name = match field_idx {
            1 => fields[0].as_ref(),
            2 => fields[1].as_ref(),
            _ => None,
        };
        if let Some(projected_name) = projected_name {
            return Expression::VarRef {
                name: VarName::new(projected_name.clone()),
                subscripts: vec![],
            };
        }
    }
    if field_idx == 1 {
        var_ref_expr(name, subscripts)
    } else {
        complex_zero()
    }
}

fn project_complex_unary(
    expr: &Expression,
    op: &OpUnary,
    rhs: &Expression,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Expression {
    if matches!(op, OpUnary::Minus(_) | OpUnary::DotMinus(_)) {
        return Expression::Unary {
            op: op.clone(),
            rhs: Box::new(project_complex_component(rhs, field_idx, projection)),
        };
    }
    if field_idx == 1 {
        expr.clone()
    } else {
        complex_zero()
    }
}

fn project_complex_mul_or_div(
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Expression {
    let lhs_re = project_complex_component(lhs, 1, projection);
    let lhs_im = project_complex_component(lhs, 2, projection);
    let rhs_re = project_complex_component(rhs, 1, projection);
    let rhs_im = project_complex_component(rhs, 2, projection);

    if matches!(op, OpBinary::Mul(_) | OpBinary::MulElem(_)) {
        return if field_idx == 1 {
            complex_sub(complex_mul(lhs_re, rhs_re), complex_mul(lhs_im, rhs_im))
        } else {
            complex_add(complex_mul(lhs_re, rhs_im), complex_mul(lhs_im, rhs_re))
        };
    }

    let denom = complex_add(
        complex_mul(rhs_re.clone(), rhs_re.clone()),
        complex_mul(rhs_im.clone(), rhs_im.clone()),
    );
    if field_idx == 1 {
        complex_div(
            complex_add(complex_mul(lhs_re, rhs_re), complex_mul(lhs_im, rhs_im)),
            denom,
        )
    } else {
        complex_div(
            complex_sub(complex_mul(lhs_im, rhs_re), complex_mul(lhs_re, rhs_im)),
            denom,
        )
    }
}

fn project_complex_binary(
    expr: &Expression,
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Expression {
    if matches!(
        op,
        OpBinary::Add(_) | OpBinary::AddElem(_) | OpBinary::Sub(_) | OpBinary::SubElem(_)
    ) {
        return Expression::Binary {
            op: op.clone(),
            lhs: Box::new(project_complex_component(lhs, field_idx, projection)),
            rhs: Box::new(project_complex_component(rhs, field_idx, projection)),
        };
    }
    if matches!(
        op,
        OpBinary::Mul(_) | OpBinary::MulElem(_) | OpBinary::Div(_) | OpBinary::DivElem(_)
    ) {
        return project_complex_mul_or_div(op, lhs, rhs, field_idx, projection);
    }
    if field_idx == 1 {
        expr.clone()
    } else {
        complex_zero()
    }
}

fn project_constructor_component(
    expr: &Expression,
    name: &VarName,
    args: &[Expression],
    field_idx: usize,
) -> Expression {
    if let Some(arg) = args.get(field_idx.saturating_sub(1)) {
        return arg.clone();
    }
    if field_idx == 2 && args.len() == 1 && name.as_str().rsplit('.').next() == Some("Complex") {
        return complex_zero();
    }
    expr.clone()
}

fn project_function_call_component(
    expr: &Expression,
    name: &VarName,
    args: &[Expression],
    is_constructor: bool,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Expression {
    if is_constructor {
        return project_constructor_component(expr, name, args, field_idx);
    }
    if let Some(by_index) = projection.function_output_index_map.get(name.as_str())
        && let Some(projected_output) = by_index.get(&field_idx)
    {
        return Expression::FunctionCall {
            name: VarName::new(format!("{}.{}", name.as_str(), projected_output)),
            args: args.to_vec(),
            is_constructor: false,
        };
    }
    if field_idx == 1 {
        expr.clone()
    } else {
        complex_zero()
    }
}

fn project_if_component(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Expression {
    Expression::If {
        branches: branches
            .iter()
            .map(|(cond, val)| {
                (
                    cond.clone(),
                    project_complex_component(val, field_idx, projection),
                )
            })
            .collect(),
        else_branch: Box::new(project_complex_component(
            else_branch,
            field_idx,
            projection,
        )),
    }
}

fn project_array_component(
    expr: &Expression,
    elements: &[Expression],
    is_matrix: bool,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Expression {
    if elements.len() == 1 {
        return project_complex_component(&elements[0], field_idx, projection);
    }
    if elements.is_empty() {
        return expr.clone();
    }
    Expression::Array {
        elements: elements
            .iter()
            .map(|element| project_complex_component(element, field_idx, projection))
            .collect(),
        is_matrix,
    }
}

fn project_complex_component(
    expr: &Expression,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Expression {
    match expr {
        Expression::Literal(_) => {
            if field_idx == 1 {
                expr.clone()
            } else {
                complex_zero()
            }
        }
        Expression::VarRef { name, subscripts } => {
            project_complex_var_ref(name, subscripts, field_idx, projection)
        }
        Expression::Unary { op, rhs } => {
            project_complex_unary(expr, op, rhs, field_idx, projection)
        }
        Expression::Binary { op, lhs, rhs } => {
            project_complex_binary(expr, op, lhs, rhs, field_idx, projection)
        }
        Expression::If {
            branches,
            else_branch,
        } => project_if_component(branches, else_branch, field_idx, projection),
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => project_function_call_component(
            expr,
            name,
            args,
            *is_constructor,
            field_idx,
            projection,
        ),
        Expression::Array {
            elements,
            is_matrix,
        } => project_array_component(expr, elements, *is_matrix, field_idx, projection),
        _ => projection.project_index(expr, field_idx),
    }
}

pub fn project_rhs_for_scalar_target(
    rhs: &Expression,
    scalar_idx: usize,
    lhs_target: Option<&str>,
    target: Option<&str>,
    projection: &ScalarProjectionContext<'_>,
) -> Expression {
    if let (Some(lhs_name), Some(target_name)) = (lhs_target, target)
        && let Some((array_selector, field_selector)) =
            parse_scalar_target_projection(lhs_name, target_name)
    {
        if let Expression::Binary {
            op,
            lhs,
            rhs: row_rhs,
        } = rhs
            && matches!(op, OpBinary::Sub(_))
            && let Expression::VarRef { name, subscripts } = lhs.as_ref()
            && scalarization_var_ref_name(name, subscripts)
                .as_deref()
                .is_some_and(|lhs_row_name| lhs_row_name == lhs_name)
        {
            let mut projected_rhs = (*row_rhs.clone()).clone();
            if let Some(idx) = array_selector {
                projected_rhs = projection.project_index(&projected_rhs, idx);
            }
            if let Some(field_idx) = field_selector {
                projected_rhs = project_complex_component(&projected_rhs, field_idx, projection);
            }
            return Expression::Binary {
                op: op.clone(),
                lhs: Box::new(target_var_ref_expr(target_name)),
                rhs: Box::new(projected_rhs),
            };
        }

        let mut projected = rhs.clone();
        if let Some(idx) = array_selector {
            projected = projection.project_index(&projected, idx);
        }
        if let Some(field_idx) = field_selector {
            projected = project_complex_component(&projected, field_idx, projection);
        }
        return projected;
    }

    projection.project_index(rhs, scalar_idx)
}

pub fn scalarized_equation_lhs(
    eq: &Equation,
    target: Option<&str>,
    scalar_idx: usize,
) -> Option<VarName> {
    let _ = eq.lhs.as_ref()?;
    if let Some(name) = target {
        return Some(VarName::new(name.to_string()));
    }
    eq.lhs
        .as_ref()
        .map(|lhs| VarName::new(format!("{}[{scalar_idx}]", lhs.as_str())))
}

/// Expand array equations (scalar_count > 1) into individual scalar equations.
///
/// After this pass every element of `dae.f_x` has `scalar_count == 1`,
/// which is required by solvers that expect one equation per unknown.
pub fn scalarize_equations(dae: &mut Dae) {
    let var_dims = build_var_dims_map(dae);
    let complex_fields = build_complex_field_map(dae);
    let component_index_map = build_component_index_projection_map(dae);
    let function_output_index_map = build_function_output_projection_map(dae);
    let projection = ScalarProjectionContext {
        var_dims: &var_dims,
        complex_fields: &complex_fields,
        component_index_map: &component_index_map,
        function_output_index_map: &function_output_index_map,
    };
    let scalar_names = build_output_names(dae);
    let mut expanded = Vec::new();
    for eq in &dae.f_x {
        let scalarization_target = eq
            .lhs
            .as_ref()
            .map(|lhs| lhs.as_str().to_string())
            .or_else(|| residual_lhs_target_name(&eq.rhs));
        let lhs_targets = scalarization_target
            .as_deref()
            .map(|lhs| scalar_targets_for_lhs(lhs, &scalar_names))
            .unwrap_or_default();
        let scalar_count = eq.scalar_count.max(lhs_targets.len()).max(1);
        if scalar_count <= 1 {
            expanded.push(eq.clone());
        } else {
            for i in 1..=scalar_count {
                let target = lhs_targets.get(i - 1).map(String::as_str);
                expanded.push(Equation {
                    lhs: scalarized_equation_lhs(eq, target, i),
                    rhs: project_rhs_for_scalar_target(
                        &eq.rhs,
                        i,
                        scalarization_target.as_deref(),
                        target,
                        &projection,
                    ),
                    span: eq.span,
                    origin: eq.origin.clone(),
                    scalar_count: 1,
                });
            }
        }
    }
    dae.f_x = expanded;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_output_names_orders_states_algebraics_outputs_and_expands_arrays() {
        let mut dae_model = dae::Dae::default();

        let mut x = dae::Variable::new(dae::VarName::new("x"));
        x.dims = vec![2];
        dae_model.states.insert(dae::VarName::new("x"), x);
        dae_model.states.insert(
            dae::VarName::new("v"),
            dae::Variable::new(dae::VarName::new("v")),
        );

        dae_model.algebraics.insert(
            dae::VarName::new("z"),
            dae::Variable::new(dae::VarName::new("z")),
        );

        let mut y = dae::Variable::new(dae::VarName::new("y"));
        y.dims = vec![2];
        dae_model.outputs.insert(dae::VarName::new("y"), y);

        let names = build_output_names(&dae_model);
        assert_eq!(
            names,
            vec![
                "x[1]".to_string(),
                "x[2]".to_string(),
                "v".to_string(),
                "z".to_string(),
                "y[1]".to_string(),
                "y[2]".to_string(),
            ]
        );
    }

    #[test]
    fn scalarize_expression_rows_flattens_matrix_literals_row_major() {
        let ctx = ExpressionScalarizationContext {
            var_dims: HashMap::new(),
            complex_fields: HashMap::new(),
            component_index_map: HashMap::new(),
            function_output_index_map: HashMap::new(),
        };
        let expr = Expression::Array {
            elements: vec![
                Expression::Array {
                    elements: vec![
                        Expression::Literal(Literal::Integer(1)),
                        Expression::Literal(Literal::Integer(2)),
                    ],
                    is_matrix: true,
                },
                Expression::Array {
                    elements: vec![
                        Expression::Literal(Literal::Integer(3)),
                        Expression::Literal(Literal::Integer(4)),
                    ],
                    is_matrix: true,
                },
            ],
            is_matrix: true,
        };

        let rows = scalarize_expression_rows(&expr, 4, &ctx);
        assert_eq!(
            rows,
            vec![
                Expression::Literal(Literal::Integer(1)),
                Expression::Literal(Literal::Integer(2)),
                Expression::Literal(Literal::Integer(3)),
                Expression::Literal(Literal::Integer(4)),
            ]
        );
    }
}
