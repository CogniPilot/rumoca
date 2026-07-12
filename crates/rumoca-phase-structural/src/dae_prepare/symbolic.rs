use super::*;
use rumoca_core::{ExpressionRewriter, Span};

use rumoca_core::NAMED_FUNCTION_ARG_PREFIX;

fn is_der_of_state(expr: &Expression, state_name: &VarName) -> bool {
    matches!(
        expr,
        Expression::BuiltinCall { function: BuiltinFunction::Der, args, .. }
        if args.len() == 1 && expr_refers_to_var(&args[0], state_name)
    )
}

fn make_binary(op: OpBinary, lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn make_unary(op: OpUnary, rhs: Expression, span: Span) -> Expression {
    Expression::Unary {
        op,
        rhs: Box::new(rhs),
        span,
    }
}

fn real_literal(value: f64, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span,
    }
}

fn split_linear_der_target(
    expr: &Expression,
    state_name: &VarName,
) -> Option<(Expression, Expression)> {
    let span = expr.span()?;
    if is_der_of_state(expr, state_name) {
        return Some((real_literal(1.0, span), real_literal(0.0, span)));
    }

    let is_target = |e: &Expression| is_der_of_state(e, state_name);
    match expr {
        Expression::Unary {
            op: OpUnary::Minus | OpUnary::DotMinus,
            rhs,
            ..
        } => {
            let (coef, rem) = split_linear_der_target(rhs, state_name)?;
            Some((
                make_unary(OpUnary::Minus, coef, span),
                make_unary(OpUnary::Minus, rem, span),
            ))
        }
        Expression::Binary { op, lhs, rhs, .. } => match op {
            OpBinary::Add | OpBinary::AddElem => {
                if let Some((coef, rem)) = split_linear_der_target(lhs, state_name)
                    && !expr_contains_der_of(rhs, state_name)
                {
                    return Some((coef, make_binary(OpBinary::Add, rem, *rhs.clone(), span)));
                }
                if let Some((coef, rem)) = split_linear_der_target(rhs, state_name)
                    && !expr_contains_der_of(lhs, state_name)
                {
                    return Some((coef, make_binary(OpBinary::Add, *lhs.clone(), rem, span)));
                }
                None
            }
            OpBinary::Sub | OpBinary::SubElem => {
                if let Some((coef, rem)) = split_linear_der_target(lhs, state_name)
                    && !expr_contains_der_of(rhs, state_name)
                {
                    return Some((coef, make_binary(OpBinary::Sub, rem, *rhs.clone(), span)));
                }
                if let Some((coef, rem)) = split_linear_der_target(rhs, state_name)
                    && !expr_contains_der_of(lhs, state_name)
                {
                    return Some((
                        make_unary(OpUnary::Minus, coef, span),
                        make_binary(OpBinary::Sub, *lhs.clone(), rem, span),
                    ));
                }
                None
            }
            OpBinary::Mul | OpBinary::MulElem => {
                if is_target(lhs) && !expr_contains_der_of(rhs, state_name) {
                    return Some((*rhs.clone(), real_literal(0.0, span)));
                }
                if is_target(rhs) && !expr_contains_der_of(lhs, state_name) {
                    return Some((*lhs.clone(), real_literal(0.0, span)));
                }
                None
            }
            _ => None,
        },
        _ => None,
    }
}

pub(super) fn try_extract_der_value(rhs: &Expression, state_name: &VarName) -> Option<Expression> {
    if let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs: row_rhs,
        ..
    } = rhs
    {
        if is_der_of_state(row_rhs, state_name) {
            return Some(*lhs.clone());
        }
        if is_der_of_state(lhs, state_name) {
            return Some(*row_rhs.clone());
        }
    }

    let span = rhs.span()?;
    let (coef, remainder) = split_linear_der_target(rhs, state_name)?;
    Some(make_binary(
        OpBinary::Div,
        make_unary(OpUnary::Minus, remainder, span),
        coef,
        span,
    ))
}

pub(super) fn build_der_value_map(dae: &Dae) -> HashMap<String, Expression> {
    let mut map = HashMap::new();
    for state_name in dae.variables.states.keys() {
        if let Some(var) = dae.variables.states.get(state_name)
            && var.size() > 1
        {
            if let Some(value) = build_array_der_value(dae, state_name, &var.dims) {
                map.insert(state_name.as_str().to_string(), value);
            }
            continue;
        }
        for eq in &dae.continuous.equations {
            if !expr_contains_der_of(&eq.rhs, state_name) {
                continue;
            }
            if let Some(value) = try_extract_der_value(&eq.rhs, state_name) {
                map.insert(state_name.as_str().to_string(), value);
                break;
            }
        }
    }
    map
}

fn build_array_der_value(dae: &Dae, state_name: &VarName, dims: &[i64]) -> Option<Expression> {
    let size = dims.iter().try_fold(1usize, |acc, dim| {
        (*dim > 0).then(|| acc.checked_mul(*dim as usize)).flatten()
    })?;
    let mut values = Vec::with_capacity(size);
    for flat_index in 0..size {
        let scalar_name = dae::scalar_name_for_flat_index(state_name, dims, flat_index);
        let value = dae
            .continuous
            .equations
            .iter()
            .find_map(|eq| try_extract_der_value(&eq.rhs, &scalar_name))?;
        values.push(value);
    }
    array_expr_from_flat_values(values, dims)
}

fn array_expr_from_flat_values(values: Vec<Expression>, dims: &[i64]) -> Option<Expression> {
    match dims {
        [n] if *n >= 0 && *n as usize == values.len() => Some(Expression::Array {
            span: expression_sequence_span(&values)?,
            elements: values,
            is_matrix: false,
        }),
        [rows, cols] if *rows >= 0 && *cols >= 0 => {
            let rows = *rows as usize;
            let cols = *cols as usize;
            if rows.checked_mul(cols)? != values.len() {
                return None;
            }
            let elements = values
                .chunks(cols)
                .map(|row| {
                    Some(Expression::Array {
                        span: expression_sequence_span(row)?,
                        elements: row.to_vec(),
                        is_matrix: false,
                    })
                })
                .collect::<Option<Vec<_>>>()?;
            Some(Expression::Array {
                span: expression_sequence_span(&elements)?,
                elements,
                is_matrix: true,
            })
        }
        _ => Some(Expression::Array {
            span: expression_sequence_span(&values)?,
            elements: values,
            is_matrix: false,
        }),
    }
}

fn expression_sequence_span(elements: &[Expression]) -> Option<Span> {
    let first = elements.first()?.span()?;
    let last = elements.last()?.span()?;
    if first.source == last.source {
        Some(Span::from_offsets(
            first.source,
            first.start.0,
            last.end.0.max(first.start.0),
        ))
    } else {
        Some(first)
    }
}

/// Highest derivative order this pass will expand symbolically. Successive
/// `Modelica.Blocks.Continuous.Der` blocks and relative-acceleration chains need
/// a handful of orders; the bound stops the `der(der(...))` arm from recursing
/// forever when a link's derivative is only known symbolically (its expansion
/// reintroduces a `der(...)` that can never be reduced to states).
const MAX_DERIVATIVE_ORDER: u32 = 8;

struct SymbolicDerivativeContext<'a> {
    dae: &'a Dae,
    der_map: &'a HashMap<String, Expression>,
    /// Current `der(der(...))` nesting depth, to bound higher-order expansion.
    der_order: std::cell::Cell<u32>,
}

impl<'a> SymbolicDerivativeContext<'a> {
    fn differentiate_variable(
        &self,
        name: &VarName,
        subscripts: &[Subscript],
        span: Span,
    ) -> Option<Expression> {
        if name.as_str() == "time" {
            return Some(real_literal(1.0, span));
        }
        if self.dae.variables.parameters.contains_key(name)
            || self.dae.variables.constants.contains_key(name)
        {
            return Some(real_literal(0.0, span));
        }
        if !subscripts.is_empty()
            && !self.dae.variables.states.contains_key(name)
            && let Some(derivative) = self.der_map.get(name.as_str())
            && let Some(dims) = variable_dims_for_name(self.dae, name)
            && let Some(indices) = static_subscript_indices(subscripts)
            && let Some(flat_index) = flat_index_from_indices(&dims, &indices)
            && let Some(first_subscript) = subscripts.first()
        {
            return project_flat_index_with_span(
                derivative,
                &dims,
                flat_index,
                Some(first_subscript.span()),
            );
        }
        if !subscripts.is_empty() && variable_dims_for_name(self.dae, name).is_some() {
            let first_subscript = subscripts.first()?;
            let span = first_subscript.span();
            return Some(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![Expression::VarRef {
                    name: rumoca_core::Reference::from_var_name(name.clone()),
                    subscripts: subscripts.to_vec(),
                    span,
                }],
                span,
            });
        }
        self.der_map.get(name.as_str()).cloned()
    }

    fn differentiate_binary(
        &self,
        op: &OpBinary,
        lhs: &Expression,
        rhs: &Expression,
        span: Span,
        active_functions: &mut Vec<VarName>,
    ) -> Option<Expression> {
        match op {
            OpBinary::Add | OpBinary::AddElem => Some(make_binary(
                OpBinary::Add,
                self.differentiate(lhs, active_functions)?,
                self.differentiate(rhs, active_functions)?,
                span,
            )),
            OpBinary::Sub | OpBinary::SubElem => Some(make_binary(
                OpBinary::Sub,
                self.differentiate(lhs, active_functions)?,
                self.differentiate(rhs, active_functions)?,
                span,
            )),
            OpBinary::Mul | OpBinary::MulElem => {
                if let Some(dot) = self.differentiate_vector_dot(lhs, rhs, span, active_functions) {
                    return Some(dot);
                }
                let da_b = make_binary(
                    OpBinary::Mul,
                    self.differentiate(lhs, active_functions)?,
                    rhs.clone(),
                    span,
                );
                let a_db = make_binary(
                    OpBinary::Mul,
                    lhs.clone(),
                    self.differentiate(rhs, active_functions)?,
                    span,
                );
                Some(make_binary(OpBinary::Add, da_b, a_db, span))
            }
            OpBinary::Div | OpBinary::DivElem => {
                let da_b = make_binary(
                    OpBinary::Mul,
                    self.differentiate(lhs, active_functions)?,
                    rhs.clone(),
                    span,
                );
                let a_db = make_binary(
                    OpBinary::Mul,
                    lhs.clone(),
                    self.differentiate(rhs, active_functions)?,
                    span,
                );
                let numer = make_binary(OpBinary::Sub, da_b, a_db, span);
                let denom = make_binary(OpBinary::Mul, rhs.clone(), rhs.clone(), span);
                Some(make_binary(OpBinary::Div, numer, denom, span))
            }
            _ => None,
        }
    }

    fn differentiate_vector_dot(
        &self,
        lhs: &Expression,
        rhs: &Expression,
        span: Span,
        active_functions: &mut Vec<VarName>,
    ) -> Option<Expression> {
        let lhs_dims = expression_dims(lhs, self.dae)?;
        let rhs_dims = expression_dims(rhs, self.dae)?;
        if lhs_dims.len() != 1 || lhs_dims != rhs_dims {
            return None;
        }
        let n = usize::try_from(lhs_dims[0]).ok()?;
        if n == 0 {
            return None;
        }

        let terms = (0..n)
            .map(|idx| {
                let lhs_i = project_flat_index(lhs, &lhs_dims, idx)?;
                let rhs_i = project_flat_index(rhs, &rhs_dims, idx)?;
                let da_b = make_binary(
                    OpBinary::Mul,
                    self.differentiate(&lhs_i, active_functions)?,
                    rhs_i.clone(),
                    span,
                );
                let a_db = make_binary(
                    OpBinary::Mul,
                    lhs_i,
                    self.differentiate(&rhs_i, active_functions)?,
                    span,
                );
                Some(make_binary(OpBinary::Add, da_b, a_db, span))
            })
            .collect::<Option<Vec<_>>>()?;
        Some(sum_terms(terms, span))
    }

    fn differentiate_unary(
        &self,
        op: &OpUnary,
        rhs: &Expression,
        span: Span,
        active_functions: &mut Vec<VarName>,
    ) -> Option<Expression> {
        match op {
            OpUnary::Minus | OpUnary::DotMinus => Some(make_unary(
                OpUnary::Minus,
                self.differentiate(rhs, active_functions)?,
                span,
            )),
            OpUnary::Plus | OpUnary::DotPlus => self.differentiate(rhs, active_functions),
            _ => None,
        }
    }

    fn differentiate_if(
        &self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
        span: Span,
        active_functions: &mut Vec<VarName>,
    ) -> Option<Expression> {
        let mut differentiated_branches = Vec::with_capacity(branches.len());
        for (cond, value) in branches {
            differentiated_branches
                .push((cond.clone(), self.differentiate(value, active_functions)?));
        }
        Some(Expression::If {
            branches: differentiated_branches,
            else_branch: Box::new(self.differentiate(else_branch, active_functions)?),
            span,
        })
    }

    fn differentiate_function_call(
        &self,
        name: &VarName,
        args: &[Expression],
        is_constructor: bool,
        active_functions: &mut Vec<VarName>,
    ) -> Option<Expression> {
        if is_constructor {
            return None;
        }
        let (function_name, function, output_selector) = resolve_function_call(self.dae, name)?;
        if active_functions
            .iter()
            .any(|active| active == function_name)
        {
            return None;
        }
        if !function.pure || function.external.is_some() || function.outputs.len() != 1 {
            return None;
        }
        active_functions.push(function_name.clone());
        let Some(output_expr) = function_output_expression(function, args, output_selector) else {
            active_functions.pop();
            return None;
        };
        let derivative = self.differentiate(&output_expr, active_functions);
        active_functions.pop();
        derivative
    }

    fn differentiate(
        &self,
        expr: &Expression,
        active_functions: &mut Vec<VarName>,
    ) -> Option<Expression> {
        match expr {
            Expression::Literal { value: _, span } => Some(real_literal(0.0, *span)),
            Expression::VarRef {
                name,
                subscripts,
                span,
            } => self.differentiate_variable(name.var_name(), subscripts, *span),
            Expression::Binary { op, lhs, rhs, span } => {
                self.differentiate_binary(op, lhs, rhs, *span, active_functions)
            }
            Expression::Unary { op, rhs, span } => {
                self.differentiate_unary(op, rhs, *span, active_functions)
            }
            Expression::If {
                branches,
                else_branch,
                span,
            } => self.differentiate_if(branches, else_branch, *span, active_functions),
            Expression::Array {
                elements,
                is_matrix,
                span,
            } => Some(Expression::Array {
                elements: elements
                    .iter()
                    .map(|element| self.differentiate(element, active_functions))
                    .collect::<Option<Vec<_>>>()?,
                is_matrix: *is_matrix,
                span: *span,
            }),
            Expression::FunctionCall {
                name,
                args,
                is_constructor,
                ..
            } => self.differentiate_function_call(
                name.var_name(),
                args,
                *is_constructor,
                active_functions,
            ),
            // d/dt(der(X)) — a higher-order derivative (successive `Der` blocks,
            // or a relative acceleration `a = der(der(phi))`). `der(X)` is X's
            // first time-derivative; differentiate that expression to climb one
            // order. This lets `compute_full_derivative_map` resolve derivative
            // chains whose links are themselves `der(...)` definitions.
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args,
                ..
            } if args.len() == 1 => {
                // Bound the recursion: each `der(der(...))` climbs one order, and
                // a link whose derivative is only known symbolically would
                // otherwise re-enter this arm forever. Stop past the supported
                // order rather than overflow the stack.
                if self.der_order.get() >= MAX_DERIVATIVE_ORDER {
                    return None;
                }
                self.der_order.set(self.der_order.get() + 1);
                let result = self.differentiate_der_call(&args[0], active_functions);
                self.der_order.set(self.der_order.get() - 1);
                result
            }
            _ => None,
        }
    }

    /// Differentiate `der(arg)` one order higher: take `arg`'s first derivative
    /// and differentiate it again. Bounded by `der_order` in the caller.
    fn differentiate_der_call(
        &self,
        arg: &Expression,
        active_functions: &mut Vec<VarName>,
    ) -> Option<Expression> {
        let first_derivative = match arg {
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => {
                let first = self.der_map.get(name.var_name().as_str()).cloned()?;
                // The symbolic fallback `der_map[X] = der(X)` (an unresolved first
                // derivative) means the second derivative is not expressible;
                // differentiating it would re-enter on the same `der(X)`.
                if expr_contains_der_of(&first, name.var_name()) {
                    return None;
                }
                first
            }
            inner => self.differentiate(inner, active_functions)?,
        };
        self.differentiate(&first_derivative, active_functions)
    }
}

pub(super) fn symbolic_time_derivative(
    expr: &Expression,
    dae: &Dae,
    der_map: &HashMap<String, Expression>,
) -> Option<Expression> {
    SymbolicDerivativeContext {
        dae,
        der_map,
        der_order: std::cell::Cell::new(0),
    }
    .differentiate(expr, &mut Vec::new())
}

fn function_output_expression(
    function: &rumoca_core::Function,
    args: &[Expression],
    output_selector: Option<&str>,
) -> Option<Expression> {
    let output = function.outputs.first()?;
    let mut scope = HashMap::new();
    bind_function_inputs(function, args, &mut scope)?;
    for statement in &function.body {
        apply_function_assignment(statement, &mut scope)?;
    }
    let expr = scope.get(output.name.as_str())?.clone();
    if let Some(selector) = output_selector {
        if selector == output.name {
            return if output.dims == [1] {
                scalar_array_element(&expr)
            } else {
                Some(expr)
            };
        }
        let scalar = rumoca_core::parse_scalar_name(selector)?;
        if scalar.base != output.name {
            return None;
        }
        let flat_index = flat_index_from_indices(&output.dims, &scalar.indices)?;
        return project_flat_index(&expr, &output.dims, flat_index);
    }
    if output.dims == [1] {
        return scalar_array_element(&expr);
    }
    Some(expr)
}

fn resolve_function_call<'a>(
    dae: &'a Dae,
    call_name: &'a VarName,
) -> Option<(&'a VarName, &'a rumoca_core::Function, Option<&'a str>)> {
    if let Some(function) = dae.symbols.functions.get(call_name) {
        return Some((call_name, function, None));
    }

    dae.symbols
        .functions
        .iter()
        .filter_map(|(function_name, function)| {
            let selector = call_name
                .as_str()
                .strip_prefix(function_name.as_str())?
                .strip_prefix('.')?;
            (!selector.is_empty()).then_some((function_name, function, Some(selector)))
        })
        .max_by_key(|(function_name, _, _)| function_name.as_str().len())
}

fn bind_function_inputs(
    function: &rumoca_core::Function,
    args: &[Expression],
    scope: &mut HashMap<String, Expression>,
) -> Option<()> {
    let (named, positional) = split_named_and_positional_args(args)?;
    let mut positional_idx = 0usize;
    for input in &function.inputs {
        let actual = named.get(input.name.as_str()).cloned().or_else(|| {
            let actual = positional.get(positional_idx).cloned();
            positional_idx += usize::from(actual.is_some());
            actual
        });
        let actual = actual.or_else(|| input.default.clone())?;
        let actual = substitute_function_scope(&actual, scope);
        scope.insert(input.name.clone(), actual);
    }
    Some(())
}

fn apply_function_assignment(
    statement: &rumoca_core::Statement,
    scope: &mut HashMap<String, Expression>,
) -> Option<()> {
    let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
        return matches!(statement, rumoca_core::Statement::Empty { .. }).then_some(());
    };
    let value = substitute_function_scope(value, scope);
    scope.insert(comp.to_var_name().as_str().to_string(), value);
    Some(())
}

fn substitute_function_scope(expr: &Expression, scope: &HashMap<String, Expression>) -> Expression {
    let mut rewriter = FunctionScopeSubstituter { scope };
    rewriter.rewrite_expression(expr)
}

struct FunctionScopeSubstituter<'a> {
    scope: &'a HashMap<String, Expression>,
}

impl ExpressionRewriter for FunctionScopeSubstituter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        let Expression::VarRef {
            name,
            subscripts,
            span,
        } = expr
        else {
            return self.walk_expression(expr);
        };
        if !subscripts.is_empty() {
            return self.walk_expression(expr);
        }
        self.scope
            .get(name.as_str())
            .cloned()
            .map(|expr| expr.with_span(*span))
            .unwrap_or_else(|| self.walk_expression(expr))
    }
}

fn split_named_and_positional_args(
    args: &[Expression],
) -> Option<(HashMap<String, Expression>, Vec<Expression>)> {
    let mut named = HashMap::new();
    let mut positional = Vec::new();
    for arg in args {
        if let Some((name, value)) = decode_named_arg(arg) {
            if named.insert(name.to_string(), value.clone()).is_some() {
                return None;
            }
        } else {
            positional.push(arg.clone());
        }
    }
    Some((named, positional))
}

fn decode_named_arg(expr: &Expression) -> Option<(&str, &Expression)> {
    let Expression::FunctionCall { name, args, .. } = expr else {
        return None;
    };
    let name = name.as_str().strip_prefix(NAMED_FUNCTION_ARG_PREFIX)?;
    Some((name, args.first()?))
}

fn scalar_array_element(expr: &Expression) -> Option<Expression> {
    match expr {
        Expression::Array { elements, .. } if elements.len() == 1 => elements.first().cloned(),
        _ => Some(expr.clone()),
    }
}

fn expression_dims(expr: &Expression, dae: &Dae) -> Option<Vec<i64>> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => variable_dims_for_name(dae, name.var_name()),
        Expression::Array {
            elements,
            is_matrix,
            ..
        } => array_expression_dims(elements, *is_matrix),
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } => args.first().and_then(|arg| expression_dims(arg, dae)),
        _ => None,
    }
}

fn variable_dims_for_name(dae: &Dae, name: &VarName) -> Option<Vec<i64>> {
    dae.variables
        .states
        .get(name)
        .or_else(|| dae.variables.algebraics.get(name))
        .or_else(|| dae.variables.outputs.get(name))
        .or_else(|| dae.variables.inputs.get(name))
        .or_else(|| dae.variables.parameters.get(name))
        .or_else(|| dae.variables.constants.get(name))
        .map(|var| var.dims.clone())
        .filter(|dims| !dims.is_empty())
}

fn array_expression_dims(elements: &[Expression], is_matrix: bool) -> Option<Vec<i64>> {
    if !is_matrix {
        return Some(vec![elements.len() as i64]);
    }
    let cols = match elements.first()? {
        Expression::Array { elements, .. } => elements.len(),
        _ => return None,
    };
    Some(vec![elements.len() as i64, cols as i64])
}

fn project_flat_index(expr: &Expression, dims: &[i64], flat_index: usize) -> Option<Expression> {
    project_flat_index_with_span(expr, dims, flat_index, None)
}

fn projection_span(expr: &Expression, fallback_span: Option<Span>) -> Option<Span> {
    expr.span()
        .or_else(|| fallback_span.filter(|span| !span.is_dummy()))
}

fn project_flat_index_with_span(
    expr: &Expression,
    dims: &[i64],
    flat_index: usize,
    fallback_span: Option<Span>,
) -> Option<Expression> {
    match expr {
        Expression::VarRef {
            name,
            subscripts,
            span,
        } if subscripts.is_empty() => {
            let indices = dae::flat_index_to_subscripts(dims, flat_index)?;
            let projection_span = projection_span(expr, fallback_span)?;
            Some(Expression::VarRef {
                name: name.clone(),
                subscripts: generated_index_subscripts(
                    indices,
                    projection_span,
                    "flat-index projected variable reference",
                )?,
                span: if span.is_dummy() {
                    projection_span
                } else {
                    *span
                },
            })
        }
        Expression::Array { elements, .. } => {
            flatten_array_elements(elements).get(flat_index).cloned()
        }
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } if args.len() == 1 => {
            let span = projection_span(expr, fallback_span)?;
            Some(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![project_flat_index_with_span(
                    &args[0],
                    dims,
                    flat_index,
                    Some(span),
                )?],
                span,
            })
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let span = projection_span(expr, fallback_span)?;
            Some(Expression::Binary {
                op: op.clone(),
                lhs: Box::new(project_flat_index_with_span(
                    lhs,
                    dims,
                    flat_index,
                    Some(span),
                )?),
                rhs: Box::new(project_flat_index_with_span(
                    rhs,
                    dims,
                    flat_index,
                    Some(span),
                )?),
                span,
            })
        }
        Expression::Unary { op, rhs, .. } => {
            let span = projection_span(expr, fallback_span)?;
            Some(Expression::Unary {
                op: op.clone(),
                rhs: Box::new(project_flat_index_with_span(
                    rhs,
                    dims,
                    flat_index,
                    Some(span),
                )?),
                span,
            })
        }
        _ => {
            let indices = dae::flat_index_to_subscripts(dims, flat_index)?;
            let span = projection_span(expr, fallback_span)?;
            Some(Expression::Index {
                base: Box::new(expr.clone()),
                subscripts: generated_index_subscripts(
                    indices,
                    span,
                    "flat-index projected expression",
                )?,
                span,
            })
        }
    }
}

fn generated_index_subscripts(
    indices: Vec<usize>,
    span: Span,
    context: &'static str,
) -> Option<Vec<Subscript>> {
    let provenance = span.require_provenance(context).ok()?;
    indices
        .into_iter()
        .map(|idx| {
            Some(Subscript::generated_index_with_provenance(
                i64::try_from(idx).ok()?,
                provenance,
            ))
        })
        .collect()
}

fn static_subscript_indices(subscripts: &[Subscript]) -> Option<Vec<i64>> {
    subscripts
        .iter()
        .map(|subscript| match subscript {
            Subscript::Index { value, .. } => Some(*value),
            Subscript::Expr { expr, .. } => match expr.as_ref() {
                Expression::Literal {
                    value: Literal::Integer(value),
                    ..
                } => Some(*value),
                Expression::Literal {
                    value: Literal::Real(value),
                    ..
                } if value.is_finite() && value.fract() == 0.0 => Some(*value as i64),
                _ => None,
            },
            Subscript::Colon { .. } => None,
        })
        .collect()
}

fn flat_index_from_indices(dims: &[i64], indices: &[i64]) -> Option<usize> {
    if dims.len() != indices.len() || dims.is_empty() {
        return None;
    }
    let mut flat_index = 0usize;
    let mut stride = 1usize;
    for (&dim, &index) in dims.iter().rev().zip(indices.iter().rev()) {
        if dim <= 0 || index <= 0 || index > dim {
            return None;
        }
        flat_index = flat_index.checked_add((index as usize - 1).checked_mul(stride)?)?;
        stride = stride.checked_mul(dim as usize)?;
    }
    Some(flat_index)
}

fn flatten_array_elements(elements: &[Expression]) -> Vec<Expression> {
    let mut flattened = Vec::new();
    for element in elements {
        match element {
            Expression::Array { elements, .. } => flattened.extend(elements.iter().cloned()),
            _ => flattened.push(element.clone()),
        }
    }
    flattened
}

fn sum_terms(mut terms: Vec<Expression>, span: Span) -> Expression {
    if terms.is_empty() {
        return real_literal(0.0, span);
    }
    let first = terms.remove(0);
    terms
        .into_iter()
        .fold(first, |lhs, rhs| make_binary(OpBinary::Add, lhs, rhs, span))
}

// SPEC_0021: Exception - exhaustive symbolic derivative expansion over Expression variants.
#[allow(clippy::too_many_lines)]
pub(super) fn expand_der_in_expr_full(
    expr: &Expression,
    dae: &Dae,
    der_map: &HashMap<String, Expression>,
    state_names: &HashSet<String>,
) -> Expression {
    match expr {
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } if args.len() == 1 => {
            let arg = &args[0];
            match arg {
                Expression::VarRef {
                    name, subscripts, ..
                } if subscripts.is_empty() => {
                    if state_names.contains(name.as_str()) {
                        expr.clone()
                    } else if let Some(deriv) = der_map.get(name.as_str()) {
                        deriv.clone()
                    } else {
                        expr.clone()
                    }
                }
                _ => {
                    if let Some(expanded) = symbolic_time_derivative(arg, dae, der_map) {
                        expanded
                    } else {
                        expr.clone()
                    }
                }
            }
        }
        Expression::Binary { op, lhs, rhs, span } => Expression::Binary {
            op: op.clone(),
            lhs: Box::new(expand_der_in_expr_full(lhs, dae, der_map, state_names)),
            rhs: Box::new(expand_der_in_expr_full(rhs, dae, der_map, state_names)),
            span: *span,
        },
        Expression::Unary { op, rhs, span } => Expression::Unary {
            op: op.clone(),
            rhs: Box::new(expand_der_in_expr_full(rhs, dae, der_map, state_names)),
            span: *span,
        },
        Expression::BuiltinCall {
            function,
            args,
            span,
        } => Expression::BuiltinCall {
            function: *function,
            args: args
                .iter()
                .map(|a| expand_der_in_expr_full(a, dae, der_map, state_names))
                .collect(),
            span: *span,
        },
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } => Expression::FunctionCall {
            name: name.clone(),
            args: args
                .iter()
                .map(|a| expand_der_in_expr_full(a, dae, der_map, state_names))
                .collect(),
            is_constructor: *is_constructor,
            span: *span,
        },
        Expression::If {
            branches,
            else_branch,
            span,
        } => Expression::If {
            branches: branches
                .iter()
                .map(|(c, v)| {
                    (
                        expand_der_in_expr_full(c, dae, der_map, state_names),
                        expand_der_in_expr_full(v, dae, der_map, state_names),
                    )
                })
                .collect(),
            else_branch: Box::new(expand_der_in_expr_full(
                else_branch,
                dae,
                der_map,
                state_names,
            )),
            span: *span,
        },
        Expression::Array {
            elements,
            is_matrix,
            span,
        } => Expression::Array {
            elements: elements
                .iter()
                .map(|e| expand_der_in_expr_full(e, dae, der_map, state_names))
                .collect(),
            is_matrix: *is_matrix,
            span: *span,
        },
        Expression::Index {
            base,
            subscripts,
            span,
        } => Expression::Index {
            base: Box::new(expand_der_in_expr_full(base, dae, der_map, state_names)),
            subscripts: subscripts.clone(),
            span: *span,
        },
        _ => expr.clone(),
    }
}

pub(super) fn truncate_debug(s: &str, max_chars: usize) -> String {
    if s.chars().count() <= max_chars {
        return s.to_string();
    }
    let mut out = String::with_capacity(max_chars + 1);
    for (i, ch) in s.chars().enumerate() {
        if i >= max_chars {
            break;
        }
        out.push(ch);
    }
    out.push('…');
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("symbolic_project.mo"),
            4,
            12,
        )
    }

    fn var_ref(name: &str, span: Span) -> Expression {
        Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts: Vec::new(),
            span,
        }
    }

    fn has_single_index_with_span(expr: &Expression, expected_span: Span) -> bool {
        let Expression::VarRef { subscripts, .. } = expr else {
            return false;
        };
        let [Subscript::Index { value, span }] = subscripts.as_slice() else {
            return false;
        };
        *value == 1 && *span == expected_span
    }

    #[test]
    fn project_flat_index_declines_unspanned_binary_projection() {
        let child_span = test_span();
        let expr = Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(var_ref("x", child_span)),
            rhs: Box::new(var_ref("y", child_span)),
            span: Span::DUMMY,
        };

        assert_eq!(project_flat_index(&expr, &[2], 0), None);
    }

    #[test]
    fn project_flat_index_preserves_binary_projection_span() {
        let span = test_span();
        let expr = Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(var_ref("x", span)),
            rhs: Box::new(var_ref("y", span)),
            span,
        };

        let projected = project_flat_index(&expr, &[2], 0).expect("spanned binary should project");

        assert_eq!(projected.span(), Some(span));
        assert!(
            matches!(
                projected,
                Expression::Binary { lhs, rhs, span: actual, .. }
                    if actual == span
                        && has_single_index_with_span(lhs.as_ref(), span)
                        && has_single_index_with_span(rhs.as_ref(), span)
            ),
            "projected binary should index both operands with the source span"
        );
    }
}
