use super::*;
use rumoca_core::ExpressionRewriter;

const NAMED_FUNCTION_ARG_PREFIX: &str = "__rumoca_named_arg__.";

fn is_der_of_state(expr: &Expression, state_name: &VarName) -> bool {
    matches!(
        expr,
        Expression::BuiltinCall { function: BuiltinFunction::Der, args, .. }
        if args.len() == 1 && expr_refers_to_var(&args[0], state_name)
    )
}

fn make_binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn make_unary(op: OpUnary, rhs: Expression) -> Expression {
    Expression::Unary {
        op,
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn zero_literal() -> Expression {
    Expression::Literal {
        value: Literal::Real(0.0),
        span: rumoca_core::Span::DUMMY,
    }
}

fn split_linear_der_target(
    expr: &Expression,
    state_name: &VarName,
) -> Option<(Expression, Expression)> {
    if is_der_of_state(expr, state_name) {
        return Some((
            Expression::Literal {
                value: Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            },
            zero_literal(),
        ));
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
                make_unary(OpUnary::Minus, coef),
                make_unary(OpUnary::Minus, rem),
            ))
        }
        Expression::Binary { op, lhs, rhs, .. } => match op {
            OpBinary::Add | OpBinary::AddElem => {
                if let Some((coef, rem)) = split_linear_der_target(lhs, state_name)
                    && !expr_contains_der_of(rhs, state_name)
                {
                    return Some((coef, make_binary(OpBinary::Add, rem, *rhs.clone())));
                }
                if let Some((coef, rem)) = split_linear_der_target(rhs, state_name)
                    && !expr_contains_der_of(lhs, state_name)
                {
                    return Some((coef, make_binary(OpBinary::Add, *lhs.clone(), rem)));
                }
                None
            }
            OpBinary::Sub | OpBinary::SubElem => {
                if let Some((coef, rem)) = split_linear_der_target(lhs, state_name)
                    && !expr_contains_der_of(rhs, state_name)
                {
                    return Some((coef, make_binary(OpBinary::Sub, rem, *rhs.clone())));
                }
                if let Some((coef, rem)) = split_linear_der_target(rhs, state_name)
                    && !expr_contains_der_of(lhs, state_name)
                {
                    return Some((
                        make_unary(OpUnary::Minus, coef),
                        make_binary(OpBinary::Sub, *lhs.clone(), rem),
                    ));
                }
                None
            }
            OpBinary::Mul | OpBinary::MulElem => {
                if is_target(lhs) && !expr_contains_der_of(rhs, state_name) {
                    return Some((*rhs.clone(), zero_literal()));
                }
                if is_target(rhs) && !expr_contains_der_of(lhs, state_name) {
                    return Some((*lhs.clone(), zero_literal()));
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
        span: rumoca_core::Span::DUMMY,
    } = rhs
    {
        if is_der_of_state(row_rhs, state_name) {
            return Some(*lhs.clone());
        }
        if is_der_of_state(lhs, state_name) {
            return Some(*row_rhs.clone());
        }
    }

    let (coef, remainder) = split_linear_der_target(rhs, state_name)?;
    Some(make_binary(
        OpBinary::Div,
        make_unary(OpUnary::Minus, remainder),
        coef,
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
            elements: values,
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        }),
        [rows, cols] if *rows >= 0 && *cols >= 0 => {
            let rows = *rows as usize;
            let cols = *cols as usize;
            if rows.checked_mul(cols)? != values.len() {
                return None;
            }
            let elements = values
                .chunks(cols)
                .map(|row| Expression::Array {
                    elements: row.to_vec(),
                    is_matrix: false,
                    span: rumoca_core::Span::DUMMY,
                })
                .collect();
            Some(Expression::Array {
                elements,
                is_matrix: true,
                span: rumoca_core::Span::DUMMY,
            })
        }
        _ => Some(Expression::Array {
            elements: values,
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        }),
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
    ) -> Option<Expression> {
        if name.as_str() == "time" {
            return Some(Expression::Literal {
                value: Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            });
        }
        if self.dae.variables.parameters.contains_key(name)
            || self.dae.variables.constants.contains_key(name)
        {
            return Some(zero_literal());
        }
        if !subscripts.is_empty()
            && !self.dae.variables.states.contains_key(name)
            && let Some(derivative) = self.der_map.get(name.as_str())
            && let Some(dims) = variable_dims_for_name(self.dae, name)
            && let Some(indices) = static_subscript_indices(subscripts)
            && let Some(flat_index) = flat_index_from_indices(&dims, &indices)
        {
            return project_flat_index(derivative, &dims, flat_index);
        }
        if !subscripts.is_empty() && variable_dims_for_name(self.dae, name).is_some() {
            let span = subscripts.first().map(Subscript::span).unwrap_or_default();
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
        active_functions: &mut Vec<VarName>,
    ) -> Option<Expression> {
        match op {
            OpBinary::Add | OpBinary::AddElem => Some(make_binary(
                OpBinary::Add,
                self.differentiate(lhs, active_functions)?,
                self.differentiate(rhs, active_functions)?,
            )),
            OpBinary::Sub | OpBinary::SubElem => Some(make_binary(
                OpBinary::Sub,
                self.differentiate(lhs, active_functions)?,
                self.differentiate(rhs, active_functions)?,
            )),
            OpBinary::Mul | OpBinary::MulElem => {
                if let Some(dot) = self.differentiate_vector_dot(lhs, rhs, active_functions) {
                    return Some(dot);
                }
                let da_b = make_binary(
                    OpBinary::Mul,
                    self.differentiate(lhs, active_functions)?,
                    rhs.clone(),
                );
                let a_db = make_binary(
                    OpBinary::Mul,
                    lhs.clone(),
                    self.differentiate(rhs, active_functions)?,
                );
                Some(make_binary(OpBinary::Add, da_b, a_db))
            }
            OpBinary::Div | OpBinary::DivElem => {
                let da_b = make_binary(
                    OpBinary::Mul,
                    self.differentiate(lhs, active_functions)?,
                    rhs.clone(),
                );
                let a_db = make_binary(
                    OpBinary::Mul,
                    lhs.clone(),
                    self.differentiate(rhs, active_functions)?,
                );
                let numer = make_binary(OpBinary::Sub, da_b, a_db);
                let denom = make_binary(OpBinary::Mul, rhs.clone(), rhs.clone());
                Some(make_binary(OpBinary::Div, numer, denom))
            }
            _ => None,
        }
    }

    fn differentiate_vector_dot(
        &self,
        lhs: &Expression,
        rhs: &Expression,
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
                );
                let a_db = make_binary(
                    OpBinary::Mul,
                    lhs_i,
                    self.differentiate(&rhs_i, active_functions)?,
                );
                Some(make_binary(OpBinary::Add, da_b, a_db))
            })
            .collect::<Option<Vec<_>>>()?;
        Some(sum_terms(terms))
    }

    fn differentiate_unary(
        &self,
        op: &OpUnary,
        rhs: &Expression,
        active_functions: &mut Vec<VarName>,
    ) -> Option<Expression> {
        match op {
            OpUnary::Minus | OpUnary::DotMinus => Some(make_unary(
                OpUnary::Minus,
                self.differentiate(rhs, active_functions)?,
            )),
            OpUnary::Plus | OpUnary::DotPlus => self.differentiate(rhs, active_functions),
            _ => None,
        }
    }

    fn differentiate_if(
        &self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
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
            span: rumoca_core::Span::DUMMY,
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
        if active_functions.iter().any(|active| active == name) {
            return None;
        }
        let function = self.dae.symbols.functions.get(name)?;
        if !function.pure || function.external.is_some() || function.outputs.len() != 1 {
            return None;
        }
        active_functions.push(name.clone());
        let Some(output_expr) = function_output_expression(function, args) else {
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
            Expression::Literal { value: _, .. } => Some(zero_literal()),
            Expression::VarRef {
                name, subscripts, ..
            } => self.differentiate_variable(name.var_name(), subscripts),
            Expression::Binary { op, lhs, rhs, .. } => {
                self.differentiate_binary(op, lhs, rhs, active_functions)
            }
            Expression::Unary { op, rhs, .. } => {
                self.differentiate_unary(op, rhs, active_functions)
            }
            Expression::If {
                branches,
                else_branch,
                ..
            } => self.differentiate_if(branches, else_branch, active_functions),
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
) -> Option<Expression> {
    let output = function.outputs.first()?;
    let mut scope = HashMap::new();
    bind_function_inputs(function, args, &mut scope)?;
    for statement in &function.body {
        apply_function_assignment(statement, &mut scope)?;
    }
    let expr = scope.get(output.name.as_str())?.clone();
    if output.dims == [1] {
        return scalar_array_element(&expr);
    }
    Some(expr)
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
    match expr {
        Expression::VarRef {
            name,
            subscripts,
            span,
        } if subscripts.is_empty() => {
            let indices = dae::flat_index_to_subscripts(dims, flat_index)?;
            Some(Expression::VarRef {
                name: name.clone(),
                subscripts: indices
                    .into_iter()
                    .map(|idx| Subscript::generated_index(idx as i64, *span))
                    .collect(),
                span: *span,
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
            let span = expr.span().unwrap_or(rumoca_core::Span::DUMMY);
            Some(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![project_flat_index(&args[0], dims, flat_index)?],
                span,
            })
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let span = expr.span().unwrap_or(rumoca_core::Span::DUMMY);
            Some(Expression::Binary {
                op: op.clone(),
                lhs: Box::new(project_flat_index(lhs, dims, flat_index)?),
                rhs: Box::new(project_flat_index(rhs, dims, flat_index)?),
                span,
            })
        }
        Expression::Unary { op, rhs, .. } => {
            let span = expr.span().unwrap_or(rumoca_core::Span::DUMMY);
            Some(Expression::Unary {
                op: op.clone(),
                rhs: Box::new(project_flat_index(rhs, dims, flat_index)?),
                span,
            })
        }
        _ => {
            let indices = dae::flat_index_to_subscripts(dims, flat_index)?;
            let span = expr.span()?;
            Some(Expression::Index {
                base: Box::new(expr.clone()),
                subscripts: indices
                    .into_iter()
                    .map(|idx| Subscript::generated_index(idx as i64, span))
                    .collect(),
                span,
            })
        }
    }
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

fn sum_terms(mut terms: Vec<Expression>) -> Expression {
    if terms.is_empty() {
        return zero_literal();
    }
    let first = terms.remove(0);
    terms
        .into_iter()
        .fold(first, |lhs, rhs| make_binary(OpBinary::Add, lhs, rhs))
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
        Expression::Binary { op, lhs, rhs, .. } => Expression::Binary {
            op: op.clone(),
            lhs: Box::new(expand_der_in_expr_full(lhs, dae, der_map, state_names)),
            rhs: Box::new(expand_der_in_expr_full(rhs, dae, der_map, state_names)),
            span: rumoca_core::Span::DUMMY,
        },
        Expression::Unary { op, rhs, .. } => Expression::Unary {
            op: op.clone(),
            rhs: Box::new(expand_der_in_expr_full(rhs, dae, der_map, state_names)),
            span: rumoca_core::Span::DUMMY,
        },
        Expression::BuiltinCall { function, args, .. } => Expression::BuiltinCall {
            function: *function,
            args: args
                .iter()
                .map(|a| expand_der_in_expr_full(a, dae, der_map, state_names))
                .collect(),
            span: rumoca_core::Span::DUMMY,
        },
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } => Expression::FunctionCall {
            name: name.clone(),
            args: args
                .iter()
                .map(|a| expand_der_in_expr_full(a, dae, der_map, state_names))
                .collect(),
            is_constructor: *is_constructor,
            span: rumoca_core::Span::DUMMY,
        },
        Expression::If {
            branches,
            else_branch,
            ..
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
            span: rumoca_core::Span::DUMMY,
        },
        Expression::Array {
            elements,
            is_matrix,
            ..
        } => Expression::Array {
            elements: elements
                .iter()
                .map(|e| expand_der_in_expr_full(e, dae, der_map, state_names))
                .collect(),
            is_matrix: *is_matrix,
            span: rumoca_core::Span::DUMMY,
        },
        Expression::Index {
            base, subscripts, ..
        } => Expression::Index {
            base: Box::new(expand_der_in_expr_full(base, dae, der_map, state_names)),
            subscripts: subscripts.clone(),
            span: rumoca_core::Span::DUMMY,
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
