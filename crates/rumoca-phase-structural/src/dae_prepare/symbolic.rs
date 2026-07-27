use super::*;
use rumoca_core::{ExpressionRewriter, Span};
use std::cell::RefCell;

#[cfg(test)]
mod differentiation_tests;
mod shape_projection;
mod zero_fold;
use shape_projection::*;

fn der_target<'a>(expr: &'a Expression, state_name: &VarName) -> Option<&'a Expression> {
    match expr {
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } if args.len() == 1 && expr_refers_to_var(&args[0], state_name) => args.first(),
        _ => None,
    }
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

fn make_builtin(function: BuiltinFunction, arg: Expression, span: Span) -> Expression {
    Expression::BuiltinCall {
        function,
        args: vec![arg],
        span,
    }
}

fn real_literal(value: f64, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span,
    }
}

fn zeros_for_dims(dims: &[i64], span: Span) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Zeros,
        args: dims
            .iter()
            .map(|dim| Expression::Literal {
                value: Literal::Integer(*dim),
                span,
            })
            .collect(),
        span,
    }
}

struct LinearDerivative {
    coefficient: Expression,
    remainder: Expression,
    target: Expression,
}

fn split_linear_der_target(expr: &Expression, state_name: &VarName) -> Option<LinearDerivative> {
    let span = expr.span()?;
    if let Some(target) = der_target(expr, state_name) {
        return Some(LinearDerivative {
            coefficient: real_literal(1.0, span),
            remainder: real_literal(0.0, span),
            target: target.clone(),
        });
    }

    let is_target = |e: &Expression| der_target(e, state_name).is_some();
    match expr {
        Expression::Unary {
            op: OpUnary::Minus | OpUnary::DotMinus,
            rhs,
            ..
        } => {
            let split = split_linear_der_target(rhs, state_name)?;
            Some(LinearDerivative {
                coefficient: make_unary(OpUnary::Minus, split.coefficient, span),
                remainder: make_unary(OpUnary::Minus, split.remainder, span),
                target: split.target,
            })
        }
        Expression::Binary { op, lhs, rhs, .. } => match op {
            OpBinary::Add | OpBinary::AddElem => {
                if let Some(split) = split_linear_der_target(lhs, state_name)
                    && !expr_contains_der_of(rhs, state_name)
                {
                    return Some(LinearDerivative {
                        coefficient: split.coefficient,
                        remainder: make_binary(OpBinary::Add, split.remainder, *rhs.clone(), span),
                        target: split.target,
                    });
                }
                if let Some(split) = split_linear_der_target(rhs, state_name)
                    && !expr_contains_der_of(lhs, state_name)
                {
                    return Some(LinearDerivative {
                        coefficient: split.coefficient,
                        remainder: make_binary(OpBinary::Add, *lhs.clone(), split.remainder, span),
                        target: split.target,
                    });
                }
                None
            }
            OpBinary::Sub | OpBinary::SubElem => {
                if let Some(split) = split_linear_der_target(lhs, state_name)
                    && !expr_contains_der_of(rhs, state_name)
                {
                    return Some(LinearDerivative {
                        coefficient: split.coefficient,
                        remainder: make_binary(OpBinary::Sub, split.remainder, *rhs.clone(), span),
                        target: split.target,
                    });
                }
                if let Some(split) = split_linear_der_target(rhs, state_name)
                    && !expr_contains_der_of(lhs, state_name)
                {
                    return Some(LinearDerivative {
                        coefficient: make_unary(OpUnary::Minus, split.coefficient, span),
                        remainder: make_binary(OpBinary::Sub, *lhs.clone(), split.remainder, span),
                        target: split.target,
                    });
                }
                None
            }
            OpBinary::Mul | OpBinary::MulElem => {
                if is_target(lhs) && !expr_contains_der_of(rhs, state_name) {
                    return Some(LinearDerivative {
                        coefficient: *rhs.clone(),
                        remainder: real_literal(0.0, span),
                        target: der_target(lhs, state_name)?.clone(),
                    });
                }
                if is_target(rhs) && !expr_contains_der_of(lhs, state_name) {
                    return Some(LinearDerivative {
                        coefficient: *lhs.clone(),
                        remainder: real_literal(0.0, span),
                        target: der_target(rhs, state_name)?.clone(),
                    });
                }
                None
            }
            _ => None,
        },
        _ => None,
    }
}

pub(super) struct DerivativeAssignment {
    pub(super) value: Expression,
    pub(super) target: Expression,
}

pub(super) fn try_extract_der_assignment(
    rhs: &Expression,
    state_name: &VarName,
) -> Option<DerivativeAssignment> {
    if let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs: row_rhs,
        ..
    } = rhs
    {
        if let Some(target) = der_target(row_rhs, state_name) {
            return Some(DerivativeAssignment {
                value: *lhs.clone(),
                target: target.clone(),
            });
        }
        if let Some(target) = der_target(lhs, state_name) {
            return Some(DerivativeAssignment {
                value: *row_rhs.clone(),
                target: target.clone(),
            });
        }
    }

    let span = rhs.span()?;
    let split = split_linear_der_target(rhs, state_name)?;
    Some(DerivativeAssignment {
        value: make_binary(
            OpBinary::Div,
            make_unary(OpUnary::Minus, split.remainder, span),
            split.coefficient,
            span,
        ),
        target: split.target,
    })
}

pub(super) fn try_extract_der_value(rhs: &Expression, state_name: &VarName) -> Option<Expression> {
    try_extract_der_assignment(rhs, state_name).map(|assignment| assignment.value)
}

pub(super) fn build_der_value_map(dae: &Dae) -> HashMap<String, Expression> {
    let equation_index = DerivativeEquationIndex::build(dae);
    let mut map = HashMap::new();
    for (state_name, variable) in &dae.variables.states {
        let value = if variable.dims.is_empty() {
            build_scalar_der_value(dae, state_name, &equation_index)
        } else {
            build_ranked_der_value(dae, state_name, &variable.dims, &equation_index)
        };
        if let Some(value) = value {
            map.insert(state_name.as_str().to_string(), value);
        }
    }
    map
}

#[derive(Default)]
struct DerivativeEquationIndex {
    by_target: HashMap<String, Vec<usize>>,
    projected_owners: HashSet<String>,
}

impl DerivativeEquationIndex {
    fn build(dae: &Dae) -> Self {
        let mut index = Self::default();
        for (equation_index, equation) in dae.continuous.equations.iter().enumerate() {
            let mut collector = DerivativeTargetCollector {
                equation_index,
                index: &mut index,
            };
            collector.visit_expression(&equation.rhs);
        }
        index
    }

    fn equations_for(&self, target: &str) -> &[usize] {
        self.by_target.get(target).map(Vec::as_slice).unwrap_or(&[])
    }
}

struct DerivativeTargetCollector<'a> {
    equation_index: usize,
    index: &'a mut DerivativeEquationIndex,
}

impl rumoca_core::ExpressionVisitor for DerivativeTargetCollector<'_> {
    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if *function == BuiltinFunction::Der
            && let [arg] = args
        {
            match derivative_argument_key(arg) {
                DerivativeArgumentKey::Reference(target) => self
                    .index
                    .by_target
                    .entry(target)
                    .or_default()
                    .push(self.equation_index),
                DerivativeArgumentKey::Expression(_) => self.record_projection_owner(arg),
            }
            return;
        }
        self.walk_builtin_call(function, args);
    }
}

impl DerivativeTargetCollector<'_> {
    fn record_projection_owner(&mut self, arg: &Expression) {
        let Some(owner) = derivative_projection_owner(arg) else {
            return;
        };
        self.index.projected_owners.insert(owner);
    }
}

fn derivative_projection_owner(expr: &Expression) -> Option<String> {
    match expr {
        Expression::VarRef { name, .. } => Some(name.as_str().to_string()),
        Expression::Index { base, .. } | Expression::FieldAccess { base, .. } => {
            derivative_projection_owner(base)
        }
        _ => None,
    }
}

fn build_scalar_der_value(
    dae: &Dae,
    state_name: &VarName,
    equation_index: &DerivativeEquationIndex,
) -> Option<Expression> {
    let [row] = equation_index.equations_for(state_name.as_str()) else {
        return None;
    };
    let value = try_extract_der_value(&dae.continuous.equations[*row].rhs, state_name)?;
    (!expr_contains_der_of(&value, state_name)).then_some(value)
}

fn build_ranked_der_value(
    dae: &Dae,
    state_name: &VarName,
    dims: &[i64],
    equation_index: &DerivativeEquationIndex,
) -> Option<Expression> {
    if dims.iter().any(|dim| *dim <= 0)
        || equation_index
            .projected_owners
            .contains(state_name.as_str())
    {
        return None;
    }

    let aggregate_rows = equation_index.equations_for(state_name.as_str());
    let scalar_names = scalar_names_for_dims(state_name, dims)?;
    let has_component_rows = scalar_names
        .iter()
        .any(|name| !equation_index.equations_for(name.as_str()).is_empty());
    if !aggregate_rows.is_empty() {
        let [row] = aggregate_rows else {
            return None;
        };
        if has_component_rows {
            return None;
        }
        let value = try_extract_der_value(&dae.continuous.equations[*row].rhs, state_name)?;
        return (expression_dims(&value, dae).as_deref() == Some(dims)
            && !expr_contains_der_of(&value, state_name))
        .then_some(value);
    }

    build_array_der_value(dae, state_name, dims, &scalar_names, equation_index)
}

fn scalar_names_for_dims(state_name: &VarName, dims: &[i64]) -> Option<Vec<VarName>> {
    let size = dims.iter().try_fold(1usize, |acc, dim| {
        (*dim > 0).then(|| acc.checked_mul(*dim as usize)).flatten()
    })?;
    Some(
        (0..size)
            .map(|flat_index| dae::scalar_name_for_flat_index(state_name, dims, flat_index))
            .collect(),
    )
}

fn build_array_der_value(
    dae: &Dae,
    state_name: &VarName,
    dims: &[i64],
    scalar_names: &[VarName],
    equation_index: &DerivativeEquationIndex,
) -> Option<Expression> {
    let size = scalar_names.len();
    let mut values = Vec::with_capacity(size);
    for scalar_name in scalar_names {
        let [row] = equation_index.equations_for(scalar_name.as_str()) else {
            return None;
        };
        let value = try_extract_der_value(&dae.continuous.equations[*row].rhs, scalar_name)?;
        if expr_contains_der_of(&value, state_name) {
            return None;
        }
        values.push(value);
    }
    array_expr_from_flat_values(values, dims)
}

pub(super) fn array_expr_from_flat_values(
    values: Vec<Expression>,
    dims: &[i64],
) -> Option<Expression> {
    let dims = dims
        .iter()
        .map(|dim| usize::try_from(*dim).ok().filter(|dim| *dim > 0))
        .collect::<Option<Vec<_>>>()?;
    let expected = dims
        .iter()
        .try_fold(1usize, |size, dim| size.checked_mul(*dim))?;
    if dims.is_empty() || expected != values.len() {
        return None;
    }
    nested_array_expr(&values, &dims)
}

fn nested_array_expr(values: &[Expression], dims: &[usize]) -> Option<Expression> {
    let [extent, tail @ ..] = dims else {
        return None;
    };
    if tail.is_empty() {
        if *extent != values.len() {
            return None;
        }
        return Some(Expression::Array {
            span: expression_sequence_span(values)?,
            elements: values.to_vec(),
            is_matrix: false,
        });
    }
    let chunk_size = tail
        .iter()
        .try_fold(1usize, |size, dim| size.checked_mul(*dim))?;
    if extent.checked_mul(chunk_size)? != values.len() {
        return None;
    }
    let elements = values
        .chunks(chunk_size)
        .map(|chunk| nested_array_expr(chunk, tail))
        .collect::<Option<Vec<_>>>()?;
    Some(Expression::Array {
        span: expression_sequence_span(&elements)?,
        elements,
        is_matrix: dims.len() == 2,
    })
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

struct SymbolicDerivativeContext<'a> {
    dae: &'a Dae,
    der_map: &'a HashMap<String, Expression>,
    active_derivative_args: RefCell<Vec<DerivativeArgumentKey>>,
}

#[derive(Clone, Debug, PartialEq)]
enum DerivativeArgumentKey {
    Reference(String),
    Expression(Expression),
}

fn derivative_argument_key(expr: &Expression) -> DerivativeArgumentKey {
    let reference = match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => Some((name.as_str().to_string(), subscripts.as_slice())),
        Expression::Index {
            base, subscripts, ..
        } => match base.as_ref() {
            Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } if base_subscripts.is_empty() => {
                Some((name.as_str().to_string(), subscripts.as_slice()))
            }
            _ => None,
        },
        _ => None,
    };
    let Some((name, subscripts)) = reference else {
        return DerivativeArgumentKey::Expression(expr.clone());
    };
    if subscripts.is_empty() {
        return DerivativeArgumentKey::Reference(name);
    }
    let Some(indices) = static_subscript_indices(subscripts) else {
        return DerivativeArgumentKey::Expression(expr.clone());
    };
    DerivativeArgumentKey::Reference(format!(
        "{name}[{}]",
        indices
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(",")
    ))
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
            let dims = self
                .dae
                .variables
                .parameters
                .get(name)
                .or_else(|| self.dae.variables.constants.get(name))?
                .dims
                .as_slice();
            return Some(if dims.is_empty() {
                real_literal(0.0, span)
            } else {
                zeros_for_dims(dims, span)
            });
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
                self.dae,
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
        active_functions: &mut Vec<rumoca_core::FunctionInstanceId>,
    ) -> Option<Expression> {
        match op {
            OpBinary::Add | OpBinary::AddElem => Some(zero_fold::binary(
                self.dae,
                OpBinary::Add,
                self.differentiate(lhs, active_functions)?,
                self.differentiate(rhs, active_functions)?,
                span,
            )),
            OpBinary::Sub | OpBinary::SubElem => Some(zero_fold::binary(
                self.dae,
                OpBinary::Sub,
                self.differentiate(lhs, active_functions)?,
                self.differentiate(rhs, active_functions)?,
                span,
            )),
            OpBinary::Mul | OpBinary::MulElem => {
                if let Some(dot) = self.differentiate_vector_dot(lhs, rhs, span, active_functions) {
                    return Some(dot);
                }
                let (da_b, a_db) = self.product_rule_terms(lhs, rhs, span, active_functions)?;
                Some(zero_fold::binary(self.dae, OpBinary::Add, da_b, a_db, span))
            }
            OpBinary::Div | OpBinary::DivElem => {
                let (da_b, a_db) = self.product_rule_terms(lhs, rhs, span, active_functions)?;
                let numer = zero_fold::binary(self.dae, OpBinary::Sub, da_b, a_db, span);
                let denom = make_binary(OpBinary::Mul, rhs.clone(), rhs.clone(), span);
                Some(make_binary(OpBinary::Div, numer, denom, span))
            }
            _ => None,
        }
    }

    /// `(d(lhs) * rhs, lhs * d(rhs))`, cloning an operand only when its term
    /// survives structural zero folding.
    fn product_rule_terms(
        &self,
        lhs: &Expression,
        rhs: &Expression,
        span: Span,
        active_functions: &mut Vec<rumoca_core::FunctionInstanceId>,
    ) -> Option<(Expression, Expression)> {
        let lhs_derivative = self.differentiate(lhs, active_functions)?;
        let da_b = zero_fold::scaled(self.dae, OpBinary::Mul, lhs_derivative, rhs, true, span);
        let rhs_derivative = self.differentiate(rhs, active_functions)?;
        let a_db = zero_fold::scaled(self.dae, OpBinary::Mul, rhs_derivative, lhs, false, span);
        Some((da_b, a_db))
    }

    fn differentiate_vector_dot(
        &self,
        lhs: &Expression,
        rhs: &Expression,
        span: Span,
        active_functions: &mut Vec<rumoca_core::FunctionInstanceId>,
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
                let lhs_i = project_flat_index(lhs, &lhs_dims, idx, self.dae)?;
                let rhs_i = project_flat_index(rhs, &rhs_dims, idx, self.dae)?;
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
        active_functions: &mut Vec<rumoca_core::FunctionInstanceId>,
    ) -> Option<Expression> {
        match op {
            OpUnary::Minus | OpUnary::DotMinus => Some(zero_fold::unary(
                self.dae,
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
        active_functions: &mut Vec<rumoca_core::FunctionInstanceId>,
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
        name: &rumoca_core::Reference,
        args: &[Expression],
        is_constructor: bool,
        active_functions: &mut Vec<rumoca_core::FunctionInstanceId>,
    ) -> Option<Expression> {
        self.differentiate_function_output(name, args, is_constructor, None, active_functions)
    }

    fn differentiate_function_output(
        &self,
        name: &rumoca_core::Reference,
        args: &[Expression],
        is_constructor: bool,
        field: Option<&str>,
        active_functions: &mut Vec<rumoca_core::FunctionInstanceId>,
    ) -> Option<Expression> {
        if is_constructor {
            return None;
        }
        let Some((instance_id, function, output_selector)) = resolve_function_call(self.dae, name)
        else {
            report_undifferentiable_call(name, "call does not resolve to a known function");
            return None;
        };
        if active_functions.contains(&instance_id) {
            return None;
        }
        if !function.pure || function.external.is_some() || function.outputs.len() != 1 {
            report_undifferentiable_call(
                name,
                "only a pure, non-external, single-output function body can be differentiated",
            );
            return None;
        }
        active_functions.push(instance_id);
        let Some(output_expr) =
            function_output_expression(function, args, output_selector.as_ref(), field, self.dae)
        else {
            active_functions.pop();
            report_undifferentiable_call(
                name,
                "function body does not reduce to a single output expression",
            );
            return None;
        };
        let derivative = self.differentiate(&output_expr, active_functions);
        active_functions.pop();
        if derivative.is_none() {
            report_undifferentiable_call(name, "function body contains a non-differentiable term");
        }
        derivative
    }

    fn differentiate_builtin_call(
        &self,
        function: BuiltinFunction,
        args: &[Expression],
        span: Span,
        active_functions: &mut Vec<rumoca_core::FunctionInstanceId>,
    ) -> Option<Expression> {
        if matches!(
            function,
            BuiltinFunction::Zeros
                | BuiltinFunction::Ones
                | BuiltinFunction::Identity
                | BuiltinFunction::OuterProduct
                | BuiltinFunction::Cross
                | BuiltinFunction::Skew
                | BuiltinFunction::Transpose
        ) {
            return self.differentiate_array_builtin(function, args, span, active_functions);
        }
        if let (BuiltinFunction::Atan2, [numerator, denominator]) = (function, args) {
            return self.differentiate_atan2(numerator, denominator, span, active_functions);
        }
        let [arg] = args else {
            return None;
        };
        let derivative = self.differentiate(arg, active_functions)?;
        differentiate_scalar_builtin(function, arg, derivative, span)
    }

    /// `d/dt atan2(y, x) = (x*dy - y*dx) / (x^2 + y^2)`.
    ///
    /// MLS 3.7 §3.7.1 defines `atan2(y, x)` as the four-quadrant arctangent of
    /// `y/x`; it is smooth wherever `(x, y) != (0, 0)`, which is exactly where
    /// the loop-closure constraints that use it are well posed.
    fn differentiate_atan2(
        &self,
        y: &Expression,
        x: &Expression,
        span: Span,
        active_functions: &mut Vec<rumoca_core::FunctionInstanceId>,
    ) -> Option<Expression> {
        let square = |value: Expression| make_binary(OpBinary::Mul, value.clone(), value, span);
        let dy = self.differentiate(y, active_functions)?;
        let dx = self.differentiate(x, active_functions)?;
        let numerator = make_binary(
            OpBinary::Sub,
            make_binary(OpBinary::Mul, x.clone(), dy, span),
            make_binary(OpBinary::Mul, y.clone(), dx, span),
            span,
        );
        let denominator = make_binary(OpBinary::Add, square(x.clone()), square(y.clone()), span);
        Some(make_binary(OpBinary::Div, numerator, denominator, span))
    }

    fn differentiate_array_builtin(
        &self,
        function: BuiltinFunction,
        args: &[Expression],
        span: Span,
        active_functions: &mut Vec<rumoca_core::FunctionInstanceId>,
    ) -> Option<Expression> {
        match (function, args) {
            (BuiltinFunction::Zeros | BuiltinFunction::Ones, dimensions) => {
                Some(Expression::BuiltinCall {
                    function: BuiltinFunction::Zeros,
                    args: dimensions.to_vec(),
                    span,
                })
            }
            (BuiltinFunction::Identity, [n]) => Some(Expression::BuiltinCall {
                function: BuiltinFunction::Zeros,
                args: vec![n.clone(), n.clone()],
                span,
            }),
            (BuiltinFunction::OuterProduct, [lhs, rhs]) => {
                let lhs_derivative = self.differentiate(lhs, active_functions)?;
                let lhs_term =
                    zero_fold::outer_product_term(self.dae, lhs_derivative, rhs, true, span);
                let rhs_derivative = self.differentiate(rhs, active_functions)?;
                let rhs_term =
                    zero_fold::outer_product_term(self.dae, rhs_derivative, lhs, false, span);
                Some(zero_fold::binary(
                    self.dae,
                    OpBinary::Add,
                    lhs_term,
                    rhs_term,
                    span,
                ))
            }
            (BuiltinFunction::Cross, [lhs, rhs]) => {
                let lhs_derivative = self.differentiate(lhs, active_functions)?;
                let lhs_term = zero_fold::array_builtin(
                    self.dae,
                    BuiltinFunction::Cross,
                    vec![lhs_derivative, rhs.clone()],
                    span,
                );
                let rhs_derivative = self.differentiate(rhs, active_functions)?;
                let rhs_term = zero_fold::array_builtin(
                    self.dae,
                    BuiltinFunction::Cross,
                    vec![lhs.clone(), rhs_derivative],
                    span,
                );
                Some(zero_fold::binary(
                    self.dae,
                    OpBinary::Add,
                    lhs_term,
                    rhs_term,
                    span,
                ))
            }
            (BuiltinFunction::Skew | BuiltinFunction::Transpose, [arg]) => {
                Some(zero_fold::array_builtin(
                    self.dae,
                    function,
                    vec![self.differentiate(arg, active_functions)?],
                    span,
                ))
            }
            _ => None,
        }
    }

    fn differentiate(
        &self,
        expr: &Expression,
        active_functions: &mut Vec<rumoca_core::FunctionInstanceId>,
    ) -> Option<Expression> {
        self.differentiate_inner(expr, active_functions)
    }

    fn differentiate_inner(
        &self,
        expr: &Expression,
        active_functions: &mut Vec<rumoca_core::FunctionInstanceId>,
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
            } => self.differentiate_function_call(name, args, *is_constructor, active_functions),
            Expression::FieldAccess { base, field, span } => match base.as_ref() {
                Expression::FunctionCall {
                    name,
                    args,
                    is_constructor,
                    ..
                } => self.differentiate_function_output(
                    name,
                    args,
                    *is_constructor,
                    Some(field),
                    active_functions,
                ),
                _ => {
                    let projected = super::record_projection::project_record_field(
                        self.dae, base, field, *span,
                    )?;
                    self.differentiate(&projected, active_functions)
                }
            },
            Expression::Index {
                base,
                subscripts,
                span,
            } => self.differentiate_index(base, subscripts, *span, active_functions),
            // d/dt(der(X)) — a higher-order derivative (successive `Der` blocks,
            // or a relative acceleration `a = der(der(phi))`). `der(X)` is X's
            // first time-derivative; differentiate that expression to climb one
            // order. This lets `compute_full_derivative_map` resolve derivative
            // chains whose links are themselves `der(...)` definitions.
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args,
                ..
            } if args.len() == 1 => self.differentiate_der_call(&args[0], active_functions),
            Expression::BuiltinCall {
                function,
                args,
                span,
            } => self.differentiate_builtin_call(*function, args, *span, active_functions),
            _ => None,
        }
    }

    /// `d/dt (base[subscripts])` for compile-time-constant subscripts.
    ///
    /// A fully indexed element commutes with differentiation, so it differentiates
    /// the base and re-indexes. A slice (`A[1, :]`, as MSL's `Frames` constraint
    /// functions write their row selections) instead expands element by element:
    /// each element then reaches [`Self::differentiate_variable`] as an indexed
    /// reference and becomes a `der(A[1, k])` leaf the DAE can close, where
    /// differentiating the whole array would demand a derivative for a matrix
    /// that no equation defines as a whole.
    fn differentiate_index(
        &self,
        base: &Expression,
        subscripts: &[Subscript],
        span: Span,
        active_functions: &mut Vec<rumoca_core::FunctionInstanceId>,
    ) -> Option<Expression> {
        if static_subscript_indices(subscripts).is_some() {
            return Some(Expression::Index {
                base: Box::new(self.differentiate(base, active_functions)?),
                subscripts: subscripts.to_vec(),
                span,
            });
        }
        let dims = sliced_dims(base, subscripts, self.dae)?;
        let size = dims
            .iter()
            .try_fold(1usize, |size, dim| {
                usize::try_from(*dim)
                    .ok()
                    .and_then(|dim| size.checked_mul(dim))
            })
            .filter(|size| *size > 0)?;
        let slice = Expression::Index {
            base: Box::new(base.clone()),
            subscripts: subscripts.to_vec(),
            span,
        };
        let values = (0..size)
            .map(|flat_index| {
                let element = project_flat_index(&slice, &dims, flat_index, self.dae)?;
                self.differentiate(&element, active_functions)
            })
            .collect::<Option<Vec<_>>>()?;
        array_expr_from_flat_values(values, &dims)
    }

    /// Differentiate `der(arg)` one order higher: take `arg`'s first derivative
    /// and differentiate it again.
    fn differentiate_der_call(
        &self,
        arg: &Expression,
        active_functions: &mut Vec<rumoca_core::FunctionInstanceId>,
    ) -> Option<Expression> {
        let key = derivative_argument_key(arg);
        {
            let mut active = self.active_derivative_args.borrow_mut();
            if active.contains(&key) {
                return None;
            }
            active.push(key.clone());
        }

        let result = (|| {
            let first_derivative = self.first_derivative_of_argument(arg, active_functions)?;
            self.differentiate(&first_derivative, active_functions)
        })();

        let popped = self.active_derivative_args.borrow_mut().pop();
        debug_assert_eq!(popped, Some(key));
        result
    }

    fn first_derivative_of_argument(
        &self,
        arg: &Expression,
        active_functions: &mut Vec<rumoca_core::FunctionInstanceId>,
    ) -> Option<Expression> {
        let Expression::VarRef {
            name, subscripts, ..
        } = arg
        else {
            return self.differentiate(arg, active_functions);
        };
        if !subscripts.is_empty() {
            return self.differentiate(arg, active_functions);
        }
        let first = self.der_map.get(name.var_name().as_str()).cloned()?;
        (!expr_contains_der_of(&first, name.var_name())).then_some(first)
    }
}

/// Record why a Modelica function call blocked symbolic differentiation.
///
/// SPEC_0008: a differentiation that cannot proceed must not vanish. The caller
/// still returns `None` — index reduction is allowed to try another candidate —
/// but the reason is now attributable to a named function instead of surfacing
/// later as an unmatched column in `ES010`.
fn report_undifferentiable_call(name: &rumoca_core::Reference, reason: &str) {
    crate::structural_trace!(
        "[sim-trace] symbolic derivative blocked function={} reason={}",
        name.as_str(),
        reason
    );
}

fn differentiate_scalar_builtin(
    function: BuiltinFunction,
    arg: &Expression,
    derivative: Expression,
    span: Span,
) -> Option<Expression> {
    if matches!(
        function,
        BuiltinFunction::Sin
            | BuiltinFunction::Cos
            | BuiltinFunction::Tan
            | BuiltinFunction::Asin
            | BuiltinFunction::Acos
            | BuiltinFunction::Atan
    ) {
        return differentiate_trigonometric_builtin(function, arg, derivative, span);
    }
    let builtin = |function| make_builtin(function, arg.clone(), span);
    let square = |value: Expression| make_binary(OpBinary::Mul, value.clone(), value, span);
    match function {
        BuiltinFunction::Sinh => Some(make_binary(
            OpBinary::Mul,
            builtin(BuiltinFunction::Cosh),
            derivative,
            span,
        )),
        BuiltinFunction::Cosh => Some(make_binary(
            OpBinary::Mul,
            builtin(BuiltinFunction::Sinh),
            derivative,
            span,
        )),
        BuiltinFunction::Tanh => Some(make_binary(
            OpBinary::Div,
            derivative,
            square(builtin(BuiltinFunction::Cosh)),
            span,
        )),
        BuiltinFunction::Exp => Some(make_binary(
            OpBinary::Mul,
            builtin(BuiltinFunction::Exp),
            derivative,
            span,
        )),
        BuiltinFunction::Log => Some(make_binary(OpBinary::Div, derivative, arg.clone(), span)),
        BuiltinFunction::Log10 => Some(make_binary(
            OpBinary::Div,
            derivative,
            make_binary(
                OpBinary::Mul,
                arg.clone(),
                real_literal(std::f64::consts::LN_10, span),
                span,
            ),
            span,
        )),
        BuiltinFunction::Sqrt => Some(make_binary(
            OpBinary::Div,
            derivative,
            make_binary(
                OpBinary::Mul,
                real_literal(2.0, span),
                builtin(BuiltinFunction::Sqrt),
                span,
            ),
            span,
        )),
        _ => None,
    }
}

fn differentiate_trigonometric_builtin(
    function: BuiltinFunction,
    arg: &Expression,
    derivative: Expression,
    span: Span,
) -> Option<Expression> {
    let builtin = |function| make_builtin(function, arg.clone(), span);
    let square = |value: Expression| make_binary(OpBinary::Mul, value.clone(), value, span);
    match function {
        BuiltinFunction::Sin => Some(make_binary(
            OpBinary::Mul,
            builtin(BuiltinFunction::Cos),
            derivative,
            span,
        )),
        BuiltinFunction::Cos => Some(make_binary(
            OpBinary::Mul,
            make_unary(OpUnary::Minus, builtin(BuiltinFunction::Sin), span),
            derivative,
            span,
        )),
        BuiltinFunction::Tan => Some(make_binary(
            OpBinary::Div,
            derivative,
            square(builtin(BuiltinFunction::Cos)),
            span,
        )),
        BuiltinFunction::Asin | BuiltinFunction::Acos => {
            let one_minus_square = make_binary(
                OpBinary::Sub,
                real_literal(1.0, span),
                square(arg.clone()),
                span,
            );
            let denominator = make_builtin(BuiltinFunction::Sqrt, one_minus_square, span);
            let quotient = make_binary(OpBinary::Div, derivative, denominator, span);
            if function == BuiltinFunction::Acos {
                Some(make_unary(OpUnary::Minus, quotient, span))
            } else {
                Some(quotient)
            }
        }
        BuiltinFunction::Atan => Some(make_binary(
            OpBinary::Div,
            derivative,
            make_binary(
                OpBinary::Add,
                real_literal(1.0, span),
                square(arg.clone()),
                span,
            ),
            span,
        )),
        _ => None,
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
        active_derivative_args: RefCell::new(Vec::new()),
    }
    .differentiate(expr, &mut Vec::new())
}

fn function_output_expression(
    function: &rumoca_core::Function,
    args: &[Expression],
    output_selector: Option<&FunctionOutputSelector>,
    field: Option<&str>,
    dae: &Dae,
) -> Option<Expression> {
    let output = function.outputs.first()?;
    let mut scope = HashMap::new();
    bind_function_inputs(function, args, &mut scope)?;
    for statement in &function.body {
        apply_function_assignment(statement, &mut scope)?;
    }
    if let Some(field) = field {
        if output_selector.is_some() {
            return None;
        }
        if let Some(expr) = scope.get(&format!("{}.{field}", output.name)) {
            return Some(expr.clone());
        }
        return record_constructor_field_expression(scope.get(&output.name)?, field, dae);
    }
    let expr = scope.get(output.name.as_str())?.clone();
    if let Some(selector) = output_selector {
        if selector.output_name != output.name {
            return None;
        }
        if selector.indices.is_empty() {
            return if output.dims == [1] {
                scalar_array_element(&expr)
            } else {
                Some(expr)
            };
        }
        let flat_index = flat_index_from_indices(&output.dims, &selector.indices)?;
        return project_flat_index(&expr, &output.dims, flat_index, dae);
    }
    if output.dims == [1] {
        return scalar_array_element(&expr);
    }
    Some(expr)
}

fn record_constructor_field_expression(
    expr: &Expression,
    field: &str,
    dae: &Dae,
) -> Option<Expression> {
    let Expression::FunctionCall {
        name,
        args,
        is_constructor: true,
        ..
    } = expr
    else {
        return None;
    };
    let (_, constructor, output_selector) = resolve_function_call(dae, name)?;
    if !constructor.is_constructor || output_selector.is_some() {
        return None;
    }
    let bindings = crate::function_arguments::bind_function_arguments(&constructor.inputs, args)?;
    constructor
        .inputs
        .iter()
        .zip(bindings)
        .find_map(|(input, value)| (input.name == field).then_some(value))
}

#[derive(Clone)]
struct FunctionOutputSelector {
    output_name: String,
    indices: Vec<i64>,
}

fn resolve_function_call<'a>(
    dae: &'a Dae,
    call_name: &rumoca_core::Reference,
) -> Option<(
    rumoca_core::FunctionInstanceId,
    &'a rumoca_core::Function,
    Option<FunctionOutputSelector>,
)> {
    let resolved = call_name.resolved_function()?;
    let function = rumoca_core::resolve_function_instance(
        dae.symbols.functions.values(),
        resolved.instance_id,
    )
    .ok()?;
    let selector = function_projection_selector(resolved, call_name)?;
    Some((resolved.instance_id, function, selector))
}

fn function_projection_selector(
    resolved: rumoca_core::ResolvedFunctionReference,
    call_name: &rumoca_core::Reference,
) -> Option<Option<FunctionOutputSelector>> {
    // A generated call reference (connection-equation residuals build one) carries
    // only the resolved function, no component reference to project through. There
    // is then no trailing output part, so the call names the whole single output.
    let Some(call_ref) = call_name.component_ref() else {
        return Some(None);
    };
    if call_ref.parts.len() == resolved.base_part_count {
        return Some(None);
    }
    if call_ref.parts.len() != resolved.base_part_count + 1 {
        return None;
    }
    let output = call_ref.parts.get(resolved.base_part_count)?;
    let indices = output
        .subs
        .iter()
        .map(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } if *value > 0 => Some(*value),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;
    Some(Some(FunctionOutputSelector {
        output_name: output.ident.clone(),
        indices,
    }))
}

fn bind_function_inputs(
    function: &rumoca_core::Function,
    args: &[Expression],
    scope: &mut HashMap<String, Expression>,
) -> Option<()> {
    let bindings = crate::function_arguments::bind_function_arguments(&function.inputs, args)?;
    for (input, value) in function.inputs.iter().zip(bindings) {
        scope.insert(input.name.clone(), value);
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

fn scalar_array_element(expr: &Expression) -> Option<Expression> {
    match expr {
        Expression::Array { elements, .. } if elements.len() == 1 => elements.first().cloned(),
        _ => Some(expr.clone()),
    }
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

        assert_eq!(project_flat_index(&expr, &[2], 0, &Dae::new()), None);
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

        let projected =
            project_flat_index(&expr, &[2], 0, &Dae::new()).expect("spanned binary should project");

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

    #[test]
    fn project_flat_index_keeps_matrix_product_intact() {
        let span = test_span();
        let expr = Expression::Binary {
            op: OpBinary::Mul,
            lhs: Box::new(var_ref("A", span)),
            rhs: Box::new(var_ref("x", span)),
            span,
        };

        let projected =
            project_flat_index(&expr, &[2], 1, &Dae::new()).expect("product result is indexable");

        assert!(matches!(
            projected,
            Expression::Index { base, subscripts, .. }
                if base.as_ref() == &expr
                    && matches!(subscripts.as_slice(), [Subscript::Index { value: 2, .. }])
        ));
    }

    #[test]
    fn project_flat_index_distributes_scalar_array_product() {
        let span = test_span();
        let second = var_ref("x2", span);
        let expr = Expression::Binary {
            op: OpBinary::Mul,
            lhs: Box::new(real_literal(2.0, span)),
            rhs: Box::new(Expression::Array {
                elements: vec![var_ref("x1", span), second.clone()],
                is_matrix: false,
                span,
            }),
            span,
        };

        let projected = project_flat_index(&expr, &[2], 1, &Dae::new())
            .expect("scalar-array product should project elementwise");

        assert!(matches!(
            projected,
            Expression::Binary { op: OpBinary::Mul, lhs, rhs, .. }
                if matches!(lhs.as_ref(), Expression::Literal { .. })
                    && rhs.as_ref() == &second
        ));
    }
}
