use super::{BuiltinFunction, Dae, Expression, OpBinary, StructuralError, VarName};
use crate::static_eval::{eval_static_number, structural_scalar_bindings};
use crate::variable_scope::{DaeVariableScope, scalar_count_from_dims};
use std::collections::HashMap;

#[cfg(test)]
use super::{Subscript, Variable};

pub(super) fn dae_variable_size(
    dae: &Dae,
    name: &VarName,
) -> Result<Option<usize>, StructuralError> {
    DaeVariableScope::new(dae)
        .exact(name)
        .map(|var| var.dims.clone())
        .map(|dims| scalar_count_from_dims(name, &dims))
        .transpose()
}

pub(super) fn required_dae_variable_size(
    dae: &Dae,
    name: &VarName,
) -> Result<usize, StructuralError> {
    DaeVariableScope::new(dae).size(name)
}

pub(super) fn residual_scalar_width(
    dae: &Dae,
    expr: &Expression,
) -> Result<usize, StructuralError> {
    match expression_dims_for_row_count(dae, expr)? {
        Some(dims) => scalar_count_from_dims(&VarName::new("<residual>"), &dims),
        None => Ok(1),
    }
}

pub(crate) fn expression_dims_for_row_count(
    dae: &Dae,
    expr: &Expression,
) -> Result<Option<Vec<i64>>, StructuralError> {
    let bindings = structural_scalar_bindings(dae);
    expression_dims_with_bindings(dae, expr, &bindings)
}

fn expression_dims_with_bindings(
    dae: &Dae,
    expr: &Expression,
    bindings: &HashMap<String, f64>,
) -> Result<Option<Vec<i64>>, StructuralError> {
    let scope = DaeVariableScope::new(dae);
    Ok(match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => match scope.dims_for_reference(name)? {
            Some(dims) => dims_after_subscripts(dae, dims, subscripts, bindings)?,
            None => None,
        },
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } => match args.first() {
            Some(arg) => expression_dims_with_bindings(dae, arg, bindings)?,
            None => None,
        },
        Expression::BuiltinCall { function, args, .. } => {
            builtin_dims_for_row_count(dae, *function, args, bindings)?
        }
        Expression::FunctionCall { name, args, .. } => {
            function_call_dims_for_row_count(dae, name.var_name(), args, bindings)?
        }
        Expression::Array {
            elements,
            is_matrix,
            ..
        } => array_dims_for_row_count(elements, *is_matrix),
        Expression::Binary { op, lhs, rhs, .. } => {
            binary_dims_for_row_count(dae, op, lhs, rhs, bindings)?
        }
        Expression::Unary { rhs, .. } => expression_dims_with_bindings(dae, rhs, bindings)?,
        Expression::Index {
            base, subscripts, ..
        } => match expression_dims_with_bindings(dae, base, bindings)? {
            Some(dims) => dims_after_subscripts(dae, dims, subscripts, bindings)?,
            None => None,
        },
        Expression::FieldAccess { base, field, .. } => {
            field_access_dims_for_row_count(dae, base, field, bindings)?
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => if_dims_for_row_count(dae, branches, else_branch, bindings)?,
        Expression::ArrayComprehension { expr, indices, .. } => {
            comprehension_dims_for_row_count(dae, expr, indices, bindings)?
        }
        _ => None,
    })
}

fn binary_dims_for_row_count(
    dae: &Dae,
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    bindings: &HashMap<String, f64>,
) -> Result<Option<Vec<i64>>, StructuralError> {
    let lhs_dims = expression_dims_with_bindings(dae, lhs, bindings)?.unwrap_or_default();
    let rhs_dims = expression_dims_with_bindings(dae, rhs, bindings)?.unwrap_or_default();
    Ok(match op {
        OpBinary::Add | OpBinary::Sub => (lhs_dims == rhs_dims).then_some(lhs_dims),
        OpBinary::Mul => multiplication_dims(&lhs_dims, &rhs_dims),
        OpBinary::Div => rhs_dims.is_empty().then_some(lhs_dims),
        OpBinary::AddElem
        | OpBinary::SubElem
        | OpBinary::MulElem
        | OpBinary::DivElem
        | OpBinary::ExpElem => elementwise_dims(&lhs_dims, &rhs_dims),
        OpBinary::Exp => (rhs_dims.is_empty()
            && (lhs_dims.is_empty()
                || matches!(lhs_dims.as_slice(), [rows, columns] if rows == columns)))
        .then_some(lhs_dims),
        OpBinary::Eq
        | OpBinary::Neq
        | OpBinary::Lt
        | OpBinary::Le
        | OpBinary::Gt
        | OpBinary::Ge
        | OpBinary::And
        | OpBinary::Or => Some(Vec::new()),
        OpBinary::Assign | OpBinary::Empty => (lhs_dims == rhs_dims).then_some(lhs_dims),
    })
}

fn elementwise_dims(lhs: &[i64], rhs: &[i64]) -> Option<Vec<i64>> {
    match (lhs.is_empty(), rhs.is_empty()) {
        (true, true) => Some(Vec::new()),
        (true, false) => Some(rhs.to_vec()),
        (false, true) => Some(lhs.to_vec()),
        (false, false) => (lhs == rhs).then(|| lhs.to_vec()),
    }
}

fn multiplication_dims(lhs: &[i64], rhs: &[i64]) -> Option<Vec<i64>> {
    match (lhs, rhs) {
        ([], _) => Some(rhs.to_vec()),
        (_, []) => Some(lhs.to_vec()),
        ([lhs_size], [rhs_size]) if lhs_size == rhs_size => Some(Vec::new()),
        ([rows, inner_lhs], [inner_rhs]) if inner_lhs == inner_rhs => Some(vec![*rows]),
        ([inner_lhs], [inner_rhs, columns]) if inner_lhs == inner_rhs => Some(vec![*columns]),
        ([rows, inner_lhs], [inner_rhs, columns]) if inner_lhs == inner_rhs => {
            Some(vec![*rows, *columns])
        }
        _ => None,
    }
}

fn field_access_dims_for_row_count(
    dae: &Dae,
    base: &Expression,
    field: &str,
    bindings: &HashMap<String, f64>,
) -> Result<Option<Vec<i64>>, StructuralError> {
    if let Some(dims) = DaeVariableScope::new(dae).indexed_component_field_dims(base, field) {
        return Ok(Some(dims));
    }
    expression_dims_with_bindings(dae, base, bindings)
}

fn builtin_dims_for_row_count(
    dae: &Dae,
    function: BuiltinFunction,
    args: &[Expression],
    bindings: &HashMap<String, f64>,
) -> Result<Option<Vec<i64>>, StructuralError> {
    match function {
        BuiltinFunction::Fill if args.len() >= 2 => {
            let Some(value) = args.first() else {
                return Ok(None);
            };
            let Some(mut dims) = static_dimension_args(&args[1..], bindings) else {
                return Ok(None);
            };
            if let Some(inner_dims) = expression_dims_with_bindings(dae, value, bindings)? {
                dims.extend(inner_dims);
            }
            Ok(Some(dims))
        }
        BuiltinFunction::Zeros | BuiltinFunction::Ones => Ok(static_dimension_args(args, bindings)),
        BuiltinFunction::Identity => Ok(args
            .first()
            .and_then(|arg| static_nonnegative_dimension(arg, bindings))
            .map(|dimension| vec![dimension, dimension])),
        BuiltinFunction::Diagonal => {
            let Some(arg) = args.first() else {
                return Ok(None);
            };
            Ok(match expression_dims_with_bindings(dae, arg, bindings)? {
                Some(dims) if dims.len() == 1 => Some(vec![dims[0], dims[0]]),
                Some(_) | None => None,
            })
        }
        BuiltinFunction::Linspace if args.len() == 3 => {
            Ok(static_nonnegative_dimension(&args[2], bindings)
                .filter(|dimension| *dimension >= 2)
                .map(|dimension| vec![dimension]))
        }
        BuiltinFunction::NoEvent | BuiltinFunction::Smooth | BuiltinFunction::Homotopy => args
            .last()
            .map(|arg| expression_dims_with_bindings(dae, arg, bindings))
            .transpose()
            .map(Option::flatten),
        _ => Ok(None),
    }
}

fn function_call_dims_for_row_count(
    dae: &Dae,
    name: &VarName,
    args: &[Expression],
    bindings: &HashMap<String, f64>,
) -> Result<Option<Vec<i64>>, StructuralError> {
    let Some(function) = dae.symbols.functions.get(name) else {
        return Ok(None);
    };
    let Some(output) = function.outputs.first() else {
        return Ok(None);
    };
    if !output.dims.is_empty() {
        return Ok(Some(output.dims.clone()));
    }
    if !output.shape_expr.is_empty() {
        return Ok(None);
    }

    let mut vector_dims: Option<Vec<i64>> = None;
    for (arg, input) in args.iter().zip(&function.inputs) {
        if !input.dims.is_empty() || !input.shape_expr.is_empty() {
            continue;
        }
        let Some(dims) = expression_dims_with_bindings(dae, arg, bindings)? else {
            continue;
        };
        if dims.is_empty() {
            continue;
        }
        match &vector_dims {
            Some(existing) if existing != &dims => return Ok(None),
            Some(_) => {}
            None => vector_dims = Some(dims),
        }
    }
    Ok(vector_dims)
}

fn if_dims_for_row_count(
    dae: &Dae,
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    bindings: &HashMap<String, f64>,
) -> Result<Option<Vec<i64>>, StructuralError> {
    let mut dimensions = branches
        .iter()
        .map(|(_, value)| value)
        .chain(std::iter::once(else_branch))
        .map(|value| expression_dims_with_bindings(dae, value, bindings));
    let Some(first) = dimensions.next() else {
        return Ok(None);
    };
    let Some(expected) = first? else {
        return Ok(None);
    };
    for actual in dimensions {
        if actual? != Some(expected.clone()) {
            return Ok(None);
        }
    }
    Ok(Some(expected))
}

fn comprehension_dims_for_row_count(
    dae: &Dae,
    expr: &Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    bindings: &HashMap<String, f64>,
) -> Result<Option<Vec<i64>>, StructuralError> {
    if indices.is_empty() {
        return Ok(None);
    }
    let mut dims = Vec::with_capacity(indices.len());
    for index in indices {
        let Some(length) = static_range_length(&index.range, bindings) else {
            return Ok(None);
        };
        dims.push(length);
    }
    if let Some(inner_dims) = expression_dims_with_bindings(dae, expr, bindings)? {
        dims.extend(inner_dims);
    }
    Ok(Some(dims))
}

fn static_range_length(range: &Expression, bindings: &HashMap<String, f64>) -> Option<i64> {
    let Expression::Range {
        start, step, end, ..
    } = range
    else {
        return static_nonnegative_dimension(range, bindings);
    };
    let start = eval_static_number(start, bindings)?;
    let step = step
        .as_deref()
        .map(|step| eval_static_number(step, bindings))
        .unwrap_or(Some(1.0))?;
    let end = eval_static_number(end, bindings)?;
    if !start.is_finite() || !step.is_finite() || !end.is_finite() || step == 0.0 {
        return None;
    }
    if (step > 0.0 && start > end) || (step < 0.0 && start < end) {
        return Some(0);
    }
    let length = ((end - start) / step).floor() + 1.0;
    finite_nonnegative_integer(length)
}

fn static_nonnegative_dimension(expr: &Expression, bindings: &HashMap<String, f64>) -> Option<i64> {
    finite_nonnegative_integer(eval_static_number(expr, bindings)?)
}

fn static_dimension_args(args: &[Expression], bindings: &HashMap<String, f64>) -> Option<Vec<i64>> {
    if args.is_empty() {
        return None;
    }
    args.iter()
        .map(|arg| static_nonnegative_dimension(arg, bindings))
        .collect()
}

fn finite_nonnegative_integer(value: f64) -> Option<i64> {
    (value.is_finite() && value >= 0.0 && value.fract() == 0.0 && value <= i64::MAX as f64)
        .then_some(value as i64)
}

fn dims_after_subscripts(
    dae: &Dae,
    dims: Vec<i64>,
    subscripts: &[rumoca_core::Subscript],
    bindings: &HashMap<String, f64>,
) -> Result<Option<Vec<i64>>, StructuralError> {
    if subscripts.len() > dims.len() {
        return Ok(None);
    }
    let mut result = Vec::new();
    for (subscript, dim) in subscripts.iter().zip(&dims) {
        match subscript {
            rumoca_core::Subscript::Index { .. } => {}
            rumoca_core::Subscript::Colon { .. } => result.push(*dim),
            rumoca_core::Subscript::Expr { expr, .. } => {
                let Some(index_dims) = static_subscript_dims(dae, expr, bindings)? else {
                    return Ok(None);
                };
                result.extend(index_dims);
            }
        }
    }
    result.extend_from_slice(&dims[subscripts.len()..]);
    Ok(Some(result))
}

fn static_subscript_dims(
    dae: &Dae,
    expr: &Expression,
    bindings: &HashMap<String, f64>,
) -> Result<Option<Vec<i64>>, StructuralError> {
    if matches!(expr, Expression::Range { .. }) {
        return Ok(static_range_length(expr, bindings).map(|length| vec![length]));
    }
    match expression_dims_with_bindings(dae, expr, bindings)? {
        Some(dims) => Ok(Some(dims)),
        None if eval_static_number(expr, bindings).is_some() => Ok(Some(Vec::new())),
        None => Ok(None),
    }
}

fn array_dims_for_row_count(elements: &[Expression], is_matrix: bool) -> Option<Vec<i64>> {
    if !is_matrix {
        return Some(vec![elements.len() as i64]);
    }
    let cols = match elements.first()? {
        Expression::Array { elements: row, .. } => row.len(),
        _ => return Some(vec![elements.len() as i64]),
    };
    Some(vec![elements.len() as i64, cols as i64])
}

#[cfg(test)]
mod tests {
    use super::*;

    fn part(ident: &str, subs: Vec<Subscript>) -> rumoca_core::ComponentRefPart {
        rumoca_core::ComponentRefPart {
            ident: ident.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs,
        }
    }

    fn component_ref(parts: Vec<rumoca_core::ComponentRefPart>) -> rumoca_core::ComponentReference {
        rumoca_core::ComponentReference {
            local: false,
            span: rumoca_core::Span::DUMMY,
            parts,
            def_id: None,
        }
    }

    fn reference(parts: Vec<rumoca_core::ComponentRefPart>) -> rumoca_core::Reference {
        rumoca_core::Reference::from_component_reference(component_ref(parts))
    }

    fn pin_field_var(index: i64, field: &str) -> Variable {
        let mut var = Variable::new(
            VarName::new(format!("inverter.ac.pin[{index}].{field}")),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        );
        var.component_ref = Some(component_ref(vec![
            part("inverter", vec![]),
            part("ac", vec![]),
            part(
                "pin",
                vec![Subscript::generated_index(index, rumoca_core::Span::DUMMY)],
            ),
            part(field, vec![]),
        ]));
        var
    }

    #[test]
    fn row_shape_counts_indexed_connector_array_field_access() {
        let mut dae = Dae::default();
        for index in 1..=3 {
            let var = pin_field_var(index, "v");
            dae.variables.algebraics.insert(var.name.clone(), var);
        }
        let expr = Expression::FieldAccess {
            base: Box::new(Expression::Index {
                base: Box::new(Expression::VarRef {
                    name: reference(vec![
                        part("inverter", vec![]),
                        part("ac", vec![]),
                        part("pin", vec![]),
                    ]),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }),
                subscripts: vec![Subscript::generated_colon(rumoca_core::Span::DUMMY)],
                span: rumoca_core::Span::DUMMY,
            }),
            field: "v".to_string(),
            span: rumoca_core::Span::DUMMY,
        };

        assert_eq!(residual_scalar_width(&dae, &expr).unwrap(), 3);
    }

    #[test]
    fn row_shape_proves_vectorized_scalar_function_if_binding() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("row_shape_vectorized_function.mo"),
            1,
            2,
        );
        let mut dae = Dae::default();
        let mut n = Variable::new(VarName::new("n"), span);
        n.start = Some(Expression::Literal {
            value: rumoca_core::Literal::Integer(2),
            span,
        });
        dae.variables.parameters.insert(VarName::new("n"), n);
        let mut function = rumoca_core::Function::new("Medium.density", span);
        function
            .inputs
            .push(rumoca_core::FunctionParam::new("p", "Real", span));
        function
            .outputs
            .push(rumoca_core::FunctionParam::new("d", "Real", span));
        dae.symbols
            .functions
            .insert(function.name.clone(), function);

        let vector = Expression::Array {
            elements: vec![
                Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span,
                },
                Expression::Literal {
                    value: rumoca_core::Literal::Real(2.0),
                    span,
                },
            ],
            is_matrix: false,
            span,
        };
        let expression = Expression::If {
            branches: vec![(
                Expression::Literal {
                    value: rumoca_core::Literal::Boolean(true),
                    span,
                },
                Expression::BuiltinCall {
                    function: BuiltinFunction::Fill,
                    args: vec![
                        Expression::Literal {
                            value: rumoca_core::Literal::Real(995.586),
                            span,
                        },
                        Expression::VarRef {
                            name: rumoca_core::Reference::new("n"),
                            subscripts: Vec::new(),
                            span,
                        },
                    ],
                    span,
                },
            )],
            else_branch: Box::new(Expression::FunctionCall {
                name: rumoca_core::Reference::new("Medium.density"),
                args: vec![vector],
                is_constructor: false,
                span,
            }),
            span,
        };

        assert_eq!(
            expression_dims_for_row_count(&dae, &expression).unwrap(),
            Some(vec![2])
        );
    }

    #[test]
    fn row_shape_uses_static_comprehension_range() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("row_shape_comprehension.mo"),
            1,
            2,
        );
        let expression = Expression::ArrayComprehension {
            expr: Box::new(Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span,
            }),
            indices: vec![rumoca_core::ComprehensionIndex {
                name: "i".to_string(),
                range: Expression::Range {
                    start: Box::new(Expression::Literal {
                        value: rumoca_core::Literal::Integer(3),
                        span,
                    }),
                    step: Some(Box::new(Expression::Literal {
                        value: rumoca_core::Literal::Integer(-1),
                        span,
                    })),
                    end: Box::new(Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span,
                    }),
                    span,
                },
            }],
            filter: None,
            span,
        };

        assert_eq!(
            expression_dims_for_row_count(&Dae::default(), &expression).unwrap(),
            Some(vec![3])
        );
    }

    #[test]
    fn row_shape_preserves_static_range_subscript_dimension() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("row_shape_range_subscript.mo"),
            1,
            2,
        );
        let mut dae = Dae::default();
        let mut values = Variable::new(VarName::new("values"), span);
        values.dims = vec![2];
        dae.variables
            .algebraics
            .insert(VarName::new("values"), values);
        let mut n = Variable::new(VarName::new("n"), span);
        n.start = Some(Expression::Literal {
            value: rumoca_core::Literal::Integer(2),
            span,
        });
        dae.variables.parameters.insert(VarName::new("n"), n);

        let slice = Expression::VarRef {
            name: rumoca_core::Reference::new("values"),
            subscripts: vec![Subscript::generated_expr(
                Box::new(Expression::Range {
                    start: Box::new(Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span,
                    }),
                    step: None,
                    end: Box::new(Expression::Binary {
                        op: rumoca_core::OpBinary::Sub,
                        lhs: Box::new(Expression::VarRef {
                            name: rumoca_core::Reference::new("n"),
                            subscripts: Vec::new(),
                            span,
                        }),
                        rhs: Box::new(Expression::Literal {
                            value: rumoca_core::Literal::Integer(1),
                            span,
                        }),
                        span,
                    }),
                    span,
                }),
                span,
            )],
            span,
        };

        assert_eq!(
            expression_dims_for_row_count(&dae, &slice).unwrap(),
            Some(vec![1])
        );

        let ones = Expression::BuiltinCall {
            function: BuiltinFunction::Ones,
            args: vec![Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(Expression::VarRef {
                    name: rumoca_core::Reference::new("n"),
                    subscripts: Vec::new(),
                    span,
                }),
                rhs: Box::new(Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span,
                }),
                span,
            }],
            span,
        };
        assert_eq!(
            expression_dims_for_row_count(&dae, &ones).unwrap(),
            Some(vec![1])
        );
    }

    #[test]
    fn row_shape_applies_matrix_product_dimensions() {
        let span = rumoca_core::Span::DUMMY;
        let mut dae = Dae::default();
        for (name, dims) in [("matrix", vec![3, 3]), ("vector", vec![3])] {
            let mut variable = Variable::new(VarName::new(name), span);
            variable.dims = dims;
            dae.variables
                .algebraics
                .insert(variable.name.clone(), variable);
        }
        let product = Expression::Binary {
            op: OpBinary::Mul,
            lhs: Box::new(Expression::VarRef {
                name: rumoca_core::Reference::new("matrix"),
                subscripts: Vec::new(),
                span,
            }),
            rhs: Box::new(Expression::VarRef {
                name: rumoca_core::Reference::new("vector"),
                subscripts: Vec::new(),
                span,
            }),
            span,
        };

        assert_eq!(
            expression_dims_for_row_count(&dae, &product).unwrap(),
            Some(vec![3])
        );
    }
}
