use super::*;

fn is_der_of_state(expr: &Expression, state_name: &VarName) -> bool {
    matches!(
        expr,
        Expression::BuiltinCall { function: BuiltinFunction::Der, args }
        if args.len() == 1 && expr_refers_to_var(&args[0], state_name)
    )
}

fn make_binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn make_unary(op: OpUnary, rhs: Expression) -> Expression {
    Expression::Unary {
        op,
        rhs: Box::new(rhs),
    }
}

fn zero_literal() -> Expression {
    Expression::Literal(Literal::Real(0.0))
}

fn split_linear_der_target(
    expr: &Expression,
    state_name: &VarName,
) -> Option<(Expression, Expression)> {
    if is_der_of_state(expr, state_name) {
        return Some((Expression::Literal(Literal::Real(1.0)), zero_literal()));
    }

    let is_target = |e: &Expression| is_der_of_state(e, state_name);
    match expr {
        Expression::Unary {
            op: OpUnary::Minus(_) | OpUnary::DotMinus(_),
            rhs,
        } => {
            let (coef, rem) = split_linear_der_target(rhs, state_name)?;
            Some((
                make_unary(OpUnary::Minus(Default::default()), coef),
                make_unary(OpUnary::Minus(Default::default()), rem),
            ))
        }
        Expression::Binary { op, lhs, rhs } => match op {
            OpBinary::Add(_) | OpBinary::AddElem(_) => {
                if let Some((coef, rem)) = split_linear_der_target(lhs, state_name)
                    && !expr_contains_der_of(rhs, state_name)
                {
                    return Some((
                        coef,
                        make_binary(OpBinary::Add(Default::default()), rem, *rhs.clone()),
                    ));
                }
                if let Some((coef, rem)) = split_linear_der_target(rhs, state_name)
                    && !expr_contains_der_of(lhs, state_name)
                {
                    return Some((
                        coef,
                        make_binary(OpBinary::Add(Default::default()), *lhs.clone(), rem),
                    ));
                }
                None
            }
            OpBinary::Sub(_) | OpBinary::SubElem(_) => {
                if let Some((coef, rem)) = split_linear_der_target(lhs, state_name)
                    && !expr_contains_der_of(rhs, state_name)
                {
                    return Some((
                        coef,
                        make_binary(OpBinary::Sub(Default::default()), rem, *rhs.clone()),
                    ));
                }
                if let Some((coef, rem)) = split_linear_der_target(rhs, state_name)
                    && !expr_contains_der_of(lhs, state_name)
                {
                    return Some((
                        make_unary(OpUnary::Minus(Default::default()), coef),
                        make_binary(OpBinary::Sub(Default::default()), *lhs.clone(), rem),
                    ));
                }
                None
            }
            OpBinary::Mul(_) | OpBinary::MulElem(_) => {
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

fn try_extract_der_value(rhs: &Expression, state_name: &VarName) -> Option<Expression> {
    if let Expression::Binary {
        op: OpBinary::Sub(_),
        lhs,
        rhs: row_rhs,
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
        OpBinary::Div(Default::default()),
        make_unary(OpUnary::Minus(Default::default()), remainder),
        coef,
    ))
}

pub(super) fn build_der_value_map(dae: &Dae) -> HashMap<String, Expression> {
    let mut map = HashMap::new();
    for state_name in dae.states.keys() {
        for eq in &dae.f_x {
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

struct SymbolicDerivativeContext<'a> {
    dae: &'a Dae,
    der_map: &'a HashMap<String, Expression>,
}

impl<'a> SymbolicDerivativeContext<'a> {
    fn differentiate_variable(
        &self,
        name: &VarName,
        subscripts: &[Subscript],
    ) -> Option<Expression> {
        if !subscripts.is_empty() {
            return None;
        }
        if name.as_str() == "time" {
            return Some(Expression::Literal(Literal::Real(1.0)));
        }
        if self.dae.parameters.contains_key(name) || self.dae.constants.contains_key(name) {
            return Some(zero_literal());
        }
        self.der_map.get(name.as_str()).cloned()
    }

    fn differentiate_binary(
        &self,
        op: &OpBinary,
        lhs: &Expression,
        rhs: &Expression,
    ) -> Option<Expression> {
        match op {
            OpBinary::Add(_) | OpBinary::AddElem(_) => Some(make_binary(
                OpBinary::Add(Default::default()),
                self.differentiate(lhs)?,
                self.differentiate(rhs)?,
            )),
            OpBinary::Sub(_) | OpBinary::SubElem(_) => Some(make_binary(
                OpBinary::Sub(Default::default()),
                self.differentiate(lhs)?,
                self.differentiate(rhs)?,
            )),
            OpBinary::Mul(_) | OpBinary::MulElem(_) => {
                let da_b = make_binary(
                    OpBinary::Mul(Default::default()),
                    self.differentiate(lhs)?,
                    rhs.clone(),
                );
                let a_db = make_binary(
                    OpBinary::Mul(Default::default()),
                    lhs.clone(),
                    self.differentiate(rhs)?,
                );
                Some(make_binary(OpBinary::Add(Default::default()), da_b, a_db))
            }
            OpBinary::Div(_) | OpBinary::DivElem(_) => {
                let da_b = make_binary(
                    OpBinary::Mul(Default::default()),
                    self.differentiate(lhs)?,
                    rhs.clone(),
                );
                let a_db = make_binary(
                    OpBinary::Mul(Default::default()),
                    lhs.clone(),
                    self.differentiate(rhs)?,
                );
                let numer = make_binary(OpBinary::Sub(Default::default()), da_b, a_db);
                let denom =
                    make_binary(OpBinary::Mul(Default::default()), rhs.clone(), rhs.clone());
                Some(make_binary(OpBinary::Div(Default::default()), numer, denom))
            }
            _ => None,
        }
    }

    fn differentiate_unary(&self, op: &OpUnary, rhs: &Expression) -> Option<Expression> {
        match op {
            OpUnary::Minus(_) | OpUnary::DotMinus(_) => Some(make_unary(
                OpUnary::Minus(Default::default()),
                self.differentiate(rhs)?,
            )),
            OpUnary::Plus(_) | OpUnary::DotPlus(_) => self.differentiate(rhs),
            _ => None,
        }
    }

    fn differentiate_if(
        &self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
    ) -> Option<Expression> {
        let mut differentiated_branches = Vec::with_capacity(branches.len());
        for (cond, value) in branches {
            differentiated_branches.push((cond.clone(), self.differentiate(value)?));
        }
        Some(Expression::If {
            branches: differentiated_branches,
            else_branch: Box::new(self.differentiate(else_branch)?),
        })
    }

    fn differentiate(&self, expr: &Expression) -> Option<Expression> {
        match expr {
            Expression::Literal(_) => Some(zero_literal()),
            Expression::VarRef { name, subscripts } => {
                self.differentiate_variable(name, subscripts)
            }
            Expression::Binary { op, lhs, rhs } => self.differentiate_binary(op, lhs, rhs),
            Expression::Unary { op, rhs } => self.differentiate_unary(op, rhs),
            Expression::If {
                branches,
                else_branch,
            } => self.differentiate_if(branches, else_branch),
            _ => None,
        }
    }
}

pub(super) fn symbolic_time_derivative(
    expr: &Expression,
    dae: &Dae,
    der_map: &HashMap<String, Expression>,
) -> Option<Expression> {
    SymbolicDerivativeContext { dae, der_map }.differentiate(expr)
}

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
        Expression::Binary { op, lhs, rhs } => Expression::Binary {
            op: op.clone(),
            lhs: Box::new(expand_der_in_expr_full(lhs, dae, der_map, state_names)),
            rhs: Box::new(expand_der_in_expr_full(rhs, dae, der_map, state_names)),
        },
        Expression::Unary { op, rhs } => Expression::Unary {
            op: op.clone(),
            rhs: Box::new(expand_der_in_expr_full(rhs, dae, der_map, state_names)),
        },
        Expression::BuiltinCall { function, args } => Expression::BuiltinCall {
            function: *function,
            args: args
                .iter()
                .map(|a| expand_der_in_expr_full(a, dae, der_map, state_names))
                .collect(),
        },
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => Expression::FunctionCall {
            name: name.clone(),
            args: args
                .iter()
                .map(|a| expand_der_in_expr_full(a, dae, der_map, state_names))
                .collect(),
            is_constructor: *is_constructor,
        },
        Expression::If {
            branches,
            else_branch,
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
        },
        Expression::Array {
            elements,
            is_matrix,
        } => Expression::Array {
            elements: elements
                .iter()
                .map(|e| expand_der_in_expr_full(e, dae, der_map, state_names))
                .collect(),
            is_matrix: *is_matrix,
        },
        Expression::Index { base, subscripts } => Expression::Index {
            base: Box::new(expand_der_in_expr_full(base, dae, der_map, state_names)),
            subscripts: subscripts.clone(),
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
