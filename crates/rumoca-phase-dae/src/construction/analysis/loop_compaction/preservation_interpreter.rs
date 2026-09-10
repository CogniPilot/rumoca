//! A total statement interpreter for the evaluable subset.
//!
//! Execution is deterministic and terminates on every input: `for` ranges are
//! materialised as finite integer sequences, a statement budget bounds the
//! total work, and every construct outside the subset returns a [`Refusal`]
//! rather than an approximate answer. The differential counts those refusals
//! by reason, so a program can never leave the comparison unobserved.

use super::preservation_values::{Environment, Refusal, Value};
use super::*;
use rumoca_core::{BuiltinFunction, ComponentReference, Literal, OpBinary, OpUnary};

/// Statements one interpretation may execute before it is declared divergent.
const STATEMENT_BUDGET: u32 = 200_000;

/// Longest `for` range or array value the subset admits.
const EXTENT_BUDGET: i64 = 1_024;

/// Which coordinates of an array value one subscript selects.
enum Selection {
    Element(usize),
    Slice(Vec<usize>),
}

/// Execute `statements` from `entry` and return the final environment.
pub(super) fn interpret(
    statements: &[rumoca_core::Statement],
    entry: &Environment,
) -> Result<Environment, Refusal> {
    let mut interpreter = Interpreter {
        environment: entry.clone(),
        fuel: STATEMENT_BUDGET,
    };
    interpreter.sequence(statements)?;
    Ok(interpreter.environment)
}

struct Interpreter {
    environment: Environment,
    fuel: u32,
}

impl Interpreter {
    fn sequence(&mut self, statements: &[rumoca_core::Statement]) -> Result<(), Refusal> {
        for statement in statements {
            self.statement(statement)?;
        }
        Ok(())
    }

    fn statement(&mut self, statement: &rumoca_core::Statement) -> Result<(), Refusal> {
        self.fuel = self.fuel.checked_sub(1).ok_or(Refusal::FuelExhausted)?;
        match statement {
            rumoca_core::Statement::Empty { .. } => Ok(()),
            rumoca_core::Statement::Assignment { comp, value, .. } => {
                let value = self.expression(value)?;
                self.store(comp, value)
            }
            rumoca_core::Statement::For {
                indices, equations, ..
            } => self.for_statement(indices, equations),
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => self.if_statement(cond_blocks, else_block.as_deref()),
            _ => Err(Refusal::UnsupportedStatement),
        }
    }

    fn if_statement(
        &mut self,
        cond_blocks: &[rumoca_core::StatementBlock],
        else_block: Option<&[rumoca_core::Statement]>,
    ) -> Result<(), Refusal> {
        for block in cond_blocks {
            if self.expression(&block.cond)?.as_boolean()? {
                return self.sequence(&block.stmts);
            }
        }
        match else_block {
            Some(statements) => self.sequence(statements),
            None => Ok(()),
        }
    }

    /// MLS §11.2.2: each index binds a scalar over its range inside the body,
    /// and the binding is removed again when the loop is left.
    fn for_statement(
        &mut self,
        indices: &[rumoca_core::ForIndex],
        body: &[rumoca_core::Statement],
    ) -> Result<(), Refusal> {
        let Some((index, rest)) = indices.split_first() else {
            return self.sequence(body);
        };
        let name = VarName::new(&index.ident);
        let coordinates = self.range_coordinates(&index.range)?;
        let shadowed = self.environment.get(&name).cloned();
        for coordinate in coordinates {
            self.environment
                .insert(name.clone(), Value::Integer(coordinate));
            self.for_statement(rest, body)?;
        }
        match shadowed {
            Some(value) => self.environment.insert(name, value),
            None => self.environment.remove(&name),
        };
        Ok(())
    }

    fn range_coordinates(&mut self, range: &Expression) -> Result<Vec<i64>, Refusal> {
        match self.expression(range)? {
            Value::Array(elements) => elements.iter().map(Value::as_integer).collect(),
            _ => Err(Refusal::UnsupportedRange),
        }
    }

    fn store(&mut self, comp: &ComponentReference, value: Value) -> Result<(), Refusal> {
        let [part] = comp.parts() else {
            return Err(Refusal::UnsupportedSubscript);
        };
        let name = VarName::new(&part.ident);
        let current = self
            .environment
            .get(&name)
            .cloned()
            .ok_or(Refusal::UnknownName)?;
        let updated = match part.subs.as_slice() {
            [] => conform(&current, value)?,
            [subscript] => {
                let selection = self.selection(subscript, &current)?;
                store_selected(current, &selection, value)?
            }
            _ => return Err(Refusal::UnsupportedSubscript),
        };
        self.environment.insert(name, updated);
        Ok(())
    }

    fn selection(
        &mut self,
        subscript: &rumoca_core::Subscript,
        base: &Value,
    ) -> Result<Selection, Refusal> {
        let Value::Array(elements) = base else {
            return Err(Refusal::TypeMismatch);
        };
        let extent = elements.len();
        match subscript {
            rumoca_core::Subscript::Index { value, .. } => {
                Ok(Selection::Element(coordinate(*value, extent)?))
            }
            rumoca_core::Subscript::Colon { .. } => Ok(Selection::Slice((0..extent).collect())),
            rumoca_core::Subscript::Expr { expr, .. } => match self.expression(expr)? {
                Value::Integer(value) => Ok(Selection::Element(coordinate(value, extent)?)),
                Value::Array(values) => values
                    .iter()
                    .map(|value| coordinate(value.as_integer()?, extent))
                    .collect::<Result<Vec<_>, _>>()
                    .map(Selection::Slice),
                _ => Err(Refusal::TypeMismatch),
            },
        }
    }

    fn expression(&mut self, expression: &Expression) -> Result<Value, Refusal> {
        self.fuel = self.fuel.checked_sub(1).ok_or(Refusal::FuelExhausted)?;
        match expression {
            Expression::Literal { value, .. } => literal_value(value),
            Expression::VarRef {
                name, subscripts, ..
            } => self.var_ref(name.as_str(), subscripts),
            Expression::Binary { op, lhs, rhs, .. } => {
                let lhs = self.expression(lhs)?;
                let rhs = self.expression(rhs)?;
                binary(op, &lhs, &rhs)
            }
            Expression::Unary { op, rhs, .. } => {
                let rhs = self.expression(rhs)?;
                unary(op, &rhs)
            }
            Expression::If {
                branches,
                else_branch,
                ..
            } => self.if_expression(branches, else_branch),
            Expression::Array {
                elements,
                is_matrix,
                ..
            } => self.array_expression(elements, *is_matrix),
            Expression::Range {
                start, step, end, ..
            } => self.range_expression(start, step.as_deref(), end),
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                ..
            } => self.comprehension(expr, indices, filter.as_deref()),
            Expression::BuiltinCall { function, args, .. } => self.builtin(*function, args),
            _ => Err(Refusal::UnsupportedExpression),
        }
    }

    fn var_ref(
        &mut self,
        name: &str,
        subscripts: &[rumoca_core::Subscript],
    ) -> Result<Value, Refusal> {
        let name = VarName::new(name);
        let base = self
            .environment
            .get(&name)
            .cloned()
            .ok_or(Refusal::UnknownName)?;
        match subscripts {
            [] => Ok(base),
            [subscript] => {
                let selection = self.selection(subscript, &base)?;
                read_selected(&base, &selection)
            }
            _ => Err(Refusal::UnsupportedSubscript),
        }
    }

    fn if_expression(
        &mut self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
    ) -> Result<Value, Refusal> {
        for (condition, value) in branches {
            if self.expression(condition)?.as_boolean()? {
                return self.expression(value);
            }
        }
        self.expression(else_branch)
    }

    fn array_expression(
        &mut self,
        elements: &[Expression],
        is_matrix: bool,
    ) -> Result<Value, Refusal> {
        if is_matrix {
            return Err(Refusal::UnsupportedExpression);
        }
        elements
            .iter()
            .map(|element| self.expression(element))
            .collect::<Result<Vec<_>, _>>()
            .map(Value::Array)
    }

    fn range_expression(
        &mut self,
        start: &Expression,
        step: Option<&Expression>,
        end: &Expression,
    ) -> Result<Value, Refusal> {
        let start = self.expression(start)?.as_integer()?;
        let end = self.expression(end)?.as_integer()?;
        let step = match step {
            Some(step) => self.expression(step)?.as_integer()?,
            None => 1,
        };
        Ok(Value::Array(sequence(start, step, end)?))
    }

    fn comprehension(
        &mut self,
        expr: &Expression,
        indices: &[rumoca_core::ComprehensionIndex],
        filter: Option<&Expression>,
    ) -> Result<Value, Refusal> {
        let ([index], None) = (indices, filter) else {
            return Err(Refusal::UnsupportedExpression);
        };
        let name = VarName::new(&index.name);
        let coordinates = self.range_coordinates(&index.range)?;
        let shadowed = self.environment.get(&name).cloned();
        let mut elements = Vec::with_capacity(coordinates.len());
        for coordinate in coordinates {
            self.environment
                .insert(name.clone(), Value::Integer(coordinate));
            elements.push(self.expression(expr)?);
        }
        match shadowed {
            Some(value) => self.environment.insert(name, value),
            None => self.environment.remove(&name),
        };
        Ok(Value::Array(elements))
    }

    fn builtin(
        &mut self,
        function: BuiltinFunction,
        args: &[Expression],
    ) -> Result<Value, Refusal> {
        let values = args
            .iter()
            .map(|argument| self.expression(argument))
            .collect::<Result<Vec<_>, _>>()?;
        match (function, values.as_slice()) {
            (BuiltinFunction::Sum, [Value::Array(elements)]) => reduce(elements, &OpBinary::Add),
            (BuiltinFunction::Product, [Value::Array(elements)]) => {
                reduce(elements, &OpBinary::Mul)
            }
            (BuiltinFunction::Abs, [Value::Real(value)]) => Value::exact_real(value.abs()),
            (BuiltinFunction::Abs, [Value::Integer(value)]) => value
                .checked_abs()
                .map(Value::Integer)
                .ok_or(Refusal::IntegerOverflow),
            (BuiltinFunction::Size, [Value::Array(elements)]) => {
                Ok(Value::Integer(elements.len() as i64))
            }
            _ => Err(Refusal::UnsupportedExpression),
        }
    }
}

/// Fold `elements` left to right; exactness makes the association irrelevant.
fn reduce(elements: &[Value], op: &OpBinary) -> Result<Value, Refusal> {
    let Some((first, rest)) = elements.split_first() else {
        return Err(Refusal::UnsupportedExpression);
    };
    rest.iter().try_fold(first.clone(), |folded, element| {
        binary(op, &folded, element)
    })
}

fn literal_value(literal: &Literal) -> Result<Value, Refusal> {
    match literal {
        Literal::Real(value) => Value::exact_real(*value),
        Literal::Integer(value) => Ok(Value::Integer(*value)),
        Literal::Boolean(value) => Ok(Value::Boolean(*value)),
        Literal::String(_) => Err(Refusal::UnsupportedExpression),
    }
}

/// Convert a one-based Modelica subscript into a zero-based coordinate.
fn coordinate(subscript: i64, extent: usize) -> Result<usize, Refusal> {
    let zero_based = subscript
        .checked_sub(1)
        .ok_or(Refusal::SubscriptOutOfRange)?;
    let zero_based = usize::try_from(zero_based).map_err(|_| Refusal::SubscriptOutOfRange)?;
    if zero_based >= extent {
        return Err(Refusal::SubscriptOutOfRange);
    }
    Ok(zero_based)
}

fn sequence(start: i64, step: i64, end: i64) -> Result<Vec<Value>, Refusal> {
    if step == 0 {
        return Err(Refusal::UnsupportedRange);
    }
    let span = end.checked_sub(start).ok_or(Refusal::UnsupportedRange)?;
    let count = if span.signum() == step.signum() || span == 0 {
        span / step + 1
    } else {
        0
    };
    if count > EXTENT_BUDGET {
        return Err(Refusal::UnsupportedRange);
    }
    (0..count)
        .map(|ordinal| {
            step.checked_mul(ordinal)
                .and_then(|offset| start.checked_add(offset))
                .map(Value::Integer)
                .ok_or(Refusal::IntegerOverflow)
        })
        .collect()
}

fn read_selected(base: &Value, selection: &Selection) -> Result<Value, Refusal> {
    let Value::Array(elements) = base else {
        return Err(Refusal::TypeMismatch);
    };
    match selection {
        Selection::Element(coordinate) => elements
            .get(*coordinate)
            .cloned()
            .ok_or(Refusal::SubscriptOutOfRange),
        Selection::Slice(coordinates) => coordinates
            .iter()
            .map(|coordinate| {
                elements
                    .get(*coordinate)
                    .cloned()
                    .ok_or(Refusal::SubscriptOutOfRange)
            })
            .collect::<Result<Vec<_>, _>>()
            .map(Value::Array),
    }
}

fn store_selected(base: Value, selection: &Selection, value: Value) -> Result<Value, Refusal> {
    let Value::Array(mut elements) = base else {
        return Err(Refusal::TypeMismatch);
    };
    match selection {
        Selection::Element(coordinate) => {
            let slot = elements
                .get_mut(*coordinate)
                .ok_or(Refusal::SubscriptOutOfRange)?;
            *slot = conform(slot, value)?;
        }
        Selection::Slice(coordinates) => {
            let Value::Array(values) = value else {
                return Err(Refusal::TypeMismatch);
            };
            if values.len() != coordinates.len() {
                return Err(Refusal::TypeMismatch);
            }
            for (coordinate, value) in coordinates.iter().zip(values) {
                let slot = elements
                    .get_mut(*coordinate)
                    .ok_or(Refusal::SubscriptOutOfRange)?;
                *slot = conform(slot, value)?;
            }
        }
    }
    Ok(Value::Array(elements))
}

/// MLS §10.6.13: an Integer value assigned to a Real coordinate converts; the
/// declared type of the coordinate is never widened by the assignment.
fn conform(declared: &Value, value: Value) -> Result<Value, Refusal> {
    match (declared, value) {
        (Value::Real(_), Value::Integer(value)) => Value::exact_real(value as f64),
        (Value::Real(_), Value::Real(value)) => Value::exact_real(value),
        (Value::Integer(_), Value::Integer(value)) => Ok(Value::Integer(value)),
        (Value::Boolean(_), Value::Boolean(value)) => Ok(Value::Boolean(value)),
        (Value::Array(declared), Value::Array(values)) if declared.len() == values.len() => {
            declared
                .iter()
                .zip(values)
                .map(|(declared, value)| conform(declared, value))
                .collect::<Result<Vec<_>, _>>()
                .map(Value::Array)
        }
        _ => Err(Refusal::TypeMismatch),
    }
}

fn unary(op: &OpUnary, rhs: &Value) -> Result<Value, Refusal> {
    match (op, rhs) {
        (OpUnary::Plus | OpUnary::DotPlus, value) => Ok(value.clone()),
        (OpUnary::Minus | OpUnary::DotMinus, Value::Real(value)) => Value::exact_real(-value),
        (OpUnary::Minus | OpUnary::DotMinus, Value::Integer(value)) => value
            .checked_neg()
            .map(Value::Integer)
            .ok_or(Refusal::IntegerOverflow),
        (OpUnary::Not, Value::Boolean(value)) => Ok(Value::Boolean(!value)),
        _ => Err(Refusal::UnsupportedOperator),
    }
}

fn binary(op: &OpBinary, lhs: &Value, rhs: &Value) -> Result<Value, Refusal> {
    match (lhs, rhs) {
        (Value::Boolean(lhs), Value::Boolean(rhs)) => logical(op, *lhs, *rhs),
        (Value::Integer(lhs), Value::Integer(rhs)) => integer_arithmetic(op, *lhs, *rhs),
        (Value::Array(lhs), Value::Array(rhs)) => elementwise(op, lhs, rhs),
        (Value::Array(elements), scalar) => broadcast(op, elements, scalar, ARRAY_LEFT),
        (scalar, Value::Array(elements)) => broadcast(op, elements, scalar, ARRAY_RIGHT),
        _ => real_arithmetic(op, promote(lhs)?, promote(rhs)?),
    }
}

/// Which side of a scalar/array operator the array is on.
const ARRAY_LEFT: bool = true;
const ARRAY_RIGHT: bool = false;

/// MLS §10.6.4 and §10.6.5: a scalar operand of `*` distributes over the array,
/// as does a scalar divisor of `/`; the dot operators distribute either way.
/// No other operator admits a scalar against an array, so no other operator is
/// given a meaning here.
fn broadcast(
    op: &OpBinary,
    elements: &[Value],
    scalar: &Value,
    array_left: bool,
) -> Result<Value, Refusal> {
    let admitted = match op {
        OpBinary::Mul | OpBinary::MulElem | OpBinary::AddElem | OpBinary::SubElem => true,
        OpBinary::Div | OpBinary::DivElem => array_left,
        _ => false,
    };
    if !admitted {
        return Err(Refusal::UnsupportedOperator);
    }
    elements
        .iter()
        .map(|element| {
            if array_left {
                binary(op, element, scalar)
            } else {
                binary(op, scalar, element)
            }
        })
        .collect::<Result<Vec<_>, _>>()
        .map(Value::Array)
}

fn promote(value: &Value) -> Result<f64, Refusal> {
    match value {
        Value::Real(value) => Ok(*value),
        Value::Integer(value) => Ok(*value as f64),
        _ => Err(Refusal::TypeMismatch),
    }
}

fn logical(op: &OpBinary, lhs: bool, rhs: bool) -> Result<Value, Refusal> {
    match op {
        OpBinary::And => Ok(Value::Boolean(lhs && rhs)),
        OpBinary::Or => Ok(Value::Boolean(lhs || rhs)),
        OpBinary::Eq => Ok(Value::Boolean(lhs == rhs)),
        OpBinary::Neq => Ok(Value::Boolean(lhs != rhs)),
        _ => Err(Refusal::UnsupportedOperator),
    }
}

fn integer_arithmetic(op: &OpBinary, lhs: i64, rhs: i64) -> Result<Value, Refusal> {
    let checked = match op {
        OpBinary::Add | OpBinary::AddElem => lhs.checked_add(rhs),
        OpBinary::Sub | OpBinary::SubElem => lhs.checked_sub(rhs),
        OpBinary::Mul | OpBinary::MulElem => lhs.checked_mul(rhs),
        _ => return comparison(op, lhs as f64, rhs as f64),
    };
    checked.map(Value::Integer).ok_or(Refusal::IntegerOverflow)
}

fn real_arithmetic(op: &OpBinary, lhs: f64, rhs: f64) -> Result<Value, Refusal> {
    match op {
        OpBinary::Add | OpBinary::AddElem => Value::exact_real(lhs + rhs),
        OpBinary::Sub | OpBinary::SubElem => Value::exact_real(lhs - rhs),
        OpBinary::Mul | OpBinary::MulElem => Value::exact_real(lhs * rhs),
        OpBinary::Div | OpBinary::DivElem if rhs != 0.0 => Value::exact_real(lhs / rhs),
        _ => comparison(op, lhs, rhs),
    }
}

fn comparison(op: &OpBinary, lhs: f64, rhs: f64) -> Result<Value, Refusal> {
    let relation = match op {
        OpBinary::Lt => lhs < rhs,
        OpBinary::Le => lhs <= rhs,
        OpBinary::Gt => lhs > rhs,
        OpBinary::Ge => lhs >= rhs,
        OpBinary::Eq => lhs == rhs,
        OpBinary::Neq => lhs != rhs,
        _ => return Err(Refusal::UnsupportedOperator),
    };
    Ok(Value::Boolean(relation))
}

/// MLS §10.6.4 makes `*` between two arrays a product contraction rather than
/// an elementwise operation, so only the operators that really are elementwise
/// are given a meaning here.
fn elementwise(op: &OpBinary, lhs: &[Value], rhs: &[Value]) -> Result<Value, Refusal> {
    if !matches!(
        op,
        OpBinary::Add
            | OpBinary::Sub
            | OpBinary::AddElem
            | OpBinary::SubElem
            | OpBinary::MulElem
            | OpBinary::DivElem
    ) {
        return Err(Refusal::UnsupportedOperator);
    }
    if lhs.len() != rhs.len() {
        return Err(Refusal::TypeMismatch);
    }
    lhs.iter()
        .zip(rhs)
        .map(|(lhs, rhs)| binary(op, lhs, rhs))
        .collect::<Result<Vec<_>, _>>()
        .map(Value::Array)
}
