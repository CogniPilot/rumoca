use ndarray::{ArrayD, IxDyn};
use rumoca_parser::ast;
use rumoca_parser::Visitor;
use std::collections::HashMap;

#[derive(Default, Clone)]
enum Value {
    #[default]
    None,
    Float(ArrayD<f64>),
    Bool(ArrayD<bool>),
}

//=============================================================================
/// Holds an evaluated version of each expression node in the tree if possible
#[derive(Default)]
pub struct Evaluator<'a> {
    /// A struct to traverse a tree evaluate expressions
    val: HashMap<&'a ast::Expression, Value>,
}

/// Traverses the tree evaluating everything that it can, turning it into bool of f64
impl<'a> Visitor<'a> for Evaluator<'a> {
    fn enter_expression(&mut self, node: &'a ast::Expression) {
        self.val.insert(
            node,
            match node {
                //=============================================================
                // Terminals
                //
                ast::Expression::UnsignedInteger(s) => {
                    let shape = IxDyn(&[1]);
                    let values = vec![(*s).parse::<f64>().expect("failed to parse f64")];
                    Value::Float(ArrayD::from_shape_vec(shape, values).unwrap())
                }
                ast::Expression::UnsignedReal(s) => {
                    let shape = IxDyn(&[1]);
                    let values = vec![(*s).parse::<f64>().expect("failed to parse f64")];
                    Value::Float(ArrayD::from_shape_vec(shape, values).unwrap())
                }
                ast::Expression::Boolean(b) => {
                    let shape = IxDyn(&[1]);
                    let values = vec![*b];
                    Value::Bool(ArrayD::from_shape_vec(shape, values).unwrap())
                }

                //=============================================================
                // Unary Expressions
                //
                ast::Expression::Unary { op, rhs } => {
                    let rhs_ref = &**rhs; // unbox ref
                    let a: Value = self
                        .val
                        .get(rhs_ref)
                        .expect("failed to get unary rhs val")
                        .clone();
                    match op {
                        ast::UnaryOp::Paren => a,
                        ast::UnaryOp::Not => match a {
                            Value::Bool(a) => Value::Bool(!a),
                            _ => Value::None,
                        },
                        ast::UnaryOp::Negative => match a {
                            Value::Float(a) => Value::Float(-a),
                            _ => Value::None,
                        },
                        ast::UnaryOp::Positive => a,
                        ast::UnaryOp::ElemNegative => match a {
                            Value::Float(a) => Value::Float(-a),
                            _ => Value::None,
                        },
                        ast::UnaryOp::ElemPositive => a,
                        _ => Value::None,
                    }
                }

                //=============================================================
                // Binary Expressions
                //
                ast::Expression::Binary { op, lhs, rhs } => {
                    let lhs_ref = &**lhs; // unbox ref
                    let rhs_ref = &**rhs; // unbox ref
                    let a: Value = self
                        .val
                        .get(lhs_ref)
                        .expect("failed to get unary rhs val")
                        .clone();
                    let b: Value = self
                        .val
                        .get(rhs_ref)
                        .expect("failed to get unary rhs val")
                        .clone();
                    match op {
                        ast::BinaryOp::Add => match (a, b) {
                            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                            _ => Value::None,
                        },
                        ast::BinaryOp::Sub => match (a, b) {
                            (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
                            _ => Value::None,
                        },
                        ast::BinaryOp::Mul => match (a, b) {
                            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
                            _ => Value::None,
                        },
                        ast::BinaryOp::Div => match (a, b) {
                            (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
                            _ => Value::None,
                        },
                        ast::BinaryOp::Exp => match (a, b) {
                            (Value::Float(a), Value::Float(b)) => {
                                if a.shape() != [1] {
                                    panic!("exp called with non-scalar base")
                                }
                                if b.shape() != [1] {
                                    panic!("exp called with non-scalar exponent")
                                }
                                let shape = IxDyn(&[1]);
                                let values = vec![a[0].powf(b[0])];
                                Value::Float(ArrayD::from_shape_vec(shape, values).unwrap())
                            }
                            _ => Value::None,
                        },
                        _ => Value::None,
                    }
                }
                _ => Value::None,
            },
        );
    }
}
