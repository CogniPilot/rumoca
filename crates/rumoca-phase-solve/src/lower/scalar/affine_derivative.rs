//! Scalar derivative isolation with symbolic coefficients, including repeated
//! occurrences created by state demotion. Coefficients are never recovered by
//! subtracting two residual evaluations, which could cancel a large offset.

use super::*;

#[derive(Clone, Copy)]
enum Arithmetic {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Arithmetic {
    fn operator(self) -> dae::BinaryOperator {
        match self {
            Self::Add => dae::BinaryOperator::Add,
            Self::Subtract => dae::BinaryOperator::Subtract,
            Self::Multiply => dae::BinaryOperator::Multiply,
            Self::Divide => dae::BinaryOperator::Divide,
        }
    }

    fn apply(self, lhs: f64, rhs: f64) -> f64 {
        match self {
            Self::Add => lhs + rhs,
            Self::Subtract => lhs - rhs,
            Self::Multiply => lhs * rhs,
            Self::Divide => lhs / rhs,
        }
    }
}

enum Term<'dae> {
    Literal(f64),
    Source(dae::ExprId<'dae>, usize),
    Binary(Arithmetic, usize, usize),
}

#[derive(Clone, Copy)]
struct Form {
    coefficient: Option<usize>,
    offset: usize,
}

pub(in crate::lower) struct AffineScalarDerivative<'dae> {
    terms: Vec<Term<'dae>>,
    coefficient: usize,
    offset: usize,
    span: Span,
}

impl<'dae> AffineScalarDerivative<'dae> {
    pub(in crate::lower) fn derive(
        selector: ScalarSelector<'dae>,
        residual: dae::ExprId<'dae>,
        scalar: usize,
        state: dae::StateId<'dae>,
        state_scalar: usize,
    ) -> Result<Self, LowerError> {
        let span = selector.node(residual).provenance().span();
        let mut proof = AffineProof {
            selector,
            state,
            state_scalar,
            span,
            terms: vec![Term::Literal(0.0), Term::Literal(1.0)],
            memo: HashMap::new(),
        };
        let form = proof.expression(residual, scalar)?;
        let coefficient = form.coefficient.ok_or_else(|| proof.unsupported())?;
        if proof
            .constant(coefficient)?
            .is_some_and(|value| value == 0.0 || !value.is_finite())
        {
            return Err(LowerError::non_computable(
                "matched derivative has a zero or non-finite affine coefficient",
                span,
            ));
        }
        Ok(Self {
            terms: proof.terms,
            coefficient,
            offset: form.offset,
            span,
        })
    }

    pub(in crate::lower) fn lower(
        &self,
        compiler: &mut ScalarCompiler<'_, 'dae>,
    ) -> Result<solve::Reg, LowerError> {
        let mut values = Vec::with_capacity(self.terms.len());
        for term in &self.terms {
            values.push(match *term {
                Term::Literal(value) => compiler.constant(value, self.span)?,
                Term::Source(expression, scalar) => compiler.expression(expression, scalar)?,
                Term::Binary(operator, lhs, rhs) => {
                    compiler.binary(operator.operator(), values[lhs], values[rhs], self.span)?
                }
            });
        }
        let numerator =
            compiler.unary(dae::UnaryOperator::Negate, values[self.offset], self.span)?;
        compiler.affine_quotient(numerator, values[self.coefficient], self.span)
    }
}

impl<'dae> ScalarCompiler<'_, 'dae> {
    /// Isolate `coefficient * der(x) = numerator` on its finite, nonzero domain.
    /// Invalid coefficients must not turn a finite numerator into a plausible
    /// zero derivative (`finite / infinity`). Zero division reaches the same
    /// non-finite-result rejection used by the existing runtime equation path.
    pub(in crate::lower) fn affine_quotient(
        &mut self,
        numerator: solve::Reg,
        coefficient: solve::Reg,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let max = self.constant(f64::MAX, span)?;
        let min = self.constant(-f64::MAX, span)?;
        let lower = self.binary(dae::BinaryOperator::GreaterEqual, coefficient, min, span)?;
        let upper = self.binary(dae::BinaryOperator::LessEqual, coefficient, max, span)?;
        let finite = self.binary(dae::BinaryOperator::And, lower, upper, span)?;
        let zero = self.constant(0.0, span)?;
        let denominator = self.select(finite, coefficient, zero, span)?;
        self.binary(dae::BinaryOperator::Divide, numerator, denominator, span)
    }

    pub(in crate::lower) fn affine_derivative_program(
        mut self,
        proof: &AffineScalarDerivative<'dae>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let src = proof.lower(&mut self)?;
        self.ops.push(solve::LinearOp::StoreOutput { src });
        Ok(self.ops)
    }
}

struct AffineProof<'dae> {
    selector: ScalarSelector<'dae>,
    state: dae::StateId<'dae>,
    state_scalar: usize,
    span: Span,
    terms: Vec<Term<'dae>>,
    memo: HashMap<(dae::ExprId<'dae>, usize), Form>,
}

impl<'dae> AffineProof<'dae> {
    fn unsupported(&self) -> LowerError {
        LowerError::non_computable(
            "matched derivative is not an isolated affine product or a proved scalar affine residual",
            self.span,
        )
    }

    fn push(&mut self, term: Term<'dae>) -> usize {
        let index = self.terms.len();
        self.terms.push(term);
        index
    }

    fn binary_term(&mut self, operator: Arithmetic, lhs: usize, rhs: usize) -> usize {
        self.push(Term::Binary(operator, lhs, rhs))
    }

    fn expression(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<Form, LowerError> {
        let (expression, scalar) = self.selector.select_array_element(expression, scalar)?;
        let expression = self.selector.structural_branch(expression, scalar)?;
        if let Some(form) = self.memo.get(&(expression, scalar)) {
            return Ok(*form);
        }
        let form = self.expression_uncached(expression, scalar)?;
        self.memo.insert((expression, scalar), form);
        Ok(form)
    }

    fn expression_uncached(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<Form, LowerError> {
        if is_target_derivative(
            &self.selector,
            expression,
            scalar,
            self.state,
            self.state_scalar,
        )? {
            return Ok(Form {
                coefficient: Some(1),
                offset: 0,
            });
        }
        let view = self.selector.view();
        if !expression_contains_derivative(view, expression) {
            return Ok(Form {
                coefficient: None,
                offset: self.push(Term::Source(expression, scalar)),
            });
        }
        match self.selector.node(expression).operation() {
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus,
                operand,
            } => self.expression(operand, scalar),
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Negate,
                operand,
            } => {
                let form = self.expression(operand, scalar)?;
                Ok(Form {
                    coefficient: form
                        .coefficient
                        .map(|value| self.binary_term(Arithmetic::Subtract, 0, value)),
                    offset: self.binary_term(Arithmetic::Subtract, 0, form.offset),
                })
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                self.binary(operator, lhs, rhs, scalar)
            }
            _ => Err(self.unsupported()),
        }
    }

    fn binary(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<Form, LowerError> {
        use dae::BinaryOperator as Op;
        let view = self.selector.view();
        let left_count = scalar_count(view, lhs);
        let right_count = scalar_count(view, rhs);
        let operator = match operator {
            Op::Add => Arithmetic::Add,
            Op::Subtract => Arithmetic::Subtract,
            Op::ElementwiseMultiply => Arithmetic::Multiply,
            Op::ElementwiseDivide => Arithmetic::Divide,
            Op::Multiply if left_count == 1 || right_count == 1 => Arithmetic::Multiply,
            Op::Divide if right_count == 1 => Arithmetic::Divide,
            _ => return Err(self.unsupported()),
        };
        let left = self.expression(lhs, if left_count == 1 { 0 } else { scalar })?;
        let right = self.expression(rhs, if right_count == 1 { 0 } else { scalar })?;
        match operator {
            Arithmetic::Add | Arithmetic::Subtract => {
                let coefficient = match (left.coefficient, right.coefficient) {
                    (None, None) => None,
                    (lhs, rhs) => {
                        Some(self.binary_term(operator, lhs.unwrap_or(0), rhs.unwrap_or(0)))
                    }
                };
                Ok(Form {
                    coefficient,
                    offset: self.binary_term(operator, left.offset, right.offset),
                })
            }
            Arithmetic::Multiply => self.product(left, right),
            Arithmetic::Divide if right.coefficient.is_none() => Ok(Form {
                coefficient: left
                    .coefficient
                    .map(|value| self.binary_term(Arithmetic::Divide, value, right.offset)),
                offset: self.binary_term(Arithmetic::Divide, left.offset, right.offset),
            }),
            Arithmetic::Divide => Err(self.unsupported()),
        }
    }

    fn product(&mut self, left: Form, right: Form) -> Result<Form, LowerError> {
        use Arithmetic::Multiply;
        let coefficient = match (left.coefficient, right.coefficient) {
            (Some(_), Some(_)) => return Err(self.unsupported()),
            (Some(value), None) => Some(self.binary_term(Multiply, value, right.offset)),
            (None, Some(value)) => Some(self.binary_term(Multiply, left.offset, value)),
            (None, None) => None,
        };
        Ok(Form {
            coefficient,
            offset: self.binary_term(Multiply, left.offset, right.offset),
        })
    }

    fn constant(&self, term: usize) -> Result<Option<f64>, LowerError> {
        let mut needed = vec![false; self.terms.len()];
        let mut pending = vec![term];
        while let Some(index) = pending.pop() {
            if std::mem::replace(&mut needed[index], true) {
                continue;
            }
            if let Term::Binary(_, lhs, rhs) = self.terms[index] {
                pending.extend([lhs, rhs]);
            }
        }
        let mut values = vec![None; self.terms.len()];
        for (index, node) in self
            .terms
            .iter()
            .enumerate()
            .filter(|(index, _)| needed[*index])
        {
            values[index] = match *node {
                Term::Literal(value) => Some(value),
                Term::Source(expression, scalar) => self.constant_source(expression, scalar)?,
                Term::Binary(operator, lhs, rhs) => values[lhs]
                    .zip(values[rhs])
                    .map(|(lhs, rhs)| operator.apply(lhs, rhs)),
            };
        }
        Ok(values[term])
    }

    fn constant_source(
        &self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<Option<f64>, LowerError> {
        let variability = self.selector.node(expression).variability();
        if variability > dae::ExpressionVariability::Parameter {
            return Ok(None);
        }
        self.selector.constant_real(expression, scalar).map(Some)
    }
}
