//! Correct-by-construction lowering of affine state-derivative BLT blocks.
//!
//! Structural matching may prove a square derivative block whose rows are not
//! already written as isolated `der(x) = rhs` equations. This module accepts
//! only blocks whose residual expressions are syntactically affine in exactly
//! those derivative coordinates, evaluates their exact coefficient functions
//! under basis seeds, and emits the existing Solve-IR `LinSolve` node. The
//! runtime therefore receives an ordinary linear solve, never Modelica or DAE
//! semantics.

use std::collections::{BTreeSet, HashMap, HashSet};

use super::*;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Affinity {
    Constant,
    Affine,
}

pub(super) fn derivative_blocks<'dae>(
    blocks: &[BltBlock<'dae>],
    rows: &HashMap<usize, UnknownId<'dae>>,
) -> Vec<Vec<(usize, UnknownId<'dae>)>> {
    blocks
        .iter()
        .filter_map(|block| {
            let BltBlock::AlgebraicLoop { equations, .. } = block else {
                return None;
            };
            let block = equations
                .iter()
                .filter_map(|equation| {
                    rows.get(&equation.0)
                        .copied()
                        .map(|unknown| (equation.0, unknown))
                })
                .collect::<Vec<_>>();
            (block.len() > 1
                && block
                    .iter()
                    .all(|(_, unknown)| matches!(unknown, UnknownId::Derivative { .. })))
            .then_some(block)
        })
        .collect()
}

pub(super) fn lower_affine_blocks<'dae>(
    context: ContinuousContext<'_, 'dae>,
    blocks: &[Vec<(usize, UnknownId<'dae>)>],
    already_covered: &BTreeSet<usize>,
    output: &mut DerivativeRows,
) -> Result<(BTreeSet<usize>, AffineDerivativeSystems<'dae>), LowerError> {
    let mut covered = BTreeSet::new();
    let mut systems = Vec::new();
    for block in blocks {
        let overlap = block
            .iter()
            .filter(|(row, _)| already_covered.contains(row))
            .count();
        if overlap == block.len() {
            continue;
        }
        if overlap != 0 {
            return Err(LowerError::contract(
                "a derivative BLT block only partially overlaps a compact tensor owner",
                block_span(context.view, context.derivatives, block)?,
            ));
        }
        let (group, rows, system) = lower_affine_block(context, block)?;
        if rows.iter().any(|row| !covered.insert(*row)) {
            return Err(LowerError::contract(
                "a continuous row belongs to two derivative BLT blocks",
                group.span,
            ));
        }
        output.push_tensor(group);
        systems.push(system);
    }
    Ok((covered, AffineDerivativeSystems::construct(systems)?))
}

fn lower_affine_block<'dae>(
    context: ContinuousContext<'_, 'dae>,
    block: &[(usize, UnknownId<'dae>)],
) -> Result<
    (
        ImplicitTensorDerivative,
        Vec<usize>,
        AffineDerivativeSystem<'dae>,
    ),
    LowerError,
> {
    let span = block_span(context.view, context.derivatives, block)?;
    let targets = block
        .iter()
        .map(|(_, unknown)| derivative_unknown(*unknown, span))
        .collect::<Result<HashSet<_>, _>>()?;
    let mut rows = block
        .iter()
        .map(|(row, unknown)| {
            let (state, scalar) = derivative_unknown(*unknown, span)?;
            let source = context
                .derivatives
                .definition(state, scalar)
                .ok_or_else(|| {
                    LowerError::contract("derivative BLT row has no indexed source", span)
                })?;
            let domain_point = source
                .domain_point
                .as_ref()
                .map(|(domain, values)| (*domain, values.as_slice()));
            let selector = ScalarSelector::new(context.view, domain_point);
            let (expression, scalar) =
                selector.select_array_element(source.expression, source.scalar)?;
            let expression = selector.structural_branch(expression, scalar)?;
            prove_affine(context.view, expression, &targets, span)?;
            Ok((
                *row,
                AffineDerivativeRow {
                    expression,
                    scalar,
                    domain_point: source.domain_point.clone(),
                },
            ))
        })
        .collect::<Result<Vec<_>, LowerError>>()?;
    rows.sort_by_key(|(row, _)| *row);

    let mut unknowns = block
        .iter()
        .map(|(_, unknown)| {
            let (state, scalar) = derivative_unknown(*unknown, span)?;
            let solve::ScalarSlot::Y { index, .. } =
                variable_scalar_slot(context.layout, state.index(), scalar, span)?
            else {
                unreachable!("state declarations are Y slots")
            };
            Ok((index, AffineDerivativeUnknown { state, scalar }))
        })
        .collect::<Result<Vec<_>, LowerError>>()?;
    unknowns.sort_by_key(|(index, _)| *index);
    let output_start = contiguous_affine_output_start(&unknowns, span)?;

    let scalar_rows = rows.iter().map(|(_, row)| row.clone()).collect::<Vec<_>>();
    let scalar_unknowns = unknowns
        .iter()
        .map(|(_, unknown)| *unknown)
        .collect::<Vec<_>>();
    let system = AffineDerivativeSystem {
        rows: scalar_rows,
        unknowns: scalar_unknowns,
        span,
    };
    let (matrix_start, rhs_start, next_reg, setup_ops) =
        ScalarCompiler::new(context.view, context.layout, None)
            .with_function_conditional_owners(context.function_conditional_owners)
            .with_derivative_definitions(context.derivatives)
            .affine_derivative_system(&system.rows, &system.unknowns, span)
            .map_err(|error| {
                LowerError::non_computable(
                    format!("affine derivative block construction failed: {error}"),
                    span,
                )
            })?;
    let n = system.unknowns.len();
    let provenance =
        solve::PatternProvenance::derived(solve::PatternDerivation::ConservativeFull, span)
            .map_err(|error| LowerError::non_computable(error.to_string(), span))?;
    let matrix_pattern = solve::StructuralPattern::full(n, n, provenance)
        .map_err(|error| LowerError::non_computable(error.to_string(), span))?;
    Ok((
        ImplicitTensorDerivative {
            node: solve::ComputeNode::LinSolve {
                setup_ops,
                matrix_start,
                rhs_start,
                n,
                next_reg,
                matrix_pattern,
                metadata: solve::TensorNodeMetadata::default(),
                span,
            },
            output_start,
            rows: n,
            span,
        },
        rows.into_iter().map(|(row, _)| row).collect(),
        system,
    ))
}

fn contiguous_affine_output_start(
    unknowns: &[(usize, AffineDerivativeUnknown<'_>)],
    span: Span,
) -> Result<usize, LowerError> {
    let output_start = unknowns[0].0;
    for (offset, (index, _)) in unknowns.iter().enumerate() {
        let expected = output_start
            .checked_add(offset)
            .ok_or_else(|| LowerError::contract("affine derivative output slot overflow", span))?;
        if *index != expected {
            return Err(LowerError::non_computable(
                "affine derivative block outputs are not contiguous Solve state slots",
                span,
            ));
        }
    }
    Ok(output_start)
}

fn derivative_unknown<'dae>(
    unknown: UnknownId<'dae>,
    span: Span,
) -> Result<(dae::StateId<'dae>, usize), LowerError> {
    let UnknownId::Derivative { state, scalar } = unknown else {
        return Err(LowerError::contract(
            "derivative BLT block contains a non-derivative unknown",
            span,
        ));
    };
    Ok((state, scalar as usize))
}

fn block_span<'dae>(
    view: dae::DaeView<'dae>,
    derivatives: &DerivativeRowIndex<'dae>,
    block: &[(usize, UnknownId<'dae>)],
) -> Result<Span, LowerError> {
    let (_, unknown) = block.first().copied().ok_or_else(|| {
        LowerError::contract("derivative BLT block is empty", first_model_span(view))
    })?;
    let (state, scalar) = derivative_unknown(unknown, first_model_span(view))?;
    let source = derivatives.definition(state, scalar).ok_or_else(|| {
        LowerError::contract(
            "derivative BLT block has no source equation",
            first_model_span(view),
        )
    })?;
    Ok(view
        .expression(source.expression)
        .expect("indexed derivative expression resolves")
        .provenance()
        .span())
}

fn prove_affine<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    targets: &HashSet<(dae::StateId<'dae>, usize)>,
    span: Span,
) -> Result<(), LowerError> {
    AffineProof {
        view,
        targets,
        span,
        active_expressions: HashSet::new(),
        active_functions: HashSet::new(),
        parameter_frames: Vec::new(),
    }
    .expression(expression)
    .map(|_| ())
}

struct AffineProof<'proof, 'dae> {
    view: dae::DaeView<'dae>,
    targets: &'proof HashSet<(dae::StateId<'dae>, usize)>,
    span: Span,
    active_expressions: HashSet<dae::ExprId<'dae>>,
    active_functions: HashSet<dae::FunctionId<'dae>>,
    parameter_frames: Vec<HashMap<dae::FunctionParameterId<'dae>, Affinity>>,
}

impl<'dae> AffineProof<'_, 'dae> {
    fn expression(&mut self, expression: dae::ExprId<'dae>) -> Result<Affinity, LowerError> {
        if !self.active_expressions.insert(expression) {
            return Err(LowerError::non_computable(
                "affine derivative proof encountered a recursive expression",
                self.span,
            ));
        }
        let node = self
            .view
            .expression(expression)
            .expect("affine proof expression resolves");
        let found = match node.operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state)) => {
                if self.targets.iter().any(|(target, _)| *target == state) {
                    Affinity::Affine
                } else {
                    Affinity::Constant
                }
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => self.parameter_affinity(parameter),
            dae::ExpressionOperation::Literal(_) | dae::ExpressionOperation::Coordinate(_) => {
                Affinity::Constant
            }
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                operand,
            } => self.expression(operand)?,
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                self.binary(operator, lhs, rhs)?
            }
            dae::ExpressionOperation::Array(elements)
            | dae::ExpressionOperation::Record(elements) => self.operands(elements)?,
            dae::ExpressionOperation::Field { base, .. } => self.expression(base)?,
            dae::ExpressionOperation::Index { base, subscripts } => self.index(base, subscripts)?,
            dae::ExpressionOperation::Builtin {
                builtin:
                    dae::PureBuiltin::Vector
                    | dae::PureBuiltin::Transpose
                    | dae::PureBuiltin::Diagonal
                    | dae::PureBuiltin::Skew,
                arguments,
            } => self.operands(arguments)?,
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
                ..
            } => self.call(function, output, arguments)?,
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.expression(definition.rhs())?
            }
            _ if !self.depends_on_derivative(expression) => Affinity::Constant,
            _ => {
                return Err(LowerError::non_computable(
                    format!(
                        "derivative BLT block is not affine: unsupported derivative-dependent operation {:?}",
                        node.kind()
                    ),
                    self.span,
                ));
            }
        };
        self.active_expressions.remove(&expression);
        Ok(found)
    }

    fn binary(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
    ) -> Result<Affinity, LowerError> {
        let lhs = self.expression(lhs)?;
        let rhs = self.expression(rhs)?;
        match operator {
            dae::BinaryOperator::Add | dae::BinaryOperator::Subtract => Ok(merge(lhs, rhs)),
            dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => {
                if lhs == Affinity::Affine && rhs == Affinity::Affine {
                    return nonlinear(self.span, "product of two derivative-dependent terms");
                }
                Ok(merge(lhs, rhs))
            }
            dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide => {
                if rhs == Affinity::Affine {
                    return nonlinear(self.span, "derivative-dependent denominator");
                }
                Ok(lhs)
            }
            _ if lhs == Affinity::Constant && rhs == Affinity::Constant => Ok(Affinity::Constant),
            _ => nonlinear(self.span, "non-affine derivative operator"),
        }
    }

    fn operands(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
    ) -> Result<Affinity, LowerError> {
        operands
            .iter()
            .try_fold(Affinity::Constant, |found, operand| {
                self.expression(operand)
                    .map(|operand| merge(found, operand))
            })
    }

    fn index(
        &mut self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
    ) -> Result<Affinity, LowerError> {
        for subscript in subscripts.iter() {
            let expression = match subscript {
                dae::SubscriptView::Index { expression, .. }
                | dae::SubscriptView::Slice { expression, .. } => expression,
                dae::SubscriptView::Whole { .. } => continue,
            };
            if self.expression(expression)? == Affinity::Affine {
                return nonlinear(self.span, "derivative-dependent array subscript");
            }
        }
        self.expression(base)
    }

    fn call(
        &mut self,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
    ) -> Result<Affinity, LowerError> {
        let argument_affinities = arguments
            .iter()
            .map(|argument| self.expression(argument))
            .collect::<Result<Vec<_>, _>>()?;
        if argument_affinities
            .iter()
            .all(|affinity| *affinity == Affinity::Constant)
        {
            return Ok(Affinity::Constant);
        }
        let function_view = self
            .view
            .function(function)
            .expect("checked call function resolves");
        if function_view.is_external() || !self.active_functions.insert(function) {
            return nonlinear(
                self.span,
                "external or recursive derivative-dependent function",
            );
        }
        let parameters = function_view
            .parameters()
            .zip(argument_affinities)
            .map(|(parameter, affinity)| (parameter.id(), affinity))
            .collect();
        self.parameter_frames.push(parameters);
        let result = self
            .prove_assertions_constant(function_view.statements())
            .and_then(|()| {
                function_view
                    .result_values()
                    .get(output as usize)
                    .ok_or_else(|| {
                        LowerError::contract("function result definition is missing", self.span)
                    })
                    .and_then(|definition| self.expression(definition.rhs()))
            });
        self.parameter_frames.pop();
        self.active_functions.remove(&function);
        result
    }

    fn prove_assertions_constant(
        &mut self,
        statements: dae::FunctionStatements<'dae>,
    ) -> Result<(), LowerError> {
        for statement in statements {
            match statement {
                dae::FunctionStatementView::Assertion { condition, .. }
                    if self.expression(condition)? == Affinity::Affine =>
                {
                    return nonlinear(
                        self.span,
                        "derivative-dependent assertion in an affine function call",
                    );
                }
                dae::FunctionStatementView::For { statements, .. } => {
                    self.prove_assertions_constant(statements)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn parameter_affinity(&self, parameter: dae::FunctionParameterId<'dae>) -> Affinity {
        self.parameter_frames
            .iter()
            .rev()
            .find_map(|frame| frame.get(&parameter).copied())
            .unwrap_or(Affinity::Constant)
    }

    fn depends_on_derivative(&self, expression: dae::ExprId<'dae>) -> bool {
        let mut depends = false;
        dae::for_each_expression(self.view, expression, |_, node| match node.operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state)) => {
                depends |= self.targets.iter().any(|(target, _)| *target == state);
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => depends |= self.parameter_affinity(parameter) == Affinity::Affine,
            _ => {}
        });
        depends
    }
}

const fn merge(lhs: Affinity, rhs: Affinity) -> Affinity {
    if matches!((lhs, rhs), (Affinity::Constant, Affinity::Constant)) {
        Affinity::Constant
    } else {
        Affinity::Affine
    }
}

fn nonlinear<T>(span: Span, reason: &str) -> Result<T, LowerError> {
    Err(LowerError::non_computable(
        format!("derivative BLT block is not affine: {reason}"),
        span,
    ))
}
