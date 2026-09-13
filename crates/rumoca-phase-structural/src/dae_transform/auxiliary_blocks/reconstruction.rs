//! Materialize a proved auxiliary block through one aggregate function owner.

use rumoca_core::VarName;
use rumoca_ir_dae as dae;

use super::super::constraints::DifferentiationFacts;
use super::super::expressions::ExpressionRebuilder;
use super::{AuxiliaryBlock, AuxiliarySystem, SourceValue};

#[derive(Clone, Copy)]
pub(in crate::dae_transform) struct AuxiliaryExpression<'dae> {
    pub(super) matrix: dae::ExprId<'dae>,
    pub(super) value: dae::ExprId<'dae>,
}

pub(in crate::dae_transform) fn create_functions<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    facts: &DifferentiationFacts,
) -> Result<Vec<Option<dae::FunctionId<'target>>>, dae::DaeConstructionError> {
    let mut functions = vec![None; facts.auxiliary_blocks.len()];
    for block in facts.auxiliary_blocks.iter().flatten() {
        let owner = &mut functions[block.variable as usize];
        if owner.is_none() {
            *owner = Some(create_function(source, target, block)?);
        }
    }
    Ok(functions)
}

fn create_function<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    block: &AuxiliaryBlock,
) -> Result<dae::FunctionId<'target>, dae::DaeConstructionError> {
    let source_row = source.expression_id(block.residual() as usize).unwrap();
    let at = dae::DaeProvenance::generated(
        dae::DaeGeneration::IndexReduction,
        source.expression(source_row).unwrap().provenance().span(),
    )?;
    let (matrix_type, vector_type) = target.types(|types| {
        Ok((
            types.derived(
                dae::ValueType::array(dae::ScalarType::Real, [block.extent, block.extent]),
                at,
            )?,
            types.derived(
                dae::ValueType::array(dae::ScalarType::Real, [block.extent]),
                at,
            )?,
        ))
    })?;
    let (function, ()) = target.function(
        dae::FunctionSignature::new(
            VarName::new(format!("$auxiliary_linear_{}", block.variable)),
            [matrix_type, vector_type],
            [vector_type],
            at,
        ),
        |model, reservation| {
            let (matrix, rhs, output) = model.functions(|functions| {
                Ok((
                    functions.parameter(&reservation, VarName::new("A"), 0, at)?,
                    functions.parameter(&reservation, VarName::new("b"), 1, at)?,
                    functions.output(&reservation, VarName::new("q"), 0, at)?,
                ))
            })?;
            let value = model.expressions(|expressions| {
                let matrix = expressions.at(at).function_parameter(matrix)?;
                let rhs = expressions.at(at).function_parameter(rhs)?;
                expressions
                    .at(at)
                    .builtin(dae::PureBuiltin::LinearSolve, [matrix, rhs])
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, output, value, at))?;
            model.functions(|functions| functions.define(body, at))
        },
    )?;
    Ok(function)
}

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    pub(in crate::dae_transform) fn auxiliary_value(
        &mut self,
        variable: u32,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let block = self.facts.auxiliary_blocks[variable as usize]
            .as_ref()
            .expect("proved auxiliary membership")
            .clone();
        let value = self
            .auxiliary_expression(block.variable, order, provenance)?
            .value;
        let AuxiliarySystem::Scalars { variables, .. } = &block.system else {
            return Ok(value);
        };
        let offset = variables
            .binary_search(&variable)
            .expect("proved scalar block membership");
        let index = self
            .target
            .at(provenance)
            .literal(dae::DaeLiteral::Integer(offset as i64 + 1))?;
        self.target.at(provenance).index(
            value,
            [dae::Subscript::Index {
                expression: index,
                provenance,
            }],
        )
    }

    fn auxiliary_expression(
        &mut self,
        variable: u32,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<AuxiliaryExpression<'target>, dae::DaeConstructionError> {
        assert!(
            order <= 2,
            "auxiliary preflight admits orders zero through two"
        );
        let key = (variable, order, order > 0 && self.state_only_derivative);
        if let Some(&expression) = self.auxiliary_expressions.get(&key) {
            return Ok(expression);
        }
        let block = self.facts.auxiliary_blocks[variable as usize]
            .as_ref()
            .expect("auxiliary source proof exists")
            .clone();
        let (matrix, mut rhs) = self.auxiliary_operands(&block, order, provenance)?;
        let solve_matrix = if order == 0 {
            matrix
        } else {
            let primal = self.auxiliary_expression(variable, 0, provenance)?;
            let product = self.target.at(provenance).binary(
                dae::BinaryOperator::Multiply,
                matrix,
                primal.value,
            )?;
            rhs = self
                .target
                .at(provenance)
                .binary(dae::BinaryOperator::Subtract, rhs, product)?;
            if order == 2 {
                let first = self.auxiliary_expression(variable, 1, provenance)?;
                let mixed = self.target.at(provenance).binary(
                    dae::BinaryOperator::Multiply,
                    first.matrix,
                    first.value,
                )?;
                let two = self
                    .target
                    .at(provenance)
                    .literal(dae::DaeLiteral::Real(2.0))?;
                let mixed =
                    self.target
                        .at(provenance)
                        .binary(dae::BinaryOperator::Multiply, two, mixed)?;
                rhs =
                    self.target
                        .at(provenance)
                        .binary(dae::BinaryOperator::Subtract, rhs, mixed)?;
            }
            primal.matrix
        };
        let function =
            self.auxiliary_functions[variable as usize].expect("proved block function reserved");
        let value = self
            .target
            .at(provenance)
            .call(function, 0, [solve_matrix, rhs])?;
        let expression = AuxiliaryExpression { matrix, value };
        self.auxiliary_expressions.insert(key, expression);
        Ok(expression)
    }

    fn auxiliary_operands(
        &mut self,
        block: &AuxiliaryBlock,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<(dae::ExprId<'target>, dae::ExprId<'target>), dae::DaeConstructionError> {
        let source_rows = match &block.system {
            AuxiliarySystem::DotRows(rows) => rows,
            AuxiliarySystem::Map { matrix, rhs, .. } => {
                return Ok((
                    self.tensor_coefficient(matrix, order, provenance)?,
                    self.auxiliary_operand(rhs, order, provenance)?,
                ));
            }
            AuxiliarySystem::Scalars { matrix, rhs, .. } => {
                return Ok((
                    self.tensor_coefficient(matrix, order, provenance)?,
                    self.tensor_coefficient(rhs, order, provenance)?,
                ));
            }
        };
        let mut rows = Vec::with_capacity(source_rows.len());
        let mut rhs = Vec::with_capacity(source_rows.len());
        for row in source_rows {
            rows.push(self.auxiliary_operand(&row.coefficient, order, provenance)?);
            rhs.push(self.auxiliary_operand(&row.rhs, order, provenance)?);
        }
        let matrix = self.target.at(provenance).array(rows)?;
        let rhs = self.target.at(provenance).array(rhs)?;
        Ok((matrix, rhs))
    }

    pub(super) fn auxiliary_operand(
        &mut self,
        expression: &SourceValue,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let source = self
            .source
            .expression_id(expression.expression as usize)
            .unwrap();
        let previous =
            std::mem::replace(&mut self.function_context, expression.context(self.source));
        let value = if order == 0 {
            self.materialize_exact_value(source, provenance)
        } else {
            self.differentiate_order(source, order, provenance)
                .and_then(|derivative| self.materialize_derivative(derivative, source, provenance))
        };
        self.function_context = previous;
        let value = value?;
        if self.target.value_type(value, provenance)?.scalar_type() == dae::ScalarType::Real {
            return Ok(value);
        }
        let one = self
            .target
            .at(provenance)
            .literal(dae::DaeLiteral::Real(1.0))?;
        self.target
            .at(provenance)
            .binary(dae::BinaryOperator::Multiply, one, value)
    }
}
