//! Materialize a proved auxiliary block through one aggregate function owner.

use rumoca_core::VarName;
use rumoca_ir_dae as dae;

use super::super::constraints::DifferentiationFacts;
use super::super::expressions::ExpressionRebuilder;
use super::super::variables::TargetVariable;
use super::{AuxiliaryBlock, AuxiliarySystem, SourceValue};

#[derive(Clone, Copy)]
pub(in crate::dae_transform) struct AuxiliaryExpression<'dae> {
    pub(super) matrix: dae::ExprId<'dae>,
    pub(super) value: dae::ExprId<'dae>,
}

pub(in crate::dae_transform) struct AuxiliaryFunctions<'dae> {
    by_variable: Vec<Option<dae::FunctionId<'dae>>>,
    by_extent: std::collections::BTreeMap<u32, dae::FunctionId<'dae>>,
}

impl<'dae> AuxiliaryFunctions<'dae> {
    pub(in crate::dae_transform) fn linear_solve(&self, extent: u32) -> dae::FunctionId<'dae> {
        self.by_extent[&extent]
    }
}

pub(in crate::dae_transform) fn create_functions<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    facts: &DifferentiationFacts,
) -> Result<AuxiliaryFunctions<'target>, dae::DaeConstructionError> {
    let mut functions = AuxiliaryFunctions {
        by_variable: vec![None; facts.auxiliary_blocks.len()],
        by_extent: std::collections::BTreeMap::new(),
    };
    for block in facts.auxiliary_blocks.iter().flatten() {
        let source_row = source.expression_id(block.residual() as usize).unwrap();
        let at = source.expression(source_row).unwrap().provenance();
        let function = insert_function(target, &mut functions, block.extent, at)?;
        functions.by_variable[block.variable as usize] = Some(function);
    }
    for index in 0..source.expression_count() {
        let node = source
            .expression(source.expression_id(index).unwrap())
            .unwrap();
        if matches!(
            node.operation(),
            dae::ExpressionOperation::Builtin {
                builtin: dae::PureBuiltin::LinearSolve,
                ..
            }
        ) {
            insert_function(
                target,
                &mut functions,
                node.value_type().dimensions()[0],
                node.provenance(),
            )?;
        }
    }
    Ok(functions)
}

fn insert_function<'target>(
    target: &mut dae::DaeConstruction<'target>,
    functions: &mut AuxiliaryFunctions<'target>,
    extent: u32,
    source: dae::DaeProvenance,
) -> Result<dae::FunctionId<'target>, dae::DaeConstructionError> {
    if let Some(&function) = functions.by_extent.get(&extent) {
        return Ok(function);
    }
    let at = dae::DaeProvenance::generated(dae::DaeGeneration::IndexReduction, source.span())?;
    let function = create_function(target, extent, at)?;
    functions.by_extent.insert(extent, function);
    Ok(function)
}

fn create_function<'target>(
    target: &mut dae::DaeConstruction<'target>,
    extent: u32,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'target>, dae::DaeConstructionError> {
    let (matrix_type, vector_type) = target.types(|types| {
        Ok((
            types.derived(
                dae::ValueType::array(dae::ScalarType::Real, [extent, extent]),
                at,
            )?,
            types.derived(dae::ValueType::array(dae::ScalarType::Real, [extent]), at)?,
        ))
    })?;
    let (function, ()) = target.function(
        dae::FunctionSignature::new(
            VarName::new(format!("$linear_solve_{extent}")),
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
        let previous = self.state_only_derivative;
        self.state_only_derivative |= order == 0;
        let result = self.auxiliary_expression_in_mode(variable, order, provenance);
        self.state_only_derivative = previous;
        result
    }

    fn auxiliary_expression_in_mode(
        &mut self,
        variable: u32,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<AuxiliaryExpression<'target>, dae::DaeConstructionError> {
        assert!(
            order <= 2,
            "auxiliary preflight admits orders zero through two"
        );
        let key = (variable, order, self.state_only_derivative);
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
            let primal = if self.state_only_derivative {
                self.auxiliary_expression(variable, 0, provenance)?
            } else {
                self.auxiliary_primal_reference(&block, provenance)?
            };
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
        let function = self.auxiliary_functions.by_variable[variable as usize]
            .expect("proved block function reserved");
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
        let matrix = self.auxiliary_matrix(block, order, provenance)?;
        let rhs = match &block.system {
            AuxiliarySystem::DotRows(rows) => {
                let values = rows
                    .iter()
                    .map(|row| self.auxiliary_operand(&row.rhs, order, provenance))
                    .collect::<Result<Vec<_>, _>>()?;
                self.target.at(provenance).array(values)?
            }
            AuxiliarySystem::Map { rhs, .. }
            | AuxiliarySystem::VectorRows { rhs, .. }
            | AuxiliarySystem::Scalars { rhs, .. } => {
                self.tensor_coefficient(rhs, order, provenance)?
            }
        };
        Ok((matrix, rhs))
    }

    fn auxiliary_matrix(
        &mut self,
        block: &AuxiliaryBlock,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        match &block.system {
            AuxiliarySystem::DotRows(rows) => {
                let values = rows
                    .iter()
                    .map(|row| self.auxiliary_operand(&row.coefficient, order, provenance))
                    .collect::<Result<Vec<_>, _>>()?;
                self.target.at(provenance).array(values)
            }
            AuxiliarySystem::Map { matrix, .. }
            | AuxiliarySystem::VectorRows { matrix, .. }
            | AuxiliarySystem::Scalars { matrix, .. } => {
                self.tensor_coefficient(matrix, order, provenance)
            }
        }
    }

    fn auxiliary_primal_reference(
        &mut self,
        block: &AuxiliaryBlock,
        provenance: dae::DaeProvenance,
    ) -> Result<AuxiliaryExpression<'target>, dae::DaeConstructionError> {
        let matrix = self.auxiliary_matrix(block, 0, provenance)?;
        let value = if let AuxiliarySystem::Scalars { variables, .. } = &block.system {
            let values = variables
                .iter()
                .map(|&variable| self.auxiliary_coordinate(variable, provenance))
                .collect::<Result<Vec<_>, _>>()?;
            self.target.at(provenance).array(values)?
        } else {
            self.auxiliary_coordinate(block.variable, provenance)?
        };
        Ok(AuxiliaryExpression { matrix, value })
    }

    fn auxiliary_coordinate(
        &mut self,
        variable: u32,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let coordinate = match self.variables[variable as usize].identity {
            TargetVariable::Algebraic(id) => dae::CoordinateInput::Algebraic(id),
            TargetVariable::State(id) => dae::CoordinateInput::State(id),
            _ => unreachable!("proved continuous Real auxiliary coordinate retains its role"),
        };
        self.target.at(provenance).coordinate(coordinate)
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
            self.differentiation_value(source, provenance)
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
