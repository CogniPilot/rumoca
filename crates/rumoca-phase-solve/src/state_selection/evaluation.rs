use super::*;
use crate::{FormalDerivativePrograms, FormalResidualProgram, FormalStageProgram};
use rumoca_eval_dae::NumericEvaluator;
use rumoca_eval_solve::dense_basis::DenseStageMatrix;
use rumoca_eval_solve::{TypedValue, eval_pure_call, eval_pure_call_directional};
use rumoca_ir_solve as solve;

/// Trial values never become runtime seeds or initialization equations.
pub(super) struct TrialPoint {
    values: Vec<Option<Vec<f64>>>,
    retained_guesses: Vec<bool>,
}

impl TrialPoint {
    pub(super) fn seed_definitions(
        &mut self,
        programs: &FormalDerivativePrograms<'_, '_, '_>,
    ) -> Result<(), StructuralError> {
        for guess in programs.guesses() {
            if self.retained_guesses[guess.target.index() as usize] {
                continue;
            }
            let program = &guess.program;
            let arguments = program
                .inputs
                .iter()
                .zip(program.site.inputs())
                .map(|(&coordinate, kind)| self.argument(coordinate, kind))
                .collect::<Result<Vec<_>, _>>()?;
            let outputs = eval_pure_call(programs.table(), program.site.owner(), &arguments)
                .map_err(failure)?;
            check_assertions(&outputs[program.value_outputs..])?;
            self.values[guess.target.index() as usize] =
                Some(real_outputs(&outputs[..program.value_outputs])?);
        }
        Ok(())
    }

    pub(super) fn new(
        formal: FormalDerivativeView<'_, '_, '_>,
        overrides: &HashMap<String, f64>,
    ) -> Result<Self, StructuralError> {
        let mut evaluator = NumericEvaluator::with_overrides(formal.source, |v, scalar| {
            v.scalar_name(scalar)
                .and_then(|name| overrides.get(&name).copied())
        });
        let mut values = vec![None; formal.view.variables().count()];
        let mut retained_guesses = vec![false; values.len()];
        for (source, variable) in formal.source.variables() {
            if variable.value_type().scalar_type() == dae::ScalarType::String {
                continue;
            }
            let value = evaluator.initial_value(source).map_err(failure)?;
            let target = formal
                .coordinate(source, 0)
                .ok_or_else(|| failure("trial source coordinate has no formal value"))?;
            values[target.index() as usize] = Some(value);
            retained_guesses[target.index() as usize] = variable.role() == dae::VariableRole::State
                || variable.fixed() == Some(true)
                || matches!(
                    variable.state_select(),
                    StateSelect::Always | StateSelect::Prefer
                )
                || (0..variable.scalar_count()).any(|scalar| {
                    variable
                        .scalar_name(scalar)
                        .is_some_and(|name| overrides.contains_key(&name))
                });
            let mut order = 1;
            while let Some(target) = formal.coordinate(source, order) {
                values[target.index() as usize] = Some(vec![0.; variable.scalar_count()]);
                order += 1;
            }
        }
        Ok(Self {
            values,
            retained_guesses,
        })
    }

    pub(super) fn settle(
        &mut self,
        programs: &FormalDerivativePrograms<'_, '_, '_>,
        stage: &FormalStageProgram<'_, '_, '_>,
        columns: &[(FormalStageCoordinate<'_, '_>, usize)],
    ) -> Result<DenseStageMatrix, StructuralError> {
        for _ in 0..12 {
            let (residual, matrix) = self.evaluate(programs, stage, columns)?;
            if residual.iter().all(|r| r.abs() <= 1e-10) {
                return Ok(matrix);
            }
            let step = matrix
                .correction(&residual)
                .map_err(|e| failure(format!("trial correction: {e:?}")))?;
            for ((coordinate, scalar), delta) in columns.iter().zip(step) {
                self.values[coordinate.value().index() as usize]
                    .as_mut()
                    .ok_or_else(|| failure("trial column has no numeric value"))?[*scalar] += delta;
            }
        }
        Err(failure(format!(
            "stage {} trial constraints did not converge",
            stage.stage().level()
        )))
    }

    fn evaluate(
        &self,
        programs: &FormalDerivativePrograms<'_, '_, '_>,
        stage: &FormalStageProgram<'_, '_, '_>,
        columns: &[(FormalStageCoordinate<'_, '_>, usize)],
    ) -> Result<(Vec<f64>, DenseStageMatrix), StructuralError> {
        let mut residual = Vec::new();
        let mut matrix = Vec::new();
        for equation in stage.equations() {
            let arguments = equation
                .inputs()
                .iter()
                .zip(equation.site().inputs())
                .map(|(&coordinate, kind)| self.argument(coordinate, kind))
                .collect::<Result<Vec<_>, _>>()?;
            let outputs = eval_pure_call(programs.table(), equation.site().owner(), &arguments)
                .map_err(failure)?;
            check_assertions(&outputs[equation.residual_outputs()..])?;
            let values = real_outputs(&outputs[..equation.residual_outputs()])?;
            let count = values.len();
            residual.extend(values);
            matrix.extend(equation_jacobian(
                programs, equation, &arguments, columns, count,
            )?);
        }
        let matrix = DenseStageMatrix::new(residual.len(), columns.len(), &matrix)
            .map_err(|e| failure(format!("stage Jacobian: {e:?}")))?;
        Ok((residual, matrix))
    }

    fn argument(
        &self,
        coordinate: dae::CoordinateView<'_>,
        kind: &solve::SolveValueType,
    ) -> Result<TypedValue, StructuralError> {
        if matches!(coordinate, dae::CoordinateView::Time) {
            return typed(kind, &[0.]);
        }
        let id = variable(coordinate).ok_or_else(|| failure("unsupported trial coordinate"))?;
        let values = self.values[id as usize]
            .as_ref()
            .ok_or_else(|| failure("missing trial coordinate value"))?;
        typed(kind, values)
    }
}

fn equation_jacobian(
    programs: &FormalDerivativePrograms<'_, '_, '_>,
    equation: &FormalResidualProgram<'_, '_>,
    arguments: &[TypedValue],
    columns: &[(FormalStageCoordinate<'_, '_>, usize)],
    rows: usize,
) -> Result<Vec<f64>, StructuralError> {
    let mut block = vec![0.; rows * columns.len()];
    for (column, &(coordinate, scalar)) in columns.iter().enumerate() {
        if !equation
            .inputs()
            .iter()
            .any(|&input| variable(input) == Some(coordinate.value().index()))
        {
            continue;
        }
        let directions = direction_arguments(equation, arguments, coordinate, scalar)?;
        let outputs =
            eval_pure_call_directional(programs.table(), equation.site().owner(), &directions)
                .map_err(failure)?;
        let tangents = outputs
            .into_iter()
            .take(2 * equation.residual_outputs())
            .skip(1)
            .step_by(2)
            .collect::<Vec<_>>();
        for (row, value) in real_outputs(&tangents)?.into_iter().enumerate() {
            block[row * columns.len() + column] = value;
        }
    }
    Ok(block)
}

fn direction_arguments(
    equation: &FormalResidualProgram<'_, '_>,
    arguments: &[TypedValue],
    coordinate: FormalStageCoordinate<'_, '_>,
    scalar: usize,
) -> Result<Vec<TypedValue>, StructuralError> {
    let mut result = Vec::new();
    for (&input, value) in equation.inputs().iter().zip(arguments) {
        result.push(value.clone());
        if matches!(
            value.value_type().element_type(),
            solve::SolveScalarType::Real { .. }
        ) {
            let mut seed = vec![0.; value.elements().len()];
            if variable(input) == Some(coordinate.value().index()) {
                seed[scalar] = 1.;
            }
            result.push(typed(value.value_type(), &seed)?);
        }
    }
    Ok(result)
}

fn variable(coordinate: dae::CoordinateView<'_>) -> Option<u32> {
    match coordinate {
        dae::CoordinateView::Algebraic(v) => Some(v.index()),
        dae::CoordinateView::Parameter(v) => Some(v.index()),
        dae::CoordinateView::Input(v) => Some(v.index()),
        dae::CoordinateView::DiscreteReal(v) => Some(v.index()),
        dae::CoordinateView::DiscreteValue(v) => Some(v.index()),
        _ => None,
    }
}

fn typed(kind: &solve::SolveValueType, values: &[f64]) -> Result<TypedValue, StructuralError> {
    let elements = values
        .iter()
        .map(|&value| {
            if !value.is_finite() {
                return Err(failure("non-finite trial value"));
            }
            Ok(match kind.element_type() {
                solve::SolveScalarType::Real { .. } => {
                    solve::SolveValueKind::Real64(value.to_bits())
                }
                solve::SolveScalarType::Integer(_)
                    if value == value.trunc()
                        && value >= i64::MIN as f64
                        && value < -(i64::MIN as f64) =>
                {
                    solve::SolveValueKind::Integer(value as i64)
                }
                solve::SolveScalarType::Boolean if value == 0. || value == 1. => {
                    solve::SolveValueKind::Boolean(value == 1.)
                }
                _ => return Err(failure("trial value does not preserve its scalar type")),
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    TypedValue::construct(kind.clone(), elements).map_err(failure)
}

fn real_outputs(values: &[TypedValue]) -> Result<Vec<f64>, StructuralError> {
    values
        .iter()
        .flat_map(|v| v.elements())
        .map(|v| match v {
            solve::SolveValueKind::Real64(v) if f64::from_bits(*v).is_finite() => {
                Ok(f64::from_bits(*v))
            }
            _ => Err(failure("non-finite or non-Real stage residual")),
        })
        .collect()
}

fn check_assertions(values: &[TypedValue]) -> Result<(), StructuralError> {
    for value in values {
        if value.elements() != [solve::SolveValueKind::Boolean(true)] {
            return Err(failure("trial point violates a source call assertion"));
        }
    }
    Ok(())
}
