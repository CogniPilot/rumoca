use super::*;

pub(super) fn lower_declarative_model_algorithm<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    algorithm: &flat::Algorithm,
    target: &VarName,
) -> Result<(), dae::DaeConstructionError> {
    let mut values = HashMap::new();
    lower_declarative_statements(
        construction,
        coordinates,
        functions,
        target,
        &algorithm.statements,
        &mut values,
    )?;
    let value = values[target];
    finish_model_algorithm_value(construction, coordinates, algorithm, target, value)
}

pub(super) fn lower_total_array_model_algorithm<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    algorithm: &flat::Algorithm,
    target: &VarName,
    domain: &StructuredIndexDomain,
    binder_spans: &[Span],
) -> Result<(), dae::DaeConstructionError> {
    let [
        rumoca_core::Statement::For {
            indices, equations, ..
        },
    ] = algorithm.statements.as_slice()
    else {
        unreachable!("analysis proves a total array-definition loop")
    };
    let [rumoca_core::Statement::Assignment { value, .. }] = equations.as_slice() else {
        unreachable!("analysis proves one array element assignment")
    };
    let owner =
        dae::DaeProvenance::generated(dae::DaeGeneration::AlgorithmEquation, algorithm.span)?;
    let domain_provenance = match binder_spans {
        [span] => dae::DaeProvenance::source(*span)?,
        _ => owner,
    };
    let domain_id =
        construction.domains(|domains| domains.structured(domain.clone(), domain_provenance))?;
    let indices = indices.iter().collect::<Vec<_>>();
    let binders = lower_function_binders(construction, domain_id, &indices, binder_spans)?;
    let body = lower_expression_scoped(
        construction,
        LoweringSymbols {
            coordinates,
            functions,
            shapes: functions.shapes.model_values(),
            function_body: None,
            values: None,
        },
        &binders,
        value,
        None,
    )?;
    let array = construction
        .expressions(|expressions| expressions.at(owner).comprehension(domain_id, body))?;
    finish_model_algorithm_value(construction, coordinates, algorithm, target, array)
}

fn finish_model_algorithm_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    algorithm: &flat::Algorithm,
    target: &VarName,
    value: dae::ExprId<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let owner =
        dae::DaeProvenance::generated(dae::DaeGeneration::AlgorithmEquation, algorithm.span)?;
    let generated = owner;
    match coordinates[target] {
        Coordinate::Algebraic(target) => {
            let lhs = construction.expressions(|expressions| {
                expressions
                    .at(generated)
                    .coordinate(dae::CoordinateInput::Algebraic(target))
            })?;
            let residual = generated_residual(construction, owner, lhs, value)?;
            construction.continuous(|continuous| continuous.value_equation(owner, residual))
        }
        Coordinate::DiscreteReal(target) => {
            let lhs = construction.expressions(|expressions| {
                expressions
                    .at(generated)
                    .coordinate(dae::CoordinateInput::DiscreteReal(target))
            })?;
            let residual = generated_residual(construction, owner, lhs, value)?;
            construction.discrete(|discrete| {
                discrete.real_equation(owner, |equation| equation.residual(residual))
            })?;
            Ok(())
        }
        Coordinate::DiscreteValue(target) => construction
            .discrete(|discrete| discrete.assignment(owner, target, value))
            .map(|_| ()),
        Coordinate::Parameter(_)
        | Coordinate::Input(_)
        | Coordinate::State(_)
        | Coordinate::FunctionParameter(_)
        | Coordinate::FunctionValue(_) => {
            unreachable!("analysis accepts only declarative algorithm output coordinates")
        }
    }
}

fn lower_declarative_statements<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    target: &VarName,
    statements: &[rumoca_core::Statement],
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { value, .. } => {
                let value = lower_model_algorithm_expression(
                    construction,
                    coordinates,
                    functions,
                    values,
                    value,
                )?;
                values.insert(target.clone(), value);
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            } => lower_declarative_conditional(
                construction,
                coordinates,
                functions,
                target,
                cond_blocks,
                else_block.as_deref(),
                *span,
                values,
            )?,
            _ => unreachable!("analysis restricts declarative model algorithm statements"),
        }
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn lower_declarative_conditional<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    target: &VarName,
    blocks: &[rumoca_core::StatementBlock],
    fallback: Option<&[rumoca_core::Statement]>,
    span: Span,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    let entry = values.clone();
    let mut branches = Vec::with_capacity(blocks.len());
    for block in blocks {
        let condition = lower_model_algorithm_expression(
            construction,
            coordinates,
            functions,
            &entry,
            &block.cond,
        )?;
        let mut branch_values = entry.clone();
        lower_declarative_statements(
            construction,
            coordinates,
            functions,
            target,
            &block.stmts,
            &mut branch_values,
        )?;
        branches.push((condition, branch_values[target]));
    }
    let mut fallback_values = entry;
    if let Some(statements) = fallback {
        lower_declarative_statements(
            construction,
            coordinates,
            functions,
            target,
            statements,
            &mut fallback_values,
        )?;
    }
    let fallback = fallback_values[target];
    let provenance = dae::DaeProvenance::generated(dae::DaeGeneration::AlgorithmEquation, span)?;
    let value = construction
        .expressions(|expressions| expressions.at(provenance).conditional(branches, fallback))?;
    values.insert(target.clone(), value);
    Ok(())
}
