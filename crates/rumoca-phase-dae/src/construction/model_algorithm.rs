use super::*;

pub(super) struct ModelAlgorithmLowering<'lower, 'shape, 'dae> {
    pub(super) construction: &'lower mut dae::DaeConstruction<'dae>,
    pub(super) discrete_values: &'lower mut DiscreteValueStaging<'dae>,
    pub(super) discrete_owner: Option<DiscreteValueOwnerHandle>,
    pub(super) coordinates: &'lower HashMap<VarName, Coordinate<'dae>>,
    pub(super) functions: &'lower FunctionRegistry<'shape, 'dae>,
}

#[derive(Clone, Copy)]
struct DeclarativeSymbols<'scope, 'shape, 'dae> {
    coordinates: &'scope HashMap<VarName, Coordinate<'dae>>,
    functions: &'scope FunctionRegistry<'shape, 'dae>,
    target: &'scope VarName,
}

pub(super) fn lower_declarative_model_algorithm<'dae>(
    lowering: &mut ModelAlgorithmLowering<'_, '_, 'dae>,
    algorithm: &flat::Algorithm,
    target: &VarName,
) -> Result<(), dae::DaeConstructionError> {
    let mut values = HashMap::new();
    lower_declarative_statements(
        lowering.construction,
        DeclarativeSymbols {
            coordinates: lowering.coordinates,
            functions: lowering.functions,
            target,
        },
        &algorithm.statements,
        &mut values,
    )?;
    let value = values[target];
    finish_model_algorithm_value(lowering, algorithm, target, value)
}

pub(super) fn lower_total_array_model_algorithm<'dae>(
    lowering: &mut ModelAlgorithmLowering<'_, '_, 'dae>,
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
    let (owner, domain_id, binders) = lower_model_algorithm_domain(
        lowering.construction,
        algorithm.span,
        indices,
        domain,
        binder_spans,
    )?;
    let body = lower_expression_scoped(
        lowering.construction,
        LoweringSymbols {
            coordinates: lowering.coordinates,
            functions: lowering.functions,
            shapes: lowering.functions.shapes.model_values(),
            function_body: None,
            values: None,
            owner_clock: None,
        },
        &binders,
        value,
        None,
    )?;
    let array = lowering
        .construction
        .expressions(|expressions| expressions.at(owner).comprehension(domain_id, body))?;
    finish_model_algorithm_value(lowering, algorithm, target, array)
}

pub(super) fn lower_separated_array_sum_model_algorithm<'dae>(
    lowering: &mut ModelAlgorithmLowering<'_, '_, 'dae>,
    algorithm: &flat::Algorithm,
    array_target: &VarName,
    scalar_target: &VarName,
    domain: &StructuredIndexDomain,
    binder_spans: &[Span],
) -> Result<(), dae::DaeConstructionError> {
    let [
        rumoca_core::Statement::Assignment { value: initial, .. },
        rumoca_core::Statement::For {
            indices, equations, ..
        },
    ] = algorithm.statements.as_slice()
    else {
        unreachable!("analysis proves a separated array-reduction sequence")
    };
    let [
        rumoca_core::Statement::Assignment { value: element, .. },
        rumoca_core::Statement::Assignment { value: update, .. },
    ] = equations.as_slice()
    else {
        unreachable!("analysis proves an element definition followed by an additive update")
    };
    let Expression::Binary {
        rhs: contribution, ..
    } = update
    else {
        unreachable!("analysis proves an additive scalar update")
    };
    let (owner, domain_id, binders) = lower_model_algorithm_domain(
        lowering.construction,
        algorithm.span,
        indices,
        domain,
        binder_spans,
    )?;
    let symbols = LoweringSymbols {
        coordinates: lowering.coordinates,
        functions: lowering.functions,
        shapes: lowering.functions.shapes.model_values(),
        function_body: None,
        values: None,
        owner_clock: None,
    };
    let element = lower_expression_scoped(lowering.construction, symbols, &binders, element, None)?;
    let array = lowering
        .construction
        .expressions(|expressions| expressions.at(owner).comprehension(domain_id, element))?;

    let mut values = HashMap::new();
    values.insert(array_target.clone(), array);
    let contribution = lower_expression_scoped(
        lowering.construction,
        LoweringSymbols {
            values: Some(&values),
            ..symbols
        },
        &binders,
        contribution,
        None,
    )?;
    let contributions = lowering
        .construction
        .expressions(|expressions| expressions.at(owner).comprehension(domain_id, contribution))?;
    let reduction_span = update
        .span()
        .expect("analysis proves additive update provenance");
    let reduction =
        dae::DaeProvenance::generated(dae::DaeGeneration::AlgorithmEquation, reduction_span)?;
    let sum = lowering.construction.expressions(|expressions| {
        expressions
            .at(reduction)
            .builtin(dae::PureBuiltin::Sum, [contributions])
    })?;
    let initial = lower_expression_scoped(
        lowering.construction,
        symbols,
        &HashMap::new(),
        initial,
        None,
    )?;
    let scalar = lowering.construction.expressions(|expressions| {
        expressions
            .at(reduction)
            .binary(dae::BinaryOperator::Add, initial, sum)
    })?;

    finish_model_algorithm_value(lowering, algorithm, array_target, array)?;
    finish_model_algorithm_value(lowering, algorithm, scalar_target, scalar)
}

fn lower_model_algorithm_domain<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    owner_span: Span,
    indices: &[rumoca_core::ForIndex],
    domain: &StructuredIndexDomain,
    binder_spans: &[Span],
) -> Result<
    (
        dae::DaeProvenance,
        dae::DomainId<'dae>,
        HashMap<VarName, dae::DomainBinderId<'dae>>,
    ),
    dae::DaeConstructionError,
> {
    let owner = dae::DaeProvenance::generated(dae::DaeGeneration::AlgorithmEquation, owner_span)?;
    let domain_provenance = match binder_spans {
        [span] => dae::DaeProvenance::source(*span)?,
        _ => owner,
    };
    let domain_id =
        construction.domains(|domains| domains.structured(domain.clone(), domain_provenance))?;
    let indices = indices.iter().collect::<Vec<_>>();
    let binders = lower_function_binders(construction, domain_id, &indices, binder_spans)?;
    Ok((owner, domain_id, binders))
}

fn finish_model_algorithm_value<'dae>(
    lowering: &mut ModelAlgorithmLowering<'_, '_, 'dae>,
    algorithm: &flat::Algorithm,
    target: &VarName,
    value: dae::ExprId<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let owner =
        dae::DaeProvenance::generated(dae::DaeGeneration::AlgorithmEquation, algorithm.span)?;
    let generated = owner;
    match lowering.coordinates[target] {
        Coordinate::Algebraic(target) => {
            let lhs = lowering.construction.expressions(|expressions| {
                expressions
                    .at(generated)
                    .coordinate(dae::CoordinateInput::Algebraic(target))
            })?;
            let residual = generated_residual(lowering.construction, owner, lhs, value)?;
            lowering
                .construction
                .continuous(|continuous| continuous.value_equation(owner, residual))
        }
        Coordinate::DiscreteReal(target) => {
            let lhs = lowering.construction.expressions(|expressions| {
                expressions
                    .at(generated)
                    .coordinate(dae::CoordinateInput::DiscreteReal(target))
            })?;
            let residual = generated_residual(lowering.construction, owner, lhs, value)?;
            lowering.construction.discrete(|discrete| {
                discrete.real_equation(owner, |equation| equation.residual(residual))
            })?;
            Ok(())
        }
        Coordinate::DiscreteValue(target) => lowering.discrete_values.always(
            lowering
                .discrete_owner
                .expect("a discrete model-algorithm output has one semantic owner"),
            target,
            value,
            owner,
            owner,
        ),
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
    symbols: DeclarativeSymbols<'_, '_, 'dae>,
    statements: &[rumoca_core::Statement],
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { value, .. } => {
                let value = lower_model_algorithm_expression(
                    construction,
                    symbols.coordinates,
                    symbols.functions,
                    values,
                    value,
                )?;
                values.insert(symbols.target.clone(), value);
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            } => lower_declarative_conditional(
                construction,
                symbols,
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

fn lower_declarative_conditional<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: DeclarativeSymbols<'_, '_, 'dae>,
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
            symbols.coordinates,
            symbols.functions,
            &entry,
            &block.cond,
        )?;
        let mut branch_values = entry.clone();
        lower_declarative_statements(construction, symbols, &block.stmts, &mut branch_values)?;
        branches.push((condition, branch_values[symbols.target]));
    }
    let mut fallback_values = entry;
    if let Some(statements) = fallback {
        lower_declarative_statements(construction, symbols, statements, &mut fallback_values)?;
    }
    let fallback = fallback_values[symbols.target];
    let provenance = dae::DaeProvenance::generated(dae::DaeGeneration::AlgorithmEquation, span)?;
    let value = construction
        .expressions(|expressions| expressions.at(provenance).conditional(branches, fallback))?;
    values.insert(symbols.target.clone(), value);
    Ok(())
}
