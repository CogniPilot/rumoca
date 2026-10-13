use super::*;

pub(in crate::construction) fn lower_ordered_function_conditional<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let (
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span,
        },
        FunctionStatementPlan::If {
            branches,
            fallback,
            condition_reads,
            ..
        },
    ) = (statement, plan)
    else {
        unreachable!("a checked runtime conditional plan owns its source statement")
    };
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionConditionLowering, *span)?;
    assert_eq!(cond_blocks.len(), branches.len());
    assert_eq!(cond_blocks.len(), condition_reads.len());
    let mut owners = Vec::with_capacity(cond_blocks.len());
    for ((block, plans), reads) in cond_blocks.iter().zip(branches).zip(condition_reads) {
        require_proven_function_reads(construction, symbols, &mut body, reads)?;
        let condition = lower_function_expression_scoped(
            construction,
            symbols.coordinates,
            symbols.functions,
            symbols.shapes,
            &body,
            symbols.binders,
            &block.cond,
        )?;
        let (child, finish) =
            construction.functions(|functions| functions.begin_branch(body, provenance))?;
        let child =
            lower_function_statements(construction, symbols, child, &block.stmts, plans, None)?;
        let (parent, owner) =
            construction.functions(|functions| functions.finish_branch(finish, child))?;
        body = parent;
        owners.push((condition, owner));
    }
    let (child, finish) =
        construction.functions(|functions| functions.begin_branch(body, provenance))?;
    let child = match (else_block.as_deref(), fallback.as_deref()) {
        (Some(source), Some(plans)) => {
            lower_function_statements(construction, symbols, child, source, plans, None)?
        }
        (None, None) => child,
        _ => unreachable!("conditional source and fallback plan have identical shape"),
    };
    let (mut body, fallback) =
        construction.functions(|functions| functions.finish_branch(finish, child))?;
    construction.functions(|functions| {
        functions.finish_conditional(&mut body, owners, fallback, provenance)
    })?;
    Ok(body)
}

pub(in crate::construction) fn require_current_function_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    name: &VarName,
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let target = function_value_coordinate(symbols.coordinates, name);
    match construction
        .functions(|functions| functions.current_definition_id(body, target, provenance))
    {
        Ok(_) => Ok(()),
        Err(dae::DaeConstructionError::IncompleteDefinition {
            kind: "function value",
            index,
            ..
        }) if index == target.ordinal() => {
            construction
                .functions(|functions| functions.require_assigned(body, target, provenance))?;
            Ok(())
        }
        Err(error) => Err(error),
    }
}

pub(in crate::construction) fn require_proven_function_reads<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    reads: &[(VarName, Span)],
) -> Result<(), dae::DaeConstructionError> {
    for (name, span) in reads {
        let provenance =
            dae::DaeProvenance::generated(dae::DaeGeneration::FunctionConditionLowering, *span)?;
        require_current_function_value(construction, symbols, body, name, provenance)?;
    }
    Ok(())
}

pub(in crate::construction) fn require_proven_loop_reads<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionLoop<'dae>,
    reads: &[(VarName, Span)],
) -> Result<(), dae::DaeConstructionError> {
    for (name, span) in reads {
        let target = function_value_coordinate(symbols.coordinates, name);
        let provenance =
            dae::DaeProvenance::generated(dae::DaeGeneration::FunctionConditionLowering, *span)?;
        match construction
            .functions(|functions| functions.current_definition_id(body.body(), target, provenance))
        {
            Ok(_) => {}
            Err(dae::DaeConstructionError::IncompleteDefinition {
                kind: "function value",
                index,
                ..
            }) if index == target.ordinal() => {
                construction.functions(|functions| {
                    functions.require_assigned_loop(body, target, provenance)
                })?;
            }
            Err(error) => return Err(error),
        }
    }
    Ok(())
}
