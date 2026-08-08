use super::*;

pub(super) fn lower_function_array_assembly<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    source: &[rumoca_core::Statement],
    plan: &FunctionArrayAssemblyPlan,
) -> Result<(), dae::DaeConstructionError> {
    let mut elements = source[..plan.direct_count]
        .iter()
        .map(|statement| {
            let rumoca_core::Statement::Assignment { value, .. } = statement else {
                unreachable!("analysis proves direct array-assembly assignments")
            };
            lower_function_expression(
                construction,
                symbols.coordinates,
                symbols.functions,
                symbols.shapes,
                body,
                value,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    if plan.loop_plan.is_some() {
        let loop_elements = lower_array_assembly_loop(
            construction,
            symbols,
            body,
            &source[plan.direct_count],
            plan,
        )?;
        elements.extend(loop_elements);
    }

    let owner_span = source[0]
        .source_span()
        .expect("analysis requires array-assembly source provenance");
    let owner =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionLoopLowering, owner_span)?;
    let array = construction.expressions(|expressions| expressions.at(owner).array(elements))?;
    let target = function_value_coordinate(symbols.coordinates, &plan.target);
    construction.functions(|functions| functions.assign(body, target, array, owner))
}

fn lower_array_assembly_loop<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    source: &rumoca_core::Statement,
    plan: &FunctionArrayAssemblyPlan,
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let (
        rumoca_core::Statement::For {
            indices,
            equations,
            span,
        },
        Some(FunctionStatementPlan::For {
            domain,
            binder_spans,
            statements,
            source_depth: 1,
            ..
        }),
    ) = (source, plan.loop_plan.as_deref())
    else {
        unreachable!("analysis proves a rank-one suffix loop")
    };
    let domain_owner = dae::DaeProvenance::source(*span)?;
    let domain_id =
        construction.domains(|domains| domains.structured(domain.clone(), domain_owner))?;
    let binders = lower_function_binders(construction, domain_id, &[&indices[0]], binder_spans)?;
    let [rumoca_core::Statement::Assignment { value, .. }] = equations.as_slice() else {
        unreachable!("analysis proves one suffix-loop assignment")
    };
    let [FunctionStatementPlan::Assignment(_)] = statements.as_slice() else {
        unreachable!("analysis proves one suffix-loop assignment plan")
    };
    let mut loop_shapes = symbols.shapes.clone();
    for binder in binders.keys() {
        // A loop binder is a scalar whose value varies over the iteration, so
        // it shadows any enclosing coordinate's proven value (MLS §11.2.2).
        loop_shapes.insert(binder.clone(), Vec::new());
    }
    let element = lower_function_expression_scoped(
        construction,
        symbols.coordinates,
        symbols.functions,
        &loop_shapes,
        body,
        &binders,
        value,
    )?;
    let generated = dae::DaeProvenance::generated(dae::DaeGeneration::FunctionLoopLowering, *span)?;
    let suffix = construction
        .expressions(|expressions| expressions.at(generated).comprehension(domain_id, element))?;
    let count = domain
        .scalar_count()
        .expect("analysis validates the suffix-loop domain");
    (0..count)
        .map(|point| {
            construction.expressions(|expressions| {
                let index = expressions
                    .at(generated)
                    .literal(dae::DaeLiteral::Integer((point + 1) as i64))?;
                expressions.at(generated).index(
                    suffix,
                    [dae::Subscript::Index {
                        expression: index,
                        provenance: generated,
                    }],
                )
            })
        })
        .collect()
}
