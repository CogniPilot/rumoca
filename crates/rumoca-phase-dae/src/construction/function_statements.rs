//! Lowering of function algorithm sections: statements, conditionals,
//! multi-output calls, assignments, and loops into checked DAE function
//! bodies.

use super::*;

#[derive(Clone, Copy)]
pub(in crate::construction) struct FunctionSymbols<'symbols, 'dae> {
    pub(in crate::construction) coordinates: &'symbols HashMap<VarName, Coordinate<'dae>>,
    pub(in crate::construction) functions: &'symbols FunctionRegistry<'symbols, 'dae>,
    pub(in crate::construction) shapes: &'symbols ShapeEnvironment,
}

/// The MLS §12.4.4 definedness plan of a function's top-level sequence and
/// the predicates its conditionals have produced so far.
pub(in crate::construction) struct TopLevelDefinedness<'plan, 'dae> {
    pub(in crate::construction) plan: &'plan FunctionDefinednessPlan,
    pub(in crate::construction) predicates: DefinednessPredicates<'dae>,
    /// The function declaration, which owns the return-point assertions.
    pub(in crate::construction) span: Span,
}

pub(in crate::construction) fn lower_function_statements<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    mut definedness: Option<&mut TopLevelDefinedness<'_, 'dae>>,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    debug_assert_eq!(statements.len(), plans.len());
    let mut index = 0usize;
    while index < statements.len() {
        let statement = &statements[index];
        let plan = &plans[index];
        if let Some(definedness) = definedness.as_deref_mut()
            && let Some(names) = definedness.plan.asserted_reads.get(&index)
        {
            let span = statement.source_span().unwrap_or(definedness.span);
            definedness
                .predicates
                .assert_defined(construction, &mut body, names, span)?;
        }
        if let (Some(definedness), FunctionStatementPlan::If { .. }) =
            (definedness.as_deref_mut(), plan)
            && let Some(partial) = definedness.plan.partial_joins.get(&index)
        {
            let span = statement.source_span().unwrap_or(definedness.span);
            let partial = definedness
                .predicates
                .partial_targets(construction, partial, span)?;
            let defined = lower_top_level_conditional(
                construction,
                symbols,
                &mut body,
                statement,
                plan,
                &partial,
            )?;
            definedness.predicates.record(defined);
            index += 1;
            continue;
        }
        if let FunctionStatementPlan::ArrayAssembly(assembly) = plan {
            let statement_count = assembly.direct_count + usize::from(assembly.loop_plan.is_some());
            lower_function_array_assembly(
                construction,
                symbols,
                &mut body,
                &statements[index..index + statement_count],
                assembly,
            )?;
            index += statement_count;
            continue;
        }
        if let FunctionStatementPlan::RecordAssembly(assembly) = plan {
            lower_function_record_assembly(
                construction,
                symbols,
                &mut body,
                &statements[index..index + assembly.statement_count],
                assembly,
            )?;
            index += assembly.statement_count;
            continue;
        }
        if let FunctionStatementPlan::RecordFieldAssembly(assembly) = plan {
            lower_function_record_field_assembly(
                construction,
                symbols,
                &mut body,
                &statements[index..index + assembly.statement_count],
                assembly,
            )?;
            index += assembly.statement_count;
            continue;
        }
        body = lower_function_statement(construction, symbols, body, statement, plan)?;
        index += 1;
    }
    Ok(body)
}

pub(in crate::construction) fn lower_function_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    match (statement, plan) {
        (_, FunctionStatementPlan::ProvenAssertion) => Ok(body),
        (statement, FunctionStatementPlan::RuntimeAssertion) => {
            lower_runtime_function_assertion(construction, symbols, body, statement)
        }
        (
            _,
            FunctionStatementPlan::GeneratedBooleanAssignment {
                target,
                value,
                span,
                ..
            },
        ) => lower_generated_boolean_assignment(construction, symbols, body, target, value, *span),
        (
            rumoca_core::Statement::Assignment { value, span, .. },
            FunctionStatementPlan::Assignment(plan),
        ) => {
            lower_function_assignment(
                construction,
                symbols,
                &mut body,
                FunctionAssignment {
                    value,
                    span: *span,
                    plan,
                },
            )?;
            Ok(body)
        }
        (
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            },
            FunctionStatementPlan::For {
                domain,
                binder_spans,
                lowering,
                statements,
                source_depth,
            },
        ) => lower_function_loop(
            construction,
            symbols,
            body,
            FunctionLoop {
                indices,
                source_statements: equations,
                span: *span,
                domain,
                binder_spans,
                lowering,
                plans: statements,
                source_depth: *source_depth,
            },
        ),
        (
            statement @ rumoca_core::Statement::If { .. },
            FunctionStatementPlan::If { .. } | FunctionStatementPlan::ProvenBranch { .. },
        ) => lower_function_conditional_statement(construction, symbols, body, statement, plan),
        (
            rumoca_core::Statement::FunctionCall {
                comp, args, span, ..
            },
            FunctionStatementPlan::MultiOutputCall { outputs },
        ) => lower_multi_output_statement(construction, symbols, body, comp, args, *span, outputs),
        (
            rumoca_core::Statement::FunctionCall {
                comp, args, span, ..
            },
            FunctionStatementPlan::RecordMultiOutputAssembly(plan),
        ) => lower_record_multi_output_statement(
            construction,
            symbols,
            body,
            comp,
            args,
            *span,
            plan,
        ),
        (_, FunctionStatementPlan::ArrayAssemblyMember) => {
            unreachable!("array assembly members are consumed by their leading owner")
        }
        (_, FunctionStatementPlan::RecordAssemblyMember) => {
            unreachable!("record assembly members are consumed by their leading owner")
        }
        (_, FunctionStatementPlan::RecordFieldAssemblyMember) => {
            unreachable!("record field members are consumed by their staged owner")
        }
        (_, FunctionStatementPlan::RecordFieldAssembly(_)) => {
            unreachable!("record field assemblies lower with their source run")
        }
        _ => unreachable!("function analysis and construction plans remain aligned"),
    }
}

pub(in crate::construction) fn lower_multi_output_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    callee: &rumoca_core::Reference,
    args: &[Expression],
    span: Span,
    outputs: &[Option<FunctionAssignmentPlan>],
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let call = FunctionMultiOutputCall {
        callee,
        args,
        span,
        outputs,
    };
    lower_function_multi_output_call(construction, symbols, &mut body, call)?;
    Ok(body)
}

pub(in crate::construction) fn lower_runtime_function_assertion<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    statement: &rumoca_core::Statement,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let assertion = function_assertion(statement, symbols.functions.flat)
        .expect("analysis already validates the assertion statement")
        .expect("a runtime assertion plan owns an assertion statement");
    let condition = lower_function_expression(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        &body,
        assertion.condition,
    )?;
    let message = lower_function_expression(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        &body,
        assertion.message,
    )?;
    let provenance = dae::DaeProvenance::source(assertion.span)?;
    construction.functions(|functions| {
        functions.assertion_with_level(&mut body, condition, message, assertion.level, provenance)
    })?;
    Ok(body)
}

pub(in crate::construction) fn lower_record_multi_output_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    callee: &rumoca_core::Reference,
    arguments: &[Expression],
    span: Span,
    plan: &FunctionRecordCallAssemblyPlan,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    lower_function_record_multi_output_assembly(
        construction,
        symbols,
        &mut body,
        callee,
        arguments,
        span,
        plan,
    )?;
    Ok(body)
}

/// Lower one MLS §11.5 conditional statement of a function body.
///
/// The conditional reaches the DAE either as its own branches, or (when
/// analysis settled every condition this specialization evaluates) as the
/// unconditional sequence the executed branch denotes, in which case no
/// condition reaches the DAE at all.
pub(in crate::construction) fn lower_function_conditional_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let rumoca_core::Statement::If {
        cond_blocks,
        else_block,
        ..
    } = statement
    else {
        unreachable!("a conditional plan owns a conditional statement")
    };
    match plan {
        FunctionStatementPlan::If { .. } => {
            lower_top_level_conditional(construction, symbols, &mut body, statement, plan, &[])?;
            Ok(body)
        }
        FunctionStatementPlan::ProvenBranch {
            selected,
            statements,
        } => {
            let selected =
                selected_conditional_statements(cond_blocks, else_block.as_deref(), *selected);
            lower_function_statements(construction, symbols, body, selected, statements, None)
        }
        _ => unreachable!("function analysis and construction plans remain aligned"),
    }
}

/// Lower one runtime conditional of a sequence, returning the definedness
/// predicate of each target in `partial`.
pub(in crate::construction) fn lower_top_level_conditional<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
    partial: &[PartialTarget<'dae>],
) -> Result<Vec<(VarName, dae::ExprId<'dae>)>, dae::DaeConstructionError> {
    let (
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span,
        },
        FunctionStatementPlan::If {
            branches,
            fallback,
            targets,
        },
    ) = (statement, plan)
    else {
        unreachable!("a runtime conditional plan owns a conditional statement")
    };
    let binders = HashMap::new();
    lower_function_conditional(
        construction,
        body,
        FunctionConditional {
            symbols,
            binders: &binders,
            blocks: cond_blocks,
            fallback: else_block.as_deref(),
            branch_plans: branches,
            fallback_plans: fallback.as_deref(),
            targets,
            partial,
            span: *span,
        },
    )
}

pub(in crate::construction) struct FunctionAssignment<'statement> {
    pub(in crate::construction) value: &'statement Expression,
    pub(in crate::construction) span: Span,
    pub(in crate::construction) plan: &'statement FunctionAssignmentPlan,
}

pub(in crate::construction) struct FunctionMultiOutputCall<'statement> {
    pub(in crate::construction) callee: &'statement rumoca_core::Reference,
    pub(in crate::construction) args: &'statement [Expression],
    pub(in crate::construction) span: Span,
    pub(in crate::construction) outputs: &'statement [Option<FunctionAssignmentPlan>],
}

/// Lower one MLS §11.2.1.1 multi-result call statement.
///
/// The call's arguments are lowered once and every received result is committed
/// as one atomic assignment group. The checked group retains the MLS §12.4.3
/// proof that all result projections belong to one call evaluation; a backend
/// may therefore materialize the call once without inferring atomicity from
/// spans or expression identity.
pub(in crate::construction) fn lower_function_multi_output_call<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    call: FunctionMultiOutputCall<'_>,
) -> Result<(), dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(call.span)?;
    let binders = HashMap::new();
    let operands = lower_call_operands(
        construction,
        LoweringSymbols {
            coordinates: symbols.coordinates,
            functions: symbols.functions,
            shapes: symbols.shapes,
            function_body: Some(body),
            values: None,
            owner_clock: None,
        },
        &binders,
        call.callee,
        call.args,
        provenance,
    )?;
    let selected = call
        .outputs
        .iter()
        .enumerate()
        .filter_map(|(ordinal, plan)| plan.as_ref().map(|plan| (ordinal, plan)))
        .collect::<Vec<_>>();
    let results = operands.results(
        construction,
        selected.iter().map(|(ordinal, _)| *ordinal),
        provenance,
    )?;
    let mut assignments = Vec::with_capacity(selected.len());
    for ((_, plan), mut value) in selected.into_iter().zip(results) {
        let target = function_value_coordinate(symbols.coordinates, plan.target());
        if !plan.subscripts().is_empty() {
            let base = plan
                .seed()
                .map(|seed| lower_function_value_seed(construction, seed, call.span))
                .transpose()?;
            value = lower_function_array_update(
                construction,
                FunctionArrayUpdate {
                    symbols: LoweringSymbols {
                        coordinates: symbols.coordinates,
                        functions: symbols.functions,
                        shapes: symbols.shapes,
                        function_body: Some(body),
                        values: None,
                        owner_clock: None,
                    },
                    binders: &binders,
                    base,
                    target,
                    subscripts: plan.subscripts(),
                    value,
                    provenance,
                },
            )?;
        }
        assignments.push((target, value));
    }
    construction.functions(|owner| owner.assign_all(body, &assignments, provenance))
}

pub(in crate::construction) fn lower_function_record_multi_output_assembly<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    callee: &rumoca_core::Reference,
    args: &[Expression],
    span: Span,
    plan: &FunctionRecordCallAssemblyPlan,
) -> Result<(), dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(span)?;
    let operands = lower_call_operands(
        construction,
        LoweringSymbols {
            coordinates: symbols.coordinates,
            functions: symbols.functions,
            shapes: symbols.shapes,
            function_body: Some(body),
            values: None,
            owner_clock: None,
        },
        &HashMap::new(),
        callee,
        args,
        provenance,
    )?;
    let fields = plan
        .fields
        .iter()
        .map(|field| operands.result(construction, field.result_ordinal, provenance))
        .collect::<Result<Vec<_>, _>>()?;
    let target = function_value_coordinate(symbols.coordinates, &plan.target);
    let value_type =
        construction.functions(|functions| functions.value_type(target, provenance))?;
    construction.types(|types| {
        types.expect_record_layout(
            value_type,
            plan.fields.iter().map(|field| field.name.clone()),
            provenance,
        )
    })?;
    let record = construction
        .expressions(|expressions| expressions.at(provenance).record(value_type, fields))?;
    construction.functions(|functions| functions.assign(body, target, record, provenance))
}

pub(in crate::construction) fn lower_function_assignment<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    assignment: FunctionAssignment<'_>,
) -> Result<(), dae::DaeConstructionError> {
    let target = function_value_coordinate(symbols.coordinates, assignment.plan.target());
    let mut value = lower_function_expression(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        body,
        assignment.value,
    )?;
    let provenance = dae::DaeProvenance::source(assignment.span)?;
    let subscripts = assignment.plan.subscripts();
    if !subscripts.is_empty() {
        let binders = HashMap::new();
        let mut base = None;
        if let Some(seed) = assignment.plan.seed() {
            let seeded = lower_function_value_seed(construction, seed, assignment.span)?;
            base = Some(seeded);
        }
        value = lower_function_array_update(
            construction,
            FunctionArrayUpdate {
                symbols: LoweringSymbols {
                    coordinates: symbols.coordinates,
                    functions: symbols.functions,
                    shapes: symbols.shapes,
                    function_body: Some(body),
                    values: None,
                    owner_clock: None,
                },
                binders: &binders,
                base,
                target,
                subscripts,
                value,
                provenance,
            },
        )?;
    }
    construction.functions(|owner| owner.assign(body, target, value, provenance))
}

pub(in crate::construction) struct FunctionLoop<'statement> {
    pub(in crate::construction) indices: &'statement [rumoca_core::ForIndex],
    pub(in crate::construction) source_statements: &'statement [rumoca_core::Statement],
    pub(in crate::construction) span: Span,
    pub(in crate::construction) domain: &'statement StructuredIndexDomain,
    pub(in crate::construction) binder_spans: &'statement [Span],
    pub(in crate::construction) lowering: &'statement FunctionLoopLowering,
    pub(in crate::construction) plans: &'statement [FunctionStatementPlan],
    pub(in crate::construction) source_depth: usize,
}

pub(in crate::construction) fn lower_function_loop<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    input: FunctionLoop<'_>,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let owner = dae::DaeProvenance::source(input.span)?;
    let domain_provenance = match input.binder_spans {
        [span] => dae::DaeProvenance::source(*span)?,
        _ => owner,
    };
    let domain = construction
        .domains(|domains| domains.structured(input.domain.clone(), domain_provenance))?;
    let (indices, statements) =
        flattened_function_loop_source(input.indices, input.source_statements, input.source_depth);
    let binders = lower_function_binders(construction, domain, &indices, input.binder_spans)?;
    let mut loop_shapes = symbols.shapes.clone();
    for binder in binders.keys() {
        // A loop binder is a scalar whose value varies over the iteration, so
        // it shadows any enclosing coordinate's proven value (MLS §11.2.2).
        loop_shapes.insert(binder.clone(), Vec::new());
    }
    let loop_symbols = FunctionSymbols {
        coordinates: symbols.coordinates,
        functions: symbols.functions,
        shapes: &loop_shapes,
    };
    match input.lowering {
        FunctionLoopLowering::TotalArrayDefinition => {
            body = lower_total_function_array_definition(
                construction,
                body,
                TotalArrayDefinition {
                    symbols: loop_symbols,
                    domain,
                    binders: &binders,
                    statements,
                    plans: input.plans,
                    owner,
                },
            )?;
            Ok(body)
        }
        FunctionLoopLowering::Fold {
            targets,
            iteration_locals,
        } => lower_function_fold(
            construction,
            loop_symbols,
            body,
            FunctionFold {
                domain,
                binders: &binders,
                statements,
                plans: input.plans,
                targets,
                iteration_locals,
                owner,
            },
        ),
    }
}

pub(in crate::construction) fn lower_function_binders<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    domain: dae::DomainId<'dae>,
    indices: &[&rumoca_core::ForIndex],
    spans: &[Span],
) -> Result<HashMap<VarName, dae::DomainBinderId<'dae>>, dae::DaeConstructionError> {
    let mut binders = HashMap::with_capacity(indices.len());
    for (ordinal, (index, span)) in indices.iter().zip(spans).enumerate() {
        let provenance = dae::DaeProvenance::source(*span)?;
        let binder = construction.domains(|domains| domains.binder(domain, ordinal, provenance))?;
        binders.insert(VarName::new(&index.ident), binder);
    }
    Ok(binders)
}
