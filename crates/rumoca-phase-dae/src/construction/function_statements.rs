//! Lowering of function algorithm sections: statements, conditionals,
//! multi-output calls, assignments, and loops into checked DAE function
//! bodies.

use super::*;

pub(in crate::construction) fn require_function_values<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    names: &[VarName],
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionConditionLowering, span)?;
    for name in names {
        function_body::require_current_function_value(
            construction,
            symbols,
            body,
            name,
            provenance,
        )?;
    }
    Ok(())
}

#[derive(Clone, Copy)]
pub(in crate::construction) struct FunctionSymbols<'symbols, 'dae> {
    pub(in crate::construction) coordinates: &'symbols HashMap<VarName, Coordinate<'dae>>,
    pub(in crate::construction) functions: &'symbols FunctionRegistry<'symbols, 'dae>,
    pub(in crate::construction) shapes: &'symbols ShapeEnvironment,
    pub(in crate::construction) binders: &'symbols HashMap<VarName, dae::DomainBinderId<'dae>>,
    pub(in crate::construction) domain: Option<dae::DomainId<'dae>>,
}

/// The MLS §12.4.4 ordered use and return checks of a function sequence.
pub(in crate::construction) struct TopLevelDefinedness<'plan> {
    pub(in crate::construction) plan: &'plan FunctionDefinednessPlan,
    /// The function declaration, which owns the return-point assertions.
    pub(in crate::construction) span: Span,
}

pub(in crate::construction) fn lower_function_statements<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    mut definedness: Option<&mut TopLevelDefinedness<'_>>,
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
            require_function_values(construction, symbols, &mut body, names, span)?;
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
                proven_reads,
                selected,
                ..
            },
        ) => match selected {
            Some(selected) => lower_function_statement(
                construction,
                symbols,
                body,
                &selected.source,
                &selected.plan,
            ),
            None => lower_generated_boolean_assignment(
                construction,
                symbols,
                body,
                target,
                value,
                *span,
                proven_reads,
            ),
        },
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
            statement @ rumoca_core::Statement::FunctionCall { .. },
            plan @ (FunctionStatementPlan::MultiOutputCall { .. }
            | FunctionStatementPlan::RecordMultiOutputAssembly(_)),
        ) => lower_function_call_statement(construction, symbols, body, statement, plan),
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

fn lower_function_call_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: dae::FunctionBody<'dae>,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    match (statement, plan) {
        (
            rumoca_core::Statement::FunctionCall {
                comp, args, span, ..
            },
            FunctionStatementPlan::MultiOutputCall {
                outputs,
                argument_reads,
            },
        ) => lower_multi_output_statement(
            construction,
            symbols,
            body,
            comp,
            args,
            *span,
            (outputs, argument_reads),
        ),
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
        _ => unreachable!("a function-call plan owns its source call"),
    }
}

pub(in crate::construction) fn lower_multi_output_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    callee: &rumoca_core::Reference,
    args: &[Expression],
    span: Span,
    planned: (&[Option<FunctionAssignmentPlan>], &[(VarName, Span)]),
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let (outputs, argument_reads) = planned;
    let call = FunctionMultiOutputCall {
        callee,
        args,
        span,
        outputs,
        argument_reads,
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
    let condition = lower_function_expression_scoped(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        &body,
        symbols.binders,
        assertion.condition,
    )?;
    let message = lower_function_expression_scoped(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        &body,
        symbols.binders,
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
    body: dae::FunctionBody<'dae>,
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
            lower_ordered_function_conditional(construction, symbols, body, statement, plan)
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
    pub(in crate::construction) argument_reads: &'statement [(VarName, Span)],
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
    let binders = symbols.binders;
    function_body::require_proven_function_reads(construction, symbols, body, call.argument_reads)?;
    let operands = lower_call_operands(
        construction,
        LoweringSymbols {
            coordinates: symbols.coordinates,
            functions: symbols.functions,
            shapes: symbols.shapes,
            function_body: Some(body),
            values: None,
            owner_clock: None,
            source_comprehension_binders: None,
        },
        binders,
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
                        source_comprehension_binders: None,
                    },
                    binders,
                    base: None,
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
    function_body::require_proven_function_reads(
        construction,
        symbols,
        body,
        &plan.argument_reads,
    )?;
    let operands = lower_call_operands(
        construction,
        LoweringSymbols {
            coordinates: symbols.coordinates,
            functions: symbols.functions,
            shapes: symbols.shapes,
            function_body: Some(body),
            values: None,
            owner_clock: None,
            source_comprehension_binders: None,
        },
        symbols.binders,
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
    function_body::require_proven_function_reads(
        construction,
        symbols,
        body,
        assignment.plan.proven_reads(),
    )?;
    let mut value = lower_function_expression_scoped(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        body,
        symbols.binders,
        assignment.value,
    )?;
    let provenance = dae::DaeProvenance::source(assignment.span)?;
    let subscripts = assignment.plan.subscripts();
    if !subscripts.is_empty() {
        function_body::require_proven_function_reads(
            construction,
            symbols,
            body,
            assignment.plan.base_reads(),
        )?;
        let binders = symbols.binders;
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
                    source_comprehension_binders: None,
                },
                binders,
                base: None,
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
    if input.domain.scalar_count().is_ok_and(|count| count == 0) {
        return Ok(body);
    }
    let owner = dae::DaeProvenance::source(input.span)?;
    let domain_provenance = match input.binder_spans {
        [span] => dae::DaeProvenance::source(*span)?,
        _ => owner,
    };
    let domain = construction.domains(|domains| match symbols.domain {
        Some(parent) => domains.nested(parent, input.domain.clone(), domain_provenance),
        None => domains.structured(input.domain.clone(), domain_provenance),
    })?;
    let (indices, statements) =
        flattened_function_loop_source(input.indices, input.source_statements, input.source_depth);
    let mut binders = symbols.binders.clone();
    binders.extend(lower_function_binders(
        construction,
        domain,
        &indices,
        input.binder_spans,
    )?);
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
        binders: &binders,
        domain: Some(domain),
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
            entry_reads,
            selected_entry,
            entry_seeds,
        } => lower_function_fold(
            construction,
            loop_symbols,
            body,
            FunctionFold {
                domain,
                structured: input.domain,
                binders: &binders,
                statements,
                plans: input.plans,
                targets,
                iteration_locals,
                entry_reads,
                entry_seeds,
                selected_condition: selected_entry
                    .as_ref()
                    .map(|entry| analysis::selected_loop_entry(entry, input.span)),
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
