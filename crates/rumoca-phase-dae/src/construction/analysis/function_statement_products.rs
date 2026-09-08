use super::*;

pub(in crate::construction) enum FunctionPlan {
    Statements {
        statements: FunctionStatementSequence,
        generated_booleans: Vec<(VarName, Span)>,
        certified_output_seeds: Vec<(VarName, FunctionValueSeed)>,
    },
    GuardedReturn {
        conditions: Vec<Expression>,
        branches: Vec<FunctionStatementSequence>,
        tail: FunctionStatementSequence,
        targets: Vec<VarName>,
        span: Span,
    },
    IntegerReduction {
        initial: FunctionStatementSequence,
        result: VarName,
        reduction: Box<FunctionIntegerReduction>,
    },
    External(ExternalFunctionPlan),
}

pub(in crate::construction) struct FunctionStatementSequence {
    products: Box<[FunctionStatementProduct]>,
    span: Span,
}

pub(in crate::construction) struct FunctionStatementProduct {
    source: Box<[rumoca_core::Statement]>,
    plan: FunctionLoweringPlan,
    runtime_assertion: Option<FunctionRuntimeAssertion>,
    span: Span,
}

pub(in crate::construction) struct FunctionRuntimeAssertion {
    pub(in crate::construction) condition: Expression,
    pub(in crate::construction) message: Expression,
    pub(in crate::construction) span: Span,
}

pub(in crate::construction) struct FunctionArrayAssemblyPlan {
    pub(in crate::construction) target: VarName,
    pub(in crate::construction) target_def_id: rumoca_core::DefId,
    pub(in crate::construction) direct_count: usize,
    pub(in crate::construction) extent: usize,
    pub(in crate::construction) suffix: Option<FunctionArraySuffixProduct>,
    pub(in crate::construction) seed: Option<FunctionValueSeed>,
}

pub(in crate::construction) struct FunctionArraySuffixProduct {
    pub(in crate::construction) domain: StructuredIndexDomain,
    pub(in crate::construction) binder_span: Span,
    pub(in crate::construction) index: rumoca_core::ForIndex,
    pub(in crate::construction) value: Expression,
    pub(in crate::construction) span: Span,
}

struct ArraySuffixIssueRequest<'a> {
    expected_index: &'a rumoca_core::ForIndex,
    target: &'a VarName,
    target_def_id: rumoca_core::DefId,
    direct_count: usize,
    extent: usize,
    owner_span: Span,
}

#[derive(Clone, Copy)]
struct AssemblyRunRequest<'a> {
    count: usize,
    member: FunctionAssemblyMember,
    target: &'a VarName,
    target_def_id: rumoca_core::DefId,
    expected_fields: Option<&'a [AssemblyFieldIdentity]>,
    owner_span: Span,
}

impl FunctionStatementSequence {
    pub(in crate::construction) fn products(&self) -> &[FunctionStatementProduct] {
        &self.products
    }

    pub(in crate::construction) fn span(&self) -> Span {
        self.span
    }
}

impl FunctionStatementProduct {
    pub(in crate::construction) fn source(&self) -> &[rumoca_core::Statement] {
        &self.source
    }

    pub(in crate::construction) fn plan(&self) -> &FunctionLoweringPlan {
        &self.plan
    }

    pub(in crate::construction) fn runtime_assertion(&self) -> Option<&FunctionRuntimeAssertion> {
        self.runtime_assertion.as_ref()
    }

    pub(in crate::construction) fn span(&self) -> Span {
        self.span
    }
}

pub(in crate::construction) enum FunctionLoweringPlan {
    Assignment(FunctionAssignmentPlan),
    ProvenAssertion,
    RuntimeAssertion,
    GeneratedBooleanAssignment {
        target: VarName,
        value: Expression,
        span: Span,
    },
    For {
        domain: StructuredIndexDomain,
        binder_spans: Vec<Span>,
        lowering: FunctionLoopLowering,
        statements: FunctionStatementSequence,
        indices: Vec<rumoca_core::ForIndex>,
        span: Span,
    },
    If {
        conditions: Vec<Expression>,
        branches: Vec<FunctionStatementSequence>,
        fallback: Option<FunctionStatementSequence>,
        targets: Vec<FunctionConditionalTarget>,
        span: Span,
    },
    ProvenBranch {
        statements: FunctionStatementSequence,
    },
    MultiOutputCall {
        outputs: Vec<Option<FunctionAssignmentPlan>>,
    },
    RecordMultiOutputAssembly(FunctionRecordCallAssemblyPlan),
    ArrayAssembly(Box<FunctionArrayAssemblyPlan>),
    RecordAssembly(FunctionRecordAssemblyPlan),
    RecordFieldAssembly(FunctionRecordFieldAssemblyPlan),
}

pub(in crate::construction) enum FunctionIntegerReduction {
    WhileExclusive {
        bound: Expression,
        one: Expression,
        span: Span,
    },
    ForInclusiveCapped {
        one: Expression,
        end: Expression,
        cap: Expression,
        span: Span,
    },
}

pub(super) fn issue_function_statement_sequence(
    source: Vec<rumoca_core::Statement>,
    plans: Vec<FunctionStatementPlan>,
    owner_span: Span,
) -> Result<FunctionStatementSequence, ToDaeError> {
    let mut source = source.into_iter();
    let mut plans = plans.into_iter();
    let mut products = Vec::new();
    loop {
        match (source.next(), plans.next()) {
            (None, None) => break,
            (Some(statement), Some(plan)) => products.push(issue_function_statement_product(
                statement,
                plan,
                &mut source,
                &mut plans,
                owner_span,
            )?),
            (Some(statement), None) => {
                return Err(function_sequence_error(
                    "source statement has no issued lowering plan",
                    statement.source_span().unwrap_or(owner_span),
                ));
            }
            (None, Some(_)) => {
                return Err(function_sequence_error(
                    "lowering plan has no exact source statement",
                    owner_span,
                ));
            }
        }
    }
    Ok(FunctionStatementSequence {
        products: products.into_boxed_slice(),
        span: owner_span,
    })
}

fn issue_function_statement_product(
    statement: rumoca_core::Statement,
    plan: FunctionStatementPlan,
    source: &mut std::vec::IntoIter<rumoca_core::Statement>,
    plans: &mut std::vec::IntoIter<FunctionStatementPlan>,
    owner_span: Span,
) -> Result<FunctionStatementProduct, ToDaeError> {
    match plan {
        FunctionStatementPlan::For {
            domain,
            binder_spans,
            lowering,
            statements,
            source_depth,
        } => {
            let (indices, body, span) = issued_loop_source(&statement, source_depth, owner_span)?;
            leaf_product(
                statement,
                FunctionLoweringPlan::For {
                    domain,
                    binder_spans,
                    lowering,
                    statements: issue_function_statement_sequence(body, statements, span)?,
                    indices,
                    span,
                },
                owner_span,
            )
        }
        FunctionStatementPlan::If {
            branches,
            fallback,
            targets,
        } => {
            let plan =
                issue_conditional_product(&statement, branches, fallback, targets, owner_span)?;
            leaf_product(statement, plan, owner_span)
        }
        FunctionStatementPlan::ProvenBranch {
            selected,
            statements,
        } => {
            let plan = issue_proven_branch_product(&statement, selected, statements, owner_span)?;
            leaf_product(statement, plan, owner_span)
        }
        FunctionStatementPlan::ArrayAssembly(assembly) => {
            issue_array_assembly_product(statement, assembly, source, plans, owner_span)
        }
        FunctionStatementPlan::RecordAssembly(assembly) => {
            let count = assembly.statement_count;
            let fields = issue_record_assembly_member_fields(&assembly, owner_span)?;
            let target = assembly.target.clone();
            let target_def_id = assembly.target_def_id;
            aggregate_product(
                statement,
                FunctionLoweringPlan::RecordAssembly(assembly),
                source,
                plans,
                AssemblyRunRequest {
                    count,
                    member: FunctionAssemblyMember::Record,
                    target: &target,
                    target_def_id,
                    expected_fields: Some(&fields),
                    owner_span,
                },
            )
        }
        FunctionStatementPlan::RecordFieldAssembly(assembly) => {
            let count = assembly.statement_count;
            let fields = vec![
                AssemblyFieldIdentity {
                    name: assembly.field.name.clone(),
                    def_id: assembly.field.def_id,
                };
                count
            ];
            let target = assembly.target.clone();
            let target_def_id = assembly.target_def_id;
            aggregate_product(
                statement,
                FunctionLoweringPlan::RecordFieldAssembly(assembly),
                source,
                plans,
                AssemblyRunRequest {
                    count,
                    member: FunctionAssemblyMember::RecordField,
                    target: &target,
                    target_def_id,
                    expected_fields: Some(&fields),
                    owner_span,
                },
            )
        }
        leaf => issue_leaf_product(statement, leaf, owner_span),
    }
}

fn issue_leaf_product(
    statement: rumoca_core::Statement,
    plan: FunctionStatementPlan,
    owner_span: Span,
) -> Result<FunctionStatementProduct, ToDaeError> {
    let lowering = match plan {
        FunctionStatementPlan::Assignment(plan) => {
            require_assignment_source(&statement, &plan, owner_span)?;
            FunctionLoweringPlan::Assignment(plan)
        }
        FunctionStatementPlan::ProvenAssertion => {
            require_proven_assertion_source(&statement, owner_span)?;
            FunctionLoweringPlan::ProvenAssertion
        }
        FunctionStatementPlan::RuntimeAssertion => FunctionLoweringPlan::RuntimeAssertion,
        FunctionStatementPlan::GeneratedBooleanAssignment {
            target,
            value,
            span,
        } => {
            require_empty_source(&statement, owner_span)?;
            FunctionLoweringPlan::GeneratedBooleanAssignment {
                target,
                value,
                span,
            }
        }
        FunctionStatementPlan::MultiOutputCall { outputs } => {
            require_call_outputs(&statement, &outputs, owner_span)?;
            FunctionLoweringPlan::MultiOutputCall { outputs }
        }
        FunctionStatementPlan::RecordMultiOutputAssembly(plan) => {
            require_record_call_outputs(&statement, &plan, owner_span)?;
            FunctionLoweringPlan::RecordMultiOutputAssembly(plan)
        }
        FunctionStatementPlan::ArrayAssemblyMember
        | FunctionStatementPlan::RecordAssemblyMember
        | FunctionStatementPlan::RecordFieldAssemblyMember => {
            return Err(function_sequence_error(
                "aggregate member has no leading source owner",
                statement.source_span().unwrap_or(owner_span),
            ));
        }
        FunctionStatementPlan::For { .. }
        | FunctionStatementPlan::If { .. }
        | FunctionStatementPlan::ProvenBranch { .. }
        | FunctionStatementPlan::ArrayAssembly(_)
        | FunctionStatementPlan::RecordAssembly(_)
        | FunctionStatementPlan::RecordFieldAssembly(_) => {
            return Err(function_sequence_error(
                "structured lowering certificate bypassed its source issuer",
                statement.source_span().unwrap_or(owner_span),
            ));
        }
    };
    leaf_product(statement, lowering, owner_span)
}

fn issue_runtime_assertion(
    statement: &rumoca_core::Statement,
    owner_span: Span,
) -> Result<FunctionRuntimeAssertion, ToDaeError> {
    let (condition, message, span) = match statement {
        rumoca_core::Statement::Assert {
            condition,
            message,
            level: None,
            span,
        } => (condition, message.as_ref(), *span),
        rumoca_core::Statement::FunctionCall {
            comp,
            args,
            outputs,
            span,
            ..
        } if outputs.iter().all(Option::is_none) && is_predefined_assert_reference(comp) => {
            match args.as_slice() {
                [condition, message] => (condition, message, *span),
                _ => return Err(foreign_source_error(statement, owner_span)),
            }
        }
        _ => return Err(foreign_source_error(statement, owner_span)),
    };
    Ok(FunctionRuntimeAssertion {
        condition: condition.clone(),
        message: message.clone(),
        span,
    })
}

fn is_predefined_assert_reference(reference: &rumoca_core::Reference) -> bool {
    reference.resolved_function().is_none()
        && rumoca_core::runtime_flow_action_function_short_name(reference.as_str())
            == Some("assert")
}

fn require_proven_assertion_source(
    statement: &rumoca_core::Statement,
    owner_span: Span,
) -> Result<(), ToDaeError> {
    let is_assertion = match statement {
        rumoca_core::Statement::Assert { .. } => true,
        rumoca_core::Statement::FunctionCall {
            comp,
            args,
            outputs,
            ..
        } => {
            outputs.iter().all(Option::is_none)
                && matches!(args.as_slice(), [_, _] | [_, _, _])
                && is_predefined_assert_reference(comp)
        }
        _ => false,
    };
    if is_assertion {
        Ok(())
    } else {
        Err(foreign_source_error(statement, owner_span))
    }
}

fn leaf_product(
    statement: rumoca_core::Statement,
    plan: FunctionLoweringPlan,
    owner_span: Span,
) -> Result<FunctionStatementProduct, ToDaeError> {
    let span = statement.source_span().unwrap_or(owner_span);
    let runtime_assertion = matches!(plan, FunctionLoweringPlan::RuntimeAssertion)
        .then(|| issue_runtime_assertion(&statement, owner_span))
        .transpose()?;
    Ok(FunctionStatementProduct {
        source: vec![statement].into_boxed_slice(),
        plan,
        runtime_assertion,
        span,
    })
}

fn require_assignment_source(
    statement: &rumoca_core::Statement,
    plan: &FunctionAssignmentPlan,
    owner_span: Span,
) -> Result<(), ToDaeError> {
    let rumoca_core::Statement::Assignment { comp, .. } = statement else {
        return Err(foreign_source_error(statement, owner_span));
    };
    if assignment_target_matches(comp, plan.target(), plan.target_def_id()) {
        Ok(())
    } else {
        Err(foreign_source_error(statement, owner_span))
    }
}

fn require_call_outputs(
    statement: &rumoca_core::Statement,
    plans: &[Option<FunctionAssignmentPlan>],
    owner_span: Span,
) -> Result<(), ToDaeError> {
    let rumoca_core::Statement::FunctionCall { outputs, .. } = statement else {
        return Err(foreign_source_error(statement, owner_span));
    };
    let mut outputs = outputs.iter();
    let mut plans = plans.iter();
    loop {
        match (outputs.next(), plans.next()) {
            (None, None) => return Ok(()),
            (Some(None), Some(None)) => {}
            (Some(Some(source)), Some(Some(plan)))
                if assignment_target_matches(source, plan.target(), plan.target_def_id()) => {}
            _ => return Err(foreign_source_error(statement, owner_span)),
        }
    }
}

fn require_record_call_outputs(
    statement: &rumoca_core::Statement,
    plan: &FunctionRecordCallAssemblyPlan,
    owner_span: Span,
) -> Result<(), ToDaeError> {
    let rumoca_core::Statement::FunctionCall { outputs, .. } = statement else {
        return Err(foreign_source_error(statement, owner_span));
    };
    if outputs.len() != plan.fields.len() {
        return Err(foreign_source_error(statement, owner_span));
    }
    let mut claimed_ordinals = vec![false; outputs.len()];
    let mut claimed_fields = std::collections::HashSet::with_capacity(plan.fields.len());
    for field in &plan.fields {
        if !claimed_fields.insert(FunctionRecordFieldIdentity {
            target: plan.target_def_id,
            field: field.def_id,
        }) {
            return Err(foreign_source_error(statement, owner_span));
        }
        let Some(claimed) = claimed_ordinals.get_mut(field.result_ordinal) else {
            return Err(foreign_source_error(statement, owner_span));
        };
        if *claimed {
            return Err(foreign_source_error(statement, owner_span));
        }
        *claimed = true;
        let Some(Some(receiver)) = outputs.get(field.result_ordinal) else {
            return Err(foreign_source_error(statement, owner_span));
        };
        let [root, source_field] = receiver.parts() else {
            return Err(foreign_source_error(statement, owner_span));
        };
        if !root.subs.is_empty()
            || !source_field.subs.is_empty()
            || root.ident != plan.target.as_str()
            || root.def_id != plan.target_def_id
            || source_field.ident != field.name.as_str()
            || source_field.def_id != field.def_id
        {
            return Err(foreign_source_error(statement, owner_span));
        }
    }
    if claimed_ordinals.into_iter().all(|claimed| claimed) {
        Ok(())
    } else {
        Err(foreign_source_error(statement, owner_span))
    }
}

fn assignment_target_matches(
    source: &rumoca_core::ComponentReference,
    target: &VarName,
    target_def_id: rumoca_core::DefId,
) -> bool {
    &source.to_var_name() == target && source.root_def_id() == target_def_id
}

fn issue_array_assembly_product(
    first: rumoca_core::Statement,
    assembly: AnalyzedFunctionArrayAssemblyPlan,
    source: &mut std::vec::IntoIter<rumoca_core::Statement>,
    plans: &mut std::vec::IntoIter<FunctionStatementPlan>,
    owner_span: Span,
) -> Result<FunctionStatementProduct, ToDaeError> {
    let span = first.source_span().unwrap_or(owner_span);
    let AnalyzedFunctionArrayAssemblyPlan {
        target,
        target_def_id,
        direct_members,
        extent,
        suffix_index,
        loop_plan,
        seed,
    } = assembly;
    let direct_count = direct_members.len();
    let suffix_is_closed = match (&loop_plan, &suffix_index) {
        (None, None) => direct_count == extent,
        (Some(_), Some(_)) => direct_count < extent,
        _ => false,
    };
    if !suffix_is_closed {
        return Err(function_sequence_error(
            "array assembly does not prove its exact declared extent",
            span,
        ));
    }
    let mut issued_source = consume_array_assembly_members(
        first,
        &direct_members,
        source,
        plans,
        &target,
        target_def_id,
        owner_span,
    )?
    .into_vec();
    let suffix = match (loop_plan, suffix_index) {
        (Some(plan), Some(expected_index)) => {
            let Some(suffix_source) = source.next() else {
                return Err(function_sequence_error(
                    "array suffix source is missing",
                    owner_span,
                ));
            };
            let Some(marker) = plans.next() else {
                return Err(function_sequence_error(
                    "array suffix marker is missing",
                    suffix_source.source_span().unwrap_or(owner_span),
                ));
            };
            if !matches!(marker, FunctionStatementPlan::ArrayAssemblyMember) {
                return Err(function_sequence_error(
                    "array suffix has a foreign lowering plan",
                    suffix_source.source_span().unwrap_or(owner_span),
                ));
            }
            let product = issue_array_suffix_product(
                &suffix_source,
                *plan,
                ArraySuffixIssueRequest {
                    expected_index: &expected_index,
                    target: &target,
                    target_def_id,
                    direct_count,
                    extent,
                    owner_span,
                },
            )?;
            issued_source.push(suffix_source);
            Some(product)
        }
        (None, None) => None,
        _ => {
            return Err(function_sequence_error(
                "array suffix certificate is incomplete",
                span,
            ));
        }
    };
    Ok(FunctionStatementProduct {
        source: issued_source.into_boxed_slice(),
        plan: FunctionLoweringPlan::ArrayAssembly(Box::new(FunctionArrayAssemblyPlan {
            target,
            target_def_id,
            direct_count,
            extent,
            suffix,
            seed,
        })),
        runtime_assertion: None,
        span,
    })
}

fn issue_array_suffix_product(
    statement: &rumoca_core::Statement,
    plan: FunctionStatementPlan,
    request: ArraySuffixIssueRequest<'_>,
) -> Result<FunctionArraySuffixProduct, ToDaeError> {
    let ArraySuffixIssueRequest {
        expected_index,
        target,
        target_def_id,
        direct_count,
        extent,
        owner_span,
    } = request;
    let (
        rumoca_core::Statement::For {
            indices,
            equations,
            span,
        },
        FunctionStatementPlan::For {
            domain,
            binder_spans,
            statements,
            source_depth: 1,
            ..
        },
    ) = (statement, plan)
    else {
        return Err(function_sequence_error(
            "array suffix does not own one exact loop",
            statement.source_span().unwrap_or(owner_span),
        ));
    };
    let ([index], [assignment], [FunctionStatementPlan::Assignment(assignment_plan)]) = (
        indices.as_slice(),
        equations.as_slice(),
        statements.as_slice(),
    ) else {
        return Err(function_sequence_error(
            "array suffix does not own one exact assignment",
            *span,
        ));
    };
    if index != expected_index {
        return Err(function_sequence_error(
            "array suffix source range differs from its analyzed witness",
            *span,
        ));
    }
    require_assembly_source(
        assignment,
        FunctionAssemblyMember::ArraySuffix,
        target,
        target_def_id,
        None,
        *span,
    )?;
    if assignment_plan.target() != target
        || assignment_plan.target_def_id() != target_def_id
        || !matches!(
            assignment_plan.subscripts(),
            [subscript] if subscript_is_binder(subscript, &index.ident)
        )
        || domain.binders.len() != 1
    {
        return Err(foreign_source_error(assignment, *span));
    }
    let rumoca_core::Statement::Assignment { comp, value, .. } = assignment else {
        return Err(foreign_source_error(assignment, *span));
    };
    if !matches!(
        comp.parts(),
        [part] if matches!(
            part.subs.as_slice(),
            [subscript] if subscript_is_binder(subscript, &index.ident)
        )
    ) {
        return Err(foreign_source_error(assignment, *span));
    }
    require_exact_array_suffix_domain(&domain, index, direct_count, extent, *span)?;
    let [binder_span] = binder_spans.as_slice() else {
        return Err(function_sequence_error(
            "array suffix does not own one exact binder span",
            *span,
        ));
    };
    if *binder_span != expression_span(&index.range)? {
        return Err(function_sequence_error(
            "array suffix binder span differs from its exact source range",
            *span,
        ));
    }
    Ok(FunctionArraySuffixProduct {
        domain,
        binder_span: *binder_span,
        index: index.clone(),
        value: value.clone(),
        span: *span,
    })
}

fn require_exact_array_suffix_domain(
    domain: &StructuredIndexDomain,
    index: &rumoca_core::ForIndex,
    direct_count: usize,
    extent: usize,
    span: Span,
) -> Result<(), ToDaeError> {
    let [binder] = domain.binders.as_slice() else {
        return Err(function_sequence_error(
            "array suffix does not own one exact binder",
            span,
        ));
    };
    let lower = direct_count
        .checked_add(1)
        .and_then(|value| i64::try_from(value).ok());
    let upper = i64::try_from(extent).ok();
    let expected_count = extent.checked_sub(direct_count);
    let actual_count = domain
        .validated()
        .map_err(|_| function_sequence_error("array suffix domain is invalid", span))?
        .scalar_count();
    if Some(binder.lower) != lower
        || Some(binder.upper) != upper
        || binder.step != 1
        || binder.display_name != index.ident
        || Some(actual_count) != expected_count
    {
        return Err(function_sequence_error(
            "array suffix does not prove the exact remaining extent",
            span,
        ));
    }
    Ok(())
}

fn consume_array_assembly_members(
    first: rumoca_core::Statement,
    members: &[AnalyzedFunctionArrayDirectMember],
    source: &mut std::vec::IntoIter<rumoca_core::Statement>,
    plans: &mut std::vec::IntoIter<FunctionStatementPlan>,
    target: &VarName,
    target_def_id: rumoca_core::DefId,
    owner_span: Span,
) -> Result<Box<[rumoca_core::Statement]>, ToDaeError> {
    let Some(first_member) = members.first() else {
        return Err(function_sequence_error(
            "array assembly source run is empty",
            owner_span,
        ));
    };
    require_array_direct_source(&first, first_member, 0, target, target_def_id, owner_span)?;
    let mut run = Vec::with_capacity(members.len());
    run.push(first);
    for (offset, member) in members.iter().enumerate().skip(1) {
        let Some(statement) = source.next() else {
            return Err(function_sequence_error(
                "array assembly source run is short",
                owner_span,
            ));
        };
        let Some(plan) = plans.next() else {
            return Err(function_sequence_error(
                "array assembly member plan is short",
                owner_span,
            ));
        };
        if !matches!(plan, FunctionStatementPlan::ArrayAssemblyMember) {
            return Err(function_sequence_error(
                "array assembly source member has a foreign lowering plan",
                statement.source_span().unwrap_or(owner_span),
            ));
        }
        require_array_direct_source(
            &statement,
            member,
            offset,
            target,
            target_def_id,
            owner_span,
        )?;
        run.push(statement);
    }
    Ok(run.into_boxed_slice())
}

fn require_array_direct_source(
    statement: &rumoca_core::Statement,
    member: &AnalyzedFunctionArrayDirectMember,
    offset: usize,
    target: &VarName,
    target_def_id: rumoca_core::DefId,
    owner_span: Span,
) -> Result<(), ToDaeError> {
    let Some(one_based_position) = offset
        .checked_add(1)
        .and_then(|position| i64::try_from(position).ok())
    else {
        return Err(function_sequence_error(
            "array assembly position exceeds the exact index domain",
            statement.source_span().unwrap_or(owner_span),
        ));
    };
    let rumoca_core::Statement::Assignment { comp, .. } = statement else {
        return Err(foreign_source_error(statement, owner_span));
    };
    let [root] = comp.parts() else {
        return Err(foreign_source_error(statement, owner_span));
    };
    if member.one_based_index == one_based_position
        && root.ident == target.as_str()
        && root.def_id == target_def_id
        && root.subs.as_slice() == member.subscripts.as_ref()
    {
        Ok(())
    } else {
        Err(foreign_source_error(statement, owner_span))
    }
}

fn aggregate_product(
    first: rumoca_core::Statement,
    plan: FunctionLoweringPlan,
    source: &mut std::vec::IntoIter<rumoca_core::Statement>,
    plans: &mut std::vec::IntoIter<FunctionStatementPlan>,
    request: AssemblyRunRequest<'_>,
) -> Result<FunctionStatementProduct, ToDaeError> {
    let span = first.source_span().unwrap_or(request.owner_span);
    let source = consume_assembly_members(first, source, plans, &request)?;
    Ok(FunctionStatementProduct {
        source,
        plan,
        runtime_assertion: None,
        span,
    })
}

fn require_empty_source(
    statement: &rumoca_core::Statement,
    owner_span: Span,
) -> Result<(), ToDaeError> {
    if matches!(statement, rumoca_core::Statement::Empty { .. }) {
        Ok(())
    } else {
        Err(foreign_source_error(statement, owner_span))
    }
}

fn foreign_source_error(statement: &rumoca_core::Statement, owner_span: Span) -> ToDaeError {
    function_sequence_error(
        "lowering certificate owns a foreign source statement",
        statement.source_span().unwrap_or(owner_span),
    )
}

fn issued_loop_source(
    statement: &rumoca_core::Statement,
    source_depth: usize,
    owner_span: Span,
) -> Result<
    (
        Vec<rumoca_core::ForIndex>,
        Vec<rumoca_core::Statement>,
        Span,
    ),
    ToDaeError,
> {
    let mut current = statement;
    let mut indices = Vec::new();
    let mut span = owner_span;
    for level in 0..source_depth {
        let rumoca_core::Statement::For {
            indices: next,
            equations,
            span: next_span,
        } = current
        else {
            return Err(function_sequence_error(
                "loop certificate does not own a perfect nested loop",
                current.source_span().unwrap_or(owner_span),
            ));
        };
        indices.extend(next.iter().cloned());
        span = *next_span;
        if level + 1 == source_depth {
            return Ok((indices, equations.clone(), span));
        }
        let [next @ rumoca_core::Statement::For { .. }] = equations.as_slice() else {
            return Err(function_sequence_error(
                "loop certificate does not own its declared source depth",
                span,
            ));
        };
        current = next;
    }
    Err(function_sequence_error(
        "loop certificate has no exact source depth",
        span,
    ))
}

fn issue_conditional_product(
    statement: &rumoca_core::Statement,
    branch_plans: Vec<Vec<FunctionStatementPlan>>,
    fallback_plans: Option<Vec<FunctionStatementPlan>>,
    targets: Vec<FunctionConditionalTarget>,
    owner_span: Span,
) -> Result<FunctionLoweringPlan, ToDaeError> {
    let rumoca_core::Statement::If {
        cond_blocks,
        else_block,
        span,
    } = statement
    else {
        return Err(function_sequence_error(
            "conditional certificate does not own an if statement",
            statement.source_span().unwrap_or(owner_span),
        ));
    };
    let mut source_blocks = cond_blocks.iter();
    let mut plans = branch_plans.into_iter();
    let mut branches = Vec::new();
    loop {
        match (source_blocks.next(), plans.next()) {
            (Some(block), Some(plans)) => branches.push(issue_function_statement_sequence(
                block.stmts.clone(),
                plans,
                *span,
            )?),
            (None, None) => break,
            _ => {
                return Err(function_sequence_error(
                    "conditional branch plan count differs",
                    *span,
                ));
            }
        }
    }
    let fallback = match (else_block, fallback_plans) {
        (Some(source), Some(plans)) => Some(issue_function_statement_sequence(
            source.clone(),
            plans,
            *span,
        )?),
        (None, None) => None,
        _ => {
            return Err(function_sequence_error(
                "conditional else plan differs",
                *span,
            ));
        }
    };
    Ok(FunctionLoweringPlan::If {
        conditions: cond_blocks.iter().map(|block| block.cond.clone()).collect(),
        branches,
        fallback,
        targets,
        span: *span,
    })
}

fn issue_proven_branch_product(
    statement: &rumoca_core::Statement,
    selected: Option<usize>,
    plans: Vec<FunctionStatementPlan>,
    owner_span: Span,
) -> Result<FunctionLoweringPlan, ToDaeError> {
    let rumoca_core::Statement::If {
        cond_blocks,
        else_block,
        span,
    } = statement
    else {
        return Err(function_sequence_error(
            "proven branch does not own an if statement",
            statement.source_span().unwrap_or(owner_span),
        ));
    };
    let selected_source = match selected {
        Some(ordinal) => cond_blocks.get(ordinal).map(|block| block.stmts.clone()),
        None => Some(else_block.clone().unwrap_or_default()),
    };
    let Some(selected_source) = selected_source else {
        return Err(function_sequence_error(
            "proven branch has no exact source arm",
            *span,
        ));
    };
    Ok(FunctionLoweringPlan::ProvenBranch {
        statements: issue_function_statement_sequence(selected_source, plans, *span)?,
    })
}

#[derive(Clone, Copy)]
enum FunctionAssemblyMember {
    ArraySuffix,
    Record,
    RecordField,
}

#[derive(Clone)]
struct AssemblyFieldIdentity {
    name: VarName,
    def_id: rumoca_core::DefId,
}

fn issue_record_assembly_member_fields(
    assembly: &FunctionRecordAssemblyPlan,
    owner_span: Span,
) -> Result<Vec<AssemblyFieldIdentity>, ToDaeError> {
    let mut members: Vec<Option<AssemblyFieldIdentity>> = vec![None; assembly.statement_count];
    for field in &assembly.fields {
        let identity = AssemblyFieldIdentity {
            name: field.name.clone(),
            def_id: field.def_id,
        };
        let offsets = field
            .aggregate_statement
            .iter()
            .copied()
            .chain(field.scalars.iter().map(|source| source.statement_offset));
        for offset in offsets {
            let Some(slot) = members.get_mut(offset) else {
                return Err(function_sequence_error(
                    "record assembly member offset is out of bounds",
                    owner_span,
                ));
            };
            match slot {
                Some(existing)
                    if existing.name == identity.name && existing.def_id == identity.def_id => {}
                Some(_) => {
                    return Err(function_sequence_error(
                        "record assembly member offset has conflicting fields",
                        owner_span,
                    ));
                }
                None => *slot = Some(identity.clone()),
            }
        }
    }
    members
        .into_iter()
        .map(|member| {
            member.ok_or_else(|| {
                function_sequence_error("record assembly member has no exact field", owner_span)
            })
        })
        .collect()
}

fn require_assembly_source(
    statement: &rumoca_core::Statement,
    member: FunctionAssemblyMember,
    target: &VarName,
    target_def_id: rumoca_core::DefId,
    expected_field: Option<&AssemblyFieldIdentity>,
    owner_span: Span,
) -> Result<(), ToDaeError> {
    let rumoca_core::Statement::Assignment { comp, .. } = statement else {
        return Err(foreign_source_error(statement, owner_span));
    };
    let valid_shape = match (member, comp.parts()) {
        (FunctionAssemblyMember::ArraySuffix, [root]) => root.subs.len() == 1,
        (FunctionAssemblyMember::Record | FunctionAssemblyMember::RecordField, [root, _]) => {
            root.subs.is_empty()
        }
        _ => false,
    };
    let Some(root) = comp.parts().first() else {
        return Err(foreign_source_error(statement, owner_span));
    };
    let field_matches = match (expected_field, comp.parts()) {
        (Some(expected), [_, field]) => {
            field.ident == expected.name.as_str() && field.def_id == expected.def_id
        }
        (None, _) => true,
        _ => false,
    };
    if valid_shape && field_matches && root.ident == target.as_str() && root.def_id == target_def_id
    {
        Ok(())
    } else {
        Err(foreign_source_error(statement, owner_span))
    }
}

fn consume_assembly_members(
    first: rumoca_core::Statement,
    source: &mut std::vec::IntoIter<rumoca_core::Statement>,
    plans: &mut std::vec::IntoIter<FunctionStatementPlan>,
    request: &AssemblyRunRequest<'_>,
) -> Result<Box<[rumoca_core::Statement]>, ToDaeError> {
    let AssemblyRunRequest {
        count,
        member,
        target,
        target_def_id,
        expected_fields,
        owner_span,
    } = *request;
    if count == 0 {
        return Err(function_sequence_error(
            "aggregate source run is empty",
            owner_span,
        ));
    }
    let mut run = Vec::with_capacity(count);
    require_assembly_source(
        &first,
        member,
        target,
        target_def_id,
        expected_fields.and_then(|fields| fields.first()),
        owner_span,
    )?;
    run.push(first);
    for _ in 1..count {
        let Some(statement) = source.next() else {
            return Err(function_sequence_error(
                "aggregate source run is short",
                owner_span,
            ));
        };
        let Some(plan) = plans.next() else {
            return Err(function_sequence_error(
                "aggregate member plan is short",
                owner_span,
            ));
        };
        let correct_member = matches!(
            (member, plan),
            (
                FunctionAssemblyMember::Record,
                FunctionStatementPlan::RecordAssemblyMember
            ) | (
                FunctionAssemblyMember::RecordField,
                FunctionStatementPlan::RecordFieldAssemblyMember
            )
        );
        if !correct_member {
            return Err(function_sequence_error(
                "aggregate source member has a foreign lowering plan",
                statement.source_span().unwrap_or(owner_span),
            ));
        }
        require_assembly_source(
            &statement,
            member,
            target,
            target_def_id,
            expected_fields.and_then(|fields| fields.get(run.len())),
            owner_span,
        )?;
        run.push(statement);
    }
    Ok(run.into_boxed_slice())
}

fn function_sequence_error(detail: &str, span: Span) -> ToDaeError {
    function_statement_product_error(detail, span)
}

pub(super) fn function_statement_product_error(detail: &str, span: Span) -> ToDaeError {
    ToDaeError::unsupported_flat("function statement product", detail, span)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{
        ComponentRefPart, ComponentReference, DefId, FunctionInstanceId, Literal,
        ResolvedFunctionReference, SourceId,
    };

    macro_rules! assert_not_implemented {
        ($ty:ty, $bound:path) => {
            const _: fn() = || {
                trait AmbiguousIfImplemented<Marker> {
                    fn probe() {}
                }
                impl<T> AmbiguousIfImplemented<()> for T {}
                struct Implements;
                impl<T: $bound> AmbiguousIfImplemented<Implements> for T {}
                let _ = <$ty as AmbiguousIfImplemented<_>>::probe;
            };
        };
    }

    assert_not_implemented!(FunctionStatementSequence, ::core::clone::Clone);
    assert_not_implemented!(FunctionStatementProduct, ::core::clone::Clone);

    #[test]
    fn statement_sequence_and_exact_products_are_affine_api_products() {}

    fn span(start: usize) -> Span {
        Span::from_offsets(
            SourceId::from_source_name("function_products.mo"),
            start,
            start + 1,
        )
    }

    fn component(
        name: &str,
        id: u32,
        subs: Vec<rumoca_core::Subscript>,
        owner: Span,
    ) -> ComponentReference {
        ComponentReference::construct(
            false,
            owner,
            vec![ComponentRefPart {
                ident: name.to_string(),
                span: owner,
                subs,
                def_id: DefId::new(id),
            }],
        )
        .expect("fixture component has one exact source identity")
    }

    fn assignment(name: &str, id: u32, span: Span) -> rumoca_core::Statement {
        rumoca_core::Statement::Assignment {
            comp: component(name, id, Vec::new(), span),
            value: Expression::Literal {
                value: Literal::Real(f64::from(id)),
                span,
            },
            span,
        }
    }

    fn assignment_plan_value(name: &str, id: u32) -> FunctionAssignmentPlan {
        FunctionAssignmentPlan {
            target: VarName::new(name),
            target_def_id: DefId::new(id),
            record_field: None,
            record_root_name: VarName::new(name),
            record_field_name: None,
            subscripts: Vec::new().into_boxed_slice(),
            seed: None,
        }
    }

    fn assignment_plan(name: &str, id: u32) -> FunctionStatementPlan {
        FunctionStatementPlan::Assignment(assignment_plan_value(name, id))
    }

    fn call_output(name: &str, id: u32, owner: Span) -> rumoca_core::Statement {
        rumoca_core::Statement::FunctionCall {
            comp: rumoca_core::Reference::new("callee"),
            args: Vec::new(),
            outputs: vec![Some(component(name, id, Vec::new(), owner))],
            span: owner,
        }
    }

    fn indexed_assignment(name: &str, id: u32, owner: Span) -> rumoca_core::Statement {
        indexed_assignment_at(name, id, 1, owner)
    }

    fn indexed_assignment_at(
        name: &str,
        id: u32,
        index: i64,
        owner: Span,
    ) -> rumoca_core::Statement {
        rumoca_core::Statement::Assignment {
            comp: component(
                name,
                id,
                vec![rumoca_core::Subscript::Index {
                    value: index,
                    span: owner,
                }],
                owner,
            ),
            value: Expression::Literal {
                value: Literal::Real(f64::from(id)),
                span: owner,
            },
            span: owner,
        }
    }

    fn record_assignment(
        name: &str,
        id: u32,
        field: &str,
        field_id: u32,
        owner: Span,
    ) -> rumoca_core::Statement {
        let comp = ComponentReference::construct(
            false,
            owner,
            vec![
                ComponentRefPart {
                    ident: name.to_string(),
                    span: owner,
                    subs: Vec::new(),
                    def_id: DefId::new(id),
                },
                ComponentRefPart {
                    ident: field.to_string(),
                    span: owner,
                    subs: Vec::new(),
                    def_id: DefId::new(field_id),
                },
            ],
        )
        .expect("fixture record target has exact root and field identities");
        rumoca_core::Statement::Assignment {
            comp,
            value: Expression::Literal {
                value: Literal::Real(f64::from(id)),
                span: owner,
            },
            span: owner,
        }
    }

    fn record_receiver(
        name: &str,
        id: u32,
        field: &str,
        field_id: u32,
        owner: Span,
    ) -> ComponentReference {
        ComponentReference::construct(
            false,
            owner,
            vec![
                ComponentRefPart {
                    ident: name.to_string(),
                    span: owner,
                    subs: Vec::new(),
                    def_id: DefId::new(id),
                },
                ComponentRefPart {
                    ident: field.to_string(),
                    span: owner,
                    subs: Vec::new(),
                    def_id: DefId::new(field_id),
                },
            ],
        )
        .expect("fixture receiver has exact root and field identities")
    }

    fn record_call_output(
        name: &str,
        id: u32,
        field: &str,
        field_id: u32,
        owner: Span,
    ) -> rumoca_core::Statement {
        rumoca_core::Statement::FunctionCall {
            comp: rumoca_core::Reference::new("callee"),
            args: Vec::new(),
            outputs: vec![Some(record_receiver(name, id, field, field_id, owner))],
            span: owner,
        }
    }

    fn record_call_plan(name: &str, id: u32, field: &str, field_id: u32) -> FunctionStatementPlan {
        FunctionStatementPlan::RecordMultiOutputAssembly(FunctionRecordCallAssemblyPlan {
            target: VarName::new(name),
            target_def_id: DefId::new(id),
            fields: vec![FunctionRecordCallField {
                name: VarName::new(field),
                def_id: DefId::new(field_id),
                result_ordinal: 0,
            }],
        })
    }

    fn integer(value: i64, owner: Span) -> Expression {
        Expression::Literal {
            value: Literal::Integer(value),
            span: owner,
        }
    }

    fn loop_index(lower: i64, upper: i64, owner: Span) -> rumoca_core::ForIndex {
        rumoca_core::ForIndex {
            ident: "i".to_string(),
            range: Expression::Range {
                start: Box::new(integer(lower, owner)),
                step: None,
                end: Box::new(integer(upper, owner)),
                span: owner,
            },
        }
    }

    fn binder_subscript(owner: Span) -> rumoca_core::Subscript {
        rumoca_core::Subscript::Expr {
            expr: Box::new(Expression::VarRef {
                name: rumoca_core::Reference::new("i"),
                subscripts: Vec::new(),
                span: owner,
            }),
            span: owner,
        }
    }

    fn binder_assignment(name: &str, id: u32, owner: Span) -> rumoca_core::Statement {
        rumoca_core::Statement::Assignment {
            comp: component(name, id, vec![binder_subscript(owner)], owner),
            value: integer(7, owner),
            span: owner,
        }
    }

    fn suffix_loop_plan(
        name: &str,
        id: u32,
        lower: i64,
        upper: i64,
        owner: Span,
    ) -> FunctionStatementPlan {
        let mut assignment = assignment_plan_value(name, id);
        assignment.subscripts = vec![binder_subscript(owner)].into_boxed_slice();
        FunctionStatementPlan::For {
            domain: StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: rumoca_core::StructuredIndexBinderId::new(0),
                    display_name: "i".to_string(),
                    lower,
                    upper,
                    step: 1,
                }],
            },
            binder_spans: vec![owner],
            lowering: FunctionLoopLowering::TotalArrayDefinition,
            statements: vec![FunctionStatementPlan::Assignment(assignment)],
            source_depth: 1,
        }
    }

    fn array_assembly_plan(
        name: &str,
        id: u32,
        direct_count: usize,
        owner: Span,
    ) -> FunctionStatementPlan {
        let direct_members = (1..=direct_count)
            .map(|position| AnalyzedFunctionArrayDirectMember {
                subscripts: vec![rumoca_core::Subscript::Index {
                    value: i64::try_from(position)
                        .expect("fixture position fits the source integer domain"),
                    span: owner,
                }]
                .into_boxed_slice(),
                one_based_index: i64::try_from(position)
                    .expect("fixture position fits the source integer domain"),
            })
            .collect();
        FunctionStatementPlan::ArrayAssembly(AnalyzedFunctionArrayAssemblyPlan {
            target: VarName::new(name),
            target_def_id: DefId::new(id),
            direct_members,
            extent: direct_count,
            suffix_index: None,
            loop_plan: None,
            seed: None,
        })
    }

    fn array_assembly_with_suffix_plan(
        owner: Span,
        binder_spans: Vec<Span>,
        binder_name: &str,
    ) -> FunctionStatementPlan {
        let direct_member = AnalyzedFunctionArrayDirectMember {
            subscripts: vec![rumoca_core::Subscript::Index {
                value: 1,
                span: owner,
            }]
            .into_boxed_slice(),
            one_based_index: 1,
        };
        let mut loop_plan = suffix_loop_plan("value", 1, 2, 3, owner);
        let FunctionStatementPlan::For {
            domain,
            binder_spans: plan_binder_spans,
            ..
        } = &mut loop_plan
        else {
            panic!("fixture suffix plan must own one loop")
        };
        domain.binders[0].display_name = binder_name.to_string();
        *plan_binder_spans = binder_spans;
        FunctionStatementPlan::ArrayAssembly(AnalyzedFunctionArrayAssemblyPlan {
            target: VarName::new("value"),
            target_def_id: DefId::new(1),
            direct_members: vec![direct_member],
            extent: 3,
            suffix_index: Some(loop_index(2, 3, owner)),
            loop_plan: Some(Box::new(loop_plan)),
            seed: None,
        })
    }

    fn issue_array_suffix_fixture(
        plan: FunctionStatementPlan,
        owner: Span,
    ) -> Result<FunctionStatementSequence, ToDaeError> {
        let suffix_source = rumoca_core::Statement::For {
            indices: vec![loop_index(2, 3, owner)],
            equations: vec![binder_assignment("value", 1, owner)],
            span: owner,
        };
        issue_function_statement_sequence(
            vec![indexed_assignment("value", 1, owner), suffix_source],
            vec![plan, FunctionStatementPlan::ArrayAssemblyMember],
            owner,
        )
    }

    #[test]
    fn exact_sequence_owns_each_source_payload_once() {
        let first = span(10);
        let second = span(20);
        let sequence = issue_function_statement_sequence(
            vec![assignment("f", 1, first), assignment("g", 2, second)],
            vec![assignment_plan("f", 1), assignment_plan("g", 2)],
            first,
        )
        .expect("exact source and lowering certificates issue one closed sequence");

        assert_eq!(sequence.products.len(), 2);
        assert_eq!(sequence.products[0].source[0].source_span(), Some(first));
        assert_eq!(sequence.products[1].source[0].source_span(), Some(second));
    }

    #[test]
    fn short_and_long_plan_mutants_are_rejected_before_product_exposure() {
        let owner = span(30);
        let short =
            issue_function_statement_sequence(vec![assignment("f", 1, owner)], Vec::new(), owner);
        let long =
            issue_function_statement_sequence(Vec::new(), vec![assignment_plan("f", 1)], owner);

        assert!(short.is_err());
        assert!(long.is_err());
    }

    #[test]
    fn same_span_swapped_assignment_payloads_are_rejected() {
        let shared = span(40);
        let swapped = issue_function_statement_sequence(
            vec![assignment("f", 1, shared), assignment("g", 2, shared)],
            vec![assignment_plan("g", 2), assignment_plan("f", 1)],
            shared,
        );

        assert!(swapped.is_err());
    }

    #[test]
    fn same_spelled_foreign_assignment_and_call_output_identities_are_rejected() {
        let owner = span(50);
        let assignment = issue_function_statement_sequence(
            vec![assignment("f", 9, owner)],
            vec![assignment_plan("f", 1)],
            owner,
        );
        let call = issue_function_statement_sequence(
            vec![call_output("f", 9, owner)],
            vec![FunctionStatementPlan::MultiOutputCall {
                outputs: vec![Some(assignment_plan_value("f", 1))],
            }],
            owner,
        );

        assert!(assignment.is_err());
        assert!(call.is_err());
    }

    #[test]
    fn runtime_assertion_issuance_rejects_a_declared_function_named_assert() {
        let owner = span(55);
        let declared_assert = rumoca_core::Reference::new("assert").with_resolved_function(
            ResolvedFunctionReference {
                instance_id: FunctionInstanceId::new(7),
                base_part_count: 1,
                transitively_non_replaceable: false,
            },
        );
        let source = rumoca_core::Statement::FunctionCall {
            comp: declared_assert,
            args: vec![integer(1, owner), integer(2, owner)],
            outputs: Vec::new(),
            span: owner,
        };
        let issued = issue_function_statement_sequence(
            vec![source],
            vec![FunctionStatementPlan::RuntimeAssertion],
            owner,
        );

        assert!(issued.is_err());
    }

    #[test]
    fn proven_assertion_issuance_rejects_a_declared_function_named_assert() {
        let owner = span(57);
        let declared_assert = rumoca_core::Reference::new("assert").with_resolved_function(
            ResolvedFunctionReference {
                instance_id: FunctionInstanceId::new(7),
                base_part_count: 1,
                transitively_non_replaceable: false,
            },
        );
        let source = rumoca_core::Statement::FunctionCall {
            comp: declared_assert,
            args: vec![integer(1, owner), integer(2, owner)],
            outputs: Vec::new(),
            span: owner,
        };
        let issued = issue_function_statement_sequence(
            vec![source],
            vec![FunctionStatementPlan::ProvenAssertion],
            owner,
        );

        assert!(issued.is_err());
    }

    #[test]
    fn proven_false_if_without_else_issues_one_empty_selected_sequence() {
        let owner = span(60);
        let source = rumoca_core::Statement::If {
            cond_blocks: vec![rumoca_core::StatementBlock {
                cond: Expression::Literal {
                    value: Literal::Boolean(false),
                    span: owner,
                },
                stmts: vec![assignment("dead", 2, owner)],
            }],
            else_block: None,
            span: owner,
        };
        let sequence = issue_function_statement_sequence(
            vec![source],
            vec![FunctionStatementPlan::ProvenBranch {
                selected: None,
                statements: Vec::new(),
            }],
            owner,
        )
        .expect("all-false conditional without else selects an empty source arm");

        let FunctionLoweringPlan::ProvenBranch { statements } = sequence.products[0].plan() else {
            panic!("fixture must issue the proven empty branch")
        };
        assert!(statements.products().is_empty());
    }

    #[test]
    fn empty_proven_branch_rejects_out_of_range_and_invented_statement_plans() {
        let owner = span(70);
        let source = rumoca_core::Statement::If {
            cond_blocks: vec![rumoca_core::StatementBlock {
                cond: Expression::Literal {
                    value: Literal::Boolean(false),
                    span: owner,
                },
                stmts: Vec::new(),
            }],
            else_block: None,
            span: owner,
        };
        let out_of_range = issue_function_statement_sequence(
            vec![source.clone()],
            vec![FunctionStatementPlan::ProvenBranch {
                selected: Some(1),
                statements: Vec::new(),
            }],
            owner,
        );
        let invented = issue_function_statement_sequence(
            vec![source],
            vec![FunctionStatementPlan::ProvenBranch {
                selected: None,
                statements: vec![assignment_plan("forged", 4)],
            }],
            owner,
        );

        assert!(out_of_range.is_err());
        assert!(invented.is_err());
    }

    #[test]
    fn every_aggregate_shape_rejects_foreign_names_and_same_name_foreign_identities() {
        let owner = span(80);
        let cases = [
            (
                FunctionAssemblyMember::Record,
                record_assignment("value", 1, "field", 11, owner),
                record_assignment("foreign", 1, "field", 11, owner),
                record_assignment("value", 2, "field", 11, owner),
            ),
            (
                FunctionAssemblyMember::RecordField,
                record_assignment("value", 1, "field", 11, owner),
                record_assignment("foreign", 1, "field", 11, owner),
                record_assignment("value", 2, "field", 11, owner),
            ),
        ];
        let expected_field = AssemblyFieldIdentity {
            name: VarName::new("field"),
            def_id: DefId::new(11),
        };
        for (member, exact, foreign_name, foreign_identity) in cases {
            let field = Some(&expected_field);
            assert!(
                require_assembly_source(
                    &exact,
                    member,
                    &VarName::new("value"),
                    DefId::new(1),
                    field,
                    owner,
                )
                .is_ok()
            );
            assert!(
                require_assembly_source(
                    &foreign_name,
                    member,
                    &VarName::new("value"),
                    DefId::new(1),
                    field,
                    owner,
                )
                .is_err()
            );
            assert!(
                require_assembly_source(
                    &foreign_identity,
                    member,
                    &VarName::new("value"),
                    DefId::new(1),
                    field,
                    owner,
                )
                .is_err()
            );
            assert!(
                require_assembly_source(
                    &record_assignment("value", 1, "field", 12, owner),
                    member,
                    &VarName::new("value"),
                    DefId::new(1),
                    field,
                    owner,
                )
                .is_err()
            );
        }
    }

    #[test]
    fn array_aggregate_issuance_rejects_a_foreign_member_in_an_exact_length_run() {
        let owner = span(90);
        let foreign_name = issue_function_statement_sequence(
            vec![
                indexed_assignment("value", 1, owner),
                indexed_assignment_at("foreign", 1, 2, owner),
            ],
            vec![
                array_assembly_plan("value", 1, 2, owner),
                FunctionStatementPlan::ArrayAssemblyMember,
            ],
            owner,
        );
        let foreign_identity = issue_function_statement_sequence(
            vec![
                indexed_assignment("value", 1, owner),
                indexed_assignment_at("value", 2, 2, owner),
            ],
            vec![
                array_assembly_plan("value", 1, 2, owner),
                FunctionStatementPlan::ArrayAssemblyMember,
            ],
            owner,
        );

        assert!(foreign_name.is_err());
        assert!(foreign_identity.is_err());
    }

    #[test]
    fn array_issuance_rejects_duplicate_and_out_of_order_direct_indices() {
        let owner = span(100);
        let duplicate = issue_function_statement_sequence(
            vec![
                indexed_assignment_at("value", 1, 1, owner),
                indexed_assignment_at("value", 1, 1, owner),
            ],
            vec![
                array_assembly_plan("value", 1, 2, owner),
                FunctionStatementPlan::ArrayAssemblyMember,
            ],
            owner,
        );
        let out_of_order = issue_function_statement_sequence(
            vec![
                indexed_assignment_at("value", 1, 2, owner),
                indexed_assignment_at("value", 1, 1, owner),
            ],
            vec![
                array_assembly_plan("value", 1, 2, owner),
                FunctionStatementPlan::ArrayAssemblyMember,
            ],
            owner,
        );

        assert!(duplicate.is_err());
        assert!(out_of_order.is_err());
    }

    #[test]
    fn array_issuance_rejects_a_shifted_suffix_domain() {
        let owner = span(110);
        let shifted_loop = rumoca_core::Statement::For {
            indices: vec![loop_index(2, 3, owner)],
            equations: vec![binder_assignment("value", 1, owner)],
            span: owner,
        };
        let direct_member = AnalyzedFunctionArrayDirectMember {
            subscripts: vec![rumoca_core::Subscript::Index {
                value: 1,
                span: owner,
            }]
            .into_boxed_slice(),
            one_based_index: 1,
        };
        let assembly = FunctionStatementPlan::ArrayAssembly(AnalyzedFunctionArrayAssemblyPlan {
            target: VarName::new("value"),
            target_def_id: DefId::new(1),
            direct_members: vec![direct_member],
            extent: 3,
            suffix_index: Some(loop_index(2, 3, owner)),
            loop_plan: Some(Box::new(suffix_loop_plan("value", 1, 3, 3, owner))),
            seed: None,
        });
        let shifted = issue_function_statement_sequence(
            vec![indexed_assignment("value", 1, owner), shifted_loop],
            vec![assembly, FunctionStatementPlan::ArrayAssemblyMember],
            owner,
        );

        assert!(shifted.is_err());
    }

    #[test]
    fn array_suffix_issuance_rejects_non_exact_binder_metadata() {
        let owner = span(115);
        let foreign = span(116);
        let exact = issue_array_suffix_fixture(
            array_assembly_with_suffix_plan(owner, vec![owner], "i"),
            owner,
        );
        let empty = issue_array_suffix_fixture(
            array_assembly_with_suffix_plan(owner, Vec::new(), "i"),
            owner,
        );
        let extra = issue_array_suffix_fixture(
            array_assembly_with_suffix_plan(owner, vec![owner, owner], "i"),
            owner,
        );
        let foreign_span = issue_array_suffix_fixture(
            array_assembly_with_suffix_plan(owner, vec![foreign], "i"),
            owner,
        );
        let foreign_identity = issue_array_suffix_fixture(
            array_assembly_with_suffix_plan(owner, vec![owner], "j"),
            owner,
        );

        if let Err(error) = exact {
            panic!("exact binder metadata must issue: {error:?}");
        }
        assert!(empty.is_err());
        assert!(extra.is_err());
        assert!(foreign_span.is_err());
        assert!(foreign_identity.is_err());
    }

    #[test]
    fn record_call_issuance_rejects_foreign_receiver_root_and_field_identities() {
        let owner = span(120);
        let foreign_root = issue_function_statement_sequence(
            vec![record_call_output("foreign", 1, "field", 11, owner)],
            vec![record_call_plan("value", 1, "field", 11)],
            owner,
        );
        let foreign_root_identity = issue_function_statement_sequence(
            vec![record_call_output("value", 2, "field", 11, owner)],
            vec![record_call_plan("value", 1, "field", 11)],
            owner,
        );
        let foreign_field_name = issue_function_statement_sequence(
            vec![record_call_output("value", 1, "other", 11, owner)],
            vec![record_call_plan("value", 1, "field", 11)],
            owner,
        );
        let foreign_field = issue_function_statement_sequence(
            vec![record_call_output("value", 1, "field", 12, owner)],
            vec![record_call_plan("value", 1, "field", 11)],
            owner,
        );

        assert!(foreign_root.is_err());
        assert!(foreign_root_identity.is_err());
        assert!(foreign_field_name.is_err());
        assert!(foreign_field.is_err());
    }

    #[test]
    fn record_call_issuance_rejects_duplicate_exact_field_identity() {
        let owner = span(130);
        let receiver = record_receiver("value", 1, "field", 11, owner);
        let source = rumoca_core::Statement::FunctionCall {
            comp: rumoca_core::Reference::new("callee"),
            args: Vec::new(),
            outputs: vec![Some(receiver.clone()), Some(receiver)],
            span: owner,
        };
        let issued = issue_function_statement_sequence(
            vec![source],
            vec![FunctionStatementPlan::RecordMultiOutputAssembly(
                FunctionRecordCallAssemblyPlan {
                    target: VarName::new("value"),
                    target_def_id: DefId::new(1),
                    fields: vec![
                        FunctionRecordCallField {
                            name: VarName::new("field"),
                            def_id: DefId::new(11),
                            result_ordinal: 0,
                        },
                        FunctionRecordCallField {
                            name: VarName::new("field"),
                            def_id: DefId::new(11),
                            result_ordinal: 1,
                        },
                    ],
                },
            )],
            owner,
        );

        assert!(issued.is_err());
    }
}
