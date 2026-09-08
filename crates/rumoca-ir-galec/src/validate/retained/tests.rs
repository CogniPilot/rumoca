use super::*;
use crate::ast as galec;

fn array_declaration(
    scalar: galec::ScalarType,
    name: &str,
    extents: &[i64],
) -> galec::VariableDeclaration {
    galec::VariableDeclaration {
        ty: galec::TypeRef::Primitive(scalar),
        name: galec::Name::ident(name),
        dimensions: extents
            .iter()
            .copied()
            .map(galec::Expression::Integer)
            .map(galec::Dimension::Expr)
            .collect(),
        range: galec::RangeAttributes::default(),
        span: Span::DUMMY,
    }
}

fn two_result_block(target_extents: [i64; 2], discard: bool) -> galec::Block {
    let outputs = ["first", "second"].map(|name| galec::Parameter {
        direction: galec::Direction::Output,
        decl: array_declaration(galec::ScalarType::Real, name, &[2]),
    });
    let function = galec::UserFunction {
        kind: galec::FunctionKind::Stateless,
        name: galec::Name::ident("pair"),
        signals: Vec::new(),
        parameters: outputs.into_iter().collect(),
        locals: Vec::new(),
        statements: Vec::new(),
        span: Span::DUMMY,
    };
    let call = galec::FunctionCall {
        function: galec::Name::ident("pair"),
        arguments: Vec::new(),
    };
    let mut block = galec::Block::new(galec::Name::ident("ProjectionBlock"));
    block.protected_functions = vec![function];
    block.do_step.locals = ["left", "right"]
        .into_iter()
        .zip(target_extents)
        .map(|(name, extent)| array_declaration(galec::ScalarType::Real, name, &[extent]))
        .collect();
    block.do_step.statements = vec![galec::Spanned::dummy(if discard {
        galec::Statement::Call(call)
    } else {
        galec::Statement::MultiAssignment {
            targets: vec![
                galec::Reference::local(galec::Name::ident("left")),
                galec::Reference::local(galec::Name::ident("right")),
            ],
            call,
        }
    })];
    block
}

fn builtin_call_block(name: &str) -> galec::Block {
    let mut block = galec::Block::new(galec::Name::ident("BuiltinResolution"));
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::Call(
        galec::FunctionCall {
            function: galec::Name::ident(name),
            arguments: Vec::new(),
        },
    ))];
    block
}

fn sole_call(block: &galec::Block) -> &galec::FunctionCall {
    let galec::Statement::Call(call) = &block.do_step.statements[0].node else {
        panic!("builtin fixture must contain one call")
    };
    call
}

fn retain_types(
    block: &galec::Block,
) -> Result<(RetainedValidationBuilder, Vec<crate::GalecError>), RetainedValidationError> {
    let mut builder = RetainedValidationBuilder::install(
        block,
        super::super::DeclarationStartContract::ParsedSyntax,
        None,
    )?;
    let context = super::super::context::BlockContext::new(block);
    let mut diagnostics = Vec::new();
    super::super::types::check(
        &context,
        super::super::DeclarationStartContract::ParsedSyntax,
        &mut builder,
        &mut diagnostics,
    )?;
    Ok((builder, diagnostics))
}

fn projection_locators(call: &CallSubject) -> Vec<CallResultProjectionLoc> {
    call.children
        .iter()
        .filter_map(|(subject, _)| match subject {
            SubjectLoc::CallResultProjection(locator) => Some(*locator),
            _ => None,
        })
        .collect()
}

fn matrix_profile() -> crate::package::AlgorithmCodeArithmeticProfile {
    crate::package::AlgorithmCodeArithmeticProfile::construct(
        crate::package::AlgorithmCodeRealFormat::Binary64,
        crate::package::AlgorithmCodeIntegerFormat::I64,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
    )
}

fn indexed_reference(name: &str, indices: Vec<galec::Expression>) -> galec::Reference {
    galec::Reference::Local(galec::RefPart {
        name: galec::Name::ident(name),
        subscripts: indices,
        span: Span::DUMMY,
    })
}

fn matrix_occurrence_statement(
    contract_count: u32,
    statement_span: Span,
) -> galec::Spanned<galec::Statement> {
    let column = galec::Name::ident("column");
    let inner = galec::Name::ident("inner");
    let column_value = galec::Expression::Ref(galec::Reference::local(column.clone()));
    let inner_value = galec::Expression::Ref(galec::Reference::local(inner.clone()));
    let target_element = indexed_reference("out", vec![column_value.clone()]);
    let scale = galec::Expression::Ref(indexed_reference("lhs", vec![inner_value.clone()]));
    let source_run = indexed_reference("rhs", vec![inner_value]);
    let source_element = indexed_reference(
        "rhs",
        vec![
            galec::Expression::Ref(galec::Reference::local(inner.clone())),
            column_value,
        ],
    );
    let product = galec::Expression::binary(
        galec::BinaryOp::Mul,
        scale.clone(),
        galec::Expression::Ref(source_element),
    );
    let accumulation = galec::Expression::binary(
        galec::BinaryOp::Add,
        galec::Expression::Ref(target_element.clone()),
        product,
    );
    let remainder = galec::ForLoop::new(
        Some(inner.clone()),
        galec::Expression::Integer(1),
        None,
        galec::Expression::Integer(2),
        vec![galec::Spanned::dummy(galec::Statement::Assignment {
            target: target_element.clone(),
            value: accumulation,
        })],
    );
    let contract = galec::RealMatrixMultiplyOccurrenceContract::new(
        crate::package::AlgorithmCodeRealFormat::Binary64,
        (
            galec::Reference::local(galec::Name::ident("out")),
            contract_count,
        ),
        (inner, 2),
        (scale, source_run),
        galec::RealMatrixMultiplySeed::PositiveZero,
    );
    galec::Spanned::new(
        galec::Statement::for_loop(
            galec::ForLoop::new(
                Some(column),
                galec::Expression::Integer(1),
                None,
                galec::Expression::Integer(2),
                vec![
                    galec::Spanned::dummy(galec::Statement::Assignment {
                        target: target_element,
                        value: galec::Expression::Real(0.0),
                    }),
                    galec::Spanned::dummy(galec::Statement::for_loop(remainder)),
                ],
            )
            .with_real_matrix_multiply_occurrence(contract),
        ),
        statement_span,
    )
}

fn matrix_owner(statements: Vec<galec::Spanned<galec::Statement>>) -> galec::BlockMethod {
    galec::BlockMethod {
        signals: Vec::new(),
        locals: vec![
            array_declaration(galec::ScalarType::Real, "out", &[2]),
            array_declaration(galec::ScalarType::Real, "lhs", &[2]),
            array_declaration(galec::ScalarType::Real, "rhs", &[2, 2]),
        ],
        statements,
        span: Span::DUMMY,
    }
}

#[test]
fn topology_issues_distinct_matrix_occurrences_for_same_spelling_and_shape() {
    let statement = matrix_occurrence_statement(2, Span::DUMMY);
    let mut block = galec::Block::new(galec::Name::ident("MatrixAuthority"));
    block.do_step = matrix_owner(vec![statement.clone(), statement]);

    let (retained, _) = topology::TopologyPlan::construct(
        &block,
        super::super::DeclarationStartContract::GeneratedPackage,
        Some(matrix_profile()),
    )
    .expect("both exact occurrences must close")
    .into_parts();
    let locators = retained
        .real_matrix_multiply_occurrence_locators()
        .collect::<Vec<_>>();
    assert_eq!(locators.len(), 2);
    assert_ne!(locators[0], locators[1]);
    let (first_owner, _, first_target, first_source, _) =
        retained.real_matrix_multiply_occurrence_facts(locators[0]);
    let (second_owner, _, second_target, second_source, _) =
        retained.real_matrix_multiply_occurrence_facts(locators[1]);
    assert_ne!(first_owner, second_owner);
    assert_eq!(first_target, second_target);
    assert_eq!(first_source, second_source);
}

#[test]
fn topology_refuses_missing_profile_and_changed_contract_at_exact_provenance() {
    let provenance = Span {
        source: rumoca_core::SourceId::from_source_name("matrix-authority.alg"),
        start: rumoca_core::BytePos(7),
        end: rumoca_core::BytePos(19),
    };
    let mut block = galec::Block::new(galec::Name::ident("MatrixAuthority"));
    block.do_step = matrix_owner(vec![matrix_occurrence_statement(2, provenance)]);
    assert!(matches!(
        topology::TopologyPlan::construct(
            &block,
            super::super::DeclarationStartContract::ParsedSyntax,
            None,
        ),
        Err(RetainedValidationError::UnexpectedRealMatrixMultiplyOccurrence)
    ));

    block.do_step.statements = vec![matrix_occurrence_statement(3, provenance)];
    let error = match topology::TopologyPlan::construct(
        &block,
        super::super::DeclarationStartContract::GeneratedPackage,
        Some(matrix_profile()),
    ) {
        Err(error) => error,
        Ok(_) => panic!("a changed contract must not acquire occurrence authority"),
    };
    assert!(matches!(
        error,
        RetainedValidationError::InvalidRealMatrixMultiplyOccurrence {
            provenance: SubjectProvenance::Exact(found),
            ..
        } if found == provenance
    ));
}

#[test]
fn duplicate_fact_installation_is_refused() {
    let mut slot = RequiredFact::Pending;
    install_fact(&mut slot, Ty::Scalar(galec::ScalarType::Real), "type", 0)
        .expect("first fact installation");
    assert_eq!(
        install_fact(&mut slot, Ty::Scalar(galec::ScalarType::Real), "type", 0),
        Err(RetainedValidationError::DuplicateFact {
            family: "type",
            index: 0,
        })
    );
}

#[test]
fn builtin_resolution_orphan_fault_leaves_zero_builder_residue() {
    let builtin = crate::builtins::BUILTINS
        .iter()
        .find(|builtin| builtin.name == "sin")
        .expect("normative sin builtin");
    let block = builtin_call_block(builtin.name);
    let mut builder = RetainedValidationBuilder::install(
        &block,
        super::super::DeclarationStartContract::ParsedSyntax,
        None,
    )
    .expect("install call");
    let base = builtin_loc(builtin).expect("catalog identity");
    let (prepared, results) = builder
        .prepare_builtin_results(base, 0, builtin.outputs.len())
        .expect("prepare local result subjects");
    let (inputs, mut outputs) =
        builtin_call_signature(builtin, 0).expect("prepare canonical signature");
    outputs.clear();
    let malformed = CallResolution {
        target: CallTarget::Builtin {
            base,
            lifted_rank: 0,
        },
        inputs,
        outputs,
        results,
        stateful: false,
    };
    let before_results = builder.retained.builtin_results.clone();
    let before_capacity = builder.retained.builtin_results.capacity();
    let before_fact = builder.retained.calls[0].resolution.clone();
    let before_capabilities = builder.capabilities.clone();
    let result = builder.commit_prepared_call_resolution(
        0,
        PreparedCallResolution {
            resolution: malformed,
            builtin_results: prepared,
        },
    );

    assert!(matches!(
        result,
        Err(RetainedValidationError::InconsistentFact {
            family: "call-signature",
            ..
        })
    ));
    assert_eq!(builder.retained.builtin_results, before_results);
    assert_eq!(builder.retained.builtin_results.capacity(), before_capacity);
    assert_eq!(builder.retained.calls[0].resolution, before_fact);
    assert_eq!(builder.capabilities, before_capabilities);
}

#[test]
fn duplicate_builtin_resolution_cannot_append_orphan_results() {
    let builtin = crate::builtins::BUILTINS
        .iter()
        .find(|builtin| builtin.name == "sin")
        .expect("normative sin builtin");
    let block = builtin_call_block(builtin.name);
    let call = sole_call(&block);
    let context = super::super::context::BlockContext::new(&block);
    let resolved = super::super::context::resolve_call(&context, call)
        .expect("exact call occurrence resolves");
    let mut builder = RetainedValidationBuilder::install(
        &block,
        super::super::DeclarationStartContract::ParsedSyntax,
        None,
    )
    .expect("install call");
    builder
        .record_call_resolution(&resolved)
        .expect("first resolution commits atomically");
    let committed_results = builder.retained.builtin_results.clone();
    let committed_capacity = builder.retained.builtin_results.capacity();
    let committed_fact = builder.retained.calls[0].resolution.clone();
    let committed_capabilities = builder.capabilities.clone();

    assert_eq!(
        builder.record_call_resolution(&resolved),
        Err(RetainedValidationError::DuplicateFact {
            family: "call-resolution",
            index: 0,
        })
    );
    assert_eq!(builder.retained.builtin_results, committed_results);
    assert_eq!(
        builder.retained.builtin_results.capacity(),
        committed_capacity
    );
    assert_eq!(builder.retained.calls[0].resolution, committed_fact);
    assert_eq!(builder.capabilities, committed_capabilities);
}

#[test]
fn same_spelled_forged_builtin_cannot_acquire_catalog_identity() {
    static FORGED_OUTPUTS: &[crate::builtins::BuiltinParam] = &[crate::builtins::BuiltinParam {
        name: "y",
        ty: crate::builtins::BuiltinType::Integer,
    }];
    static FORGED_SIN: crate::builtins::Builtin = crate::builtins::Builtin {
        name: "sin",
        inputs: &[],
        outputs: FORGED_OUTPUTS,
        signals: &[],
    };
    assert_eq!(
        builtin_loc(&FORGED_SIN),
        Err(RetainedValidationError::MissingBuiltin)
    );
}

#[test]
fn foreign_prepared_builtin_bundle_leaves_zero_builder_residue() {
    let sin = crate::builtins::BUILTINS
        .iter()
        .find(|builtin| builtin.name == "sin")
        .expect("normative sin builtin");
    let cos = crate::builtins::BUILTINS
        .iter()
        .find(|builtin| builtin.name == "cos")
        .expect("normative cos builtin");
    let block = builtin_call_block(sin.name);
    let mut builder = RetainedValidationBuilder::install(
        &block,
        super::super::DeclarationStartContract::ParsedSyntax,
        None,
    )
    .expect("install call");
    let sin_loc = builtin_loc(sin).expect("sin identity");
    let cos_loc = builtin_loc(cos).expect("cos identity");
    let (mut subjects, results) = builder
        .prepare_builtin_results(sin_loc, 0, sin.outputs.len())
        .expect("prepare local result subjects");
    subjects[0].base = cos_loc;
    let (inputs, outputs) = builtin_call_signature(sin, 0).expect("prepare canonical signature");
    let prepared = PreparedCallResolution {
        resolution: CallResolution {
            target: CallTarget::Builtin {
                base: sin_loc,
                lifted_rank: 0,
            },
            inputs,
            outputs,
            results,
            stateful: false,
        },
        builtin_results: subjects,
    };
    let before_results = builder.retained.builtin_results.clone();
    let before_capacity = builder.retained.builtin_results.capacity();
    let before_fact = builder.retained.calls[0].resolution.clone();
    let before_capabilities = builder.capabilities.clone();

    assert_eq!(
        builder.commit_prepared_call_resolution(0, prepared),
        Err(RetainedValidationError::InconsistentFact {
            family: "prepared-builtin-results",
            index: 0,
        })
    );
    assert_eq!(builder.retained.builtin_results, before_results);
    assert_eq!(builder.retained.builtin_results.capacity(), before_capacity);
    assert_eq!(builder.retained.calls[0].resolution, before_fact);
    assert_eq!(builder.capabilities, before_capabilities);
}

#[test]
fn missing_fact_prevents_root_close() {
    let mut block = galec::Block::new(galec::Name::ident("MissingExpressionFact"));
    block.do_step.locals = vec![array_declaration(galec::ScalarType::Real, "value", &[])];
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::Assignment {
        target: galec::Reference::local(galec::Name::ident("value")),
        value: galec::Expression::Real(1.0),
    })];
    let mut builder = RetainedValidationBuilder::install(
        &block,
        super::super::DeclarationStartContract::ParsedSyntax,
        None,
    )
    .expect("install subjects");
    builder
        .close_user_call_graph()
        .expect("empty call graph closes");
    assert_eq!(
        builder
            .finish()
            .expect_err("missing expression fact must fail"),
        RetainedValidationError::MissingFact {
            family: "expression-type",
            index: 0,
        }
    );
}

#[test]
fn missing_provenance_prevents_close() {
    assert_eq!(
        require_provenance(SubjectProvenance::Missing(MissingProvenance::FaultInjected,)),
        Err(RetainedValidationError::InconsistentFact {
            family: "provenance",
            index: 0,
        })
    );
}

#[test]
fn user_call_graph_is_closed_once_from_every_retained_executable_call() {
    let call_statement = |name: &str| {
        galec::Spanned::dummy(galec::Statement::Call(galec::FunctionCall {
            function: galec::Name::ident(name),
            arguments: Vec::new(),
        }))
    };
    let function = |name: &str, statements| galec::UserFunction {
        kind: galec::FunctionKind::Stateless,
        name: galec::Name::ident(name),
        signals: Vec::new(),
        parameters: Vec::new(),
        locals: Vec::new(),
        statements,
        span: Span::DUMMY,
    };
    let mut block = galec::Block::new(galec::Name::ident("CallGraph"));
    block.protected_functions = vec![
        function(
            "outer",
            vec![galec::Spanned::dummy(galec::Statement::If(
                galec::IfStatement {
                    branches: vec![galec::IfBranch {
                        condition: galec::Condition::Expression(galec::Expression::Bool(true)),
                        body: vec![call_statement("leaf")],
                        span: Span::DUMMY,
                    }],
                    else_body: None,
                },
            ))],
        ),
        function("leaf", Vec::new()),
    ];
    block.do_step.statements = vec![call_statement("outer"), call_statement("leaf")];

    let mut builder = RetainedValidationBuilder::install(
        &block,
        super::super::DeclarationStartContract::ParsedSyntax,
        None,
    )
    .expect("install subjects");
    assert_eq!(
        builder.close_user_call_graph(),
        Err(RetainedValidationError::MissingFact {
            family: "call-resolution",
            index: 0,
        }),
        "the graph cannot close before type analysis classifies every call"
    );
    let ctx = super::super::context::BlockContext::new(&block);
    let mut diagnostics = Vec::new();
    super::super::types::check(
        &ctx,
        super::super::DeclarationStartContract::ParsedSyntax,
        &mut builder,
        &mut diagnostics,
    )
    .expect("retain call resolution");
    assert!(diagnostics.is_empty(), "{diagnostics:#?}");
    assert!(builder
        .retained
        .calls
        .iter()
        .all(|call| call.result_set.checked().is_some() && projection_locators(call).is_empty()));

    builder.close_user_call_graph().expect("close graph once");
    let graph = builder.user_call_graph().expect("closed graph");
    assert_eq!(graph.methods[2].len(), 2);
    assert_eq!(graph.functions[0].len(), 1);
    assert_eq!(graph.functions[1].len(), 0);
    assert_eq!(
        graph.functions[0][0].path.as_ref(),
        [
            CallPathSegment::Statement(0),
            CallPathSegment::Branch(0),
            CallPathSegment::Statement(0),
        ]
    );
    assert_eq!(
        builder.close_user_call_graph(),
        Err(RetainedValidationError::DuplicateFact {
            family: "user-call-graph",
            index: 0,
        })
    );
}

#[test]
fn two_same_shaped_results_keep_independent_origins_and_receivers() {
    let block = two_result_block([2, 2], false);
    let (builder, diagnostics) = retain_types(&block).expect("retain exact call relations");
    assert!(diagnostics.is_empty(), "{diagnostics:#?}");
    let call = &builder.retained.calls[0];
    let result_set = call.result_set.checked().expect("closed result set");
    let projections = projection_locators(call);
    let [first, second] = projections.as_slice() else {
        panic!("two-output call must issue two projections");
    };
    assert_ne!(first, second, "projection identity is not an ordinal pair");
    let RequiredFact::Checked(CallResolutionFact::Known(resolution)) = &call.resolution else {
        panic!("call resolution")
    };
    assert_ne!(resolution.results[0], resolution.results[1]);
    assert_ne!(
        builder.retained.projection_receiver(*first),
        builder.retained.projection_receiver(*second)
    );
    assert_eq!(builder.retained.projection_order(*first), 0);
    assert_eq!(builder.retained.projection_order(*second), 1);
    assert_eq!(result_set.count, 2);
    assert_eq!(
        builder.retained.projection_shape(*first).extents.as_ref(),
        [2]
    );
    assert_eq!(
        builder.retained.projection_shape(*second).extents.as_ref(),
        [2]
    );
}

#[test]
fn bare_call_issues_one_explicit_discard_per_result() {
    let block = two_result_block([2, 2], true);
    let (builder, diagnostics) = retain_types(&block).expect("retain discarded results");
    assert!(diagnostics.is_empty(), "{diagnostics:#?}");
    let projections = projection_locators(&builder.retained.calls[0]);
    assert_eq!(projections.len(), 2);
    for locator in &projections {
        assert_eq!(
            builder.retained.projection_receiver(*locator),
            CallResultReceiverLoc::DiscardedBy(StatementLoc(0))
        );
    }
}

#[test]
fn same_rank_extent_mismatch_is_rejected_during_relation_construction() {
    let block = two_result_block([2, 3], false);
    let (_, diagnostics) = retain_types(&block).expect("shape mismatch is a language error");
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].code(), "EG017");
}

#[test]
fn call_result_counter_reports_the_exact_outstanding_call() {
    let builtin = crate::builtins::BUILTINS
        .iter()
        .find(|builtin| builtin.name == "sin")
        .expect("normative sin builtin");
    let block = builtin_call_block(builtin.name);
    let call = sole_call(&block);
    let context = super::super::context::BlockContext::new(&block);
    let resolved = super::super::context::resolve_call(&context, call)
        .expect("exact call occurrence resolves");
    let mut builder = RetainedValidationBuilder::install(
        &block,
        super::super::DeclarationStartContract::ParsedSyntax,
        None,
    )
    .expect("install call");
    builder
        .record_call_resolution(&resolved)
        .expect("issue exact call-result capability");
    builder
        .capabilities
        .consume_call_result_fact_only(0)
        .expect("fault consume only the parallel fact");
    builder
        .close_user_call_graph()
        .expect("builtin-only graph closes");

    assert_eq!(
        builder
            .finish()
            .expect_err("outstanding call-result capability must fail"),
        RetainedValidationError::InconsistentFact {
            family: "call-result-capability",
            index: 0,
        }
    );
}

#[test]
fn builtin_census_and_outstanding_capability_are_not_silent() {
    let builtin = crate::builtins::BUILTINS
        .iter()
        .find(|builtin| builtin.name == "sin")
        .expect("normative sin builtin");
    let block = builtin_call_block(builtin.name);
    let call = sole_call(&block);
    let context = super::super::context::BlockContext::new(&block);
    let resolved = super::super::context::resolve_call(&context, call)
        .expect("exact call occurrence resolves");
    let mut builder = RetainedValidationBuilder::install(
        &block,
        super::super::DeclarationStartContract::ParsedSyntax,
        None,
    )
    .expect("install call");
    let before = builder.retained.subject_count();
    builder
        .record_call_resolution(&resolved)
        .expect("resolution installs builtin result atomically");
    assert_eq!(
        builder.retained.subject_count(),
        before + builtin.outputs.len()
    );
    builder
        .close_discarded_call_results(call)
        .expect("close the exact result projection");
    builder
        .close_user_call_graph()
        .expect("builtin-only call graph closes");

    builder
        .capabilities
        .issue_unconsumed_builtin_result()
        .expect("inject exact outstanding census capability");
    assert_eq!(
        builder.capabilities.finish(),
        Err(RetainedValidationError::InconsistentFact {
            family: "builtin-result-capability",
            index: 1,
        })
    );
}

#[test]
fn every_call_result_reservation_failure_is_semantically_atomic() {
    let builtin = crate::builtins::BUILTINS
        .iter()
        .find(|builtin| builtin.name == "sin")
        .expect("normative sin builtin");
    let families = [
        "call-result-projection-transaction",
        "call-result-child-transaction",
        "call-result-projection-arena",
        "call-result-child-arena",
    ];
    for (reservation, family) in families.into_iter().enumerate() {
        let block = builtin_call_block(builtin.name);
        let call = sole_call(&block);
        let context = super::super::context::BlockContext::new(&block);
        let resolved = super::super::context::resolve_call(&context, call)
            .expect("exact call occurrence resolves");
        let mut builder = RetainedValidationBuilder::install(
            &block,
            super::super::DeclarationStartContract::ParsedSyntax,
            None,
        )
        .expect("install call");
        builder
            .record_call_resolution(&resolved)
            .expect("prepare a known output relation");
        let before_capabilities = builder.capabilities.clone();
        let before_shapes = builder.fixed_shapes.clone();
        let before_children = builder.retained.calls[0].children.clone();
        let before_projections = builder.retained.call_result_projections.clone();
        builder.reservations.fail_at(reservation);

        assert!(matches!(
            builder.close_discarded_call_results(call),
            Err(ShapeRelationError::Index(
                RetainedValidationError::AllocationFailed { family: found }
            )) if found == family
        ));
        assert_eq!(builder.capabilities, before_capabilities);
        assert_eq!(builder.fixed_shapes, before_shapes);
        assert_eq!(builder.retained.calls[0].children, before_children);
        assert_eq!(builder.retained.call_result_projections, before_projections);
        assert!(matches!(
            builder.retained.calls[0].result_set,
            RequiredFact::Pending
        ));
    }
}

#[test]
fn fixed_shape_finalization_is_monotone_and_rejects_double_finalize() {
    let mut expression_block = galec::Block::new(galec::Name::ident("ShapeExpression"));
    expression_block.do_step.locals =
        vec![array_declaration(galec::ScalarType::Real, "value", &[])];
    expression_block.do_step.statements =
        vec![galec::Spanned::dummy(galec::Statement::Assignment {
            target: galec::Reference::local(galec::Name::ident("value")),
            value: galec::Expression::Real(1.0),
        })];
    let mut builder = RetainedValidationBuilder::install(
        &expression_block,
        super::super::DeclarationStartContract::ParsedSyntax,
        None,
    )
    .expect("install expression");
    builder
        .fixed_shapes
        .fault_finalize_expression(0)
        .expect("first finalization");
    assert_eq!(
        builder.fixed_shapes.fault_finalize_expression(0),
        Err(RetainedValidationError::DuplicateFact {
            family: "expression-fixed-shape-finalization",
            index: 0,
        })
    );
}

#[test]
fn known_assignment_types_cannot_hide_a_missing_reference_shape_fact() {
    let mut block = galec::Block::new(galec::Name::ident("MissingKnownShape"));
    block.do_step.locals = vec![array_declaration(galec::ScalarType::Real, "value", &[])];
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::Assignment {
        target: galec::Reference::local(galec::Name::ident("value")),
        value: galec::Expression::Real(1.0),
    })];
    let galec::Statement::Assignment { target, value } = &block.do_step.statements[0].node else {
        panic!("fixture has one assignment")
    };
    let body = super::super::context::BlockContext::new(&block)
        .bodies()
        .into_iter()
        .find(|body| body.method == Some(galec::BlockMethodKind::DoStep))
        .expect("DoStep body exists");
    let scope = super::super::context::FunctionScope::new(&body);
    let context = super::super::context::BlockContext::new(&block);
    let resolved = super::super::context::resolve(&context, &scope, target)
        .expect("target has a known Real type");
    let mut builder = RetainedValidationBuilder::install(
        &block,
        super::super::DeclarationStartContract::ParsedSyntax,
        None,
    )
    .expect("install assignment subjects");
    builder
        .record_reference_resolution(target, &resolved)
        .expect("install known target type and shape");
    builder
        .record_expression_type(value, Ty::Scalar(galec::ScalarType::Real))
        .expect("install known value type and shape");

    builder.retained.references[0].fixed_shape = RequiredFact::Pending;
    assert!(matches!(
        builder.require_assignment_shape(target, value),
        Err(ShapeRelationError::Index(
            RetainedValidationError::MissingFact {
                family: "reference-fixed-shape",
                index: 0,
            }
        ))
    ));
}

#[test]
fn parsed_start_absence_is_an_explicit_syntax_only_fact() {
    let mut block = galec::Block::new(galec::Name::ident("ParsedStartDisposition"));
    block.interface = vec![galec::InterfaceVariable {
        kind: galec::InterfaceKind::Input,
        decl: array_declaration(galec::ScalarType::Real, "value", &[]),
        start: None,
    }];
    let builder = RetainedValidationBuilder::install(
        &block,
        super::super::DeclarationStartContract::ParsedSyntax,
        None,
    )
    .expect("install parsed declaration");
    assert!(matches!(
        builder.retained.declarations[0].start,
        RequiredFact::Checked(DeclarationStartFact::NotRepresentedInSyntax)
    ));
}

#[test]
fn prepared_capability_guards_are_compiler_proven_non_clone_and_non_copy() {
    trait AmbiguousIfClone<A> {
        fn marker() {}
    }
    impl<T: ?Sized> AmbiguousIfClone<()> for T {}
    struct CloneMarker;
    impl<T: Clone> AmbiguousIfClone<CloneMarker> for T {}

    trait AmbiguousIfCopy<A> {
        fn marker() {}
    }
    impl<T: ?Sized> AmbiguousIfCopy<()> for T {}
    struct CopyMarker;
    impl<T: Copy> AmbiguousIfCopy<CopyMarker> for T {}

    macro_rules! assert_affine {
        ($ty:ty) => {{
            let _ = <$ty as AmbiguousIfClone<_>>::marker;
            let _ = <$ty as AmbiguousIfCopy<_>>::marker;
        }};
    }
    assert_affine!(capabilities::PreparedExpressionCapabilities<'static>);
    assert_affine!(capabilities::PreparedReferenceCapabilities<'static>);
    assert_affine!(capabilities::PreparedCallResolutionCapabilities<'static>);
    assert_affine!(capabilities::PreparedCallResultsCapabilities<'static>);
    assert_affine!(capabilities::PreparedUserCallGraph<'static>);
    assert_affine!(shapes::PreparedFixedShapeFinalization<'static>);
}

fn function_body<'a>(source: &'a str, signature: &str) -> &'a str {
    let signature_start = source.find(signature).expect("function signature exists");
    let body_start = signature_start
        + source[signature_start..]
            .find('{')
            .expect("function body starts");
    let mut depth = 0_u32;
    for (offset, byte) in source.as_bytes()[body_start..].iter().enumerate() {
        match byte {
            b'{' => depth += 1,
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    return &source[body_start..=body_start + offset];
                }
            }
            _ => {}
        }
    }
    panic!("function body closes")
}

fn function_bodies<'a>(source: &'a str, signature: &str) -> Vec<&'a str> {
    let mut bodies = Vec::new();
    let mut offset = 0;
    while let Some(found) = source[offset..].find(signature) {
        let start = offset + found;
        let body = function_body(&source[start..], signature);
        offset = start + body.len();
        bodies.push(body);
    }
    bodies
}

fn close_backstop_violations(body: &str) -> Vec<&'static str> {
    ["for ", "while ", ".iter(", ".into_iter(", ".states"]
        .into_iter()
        .filter(|forbidden| body.contains(forbidden))
        .collect()
}

#[test]
fn finish_is_a_fixed_size_capability_check_without_retired_scanners() {
    let parent_source = include_str!("../retained.rs");
    let builder_source = include_str!("builder.rs");
    let capability_source = include_str!("capabilities.rs");
    let shape_source = include_str!("shapes.rs");
    let topology_source = include_str!("topology.rs");
    let production_source = [
        parent_source,
        builder_source,
        capability_source,
        shape_source,
    ]
    .concat();
    // This catalog scan is a regression backstop. The behavioral and type
    // tests below are the proof that close cannot silently accept a gap.
    for retired in [
        "exact_edges",
        "require_closed_facts",
        "require_call_result_capability_exhaustion",
        "require_builtin_result_capability_exhaustion",
        "require_subject_facts",
        "append_child_edges",
        "require_known_call_signature",
        "invalid_expression_type",
        "invalid_call_resolution",
    ] {
        assert!(
            !production_source.contains(retired),
            "retired closure scanner `{retired}` must not return"
        );
    }

    let finish = function_body(builder_source, "pub(in crate::validate) fn finish(");
    assert!(finish.contains("self.capabilities.finish()?;"));
    assert!(finish.contains("self.fixed_shapes.finish()?;"));
    for forbidden in [
        "for ",
        "while ",
        ".iter(",
        ".into_iter(",
        ".declarations",
        ".statements",
        ".expressions",
        ".references",
        ".calls",
    ] {
        assert!(
            !finish.contains(forbidden),
            "finish must not contain arena traversal `{forbidden}`"
        );
    }

    let capability_finish = function_body(capability_source, "pub(super) fn finish(&self)");
    for required in [
        "self.facts.finish()?;",
        "self.call_results.finish()?;",
        "self.builtin_results.finish()",
    ] {
        assert!(
            capability_finish.contains(required),
            "root capability close must retain `{required}`"
        );
    }
    for forbidden in ["for ", "while ", ".iter(", ".into_iter(", ".states"] {
        assert!(
            !capability_finish.contains(forbidden),
            "root capability close must remain fixed-size: `{forbidden}`"
        );
    }
    let shape_finish = function_body(shape_source, "pub(super) fn finish(&self)");
    for forbidden in ["for ", "while ", ".iter(", ".into_iter(", ".states"] {
        assert!(!shape_finish.contains(forbidden));
    }
    assert!(!topology_source.contains(".. }"));
    assert!(!topology_source.contains("..}"));
    assert!(!builder_source.contains("fn block("));
    assert!(!production_source.contains("expected::"));
}

#[test]
fn close_scan_backstop_detects_an_injected_arena_walk() {
    let clean = "fn finish(&self) { self.receipt.close(); }";
    let mutated = "fn finish(&self) { for item in self.arena.iter() { check(item); } }";
    assert!(close_backstop_violations(function_body(clean, "fn finish")).is_empty());
    assert_eq!(
        close_backstop_violations(function_body(mutated, "fn finish")),
        ["for ", ".iter("]
    );
}

#[test]
fn every_prepared_commit_is_an_infallible_non_indexing_assignment_boundary() {
    for source in [
        include_str!("builder.rs"),
        include_str!("capabilities.rs"),
        include_str!("shapes.rs"),
    ] {
        let commits = function_bodies(source, "fn commit(");
        assert!(!commits.is_empty());
        for commit in commits {
            for forbidden in [
                "Result<",
                "panic!",
                "unreachable!",
                ".unwrap(",
                ".expect(",
                "[index]",
                "get_mut(",
            ] {
                assert!(
                    !commit.contains(forbidden),
                    "prepared commit contains fallible/indexing operation `{forbidden}`: {commit}"
                );
            }
        }
    }
}

#[test]
fn each_subject_family_has_one_auditable_arena_insertion_authority() {
    let topology_source = include_str!("topology.rs");
    for arena_push in [
        "self.retained.declarations.push(",
        "self.retained.methods.push(",
        "self.retained.functions.push(",
        "self.retained.statements.push(",
        "self.retained.binders.push(",
        "self.retained.expressions.push(",
        "self.retained.references.push(",
        "self.retained.calls.push(",
    ] {
        assert_eq!(
            topology_source.matches(arena_push).count(),
            1,
            "each subject family must retain one auditable insertion authority: `{arena_push}`"
        );
    }
}

fn maximal_topology_expression() -> galec::Expression {
    let call = galec::Expression::Call(galec::FunctionCall {
        function: galec::Name::ident("sin"),
        arguments: vec![galec::Expression::Real(1.0)],
    });
    let selection = galec::Expression::If(galec::IfExpression::new(
        vec![
            (
                galec::Expression::Bool(true),
                galec::Expression::Paren(Box::new(call)),
            ),
            (
                galec::Expression::Bool(false),
                galec::Expression::Ref(galec::Reference::local(galec::Name::ident("value"))),
            ),
        ],
        galec::Expression::Array(vec![
            galec::Expression::Neg(galec::Reference::local(galec::Name::ident("value"))),
            galec::Expression::Not(Box::new(galec::Expression::Bool(false))),
        ]),
    ));
    galec::Expression::binary(
        galec::BinaryOp::Add,
        selection,
        galec::Expression::Size {
            array: galec::Reference::local(galec::Name::ident("values")),
            dimension: Box::new(galec::Expression::Integer(1)),
        },
    )
}

fn maximal_topology_block() -> galec::Block {
    let mut block = two_result_block([2, 2], false);
    block.interface.push(galec::InterfaceVariable {
        kind: galec::InterfaceKind::Input,
        decl: array_declaration(galec::ScalarType::Real, "input", &[]),
        start: Some(galec::Expression::Real(0.0)),
    });
    let mut ranged = array_declaration(galec::ScalarType::Real, "values", &[2]);
    ranged.range = galec::RangeAttributes {
        min: Some(galec::Expression::Real(-1.0)),
        max: Some(galec::Expression::Real(1.0)),
    };
    block.protected.push(galec::ProtectedEntity {
        kind: galec::ProtectedKind::State,
        decl: ranged,
        start: Some(galec::Expression::Array(vec![
            galec::Expression::Real(0.0),
            galec::Expression::Real(0.0),
        ])),
    });
    block.compartments.push(galec::StateCompartment {
        name: galec::Name::ident("State"),
        entities: vec![galec::ProtectedEntity {
            kind: galec::ProtectedKind::Constant,
            decl: array_declaration(galec::ScalarType::Integer, "member", &[]),
            start: Some(galec::Expression::Integer(1)),
        }],
        span: Span::DUMMY,
    });
    block
        .do_step
        .locals
        .push(array_declaration(galec::ScalarType::Real, "value", &[]));
    block
        .do_step
        .statements
        .extend(maximal_topology_statements());
    block
        .startup
        .statements
        .push(galec::Spanned::dummy(galec::Statement::Assignment {
            target: galec::Reference::local(galec::Name::ident("value")),
            value: galec::Expression::Real(0.0),
        }));
    block
}

fn maximal_topology_statements() -> Vec<galec::Spanned<galec::Statement>> {
    let assignment = galec::Statement::Assignment {
        target: galec::Reference::local(galec::Name::ident("value")),
        value: maximal_topology_expression(),
    };
    let conditional = galec::Statement::If(galec::IfStatement {
        branches: vec![
            galec::IfBranch {
                condition: galec::Condition::Expression(galec::Expression::Bool(true)),
                body: vec![galec::Spanned::dummy(galec::Statement::Signal(vec![
                    galec::Identifier::new("NAN"),
                ]))],
                span: Span::DUMMY,
            },
            galec::IfBranch {
                condition: galec::Condition::SignalCheck(galec::SignalCheck {
                    closure: Some(galec::Identifier::new("caught")),
                    test: Some(galec::SignalTest {
                        negated: false,
                        signals: vec![galec::Identifier::new("NAN")],
                    }),
                    fallback: Some(galec::Expression::Bool(false)),
                }),
                body: Vec::new(),
                span: Span::DUMMY,
            },
        ],
        else_body: Some(vec![galec::Spanned::dummy(galec::Statement::Limit(vec![
            galec::LimitTarget::SelfState,
            galec::LimitTarget::Reference(galec::Reference::local(galec::Name::ident("value"))),
        ]))]),
    });
    let loop_ = galec::Statement::for_loop(galec::ForLoop::new(
        Some(galec::Name::ident("index")),
        galec::Expression::Integer(1),
        Some(galec::Expression::Integer(1)),
        galec::Expression::Integer(2),
        vec![galec::Spanned::dummy(galec::Statement::Call(
            galec::FunctionCall {
                function: galec::Name::ident("tick"),
                arguments: vec![galec::Expression::Ref(galec::Reference::local(
                    galec::Name::ident("value"),
                ))],
            },
        ))],
    ));
    [assignment, conditional, loop_]
        .into_iter()
        .map(galec::Spanned::dummy)
        .collect()
}

fn topology_subjects(retained: &RetainedValidation) -> Vec<SubjectLoc> {
    let mut subjects = Vec::new();
    subjects.extend(
        (0..retained.declarations.len())
            .map(|index| SubjectLoc::Declaration(DeclarationLoc(index as u32))),
    );
    subjects.extend(
        (0..retained.methods.len()).map(|index| SubjectLoc::Method(MethodLoc(index as u8))),
    );
    subjects.extend(
        (0..retained.functions.len()).map(|index| SubjectLoc::Function(FunctionLoc(index as u32))),
    );
    subjects.extend(
        (0..retained.binders.len()).map(|index| SubjectLoc::Binder(BinderLoc(index as u32))),
    );
    subjects.extend(
        (0..retained.statements.len())
            .map(|index| SubjectLoc::Statement(StatementLoc(index as u32))),
    );
    subjects.extend(
        (0..retained.expressions.len())
            .map(|index| SubjectLoc::Expression(ExpressionLoc(index as u32))),
    );
    subjects.extend(
        (0..retained.references.len())
            .map(|index| SubjectLoc::Reference(ReferenceLoc(index as u32))),
    );
    subjects.extend((0..retained.calls.len()).map(|index| SubjectLoc::Call(CallLoc(index as u32))));
    subjects
}

fn topology_children(
    retained: &RetainedValidation,
    owner: SubjectLoc,
) -> &[(SubjectLoc, ChildRole)] {
    match owner {
        SubjectLoc::Declaration(loc) => &retained.declarations[loc.0 as usize].children,
        SubjectLoc::Method(loc) => &retained.methods[loc.0 as usize].children,
        SubjectLoc::Function(loc) => &retained.functions[loc.0 as usize].children,
        SubjectLoc::Statement(loc) => &retained.statements[loc.0 as usize].children,
        SubjectLoc::Expression(loc) => &retained.expressions[loc.0 as usize].children,
        SubjectLoc::Reference(loc) => &retained.references[loc.0 as usize].children,
        SubjectLoc::Call(loc) => &retained.calls[loc.0 as usize].children,
        SubjectLoc::Binder(_) | SubjectLoc::CallResultProjection(_) => &[],
    }
}

#[test]
fn maximal_ast_topology_installs_every_subject_once_under_its_exact_parent() {
    let block = maximal_topology_block();
    let builder = RetainedValidationBuilder::install(
        &block,
        super::super::DeclarationStartContract::ParsedSyntax,
        None,
    )
    .expect("complete topology plan");
    let retained = &builder.retained;
    let subjects = topology_subjects(retained);
    assert!(retained.declarations.len() >= 8);
    assert!(!retained.functions.is_empty());
    assert!(!retained.binders.is_empty());
    assert!(retained.statements.len() >= 8);
    assert!(retained.expressions.len() >= 20);
    assert!(retained.references.len() >= 8);
    assert!(retained.calls.len() >= 3);

    let mut incoming = subjects
        .iter()
        .copied()
        .map(|subject| (subject, 0_usize))
        .collect::<FxHashMap<_, _>>();
    for &(child, role) in &retained.root_children {
        *incoming
            .get_mut(&child)
            .expect("root child is in the census") += 1;
        assert_eq!(
            retained.subject_parent(child),
            Ok(SubjectParent::Block(role))
        );
    }
    for owner in subjects.iter().copied() {
        for &(child, role) in topology_children(retained, owner) {
            *incoming
                .get_mut(&child)
                .expect("nested child is in the census") += 1;
            assert_eq!(
                retained.subject_parent(child),
                Ok(SubjectParent::child(owner, role))
            );
        }
    }
    assert!(incoming.values().all(|count| *count == 1));
    assert_eq!(incoming.len(), subjects.len());
}

#[test]
fn declaration_classes_are_fixed_by_the_sole_topology_traversal() {
    let block = declaration_classes_fixture();

    let builder = RetainedValidationBuilder::install(
        &block,
        super::super::DeclarationStartContract::ParsedSyntax,
        None,
    )
    .expect("install declarations");
    let classes = builder
        .retained
        .declarations
        .iter()
        .map(|subject| subject.class)
        .collect::<Vec<_>>();
    assert_eq!(
        classes,
        vec![
            DeclarationClass::Input,
            DeclarationClass::Output,
            DeclarationClass::TunableParameter,
            DeclarationClass::DependentParameter,
            DeclarationClass::Constant,
            DeclarationClass::PersistentState,
            DeclarationClass::CompartmentDependentParameter,
            DeclarationClass::CompartmentConstant,
            DeclarationClass::CompartmentPersistentState,
            DeclarationClass::MethodLocal,
            DeclarationClass::FunctionInput,
            DeclarationClass::FunctionOutput,
            DeclarationClass::FunctionLocal,
        ]
    );
}

fn declaration_classes_fixture() -> galec::Block {
    let mut block = galec::Block::new(galec::Name::ident("DeclarationClasses"));
    block.interface = vec![
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Input,
            decl: array_declaration(galec::ScalarType::Real, "input", &[]),
            start: None,
        },
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Output,
            decl: array_declaration(galec::ScalarType::Real, "output", &[]),
            start: None,
        },
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::TunableParameter,
            decl: array_declaration(galec::ScalarType::Real, "tunable", &[]),
            start: None,
        },
    ];
    block.protected = vec![
        galec::ProtectedEntity {
            kind: galec::ProtectedKind::DependentParameter,
            decl: array_declaration(galec::ScalarType::Real, "dependent", &[]),
            start: None,
        },
        galec::ProtectedEntity {
            kind: galec::ProtectedKind::Constant,
            decl: array_declaration(galec::ScalarType::Real, "constant", &[]),
            start: None,
        },
        galec::ProtectedEntity {
            kind: galec::ProtectedKind::State,
            decl: array_declaration(galec::ScalarType::Real, "state", &[]),
            start: None,
        },
    ];
    block.compartments = vec![galec::StateCompartment {
        name: galec::Name::ident("Compartment"),
        entities: vec![
            galec::ProtectedEntity {
                kind: galec::ProtectedKind::DependentParameter,
                decl: array_declaration(galec::ScalarType::Real, "member_dependent", &[]),
                start: None,
            },
            galec::ProtectedEntity {
                kind: galec::ProtectedKind::Constant,
                decl: array_declaration(galec::ScalarType::Real, "member_constant", &[]),
                start: None,
            },
            galec::ProtectedEntity {
                kind: galec::ProtectedKind::State,
                decl: array_declaration(galec::ScalarType::Real, "member_state", &[]),
                start: None,
            },
        ],
        span: Span::DUMMY,
    }];
    block.startup.locals = vec![array_declaration(
        galec::ScalarType::Real,
        "method_local",
        &[],
    )];
    block.public_functions = vec![galec::UserFunction {
        kind: galec::FunctionKind::Stateless,
        name: galec::Name::ident("classify"),
        signals: Vec::new(),
        parameters: vec![
            galec::Parameter {
                direction: galec::Direction::Input,
                decl: array_declaration(galec::ScalarType::Real, "function_input", &[]),
            },
            galec::Parameter {
                direction: galec::Direction::Output,
                decl: array_declaration(galec::ScalarType::Real, "function_output", &[]),
            },
        ],
        locals: vec![array_declaration(
            galec::ScalarType::Real,
            "function_local",
            &[],
        )],
        statements: Vec::new(),
        span: Span::DUMMY,
    }];
    block
}

#[test]
fn lifted_builtin_result_retains_its_exact_argument_extents() {
    let mut block = galec::Block::new(galec::Name::ident("LiftedShape"));
    block.do_step.locals = vec![array_declaration(galec::ScalarType::Real, "values", &[3])];
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::Call(
        galec::FunctionCall {
            function: galec::Name::ident("sin1D"),
            arguments: vec![galec::Expression::Ref(galec::Reference::local(
                galec::Name::ident("values"),
            ))],
        },
    ))];
    let (builder, diagnostics) = retain_types(&block).expect("retain lifted result shape");
    assert!(diagnostics.is_empty(), "{diagnostics:#?}");
    let projections = projection_locators(&builder.retained.calls[0]);
    assert_eq!(projections.len(), 1);
    assert_eq!(
        builder
            .retained
            .projection_shape(projections[0])
            .extents
            .as_ref(),
        [3]
    );
}

#[test]
fn lu_factorization_results_retain_matrix_and_pivot_shapes() {
    let mut block = galec::Block::new(galec::Name::ident("LuShape"));
    block.do_step.locals = vec![array_declaration(
        galec::ScalarType::Real,
        "matrix",
        &[3, 3],
    )];
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::Call(
        galec::FunctionCall {
            function: galec::Name::ident("luFactorize"),
            arguments: vec![galec::Expression::Ref(galec::Reference::local(
                galec::Name::ident("matrix"),
            ))],
        },
    ))];
    let (builder, diagnostics) = retain_types(&block).expect("retain LU result shapes");
    assert!(diagnostics.is_empty(), "{diagnostics:#?}");
    let projections = projection_locators(&builder.retained.calls[0]);
    let shapes = projections
        .iter()
        .map(|locator| builder.retained.projection_shape(*locator).extents.as_ref())
        .collect::<Vec<_>>();
    assert_eq!(shapes[0], [3, 3]);
    assert_eq!(shapes[1], [3]);
}

#[test]
fn inconsistent_linear_solve_extents_fail_at_the_single_shape_check() {
    let mut block = galec::Block::new(galec::Name::ident("SolveShape"));
    block.do_step.locals = vec![
        array_declaration(galec::ScalarType::Real, "matrix", &[2, 2]),
        array_declaration(galec::ScalarType::Real, "rhs", &[3]),
    ];
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::Call(
        galec::FunctionCall {
            function: galec::Name::ident("solveLinearEquations"),
            arguments: vec![
                galec::Expression::Ref(galec::Reference::local(galec::Name::ident("matrix"))),
                galec::Expression::Ref(galec::Reference::local(galec::Name::ident("rhs"))),
            ],
        },
    ))];
    let (_, diagnostics) = retain_types(&block).expect("shape mismatch is a language error");
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].code(), "EG017");
}
