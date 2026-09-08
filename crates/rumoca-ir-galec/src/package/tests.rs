use super::*;
use crate::ast as galec;

fn positive_zero_arithmetic() -> AlgorithmCodeArithmeticProfile {
    AlgorithmCodeArithmeticProfile::construct(
        AlgorithmCodeRealFormat::Binary64,
        AlgorithmCodeIntegerFormat::I32,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
    )
}

#[test]
fn generated_package_cannot_mint_the_parsed_syntax_start_disposition() {
    let mut block = galec::Block::new(galec::Name::ident("MissingGeneratedStart"));
    block.protected = vec![galec::ProtectedEntity {
        kind: galec::ProtectedKind::Constant,
        decl: galec::VariableDeclaration::scalar(
            galec::ScalarType::Real,
            galec::Name::ident("period"),
        ),
        start: None,
    }];
    let error = AlgorithmCodePackage::construct(
        block,
        AlgorithmCodePackageMetadata::new(
            vec![None],
            "period",
            Vec::new(),
            positive_zero_arithmetic(),
        ),
    )
    .expect_err("a generated package must carry an exact start relation");
    let PackageError::Block(diagnostics) = error else {
        panic!("missing generated start must remain an EG042 language rejection: {error:?}");
    };
    assert_eq!(diagnostics.errors().len(), 1);
    assert_eq!(diagnostics.errors()[0].code(), "EG042");
}

#[test]
fn source_integer_domain_is_derived_from_its_signed_representation() {
    for (format, minimum, maximum) in [
        (
            AlgorithmCodeIntegerFormat::I8,
            i8::MIN as i64,
            i8::MAX as i64,
        ),
        (
            AlgorithmCodeIntegerFormat::I16,
            i16::MIN as i64,
            i16::MAX as i64,
        ),
        (
            AlgorithmCodeIntegerFormat::I32,
            i32::MIN as i64,
            i32::MAX as i64,
        ),
        (AlgorithmCodeIntegerFormat::I64, i64::MIN, i64::MAX),
    ] {
        assert_eq!(format.minimum(), minimum);
        assert_eq!(format.maximum(), maximum);
    }
}

fn package_with_i8_integer_literal(value: i64) -> Result<AlgorithmCodePackage, PackageError> {
    let mut block = galec::Block::new(galec::Name::ident("I8Literal"));
    block.protected = vec![
        galec::ProtectedEntity {
            kind: galec::ProtectedKind::Constant,
            decl: galec::VariableDeclaration::scalar(
                galec::ScalarType::Real,
                galec::Name::ident("period"),
            ),
            start: Some(galec::Expression::Real(0.01)),
        },
        galec::ProtectedEntity {
            kind: galec::ProtectedKind::Constant,
            decl: galec::VariableDeclaration::scalar(
                galec::ScalarType::Integer,
                galec::Name::ident("count"),
            ),
            start: Some(galec::Expression::Integer(value)),
        },
    ];
    AlgorithmCodePackage::construct(
        block,
        AlgorithmCodePackageMetadata::new(
            vec![None, None],
            "period",
            Vec::new(),
            AlgorithmCodeArithmeticProfile::construct(
                AlgorithmCodeRealFormat::Binary64,
                AlgorithmCodeIntegerFormat::I8,
                rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
            ),
        ),
    )
}

#[test]
fn package_construction_refuses_integer_literals_outside_the_source_representation() {
    for boundary in [i64::from(i8::MIN), i64::from(i8::MAX)] {
        let package = package_with_i8_integer_literal(boundary)
            .expect("both signed I8 boundary literals are representable");
        assert_eq!(
            package.arithmetic_profile().source_integer(),
            AlgorithmCodeIntegerFormat::I8
        );
    }

    for overflow in [i64::from(i8::MIN) - 1, i64::from(i8::MAX) + 1] {
        assert_eq!(
            package_with_i8_integer_literal(overflow)
                .expect_err("an out-of-domain literal cannot enter an I8 package"),
            PackageError::IntegerLiteralOutOfDomain {
                value: overflow,
                minimum: i64::from(i8::MIN),
                maximum: i64::from(i8::MAX),
                provenance: SemanticProvenance::Generated(GeneratedSubjectKind::Expression),
            }
        );
    }
}

fn call(name: &str) -> galec::Spanned<galec::Statement> {
    galec::Spanned::dummy(galec::Statement::Call(galec::FunctionCall {
        function: galec::Name::ident(name),
        arguments: Vec::new(),
    }))
}

fn function(name: &str, statements: Vec<galec::Spanned<galec::Statement>>) -> galec::UserFunction {
    galec::UserFunction {
        kind: galec::FunctionKind::Stateless,
        name: galec::Name::ident(name),
        signals: Vec::new(),
        parameters: Vec::new(),
        locals: Vec::new(),
        statements,
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn generated_signal_clauses_propagate_once_in_callee_first_order() {
    let mut block = galec::Block::new(galec::Name::ident("SignalClosure"));
    block.protected = vec![galec::ProtectedEntity {
        kind: galec::ProtectedKind::Constant,
        decl: galec::VariableDeclaration::scalar(
            galec::ScalarType::Real,
            galec::Name::ident("period"),
        ),
        start: Some(galec::Expression::Real(0.01)),
    }];
    block.protected_functions = vec![
        function("caller", vec![call("callee")]),
        function(
            "callee",
            vec![galec::Spanned::dummy(galec::Statement::Signal(vec![
                galec::Identifier::new("INVALID_ARGUMENT"),
            ]))],
        ),
    ];
    block.do_step.statements = vec![call("caller")];

    let package = AlgorithmCodePackage::construct(
        block,
        AlgorithmCodePackageMetadata::new(
            vec![None],
            "period",
            Vec::new(),
            positive_zero_arithmetic(),
        ),
    )
    .expect("generated signal clauses must close before validation");
    let block = package.block();
    assert_eq!(
        block.do_step.signals,
        vec![galec::PredefinedSignal::InvalidArgument]
    );
    for function in &block.protected_functions {
        assert_eq!(
            function.signals,
            vec![galec::Identifier::new("INVALID_ARGUMENT")]
        );
    }
}

#[test]
fn constructed_package_retains_closed_index() {
    let mut block = galec::Block::new(galec::Name::ident("RetainedIndex"));
    block.protected = vec![galec::ProtectedEntity {
        kind: galec::ProtectedKind::Constant,
        decl: galec::VariableDeclaration::scalar(
            galec::ScalarType::Real,
            galec::Name::ident("period"),
        ),
        start: Some(galec::Expression::Real(0.01)),
    }];
    let package = AlgorithmCodePackage::construct(
        block,
        AlgorithmCodePackageMetadata::new(
            vec![None],
            "period",
            Vec::new(),
            positive_zero_arithmetic(),
        ),
    )
    .expect("package construction retains its semantic index");

    package.inspect(|view| {
        assert!(view.subject_count() > 3);
        let declaration = view
            .subjects()
            .find_map(|subject| match subject {
                AlgorithmCodeSubject::Declaration(declaration)
                    if declaration.block_index().is_some() =>
                {
                    Some(declaration)
                }
                _ => None,
            })
            .expect("the block declaration has one branded subject");
        let start = declaration
            .start_expression()
            .expect("a block declaration retains its exact start expression");
        let AlgorithmCodeSubject::Expression(start) =
            view.subject(AlgorithmCodeSubjectId::Expression(start))
        else {
            panic!("the retained start identity resolves only to its expression subject");
        };
        assert_eq!(
            start.kind(),
            AlgorithmCodeExpressionKind::RealBits(0.01_f64.to_bits())
        );
        assert_eq!(start.value().scalar(), declaration.value().scalar());
        assert_eq!(start.value().extents(), declaration.value().extents());

        let methods = view.lifecycle_methods().collect::<Vec<_>>();
        assert_eq!(methods.len(), 3);
        assert_eq!(methods[0].kind(), galec::BlockMethodKind::Startup);
        assert_eq!(methods[1].kind(), galec::BlockMethodKind::Recalibrate);
        assert_eq!(methods[2].kind(), galec::BlockMethodKind::DoStep);
        assert!(methods.iter().all(|method| {
            method.provenance()
                == SemanticProvenance::Generated(GeneratedSubjectKind::LifecycleMethod)
        }));
    });
}

fn tensor_start_package(values: [f64; 2]) -> AlgorithmCodePackage {
    let mut block = galec::Block::new(galec::Name::ident("TensorStart"));
    let mut input =
        galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident("samples"));
    input.dimensions = vec![galec::Dimension::Expr(galec::Expression::Integer(2))];
    block.interface = vec![galec::InterfaceVariable {
        kind: galec::InterfaceKind::Input,
        decl: input,
        start: Some(galec::Expression::Array(
            values.into_iter().map(galec::Expression::Real).collect(),
        )),
    }];
    block.protected = vec![galec::ProtectedEntity {
        kind: galec::ProtectedKind::Constant,
        decl: galec::VariableDeclaration::scalar(
            galec::ScalarType::Real,
            galec::Name::ident("period"),
        ),
        start: Some(galec::Expression::Real(0.01)),
    }];
    AlgorithmCodePackage::construct(
        block,
        AlgorithmCodePackageMetadata::new(
            vec![None, None],
            "period",
            Vec::new(),
            positive_zero_arithmetic(),
        ),
    )
    .expect("the typed tensor start closes its package")
}

#[test]
fn tensor_start_evaluation_is_compact_and_bit_exact() {
    let uniform = tensor_start_package([-0.0, -0.0]);
    uniform.inspect(|view| {
        let declaration = view
            .subjects()
            .find_map(|subject| match subject {
                AlgorithmCodeSubject::Declaration(declaration)
                    if declaration.block_index().is_some()
                        && !declaration.value().extents().is_empty() =>
                {
                    Some(declaration)
                }
                _ => None,
            })
            .expect("the tensor declaration is issued once");
        assert_eq!(declaration.value().extents(), &[2]);
        assert_eq!(
            declaration.evaluated_start(),
            Some(AlgorithmCodeEvaluatedStart::UniformTensorFill(
                AlgorithmCodeEvaluatedScalar::RealBits((-0.0_f64).to_bits())
            ))
        );
        let start = declaration
            .start_expression()
            .expect("the tensor start has one aggregate subject");
        let AlgorithmCodeSubject::Expression(start) =
            view.subject(AlgorithmCodeSubjectId::Expression(start))
        else {
            panic!("the tensor start identity resolves to its exact expression")
        };
        assert_eq!(
            start.evaluated_literal(),
            AlgorithmCodeEvaluatedLiteral::UniformTensorFill(
                AlgorithmCodeEvaluatedScalar::RealBits((-0.0_f64).to_bits())
            )
        );
    });

    let signed_zero_mismatch = tensor_start_package([0.0, -0.0]);
    signed_zero_mismatch.inspect(|view| {
        let declaration = view
            .subjects()
            .find_map(|subject| match subject {
                AlgorithmCodeSubject::Declaration(declaration)
                    if declaration.block_index().is_some()
                        && !declaration.value().extents().is_empty() =>
                {
                    Some(declaration)
                }
                _ => None,
            })
            .expect("the tensor declaration is issued once");
        assert_eq!(
            declaration.evaluated_start(),
            Some(AlgorithmCodeEvaluatedStart::UnsupportedNonUniformTensor)
        );
    });
}

#[test]
fn inspection_issues_closed_scalar_assignment_facts_and_resolved_target() {
    let mut block = galec::Block::new(galec::Name::ident("ScalarAssignment"));
    block.protected = vec![galec::ProtectedEntity {
        kind: galec::ProtectedKind::Constant,
        decl: galec::VariableDeclaration::scalar(
            galec::ScalarType::Real,
            galec::Name::ident("period"),
        ),
        start: Some(galec::Expression::Real(0.01)),
    }];
    block.do_step.locals = vec![galec::VariableDeclaration::scalar(
        galec::ScalarType::Real,
        galec::Name::ident("sink"),
    )];
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::Assignment {
        target: galec::Reference::local(galec::Name::ident("sink")),
        value: galec::Expression::Real(1.25),
    })];
    let package = AlgorithmCodePackage::construct(
        block,
        AlgorithmCodePackageMetadata::new(
            vec![None],
            "period",
            Vec::new(),
            positive_zero_arithmetic(),
        ),
    )
    .expect("the scalar assignment closes package facts");

    package.inspect(|view| {
        let statement = view
            .subjects()
            .find_map(|subject| match subject {
                AlgorithmCodeSubject::Statement(statement) => Some(statement),
                _ => None,
            })
            .expect("one assignment statement is issued");
        assert_eq!(statement.kind(), AlgorithmCodeStatementKind::Assignment);

        let children = view
            .children(AlgorithmCodeSubjectId::Statement(statement.id()))
            .collect::<Vec<_>>();
        let target = children
            .iter()
            .find(|edge| edge.role() == AlgorithmCodeChildRole::AssignmentTarget)
            .map(|edge| view.subject(edge.subject()))
            .expect("assignment target is retained");
        let value = children
            .iter()
            .find(|edge| edge.role() == AlgorithmCodeChildRole::AssignmentValue)
            .map(|edge| view.subject(edge.subject()))
            .expect("assignment value is retained");

        let AlgorithmCodeSubject::Reference(target) = target else {
            panic!("assignment target is a reference subject");
        };
        let AlgorithmCodeReferenceTarget::Declaration(declaration) = target.target() else {
            panic!("the local reference resolves to its exact declaration");
        };
        assert_eq!(
            view.parent(AlgorithmCodeSubjectId::Declaration(declaration.id())),
            AlgorithmCodeSubjectParent::Subject {
                owner: AlgorithmCodeSubjectId::LifecycleMethod(
                    view.lifecycle_methods()
                        .find(|method| method.kind() == galec::BlockMethodKind::DoStep)
                        .expect("do-step method is issued")
                        .id(),
                ),
                role: AlgorithmCodeChildRole::MethodLocal(0),
            }
        );

        let AlgorithmCodeSubject::Expression(value) = value else {
            panic!("assignment value is an expression subject");
        };
        assert_eq!(
            value.kind(),
            AlgorithmCodeExpressionKind::RealBits(1.25_f64.to_bits())
        );
    });
}

#[test]
fn public_subject_count_matches_the_exact_graph_iterator() {
    let mut block = galec::Block::new(galec::Name::ident("BuiltinResultDescriptor"));
    block.protected = vec![galec::ProtectedEntity {
        kind: galec::ProtectedKind::Constant,
        decl: galec::VariableDeclaration::scalar(
            galec::ScalarType::Real,
            galec::Name::ident("period"),
        ),
        start: Some(galec::Expression::Real(0.01)),
    }];
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::Call(
        galec::FunctionCall {
            function: galec::Name::ident("sin"),
            arguments: vec![galec::Expression::Real(1.0)],
        },
    ))];
    let package = AlgorithmCodePackage::construct(
        block,
        AlgorithmCodePackageMetadata::new(
            vec![None],
            "period",
            Vec::new(),
            positive_zero_arithmetic(),
        ),
    )
    .expect("builtin result descriptors close inside the checked package");

    package.inspect(|view| {
        let graph_subject_count = view.subjects().len();
        assert_eq!(view.subject_count(), graph_subject_count);
        assert!(matches!(
            view.call_result_projections()
                .next()
                .expect("sin owns one result projection")
                .callee_result(),
            CalleeResultSubject::BuiltinOutput(_)
        ));
    });
}

#[test]
fn atomic_metadata_constructor_retains_every_supplied_field() {
    let mut block = galec::Block::new(galec::Name::ident("AtomicPackage"));
    block.protected = vec![
        galec::ProtectedEntity {
            kind: galec::ProtectedKind::Constant,
            decl: galec::VariableDeclaration::scalar(
                galec::ScalarType::Real,
                galec::Name::ident("period"),
            ),
            start: Some(galec::Expression::Real(0.01)),
        },
        galec::ProtectedEntity {
            kind: galec::ProtectedKind::DependentParameter,
            decl: galec::VariableDeclaration::scalar(
                galec::ScalarType::Real,
                galec::Name::ident("gain"),
            ),
            start: Some(galec::Expression::Real(2.0)),
        },
    ];
    let folded = ConstantFoldedParameter {
        variable: "gain".to_string(),
        folded_from: "Controller.gain".to_string(),
        scalars: 1,
    };
    let arithmetic = AlgorithmCodeArithmeticProfile::construct(
        AlgorithmCodeRealFormat::Binary64,
        AlgorithmCodeIntegerFormat::I16,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let package = AlgorithmCodePackage::construct(
        block,
        AlgorithmCodePackageMetadata::new(
            vec![None, Some(1.0)],
            "period",
            vec![folded.clone()],
            arithmetic,
        ),
    )
    .expect("complete metadata must close atomically");

    assert_eq!(package.variable_nominals(), [None, Some(1.0)]);
    assert_eq!(package.clock_variable_ordinal(), 1);
    assert_eq!(package.constant_folded_parameters(), [folded]);
    assert_eq!(package.arithmetic_profile(), arithmetic);
    assert_eq!(
        package.arithmetic_profile().source_real(),
        AlgorithmCodeRealFormat::Binary64
    );
    assert_eq!(
        package.arithmetic_profile().source_integer(),
        AlgorithmCodeIntegerFormat::I16
    );
    assert_eq!(
        package.arithmetic_profile().source_integer().minimum(),
        -32_768
    );
    assert_eq!(
        package.arithmetic_profile().source_integer().maximum(),
        32_767
    );
}

#[test]
fn atomic_metadata_constructor_rejects_an_invalid_clock() {
    let block = galec::Block::new(galec::Name::ident("AtomicFailure"));
    let result = AlgorithmCodePackage::construct(
        block,
        AlgorithmCodePackageMetadata::new(
            Vec::new(),
            "missingPeriod",
            vec![ConstantFoldedParameter {
                variable: "uninstalled".to_string(),
                folded_from: "Controller.value".to_string(),
                scalars: 1,
            }],
            positive_zero_arithmetic(),
        ),
    );

    assert_eq!(
        result.expect_err("invalid clock correlation must prevent package exposure"),
        PackageError::InvalidClockReference("missingPeriod".to_string())
    );
}

#[test]
fn package_rejects_forged_constant_fold_provenance() {
    let mut block = galec::Block::new(galec::Name::ident("ForgedFold"));
    block.protected.push(galec::ProtectedEntity {
        kind: galec::ProtectedKind::Constant,
        decl: galec::VariableDeclaration::scalar(
            galec::ScalarType::Real,
            galec::Name::ident("period"),
        ),
        start: Some(galec::Expression::Real(0.01)),
    });
    let error = AlgorithmCodePackage::construct(
        block,
        AlgorithmCodePackageMetadata::new(
            vec![None],
            "period",
            vec![ConstantFoldedParameter {
                variable: "fabricated".to_string(),
                folded_from: "Fabricated.call".to_string(),
                scalars: 1,
            }],
            positive_zero_arithmetic(),
        ),
    )
    .expect_err("presentation provenance cannot cite a nonexistent semantic owner");

    assert_eq!(
        error,
        PackageError::InvalidConstantFold {
            variable: "fabricated".to_string(),
            detail: "the variable is not a protected dependent parameter",
        }
    );
}

#[test]
fn checked_block_closes_an_exact_bounded_selection_correlation() {
    let mut block = galec::Block::new(galec::Name::ident("SelectionCoverage"));
    let mut samples =
        galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident("samples"));
    samples.dimensions = vec![galec::Dimension::Expr(galec::Expression::Integer(3))];
    block.do_step.locals = vec![
        samples,
        galec::VariableDeclaration::scalar(galec::ScalarType::Integer, galec::Name::ident("index")),
        galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident("selected")),
    ];
    let reference = galec::Reference::Local(galec::RefPart {
        name: galec::Name::ident("samples"),
        subscripts: vec![galec::Expression::Ref(galec::Reference::local(
            galec::Name::ident("index"),
        ))],
        span: rumoca_core::Span::DUMMY,
    });
    let selection = galec::IfExpression::bounded_selection(reference, vec![3])
        .expect("valid bounded selection");
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::Assignment {
        target: galec::Reference::local(galec::Name::ident("selected")),
        value: galec::Expression::If(selection),
    })];

    CheckedAlgorithmBlock::construct(block)
        .expect("an exact bounded-selection correlation must close its retained facts");
}

#[test]
fn checked_block_rejects_a_mutated_bounded_selection_expansion() {
    let mut block = galec::Block::new(galec::Name::ident("SelectionIntegrity"));
    let mut samples =
        galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident("samples"));
    samples.dimensions = vec![galec::Dimension::Expr(galec::Expression::Integer(3))];
    block.do_step.locals = vec![
        samples,
        galec::VariableDeclaration::scalar(galec::ScalarType::Integer, galec::Name::ident("index")),
        galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident("selected")),
    ];
    let reference = galec::Reference::Local(galec::RefPart {
        name: galec::Name::ident("samples"),
        subscripts: vec![galec::Expression::Ref(galec::Reference::local(
            galec::Name::ident("index"),
        ))],
        span: rumoca_core::Span::DUMMY,
    });
    let mut selection = galec::IfExpression::bounded_selection(reference, vec![3])
        .expect("valid bounded selection");
    *selection.else_value = galec::Expression::Ref(galec::Reference::Local(galec::RefPart {
        name: galec::Name::ident("samples"),
        subscripts: vec![galec::Expression::Integer(1)],
        span: rumoca_core::Span::DUMMY,
    }));
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::Assignment {
        target: galec::Reference::local(galec::Name::ident("selected")),
        value: galec::Expression::If(selection),
    })];

    let error = CheckedAlgorithmBlock::construct(block)
        .expect_err("a stale correlation must fail checked construction");
    assert!(error.to_string().contains("EG041"), "{error}");
}

#[test]
fn package_inspection_pairs_result_identities_with_typed_subjects() {
    let array = |name: &str| galec::VariableDeclaration {
        ty: galec::TypeRef::Primitive(galec::ScalarType::Real),
        name: galec::Name::ident(name),
        dimensions: vec![galec::Dimension::Expr(galec::Expression::Integer(2))],
        range: galec::RangeAttributes::default(),
        span: rumoca_core::Span::DUMMY,
    };
    let mut block = galec::Block::new(galec::Name::ident("ResultIdentities"));
    block.protected = vec![galec::ProtectedEntity {
        kind: galec::ProtectedKind::Constant,
        decl: galec::VariableDeclaration::scalar(
            galec::ScalarType::Real,
            galec::Name::ident("period"),
        ),
        start: Some(galec::Expression::Real(0.01)),
    }];
    block.protected_functions = vec![galec::UserFunction {
        kind: galec::FunctionKind::Stateless,
        name: galec::Name::ident("pair"),
        signals: Vec::new(),
        parameters: ["first", "second"]
            .into_iter()
            .map(|name| galec::Parameter {
                direction: galec::Direction::Output,
                decl: array(name),
            })
            .collect(),
        locals: Vec::new(),
        statements: Vec::new(),
        span: rumoca_core::Span::DUMMY,
    }];
    block.do_step.locals = vec![array("left"), array("right")];
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::MultiAssignment {
        targets: vec![
            galec::Reference::local(galec::Name::ident("left")),
            galec::Reference::local(galec::Name::ident("right")),
        ],
        call: galec::FunctionCall {
            function: galec::Name::ident("pair"),
            arguments: Vec::new(),
        },
    })];
    let package = AlgorithmCodePackage::construct(
        block,
        AlgorithmCodePackageMetadata::new(
            vec![None],
            "period",
            Vec::new(),
            positive_zero_arithmetic(),
        ),
    )
    .expect("fixed result shapes close the compiler package");

    package.inspect(|view| {
        let results = view.call_result_projections().collect::<Vec<_>>();
        assert_eq!(results.len(), 2);
        assert_ne!(results[0].id(), results[1].id());
        assert_eq!(results[0].call().id(), results[1].call().id());
        assert_eq!(results[0].value().extents(), [2]);
        assert_eq!(results[1].value().extents(), [2]);
        let (
            CalleeResultSubject::UserOutput(first_output),
            CalleeResultSubject::UserOutput(second_output),
        ) = (results[0].callee_result(), results[1].callee_result())
        else {
            panic!("user call results cite output declarations");
        };
        assert_ne!(first_output.id(), second_output.id());
        let (
            CallResultReceiver::MultiAssignmentDestination(left),
            CallResultReceiver::MultiAssignmentDestination(right),
        ) = (results[0].receiver(), results[1].receiver())
        else {
            panic!("multi-assignment results cite exact references");
        };
        assert_ne!(left.id(), right.id());
    });
}

#[test]
fn symbolic_call_result_fails_package_construction_at_its_receiver_provenance() {
    let scalar = |name: &str| {
        galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident(name))
    };
    let mut input = scalar("arg");
    input.dimensions = vec![galec::Dimension::Derived];
    let mut output = scalar("result");
    output.dimensions = vec![galec::Dimension::Expr(galec::Expression::Size {
        array: galec::Reference::local(galec::Name::ident("arg")),
        dimension: Box::new(galec::Expression::Integer(1)),
    })];
    let mut actual = scalar("actual");
    actual.dimensions = vec![galec::Dimension::Expr(galec::Expression::Integer(2))];
    let target = actual.clone();
    let call_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("symbolic-result.mo"),
        10,
        20,
    );
    let mut block = galec::Block::new(galec::Name::ident("SymbolicResult"));
    block.protected = vec![galec::ProtectedEntity {
        kind: galec::ProtectedKind::Constant,
        decl: scalar("period"),
        start: Some(galec::Expression::Real(0.01)),
    }];
    block.protected_functions = vec![galec::UserFunction {
        kind: galec::FunctionKind::Stateless,
        name: galec::Name::ident("copy"),
        signals: Vec::new(),
        parameters: vec![
            galec::Parameter {
                direction: galec::Direction::Input,
                decl: input,
            },
            galec::Parameter {
                direction: galec::Direction::Output,
                decl: output,
            },
        ],
        locals: Vec::new(),
        statements: Vec::new(),
        span: call_span,
    }];
    block.do_step.locals = vec![actual, target];
    block.do_step.locals[1].name = galec::Name::ident("target");
    block.do_step.statements = vec![galec::Spanned {
        node: galec::Statement::MultiAssignment {
            targets: vec![galec::Reference::local(galec::Name::ident("target"))],
            call: galec::FunctionCall {
                function: galec::Name::Ident(galec::Identifier::new("copy"), call_span),
                arguments: vec![galec::Expression::Ref(galec::Reference::local(
                    galec::Name::ident("actual"),
                ))],
            },
        },
        span: call_span,
    }];

    let error = AlgorithmCodePackage::construct(
        block,
        AlgorithmCodePackageMetadata::new(
            vec![None],
            "period",
            Vec::new(),
            positive_zero_arithmetic(),
        ),
    )
    .expect_err("symbolic result shape is not yet a packaged capability");
    assert_eq!(
        error,
        PackageError::UnprovenValueShape {
            subject: "call-result projection",
            provenance: SemanticProvenance::NearestStatement(call_span),
        }
    );
    assert!(
        error
            .to_string()
            .contains("call-result projection in the statement at source")
    );
}

#[test]
fn nearest_statement_shape_failure_uses_statement_wording() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("nearest.mo"),
        4,
        9,
    );
    let error = PackageError::UnprovenValueShape {
        subject: "call-result projection",
        provenance: SemanticProvenance::NearestStatement(span),
    };
    assert!(error.to_string().contains("in the statement at source"));
}
