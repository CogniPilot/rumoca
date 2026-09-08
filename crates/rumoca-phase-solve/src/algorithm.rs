//! Checked Algorithm Code to executable Solve refinement.

use rumoca_core::{Diagnostic, PhaseError, PrimaryLabel, Span};
use rumoca_ir_galec as galec;
use rumoca_ir_solve as solve;

/// Refine one non-cloneable checked package into one non-cloneable executable
/// lifecycle root.
///
/// The current implementation deliberately accepts only lifecycle methods
/// whose complete semantic-subject set is representable by the sealed Solve
/// builder. Every other checked construct returns a typed error before a root
/// is exposed; no statement is omitted and no raw GALEC-to-C route exists.
pub fn lower_solve_algorithm_product<'inv>(
    package: galec::TracedAlgorithmCodeProduct<'inv>,
) -> Result<solve::SolveAlgorithmProduct<'inv>, AlgorithmLowerError> {
    solve::SolveAlgorithmBlock::construct(package, |inspection, builder| {
        for subject in inspection.subjects() {
            match subject {
                galec::package::AlgorithmCodeSubject::LifecycleMethod(subject) => {
                    builder
                        .issue_lifecycle_method(inspection, subject)
                        .map_err(AlgorithmLowerError::from)?;
                }
                galec::package::AlgorithmCodeSubject::Declaration(subject) => {
                    builder
                        .issue_declaration(inspection, subject)
                        .map_err(AlgorithmLowerError::from)?;
                }
                galec::package::AlgorithmCodeSubject::UserFunction(subject) => {
                    return Err(AlgorithmLowerError::unsupported(
                        "algorithm-user-function",
                        subject.provenance(),
                    ));
                }
                galec::package::AlgorithmCodeSubject::LoopBinder(subject) => {
                    return Err(AlgorithmLowerError::unsupported(
                        "algorithm-loop-binder",
                        subject.provenance(),
                    ));
                }
                galec::package::AlgorithmCodeSubject::Statement(subject) => {
                    builder
                        .issue_statement(inspection, subject)
                        .map_err(AlgorithmLowerError::from)?;
                }
                galec::package::AlgorithmCodeSubject::Expression(subject) => {
                    builder
                        .issue_expression(inspection, subject)
                        .map_err(AlgorithmLowerError::from)?;
                }
                galec::package::AlgorithmCodeSubject::Reference(subject) => {
                    builder
                        .issue_reference(inspection, subject)
                        .map_err(AlgorithmLowerError::from)?;
                }
                galec::package::AlgorithmCodeSubject::Call(subject) => {
                    return Err(AlgorithmLowerError::unsupported(
                        "algorithm-call",
                        subject.provenance(),
                    ));
                }
                galec::package::AlgorithmCodeSubject::CallResultProjection(subject) => {
                    return Err(AlgorithmLowerError::unsupported(
                        "algorithm-call-result-projection",
                        subject.provenance(),
                    ));
                }
            }
        }

        let call_transfers = solve::CallTransferPlanSet::construct(Vec::new(), |_| Ok(()))
            .map_err(solve::SolveAlgorithmBlockConstructionError::from)
            .map_err(AlgorithmLowerError::from)?;
        builder
            .issue_call_transfers(call_transfers)
            .map_err(AlgorithmLowerError::from)
    })
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum AlgorithmLowerError {
    #[error("unsupported-feature:{feature} {provenance}")]
    Unsupported {
        feature: &'static str,
        provenance: galec::package::SemanticProvenance,
    },
    #[error("Algorithm Code to Solve construction failed: {0}")]
    Construction(solve::SolveAlgorithmBlockConstructionError),
}

impl AlgorithmLowerError {
    fn unsupported(feature: &'static str, provenance: galec::package::SemanticProvenance) -> Self {
        Self::Unsupported {
            feature,
            provenance,
        }
    }

    #[must_use]
    pub const fn code(&self) -> &'static str {
        match self {
            Self::Unsupported { .. } => crate::diagnostic_codes::EL001_UNSUPPORTED_EXPRESSION,
            Self::Construction(_) => crate::diagnostic_codes::EL005_INVALID_SOLVE_CONTRACT,
        }
    }

    #[must_use]
    pub fn source_span(&self) -> Option<Span> {
        match self {
            Self::Unsupported { provenance, .. } => provenance_span(*provenance),
            Self::Construction(error) => error.source_span(),
        }
    }
}

impl From<solve::SolveAlgorithmBlockConstructionError> for AlgorithmLowerError {
    fn from(error: solve::SolveAlgorithmBlockConstructionError) -> Self {
        match error {
            solve::SolveAlgorithmBlockConstructionError::UnsupportedTensorInitializationPlan {
                plan,
                provenance,
            } => Self::unsupported(plan.feature_name(), provenance),
            solve::SolveAlgorithmBlockConstructionError::ExpressionIsNotDeclarationDimension {
                provenance,
            } => Self::unsupported("algorithm-expression", provenance),
            error => Self::Construction(error),
        }
    }
}

impl PhaseError for AlgorithmLowerError {
    fn to_diagnostic(&self) -> Diagnostic {
        match self.source_span() {
            Some(span) => Diagnostic::error(
                self.code(),
                self.to_string(),
                PrimaryLabel::new(span).with_message("Algorithm Code refinement failed here"),
            ),
            None => Diagnostic::global_error(self.code(), self.to_string()),
        }
    }
}

fn provenance_span(provenance: galec::package::SemanticProvenance) -> Option<Span> {
    match provenance {
        galec::package::SemanticProvenance::Exact(span)
        | galec::package::SemanticProvenance::NearestStatement(span)
            if !span.is_dummy() =>
        {
            Some(span)
        }
        galec::package::SemanticProvenance::Exact(_)
        | galec::package::SemanticProvenance::NearestStatement(_)
        | galec::package::SemanticProvenance::Generated(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use rumoca_core::{RealMatrixMultiplySemantics, TargetInvocationBrand};

    use super::*;

    fn traced_package<'inv, F>(
        brand: TargetInvocationBrand<'inv>,
        construct: F,
    ) -> galec::TracedAlgorithmCodeProduct<'inv>
    where
        F: for<'origin> FnOnce(
            galec::AlgorithmCodePackageIssuer<'origin>,
        ) -> Result<
            galec::OriginBoundAlgorithmCodePackage<'origin>,
            galec::package::PackageError,
        >,
    {
        let mut sources = rumoca_core::SourceMap::new();
        for name in [
            &rumoca_core::placeholder_source_name(Span::DUMMY.source),
            "method-local.mo",
            "tensor-start.mo",
            "startup-contract.mo",
            "function-local.mo",
            "scalar-literal.mo",
            "unsupported-expression.mo",
        ] {
            let source = rumoca_core::source_id_for_name(name);
            assert!(sources.register_id(source, name, Arc::<str>::from(" ".repeat(128)),));
        }
        galec::TracedAlgorithmCodeProduct::project_from_origin(
            brand,
            &sources,
            "SolveAlgorithmFixture",
            construct,
        )
        .expect("fixture package provenance resolves in its retained map")
    }

    fn package<'origin>(
        issuer: galec::AlgorithmCodePackageIssuer<'origin>,
        matrix: RealMatrixMultiplySemantics,
        with_input: bool,
        with_method_local: bool,
    ) -> Result<galec::OriginBoundAlgorithmCodePackage<'origin>, galec::package::PackageError> {
        let mut block = galec::Block::new(galec::ast::Name::ident("SolveAlgorithmFixture"));
        if with_input {
            block.interface.push(galec::ast::InterfaceVariable {
                kind: galec::ast::InterfaceKind::Input,
                decl: galec::ast::VariableDeclaration::scalar(
                    galec::ast::ScalarType::Real,
                    galec::ast::Name::ident("u"),
                ),
                start: Some(galec::ast::Expression::Real(0.0)),
            });
        }
        block.protected.push(galec::ast::ProtectedEntity {
            kind: galec::ast::ProtectedKind::Constant,
            decl: galec::ast::VariableDeclaration::scalar(
                galec::ast::ScalarType::Real,
                galec::ast::Name::ident("period"),
            ),
            start: Some(galec::ast::Expression::Real(0.01)),
        });
        block.startup.statements.push(galec::ast::Spanned {
            node: galec::ast::Statement::Assignment {
                target: galec::ast::Reference::state(galec::ast::Name::ident("period")),
                value: galec::ast::Expression::Real(0.01),
            },
            span: Span::DUMMY,
        });
        if with_method_local {
            let mut local = galec::ast::VariableDeclaration::scalar(
                galec::ast::ScalarType::Real,
                galec::ast::Name::ident("scratch"),
            );
            local.span = method_local_span();
            local.dimensions.push(galec::ast::Dimension::Expr(
                galec::ast::Expression::Integer(10),
            ));
            block.do_step.locals.push(local);
        }
        let variable_count = usize::from(with_input) + 1;
        issuer.construct(
            block,
            galec::package::AlgorithmCodePackageMetadata::new(
                vec![None; variable_count],
                "period",
                Vec::new(),
                galec::package::AlgorithmCodeArithmeticProfile::construct(
                    galec::package::AlgorithmCodeRealFormat::Binary32,
                    galec::package::AlgorithmCodeIntegerFormat::I32,
                    matrix,
                ),
            ),
        )
    }

    fn method_local_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("method-local.mo"),
            41,
            58,
        )
    }

    fn function_local_package<'origin>(
        issuer: galec::AlgorithmCodePackageIssuer<'origin>,
        local_span: Span,
    ) -> Result<galec::OriginBoundAlgorithmCodePackage<'origin>, galec::package::PackageError> {
        let matrix = RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero;
        let mut block = galec::Block::new(galec::ast::Name::ident("FunctionLocal"));
        block.protected.push(galec::ast::ProtectedEntity {
            kind: galec::ast::ProtectedKind::Constant,
            decl: galec::ast::VariableDeclaration::scalar(
                galec::ast::ScalarType::Real,
                galec::ast::Name::ident("period"),
            ),
            start: Some(galec::ast::Expression::Real(0.01)),
        });
        block.startup.statements.push(galec::ast::Spanned {
            node: galec::ast::Statement::Assignment {
                target: galec::ast::Reference::state(galec::ast::Name::ident("period")),
                value: galec::ast::Expression::Real(0.01),
            },
            span: local_span,
        });
        let mut local = galec::ast::VariableDeclaration::scalar(
            galec::ast::ScalarType::Real,
            galec::ast::Name::ident("scratch"),
        );
        local.span = local_span;
        block.public_functions.push(galec::ast::UserFunction {
            kind: galec::ast::FunctionKind::Stateless,
            name: galec::ast::Name::ident("helper"),
            signals: Vec::new(),
            parameters: Vec::new(),
            locals: vec![local],
            statements: Vec::new(),
            span: local_span,
        });
        block.do_step.statements.push(galec::ast::Spanned {
            node: galec::ast::Statement::Call(galec::ast::FunctionCall {
                function: galec::ast::Name::ident("helper"),
                arguments: Vec::new(),
            }),
            span: local_span,
        });
        issuer.construct(
            block,
            galec::package::AlgorithmCodePackageMetadata::new(
                vec![None],
                "period",
                Vec::new(),
                galec::package::AlgorithmCodeArithmeticProfile::construct(
                    galec::package::AlgorithmCodeRealFormat::Binary32,
                    galec::package::AlgorithmCodeIntegerFormat::I32,
                    matrix,
                ),
            ),
        )
    }

    fn tensor_start_package<'origin>(
        issuer: galec::AlgorithmCodePackageIssuer<'origin>,
        external: bool,
        start: galec::ast::Expression,
        startup_value: Option<galec::ast::Expression>,
    ) -> Result<galec::OriginBoundAlgorithmCodePackage<'origin>, galec::package::PackageError> {
        let matrix = RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero;
        let mut tensor = galec::ast::VariableDeclaration::scalar(
            galec::ast::ScalarType::Real,
            galec::ast::Name::ident("samples"),
        );
        tensor.span = tensor_start_span();
        tensor.dimensions = vec![galec::ast::Dimension::Expr(
            galec::ast::Expression::Integer(2),
        )];
        let mut block = galec::Block::new(galec::ast::Name::ident("TensorStart"));
        block.interface.push(galec::ast::InterfaceVariable {
            kind: if external {
                galec::ast::InterfaceKind::Input
            } else {
                galec::ast::InterfaceKind::Output
            },
            decl: tensor,
            start: Some(start),
        });
        block.protected.push(galec::ast::ProtectedEntity {
            kind: galec::ast::ProtectedKind::Constant,
            decl: galec::ast::VariableDeclaration::scalar(
                galec::ast::ScalarType::Real,
                galec::ast::Name::ident("period"),
            ),
            start: Some(galec::ast::Expression::Real(0.01)),
        });
        if let Some(value) = startup_value {
            block.startup.statements.push(galec::ast::Spanned::dummy(
                galec::ast::Statement::Assignment {
                    target: galec::ast::Reference::state(galec::ast::Name::ident("samples")),
                    value,
                },
            ));
        }
        block.startup.statements.push(galec::ast::Spanned::dummy(
            galec::ast::Statement::Assignment {
                target: galec::ast::Reference::state(galec::ast::Name::ident("period")),
                value: galec::ast::Expression::Real(0.01),
            },
        ));
        issuer.construct(
            block,
            galec::package::AlgorithmCodePackageMetadata::new(
                vec![None; 2],
                "period",
                Vec::new(),
                galec::package::AlgorithmCodeArithmeticProfile::construct(
                    galec::package::AlgorithmCodeRealFormat::Binary32,
                    galec::package::AlgorithmCodeIntegerFormat::I32,
                    matrix,
                ),
            ),
        )
    }

    fn tensor_start_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("tensor-start.mo"),
            7,
            29,
        )
    }

    fn real_array(values: [f64; 2]) -> galec::ast::Expression {
        galec::ast::Expression::Array(
            values
                .into_iter()
                .map(galec::ast::Expression::Real)
                .collect(),
        )
    }

    fn startup_contract_package<'origin>(
        issuer: galec::AlgorithmCodePackageIssuer<'origin>,
        startup: &[(&str, f64)],
        do_step: Option<(&str, f64)>,
    ) -> Result<galec::OriginBoundAlgorithmCodePackage<'origin>, galec::package::PackageError> {
        let matrix = RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero;
        let source = rumoca_core::SourceId::from_source_name("startup-contract.mo");
        let span = Span::from_offsets(source, 1, 12);
        let mut block = galec::Block::new(galec::ast::Name::ident("StartupContract"));
        block.interface.push(galec::ast::InterfaceVariable {
            kind: galec::ast::InterfaceKind::Output,
            decl: galec::ast::VariableDeclaration::scalar(
                galec::ast::ScalarType::Real,
                galec::ast::Name::ident("y"),
            ),
            start: Some(galec::ast::Expression::Real(0.0)),
        });
        block.protected.push(galec::ast::ProtectedEntity {
            kind: galec::ast::ProtectedKind::Constant,
            decl: galec::ast::VariableDeclaration::scalar(
                galec::ast::ScalarType::Real,
                galec::ast::Name::ident("period"),
            ),
            start: Some(galec::ast::Expression::Real(0.01)),
        });
        for (name, value) in startup {
            block.startup.statements.push(galec::ast::Spanned {
                node: galec::ast::Statement::Assignment {
                    target: galec::ast::Reference::state(galec::ast::Name::ident(*name)),
                    value: galec::ast::Expression::Real(*value),
                },
                span,
            });
        }
        if let Some((name, value)) = do_step {
            block.do_step.statements.push(galec::ast::Spanned {
                node: galec::ast::Statement::Assignment {
                    target: galec::ast::Reference::state(galec::ast::Name::ident(name)),
                    value: galec::ast::Expression::Real(value),
                },
                span,
            });
        }
        issuer.construct(
            block,
            galec::package::AlgorithmCodePackageMetadata::new(
                vec![None; 2],
                "period",
                Vec::new(),
                galec::package::AlgorithmCodeArithmeticProfile::construct(
                    galec::package::AlgorithmCodeRealFormat::Binary32,
                    galec::package::AlgorithmCodeIntegerFormat::I32,
                    matrix,
                ),
            ),
        )
    }

    #[test]
    fn minimal_package_refines_into_sealed_lifecycle_root() {
        rumoca_core::with_target_invocation_brand(|brand| {
            let matrix = RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero;
            let product = lower_solve_algorithm_product(traced_package(brand, |issuer| {
                package(issuer, matrix, false, false)
            }))
            .expect("the exactly mapped minimal package refines");
            let block = product.solve_algorithm_block();

            assert_eq!(block.declarations().len(), 1);
            assert_eq!(
                block.declarations()[0].storage(),
                solve::SolveStorageClass::Constant
            );
            assert_eq!(
                block.declarations()[0].access(),
                solve::SolveSlotAccess::ReadOnly
            );
            assert_eq!(
                block.declarations()[0].logical_storage().storage(),
                solve::SolveStorageClass::Constant
            );
            assert_eq!(block.declarations()[0].logical_storage().scalar_base(), 0);
            assert_eq!(block.declarations()[0].logical_storage().scalar_count(), 1);
            assert_eq!(block.declarations()[0].logical_storage().scalar_end(), 1);
            assert_eq!(
                block
                    .storage_totals()
                    .scalar_count(solve::SolveAlgorithmBlockStorageClass::Constant),
                1
            );
            for kind in [
                solve::SolveAlgorithmMethodKind::Startup,
                solve::SolveAlgorithmMethodKind::Recalibrate,
                solve::SolveAlgorithmMethodKind::DoStep,
            ] {
                let method = block.method(kind);
                assert_eq!(method.kind(), kind);
                assert!(method.program().operations().is_empty());
            }
            assert!(block.call_transfers().entries().is_empty());
            assert_eq!(
                product
                    .algorithm_code()
                    .arithmetic_profile()
                    .real_matrix_multiply(),
                block.arithmetic().real_matrix_multiply_semantics()
            );
            assert_eq!(
                product.algorithm_code().arithmetic_profile().source_real(),
                galec::package::AlgorithmCodeRealFormat::Binary32
            );
            assert_eq!(
                product
                    .algorithm_code()
                    .arithmetic_profile()
                    .source_integer(),
                galec::package::AlgorithmCodeIntegerFormat::I32
            );
            assert_eq!(
                block.arithmetic().real_format(),
                solve::SolveRealFormat::Binary32
            );
            assert_eq!(
                block.arithmetic().integer_domain(),
                solve::SolveIntegerDomain::I32
            );
        });
    }

    #[test]
    fn startup_initialization_value_must_match_the_catalog_bit_exactly() {
        rumoca_core::with_target_invocation_brand(|brand| {
            let error = lower_solve_algorithm_product(traced_package(brand, |issuer| {
                startup_contract_package(issuer, &[("y", 1.0), ("period", 0.01)], None)
            }))
            .expect_err("Startup cannot disagree with the sealed declaration start");
            assert!(matches!(
            error,
            AlgorithmLowerError::Construction(
                solve::SolveAlgorithmBlockConstructionError::StartupInitializationValueMismatch { .. }
            )
        ));
        });
    }

    #[test]
    fn every_internal_declaration_requires_one_startup_initialization() {
        rumoca_core::with_target_invocation_brand(|brand| {
            let error = lower_solve_algorithm_product(traced_package(brand, |issuer| {
                startup_contract_package(issuer, &[("period", 0.01)], None)
            }))
            .expect_err("missing Startup coverage cannot expose a Solve root");
            assert!(matches!(
                error,
                AlgorithmLowerError::Construction(
                    solve::SolveAlgorithmBlockConstructionError::MissingStartupInitialization { .. }
                )
            ));
        });
    }

    #[test]
    fn duplicate_startup_initialization_is_rejected() {
        rumoca_core::with_target_invocation_brand(|brand| {
            let error = lower_solve_algorithm_product(traced_package(brand, |issuer| {
                startup_contract_package(issuer, &[("y", 0.0), ("y", 0.0), ("period", 0.01)], None)
            }))
            .expect_err("duplicate Startup coverage cannot expose a Solve root");
            assert!(matches!(
                error,
                AlgorithmLowerError::Construction(
                    solve::SolveAlgorithmBlockConstructionError::DuplicateStartupInitialization { .. }
                )
            ));
        });
    }

    #[test]
    fn constant_has_no_general_write_arm_after_startup() {
        rumoca_core::with_target_invocation_brand(|brand| {
            let error = lower_solve_algorithm_product(traced_package(brand, |issuer| {
                startup_contract_package(
                    issuer,
                    &[("y", 0.0), ("period", 0.01)],
                    Some(("period", 0.01)),
                )
            }))
            .expect_err("a constant write outside Startup is unrepresentable");
            assert!(matches!(
                error,
                AlgorithmLowerError::Construction(
                    solve::SolveAlgorithmBlockConstructionError::IllegalLifecycleWrite {
                        storage: solve::SolveStorageClass::Constant,
                        method: solve::SolveAlgorithmMethodKind::DoStep,
                        ..
                    }
                )
            ));
        });
    }

    #[test]
    fn branded_input_class_maps_without_name_or_ordinal_inference() {
        rumoca_core::with_target_invocation_brand(|brand| {
            let matrix = RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero;
            let product = lower_solve_algorithm_product(traced_package(brand, |issuer| {
                package(issuer, matrix, true, false)
            }))
            .expect("the package-issued input class has an exact Solve mapping");
            let block = product.solve_algorithm_block();

            assert_eq!(block.declarations().len(), 2);
            assert_eq!(
                block.declarations()[0].storage(),
                solve::SolveStorageClass::Input
            );
            assert_eq!(
                block.declarations()[0].access(),
                solve::SolveSlotAccess::ReadOnly
            );
            assert_eq!(block.declarations()[0].logical_storage().scalar_base(), 0);
            assert_eq!(
                block
                    .storage_totals()
                    .scalar_count(solve::SolveAlgorithmBlockStorageClass::Input),
                1
            );
            assert_eq!(block.declarations()[1].logical_storage().scalar_base(), 0);
        });
    }

    #[test]
    fn uniform_tensor_starts_remain_compact_for_external_and_startup_owners() {
        rumoca_core::with_target_invocation_brand(|brand| {
            let external = lower_solve_algorithm_product(traced_package(brand, |issuer| {
                tensor_start_package(issuer, true, real_array([-0.0, -0.0]), None)
            }))
            .expect("a uniform external tensor suggestion has one compact Solve value");
            assert!(matches!(
                external.solve_algorithm_block().declarations()[0].initialization(),
                solve::SolveDeclarationInitialization::External {
                    suggested_value: solve::SolveDeclarationStartValue::UniformTensorFill(value),
                    ..
                } if matches!(value.kind(), solve::SolveValueKind::Real32(bits) if bits == (-0.0_f32).to_bits())
            ));

            let internal = lower_solve_algorithm_product(traced_package(brand, |issuer| {
                tensor_start_package(
                    issuer,
                    false,
                    real_array([-0.0, -0.0]),
                    Some(real_array([-0.0, -0.0])),
                )
            }))
            .expect("one aggregate Startup action owns a uniform internal tensor fill");
            let block = internal.solve_algorithm_block();
            assert_eq!(block.declarations()[0].value_type().dimensions(), &[2]);
            assert_eq!(block.declarations()[0].logical_storage().scalar_count(), 2);
            let startup = block.method(solve::SolveAlgorithmMethodKind::Startup);
            assert_eq!(startup.actions().len(), 2);
            assert!(matches!(
                startup.actions()[0].kind(),
                solve::SolveAlgorithmActionKind::StartupInitialize {
                    value: solve::SolveDeclarationStartValue::UniformTensorFill(value),
                    ..
                } if matches!(value.kind(), solve::SolveValueKind::Real32(bits) if bits == (-0.0_f32).to_bits())
            ));
            assert!(startup.program().operations().is_empty());
        });
    }

    #[test]
    fn nonuniform_and_symbolic_tensor_starts_name_future_capabilities() {
        rumoca_core::with_target_invocation_brand(|brand| {
            let nonuniform = lower_solve_algorithm_product(traced_package(brand, |issuer| {
                tensor_start_package(issuer, true, real_array([0.0, -0.0]), None)
            }))
            .expect_err("signed-zero disagreement is not a uniform fill");
            assert!(matches!(
                nonuniform,
                AlgorithmLowerError::Unsupported {
                    feature: "future-non-uniform-tensor-initialization-plan",
                    ..
                }
            ));
            assert_eq!(nonuniform.source_span(), Some(tensor_start_span()));

            let symbolic_value = galec::ast::Expression::Ref(galec::ast::Reference::state(
                galec::ast::Name::ident("period"),
            ));
            let symbolic = lower_solve_algorithm_product(traced_package(brand, |issuer| {
                tensor_start_package(
                    issuer,
                    true,
                    galec::ast::Expression::Array(vec![symbolic_value.clone(), symbolic_value]),
                    None,
                )
            }))
            .expect_err("symbolic tensor initialization needs a future compact plan");
            assert!(matches!(
                symbolic,
                AlgorithmLowerError::Unsupported {
                    feature: "future-symbolic-tensor-initialization-plan",
                    ..
                }
            ));
            assert_eq!(symbolic.source_span(), Some(tensor_start_span()));
            assert_eq!(
                symbolic.code(),
                crate::diagnostic_codes::EL001_UNSUPPORTED_EXPRESSION
            );
        });
    }

    #[test]
    fn startup_tensor_match_uses_package_bits_before_binary32_rounding() {
        rumoca_core::with_target_invocation_brand(|brand| {
            let rounds_to_one = 1.0 + f64::from(f32::EPSILON) / 4.0;
            assert_eq!(rounds_to_one as f32, 1.0_f32);
            let error = lower_solve_algorithm_product(traced_package(brand, |issuer| {
                tensor_start_package(
                    issuer,
                    false,
                    real_array([1.0, 1.0]),
                    Some(real_array([rounds_to_one, rounds_to_one])),
                )
            }))
            .expect_err("binary32 rounding cannot hide a package-start bit mismatch");
            assert!(matches!(
            error,
            AlgorithmLowerError::Construction(
                solve::SolveAlgorithmBlockConstructionError::StartupInitializationValueMismatch { .. }
            )
        ));
        });
    }

    #[test]
    fn method_local_is_owned_by_one_invocation_region_without_scalarization() {
        rumoca_core::with_target_invocation_brand(|brand| {
            let matrix = RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero;
            let product = lower_solve_algorithm_product(traced_package(brand, |issuer| {
                package(issuer, matrix, false, true)
            }))
            .expect("a checked method-local declaration has a scoped Solve owner");
            let block = product.solve_algorithm_block();
            let do_step = block.method(solve::SolveAlgorithmMethodKind::DoStep);

            assert_eq!(
                block.declarations().len(),
                1,
                "locals are not block storage"
            );
            assert_eq!(do_step.locals().len(), 1);
            assert_eq!(do_step.method_local_scalar_count(), 10);
            let local = &do_step.locals()[0];
            assert_eq!(
                local.source_class(),
                galec::package::AlgorithmCodeDeclarationClass::MethodLocal
            );
            assert_eq!(local.storage(), solve::SolveStorageClass::MethodLocal);
            assert_eq!(local.access(), solve::SolveSlotAccess::ReadWrite);
            assert_eq!(
                local.lifetime(),
                solve::SolveAlgorithmScopedLifetime::MethodInvocation
            );
            assert_eq!(local.value_type().dimensions(), &[10]);
            assert_eq!(local.dimensions().len(), 1);
            assert_eq!(local.dimensions()[0].extent(), 10);
            assert_eq!(local.logical_storage().scalar_base(), 0);
            assert_eq!(local.logical_storage().scalar_count(), 10);
            assert_eq!(
                local.provenance(),
                galec::package::SemanticProvenance::Exact(method_local_span())
            );
            assert!(
                block
                    .method(solve::SolveAlgorithmMethodKind::Startup)
                    .locals()
                    .is_empty()
            );
            assert!(
                block
                    .method(solve::SolveAlgorithmMethodKind::Recalibrate)
                    .locals()
                    .is_empty()
            );
        });
    }

    #[test]
    fn unsupported_function_region_local_refuses_at_its_exact_declaration_span() {
        rumoca_core::with_target_invocation_brand(|brand| {
            let local_span = Span::from_offsets(
                rumoca_core::SourceId::from_source_name("function-local.mo"),
                71,
                86,
            );
            let error = lower_solve_algorithm_product(traced_package(brand, |issuer| {
                function_local_package(issuer, local_span)
            }))
            .expect_err("a lifecycle local does not authorize an unrepresented function region");

            assert_eq!(error.source_span(), Some(local_span));
            assert!(matches!(
                error,
                AlgorithmLowerError::Construction(
                    solve::SolveAlgorithmBlockConstructionError::UnsupportedDeclarationClass {
                        class: galec::package::AlgorithmCodeDeclarationClass::FunctionLocal,
                        ..
                    }
                )
            ));
        });
    }

    #[test]
    fn scalar_literal_assignment_constructs_action_program_and_storage_binding() {
        rumoca_core::with_target_invocation_brand(|brand| {
            let matrix = RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero;
            let source = rumoca_core::SourceId::from_source_name("scalar-literal.mo");
            let declaration_span = Span::from_offsets(source, 1, 8);
            let statement_span = Span::from_offsets(source, 10, 24);
            let mut output = galec::ast::VariableDeclaration::scalar(
                galec::ast::ScalarType::Real,
                galec::ast::Name::ident("y"),
            );
            output.span = declaration_span;
            let mut block = galec::Block::new(galec::ast::Name::ident("ScalarLiteral"));
            block.interface.push(galec::ast::InterfaceVariable {
                kind: galec::ast::InterfaceKind::Output,
                decl: output,
                start: Some(galec::ast::Expression::Real(0.0)),
            });
            block.protected.push(galec::ast::ProtectedEntity {
                kind: galec::ast::ProtectedKind::Constant,
                decl: galec::ast::VariableDeclaration::scalar(
                    galec::ast::ScalarType::Real,
                    galec::ast::Name::ident("period"),
                ),
                start: Some(galec::ast::Expression::Real(0.01)),
            });
            for (name, value) in [("y", 0.0), ("period", 0.01)] {
                block.startup.statements.push(galec::ast::Spanned {
                    node: galec::ast::Statement::Assignment {
                        target: galec::ast::Reference::state(galec::ast::Name::ident(name)),
                        value: galec::ast::Expression::Real(value),
                    },
                    span: statement_span,
                });
            }
            block.do_step.statements.push(galec::ast::Spanned {
                node: galec::ast::Statement::Assignment {
                    target: galec::ast::Reference::state(galec::ast::Name::ident("y")),
                    value: galec::ast::Expression::Real(1.25),
                },
                span: statement_span,
            });
            let product = lower_solve_algorithm_product(traced_package(brand, |issuer| {
                issuer.construct(
                    block,
                    galec::package::AlgorithmCodePackageMetadata::new(
                        vec![None; 2],
                        "period",
                        Vec::new(),
                        galec::package::AlgorithmCodeArithmeticProfile::construct(
                            galec::package::AlgorithmCodeRealFormat::Binary32,
                            galec::package::AlgorithmCodeIntegerFormat::I32,
                            matrix,
                        ),
                    ),
                )
            }))
            .expect("the scalar assignment has a total Solve refinement");
            let method = product
                .solve_algorithm_block()
                .method(solve::SolveAlgorithmMethodKind::DoStep);

            assert_eq!(method.actions().len(), 1);
            assert_eq!(method.storage_bindings().len(), 1);
            assert_eq!(method.storage_bindings()[0].declaration(), 0);
            assert_eq!(
                method.storage_bindings()[0].logical_storage(),
                product.solve_algorithm_block().declarations()[0].logical_storage()
            );
            assert_eq!(
                method.error_effects(),
                solve::SolveAlgorithmErrorEffects::ResetOnly
            );
            assert_eq!(
                method.abi(),
                solve::SolveAlgorithmMethodAbi::ParameterFreeInfallible
            );
            assert!(matches!(
                method.actions()[0].kind(),
                solve::SolveAlgorithmActionKind::AssignScalarLiteral { declaration: 0, .. }
            ));
            let operation_run = match method.actions()[0].execution() {
                solve::SolveAlgorithmActionExecution::ProgramOwned { operations } => operations,
                solve::SolveAlgorithmActionExecution::StartupOwned => {
                    panic!("the DoStep assignment must be owned by its typed program")
                }
            };
            assert_eq!(operation_run.first(), 0);
            assert_eq!(operation_run.count(), 2);
            assert_eq!(operation_run.end(), 2);
            assert_eq!(method.program().slots().len(), 1);
            assert_eq!(method.program().operations().len(), 2);
            assert!(matches!(
                method.program().operations()[0].operation(),
                solve::SolveOperation::Constant { .. }
            ));
            assert!(matches!(
                method.program().operations()[1].operation(),
                solve::SolveOperation::Store { .. }
            ));
        });
    }

    #[test]
    fn unsupported_expression_fails_at_issued_source_provenance() {
        rumoca_core::with_target_invocation_brand(|brand| {
            let matrix = RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero;
            let source = rumoca_core::SourceId::from_source_name("unsupported-expression.mo");
            let statement_span = Span::from_offsets(source, 30, 48);
            let mut block = galec::Block::new(galec::ast::Name::ident("UnsupportedExpression"));
            block.interface.push(galec::ast::InterfaceVariable {
                kind: galec::ast::InterfaceKind::Output,
                decl: galec::ast::VariableDeclaration::scalar(
                    galec::ast::ScalarType::Real,
                    galec::ast::Name::ident("y"),
                ),
                start: Some(galec::ast::Expression::Real(0.0)),
            });
            block.protected.push(galec::ast::ProtectedEntity {
                kind: galec::ast::ProtectedKind::Constant,
                decl: galec::ast::VariableDeclaration::scalar(
                    galec::ast::ScalarType::Real,
                    galec::ast::Name::ident("period"),
                ),
                start: Some(galec::ast::Expression::Real(0.01)),
            });
            for (name, value) in [("y", 0.0), ("period", 0.01)] {
                block.startup.statements.push(galec::ast::Spanned {
                    node: galec::ast::Statement::Assignment {
                        target: galec::ast::Reference::state(galec::ast::Name::ident(name)),
                        value: galec::ast::Expression::Real(value),
                    },
                    span: statement_span,
                });
            }
            block.do_step.statements.push(galec::ast::Spanned {
                node: galec::ast::Statement::Assignment {
                    target: galec::ast::Reference::state(galec::ast::Name::ident("y")),
                    value: galec::ast::Expression::binary(
                        galec::ast::BinaryOp::Add,
                        galec::ast::Expression::Real(1.0),
                        galec::ast::Expression::Real(2.0),
                    ),
                },
                span: statement_span,
            });
            let error = lower_solve_algorithm_product(traced_package(brand, |issuer| {
                issuer.construct(
                    block,
                    galec::package::AlgorithmCodePackageMetadata::new(
                        vec![None; 2],
                        "period",
                        Vec::new(),
                        galec::package::AlgorithmCodeArithmeticProfile::construct(
                            galec::package::AlgorithmCodeRealFormat::Binary32,
                            galec::package::AlgorithmCodeIntegerFormat::I32,
                            matrix,
                        ),
                    ),
                )
            }))
            .expect_err("unsupported behavior cannot expose a partial Solve root");

            assert_eq!(error.source_span(), Some(statement_span));
            assert!(matches!(
                error,
                AlgorithmLowerError::Construction(
                    solve::SolveAlgorithmBlockConstructionError::UnsupportedExpression {
                        kind: galec::package::AlgorithmCodeExpressionKind::Binary(
                            galec::package::AlgorithmCodeBinaryOperator::Add
                        ),
                        ..
                    }
                )
            ));
        });
    }
}
