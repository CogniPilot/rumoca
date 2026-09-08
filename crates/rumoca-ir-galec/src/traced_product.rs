//! Affine semantic and trace authority for one Algorithm Code projection.

use std::marker::PhantomData;

use rumoca_core::{SourceId, SourceMap, Span, TargetInvocationBrand};

use crate::package::{
    AlgorithmCodePackage, AlgorithmCodePackageMetadata, AlgorithmCodeSubject, PackageError,
    SemanticProvenance,
};

/// Exact source-model identity retained by one Algorithm Code projection.
#[derive(Debug, PartialEq, Eq)]
pub struct AlgorithmCodeSemanticModelIdentity(Box<str>);

impl AlgorithmCodeSemanticModelIdentity {
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Failure to close a checked package under its construction-issued origin.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum AlgorithmCodeTraceOriginError {
    #[error("semantic model identity must not be empty")]
    EmptySemanticModelIdentity,
    #[error("Algorithm Code provenance references source {source_id:?} absent from its origin map")]
    MissingProvenanceSource { source_id: SourceId },
    #[error(
        "Algorithm Code provenance span {span:?} exceeds its {source_bytes}-byte origin source"
    )]
    ProvenanceSpanOutOfBounds { span: Span, source_bytes: usize },
    #[error("Algorithm Code provenance span {span:?} offset {offset} is not a UTF-8 boundary")]
    ProvenanceSpanNotUtf8Boundary { span: Span, offset: usize },
}

/// Failure of either semantic projection or origin closure.
#[derive(Debug, PartialEq, Eq)]
pub enum AlgorithmCodeOriginProjectionError<E> {
    Projection(E),
    Origin(AlgorithmCodeTraceOriginError),
}

/// Audit identity of the exact ordered source-map bytes retained by a product.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AlgorithmCodeSourceContentDigest([u8; 32]);

impl AlgorithmCodeSourceContentDigest {
    #[must_use]
    pub const fn as_bytes(self) -> [u8; 32] {
        self.0
    }
}

/// The only authority that can close a package for one source-origin session.
///
/// The issuer is invariant, affine, and consumed by [`Self::construct`]. It is
/// minted only inside [`TracedAlgorithmCodeProduct::project_from_origin`].
/// Existing raw packages cannot be admitted into an origin session.
///
/// ```compile_fail
/// use rumoca_ir_galec::AlgorithmCodePackageIssuer;
/// fn duplicate<'origin>(issuer: &AlgorithmCodePackageIssuer<'origin>) -> AlgorithmCodePackageIssuer<'origin> {
///     issuer.clone()
/// }
/// ```
pub struct AlgorithmCodePackageIssuer<'origin> {
    _brand: PhantomData<fn(&'origin mut ()) -> &'origin mut ()>,
}

impl<'origin> AlgorithmCodePackageIssuer<'origin> {
    fn mint(_scope: &'origin mut ()) -> Self {
        Self {
            _brand: PhantomData,
        }
    }

    /// Atomically construct and brand one package for this origin session.
    pub fn construct(
        self,
        block: crate::Block,
        metadata: AlgorithmCodePackageMetadata,
    ) -> Result<OriginBoundAlgorithmCodePackage<'origin>, PackageError> {
        AlgorithmCodePackage::construct(block, metadata).map(|package| {
            OriginBoundAlgorithmCodePackage {
                package,
                _brand: self._brand,
            }
        })
    }
}

/// A checked package that can close only the source-origin session that issued it.
///
/// There is intentionally no raw-package accessor or public constructor. The
/// value is affine and is consumed inside the higher-ranked origin callback.
///
/// ```compile_fail
/// use rumoca_ir_galec::OriginBoundAlgorithmCodePackage;
/// fn duplicate<'origin>(package: &OriginBoundAlgorithmCodePackage<'origin>) -> OriginBoundAlgorithmCodePackage<'origin> {
///     package.clone()
/// }
/// ```
///
/// ```compile_fail
/// use rumoca_ir_galec::OriginBoundAlgorithmCodePackage;
/// let _ = OriginBoundAlgorithmCodePackage::default();
/// ```
pub struct OriginBoundAlgorithmCodePackage<'origin> {
    package: AlgorithmCodePackage,
    _brand: PhantomData<fn(&'origin mut ()) -> &'origin mut ()>,
}

/// One non-cloneable Algorithm Code package and its exact trace authority.
///
/// The package retains its existing wire representation. The invocation brand,
/// shared source snapshot, and semantic model identity live only in this
/// affine orchestration product and are therefore neither serialized nor
/// deserialized independently.
///
/// ```compile_fail
/// use rumoca_ir_galec::TracedAlgorithmCodeProduct;
/// fn duplicate<'inv>(
///     product: &TracedAlgorithmCodeProduct<'inv>,
/// ) -> TracedAlgorithmCodeProduct<'inv> {
///     product.clone()
/// }
/// ```
///
/// ```compile_fail
/// use rumoca_ir_galec::TracedAlgorithmCodeProduct;
/// let _ = TracedAlgorithmCodeProduct::default();
/// ```
pub struct TracedAlgorithmCodeProduct<'inv> {
    brand: TargetInvocationBrand<'inv>,
    package: AlgorithmCodePackage,
    sources: SourceMap,
    source_content_digest: AlgorithmCodeSourceContentDigest,
    semantic_model: AlgorithmCodeSemanticModelIdentity,
}

impl std::fmt::Debug for TracedAlgorithmCodeProduct<'_> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("TracedAlgorithmCodeProduct")
            .field("package", &self.package)
            .field("sources", &self.sources)
            .field("source_content_digest", &self.source_content_digest)
            .field("semantic_model", &self.semantic_model)
            .finish_non_exhaustive()
    }
}

impl<'inv> TracedAlgorithmCodeProduct<'inv> {
    /// Construct one package under the exact source snapshot selected before
    /// semantic projection begins.
    ///
    /// The higher-ranked callback prevents an origin-bound candidate from a
    /// different session from being returned. The callback receives no way to
    /// admit an existing raw package: its affine issuer must construct the
    /// block and metadata directly. The retained snapshot is taken before the
    /// callback runs, so later copy-on-write mutation cannot change the
    /// artifact's trace authority.
    ///
    /// Origin-bound candidates cannot cross sessions, even when two maps use
    /// the same stable source identities:
    ///
    /// ```compile_fail
    /// use rumoca_core::{SourceMap, with_target_invocation_brand};
    /// use rumoca_ir_galec::{
    ///     AlgorithmCodePackageIssuer, OriginBoundAlgorithmCodePackage,
    ///     TracedAlgorithmCodeProduct,
    /// };
    ///
    /// fn require_same<'origin>(
    ///     _candidate: OriginBoundAlgorithmCodePackage<'origin>,
    ///     _issuer: AlgorithmCodePackageIssuer<'origin>,
    /// ) {}
    ///
    /// fn never<T>() -> T {
    ///     loop {}
    /// }
    ///
    /// with_target_invocation_brand(|brand| {
    ///     let first = SourceMap::new();
    ///     let second = SourceMap::new();
    ///     let _ = TracedAlgorithmCodeProduct::project_from_origin(
    ///         brand,
    ///         &first,
    ///         "First",
    ///         |first_issuer| {
    ///             let first_candidate = first_issuer
    ///                 .construct(never(), never())
    ///                 .unwrap();
    ///             let _ = TracedAlgorithmCodeProduct::project_from_origin(
    ///                 brand,
    ///                 &second,
    ///                 "Second",
    ///                 |second_issuer| {
    ///                     require_same(first_candidate, second_issuer);
    ///                     unreachable!()
    ///                 },
    ///             );
    ///             unreachable!()
    ///         },
    ///     );
    /// });
    /// ```
    pub fn project_from_origin<E>(
        brand: TargetInvocationBrand<'inv>,
        sources: &SourceMap,
        semantic_model: &str,
        project: impl for<'origin> FnOnce(
            AlgorithmCodePackageIssuer<'origin>,
        )
            -> Result<OriginBoundAlgorithmCodePackage<'origin>, E>,
    ) -> Result<Self, AlgorithmCodeOriginProjectionError<E>> {
        let sources = sources.shared_snapshot();
        let source_content_digest = AlgorithmCodeSourceContentDigest(sources.content_digest());
        let semantic_model = Box::<str>::from(semantic_model);
        if semantic_model.is_empty() {
            return Err(AlgorithmCodeOriginProjectionError::Origin(
                AlgorithmCodeTraceOriginError::EmptySemanticModelIdentity,
            ));
        }
        let package = consume_origin_candidate(project)
            .map_err(AlgorithmCodeOriginProjectionError::Projection)?;
        let trace_error = package.inspect(|inspection| {
            inspection
                .subjects()
                .find_map(|subject| validate_subject_origin(subject, &sources))
        });
        if let Some(error) = trace_error {
            return Err(AlgorithmCodeOriginProjectionError::Origin(error));
        }
        Ok(Self {
            brand,
            package,
            sources,
            source_content_digest,
            semantic_model: AlgorithmCodeSemanticModelIdentity(semantic_model),
        })
    }

    #[must_use]
    pub const fn brand(&self) -> TargetInvocationBrand<'inv> {
        self.brand
    }

    #[must_use]
    pub const fn package(&self) -> &AlgorithmCodePackage {
        &self.package
    }

    #[must_use]
    pub const fn sources(&self) -> &SourceMap {
        &self.sources
    }

    #[must_use]
    pub const fn source_content_digest(&self) -> AlgorithmCodeSourceContentDigest {
        self.source_content_digest
    }

    #[must_use]
    pub const fn semantic_model(&self) -> &AlgorithmCodeSemanticModelIdentity {
        &self.semantic_model
    }
}

fn validate_subject_origin(
    subject: AlgorithmCodeSubject<'_, '_>,
    sources: &SourceMap,
) -> Option<AlgorithmCodeTraceOriginError> {
    let span = match subject.provenance() {
        SemanticProvenance::Exact(span) | SemanticProvenance::NearestStatement(span) => span,
        SemanticProvenance::Generated(_) => return None,
    };
    let Some((_, source_text)) = sources.get_source(span.source) else {
        return Some(AlgorithmCodeTraceOriginError::MissingProvenanceSource {
            source_id: span.source,
        });
    };
    validate_source_span(span, source_text)
}

fn validate_source_span(span: Span, source_text: &str) -> Option<AlgorithmCodeTraceOriginError> {
    if span.start.0 > span.end.0 || span.end.0 > source_text.len() {
        return Some(AlgorithmCodeTraceOriginError::ProvenanceSpanOutOfBounds {
            span,
            source_bytes: source_text.len(),
        });
    }
    [span.start.0, span.end.0]
        .into_iter()
        .find(|offset| !source_text.is_char_boundary(*offset))
        .map(|offset| AlgorithmCodeTraceOriginError::ProvenanceSpanNotUtf8Boundary { span, offset })
}

fn consume_origin_candidate<E>(
    project: impl for<'origin> FnOnce(
        AlgorithmCodePackageIssuer<'origin>,
    ) -> Result<OriginBoundAlgorithmCodePackage<'origin>, E>,
) -> Result<AlgorithmCodePackage, E> {
    fn scoped<'origin, E>(
        scope: &'origin mut (),
        project: impl FnOnce(
            AlgorithmCodePackageIssuer<'origin>,
        ) -> Result<OriginBoundAlgorithmCodePackage<'origin>, E>,
    ) -> Result<AlgorithmCodePackage, E> {
        project(AlgorithmCodePackageIssuer::mint(scope)).map(|candidate| candidate.package)
    }

    let mut scope = ();
    scoped(&mut scope, project)
}

#[cfg(test)]
mod tests {
    use rumoca_core::{RealMatrixMultiplySemantics, SourceId, Span, with_target_invocation_brand};

    use super::*;
    use crate::package::{AlgorithmCodeArithmeticProfile, AlgorithmCodePackageMetadata};

    fn block_and_metadata(span: Span) -> (crate::Block, AlgorithmCodePackageMetadata) {
        let mut block = crate::Block::new(crate::ast::Name::ident("TraceFixture"));
        let mut declaration = crate::ast::VariableDeclaration::scalar(
            crate::ast::ScalarType::Real,
            crate::ast::Name::ident("period"),
        );
        declaration.span = span;
        block.protected.push(crate::ast::ProtectedEntity {
            kind: crate::ast::ProtectedKind::Constant,
            decl: declaration,
            start: Some(crate::ast::Expression::Real(0.1)),
        });
        block.startup.statements.push(crate::ast::Spanned {
            node: crate::ast::Statement::Assignment {
                target: crate::ast::Reference::state(crate::ast::Name::ident("period")),
                value: crate::ast::Expression::Real(0.1),
            },
            span,
        });
        let metadata = AlgorithmCodePackageMetadata::new(
            vec![None],
            "period",
            Vec::new(),
            AlgorithmCodeArithmeticProfile::construct(
                crate::package::AlgorithmCodeRealFormat::Binary64,
                crate::package::AlgorithmCodeIntegerFormat::I32,
                RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
            ),
        );
        (block, metadata)
    }

    fn raw_package(span: Span) -> AlgorithmCodePackage {
        let (block, metadata) = block_and_metadata(span);
        AlgorithmCodePackage::construct(block, metadata)
            .expect("trace fixture is checked Algorithm Code")
    }

    #[test]
    fn construction_retains_the_exact_shared_snapshot_and_package_wire() {
        with_target_invocation_brand(|brand| {
            let mut sources = SourceMap::new();
            let source = sources.add("trace-fixture.mo", "period := 0.1;\n");
            let span = Span::from_offsets(source, 0, 14);
            let before = serde_json::to_vec(&raw_package(span)).expect("package wire serializes");
            let product = TracedAlgorithmCodeProduct::project_from_origin(
                brand,
                &sources,
                "Pkg.TraceFixture",
                |issuer| {
                    let (block, metadata) = block_and_metadata(span);
                    issuer.construct(block, metadata)
                },
            )
            .expect("the exact source map covers every retained span");

            sources.add("later.mo", "not part of the issued trace snapshot");
            assert_eq!(
                product.sources().get_source(source),
                Some(("trace-fixture.mo", "period := 0.1;\n")),
            );
            assert!(product.sources().get_id("later.mo").is_none());
            assert_eq!(product.semantic_model().as_str(), "Pkg.TraceFixture");
            assert_eq!(
                serde_json::to_vec(product.package()).expect("retained package wire serializes"),
                before,
                "trace authority is outside the unchanged AlgorithmCodePackage wire",
            );
        });
    }

    #[test]
    fn construction_refuses_an_origin_that_does_not_cover_exact_provenance() {
        with_target_invocation_brand(|brand| {
            let source = SourceId::from_source_name("missing.mo");
            let span = Span::from_offsets(source, 4, 9);
            let error = TracedAlgorithmCodeProduct::project_from_origin(
                brand,
                &SourceMap::new(),
                "TraceFixture",
                |issuer| {
                    let (block, metadata) = block_and_metadata(span);
                    issuer.construct(block, metadata)
                },
            )
            .expect_err("an unresolved exact span cannot enter a traced product");
            assert_eq!(
                error,
                AlgorithmCodeOriginProjectionError::Origin(
                    AlgorithmCodeTraceOriginError::MissingProvenanceSource { source_id: source },
                ),
            );
        });
    }

    #[test]
    fn construction_refuses_provenance_outside_the_exact_source_bytes() {
        with_target_invocation_brand(|brand| {
            let mut sources = SourceMap::new();
            let source = sources.add("short.mo", "short");
            let span = Span::from_offsets(source, 1, 9);
            let error = TracedAlgorithmCodeProduct::project_from_origin(
                brand,
                &sources,
                "TraceFixture",
                |issuer| {
                    let (block, metadata) = block_and_metadata(span);
                    issuer.construct(block, metadata)
                },
            )
            .expect_err("an out-of-bounds retained span cannot enter a traced product");
            assert_eq!(
                error,
                AlgorithmCodeOriginProjectionError::Origin(
                    AlgorithmCodeTraceOriginError::ProvenanceSpanOutOfBounds {
                        span,
                        source_bytes: 5,
                    },
                ),
            );
        });
    }

    #[test]
    fn construction_refuses_reversed_provenance_range() {
        with_target_invocation_brand(|brand| {
            let mut sources = SourceMap::new();
            let source = sources.add("reversed.mo", "short");
            let span = Span::from_offsets(source, 4, 1);
            let error = TracedAlgorithmCodeProduct::project_from_origin(
                brand,
                &sources,
                "TraceFixture",
                |issuer| {
                    let (block, metadata) = block_and_metadata(span);
                    issuer.construct(block, metadata)
                },
            )
            .expect_err("a reversed retained span cannot enter a traced product");
            assert_eq!(
                error,
                AlgorithmCodeOriginProjectionError::Origin(
                    AlgorithmCodeTraceOriginError::ProvenanceSpanOutOfBounds {
                        span,
                        source_bytes: 5,
                    },
                ),
            );
        });
    }

    #[test]
    fn construction_refuses_provenance_inside_a_utf8_code_point() {
        with_target_invocation_brand(|brand| {
            let text = "é := 0.1;";
            let mut sources = SourceMap::new();
            let source = sources.add("unicode.mo", text);
            let span = Span::from_offsets(source, 1, text.len());
            let error = TracedAlgorithmCodeProduct::project_from_origin(
                brand,
                &sources,
                "TraceFixture",
                |issuer| {
                    let (block, metadata) = block_and_metadata(span);
                    issuer.construct(block, metadata)
                },
            )
            .expect_err("a retained span cannot split a UTF-8 code point");
            assert_eq!(
                error,
                AlgorithmCodeOriginProjectionError::Origin(
                    AlgorithmCodeTraceOriginError::ProvenanceSpanNotUtf8Boundary {
                        span,
                        offset: 1,
                    },
                ),
            );
        });
    }

    #[test]
    fn exact_and_nearest_unicode_provenance_close_on_character_boundaries() {
        with_target_invocation_brand(|brand| {
            let text = "é := 0.1;";
            let mut sources = SourceMap::new();
            let source = sources.add("unicode.mo", text);
            let span = Span::from_offsets(source, 0, text.len());
            let product = TracedAlgorithmCodeProduct::project_from_origin(
                brand,
                &sources,
                "TraceFixture",
                |issuer| {
                    let (block, metadata) = block_and_metadata(span);
                    issuer.construct(block, metadata)
                },
            )
            .expect("character-aligned Unicode provenance closes");
            let provenances = product.package().inspect(|inspection| {
                inspection
                    .subjects()
                    .map(|subject| subject.provenance())
                    .collect::<Vec<_>>()
            });
            assert!(provenances.contains(&SemanticProvenance::Exact(span)));
            assert!(provenances.contains(&SemanticProvenance::NearestStatement(span)));
        });
    }

    #[test]
    fn empty_semantic_model_identity_is_refused_before_projection() {
        with_target_invocation_brand(|brand| {
            let error = TracedAlgorithmCodeProduct::project_from_origin::<()>(
                brand,
                &SourceMap::new(),
                "",
                |_issuer| panic!("an invalid origin must not start semantic projection"),
            )
            .expect_err("a traced product cannot lose its semantic model identity");
            assert_eq!(
                error,
                AlgorithmCodeOriginProjectionError::Origin(
                    AlgorithmCodeTraceOriginError::EmptySemanticModelIdentity,
                ),
            );
        });
    }

    #[test]
    fn same_source_id_with_different_bytes_stays_in_separate_origin_sessions() {
        with_target_invocation_brand(|brand| {
            let mut first = SourceMap::new();
            let source = first.add("same-name.mo", "first bytes");
            let mut second = SourceMap::new();
            assert_eq!(second.add("same-name.mo", "second bytes"), source);
            let span = Span::from_offsets(source, 0, 5);
            let first_product =
                TracedAlgorithmCodeProduct::project_from_origin(brand, &first, "First", |issuer| {
                    let (block, metadata) = block_and_metadata(span);
                    issuer.construct(block, metadata)
                })
                .expect("first session closes");
            let second_product = TracedAlgorithmCodeProduct::project_from_origin(
                brand,
                &second,
                "Second",
                |issuer| {
                    let (block, metadata) = block_and_metadata(span);
                    issuer.construct(block, metadata)
                },
            )
            .expect("second session closes independently");
            assert_eq!(
                first_product.sources().get_source(source),
                Some(("same-name.mo", "first bytes")),
            );
            assert_eq!(
                second_product.sources().get_source(source),
                Some(("same-name.mo", "second bytes")),
            );
            assert_ne!(
                first_product.source_content_digest(),
                second_product.source_content_digest(),
                "audit identity must expose different bytes under the same stable SourceId",
            );
        });
    }
}
