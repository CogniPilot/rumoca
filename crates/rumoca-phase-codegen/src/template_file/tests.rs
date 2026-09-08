use super::*;
use rumoca_core::{TargetInvocationBrand, with_target_invocation_brand};

fn algorithm_code_file<'inv, 'path>(
    brand: TargetInvocationBrand<'inv>,
    output_path: &'path AlgorithmCodeSourceOutputPathTemplate,
    kind: TemplateArtifactKind,
) -> Result<AlgorithmCodeTemplateFile<'inv, 'path>, TemplateFilePlanError> {
    AlgorithmCodeTemplateFile::construct(
        brand,
        kind,
        TemplateSemanticContext::Galec,
        output_path,
        "body",
    )
}

fn correlated_algorithm_code_file<'inv>(
    brand: TargetInvocationBrand<'inv>,
    kind: TemplateArtifactKind,
    context: TemplateSemanticContext,
) -> Result<CorrelatedAlgorithmCodeTemplateFile<'inv>, TemplateFilePlanError> {
    let role = if kind == TemplateArtifactKind::AlgorithmCode {
        CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeSource
    } else {
        CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeManifest
    };
    CorrelatedAlgorithmCodeTemplateFile::construct(brand, role, kind, context, "body")
}

fn packaged_algorithm_code_file<'inv>(
    brand: TargetInvocationBrand<'inv>,
    kind: TemplateArtifactKind,
    context: TemplateSemanticContext,
) -> Result<PackagedAlgorithmCodeTemplateFile<'inv>, TemplateFilePlanError> {
    let role = if kind == TemplateArtifactKind::AlgorithmCode {
        AlgorithmCodeArtifactRole::AlgorithmCodeSource
    } else {
        AlgorithmCodeArtifactRole::AlgorithmCodeManifest
    };
    PackagedAlgorithmCodeTemplateFile::construct(brand, role, kind, context, "body")
}

#[test]
fn generic_algorithm_code_admits_source_only() {
    with_target_invocation_brand(|brand| {
        let output_path =
            AlgorithmCodeSourceOutputPathTemplate::construct("checked.alg".into()).unwrap();
        for kind in TemplateArtifactKind::ALL {
            let result = algorithm_code_file(brand, &output_path, *kind);
            let admitted = *kind == TemplateArtifactKind::AlgorithmCode;
            assert_eq!(result.is_ok(), admitted, "kind {kind:?}");
        }
    });
}

#[test]
fn packaged_algorithm_code_has_no_path_and_admits_only_galec_source_or_xml() {
    with_target_invocation_brand(|brand| {
        for context in TemplateSemanticContext::ALL {
            for kind in TemplateArtifactKind::ALL {
                let admitted = *context == TemplateSemanticContext::Galec
                    && matches!(
                        kind,
                        TemplateArtifactKind::AlgorithmCode | TemplateArtifactKind::Xml
                    );
                assert_eq!(
                    packaged_algorithm_code_file(brand, *kind, *context).is_ok(),
                    admitted,
                    "kind={kind:?} context={context:?}"
                );
            }
        }
    });

    let source = include_str!("../template_file.rs");
    let declaration = source
        .split("pub struct PackagedAlgorithmCodeTemplateFile")
        .nth(1)
        .and_then(|tail| {
            tail.split("impl<'inv> PackagedAlgorithmCodeTemplateFile")
                .next()
        })
        .expect("the packaged carrier declaration remains visible to the gate");
    assert!(!declaration.contains("path"));
    assert!(!declaration.contains("Deserialize"));
}

#[test]
fn algorithm_code_can_never_form_a_c_family_file_plan() {
    with_target_invocation_brand(|brand| {
        let output_path =
            AlgorithmCodeSourceOutputPathTemplate::construct("checked.alg".into()).unwrap();
        for kind in [
            TemplateArtifactKind::CHeader,
            TemplateArtifactKind::CSource,
            TemplateArtifactKind::CudaSource,
        ] {
            assert_eq!(
                algorithm_code_file(brand, &output_path, kind).unwrap_err(),
                TemplateFilePlanError::ArtifactForbiddenForContext {
                    artifact_kind: kind,
                    semantic_context: TemplateSemanticContext::Galec,
                }
            );
        }
    });
}

#[test]
fn algorithm_code_cannot_be_relabelled_as_a_solve_root() {
    with_target_invocation_brand(|brand| {
        let output_path =
            AlgorithmCodeSourceOutputPathTemplate::construct("checked.alg".into()).unwrap();
        let error = AlgorithmCodeTemplateFile::construct(
            brand,
            TemplateArtifactKind::AlgorithmCode,
            TemplateSemanticContext::Solve,
            &output_path,
            "body",
        )
        .unwrap_err();
        assert_eq!(
            error,
            TemplateFilePlanError::SemanticContextMismatch {
                expected: TemplateSemanticContext::Galec,
                actual: TemplateSemanticContext::Solve,
            }
        );
    });
}

#[test]
fn correlated_algorithm_code_file_has_no_path_and_admits_only_galec_source_or_xml() {
    with_target_invocation_brand(|brand| {
        for context in TemplateSemanticContext::ALL {
            for kind in TemplateArtifactKind::ALL {
                let admitted = *context == TemplateSemanticContext::Galec
                    && matches!(
                        kind,
                        TemplateArtifactKind::AlgorithmCode | TemplateArtifactKind::Xml
                    );
                assert_eq!(
                    correlated_algorithm_code_file(brand, *kind, *context).is_ok(),
                    admitted,
                    "kind={kind:?} context={context:?}"
                );
            }
        }
    });

    let source = include_str!("../template_file.rs");
    let declaration = source
        .split("pub struct CorrelatedAlgorithmCodeTemplateFile")
        .nth(1)
        .and_then(|tail| {
            tail.split("impl<'inv> CorrelatedAlgorithmCodeTemplateFile")
                .next()
        })
        .expect("the correlated carrier declaration remains visible to the gate");
    assert!(!declaration.contains("path"));
    assert!(!declaration.contains("Deserialize"));
}

#[test]
fn solve_algorithm_admits_no_algorithm_code_source() {
    with_target_invocation_brand(|brand| {
        let error = SolveAlgorithmTemplateFile::construct(
            brand,
            ProductionCodeFileRole::Source,
            TemplateArtifactKind::AlgorithmCode,
            TemplateSemanticContext::Solve,
            "body",
        )
        .unwrap_err();
        assert_eq!(
            error,
            TemplateFilePlanError::ProductionCodeFileRoleMismatch {
                role: ProductionCodeFileRole::Source,
                artifact_kind: TemplateArtifactKind::AlgorithmCode,
            }
        );
    });
}

#[test]
fn shared_xml_kind_keeps_independent_semantic_authority() {
    with_target_invocation_brand(|brand| {
        let algorithm_code = packaged_algorithm_code_file(
            brand,
            TemplateArtifactKind::Xml,
            TemplateSemanticContext::Galec,
        )
        .unwrap();
        let solve = SolveAlgorithmTemplateFile::construct(
            brand,
            ProductionCodeFileRole::Manifest,
            TemplateArtifactKind::Xml,
            TemplateSemanticContext::Solve,
            "<manifest/>",
        )
        .unwrap();
        assert_eq!(
            algorithm_code.semantic_context(),
            TemplateSemanticContext::Galec
        );
        assert_eq!(solve.semantic_context(), TemplateSemanticContext::Solve);
    });
}

#[test]
fn construction_rejects_unsafe_path_or_empty_body_before_a_plan_exists() {
    for path in [
        "",
        "  ",
        ".",
        "..",
        "AlgorithmCode/model.alg",
        "../model.alg",
        "{{ model_name }}.alg",
        "model\\nested.alg",
        "model.txt",
        "NUL.alg",
    ] {
        assert_eq!(
            AlgorithmCodeSourceOutputPathTemplate::construct(path.into()).unwrap_err(),
            AlgorithmCodeSourceOutputPathError::NonPortableBasename
        );
    }
    let output_path = AlgorithmCodeSourceOutputPathTemplate::construct("model.alg".into()).unwrap();
    with_target_invocation_brand(|brand| {
        assert_eq!(
            AlgorithmCodeTemplateFile::construct(
                brand,
                TemplateArtifactKind::AlgorithmCode,
                TemplateSemanticContext::Galec,
                &output_path,
                "\n\t",
            )
            .unwrap_err(),
            TemplateFilePlanError::EmptyBodyTemplate
        );
    });
}

#[test]
fn debug_output_does_not_copy_either_template_string() {
    with_target_invocation_brand(|brand| {
        let output_path =
            AlgorithmCodeSourceOutputPathTemplate::construct("secret-output-path.alg".into())
                .unwrap();
        let file = AlgorithmCodeTemplateFile::construct(
            brand,
            TemplateArtifactKind::AlgorithmCode,
            TemplateSemanticContext::Galec,
            &output_path,
            "secret-template-body",
        )
        .unwrap();
        let debug = format!("{file:?}");
        assert!(!debug.contains("secret-output-path"));
        assert!(!debug.contains("secret-template-body"));
    });
}

#[test]
fn admission_uses_no_file_extension_or_target_name_dispatch() {
    let source = include_str!("../template_file.rs");
    assert!(!source.contains("target_name"));
    assert!(!source.contains("target =="));
}

#[test]
fn semantic_context_vocabulary_is_exactly_the_ir_crate_inventory() {
    let crate_directory = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("phase-codegen crate is below the workspace crates directory");
    let mut actual = std::fs::read_dir(crate_directory)
        .expect("read workspace crates directory")
        .map(|entry| entry.expect("read workspace crate entry"))
        .filter(|entry| entry.file_type().is_ok_and(|kind| kind.is_dir()))
        .filter_map(|entry| {
            entry
                .file_name()
                .to_str()
                .and_then(|name| name.strip_prefix("rumoca-ir-").map(str::to_owned))
        })
        .collect::<Vec<_>>();
    actual.sort();
    let mut declared = TemplateSemanticContext::ALL
        .iter()
        .map(|context| context.as_str().to_owned())
        .collect::<Vec<_>>();
    declared.sort();
    assert_eq!(declared, actual);
}
