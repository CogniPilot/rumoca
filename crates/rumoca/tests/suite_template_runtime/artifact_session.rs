pub(crate) fn pinned_artifact_input() -> rumoca_compile::codegen::targets::ArtifactSessionInput {
    rumoca_compile::codegen::targets::ArtifactSessionInput::construct(
        "2026-08-30T00:00:00Z"
            .parse()
            .expect("test artifact instant is canonical"),
        "12345678-1234-5678-9234-567812345678"
            .parse()
            .expect("test artifact seed is canonical"),
    )
}
