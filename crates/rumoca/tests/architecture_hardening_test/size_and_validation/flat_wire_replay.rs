use std::fs;

use crate::architecture_hardening_support::workspace_root;

#[test]
fn flat_model_replay_stays_root_checked_and_non_defaulting() {
    let root = workspace_root();
    let model_source = fs::read_to_string(root.join("crates/rumoca-ir-flat/src/lib.rs"))
        .expect("read Flat model source");
    let wire_source = fs::read_to_string(root.join("crates/rumoca-ir-flat/src/wire.rs"))
        .expect("read Flat wire checker");
    let model_declaration = model_source
        .split("pub struct Model {")
        .next()
        .and_then(|prefix| prefix.rsplit("#[derive(").next())
        .expect("Flat Model has an explicit derive list");
    assert!(
        !model_declaration.contains("Deserialize"),
        "Flat Model must replay through its checked root wire, never fieldwise Deserialize"
    );
    assert!(
        wire_source.contains("struct ModelWire")
            && wire_source.contains("impl<'de> Deserialize<'de> for Model"),
        "Flat Model must retain one private operation-shaped replay root"
    );
    assert!(
        !wire_source.contains("#[serde(default"),
        "semantic Flat wire fields must be explicit; defaulting can silently invent IR"
    );
    assert!(
        wire_source.matches("#[serde(deny_unknown_fields)]").count() >= 8,
        "every private Flat replay operation must reject unknown semantic fields"
    );
}

#[test]
fn structured_binder_identity_cannot_regress_to_raw_ordinals() {
    let root = workspace_root();
    let domain_source =
        fs::read_to_string(root.join("crates/rumoca-core/src/structured_domain.rs"))
            .expect("read structured-domain source");
    let reference_source = fs::read_to_string(root.join("crates/rumoca-core/src/ir_primitives.rs"))
        .expect("read semantic-reference source");

    assert!(
        domain_source.contains("pub id: StructuredIndexBinderId"),
        "structured binder declarations must carry the dedicated typed identity"
    );
    assert!(
        reference_source.contains("structured_binder: Option<StructuredIndexBinderId>"),
        "structured binder occurrences must carry the same typed domain identity"
    );
    assert!(
        !domain_source.contains("pub id: usize"),
        "raw binder ordinals must not re-enter the valid-by-construction domain"
    );
}

#[test]
fn structured_equation_rows_have_one_live_and_wire_proof_constructor() {
    let root = workspace_root();
    let owner_source =
        fs::read_to_string(root.join("crates/rumoca-ir-flat/src/structured_equation_owners.rs"))
            .expect("read Flat structured-equation owner proof");
    let wire_source = fs::read_to_string(root.join("crates/rumoca-ir-flat/src/wire.rs"))
        .expect("read Flat wire checker");
    let wire_semantics =
        fs::read_to_string(root.join("crates/rumoca-ir-flat/src/wire/semantics.rs"))
            .expect("read Flat wire semantics");

    assert!(
        owner_source.contains("pub fn structured_equation_owners(")
            && owner_source.contains("CheckedStructuredEquationOwners")
            && wire_source.contains(".structured_equation_owners()"),
        "live Flat consumers and wire replay must use the same checked row-owner constructor"
    );
    for deleted_duplicate in [
        "validate_wire_equation_partition",
        "validate_structured_row_correlation",
        "validate_regular_family_claim",
        "represented_family_rows",
    ] {
        assert!(
            !wire_source.contains(deleted_duplicate) && !wire_semantics.contains(deleted_duplicate),
            "deleted Flat row-proof route `{deleted_duplicate}` must not return"
        );
    }
}

#[test]
fn connection_trace_origins_cannot_become_replay_authority() {
    let root = workspace_root();
    let flat_source = fs::read_to_string(root.join("crates/rumoca-ir-flat/src/lib.rs"))
        .expect("read Flat origin schema");
    let wire_source = fs::read_to_string(root.join("crates/rumoca-ir-flat/src/wire.rs"))
        .expect("read Flat wire checker");
    let origin_replay = wire_source
        .split_once("impl WireEquationOrigin {")
        .and_then(|(_, suffix)| suffix.split_once("impl From<&Equation>"))
        .map(|(implementation, _)| implementation)
        .expect("Flat wire keeps one explicit equation-origin replay implementation");

    assert!(
        !flat_source.contains("ConnectionEndpoint")
            && !flat_source.contains("pub struct SignedConnectionMember")
            && !wire_source.contains("fn validate_outside_stream(")
            && !wire_source.contains("fn validate_equality_constraint("),
        "standalone equation origins must not regain caller-authored endpoint or derived connection authority"
    );
    assert!(
        origin_replay.contains(
            "connection-derived rows cannot replay without canonical Instance source groups and an atomic connection transaction"
        ) && origin_replay.contains("Self::Connection { .. }")
            && origin_replay.contains("Self::OutsideStream { .. }")
            && origin_replay.contains("Self::EqualityConstraint { .. }")
            && origin_replay.contains("Self::FlowSum { .. }")
            && origin_replay.contains("Self::UnconnectedFlow { .. }"),
        "standalone Flat replay must refuse connection-derived rows before treating trace origins as proof"
    );
}

#[test]
fn stream_rewrite_apply_requires_a_consumed_validation_token() {
    let root = workspace_root();
    let stream_parent = fs::read_to_string(
        root.join("crates/rumoca-phase-flatten/src/connections/stream_operators.rs"),
    )
    .expect("read stream operator parent module");
    let projection_source = fs::read_to_string(
        root.join("crates/rumoca-phase-flatten/src/connections/stream_operators/projection.rs"),
    )
    .expect("read stream rewrite projection module");
    let transaction_source =
        fs::read_to_string(root.join("crates/rumoca-phase-flatten/src/connections/transaction.rs"))
            .expect("read connection commit transaction");
    let raw_projection_impl = projection_source
        .split_once("impl StreamRewriteProjection {")
        .and_then(|(_, suffix)| suffix.split_once("impl ValidatedStreamRewrite"))
        .map(|(projection, _)| projection)
        .expect("stream transaction keeps separate raw and validated implementations");
    let validated_impl = projection_source
        .split_once("impl ValidatedStreamRewrite")
        .map(|(_, implementation)| implementation)
        .expect("stream transaction keeps its consuming validation token");

    assert!(
        stream_parent.contains("mod projection;")
            && projection_source.contains("struct ValidatedStreamRewrite")
            && projection_source.contains(
                "struct ValidatedStreamRewrite {\n    projection: StreamRewriteProjection"
            )
            && !projection_source.contains("pub projection: StreamRewriteProjection")
            && projection_source.contains("self.validate_matches(model)?;")
            && transaction_source.contains("self.stream_rewrite.validate(flat)?")
            && transaction_source.contains("validated_stream.apply(flat);")
            && !raw_projection_impl.contains("fn apply(")
            && validated_impl.contains("fn apply(self, model: &mut flat::Model)")
            && validated_impl.contains("let projection = self.projection;")
            && !projection_source.contains("commit_prevalidated"),
        "a raw stream rewrite projection must be consumed into a private-field, single-use validated token before apply"
    );
}

#[test]
fn canonical_connection_sources_require_pruning_and_augmentation_preflight() {
    let root = workspace_root();
    let source = fs::read_to_string(
        root.join("crates/rumoca-phase-flatten/src/connections/source_inventory.rs"),
    )
    .expect("read connection source transaction");

    assert!(
        source.contains("pub(super) struct PrunedConnectionSources")
            && source.contains("pub(super) fn plan_after_expandable_check(")
            && source.contains("fn plan(self)")
            && !source.contains("pub(super) fn plan(self)"),
        "canonical source positions must be unavailable until false pruning and expandable augmentation preflight succeed"
    );
}

#[test]
fn connection_transaction_files_remain_split_by_semantic_owner() {
    let root = workspace_root();
    let connections = root.join("crates/rumoca-phase-flatten/src/connections");
    let module = fs::read_to_string(connections.join("mod.rs")).expect("read connection module");
    let equations = fs::read_to_string(connections.join("equation_generation.rs"))
        .expect("read topology orchestration module");
    let equality = fs::read_to_string(connections.join("equality_projection.rs"))
        .expect("read equality projection module");
    let stream = fs::read_to_string(connections.join("stream_operators.rs"))
        .expect("read stream semantics module");
    let projection = fs::read_to_string(connections.join("stream_operators/projection.rs"))
        .expect("read stream projection module");

    assert!(
        module.contains("mod equality_projection;")
            && !equations.contains("fn plan_equality_constraint_equation(")
            && equality.contains("fn checked_record_edge")
            && equality.contains("fn checked_equality_function")
            && stream.contains("mod projection;")
            && !stream.contains("struct StreamRewriteProjection")
            && projection.contains("struct StreamRewriteProjection"),
        "topology, equality ABI validation, stream semantics, and stream owner snapshots must retain separate module owners"
    );
}
