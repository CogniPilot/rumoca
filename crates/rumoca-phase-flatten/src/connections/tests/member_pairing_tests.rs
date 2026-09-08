//! MLS §9.3 pairing rules for matched primitive connection members.
//!
//! The acceptance contract these pin is stated in the
//! [`member_pairing`](super::super::member_pairing) module docs: agreeing
//! prefixes connect, structural pairs route to assertion ownership, absent
//! evidence is rejected, and the two provably impossible pairings are reported
//! against both member declarations instead of dropped.

use super::super::*;

fn member_span(offset: usize) -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("connection_member_pairing_tests.mo"),
        offset,
        offset + 4,
    )
}

fn member(span: Span) -> flat::Variable {
    connection_test_variable(span)
}

fn stream_member(span: Span) -> flat::Variable {
    flat::Variable {
        stream: true,
        ..connection_test_variable(span)
    }
}

fn parameter_member(span: Span) -> flat::Variable {
    flat::Variable {
        variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
        ..connection_test_variable(span)
    }
}

fn constant_member(span: Span) -> flat::Variable {
    flat::Variable {
        variability: rumoca_core::Variability::Constant(rumoca_core::Token::default()),
        ..connection_test_variable(span)
    }
}

fn model_with(members: Vec<(&str, flat::Variable)>) -> flat::Model {
    let mut flat = connection_test_model();
    for (name, variable) in members {
        flat.add_test_variable(rumoca_core::VarName::new(name), variable);
    }
    flat
}

fn classify(flat: &flat::Model, a: &str, b: &str) -> Result<MemberPairing, FlattenError> {
    classify_connection_member_pair(
        flat,
        &rumoca_core::VarName::new(a),
        &rumoca_core::VarName::new(b),
        member_span(1),
    )
}

// ---------------------------------------------------------------------------
// Accepted pairings
// ---------------------------------------------------------------------------

/// MLS §9.3 admits stream-to-stream, and MLS §15.2 routes it to a stream set.
#[test]
fn stream_paired_with_stream_connects() {
    let flat = model_with(vec![
        ("a.h_outflow", stream_member(member_span(10))),
        ("b.h_outflow", stream_member(member_span(30))),
    ]);

    assert_eq!(
        classify(&flat, "a.h_outflow", "b.h_outflow").expect("stream-to-stream is admitted"),
        MemberPairing::Connect
    );
}

#[test]
fn non_stream_paired_with_non_stream_connects() {
    let flat = model_with(vec![
        ("a.v", member(member_span(10))),
        ("b.v", member(member_span(30))),
    ]);

    assert_eq!(
        classify(&flat, "a.v", "b.v").expect("two potential members are admitted"),
        MemberPairing::Connect
    );
}

/// MLS §9.3: "Constants or parameters in connected components yield the
/// appropriate assert-statements [...]; connections are not generated."
#[test]
fn parameter_paired_with_parameter_requires_an_assertion() {
    let flat = model_with(vec![
        ("a.m", parameter_member(member_span(10))),
        ("b.m", parameter_member(member_span(30))),
    ]);

    assert_eq!(
        classify(&flat, "a.m", "b.m").expect("parameter-to-parameter is admitted"),
        MemberPairing::StructuralAssertion
    );
}

#[test]
fn constant_paired_with_constant_requires_an_assertion() {
    let flat = model_with(vec![
        ("a.k", constant_member(member_span(10))),
        ("b.k", constant_member(member_span(30))),
    ]);

    assert_eq!(
        classify(&flat, "a.k", "b.k").expect("constant-to-constant is admitted"),
        MemberPairing::StructuralAssertion
    );
}

/// MLS §9.3 says parameter members may connect only to parameter members and
/// constant members only to constant members.
#[test]
fn parameter_paired_with_constant_is_rejected() {
    let flat = model_with(vec![
        ("a.m", parameter_member(member_span(10))),
        ("b.m", constant_member(member_span(30))),
    ]);

    let error = classify(&flat, "a.m", "b.m")
        .expect_err("a parameter/constant pair violates the exact variability rule");
    assert!(matches!(
        error,
        FlattenError::ConnectionVariabilityMismatch {
            a_variability: "parameter",
            b_variability: "constant",
            ..
        }
    ));
}

/// By flatten, both primitive declarations must resolve. Legal indexed members
/// resolve through their declared compact base; an actually absent counterpart
/// is invalid phase evidence, never permission to connect.
#[test]
fn member_without_a_declaration_is_rejected() {
    let flat = model_with(vec![("a.h_outflow", stream_member(member_span(10)))]);

    for (a, b) in [
        ("a.h_outflow", "b.h_outflow"),
        ("b.h_outflow", "a.h_outflow"),
    ] {
        let error = classify(&flat, a, b)
            .expect_err("an absent member declaration cannot prove a legal pairing");
        assert!(matches!(
            error,
            FlattenError::InvalidConnectionEvidence { span, .. } if span == member_span(1)
        ));
    }
}

/// Structural variability on the visible side cannot turn an absent
/// counterpart into evidence. This used to be the most dangerous asymmetric
/// fail-open case because it joined a parameter/constant to a potential set.
#[test]
fn structural_member_with_an_unresolvable_counterpart_is_rejected() {
    for visible in [
        parameter_member(member_span(10)),
        constant_member(member_span(10)),
    ] {
        let flat = model_with(vec![("a.m", visible)]);
        for (a, b) in [("a.m", "b.m"), ("b.m", "a.m")] {
            assert!(matches!(
                classify(&flat, a, b),
                Err(FlattenError::InvalidConnectionEvidence { .. })
            ));
        }
    }
}

/// An element path is resolved through its declared base, exactly like the
/// `flow` and dimension questions asked about the same member.
#[test]
fn element_paths_resolve_through_their_declared_base() {
    let mut declaration = stream_member(member_span(10));
    declaration.dims = vec![3];
    let flat = model_with(vec![
        ("a.h_outflow", declaration),
        ("b.v", member(member_span(30))),
    ]);

    let error = classify(&flat, "a.h_outflow[2]", "b.v")
        .expect_err("an element of a stream declaration is still a stream member");
    assert_eq!(
        format!("{error}"),
        "connect matches stream variable `a.h_outflow[2]` with non-stream variable `b.v`"
    );
}

// ---------------------------------------------------------------------------
// Rejected pairings
// ---------------------------------------------------------------------------

/// MLS §9.3: "stream variables only to other stream variables". The pair used
/// to be dropped, producing neither an equation nor an error.
#[test]
fn stream_paired_with_non_stream_is_rejected_with_both_member_spans() {
    let flat = model_with(vec![
        ("a.h_outflow", stream_member(member_span(10))),
        ("b.h_outflow", member(member_span(30))),
    ]);

    let error = classify(&flat, "a.h_outflow", "b.h_outflow")
        .expect_err("MLS §9.3 admits only stream-to-stream");
    let FlattenError::StreamMemberPairedWithNonStream {
        stream_member: stream_name,
        plain_member: plain_name,
        stream_span,
        plain_span,
    } = &error
    else {
        panic!("expected a stream pairing error, got {error:?}");
    };
    assert_eq!(stream_name, "a.h_outflow");
    assert_eq!(plain_name, "b.h_outflow");
    assert_eq!(*stream_span, member_span(10));
    assert_eq!(*plain_span, member_span(30));
}

/// The same rejection when the stream member is the second argument, with the
/// spans still naming the right side of the pair.
#[test]
fn non_stream_paired_with_stream_is_rejected_with_both_member_spans() {
    let flat = model_with(vec![
        ("a.h_outflow", member(member_span(10))),
        ("b.h_outflow", stream_member(member_span(30))),
    ]);

    let error = classify(&flat, "a.h_outflow", "b.h_outflow")
        .expect_err("MLS §9.3 admits only stream-to-stream");
    let FlattenError::StreamMemberPairedWithNonStream {
        stream_member: stream_name,
        plain_member: plain_name,
        stream_span,
        plain_span,
    } = &error
    else {
        panic!("expected a stream pairing error, got {error:?}");
    };
    assert_eq!(stream_name, "b.h_outflow");
    assert_eq!(plain_name, "a.h_outflow");
    assert_eq!(*stream_span, member_span(30));
    assert_eq!(*plain_span, member_span(10));
}

/// MLS §9.3: "the primitive components may only connect parameter variables to
/// parameter variables". Dropping the pair left `b.m` with no equation at all.
#[test]
fn parameter_paired_with_variable_is_rejected_with_both_member_spans() {
    let flat = model_with(vec![
        ("a.m", parameter_member(member_span(10))),
        ("b.m", member(member_span(30))),
    ]);

    let error =
        classify(&flat, "a.m", "b.m").expect_err("MLS §9.3 admits only parameter-to-parameter");
    let FlattenError::ConnectionVariabilityMismatch {
        a_member,
        a_variability,
        b_member,
        b_variability,
        a_span,
        b_span,
    } = &error
    else {
        panic!("expected a variability pairing error, got {error:?}");
    };
    assert_eq!(a_member.as_ref(), "a.m");
    assert_eq!(*a_variability, "parameter");
    assert_eq!(b_member.as_ref(), "b.m");
    assert_eq!(*b_variability, "non-structural");
    assert_eq!(*a_span, member_span(10));
    assert_eq!(*b_span, member_span(30));
}

#[test]
fn variable_paired_with_constant_is_rejected_with_both_member_spans() {
    let flat = model_with(vec![
        ("a.k", member(member_span(10))),
        ("b.k", constant_member(member_span(30))),
    ]);

    let error =
        classify(&flat, "a.k", "b.k").expect_err("MLS §9.3 admits only constant-to-constant");
    let FlattenError::ConnectionVariabilityMismatch {
        a_member,
        a_variability,
        b_member,
        b_variability,
        a_span,
        b_span,
    } = &error
    else {
        panic!("expected a variability pairing error, got {error:?}");
    };
    assert_eq!(a_member.as_ref(), "a.k");
    assert_eq!(*a_variability, "non-structural");
    assert_eq!(b_member.as_ref(), "b.k");
    assert_eq!(*b_variability, "constant");
    assert_eq!(*a_span, member_span(10));
    assert_eq!(*b_span, member_span(30));
}

// ---------------------------------------------------------------------------
// The pairing decision reaches both connection-set construction paths
// ---------------------------------------------------------------------------

/// `connect_primitive_vars` used to `return` on a one-sided stream, producing
/// no equation and no error.
#[test]
fn primitive_connect_rejects_a_one_sided_stream_instead_of_dropping_it() {
    let flat = model_with(vec![
        ("a.h_outflow", stream_member(member_span(10))),
        ("b.h_outflow", member(member_span(30))),
    ]);
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();

    let error = connect_primitive_vars(
        &rumoca_core::VarName::new("a.h_outflow"),
        &rumoca_core::VarName::new("b.h_outflow"),
        &flat,
        &mut flow_pairs,
        &mut potential_uf,
        &mut stream_uf,
        member_span(1),
    )
    .expect_err("a stream/non-stream pair has no equation to generate");

    assert!(matches!(
        error,
        FlattenError::StreamMemberPairedWithNonStream { .. }
    ));
    assert!(flow_pairs.is_empty());
    assert!(potential_uf.get_sets().is_empty());
    assert!(stream_uf.get_sets().is_empty());
}

/// `connect_primitive_vars` used to skip whenever *either* side was structural,
/// leaving the non-structural side with no equation.
#[test]
fn primitive_connect_rejects_a_one_sided_parameter_instead_of_dropping_it() {
    let flat = model_with(vec![
        ("a.m", parameter_member(member_span(10))),
        ("b.m", member(member_span(30))),
    ]);
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();

    let error = connect_primitive_vars(
        &rumoca_core::VarName::new("a.m"),
        &rumoca_core::VarName::new("b.m"),
        &flat,
        &mut flow_pairs,
        &mut potential_uf,
        &mut stream_uf,
        member_span(1),
    )
    .expect_err("MLS §9.3 admits only parameter-to-parameter");

    assert!(matches!(
        error,
        FlattenError::ConnectionVariabilityMismatch { .. }
    ));
    assert!(potential_uf.get_sets().is_empty());
}

/// A structural pair joins the assertion set instead of disappearing.
#[test]
fn primitive_connect_routes_a_structural_pair_to_assertion_ownership() {
    let flat = model_with(vec![
        ("a.m", parameter_member(member_span(10))),
        ("b.m", parameter_member(member_span(30))),
    ]);
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();

    connect_primitive_vars(
        &rumoca_core::VarName::new("a.m"),
        &rumoca_core::VarName::new("b.m"),
        &flat,
        &mut flow_pairs,
        &mut potential_uf,
        &mut stream_uf,
        member_span(1),
    )
    .expect("MLS §9.3 admits the structural pair through assertion ownership");

    assert!(flow_pairs.is_empty());
    assert_eq!(potential_uf.get_sets().len(), 1);
    assert!(stream_uf.get_sets().is_empty());
}

/// `connect_sub_variable` used to union a one-sided stream into the *potential*
/// set, generating an equality MLS §15.1 forbids.
#[test]
fn expanded_connect_rejects_a_one_sided_stream_instead_of_equating_it() {
    let flat = model_with(vec![
        ("port_a.h_outflow", stream_member(member_span(10))),
        ("port_b.h_outflow", member(member_span(30))),
    ]);
    let subs_b = vec![rumoca_core::VarName::new("port_b.h_outflow")];
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let mut ctx = ConnectionBuildCtx {
        flat: &flat,
        var_index: &var_index,
        span: member_span(1),
        flow_pairs: &mut flow_pairs,
        potential_uf: &mut potential_uf,
        stream_uf: &mut stream_uf,
    };
    let sub_match_index = ConnectionSubMatchIndex::new("port_b", &subs_b, &var_index);

    let error = connect_sub_variable(
        &rumoca_core::VarName::new("port_a.h_outflow"),
        "port_a",
        "port_b",
        &sub_match_index,
        &mut ctx,
    )
    .expect_err("a stream member must not be equated to a non-stream member");

    assert!(matches!(
        error,
        FlattenError::StreamMemberPairedWithNonStream { .. }
    ));
    assert!(potential_uf.get_sets().is_empty());
}

/// The matching stream-to-stream expansion still reaches the §15.2 stream set.
#[test]
fn expanded_connect_still_routes_stream_to_stream_into_the_stream_set() {
    let flat = model_with(vec![
        ("port_a.h_outflow", stream_member(member_span(10))),
        ("port_b.h_outflow", stream_member(member_span(30))),
    ]);
    let subs_b = vec![rumoca_core::VarName::new("port_b.h_outflow")];
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let mut ctx = ConnectionBuildCtx {
        flat: &flat,
        var_index: &var_index,
        span: member_span(1),
        flow_pairs: &mut flow_pairs,
        potential_uf: &mut potential_uf,
        stream_uf: &mut stream_uf,
    };
    let sub_match_index = ConnectionSubMatchIndex::new("port_b", &subs_b, &var_index);

    let matched = connect_sub_variable(
        &rumoca_core::VarName::new("port_a.h_outflow"),
        "port_a",
        "port_b",
        &sub_match_index,
        &mut ctx,
    )
    .expect("stream-to-stream is the pairing MLS §9.3 admits");

    assert!(matched);
    assert!(potential_uf.get_sets().is_empty());
    assert_eq!(stream_uf.get_sets().len(), 1);
}

/// `connect_sub_variable` only ever inspected side A's variability, so a
/// parameter on side B was equated to a variable on side A.
#[test]
fn expanded_connect_rejects_a_parameter_on_the_matched_side() {
    let flat = model_with(vec![
        ("plug_a.m", member(member_span(10))),
        ("plug_b.m", parameter_member(member_span(30))),
    ]);
    let subs_b = vec![rumoca_core::VarName::new("plug_b.m")];
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let mut ctx = ConnectionBuildCtx {
        flat: &flat,
        var_index: &var_index,
        span: member_span(1),
        flow_pairs: &mut flow_pairs,
        potential_uf: &mut potential_uf,
        stream_uf: &mut stream_uf,
    };
    let sub_match_index = ConnectionSubMatchIndex::new("plug_b", &subs_b, &var_index);

    let error = connect_sub_variable(
        &rumoca_core::VarName::new("plug_a.m"),
        "plug_a",
        "plug_b",
        &sub_match_index,
        &mut ctx,
    )
    .expect_err("MLS §9.3 admits only parameter-to-parameter");

    assert!(matches!(
        error,
        FlattenError::ConnectionVariabilityMismatch { .. }
    ));
    assert!(potential_uf.get_sets().is_empty());
}

/// A structural pair is a semantic name match and joins assertion ownership.
#[test]
fn expanded_connect_routes_a_structural_pair_to_assertion_ownership() {
    let flat = model_with(vec![
        ("plug_a.m", parameter_member(member_span(10))),
        ("plug_b.m", parameter_member(member_span(30))),
    ]);
    let subs_b = vec![rumoca_core::VarName::new("plug_b.m")];
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let mut ctx = ConnectionBuildCtx {
        flat: &flat,
        var_index: &var_index,
        span: member_span(1),
        flow_pairs: &mut flow_pairs,
        potential_uf: &mut potential_uf,
        stream_uf: &mut stream_uf,
    };
    let sub_match_index = ConnectionSubMatchIndex::new("plug_b", &subs_b, &var_index);

    let matched = connect_sub_variable(
        &rumoca_core::VarName::new("plug_a.m"),
        "plug_a",
        "plug_b",
        &sub_match_index,
        &mut ctx,
    )
    .expect("MLS §9.3 admits the structural pair through assertion ownership");

    assert!(matched);
    assert_eq!(potential_uf.get_sets().len(), 1);
}

fn structural_primitive(
    name: &str,
    variability: rumoca_core::Variability,
    dims: Vec<i64>,
) -> (rumoca_core::VarName, flat::Variable) {
    let name = rumoca_core::VarName::new(name);
    let variable = flat::Variable {
        name: name.clone(),
        type_id: CONNECTION_TEST_SCALAR_TYPE,
        variability,
        dims,
        is_primitive: true,
        ..connection_test_variable(member_span(10))
    };
    (name, variable)
}

fn scalar_connection(a: &str, b: &str, offset: usize) -> ast::InstanceConnection {
    ast::InstanceConnection::scalar(
        ast::QualifiedName::from_dotted(a),
        ast::QualifiedName::from_dotted(b),
        None,
        member_span(offset),
        String::new(),
    )
    .expect("test connection has valid endpoints and provenance")
}

fn connection_overlay(connections: Vec<ast::InstanceConnection>) -> ast::InstanceOverlay {
    let mut overlay = ast::InstanceOverlay::new();
    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: class_instance_id,
            connections,
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    overlay
}

fn process_checked_connections(
    flat: &mut flat::Model,
    overlay: &ast::InstanceOverlay,
    forest: &mut crate::vcg::OverconstrainedEquationForest,
) -> Result<(), FlattenError> {
    let overconstrained = overlay
        .finalized_overconstrained()
        .expect("member-pairing fixture must construct finalized occurrence proofs");
    process_connections_for_test(flat, &overconstrained, forest)
}

#[test]
fn scalar_structural_connection_constructs_the_required_assertion() {
    let mut flat = connection_test_model();
    for (name, variable) in [
        structural_primitive(
            "a.m",
            rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            Vec::new(),
        ),
        structural_primitive(
            "b.m",
            rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            Vec::new(),
        ),
    ] {
        flat.add_test_variable(name, variable);
    }
    let overlay = connection_overlay(vec![scalar_connection("a.m", "b.m", 1)]);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    finalize_connection_test_flat(&mut flat);
    process_checked_connections(&mut flat, &overlay, &mut forest)
        .expect("a scalar parameter pair has an exact Flat assertion owner");

    assert!(flat.equations.is_empty());
    assert_eq!(flat.assert_equations.len(), 1);
    let assertion = &flat.assert_equations[0];
    assert_eq!(assertion.span, member_span(1));
    assert!(matches!(
        &assertion.condition,
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Eq,
            lhs,
            rhs,
            ..
        } if matches!(lhs.as_ref(), rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "a.m")
            && matches!(rhs.as_ref(), rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "b.m")
    ));
    assert!(matches!(
        &assertion.message,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String(message),
            ..
        } if message == "Connected constants/parameters must be equal"
    ));
    assert!(
        flat.variables
            .values()
            .all(|variable| variable.connected.is_unconnected())
    );
}

#[test]
fn nonempty_structural_array_refusal_rolls_back_an_earlier_assertion() {
    let mut flat = connection_test_model();
    for (name, variable) in [
        structural_primitive(
            "ok_a",
            rumoca_core::Variability::Constant(rumoca_core::Token::default()),
            Vec::new(),
        ),
        structural_primitive(
            "ok_b",
            rumoca_core::Variability::Constant(rumoca_core::Token::default()),
            Vec::new(),
        ),
        structural_primitive(
            "array_a",
            rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            vec![2],
        ),
        structural_primitive(
            "array_b",
            rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            vec![2],
        ),
    ] {
        flat.add_test_variable(name, variable);
    }
    let overlay = connection_overlay(vec![
        scalar_connection("ok_a", "ok_b", 1),
        scalar_connection("array_a", "array_b", 2),
    ]);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();
    let forest_before = forest.state_snapshot();

    finalize_connection_test_flat(&mut flat);
    let error = process_checked_connections(&mut flat, &overlay, &mut forest)
        .expect_err("a nonempty structural array needs a compact assertion-family owner");

    assert!(matches!(
        error,
        FlattenError::InvalidConnectionEvidence { .. }
    ));
    assert!(error.to_string().contains("compact assertion-family owner"));
    assert!(flat.equations.is_empty());
    assert!(flat.assert_equations.is_empty());
    assert!(
        flat.variables
            .values()
            .all(|variable| variable.connected.is_unconnected())
    );
    assert_eq!(forest.state_snapshot(), forest_before);
}

#[test]
fn empty_structural_arrays_are_vacuously_equal_without_connected_state() {
    let mut flat = connection_test_model();
    for (name, variable) in [
        structural_primitive(
            "empty_a",
            rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            vec![0],
        ),
        structural_primitive(
            "empty_b",
            rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            vec![0],
        ),
    ] {
        flat.add_test_variable(name, variable);
    }
    let overlay = connection_overlay(vec![scalar_connection("empty_a", "empty_b", 1)]);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    finalize_connection_test_flat(&mut flat);
    process_checked_connections(&mut flat, &overlay, &mut forest)
        .expect("an empty structural value needs no scalar assertions");

    assert!(flat.assert_equations.is_empty());
    assert!(flat.equations.is_empty());
    assert!(
        flat.variables
            .values()
            .all(|variable| variable.connected.is_unconnected())
    );
}
