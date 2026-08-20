//! MLS §9.3 pairing rules for matched primitive connection members.
//!
//! The acceptance contract these pin is stated in the
//! [`member_pairing`](super::super::member_pairing) module docs: agreeing
//! prefixes connect, structural pairs generate nothing, absent evidence is
//! never a rejection, and the two provably impossible pairings are reported
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
    flat::Variable::empty_with_span(span)
}

fn stream_member(span: Span) -> flat::Variable {
    flat::Variable {
        stream: true,
        ..flat::Variable::empty_with_span(span)
    }
}

fn parameter_member(span: Span) -> flat::Variable {
    flat::Variable {
        variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
        ..flat::Variable::empty_with_span(span)
    }
}

fn constant_member(span: Span) -> flat::Variable {
    flat::Variable {
        variability: rumoca_core::Variability::Constant(rumoca_core::Token::default()),
        ..flat::Variable::empty_with_span(span)
    }
}

fn model_with(members: Vec<(&str, flat::Variable)>) -> flat::Model {
    let mut flat = flat::Model::new();
    for (name, variable) in members {
        flat.add_variable(rumoca_core::VarName::new(name), variable);
    }
    flat
}

fn classify(flat: &flat::Model, a: &str, b: &str) -> Result<MemberPairing, FlattenError> {
    classify_connection_member_pair(
        flat,
        &rumoca_core::VarName::new(a),
        &rumoca_core::VarName::new(b),
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
fn parameter_paired_with_parameter_generates_no_equation() {
    let flat = model_with(vec![
        ("a.m", parameter_member(member_span(10))),
        ("b.m", parameter_member(member_span(30))),
    ]);

    assert_eq!(
        classify(&flat, "a.m", "b.m").expect("parameter-to-parameter is admitted"),
        MemberPairing::NoEquation
    );
}

#[test]
fn constant_paired_with_constant_generates_no_equation() {
    let flat = model_with(vec![
        ("a.k", constant_member(member_span(10))),
        ("b.k", constant_member(member_span(30))),
    ]);

    assert_eq!(
        classify(&flat, "a.k", "b.k").expect("constant-to-constant is admitted"),
        MemberPairing::NoEquation
    );
}

/// Both sides are translation-time values, so MLS §9.3's assert is well formed
/// and no unknown is left behind. The contract accepts this rather than reading
/// MLS §9.3 as forbidding the mixed pair.
#[test]
fn parameter_paired_with_constant_generates_no_equation() {
    let flat = model_with(vec![
        ("a.m", parameter_member(member_span(10))),
        ("b.m", constant_member(member_span(30))),
    ]);

    assert_eq!(
        classify(&flat, "a.m", "b.m").expect("both sides are translation-time values"),
        MemberPairing::NoEquation
    );
}

/// Absent evidence is never a rejection: a member with no declaration in view
/// keeps the pair connected.
#[test]
fn member_without_a_declaration_is_not_judged() {
    let flat = model_with(vec![("a.h_outflow", stream_member(member_span(10)))]);

    assert_eq!(
        classify(&flat, "a.h_outflow", "b.h_outflow")
            .expect("a member this phase cannot see is not evidence of a violation"),
        MemberPairing::Connect
    );
    assert_eq!(
        classify(&flat, "b.h_outflow", "a.h_outflow")
            .expect("the same holds in the opposite order"),
        MemberPairing::Connect
    );
}

/// The deliberate asymmetric case in the acceptance contract: a `parameter` or
/// `constant` whose counterpart resolves to no declaration is **connected**,
/// not skipped and not rejected.
///
/// For the MLS §9.3 variability rule alone this is less conservative than the
/// skip it replaced, which needed only one side's declaration to decide: the
/// structural member joins the potential set here. That is the stated choice —
/// absent counterpart evidence means connect, because a one-sided rejection
/// would be a rejection on absent evidence, which the contract forbids, and the
/// collapsed-array representations that make a counterpart unresolvable are
/// legal models.
///
/// No reachable input reaches this branch today (an element path either
/// resolves through its declared base or is rejected in typecheck), so this
/// test pins the decision rather than a corpus behaviour. If a reachable path
/// is ever found, the fix is to make the structural side's evidence alone
/// sufficient to abstain — this test then flips to `NoEquation` — not to widen
/// `EF028` into a one-sided rejection.
#[test]
fn structural_member_with_an_unresolvable_counterpart_is_connected() {
    let flat = model_with(vec![("a.m", parameter_member(member_span(10)))]);

    assert_eq!(
        classify(&flat, "a.m", "b.m")
            .expect("an unresolvable counterpart is not evidence of a violation"),
        MemberPairing::Connect
    );
    assert_eq!(
        classify(&flat, "b.m", "a.m").expect("the same holds in the opposite order"),
        MemberPairing::Connect
    );

    let flat = model_with(vec![("a.k", constant_member(member_span(10)))]);
    assert_eq!(
        classify(&flat, "a.k", "b.k").expect("constants take the same branch"),
        MemberPairing::Connect
    );
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
    let FlattenError::StructuralMemberPairedWithVariable {
        structural_member,
        structural_variability,
        variable_member,
        structural_span,
        variable_span,
    } = &error
    else {
        panic!("expected a variability pairing error, got {error:?}");
    };
    assert_eq!(structural_member, "a.m");
    assert_eq!(*structural_variability, "parameter");
    assert_eq!(variable_member, "b.m");
    assert_eq!(*structural_span, member_span(10));
    assert_eq!(*variable_span, member_span(30));
}

#[test]
fn variable_paired_with_constant_is_rejected_with_both_member_spans() {
    let flat = model_with(vec![
        ("a.k", member(member_span(10))),
        ("b.k", constant_member(member_span(30))),
    ]);

    let error =
        classify(&flat, "a.k", "b.k").expect_err("MLS §9.3 admits only constant-to-constant");
    let FlattenError::StructuralMemberPairedWithVariable {
        structural_member,
        structural_variability,
        variable_member,
        structural_span,
        variable_span,
    } = &error
    else {
        panic!("expected a variability pairing error, got {error:?}");
    };
    assert_eq!(structural_member, "b.k");
    assert_eq!(*structural_variability, "constant");
    assert_eq!(variable_member, "a.k");
    assert_eq!(*structural_span, member_span(30));
    assert_eq!(*variable_span, member_span(10));
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
    )
    .expect_err("MLS §9.3 admits only parameter-to-parameter");

    assert!(matches!(
        error,
        FlattenError::StructuralMemberPairedWithVariable { .. }
    ));
    assert!(potential_uf.get_sets().is_empty());
}

/// A structural pair still generates nothing, and still generates it silently.
#[test]
fn primitive_connect_generates_nothing_for_a_structural_pair() {
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
    )
    .expect("MLS §9.3 generates no connection for a structural pair");

    assert!(flow_pairs.is_empty());
    assert!(potential_uf.get_sets().is_empty());
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
        FlattenError::StructuralMemberPairedWithVariable { .. }
    ));
    assert!(potential_uf.get_sets().is_empty());
}

/// A structural pair reports "unmatched" so the reverse expansion direction is
/// still attempted, exactly as the pre-match skip it replaced did.
#[test]
fn expanded_connect_reports_a_structural_pair_as_unmatched() {
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
    .expect("MLS §9.3 generates no connection for a structural pair");

    assert!(!matched);
    assert!(potential_uf.get_sets().is_empty());
}
