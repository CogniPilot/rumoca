//! What one matched pair of primitive connection members generates (MLS §9.3,
//! §15.1).
//!
//! ## Acceptance contract (SPEC_0008)
//!
//! Expanding a `connect` pairs the primitive members of the two connectors by
//! name (MLS §9.3: "In a connect-equation the two connectors must have the same
//! named component elements with the same dimensions; recursively down to the
//! primitive components"). MLS §9.3 then constrains what such a matched pair
//! may be:
//!
//! > The matched primitive components of the two connectors must have the same
//! > primitive types, and flow variables may only connect to other flow
//! > variables, stream variables only to other stream variables, and causal
//! > variables (input/output) only to causal variables (input/output).
//!
//! and constrains its variability in the same section:
//!
//! > In the connect-equation the primitive components may only connect
//! > parameter variables to parameter variables and constant variables to
//! > constant variables.
//!
//! Both clauses are §9.3, which is also where SPEC_0022 files the CONN-028
//! contract. MLS §9.1 governs a different rule that is easy to confuse with
//! this one — a *connector component* may not itself be declared `parameter` or
//! `constant` — and that one is enforced in the resolve phase as `ER027`.
//!
//! [`classify_connection_member_pair`] is the single decision point for both,
//! and it is stated as acceptance before rejection:
//!
//! 1. **Accepted — pairs whose prefixes agree.** `flow`/`flow` becomes a §9.2
//!    sum term, `stream`/`stream` joins a §15.2 stream set, and a pair with
//!    neither prefix becomes a §9.2 equality. The `stream` and variability
//!    questions are never answered from one side alone. The `flow` question
//!    still is: once this classifier admits a pair, the routing in
//!    `connect_sub_variable` reads `flow` off side A only and lets the §9.2
//!    flow/non-flow validator (below) own the mismatch.
//!
//! 2. **Accepted, generating no equation — structural pairs.** When *both*
//!    members are `parameter` or `constant`, MLS §9.3 says "Constants or
//!    parameters in connected components yield the appropriate assert-statements
//!    to check that they have the same value; connections are not generated".
//!    Both sides are translation-time values, so generating nothing leaves no
//!    unknown behind. Accepted for the same reason: `parameter` paired with
//!    `constant`, which no comparator implementation rejects and which §9.3's
//!    assert is well-formed over.
//!
//! 3. **Accepted — pairs this phase has no evidence about.** A member whose
//!    path resolves to no declaration in the flat model carries no prefix
//!    information here, so the pair is connected rather than judged. Rejecting
//!    on absent evidence would reject legal models whose connector members
//!    reach this phase in a collapsed array representation.
//!
//!    This is decided on the pair, not per side, and that choice is deliberate
//!    in one asymmetric case: **when one side resolves to a `parameter` or
//!    `constant` and its counterpart resolves to nothing, the pair is
//!    connected** — the structural member joins the potential set instead of
//!    being dropped. For the variability rule alone that is *less* conservative
//!    than the skip this classifier replaced, which needed only side A's
//!    declaration to decide. The choice is: absent counterpart evidence means
//!    connect, accepting the §9.3 variability risk on such a path, because a
//!    one-sided rejection would be a rejection on absent evidence — exactly
//!    what rule 3 exists to forbid — and the collapsed-array representations
//!    that make a counterpart unresolvable are legal models. No reachable input
//!    reaches it today (an element path either resolves through its declared
//!    base here or is rejected in typecheck), so it is pinned by
//!    `structural_member_with_an_unresolvable_counterpart_is_connected` rather
//!    than by a corpus model. If a reachable path is ever found, the fix is to
//!    make the structural side's evidence alone sufficient to abstain, not to
//!    widen `EF028` to a one-sided rejection.
//!
//! Only the provably impossible remainder is rejected, and never by dropping
//! the pair:
//!
//! - **One side `stream`, the other not** (`EF027`, the CONN-030 contract).
//!   MLS §9.3 admits only stream-to-stream. Such a pair selects no equation on
//!   either reading — MLS
//!   §15.1 gives the stream side mixing semantics ("For inside connectors,
//!   variables with the stream prefix do not lead to connection equations")
//!   while the non-stream side expects a §9.2 equality — so accepting it
//!   silently under-constrains the model. Reported against both member
//!   declarations.
//! - **One side `parameter`/`constant`, the other neither** (`EF028`, the
//!   CONN-028 contract). MLS §9.3 admits only parameter-to-parameter and
//!   constant-to-constant. Dropping the pair, which is what this phase did
//!   before, leaves the non-structural side with no equation at all. Reported
//!   against both member declarations.
//!
//! ## Scope of `EF027`/`EF028`
//!
//! Shapes MLS §9.3 also governs that this decision point structurally does not
//! see or deliberately does not judge, each with its current behaviour and its
//! owner:
//!
//! - **The `flow` half of the same §9.3 sentence** ("flow variables may only
//!   connect to other flow variables", CONN-003). Not raised here: a flow/
//!   non-flow mismatch is rejected before connection-set construction by
//!   `validate_flow_consistency`, reached from `validate_connections` on both
//!   the primitive-endpoint and expanded-member paths, and gated on
//!   `FlattenOptions::strict_connection_validation` (true in every production
//!   session). That validator owns the clause and reports it as `EF002`; this
//!   classifier deliberately does not duplicate it.
//! - **Members present on one connector only.** MLS §9.3 requires "the same
//!   named component elements"; an unmatched member is silently dropped by
//!   `connect_sub_variable`, because member matching returns no counterpart to
//!   judge. Owned by connector type-compatibility checking
//!   (`validate_expanded_connector_connection`), not by this classifier.
//! - **Causal (`input`/`output`) prefix pairing.** MLS §9.3's third clause is
//!   not checked here; causality is judged by the §9.2 single-source contract
//!   (CONN-004) in the resolve phase.
//! - **The §9.3 assert-statement for a structural pair.** MLS §9.3 pairs
//!   "connections are not generated" with an assertion that the two values
//!   agree, and OMC emits exactly that — `assert(a.m == b.m, "Connected
//!   constants/parameters must be equal")` for a parameter pair, and the
//!   constant-folded `assert(a.m == 3)` when one side is a `constant`. This
//!   phase generates neither, so two structurally conflicting values (`a.m = 3`
//!   connected to `b.m = 4`) compile silently instead of failing the assertion.
//!   That is a missing check, not a missing equation — parameters and constants
//!   are not unknowns, so it cannot under-constrain the model — and it is owned
//!   by the connection-equation generator (`equation_generation`).
//! - **Array-output expansions.** `connect_array_to_expanded` and
//!   `connect_output_to_array_element` pair a whole array endpoint against
//!   expanded elements and read the prefixes off one side only. They keep their
//!   existing behaviour and are owned by `connect_array_output_variables`; the
//!   `connect`-expansion paths (`connect_primitive_vars`,
//!   `connect_sub_variable`) are the ones routed through this classifier.

use super::*;

/// What MLS §9.3/§15.1 say to do with one matched pair of primitive
/// connection members.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum MemberPairing {
    /// The pair joins a connection set and generates the equation its prefixes
    /// select (MLS §9.2 equality or flow sum, MLS §15.2 stream mixing).
    Connect,
    /// MLS §9.3: "Constants or parameters in connected components yield the
    /// appropriate assert-statements to check that they have the same value;
    /// connections are not generated." Both sides are translation-time values,
    /// so no unknown is left behind by generating nothing.
    NoEquation,
}

/// The declaration a connection-set member resolves to, or `None` when this
/// phase holds no declaration for it.
///
/// One shared resolution for every prefix question asked about a member —
/// `flow` (MLS §9.2), `stream` (MLS §9.3/§15.1) and variability (MLS §9.3) —
/// so a member can never be judged flow by one rule and unknown by another.
/// An element path resolves through its declared base exactly as
/// `connection_endpoint_dims` does.
pub(super) fn connection_member_declaration<'flat>(
    flat: &'flat flat::Model,
    var_name: &rumoca_core::VarName,
) -> Option<&'flat flat::Variable> {
    if let Some(v) = flat.variables.get(var_name) {
        return Some(v);
    }
    let base = subscripted_base_var(var_name, flat)?;
    flat.variables.get(&base)
}

/// Name of the variability prefix that makes a member structural, or `None`
/// when the member is an equation unknown.
fn structural_variability_label(variable: &flat::Variable) -> Option<&'static str> {
    match variable.variability {
        rumoca_core::Variability::Parameter(_) => Some("parameter"),
        rumoca_core::Variability::Constant(_) => Some("constant"),
        _ => None,
    }
}

/// Decide what one matched pair of primitive connection members generates.
///
/// See this module's acceptance contract: the pair is accepted and connected
/// unless MLS proves it cannot be, and "no declaration in view" is never such a
/// proof.
pub(super) fn classify_connection_member_pair(
    flat: &flat::Model,
    var_a: &rumoca_core::VarName,
    var_b: &rumoca_core::VarName,
) -> Result<MemberPairing, FlattenError> {
    let (Some(decl_a), Some(decl_b)) = (
        connection_member_declaration(flat, var_a),
        connection_member_declaration(flat, var_b),
    ) else {
        // A member with no declaration in view carries no evidence about its
        // prefixes, so this phase does not judge the pair.
        return Ok(MemberPairing::Connect);
    };

    // MLS §9.3: "the primitive components may only connect parameter variables
    // to parameter variables and constant variables to constant variables".
    match (
        structural_variability_label(decl_a),
        structural_variability_label(decl_b),
    ) {
        (Some(_), Some(_)) => return Ok(MemberPairing::NoEquation),
        (Some(label), None) => {
            return Err(FlattenError::structural_member_paired_with_variable(
                var_a.as_str(),
                label,
                decl_a.source_span,
                var_b.as_str(),
                decl_b.source_span,
            ));
        }
        (None, Some(label)) => {
            return Err(FlattenError::structural_member_paired_with_variable(
                var_b.as_str(),
                label,
                decl_b.source_span,
                var_a.as_str(),
                decl_a.source_span,
            ));
        }
        (None, None) => {}
    }

    // MLS §9.3: "stream variables only to other stream variables". MLS §15.1
    // gives a stream variable mixing semantics rather than a connection
    // equation, so a stream/non-stream pair selects no equation on either
    // reading and would silently under-constrain the model.
    match (decl_a.stream, decl_b.stream) {
        (true, false) => Err(FlattenError::stream_member_paired_with_non_stream(
            var_a.as_str(),
            decl_a.source_span,
            var_b.as_str(),
            decl_b.source_span,
        )),
        (false, true) => Err(FlattenError::stream_member_paired_with_non_stream(
            var_b.as_str(),
            decl_b.source_span,
            var_a.as_str(),
            decl_a.source_span,
        )),
        _ => Ok(MemberPairing::Connect),
    }
}
