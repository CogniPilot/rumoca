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
//! 2. **Accepted, generating an equality assertion — like structural pairs.**
//!    `parameter`/`parameter` and `constant`/`constant` are admitted and routed
//!    to the connection assertion owner. They never enter a potential or flow
//!    residual set. A mixed `parameter`/`constant` pair is rejected by the same
//!    explicit "only ... to ..." rule.
//!
//! 3. **Rejected — absent declaration evidence.** By this flatten boundary,
//!    legal direct members and scalar array selections resolve either exactly or
//!    through their checked declared base. Expandable-member union is handled
//!    (or explicitly refused as unsupported) before this classifier, and false
//!    conditional connections have already been pruned. Any remaining missing
//!    declaration is invalid phase evidence, so `EF038` fires before connection
//!    sets or equations can be constructed.
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
//! - **Incompatible variability** (`EF028`, the CONN-028 contract). MLS §9.3
//!   admits only parameter-to-parameter and constant-to-constant structural
//!   pairs. This rejects structural/non-structural and parameter/constant
//!   pairings against both member declarations.
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
//!   the unconditional pre-construction validation pass. That validator owns
//!   the clause and reports it as `EF002`; this
//!   classifier deliberately does not duplicate it.
//! - **Members present on one connector only.** MLS §9.3 requires "the same
//!   named component elements". Complete bidirectional coverage is proven
//!   before connection-set mutation; a partial or empty intersection returns
//!   `EF002`. The asymmetric compact-array form is accepted only when every
//!   member is covered in one complete direction. This remains owned by
//!   connector expansion, not by this per-pair classifier.
//! - **Causal (`input`/`output`) prefix pairing.** MLS §9.3's third clause is
//!   not checked here; causality is judged by the §9.2 single-source contract
//!   (CONN-004) in the resolve phase.
//! - **The §9.3 assert-statement for a structural pair.** MLS §9.3 pairs
//!   "connections are not generated" with an assertion that the two values
//!   agree. Scalar pairs are preserved as Flat `AssertEquation` owners by
//!   `equation_generation`; an empty value is vacuously equal. A nonempty
//!   array-valued structural pair is rejected with `EF038` until Flat owns a
//!   compact assertion family, because treating array `==` as the scalar
//!   Boolean condition of one assertion would be invalid IR.
//! - **Array-output expansions.** `connect_array_to_expanded` pairs a whole
//!   array endpoint against checked expanded members. The `connect`-expansion
//!   paths (`connect_primitive_vars`, `connect_sub_variable`) are the ones
//!   routed through this classifier.

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
    /// connections are not generated."
    StructuralAssertion,
}

/// Read the `flow` role only from a required exact declaration token.
pub(crate) fn is_flow_variable(
    flat: &flat::Model,
    var_name: &rumoca_core::VarName,
    span: rumoca_core::Span,
) -> Result<bool, FlattenError> {
    Ok(require_connection_declaration(flat, var_name, span)?
        .declaration()
        .flow)
}

/// Read the `stream` role only from a required exact declaration token.
pub(super) fn is_stream_variable(
    flat: &flat::Model,
    var_name: &rumoca_core::VarName,
    span: rumoca_core::Span,
) -> Result<bool, FlattenError> {
    Ok(require_connection_declaration(flat, var_name, span)?
        .declaration()
        .stream)
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

fn connection_variability_label(variable: &flat::Variable) -> &'static str {
    structural_variability_label(variable).unwrap_or("non-structural")
}

/// Decide what one matched pair of primitive connection members generates.
///
/// See this module's acceptance contract: a pair is classified only after both
/// declarations resolve exactly or through checked array-selection evidence.
pub(super) fn classify_connection_member_pair(
    flat: &flat::Model,
    var_a: &rumoca_core::VarName,
    var_b: &rumoca_core::VarName,
    span: rumoca_core::Span,
) -> Result<MemberPairing, FlattenError> {
    let evidence_a = require_connection_declaration(flat, var_a, span)?;
    let evidence_b = require_connection_declaration(flat, var_b, span)?;
    let decl_a = evidence_a.declaration();
    let decl_b = evidence_b.declaration();

    // MLS §9.3: "the primitive components may only connect parameter variables
    // to parameter variables and constant variables to constant variables".
    let variability_a = connection_variability_label(decl_a);
    let variability_b = connection_variability_label(decl_b);
    match (variability_a, variability_b) {
        ("parameter", "parameter") | ("constant", "constant") => {
            return Ok(MemberPairing::StructuralAssertion);
        }
        ("non-structural", "non-structural") => {}
        _ => {
            return Err(FlattenError::connection_variability_mismatch(
                var_a.as_str(),
                variability_a,
                decl_a.source_span,
                var_b.as_str(),
                variability_b,
                decl_b.source_span,
            ));
        }
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
