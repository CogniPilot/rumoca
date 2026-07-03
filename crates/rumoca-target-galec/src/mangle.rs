//! Modelica name → GALEC [`Name`] mapping (GAL-015, trap T13).
//!
//! # Scheme
//!
//! For a scalarized Modelica variable name `n`:
//!
//! 1. **Pre-slot** (generated `__pre__.x` slot, detected structurally by the
//!    classifier): maps to the quoted identifier `'previous(x)'` — the T2
//!    convention for materialized previous-value state.
//! 2. **Plain identifier**: when `n` is lexically a legal GALEC identifier
//!    (ASCII letter first, then ASCII alphanumerics/underscores) *and* not
//!    reserved ([`rumoca_galec::builtins::is_reserved_name`]: keywords,
//!    future-reserved words, the `__` prefix space, builtins including
//!    lifted variants, Appendix C names, predefined signal names), `n` maps
//!    to itself as a plain identifier.
//! 3. **Quoted identifier**: everything else (hierarchical names `a.b[2]`,
//!    reserved collisions like `absolute` or `if`, `__`-prefixed names) maps
//!    to the quoted identifier `'n'`, carrying the original scalarized name
//!    verbatim for traceability.
//!
//! Names that cannot be a single quoted lexeme (containing `'`, whitespace,
//! control characters — e.g. flattened Modelica quoted identifiers such as
//! `Logic.'1'`) are rejected with `ET012` rather than rewritten: rewriting
//! would lose the original name and risk collisions.
//!
//! # Injectivity and reserved-disjointness (by construction)
//!
//! - Rule 2 outputs equal only when inputs are equal (identity), and are
//!   never reserved (checked).
//! - Rule 3 outputs equal only when inputs are equal (identity on quoted
//!   content). Quoted identifiers are a separate lexeme class, disjoint
//!   from keywords/builtins by the grammar.
//! - Rules 2 and 3 cannot collide with each other: a given string is routed
//!   deterministically to exactly one rule.
//! - Rule 1 outputs `previous(x)` contain parentheses; scalarized Modelica
//!   names never contain parentheses outside Modelica quoted segments (which
//!   carry `'` and are rejected), so the pre-space is disjoint from rules
//!   2/3. Defensively, unquoted source names containing `(`/`)` are rejected
//!   so this argument holds even for corrupt input.

use rumoca_galec::ast::Name;
use rumoca_galec::builtins::is_reserved_name;

use crate::diagnostic::GalecTargetError;

/// True when `name` is lexically a legal *plain* GALEC identifier (ASCII
/// letter first, ASCII alphanumeric/underscore continuation). Reservedness
/// is a separate question — see [`galec_variable_name`].
#[must_use]
pub fn is_legal_plain_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    first.is_ascii_alphabetic() && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// Reject content that cannot form a single quoted-identifier lexeme.
fn check_quotable(name: &str) -> Result<(), &'static str> {
    if name.is_empty() {
        return Err("empty name");
    }
    if name.contains('\'') {
        return Err("contains a quote character (flattened Modelica quoted identifier)");
    }
    if name.chars().any(|c| c.is_whitespace() || c.is_control()) {
        return Err("contains whitespace or control characters");
    }
    Ok(())
}

/// Map a scalarized Modelica variable name to its GALEC name (rules 2/3 of
/// the module scheme). Pre-slots must go through [`pre_state_name`] instead;
/// the classifier owns that routing.
pub fn galec_variable_name(source_name: &str) -> Result<Name, GalecTargetError> {
    let unrepresentable = |reason| GalecTargetError::UnrepresentableName {
        variable: source_name.to_owned(),
        reason,
    };
    check_quotable(source_name).map_err(unrepresentable)?;
    if source_name.contains('(') || source_name.contains(')') {
        return Err(unrepresentable(
            "contains parentheses, which are reserved for the 'previous(x)' state convention",
        ));
    }
    if is_legal_plain_identifier(source_name) && !is_reserved_name(source_name) {
        Ok(Name::ident(source_name))
    } else {
        Ok(Name::quoted(source_name))
    }
}

/// Map the base name of a generated `__pre__.` slot to its GALEC state name
/// `'previous(<base>)'` (rule 1 of the module scheme, trap T2).
pub fn pre_state_name(base_source_name: &str) -> Result<Name, GalecTargetError> {
    let unrepresentable = |reason| GalecTargetError::UnrepresentableName {
        variable: format!("__pre__.{base_source_name}"),
        reason,
    };
    check_quotable(base_source_name).map_err(unrepresentable)?;
    if base_source_name.contains('(') || base_source_name.contains(')') {
        return Err(unrepresentable(
            "pre() base name contains parentheses and cannot be nested in 'previous(...)'",
        ));
    }
    Ok(Name::quoted(format!("previous({base_source_name})")))
}

/// The manifest spelling of a GALEC name: the plain identifier text or the
/// quoted-identifier *content* (manifests carry `previous(I.x)` without the
/// surrounding quotes). Unique whenever the mangling inputs were unique,
/// because both rules are content-preserving and route disjoint inputs.
#[must_use]
pub fn manifest_name(name: &Name) -> &str {
    match name {
        Name::Ident(ident) => ident.as_str(),
        Name::Quoted(content) => content.as_str(),
    }
}
