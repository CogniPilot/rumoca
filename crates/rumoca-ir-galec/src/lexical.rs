//! The GALEC plain-identifier lexical rule (§3.2.4 grammar G-1.x): the single
//! owner of "what is a legal unquoted identifier". The validator (name
//! analysis) and the codegen mangler both consult it, so widening the grammar
//! (e.g. a new continuation character) is a one-line change here rather than
//! three copies drifting apart across two crates. Reservedness is a separate
//! question — see [`crate::is_reserved_name`].

/// The shape error of a plain GALEC identifier, or `None` when `name` is a
/// legal plain identifier: a non-empty string whose first character is an ASCII
/// letter and whose remaining characters are ASCII letters, digits, or `_`.
#[must_use]
pub fn plain_identifier_shape_error(name: &str) -> Option<&'static str> {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return Some("must start with an ASCII letter");
    };
    if !first.is_ascii_alphabetic() {
        return Some("must start with an ASCII letter");
    }
    if !chars.all(|c| c.is_ascii_alphanumeric() || c == '_') {
        return Some("may contain only ASCII letters, digits, and `_`");
    }
    None
}

/// Whether `name` is lexically a legal plain GALEC identifier.
#[must_use]
pub fn is_legal_plain_identifier(name: &str) -> bool {
    plain_identifier_shape_error(name).is_none()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accepts_and_rejects_by_shape() {
        for ok in ["a", "A1", "gain_2", "x_"] {
            assert!(is_legal_plain_identifier(ok), "{ok} should be legal");
            assert_eq!(plain_identifier_shape_error(ok), None);
        }
        // Empty and non-letter-first share one reason; a bad continuation
        // character is the other. (An empty identifier is NOT legal — the copies
        // this replaced disagreed on exactly this case.)
        for bad in ["", "1a", "_x", "9"] {
            assert!(!is_legal_plain_identifier(bad), "{bad:?} should be illegal");
            assert_eq!(
                plain_identifier_shape_error(bad),
                Some("must start with an ASCII letter"),
                "{bad:?}"
            );
        }
        assert_eq!(
            plain_identifier_shape_error("a b"),
            Some("may contain only ASCII letters, digits, and `_`")
        );
    }
}
