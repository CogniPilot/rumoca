//! The declaration of one enumeration type (MLS §4.9.5).

use serde::{Deserialize, Serialize};

/// An enumeration type as its declaration states it: the qualified name and
/// the literals in declaration order, the ordinal of a literal being its
/// one-based position.
///
/// A variable of the type carries the declaration, so a consumer that exposes
/// the variable (an FMI model description) names its literals without
/// recovering them from spellings.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct EnumerationDeclaration {
    pub name: String,
    pub literals: Vec<String>,
}

impl EnumerationDeclaration {
    /// Whether `ordinal` denotes one of the declared literals.
    pub fn contains_ordinal(&self, ordinal: i64) -> bool {
        usize::try_from(ordinal).is_ok_and(|ordinal| (1..=self.literals.len()).contains(&ordinal))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_ordinal_is_valid_between_one_and_the_literal_count() {
        let declaration = EnumerationDeclaration {
            name: "Mode".to_string(),
            literals: vec!["Off".to_string(), "On".to_string()],
        };
        assert!(!declaration.contains_ordinal(0));
        assert!(declaration.contains_ordinal(1));
        assert!(declaration.contains_ordinal(2));
        assert!(!declaration.contains_ordinal(3));
        assert!(!declaration.contains_ordinal(-1));
    }
}
