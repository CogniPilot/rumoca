//! MLS §8.3.7 assertion levels.
//!
//! "assertionLevel is an optional evaluable expression" of the predefined
//! enumeration `AssertionLevel = enumeration(warning, error)`; an omitted
//! level is `AssertionLevel.error`. Flattening settles every level that names
//! a predefined literal, identified by the literal's declaration and never by
//! a rendered spelling: the error level becomes the omitted form and the
//! warning level becomes its enumeration ordinal, the Integer literal
//! `Integer(AssertionLevel.warning) = 1`. Every later phase reads the settled
//! form through [`AssertionLevel::of_settled`]; any other level expression is
//! one flattening could not identify.

use rumoca_core::{DefId, Expression, Literal, Span};

/// The level an assertion is checked at.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum AssertionLevel {
    /// The current evaluation is aborted when the condition is false.
    Error,
    /// The evaluation is not aborted; a violation is reported and has no
    /// influence on the behavior of the model.
    Warning,
}

/// The enumeration ordinal of `AssertionLevel.warning`.
const WARNING_ORDINAL: i64 = 1;

impl AssertionLevel {
    /// The level a settled level expression denotes, or `None` for a level
    /// flattening could not identify.
    pub fn of_settled(level: Option<&Expression>) -> Option<Self> {
        match level {
            None => Some(Self::Error),
            Some(Expression::Literal {
                value: Literal::Integer(WARNING_ORDINAL),
                ..
            }) => Some(Self::Warning),
            Some(_) => None,
        }
    }

    /// The settled level expression of this level.
    pub fn settled_expression(self, span: Span) -> Option<Expression> {
        match self {
            Self::Error => None,
            Self::Warning => Some(Expression::Literal {
                value: Literal::Integer(WARNING_ORDINAL),
                span,
            }),
        }
    }
}

/// Exact declaration identities of the predefined `AssertionLevel` literals.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct AssertionLevelLiterals {
    pub error: Option<DefId>,
    pub warning: Option<DefId>,
}

impl AssertionLevelLiterals {
    /// The level a source level expression names, or `None` when the
    /// expression is not a reference to a predefined literal.
    pub fn classify(&self, level: Option<&Expression>) -> Option<AssertionLevel> {
        let Some(level) = level else {
            return Some(AssertionLevel::Error);
        };
        let Expression::VarRef {
            name, subscripts, ..
        } = level
        else {
            return None;
        };
        let target = name
            .component_ref()
            .filter(|_| subscripts.is_empty())
            .map(rumoca_core::ComponentReference::target_def_id)?;
        if Some(target) == self.error {
            Some(AssertionLevel::Error)
        } else if Some(target) == self.warning {
            Some(AssertionLevel::Warning)
        } else {
            None
        }
    }
}
