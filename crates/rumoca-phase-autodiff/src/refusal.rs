//! Refusals: the stated boundary of Jacobian synthesis.
//!
//! Every refusal names the rule that states it. The rule identifiers are the
//! ones written in the Jacobian synthesis specification, so a diagnostic text
//! and the specification row are the same fact spelled once.

use std::fmt;

use rumoca_core::{Location, Span};

/// A rule of the Jacobian synthesis specification.
///
/// The identifier is the stable half of every refusal diagnostic: message
/// wording may improve, the identifier may not change.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Rule {
    /// Recognized call form.
    CallForm,
    /// The differentiated function must be found from the call site.
    CalleeLookup,
    /// The differentiated signature must have a synthesizable shape.
    Signature,
    /// Only the stated statement forms carry a tangent.
    StatementForm,
    /// Only the stated expression forms carry a tangent.
    ExpressionForm,
    /// A called function must itself admit a synthesized tangent.
    CalleeTangent,
    /// Generated names must not collide with declared names.
    NameCollision,
    /// Only Real is differentiable; Integer, Boolean and String are constant.
    DifferentiableType,
    /// A declaration binding must have a tangent this engine can place.
    DeclarationBinding,
    /// An actual argument must carry the rank its formal declares, or the
    /// call is vectorized and its result is not the shape the wrapper states.
    ActualShape,
}

impl Rule {
    /// The stable identifier carried by every diagnostic citing this rule.
    pub fn id(self) -> &'static str {
        match self {
            Self::CallForm => "JAC-R1",
            Self::CalleeLookup => "JAC-R2",
            Self::Signature => "JAC-R3",
            Self::StatementForm => "JAC-R4",
            Self::ExpressionForm => "JAC-R5",
            Self::CalleeTangent => "JAC-R6",
            Self::NameCollision => "JAC-R7",
            Self::DifferentiableType => "JAC-R8",
            Self::DeclarationBinding => "JAC-R9",
            Self::ActualShape => "JAC-R10",
        }
    }
}

/// Where a refusal happened, as a source position of the parsed input.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Site {
    /// File name the parse was given.
    pub file: String,
    /// One-based line of the offending construct.
    pub line: u32,
    /// One-based column of the offending construct.
    pub column: u32,
    /// The construct's own span, for a source-located diagnostic.
    pub span: Option<Span>,
}

impl Site {
    /// A site at a parsed location inside `file`.
    pub fn at(file: &str, location: Option<&Location>) -> Self {
        let (line, column) = location.map_or((0, 0), |at| (at.start_line, at.start_column));
        Self {
            file: file.to_string(),
            line,
            column,
            span: location.filter(|at| at.has_source()).map(Location::span),
        }
    }

    /// Preserve an exact offending span while retaining the nearest honest
    /// line/column for syntax shapes that do not carry their own location.
    pub fn with_span(file: &str, location: Option<&Location>, span: Span) -> Self {
        let mut site = Self::at(file, location);
        if !span.is_dummy() {
            site.span = Some(span);
        }
        site
    }
}

impl fmt::Display for Site {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}:{}:{}", self.file, self.line, self.column)
    }
}

/// A construct Jacobian synthesis declines to differentiate.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Refusal {
    /// The specification rule that states the boundary.
    pub rule: Rule,
    /// What was refused, in the reader's terms.
    pub detail: String,
    /// Where the refused construct is.
    pub site: Site,
}

impl Refusal {
    /// Refuse `detail` under `rule` at `site`.
    pub fn new(rule: Rule, site: Site, detail: impl Into<String>) -> Self {
        Self {
            rule,
            detail: detail.into(),
            site,
        }
    }

    /// The refused construct's span, when the parse gave one.
    pub fn span(&self) -> Option<Span> {
        self.site.span
    }
}

impl fmt::Display for Refusal {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "jacobian refusal [{}] at {}: {}",
            self.rule.id(),
            self.site,
            self.detail
        )
    }
}

impl std::error::Error for Refusal {}

/// Result of any step that can refuse.
pub type Refusable<T> = Result<T, Refusal>;
