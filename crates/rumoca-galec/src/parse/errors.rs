//! Parse-phase error type for the GALEC parser.
//!
//! `GalecParseError` is a dedicated SPEC_0008-shaped error in the GALEC
//! module's own `EG` code family, reserved sub-range **EG050–EG069** (existing
//! GALEC semantic codes reach EG040). It is a *distinct* type from
//! [`crate::diagnostic::GalecError`] (the semantic/lexeme family, whose location
//! is a structural AST path): a parse error carries **local byte offsets**, not
//! a `rumoca_core::Span` — importing `rumoca_core` here would violate GAL-010.
//!
//! ## Builder-error bridging (WI-2 decision)
//!
//! parol 4.2.2 generates child `%nt_type` conversions as
//! `.try_into().map_err(parol_runtime::ParolError::UserError)?`. Because
//! `ParolError::UserError` is `fn(anyhow::Error) -> ParolError`, every builder
//! `TryFrom` **must** use `type Error = anyhow::Error`. A typed builder error is
//! therefore constructed as `anyhow::Error::new(GalecParseError::…)`, bridged
//! through parol's `UserError` channel automatically by the generated code, and
//! recovered by [`GalecParseError::from_parol`] (which downcasts) at the
//! `parse()` boundary. The alternative "stash on the grammar struct" is not
//! viable: child conversions run inside generated code that never touches the
//! user grammar struct on the error path.

/// A GALEC parse error (EG050–EG069).
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum GalecParseError {
    /// Syntax error (unexpected token, lexer error, or a builder rejection that
    /// carries no more specific variant).
    #[error("{message} [EG050]")]
    Syntax {
        message: String,
        expected: Vec<String>,
        unexpected: Option<String>,
        /// Local byte offsets `(start, end)` — NOT `rumoca_core::Span` (GAL-010).
        span: Option<(usize, usize)>,
    },
    /// A `block` / `record` / `function` / `method` terminator name did not
    /// match its header name (trap: `end Foo;` after `block Bar`).
    #[error("mismatched terminator: expected `{expected}`, found `{found}` [EG051]")]
    TerminatorMismatch {
        expected: String,
        found: String,
        span: Option<(usize, usize)>,
    },
    /// A block-interface method declared parameters (trap T1).
    #[error("block method `{name}` must not declare parameters [EG052]")]
    MethodHasParameters { name: String },
    /// A mandatory block method (`Startup`/`Recalibrate`/`DoStep`) is missing.
    #[error("block is missing mandatory method `{name}` [EG053]")]
    MissingMethod { name: String },
    /// A block method appeared more than once.
    #[error("duplicate block method `{name}` [EG054]")]
    DuplicateMethod { name: String },
    /// A method `signals` clause named a non-predefined signal.
    #[error("unknown predefined signal `{name}` in method signals clause [EG055]")]
    UnknownPredefinedSignal { name: String },
    /// The driver returned without producing a block.
    #[error("parser produced no block [EG056]")]
    NoAstProduced,
}

impl GalecParseError {
    /// Construct a `Syntax` error with just a message (no expected set / span).
    #[must_use]
    pub fn syntax(message: impl Into<String>) -> Self {
        Self::Syntax {
            message: message.into(),
            expected: Vec::new(),
            unexpected: None,
            span: None,
        }
    }

    /// Wrap this typed error for parol's user-error channel. See the module
    /// docs: builder `TryFrom`s return `anyhow::Error`, so callers usually
    /// construct `anyhow::Error::new(err)` directly; this helper names the
    /// bridge explicitly where that reads clearer.
    #[cfg(feature = "parse")]
    #[must_use]
    pub fn into_anyhow(self) -> anyhow::Error {
        anyhow::Error::new(self)
    }

    /// Normalize a parol driver error back into a typed `GalecParseError` at the
    /// `parse()` boundary. Builder errors bridged via `UserError` are recovered
    /// by downcast; genuine syntax/lexer errors become `Syntax`.
    #[cfg(feature = "parse")]
    #[must_use]
    pub fn from_parol(err: &parol_runtime::errors::ParolError, _source: &str) -> Self {
        use parol_runtime::errors::ParolError;
        match err {
            ParolError::UserError(user) => user
                .downcast_ref::<Self>()
                .cloned()
                .unwrap_or_else(|| Self::syntax(format!("parse error: {user}"))),
            ParolError::ParserError(parser_err) => {
                Self::syntax(format!("syntax error: {parser_err}"))
            }
            ParolError::LexerError(lexer_err) => Self::syntax(format!("lexer error: {lexer_err}")),
        }
    }
}
