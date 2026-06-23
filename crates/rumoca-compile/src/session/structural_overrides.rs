//! Structural parameter overrides: re-instantiate a model with parameter values
//! injected at the root, so array dimensions and conditional components
//! re-evaluate (MLS §7.2 modification semantics).
//!
//! This is the compile-API counterpart to the sim-API tunable overrides. A
//! structural parameter (`Evaluate=true`, or an Integer/Boolean that sizes an
//! array or gates a conditional component) is baked during instantiation, so it
//! cannot be changed by a runtime override — it needs a recompile. We do that
//! the sanctioned way: a *synthetic root modification* fed to the instantiator,
//! never by editing source or patching IR.

use std::sync::Arc;

use rumoca_core::{Span, Token};
use rumoca_ir_ast::{Expression, ModificationValue, QualifiedName, TerminalType};

use crate::session::Session;

/// A typed structural-override value. The variant determines the literal kind so
/// dimension evaluation (Integer) and conditional activation (Boolean) accept it.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StructuralOverride {
    Bool(bool),
    Int(i64),
    Real(f64),
}

impl StructuralOverride {
    /// Build the synthetic literal modification expression for this value.
    ///
    /// The value is a self-contained literal, so it references nothing — but
    /// flatten still requires a binding source scope to resolve against. The
    /// root scope (empty [`QualifiedName`]) is correct: the literal needs no
    /// enclosing names.
    fn to_modification(self) -> ModificationValue {
        let (terminal_type, text) = match self {
            Self::Bool(value) => (TerminalType::Bool, value.to_string()),
            Self::Int(value) => (TerminalType::UnsignedInteger, value.to_string()),
            Self::Real(value) => (TerminalType::UnsignedReal, value.to_string()),
        };
        // A real (non-dummy) span: downstream runtime-metadata validation
        // requires every variable-attribute expression to carry source
        // provenance. We attribute the synthetic literal to a virtual source.
        let span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name("<rumoca structural override>"),
            0,
            text.len(),
        );
        let literal = Expression::Terminal {
            terminal_type,
            token: Token {
                text: Arc::from(text.as_str()),
                ..Default::default()
            },
            span,
        };
        // A literal is its own symbolic source; ToDae requires the `source` form
        // for runtime-metadata provenance, and a source scope to resolve it.
        ModificationValue::with_source_scope(
            literal.clone(),
            Some(literal),
            Some(QualifiedName::default()),
        )
    }
}

impl Session {
    /// Set the structural parameter overrides applied on the next compile.
    ///
    /// Each override names a parameter (`"n"`, or a dotted path like
    /// `"gear.ratio"`) and a typed value, injected as a root modification so the
    /// instantiator re-evaluates dimensions/conditionals under the new value.
    /// Replaces any previously-set overrides; pass an empty slice to clear.
    pub fn set_structural_overrides(&mut self, overrides: &[(String, StructuralOverride)]) {
        self.instantiation_options.root_modifications = overrides
            .iter()
            .map(|(name, value)| (QualifiedName::from_dotted(name), value.to_modification()))
            .collect();
    }
}
