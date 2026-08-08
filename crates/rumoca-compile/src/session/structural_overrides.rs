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

use rumoca_core::{SourceMap, Span, Token};
use rumoca_ir_ast::{Expression, ModificationValue, QualifiedName, TerminalType};

use crate::session::Session;

#[derive(Debug, Clone)]
pub(super) struct StructuralOverrideSource {
    name: String,
    content: Arc<str>,
}

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
    fn to_modification(
        self,
        target: &QualifiedName,
    ) -> (ModificationValue, StructuralOverrideSource) {
        let (terminal_type, text) = match self {
            Self::Bool(value) => (TerminalType::Bool, value.to_string()),
            Self::Int(value) => (TerminalType::UnsignedInteger, value.to_string()),
            Self::Real(value) => (TerminalType::UnsignedReal, value.to_string()),
        };
        // A real (non-dummy) span: downstream runtime-metadata validation
        // requires every variable-attribute expression to carry source
        // provenance. We attribute the synthetic literal to a virtual source.
        let source_name = structural_override_source_name(target);
        let span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name(&source_name),
            0,
            text.len(),
        );
        let content = Arc::<str>::from(text);
        let literal = Expression::Terminal {
            terminal_type,
            token: Token {
                text: content.clone(),
                ..Default::default()
            },
            span,
        };
        // A literal is its own symbolic source; ToDae requires the `source` form
        // for runtime-metadata provenance, and a source scope to resolve it.
        (
            ModificationValue::with_source_scope(
                literal.clone(),
                Some(literal),
                Some(QualifiedName::default()),
            ),
            StructuralOverrideSource {
                name: source_name,
                content,
            },
        )
    }
}

fn structural_override_source_name(target: &QualifiedName) -> String {
    format!("<rumoca structural override:{target}>")
}

impl Session {
    /// Set the structural parameter overrides applied on the next compile.
    ///
    /// Each override names a parameter (`"n"`, or a dotted path like
    /// `"gear.ratio"`) and a typed value, injected as a root modification so the
    /// instantiator re-evaluates dimensions/conditionals under the new value.
    /// Replaces any previously-set overrides; pass an empty slice to clear.
    pub fn set_structural_overrides(&mut self, overrides: &[(String, StructuralOverride)]) {
        self.bump_revision();
        let prepared = overrides.iter().map(|(name, value)| {
            let target = QualifiedName::from_dotted(name);
            let (modification, source) = value.to_modification(&target);
            ((target, modification), source)
        });
        (
            self.instantiation_options.root_modifications,
            self.structural_override_sources,
        ) = prepared.unzip();
        // Structural options are semantic compile inputs but do not change the
        // source dependency fingerprint. Discard every artifact at and below
        // instantiation so a repeated compile cannot reuse the old structure.
        self.query_state.flat = Default::default();
        self.query_state.dae = Default::default();
    }

    pub(in crate::session) fn register_structural_override_sources(
        &self,
        source_map: &mut SourceMap,
    ) {
        for source in &self.structural_override_sources {
            source_map.add_shared(&source.name, source.content.clone());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::SessionConfig;

    #[test]
    fn structural_override_recompile_preserves_provenance_and_invalidates_structure() {
        let mut session = Session::new(SessionConfig::default());
        session
            .add_document(
                "input.mo",
                "model M\n  parameter Integer n = 2;\n  Real x[n];\nequation\n  der(x) = -x;\nend M;\n",
            )
            .expect("fixture should parse");

        session.set_structural_overrides(&[("n".to_string(), StructuralOverride::Int(2))]);
        let first = session
            .compile_model_strict("M")
            .expect("first structural variant should compile");
        assert_eq!(state_scalar_count(first.result()), 2);

        session.set_structural_overrides(&[("n".to_string(), StructuralOverride::Int(4))]);
        let second = session
            .compile_model_strict("M")
            .expect("second structural variant should compile");
        assert_eq!(state_scalar_count(second.result()), 4);
    }

    fn state_scalar_count(result: &crate::session::CompilationResult) -> usize {
        result.dae.inspect(|view| {
            view.variables()
                .find_map(|(_, variable)| {
                    (variable.name().as_str() == "x").then(|| variable.scalar_count())
                })
                .expect("fixture state should exist")
        })
    }
}
