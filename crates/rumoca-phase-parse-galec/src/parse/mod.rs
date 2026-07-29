//! Private GALEC parser implementation (parol-generated LL(k)).
//!
//! The raw block exists only inside this phase. The public production entry
//! point closes it as `CheckedAlgorithmBlock`; the editor entry retains it in
//! an opaque document that exposes only positioned diagnostics/navigation.
//! This phase never parses Modelica or constructs DAE/Solve.
//!
//! The only overridden trait method is the start symbol `block`; every other
//! nonterminal is converted purely by `%nt_type` + a `TryFrom` builder. Because
//! parol 4.2.2 emits child conversions as
//! `.try_into().map_err(parol_runtime::ParolError::UserError)?`, every builder
//! `TryFrom` uses `type Error = anyhow::Error` (wrapping a
//! [`GalecSyntaxError`]); see [`errors`] for the bridging rationale.

mod block;
mod errors;
mod expr;
pub(crate) mod generated;
mod refs;
mod span;
mod stmt;
mod token;

pub use errors::GalecSyntaxError;

use generated::galec_grammar_trait;

/// User grammar struct: collects the single top-level [`rumoca_ir_galec::ast::Block`].
#[derive(Debug, Default)]
pub(crate) struct GalecGrammar {
    /// The parsed block, populated by the `block` semantic action.
    pub(crate) block: Option<rumoca_ir_galec::ast::Block>,
    /// When set, the `name_headed_statement` action records the right-hand side
    /// of a `name := expr` assignment into [`Self::captured_rhs`]. Enabled only
    /// by [`parse_expression`], which extracts a bare expression wrapped in a
    /// minimal block without needing the full block/statement builders.
    #[cfg(test)]
    capture_rhs: bool,
    /// Right-hand side captured while [`Self::capture_rhs`] is set.
    #[cfg(test)]
    captured_rhs: Option<rumoca_ir_galec::ast::Expression>,
}

impl galec_grammar_trait::GalecGrammarTrait for GalecGrammar {
    /// The start symbol receives the raw generated struct (auto-conversion only
    /// fires for a nonterminal consumed as a child; the root has no parent), so
    /// convert manually and bridge the builder error via parol's user channel.
    fn block(&mut self, arg: &galec_grammar_trait::Block) -> parol_runtime::Result<()> {
        self.block = Some(
            arg.try_into()
                .map_err(parol_runtime::ParolError::UserError)?,
        );
        Ok(())
    }

    /// Capture the right-hand side of a `name := expr` assignment when
    /// extracting a bare expression ([`parse_expression`]). The `:=` alternative
    /// already holds the converted `rumoca_ir_galec::ast::Expression`; a `name(args)` call
    /// statement carries no assignment RHS and is ignored. With error recovery
    /// disabled, this fires only for the wrapper's single well-formed
    /// assignment, so no garbage is captured on malformed input.
    #[cfg(test)]
    fn name_headed_statement(
        &mut self,
        arg: &galec_grammar_trait::NameHeadedStatement,
    ) -> parol_runtime::Result<()> {
        if !self.capture_rhs {
            return Ok(());
        }
        if let galec_grammar_trait::NameHeadedStatementGroup::NameHeadedStatementOptColonEquExpression(
            assignment,
        ) = &arg.name_headed_statement_group
        {
            self.captured_rhs = Some(assignment.expression.clone());
        }
        Ok(())
    }
}

/// Parse GALEC source into a phase-private raw block.
///
/// On failure the parol driver error is normalized into a typed
/// [`GalecSyntaxError`]. The crate-root production API closes successful output
/// through the checked IR constructor before returning it.
pub(crate) fn parse_block(
    source: &str,
    file_name: &str,
) -> Result<rumoca_ir_galec::ast::Block, GalecSyntaxError> {
    let mut grammar = GalecGrammar::default();
    generated::galec_parser::parse(source, file_name, &mut grammar)
        .map_err(|e| GalecSyntaxError::from_parol(&e, source))?;
    grammar.block.ok_or(GalecSyntaxError::NoAstProduced)
}

/// Parse a single GALEC expression into a [`rumoca_ir_galec::ast::Expression`].
///
/// The sole grammar start symbol is `block`, so the expression is wrapped in the
/// minimal block a single `DoStep` assignment would produce (`probe := <expr>;`)
/// and the reduced right-hand-side expression is extracted (contract §5.3). This
/// is the entry point the expression round-trip tests use; it depends only on
/// the expression-core builders, not on the block/statement builders, so it is
/// usable before those land. Error recovery is disabled, so a malformed
/// expression fails before the assignment reduces: nothing is captured and the
/// underlying parse error is surfaced.
#[cfg(test)]
pub(crate) fn parse_expression(
    source: &str,
    file_name: &str,
) -> Result<rumoca_ir_galec::ast::Expression, GalecSyntaxError> {
    let wrapped = format!(
        "block ExprProbe\nprotected\npublic\nmethod DoStep\nalgorithm\nprobe := {source};\nend DoStep;\nend ExprProbe;\n"
    );
    let mut grammar = GalecGrammar {
        capture_rhs: true,
        ..GalecGrammar::default()
    };
    let outcome = generated::galec_parser::parse(&wrapped, file_name, &mut grammar);
    if let Some(expression) = grammar.captured_rhs.take() {
        return Ok(expression);
    }
    match outcome {
        Ok(_) => Err(GalecSyntaxError::NoAstProduced),
        Err(err) => Err(GalecSyntaxError::from_parol(&err, &wrapped)),
    }
}
