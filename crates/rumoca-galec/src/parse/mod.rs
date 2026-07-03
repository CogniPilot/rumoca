//! GALEC parser (parol-generated LL(k)), gated behind the default-off `parse`
//! cargo feature (GAL-014: parses into the GALEC AST only — never DAE/Solve,
//! never Modelica input).
//!
//! The only overridden trait method is the start symbol `block`; every other
//! nonterminal is converted purely by `%nt_type` + a `TryFrom` builder. Because
//! parol 4.2.2 emits child conversions as
//! `.try_into().map_err(parol_runtime::ParolError::UserError)?`, every builder
//! `TryFrom` uses `type Error = anyhow::Error` (wrapping a
//! [`GalecParseError`]); see [`errors`] for the bridging rationale.

pub mod block;
pub mod errors;
pub mod expr;
pub mod generated;
pub mod refs;
pub mod stmt;
pub mod token;

pub use errors::GalecParseError;

use generated::galec_grammar_trait;

/// User grammar struct: collects the single top-level [`crate::ast::Block`].
#[derive(Debug, Default)]
pub struct GalecGrammar {
    /// The parsed block, populated by the `block` semantic action.
    pub block: Option<crate::ast::Block>,
    /// When set, the `name_headed_statement` action records the right-hand side
    /// of a `name := expr` assignment into [`Self::captured_rhs`]. Enabled only
    /// by [`parse_expression`], which extracts a bare expression wrapped in a
    /// minimal block without needing the full block/statement builders.
    capture_rhs: bool,
    /// Right-hand side captured while [`Self::capture_rhs`] is set.
    captured_rhs: Option<crate::ast::Expression>,
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
    /// already holds the converted `crate::ast::Expression`; a `name(args)` call
    /// statement carries no assignment RHS and is ignored. With error recovery
    /// disabled, this fires only for the wrapper's single well-formed
    /// assignment, so no garbage is captured on malformed input.
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

/// Parse GALEC source into a [`crate::ast::Block`].
///
/// GAL-014: the parser produces the GALEC AST only. On failure the parol driver
/// error is normalized back into a typed [`GalecParseError`] (builder errors are
/// recovered by downcast; syntax/lexer errors become `Syntax`).
pub fn parse(source: &str, file_name: &str) -> Result<crate::ast::Block, GalecParseError> {
    let mut grammar = GalecGrammar::default();
    generated::galec_parser::parse(source, file_name, &mut grammar)
        .map_err(|e| GalecParseError::from_parol(&e, source))?;
    grammar.block.ok_or(GalecParseError::NoAstProduced)
}

/// Parse a single GALEC expression into a [`crate::ast::Expression`].
///
/// The sole grammar start symbol is `block`, so the expression is wrapped in the
/// minimal block a single `DoStep` assignment would produce (`probe := <expr>;`)
/// and the reduced right-hand-side expression is extracted (contract §5.3). This
/// is the entry point the expression round-trip tests use; it depends only on
/// the expression-core builders, not on the block/statement builders, so it is
/// usable before those land. Error recovery is disabled, so a malformed
/// expression fails before the assignment reduces: nothing is captured and the
/// underlying parse error is surfaced.
pub fn parse_expression(
    source: &str,
    file_name: &str,
) -> Result<crate::ast::Expression, GalecParseError> {
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
        Ok(_) => Err(GalecParseError::NoAstProduced),
        Err(err) => Err(GalecParseError::from_parol(&err, &wrapped)),
    }
}
