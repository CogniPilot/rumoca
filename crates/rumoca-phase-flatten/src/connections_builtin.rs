use rumoca_ir_ast as ast;

use crate::FlattenError;

/// Extract the optional priority argument from `Connections.potentialRoot`.
///
/// MLS §9.4 defines omitted priority as zero. A present numeric token must
/// parse successfully; otherwise the lexer/parser accepted malformed source.
pub(crate) fn extract_potential_root_priority(
    args: &[ast::Expression],
    fallback_span: rumoca_core::Span,
) -> Result<i64, FlattenError> {
    let Some(priority) = args.get(1) else {
        return Ok(0);
    };
    let ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token,
        ..
    } = priority
    else {
        let span = {
            let s = priority.span();
            if s.is_dummy() { fallback_span } else { s }
        };
        return Err(FlattenError::unsupported_equation(
            "Connections.potentialRoot priority must be an integer literal (MLS §9.4)".to_string(),
            span,
        ));
    };
    let span = {
        let s = priority.span();
        if s.is_dummy() { fallback_span } else { s }
    };
    token
        .text
        .parse()
        .map_err(|_| FlattenError::malformed_numeric_literal(token.text.to_string(), span))
}
