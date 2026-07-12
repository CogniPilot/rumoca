//! Builders for `Name` / `Reference` / `RefPart` (WI-3).
//!
//! Every builder is `TryFrom<&Generated> for AstType` with
//! `type Error = anyhow::Error`: parol 4.2.2 emits each `%nt_type` child
//! conversion as `.try_into().map_err(parol_runtime::ParolError::UserError)?`,
//! which pins the associated error type to `anyhow::Error`. Typed rejections are
//! built as [`crate::parse::errors::GalecParseError`] and bridged with
//! `into_anyhow()`.

use crate::parse::errors::GalecParseError;
use crate::parse::generated::galec_grammar_trait as g;

/// `name : ident | quoted` → [`crate::ast::Name`].
///
/// The quoted lexeme keeps its surrounding `'` delimiters in the token text
/// (regex `'[^'\r\n]*'`); the AST stores only the content between them, so the
/// delimiters are stripped here. Well-formedness of the content is a validator
/// concern (`validate/names.rs`), not the parser's.
impl TryFrom<&g::Name> for crate::ast::Name {
    type Error = anyhow::Error;

    fn try_from(ast: &g::Name) -> Result<Self, Self::Error> {
        Ok(match ast {
            g::Name::Ident(n) => Self::Ident(
                crate::ast::Identifier::new(n.ident.ident.text()),
                n.ident.ident.span(),
            ),
            g::Name::Quoted(n) => {
                let text = n.quoted.quoted.text();
                let content = text
                    .strip_prefix('\'')
                    .and_then(|s| s.strip_suffix('\''))
                    .ok_or_else(|| {
                        GalecParseError::syntax(format!(
                            "malformed quoted identifier lexeme `{text}`"
                        ))
                        .into_anyhow()
                    })?;
                Self::Quoted(content.to_string(), n.quoted.quoted.span())
            }
        })
    }
}

/// `reference : state_reference | local_reference` → [`crate::ast::Reference`].
impl TryFrom<&g::Reference> for crate::ast::Reference {
    type Error = anyhow::Error;

    fn try_from(ast: &g::Reference) -> Result<Self, Self::Error> {
        Ok(match ast {
            g::Reference::StateReference(r) => Self::State(state_reference_tail_parts(
                &r.state_reference.state_reference_tail,
            )),
            g::Reference::LocalReference(r) => {
                Self::Local(local_reference_ref_part(&r.local_reference))
            }
        })
    }
}

/// `ref_part : name [ computed_dimensions ]` → [`crate::ast::RefPart`].
impl TryFrom<&g::RefPart> for crate::ast::RefPart {
    type Error = anyhow::Error;

    fn try_from(ast: &g::RefPart) -> Result<Self, Self::Error> {
        Ok(Self {
            span: ast.name.span(),
            name: ast.name.clone(),
            subscripts: match &ast.ref_part_opt {
                Some(opt) => computed_dimensions_to_vec(&opt.computed_dimensions),
                None => Vec::new(),
            },
        })
    }
}

/// Collect the `≥ 1` parts of a `self.a.b…` state-reference tail. The `ref_part`
/// fields are already `crate::ast::RefPart` (converted via `%nt_type`).
pub(crate) fn state_reference_tail_parts(tail: &g::StateReferenceTail) -> Vec<crate::ast::RefPart> {
    let mut parts = Vec::with_capacity(1 + tail.state_reference_tail_list.len());
    parts.push(tail.ref_part.clone());
    for extra in &tail.state_reference_tail_list {
        parts.push(extra.ref_part.clone());
    }
    parts
}

/// `local_reference : name [ computed_dimensions ]` → a single [`crate::ast::RefPart`].
pub(crate) fn local_reference_ref_part(local: &g::LocalReference) -> crate::ast::RefPart {
    crate::ast::RefPart {
        span: local.name.span(),
        name: local.name.clone(),
        subscripts: match &local.local_reference_opt {
            Some(opt) => computed_dimensions_to_vec(&opt.computed_dimensions),
            None => Vec::new(),
        },
    }
}

/// `computed_dimensions : '[' expression { ',' expression } ']'` → the subscript
/// list (each `expression` already converted via `%nt_type`).
pub(crate) fn computed_dimensions_to_vec(
    dims: &g::ComputedDimensions,
) -> Vec<crate::ast::Expression> {
    let mut out = Vec::with_capacity(1 + dims.computed_dimensions_list.len());
    out.push(dims.expression.clone());
    for extra in &dims.computed_dimensions_list {
        out.push(extra.expression.clone());
    }
    out
}
