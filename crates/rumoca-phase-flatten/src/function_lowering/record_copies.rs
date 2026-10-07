//! Record-input rewrites of function bodies that read or copy a decomposed
//! record whole (MLS 3.7 section 12.4.4).

use super::{DecomposedParam, record_param_field_var_ref};

/// Lower a whole-record copy of a decomposed record input (`next := previous`,
/// MLS 3.7 section 12.4.4) to one assignment per field, `next.f := previous_f`,
/// recursively through nested statements. An array field is copied whole; a
/// record-typed field is copied as one assignment that the next decomposition
/// pass expands again. No constructor call is built for a copy.
pub(super) fn expand_record_copies(
    statements: Vec<rumoca_core::Statement>,
    params: &[DecomposedParam],
) -> Vec<rumoca_core::Statement> {
    let mut expanded = Vec::with_capacity(statements.len());
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, span } => {
                match record_copy_fields(&comp, &value, params, span) {
                    Some(copies) => expanded.extend(copies),
                    None => expanded.push(rumoca_core::Statement::Assignment { comp, value, span }),
                }
            }
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            } => expanded.push(rumoca_core::Statement::For {
                indices,
                equations: expand_record_copies(equations, params),
                span,
            }),
            rumoca_core::Statement::While { block, span } => {
                expanded.push(rumoca_core::Statement::While {
                    block: expand_record_block(block, params),
                    span,
                })
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            } => expanded.push(rumoca_core::Statement::If {
                cond_blocks: cond_blocks
                    .into_iter()
                    .map(|block| expand_record_block(block, params))
                    .collect(),
                else_block: else_block.map(|block| expand_record_copies(block, params)),
                span,
            }),
            rumoca_core::Statement::When { blocks, span } => {
                expanded.push(rumoca_core::Statement::When {
                    blocks: blocks
                        .into_iter()
                        .map(|block| expand_record_block(block, params))
                        .collect(),
                    span,
                })
            }
            other => expanded.push(other),
        }
    }
    expanded
}

fn expand_record_block(
    block: rumoca_core::StatementBlock,
    params: &[DecomposedParam],
) -> rumoca_core::StatementBlock {
    rumoca_core::StatementBlock {
        cond: block.cond,
        stmts: expand_record_copies(block.stmts, params),
    }
}

/// The per-field assignments of `target := value` when `value` is a whole
/// decomposed record input, or `None` when the statement is not such a copy.
fn record_copy_fields(
    target: &rumoca_core::ComponentReference,
    value: &rumoca_core::Expression,
    params: &[DecomposedParam],
    span: rumoca_core::Span,
) -> Option<Vec<rumoca_core::Statement>> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = value
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let param = params
        .iter()
        .find(|param| param.param_name == name.as_str())?;
    param
        .fields
        .iter()
        .map(|field| {
            let mut parts = target.parts().to_vec();
            parts.push(rumoca_core::ComponentRefPart {
                ident: field.name.clone(),
                span,
                subs: Vec::new(),
                def_id: field.def_id?,
            });
            let comp =
                rumoca_core::ComponentReference::construct(target.local(), span, parts).ok()?;
            Some(rumoca_core::Statement::Assignment {
                comp,
                value: record_param_field_var_ref(&param.param_name, &field.name, span),
                span,
            })
        })
        .collect()
}

/// Fuse a record-field read spelled as a dotted name without structured parts
/// (`previous.catalog.n`, as a function input default carries it) onto the
/// decomposed field it names: the longest leading run of field segments joins
/// into `<param>_<field>`, and any remaining segments stay dotted for the next
/// decomposition pass.
pub(super) fn dotted_record_param_field(
    expression: &rumoca_core::Expression,
    params: &[DecomposedParam],
) -> Option<rumoca_core::Expression> {
    let rumoca_core::Expression::VarRef {
        name,
        subscripts,
        span,
    } = expression
    else {
        return None;
    };
    if name.component_ref().is_some() {
        return None;
    }
    let segments = name.as_str().split('.').collect::<Vec<_>>();
    let (head, fields) = segments.split_first()?;
    let param = params.iter().find(|param| param.param_name == *head)?;
    (1..=fields.len()).rev().find_map(|fused_len| {
        let field = fields[..fused_len].join("_");
        param
            .fields
            .iter()
            .any(|candidate| candidate.name == field)
            .then(|| {
                let mut spelled = format!("{}_{}", param.param_name, field);
                for rest in &fields[fused_len..] {
                    spelled.push('.');
                    spelled.push_str(rest);
                }
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::generated(spelled),
                    subscripts: subscripts.clone(),
                    span: *span,
                }
            })
    })
}
