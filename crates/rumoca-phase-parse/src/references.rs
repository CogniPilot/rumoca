//! Conversion for component references and names.

use crate::generated::modelica_grammar_trait;
use crate::helpers::{merge_spans, token_span};

impl TryFrom<&modelica_grammar_trait::ComponentReference> for rumoca_ir_ast::ComponentReference {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ComponentReference,
    ) -> std::result::Result<Self, Self::Error> {
        let mut parts = Vec::new();

        // Handle subscripts for the first part (e.g., x[i] in component_reference_opt0)
        let first_subs = ast
            .component_reference_opt0
            .as_ref()
            .map(|opt| opt.array_subscripts.subscripts.clone());

        let start = match &ast.component_reference_opt {
            Some(opt) => token_span(&opt.dot)?,
            None => token_span(&ast.ident)?,
        };
        let mut end = match &ast.component_reference_opt0 {
            Some(opt) => opt.array_subscripts.delimiter_span,
            None => token_span(&ast.ident)?,
        };
        parts.push(rumoca_ir_ast::ComponentRefPart {
            ident: ast.ident.clone(),
            subs: first_subs,
            def_id: None,
        });
        for comp_ref in &ast.component_reference_list {
            let part = &comp_ref.component_ref_part;
            end = match &part.component_ref_part_opt {
                Some(opt) => opt.array_subscripts.delimiter_span,
                None => token_span(&part.ident)?,
            };
            parts.push(part.as_ref().try_into()?);
        }
        let span = merge_spans(start, end);
        Ok(rumoca_ir_ast::ComponentReference {
            local: ast.component_reference_opt.is_some(),
            parts,
            span,
            qualified_display_name: None,
        })
    }
}

impl TryFrom<&modelica_grammar_trait::ComponentRefPart> for rumoca_ir_ast::ComponentRefPart {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ComponentRefPart,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(rumoca_ir_ast::ComponentRefPart {
            ident: ast.ident.clone(),
            subs: ast
                .component_ref_part_opt
                .as_ref()
                .map(|subs| subs.array_subscripts.subscripts.clone()),
            def_id: None,
        })
    }
}

impl TryFrom<&modelica_grammar_trait::Name> for rumoca_ir_ast::Name {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Name) -> std::result::Result<Self, Self::Error> {
        let mut name = vec![ast.ident.clone()];
        for ident in &ast.name_list {
            name.push(ident.ident.clone());
        }
        Ok(rumoca_ir_ast::Name { name, def_id: None })
    }
}
