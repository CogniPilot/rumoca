//! Conversion for component references and names.

use crate::ir;
use crate::modelica_grammar_trait;

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::ComponentReference> for ir::ast::ComponentReference {
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

        parts.push(ir::ast::ComponentRefPart {
            ident: ast.ident.clone(),
            subs: first_subs,
        });
        for comp_ref in &ast.component_reference_list {
            parts.push(comp_ref.component_ref_part.clone());
        }
        Ok(ir::ast::ComponentReference {
            local: ast.component_reference_opt.is_some(),
            parts,
        })
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::ComponentRefPart> for ir::ast::ComponentRefPart {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ComponentRefPart,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ir::ast::ComponentRefPart {
            ident: ast.ident.clone(),
            subs: ast
                .component_ref_part_opt
                .as_ref()
                .map(|subs| subs.array_subscripts.subscripts.clone()),
        })
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::Name> for ir::ast::Name {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Name) -> std::result::Result<Self, Self::Error> {
        let mut name = vec![ast.ident.clone()];
        for ident in &ast.name_list {
            name.push(ident.ident.clone());
        }
        Ok(ir::ast::Name { name })
    }
}
