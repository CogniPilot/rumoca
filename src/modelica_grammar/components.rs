//! Conversion for tokens, strings, and component lists.

use crate::ir;
use crate::modelica_grammar_trait;

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::String> for ir::ast::Token {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::String) -> std::result::Result<Self, Self::Error> {
        let mut tok = ast.string.clone();
        // remove quotes from string text (with bounds check for malformed input)
        if tok.text.len() >= 2 {
            tok.text = tok.text[1..tok.text.len() - 1].to_string();
        }
        Ok(tok)
    }
}

//-----------------------------------------------------------------------------
#[derive(Default, Clone, Debug, PartialEq)]

pub struct TokenList {
    pub tokens: Vec<ir::ast::Token>,
}

impl TryFrom<&modelica_grammar_trait::DescriptionString> for TokenList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::DescriptionString,
    ) -> std::result::Result<Self, Self::Error> {
        let mut tokens = Vec::new();
        if let Some(opt) = &ast.description_string_opt {
            tokens.push(opt.string.clone());
            for string in &opt.description_string_opt_list {
                tokens.push(string.string.clone());
            }
        }
        Ok(TokenList { tokens })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]

pub struct ComponentList {
    pub components: Vec<modelica_grammar_trait::ComponentDeclaration>,
}

impl TryFrom<&modelica_grammar_trait::ComponentList> for ComponentList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ComponentList,
    ) -> std::result::Result<Self, Self::Error> {
        let mut components = vec![ast.component_declaration.clone()];
        for comp in &ast.component_list_list {
            components.push(comp.component_declaration.clone());
        }
        Ok(ComponentList { components })
    }
}
