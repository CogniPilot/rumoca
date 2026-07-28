//! Conversion for tokens, strings, and component lists.

use crate::generated::modelica_grammar_trait;

fn decode_modelica_string_literal(text: &str) -> anyhow::Result<String> {
    let Some(contents) = text
        .strip_prefix('"')
        .and_then(|text| text.strip_suffix('"'))
    else {
        anyhow::bail!("malformed Modelica string literal");
    };
    let mut decoded = String::with_capacity(contents.len());
    let mut chars = contents.chars();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            decoded.push(ch);
            continue;
        }
        let Some(escaped) = chars.next() else {
            anyhow::bail!("unterminated Modelica string escape");
        };
        decoded.push(match escaped {
            '\'' => '\'',
            '"' => '"',
            '?' => '?',
            '\\' => '\\',
            'a' => '\x07',
            'b' => '\x08',
            'f' => '\x0c',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'v' => '\x0b',
            _ => anyhow::bail!("invalid Modelica string escape `\\{escaped}`"),
        });
    }
    Ok(decoded)
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::String> for rumoca_core::Token {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::String) -> std::result::Result<Self, Self::Error> {
        let mut tok: rumoca_core::Token = ast.string.clone().into();
        tok.text = std::sync::Arc::from(decode_modelica_string_literal(&tok.text)?);
        Ok(tok)
    }
}

//-----------------------------------------------------------------------------
#[derive(Default, Clone, Debug, PartialEq)]

pub struct TokenList {
    pub tokens: Vec<rumoca_core::Token>,
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
