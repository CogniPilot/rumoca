use anyhow::Result;
use rumoca::ir::ast::StoredDefinition;
use rumoca::modelica_grammar::ModelicaGrammar;
use rumoca::modelica_parser::parse;
use std::fs;

/// Parse a test file from the fixtures directory
#[allow(dead_code)]
pub fn parse_test_file(name: &str) -> Result<StoredDefinition> {
    let path = format!("tests/fixtures/{}.mo", name);
    let input = fs::read_to_string(&path)
        .map_err(|e| anyhow::anyhow!("Failed to read test file {}: {}", path, e))?;

    let mut grammar = ModelicaGrammar::new();
    parse(&input, &path, &mut grammar)?;

    grammar
        .modelica
        .ok_or_else(|| anyhow::anyhow!("Parser succeeded but produced no AST for {}", path))
}
