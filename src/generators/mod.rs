pub mod casadi_mx;
pub mod casadi_sx;
pub mod collimator;
pub mod json;
pub mod sympy;

use crate::ast;
use crate::lexer;
use crate::parser;

use lexer::Lexer;
use parser::StoredDefinitionParser;

pub fn parse_file(filename: &str) -> ast::StoredDefinition {
    let source_code = std::fs::read_to_string(filename).unwrap();
    let lexer = Lexer::new(&source_code);
    let parser = StoredDefinitionParser::new();
    let def = parser.parse(lexer).expect("failed to parse");
    return def;
}
