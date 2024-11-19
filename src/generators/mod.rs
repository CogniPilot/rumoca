pub mod casadi_mx;
pub mod casadi_sx;
pub mod collimator;
pub mod json;
pub mod sympy;

use crate::ast;
use crate::lexer;
use crate::parser;
use crate::tokens::{LexicalError, Token};

use codespan_reporting::files::SimpleFiles;

use lexer::Lexer;
use parser::StoredDefinitionParser;
use lalrpop_util::ParseError;

pub fn parse_file(filename: &str) -> Result<ast::StoredDefinition, ParseError<usize, Token, LexicalError>> {
    let mut files = SimpleFiles::new();
    let file_id = files.add(
        filename,
        std::fs::read_to_string(filename).unwrap(),
    );
    let file = files.get(file_id).expect("failed to get file");
    let lexer = Lexer::new(file.source());
    let parser = StoredDefinitionParser::new();
    parser.parse(lexer)
    // if def.is_err() {
    //     // let type_id = def.as_ref().unwrap().type_id();
    //     // match type_id {
    //     //     UnrecognizedToken => println!("unrecognized token!"),
    //     //     _ => println!("type unhandled {:?}", type_id.)
    //     // }
    //     let err = def.as_ref().expect_err("error");

    //     let writer = StandardStream::stderr(ColorChoice::Always);
    //     let config = codespan_reporting::term::Config::default();

    //     match err {
    //         ParseError::InvalidToken { location } => {
    //             println!("invalid token loc:{}", location)
    //         },
    //         // ParseError::UnrecognizedEof { location , expected } => {
    //         //     println!("unrecognized EOF {}, expected:", location);
    //         //     // for tok in expected {
    //         //     //     println!("expected: {}", tok)
    //         //     // }
    //         // },
    //         ParseError::UnrecognizedToken { token, expected } => {
    //             // for tok in expected {
    //             //     println!("{}", tok)
    //             // }
    //             let diagonistic = Diagnostic::error()
    //                 .with_message("failed to parse")
    //                 .with_code("E001")
    //                 .with_labels(vec![
    //                     Label::primary(file_id, (token.0)
    //                     Label::secondary(file_id, (0)..(token.2+100)),
    //                 ])
    //                 .with_notes(vec![expected[0].clone(), unindent(
    //                     "
    //                         expected type \"=\"
    //                     "
    //                 )]);
    //             codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagonistic).expect("fail");
    //         }
    //         _ => { println!("unhandled") }
    //     }

    // }
    //return def.expect("failed to parse");
}
