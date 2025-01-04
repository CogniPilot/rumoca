use crate::s0_lexer::lexer::Lexer;
use crate::s0_lexer::tokens::{LexicalError, Token};
use crate::s1_parser::ast;
use std::process;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use crate::s1_parser::modelica::StoredDefinitionParser;
use lalrpop_util::ParseError;

pub fn parse_file(
    filename: &str,
) -> Result<ast::StoredDefinition, ParseError<usize, Token, LexicalError>> {
    let mut files = SimpleFiles::new();
    let file_id = files.add(
        filename,
        std::fs::read_to_string(filename).expect("failed to read file"),
    );
    let file = files.get(file_id).expect("failed to get file id");
    let lexer = Lexer::new(file.source());
    let parser = StoredDefinitionParser::new();
    let def = parser.parse(lexer);
    if def.is_err() {
        let err = def.as_ref().expect_err("error");
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        match err {
            ParseError::User { error } => match error {
                LexicalError::InvalidInteger(e) => {
                    println!("lexer invalid integer:{:?}", e);
                }
                LexicalError::InvalidToken => {
                    println!("lexer invalid token {:?}", error);
                }
            },
            ParseError::InvalidToken { location } => {
                println!("invalid token loc:{:?}", location);
            }
            ParseError::ExtraToken { token } => {
                println!("extra token: {:?}", token);
            }
            ParseError::UnrecognizedEof { location, expected } => {
                println!("unrecognized Eof loc: {:?}, expected:", location);
                for tok in expected {
                    println!("{:?}", tok)
                }
            }
            ParseError::UnrecognizedToken { token, expected } => {
                let diagonistic = Diagnostic::error()
                    .with_message("Unrecognized Token")
                    .with_code("E001")
                    .with_labels(vec![
                        Label::primary(file_id, (token.0)..(token.2)),
                        Label::secondary(file_id, 0..(token.2)),
                    ])
                    .with_notes(vec!["expected one of: ".to_string(), expected.join(", ")]);
                codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagonistic)
                    .expect("fail");
            }
        }
        // kill process to avoid panicing when parse fails, codespan already reports error
        process::exit(1);
    }
    def
}
