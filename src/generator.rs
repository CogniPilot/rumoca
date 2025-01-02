use crate::ast;
use crate::flat_ast;
use crate::lexer;
use crate::parser;
use crate::tokens::{LexicalError, Token};
use std::collections::HashMap;
use std::collections::HashSet;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use lalrpop_util::ParseError;
use lexer::Lexer;
use minijinja::{context, Environment};
use parser::StoredDefinitionParser;

pub fn parse_file(
    filename: &str,
) -> Result<ast::StoredDefinition, ParseError<usize, Token, LexicalError>> {
    let mut files = SimpleFiles::new();
    let file_id = files.add(filename, std::fs::read_to_string(filename).unwrap());
    let file = files.get(file_id).expect("failed to get file");
    let lexer = Lexer::new(file.source());
    let parser = StoredDefinitionParser::new();
    let def = parser.parse(lexer);
    if def.is_err() {
        // let type_id = def.as_ref().unwrap().type_id();
        // match type_id {
        //     UnrecognizedToken => println!("unrecognized token!"),
        //     _ => println!("type unhandled {:?}", type_id.)
        // }
        let err = def.as_ref().expect_err("error");

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        match err {
            ParseError::User { error } => match error {
                LexicalError::InvalidInteger(e) => {
                    println!("lexer invalid integer:{}", e);
                }
                LexicalError::InvalidToken => {
                    println!("lexer invalid token {:?}", error);
                }
            },
            ParseError::InvalidToken { location } => {
                println!("invalid token loc:{}", location);
            }
            ParseError::ExtraToken { token } => {
                println!("extra token: {:?}", token);
            }
            ParseError::UnrecognizedEof { location, expected } => {
                println!("unrecognized Eof loc: {}, expected:", location);
                for tok in expected {
                    println!("{}", tok)
                }
            }
            ParseError::UnrecognizedToken { token, expected } => {
                println!("unrecognized token {:?}, expected:", token);
                for tok in expected {
                    println!("expected: {}", tok)
                }
                let diagonistic = Diagnostic::error()
                    .with_message("failed to parse")
                    .with_code("E001")
                    .with_labels(vec![
                        Label::primary(file_id, (token.0)..(token.2)),
                        Label::secondary(file_id, (0)..(token.2)),
                    ])
                    .with_notes(vec![
                        expected[0].clone(),
                        unindent::unindent(
                            "
                            expected type \"=\"
                        ",
                        ),
                    ]);
                codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagonistic)
                    .expect("fail");
            }
        }
    }
    def
}

pub fn panic(msg: &str) {
    panic!("{:?}", msg);
}

pub fn generate(
    classes: &Vec<flat_ast::Class>,
    template_file: &str,
) -> Result<String, Box<dyn std::error::Error>> {
    let template_text = std::fs::read_to_string(template_file)?;
    let mut env = Environment::new();
    env.add_function("panic", panic);
    env.add_template("template", &template_text)?;
    let tmpl = env.get_template("template").unwrap();
    let txt = tmpl.render(context!(classes => classes)).unwrap();
    Ok(txt)
}

pub fn flatten(
    def: &ast::StoredDefinition,
) -> Result<Vec<flat_ast::Class>, Box<dyn std::error::Error>> {
    let mut class_order = Vec::new();
    let mut classes = HashMap::new();

    for class in &def.classes {
        let mut fclass: flat_ast::Class = Default::default();
        let mut states = HashSet::new();

        fclass.name = class.name.clone();
        fclass.class_type = class.class_type.clone();
        fclass.description = class.description.clone();

        // find all states in the class by searching
        // for component references that are taken the derivative of
        for eq in &class.equations {
            if let ast::Equation::Der { comp, rhs } = eq {
                states.insert(comp.name.clone());
                fclass.ode.push(*rhs.clone());
            }
        }

        // algorithms
        fclass.alg = class.algorithms.clone();

        // create component vectors
        for comp in &class.components {
            let flat_comp = flat_ast::Component {
                name: comp.name.clone(),
                start: comp.modification.expression.clone(),
                array_subscripts: comp.array_subscripts.clone(),
            };
            match comp.variability {
                ast::Variability::Constant => {
                    fclass.c.push(flat_comp);
                }

                ast::Variability::Continuous => {
                    if states.contains(&comp.name) {
                        fclass.x.push(flat_comp);
                    } else if comp.causality == ast::Causality::Input {
                        fclass.u.push(flat_comp);
                    } else {
                        fclass.y.push(flat_comp);
                    }
                }
                ast::Variability::Discrete => {
                    fclass.z.push(flat_comp);
                }
                ast::Variability::Parameter => {
                    fclass.p.push(flat_comp);
                }
            }
        }
        classes.insert(fclass.name.to_string(), fclass.clone());
        class_order.push(fclass.name.to_string());
    }

    Ok(class_order
        .iter()
        .map(|name| classes[name].clone())
        .collect::<Vec<flat_ast::Class>>())
}

// pub fn generate_json(def: &ast::StoredDefinition) -> Result<String, std::io::Error> {
//     let s = serde_json::to_string_pretty(def)?;
//     Ok(s)
// }
