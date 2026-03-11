//! Modelica parser for the Rumoca compiler.
//!
//! This crate provides parsing of Modelica source code using a parol-generated
//! LL(k) parser. The parser produces an AST that represents the Modelica source.

mod components;
mod definitions;
mod elements;
mod equations;
pub mod errors;
mod expressions;
pub mod generated;
mod grammar;
mod helpers;
mod recovery;
mod references;
mod sections;

use generated::modelica_grammar_trait;
use parol_runtime::{Result, Token};
use rumoca_core::{BytePos, Span};
use std::collections::HashSet;
use std::fmt::{Display, Error, Formatter};
use std::ops::Deref;
use std::sync::Arc;

pub use errors::{ParseError, convert_parol_error, format_parse_error};
pub use recovery::parse_to_recovered_ast;

// Re-export at crate root for parol-generated code expectations
pub use generated::modelica_grammar_trait as grammar_trait;

// Re-export for convenience
pub use generated::modelica_parser;

// Re-export types used by modelica_grammar_trait (generated code references these)
pub use components::{ComponentList, TokenList};
pub use definitions::{Composition, ElementList};
pub use expressions::{ArraySubscripts, ExpressionList, ModificationArg};
pub use sections::{AlgorithmSection, EquationSection};

/// A parsed comment with its location information
#[derive(Debug, Clone, Default)]
pub struct ParsedComment {
    /// The comment text (including // or /* */)
    pub text: String,
    /// Line number (1-based)
    pub line: u32,
    /// Column number (1-based)
    pub column: u32,
    /// Whether this is a line comment (//) or block comment (/* */)
    pub is_line_comment: bool,
}

/// Parser-local terminal token type used by the generated grammar actions.
///
/// This keeps Parol coupling in the parser crate while the canonical token
/// definition lives in `rumoca-ir-core` (re-exported by `rumoca-ir-ast`).
#[derive(Debug, Clone, Default, PartialEq)]
pub struct ParserToken(pub rumoca_ir_core::Token);

impl Deref for ParserToken {
    type Target = rumoca_ir_core::Token;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<ParserToken> for rumoca_ir_core::Token {
    fn from(value: ParserToken) -> Self {
        value.0
    }
}

impl From<&ParserToken> for rumoca_ir_core::Token {
    fn from(value: &ParserToken) -> Self {
        value.0.clone()
    }
}

impl TryFrom<&parol_runtime::Token<'_>> for ParserToken {
    type Error = anyhow::Error;

    fn try_from(value: &parol_runtime::Token<'_>) -> std::result::Result<Self, Self::Error> {
        Ok(Self(rumoca_ir_core::Token {
            text: Arc::from(value.text()),
            location: rumoca_ir_core::Location {
                start_line: value.location.start_line,
                start_column: value.location.start_column,
                end_line: value.location.end_line,
                end_column: value.location.end_column,
                start: value.location.start,
                end: value.location.end,
                // Preserve full source path so cross-file features (goto definition,
                // diagnostics attribution) can locate the real file, not only basename.
                file_name: value.location.file_name.to_string_lossy().to_string(),
            },
            token_number: value.token_number,
            token_type: value.token_type,
        }))
    }
}

#[derive(Debug, Default)]
pub struct ModelicaGrammar<'t> {
    pub modelica: Option<rumoca_ir_ast::StoredDefinition>,
    /// Comments collected during parsing, in order of appearance
    pub comments: Vec<ParsedComment>,
    _phantom: std::marker::PhantomData<&'t str>,
}

impl ModelicaGrammar<'_> {
    pub fn new() -> Self {
        ModelicaGrammar::default()
    }
}

impl Display for modelica_grammar_trait::StoredDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), Error> {
        write!(f, "{:?}", self)
    }
}

impl Display for ModelicaGrammar<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), Error> {
        match &self.modelica {
            Some(modelica) => writeln!(f, "{:#?}", modelica),
            None => write!(f, "No parse result"),
        }
    }
}

impl<'t> modelica_grammar_trait::ModelicaGrammarTrait for ModelicaGrammar<'t> {
    fn stored_definition(&mut self, arg: &modelica_grammar_trait::StoredDefinition) -> Result<()> {
        self.modelica = Some(arg.try_into()?);
        Ok(())
    }

    /// Collect comments during parsing for later use (e.g., in formatter)
    fn on_comment(&mut self, token: Token<'_>) {
        let text = token.text().to_string();
        let is_line_comment = text.starts_with("//");

        self.comments.push(ParsedComment {
            text,
            line: token.location.start_line,
            column: token.location.start_column,
            is_line_comment,
        });
    }
}

use std::path::Path;

/// Parse a Modelica source file.
pub fn parse_file(path: &Path) -> anyhow::Result<()> {
    let source = std::fs::read_to_string(path)?;
    parse_string(&source, path.to_string_lossy().as_ref())
}

/// Parse a Modelica source string.
///
/// Returns `Ok(())` if parsing succeeds.
pub fn parse_string(source: &str, file_name: &str) -> anyhow::Result<()> {
    parse_to_ast(source, file_name).map(|_| ())
}

/// Parse a Modelica source string and return the AST.
pub fn parse_to_ast(
    source: &str,
    file_name: &str,
) -> anyhow::Result<rumoca_ir_ast::StoredDefinition> {
    match parse_to_ast_with_errors(source, file_name) {
        Ok(ast) => Ok(ast),
        Err(parse_errors) => {
            // Format all errors with source context
            let formatted: Vec<String> = parse_errors
                .iter()
                .map(|e| format_parse_error(e, file_name, source))
                .collect();
            Err(anyhow::anyhow!("{}", formatted.join("\n\n")))
        }
    }
}

/// Parse a Modelica source string and return either AST or structured parse errors.
///
/// Unlike [`parse_to_ast`], this preserves structured error information (including
/// spans) so tooling can render precise diagnostics without parsing formatted text.
pub fn parse_to_ast_with_errors(
    source: &str,
    file_name: &str,
) -> std::result::Result<rumoca_ir_ast::StoredDefinition, Vec<ParseError>> {
    parse_to_ast_internal(source, file_name)
}

const MAX_SEMICOLON_RECOVERY_PASSES: usize = 32;

fn parse_to_ast_internal(
    source: &str,
    file_name: &str,
) -> std::result::Result<rumoca_ir_ast::StoredDefinition, Vec<ParseError>> {
    match parse_once_to_ast(source, file_name) {
        Ok(ast) => Ok(ast),
        Err(initial_errors) => Err(collect_recovered_parse_errors(
            source,
            file_name,
            initial_errors,
        )),
    }
}

fn parse_once_to_ast(
    source: &str,
    file_name: &str,
) -> std::result::Result<rumoca_ir_ast::StoredDefinition, Vec<ParseError>> {
    let mut grammar = ModelicaGrammar::new();
    if let Err(parol_err) = generated::modelica_parser::parse(source, file_name, &mut grammar) {
        return Err(convert_parol_error(parol_err, source));
    }
    match grammar.modelica {
        Some(ast) => Ok(ast),
        None => Err(vec![ParseError::NoAstProduced]),
    }
}

fn collect_recovered_parse_errors(
    source: &str,
    file_name: &str,
    initial_errors: Vec<ParseError>,
) -> Vec<ParseError> {
    let mut all_errors = Vec::new();
    let mut seen = HashSet::new();

    let mut current_errors = initial_errors;
    let mut patched_source = source.to_string();
    let mut inserted_positions: Vec<usize> = Vec::new();

    for _ in 0..MAX_SEMICOLON_RECOVERY_PASSES {
        append_unique_mapped_errors(
            &mut all_errors,
            &mut seen,
            &current_errors,
            &inserted_positions,
        );

        let Some(insert_pos) =
            next_missing_semicolon_recovery_pos(&current_errors, &inserted_positions)
        else {
            break;
        };

        if !insert_semicolon(&mut patched_source, &mut inserted_positions, insert_pos) {
            break;
        }

        match parse_once_to_ast(&patched_source, file_name) {
            Ok(_) => break,
            Err(next_errors) => current_errors = next_errors,
        }
    }

    all_errors
}

fn append_unique_mapped_errors(
    all_errors: &mut Vec<ParseError>,
    seen: &mut HashSet<String>,
    errors: &[ParseError],
    inserted_positions: &[usize],
) {
    for error in errors {
        let mapped = map_parse_error_to_original(error, inserted_positions);
        let key = parse_error_key(&mapped);
        if seen.insert(key) {
            all_errors.push(mapped);
        }
    }
}

fn map_parse_error_to_original(error: &ParseError, inserted_positions: &[usize]) -> ParseError {
    match error {
        ParseError::SyntaxError {
            message,
            expected,
            unexpected,
            span,
        } => ParseError::SyntaxError {
            message: message.clone(),
            expected: expected.clone(),
            unexpected: unexpected.clone(),
            span: map_span_to_original(*span, inserted_positions),
        },
        ParseError::NoAstProduced => ParseError::NoAstProduced,
        ParseError::IoError { path, message } => ParseError::IoError {
            path: path.clone(),
            message: message.clone(),
        },
    }
}

fn map_span_to_original(span: Span, inserted_positions: &[usize]) -> Span {
    let start = map_pos_to_original(span.start.0, inserted_positions);
    let end = map_pos_to_original(span.end.0, inserted_positions);
    Span::new(span.source, BytePos(start), BytePos(end))
}

fn map_pos_to_original(pos: usize, inserted_positions: &[usize]) -> usize {
    let inserted_before = inserted_positions.iter().filter(|&&p| p < pos).count();
    pos.saturating_sub(inserted_before)
}

fn next_missing_semicolon_recovery_pos(
    errors: &[ParseError],
    inserted_positions: &[usize],
) -> Option<usize> {
    errors.iter().find_map(|error| {
        let pos = semicolon_insertion_pos(error)?;
        if inserted_positions.contains(&pos) {
            None
        } else {
            Some(pos)
        }
    })
}

fn semicolon_insertion_pos(error: &ParseError) -> Option<usize> {
    let ParseError::SyntaxError {
        expected,
        unexpected,
        span,
        ..
    } = error
    else {
        return None;
    };

    if !expected.iter().any(|e| e == ";") {
        return None;
    }

    let unexpected_lower = unexpected.as_ref().map(|u| u.to_ascii_lowercase());
    let insert_before_unexpected = unexpected_lower.as_deref().is_some_and(|u| {
        matches!(
            u,
            "equation"
                | "algorithm"
                | "public"
                | "protected"
                | "end"
                | "else"
                | "elseif"
                | "elsewhen"
                | "when"
                | "for"
                | "while"
                | "annotation"
                | "external"
        )
    });

    if insert_before_unexpected {
        Some(span.start.0)
    } else {
        Some(span.end.0)
    }
}

fn insert_semicolon(
    source: &mut String,
    inserted_positions: &mut Vec<usize>,
    insert_pos: usize,
) -> bool {
    if insert_pos > source.len() {
        return false;
    }
    if source
        .as_bytes()
        .get(insert_pos)
        .is_some_and(|&byte| byte == b';')
    {
        return false;
    }

    source.insert(insert_pos, ';');

    for pos in inserted_positions.iter_mut() {
        if *pos >= insert_pos {
            *pos += 1;
        }
    }
    let idx = inserted_positions
        .iter()
        .position(|&pos| pos > insert_pos)
        .unwrap_or(inserted_positions.len());
    inserted_positions.insert(idx, insert_pos);
    true
}

fn parse_error_key(error: &ParseError) -> String {
    match error {
        ParseError::SyntaxError {
            message,
            expected,
            unexpected,
            span,
        } => format!(
            "syntax:{}:{}:{}:{:?}:{:?}",
            span.start.0, span.end.0, message, expected, unexpected
        ),
        ParseError::NoAstProduced => "no-ast".to_string(),
        ParseError::IoError { path, message } => format!("io:{}:{}", path, message),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_ast as ast;
    use rumoca_ir_ast::{Import, TerminalType};

    fn round_trip_ast(source: &str) -> rumoca_ir_ast::StoredDefinition {
        let ast = parse_to_ast(source, "test.mo").expect("initial parse should succeed");
        let rendered = ast.to_modelica();
        parse_to_ast(&rendered, "roundtrip.mo").expect("round-trip parse should succeed")
    }

    #[test]
    fn test_parse_extends() {
        let source = r#"
model Base
    Real x = 1;
end Base;

model Derived
    extends Base;
    Real y = 2;
end Derived;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("Parse should succeed");

        assert_eq!(ast.classes.len(), 2, "Expected 2 definitions");

        // Check the Derived model has an extends element
        let derived = ast.classes.get("Derived").expect("Derived should exist");
        assert_eq!(&*derived.name.text, "Derived");

        assert!(!derived.extends.is_empty(), "Derived should have extends");
        assert_eq!(
            derived.extends[0].base_name.to_string(),
            "Base",
            "Derived should extend Base"
        );
    }

    #[test]
    fn test_parse_import() {
        let source = r#"
model Test
    import MyPackage.SomeClass;
    Real x;
end Test;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("Parse should succeed");

        assert_eq!(ast.classes.len(), 1);
        let model = ast.classes.get("Test").expect("Test should exist");

        assert!(!model.imports.is_empty(), "Model should have imports");
        if let Import::Qualified { path, location, .. } = &model.imports[0] {
            assert_eq!(path.to_string(), "MyPackage.SomeClass");
            // Import span should cover the full qualified path, not just `import`.
            assert!(location.end > location.start);
            assert!(
                location.end_column > location.start_column + 6,
                "import span should extend beyond keyword: {:?}",
                location
            );
        }
    }

    #[test]
    fn test_parse_reinit() {
        let source = r#"
model Test
    Real v;
equation
    when v <= 0 then
        reinit(v, 1.0);
    end when;
end Test;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("Parse should succeed");

        let model = ast.classes.get("Test").expect("Test should exist");

        let mut has_when = false;
        for eq in &model.equations {
            if matches!(eq, ast::Equation::When { .. }) {
                has_when = true;
            }
        }

        assert!(has_when, "Should have when equation");
    }

    #[test]
    fn test_parse_empty_model() {
        let source = r#"model Empty end Empty;"#;
        let result = parse_string(source, "test.mo");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_simple_model() {
        let source = r#"
model Ball
  Real x;
  Real v;
equation
  der(x) = v;
  der(v) = -9.81;
end Ball;
"#;
        let result = parse_string(source, "test.mo");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_to_ast_simple() {
        let source = r#"model Test end Test;"#;
        let ast = parse_to_ast(source, "test.mo");
        assert!(ast.is_ok());
        let ast = ast.unwrap();
        assert_eq!(ast.classes.len(), 1);
        assert!(ast.classes.contains_key("Test"));
    }

    #[test]
    fn test_parse_simple_equation() {
        let source = r#"model Test equation x = y; end Test;"#;
        let ast = parse_to_ast(source, "test.mo").expect("Parse should succeed");

        let model = ast.classes.get("Test").expect("Test should exist");
        assert_eq!(&*model.name.text, "Test");
        assert!(!model.equations.is_empty(), "Should have equations");
    }

    #[test]
    fn test_parse_component_declarations() {
        let source = r#"
model Test
  Real h;
  Real v;
  parameter Real g;
end Test;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("Parse should succeed");

        let model = ast.classes.get("Test").expect("Test should exist");
        assert_eq!(&*model.name.text, "Test");
        assert_eq!(model.components.len(), 3, "Should have 3 components");
    }

    #[test]
    fn test_parse_replaceable_component_preserves_array_shape() {
        let source = r#"
model Base
  replaceable Real cell[3, 2];
end Base;
"#;

        let ast = parse_to_ast(source, "test.mo").expect("Parse should succeed");
        let base = ast.classes.get("Base").expect("Base should exist");
        let cell = base.components.get("cell").expect("cell should exist");

        assert!(cell.is_replaceable);
        assert_eq!(cell.shape, vec![3, 2]);
        assert_eq!(cell.shape_expr.len(), 2);
    }

    #[test]
    fn test_parse_der_class_specifier_short_form() {
        let source = r#"
function f
  input Real x;
  output Real y;
algorithm
  y := x;
end f;

function f_der = der(f, x);
"#;

        let ast = parse_to_ast(source, "test.mo").expect("Parse should succeed");
        let f_der = ast.classes.get("f_der").expect("f_der should exist");
        assert_eq!(&*f_der.name.text, "f_der");
        assert_eq!(f_der.extends.len(), 1, "expected one extends entry");
        assert_eq!(
            f_der.extends[0].base_name.to_string(),
            "f",
            "der short form should reference base function"
        );
    }

    #[test]
    fn test_roundtrip_der_class_specifier_short_form() {
        let source = r#"
function f
  input Real x;
  output Real y;
algorithm
  y := x;
end f;

function f_der = der(f, x);
"#;

        let reparsed = round_trip_ast(source);
        let f_der = reparsed.classes.get("f_der").expect("f_der should exist");
        assert_eq!(&*f_der.name.text, "f_der");
        assert_eq!(f_der.extends.len(), 1, "expected one extends entry");
        assert_eq!(f_der.extends[0].base_name.to_string(), "f");
    }

    #[test]
    fn test_parse_output_primary_array_subscript_postfix() {
        let source = r#"
model Test
  Real x;
equation
  x = (fill(1.0, 3))[2];
end Test;
"#;

        let ast = parse_to_ast(source, "test.mo").expect("Parse should succeed");
        let model = ast.classes.get("Test").expect("Test should exist");
        let eq = model.equations.first().expect("expected one equation");
        let rhs = match eq {
            ast::Equation::Simple { rhs, .. } => rhs,
            _ => panic!("expected simple equation"),
        };

        match rhs {
            ast::Expression::ArrayIndex { base, subscripts } => {
                assert_eq!(subscripts.len(), 1, "expected one subscript");
                assert!(
                    matches!(&**base, ast::Expression::Parenthesized { .. }),
                    "expected parenthesized base expression, got: {:?}",
                    base
                );
            }
            other => panic!("expected ArrayIndex RHS, got: {:?}", other),
        }
    }

    #[test]
    fn test_roundtrip_output_primary_array_subscript_postfix() {
        let source = r#"
model Test
  Real x;
equation
  x = (fill(1.0, 3))[2];
end Test;
"#;

        let reparsed = round_trip_ast(source);
        let model = reparsed.classes.get("Test").expect("Test should exist");
        let eq = model.equations.first().expect("expected one equation");
        let rhs = match eq {
            ast::Equation::Simple { rhs, .. } => rhs,
            _ => panic!("expected simple equation"),
        };
        assert!(
            matches!(rhs, ast::Expression::ArrayIndex { .. }),
            "expected round-trip array index rhs, got: {:?}",
            rhs
        );
    }

    #[test]
    fn test_parse_output_primary_dot_ident_postfix() {
        let source = r#"
model Test
  Real x;
equation
  x = (Complex(1, 2)).re;
end Test;
"#;

        let ast = parse_to_ast(source, "test.mo").expect("Parse should succeed");
        let model = ast.classes.get("Test").expect("Test should exist");
        let eq = model.equations.first().expect("expected one equation");
        let rhs = match eq {
            ast::Equation::Simple { rhs, .. } => rhs,
            _ => panic!("expected simple equation"),
        };

        match rhs {
            ast::Expression::FieldAccess { base, field } => {
                assert_eq!(field, "re");
                assert!(
                    matches!(&**base, ast::Expression::Parenthesized { .. }),
                    "expected parenthesized base expression, got: {:?}",
                    base
                );
            }
            other => panic!("expected FieldAccess RHS, got: {:?}", other),
        }
    }

    #[test]
    fn test_roundtrip_output_primary_dot_ident_postfix() {
        let source = r#"
record R
  Real re;
end R;

model Test
  R r;
  Real x;
equation
  x = (r).re;
end Test;
"#;

        let reparsed = round_trip_ast(source);
        let model = reparsed.classes.get("Test").expect("Test should exist");
        let eq = model.equations.first().expect("expected one equation");
        let rhs = match eq {
            ast::Equation::Simple { rhs, .. } => rhs,
            _ => panic!("expected simple equation"),
        };
        assert!(
            matches!(rhs, ast::Expression::FieldAccess { .. }),
            "expected round-trip field access rhs, got: {:?}",
            rhs
        );
    }

    #[test]
    fn test_parse_class_modification_replaceable_argument() {
        let source = r#"
model DefaultVariant
  Real x;
end DefaultVariant;

model Base
  replaceable model Variant = DefaultVariant;
end Base;

model Test
  Base base(replaceable model Variant = DefaultVariant);
end Test;
"#;

        let ast = parse_to_ast(source, "test.mo").expect("Parse should succeed");
        let model = ast.classes.get("Test").expect("Test should exist");
        let base = model.components.get("base").expect("base should exist");
        let variant_mod = base
            .modifications
            .get("Variant")
            .expect("replaceable class argument should be preserved as component modification");

        assert!(
            matches!(
                variant_mod,
                ast::Expression::ClassModification { .. }
                    | ast::Expression::Modification { .. }
                    | ast::Expression::Binary { .. }
            ),
            "expected replaceable class argument expression, got: {:?}",
            variant_mod
        );
    }

    #[test]
    fn test_roundtrip_class_modification_replaceable_argument() {
        let source = r#"
model DefaultVariant
  Real x;
end DefaultVariant;

model Base
  replaceable model Variant = DefaultVariant;
end Base;

model Test
  Base base(replaceable model Variant = DefaultVariant);
end Test;
"#;

        let reparsed = round_trip_ast(source);
        let model = reparsed.classes.get("Test").expect("Test should exist");
        let base = model.components.get("base").expect("base should exist");
        assert!(
            base.modifications.contains_key("Variant"),
            "round-trip should preserve Variant modification key, got {:?}",
            base.modifications.keys().collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_boolean_component_default_start_is_false() {
        let source = r#"
model Test
  Boolean flag;
end Test;
"#;

        let ast = parse_to_ast(source, "test.mo").expect("Parse should succeed");
        let model = ast.classes.get("Test").expect("Test should exist");
        let flag = model.components.get("flag").expect("flag should exist");

        match &flag.start {
            ast::Expression::Terminal {
                terminal_type,
                token,
            } => {
                assert_eq!(*terminal_type, TerminalType::Bool);
                assert_eq!(&*token.text, "false");
            }
            other => panic!("expected Boolean default terminal start, got: {:?}", other),
        }
    }

    #[test]
    fn test_parse_der_equation() {
        let source = r#"
model Test
  Real v;
equation
  der(v) = -9.81;
end Test;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("Parse should succeed");

        let model = ast.classes.get("Test").expect("Test should exist");
        assert!(!model.equations.is_empty(), "Should have equations");
    }

    #[test]
    fn test_parse_bouncing_ball_ast() {
        let source = r#"
model BouncingBall "A bouncing ball model"
  Real h(start = 1) "height above ground";
  Real v(start = 0) "velocity";
  parameter Real g = 9.81 "gravitational acceleration";
  parameter Real e = 0.8 "coefficient of restitution";
equation
  der(h) = v;
  der(v) = -g;
  when h <= 0 then
    reinit(v, -e * pre(v));
  end when;
end BouncingBall;
"#;
        let result = parse_to_ast(source, "bouncing_ball.mo");
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        let ast = result.unwrap();

        // Verify we have one definition
        assert_eq!(ast.classes.len(), 1, "Expected 1 definition");

        let model = ast
            .classes
            .get("BouncingBall")
            .expect("BouncingBall should exist");
        assert_eq!(&*model.name.text, "BouncingBall");

        // Verify component count
        assert_eq!(
            model.components.len(),
            4,
            "Expected 4 components (h, v, g, e)"
        );

        // Verify equation count (2 simple + 1 when)
        assert_eq!(
            model.equations.len(),
            3,
            "Expected 3 equations (2 der() + 1 when)"
        );

        // Count equation types
        let mut simple_count = 0;
        let mut when_count = 0;
        for eq in &model.equations {
            match eq {
                ast::Equation::Simple { .. } => simple_count += 1,
                ast::Equation::When { .. } => when_count += 1,
                _ => {}
            }
        }
        assert_eq!(simple_count, 2, "Expected 2 simple equations");
        assert_eq!(when_count, 1, "Expected 1 when equation");
    }

    #[test]
    fn test_case_sensitivity_keywords() {
        // MLS §2.3.3: Modelica is case-sensitive
        // "Outer" should be a valid identifier, not confused with keyword "outer"
        let source = r#"
model Outer
    Real x;
equation
    x = 1;
end Outer;
"#;
        let result = parse_to_ast(source, "test.mo");
        assert!(
            result.is_ok(),
            "Model named 'Outer' should parse - got: {:?}",
            result
        );

        let ast = result.unwrap();
        assert!(
            ast.classes.contains_key("Outer"),
            "Should have model named 'Outer'"
        );
    }

    #[test]
    fn test_case_sensitivity_inner_outer() {
        // Both "Inner" and "Outer" should be valid model names
        // Note: "inner" (lowercase) IS a keyword, so we can't use it as a variable name
        let source = r#"
model Inner
    Real x(start = 5);
equation
    der(x) = -x;
end Inner;

model Outer
    Inner sub;
end Outer;
"#;
        let result = parse_to_ast(source, "test.mo");
        assert!(
            result.is_ok(),
            "Models named 'Inner' and 'Outer' should parse - got: {:?}",
            result
        );

        let ast = result.unwrap();
        assert!(
            ast.classes.contains_key("Inner"),
            "Should have model named 'Inner'"
        );
        assert!(
            ast.classes.contains_key("Outer"),
            "Should have model named 'Outer'"
        );
    }

    #[test]
    fn test_nested_modification_parsing() {
        // Test how nested modifications like `sub(x(start = 10))` are parsed
        let source = r#"
model Inner
    Real x(start = 5);
equation
    der(x) = -x;
end Inner;

model Outer
    Inner sub(x(start = 10));
end Outer;
"#;
        let result = parse_to_ast(source, "test.mo");
        assert!(result.is_ok(), "Parse failed: {:?}", result);

        let ast = result.unwrap();
        let outer = ast.classes.get("Outer").expect("Outer should exist");
        let sub = outer.components.get("sub").expect("sub should exist");

        // Print modifications to understand the structure
        println!("sub.modifications = {:?}", sub.modifications);
        println!("sub.start = {:?}", sub.start);

        // The modification should be captured in modifications map
        // For `sub(x(start = 10))`, we expect modifications to have "x" key
        // or start to be stored somewhere
        println!("sub.annotation = {:?}", sub.annotation);
    }

    #[test]
    fn test_alias_component_start_is_parsed_as_start_value() {
        let source = r#"
type Voltage = Real;

model Test
    Voltage v(start = 1.25);
end Test;
"#;

        let ast = parse_to_ast(source, "test.mo").expect("Parse should succeed");
        let model = ast.classes.get("Test").expect("Test should exist");
        let v = model.components.get("v").expect("v should exist");

        match &v.start {
            ast::Expression::Terminal {
                terminal_type,
                token,
            } => {
                assert_eq!(*terminal_type, ast::TerminalType::UnsignedReal);
                assert_eq!(&*token.text, "1.25");
            }
            other => panic!(
                "expected start expression for alias component, got: {:?}",
                other
            ),
        }

        assert!(
            v.modifications.contains_key("start"),
            "Alias start modifier should still be preserved for non-builtins"
        );
    }

    #[test]
    fn test_inner_keyword_vs_identifier() {
        // "inner" (lowercase) is a keyword, but "Inner" (capital) should be a valid identifier
        let source = r#"
model Test
    inner Real x;
end Test;
"#;
        let result = parse_to_ast(source, "test.mo");
        assert!(
            result.is_ok(),
            "'inner Real x' should parse as inner-prefixed component - got: {:?}",
            result
        );
    }

    #[test]
    fn test_parse_reports_multiple_missing_semicolons() {
        let source = r#"
model Broken
  Real a
  Real b
equation
  a = 1
  b = 2
end Broken
"#;

        let result = parse_to_ast_internal(source, "test.mo");
        assert!(result.is_err(), "Expected parse failure");

        let errors = result.expect_err("parse should fail");
        assert!(
            errors.len() >= 3,
            "expected multiple syntax errors, got {}",
            errors.len()
        );
    }

    #[test]
    fn test_missing_semicolon_does_not_mislabel_reserved_keyword() {
        let source = r#"
model A
  Real a
equation
  a = 1;
end A;
"#;

        let err = parse_to_ast(source, "test.mo").expect_err("Expected parse failure");
        let msg = err.to_string();
        assert!(
            !msg.contains("`equation` is a reserved keyword"),
            "should report missing semicolon, not reserved keyword misuse:\n{}",
            msg
        );
    }

    #[test]
    fn test_missing_semicolon_before_end_has_non_dummy_span() {
        let source = r#"
model Ball
  Real x(start=0);
  Real v(start=1);
equation
  der(x) = v;
  der(v) = -9.81
end Ball;
"#;

        let errors =
            parse_to_ast_with_errors(source, "test.mo").expect_err("Expected parse failure");
        let end_error = errors
            .iter()
            .find_map(|error| {
                let ParseError::SyntaxError { message, span, .. } = error else {
                    return None;
                };
                (message.contains("`end`") || message.contains("'end'")).then_some(*span)
            })
            .expect("expected reserved/end parse error");
        assert!(
            end_error.start.0 > 0 || end_error.end.0 > 1,
            "expected non-dummy span for missing semicolon before `end`, got {:?}",
            end_error
        );
    }

    #[test]
    fn test_missing_semicolon_between_equations_before_der_has_non_dummy_span() {
        let source = r#"
model Ball
  Real x(start=0);
  Real v(start=1);
equation
  der(x) = v
  der(v) = -9.81;
end Ball;
"#;

        let errors =
            parse_to_ast_with_errors(source, "test.mo").expect_err("Expected parse failure");
        let der_error = errors
            .iter()
            .find_map(|error| {
                let ParseError::SyntaxError { message, span, .. } = error else {
                    return None;
                };
                (message.contains("`der`") || message.contains("'der'")).then_some(*span)
            })
            .expect("expected der-related parse error");
        assert!(
            der_error.start.0 > 1 || der_error.end.0 > 2,
            "expected non-origin span for missing semicolon before `der`, got {:?}",
            der_error
        );
    }

    #[test]
    fn test_duplicate_declaration_has_non_dummy_identifier_span() {
        let source = r#"
model Ball
  Real x(start=0);
  Real x;
  Real v(start=1);
equation
  der(x) = v;
  der(v) = -9.81;
end Ball;
"#;
        let errors =
            parse_to_ast_with_errors(source, "test.mo").expect_err("Expected parse failure");
        let duplicate_span = errors
            .iter()
            .find_map(|error| {
                let ParseError::SyntaxError { message, span, .. } = error else {
                    return None;
                };
                message
                    .contains("Duplicate declaration of 'x'")
                    .then_some(*span)
            })
            .expect("expected duplicate declaration error");
        assert!(
            duplicate_span.start.0 > 0 && duplicate_span.end.0 > duplicate_span.start.0,
            "expected non-dummy duplicate declaration span, got {:?}",
            duplicate_span
        );
        let highlighted = &source[duplicate_span.start.0..duplicate_span.end.0];
        assert_eq!(
            highlighted, "x",
            "expected duplicate identifier span to point at `x`, got {:?}",
            highlighted
        );
    }

    #[test]
    fn test_predefined_type_redeclaration_has_identifier_span() {
        let source = r#"
model Real
  Real x;
equation
  der(x) = 1;
end Real;
"#;

        let errors =
            parse_to_ast_with_errors(source, "test.mo").expect_err("Expected parse failure");
        let redeclare_span = errors
            .iter()
            .find_map(|error| {
                let ParseError::SyntaxError { message, span, .. } = error else {
                    return None;
                };
                message
                    .contains("Cannot redeclare predefined type 'Real'")
                    .then_some(*span)
            })
            .expect("expected predefined-type redeclaration error");
        assert!(
            redeclare_span.start.0 > 0 && redeclare_span.end.0 > redeclare_span.start.0,
            "expected non-dummy redeclaration span, got {:?}",
            redeclare_span
        );
        let highlighted = &source[redeclare_span.start.0..redeclare_span.end.0];
        assert_eq!(
            highlighted, "Real",
            "expected redeclared type span to point at class name, got {:?}",
            highlighted
        );
    }
}
