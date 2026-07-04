//! GALEC `.alg` printer (SPEC_0034 GAL-009/GAL-019).
//!
//! Conformance rules implemented here:
//!
//! - every cross-precedence-class operator mix is parenthesized and AST
//!   order is preserved — no re-association, even `a + b + c` (traps T6);
//! - unary minus is only ever printed over references; the AST makes other
//!   shapes unrepresentable (trap T4) and
//!   [`Expression::negated_real`](crate::ast::Expression::negated_real)
//!   rewrites negations as `0.0 - (expr)`;
//! - Real literals always carry decimal places and a signed exponent
//!   (trap T7), see [`format_real_literal`];
//! - if-expressions are self-parenthesized with a structurally mandatory
//!   `else`; `not` arguments are parenthesized (trap T12);
//! - only `/* */` comments exist in GALEC; this printer emits no comments;
//! - block layout uses lexically matching `end` names;
//! - multi-dimension constructors print nested `{…}` row-major.

use crate::ast::{
    Associativity, Block, BlockMethod, BlockMethodKind, Condition, Dimension, Expression, ForLoop,
    FunctionCall, Identifier, IfExpression, IfStatement, LimitTarget, Name, PrecedenceClass,
    RangeAttributes, RefPart, Reference, SignalCheck, Spanned, StateCompartment, Statement,
    TypeRef, UserFunction, VariableDeclaration,
};
use crate::diagnostic::{GalecError, Location, PathSegment};

/// Print a complete block as `.alg` text.
pub fn print_block(block: &Block) -> Result<String, GalecError> {
    Printer::default().block(block)
}

/// Print a single expression (top-level context). Useful for tests and for
/// embedding expression text in diagnostics.
pub fn print_expression(expression: &Expression) -> Result<String, GalecError> {
    expr(expression, &Location::detached())
}

/// Format a finite `f64` as a grammar-conformant GALEC Real literal:
/// decimal places mandatory, exponent (when used) lowercase `e` with a
/// mandatory sign (trap T7). Non-finite values have no GALEC spelling and
/// are rejected (`EG001`).
pub fn format_real_literal(value: f64) -> Result<String, GalecError> {
    format_real_at(value, &Location::detached())
}

/// Check a Real literal spelling against the GALEC lexeme grammar (G-1.13):
/// rejects `1e5` (no decimal places), `1.` / `.5` (incomplete decimal
/// places), `1.0e5` (unsigned exponent), `01.0` (leading zero), `1.0E+5`
/// (uppercase exponent).
#[must_use]
pub fn is_conformant_real_literal(text: &str) -> bool {
    parse_real_literal(text).is_some()
}

fn parse_real_literal(text: &str) -> Option<()> {
    let s = text.strip_prefix('-').unwrap_or(text);
    let s = strip_integer_places(s)?;
    let s = s.strip_prefix('.')?;
    let s = strip_digits(s)?;
    let s = match s.strip_prefix('e') {
        None => s,
        Some(rest) => {
            let rest = rest.strip_prefix('+').or_else(|| rest.strip_prefix('-'))?;
            strip_digits(rest)?
        }
    };
    s.is_empty().then_some(())
}

/// `integer` without sign: `"0"` or a nonzero digit followed by digits.
fn strip_integer_places(s: &str) -> Option<&str> {
    if let Some(rest) = s.strip_prefix('0') {
        return Some(rest);
    }
    strip_digits(s)
}

/// At least one ASCII digit.
fn strip_digits(s: &str) -> Option<&str> {
    let end = s.find(|c: char| !c.is_ascii_digit()).unwrap_or(s.len());
    (end > 0).then(|| &s[end..])
}

fn format_real_at(value: f64, location: &Location) -> Result<String, GalecError> {
    if !value.is_finite() {
        return Err(GalecError::NonFiniteRealLiteral {
            location: location.clone(),
            value,
        });
    }
    // `Display` for f64 is the shortest round-tripping decimal and never
    // uses exponent notation; fall back to normalized exponent form when
    // the plain spelling gets unwieldy.
    let plain = format!("{value}");
    if plain.len() <= 21 {
        return Ok(ensure_decimal_places(plain));
    }
    let scientific = format!("{value:e}");
    let (mantissa, exponent) = scientific
        .split_once('e')
        .expect("LowerExp for f64 always contains 'e'");
    let mantissa = ensure_decimal_places(mantissa.to_string());
    if exponent.starts_with('-') {
        Ok(format!("{mantissa}e{exponent}"))
    } else {
        Ok(format!("{mantissa}e+{exponent}"))
    }
}

fn ensure_decimal_places(mut s: String) -> String {
    if !s.contains('.') {
        s.push_str(".0");
    }
    s
}

// ---------------------------------------------------------------------------
// Names and references
// ---------------------------------------------------------------------------

/// Path-segment display text for a name (infallible; used to build
/// diagnostic locations, not `.alg` output).
fn display_name(name: &Name) -> String {
    match name {
        Name::Ident(id, _) => id.0.clone(),
        Name::Quoted(content, _) => format!("'{content}'"),
    }
}

fn print_name(name: &Name, location: &Location) -> Result<String, GalecError> {
    match name {
        Name::Ident(id, _) => print_identifier(id, location),
        Name::Quoted(content, _) => print_quoted(content, location),
    }
}

fn print_identifier(id: &Identifier, location: &Location) -> Result<String, GalecError> {
    if id.0.is_empty() {
        return Err(GalecError::EmptyIdentifier {
            location: location.clone(),
        });
    }
    Ok(id.0.clone())
}

fn print_quoted(content: &str, location: &Location) -> Result<String, GalecError> {
    let reason = if content.is_empty() {
        Some("content is empty")
    } else if content.contains('\'') {
        Some("content contains a quote character")
    } else if content.chars().any(|c| c.is_whitespace() || c.is_control()) {
        Some("content contains whitespace or control characters")
    } else {
        None
    };
    if let Some(reason) = reason {
        return Err(GalecError::MalformedQuotedIdentifier {
            location: location.clone(),
            content: content.to_string(),
            reason,
        });
    }
    Ok(format!("'{content}'"))
}

fn print_reference(reference: &Reference, location: &Location) -> Result<String, GalecError> {
    match reference {
        Reference::Local(part) => print_ref_part(part, location),
        Reference::State(parts) => {
            if parts.is_empty() {
                // A state reference needs at least one part after `self.`.
                return Err(GalecError::EmptyIdentifier {
                    location: location.clone(),
                });
            }
            let mut out = String::from("self");
            for part in parts {
                out.push('.');
                out.push_str(&print_ref_part(part, location)?);
            }
            Ok(out)
        }
    }
}

fn print_ref_part(part: &RefPart, location: &Location) -> Result<String, GalecError> {
    let mut out = print_name(&part.name, location)?;
    if !part.subscripts.is_empty() {
        out.push('[');
        out.push_str(&expr_list(&part.subscripts, location)?);
        out.push(']');
    }
    Ok(out)
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

fn expr(expression: &Expression, location: &Location) -> Result<String, GalecError> {
    match expression {
        Expression::Bool(value) => Ok(if *value { "true" } else { "false" }.to_string()),
        Expression::Integer(value) => Ok(value.to_string()),
        Expression::Real(value) => format_real_at(*value, location),
        Expression::Ref(reference) => print_reference(reference, location),
        Expression::Size { array, dimension } => Ok(format!(
            "size({}, {})",
            print_reference(array, location)?,
            expr(dimension, location)?
        )),
        Expression::Call(call) => print_call(call, location),
        Expression::Paren(inner) => Ok(format!("({})", expr(inner, location)?)),
        Expression::If(if_expression) => print_if_expression(if_expression, location),
        Expression::Array(elements) => print_array(elements, location),
        Expression::Neg(reference) => Ok(format!("-{}", print_reference(reference, location)?)),
        Expression::Not(inner) => print_not(inner, location),
        Expression::Binary { op, lhs, rhs } => {
            let class = op.precedence_class();
            let left = operand(lhs, class, OperandSide::Left, location)?;
            let right = operand(rhs, class, OperandSide::Right, location)?;
            Ok(format!("{left} {} {right}", op.token()))
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum OperandSide {
    Left,
    Right,
}

fn operand(
    child: &Expression,
    parent_class: PrecedenceClass,
    side: OperandSide,
    location: &Location,
) -> Result<String, GalecError> {
    let text = expr(child, location)?;
    if operand_needs_parens(child, parent_class, side) {
        Ok(format!("({text})"))
    } else {
        Ok(text)
    }
}

/// GAL-019 parenthesization: cross-class mixes always get parentheses
/// (trap T6); same-class chains print bare only on the associative side so
/// the AST shape (normative evaluation order) is preserved. Unary
/// operations and negative literals are parenthesized as operands for
/// lexical safety.
fn operand_needs_parens(
    child: &Expression,
    parent_class: PrecedenceClass,
    side: OperandSide,
) -> bool {
    match child {
        Expression::Binary { op, .. } => {
            let child_class = op.precedence_class();
            if child_class != parent_class {
                return true;
            }
            let associative_side = match parent_class.associativity() {
                Associativity::Left => OperandSide::Left,
                Associativity::Right => OperandSide::Right,
            };
            side != associative_side
        }
        Expression::Neg(_) | Expression::Not(_) => true,
        Expression::Integer(value) => *value < 0,
        Expression::Real(value) => value.is_sign_negative(),
        _ => false,
    }
}

fn print_not(inner: &Expression, location: &Location) -> Result<String, GalecError> {
    let text = expr(inner, location)?;
    match inner {
        // Already self-delimiting per the `not` grammar (G-3.5).
        Expression::If(_) | Expression::Paren(_) => Ok(format!("not {text}")),
        _ => Ok(format!("not ({text})")),
    }
}

fn print_if_expression(
    if_expression: &IfExpression,
    location: &Location,
) -> Result<String, GalecError> {
    if if_expression.branches.is_empty() {
        return Err(GalecError::IfExpressionWithoutBranches {
            location: location.clone(),
        });
    }
    let mut out = String::from("(");
    for (index, (condition, value)) in if_expression.branches.iter().enumerate() {
        let keyword = if index == 0 { "if" } else { " elseif" };
        out.push_str(keyword);
        out.push(' ');
        out.push_str(&expr(condition, location)?);
        out.push_str(" then ");
        out.push_str(&expr(value, location)?);
    }
    out.push_str(" else ");
    out.push_str(&expr(&if_expression.else_value, location)?);
    out.push(')');
    Ok(out)
}

fn print_array(elements: &[Expression], location: &Location) -> Result<String, GalecError> {
    if elements.is_empty() {
        return Err(GalecError::EmptyArrayConstructor {
            location: location.clone(),
        });
    }
    Ok(format!("{{{}}}", expr_list(elements, location)?))
}

fn print_call(call: &FunctionCall, location: &Location) -> Result<String, GalecError> {
    Ok(format!(
        "{}({})",
        print_name(&call.function, location)?,
        expr_list(&call.arguments, location)?
    ))
}

fn expr_list(expressions: &[Expression], location: &Location) -> Result<String, GalecError> {
    let parts = expressions
        .iter()
        .map(|e| expr(e, location))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(parts.join(", "))
}

fn identifier_list(ids: &[Identifier], location: &Location) -> Result<String, GalecError> {
    let parts = ids
        .iter()
        .map(|id| print_identifier(id, location))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(parts.join(", "))
}

// ---------------------------------------------------------------------------
// Declarations
// ---------------------------------------------------------------------------

fn print_declaration(
    decl: &VariableDeclaration,
    prefix: &str,
    location: &Location,
) -> Result<String, GalecError> {
    let ty = match &decl.ty {
        TypeRef::Primitive(scalar) => scalar.keyword().to_string(),
        TypeRef::Compartment(name) => print_name(name, location)?,
    };
    let mut out = format!("{prefix}{ty} {}", print_name(&decl.name, location)?);
    if !decl.dimensions.is_empty() {
        out.push('[');
        let dims = decl
            .dimensions
            .iter()
            .map(|d| print_dimension(d, location))
            .collect::<Result<Vec<_>, _>>()?;
        out.push_str(&dims.join(", "));
        out.push(']');
    }
    out.push_str(&print_range(&decl.range, location)?);
    out.push(';');
    Ok(out)
}

fn print_dimension(dimension: &Dimension, location: &Location) -> Result<String, GalecError> {
    match dimension {
        Dimension::Derived => Ok(":".to_string()),
        Dimension::Expr(expression) => expr(expression, location),
    }
}

fn print_range(range: &RangeAttributes, location: &Location) -> Result<String, GalecError> {
    if range.is_empty() {
        return Ok(String::new());
    }
    let mut attrs = Vec::new();
    if let Some(min) = &range.min {
        attrs.push(format!("min = {}", expr(min, location)?));
    }
    if let Some(max) = &range.max {
        attrs.push(format!("max = {}", expr(max, location)?));
    }
    Ok(format!("({})", attrs.join(", ")))
}

// ---------------------------------------------------------------------------
// Statements and block structure
// ---------------------------------------------------------------------------

const INDENT: &str = "    ";

#[derive(Default)]
struct Printer {
    out: String,
    indent: usize,
    path: Vec<PathSegment>,
}

impl Printer {
    fn loc(&self) -> Location {
        Location::at(self.path.clone())
    }

    fn line(&mut self, text: &str) {
        for _ in 0..self.indent {
            self.out.push_str(INDENT);
        }
        self.out.push_str(text);
        self.out.push('\n');
    }

    fn block(mut self, block: &Block) -> Result<String, GalecError> {
        self.path
            .push(PathSegment::Block(display_name(&block.name)));
        let block_name = print_name(&block.name, &self.loc())?;
        self.line(&format!("block {block_name}"));
        self.indent = 1;
        for variable in &block.interface {
            self.entity_line(&variable.decl, interface_prefix(variable.kind))?;
        }
        self.indent = 0;
        self.line("protected");
        self.indent = 1;
        for compartment in &block.compartments {
            self.compartment(compartment)?;
        }
        for entity in &block.protected {
            self.entity_line(&entity.decl, protected_prefix(entity.kind))?;
        }
        for signal in &block.error_signals {
            let text = print_identifier(signal, &self.loc())?;
            self.line(&format!("signal {text};"));
        }
        for function in &block.protected_functions {
            self.function(function)?;
        }
        self.indent = 0;
        self.line("public");
        self.indent = 1;
        self.method(BlockMethodKind::Startup, &block.startup)?;
        self.method(BlockMethodKind::Recalibrate, &block.recalibrate)?;
        self.method(BlockMethodKind::DoStep, &block.do_step)?;
        for function in &block.public_functions {
            self.function(function)?;
        }
        self.indent = 0;
        self.line(&format!("end {block_name};"));
        Ok(self.out)
    }

    fn entity_line(
        &mut self,
        decl: &VariableDeclaration,
        prefix: &'static str,
    ) -> Result<(), GalecError> {
        self.path
            .push(PathSegment::Variable(display_name(&decl.name)));
        let text = print_declaration(decl, prefix, &self.loc())?;
        self.line(&text);
        self.path.pop();
        Ok(())
    }

    fn compartment(&mut self, compartment: &StateCompartment) -> Result<(), GalecError> {
        self.path
            .push(PathSegment::Compartment(display_name(&compartment.name)));
        let name = print_name(&compartment.name, &self.loc())?;
        self.line(&format!("record {name}"));
        self.indent += 1;
        for entity in &compartment.entities {
            self.entity_line(&entity.decl, protected_prefix(entity.kind))?;
        }
        self.indent -= 1;
        self.line(&format!("end {name};"));
        self.path.pop();
        Ok(())
    }

    fn method(&mut self, kind: BlockMethodKind, method: &BlockMethod) -> Result<(), GalecError> {
        self.path.push(PathSegment::Method(kind));
        self.line(&format!("method {}", kind.name()));
        if !method.signals.is_empty() {
            let names: Vec<&str> = method.signals.iter().map(|s| s.name()).collect();
            self.indent += 1;
            self.line(&format!("signals {};", names.join(", ")));
            self.indent -= 1;
        }
        self.body(&method.locals, &method.statements)?;
        self.line(&format!("end {};", kind.name()));
        self.path.pop();
        Ok(())
    }

    fn function(&mut self, function: &UserFunction) -> Result<(), GalecError> {
        self.path
            .push(PathSegment::Function(display_name(&function.name)));
        let name = print_name(&function.name, &self.loc())?;
        self.line(&format!("{} {name}", function.kind.keyword()));
        self.indent += 1;
        if !function.signals.is_empty() {
            let text = identifier_list(&function.signals, &self.loc())?;
            self.line(&format!("signals {text};"));
        }
        for parameter in &function.parameters {
            self.path
                .push(PathSegment::Parameter(display_name(&parameter.decl.name)));
            let prefix = format!("{} ", parameter.direction.keyword());
            let text = print_declaration(&parameter.decl, &prefix, &self.loc())?;
            self.line(&text);
            self.path.pop();
        }
        self.indent -= 1;
        self.body(&function.locals, &function.statements)?;
        self.line(&format!("end {name};"));
        self.path.pop();
        Ok(())
    }

    /// Shared `[protected locals] algorithm statements` tail of functions
    /// and methods.
    fn body(
        &mut self,
        locals: &[VariableDeclaration],
        statements: &[Spanned<Statement>],
    ) -> Result<(), GalecError> {
        if !locals.is_empty() {
            self.line("protected");
            self.indent += 1;
            for local in locals {
                self.path
                    .push(PathSegment::Local(display_name(&local.name)));
                let text = print_declaration(local, "", &self.loc())?;
                self.line(&text);
                self.path.pop();
            }
            self.indent -= 1;
        }
        self.line("algorithm");
        self.indent += 1;
        self.statements(statements)?;
        self.indent -= 1;
        Ok(())
    }

    fn statements(&mut self, statements: &[Spanned<Statement>]) -> Result<(), GalecError> {
        for (index, statement) in statements.iter().enumerate() {
            self.path.push(PathSegment::Statement(index));
            self.statement(&statement.node)?;
            self.path.pop();
        }
        Ok(())
    }

    fn statement(&mut self, statement: &Statement) -> Result<(), GalecError> {
        match statement {
            Statement::Assignment { target, value } => {
                let location = self.loc();
                let text = format!(
                    "{} := {};",
                    print_reference(target, &location)?,
                    expr(value, &location)?
                );
                self.line(&text);
            }
            Statement::MultiAssignment { targets, call } => {
                let location = self.loc();
                let refs = targets
                    .iter()
                    .map(|t| print_reference(t, &location))
                    .collect::<Result<Vec<_>, _>>()?;
                let text = format!("({}) := {};", refs.join(", "), print_call(call, &location)?);
                self.line(&text);
            }
            Statement::Call(call) => {
                let text = format!("{};", print_call(call, &self.loc())?);
                self.line(&text);
            }
            Statement::If(if_statement) => self.if_statement(if_statement)?,
            Statement::For(for_loop) => self.for_loop(for_loop)?,
            Statement::Limit(targets) => self.limit(targets)?,
            Statement::Signal(signals) => {
                let location = self.loc();
                if signals.is_empty() {
                    return Err(GalecError::EmptySignalStatement { location });
                }
                let text = format!("signal {};", identifier_list(signals, &location)?);
                self.line(&text);
            }
        }
        Ok(())
    }

    fn if_statement(&mut self, if_statement: &IfStatement) -> Result<(), GalecError> {
        if if_statement.branches.is_empty() {
            return Err(GalecError::IfStatementWithoutBranches {
                location: self.loc(),
            });
        }
        for (index, branch) in if_statement.branches.iter().enumerate() {
            self.path.push(PathSegment::Branch(index));
            let keyword = if index == 0 { "if" } else { "elseif" };
            self.path.push(PathSegment::Condition);
            let condition = print_condition(&branch.condition, &self.loc())?;
            self.path.pop();
            self.line(&format!("{keyword} {condition} then"));
            self.indent += 1;
            self.statements(&branch.body)?;
            self.indent -= 1;
            self.path.pop();
        }
        if let Some(else_body) = &if_statement.else_body {
            self.path.push(PathSegment::Else);
            self.line("else");
            self.indent += 1;
            self.statements(else_body)?;
            self.indent -= 1;
            self.path.pop();
        }
        self.line("end if;");
        Ok(())
    }

    fn for_loop(&mut self, for_loop: &ForLoop) -> Result<(), GalecError> {
        let location = self.loc();
        let mut head = String::from("for ");
        if let Some(iterator) = &for_loop.iterator {
            head.push_str(&print_name(iterator, &location)?);
            head.push_str(" in ");
        }
        head.push_str(&expr(&for_loop.start, &location)?);
        head.push(':');
        if let Some(step) = &for_loop.step {
            head.push_str(&expr(step, &location)?);
            head.push(':');
        }
        head.push_str(&expr(&for_loop.stop, &location)?);
        head.push_str(" loop");
        self.line(&head);
        self.indent += 1;
        self.statements(&for_loop.body)?;
        self.indent -= 1;
        self.line("end for;");
        Ok(())
    }

    fn limit(&mut self, targets: &[LimitTarget]) -> Result<(), GalecError> {
        let location = self.loc();
        if targets.is_empty() {
            return Err(GalecError::EmptyLimitStatement { location });
        }
        let parts = targets
            .iter()
            .map(|target| match target {
                LimitTarget::SelfState => Ok("self".to_string()),
                LimitTarget::Reference(reference) => print_reference(reference, &location),
            })
            .collect::<Result<Vec<_>, _>>()?;
        self.line(&format!("limit {};", parts.join(", ")));
        Ok(())
    }
}

fn print_condition(condition: &Condition, location: &Location) -> Result<String, GalecError> {
    match condition {
        Condition::Expression(expression) => expr(expression, location),
        Condition::SignalCheck(check) => print_signal_check(check, location),
    }
}

fn print_signal_check(check: &SignalCheck, location: &Location) -> Result<String, GalecError> {
    let mut out = String::from("signal");
    if let Some(closure) = &check.closure {
        out.push(' ');
        out.push_str(&print_identifier(closure, location)?);
    }
    if let Some(test) = &check.test {
        if test.signals.is_empty() {
            return Err(GalecError::EmptySignalTestList {
                location: location.clone(),
            });
        }
        if test.negated {
            out.push_str(" not");
        }
        out.push_str(" in ");
        out.push_str(&identifier_list(&test.signals, location)?);
    }
    if let Some(fallback) = &check.fallback {
        out.push_str(" or ");
        out.push_str(&expr(fallback, location)?);
    }
    Ok(out)
}

fn interface_prefix(kind: crate::ast::InterfaceKind) -> &'static str {
    match kind {
        crate::ast::InterfaceKind::Input => "input ",
        crate::ast::InterfaceKind::Output => "output ",
        crate::ast::InterfaceKind::TunableParameter => "parameter ",
    }
}

fn protected_prefix(kind: crate::ast::ProtectedKind) -> &'static str {
    match kind {
        crate::ast::ProtectedKind::DependentParameter => "parameter ",
        crate::ast::ProtectedKind::Constant => "constant ",
        crate::ast::ProtectedKind::State => "",
    }
}
