//! Name analysis (trap T13, S-2.1/S-2.5, GAL-015 collision surface).
//!
//! Checks every DECLARATION site: identifier legality (ASCII-letter-first,
//! `[A-Za-z0-9_]` continuation, no reserved-name collision per namespace),
//! quoted-identifier well-formedness against the full `quoted-identifier`
//! grammar (literal positive indices, no whitespace), declaration
//! uniqueness per namespace, and block-interface ordering.
//!
//! Two reserved surfaces apply (S-2.5): declarations in the function
//! namespace (functions, compartments, parameters, locals, error signals,
//! iterators, closures) must avoid builtins (ordinary functions, S-2.9),
//! Appendix C reservations, and the predefined signal names; state entities
//! are a SEPARATE namespace always accessed via `self.` (S-2.5 R-2) and MAY
//! share those names — only the lexical surface (keywords, reserved
//! keywords, `__` prefix) binds them.
//!
//! Reference sites are NOT name-checked here — they legitimately name
//! reserved things (builtin calls, predefined signals) and are covered by
//! resolution in the other analyses. Matching `end` names (S-2.1) are
//! guaranteed by construction: the AST stores each block/compartment/
//! function name exactly once and the printer emits it at both ends.

use crate::ast::{
    Condition, Identifier, InterfaceKind, Name, Statement, UserFunction, VariableDeclaration,
};
use crate::builtins::{is_lexically_reserved, is_reserved_name};
use crate::diagnostic::{GalecError, Location, PathSegment};

use super::context::{BlockContext, lexeme};

/// Which reserved-name surface applies at a declaration site (S-2.5).
#[derive(Clone, Copy)]
enum ReservedSurface {
    /// The function namespace: keywords plus builtins, Appendix C names,
    /// and predefined signals.
    Full,
    /// State entities: keywords, reserved keywords, and `__` only.
    Lexical,
}

impl ReservedSurface {
    fn is_reserved(self, name: &str) -> bool {
        match self {
            Self::Full => is_reserved_name(name),
            Self::Lexical => is_lexically_reserved(name),
        }
    }
}

pub(super) fn check(ctx: &BlockContext<'_>, diags: &mut Vec<GalecError>) {
    let block = ctx.block;
    let block_path = || vec![PathSegment::Block(lexeme(&block.name))];
    check_name(
        &block.name,
        &Location::at(block_path()),
        ReservedSurface::Full,
        diags,
    );
    check_interface(ctx, diags);
    check_state_entities(ctx, diags);
    check_compartments(ctx, diags);
    check_error_signals(ctx, diags);
    check_functions(ctx, diags);
    for body in ctx.bodies() {
        check_body_names(ctx, &body, diags);
    }
}

// ---------------------------------------------------------------------------
// Identifier and quoted-identifier legality
// ---------------------------------------------------------------------------

/// Check a declared [`Name`] (plain or quoted) at a location.
fn check_name(
    name: &Name,
    location: &Location,
    surface: ReservedSurface,
    diags: &mut Vec<GalecError>,
) {
    match name {
        Name::Ident(id) => check_identifier(id, location, surface, diags),
        Name::Quoted(content) => check_quoted(content, location, diags),
    }
}

fn check_identifier(
    id: &Identifier,
    location: &Location,
    surface: ReservedSurface,
    diags: &mut Vec<GalecError>,
) {
    let name = id.as_str();
    if name.is_empty() {
        diags.push(GalecError::EmptyIdentifier {
            location: location.clone(),
        });
        return;
    }
    if let Some(reason) = identifier_shape_error(name) {
        diags.push(GalecError::IllegalIdentifier {
            location: location.clone(),
            name: name.to_string(),
            reason,
        });
        return;
    }
    if surface.is_reserved(name) {
        diags.push(GalecError::ReservedName {
            location: location.clone(),
            name: name.to_string(),
        });
    }
}

fn identifier_shape_error(name: &str) -> Option<&'static str> {
    let mut chars = name.chars();
    let first = chars.next()?;
    if !first.is_ascii_alphabetic() {
        return Some("must start with an ASCII letter");
    }
    if !chars.all(|c| c.is_ascii_alphanumeric() || c == '_') {
        return Some("may contain only ASCII letters, digits, and `_`");
    }
    None
}

/// Full `quoted-identifier` grammar check (G-1.21..G-1.24): content is
/// `previous(scalarized)`, nested `derivative(…)`, or a scalarized
/// reference; fixed dimensions are literal POSITIVE integers; no whitespace.
fn check_quoted(content: &str, location: &Location, diags: &mut Vec<GalecError>) {
    if let Some(reason) = quoted_content_error(content) {
        diags.push(GalecError::MalformedQuotedIdentifier {
            location: location.clone(),
            content: content.to_string(),
            reason,
        });
    }
}

fn quoted_content_error(content: &str) -> Option<&'static str> {
    if content.is_empty() {
        return Some("empty quoted identifier");
    }
    if content.chars().any(|c| c.is_whitespace() || c.is_control()) {
        return Some("whitespace is not allowed inside quoted identifiers");
    }
    if content.contains('\'') {
        return Some("quote character inside quoted identifier");
    }
    check_quoted_form(content)
}

fn check_quoted_form(content: &str) -> Option<&'static str> {
    if let Some(inner) = wrapped(content, "previous") {
        return check_scalarized(inner);
    }
    if let Some(inner) = wrapped(content, "derivative") {
        return check_higher_order(inner);
    }
    check_scalarized(content)
}

fn check_higher_order(content: &str) -> Option<&'static str> {
    match wrapped(content, "derivative") {
        Some(inner) => check_higher_order(inner),
        None => check_scalarized(content),
    }
}

/// `<keyword>(inner)` unwrapping; scalarized references contain no
/// parentheses, so the prefix test cannot misfire.
fn wrapped<'c>(content: &'c str, keyword: &str) -> Option<&'c str> {
    let rest = content.strip_prefix(keyword)?;
    let rest = rest.strip_prefix('(')?;
    rest.strip_suffix(')')
}

/// Separator between the segments of a scalarized reference (GAL-015).
const SCALARIZED_SEGMENT_SEPARATOR: char = '.';

/// The single boundary parser for the GAL-015 dot-separated scalarized-
/// reference convention inside quoted identifiers. The content is GALEC's
/// own language surface (already-flattened text), not Modelica IR structure,
/// so segmenting it here is legitimate — but only through this helper.
fn scalarized_segments(content: &str) -> std::str::Split<'_, char> {
    content.split(SCALARIZED_SEGMENT_SEPARATOR)
}

fn check_scalarized(content: &str) -> Option<&'static str> {
    if content.is_empty() {
        return Some("empty scalarized reference");
    }
    for segment in scalarized_segments(content) {
        let (name_part, dims) = match segment.find('[') {
            Some(open) => {
                let closed = segment
                    .strip_suffix(']')
                    .map(|trimmed| &trimmed[open + 1..]);
                let Some(dims) = closed else {
                    return Some("unterminated `[` in scalarized reference");
                };
                (&segment[..open], Some(dims))
            }
            None => (segment, None),
        };
        if let Some(reason) = scalarized_segment_name_error(name_part) {
            return Some(reason);
        }
        if let Some(dims) = dims
            && !dims.split(',').all(is_positive_integer)
        {
            return Some("indices must be literal positive integers");
        }
    }
    None
}

/// Segment names are `identifier | keyword`: ASCII-letter-first, or the
/// `__`-prefixed keyword space.
fn scalarized_segment_name_error(name: &str) -> Option<&'static str> {
    let body = name.strip_prefix("__").unwrap_or(name);
    let mut chars = body.chars();
    let starts_with_letter = chars.next().is_some_and(|c| c.is_ascii_alphabetic());
    let tail_ok = chars.all(|c| c.is_ascii_alphanumeric() || c == '_');
    if starts_with_letter && tail_ok {
        None
    } else {
        Some("segment is not an identifier or keyword")
    }
}

fn is_positive_integer(text: &str) -> bool {
    let mut chars = text.chars();
    chars.next().is_some_and(|c| c.is_ascii_digit() && c != '0')
        && chars.all(|c| c.is_ascii_digit())
}

// ---------------------------------------------------------------------------
// Section checks
// ---------------------------------------------------------------------------

fn check_interface(ctx: &BlockContext<'_>, diags: &mut Vec<GalecError>) {
    let rank = |kind: InterfaceKind| match kind {
        InterfaceKind::Input => 0,
        InterfaceKind::Output => 1,
        InterfaceKind::TunableParameter => 2,
    };
    let mut highest = 0;
    for var in &ctx.block.interface {
        if rank(var.kind) < highest {
            diags.push(GalecError::InterfaceVariableOrder {
                location: variable_location(ctx, &var.decl),
                name: lexeme(&var.decl.name),
            });
        }
        highest = highest.max(rank(var.kind));
    }
}

/// Interface + protected state entities share one namespace (S-2.5; it is
/// separate from functions/compartments/builtins — those MAY share entity
/// names, so only the lexical surface applies).
fn check_state_entities(ctx: &BlockContext<'_>, diags: &mut Vec<GalecError>) {
    let mut seen: Vec<String> = Vec::new();
    let decls = ctx
        .block
        .interface
        .iter()
        .map(|v| &v.decl)
        .chain(ctx.block.protected.iter().map(|e| &e.decl));
    for decl in decls {
        let location = variable_location(ctx, decl);
        check_name(&decl.name, &location, ReservedSurface::Lexical, diags);
        note_duplicate(
            &mut seen,
            lexeme(&decl.name),
            "block state entities",
            &location,
            diags,
        );
    }
}

fn check_compartments(ctx: &BlockContext<'_>, diags: &mut Vec<GalecError>) {
    let mut seen_compartments: Vec<String> = Vec::new();
    for compartment in &ctx.block.compartments {
        let path = vec![
            PathSegment::Block(lexeme(&ctx.block.name)),
            PathSegment::Compartment(lexeme(&compartment.name)),
        ];
        let location = Location::at(path.clone());
        check_name(&compartment.name, &location, ReservedSurface::Full, diags);
        note_duplicate(
            &mut seen_compartments,
            lexeme(&compartment.name),
            "functions and compartments",
            &location,
            diags,
        );
        let mut seen: Vec<String> = Vec::new();
        for entity in &compartment.entities {
            let mut entity_path = path.clone();
            entity_path.push(PathSegment::Variable(lexeme(&entity.decl.name)));
            let entity_location = Location::at(entity_path);
            // Compartment entities are state entities: lexical surface only.
            check_name(
                &entity.decl.name,
                &entity_location,
                ReservedSurface::Lexical,
                diags,
            );
            note_duplicate(
                &mut seen,
                lexeme(&entity.decl.name),
                "compartment entities",
                &entity_location,
                diags,
            );
        }
    }
}

fn check_error_signals(ctx: &BlockContext<'_>, diags: &mut Vec<GalecError>) {
    let mut seen: Vec<String> = Vec::new();
    for signal in &ctx.block.error_signals {
        let location = Location::at(vec![
            PathSegment::Block(lexeme(&ctx.block.name)),
            PathSegment::Variable(signal.0.clone()),
        ]);
        check_identifier(signal, &location, ReservedSurface::Full, diags);
        note_duplicate(
            &mut seen,
            signal.0.clone(),
            "error signals",
            &location,
            diags,
        );
    }
}

/// Functions and compartments share one namespace (S-2.5).
fn check_functions(ctx: &BlockContext<'_>, diags: &mut Vec<GalecError>) {
    let mut seen: Vec<String> = ctx
        .block
        .compartments
        .iter()
        .map(|c| lexeme(&c.name))
        .collect();
    let functions = ctx
        .block
        .protected_functions
        .iter()
        .chain(&ctx.block.public_functions);
    for function in functions {
        let location = function_location(ctx, function);
        check_name(&function.name, &location, ReservedSurface::Full, diags);
        note_duplicate(
            &mut seen,
            lexeme(&function.name),
            "functions and compartments",
            &location,
            diags,
        );
    }
}

/// Parameter/local names within one body: legality, mutual uniqueness, and
/// no collision with function/compartment names (S-2.5). Loop iterators and
/// signal closures are declaration sites too.
fn check_body_names(
    ctx: &BlockContext<'_>,
    body: &super::context::BodyView<'_>,
    diags: &mut Vec<GalecError>,
) {
    let base = vec![
        PathSegment::Block(lexeme(&ctx.block.name)),
        body.segment.clone(),
    ];
    let mut seen: Vec<String> = Vec::new();
    let decls = body
        .parameters
        .iter()
        .map(|p| &p.decl)
        .chain(body.locals.iter());
    for decl in decls {
        let mut path = base.clone();
        path.push(PathSegment::Local(lexeme(&decl.name)));
        let location = Location::at(path);
        check_name(&decl.name, &location, ReservedSurface::Full, diags);
        let name = lexeme(&decl.name);
        if ctx.functions.contains_key(&name) || ctx.compartments.contains_key(&name) {
            diags.push(GalecError::DuplicateName {
                location: location.clone(),
                name: name.clone(),
                namespace: "parameters/locals vs functions/compartments",
            });
        }
        note_duplicate(&mut seen, name, "parameters and locals", &location, diags);
    }
    check_statement_declarations(body.statements, &base, diags);
}

fn check_statement_declarations(
    statements: &[Statement],
    base: &[PathSegment],
    diags: &mut Vec<GalecError>,
) {
    for (index, statement) in statements.iter().enumerate() {
        let mut path = base.to_vec();
        path.push(PathSegment::Statement(index));
        match statement {
            Statement::For(for_loop) => {
                if let Some(iterator) = &for_loop.iterator {
                    check_name(
                        iterator,
                        &Location::at(path.clone()),
                        ReservedSurface::Full,
                        diags,
                    );
                }
                check_statement_declarations(&for_loop.body, &path, diags);
            }
            Statement::If(if_statement) => {
                check_if_declarations(if_statement, &path, diags);
            }
            Statement::Assignment { .. }
            | Statement::MultiAssignment { .. }
            | Statement::Call(_)
            | Statement::Limit(_)
            | Statement::Signal(_) => {}
        }
    }
}

/// Signal-closures are declaration sites (scoped to the branch body).
fn check_if_declarations(
    if_statement: &crate::ast::IfStatement,
    path: &[PathSegment],
    diags: &mut Vec<GalecError>,
) {
    for (branch_index, branch) in if_statement.branches.iter().enumerate() {
        let mut branch_path = path.to_vec();
        branch_path.push(PathSegment::Branch(branch_index));
        if let Condition::SignalCheck(check) = &branch.condition
            && let Some(closure) = &check.closure
        {
            let mut closure_path = branch_path.clone();
            closure_path.push(PathSegment::Condition);
            check_identifier(
                closure,
                &Location::at(closure_path),
                ReservedSurface::Full,
                diags,
            );
        }
        check_statement_declarations(&branch.body, &branch_path, diags);
    }
    if let Some(else_body) = &if_statement.else_body {
        let mut else_path = path.to_vec();
        else_path.push(PathSegment::Else);
        check_statement_declarations(else_body, &else_path, diags);
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn note_duplicate(
    seen: &mut Vec<String>,
    name: String,
    namespace: &'static str,
    location: &Location,
    diags: &mut Vec<GalecError>,
) {
    if seen.contains(&name) {
        diags.push(GalecError::DuplicateName {
            location: location.clone(),
            name,
            namespace,
        });
    } else {
        seen.push(name);
    }
}

fn variable_location(ctx: &BlockContext<'_>, decl: &VariableDeclaration) -> Location {
    Location::at(vec![
        PathSegment::Block(lexeme(&ctx.block.name)),
        PathSegment::Variable(lexeme(&decl.name)),
    ])
}

fn function_location(ctx: &BlockContext<'_>, function: &UserFunction) -> Location {
    Location::at(vec![
        PathSegment::Block(lexeme(&ctx.block.name)),
        PathSegment::Function(lexeme(&function.name)),
    ])
}
