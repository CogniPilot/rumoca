//! Builders for `Block`, declarations, and method routing (WI-4).
//!
//! The grammar parses the block sections as uniform repetitions; this module
//! folds that flat CST into the strict [`crate::ast::Block`] (contract §4.3):
//!
//! - interface `parameter` (before `protected`) vs protected `parameter`
//!   (after) is disambiguated purely by which section repetition it came from;
//! - the three fixed public methods (`Startup`/`Recalibrate`/`DoStep`) are
//!   routed by name into their dedicated [`crate::ast::Block`] fields, every
//!   other `function`/`method` becomes a [`crate::ast::UserFunction`];
//! - a fixed method that declares parameters, a duplicate method, a missing
//!   method, an unknown predefined signal, and an `end <Name>;` mismatch are
//!   each a typed [`crate::parse::errors::GalecParseError`] (fail-early,
//!   SPEC_0008 — never a silent default).
//!
//! Every builder is `TryFrom<&Generated> for AstType` with
//! `type Error = anyhow::Error`; typed rejections are bridged via
//! `GalecParseError::into_anyhow()` (see [`crate::parse::errors`]).

use crate::ast::{
    Block, BlockMethod, BlockMethodKind, Dimension, Direction, FunctionKind, Identifier,
    InterfaceKind, InterfaceVariable, Name, Parameter, PredefinedSignal, ProtectedEntity,
    ProtectedKind, RangeAttributes, ScalarType, Spanned, StateCompartment, Statement, TypeRef,
    UserFunction, VariableDeclaration,
};
use crate::parse::errors::GalecParseError;
use crate::parse::generated::galec_grammar_trait as g;
use crate::parse::span::{spanned_statement, union};

// ---------------------------------------------------------------------------
// Block
// ---------------------------------------------------------------------------

/// `block : 'block' name { interface } 'protected' { compartment }
///   { protected-entity } { error-signal } { function } 'public' { function }
///   'end' name ';'` — folded into the strict [`Block`] (contract §4.3).
impl TryFrom<&g::Block> for Block {
    type Error = anyhow::Error;

    fn try_from(ast: &g::Block) -> Result<Self, Self::Error> {
        check_terminator(&ast.name, &ast.name0)?;
        let mut block = Block::new(ast.name.clone());
        for item in &ast.block_list {
            block
                .interface
                .push(interface_variable(&item.state_entity_declaration)?);
        }
        for item in &ast.block_list0 {
            block
                .compartments
                .push(compartment(&item.state_compartment_declaration)?);
        }
        for item in &ast.block_list1 {
            block
                .protected
                .push(protected_entity(&item.state_entity_declaration)?);
        }
        for item in &ast.block_list2 {
            block.error_signals.push(Identifier::new(
                item.error_signal_declaration.ident.ident.text(),
            ));
        }
        for item in &ast.block_list3 {
            block
                .protected_functions
                .push(user_function(&item.function_declaration)?);
        }
        route_public_functions(&mut block, &ast.block_list4)?;
        // Span the whole block from its header name to its `end` name (D11).
        block.span = union(ast.name.span(), ast.name0.span());
        Ok(block)
    }
}

/// Route the public-section functions: fixed methods (by name) into their
/// dedicated fields, everything else into `public_functions`; then enforce that
/// all three mandatory methods appeared exactly once (contract §4.3 points 2–3).
fn route_public_functions(block: &mut Block, items: &[g::BlockList4]) -> anyhow::Result<()> {
    let mut seen_startup = false;
    let mut seen_recalibrate = false;
    let mut seen_do_step = false;
    for item in items {
        let fd = &item.function_declaration;
        match method_kind(&fd.name) {
            Some(BlockMethodKind::Startup) => {
                assign_method(&mut seen_startup, &mut block.startup, fd, "Startup")?;
            }
            Some(BlockMethodKind::Recalibrate) => {
                assign_method(
                    &mut seen_recalibrate,
                    &mut block.recalibrate,
                    fd,
                    "Recalibrate",
                )?;
            }
            Some(BlockMethodKind::DoStep) => {
                assign_method(&mut seen_do_step, &mut block.do_step, fd, "DoStep")?;
            }
            None => block.public_functions.push(user_function(fd)?),
        }
    }
    require_method(seen_startup, "Startup")?;
    require_method(seen_recalibrate, "Recalibrate")?;
    require_method(seen_do_step, "DoStep")?;
    Ok(())
}

/// Fill a fixed-method slot, rejecting a second occurrence (trap T1 duplicate).
fn assign_method(
    seen: &mut bool,
    slot: &mut BlockMethod,
    fd: &g::FunctionDeclaration,
    name: &str,
) -> anyhow::Result<()> {
    if *seen {
        return Err(GalecParseError::DuplicateMethod {
            name: name.to_string(),
        }
        .into_anyhow());
    }
    *seen = true;
    *slot = block_method(fd)?;
    Ok(())
}

/// A mandatory method that never appeared is a fail-early parse error.
fn require_method(seen: bool, name: &str) -> anyhow::Result<()> {
    if seen {
        Ok(())
    } else {
        Err(GalecParseError::MissingMethod {
            name: name.to_string(),
        }
        .into_anyhow())
    }
}

/// The fixed block-method a `function`/`method` header names, if any (plain
/// identifier only — a quoted name never denotes a fixed method).
fn method_kind(name: &Name) -> Option<BlockMethodKind> {
    let Name::Ident(id, _) = name else {
        return None;
    };
    match id.as_str() {
        "Startup" => Some(BlockMethodKind::Startup),
        "Recalibrate" => Some(BlockMethodKind::Recalibrate),
        "DoStep" => Some(BlockMethodKind::DoStep),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Methods & user functions
// ---------------------------------------------------------------------------

/// Build a fixed [`BlockMethod`]: parameter-free (trap T1) with only predefined
/// signals in its `signals` clause (§3.2.5 §1.3).
fn block_method(fd: &g::FunctionDeclaration) -> anyhow::Result<BlockMethod> {
    check_terminator(&fd.name, &fd.name0)?;
    if !fd.function_declaration_list.is_empty() {
        return Err(GalecParseError::MethodHasParameters {
            name: name_text(&fd.name),
        }
        .into_anyhow());
    }
    Ok(BlockMethod {
        signals: predefined_signals(fd)?,
        locals: locals(fd),
        statements: function_statements(fd),
        // `method DoStep … end DoStep` — header name to `end` name (D11).
        span: union(fd.name.span(), fd.name0.span()),
    })
}

/// Build a user-defined `function` (stateless) or `method` (stateful).
fn user_function(fd: &g::FunctionDeclaration) -> anyhow::Result<UserFunction> {
    check_terminator(&fd.name, &fd.name0)?;
    let kind = match &fd.function_declaration_group {
        g::FunctionDeclarationGroup::Function(_) => FunctionKind::Stateless,
        g::FunctionDeclarationGroup::Method(_) => FunctionKind::Stateful,
    };
    Ok(UserFunction {
        kind,
        name: fd.name.clone(),
        signals: signal_idents(fd),
        parameters: parameters(fd),
        locals: locals(fd),
        statements: function_statements(fd),
        // `function f … end f` — header name to `end` name (D11).
        span: union(fd.name.span(), fd.name0.span()),
    })
}

/// Map a fixed method's `signals` clause to the six predefined signals; an
/// unknown name is a typed error (no silent drop).
fn predefined_signals(fd: &g::FunctionDeclaration) -> anyhow::Result<Vec<PredefinedSignal>> {
    let mut out = Vec::new();
    for ident in signal_idents(fd) {
        match PredefinedSignal::ALL
            .into_iter()
            .find(|s| s.name() == ident.as_str())
        {
            Some(signal) => out.push(signal),
            None => {
                return Err(GalecParseError::UnknownPredefinedSignal {
                    name: ident.as_str().to_string(),
                }
                .into_anyhow());
            }
        }
    }
    Ok(out)
}

/// `signal_interface : 'signals' ident { ',' ident } ';'` → the (possibly
/// absent) identifier list.
fn signal_idents(fd: &g::FunctionDeclaration) -> Vec<Identifier> {
    match &fd.function_declaration_opt {
        None => Vec::new(),
        Some(opt) => {
            let si = &opt.signal_interface;
            let mut out = Vec::with_capacity(1 + si.signal_interface_list.len());
            out.push(Identifier::new(si.ident.ident.text()));
            for extra in &si.signal_interface_list {
                out.push(Identifier::new(extra.ident.ident.text()));
            }
            out
        }
    }
}

/// `{ parameter_declaration }` → the function's `input`/`output` parameters.
fn parameters(fd: &g::FunctionDeclaration) -> Vec<Parameter> {
    fd.function_declaration_list
        .iter()
        .map(|item| {
            let pd = &item.parameter_declaration;
            let direction = match &pd.parameter_declaration_group {
                g::ParameterDeclarationGroup::Input(_) => Direction::Input,
                g::ParameterDeclarationGroup::Output(_) => Direction::Output,
            };
            Parameter {
                direction,
                decl: pd.variable_declaration.clone(),
            }
        })
        .collect()
}

/// `[ 'protected' { local_variable_declaration } ]` → the method-local variables.
fn locals(fd: &g::FunctionDeclaration) -> Vec<VariableDeclaration> {
    match &fd.function_declaration_opt0 {
        None => Vec::new(),
        Some(opt) => opt
            .function_declaration_opt0_list
            .iter()
            .map(|item| item.local_variable_declaration.variable_declaration.clone())
            .collect(),
    }
}

/// `'algorithm' { statement }` → the body statements, each wrapped with its
/// reconstructed span (D11).
fn function_statements(fd: &g::FunctionDeclaration) -> Vec<Spanned<Statement>> {
    fd.function_declaration_list0
        .iter()
        .map(|item| spanned_statement(&item.statement))
        .collect()
}

// ---------------------------------------------------------------------------
// State entities & compartments
// ---------------------------------------------------------------------------

/// An interface variable (before `protected`) must carry an `input`/`output`/
/// `parameter` causality; `constant` or a bare declaration is unrepresentable
/// and rejected fail-early.
fn interface_variable(sed: &g::StateEntityDeclaration) -> anyhow::Result<InterfaceVariable> {
    let kind = match &sed.state_entity_declaration_opt {
        Some(opt) => match &opt.state_entity_declaration_opt_group {
            g::StateEntityDeclarationOptGroup::Input(_) => InterfaceKind::Input,
            g::StateEntityDeclarationOptGroup::Output(_) => InterfaceKind::Output,
            g::StateEntityDeclarationOptGroup::Parameter(_) => InterfaceKind::TunableParameter,
            g::StateEntityDeclarationOptGroup::Constant(_) => {
                return Err(GalecParseError::syntax(format!(
                    "interface variable `{}` may not be `constant` (use input, output, or parameter)",
                    name_text(&sed.variable_declaration.name)
                ))
                .into_anyhow());
            }
        },
        None => {
            return Err(GalecParseError::syntax(format!(
                "interface variable `{}` must be declared input, output, or parameter",
                name_text(&sed.variable_declaration.name)
            ))
            .into_anyhow());
        }
    };
    Ok(InterfaceVariable {
        kind,
        decl: sed.variable_declaration.clone(),
        start: None,
    })
}

/// A protected state entity (after `protected`): `parameter` → dependent
/// parameter, `constant` → constant, bare → discrete state; an `input`/`output`
/// prefix is unrepresentable there and rejected fail-early.
fn protected_entity(sed: &g::StateEntityDeclaration) -> anyhow::Result<ProtectedEntity> {
    let kind = match &sed.state_entity_declaration_opt {
        Some(opt) => match &opt.state_entity_declaration_opt_group {
            g::StateEntityDeclarationOptGroup::Parameter(_) => ProtectedKind::DependentParameter,
            g::StateEntityDeclarationOptGroup::Constant(_) => ProtectedKind::Constant,
            g::StateEntityDeclarationOptGroup::Input(_)
            | g::StateEntityDeclarationOptGroup::Output(_) => {
                return Err(GalecParseError::syntax(format!(
                    "protected entity `{}` may not use an input/output causality prefix",
                    name_text(&sed.variable_declaration.name)
                ))
                .into_anyhow());
            }
        },
        None => ProtectedKind::State,
    };
    Ok(ProtectedEntity {
        kind,
        decl: sed.variable_declaration.clone(),
        start: None,
    })
}

/// `record name { state-entity } end name ;` → a [`StateCompartment`].
fn compartment(scd: &g::StateCompartmentDeclaration) -> anyhow::Result<StateCompartment> {
    check_terminator(&scd.name, &scd.name0)?;
    let mut entities = Vec::with_capacity(scd.state_compartment_declaration_list.len());
    for item in &scd.state_compartment_declaration_list {
        entities.push(protected_entity(&item.state_entity_declaration)?);
    }
    Ok(StateCompartment {
        span: union(scd.name.span(), scd.name0.span()),
        name: scd.name.clone(),
        entities,
    })
}

// ---------------------------------------------------------------------------
// Variable declaration / dimension / range attributes (%nt_type targets)
// ---------------------------------------------------------------------------

/// `variable_declaration : type name [ '[' dimension,… ']' ] [ range ] ';'`.
impl TryFrom<&g::VariableDeclaration> for VariableDeclaration {
    type Error = anyhow::Error;

    fn try_from(ast: &g::VariableDeclaration) -> Result<Self, Self::Error> {
        let ty = match &ast.declared_type {
            g::DeclaredType::Boolean(_) => TypeRef::Primitive(ScalarType::Boolean),
            g::DeclaredType::Integer(_) => TypeRef::Primitive(ScalarType::Integer),
            g::DeclaredType::Real(_) => TypeRef::Primitive(ScalarType::Real),
            g::DeclaredType::Name(n) => TypeRef::Compartment(n.name.clone()),
        };
        let dimensions = match &ast.variable_declaration_opt {
            None => Vec::new(),
            Some(opt) => {
                let cd = &opt.constant_dimensions;
                let mut out = Vec::with_capacity(1 + cd.constant_dimensions_list.len());
                out.push(cd.dimension.clone());
                for extra in &cd.constant_dimensions_list {
                    out.push(extra.dimension.clone());
                }
                out
            }
        };
        let range = match &ast.variable_declaration_opt0 {
            None => RangeAttributes::default(),
            Some(opt) => opt.range_attributes.clone(),
        };
        Ok(Self {
            span: ast.name.span(),
            ty,
            name: ast.name.clone(),
            dimensions,
            range,
        })
    }
}

/// `dimension : ':' | expression` — `:` is a derived (function-input) dimension.
impl TryFrom<&g::Dimension> for Dimension {
    type Error = anyhow::Error;

    fn try_from(ast: &g::Dimension) -> Result<Self, Self::Error> {
        Ok(match ast {
            g::Dimension::Colon(_) => Self::Derived,
            g::Dimension::Expression(d) => Self::Expr(d.expression.clone()),
        })
    }
}

/// `range_attributes : '(' range_attr { ',' range_attr } ')'` where each
/// `range_attr : ident '=' expression`. Only `min`/`max` are representable; an
/// unknown key or a duplicate is a typed error (the AST cannot hold either).
impl TryFrom<&g::RangeAttributes> for RangeAttributes {
    type Error = anyhow::Error;

    fn try_from(ast: &g::RangeAttributes) -> Result<Self, Self::Error> {
        let mut range = RangeAttributes::default();
        apply_range_attr(&mut range, &ast.range_attr)?;
        for extra in &ast.range_attributes_list {
            apply_range_attr(&mut range, &extra.range_attr)?;
        }
        Ok(range)
    }
}

fn apply_range_attr(range: &mut RangeAttributes, attr: &g::RangeAttr) -> anyhow::Result<()> {
    let key = attr.ident.ident.text();
    let slot = match key {
        "min" => &mut range.min,
        "max" => &mut range.max,
        other => {
            return Err(GalecParseError::syntax(format!(
                "unknown range attribute `{other}` (expected `min` or `max`)"
            ))
            .into_anyhow());
        }
    };
    if slot.is_some() {
        return Err(
            GalecParseError::syntax(format!("duplicate range attribute `{key}`")).into_anyhow(),
        );
    }
    *slot = Some(attr.expression.clone());
    Ok(())
}

// ---------------------------------------------------------------------------
// Shared helpers
// ---------------------------------------------------------------------------

/// Reject an `end <Name>;` terminator that does not lexically equal its header
/// name (contract §4.3 point 4). Compares the lexeme text only: names carry
/// source spans (provenance, not identity), and the header and footer occupy
/// distinct source positions, so raw `Name` equality would spuriously fail.
fn check_terminator(header: &Name, footer: &Name) -> anyhow::Result<()> {
    if name_text(header) == name_text(footer) {
        Ok(())
    } else {
        Err(GalecParseError::TerminatorMismatch {
            expected: name_text(header),
            found: name_text(footer),
            span: None,
        }
        .into_anyhow())
    }
}

/// Human-readable rendering of a name for diagnostics (quoted names keep their
/// `'…'` delimiters).
fn name_text(name: &Name) -> String {
    match name {
        Name::Ident(id, _) => id.as_str().to_string(),
        Name::Quoted(content, _) => format!("'{content}'"),
    }
}
