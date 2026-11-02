//! Shared by the GALEC C integration tests; `src/codegen/solve_render_fixture_tests.rs`
//! is the unit-test copy. Keep the two in step.
//!
//! Render a hand-built GALEC block the way the shipped C targets do (#483):
//! refine it into a Solve algorithm block under the targets' binary32/signed32
//! profile, then render with both the Algorithm Code view (header ABI, symbol
//! policy) and the Solve view (method and function bodies).

use rumoca_ir_galec::ast as galec;
use rumoca_ir_galec::package::{AlgorithmCodePackage, CheckedAlgorithmBlock};
use rumoca_ir_solve::{SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat};
use rumoca_phase_codegen::AlgorithmCodeTemplateRenderer;

/// The name the package's clock reference uses when a fixture declares no
/// tunable parameter or constant of its own.
pub(crate) const FIXTURE_PERIOD: &str = "rumoca_fixture_period";

/// Refine `block` and render `template` for `model`, with `sources` resolving
/// trace anchors.
pub(crate) fn render_refined(
    block: &CheckedAlgorithmBlock,
    sources: &rumoca_core::SourceMap,
    template: &str,
    model: &str,
) -> Result<String, String> {
    let (package, refined) = refine(block)?;
    AlgorithmCodeTemplateRenderer::with_solve_block(&package, &refined, sources)
        .and_then(|renderer| renderer.render_with_name_and_artifact(template, model, &()))
        .map_err(|error| error.to_string())
}

/// A fixture block's package and its refinement under the C targets'
/// binary32/signed32 profile.
pub(crate) fn refine(
    block: &CheckedAlgorithmBlock,
) -> Result<(AlgorithmCodePackage, rumoca_ir_solve::SolveAlgorithmBlock), String> {
    let package = package(block)?;
    let profile = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into())
            .expect("the signed32 domain is valid"),
    );
    let refined = rumoca_phase_solve::lower_algorithm_block(&package, profile)
        .map_err(|error| error.to_string())?;
    Ok((package, refined))
}

/// The package a fixture block refines from: its first tunable parameter or
/// constant is the clock reference, or an added constant when it has none.
pub(crate) fn package(block: &CheckedAlgorithmBlock) -> Result<AlgorithmCodePackage, String> {
    let mut raw = block.block().clone();
    let clock = raw
        .interface
        .iter()
        .find(|variable| matches!(variable.kind, galec::InterfaceKind::TunableParameter))
        .map(|variable| variable.decl.name.lexeme().to_owned())
        .or_else(|| {
            raw.protected
                .iter()
                .find(|variable| matches!(variable.kind, galec::ProtectedKind::Constant))
                .map(|variable| variable.decl.name.lexeme().to_owned())
        });
    let clock = clock.unwrap_or_else(|| {
        let mut decl = galec::VariableDeclaration::scalar(
            galec::ScalarType::Real,
            galec::Name::ident(FIXTURE_PERIOD),
        );
        decl.span = raw.span;
        raw.protected.push(galec::ProtectedEntity {
            kind: galec::ProtectedKind::Constant,
            decl,
            start: Some(galec::Expression::Real(0.01)),
        });
        FIXTURE_PERIOD.to_owned()
    });
    complete_startup(&mut raw);
    fill_dummy_spans(&mut raw);
    let declarations = raw.interface.len() + raw.protected.len();
    AlgorithmCodePackage::construct(raw, vec![None; declarations], &clock)
        .map_err(|error| format!("fixture package: {error:?}"))
}

/// Solve operations carry provenance, so a hand-built fixture's empty spans
/// are filled: with the fixture's first real span when it has one (so a
/// traceability fixture's every anchor still resolves), else with distinct
/// synthetic spans. Real spans are kept.
fn fill_dummy_spans(block: &mut galec::Block) {
    let anchor = first_real_span(block);
    let source = rumoca_core::SourceId::from_source_name("fixture.alg");
    let mut next = 0usize;
    let mut fresh = |span: &mut rumoca_core::Span| {
        if *span == rumoca_core::Span::DUMMY {
            next += 1;
            *span =
                anchor.unwrap_or_else(|| rumoca_core::Span::from_offsets(source, next, next + 1));
        }
    };
    fresh(&mut block.span);
    for variable in &mut block.interface {
        fresh(&mut variable.decl.span);
    }
    for entity in &mut block.protected {
        fresh(&mut entity.decl.span);
    }
    for method in [
        &mut block.startup,
        &mut block.recalibrate,
        &mut block.do_step,
    ] {
        fresh(&mut method.span);
        for local in &mut method.locals {
            fresh(&mut local.span);
        }
        fill_statements(&mut method.statements, &mut fresh);
    }
    for function in block
        .protected_functions
        .iter_mut()
        .chain(&mut block.public_functions)
    {
        fresh(&mut function.span);
        for parameter in &mut function.parameters {
            fresh(&mut parameter.decl.span);
        }
        for local in &mut function.locals {
            fresh(&mut local.span);
        }
        fill_statements(&mut function.statements, &mut fresh);
    }
}

fn fill_statements(
    statements: &mut [galec::Spanned<galec::Statement>],
    fresh: &mut impl FnMut(&mut rumoca_core::Span),
) {
    for statement in statements {
        fresh(&mut statement.span);
        match &mut statement.node {
            galec::Statement::If(if_statement) => {
                for branch in &mut if_statement.branches {
                    fresh(&mut branch.span);
                    fill_statements(&mut branch.body, fresh);
                }
                if let Some(body) = &mut if_statement.else_body {
                    fill_statements(body, fresh);
                }
            }
            galec::Statement::For(for_loop) => fill_statements(&mut for_loop.body, fresh),
            _ => {}
        }
    }
}

/// Solve requires Startup to define every output, calculated parameter and
/// state, as the DAE projection always does. A hand-built fixture that
/// leaves one to the caller gets the assignment the projection would emit:
/// its declared start, else zero of its shape. DoStep semantics are
/// untouched.
fn complete_startup(block: &mut galec::Block) {
    let assigned: Vec<String> = block
        .startup
        .statements
        .iter()
        .filter_map(|statement| match &statement.node {
            galec::Statement::Assignment {
                target: galec::Reference::State(parts),
                ..
            } if parts.len() == 1 && parts[0].subscripts.is_empty() => {
                Some(parts[0].name.lexeme().to_owned())
            }
            _ => None,
        })
        .collect();
    let needed = block
        .interface
        .iter()
        .filter(|variable| matches!(variable.kind, galec::InterfaceKind::Output))
        .map(|variable| (variable.decl.clone(), variable.start.clone()))
        .chain(
            block
                .protected
                .iter()
                .map(|entity| (entity.decl.clone(), entity.start.clone())),
        )
        .filter(|(decl, _)| !assigned.iter().any(|name| name == decl.name.lexeme()))
        .collect::<Vec<_>>();
    // The package view states every interface start (GAL-020): row-major.
    for variable in &mut block.interface {
        if variable.start.is_none() {
            variable.start = flat_zero(&variable.decl);
        }
    }
    for (decl, start) in needed {
        let Some(value) = start
            .map(|start| reshape(start, &extents(&decl)))
            .or_else(|| zero(&decl))
        else {
            continue;
        };
        block
            .startup
            .statements
            .push(galec::Spanned::dummy(galec::Statement::Assignment {
                target: galec::Reference::state(decl.name.clone()),
                value,
            }));
    }
}

fn extents(decl: &galec::VariableDeclaration) -> Vec<usize> {
    decl.dimensions
        .iter()
        .filter_map(|dimension| match dimension {
            galec::Dimension::Expr(galec::Expression::Integer(extent)) => {
                usize::try_from(*extent).ok()
            }
            _ => None,
        })
        .collect()
}

/// A row-major start nested to the declaration's shape.
fn reshape(start: galec::Expression, extents: &[usize]) -> galec::Expression {
    let galec::Expression::Array(flat) = start else {
        return start;
    };
    if extents.len() < 2 {
        return galec::Expression::Array(flat);
    }
    let inner: usize = extents[1..].iter().product();
    galec::Expression::Array(
        flat.chunks(inner.max(1))
            .map(|chunk| reshape(galec::Expression::Array(chunk.to_vec()), &extents[1..]))
            .collect(),
    )
}

fn zero(decl: &galec::VariableDeclaration) -> Option<galec::Expression> {
    let galec::TypeRef::Primitive(scalar) = &decl.ty else {
        return None;
    };
    let element = match scalar {
        galec::ScalarType::Real => galec::Expression::Real(0.0),
        galec::ScalarType::Integer => galec::Expression::Integer(0),
        galec::ScalarType::Boolean => galec::Expression::Bool(false),
    };
    Some(extents(decl).iter().rev().fold(element, |inner, extent| {
        galec::Expression::Array(vec![inner; *extent])
    }))
}

fn flat_zero(decl: &galec::VariableDeclaration) -> Option<galec::Expression> {
    let element = zero(&galec::VariableDeclaration {
        dimensions: Vec::new(),
        ..decl.clone()
    })?;
    let extents = extents(decl);
    Some(if extents.is_empty() {
        element
    } else {
        galec::Expression::Array(vec![element; extents.iter().product()])
    })
}

fn first_real_span(block: &galec::Block) -> Option<rumoca_core::Span> {
    fn in_statements(statements: &[galec::Spanned<galec::Statement>]) -> Option<rumoca_core::Span> {
        statements.iter().find_map(|statement| {
            if statement.span != rumoca_core::Span::DUMMY {
                return Some(statement.span);
            }
            match &statement.node {
                galec::Statement::If(if_statement) => if_statement
                    .branches
                    .iter()
                    .find_map(|branch| in_statements(&branch.body))
                    .or_else(|| if_statement.else_body.as_deref().and_then(in_statements)),
                galec::Statement::For(for_loop) => in_statements(&for_loop.body),
                _ => None,
            }
        })
    }
    [&block.startup, &block.recalibrate, &block.do_step]
        .into_iter()
        .find_map(|method| in_statements(&method.statements))
}
