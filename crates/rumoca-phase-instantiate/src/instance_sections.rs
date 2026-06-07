use rumoca_ir_ast as ast;

use super::InstantiateContext;
use super::connections;
use super::inheritance::option_location_to_span;
use super::source_scope::{location_source_scope, location_source_scope_id};

/// Convert algorithm statements to instance statements.
pub(super) fn algorithms_to_instance(
    ctx: &InstantiateContext,
    algorithms: &[Vec<rumoca_ir_ast::Statement>],
    origin: &ast::QualifiedName,
    source_map: &rumoca_core::SourceMap,
) -> Vec<Vec<ast::InstanceStatement>> {
    algorithms
        .iter()
        .map(|stmts| {
            stmts
                .iter()
                .map(|stmt| {
                    let location = stmt.get_location();
                    ast::InstanceStatement {
                        statement: stmt.clone(),
                        origin: origin.clone(),
                        source_scope: location_source_scope(ctx, location),
                        source_scope_id: location_source_scope_id(ctx, location),
                        span: option_location_to_span(location, source_map),
                    }
                })
                .collect()
        })
        .collect()
}

/// Convert borrowed equations to instance equations.
pub(super) fn equations_to_instance_cloned(
    ctx: &InstantiateContext,
    equations: &[ast::Equation],
    origin: &ast::QualifiedName,
    source_map: &rumoca_core::SourceMap,
) -> Vec<ast::InstanceEquation> {
    equations
        .iter()
        .map(|eq| {
            let location = eq.get_location();
            ast::InstanceEquation {
                equation: eq.clone(),
                origin: origin.clone(),
                source_scope: location_source_scope(ctx, location),
                source_scope_id: location_source_scope_id(ctx, location),
                span: option_location_to_span(location, source_map),
            }
        })
        .collect()
}

/// Convert non-connection equations to instance equations in one pass.
pub(super) fn equations_to_instance_without_connections(
    ctx: &InstantiateContext,
    equations: &[ast::Equation],
    origin: &ast::QualifiedName,
    source_map: &rumoca_core::SourceMap,
) -> Vec<ast::InstanceEquation> {
    equations
        .iter()
        .filter(|eq| !connections::is_connect_equation(eq))
        .map(|eq| {
            let location = eq.get_location();
            ast::InstanceEquation {
                equation: eq.clone(),
                origin: origin.clone(),
                source_scope: location_source_scope(ctx, location),
                source_scope_id: location_source_scope_id(ctx, location),
                span: option_location_to_span(location, source_map),
            }
        })
        .collect()
}
