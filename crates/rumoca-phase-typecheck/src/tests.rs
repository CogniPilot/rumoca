//! Shared fixtures for the typecheck regression suite.
//!
//! The parse/resolve helpers and the overlay builders used by every topic
//! module live here; the tests themselves live in the sibling modules
//! declared below.
use super::*;

mod builtin_type_tests;
mod causality_and_function_tests;
mod dimension_tests;
mod equation_and_algorithm_tests;
mod instanced_check_tests;
mod instanced_dimension_tests;
mod instanced_expression_tests;
mod instanced_semantics_tests;
mod instanced_type_name_tests;
mod modifier_tests;
mod parameter_if_branch_tests;
mod record_constructor_alias_tests;
mod user_defined_type_tests;
mod variability_tests;

use rumoca_core::Token;
use rumoca_ir_ast::{
    ComponentRefPart, ComponentReference, Connection, Expression, InstanceData, InstanceId,
    ParsedTree, QualifiedName, Subscript, TerminalType,
};
use rumoca_phase_parse::parse_to_ast;
use rumoca_phase_resolve::resolve;
use std::sync::Arc;

/// Helper to parse source code into a ParsedTree.
fn parse(source: &str) -> ParsedTree {
    let file_name = "<test>";
    let stored_def = parse_to_ast(source, file_name).expect("parse should succeed");
    let mut tree = ClassTree::from_parsed(stored_def);
    tree.source_map.add(file_name, source);
    ParsedTree::new(tree)
}

fn typecheck_diagnostics(source: &str) -> rumoca_core::Diagnostics {
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let mut tree = resolved.into_inner();
    let mut checker = TypeChecker::new();
    checker.check(&mut tree);
    checker.take_diagnostics()
}

fn add_test_instance(
    overlay: &mut InstanceOverlay,
    qualified_name: &str,
    component: &Component,
    binding: Option<Expression>,
) {
    let instance_id = overlay.alloc_id();
    overlay.add_component(InstanceData {
        instance_id,
        qualified_name: QualifiedName::from_dotted(qualified_name),
        type_id: component.type_id.unwrap_or(TypeId::UNKNOWN),
        type_name: component.type_name.to_string(),
        type_def_id: component.type_def_id,
        source_location: component.location.clone(),
        dims: component.shape.iter().map(|&dim| dim as i64).collect(),
        dims_expr: component.shape_expr.clone(),
        variability: component.variability.clone(),
        causality: component.causality.clone(),
        flow: matches!(component.connection, Connection::Flow(_)),
        stream: matches!(component.connection, Connection::Stream(_)),
        binding,
        is_primitive: true,
        is_final: component.is_final,
        is_protected: component.is_protected,
        ..Default::default()
    });
}

fn make_comp_ref(name: &str) -> ComponentReference {
    ComponentReference {
        local: false,
        parts: vec![ComponentRefPart {
            ident: Token {
                text: Arc::from(name),
                ..Default::default()
            },
            subs: None,
        }],
        span: rumoca_core::Span::DUMMY,
        def_id: None,
    }
}

fn add_instanced_component(
    overlay: &mut InstanceOverlay,
    path: &str,
    component: &Component,
    is_primitive: bool,
) {
    let instance_id = overlay.alloc_id();
    overlay.add_component(InstanceData {
        instance_id,
        qualified_name: QualifiedName::from_dotted(path),
        source_location: component.location.clone(),
        dims: component.shape.iter().map(|&dim| dim as i64).collect(),
        dims_expr: component.shape_expr.clone(),
        type_id: TypeId::UNKNOWN,
        type_name: component.type_name.to_string(),
        type_def_id: component.type_def_id,
        variability: component.variability.clone(),
        causality: component.causality.clone(),
        binding: component.binding.clone(),
        start: (!matches!(component.start, Expression::Empty { .. }))
            .then(|| component.start.clone()),
        is_primitive,
        ..Default::default()
    });
}
