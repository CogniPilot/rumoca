//! Resolve-phase test suite.
//!
//! The tests live in topic-grouped submodules; this module holds only the
//! shared source-to-resolved-tree helpers that they all build on.

use super::*;
use rumoca_phase_parse::parse_to_ast;

mod component_lookup;
mod component_reference_identity;
mod encapsulation;
mod extends_resolution;
mod external_objects;
mod function_calls;
mod imports;
mod inherited_lookup;
mod loops_and_scopes;
mod partial_replaceable;
mod redeclare_modifiers;
mod semantic_rules;

fn resolve_test_source(source: &str) -> Result<ResolvedTree, Diagnostics> {
    resolve(parsed_tree_from_source(source))
}

fn parsed_tree_from_source(source: &str) -> ParsedTree {
    let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
    let mut tree = ClassTree::from_parsed(ast);
    tree.source_map.add("test.mo", source);
    ParsedTree::new(tree)
}

fn resolve_parsed_tree_source(source: &str) -> Result<ResolvedTree, Diagnostics> {
    resolve(parsed_tree_from_source(source))
}

fn resolve_tree_source(source: &str) -> ResolvedTree {
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_ok(), "resolution should succeed");
    match result {
        Ok(tree) => tree,
        Err(_) => unreachable!("resolution result was checked above"),
    }
}

fn find_comp_ref_def_id(expr: &rumoca_ir_ast::Expression) -> Option<DefId> {
    match expr {
        ast::Expression::ComponentReference(cr) => cr.def_id,
        ast::Expression::Binary { lhs, rhs, .. } => {
            find_comp_ref_def_id(lhs).or_else(|| find_comp_ref_def_id(rhs))
        }
        ast::Expression::Unary { rhs, .. } => find_comp_ref_def_id(rhs),
        ast::Expression::Range {
            start, step, end, ..
        } => find_comp_ref_def_id(start)
            .or_else(|| step.as_ref().and_then(|s| find_comp_ref_def_id(s)))
            .or_else(|| find_comp_ref_def_id(end)),
        ast::Expression::FunctionCall { comp, args, .. } => comp
            .def_id
            .or_else(|| args.iter().find_map(find_comp_ref_def_id)),
        ast::Expression::ClassModification {
            target,
            modifications,
            ..
        } => target
            .def_id
            .or_else(|| modifications.iter().find_map(find_comp_ref_def_id)),
        ast::Expression::NamedArgument { value, .. } => find_comp_ref_def_id(value),
        ast::Expression::Modification { target, value, .. } => {
            target.def_id.or_else(|| find_comp_ref_def_id(value))
        }
        ast::Expression::Array { elements, .. } | ast::Expression::Tuple { elements, .. } => {
            elements.iter().find_map(find_comp_ref_def_id)
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => branches
            .iter()
            .find_map(|(cond, value)| {
                find_comp_ref_def_id(cond).or_else(|| find_comp_ref_def_id(value))
            })
            .or_else(|| find_comp_ref_def_id(else_branch)),
        ast::Expression::Parenthesized { inner, .. } => find_comp_ref_def_id(inner),
        ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => find_comp_ref_def_id(expr)
            .or_else(|| {
                indices
                    .iter()
                    .find_map(|idx| find_comp_ref_def_id(&idx.range))
            })
            .or_else(|| filter.as_ref().and_then(|f| find_comp_ref_def_id(f))),
        ast::Expression::ArrayIndex {
            base, subscripts, ..
        } => find_comp_ref_def_id(base).or_else(|| {
            subscripts.iter().find_map(|sub| match sub {
                rumoca_ir_ast::Subscript::Expression(expr) => find_comp_ref_def_id(expr),
                rumoca_ir_ast::Subscript::Range { .. } => None,
                rumoca_ir_ast::Subscript::Empty => None,
            })
        }),
        ast::Expression::FieldAccess { base, .. } => find_comp_ref_def_id(base),
        ast::Expression::Empty { .. } | ast::Expression::Terminal { .. } => None,
    }
}
