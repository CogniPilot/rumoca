//! AST visitor and transformer traits.
//!
//! These modules provide reusable, representation-local AST traversal helpers.
//! Read-only traversal and lightweight query helpers are kept separate from
//! rewrite-shape traits so IR helper behavior stays explicit.

mod query;
mod read_only;
mod rewrite;
#[cfg(test)]
mod tests;

pub use query::{collect_component_refs, contains_component_ref, contains_function_call};
pub use read_only::{
    ComponentReferenceContext, ExpressionContext, FunctionCallContext, NameContext,
    SubscriptContext, TypeNameContext, VisitScope, Visitor, walk_class_def_default,
    walk_component_default, walk_component_reference_default, walk_equation_default,
    walk_expr_function_call_ctx_default, walk_expression_default, walk_extend_default,
    walk_statement_default,
};
pub use rewrite::ExpressionTransformer;
