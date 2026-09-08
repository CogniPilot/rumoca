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

pub use query::{
    RequiredValueViolation, RequiredValueViolationKind, collect_component_refs,
    contains_component_ref, contains_function_call, declaration_subscript_required_value_violation,
    equation_contains_required_recovery, equation_required_value_violation,
    expression_component_path, expression_contains_required_recovery,
    expression_required_value_violation, is_invocation_tuple_equation,
    modifier_required_value_violation, statement_contains_required_recovery,
    statement_required_value_violation, subscript_required_value_violation,
};
pub use read_only::{
    ComponentReferenceContext, ExpressionContext, FunctionCallContext, NameContext,
    SubscriptContext, TypeNameContext, VisitScope, Visitor, walk_class_def_default,
    walk_component_default, walk_component_reference_default, walk_equation_default,
    walk_expr_function_call_ctx_default, walk_expression_default, walk_extend_default,
    walk_statement_default,
};
pub use rewrite::{
    CalleeSite, ComponentReferencePartView, ComponentReferenceSite, ComponentReferenceView,
    ExpressionTransformer, IteratorStep, PartIdentitySlot, SemanticReferenceEditor,
    schedule_comprehension_iterators_mut, schedule_loop_iterators_mut,
    substitute_integer_loop_index, transform_callee_in_place,
    transform_component_reference_in_place, transform_expression_in_place,
    transform_for_index_in_place, transform_subscripts_in_place,
};
