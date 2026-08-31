//! Source-location-independent declaration identity used by semantic phases.

use crate::{
    ClassDef, Component, ComponentReference, Connection, Equation, EquationBlock, Expression,
    Extend, ExtendModification, ExternalFunction, ForIndex, Import, Name, Statement,
    StatementBlock, Subscript,
};
use rumoca_core::{Causality, DefId, Token, Variability};

/// Return whether two components denote the same inherited declaration.
///
/// MLS §5.6 permits an element inherited through multiple paths to contribute
/// once. Distinct declaration IDs are compatible only when their complete
/// semantic declarations agree. Source spans, descriptions, and cached display
/// data are deliberately absent from this comparison.
pub fn components_are_semantically_compatible(existing: &Component, incoming: &Component) -> bool {
    enforce_component_field_inventory(existing);
    enforce_component_field_inventory(incoming);
    if same_present_def_id(existing.def_id, incoming.def_id) {
        return true;
    }

    component_types_are_equivalent(existing, incoming)
        && component_prefixes_are_equivalent(existing, incoming)
        && component_values_are_equivalent(existing, incoming)
        && component_modifiers_are_equivalent(existing, incoming)
}

/// Return whether two inherited child classes have the same declaration.
///
/// This is a typed comparison of the AST declaration, not a comparison of its
/// Modelica display rendering. In particular, function purity, external
/// clauses, visibility/redeclaration prefixes, resolved references, and every
/// executable section participate even when a formatter omits them.
pub fn classes_are_semantically_compatible(existing: &ClassDef, incoming: &ClassDef) -> bool {
    enforce_class_field_inventory(existing);
    enforce_class_field_inventory(incoming);
    if same_present_def_id(existing.def_id, incoming.def_id) {
        return true;
    }

    class_headers_are_equivalent(existing, incoming)
        && slices_are_equivalent(&existing.extends, &incoming.extends, extends_are_equivalent)
        && unordered_slices_are_equivalent(
            &existing.imports,
            &incoming.imports,
            imports_are_equivalent,
        )
        && named_classes_are_equivalent(existing, incoming)
        && named_components_are_equivalent(existing, incoming)
        && equations_are_equivalent(&existing.equations, &incoming.equations)
        && equations_are_equivalent(&existing.initial_equations, &incoming.initial_equations)
        && statement_sections_are_equivalent(&existing.algorithms, &incoming.algorithms)
        && statement_sections_are_equivalent(
            &existing.initial_algorithms,
            &incoming.initial_algorithms,
        )
        && enum_literals_are_equivalent(existing, incoming)
        && expressions_are_equivalent(&existing.annotation, &incoming.annotation)
        && optional_external_is_equivalent(existing, incoming)
}

/// Exhaustive field inventory: adding a `ClassDef` field makes this module fail
/// to compile until identity explicitly classifies it as semantic or source-only.
fn enforce_class_field_inventory(class: &ClassDef) {
    let ClassDef {
        def_id: _,
        scope_id: _,
        name: _,
        class_type: _,
        class_type_token: _,
        encapsulated: _,
        partial: _,
        expandable: _,
        operator_record: _,
        pure: _,
        purity_declared: _,
        causality: _,
        description: _,
        location: _,
        extends: _,
        imports: _,
        classes: _,
        components: _,
        equations: _,
        initial_equations: _,
        algorithms: _,
        initial_algorithms: _,
        equation_keyword: _,
        initial_equation_keyword: _,
        algorithm_keyword: _,
        initial_algorithm_keyword: _,
        end_name_token: _,
        enum_literals: _,
        annotation: _,
        is_protected: _,
        is_final: _,
        is_inner: _,
        is_outer: _,
        is_replaceable: _,
        is_redeclare: _,
        redeclare_target_def_id: _,
        constrainedby: _,
        array_subscripts: _,
        external: _,
    } = class;
}

/// Component counterpart of [`enforce_class_field_inventory`].
fn enforce_component_field_inventory(component: &Component) {
    let Component {
        def_id: _,
        type_id: _,
        type_def_id: _,
        name: _,
        name_token: _,
        type_name: _,
        variability: _,
        causality: _,
        connection: _,
        description: _,
        start: _,
        start_is_modification: _,
        start_has_each: _,
        has_explicit_binding: _,
        binding: _,
        shape: _,
        shape_expr: _,
        shape_is_modification: _,
        annotation: _,
        source_modifications: _,
        source_modification_each_flags: _,
        source_modification_final_flags: _,
        source_modification_redeclare_flags: _,
        modifications: _,
        location: _,
        condition: _,
        inner: _,
        outer: _,
        final_attributes: _,
        each_modifications: _,
        is_protected: _,
        is_final: _,
        is_replaceable: _,
        is_redeclare: _,
        redeclared_by_modification: _,
        constrainedby: _,
        is_structural: _,
    } = component;
}

fn same_present_def_id(existing: Option<DefId>, incoming: Option<DefId>) -> bool {
    matches!((existing, incoming), (Some(existing), Some(incoming)) if existing == incoming)
}

fn class_headers_are_equivalent(existing: &ClassDef, incoming: &ClassDef) -> bool {
    token_text_eq(&existing.name, &incoming.name)
        && existing.class_type == incoming.class_type
        && existing.encapsulated == incoming.encapsulated
        && existing.partial == incoming.partial
        && existing.expandable == incoming.expandable
        && existing.operator_record == incoming.operator_record
        && existing.pure == incoming.pure
        && existing.purity_declared == incoming.purity_declared
        && causality_eq(&existing.causality, &incoming.causality)
        && existing.is_protected == incoming.is_protected
        && existing.is_final == incoming.is_final
        && existing.is_inner == incoming.is_inner
        && existing.is_outer == incoming.is_outer
        && existing.is_replaceable == incoming.is_replaceable
        && existing.is_redeclare == incoming.is_redeclare
        && existing.redeclare_target_def_id == incoming.redeclare_target_def_id
        && optional_names_are_equivalent(
            existing.constrainedby.as_ref(),
            incoming.constrainedby.as_ref(),
        )
        && subscripts_are_equivalent(&existing.array_subscripts, &incoming.array_subscripts)
}

fn named_classes_are_equivalent(existing: &ClassDef, incoming: &ClassDef) -> bool {
    existing.classes.len() == incoming.classes.len()
        && existing.classes.iter().all(|(name, existing)| {
            incoming
                .classes
                .get(name)
                .is_some_and(|incoming| classes_are_semantically_compatible(existing, incoming))
        })
}

fn named_components_are_equivalent(existing: &ClassDef, incoming: &ClassDef) -> bool {
    if matches!(
        existing.class_type,
        rumoca_core::ClassType::Function | rumoca_core::ClassType::Record
    ) {
        return ordered_components_are_equivalent(existing, incoming);
    }
    existing.components.len() == incoming.components.len()
        && existing.components.iter().all(|(name, existing)| {
            incoming
                .components
                .get(name)
                .is_some_and(|incoming| components_are_semantically_compatible(existing, incoming))
        })
}

/// Function parameter/result position and record constructor/field position are
/// semantic (MLS FUNC-018 and record construction), so those kinds retain order.
fn ordered_components_are_equivalent(existing: &ClassDef, incoming: &ClassDef) -> bool {
    existing.components.len() == incoming.components.len()
        && existing.components.iter().zip(&incoming.components).all(
            |((existing_name, existing), (incoming_name, incoming))| {
                existing_name == incoming_name
                    && components_are_semantically_compatible(existing, incoming)
            },
        )
}

fn component_types_are_equivalent(existing: &Component, incoming: &Component) -> bool {
    resolved_or_structured_name_eq(
        existing.type_def_id,
        &existing.type_name,
        incoming.type_def_id,
        &incoming.type_name,
    )
}

fn component_prefixes_are_equivalent(existing: &Component, incoming: &Component) -> bool {
    variability_eq(&existing.variability, &incoming.variability)
        && causality_eq(&existing.causality, &incoming.causality)
        && connection_eq(&existing.connection, &incoming.connection)
        && existing.inner == incoming.inner
        && existing.outer == incoming.outer
        && existing.is_final == incoming.is_final
        && existing.is_replaceable == incoming.is_replaceable
        && existing.is_redeclare == incoming.is_redeclare
        && optional_names_are_equivalent(
            existing.constrainedby.as_ref(),
            incoming.constrainedby.as_ref(),
        )
}

fn variability_eq(existing: &Variability, incoming: &Variability) -> bool {
    matches!(
        (existing, incoming),
        (Variability::Empty, Variability::Empty)
            | (Variability::Constant(_), Variability::Constant(_))
            | (Variability::Discrete(_), Variability::Discrete(_))
            | (Variability::Parameter(_), Variability::Parameter(_))
            | (Variability::Continuous(_), Variability::Continuous(_))
    )
}

fn causality_eq(existing: &Causality, incoming: &Causality) -> bool {
    matches!(
        (existing, incoming),
        (Causality::Empty, Causality::Empty)
            | (Causality::Input(_), Causality::Input(_))
            | (Causality::Output(_), Causality::Output(_))
    )
}

fn connection_eq(existing: &Connection, incoming: &Connection) -> bool {
    matches!(
        (existing, incoming),
        (Connection::Empty, Connection::Empty)
            | (Connection::Flow(_), Connection::Flow(_))
            | (Connection::Stream(_), Connection::Stream(_))
    )
}

fn component_values_are_equivalent(existing: &Component, incoming: &Component) -> bool {
    existing.has_explicit_binding == incoming.has_explicit_binding
        && optional_expression_eq(existing.binding.as_ref(), incoming.binding.as_ref())
        && expression_is_equivalent(&existing.start, &incoming.start)
        && existing.start_is_modification == incoming.start_is_modification
        && existing.start_has_each == incoming.start_has_each
        && existing.shape == incoming.shape
        && subscripts_are_equivalent(&existing.shape_expr, &incoming.shape_expr)
        && existing.shape_is_modification == incoming.shape_is_modification
        && optional_expression_eq(existing.condition.as_ref(), incoming.condition.as_ref())
}

fn component_modifiers_are_equivalent(existing: &Component, incoming: &Component) -> bool {
    ordered_expression_map_is_equivalent(&existing.modifications, &incoming.modifications)
        && existing.final_attributes == incoming.final_attributes
        && existing.each_modifications == incoming.each_modifications
}

fn ordered_expression_map_is_equivalent(
    existing: &crate::AstIndexMap<String, Expression>,
    incoming: &crate::AstIndexMap<String, Expression>,
) -> bool {
    existing.len() == incoming.len()
        && existing.iter().all(|(name, existing)| {
            incoming
                .get(name)
                .is_some_and(|incoming| expression_is_equivalent(existing, incoming))
        })
}

fn resolved_or_structured_name_eq(
    existing_id: Option<DefId>,
    existing: &Name,
    incoming_id: Option<DefId>,
    incoming: &Name,
) -> bool {
    match (existing_id, incoming_id) {
        (Some(existing), Some(incoming)) => existing == incoming,
        _ => name_parts_are_equivalent(existing, incoming),
    }
}

fn optional_names_are_equivalent(existing: Option<&Name>, incoming: Option<&Name>) -> bool {
    match (existing, incoming) {
        (Some(existing), Some(incoming)) => names_are_equivalent(existing, incoming),
        (None, None) => true,
        _ => false,
    }
}

fn names_are_equivalent(existing: &Name, incoming: &Name) -> bool {
    resolved_or_structured_name_eq(existing.def_id, existing, incoming.def_id, incoming)
}

fn name_parts_are_equivalent(existing: &Name, incoming: &Name) -> bool {
    slices_are_equivalent(&existing.name, &incoming.name, token_text_eq)
}

fn token_text_eq(existing: &Token, incoming: &Token) -> bool {
    existing.text == incoming.text
}

fn optional_expression_eq(existing: Option<&Expression>, incoming: Option<&Expression>) -> bool {
    match (existing, incoming) {
        (Some(existing), Some(incoming)) => expression_is_equivalent(existing, incoming),
        (None, None) => true,
        _ => false,
    }
}

fn expressions_are_equivalent(existing: &[Expression], incoming: &[Expression]) -> bool {
    slices_are_equivalent(existing, incoming, expression_is_equivalent)
}

// SPEC_0021: Exception - exhaustive structural comparison over every AST expression variant.
#[allow(clippy::too_many_lines)]
fn expression_is_equivalent(existing: &Expression, incoming: &Expression) -> bool {
    match (existing, incoming) {
        (Expression::Empty { .. }, Expression::Empty { .. }) => true,
        (
            Expression::Range {
                start: existing_start,
                step: existing_step,
                end: existing_end,
                ..
            },
            Expression::Range {
                start: incoming_start,
                step: incoming_step,
                end: incoming_end,
                ..
            },
        ) => {
            expression_is_equivalent(existing_start, incoming_start)
                && optional_arc_expression_eq(existing_step.as_ref(), incoming_step.as_ref())
                && expression_is_equivalent(existing_end, incoming_end)
        }
        (
            Expression::Unary {
                op: existing_op,
                rhs: existing_rhs,
                ..
            },
            Expression::Unary {
                op: incoming_op,
                rhs: incoming_rhs,
                ..
            },
        ) => existing_op == incoming_op && expression_is_equivalent(existing_rhs, incoming_rhs),
        (
            Expression::Binary {
                op: existing_op,
                lhs: existing_lhs,
                rhs: existing_rhs,
                ..
            },
            Expression::Binary {
                op: incoming_op,
                lhs: incoming_lhs,
                rhs: incoming_rhs,
                ..
            },
        ) => {
            existing_op == incoming_op
                && expression_is_equivalent(existing_lhs, incoming_lhs)
                && expression_is_equivalent(existing_rhs, incoming_rhs)
        }
        (
            Expression::Terminal {
                terminal_type: existing_type,
                token: existing_token,
                ..
            },
            Expression::Terminal {
                terminal_type: incoming_type,
                token: incoming_token,
                ..
            },
        ) => existing_type == incoming_type && token_text_eq(existing_token, incoming_token),
        (Expression::ComponentReference(existing), Expression::ComponentReference(incoming)) => {
            component_references_are_equivalent(existing, incoming)
        }
        (
            Expression::FunctionCall {
                comp: existing_comp,
                args: existing_args,
                is_partial_application: existing_partial,
                ..
            },
            Expression::FunctionCall {
                comp: incoming_comp,
                args: incoming_args,
                is_partial_application: incoming_partial,
                ..
            },
        ) => {
            existing_partial == incoming_partial
                && component_references_are_equivalent(existing_comp, incoming_comp)
                && expressions_are_equivalent(existing_args, incoming_args)
        }
        (
            Expression::ClassModification {
                target: existing_target,
                modifications: existing_modifications,
                each_flags: existing_each,
                final_flags: existing_final,
                redeclare_flags: existing_redeclare,
                ..
            },
            Expression::ClassModification {
                target: incoming_target,
                modifications: incoming_modifications,
                each_flags: incoming_each,
                final_flags: incoming_final,
                redeclare_flags: incoming_redeclare,
                ..
            },
        ) => {
            component_references_are_equivalent(existing_target, incoming_target)
                && expressions_are_equivalent(existing_modifications, incoming_modifications)
                && existing_each == incoming_each
                && existing_final == incoming_final
                && existing_redeclare == incoming_redeclare
        }
        (
            Expression::NamedArgument {
                name: existing_name,
                value: existing_value,
                ..
            },
            Expression::NamedArgument {
                name: incoming_name,
                value: incoming_value,
                ..
            },
        ) => {
            token_text_eq(existing_name, incoming_name)
                && expression_is_equivalent(existing_value, incoming_value)
        }
        (
            Expression::Modification {
                target: existing_target,
                value: existing_value,
                ..
            },
            Expression::Modification {
                target: incoming_target,
                value: incoming_value,
                ..
            },
        ) => {
            component_references_are_equivalent(existing_target, incoming_target)
                && expression_is_equivalent(existing_value, incoming_value)
        }
        (
            Expression::Array {
                elements: existing_elements,
                is_matrix: existing_matrix,
                ..
            },
            Expression::Array {
                elements: incoming_elements,
                is_matrix: incoming_matrix,
                ..
            },
        ) => {
            existing_matrix == incoming_matrix
                && expressions_are_equivalent(existing_elements, incoming_elements)
        }
        (
            Expression::Tuple {
                elements: existing, ..
            },
            Expression::Tuple {
                elements: incoming, ..
            },
        ) => expressions_are_equivalent(existing, incoming),
        (
            Expression::If {
                branches: existing_branches,
                else_branch: existing_else,
                ..
            },
            Expression::If {
                branches: incoming_branches,
                else_branch: incoming_else,
                ..
            },
        ) => {
            expression_pairs_are_equivalent(existing_branches, incoming_branches)
                && expression_is_equivalent(existing_else, incoming_else)
        }
        (
            Expression::Parenthesized {
                inner: existing, ..
            },
            Expression::Parenthesized {
                inner: incoming, ..
            },
        ) => expression_is_equivalent(existing, incoming),
        (
            Expression::ArrayComprehension {
                expr: existing_expr,
                indices: existing_indices,
                filter: existing_filter,
                ..
            },
            Expression::ArrayComprehension {
                expr: incoming_expr,
                indices: incoming_indices,
                filter: incoming_filter,
                ..
            },
        ) => {
            expression_is_equivalent(existing_expr, incoming_expr)
                && for_indices_are_equivalent(existing_indices, incoming_indices)
                && optional_arc_expression_eq(existing_filter.as_ref(), incoming_filter.as_ref())
        }
        (
            Expression::ArrayIndex {
                base: existing_base,
                subscripts: existing_subscripts,
                ..
            },
            Expression::ArrayIndex {
                base: incoming_base,
                subscripts: incoming_subscripts,
                ..
            },
        ) => {
            expression_is_equivalent(existing_base, incoming_base)
                && subscripts_are_equivalent(existing_subscripts, incoming_subscripts)
        }
        (
            Expression::FieldAccess {
                base: existing_base,
                field: existing_field,
                field_def_id: existing_id,
                ..
            },
            Expression::FieldAccess {
                base: incoming_base,
                field: incoming_field,
                field_def_id: incoming_id,
                ..
            },
        ) => {
            expression_is_equivalent(existing_base, incoming_base)
                && resolved_or_spelled_eq(
                    *existing_id,
                    existing_field,
                    *incoming_id,
                    incoming_field,
                )
        }
        _ => false,
    }
}

fn optional_arc_expression_eq(
    existing: Option<&std::sync::Arc<Expression>>,
    incoming: Option<&std::sync::Arc<Expression>>,
) -> bool {
    match (existing, incoming) {
        (Some(existing), Some(incoming)) => expression_is_equivalent(existing, incoming),
        (None, None) => true,
        _ => false,
    }
}

fn expression_pairs_are_equivalent(
    existing: &[(Expression, Expression)],
    incoming: &[(Expression, Expression)],
) -> bool {
    slices_are_equivalent(existing, incoming, |existing, incoming| {
        expression_is_equivalent(&existing.0, &incoming.0)
            && expression_is_equivalent(&existing.1, &incoming.1)
    })
}

fn component_references_are_equivalent(
    existing: &ComponentReference,
    incoming: &ComponentReference,
) -> bool {
    existing.local == incoming.local
        && slices_are_equivalent(&existing.parts, &incoming.parts, |existing, incoming| {
            resolved_or_spelled_eq(
                existing.def_id,
                existing.ident.text.as_ref(),
                incoming.def_id,
                incoming.ident.text.as_ref(),
            ) && optional_subscripts_are_equivalent(
                existing.subs.as_deref(),
                incoming.subs.as_deref(),
            )
        })
}

fn resolved_or_spelled_eq(
    existing_id: Option<DefId>,
    existing: &str,
    incoming_id: Option<DefId>,
    incoming: &str,
) -> bool {
    match (existing_id, incoming_id) {
        (Some(existing), Some(incoming)) => existing == incoming,
        (None, None) => existing == incoming,
        _ => false,
    }
}

fn optional_subscripts_are_equivalent(
    existing: Option<&[Subscript]>,
    incoming: Option<&[Subscript]>,
) -> bool {
    match (existing, incoming) {
        (Some(existing), Some(incoming)) => subscripts_are_equivalent(existing, incoming),
        (None, None) => true,
        _ => false,
    }
}

fn subscripts_are_equivalent(existing: &[Subscript], incoming: &[Subscript]) -> bool {
    slices_are_equivalent(existing, incoming, subscript_is_equivalent)
}

fn subscript_is_equivalent(existing: &Subscript, incoming: &Subscript) -> bool {
    match (existing, incoming) {
        (Subscript::Empty, Subscript::Empty)
        | (Subscript::Range { .. }, Subscript::Range { .. }) => true,
        (Subscript::Expression(existing), Subscript::Expression(incoming)) => {
            expression_is_equivalent(existing, incoming)
        }
        _ => false,
    }
}

fn for_indices_are_equivalent(existing: &[ForIndex], incoming: &[ForIndex]) -> bool {
    slices_are_equivalent(existing, incoming, |existing, incoming| {
        token_text_eq(&existing.ident, &incoming.ident)
            && expression_is_equivalent(&existing.range, &incoming.range)
    })
}

fn extends_are_equivalent(existing: &Extend, incoming: &Extend) -> bool {
    names_are_equivalent(&existing.base_name, &incoming.base_name)
        && existing.base_def_id == incoming.base_def_id
        && slices_are_equivalent(
            &existing.modifications,
            &incoming.modifications,
            extend_modifications_are_equivalent,
        )
        && existing.break_names == incoming.break_names
        && existing.is_protected == incoming.is_protected
        && expressions_are_equivalent(&existing.annotation, &incoming.annotation)
}

fn extend_modifications_are_equivalent(
    existing: &ExtendModification,
    incoming: &ExtendModification,
) -> bool {
    expression_is_equivalent(&existing.expr, &incoming.expr)
        && existing.each == incoming.each
        && existing.final_ == incoming.final_
        && existing.redeclare == incoming.redeclare
}

fn imports_are_equivalent(existing: &Import, incoming: &Import) -> bool {
    match (existing, incoming) {
        (
            Import::Qualified {
                path: existing_path,
                global_scope: existing_global,
                ..
            },
            Import::Qualified {
                path: incoming_path,
                global_scope: incoming_global,
                ..
            },
        )
        | (
            Import::Unqualified {
                path: existing_path,
                global_scope: existing_global,
                ..
            },
            Import::Unqualified {
                path: incoming_path,
                global_scope: incoming_global,
                ..
            },
        ) => {
            existing_global == incoming_global && names_are_equivalent(existing_path, incoming_path)
        }
        (
            Import::Renamed {
                alias: existing_alias,
                path: existing_path,
                global_scope: existing_global,
                ..
            },
            Import::Renamed {
                alias: incoming_alias,
                path: incoming_path,
                global_scope: incoming_global,
                ..
            },
        ) => {
            existing_global == incoming_global
                && token_text_eq(existing_alias, incoming_alias)
                && names_are_equivalent(existing_path, incoming_path)
        }
        (
            Import::Selective {
                path: existing_path,
                names: existing_names,
                global_scope: existing_global,
                ..
            },
            Import::Selective {
                path: incoming_path,
                names: incoming_names,
                global_scope: incoming_global,
                ..
            },
        ) => {
            existing_global == incoming_global
                && names_are_equivalent(existing_path, incoming_path)
                && slices_are_equivalent(existing_names, incoming_names, token_text_eq)
        }
        _ => false,
    }
}

fn equations_are_equivalent(existing: &[Equation], incoming: &[Equation]) -> bool {
    slices_are_equivalent(existing, incoming, equation_is_equivalent)
}

fn equation_is_equivalent(existing: &Equation, incoming: &Equation) -> bool {
    match (existing, incoming) {
        (Equation::Empty, Equation::Empty) => true,
        (
            Equation::Simple {
                lhs: existing_lhs,
                rhs: existing_rhs,
            },
            Equation::Simple {
                lhs: incoming_lhs,
                rhs: incoming_rhs,
            },
        ) => {
            expression_is_equivalent(existing_lhs, incoming_lhs)
                && expression_is_equivalent(existing_rhs, incoming_rhs)
        }
        (
            Equation::Connect {
                lhs: existing_lhs,
                rhs: existing_rhs,
            },
            Equation::Connect {
                lhs: incoming_lhs,
                rhs: incoming_rhs,
            },
        ) => {
            component_references_are_equivalent(existing_lhs, incoming_lhs)
                && component_references_are_equivalent(existing_rhs, incoming_rhs)
        }
        (
            Equation::For {
                indices: existing_indices,
                equations: existing_equations,
            },
            Equation::For {
                indices: incoming_indices,
                equations: incoming_equations,
            },
        ) => {
            for_indices_are_equivalent(existing_indices, incoming_indices)
                && equations_are_equivalent(existing_equations, incoming_equations)
        }
        (Equation::When(existing), Equation::When(incoming)) => {
            equation_blocks_are_equivalent(existing, incoming)
        }
        (
            Equation::If {
                cond_blocks: existing_blocks,
                else_block: existing_else,
            },
            Equation::If {
                cond_blocks: incoming_blocks,
                else_block: incoming_else,
            },
        ) => {
            equation_blocks_are_equivalent(existing_blocks, incoming_blocks)
                && optional_equations_are_equivalent(
                    existing_else.as_deref(),
                    incoming_else.as_deref(),
                )
        }
        (
            Equation::FunctionCall {
                comp: existing_comp,
                args: existing_args,
                ..
            },
            Equation::FunctionCall {
                comp: incoming_comp,
                args: incoming_args,
                ..
            },
        ) => {
            component_references_are_equivalent(existing_comp, incoming_comp)
                && expressions_are_equivalent(existing_args, incoming_args)
        }
        (
            Equation::Assert {
                condition: existing_condition,
                message: existing_message,
                level: existing_level,
            },
            Equation::Assert {
                condition: incoming_condition,
                message: incoming_message,
                level: incoming_level,
            },
        ) => {
            expression_is_equivalent(existing_condition, incoming_condition)
                && expression_is_equivalent(existing_message, incoming_message)
                && optional_expression_eq(existing_level.as_ref(), incoming_level.as_ref())
        }
        _ => false,
    }
}

fn equation_blocks_are_equivalent(existing: &[EquationBlock], incoming: &[EquationBlock]) -> bool {
    slices_are_equivalent(existing, incoming, |existing, incoming| {
        expression_is_equivalent(&existing.cond, &incoming.cond)
            && equations_are_equivalent(&existing.eqs, &incoming.eqs)
    })
}

fn optional_equations_are_equivalent(
    existing: Option<&[Equation]>,
    incoming: Option<&[Equation]>,
) -> bool {
    match (existing, incoming) {
        (Some(existing), Some(incoming)) => equations_are_equivalent(existing, incoming),
        (None, None) => true,
        _ => false,
    }
}

fn statement_sections_are_equivalent(
    existing: &[Vec<Statement>],
    incoming: &[Vec<Statement>],
) -> bool {
    slices_are_equivalent(existing, incoming, |existing, incoming| {
        statements_are_equivalent(existing, incoming)
    })
}

fn statements_are_equivalent(existing: &[Statement], incoming: &[Statement]) -> bool {
    slices_are_equivalent(existing, incoming, statement_is_equivalent)
}

/// Structural equivalence for the statement forms that nest other statements.
///
/// A pair of different variants never matches an arm here, so it falls through
/// to the operand-only forms below; the two functions together decide the whole
/// statement grammar, and a pair that neither decides is not equivalent.
fn statement_is_equivalent(existing: &Statement, incoming: &Statement) -> bool {
    match (existing, incoming) {
        (Statement::Empty, Statement::Empty)
        | (Statement::Return { .. }, Statement::Return { .. })
        | (Statement::Break { .. }, Statement::Break { .. }) => true,
        (
            Statement::For {
                indices: existing_indices,
                equations: existing_statements,
            },
            Statement::For {
                indices: incoming_indices,
                equations: incoming_statements,
            },
        ) => {
            for_indices_are_equivalent(existing_indices, incoming_indices)
                && statements_are_equivalent(existing_statements, incoming_statements)
        }
        (Statement::While(existing), Statement::While(incoming)) => {
            statement_blocks_are_equivalent(
                std::slice::from_ref(existing),
                std::slice::from_ref(incoming),
            )
        }
        (
            Statement::If {
                cond_blocks: existing_blocks,
                else_block: existing_else,
            },
            Statement::If {
                cond_blocks: incoming_blocks,
                else_block: incoming_else,
            },
        ) => {
            statement_blocks_are_equivalent(existing_blocks, incoming_blocks)
                && optional_statements_are_equivalent(
                    existing_else.as_deref(),
                    incoming_else.as_deref(),
                )
        }
        (Statement::When(existing), Statement::When(incoming)) => {
            statement_blocks_are_equivalent(existing, incoming)
        }
        _ => operand_statement_is_equivalent(existing, incoming),
    }
}

/// Structural equivalence for the statement forms whose meaning is carried
/// entirely by their own operands, with no nested statement block.
fn operand_statement_is_equivalent(existing: &Statement, incoming: &Statement) -> bool {
    match (existing, incoming) {
        (
            Statement::Assignment {
                comp: existing_comp,
                value: existing_value,
            },
            Statement::Assignment {
                comp: incoming_comp,
                value: incoming_value,
            },
        ) => {
            component_references_are_equivalent(existing_comp, incoming_comp)
                && expression_is_equivalent(existing_value, incoming_value)
        }
        (
            Statement::FunctionCall {
                comp: existing_comp,
                args: existing_args,
                outputs: existing_outputs,
            },
            Statement::FunctionCall {
                comp: incoming_comp,
                args: incoming_args,
                outputs: incoming_outputs,
            },
        ) => {
            component_references_are_equivalent(existing_comp, incoming_comp)
                && expressions_are_equivalent(existing_args, incoming_args)
                && expressions_are_equivalent(existing_outputs, incoming_outputs)
        }
        (
            Statement::Reinit {
                variable: existing_variable,
                value: existing_value,
            },
            Statement::Reinit {
                variable: incoming_variable,
                value: incoming_value,
            },
        ) => {
            component_references_are_equivalent(existing_variable, incoming_variable)
                && expression_is_equivalent(existing_value, incoming_value)
        }
        (
            Statement::Assert {
                condition: existing_condition,
                message: existing_message,
                level: existing_level,
            },
            Statement::Assert {
                condition: incoming_condition,
                message: incoming_message,
                level: incoming_level,
            },
        ) => {
            expression_is_equivalent(existing_condition, incoming_condition)
                && expression_is_equivalent(existing_message, incoming_message)
                && optional_expression_eq(existing_level.as_deref(), incoming_level.as_deref())
        }
        _ => false,
    }
}

fn statement_blocks_are_equivalent(
    existing: &[StatementBlock],
    incoming: &[StatementBlock],
) -> bool {
    slices_are_equivalent(existing, incoming, |existing, incoming| {
        expression_is_equivalent(&existing.cond, &incoming.cond)
            && statements_are_equivalent(&existing.stmts, &incoming.stmts)
    })
}

fn optional_statements_are_equivalent(
    existing: Option<&[Statement]>,
    incoming: Option<&[Statement]>,
) -> bool {
    match (existing, incoming) {
        (Some(existing), Some(incoming)) => statements_are_equivalent(existing, incoming),
        (None, None) => true,
        _ => false,
    }
}

fn enum_literals_are_equivalent(existing: &ClassDef, incoming: &ClassDef) -> bool {
    slices_are_equivalent(
        &existing.enum_literals,
        &incoming.enum_literals,
        |existing, incoming| token_text_eq(&existing.ident, &incoming.ident),
    )
}

fn optional_external_is_equivalent(existing: &ClassDef, incoming: &ClassDef) -> bool {
    match (existing.external.as_ref(), incoming.external.as_ref()) {
        (Some(existing_external), Some(incoming_external)) => external_functions_are_equivalent(
            &existing.name,
            existing_external,
            &incoming.name,
            incoming_external,
        ),
        (None, None) => true,
        _ => false,
    }
}

fn external_functions_are_equivalent(
    existing_class_name: &Token,
    existing: &ExternalFunction,
    incoming_class_name: &Token,
    incoming: &ExternalFunction,
) -> bool {
    effective_external_language(existing) == effective_external_language(incoming)
        && effective_external_name(existing_class_name, existing)
            == effective_external_name(incoming_class_name, incoming)
        && optional_component_reference_eq(existing.output.as_ref(), incoming.output.as_ref())
        && expressions_are_equivalent(&existing.args, &incoming.args)
        && expressions_are_equivalent(&existing.annotation, &incoming.annotation)
}

fn effective_external_language(external: &ExternalFunction) -> &str {
    external.language.as_deref().unwrap_or("C")
}

fn effective_external_name<'a>(class_name: &'a Token, external: &'a ExternalFunction) -> &'a str {
    external
        .function_name
        .as_ref()
        .map_or(class_name.text.as_ref(), |name| name.text.as_ref())
}

fn optional_component_reference_eq(
    existing: Option<&ComponentReference>,
    incoming: Option<&ComponentReference>,
) -> bool {
    match (existing, incoming) {
        (Some(existing), Some(incoming)) => component_references_are_equivalent(existing, incoming),
        (None, None) => true,
        _ => false,
    }
}

/// Compare declarative collections whose source order has no semantic effect,
/// while retaining duplicate multiplicity.
fn unordered_slices_are_equivalent<T, F>(existing: &[T], incoming: &[T], equivalent: F) -> bool
where
    F: Fn(&T, &T) -> bool,
{
    if existing.len() != incoming.len() {
        return false;
    }
    let mut matched = vec![false; incoming.len()];
    existing.iter().all(|existing| {
        let Some(index) = incoming.iter().enumerate().find_map(|(index, incoming)| {
            (!matched[index] && equivalent(existing, incoming)).then_some(index)
        }) else {
            return false;
        };
        matched[index] = true;
        true
    })
}

fn slices_are_equivalent<T, F>(existing: &[T], incoming: &[T], mut equivalent: F) -> bool
where
    F: FnMut(&T, &T) -> bool,
{
    existing.len() == incoming.len()
        && existing
            .iter()
            .zip(incoming)
            .all(|(existing, incoming)| equivalent(existing, incoming))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{ClassType, Span};
    use std::sync::Arc;

    fn token(text: &str) -> Token {
        Token {
            text: Arc::from(text),
            ..Token::default()
        }
    }

    fn function(name: &str) -> ClassDef {
        ClassDef {
            name: token(name),
            class_type: ClassType::Function,
            pure: true,
            ..ClassDef::default()
        }
    }

    fn reference(name: &str) -> ComponentReference {
        ComponentReference {
            local: false,
            parts: vec![crate::ComponentRefPart {
                ident: token(name),
                subs: None,
                def_id: None,
            }],
            span: Span::DUMMY,
            qualified_display_name: None,
        }
    }

    fn component(name: &str) -> Component {
        Component {
            name: name.to_string(),
            name_token: token(name),
            ..Component::empty_with_span(Span::DUMMY)
        }
    }

    #[test]
    fn class_identity_ignores_source_provenance_but_not_semantics() {
        let existing = function("f");
        let mut incoming = existing.clone();
        incoming.location.start = 100;
        incoming.name.location.start = 100;

        assert!(classes_are_semantically_compatible(&existing, &incoming));
    }

    #[test]
    fn equal_display_text_cannot_hide_different_function_purity() {
        let existing = function("f");
        let mut incoming = existing.clone();
        incoming.pure = false;
        incoming.purity_declared = true;

        assert_eq!(existing.to_modelica(""), incoming.to_modelica(""));
        assert!(!classes_are_semantically_compatible(&existing, &incoming));
    }

    #[test]
    fn equal_display_text_cannot_hide_different_external_abi() {
        let mut existing = function("f");
        existing.external = Some(ExternalFunction {
            language: Some("C".to_string()),
            function_name: Some(token("left_abi")),
            output: Some(reference("y")),
            args: Vec::new(),
            annotation: Vec::new(),
        });
        let mut incoming = existing.clone();
        incoming.external.as_mut().expect("external").function_name = Some(token("right_abi"));

        assert_eq!(existing.to_modelica(""), incoming.to_modelica(""));
        assert!(!classes_are_semantically_compatible(&existing, &incoming));
    }

    #[test]
    fn implicit_and_explicit_default_external_names_are_equivalent() {
        let mut existing = function("f");
        existing.external = Some(ExternalFunction {
            language: None,
            function_name: None,
            output: None,
            args: Vec::new(),
            annotation: Vec::new(),
        });
        let mut incoming = existing.clone();
        incoming.external.as_mut().expect("external").language = Some("C".to_string());
        incoming.external.as_mut().expect("external").function_name = Some(token("f"));

        assert!(classes_are_semantically_compatible(&existing, &incoming));
    }

    #[test]
    fn equal_display_text_cannot_hide_redeclaration_attributes() {
        let existing = function("f");
        let mut incoming = existing.clone();
        incoming.is_redeclare = true;
        incoming.redeclare_target_def_id = Some(DefId::new(41));

        assert_eq!(existing.to_modelica(""), incoming.to_modelica(""));
        assert!(!classes_are_semantically_compatible(&existing, &incoming));
    }

    #[test]
    fn named_model_members_do_not_acquire_source_order_identity() {
        let mut existing = ClassDef {
            name: token("M"),
            ..ClassDef::default()
        };
        existing.components.insert("a".to_string(), component("a"));
        existing.components.insert("b".to_string(), component("b"));
        let mut incoming = existing.clone();
        incoming.components.swap_indices(0, 1);

        assert!(classes_are_semantically_compatible(&existing, &incoming));
    }

    #[test]
    fn function_parameter_order_remains_semantic() {
        let mut existing = function("f");
        existing.components.insert("a".to_string(), component("a"));
        existing.components.insert("b".to_string(), component("b"));
        let mut incoming = existing.clone();
        incoming.components.swap_indices(0, 1);

        assert!(!classes_are_semantically_compatible(&existing, &incoming));
    }
}
