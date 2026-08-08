//! Postprocessing passes over the flattened model.
//!
//! Flattening leaves references spelled the way instantiation produced them and
//! constants recorded on the context rather than folded into the equations.
//! These passes canonicalize every reference onto a variable the flat model
//! owns and then substitute the known constants, walking each place the model
//! stores an expression exactly once.

mod constant_expansion;
mod constant_lookup;
mod constant_substituter;
mod constructor_calls;
mod field_access;
mod function_shape_constants;
mod index_collapse;
mod indexed_dimension_recovery;
mod occurrence_graph;
mod record_alias;

use super::*;

pub(crate) use constant_substituter::substitute_known_constants_expr;
pub(crate) use constructor_calls::mark_record_constructor_calls;
pub(super) use field_access::{
    drop_invalid_field_access_bindings, normalize_record_array_field_access_bindings,
    resolve_nested_constructor_field_access_bindings,
};
pub(crate) use index_collapse::{collapse_index_refs_to_known_varrefs, field_access_flat_path};
pub(crate) use indexed_dimension_recovery::recover_indexed_lhs_dimensions;

use constant_substituter::{
    substitute_known_constants_expr_with_options, substitute_known_constants_statement,
};
use function_shape_constants::materialize_function_shape_constants;
// `field_access` resolves declared extents through the same compile-time bound.
use indexed_dimension_recovery::constant_integer_bound;
use record_alias::*;

pub(super) fn canonicalize_varrefs_via_record_aliases(flat: &mut flat::Model, ctx: &Context) {
    if ctx.record_aliases.is_empty() {
        return;
    }
    let known_variables: HashSet<String> = flat.variables.keys().map(ToString::to_string).collect();
    for equation in &mut flat.equations {
        canonicalize_record_alias_expr(&mut equation.residual, ctx, &known_variables);
    }
    for equation in &mut flat.initial_equations {
        canonicalize_record_alias_expr(&mut equation.residual, ctx, &known_variables);
    }
    for chain in &mut flat.when_chains {
        for branch in chain.branches_mut() {
            canonicalize_record_alias_expr(&mut branch.condition, ctx, &known_variables);
            canonicalize_record_alias_when_equations(&mut branch.equations, ctx, &known_variables);
        }
    }
    for algorithm in &mut flat.algorithms {
        canonicalize_record_alias_statements(&mut algorithm.statements, ctx, &known_variables);
    }
    for algorithm in &mut flat.initial_algorithms {
        canonicalize_record_alias_statements(&mut algorithm.statements, ctx, &known_variables);
    }
}

fn record_alias_rewrite_name(
    name: &str,
    ctx: &Context,
    known_variables: &HashSet<String>,
) -> Option<String> {
    let name_path = rumoca_core::ComponentPath::from_flat_path(name);
    ctx.record_aliases.iter().find_map(|(alias, target)| {
        if !name_path.starts_with(alias) || name_path.len() == alias.len() {
            return None;
        }
        let suffix = name_path
            .suffix_from(alias.len())
            .expect("suffix index is in range");
        let candidate = target.join(&suffix).to_flat_string();
        known_variables.contains(&candidate).then_some(candidate)
    })
}

pub(super) fn substitute_known_constants_in_flat(
    flat: &mut flat::Model,
    ctx: &Context,
) -> Result<(), FlattenError> {
    let live_vars: rustc_hash::FxHashSet<String> = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();
    let no_locals: HashSet<String> = HashSet::new();

    for eq in &mut flat.equations {
        let scope = equation_origin_scope(&eq.origin);
        eq.residual = substitute_known_constants_expr(
            eq.residual.clone(),
            ctx,
            &live_vars,
            &no_locals,
            &scope,
        )?;
    }
    for eq in &mut flat.initial_equations {
        let scope = equation_origin_scope(&eq.origin);
        eq.residual = substitute_known_constants_expr(
            eq.residual.clone(),
            ctx,
            &live_vars,
            &no_locals,
            &scope,
        )?;
    }
    substitute_structured_templates(&mut flat.structured_equations, ctx, &live_vars)?;
    substitute_structured_templates(&mut flat.initial_structured_equations, ctx, &live_vars)?;
    substitute_assert_equations(&mut flat.assert_equations, ctx, &live_vars, &no_locals)?;
    substitute_assert_equations(
        &mut flat.initial_assert_equations,
        ctx,
        &live_vars,
        &no_locals,
    )?;
    for chain in &mut flat.when_chains {
        for branch in chain.branches_mut() {
            branch.condition = substitute_known_constants_expr(
                branch.condition.clone(),
                ctx,
                &live_vars,
                &no_locals,
                "",
            )?;
            for equation in &mut branch.equations {
                substitute_known_constants_when_equation(equation, ctx, &live_vars, &no_locals)?;
            }
        }
    }
    substitute_algorithms(&mut flat.algorithms, ctx, &live_vars, &no_locals)?;
    substitute_algorithms(&mut flat.initial_algorithms, ctx, &live_vars, &no_locals)?;
    substitute_variable_annotations(&mut flat.variables, ctx, &live_vars, &no_locals)?;
    substitute_function_bodies(&mut flat.functions, ctx, &live_vars)?;
    crate::zero_sized_arrays::materialize_referenced_zero_sized_array_variables(flat, ctx)?;
    Ok(())
}

/// Substitute the canonical template view of materialized equation rows.
///
/// Templates are authoritative downstream owners, not diagnostic metadata, so
/// every compile-time constant folded in a scalar row must be folded in its
/// template peer as well. Domain binders remain lexical locals while the
/// family's typed origin provides the same declaration scope as its rows.
fn substitute_structured_templates(
    families: &mut [flat::StructuredEquationFamily],
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
) -> Result<(), FlattenError> {
    for family in families {
        let scope = equation_origin_scope(&family.origin);
        let locals = family
            .domain
            .binders
            .iter()
            .map(|binder| binder.display_name.clone())
            .collect::<HashSet<_>>();
        let Some(template) = family.template.as_mut() else {
            continue;
        };
        for body in &mut template.body {
            *body = substitute_known_constants_expr(body.clone(), ctx, live_vars, &locals, &scope)?;
        }
    }
    Ok(())
}

fn equation_origin_scope(origin: &flat::EquationOrigin) -> String {
    match origin {
        flat::EquationOrigin::ComponentEquation { component }
        | flat::EquationOrigin::Algorithm { component } => component.clone(),
        flat::EquationOrigin::Binding { variable }
        | flat::EquationOrigin::Reinit { state: variable }
        | flat::EquationOrigin::WhenAssignment { target: variable }
        | flat::EquationOrigin::UnconnectedFlow { variable } => parent_component_scope(variable),
        flat::EquationOrigin::Connection { .. } | flat::EquationOrigin::FlowSum { .. } => {
            String::new()
        }
    }
}

fn parent_component_scope(name: &str) -> String {
    rumoca_core::ComponentPath::from_flat_path(name)
        .parent()
        .unwrap_or_else(rumoca_core::ComponentPath::root)
        .to_flat_string()
}

/// Split a flat key such as `pkg.table[1,2]` into its base and its subscripts.
///
/// Flat keys spell an element of a constant array inline, so both the
/// substituter and the constant lookups have to reach the base declaration
/// through the same parse.
fn split_inline_indexed_name(name: &str) -> Option<(&str, Vec<i64>)> {
    let scalar = rumoca_core::parse_scalar_name(name)?;
    Some((scalar.base, scalar.indices))
}

fn substitute_assert_equations(
    equations: &mut [flat::AssertEquation],
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> Result<(), FlattenError> {
    for assert_eq in equations {
        let scope = equation_origin_scope(&assert_eq.origin);
        assert_eq.condition = substitute_known_constants_expr_with_options(
            assert_eq.condition.clone(),
            ctx,
            live_vars,
            locals,
            &scope,
            true,
        )?;
        assert_eq.message = substitute_known_constants_expr_with_options(
            assert_eq.message.clone(),
            ctx,
            live_vars,
            locals,
            &scope,
            true,
        )?;
        substitute_opt_expr_with_options(
            &mut assert_eq.level,
            ctx,
            live_vars,
            locals,
            &scope,
            true,
        )?;
    }
    Ok(())
}

fn substitute_algorithms(
    algorithms: &mut [flat::Algorithm],
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> Result<(), FlattenError> {
    for algorithm in algorithms {
        for statement in &mut algorithm.statements {
            substitute_known_constants_statement(statement, ctx, live_vars, locals, "")?;
        }
    }
    Ok(())
}

fn substitute_variable_annotations(
    variables: &mut flat::VarNameIndexMap<flat::Variable>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> Result<(), FlattenError> {
    for var in variables.values_mut() {
        let scope = parent_component_scope(var.name.as_str());
        substitute_opt_expr(&mut var.binding, ctx, live_vars, locals, &scope)?;
        substitute_opt_expr(&mut var.start, ctx, live_vars, locals, &scope)?;
        substitute_opt_expr(&mut var.min, ctx, live_vars, locals, &scope)?;
        substitute_opt_expr(&mut var.max, ctx, live_vars, locals, &scope)?;
        substitute_opt_expr(&mut var.nominal, ctx, live_vars, locals, &scope)?;
    }
    Ok(())
}

fn substitute_function_bodies(
    functions: &mut flat::VarNameIndexMap<rumoca_core::Function>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
) -> Result<(), FlattenError> {
    for function in functions.values_mut() {
        materialize_function_shape_constants(function, ctx)?;
        let function_locals: HashSet<String> = function
            .inputs
            .iter()
            .chain(function.outputs.iter())
            .chain(function.locals.iter())
            .map(|param| param.name.clone())
            .collect();
        let function_scope =
            crate::path_utils::enclosing_scope(function.name.as_str()).unwrap_or("");

        for param in function
            .inputs
            .iter_mut()
            .chain(function.outputs.iter_mut())
            .chain(function.locals.iter_mut())
        {
            substitute_opt_expr(
                &mut param.default,
                ctx,
                live_vars,
                &function_locals,
                function_scope,
            )?;
        }
        for statement in &mut function.body {
            substitute_known_constants_statement(
                statement,
                ctx,
                live_vars,
                &function_locals,
                function_scope,
            )?;
        }
    }
    Ok(())
}

fn substitute_opt_expr(
    expr: &mut Option<rumoca_core::Expression>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
) -> Result<(), FlattenError> {
    substitute_opt_expr_with_options(expr, ctx, live_vars, locals, scope, false)
}

fn substitute_opt_expr_with_options(
    expr: &mut Option<rumoca_core::Expression>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
    prefer_scoped_parameters: bool,
) -> Result<(), FlattenError> {
    if let Some(expr) = expr {
        *expr = substitute_known_constants_expr_with_options(
            expr.clone(),
            ctx,
            live_vars,
            locals,
            scope,
            prefer_scoped_parameters,
        )?;
    }
    Ok(())
}

fn substitute_known_constants_when_equation(
    equation: &mut flat::WhenEquation,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> Result<(), FlattenError> {
    match equation {
        flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
            *value = substitute_known_constants_expr(value.clone(), ctx, live_vars, locals, "")?;
        }
        flat::WhenEquation::Assert {
            condition,
            message,
            level,
            ..
        } => {
            *condition =
                substitute_known_constants_expr(condition.clone(), ctx, live_vars, locals, "")?;
            *message =
                substitute_known_constants_expr(message.clone(), ctx, live_vars, locals, "")?;
            if let Some(level) = level.as_deref_mut() {
                *level =
                    substitute_known_constants_expr(level.clone(), ctx, live_vars, locals, "")?;
            }
        }
        flat::WhenEquation::Terminate { message, .. } => {
            *message =
                substitute_known_constants_expr(message.clone(), ctx, live_vars, locals, "")?;
        }
        flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (condition, equations) in branches {
                *condition =
                    substitute_known_constants_expr(condition.clone(), ctx, live_vars, locals, "")?;
                for nested in equations {
                    substitute_known_constants_when_equation(nested, ctx, live_vars, locals)?;
                }
            }
            if let Some(else_branch) = else_branch {
                for nested in else_branch {
                    substitute_known_constants_when_equation(nested, ctx, live_vars, locals)?;
                }
            }
        }
        flat::WhenEquation::FunctionCallOutputs { function, .. } => {
            *function =
                substitute_known_constants_expr(function.clone(), ctx, live_vars, locals, "")?;
        }
    }
    Ok(())
}

#[cfg(test)]
#[path = "postprocess_record_alias_tests.rs"]
mod record_alias_postprocess_tests;

#[cfg(test)]
mod substitute_constant_tests;
