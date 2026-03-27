//! ToDae phase for the Rumoca compiler.
//!
//! This crate implements the conversion from Model to DAE (Differential-Algebraic Equation)
//! representation per MLS Appendix B.
//!
//! # Overview
//!
//! The ToDae phase is responsible for:
//! - Classifying variables by role (state, algebraic, input, output, parameter, constant, discrete)
//! - Separating equations into ODE, algebraic, and output categories
//! - Identifying state variables (those with derivatives)
//! - Building the hybrid DAE system structure
//!
//! # Example
//!
//! ```ignore
//! use rumoca_phase_dae::to_dae;
//!
//! let flat: Model = flatten(instanced)?;
//! let dae: Dae = to_dae(&flat)?;
//! ```

mod algorithm_lowering;
mod appendix_b_validation;
mod balance_counting;
mod binding_conversion;
mod classification;
mod condition_lowering;
mod connector_input_analysis;
mod convert;
mod dae_lowering;
mod definition_analysis;
mod discrete_partition;
mod equation_conversion;
mod errors;
mod initial;
mod name_resolution;
mod overconstrained_interface;
mod path_utils;
mod pre_lowering;
mod reference_validation;
mod runtime_precompute;
mod scalar_inference;
mod scalar_size;
mod variable_analysis;
mod when_analysis;
mod when_conversion;
mod when_guard;

use algorithm_lowering::{
    canonicalize_discrete_assignment_equations, lower_algorithms_to_equations,
    route_discrete_event_equations,
};
use condition_lowering::populate_canonical_conditions;
pub(crate) use convert::{
    dae_to_flat_expression, dae_to_flat_var_name, flat_to_dae_expression, flat_to_dae_function_map,
    flat_to_dae_var_name,
};
use dae_lowering::sort_parameters_by_start_dependency;
use indexmap::{IndexMap, IndexSet};
#[cfg(test)]
use path_utils::strip_subscript;
use path_utils::subscript_fallback_chain;
use reference_validation::{validate_dae_constructor_field_projections, validate_dae_references};
use rumoca_core::{
    Span,
    timing::{maybe_elapsed_seconds, maybe_start_timer_if},
};
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use runtime_precompute::populate_runtime_precompute;
use rustc_hash::FxHashMap;
use scalar_inference::*;
use std::collections::{HashMap, HashSet};
use variable_analysis::{
    count_interface_flows, count_overconstrained_interface, filter_state_variables,
    find_connected_inputs, find_discrete_connected_internal_inputs, find_equation_defined_inputs,
    find_when_only_vars, is_internal_input, is_when_only_var, validate_flat_function_calls,
};
use when_conversion::convert_when_clause;

pub use dae_lowering::{
    insert_array_size_args_dae, lower_record_function_params_dae,
    scalarize_phantom_vector_equations,
};
pub use errors::{ToDaeError, ToDaeResult};
// Re-export moved functions so sibling modules can still use `super::`.
pub(crate) use variable_analysis::{
    collect_continuous_equation_lhs, component_reference_to_var_name,
    infer_record_subscript_size_from_prefix_chain, is_continuous_unknown,
    record_subscript_scalar_size, resolve_flat_function, resolve_internal_input,
};

type Dae = dae::Dae;
type Variable = dae::Variable;
type VariableKind = dae::VariableKind;
type BuiltinFunction = flat::BuiltinFunction;
type ComponentReference = flat::ComponentReference;
type ComprehensionIndex = flat::ComprehensionIndex;
type Expression = flat::Expression;
#[cfg(test)]
type Function = flat::Function;
type Literal = flat::Literal;
type Model = flat::Model;
type Statement = flat::Statement;
type StatementBlock = flat::StatementBlock;
type Subscript = flat::Subscript;
type VarName = flat::VarName;
type Variability = rumoca_ir_core::Variability;

fn todae_subphase_timing_enabled() -> bool {
    std::env::var("RUMOCA_TODAE_PROFILE").is_ok()
}

fn log_todae_subphase(label: &str, start: rumoca_core::timing::OptionalTimer) {
    if start.is_some() {
        let elapsed = maybe_elapsed_seconds(start);
        eprintln!("ToDae subphase {label}: {elapsed:.3}s");
    }
}

/// Options controlling ToDAE conversion strictness.
#[derive(Debug, Clone, Copy)]
pub struct ToDaeOptions {
    /// Whether to return an error for non-partial unbalanced models.
    pub error_on_unbalanced: bool,
}

impl Default for ToDaeOptions {
    fn default() -> Self {
        Self {
            error_on_unbalanced: true,
        }
    }
}

/// Takes a reference to avoid cloning the Model when the caller also needs it.
pub fn to_dae(flat: &Model) -> Result<Dae, ToDaeError> {
    to_dae_with_options(flat, ToDaeOptions::default())
}

/// Convert a Model to DAE with configurable strictness.
pub fn to_dae_with_options(flat: &Model, options: ToDaeOptions) -> Result<Dae, ToDaeError> {
    let mut dae = Dae::new();

    // Fail fast on unresolved or non-executable function calls so unsupported
    // evaluation paths don't leak into simulation.
    validate_flat_function_calls(flat)?;

    // MLS §4.7: Propagate partial status and class type for balance checking
    dae.is_partial = flat.is_partial;
    dae.class_type = flat.class_type.clone();
    dae.model_description = flat.model_description.clone();

    // Build prefix-to-children index once for O(1) record field lookups
    let prefix_children = build_prefix_children(flat);

    // First pass: identify state variables, connected inputs, and when-only vars
    let der_vars = classification::find_state_variables(flat);
    let state_vars = filter_state_variables(der_vars, flat);
    let mut connected_inputs = find_connected_inputs(flat);
    // Also promote inputs defined by model equations (e.g., `medium.p = p`)
    let eq_defined = find_equation_defined_inputs(flat);
    connected_inputs.extend(eq_defined);
    let connector_input_members =
        connector_input_analysis::find_top_level_connector_input_members(flat, &state_vars);
    let when_only_vars = find_when_only_vars(flat, &prefix_children);
    let discrete_connected_inputs = find_discrete_connected_internal_inputs(flat, &when_only_vars);

    // Derivative alias detection is intentionally disabled. Equations like `a = der(w)`
    // define both the state derivative AND the algebraic variable `a`. The proper solution
    // is to let the backend handle this: in codegen, equations of the form `var = der(state)`
    // become direct assignments (e.g., `a = ode[idx_w]`) and the balance checker counts
    // them as ODE equations.
    let derivative_aliases = HashSet::default();

    // Second pass: classify all variables
    let classification_inputs = ClassificationInputs {
        prefix_children: &prefix_children,
        state_vars: &state_vars,
        connected_inputs: &connected_inputs,
        discrete_connected_inputs: &discrete_connected_inputs,
        connector_input_members: &connector_input_members,
        when_only_vars: &when_only_vars,
        derivative_aliases: &derivative_aliases,
    };
    classify_variables(&mut dae, flat, &classification_inputs);

    // Collect variables defined by algorithm outputs (including record field expansion)
    // Per MLS §11.1, algorithm sections contribute equations for assigned variables.
    // We need to skip binding equations for these variables to avoid double-counting.
    let algorithm_defined_vars =
        definition_analysis::collect_algorithm_defined_vars(flat, &prefix_children);

    // Collect record fields that are already defined by record-level equations.
    // When `cc = func(...)` defines all fields of record `cc`, and `cc.m_capgd`
    // also has a binding, skip the binding to avoid double-counting.
    let record_eq_defined_vars =
        definition_analysis::collect_record_equation_defined_vars(flat, &prefix_children);

    // Third pass: convert variable bindings to equations (MLS §4.4.1)
    convert_bindings_to_equations(
        &mut dae,
        flat,
        &state_vars,
        &connected_inputs,
        &algorithm_defined_vars,
        &record_eq_defined_vars,
    );

    // Build prefix count map for efficient scalar count inference
    let prefix_counts = build_prefix_counts(flat);

    // Fourth pass: classify equations
    classify_equations(&mut dae, flat, &prefix_counts);

    // Process initial equations
    initial::convert_initial_equations(&mut dae, flat, &prefix_counts, infer_equation_scalar_count);

    // Process when clauses into discrete update sets.
    for when in &flat.when_clauses {
        let dae_when = convert_when_clause(when, &state_vars, flat)?;
        route_discrete_event_equations(&mut dae, &dae_when);
    }

    // Process model/initial algorithms strictly through equation lowering.
    lower_algorithms_to_equations(&mut dae, flat)?;
    canonicalize_discrete_assignment_equations(&mut dae);
    populate_canonical_conditions(&mut dae, flat);
    // MLS §3.7.5: Lower pre() operator calls to dedicated parameter symbols.
    // This must run after equation construction but before parameter sorting,
    // so that the new __pre__ parameters are included in dependency ordering.
    pre_lowering::lower_pre_operator(&mut dae);

    dae.functions = flat_to_dae_function_map(&flat.functions);
    dae.enum_literal_ordinals = flat.enum_literal_ordinals.clone();

    // Sort parameters so that start-value dependencies are satisfied in order.
    // If parameter A's start expression references parameter B, B must appear
    // before A so that code generators can evaluate start values sequentially.
    sort_parameters_by_start_dependency(&mut dae);

    // Scalarize vector equations whose expressions reference "phantom" base names.
    // Connector arrays like `plug_p.pin[3]` produce scalarized variables but some
    // component equations still reference the unsubscripted base (`plug_p.pin.v`).
    // Expanding them into per-element scalar equations ensures all backends can
    // resolve every VarRef to a declared variable.
    dae_lowering::scalarize_phantom_vector_equations(&mut dae);

    let todae_subphase_timing = todae_subphase_timing_enabled();
    let runtime_precompute_start = maybe_start_timer_if(todae_subphase_timing);
    populate_runtime_precompute(&mut dae)?;
    log_todae_subphase("runtime_precompute", runtime_precompute_start);
    let appendix_b_start = maybe_start_timer_if(todae_subphase_timing);
    appendix_b_validation::validate_appendix_b_invariants(&dae)?;
    log_todae_subphase("appendix_b_validation", appendix_b_start);

    // MLS §4.7: Count interface flow variables (flows in top-level connectors)
    // These count toward the equation size, not as unknowns, because they'll
    // receive their defining equations from external connections.
    dae.interface_flow_count = count_interface_flows(flat);

    // MLS §9.4: Propagate break edge excess from flatten phase.
    dae.oc_break_edge_scalar_count = flat.oc_break_edge_scalar_count;

    // MLS §4.8: Count overconstrained connector interface contributions.
    // Uses all OC vars for connectivity, filtered to components with top-level vars.
    // Positive = root equations needed, negative = redundant equations (cycles).
    let oc_correction = count_overconstrained_interface(flat, &state_vars);
    if oc_correction >= 0 {
        dae.overconstrained_interface_count = oc_correction;
    } else {
        dae.overconstrained_interface_count = 0;
        dae.oc_break_edge_scalar_count += (-oc_correction) as usize;
    }

    // Final compile-phase safety check on the generated DAE catches unresolved
    // constructor field projections introduced during ToDae conversion.
    let constructor_projection_start = maybe_start_timer_if(todae_subphase_timing);
    validate_dae_constructor_field_projections(&dae)?;
    log_todae_subphase(
        "constructor_projection_validation",
        constructor_projection_start,
    );
    let known_flat_var_names: HashSet<String> = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();
    let reference_validation_start = maybe_start_timer_if(todae_subphase_timing);
    validate_dae_references(&dae, &known_flat_var_names)?;
    log_todae_subphase("reference_validation", reference_validation_start);

    if options.error_on_unbalanced
        && !dae.is_partial
        && rumoca_eval_dae::analysis::balance(&dae) != 0
    {
        let (equations, unknowns) = balance_counting::compute_balance_counts(&dae);
        return Err(ToDaeError::unbalanced(equations, unknowns));
    }

    Ok(dae)
}

/// Determine if an algebraic variable should be stored in discretes or derivative_aliases.
/// Returns Some(map_name) if not a regular algebraic, None if it's a regular algebraic.
enum AlgebraicCategory {
    Discrete,
    DerivativeAlias,
    Regular,
}

fn categorize_algebraic(
    name: &VarName,
    var: &flat::Variable,
    when_only_vars: &HashSet<VarName>,
    derivative_aliases: &HashSet<VarName>,
) -> AlgebraicCategory {
    // When-only vars or unused expandable connector members are discrete (MLS §9.1.3)
    let is_unused_expandable =
        var.from_expandable_connector && !var.connected && var.binding.is_none();
    if is_when_only_var(name, when_only_vars) || is_unused_expandable {
        AlgebraicCategory::Discrete
    } else if derivative_aliases.contains(name) {
        AlgebraicCategory::DerivativeAlias
    } else {
        AlgebraicCategory::Regular
    }
}

fn has_clocked_or_event_binding(var: &flat::Variable) -> bool {
    var.binding
        .as_ref()
        .is_some_and(discrete_partition::expression_contains_clocked_or_event_operators)
}

/// Classify all variables from Model into DAE categories.
struct ClassificationInputs<'a> {
    prefix_children: &'a FxHashMap<String, Vec<VarName>>,
    state_vars: &'a HashSet<VarName>,
    connected_inputs: &'a HashSet<VarName>,
    discrete_connected_inputs: &'a HashSet<VarName>,
    connector_input_members: &'a HashSet<VarName>,
    when_only_vars: &'a HashSet<VarName>,
    derivative_aliases: &'a HashSet<VarName>,
}

fn classify_variables(dae: &mut Dae, flat: &Model, inputs: &ClassificationInputs<'_>) {
    let known_var_names: HashSet<String> = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();

    for (name, var) in &flat.variables {
        // Skip non-primitive aggregate variables whose primitive fields are
        // represented separately (MLS §4.8). Keep non-primitive leaves so
        // connector-typed scalar aliases remain available in the DAE.
        if !var.is_primitive && inputs.prefix_children.contains_key(name.as_str()) {
            continue;
        }

        let kind = classification::classify_variable(var, inputs.state_vars);
        let mut dae_var = create_dae_variable(name, var, &known_var_names);
        inherit_scalarized_start_from_base(name, flat, &mut dae_var, &known_var_names);

        // Top-level connector members connected only to internal inputs act as
        // external values and should remain inputs (not algebraic unknowns).
        if inputs.connector_input_members.contains(name) {
            dae.inputs.insert(flat_to_dae_var_name(name), dae_var);
            continue;
        }

        match kind {
            VariableKind::State => {
                dae.states.insert(flat_to_dae_var_name(name), dae_var);
            }
            VariableKind::Algebraic => {
                if has_clocked_or_event_binding(var) {
                    insert_discrete_var(dae, name, dae_var, var);
                    continue;
                }
                let category = categorize_algebraic(
                    name,
                    var,
                    inputs.when_only_vars,
                    inputs.derivative_aliases,
                );
                match category {
                    AlgebraicCategory::Discrete => {
                        insert_discrete_var(dae, name, dae_var, var);
                    }
                    AlgebraicCategory::DerivativeAlias => {
                        dae.derivative_aliases
                            .insert(flat_to_dae_var_name(name), dae_var);
                    }
                    AlgebraicCategory::Regular => {
                        dae.algebraics.insert(flat_to_dae_var_name(name), dae_var);
                    }
                };
            }
            VariableKind::Input => {
                if has_clocked_or_event_binding(var) {
                    insert_discrete_var(dae, name, dae_var, var);
                    continue;
                }
                if is_when_only_var(name, inputs.when_only_vars) {
                    insert_discrete_var(dae, name, dae_var, var);
                    continue;
                }
                if inputs.discrete_connected_inputs.contains(name) {
                    insert_discrete_var(dae, name, dae_var, var);
                    continue;
                }
                // Internal inputs that appear in der() are local dynamic unknowns.
                // External interface inputs remain inputs (handled below).
                if inputs.state_vars.contains(name) && is_internal_input(name, flat) {
                    dae.states.insert(flat_to_dae_var_name(name), dae_var);
                    continue;
                }
                // Connected inputs or inputs with bindings become algebraic (MLS §4.4.1)
                if !(inputs.connected_inputs.contains(name) || var.binding.is_some()) {
                    dae.inputs.insert(flat_to_dae_var_name(name), dae_var);
                    continue;
                }

                // Discrete-valued inputs (Boolean/Integer/enum) must stay in
                // the event-driven partition. Promoting them to continuous
                // algebraics creates false balance deficits because their
                // defining equations are routed to f_m/f_z (MLS Appendix B).
                let is_discrete_input = var.is_discrete_type
                    || matches!(var.variability, rumoca_ir_core::Variability::Discrete(_));
                if is_discrete_input {
                    insert_discrete_var(dae, name, dae_var, var);
                    continue;
                }
                dae.algebraics.insert(flat_to_dae_var_name(name), dae_var);
            }
            VariableKind::Output => {
                if is_when_only_var(name, inputs.when_only_vars)
                    || has_clocked_or_event_binding(var)
                {
                    insert_discrete_var(dae, name, dae_var, var);
                } else {
                    dae.outputs.insert(flat_to_dae_var_name(name), dae_var);
                }
            }
            VariableKind::Parameter => {
                dae.parameters.insert(flat_to_dae_var_name(name), dae_var);
            }
            VariableKind::Constant => {
                dae.constants.insert(flat_to_dae_var_name(name), dae_var);
            }
            VariableKind::Discrete => {
                insert_discrete_var(dae, name, dae_var, var);
            }
            VariableKind::Derivative => {} // Implicit, not stored
        }
    }
}

fn inherit_scalarized_start_from_base(
    name: &VarName,
    flat: &Model,
    dae_var: &mut Variable,
    known_var_names: &HashSet<String>,
) {
    if dae_var.start.is_some() {
        return;
    }

    let Some(base_name) = flat::component_base_name(name.as_str()) else {
        return;
    };
    if base_name == name.as_str() {
        return;
    }

    let base_var_name = VarName::new(base_name.clone());
    let Some(base_var) = flat.variables.get(&base_var_name) else {
        return;
    };
    let Some(base_start) = base_var.start.as_ref() else {
        return;
    };

    if matches!(
        base_start,
        Expression::Array { .. } | Expression::Tuple { .. }
    ) {
        return;
    }

    let projected_start = project_scalarized_start_expr(base_start, &base_name, name.as_str());
    dae_var.start = Some(flat_to_dae_expression(&rewrite_start_expr_missing_refs(
        &projected_start,
        known_var_names,
    )));
    if dae_var.fixed.is_none() {
        dae_var.fixed = base_var.fixed;
    }
}

fn project_scalarized_start_expr(
    base_start: &Expression,
    base_name: &str,
    scalar_name: &str,
) -> Expression {
    let Some(suffix) = scalar_name.strip_prefix(base_name) else {
        return base_start.clone();
    };
    if !suffix.starts_with('.') {
        return base_start.clone();
    }

    match base_start {
        Expression::VarRef { name, subscripts } if subscripts.is_empty() => Expression::VarRef {
            name: VarName::new(format!("{}{}", name.as_str(), suffix)),
            subscripts: vec![],
        },
        _ => base_start.clone(),
    }
}

/// Insert a variable into the appropriate discrete map (discrete_reals or discrete_valued).
///
/// Per MLS B.1, discrete Real variables (z) go to `discrete_reals`, while
/// Boolean/Integer/enum variables (m) go to `discrete_valued`.
/// Flat IR carries `is_discrete_type`, which identifies Integer/Boolean/enum
/// variables (MLS §4.5). Those are routed to `m`; other discrete variables
/// remain in `z`.
fn insert_discrete_var(
    dae: &mut Dae,
    name: &VarName,
    dae_var: dae::Variable,
    var: &flat::Variable,
) {
    if var.is_discrete_type {
        dae.discrete_valued
            .insert(flat_to_dae_var_name(name), dae_var);
    } else {
        dae.discrete_reals
            .insert(flat_to_dae_var_name(name), dae_var);
    }
}
fn convert_bindings_to_equations(
    dae: &mut Dae,
    flat: &Model,
    state_vars: &HashSet<VarName>,
    connected_inputs: &HashSet<VarName>,
    algorithm_defined_vars: &HashSet<VarName>,
    record_eq_defined_vars: &HashSet<VarName>,
) {
    binding_conversion::convert_bindings_to_equations(
        dae,
        flat,
        state_vars,
        connected_inputs,
        algorithm_defined_vars,
        record_eq_defined_vars,
    );
}

#[cfg(test)]
fn build_record_components<'a>(
    record_paths: &[&'a str],
    branches: &[(String, String)],
    optional_edges: &[(String, String)],
) -> (FxHashMap<&'a str, usize>, usize) {
    overconstrained_interface::build_record_components(record_paths, branches, optional_edges)
}

#[cfg(test)]
fn should_keep_connected_input_binding(
    kind: &VariableKind,
    name: &VarName,
    var: &flat::Variable,
    connected_inputs_only_connected_to_inputs: &HashSet<VarName>,
) -> bool {
    binding_conversion::should_keep_connected_input_binding(
        kind,
        name,
        var,
        connected_inputs_only_connected_to_inputs,
    )
}

#[cfg(test)]
fn build_unknown_prefix_children(unknowns: &HashSet<VarName>) -> FxHashMap<String, Vec<VarName>> {
    binding_conversion::build_unknown_prefix_children(unknowns)
}

#[cfg(test)]
fn should_skip_binding_for_explicit_var(
    name: &VarName,
    var: &flat::Variable,
    unknowns: &HashSet<VarName>,
    unknown_prefix_children: &FxHashMap<String, Vec<VarName>>,
) -> bool {
    binding_conversion::should_skip_binding_for_explicit_var(
        name,
        var,
        unknowns,
        unknown_prefix_children,
    )
}

#[cfg(test)]
fn collect_vars_with_unknown_rhs(flat: &Model, unknowns: &HashSet<VarName>) -> HashSet<VarName> {
    binding_conversion::collect_vars_with_unknown_rhs(flat, unknowns)
}

#[cfg(test)]
fn collect_when_statement_targets(
    statements: &[rumoca_ir_flat::Statement],
    targets: &mut HashSet<VarName>,
) {
    when_analysis::collect_when_statement_targets(statements, targets);
}

#[cfg(test)]
fn find_top_level_connector_input_members(
    flat: &Model,
    state_vars: &HashSet<VarName>,
) -> HashSet<VarName> {
    connector_input_analysis::find_top_level_connector_input_members(flat, state_vars)
}

/// Check if a connection equation connects only input variables.
/// Such equations are identity constraints (aliases), not defining equations,
/// and should not be counted in the balance.
#[cfg(test)]
fn is_input_input_connection(eq: &rumoca_ir_flat::Equation, dae: &Dae) -> bool {
    equation_conversion::is_input_input_connection(eq, dae)
}

#[cfg(test)]
fn is_input_default_equation(eq: &rumoca_ir_flat::Equation, flat: &Model, dae: &Dae) -> bool {
    equation_conversion::is_input_default_equation(eq, flat, dae)
}

#[cfg(test)]
fn get_output_in_input_output_connection(
    eq: &rumoca_ir_flat::Equation,
    dae: &Dae,
) -> Option<VarName> {
    equation_conversion::get_component_alias_connection_side(eq, dae).map(|(name, _)| name)
}

fn classify_equations(dae: &mut Dae, flat: &Model, prefix_counts: &FxHashMap<String, usize>) {
    equation_conversion::classify_equations(dae, flat, prefix_counts);
}

#[cfg(test)]
mod tests;
#[cfg(test)]
mod tests_conditions;
