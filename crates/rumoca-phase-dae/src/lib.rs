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
mod analysis;
mod appendix_b_validation;
pub mod balance;
mod binding_conversion;
mod condition_activation;
mod condition_lowering;
mod connector_input_analysis;
mod constructor_field_selection;
mod convert;
mod dae_lowering;
mod equation_conversion;
mod errors;
mod fold_start_values;
mod initial;
mod name_resolution;
mod overconstrained_interface;
mod path_utils;
mod pre_lowering;
mod reference_validation;
mod runtime_precompute;
mod scalar_inference;
mod scalar_size;
mod when_analysis;
mod when_conversion;
mod when_guard;

use algorithm_lowering::{
    canonicalize_discrete_assignment_equations, lower_algorithms_to_equations,
    route_discrete_event_equations,
};
use analysis::{classification, definition_analysis, discrete_partition, variable_analysis};
use condition_lowering::{finalize_canonical_condition_variables, populate_canonical_conditions};
pub use convert::attach_dae_reference_metadata;
pub(crate) use convert::{
    dae_to_flat_expression, dae_to_flat_var_name, flat_to_dae_expression,
    flat_to_dae_expression_with_refs, flat_to_dae_function_map, flat_to_dae_var_name,
    remap_flat_structured_equations,
};
use dae_lowering::sort_parameters_by_start_dependency;
use indexmap::{IndexMap, IndexSet};
use path_utils::subscript_fallback_chain;
use reference_validation::{validate_dae_constructor_field_selections, validate_dae_references};
#[cfg(test)]
use rumoca_core::strip_subscript;
use rumoca_core::timing::{maybe_elapsed_seconds, maybe_start_timer_if};
use rumoca_core::{
    BuiltinFunction, ComponentReference, ComprehensionIndex, Expression, Literal, Span, Statement,
    StatementBlock, Subscript, VarName, Variability,
};
use rumoca_ir_dae as dae;
use rumoca_ir_dae::{Dae, Variable};
use rumoca_ir_flat as flat;
use rumoca_ir_flat::Model;
use runtime_precompute::populate_runtime_precompute;
use rustc_hash::{FxHashMap, FxHashSet};
use scalar_inference::*;
use std::collections::{HashMap, HashSet};
use variable_analysis::{
    InternalInputIndex, count_interface_flows, count_overconstrained_interface,
    filter_state_variables, find_connected_inputs, find_discrete_connected_internal_inputs,
    find_equation_defined_inputs, find_overconstrained_derivative_alias_roots, find_when_only_vars,
    is_when_only_var, validate_flat_function_calls,
};
use when_conversion::convert_when_clause;

pub use balance::{
    BalanceError, InitialClosureBalanceDetail, balance, balance_detail, equations_unknowns,
    initial_closure_balance_detail, is_balanced, is_balanced_for_admission,
};
pub use dae_lowering::{
    CodegenDae, insert_array_size_args_dae, lower_record_function_params_dae,
    prepare_dae_for_codegen, scalarize_phantom_vector_equations,
};
pub use errors::{ToDaeError, ToDaeResult};
// Re-export moved functions so sibling modules can still use `super::`.
pub(crate) use variable_analysis::{
    collect_continuous_equation_lhs, find_connected_input_binding_anchors,
    find_connected_inputs_only_connected_to_inputs, infer_record_subscript_size_from_prefix_chain,
    is_continuous_unknown, is_internal_input, record_subscript_scalar_size, resolve_flat_function,
};

#[cfg(test)]
use rumoca_core::Function;
#[cfg(test)]
use rumoca_ir_dae::VariableKind;

fn todae_subphase_timing_enabled() -> bool {
    #[cfg(feature = "tracing")]
    {
        tracing::enabled!(target: "rumoca_phase_dae::profile", tracing::Level::DEBUG)
    }
    #[cfg(not(feature = "tracing"))]
    {
        false
    }
}

fn log_todae_subphase(label: &str, start: rumoca_core::timing::OptionalTimer) {
    if start.is_some() {
        let elapsed = maybe_elapsed_seconds(start);
        log_todae_profile(label, elapsed);
    }
}

fn log_todae_profile(label: &str, elapsed: f64) {
    #[cfg(feature = "tracing")]
    tracing::debug!(
        target: "rumoca_phase_dae::profile",
        phase = label,
        elapsed_seconds = elapsed,
        "ToDae subphase"
    );

    #[cfg(not(feature = "tracing"))]
    let _ = (label, elapsed);
}

pub(crate) fn log_todae_debug(message: String) {
    #[cfg(feature = "tracing")]
    tracing::debug!(target: "rumoca_phase_dae::debug", message = %message);

    #[cfg(not(feature = "tracing"))]
    let _ = message;
}

pub(crate) fn todae_debug_enabled() -> bool {
    #[cfg(feature = "tracing")]
    {
        tracing::enabled!(target: "rumoca_phase_dae::debug", tracing::Level::DEBUG)
    }
    #[cfg(not(feature = "tracing"))]
    {
        false
    }
}

pub(crate) fn equation_filter_debug_enabled() -> bool {
    #[cfg(feature = "tracing")]
    {
        tracing::enabled!(target: "rumoca_phase_dae::equation_filter", tracing::Level::DEBUG)
    }
    #[cfg(not(feature = "tracing"))]
    {
        false
    }
}

pub(crate) fn log_equation_filter_debug(message: String) {
    #[cfg(feature = "tracing")]
    tracing::debug!(target: "rumoca_phase_dae::equation_filter", message = %message);

    #[cfg(not(feature = "tracing"))]
    let _ = message;
}

pub(crate) fn fm_canon_debug_enabled() -> bool {
    #[cfg(feature = "tracing")]
    {
        tracing::enabled!(target: "rumoca_phase_dae::fm_canon", tracing::Level::DEBUG)
    }
    #[cfg(not(feature = "tracing"))]
    {
        false
    }
}

pub(crate) fn log_fm_canon_debug(message: String) {
    #[cfg(feature = "tracing")]
    tracing::debug!(target: "rumoca_phase_dae::fm_canon", message = %message);

    #[cfg(not(feature = "tracing"))]
    let _ = message;
}

fn run_todae_phase<R>(timing_enabled: bool, label: &str, f: impl FnOnce() -> R) -> R {
    let start = maybe_start_timer_if(timing_enabled);
    let result = f();
    log_todae_subphase(label, start);
    result
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
pub fn to_dae(flat: &flat::Model) -> Result<dae::Dae, ToDaeError> {
    to_dae_with_options(flat, ToDaeOptions::default())
}

/// Convert a Model to DAE with configurable strictness.
pub fn to_dae_with_options(
    flat: &flat::Model,
    options: ToDaeOptions,
) -> Result<dae::Dae, ToDaeError> {
    let mut dae = dae::Dae::new();
    let todae_subphase_timing = todae_subphase_timing_enabled();

    // Fail fast on unresolved or non-executable function calls so unsupported
    // evaluation paths don't leak into simulation.
    validate_flat_function_calls(flat)?;
    if ir_boundary_validation_enabled() {
        flat.validate_shape_contract().map_err(|err| {
            ToDaeError::runtime_contract_violation_at(
                format!("invalid Flat IR shape contract: {err:?}"),
                err.span(),
            )
        })?;
    }

    // MLS §4.7: Propagate partial status and class type for balance checking
    dae.metadata.is_partial = flat.is_partial;
    dae.metadata.class_type = flat.class_type.clone();
    dae.metadata.model_description = flat.model_description.clone();
    dae.metadata.symbol_ancestry = flat.symbol_ancestry.clone();
    dae.metadata.nonnumeric_variable_names = nonnumeric_variable_names(flat);

    let classification_indexes = build_variable_classification_indexes(flat)?;
    let prefix_children = &classification_indexes.prefix_children;
    let state_vars = &classification_indexes.state_vars;
    let connected_inputs = &classification_indexes.connected_inputs;

    // Second pass: classify all variables
    let classification_inputs = classification_indexes.as_inputs();
    classify_variables(&mut dae, flat, &classification_inputs)?;

    // Collect variables defined by algorithm outputs (including record field expansion)
    // Per MLS §11.1, algorithm sections contribute equations for assigned variables.
    // We need to skip binding equations for these variables to avoid double-counting.
    let algorithm_defined_vars =
        definition_analysis::collect_algorithm_defined_vars(flat, prefix_children);

    // Collect record fields that are already defined by record-level equations.
    // When `cc = func(...)` defines all fields of record `cc`, and `cc.m_capgd`
    // also has a binding, skip the binding to avoid double-counting.
    let record_eq_defined_vars =
        definition_analysis::collect_record_equation_defined_vars(flat, prefix_children)?;

    // Third pass: convert variable bindings to equations (MLS §4.4.1)
    run_todae_phase(todae_subphase_timing, "binding_conversion", || {
        convert_bindings_to_equations(
            &mut dae,
            flat,
            prefix_children,
            state_vars,
            connected_inputs,
            &algorithm_defined_vars,
            &record_eq_defined_vars,
        )?;
        Ok::<(), ToDaeError>(())
    })?;

    // Build prefix count map for efficient scalar count inference
    let prefix_counts = build_prefix_counts(flat);

    // Fourth pass: classify equations
    run_todae_phase(todae_subphase_timing, "equation_classification", || {
        classify_equations(&mut dae, flat, &prefix_counts)
    })?;

    // Process initial equations
    run_todae_phase(todae_subphase_timing, "initial_equations", || {
        initial::convert_initial_equations(
            &mut dae,
            flat,
            &prefix_counts,
            infer_equation_scalar_count,
        )
    })?;

    // Process when clauses into discrete update sets.
    run_todae_phase(todae_subphase_timing, "when_conversion", || {
        for when in &flat.when_clauses {
            let dae_when = convert_when_clause(when, state_vars, flat)?;
            route_discrete_event_equations(&mut dae, &dae_when)?;
        }
        Ok::<(), ToDaeError>(())
    })?;

    // Process model/initial algorithms strictly through equation lowering.
    run_todae_phase(todae_subphase_timing, "algorithm_lowering", || {
        lower_algorithms_to_equations(&mut dae, flat)
    })?;
    run_todae_phase(
        todae_subphase_timing,
        "canonicalize_discrete_assignments",
        || canonicalize_discrete_assignment_equations(&mut dae),
    )?;
    dae.symbols.enum_literal_ordinals = flat.enum_literal_ordinals.clone();
    run_todae_phase(todae_subphase_timing, "enum_literal_ordinals", || {
        dae_lowering::lower_enum_literal_refs_to_ordinals(&mut dae);
    });
    run_todae_phase(
        todae_subphase_timing,
        "fixed_start_initial_equations",
        || initial::add_fixed_start_initial_equations(&mut dae),
    )?;
    run_todae_phase(todae_subphase_timing, "canonical_conditions", || {
        populate_canonical_conditions(&mut dae)
    })?;
    run_todae_phase(todae_subphase_timing, "condition_variable_finalize", || {
        finalize_canonical_condition_variables(&mut dae)
    })?;
    // MLS §3.7.5: Lower pre() operator calls to dedicated parameter symbols.
    // This must run after equation construction but before parameter sorting,
    // so that the new __pre__ parameters are included in dependency ordering.
    run_todae_phase(todae_subphase_timing, "pre_lowering", || {
        pre_lowering::lower_pre_operator(&mut dae)
    })?;
    run_todae_phase(
        todae_subphase_timing,
        "overconstrained_derivative_alias_rewrite",
        || {
            rewrite_overconstrained_derivative_alias_refs(
                &mut dae,
                flat,
                &classification_indexes.overconstrained_derivative_alias_roots,
            )
        },
    )?;

    dae.symbols.functions = flat_to_dae_function_map(&flat.functions);
    finalize_lowered_dae(&mut dae, flat, state_vars, todae_subphase_timing, options)?;

    Ok(dae)
}

fn nonnumeric_variable_names(flat: &flat::Model) -> Vec<String> {
    flat.variable_type_names
        .iter()
        .filter(|(name, type_name)| {
            rumoca_core::qualified_type_name_matches(type_name, "String")
                || flat.variables.get(*name).is_some_and(|var| {
                    variable_analysis::is_external_constructor_handle(flat, name, var)
                })
        })
        .map(|(name, _)| name.as_str().to_string())
        .collect()
}

fn finalize_lowered_dae(
    dae: &mut dae::Dae,
    flat: &flat::Model,
    state_vars: &IndexSet<rumoca_core::VarName>,
    todae_subphase_timing: bool,
    options: ToDaeOptions,
) -> Result<(), ToDaeError> {
    // Sort parameters so that start-value dependencies are satisfied in order.
    // If parameter A's start expression references parameter B, B must appear
    // before A so that code generators can evaluate start values sequentially.
    run_todae_phase(todae_subphase_timing, "parameter_sort", || {
        sort_parameters_by_start_dependency(dae);
    });

    // Scalarize vector equations whose expressions reference "phantom" base names.
    // Connector arrays like `plug_p.pin[3]` produce scalarized variables but some
    // component equations still reference the unsubscripted base (`plug_p.pin.v`).
    // Expanding them into per-element scalar equations ensures all backends can
    // resolve every VarRef to a declared variable.
    run_todae_phase(todae_subphase_timing, "scalarize_phantom", || {
        dae_lowering::scalarize_phantom_vector_equations(dae)
    })?;

    // Fold symbolic start-value expressions to literal constants where
    // possible. Safe as an always-on pass: start values are init-time
    // metadata, not user-observable at runtime.
    run_todae_phase(todae_subphase_timing, "fold_start_values", || {
        fold_start_values::fold_start_values_to_literals(dae)
    })?;

    // Reorder algebraics so any algebraic used in another's defining
    // equation appears first. Pure reorder, no information loss; lets
    // forward-evaluation backends (embedded C, Julia MTK) emit
    // straight-line code without extra topo-sort inside the template.
    run_todae_phase(todae_subphase_timing, "sort_algebraics_by_deps", || {
        fold_start_values::sort_algebraics_by_equation_deps(dae)
    })?;

    run_todae_phase(todae_subphase_timing, "runtime_precompute", || {
        populate_runtime_precompute(dae)
    })?;
    run_todae_phase(todae_subphase_timing, "temporal_lowering_finalize", || {
        pre_lowering::lower_pre_operator(dae)?;
        sort_parameters_by_start_dependency(dae);
        Ok::<(), ToDaeError>(())
    })?;
    run_todae_phase(todae_subphase_timing, "reference_metadata", || {
        attach_dae_reference_metadata(dae)
    })?;
    run_todae_phase(todae_subphase_timing, "discrete_input_metadata", || {
        refresh_external_discrete_input_metadata(dae, flat);
    });
    run_todae_phase(todae_subphase_timing, "appendix_b_validation", || {
        appendix_b_validation::validate_appendix_b_invariants(dae)
    })?;

    run_todae_phase(todae_subphase_timing, "metadata_counts", || {
        // MLS §4.7 / §4.8 / §9.4: propagate interface counts from flatten.
        dae.metadata.interface_flow_count = count_interface_flows(flat);
        dae.metadata.stream_interface_equation_count = flat.stream_interface_equation_count;
        dae.metadata.oc_break_edge_scalar_count = flat.oc_break_edge_scalar_count;
        overconstrained_interface::validate_connection_graph(flat)?;
        dae.metadata.overconstrained_root_gauge_count =
            overconstrained_interface::count_overconstrained_root_gauge(flat, state_vars)?;
        let oc_correction = count_overconstrained_interface(flat, state_vars)?;
        if oc_correction >= 0 {
            dae.metadata.overconstrained_interface_count = oc_correction;
        } else {
            dae.metadata.overconstrained_interface_count = 0;
            dae.metadata.oc_break_edge_scalar_count += (-oc_correction) as usize;
        }
        Ok::<(), ToDaeError>(())
    })?;

    run_todae_phase(
        todae_subphase_timing,
        "constructor_selection_validation",
        || validate_dae_constructor_field_selections(dae),
    )?;
    let known_flat_var_names: HashSet<String> = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();
    run_todae_phase(todae_subphase_timing, "reference_validation", || {
        validate_dae_references(dae, &known_flat_var_names)
    })?;
    if ir_boundary_validation_enabled() {
        dae.validate_shape_contract().map_err(|err| {
            ToDaeError::runtime_contract_violation_at(
                format!("invalid DAE IR shape contract: {err:?}"),
                err.span(),
            )
        })?;
    }

    if options.error_on_unbalanced
        && !dae.metadata.is_partial
        && !balance::is_balanced_for_admission(dae)?
    {
        let (equations, unknowns) = balance::equations_unknowns(dae)?;
        return Err(ToDaeError::unbalanced(equations, unknowns));
    }

    Ok(())
}

fn ir_boundary_validation_enabled() -> bool {
    cfg!(any(
        debug_assertions,
        test,
        feature = "strict-ir-validation"
    ))
}

fn refresh_external_discrete_input_metadata(dae: &mut dae::Dae, flat: &flat::Model) {
    let targeted = targeted_discrete_variable_names(dae);
    dae.metadata.discrete_input_names.clear();
    let mut names = Vec::new();
    names.extend(external_discrete_input_names(
        &dae.variables.discrete_valued,
        &targeted,
        flat,
    ));
    names.sort();
    names.dedup();
    dae.metadata.discrete_input_names = names;
}

fn targeted_discrete_variable_names(dae: &dae::Dae) -> HashSet<rumoca_core::VarName> {
    dae.discrete
        .real_updates
        .iter()
        .chain(dae.discrete.valued_updates.iter())
        .chain(dae.conditions.equations.iter())
        .filter_map(|eq| eq.lhs.as_ref().map(|lhs| lhs.var_name().clone()))
        .collect()
}

fn external_discrete_input_names(
    variables: &IndexMap<rumoca_core::VarName, dae::Variable>,
    targeted: &HashSet<rumoca_core::VarName>,
    flat: &flat::Model,
) -> Vec<String> {
    variables
        .iter()
        .filter(|(name, variable)| {
            matches!(
                variable.causality,
                dae::VariableCausality::Input | dae::VariableCausality::Output
            ) && !targeted.contains(*name)
                && flat_variable_is_connected(flat, name)
        })
        .map(|(name, _)| name.as_str().to_string())
        .collect()
}

fn flat_variable_is_connected(flat: &flat::Model, name: &rumoca_core::VarName) -> bool {
    flat.variables
        .get(name)
        .is_some_and(|variable| variable.connected)
        || subscript_fallback_chain(name.as_str())
            .into_iter()
            .any(|candidate| flat.variables.get(&candidate).is_some_and(|v| v.connected))
}

/// Determine if an algebraic variable should be stored as discrete or regular algebraic.
/// Returns Some(map_name) if not a regular algebraic, None if it's a regular algebraic.
enum AlgebraicCategory {
    Discrete,
    Regular,
}

fn categorize_algebraic(
    name: &rumoca_core::VarName,
    var: &flat::Variable,
    when_only_vars: &IndexSet<rumoca_core::VarName>,
) -> AlgebraicCategory {
    // When-only vars or unused expandable connector members are discrete (MLS §9.1.3)
    let is_unused_expandable =
        var.from_expandable_connector && !var.connected && var.binding.is_none();
    if is_when_only_var(name, when_only_vars) || is_unused_expandable {
        AlgebraicCategory::Discrete
    } else {
        AlgebraicCategory::Regular
    }
}

fn has_clocked_binding(var: &flat::Variable) -> bool {
    var.binding
        .as_ref()
        .is_some_and(discrete_partition::expression_contains_clocked_operators)
}

/// Classify all variables from Model into DAE categories.
struct VariableClassificationIndexes {
    prefix_children: FxHashMap<String, Vec<rumoca_core::VarName>>,
    state_vars: IndexSet<rumoca_core::VarName>,
    overconstrained_derivative_alias_roots: FxHashMap<rumoca_core::VarName, rumoca_core::VarName>,
    connected_inputs: IndexSet<rumoca_core::VarName>,
    discrete_connected_inputs: IndexSet<rumoca_core::VarName>,
    input_only_connected_inputs: IndexSet<rumoca_core::VarName>,
    connector_input_members: IndexSet<rumoca_core::VarName>,
    when_only_vars: IndexSet<rumoca_core::VarName>,
    internal_inputs: InternalInputIndex,
}

impl VariableClassificationIndexes {
    fn as_inputs(&self) -> ClassificationInputs<'_> {
        ClassificationInputs {
            prefix_children: &self.prefix_children,
            state_vars: &self.state_vars,
            connected_inputs: &self.connected_inputs,
            discrete_connected_inputs: &self.discrete_connected_inputs,
            input_only_connected_inputs: &self.input_only_connected_inputs,
            connector_input_members: &self.connector_input_members,
            when_only_vars: &self.when_only_vars,
            internal_inputs: &self.internal_inputs,
        }
    }
}

fn build_variable_classification_indexes(
    flat: &flat::Model,
) -> Result<VariableClassificationIndexes, ToDaeError> {
    let prefix_children = build_prefix_children(flat);
    let internal_inputs = InternalInputIndex::new(flat)?;
    let der_vars = classification::find_state_variables(flat);
    let overconstrained_derivative_alias_roots =
        find_overconstrained_derivative_alias_roots(&der_vars, flat);
    let state_vars = filter_state_variables(der_vars, flat, &internal_inputs);
    let mut connected_input_set = find_connected_inputs(flat, &internal_inputs);
    connected_input_set.extend(find_equation_defined_inputs(flat, &internal_inputs));
    let connected_inputs = ordered_var_set(flat, connected_input_set);
    let connector_input_members = ordered_var_set(
        flat,
        connector_input_analysis::find_top_level_connector_input_members(flat, &state_vars)?,
    );
    let when_only_vars = ordered_var_set(flat, find_when_only_vars(flat, &prefix_children));
    let discrete_connected_inputs = ordered_var_set(
        flat,
        find_discrete_connected_internal_inputs(flat, &when_only_vars, &internal_inputs),
    );
    let input_only_connected_inputs = ordered_var_set(
        flat,
        find_connected_inputs_only_connected_to_inputs(flat, &internal_inputs),
    );

    Ok(VariableClassificationIndexes {
        prefix_children,
        state_vars,
        overconstrained_derivative_alias_roots,
        connected_inputs,
        discrete_connected_inputs,
        input_only_connected_inputs,
        connector_input_members,
        when_only_vars,
        internal_inputs,
    })
}

fn ordered_var_set(
    flat: &flat::Model,
    values: HashSet<rumoca_core::VarName>,
) -> IndexSet<rumoca_core::VarName> {
    flat.variables
        .keys()
        .filter(|name| values.contains(*name))
        .cloned()
        .collect()
}

struct ClassificationInputs<'a> {
    // SPEC_0021: these sets are membership indexes only. DAE insertion order
    // is driven by `flat.variables`, an IndexMap, so HashSet iteration order
    // cannot affect serialized/output variable ordering here.
    prefix_children: &'a FxHashMap<String, Vec<rumoca_core::VarName>>,
    state_vars: &'a IndexSet<rumoca_core::VarName>,
    connected_inputs: &'a IndexSet<rumoca_core::VarName>,
    discrete_connected_inputs: &'a IndexSet<rumoca_core::VarName>,
    input_only_connected_inputs: &'a IndexSet<rumoca_core::VarName>,
    connector_input_members: &'a IndexSet<rumoca_core::VarName>,
    when_only_vars: &'a IndexSet<rumoca_core::VarName>,
    internal_inputs: &'a InternalInputIndex,
}

// SPEC_0021: Exception - top-level DAE variable classification pass.
#[allow(clippy::too_many_lines)]
fn classify_variables(
    dae: &mut dae::Dae,
    flat: &flat::Model,
    inputs: &ClassificationInputs<'_>,
) -> Result<(), ToDaeError> {
    let known_var_names: HashSet<String> = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();

    for (name, var) in &flat.variables {
        if variable_analysis::is_external_constructor_handle(flat, name, var) {
            continue;
        }

        // Skip non-primitive aggregate variables whose primitive fields are
        // represented separately (MLS §4.8). Keep non-primitive leaves so
        // connector-typed scalar aliases remain available in the DAE.
        if !var.is_primitive && inputs.prefix_children.contains_key(name.as_str()) {
            continue;
        }

        let kind = classification::classify_variable(var, inputs.state_vars);
        let mut dae_var = create_dae_variable(name, var, &known_var_names)?;
        inherit_scalarized_start_from_base(name, flat, &mut dae_var, &known_var_names)?;
        record_variable_start_metadata(dae, name, &dae_var);

        // Top-level connector members connected only to internal inputs act as
        // external values and should remain inputs (not algebraic unknowns).
        if inputs.connector_input_members.contains(name) {
            dae.variables
                .inputs
                .insert(flat_to_dae_var_name(name), dae_var);
            continue;
        }

        match kind {
            dae::VariableKind::State => {
                dae.variables
                    .states
                    .insert(flat_to_dae_var_name(name), dae_var);
            }
            dae::VariableKind::Algebraic => {
                if has_clocked_binding(var) {
                    insert_discrete_var(dae, name, dae_var, var);
                    continue;
                }
                let category = categorize_algebraic(name, var, inputs.when_only_vars);
                match category {
                    AlgebraicCategory::Discrete => {
                        insert_discrete_var(dae, name, dae_var, var);
                    }
                    AlgebraicCategory::Regular => {
                        dae.variables
                            .algebraics
                            .insert(flat_to_dae_var_name(name), dae_var);
                    }
                };
            }
            dae::VariableKind::Input => {
                if has_clocked_binding(var) {
                    insert_discrete_var(dae, name, dae_var, var);
                    continue;
                }
                if is_when_only_var(name, inputs.when_only_vars) {
                    insert_discrete_var(dae, name, dae_var, var);
                    continue;
                }
                if inputs.discrete_connected_inputs.contains(name) {
                    record_input_only_discrete_input_metadata(dae, name, inputs);
                    insert_discrete_var(dae, name, dae_var, var);
                    continue;
                }
                // Internal inputs that appear in der() are local dynamic unknowns.
                // External interface inputs remain inputs (handled below).
                if inputs.state_vars.contains(name) && inputs.internal_inputs.contains(name) {
                    dae.variables
                        .states
                        .insert(flat_to_dae_var_name(name), dae_var);
                    continue;
                }
                // Connected inputs or inputs with bindings become algebraic (MLS §4.4.1)
                if !(inputs.connected_inputs.contains(name) || var.binding.is_some()) {
                    dae.variables
                        .inputs
                        .insert(flat_to_dae_var_name(name), dae_var);
                    continue;
                }

                // Discrete-valued inputs (Boolean/Integer/enum) must stay in
                // the event-driven partition. Promoting them to continuous
                // algebraics creates false balance deficits because their
                // defining equations are routed to f_m/f_z (MLS Appendix B).
                let is_discrete_input = var.is_discrete_type
                    || matches!(var.variability, rumoca_core::Variability::Discrete(_));
                if is_discrete_input {
                    record_input_only_discrete_input_metadata(dae, name, inputs);
                    insert_discrete_var(dae, name, dae_var, var);
                    continue;
                }
                dae.variables
                    .algebraics
                    .insert(flat_to_dae_var_name(name), dae_var);
            }
            dae::VariableKind::Output => {
                if is_when_only_var(name, inputs.when_only_vars) || has_clocked_binding(var) {
                    insert_discrete_var(dae, name, dae_var, var);
                } else {
                    dae.variables
                        .outputs
                        .insert(flat_to_dae_var_name(name), dae_var);
                }
            }
            dae::VariableKind::Parameter => {
                dae.variables
                    .parameters
                    .insert(flat_to_dae_var_name(name), dae_var);
            }
            dae::VariableKind::Constant => {
                dae.variables
                    .constants
                    .insert(flat_to_dae_var_name(name), dae_var);
            }
            dae::VariableKind::Discrete => {
                insert_discrete_var(dae, name, dae_var, var);
            }
            dae::VariableKind::Derivative => {} // Implicit, not stored
        }
    }
    Ok(())
}

fn rewrite_overconstrained_derivative_alias_refs(
    dae: &mut dae::Dae,
    flat: &flat::Model,
    alias_roots: &FxHashMap<rumoca_core::VarName, rumoca_core::VarName>,
) -> Result<(), ToDaeError> {
    if alias_roots.is_empty() {
        return Ok(());
    }

    let mut rewritten_aliases = FxHashSet::default();
    for equation in dae
        .continuous
        .equations
        .iter_mut()
        .chain(dae.initialization.equations.iter_mut())
        .chain(dae.discrete.real_updates.iter_mut())
        .chain(dae.discrete.valued_updates.iter_mut())
        .chain(dae.conditions.equations.iter_mut())
    {
        rewrite_overconstrained_derivative_alias_expr(
            &mut equation.rhs,
            alias_roots,
            &mut rewritten_aliases,
        );
    }

    for expr in dae
        .conditions
        .relations
        .iter_mut()
        .chain(dae.events.synthetic_root_conditions.iter_mut())
        .chain(dae.clocks.constructor_exprs.iter_mut())
        .chain(dae.clocks.triggered_conditions.iter_mut())
    {
        rewrite_overconstrained_derivative_alias_expr(expr, alias_roots, &mut rewritten_aliases);
    }

    add_overconstrained_derivative_alias_equations(dae, flat, alias_roots)
}

fn add_overconstrained_derivative_alias_equations(
    dae: &mut dae::Dae,
    flat: &flat::Model,
    alias_roots: &FxHashMap<rumoca_core::VarName, rumoca_core::VarName>,
) -> Result<(), ToDaeError> {
    // Rewriting der(alias) to der(root) preserves the dynamic derivative use,
    // but the alias variable remains an algebraic unknown after state filtering.
    // Connection-graph break-edge accounting cannot stand in for this local
    // alias closure, so emit one residual row for every removed derivative state.
    for (alias, root) in alias_roots {
        let Some(alias_var) = flat.variables.get(alias) else {
            continue;
        };
        if !flat.variables.contains_key(root) {
            continue;
        }
        let residual = derivative_alias_residual(alias, root, alias_var.source_span);
        let rhs = flat_to_dae_expression_with_refs(&residual, flat)?;
        dae.continuous.equations.push(dae::Equation::residual_array(
            rhs,
            alias_var.source_span,
            format!(
                "overconstrained derivative alias: {} = {}",
                alias.as_str(),
                root.as_str()
            ),
            flat_variable_scalar_count(alias_var),
        ));
    }
    Ok(())
}

fn rewrite_overconstrained_derivative_alias_expr(
    expr: &mut rumoca_core::Expression,
    alias_roots: &FxHashMap<rumoca_core::VarName, rumoca_core::VarName>,
    rewritten_aliases: &mut FxHashSet<rumoca_core::VarName>,
) {
    match expr {
        rumoca_core::Expression::BuiltinCall {
            function,
            args,
            span,
        } if *function == BuiltinFunction::Der => {
            if rewrite_derivative_alias_call(args, *span, alias_roots, rewritten_aliases) {
                return;
            }
            rewrite_overconstrained_derivative_alias_exprs(args, alias_roots, rewritten_aliases);
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            rewrite_overconstrained_derivative_alias_expr(lhs, alias_roots, rewritten_aliases);
            rewrite_overconstrained_derivative_alias_expr(rhs, alias_roots, rewritten_aliases);
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            rewrite_overconstrained_derivative_alias_expr(rhs, alias_roots, rewritten_aliases);
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. }
        | rumoca_core::Expression::Array { elements: args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. } => {
            rewrite_overconstrained_derivative_alias_exprs(args, alias_roots, rewritten_aliases);
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                rewrite_overconstrained_derivative_alias_expr(
                    condition,
                    alias_roots,
                    rewritten_aliases,
                );
                rewrite_overconstrained_derivative_alias_expr(
                    value,
                    alias_roots,
                    rewritten_aliases,
                );
            }
            rewrite_overconstrained_derivative_alias_expr(
                else_branch,
                alias_roots,
                rewritten_aliases,
            );
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            rewrite_overconstrained_derivative_alias_expr(start, alias_roots, rewritten_aliases);
            if let Some(step) = step {
                rewrite_overconstrained_derivative_alias_expr(step, alias_roots, rewritten_aliases);
            }
            rewrite_overconstrained_derivative_alias_expr(end, alias_roots, rewritten_aliases);
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            rewrite_overconstrained_derivative_alias_expr(expr, alias_roots, rewritten_aliases);
            for index in indices {
                rewrite_overconstrained_derivative_alias_expr(
                    &mut index.range,
                    alias_roots,
                    rewritten_aliases,
                );
            }
            if let Some(filter) = filter {
                rewrite_overconstrained_derivative_alias_expr(
                    filter,
                    alias_roots,
                    rewritten_aliases,
                );
            }
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            rewrite_overconstrained_derivative_alias_expr(base, alias_roots, rewritten_aliases);
            rewrite_overconstrained_derivative_alias_subscripts(
                subscripts,
                alias_roots,
                rewritten_aliases,
            );
        }
        rumoca_core::Expression::FieldAccess { base, .. } => {
            rewrite_overconstrained_derivative_alias_expr(base, alias_roots, rewritten_aliases);
        }
        rumoca_core::Expression::VarRef { subscripts, .. } => {
            rewrite_overconstrained_derivative_alias_subscripts(
                subscripts,
                alias_roots,
                rewritten_aliases,
            );
        }
        rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => {}
    }
}

fn rewrite_derivative_alias_call(
    args: &mut Vec<rumoca_core::Expression>,
    span: Span,
    alias_roots: &FxHashMap<rumoca_core::VarName, rumoca_core::VarName>,
    rewritten_aliases: &mut FxHashSet<rumoca_core::VarName>,
) -> bool {
    let Some(rumoca_core::Expression::VarRef {
        name, subscripts, ..
    }) = args.first_mut()
    else {
        return false;
    };
    if !subscripts.is_empty() {
        return false;
    }
    let Some(root) = alias_roots.get(name.var_name()) else {
        return false;
    };
    rewritten_aliases.insert(name.var_name().clone());
    *args = vec![derivative_alias_var_ref(root, span)];
    true
}

fn rewrite_overconstrained_derivative_alias_exprs(
    exprs: &mut [rumoca_core::Expression],
    alias_roots: &FxHashMap<rumoca_core::VarName, rumoca_core::VarName>,
    rewritten_aliases: &mut FxHashSet<rumoca_core::VarName>,
) {
    for expr in exprs {
        rewrite_overconstrained_derivative_alias_expr(expr, alias_roots, rewritten_aliases);
    }
}

fn rewrite_overconstrained_derivative_alias_subscripts(
    subscripts: &mut [rumoca_core::Subscript],
    alias_roots: &FxHashMap<rumoca_core::VarName, rumoca_core::VarName>,
    rewritten_aliases: &mut FxHashSet<rumoca_core::VarName>,
) {
    for subscript in subscripts {
        if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
            rewrite_overconstrained_derivative_alias_expr(expr, alias_roots, rewritten_aliases);
        }
    }
}

fn derivative_alias_var_ref(name: &rumoca_core::VarName, span: Span) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: name.clone().into(),
        subscripts: Vec::new(),
        span,
    }
}

fn derivative_alias_residual(
    alias: &rumoca_core::VarName,
    root: &rumoca_core::VarName,
    span: Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(derivative_var_ref(alias, span)),
        rhs: Box::new(derivative_var_ref(root, span)),
        span,
    }
}

fn derivative_var_ref(name: &rumoca_core::VarName, span: Span) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![rumoca_core::Expression::VarRef {
            name: name.clone().into(),
            subscripts: Vec::new(),
            span,
        }],
        span,
    }
}

fn flat_variable_scalar_count(var: &flat::Variable) -> usize {
    if var.dims.is_empty() {
        return 1;
    }
    var.dims
        .iter()
        .map(|dimension| usize::try_from((*dimension).max(1)).unwrap_or(1))
        .product::<usize>()
        .max(1)
}

fn record_variable_start_metadata(
    dae: &mut dae::Dae,
    name: &rumoca_core::VarName,
    var: &dae::Variable,
) {
    let Some(start) = var.start.as_ref() else {
        return;
    };
    dae.metadata.variable_starts.insert(
        flat_to_dae_var_name(name).as_str().to_string(),
        start.clone(),
    );
}

fn inherit_scalarized_start_from_base(
    name: &rumoca_core::VarName,
    flat: &flat::Model,
    dae_var: &mut dae::Variable,
    known_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    if dae_var.start.is_some() {
        return Ok(());
    }

    let Some(base_name) = flat::component_base_name(name.as_str()) else {
        return Ok(());
    };
    if base_name == name.as_str() {
        return Ok(());
    }

    let base_var_name = rumoca_core::VarName::new(base_name.clone());
    let Some(base_var) = flat.variables.get(&base_var_name) else {
        return Ok(());
    };
    let Some(base_start) = base_var.start.as_ref() else {
        return Ok(());
    };

    if matches!(
        base_start,
        rumoca_core::Expression::Array { .. } | rumoca_core::Expression::Tuple { .. }
    ) {
        return Ok(());
    }

    let owner_span = base_start
        .span()
        .filter(|span| !span.is_dummy())
        .or_else(|| (!base_var.source_span.is_dummy()).then_some(base_var.source_span))
        .ok_or_else(|| {
            ToDaeError::runtime_metadata_violation(format!(
                "scalarized inherited start for `{}` is missing source provenance",
                name.as_str()
            ))
        })?;
    let selected_start =
        select_scalarized_start_expr(base_start, &base_name, name.as_str(), owner_span);
    dae_var.start_span = Some(owner_span);
    dae_var.start = Some(flat_to_dae_expression(&rewrite_start_expr_missing_refs(
        &selected_start,
        known_var_names,
    )));
    if dae_var.fixed.is_none() {
        dae_var.fixed = base_var.fixed;
    }
    Ok(())
}

fn select_scalarized_start_expr(
    base_start: &rumoca_core::Expression,
    base_name: &str,
    scalar_name: &str,
    owner_span: Span,
) -> rumoca_core::Expression {
    let Some(suffix) = scalar_name.strip_prefix(base_name) else {
        return base_start.clone();
    };
    if !suffix.starts_with('.') {
        return base_start.clone();
    }

    match base_start {
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } if subscripts.is_empty() => {
            // `suffix` is a `.member[.member...]` path; append each part so
            // the rendered name and the structured reference stay in lockstep.
            let mut reference = name.clone();
            rumoca_core::visit_top_level_path_segments(suffix.trim_start_matches('.'), |field| {
                reference = reference.with_appended_field(field);
            });
            rumoca_core::Expression::VarRef {
                name: reference,
                subscripts: vec![],
                span: if span.is_dummy() { owner_span } else { *span },
            }
        }
        _ => base_start.clone(),
    }
}

fn record_discrete_input_metadata(dae: &mut dae::Dae, name: &rumoca_core::VarName) {
    let dae_name = flat_to_dae_var_name(name).to_string();
    if !dae.metadata.discrete_input_names.contains(&dae_name) {
        dae.metadata.discrete_input_names.push(dae_name);
    }
}

fn record_input_only_discrete_input_metadata(
    dae: &mut dae::Dae,
    name: &rumoca_core::VarName,
    inputs: &ClassificationInputs<'_>,
) {
    if inputs.input_only_connected_inputs.contains(name) {
        record_discrete_input_metadata(dae, name);
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
    dae: &mut dae::Dae,
    name: &rumoca_core::VarName,
    dae_var: dae::Variable,
    var: &flat::Variable,
) {
    if var.is_discrete_type {
        dae.variables
            .discrete_valued
            .insert(flat_to_dae_var_name(name), dae_var);
    } else {
        dae.variables
            .discrete_reals
            .insert(flat_to_dae_var_name(name), dae_var);
    }
}
fn convert_bindings_to_equations(
    dae: &mut dae::Dae,
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    state_vars: &IndexSet<rumoca_core::VarName>,
    connected_inputs: &IndexSet<rumoca_core::VarName>,
    algorithm_defined_vars: &HashSet<rumoca_core::VarName>,
    record_eq_defined_vars: &HashSet<rumoca_core::VarName>,
) -> Result<(), ToDaeError> {
    binding_conversion::convert_bindings_to_equations(
        dae,
        flat,
        prefix_children,
        state_vars,
        connected_inputs,
        algorithm_defined_vars,
        record_eq_defined_vars,
    )
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
    kind: &dae::VariableKind,
    name: &rumoca_core::VarName,
    var: &flat::Variable,
    connected_inputs_only_connected_to_inputs: &HashSet<rumoca_core::VarName>,
) -> bool {
    binding_conversion::should_keep_connected_input_binding(
        kind,
        name,
        var,
        connected_inputs_only_connected_to_inputs,
    )
}

#[cfg(test)]
fn build_unknown_prefix_children(
    unknowns: &HashSet<rumoca_core::VarName>,
) -> FxHashMap<String, Vec<rumoca_core::VarName>> {
    binding_conversion::build_unknown_prefix_children(unknowns)
        .expect("unknown prefix index should build for test-provided unknown names")
}

#[cfg(test)]
fn should_skip_binding_for_explicit_var(
    name: &rumoca_core::VarName,
    var: &flat::Variable,
    unknowns: &HashSet<rumoca_core::VarName>,
    unknown_prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
) -> bool {
    binding_conversion::should_skip_binding_for_explicit_var(
        name,
        var,
        unknowns,
        unknown_prefix_children,
    )
}

#[cfg(test)]
fn collect_vars_with_unknown_rhs(
    flat: &flat::Model,
    unknowns: &HashSet<rumoca_core::VarName>,
) -> HashSet<rumoca_core::VarName> {
    binding_conversion::collect_vars_with_unknown_rhs(flat, unknowns)
}

#[cfg(test)]
fn collect_when_statement_targets(
    statements: &[rumoca_core::Statement],
    targets: &mut HashSet<rumoca_core::VarName>,
) {
    when_analysis::collect_when_statement_targets(statements, targets);
}

#[cfg(test)]
fn find_top_level_connector_input_members(
    flat: &flat::Model,
    state_vars: &IndexSet<rumoca_core::VarName>,
) -> HashSet<rumoca_core::VarName> {
    connector_input_analysis::find_top_level_connector_input_members(flat, state_vars)
        .expect("connector input member analysis should succeed")
}

/// Check if a connection equation connects only input variables.
/// Such equations are identity constraints (aliases), not defining equations,
/// and should not be counted in the balance.
#[cfg(test)]
fn is_input_input_connection(eq: &rumoca_ir_flat::Equation, dae: &dae::Dae) -> bool {
    equation_conversion::is_input_input_connection(eq, dae)
}

#[cfg(test)]
fn is_input_default_equation(
    eq: &rumoca_ir_flat::Equation,
    flat: &flat::Model,
    dae: &dae::Dae,
) -> bool {
    equation_conversion::is_input_default_equation(eq, flat, dae)
}

#[cfg(test)]
fn get_output_in_input_output_connection(
    eq: &rumoca_ir_flat::Equation,
    dae: &dae::Dae,
) -> Option<rumoca_core::VarName> {
    equation_conversion::get_component_alias_connection_side(eq, dae).map(|(name, _)| name)
}

fn classify_equations(
    dae: &mut dae::Dae,
    flat: &flat::Model,
    prefix_counts: &FxHashMap<String, usize>,
) -> Result<(), ToDaeError> {
    equation_conversion::classify_equations(dae, flat, prefix_counts)
}

#[cfg(test)]
pub(crate) mod test_support;
#[cfg(test)]
mod tests;
