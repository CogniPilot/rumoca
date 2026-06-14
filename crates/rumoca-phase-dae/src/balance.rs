//! DAE balance arithmetic.
//!
//! This is the single canonical implementation of balance checking for the
//! Rumoca DAE IR. All other crates must call these functions rather than
//! reimplementing the formula (AGENTS.md: "Balance arithmetic lives in
//! `rumoca-phase-dae`").

use std::collections::HashSet;

use indexmap::{IndexMap, IndexSet};
use rumoca_ir_dae as dae;

/// Detailed breakdown of balance calculation components.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct BalanceDetail {
    pub state_unknowns: usize,
    pub alg_unknowns: usize,
    pub output_unknowns: usize,
    pub discrete_real_unknowns: usize,
    pub discrete_valued_unknowns: usize,
    pub f_x_scalar: usize,
    pub f_z_scalar: usize,
    pub f_m_scalar: usize,
    pub f_c_scalar: usize,
    pub algorithm_outputs: usize,
    pub when_eq_scalar: usize,
    pub interface_flow_count: usize,
    pub stream_interface_equation_count: usize,
    pub overconstrained_interface_count: i64,
    pub oc_break_edge_scalar_count: usize,
}

impl std::fmt::Display for BalanceDetail {
    // Shows raw component counts only. To compute the final balance, call
    // balance(), which applies deficit-only clamping for iflow/oc and
    // surplus-only clamping for break-edge correction.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let unknowns = self.state_unknowns
            + self.alg_unknowns
            + self.output_unknowns
            + self.discrete_real_unknowns
            + self.discrete_valued_unknowns;
        writeln!(
            f,
            "  Unknowns: {} = states({}) + alg({}) + out({}) + z({}) + m({})",
            unknowns,
            self.state_unknowns,
            self.alg_unknowns,
            self.output_unknowns,
            self.discrete_real_unknowns,
            self.discrete_valued_unknowns
        )?;
        write!(
            f,
            "  Equations (raw): f_x({}) + f_z({}) + f_m({}) + f_c({}) + algo({}) + when({}) + stream({}) + iflow({}) + oc({}) - brk({})",
            self.f_x_scalar,
            self.f_z_scalar,
            self.f_m_scalar,
            self.f_c_scalar,
            self.algorithm_outputs,
            self.when_eq_scalar,
            self.stream_interface_equation_count,
            self.interface_flow_count,
            self.overconstrained_interface_count,
            self.oc_break_edge_scalar_count,
        )
    }
}

/// Check if the system is balanced (equations match unknowns).
pub fn is_balanced(dae_model: &dae::Dae) -> bool {
    balance(dae_model) == 0
}

/// Get the balance: equations - unknowns.
///
/// Positive means over-determined, negative means under-determined.
pub fn balance(dae_model: &dae::Dae) -> i64 {
    let state_unknowns: usize = dae_model.variables.states.values().map(|v| v.size()).sum();
    let alg_unknowns: usize = dae_model
        .variables
        .algebraics
        .values()
        .map(|v| v.size())
        .sum();
    let output_unknowns: usize = dae_model.variables.outputs.values().map(|v| v.size()).sum();
    let discrete_real_unknowns = count_referenced_discrete_real_unknown_scalars(dae_model);
    let discrete_valued_unknowns = count_referenced_discrete_valued_unknown_scalars(dae_model);
    let unknowns = (state_unknowns
        + alg_unknowns
        + output_unknowns
        + discrete_real_unknowns
        + discrete_valued_unknowns) as i64;

    // f_x: unified continuous equations (MLS B.1a).
    // Only equations that constrain at least one continuous unknown belong
    // to local continuous balance accounting.
    let f_x_scalar = count_f_x_scalars_with_continuous_unknowns(dae_model);
    // MLS Appendix B: model algorithms and when-equations have already been
    // lowered into the B.1 equation buckets before DAE balance is checked.
    // Algorithm and when-equation details are explicit at zero because those
    // rows have already been lowered into the MLS B.1 equation buckets.
    let algorithm_outputs = 0usize;
    let when_eq_scalar = 0usize;

    // Per MLS §15.1: outside stream connectors contribute stream equations.
    // Per MLS §4.7: interface flow variables count as equations
    // Per MLS §4.8/§9.4: overconstrained correction
    //
    // OC interface correction is applied only to close an existing deficit
    // (oc_needed), so it cannot over-correct. Break-edge correction is then
    // applied at the end only when the system is over-determined.
    let brk = dae_model.metadata.oc_break_edge_scalar_count as i64;
    let available_oc_interface = dae_model.metadata.overconstrained_interface_count.max(0);
    let f_z_scalar = count_discrete_real_update_scalars(dae_model);
    let f_m_scalar = count_discrete_valued_update_scalars(dae_model);
    let f_c_scalar = count_condition_memory_equation_scalars(dae_model);
    let base_without_stream =
        (f_x_scalar + f_z_scalar + f_m_scalar + f_c_scalar + algorithm_outputs + when_eq_scalar)
            as i64;
    let stream_needed = (unknowns - base_without_stream).max(0);
    let effective_stream =
        (dae_model.metadata.stream_interface_equation_count as i64).min(stream_needed);
    let base_without_iflow = base_without_stream + effective_stream;
    // Interface-flow equations should only compensate a remaining local deficit.
    // This prevents double-counting when explicit unconnected-flow equations
    // already close top-level connector flows in standalone models.
    let iflow_needed = (unknowns - base_without_iflow).max(0);
    let effective_iflow = (dae_model.metadata.interface_flow_count as i64).min(iflow_needed);
    let base_equations = base_without_iflow + effective_iflow;
    // OC interface correction must never over-correct a model that is already
    // balanced (or over-determined) before OC terms are applied.
    let oc_needed = (unknowns - base_equations).max(0);
    let effective_oc_interface = available_oc_interface.min(oc_needed);

    let raw_equations = base_equations + effective_oc_interface;
    let raw_balance = raw_equations - unknowns;

    // Per MLS §9.4: subtract break edge excess (cycles in OC graph).
    // Cap the correction so it only reduces positive balance toward zero.
    let effective_brk = brk.min(raw_balance.max(0));
    raw_balance - effective_brk
}

/// Return detailed breakdown of the balance calculation components.
pub fn balance_detail(dae_model: &dae::Dae) -> BalanceDetail {
    let state_unknowns: usize = dae_model.variables.states.values().map(|v| v.size()).sum();
    let alg_unknowns: usize = dae_model
        .variables
        .algebraics
        .values()
        .map(|v| v.size())
        .sum();
    let output_unknowns: usize = dae_model.variables.outputs.values().map(|v| v.size()).sum();
    let discrete_real_unknowns = count_referenced_discrete_real_unknown_scalars(dae_model);
    let discrete_valued_unknowns = count_referenced_discrete_valued_unknown_scalars(dae_model);
    // See balance(): algorithms/when-equations are represented through
    // lowered B.1 rows by this phase, not counted as separate terms here.
    let algorithm_outputs = 0usize;
    let when_eq_scalar = 0usize;
    let f_x_scalar = count_f_x_scalars_with_continuous_unknowns(dae_model);
    let f_z_scalar = count_discrete_real_update_scalars(dae_model);
    let f_m_scalar = count_discrete_valued_update_scalars(dae_model);
    let f_c_scalar = count_condition_memory_equation_scalars(dae_model);
    BalanceDetail {
        state_unknowns,
        alg_unknowns,
        output_unknowns,
        discrete_real_unknowns,
        discrete_valued_unknowns,
        f_x_scalar,
        f_z_scalar,
        f_m_scalar,
        f_c_scalar,
        algorithm_outputs,
        when_eq_scalar,
        interface_flow_count: dae_model.metadata.interface_flow_count,
        stream_interface_equation_count: dae_model.metadata.stream_interface_equation_count,
        overconstrained_interface_count: dae_model.metadata.overconstrained_interface_count,
        oc_break_edge_scalar_count: dae_model.metadata.oc_break_edge_scalar_count,
    }
}

/// Return `(effective_equations, unknowns)` for unbalanced error diagnostics.
///
/// Uses the same clamping logic as `balance()` so the gate and the error
/// payload always agree.
pub fn equations_unknowns(dae_model: &dae::Dae) -> (usize, usize) {
    let detail = balance_detail(dae_model);
    let unknowns = detail.state_unknowns
        + detail.alg_unknowns
        + detail.output_unknowns
        + detail.discrete_real_unknowns
        + detail.discrete_valued_unknowns;
    let brk = detail.oc_break_edge_scalar_count as i64;
    let available_oc_interface = detail.overconstrained_interface_count.max(0);
    let base_without_stream = (detail.f_x_scalar
        + detail.f_z_scalar
        + detail.f_m_scalar
        + detail.f_c_scalar
        + detail.algorithm_outputs
        + detail.when_eq_scalar) as i64;
    let stream_needed = (unknowns as i64 - base_without_stream).max(0);
    let effective_stream = (detail.stream_interface_equation_count as i64).min(stream_needed);
    let base_without_iflow = base_without_stream + effective_stream;
    let iflow_needed = (unknowns as i64 - base_without_iflow).max(0);
    let effective_iflow = (detail.interface_flow_count as i64).min(iflow_needed);
    let base_equations = base_without_iflow + effective_iflow;
    let oc_needed = (unknowns as i64 - base_equations).max(0);
    let effective_oc_interface = available_oc_interface.min(oc_needed);
    let raw_equations = base_equations + effective_oc_interface;
    let raw_balance = raw_equations - unknowns as i64;
    let effective_brk = brk.min(raw_balance.max(0));
    let equations = (raw_equations - effective_brk) as usize;
    if std::env::var_os("RUMOCA_DEBUG_BALANCE_DETAIL").is_some() {
        eprintln!(
            "DEBUG BALANCE DETAIL: equations={} unknowns={} balance={} state={} alg={} out={} z_unknown={} m_unknown={} f_x={} f_z={} f_m={} f_c={} stream={} effective_stream={} iflow={} effective_iflow={} oc={} effective_oc={} brk={} effective_brk={}",
            equations,
            unknowns,
            equations as i64 - unknowns as i64,
            detail.state_unknowns,
            detail.alg_unknowns,
            detail.output_unknowns,
            detail.discrete_real_unknowns,
            detail.discrete_valued_unknowns,
            detail.f_x_scalar,
            detail.f_z_scalar,
            detail.f_m_scalar,
            detail.f_c_scalar,
            detail.stream_interface_equation_count,
            effective_stream,
            detail.interface_flow_count,
            effective_iflow,
            detail.overconstrained_interface_count,
            effective_oc_interface,
            detail.oc_break_edge_scalar_count,
            effective_brk
        );
    }
    (equations, unknowns)
}

pub(crate) fn count_f_x_scalars_with_continuous_unknowns(dae_model: &dae::Dae) -> usize {
    let continuous_unknowns = collect_continuous_unknown_names(dae_model);
    let input_names = collect_input_names(dae_model);
    let component_defined_targets =
        collect_component_defined_targets_for_balance(dae_model, &continuous_unknowns);
    let mut connection_rank = ConnectionUpdateRank::new(IndexSet::new());
    let debug_connection_rank = std::env::var_os("RUMOCA_DEBUG_CONNECTION_RANK").is_some();
    let debug_connection_rank_summary =
        std::env::var_os("RUMOCA_DEBUG_CONNECTION_RANK_SUMMARY").is_some();
    let debug_class_summary = std::env::var_os("RUMOCA_DEBUG_F_X_CLASS_SUMMARY").is_some();
    let debug_prefix_filter = std::env::var("RUMOCA_DEBUG_F_X_PREFIX_FILTER")
        .ok()
        .filter(|value| !value.is_empty());
    let mut connection_raw = 0usize;
    let mut connection_counted = 0usize;
    let mut connection_drops = std::collections::BTreeMap::<String, (usize, usize)>::new();
    let mut continuous_unknown_by_prefix = std::collections::BTreeMap::<String, usize>::new();
    let mut input_only_by_prefix = std::collections::BTreeMap::<String, usize>::new();
    let mut counted_connection_by_prefix = std::collections::BTreeMap::<String, usize>::new();
    let count = dae_model
        .continuous
        .equations
        .iter()
        .map(|eq| {
            if !equation_counts_for_balance(
                dae_model,
                eq,
                &continuous_unknowns,
                &input_names,
                &component_defined_targets,
            ) {
                debug_f_x_prefix_filter(
                    &debug_prefix_filter,
                    "excluded",
                    0,
                    eq,
                    &continuous_unknowns,
                    &input_names,
                );
                return 0;
            }
            let has_continuous_unknown =
                equation_references_continuous_unknown(eq, &continuous_unknowns);
            let has_input = equation_references_input(eq, &input_names);
            if is_connection_origin(eq.origin.as_str()) {
                if let Some(count) =
                    count_continuous_connection_rank(eq, dae_model, &mut connection_rank)
                {
                    if debug_class_summary && count != 0 {
                        let key = equation_debug_prefix(eq, &continuous_unknowns, &input_names);
                        *counted_connection_by_prefix.entry(key).or_default() += count;
                    }
                    connection_raw += eq.scalar_count;
                    connection_counted += count;
                    if count != eq.scalar_count {
                        let key = equation_debug_prefix(eq, &continuous_unknowns, &input_names);
                        let entry = connection_drops.entry(key).or_default();
                        entry.0 += eq.scalar_count;
                        entry.1 += count;
                    }
                    if debug_connection_rank && count != eq.scalar_count {
                        eprintln!(
                            "DEBUG CONTINUOUS CONNECTION RANK DROP: {} -> {} | {}",
                            eq.scalar_count, count, eq.origin
                        );
                    }
                    debug_f_x_prefix_filter(
                        &debug_prefix_filter,
                        "included_connection",
                        count,
                        eq,
                        &continuous_unknowns,
                        &input_names,
                    );
                    return count;
                }
            }
            if debug_class_summary {
                let key = equation_debug_prefix(eq, &continuous_unknowns, &input_names);
                if has_continuous_unknown {
                    *continuous_unknown_by_prefix.entry(key).or_default() += eq.scalar_count;
                } else if has_input {
                    *input_only_by_prefix.entry(key).or_default() += eq.scalar_count;
                }
            }
            debug_f_x_prefix_filter(
                &debug_prefix_filter,
                "included",
                eq.scalar_count,
                eq,
                &continuous_unknowns,
                &input_names,
            );
            eq.scalar_count
        })
        .sum();
    if debug_class_summary {
        push_balance_debug_map_to_stderr(
            "f_x_continuous_unknown",
            continuous_unknown_by_prefix,
            120,
        );
        push_balance_debug_map_to_stderr("f_x_input_only", input_only_by_prefix, 120);
        push_balance_debug_map_to_stderr(
            "f_x_connection_counted",
            counted_connection_by_prefix,
            120,
        );
    }
    if debug_connection_rank_summary {
        let mut drops = connection_drops.into_iter().collect::<Vec<_>>();
        drops.sort_by_key(|(_, (raw, counted))| std::cmp::Reverse(raw.saturating_sub(*counted)));
        eprintln!(
            "DEBUG CONTINUOUS CONNECTION RANK SUMMARY: raw={} counted={} dropped={}",
            connection_raw,
            connection_counted,
            connection_raw.saturating_sub(connection_counted)
        );
        for (key, (raw, counted)) in drops.into_iter().take(80) {
            eprintln!(
                "  drop={:>6} raw={:>6} counted={:>6} {}",
                raw.saturating_sub(counted),
                raw,
                counted,
                key
            );
        }
    }
    count
}

fn debug_f_x_prefix_filter(
    filter: &Option<String>,
    status: &str,
    count: usize,
    eq: &dae::Equation,
    continuous_unknowns: &HashSet<rumoca_core::VarName>,
    input_names: &HashSet<rumoca_core::VarName>,
) {
    let Some(filter) = filter else {
        return;
    };
    let key = equation_debug_prefix(eq, continuous_unknowns, input_names);
    let mut refs = IndexSet::new();
    eq.rhs.collect_var_refs(&mut refs);
    let text = format!("{} {} {:?} {}", key, eq.origin, eq.lhs, refs.len());
    if !text.contains(filter) {
        return;
    }
    eprintln!(
        "DEBUG F_X FILTER {status}: count={count} scalar_count={} key={} lhs={:?} origin={} refs={:?}",
        eq.scalar_count, key, eq.lhs, eq.origin, refs
    );
}

fn push_balance_debug_map_to_stderr(
    label: &str,
    counts: std::collections::BTreeMap<String, usize>,
    limit: usize,
) {
    let total: usize = counts.values().sum();
    let mut counts = counts.into_iter().collect::<Vec<_>>();
    counts.sort_by(|(_, lhs), (_, rhs)| rhs.cmp(lhs));
    eprintln!("DEBUG DAE BALANCE {label}: total={total}");
    for (key, count) in counts.into_iter().take(limit) {
        eprintln!("  {count:>8} {key}");
    }
}

#[derive(Debug, Clone)]
pub struct BalanceOriginReason {
    pub origin: String,
    pub continuous_unknown_scalars: usize,
    pub input_only_scalars: usize,
}

#[derive(Debug, Clone)]
pub struct BalanceUnclassifiedRef {
    pub name: String,
    pub scalar_count: usize,
}

pub fn balance_unclassified_refs(dae_model: &dae::Dae) -> Vec<BalanceUnclassifiedRef> {
    let known_names = dae_model
        .variables
        .states
        .keys()
        .chain(dae_model.variables.algebraics.keys())
        .chain(dae_model.variables.outputs.keys())
        .chain(dae_model.variables.inputs.keys())
        .chain(dae_model.variables.parameters.keys())
        .chain(dae_model.variables.constants.keys())
        .chain(dae_model.variables.discrete_reals.keys())
        .chain(dae_model.variables.discrete_valued.keys())
        .cloned()
        .collect::<HashSet<_>>();
    let mut counts = IndexMap::<String, usize>::new();
    for eq in &dae_model.continuous.equations {
        for name in eq_binary_var_refs(&eq.rhs) {
            if name_matches_set(name, &known_names) {
                continue;
            }
            *counts.entry(name.as_str().to_string()).or_default() += eq.scalar_count;
        }
    }
    let mut out = counts
        .into_iter()
        .map(|(name, scalar_count)| BalanceUnclassifiedRef { name, scalar_count })
        .collect::<Vec<_>>();
    out.sort_by_key(|entry| std::cmp::Reverse(entry.scalar_count));
    out
}

pub fn balance_counted_prefix_debug_lines(dae_model: &dae::Dae) -> Vec<String> {
    let continuous_unknowns = collect_continuous_unknown_names(dae_model);
    let input_names = collect_input_names(dae_model);
    let component_defined_targets =
        collect_component_defined_targets_for_balance(dae_model, &continuous_unknowns);
    let mut connection_rank = ConnectionUpdateRank::new(IndexSet::new());
    let mut continuous = std::collections::BTreeMap::<String, usize>::new();
    for eq in &dae_model.continuous.equations {
        if !equation_counts_for_balance(
            dae_model,
            eq,
            &continuous_unknowns,
            &input_names,
            &component_defined_targets,
        ) {
            continue;
        }
        let count = if is_connection_origin(eq.origin.as_str()) {
            count_continuous_connection_rank(eq, dae_model, &mut connection_rank)
                .unwrap_or(eq.scalar_count)
        } else {
            eq.scalar_count
        };
        if count == 0 {
            continue;
        }
        let key = equation_debug_prefix(eq, &continuous_unknowns, &input_names);
        *continuous.entry(key).or_default() += count;
    }

    let mut unknowns = std::collections::BTreeMap::<String, usize>::new();
    for (name, variable) in dae_model
        .variables
        .states
        .iter()
        .chain(dae_model.variables.algebraics.iter())
        .chain(dae_model.variables.outputs.iter())
    {
        *unknowns
            .entry(debug_name_prefix(name.as_str()))
            .or_default() += variable.size();
    }

    let mut discrete_updates = std::collections::BTreeMap::<String, usize>::new();
    let discrete_input_names = metadata_discrete_input_names(dae_model);
    let connection_anchors =
        collect_discrete_connection_balance_anchors(dae_model, &discrete_input_names);
    let mut discrete_rank = ConnectionUpdateRank::new(connection_anchors);
    for eq in &dae_model.discrete.valued_updates {
        if is_discrete_input_update(eq, &discrete_input_names) {
            continue;
        }
        let count = if is_discrete_connection_update_origin(eq.origin.as_str()) {
            count_connection_update_rank(
                eq,
                &dae_model.variables.discrete_valued,
                &mut discrete_rank,
            )
            .unwrap_or(eq.scalar_count)
        } else {
            eq.scalar_count
        };
        if count == 0 {
            continue;
        }
        let key = equation_debug_prefix(
            eq,
            &dae_model
                .variables
                .discrete_valued
                .keys()
                .cloned()
                .collect(),
            &HashSet::new(),
        );
        *discrete_updates.entry(key).or_default() += count;
    }

    let mut lines = Vec::new();
    push_top_debug_lines("continuous_counted", continuous, &mut lines);
    push_top_debug_lines("continuous_unknowns", unknowns, &mut lines);
    push_top_debug_lines("discrete_valued_updates", discrete_updates, &mut lines);
    lines
}

pub fn balance_prefix_delta_debug_lines(dae_model: &dae::Dae) -> Vec<String> {
    let continuous_unknowns = collect_continuous_unknown_names(dae_model);
    let input_names = collect_input_names(dae_model);
    let component_defined_targets =
        collect_component_defined_targets_for_balance(dae_model, &continuous_unknowns);
    let mut connection_rank = ConnectionUpdateRank::new(IndexSet::new());
    let mut counted = std::collections::BTreeMap::<String, i64>::new();
    for eq in &dae_model.continuous.equations {
        if !equation_counts_for_balance(
            dae_model,
            eq,
            &continuous_unknowns,
            &input_names,
            &component_defined_targets,
        ) {
            continue;
        }
        let count = if is_connection_origin(eq.origin.as_str()) {
            count_continuous_connection_rank(eq, dae_model, &mut connection_rank)
                .unwrap_or(eq.scalar_count)
        } else {
            eq.scalar_count
        };
        if count == 0 {
            continue;
        }
        let key = equation_debug_prefix(eq, &continuous_unknowns, &input_names);
        *counted.entry(key).or_default() += count as i64;
    }

    let mut unknowns = std::collections::BTreeMap::<String, i64>::new();
    for (name, variable) in dae_model
        .variables
        .states
        .iter()
        .chain(dae_model.variables.algebraics.iter())
        .chain(dae_model.variables.outputs.iter())
    {
        *unknowns
            .entry(debug_name_prefix(name.as_str()))
            .or_default() += variable.size() as i64;
    }

    let keys = counted
        .keys()
        .chain(unknowns.keys())
        .cloned()
        .collect::<std::collections::BTreeSet<_>>();
    let mut deltas = keys
        .into_iter()
        .filter_map(|key| {
            let counted = counted.get(&key).copied().unwrap_or(0);
            let unknowns = unknowns.get(&key).copied().unwrap_or(0);
            let delta = counted - unknowns;
            (delta != 0).then_some((key, counted, unknowns, delta))
        })
        .collect::<Vec<_>>();
    deltas.sort_by_key(|(_, _, _, delta)| std::cmp::Reverse(delta.abs()));

    let mut lines = Vec::new();
    lines.push("DEBUG DAE BALANCE CONTINUOUS PREFIX DELTAS:".to_string());
    for (key, counted, unknowns, delta) in deltas.into_iter().take(240) {
        lines.push(format!(
            "  delta={delta:>8} counted={counted:>8} unknowns={unknowns:>8} {key}"
        ));
    }
    lines
}

pub fn discrete_valued_prefix_delta_debug_lines(dae_model: &dae::Dae) -> Vec<String> {
    let discrete_input_names = metadata_discrete_input_names(dae_model);
    let mut equations = std::collections::BTreeMap::<String, i64>::new();
    let connection_anchors =
        collect_discrete_connection_balance_anchors(dae_model, &discrete_input_names);
    let mut discrete_rank = ConnectionUpdateRank::new(connection_anchors);
    for eq in &dae_model.discrete.valued_updates {
        if is_discrete_input_update(eq, &discrete_input_names) {
            continue;
        }
        let count = if is_discrete_connection_update_origin(eq.origin.as_str()) {
            count_connection_update_rank(
                eq,
                &dae_model.variables.discrete_valued,
                &mut discrete_rank,
            )
            .unwrap_or(eq.scalar_count)
        } else {
            eq.scalar_count
        };
        if count == 0 {
            continue;
        }
        let key = eq
            .lhs
            .as_ref()
            .map(|lhs| debug_name_prefix(lhs.as_str()))
            .unwrap_or_else(|| debug_origin_key(eq.origin.as_str()));
        *equations.entry(key).or_default() += count as i64;
    }
    let discrete_valued_names = dae_model
        .variables
        .discrete_valued
        .keys()
        .cloned()
        .collect::<HashSet<_>>();
    for eq in &dae_model.conditions.equations {
        if !equation_lhs_matches_name(eq, &discrete_valued_names) {
            continue;
        }
        let key = eq
            .lhs
            .as_ref()
            .map(|lhs| debug_name_prefix(lhs.as_str()))
            .unwrap_or_else(|| debug_origin_key(eq.origin.as_str()));
        *equations.entry(key).or_default() += eq.scalar_count as i64;
    }

    let mut referenced = IndexMap::new();
    let mut residual_unknowns = std::collections::BTreeMap::<String, i64>::new();
    let target_scalar_counts = collect_update_target_scalar_counts(
        &dae_model.variables.discrete_valued,
        dae_model
            .discrete
            .valued_updates
            .iter()
            .chain(dae_model.conditions.equations.iter()),
    );
    for eq in &dae_model.discrete.valued_updates {
        if eq.lhs.is_some() {
            add_update_target_scalar_counts(
                eq,
                &dae_model.variables.discrete_valued,
                &mut referenced,
            );
            if is_discrete_connection_update_origin(eq.origin.as_str()) {
                insert_complete_connection_variable_names_from_expression(
                    eq,
                    &eq.rhs,
                    &dae_model.variables.discrete_valued,
                    &target_scalar_counts,
                    &mut referenced,
                );
            }
        } else if expression_references_included_variable(
            &eq.rhs,
            &dae_model.variables.discrete_valued,
            &dae_model.variables.inputs,
            &discrete_input_names,
        ) {
            let key = equation_debug_prefix(eq, &discrete_valued_names, &HashSet::new());
            *residual_unknowns.entry(key).or_default() += eq.scalar_count as i64;
        }
    }
    for eq in &dae_model.conditions.equations {
        if eq.lhs.is_some() {
            add_update_target_scalar_counts(
                eq,
                &dae_model.variables.discrete_valued,
                &mut referenced,
            );
        }
    }
    let mut unknowns = residual_unknowns;
    for (name, count) in referenced {
        if variable_overlaps_any(&name, &dae_model.variables.inputs)
            || name_matches_set(&name, &discrete_input_names)
        {
            continue;
        }
        if let Some(variable) = dae_model.variables.discrete_valued.get(&name) {
            *unknowns
                .entry(debug_name_prefix(name.as_str()))
                .or_default() += count.min(variable.size()) as i64;
        }
    }

    let keys = equations
        .keys()
        .chain(unknowns.keys())
        .cloned()
        .collect::<std::collections::BTreeSet<_>>();
    let mut deltas = keys
        .into_iter()
        .filter_map(|key| {
            let counted = equations.get(&key).copied().unwrap_or(0);
            let unknowns = unknowns.get(&key).copied().unwrap_or(0);
            let delta = counted - unknowns;
            (delta != 0).then_some((key, counted, unknowns, delta))
        })
        .collect::<Vec<_>>();
    deltas.sort_by_key(|(_, _, _, delta)| std::cmp::Reverse(delta.abs()));

    let mut lines = Vec::new();
    lines.push("DEBUG DAE BALANCE DISCRETE VALUED PREFIX DELTAS:".to_string());
    for (key, counted, unknowns, delta) in deltas.into_iter().take(240) {
        lines.push(format!(
            "  delta={delta:>8} counted={counted:>8} unknowns={unknowns:>8} {key}"
        ));
    }
    lines
}

fn push_top_debug_lines(
    label: &str,
    counts: std::collections::BTreeMap<String, usize>,
    lines: &mut Vec<String>,
) {
    let mut counts = counts.into_iter().collect::<Vec<_>>();
    counts.sort_by(|(_, lhs), (_, rhs)| rhs.cmp(lhs));
    lines.push(format!("DEBUG DAE BALANCE PREFIX {label}:"));
    for (key, count) in counts.into_iter().take(120) {
        lines.push(format!("  {count:>8} {key}"));
    }
}

fn equation_debug_prefix(
    eq: &dae::Equation,
    primary_names: &HashSet<rumoca_core::VarName>,
    secondary_names: &HashSet<rumoca_core::VarName>,
) -> String {
    if let Some(lhs) = &eq.lhs {
        if name_matches_set(lhs, primary_names) || name_matches_set(lhs, secondary_names) {
            return debug_name_prefix(lhs.as_str());
        }
    }
    let mut refs = IndexSet::new();
    eq.rhs.collect_var_refs(&mut refs);
    for name in &refs {
        if name_matches_set(name, primary_names) {
            return debug_name_prefix(name.as_str());
        }
    }
    for name in &refs {
        if name_matches_set(name, secondary_names) {
            return debug_name_prefix(name.as_str());
        }
    }
    debug_origin_key(eq.origin.as_str())
}

fn debug_name_prefix(name: &str) -> String {
    let mut parts = name.split('.');
    let Some(first) = parts.next() else {
        return name.to_string();
    };
    let Some(second) = parts.next() else {
        return first.to_string();
    };
    format!("{first}.{second}")
}

pub fn balance_origin_reasons(dae_model: &dae::Dae) -> Vec<BalanceOriginReason> {
    let continuous_unknowns = collect_continuous_unknown_names(dae_model);
    let input_names = collect_input_names(dae_model);
    let component_defined_targets =
        collect_component_defined_targets_for_balance(dae_model, &continuous_unknowns);
    let mut origins = IndexMap::<String, (usize, usize)>::new();
    for eq in &dae_model.continuous.equations {
        if is_connection_origin(eq.origin.as_str())
            && is_redundant_connection_alias(
                dae_model,
                eq,
                &continuous_unknowns,
                &component_defined_targets,
            )
        {
            continue;
        }
        let key = debug_origin_key(eq.origin.as_str());
        if equation_references_continuous_unknown(eq, &continuous_unknowns) {
            origins.entry(key).or_default().0 += eq.scalar_count;
            continue;
        }
        if is_connection_origin(eq.origin.as_str()) || eq.origin.starts_with("binding equation for")
        {
            continue;
        }
        if equation_references_input(eq, &input_names) {
            origins.entry(key).or_default().1 += eq.scalar_count;
        }
    }
    let mut out = origins
        .into_iter()
        .map(
            |(origin, (continuous_unknown_scalars, input_only_scalars))| BalanceOriginReason {
                origin,
                continuous_unknown_scalars,
                input_only_scalars,
            },
        )
        .collect::<Vec<_>>();
    out.sort_by_key(|entry| {
        std::cmp::Reverse(entry.continuous_unknown_scalars + entry.input_only_scalars)
    });
    out
}

fn debug_origin_key(origin: &str) -> String {
    if let Some((prefix, _)) = origin.split_once(" for ") {
        return prefix.to_string();
    }
    if let Some((prefix, _)) = origin.split_once(" in ") {
        return prefix.to_string();
    }
    if let Some((prefix, _)) = origin.split_once(':') {
        return prefix.to_string();
    }
    origin.to_string()
}

fn equation_counts_for_balance(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
    continuous_unknowns: &HashSet<rumoca_core::VarName>,
    input_names: &HashSet<rumoca_core::VarName>,
    component_defined_targets: &HashSet<rumoca_core::VarName>,
) -> bool {
    if is_connection_origin(eq.origin.as_str())
        && is_redundant_connection_alias(
            dae_model,
            eq,
            continuous_unknowns,
            component_defined_targets,
        )
    {
        return false;
    }
    if equation_references_continuous_unknown(eq, continuous_unknowns) {
        return true;
    }
    // Connection aliases that do not constrain any continuous unknown should
    // not contribute to local continuous balance.
    if is_connection_origin(eq.origin.as_str()) {
        return false;
    }
    // Binding equations for internal promoted inputs/discrete partitions can
    // be input-only aliases and should not inflate continuous balance.
    if eq.origin.starts_with("binding equation for") {
        return false;
    }
    // Preserve explicit user equations constraining interface inputs.
    equation_references_input(eq, input_names)
}

fn is_redundant_connection_alias(
    _dae_model: &dae::Dae,
    eq: &dae::Equation,
    continuous_unknowns: &HashSet<rumoca_core::VarName>,
    component_defined_targets: &HashSet<rumoca_core::VarName>,
) -> bool {
    let names = eq_binary_var_refs(&eq.rhs);
    if names.len() != 2 {
        return false;
    }
    let lhs = names[0];
    let rhs = names[1];

    let lhs_component_defined = name_matches_set(lhs, component_defined_targets);
    let rhs_component_defined = name_matches_set(rhs, component_defined_targets);
    let lhs_is_continuous_unknown = name_matches_set(lhs, continuous_unknowns);
    let rhs_is_continuous_unknown = name_matches_set(rhs, continuous_unknowns);

    (lhs_component_defined && !rhs_is_continuous_unknown)
        || (rhs_component_defined && !lhs_is_continuous_unknown)
}

fn collect_component_defined_targets_for_balance(
    dae_model: &dae::Dae,
    continuous_unknowns: &HashSet<rumoca_core::VarName>,
) -> HashSet<rumoca_core::VarName> {
    let mut targets = HashSet::new();
    for eq in &dae_model.continuous.equations {
        if is_connection_origin(eq.origin.as_str()) {
            continue;
        }
        let unknown_refs = eq_binary_var_refs(&eq.rhs)
            .into_iter()
            .filter(|name| name_matches_set(name, continuous_unknowns))
            .collect::<Vec<_>>();
        if let [target] = unknown_refs.as_slice() {
            targets.insert((*target).clone());
        }
    }
    targets
}

fn collect_continuous_unknown_names(dae_model: &dae::Dae) -> HashSet<rumoca_core::VarName> {
    dae_model
        .variables
        .states
        .keys()
        .chain(dae_model.variables.algebraics.keys())
        .chain(dae_model.variables.outputs.keys())
        .cloned()
        .collect()
}

fn collect_input_names(dae_model: &dae::Dae) -> HashSet<rumoca_core::VarName> {
    dae_model.variables.inputs.keys().cloned().collect()
}

fn count_discrete_real_update_scalars(dae_model: &dae::Dae) -> usize {
    let discrete_real_names = dae_model.variables.discrete_reals.keys().cloned().collect();
    dae_model
        .discrete
        .real_updates
        .iter()
        .filter(|eq| update_equation_targets_name(eq, &discrete_real_names))
        .map(|eq| eq.scalar_count)
        .sum()
}

fn count_discrete_valued_update_scalars(dae_model: &dae::Dae) -> usize {
    let discrete_input_names = metadata_discrete_input_names(dae_model);
    let mut scalar_count = 0usize;
    let connection_anchors =
        collect_discrete_connection_balance_anchors(dae_model, &discrete_input_names);
    let mut connection_rank = ConnectionUpdateRank::new(connection_anchors);
    let debug_connection_rank = std::env::var_os("RUMOCA_DEBUG_DISCRETE_CONNECTION_RANK").is_some();
    let debug_connection_rank_summary =
        std::env::var_os("RUMOCA_DEBUG_DISCRETE_CONNECTION_RANK_SUMMARY").is_some();
    let debug_connection_rank_filter =
        std::env::var("RUMOCA_DEBUG_DISCRETE_CONNECTION_RANK_FILTER")
            .ok()
            .filter(|value| !value.is_empty());
    let mut connection_raw = 0usize;
    let mut connection_counted = 0usize;
    let mut connection_drops = std::collections::BTreeMap::<String, (usize, usize)>::new();
    for eq in &dae_model.discrete.valued_updates {
        if is_discrete_input_update(eq, &discrete_input_names) {
            continue;
        }
        if is_discrete_connection_update_origin(eq.origin.as_str())
            && let Some(count) = count_connection_update_rank(
                eq,
                &dae_model.variables.discrete_valued,
                &mut connection_rank,
            )
        {
            connection_raw += eq.scalar_count;
            connection_counted += count;
            if count != eq.scalar_count {
                let key = discrete_connection_rank_debug_key(eq);
                let entry = connection_drops.entry(key).or_default();
                entry.0 += eq.scalar_count;
                entry.1 += count;
            }
            if debug_connection_rank
                && count != eq.scalar_count
                && debug_connection_rank_filter
                    .as_deref()
                    .is_none_or(|filter| discrete_connection_rank_debug_text(eq).contains(filter))
            {
                eprintln!(
                    "DEBUG DISCRETE CONNECTION RANK DROP: {} -> {} | lhs={:?} | {}",
                    eq.scalar_count, count, eq.lhs, eq.origin
                );
            }
            scalar_count += count;
            continue;
        }
        scalar_count += eq.scalar_count;
    }
    if debug_connection_rank_summary {
        let mut drops = connection_drops.into_iter().collect::<Vec<_>>();
        drops.sort_by_key(|(_, (raw, counted))| std::cmp::Reverse(raw.saturating_sub(*counted)));
        eprintln!(
            "DEBUG DISCRETE CONNECTION RANK SUMMARY: raw={} counted={} dropped={}",
            connection_raw,
            connection_counted,
            connection_raw.saturating_sub(connection_counted)
        );
        for (key, (raw, counted)) in drops.into_iter().take(80) {
            eprintln!(
                "  drop={:>6} raw={:>6} counted={:>6} {}",
                raw.saturating_sub(counted),
                raw,
                counted,
                key
            );
        }
    }
    scalar_count
}

fn discrete_connection_rank_debug_text(eq: &dae::Equation) -> String {
    let mut refs = IndexSet::new();
    eq.rhs.collect_var_refs(&mut refs);
    format!("{} {:?} {:?}", eq.origin, eq.lhs, refs)
}

fn discrete_connection_rank_debug_key(eq: &dae::Equation) -> String {
    if let Some(lhs) = &eq.lhs {
        return debug_name_prefix(lhs.as_str());
    }
    let mut refs = IndexSet::new();
    eq.rhs.collect_var_refs(&mut refs);
    if let Some(name) = refs.first() {
        return debug_name_prefix(name.as_str());
    }
    debug_origin_key(eq.origin.as_str())
}

fn collect_discrete_connection_balance_anchors(
    dae_model: &dae::Dae,
    discrete_input_names: &HashSet<rumoca_core::VarName>,
) -> IndexSet<rumoca_core::VarName> {
    let mut anchors = IndexSet::new();
    for name in discrete_input_names {
        insert_connection_rank_nodes(
            name,
            dae_model
                .variables
                .discrete_valued
                .get(name)
                .map(dae::Variable::size)
                .unwrap_or(1),
            &dae_model.variables.discrete_valued,
            &mut anchors,
        );
    }
    for eq in dae_model
        .discrete
        .valued_updates
        .iter()
        .chain(dae_model.conditions.equations.iter())
    {
        if is_discrete_connection_update_origin(eq.origin.as_str()) {
            continue;
        }
        if let Some(lhs) = &eq.lhs {
            insert_connection_rank_nodes(
                lhs,
                eq.scalar_count,
                &dae_model.variables.discrete_valued,
                &mut anchors,
            );
        }
    }
    anchors
}

fn insert_connection_rank_nodes(
    name: &rumoca_core::VarName,
    scalar_count: usize,
    variables: &IndexMap<rumoca_core::VarName, dae::Variable>,
    nodes: &mut IndexSet<rumoca_core::VarName>,
) {
    if let Some(expanded) = expand_connection_rank_nodes(name, scalar_count, variables) {
        nodes.extend(expanded);
    } else {
        nodes.insert(name.clone());
    }
}

fn is_discrete_input_update(
    eq: &dae::Equation,
    discrete_input_names: &HashSet<rumoca_core::VarName>,
) -> bool {
    let Some(lhs) = &eq.lhs else {
        return false;
    };
    if !name_matches_set(lhs, discrete_input_names) {
        return false;
    }
    let mut refs = IndexSet::new();
    eq.rhs.collect_var_refs(&mut refs);
    refs.into_iter()
        .all(|name| name_matches_set(&name, discrete_input_names))
}

struct ConnectionUpdateRank {
    node_to_idx: IndexMap<rumoca_core::VarName, usize>,
    parent: Vec<usize>,
    rank: Vec<usize>,
    anchored: Vec<bool>,
}

impl ConnectionUpdateRank {
    fn new(anchors: IndexSet<rumoca_core::VarName>) -> Self {
        let mut rank = Self {
            node_to_idx: IndexMap::new(),
            parent: Vec::new(),
            rank: Vec::new(),
            anchored: Vec::new(),
        };
        for anchor in anchors {
            let idx = rank.node_idx(anchor);
            rank.anchored[idx] = true;
        }
        rank
    }

    fn add_edge(&mut self, lhs: rumoca_core::VarName, rhs: rumoca_core::VarName) -> bool {
        let lhs_idx = self.node_idx(lhs);
        let rhs_idx = self.node_idx(rhs);
        let lhs_root = self.find_idx(lhs_idx);
        let rhs_root = self.find_idx(rhs_idx);
        if lhs_root == rhs_root {
            return false;
        }
        let contributes_balance = !(self.anchored[lhs_root] && self.anchored[rhs_root]);
        let anchored = self.anchored[lhs_root] || self.anchored[rhs_root];
        if self.rank[lhs_root] < self.rank[rhs_root] {
            self.parent[lhs_root] = rhs_root;
            self.anchored[rhs_root] = anchored;
        } else if self.rank[lhs_root] > self.rank[rhs_root] {
            self.parent[rhs_root] = lhs_root;
            self.anchored[lhs_root] = anchored;
        } else {
            self.parent[rhs_root] = lhs_root;
            self.rank[lhs_root] += 1;
            self.anchored[lhs_root] = anchored;
        }
        contributes_balance
    }

    fn node_idx(&mut self, node: rumoca_core::VarName) -> usize {
        if let Some(idx) = self.node_to_idx.get(&node) {
            return *idx;
        }
        let idx = self.parent.len();
        self.node_to_idx.insert(node, idx);
        self.parent.push(idx);
        self.rank.push(0);
        self.anchored.push(false);
        idx
    }

    fn find_idx(&mut self, mut idx: usize) -> usize {
        let mut root = idx;
        while self.parent[root] != root {
            root = self.parent[root];
        }
        while self.parent[idx] != root {
            let next = self.parent[idx];
            self.parent[idx] = root;
            idx = next;
        }
        root
    }
}

fn count_connection_update_rank(
    eq: &dae::Equation,
    variables: &IndexMap<rumoca_core::VarName, dae::Variable>,
    connection_rank: &mut ConnectionUpdateRank,
) -> Option<usize> {
    let (lhs, rhs) = connection_update_var_refs(eq)?;
    let lhs_nodes = expand_connection_rank_nodes(&lhs, eq.scalar_count, variables)?;
    let rhs_nodes = expand_connection_rank_nodes(&rhs, eq.scalar_count, variables)?;
    if lhs_nodes.len() != rhs_nodes.len() {
        return None;
    }
    Some(
        lhs_nodes
            .into_iter()
            .zip(rhs_nodes)
            .filter(|(lhs, rhs)| connection_rank.add_edge(lhs.clone(), rhs.clone()))
            .count(),
    )
}

fn count_continuous_connection_rank(
    eq: &dae::Equation,
    dae_model: &dae::Dae,
    connection_rank: &mut ConnectionUpdateRank,
) -> Option<usize> {
    let names = eq_binary_var_ref_names(&eq.rhs);
    if names.len() != 2 {
        return None;
    }
    let lhs_nodes = expand_continuous_connection_rank_nodes(&names[0], eq.scalar_count, dae_model)?;
    let rhs_nodes = expand_continuous_connection_rank_nodes(&names[1], eq.scalar_count, dae_model)?;
    if lhs_nodes.len() != rhs_nodes.len() {
        return None;
    }
    Some(
        lhs_nodes
            .into_iter()
            .zip(rhs_nodes)
            .filter(|(lhs, rhs)| connection_rank.add_edge(lhs.clone(), rhs.clone()))
            .count(),
    )
}

fn connection_update_var_refs(
    eq: &dae::Equation,
) -> Option<(rumoca_core::VarName, rumoca_core::VarName)> {
    let lhs = eq.lhs.as_ref()?.clone();
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = &eq.rhs
    else {
        return None;
    };
    Some((
        lhs,
        append_connection_rank_subscripts(name.var_name().as_str(), subscripts),
    ))
}

fn append_connection_rank_subscripts(
    name: &str,
    subscripts: &[rumoca_core::Subscript],
) -> rumoca_core::VarName {
    if subscripts.is_empty() {
        return rumoca_core::VarName::new(name);
    }
    let rendered = subscripts
        .iter()
        .map(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } => value.to_string(),
            rumoca_core::Subscript::Colon { .. } => ":".to_string(),
            rumoca_core::Subscript::Expr { expr, .. } => format!("{expr:?}"),
        })
        .collect::<Vec<_>>()
        .join(",");
    rumoca_core::VarName::new(format!("{name}[{rendered}]"))
}

fn expand_connection_rank_nodes(
    name: &rumoca_core::VarName,
    scalar_count: usize,
    variables: &IndexMap<rumoca_core::VarName, dae::Variable>,
) -> Option<Vec<rumoca_core::VarName>> {
    if scalar_count <= 1 {
        return Some(vec![name.clone()]);
    }
    let variable = variables.get(name)?;
    if variable.size() < scalar_count {
        return None;
    }
    Some(
        (1..=scalar_count)
            .map(|idx| rumoca_core::VarName::new(format!("{}[{idx}]", name.as_str())))
            .collect(),
    )
}

fn expand_continuous_connection_rank_nodes(
    name: &rumoca_core::VarName,
    scalar_count: usize,
    dae_model: &dae::Dae,
) -> Option<Vec<rumoca_core::VarName>> {
    if scalar_count <= 1 {
        return Some(vec![name.clone()]);
    }
    let variable = dae_model
        .variables
        .states
        .get(name)
        .or_else(|| dae_model.variables.algebraics.get(name))
        .or_else(|| dae_model.variables.outputs.get(name))
        .or_else(|| dae_model.variables.inputs.get(name))?;
    if variable.size() < scalar_count {
        return None;
    }
    Some(
        (1..=scalar_count)
            .map(|idx| rumoca_core::VarName::new(format!("{}[{idx}]", name.as_str())))
            .collect(),
    )
}

fn count_condition_memory_equation_scalars(dae_model: &dae::Dae) -> usize {
    let discrete_valued_names = dae_model
        .variables
        .discrete_valued
        .keys()
        .cloned()
        .collect();
    dae_model
        .conditions
        .equations
        .iter()
        .filter(|eq| equation_lhs_matches_name(eq, &discrete_valued_names))
        .map(|eq| eq.scalar_count)
        .sum()
}

fn count_referenced_discrete_real_unknown_scalars(dae_model: &dae::Dae) -> usize {
    count_referenced_update_unknown_scalars(
        &dae_model.variables.discrete_reals,
        &dae_model.variables.inputs,
        dae_model.discrete.real_updates.iter(),
    )
}

fn count_referenced_discrete_valued_unknown_scalars(dae_model: &dae::Dae) -> usize {
    let mut referenced = IndexMap::new();
    let mut residual_scalars = 0usize;
    let discrete_input_names = metadata_discrete_input_names(dae_model);
    let target_scalar_counts = collect_update_target_scalar_counts(
        &dae_model.variables.discrete_valued,
        dae_model
            .discrete
            .valued_updates
            .iter()
            .chain(dae_model.conditions.equations.iter()),
    );
    for eq in &dae_model.discrete.valued_updates {
        if eq.lhs.is_some() {
            add_update_target_scalar_counts(
                eq,
                &dae_model.variables.discrete_valued,
                &mut referenced,
            );
            if is_discrete_connection_update_origin(eq.origin.as_str()) {
                insert_complete_connection_variable_names_from_expression(
                    eq,
                    &eq.rhs,
                    &dae_model.variables.discrete_valued,
                    &target_scalar_counts,
                    &mut referenced,
                );
            }
        } else if expression_references_included_variable(
            &eq.rhs,
            &dae_model.variables.discrete_valued,
            &dae_model.variables.inputs,
            &discrete_input_names,
        ) {
            residual_scalars += eq.scalar_count;
        }
    }
    for eq in &dae_model.conditions.equations {
        if eq.lhs.is_some() {
            add_update_target_scalar_counts(
                eq,
                &dae_model.variables.discrete_valued,
                &mut referenced,
            );
        }
    }
    if std::env::var_os("RUMOCA_DEBUG_DISCRETE_UNKNOWNS").is_some() {
        for (name, variable) in &dae_model.variables.discrete_valued {
            let excluded = variable_overlaps_any(name, &dae_model.variables.inputs)
                || name_matches_set(name, &discrete_input_names);
            let count = referenced
                .get(name)
                .copied()
                .unwrap_or(0)
                .min(variable.size());
            if excluded || count < variable.size() {
                eprintln!(
                    "DEBUG DISCRETE UNKNOWN COUNT: name={} size={} count={} excluded={} causality={:?}",
                    name,
                    variable.size(),
                    count,
                    excluded,
                    variable.causality
                );
            }
        }
    }
    residual_scalars
        + count_referenced_scalar_counts(
            &dae_model.variables.discrete_valued,
            &dae_model.variables.inputs,
            &discrete_input_names,
            &referenced,
        )
}

fn metadata_discrete_input_names(dae_model: &dae::Dae) -> HashSet<rumoca_core::VarName> {
    dae_model
        .metadata
        .discrete_input_names
        .iter()
        .map(rumoca_core::VarName::new)
        .collect()
}

fn count_referenced_update_unknown_scalars<'a>(
    variables: &'a indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    excluded: &'a indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    equations: impl Iterator<Item = &'a dae::Equation>,
) -> usize {
    let equations = equations.collect::<Vec<_>>();
    let target_scalar_counts =
        collect_update_target_scalar_counts(variables, equations.iter().copied());
    let mut referenced = IndexMap::new();
    let mut residual_scalars = 0usize;
    for eq in equations {
        if eq.lhs.is_some() {
            add_update_target_scalar_counts(eq, variables, &mut referenced);
            if is_discrete_connection_update_origin(eq.origin.as_str()) {
                insert_complete_connection_variable_names_from_expression(
                    eq,
                    &eq.rhs,
                    variables,
                    &target_scalar_counts,
                    &mut referenced,
                );
            }
        } else if expression_references_included_variable(
            &eq.rhs,
            variables,
            excluded,
            &HashSet::new(),
        ) {
            residual_scalars += eq.scalar_count;
        }
    }
    residual_scalars
        + count_referenced_scalar_counts(variables, excluded, &HashSet::new(), &referenced)
}

fn collect_update_target_scalar_counts<'a>(
    variables: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    equations: impl Iterator<Item = &'a dae::Equation>,
) -> IndexMap<rumoca_core::VarName, usize> {
    let mut counts = IndexMap::new();
    for eq in equations {
        let Some(lhs) = &eq.lhs else {
            continue;
        };
        for candidate in variables.keys() {
            if variable_names_overlap(lhs, candidate) {
                *counts.entry(candidate.clone()).or_insert(0) += eq.scalar_count;
            }
        }
    }
    counts
}

fn expression_references_included_variable(
    expr: &rumoca_core::Expression,
    variables: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    excluded: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    extra_excluded: &HashSet<rumoca_core::VarName>,
) -> bool {
    let included_names = variables
        .keys()
        .filter(|name| !variable_overlaps_any(name, excluded))
        .filter(|name| !name_matches_set(name, extra_excluded))
        .cloned()
        .collect();
    expression_references_names(expr, &included_names)
}

fn count_referenced_scalar_counts(
    variables: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    excluded: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    extra_excluded: &HashSet<rumoca_core::VarName>,
    referenced: &IndexMap<rumoca_core::VarName, usize>,
) -> usize {
    referenced
        .iter()
        .filter(|(name, _)| !variable_overlaps_any(name, excluded))
        .filter(|(name, _)| !name_matches_set(name, extra_excluded))
        .filter_map(|(name, count)| variables.get(name).map(|variable| (*count, variable)))
        .map(|(count, variable)| count.min(variable.size()))
        .sum()
}

fn variable_overlaps_any(
    name: &rumoca_core::VarName,
    variables: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
) -> bool {
    variables
        .keys()
        .any(|candidate| variable_names_overlap(name, candidate))
}

fn add_update_target_scalar_counts(
    eq: &dae::Equation,
    variables: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    referenced: &mut IndexMap<rumoca_core::VarName, usize>,
) {
    if let Some(lhs) = &eq.lhs {
        add_matching_variable_scalar_counts(lhs, eq.scalar_count, variables, referenced);
    }
}

fn insert_complete_connection_variable_names_from_expression(
    eq: &dae::Equation,
    expr: &rumoca_core::Expression,
    variables: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    target_scalar_counts: &IndexMap<rumoca_core::VarName, usize>,
    referenced: &mut IndexMap<rumoca_core::VarName, usize>,
) {
    let mut refs = IndexSet::new();
    expr.collect_var_refs(&mut refs);
    for name in refs {
        insert_complete_connection_variable_names(
            eq,
            &name,
            variables,
            target_scalar_counts,
            referenced,
        );
    }
}

fn insert_complete_connection_variable_names(
    eq: &dae::Equation,
    name: &rumoca_core::VarName,
    variables: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    target_scalar_counts: &IndexMap<rumoca_core::VarName, usize>,
    referenced: &mut IndexMap<rumoca_core::VarName, usize>,
) {
    for (candidate, variable) in variables {
        if variable_names_overlap(name, candidate)
            && (eq.scalar_count >= variable.size()
                || target_scalar_counts.get(candidate).copied().unwrap_or(0) >= variable.size())
        {
            add_referenced_scalar_count(candidate, variable.size(), referenced);
        }
    }
}

fn add_matching_variable_scalar_counts(
    name: &rumoca_core::VarName,
    scalar_count: usize,
    variables: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    referenced: &mut IndexMap<rumoca_core::VarName, usize>,
) {
    for candidate in variables.keys() {
        if variable_names_overlap(name, candidate) {
            add_referenced_scalar_count(candidate, scalar_count, referenced);
        }
    }
}

fn add_referenced_scalar_count(
    name: &rumoca_core::VarName,
    scalar_count: usize,
    referenced: &mut IndexMap<rumoca_core::VarName, usize>,
) {
    *referenced.entry(name.clone()).or_insert(0) += scalar_count;
}

fn variable_names_overlap(lhs: &rumoca_core::VarName, rhs: &rumoca_core::VarName) -> bool {
    if lhs == rhs {
        return true;
    }
    if var_name_base_overlaps(lhs, rhs) || var_name_base_overlaps(rhs, lhs) {
        return true;
    }
    let lhs_prefix = format!("{}.", lhs.as_str());
    let rhs_prefix = format!("{}.", rhs.as_str());
    lhs.as_str().starts_with(&rhs_prefix) || rhs.as_str().starts_with(&lhs_prefix)
}

fn var_name_base_overlaps(name: &rumoca_core::VarName, candidate: &rumoca_core::VarName) -> bool {
    let Some(base_name) = dae::component_base_name(name.as_str()) else {
        return false;
    };
    if base_name == name.as_str() {
        return false;
    }
    if base_name == candidate.as_str() {
        return true;
    }
    let base_prefix = format!("{base_name}.");
    candidate.as_str().starts_with(&base_prefix)
}

fn update_equation_targets_name(eq: &dae::Equation, names: &HashSet<rumoca_core::VarName>) -> bool {
    if equation_lhs_matches_name(eq, names) {
        return true;
    }
    if eq.lhs.is_some() {
        return false;
    }
    expression_references_names(&eq.rhs, names)
}

fn equation_lhs_matches_name(eq: &dae::Equation, names: &HashSet<rumoca_core::VarName>) -> bool {
    eq.lhs
        .as_ref()
        .is_some_and(|name| name_matches_set(name, names))
}

fn expression_references_names(
    expr: &rumoca_core::Expression,
    names: &HashSet<rumoca_core::VarName>,
) -> bool {
    let mut refs = IndexSet::new();
    expr.collect_var_refs(&mut refs);
    refs.into_iter().any(|name| name_matches_set(&name, names))
}

fn equation_references_continuous_unknown(
    eq: &dae::Equation,
    continuous_unknowns: &HashSet<rumoca_core::VarName>,
) -> bool {
    if eq
        .lhs
        .as_ref()
        .is_some_and(|name| name_matches_set(name, continuous_unknowns))
    {
        return true;
    }

    let mut refs = IndexSet::new();
    eq.rhs.collect_var_refs(&mut refs);
    refs.into_iter()
        .any(|name| name_matches_set(&name, continuous_unknowns))
}

fn equation_references_input(
    eq: &dae::Equation,
    input_names: &HashSet<rumoca_core::VarName>,
) -> bool {
    if eq
        .lhs
        .as_ref()
        .is_some_and(|name| name_matches_set(name, input_names))
    {
        return true;
    }

    let mut refs = IndexSet::new();
    eq.rhs.collect_var_refs(&mut refs);
    refs.into_iter()
        .any(|name| name_matches_set(&name, input_names))
}
fn name_matches_set(name: &rumoca_core::VarName, names: &HashSet<rumoca_core::VarName>) -> bool {
    if names.contains(name) {
        return true;
    }
    if let Some(base_name) = dae::component_base_name(name.as_str())
        && base_name != name.as_str()
    {
        let base = rumoca_core::VarName::new(base_name.clone());
        if names.contains(&base) {
            return true;
        }
        let base_prefix = format!("{base_name}.");
        if names
            .iter()
            .any(|candidate| candidate.as_str().starts_with(&base_prefix))
        {
            return true;
        }
    }
    let prefix = format!("{}.", name.as_str());
    names
        .iter()
        .any(|candidate| candidate.as_str().starts_with(&prefix))
}

pub(crate) fn is_connection_origin(origin: &str) -> bool {
    origin.starts_with("connect(") || origin.starts_with("connection equation:")
}

fn is_discrete_connection_update_origin(origin: &str) -> bool {
    is_connection_origin(origin) || origin.starts_with("explicit connection equation:")
}

/// Extract VarName references from a `lhs - rhs` binary expression.
fn eq_binary_var_refs(expr: &rumoca_core::Expression) -> Vec<&rumoca_core::VarName> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        span: _,
    } = expr
    else {
        return Vec::new();
    };

    let mut names = Vec::with_capacity(2);
    if let rumoca_core::Expression::VarRef { name, .. } = lhs.as_ref() {
        names.push(name.var_name());
    }
    if let rumoca_core::Expression::VarRef { name, .. } = rhs.as_ref() {
        names.push(name.var_name());
    }
    names
}

fn eq_binary_var_ref_names(expr: &rumoca_core::Expression) -> Vec<rumoca_core::VarName> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        span: _,
    } = expr
    else {
        return Vec::new();
    };

    let mut names = Vec::with_capacity(2);
    if let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    {
        names.push(append_connection_rank_subscripts(
            name.var_name().as_str(),
            subscripts,
        ));
    }
    if let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = rhs.as_ref()
    {
        names.push(append_connection_rank_subscripts(
            name.var_name().as_str(),
            subscripts,
        ));
    }
    names
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;

    fn scalar_eq(count: usize) -> dae::Equation {
        scalar_eq_with_lhs("x", count)
    }

    fn scalar_eq_with_lhs(lhs_name: &str, count: usize) -> dae::Equation {
        dae::Equation {
            lhs: Some(rumoca_core::VarName::new(lhs_name)),
            rhs: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new(lhs_name).into(),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }),
                rhs: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            span: Span::DUMMY,
            origin: "test".to_string(),
            scalar_count: count,
        }
    }

    fn scalar_assignment_with_rhs_ref(lhs_name: &str, rhs_name: &str) -> dae::Equation {
        dae::Equation {
            lhs: Some(rumoca_core::VarName::new(lhs_name)),
            rhs: rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new(rhs_name).into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            },
            span: Span::DUMMY,
            origin: "test".to_string(),
            scalar_count: 1,
        }
    }

    fn connection_assignment_with_rhs_ref(lhs_name: &str, rhs_name: &str) -> dae::Equation {
        dae::Equation {
            origin: "explicit connection equation: a = b".to_string(),
            ..scalar_assignment_with_rhs_ref(lhs_name, rhs_name)
        }
    }

    fn connection_assignment_with_count(
        lhs_name: &str,
        rhs_name: &str,
        scalar_count: usize,
    ) -> dae::Equation {
        dae::Equation {
            scalar_count,
            ..connection_assignment_with_rhs_ref(lhs_name, rhs_name)
        }
    }

    fn connection_assignment_with_rhs_index(
        lhs_name: &str,
        rhs_name: &str,
        rhs_index: i64,
    ) -> dae::Equation {
        dae::Equation {
            lhs: Some(rumoca_core::VarName::new(lhs_name)),
            rhs: rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new(rhs_name).into(),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    rhs_index,
                    rumoca_core::Span::DUMMY,
                )],
                span: rumoca_core::Span::DUMMY,
            },
            span: Span::DUMMY,
            origin: "explicit connection equation: a = b[1]".to_string(),
            scalar_count: 1,
        }
    }

    fn binary_eq(lhs_name: &str, rhs_name: &str, origin: &str) -> dae::Equation {
        dae::Equation {
            lhs: None,
            rhs: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var_ref(lhs_name)),
                rhs: Box::new(var_ref(rhs_name)),
                span: rumoca_core::Span::DUMMY,
            },
            span: Span::DUMMY,
            origin: origin.to_string(),
            scalar_count: 1,
        }
    }

    fn binary_eq_indexed(
        lhs_name: &str,
        lhs_index: i64,
        rhs_name: &str,
        rhs_index: i64,
        origin: &str,
    ) -> dae::Equation {
        dae::Equation {
            lhs: None,
            rhs: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var_ref_indexed(lhs_name, lhs_index)),
                rhs: Box::new(var_ref_indexed(rhs_name, rhs_index)),
                span: rumoca_core::Span::DUMMY,
            },
            span: Span::DUMMY,
            origin: origin.to_string(),
            scalar_count: 1,
        }
    }

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new(name).into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn var_ref_indexed(name: &str, index: i64) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new(name).into(),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                index,
                rumoca_core::Span::DUMMY,
            )],
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn dae_with_unknown_scalars(unknown_scalars: i64) -> dae::Dae {
        let mut dae = dae::Dae::default();
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("x"),
            dae::Variable {
                name: rumoca_core::VarName::new("x"),
                dims: vec![unknown_scalars],
                ..Default::default()
            },
        );
        dae
    }

    fn algebraic_vector(name: &str, size: i64) -> dae::Variable {
        dae::Variable {
            name: rumoca_core::VarName::new(name),
            dims: vec![size],
            ..Default::default()
        }
    }

    fn scalar_input(name: &str) -> dae::Variable {
        dae::Variable {
            name: rumoca_core::VarName::new(name),
            ..Default::default()
        }
    }

    fn discrete_var(name: &str) -> dae::Variable {
        dae::Variable {
            name: rumoca_core::VarName::new(name),
            ..Default::default()
        }
    }

    fn discrete_vector_var(name: &str, size: i64) -> dae::Variable {
        dae::Variable {
            name: rumoca_core::VarName::new(name),
            dims: vec![size],
            ..Default::default()
        }
    }

    #[test]
    fn test_balance_clamps_overconstrained_interface_to_deficit() {
        let mut dae = dae_with_unknown_scalars(4);
        dae.continuous.equations.push(scalar_eq(4));
        dae.metadata.overconstrained_interface_count = 9;
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_uses_only_needed_overconstrained_interface() {
        let mut dae = dae_with_unknown_scalars(4);
        dae.continuous.equations.push(scalar_eq(3));
        dae.metadata.overconstrained_interface_count = 9;
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_applies_oc_interface_even_with_break_edges() {
        let mut dae = dae_with_unknown_scalars(10);
        dae.continuous.equations.push(scalar_eq(1));
        dae.metadata.overconstrained_interface_count = 9;
        dae.metadata.oc_break_edge_scalar_count = 12;
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_clamps_interface_flow_to_remaining_deficit() {
        let mut dae = dae_with_unknown_scalars(4);
        dae.continuous.equations.push(scalar_eq(4));
        dae.metadata.interface_flow_count = 3;
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_clamps_stream_interface_to_remaining_deficit() {
        let mut dae = dae_with_unknown_scalars(4);
        dae.continuous.equations.push(scalar_eq(4));
        dae.metadata.stream_interface_equation_count = 3;
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_uses_stream_interface_to_close_deficit_only() {
        let mut dae = dae_with_unknown_scalars(5);
        dae.continuous.equations.push(scalar_eq(3));
        dae.metadata.stream_interface_equation_count = 9;
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_uses_interface_flow_to_close_deficit_only() {
        let mut dae = dae_with_unknown_scalars(5);
        dae.continuous.equations.push(scalar_eq(3));
        dae.metadata.interface_flow_count = 9;
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_ignores_unconstrained_discrete_real_declaration() {
        let mut dae = dae::Dae::default();
        dae.variables
            .discrete_reals
            .insert(rumoca_core::VarName::new("z"), discrete_var("z"));
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_counts_discrete_updates_against_discrete_unknowns() {
        let mut dae = dae::Dae::default();
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("m"), discrete_var("m"));
        dae.discrete.valued_updates.push(scalar_eq_with_lhs("m", 1));
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_ignores_solved_discrete_update_rhs_refs_as_unknowns() {
        let mut dae = dae::Dae::default();
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("guard"), discrete_var("guard"));
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("m"), discrete_var("m"));
        dae.discrete
            .valued_updates
            .push(scalar_assignment_with_rhs_ref("m", "guard"));
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_counts_discrete_connection_update_rhs_refs_as_unknowns() {
        let mut dae = dae::Dae::default();
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("a"), discrete_var("a"));
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("b"), discrete_var("b"));
        dae.discrete
            .valued_updates
            .push(connection_assignment_with_rhs_ref("a", "b"));
        assert_eq!(balance(&dae), -1);
    }

    #[test]
    fn test_balance_ignores_partial_vector_connection_rhs_refs_as_unknowns() {
        let mut dae = dae::Dae::default();
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("a"), discrete_var("a"));
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("b"), discrete_vector_var("b", 2));
        dae.discrete
            .valued_updates
            .push(connection_assignment_with_rhs_ref("a", "b"));
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_counts_discrete_connection_cycles_by_graph_rank() {
        let mut dae = dae::Dae::default();
        for name in ["a", "b", "c"] {
            dae.variables
                .discrete_valued
                .insert(rumoca_core::VarName::new(name), discrete_var(name));
        }
        dae.discrete
            .valued_updates
            .push(scalar_assignment_with_rhs_ref("a", "source"));
        dae.discrete
            .valued_updates
            .push(connection_assignment_with_rhs_ref("a", "b"));
        dae.discrete
            .valued_updates
            .push(connection_assignment_with_rhs_ref("b", "c"));
        dae.discrete
            .valued_updates
            .push(connection_assignment_with_rhs_ref("c", "a"));
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_skips_discrete_connection_between_component_defined_targets() {
        let mut dae = dae::Dae::default();
        for name in ["a", "b"] {
            dae.variables
                .discrete_valued
                .insert(rumoca_core::VarName::new(name), discrete_var(name));
            dae.discrete
                .valued_updates
                .push(scalar_assignment_with_rhs_ref(name, "source"));
        }
        dae.discrete
            .valued_updates
            .push(connection_assignment_with_rhs_ref("a", "b"));
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_matches_subscripted_connection_rhs_to_vector_anchor() {
        let mut dae = dae::Dae::default();
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("a"), discrete_var("a"));
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("b"), discrete_vector_var("b", 2));
        dae.discrete
            .valued_updates
            .push(scalar_assignment_with_rhs_ref("a", "source"));
        dae.discrete.valued_updates.push(dae::Equation {
            lhs: Some(rumoca_core::VarName::new("b")),
            scalar_count: 2,
            ..scalar_assignment_with_rhs_ref("b", "source")
        });
        dae.discrete
            .valued_updates
            .push(connection_assignment_with_rhs_index("a", "b", 1));
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_counts_vector_discrete_connection_cycles_by_scalar_rank() {
        let mut dae = dae::Dae::default();
        for name in ["a", "b", "c"] {
            dae.variables.discrete_valued.insert(
                rumoca_core::VarName::new(name),
                discrete_vector_var(name, 2),
            );
        }
        dae.discrete.valued_updates.push(dae::Equation {
            scalar_count: 2,
            ..scalar_assignment_with_rhs_ref("a", "source")
        });
        dae.discrete
            .valued_updates
            .push(connection_assignment_with_count("a", "b", 2));
        dae.discrete
            .valued_updates
            .push(connection_assignment_with_count("b", "c", 2));
        dae.discrete
            .valued_updates
            .push(connection_assignment_with_count("c", "a", 2));
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_counts_continuous_connection_cycles_by_scalar_rank() {
        let mut dae = dae::Dae::default();
        for name in ["a", "b", "c"] {
            dae.variables
                .algebraics
                .insert(rumoca_core::VarName::new(name), algebraic_vector(name, 3));
        }
        let mut ab = binary_eq("a", "b", "connection equation: a = b");
        ab.scalar_count = 3;
        let mut bc = binary_eq("b", "c", "connection equation: b = c");
        bc.scalar_count = 3;
        let mut ca = binary_eq("c", "a", "connection equation: c = a");
        ca.scalar_count = 3;
        dae.continuous.equations.extend([ab, bc, ca]);

        let detail = balance_detail(&dae);
        assert_eq!(detail.alg_unknowns, 9);
        assert_eq!(
            detail.f_x_scalar, 6,
            "three vector aliases over the same connection set have rank two per scalar"
        );
    }

    #[test]
    fn test_balance_counts_mixed_indexed_continuous_connection_rank() {
        let mut dae = dae::Dae::default();
        for name in ["a", "b", "c"] {
            dae.variables
                .algebraics
                .insert(rumoca_core::VarName::new(name), algebraic_vector(name, 3));
        }
        let mut ab = binary_eq("a", "b", "connection equation: a = b");
        ab.scalar_count = 3;
        let mut ca = binary_eq("c", "a", "connection equation: c = a");
        ca.scalar_count = 3;
        dae.continuous.equations.push(ab);
        for idx in 1..=3 {
            dae.continuous.equations.push(binary_eq_indexed(
                "b",
                idx,
                "c",
                idx,
                "connection equation: b[i] = c[i]",
            ));
        }
        dae.continuous.equations.push(ca);

        let detail = balance_detail(&dae);
        assert_eq!(detail.alg_unknowns, 9);
        assert_eq!(
            detail.f_x_scalar, 6,
            "indexed scalar aliases must join the same rank nodes as vector aliases"
        );
    }

    #[test]
    fn test_balance_expands_vector_connection_under_arrayed_component_name() {
        let mut dae = dae::Dae::default();
        for name in ["zon[1].heaGai.qGai_flow", "zon[1].qGai_flow"] {
            dae.variables
                .algebraics
                .insert(rumoca_core::VarName::new(name), algebraic_vector(name, 3));
        }
        let mut connection = binary_eq(
            "zon[1].heaGai.qGai_flow",
            "zon[1].qGai_flow",
            "connection equation: zon[1].heaGai.qGai_flow = zon[1].qGai_flow",
        );
        connection.scalar_count = 3;
        dae.continuous.equations.push(connection);
        dae.continuous
            .equations
            .push(scalar_eq_with_lhs("zon[1].qGai_flow", 3));

        let detail = balance_detail(&dae);
        assert_eq!(detail.alg_unknowns, 6);
        assert_eq!(detail.f_x_scalar, 6);
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_counts_continuous_connection_rank_through_inputs() {
        let mut dae = dae::Dae::default();
        for name in ["a", "b"] {
            dae.variables
                .algebraics
                .insert(rumoca_core::VarName::new(name), algebraic_vector(name, 3));
        }
        dae.variables.inputs.insert(
            rumoca_core::VarName::new("u"),
            dae::Variable {
                name: rumoca_core::VarName::new("u"),
                dims: vec![3],
                ..Default::default()
            },
        );

        let mut au = binary_eq("a", "u", "connection equation: a = u");
        au.scalar_count = 3;
        let mut ub = binary_eq("u", "b", "connection equation: u = b");
        ub.scalar_count = 3;
        let mut ba = binary_eq("b", "a", "connection equation: b = a");
        ba.scalar_count = 3;
        dae.continuous.equations.extend([au, ub, ba]);

        let detail = balance_detail(&dae);
        assert_eq!(detail.alg_unknowns, 6);
        assert_eq!(
            detail.f_x_scalar, 6,
            "input connector nodes must join the same continuous connection rank graph"
        );
    }

    #[test]
    fn test_balance_counts_discrete_real_updates_against_discrete_real_unknowns() {
        let mut dae = dae::Dae::default();
        dae.variables
            .discrete_reals
            .insert(rumoca_core::VarName::new("z"), discrete_var("z"));
        dae.discrete.real_updates.push(scalar_eq_with_lhs("z", 1));
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_counts_residual_discrete_real_updates() {
        let mut dae = dae::Dae::default();
        dae.variables
            .discrete_reals
            .insert(rumoca_core::VarName::new("z"), discrete_var("z"));
        let mut eq = scalar_eq_with_lhs("z", 1);
        eq.lhs = None;
        dae.discrete.real_updates.push(eq);
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_ignores_state_reinit_updates_for_static_balance() {
        let mut dae = dae::Dae::default();
        dae.variables
            .states
            .insert(rumoca_core::VarName::new("x"), discrete_var("x"));
        dae.continuous.equations.push(scalar_eq(1));
        dae.discrete.real_updates.push(scalar_eq(1));
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_counts_condition_equations_against_condition_unknowns() {
        let mut dae = dae::Dae::default();
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("c"), discrete_var("c"));
        dae.conditions.equations.push(scalar_eq_with_lhs("c", 1));
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_ignores_condition_rhs_refs_as_unknowns() {
        let mut dae = dae::Dae::default();
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("guard"), discrete_var("guard"));
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("c"), discrete_var("c"));
        dae.conditions
            .equations
            .push(scalar_assignment_with_rhs_ref("c", "guard"));
        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn component_defined_targets_include_second_binary_ref_when_it_is_the_only_unknown() {
        let mut dae = dae::Dae::default();
        dae.variables
            .algebraics
            .insert(rumoca_core::VarName::new("y"), discrete_var("y"));
        dae.variables
            .inputs
            .insert(rumoca_core::VarName::new("u"), scalar_input("u"));
        dae.variables.inputs.insert(
            rumoca_core::VarName::new("external"),
            scalar_input("external"),
        );

        dae.continuous
            .equations
            .push(binary_eq("u", "y", "component equation"));
        dae.continuous
            .equations
            .push(binary_eq("y", "external", "connect(y, external)"));

        assert_eq!(
            balance(&dae),
            0,
            "the connection alias should be redundant because y is already constrained"
        );
    }

    #[test]
    fn component_defined_targets_do_not_treat_two_unknown_residual_as_two_definitions() {
        let mut dae = dae::Dae::default();
        for name in ["a", "b"] {
            dae.variables
                .algebraics
                .insert(rumoca_core::VarName::new(name), discrete_var(name));
        }
        dae.variables.inputs.insert(
            rumoca_core::VarName::new("external"),
            scalar_input("external"),
        );

        dae.continuous
            .equations
            .push(binary_eq("a", "b", "component equation"));
        dae.continuous
            .equations
            .push(binary_eq("b", "external", "connect(b, external)"));

        assert_eq!(
            balance(&dae),
            0,
            "the connection still supplies the second equation for coupled unknowns"
        );
    }
}
