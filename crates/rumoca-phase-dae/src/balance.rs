//! DAE balance arithmetic.
//!
//! This is the single canonical implementation of balance checking for the
//! Rumoca DAE IR. All other crates must call these functions rather than
//! reimplementing the formula (AGENTS.md: "Balance arithmetic lives in
//! `rumoca-phase-dae`").

use std::collections::HashSet;

use indexmap::{IndexMap, IndexSet};
use rumoca_core::DefId;
use rumoca_ir_dae as dae;

#[path = "balance_initial_closure.rs"]
mod balance_initial_closure;
pub use balance_initial_closure::InitialClosureBalanceDetail;
#[path = "balance_alias.rs"]
mod balance_alias;
use balance_alias::{
    is_absent_lhs_component_alias, is_input_forwarding_connection_alias,
    is_non_constraining_binding_alias, is_vector_forwarding_alias,
};

pub type BalanceResult<T> = Result<T, BalanceError>;

#[derive(Debug, Clone, thiserror::Error)]
pub enum BalanceError {
    #[error("invalid DAE balance contract: missing discrete variable metadata for `{name}`")]
    MissingDiscreteVariableMetadata {
        name: rumoca_core::VarName,
        span: Option<rumoca_core::Span>,
    },
}

impl BalanceError {
    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        match self {
            Self::MissingDiscreteVariableMetadata { span, .. } => *span,
        }
    }
}

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

impl BalanceDetail {
    pub fn balance(&self) -> i64 {
        balance_from_detail(self)
    }

    pub fn is_balanced(&self) -> bool {
        self.balance() == 0
    }

    pub fn equations_unknowns(&self) -> (usize, usize) {
        equations_unknowns_from_detail(self)
    }
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
            "  Equations (raw): f_x({}) + f_z({}) + f_m({}) + f_c({}) + algo({}) + when({}) + iflow({}) + stream({}) + oc({}) - brk({})",
            self.f_x_scalar,
            self.f_z_scalar,
            self.f_m_scalar,
            self.f_c_scalar,
            self.algorithm_outputs,
            self.when_eq_scalar,
            self.interface_flow_count,
            self.stream_interface_equation_count,
            self.overconstrained_interface_count,
            self.oc_break_edge_scalar_count,
        )
    }
}

/// Get the balance: equations - unknowns.
///
/// Positive means over-determined, negative means under-determined.
pub fn balance(dae_model: &dae::Dae) -> BalanceResult<i64> {
    let detail = balance_detail(dae_model)?;
    let raw_balance = balance_from_detail(&detail);
    let input_alias_deficit = input_only_discrete_alias_deficit(dae_model);
    if input_alias_deficit > 0 && raw_balance == -(input_alias_deficit as i64) {
        return Ok(0);
    }
    Ok(raw_balance)
}

/// Check if the system is balanced (equations match unknowns).
pub fn is_balanced(dae_model: &dae::Dae) -> BalanceResult<bool> {
    Ok(balance(dae_model)? == 0)
}

/// Return strict DAE admission balance after initialization deficit closure.
///
/// Raw continuous/event balance remains canonical for steady-state matching.
/// This helper only admits a raw underdetermined DAE when generated
/// initialization equations close the exact missing scalar count. It does not
/// use initialization equations to hide overdetermined systems.
pub fn initial_closure_balance_detail(
    dae_model: &dae::Dae,
) -> BalanceResult<InitialClosureBalanceDetail> {
    let (scalar_equations, scalar_unknowns) = equations_unknowns(dae_model)?;
    Ok(initial_closure_balance_detail_from_counts(
        dae_model,
        scalar_equations,
        scalar_unknowns,
    ))
}

pub fn is_balanced_for_admission(dae_model: &dae::Dae) -> BalanceResult<bool> {
    let raw_balance = balance(dae_model)?;
    if raw_balance == 0 {
        return Ok(true);
    }
    if raw_balance > 0 {
        return Ok(false);
    }
    Ok(initial_closure_balance_detail(dae_model)?.is_admissible())
}

/// Return detailed breakdown of the balance calculation components.
pub fn balance_detail(dae_model: &dae::Dae) -> BalanceResult<BalanceDetail> {
    let state_unknowns: usize = dae_model.variables.states.values().map(|v| v.size()).sum();
    let alg_unknowns: usize = dae_model
        .variables
        .algebraics
        .values()
        .map(|v| v.size())
        .sum();
    let output_unknowns: usize = dae_model.variables.outputs.values().map(|v| v.size()).sum();
    let discrete_real_unknowns = count_referenced_discrete_real_unknown_scalars(dae_model);
    let discrete_valued_unknowns = count_referenced_discrete_valued_unknown_scalars(dae_model)?;
    // See balance(): algorithms/when-equations are represented through
    // lowered B.1 rows by this phase, not counted as separate terms here.
    let algorithm_outputs = 0usize;
    let when_eq_scalar = 0usize;
    let f_x_scalar = count_f_x_scalars_with_continuous_unknowns(dae_model);
    let f_z_scalar = count_discrete_real_update_scalars(dae_model);
    let f_m_scalar = count_discrete_valued_update_scalars(dae_model)?;
    let f_c_scalar = count_condition_memory_equation_scalars(dae_model);
    Ok(BalanceDetail {
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
    })
}

/// Return `(effective_equations, unknowns)` for unbalanced error diagnostics.
///
/// Uses the same clamping logic as `balance()` so the gate and the error
/// payload always agree.
pub fn equations_unknowns(dae_model: &dae::Dae) -> BalanceResult<(usize, usize)> {
    let detail = balance_detail(dae_model)?;
    Ok(detail.equations_unknowns())
}

fn initial_closure_balance_detail_from_counts(
    dae_model: &dae::Dae,
    scalar_equations: usize,
    scalar_unknowns: usize,
) -> InitialClosureBalanceDetail {
    let scalar_equations = scalar_equations as i64;
    let scalar_unknowns = scalar_unknowns as i64;
    let deficit_before = (scalar_unknowns - scalar_equations).max(0);
    let initial_equation_scalars = dae_model
        .initialization
        .equations
        .iter()
        .map(|eq| eq.scalar_count as i64)
        .sum::<i64>();
    let initial_algorithm_scalars = 0;
    let overconstrained_root_gauge_scalars =
        dae_model.metadata.overconstrained_root_gauge_count as i64;
    let overconstrained_break_edge_scalars = dae_model.metadata.oc_break_edge_scalar_count as i64;
    let closure_used = (initial_equation_scalars
        + initial_algorithm_scalars
        + overconstrained_root_gauge_scalars
        + overconstrained_break_edge_scalars)
        .min(deficit_before);
    let deficit_after = deficit_before - closure_used;
    InitialClosureBalanceDetail {
        scalar_equations: scalar_equations as usize,
        scalar_unknowns: scalar_unknowns as usize,
        deficit_before,
        overconstrained_root_gauge_scalars,
        overconstrained_break_edge_scalars,
        initial_equation_scalars,
        initial_algorithm_scalars,
        closure_used,
        deficit_after,
    }
}

fn balance_from_detail(detail: &BalanceDetail) -> i64 {
    let (equations, unknowns) = equations_unknowns_from_detail(detail);
    equations as i64 - unknowns as i64
}

fn equations_unknowns_from_detail(detail: &BalanceDetail) -> (usize, usize) {
    let unknowns = detail.state_unknowns
        + detail.alg_unknowns
        + detail.output_unknowns
        + detail.discrete_real_unknowns
        + detail.discrete_valued_unknowns;
    let brk = detail.oc_break_edge_scalar_count as i64;
    let available_oc_interface = detail.overconstrained_interface_count.max(0);
    let base_without_iflow = (detail.f_x_scalar
        + detail.f_z_scalar
        + detail.f_m_scalar
        + detail.f_c_scalar
        + detail.algorithm_outputs
        + detail.when_eq_scalar) as i64;
    let iflow_needed = (unknowns as i64 - base_without_iflow).max(0);
    let effective_iflow = (detail.interface_flow_count as i64).min(iflow_needed);
    let base_with_iflow = base_without_iflow + effective_iflow;
    let stream_needed = (unknowns as i64 - base_with_iflow).max(0);
    let effective_stream = (detail.stream_interface_equation_count as i64).min(stream_needed);
    let base_equations = base_with_iflow + effective_stream;
    let oc_needed = (unknowns as i64 - base_equations).max(0);
    let effective_oc_interface = available_oc_interface.min(oc_needed);
    let raw_equations = base_equations + effective_oc_interface;
    let raw_balance = raw_equations - unknowns as i64;
    let effective_brk = brk.min(raw_balance.max(0));
    let equations = (raw_equations - effective_brk) as usize;
    (equations, unknowns)
}

struct BalanceSymbolSet<'a> {
    names: &'a HashSet<rumoca_core::VarName>,
    /// Aggregate prefixes of `names` (`a.b` for `a.b.c`), so record- or
    /// component-level references count as referencing their scalarized
    /// members.
    prefixes: HashSet<rumoca_core::VarName>,
    def_ids: IndexSet<DefId>,
    ancestry: &'a IndexMap<DefId, Vec<DefId>>,
}

impl<'a> BalanceSymbolSet<'a> {
    fn new(dae_model: &'a dae::Dae, names: &'a HashSet<rumoca_core::VarName>) -> Self {
        let def_ids = names
            .iter()
            .filter_map(|name| variable_def_id(dae_model, name))
            .collect();
        let mut prefixes = HashSet::new();
        for name in names {
            prefixes.extend(name.structural_ancestors());
        }
        Self {
            names,
            prefixes,
            def_ids,
            ancestry: &dae_model.metadata.symbol_ancestry,
        }
    }

    fn matches_reference(&self, reference: &rumoca_core::Reference) -> bool {
        self.names.contains(reference.var_name())
            || self.prefixes.contains(reference.var_name())
            || reference
                .target_def_id()
                .is_some_and(|def_id| self.matches_def_id(def_id))
    }

    fn matches_variable(&self, name: &rumoca_core::VarName, variable: &dae::Variable) -> bool {
        self.names.contains(name)
            || variable_def_id_from_variable(variable)
                .is_some_and(|def_id| self.matches_def_id(def_id))
    }

    fn matches_def_id(&self, def_id: DefId) -> bool {
        self.def_ids.contains(&def_id)
            || self
                .ancestry
                .get(&def_id)
                .is_some_and(|chain| chain.iter().any(|ancestor| self.def_ids.contains(ancestor)))
            || self.def_ids.iter().any(|candidate| {
                self.ancestry
                    .get(candidate)
                    .is_some_and(|chain| chain.contains(&def_id))
            })
    }
}

fn variable_def_id(
    dae_model: &dae::Dae,
    name: &rumoca_core::VarName,
) -> Option<rumoca_core::DefId> {
    find_variable(dae_model, name)
        .and_then(|variable| variable.component_ref.as_ref())
        .and_then(|component_ref| component_ref.def_id)
}

fn find_variable<'a>(
    dae_model: &'a dae::Dae,
    name: &rumoca_core::VarName,
) -> Option<&'a dae::Variable> {
    dae_model
        .variables
        .states
        .get(name)
        .or_else(|| dae_model.variables.algebraics.get(name))
        .or_else(|| dae_model.variables.outputs.get(name))
        .or_else(|| dae_model.variables.inputs.get(name))
        .or_else(|| dae_model.variables.discrete_reals.get(name))
        .or_else(|| dae_model.variables.discrete_valued.get(name))
        .or_else(|| dae_model.variables.parameters.get(name))
        .or_else(|| dae_model.variables.constants.get(name))
}

pub(crate) fn count_f_x_scalars_with_continuous_unknowns(dae_model: &dae::Dae) -> usize {
    let continuous_unknowns = collect_continuous_unknown_names(dae_model);
    let input_names = collect_input_names(dae_model);
    let continuous_unknown_symbols = BalanceSymbolSet::new(dae_model, &continuous_unknowns);
    let input_symbols = BalanceSymbolSet::new(dae_model, &input_names);
    let output_names = collect_output_names(dae_model);
    let output_symbols = BalanceSymbolSet::new(dae_model, &output_names);
    let component_defined_targets =
        collect_component_defined_targets_for_balance(dae_model, &continuous_unknown_symbols);
    let component_defined_symbols = BalanceSymbolSet::new(dae_model, &component_defined_targets);
    dae_model
        .continuous
        .equations
        .iter()
        .filter(|eq| {
            equation_counts_for_balance(
                dae_model,
                eq,
                &continuous_unknown_symbols,
                &input_symbols,
                &output_symbols,
                &component_defined_symbols,
            )
        })
        .map(|eq| eq.scalar_count)
        .sum()
}

fn equation_counts_for_balance(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
    continuous_unknowns: &BalanceSymbolSet,
    input_names: &BalanceSymbolSet,
    output_names: &BalanceSymbolSet,
    component_defined_targets: &BalanceSymbolSet,
) -> bool {
    if eq.origin.starts_with("binding equation for")
        && is_non_constraining_binding_alias(
            eq,
            continuous_unknowns,
            output_names,
            component_defined_targets,
        )
    {
        return false;
    }
    if is_vector_forwarding_alias(
        eq,
        continuous_unknowns,
        output_names,
        component_defined_targets,
    ) {
        return false;
    }
    if eq.origin.starts_with("equation from ")
        && is_absent_lhs_component_alias(eq, continuous_unknowns, input_names)
    {
        return false;
    }
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
    if is_connection_origin(eq.origin.as_str())
        && is_input_forwarding_connection_alias(eq, continuous_unknowns, input_names)
    {
        return false;
    }
    if is_connection_origin(eq.origin.as_str())
        && is_absent_lhs_component_alias(eq, continuous_unknowns, input_names)
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
    if eq.origin.starts_with("equation from ") {
        return false;
    }
    // Preserve explicit user equations constraining interface inputs.
    equation_references_input(eq, input_names)
}

fn is_redundant_connection_alias(
    _dae_model: &dae::Dae,
    eq: &dae::Equation,
    continuous_unknowns: &BalanceSymbolSet,
    component_defined_targets: &BalanceSymbolSet,
) -> bool {
    let refs = eq_binary_var_refs(&eq.rhs);
    if refs.len() != 2 {
        return false;
    }
    let lhs = refs[0];
    let rhs = refs[1];

    let lhs_component_defined = component_defined_targets.matches_reference(lhs);
    let rhs_component_defined = component_defined_targets.matches_reference(rhs);
    let lhs_is_continuous_unknown = continuous_unknowns.matches_reference(lhs);
    let rhs_is_continuous_unknown = continuous_unknowns.matches_reference(rhs);

    if eq.scalar_count > 1 && lhs_component_defined {
        return true;
    }
    if lhs_component_defined && rhs_component_defined {
        return true;
    }
    (lhs_component_defined && !rhs_is_continuous_unknown)
        || (rhs_component_defined && !lhs_is_continuous_unknown)
}

fn collect_component_defined_targets_for_balance(
    dae_model: &dae::Dae,
    continuous_unknowns: &BalanceSymbolSet,
) -> HashSet<rumoca_core::VarName> {
    let mut targets = HashSet::new();
    for eq in &dae_model.continuous.equations {
        if is_connection_origin(eq.origin.as_str()) || eq.origin.starts_with("binding equation for")
        {
            continue;
        }
        let unknown_refs = eq_binary_var_refs(&eq.rhs)
            .into_iter()
            .filter(|name| continuous_unknowns.matches_reference(name))
            .collect::<Vec<_>>();
        if let [target] = unknown_refs.as_slice() {
            targets.insert(target.var_name().clone());
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

fn collect_output_names(dae_model: &dae::Dae) -> HashSet<rumoca_core::VarName> {
    dae_model.variables.outputs.keys().cloned().collect()
}

fn count_discrete_real_update_scalars(dae_model: &dae::Dae) -> usize {
    let discrete_real_names = dae_model.variables.discrete_reals.keys().cloned().collect();
    let discrete_real_symbols = BalanceSymbolSet::new(dae_model, &discrete_real_names);
    dae_model
        .discrete
        .real_updates
        .iter()
        .filter(|eq| update_equation_targets_symbol(eq, &discrete_real_symbols))
        .map(|eq| eq.scalar_count)
        .sum()
}

fn count_discrete_valued_update_scalars(dae_model: &dae::Dae) -> BalanceResult<usize> {
    let discrete_input_names = metadata_discrete_input_names(dae_model);
    let discrete_input_symbols = BalanceSymbolSet::new(dae_model, &discrete_input_names);
    let mut scalar_count = 0usize;
    let connection_anchors =
        collect_discrete_connection_balance_anchors(dae_model, &discrete_input_names)?;
    let mut connection_rank = ConnectionUpdateRank::new(connection_anchors);
    for eq in &dae_model.discrete.valued_updates {
        if is_discrete_input_update(eq, &discrete_input_symbols) {
            continue;
        }
        if is_discrete_connection_update_origin(eq.origin.as_str())
            && let Some(count) = count_connection_update_rank(
                eq,
                &dae_model.variables.discrete_valued,
                &mut connection_rank,
            )
        {
            scalar_count += count;
            continue;
        }
        scalar_count += eq.scalar_count;
    }
    Ok(scalar_count)
}

fn collect_discrete_connection_balance_anchors(
    dae_model: &dae::Dae,
    discrete_input_names: &HashSet<rumoca_core::VarName>,
) -> BalanceResult<IndexSet<rumoca_core::VarName>> {
    let mut anchors = IndexSet::new();
    for name in discrete_input_names {
        let variable = dae_model
            .variables
            .discrete_valued
            .get(name)
            .ok_or_else(|| BalanceError::MissingDiscreteVariableMetadata {
                name: name.clone(),
                span: None,
            })?;
        insert_connection_rank_nodes(
            name,
            variable.size(),
            &dae_model.variables.discrete_valued,
            &mut anchors,
            real_balance_span(variable.source_span),
        )?;
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
                lhs.var_name(),
                eq.scalar_count,
                &dae_model.variables.discrete_valued,
                &mut anchors,
                real_balance_span(eq.span),
            )?;
        }
    }
    Ok(anchors)
}

fn insert_connection_rank_nodes(
    name: &rumoca_core::VarName,
    scalar_count: usize,
    variables: &IndexMap<rumoca_core::VarName, dae::Variable>,
    nodes: &mut IndexSet<rumoca_core::VarName>,
    span: Option<rumoca_core::Span>,
) -> BalanceResult<()> {
    if let Some(expanded) = expand_connection_rank_nodes(name, scalar_count, variables) {
        nodes.extend(expanded);
    } else {
        if scalar_count > 1 && !variables.contains_key(name) {
            return Err(BalanceError::MissingDiscreteVariableMetadata {
                name: name.clone(),
                span,
            });
        }
        nodes.insert(name.clone());
    }
    Ok(())
}

fn real_balance_span(span: rumoca_core::Span) -> Option<rumoca_core::Span> {
    (!span.is_dummy()).then_some(span)
}

fn is_discrete_input_update(eq: &dae::Equation, discrete_input_names: &BalanceSymbolSet) -> bool {
    let Some(lhs) = &eq.lhs else {
        return false;
    };
    if !discrete_input_names.matches_reference(lhs) {
        return false;
    }
    let refs = expression_var_refs(&eq.rhs);
    refs.into_iter()
        .all(|reference| discrete_input_names.matches_reference(reference))
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

fn connection_update_var_refs(
    eq: &dae::Equation,
) -> Option<(rumoca_core::VarName, rumoca_core::VarName)> {
    let lhs = eq.lhs.as_ref()?.var_name().clone();
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
    if let Some(variable) = variables.get(name) {
        if variable.size() < scalar_count {
            return None;
        }
        if variable.is_scalar() {
            return Some(vec![name.clone()]);
        }
        return Some(
            (1..=scalar_count)
                .map(|idx| rumoca_core::VarName::new(format!("{}[{idx}]", name.as_str())))
                .collect(),
        );
    }
    if scalar_count <= 1 {
        return Some(vec![name.clone()]);
    }
    let indexed = (1..=scalar_count)
        .map(|idx| rumoca_core::VarName::new(format!("{}[{idx}]", name.as_str())))
        .collect::<Vec<_>>();
    indexed
        .iter()
        .all(|candidate| variables.contains_key(candidate))
        .then_some(indexed)
}

fn count_condition_memory_equation_scalars(dae_model: &dae::Dae) -> usize {
    let discrete_valued_names = dae_model
        .variables
        .discrete_valued
        .keys()
        .cloned()
        .collect();
    let discrete_valued_symbols = BalanceSymbolSet::new(dae_model, &discrete_valued_names);
    dae_model
        .conditions
        .equations
        .iter()
        .filter(|eq| equation_lhs_matches_symbol(eq, &discrete_valued_symbols))
        .map(|eq| eq.scalar_count)
        .sum()
}

fn count_referenced_discrete_real_unknown_scalars(dae_model: &dae::Dae) -> usize {
    count_referenced_update_unknown_scalars(
        dae_model,
        &dae_model.variables.discrete_reals,
        &dae_model.variables.inputs,
        dae_model.discrete.real_updates.iter(),
    )
}

fn count_referenced_discrete_valued_unknown_scalars(dae_model: &dae::Dae) -> BalanceResult<usize> {
    let mut referenced = IndexMap::new();
    let mut residual_scalars = 0usize;
    let discrete_input_names = metadata_discrete_input_names(dae_model);
    let target_scalar_counts = collect_update_target_scalar_counts(
        dae_model,
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
                dae_model,
                eq,
                &dae_model.variables.discrete_valued,
                &mut referenced,
            );
            if is_discrete_connection_update_origin(eq.origin.as_str()) {
                insert_complete_connection_variable_names_from_expression(
                    dae_model,
                    eq,
                    &eq.rhs,
                    &dae_model.variables.discrete_valued,
                    &target_scalar_counts,
                    &mut referenced,
                );
            }
        } else if expression_references_included_variable(
            dae_model,
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
                dae_model,
                eq,
                &dae_model.variables.discrete_valued,
                &mut referenced,
            );
        }
    }
    Ok(residual_scalars
        + count_referenced_scalar_counts(
            dae_model,
            &dae_model.variables.discrete_valued,
            &dae_model.variables.inputs,
            &discrete_input_names,
            &referenced,
        ))
}

fn metadata_discrete_input_names(dae_model: &dae::Dae) -> HashSet<rumoca_core::VarName> {
    dae_model
        .metadata
        .discrete_input_names
        .iter()
        .map(rumoca_core::VarName::new)
        .collect()
}

fn input_only_discrete_alias_deficit(dae_model: &dae::Dae) -> usize {
    let component_defined_targets =
        collect_component_defined_discrete_targets_for_balance(dae_model);
    let mut graph = ConnectionUpdateRank::new(IndexSet::new());
    let mut nodes = IndexSet::new();

    for eq in &dae_model.discrete.valued_updates {
        if !is_discrete_connection_update_origin(eq.origin.as_str()) {
            continue;
        }
        let Some((lhs, rhs)) = connection_update_var_refs(eq) else {
            continue;
        };
        let Some(lhs_nodes) = expand_connection_rank_nodes(
            &lhs,
            eq.scalar_count,
            &dae_model.variables.discrete_valued,
        ) else {
            continue;
        };
        let Some(rhs_nodes) = expand_connection_rank_nodes(
            &rhs,
            eq.scalar_count,
            &dae_model.variables.discrete_valued,
        ) else {
            continue;
        };
        if lhs_nodes.len() != rhs_nodes.len() {
            continue;
        }
        for (lhs_node, rhs_node) in lhs_nodes.into_iter().zip(rhs_nodes) {
            nodes.insert(lhs_node.clone());
            nodes.insert(rhs_node.clone());
            graph.add_edge(lhs_node, rhs_node);
        }
    }

    let mut components: IndexMap<usize, Vec<rumoca_core::VarName>> = IndexMap::new();
    for node in nodes {
        let Some(idx) = graph.node_to_idx.get(&node).copied() else {
            continue;
        };
        let root = graph.find_idx(idx);
        components.entry(root).or_default().push(node);
    }

    components
        .into_values()
        .filter(|component| {
            component.iter().all(|name| {
                !component_defined_targets.contains(name)
                    && discrete_connection_node_is_external(dae_model, name)
            })
        })
        .count()
}

fn collect_component_defined_discrete_targets_for_balance(
    dae_model: &dae::Dae,
) -> HashSet<rumoca_core::VarName> {
    let mut targets = HashSet::new();
    for eq in dae_model
        .discrete
        .valued_updates
        .iter()
        .chain(dae_model.conditions.equations.iter())
    {
        if is_discrete_connection_update_origin(eq.origin.as_str()) {
            continue;
        }
        let Some(lhs) = &eq.lhs else {
            continue;
        };
        if let Some(nodes) = expand_connection_rank_nodes(
            lhs.var_name(),
            eq.scalar_count,
            &dae_model.variables.discrete_valued,
        ) {
            targets.extend(nodes);
        } else {
            targets.insert(lhs.var_name().clone());
        }
    }
    targets
}

fn discrete_connection_node_is_external(dae_model: &dae::Dae, name: &rumoca_core::VarName) -> bool {
    find_variable(dae_model, name)
        .or_else(|| {
            rumoca_core::strip_trailing_subscript_suffix(name.as_str())
                .map(rumoca_core::VarName::new)
                .and_then(|base| find_variable(dae_model, &base))
        })
        .is_some_and(|variable| matches!(variable.causality, dae::VariableCausality::Input))
}

fn count_referenced_update_unknown_scalars<'a>(
    dae_model: &dae::Dae,
    variables: &'a indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    excluded: &'a indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    equations: impl Iterator<Item = &'a dae::Equation>,
) -> usize {
    let equations = equations.collect::<Vec<_>>();
    let target_scalar_counts =
        collect_update_target_scalar_counts(dae_model, variables, equations.iter().copied());
    let mut referenced = IndexMap::new();
    let mut residual_scalars = 0usize;
    for eq in equations {
        if eq.lhs.is_some() {
            add_update_target_scalar_counts(dae_model, eq, variables, &mut referenced);
            if is_discrete_connection_update_origin(eq.origin.as_str()) {
                insert_complete_connection_variable_names_from_expression(
                    dae_model,
                    eq,
                    &eq.rhs,
                    variables,
                    &target_scalar_counts,
                    &mut referenced,
                );
            }
        } else if expression_references_included_variable(
            dae_model,
            &eq.rhs,
            variables,
            excluded,
            &HashSet::new(),
        ) {
            residual_scalars += eq.scalar_count;
        }
    }
    residual_scalars
        + count_referenced_scalar_counts(
            dae_model,
            variables,
            excluded,
            &HashSet::new(),
            &referenced,
        )
}

fn collect_update_target_scalar_counts<'a>(
    dae_model: &dae::Dae,
    variables: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    equations: impl Iterator<Item = &'a dae::Equation>,
) -> IndexMap<rumoca_core::VarName, usize> {
    let mut counts = IndexMap::new();
    for eq in equations {
        let Some(lhs) = &eq.lhs else {
            continue;
        };
        for (candidate, variable) in variables {
            if reference_overlaps_variable(dae_model, lhs, candidate, variable) {
                *counts.entry(candidate.clone()).or_insert(0) += eq.scalar_count;
            }
        }
    }
    counts
}

fn expression_references_included_variable(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
    variables: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    excluded: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    extra_excluded: &HashSet<rumoca_core::VarName>,
) -> bool {
    let extra_excluded = BalanceSymbolSet::new(dae_model, extra_excluded);
    expression_var_refs(expr).into_iter().any(|reference| {
        variables.iter().any(|(name, variable)| {
            reference_overlaps_variable(dae_model, reference, name, variable)
                && !variable_overlaps_any(dae_model, name, variable, excluded)
                && !variable_matches_symbol_set(dae_model, name, variable, &extra_excluded)
        })
    })
}

fn count_referenced_scalar_counts(
    dae_model: &dae::Dae,
    variables: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    excluded: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    extra_excluded: &HashSet<rumoca_core::VarName>,
    referenced: &IndexMap<rumoca_core::VarName, usize>,
) -> usize {
    let extra_excluded = BalanceSymbolSet::new(dae_model, extra_excluded);
    referenced
        .iter()
        .filter_map(|(name, count)| variables.get(name).map(|variable| (name, *count, variable)))
        .filter(|(name, _, variable)| !variable_overlaps_any(dae_model, name, variable, excluded))
        .filter(|(name, _, variable)| {
            !variable_matches_symbol_set(dae_model, name, variable, &extra_excluded)
        })
        .map(|(_, count, variable)| count.min(variable.size()))
        .sum()
}

fn variable_overlaps_any(
    dae_model: &dae::Dae,
    name: &rumoca_core::VarName,
    variable: &dae::Variable,
    variables: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
) -> bool {
    variables.iter().any(|(candidate_name, candidate)| {
        variables_overlap_by_identity(dae_model, name, variable, candidate_name, candidate)
    })
}

fn add_update_target_scalar_counts(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
    variables: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    referenced: &mut IndexMap<rumoca_core::VarName, usize>,
) {
    if let Some(lhs) = &eq.lhs {
        add_matching_variable_scalar_counts(dae_model, lhs, eq.scalar_count, variables, referenced);
    }
}

fn insert_complete_connection_variable_names_from_expression(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
    expr: &rumoca_core::Expression,
    variables: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    target_scalar_counts: &IndexMap<rumoca_core::VarName, usize>,
    referenced: &mut IndexMap<rumoca_core::VarName, usize>,
) {
    for name in expression_var_refs(expr) {
        insert_complete_connection_variable_names(
            eq,
            dae_model,
            name,
            variables,
            target_scalar_counts,
            referenced,
        );
    }
}

fn insert_complete_connection_variable_names(
    eq: &dae::Equation,
    dae_model: &dae::Dae,
    reference: &rumoca_core::Reference,
    variables: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    target_scalar_counts: &IndexMap<rumoca_core::VarName, usize>,
    referenced: &mut IndexMap<rumoca_core::VarName, usize>,
) {
    for (candidate, variable) in variables {
        if reference_overlaps_variable(dae_model, reference, candidate, variable)
            && (eq.scalar_count >= variable.size()
                || target_scalar_counts
                    .get(candidate)
                    .is_some_and(|count| *count >= variable.size()))
        {
            add_referenced_scalar_count(candidate, variable.size(), referenced);
        }
    }
}

fn add_matching_variable_scalar_counts(
    dae_model: &dae::Dae,
    reference: &rumoca_core::Reference,
    scalar_count: usize,
    variables: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    referenced: &mut IndexMap<rumoca_core::VarName, usize>,
) {
    for (candidate, variable) in variables {
        if reference_overlaps_variable(dae_model, reference, candidate, variable) {
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

fn reference_overlaps_variable(
    dae_model: &dae::Dae,
    reference: &rumoca_core::Reference,
    variable_name: &rumoca_core::VarName,
    variable: &dae::Variable,
) -> bool {
    reference.var_name() == variable_name
        || reference.target_def_id().is_some_and(|reference_def_id| {
            variable_def_id_from_variable(variable).is_some_and(|variable_def_id| {
                def_ids_overlap(dae_model, reference_def_id, variable_def_id)
            })
        })
}

fn variable_matches_symbol_set(
    _dae_model: &dae::Dae,
    name: &rumoca_core::VarName,
    variable: &dae::Variable,
    symbols: &BalanceSymbolSet,
) -> bool {
    symbols.matches_variable(name, variable)
}

fn variables_overlap_by_identity(
    dae_model: &dae::Dae,
    lhs_name: &rumoca_core::VarName,
    lhs_variable: &dae::Variable,
    rhs_name: &rumoca_core::VarName,
    rhs_variable: &dae::Variable,
) -> bool {
    lhs_name == rhs_name
        || variable_def_id_from_variable(lhs_variable).is_some_and(|lhs_def_id| {
            variable_def_id_from_variable(rhs_variable)
                .is_some_and(|rhs_def_id| def_ids_overlap(dae_model, lhs_def_id, rhs_def_id))
        })
}

fn variable_def_id_from_variable(variable: &dae::Variable) -> Option<DefId> {
    variable
        .component_ref
        .as_ref()
        .and_then(|component_ref| component_ref.def_id)
}

fn def_ids_overlap(dae_model: &dae::Dae, lhs: DefId, rhs: DefId) -> bool {
    lhs == rhs
        || dae_model
            .metadata
            .symbol_ancestry
            .get(&lhs)
            .is_some_and(|chain| chain.contains(&rhs))
        || dae_model
            .metadata
            .symbol_ancestry
            .get(&rhs)
            .is_some_and(|chain| chain.contains(&lhs))
}

fn update_equation_targets_symbol(eq: &dae::Equation, names: &BalanceSymbolSet) -> bool {
    if equation_lhs_matches_symbol(eq, names) {
        return true;
    }
    if eq.lhs.is_some() {
        return false;
    }
    expression_var_refs(&eq.rhs)
        .into_iter()
        .any(|reference| names.matches_reference(reference))
}

fn equation_lhs_matches_symbol(eq: &dae::Equation, names: &BalanceSymbolSet) -> bool {
    eq.lhs
        .as_ref()
        .is_some_and(|reference| names.matches_reference(reference))
}

fn equation_references_continuous_unknown(
    eq: &dae::Equation,
    continuous_unknowns: &BalanceSymbolSet,
) -> bool {
    if eq
        .lhs
        .as_ref()
        .is_some_and(|name| continuous_unknowns.matches_reference(name))
    {
        return true;
    }

    let refs = expression_var_refs(&eq.rhs);
    refs.into_iter()
        .any(|reference| continuous_unknowns.matches_reference(reference))
}

fn equation_references_input(eq: &dae::Equation, input_names: &BalanceSymbolSet) -> bool {
    if eq
        .lhs
        .as_ref()
        .is_some_and(|name| input_names.matches_reference(name))
    {
        return true;
    }

    let refs = expression_var_refs(&eq.rhs);
    refs.into_iter()
        .any(|reference| input_names.matches_reference(reference))
}

pub(crate) fn is_connection_origin(origin: &str) -> bool {
    origin.starts_with("connect(") || origin.starts_with("connection equation:")
}

fn is_discrete_connection_update_origin(origin: &str) -> bool {
    is_connection_origin(origin) || origin.starts_with("explicit connection equation:")
}

/// Extract references from a `lhs - rhs` binary expression.
fn eq_binary_var_refs(expr: &rumoca_core::Expression) -> Vec<&rumoca_core::Reference> {
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
        names.push(name);
    }
    if let rumoca_core::Expression::VarRef { name, .. } = rhs.as_ref() {
        names.push(name);
    }
    names
}

fn expression_var_refs(expr: &rumoca_core::Expression) -> Vec<&rumoca_core::Reference> {
    let mut refs = Vec::new();
    append_expression_var_refs(expr, &mut refs);
    refs
}

fn append_expression_var_refs<'a>(
    expr: &'a rumoca_core::Expression,
    refs: &mut Vec<&'a rumoca_core::Reference>,
) {
    match expr {
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            append_expression_var_refs(lhs, refs);
            append_expression_var_refs(rhs, refs);
        }
        rumoca_core::Expression::Unary { rhs, .. } => append_expression_var_refs(rhs, refs),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            refs.push(name);
            append_subscript_var_refs(subscripts, refs);
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => {
            for arg in args {
                append_expression_var_refs(arg, refs);
            }
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                append_expression_var_refs(condition, refs);
                append_expression_var_refs(value, refs);
            }
            append_expression_var_refs(else_branch, refs);
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            for element in elements {
                append_expression_var_refs(element, refs);
            }
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            append_expression_var_refs(start, refs);
            if let Some(step) = step {
                append_expression_var_refs(step, refs);
            }
            append_expression_var_refs(end, refs);
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            append_expression_var_refs(expr, refs);
            for index in indices {
                append_expression_var_refs(&index.range, refs);
            }
            if let Some(filter) = filter {
                append_expression_var_refs(filter, refs);
            }
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            append_expression_var_refs(base, refs);
            append_subscript_var_refs(subscripts, refs);
        }
        rumoca_core::Expression::FieldAccess { base, .. } => append_expression_var_refs(base, refs),
        rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => {}
    }
}

fn append_subscript_var_refs<'a>(
    subscripts: &'a [rumoca_core::Subscript],
    refs: &mut Vec<&'a rumoca_core::Reference>,
) {
    for subscript in subscripts {
        if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
            append_expression_var_refs(expr, refs);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;

    fn test_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("balance_fixture.mo"),
            1,
            2,
        )
    }

    fn scalar_eq(count: usize) -> dae::Equation {
        scalar_eq_with_lhs("x", count)
    }

    fn scalar_eq_with_lhs(lhs_name: &str, count: usize) -> dae::Equation {
        dae::Equation {
            lhs: Some(rumoca_core::VarName::new(lhs_name).into()),
            rhs: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new(lhs_name).into(),
                    subscripts: vec![],
                    span: test_span(),
                }),
                rhs: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: test_span(),
                }),
                span: test_span(),
            },
            span: test_span(),
            origin: "test".to_string(),
            scalar_count: count,
        }
    }

    fn scalar_assignment_with_rhs_ref(lhs_name: &str, rhs_name: &str) -> dae::Equation {
        dae::Equation {
            lhs: Some(rumoca_core::VarName::new(lhs_name).into()),
            rhs: rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new(rhs_name).into(),
                subscripts: vec![],
                span: test_span(),
            },
            span: test_span(),
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
            lhs: Some(rumoca_core::VarName::new(lhs_name).into()),
            rhs: rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new(rhs_name).into(),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    rhs_index,
                    test_span(),
                )],
                span: test_span(),
            },
            span: test_span(),
            origin: "explicit connection equation: a = b[1]".to_string(),
            scalar_count: 1,
        }
    }

    fn dae_with_unknown_scalars(unknown_scalars: i64) -> dae::Dae {
        let mut dae = dae::Dae::default();
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("x"),
            dae::Variable {
                name: rumoca_core::VarName::new("x"),
                dims: vec![unknown_scalars],
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );
        dae
    }

    fn discrete_var(name: &str) -> dae::Variable {
        dae::Variable {
            name: rumoca_core::VarName::new(name),
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }
    }

    fn discrete_vector_var(name: &str, size: i64) -> dae::Variable {
        dae::Variable {
            name: rumoca_core::VarName::new(name),
            dims: vec![size],
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }
    }

    #[test]
    fn test_balance_clamps_overconstrained_interface_to_deficit() {
        let mut dae = dae_with_unknown_scalars(4);
        dae.continuous.equations.push(scalar_eq(4));
        dae.metadata.overconstrained_interface_count = 9;
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn admission_accepts_exact_initial_deficit_closure() {
        let mut dae = dae_with_unknown_scalars(3);
        dae.continuous.equations.push(scalar_eq(1));
        dae.initialization
            .equations
            .push(scalar_eq_with_lhs("x", 2));

        let detail =
            initial_closure_balance_detail(&dae).expect("valid DAE initial balance fixture");

        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), -2);
        assert_eq!(detail.deficit_before, 2);
        assert_eq!(detail.initial_equation_scalars, 2);
        assert_eq!(detail.closure_used, 2);
        assert_eq!(detail.deficit_after, 0);
        assert!(detail.is_admissible());
        assert!(is_balanced_for_admission(&dae).expect("valid DAE balance fixture"));
    }

    #[test]
    fn admission_rejects_partial_initial_deficit_closure() {
        let mut dae = dae_with_unknown_scalars(4);
        dae.continuous.equations.push(scalar_eq(1));
        dae.initialization
            .equations
            .push(scalar_eq_with_lhs("x", 2));

        let detail =
            initial_closure_balance_detail(&dae).expect("valid DAE initial balance fixture");

        assert_eq!(detail.deficit_before, 3);
        assert_eq!(detail.closure_used, 2);
        assert_eq!(detail.deficit_after, 1);
        assert!(!detail.is_admissible());
        assert!(!is_balanced_for_admission(&dae).expect("valid DAE balance fixture"));
    }

    #[test]
    fn admission_uses_overconstrained_break_edges_as_deficit_closure() {
        let mut dae = dae_with_unknown_scalars(4);
        dae.continuous.equations.push(scalar_eq(2));
        dae.metadata.oc_break_edge_scalar_count = 2;
        let detail = initial_closure_balance_detail(&dae).expect("valid DAE fixture");

        assert_eq!(
            (
                balance(&dae).expect("valid DAE fixture"),
                detail.deficit_before,
                detail.overconstrained_break_edge_scalars,
                detail.closure_used,
                detail.deficit_after,
                is_balanced_for_admission(&dae).expect("valid DAE fixture"),
            ),
            (-2, 2, 2, 2, 0, true)
        );
    }

    #[test]
    fn admission_rejects_missing_initial_deficit_closure() {
        let mut dae = dae_with_unknown_scalars(2);
        dae.continuous.equations.push(scalar_eq(1));

        let detail =
            initial_closure_balance_detail(&dae).expect("valid DAE initial balance fixture");

        assert_eq!(detail.deficit_before, 1);
        assert_eq!(detail.initial_equation_scalars, 0);
        assert_eq!(detail.deficit_after, 1);
        assert!(!is_balanced_for_admission(&dae).expect("valid DAE balance fixture"));
    }

    #[test]
    fn admission_does_not_mask_overdetermined_balance_with_initial_equations() {
        let mut dae = dae_with_unknown_scalars(1);
        dae.continuous.equations.push(scalar_eq(2));
        dae.initialization
            .equations
            .push(scalar_eq_with_lhs("x", 3));

        let detail =
            initial_closure_balance_detail(&dae).expect("valid DAE initial balance fixture");

        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 1);
        assert_eq!(detail.deficit_before, 0);
        assert_eq!(detail.closure_used, 0);
        assert!(!detail.is_admissible());
        assert!(!is_balanced_for_admission(&dae).expect("valid DAE balance fixture"));
    }

    #[test]
    fn test_balance_uses_only_needed_overconstrained_interface() {
        let mut dae = dae_with_unknown_scalars(4);
        dae.continuous.equations.push(scalar_eq(3));
        dae.metadata.overconstrained_interface_count = 9;
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn test_balance_applies_oc_interface_even_with_break_edges() {
        let mut dae = dae_with_unknown_scalars(10);
        dae.continuous.equations.push(scalar_eq(1));
        dae.metadata.overconstrained_interface_count = 9;
        dae.metadata.oc_break_edge_scalar_count = 12;
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn test_balance_clamps_interface_flow_to_remaining_deficit() {
        let mut dae = dae_with_unknown_scalars(4);
        dae.continuous.equations.push(scalar_eq(4));
        dae.metadata.interface_flow_count = 3;
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn test_balance_uses_interface_flow_to_close_deficit_only() {
        let mut dae = dae_with_unknown_scalars(5);
        dae.continuous.equations.push(scalar_eq(3));
        dae.metadata.interface_flow_count = 9;
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn test_balance_clamps_stream_interface_equations_to_remaining_deficit() {
        let mut dae = dae_with_unknown_scalars(4);
        dae.continuous.equations.push(scalar_eq(4));
        dae.metadata.stream_interface_equation_count = 3;
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn test_balance_uses_stream_interface_equations_to_close_deficit_only() {
        let mut dae = dae_with_unknown_scalars(5);
        dae.continuous.equations.push(scalar_eq(3));
        dae.metadata.stream_interface_equation_count = 9;
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn test_balance_ignores_unconstrained_discrete_real_declaration() {
        let mut dae = dae::Dae::default();
        dae.variables
            .discrete_reals
            .insert(rumoca_core::VarName::new("z"), discrete_var("z"));
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn test_balance_counts_discrete_updates_against_discrete_unknowns() {
        let mut dae = dae::Dae::default();
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("m"), discrete_var("m"));
        dae.discrete.valued_updates.push(scalar_eq_with_lhs("m", 1));
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
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
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
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
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), -1);
    }

    #[test]
    fn balance_rejects_missing_discrete_input_metadata_variable() {
        let mut dae = dae::Dae::default();
        dae.metadata
            .discrete_input_names
            .push("missing_input".to_string());

        let err = balance(&dae).expect_err("missing discrete metadata should fail");

        assert!(matches!(
            err,
            BalanceError::MissingDiscreteVariableMetadata { ref name, .. }
                if name.as_str() == "missing_input"
        ));
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
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn test_balance_expands_scalarized_discrete_connection_targets() {
        let mut dae = dae::Dae::default();
        for name in ["a[1]", "a[2]", "b[1]", "b[2]"] {
            dae.variables
                .discrete_valued
                .insert(rumoca_core::VarName::new(name), discrete_var(name));
        }
        dae.discrete
            .valued_updates
            .push(connection_assignment_with_count("a", "b", 2));

        assert_eq!(
            balance(&dae).expect("scalarized vector connection should expand to children"),
            2
        );
    }

    #[test]
    fn test_balance_anchors_size_one_discrete_input_vectors_by_index() {
        let mut dae = dae::Dae::default();
        dae.variables.discrete_valued.insert(
            rumoca_core::VarName::new("source"),
            discrete_vector_var("source", 1),
        );
        for name in ["a", "b"] {
            dae.variables
                .discrete_valued
                .insert(rumoca_core::VarName::new(name), discrete_var(name));
        }
        dae.metadata.discrete_input_names.push("source".to_string());
        dae.discrete.valued_updates.push(scalar_eq_with_lhs("b", 1));
        dae.discrete
            .valued_updates
            .push(connection_assignment_with_rhs_index("a", "source", 1));
        dae.discrete
            .valued_updates
            .push(connection_assignment_with_rhs_ref("a", "b"));

        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
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
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
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
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
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
            lhs: Some(rumoca_core::VarName::new("b").into()),
            scalar_count: 2,
            ..scalar_assignment_with_rhs_ref("b", "source")
        });
        dae.discrete
            .valued_updates
            .push(connection_assignment_with_rhs_index("a", "b", 1));
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
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
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn test_balance_counts_discrete_real_updates_against_discrete_real_unknowns() {
        let mut dae = dae::Dae::default();
        dae.variables
            .discrete_reals
            .insert(rumoca_core::VarName::new("z"), discrete_var("z"));
        dae.discrete.real_updates.push(scalar_eq_with_lhs("z", 1));
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
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
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn test_balance_ignores_state_reinit_updates_for_static_balance() {
        let mut dae = dae::Dae::default();
        dae.variables
            .states
            .insert(rumoca_core::VarName::new("x"), discrete_var("x"));
        dae.continuous.equations.push(scalar_eq(1));
        dae.discrete.real_updates.push(scalar_eq(1));
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn test_balance_counts_condition_equations_against_condition_unknowns() {
        let mut dae = dae::Dae::default();
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("c"), discrete_var("c"));
        dae.conditions.equations.push(scalar_eq_with_lhs("c", 1));
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
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
        assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
    }
}
