//! Hybrid DAE representation for the Rumoca compiler (MLS Appendix B).
//!
//! This crate defines the canonical DAE form per MLS Appendix B (B.1):
//!
//! ```text
//! 0 = f_x(v, c)                    (B.1a) — implicit continuous equations
//! z = f_z(v, c)                    (B.1b) — discrete Real updates (at events)
//! m := f_m(v, c)                   (B.1c) — discrete-valued updates (Boolean, Integer)
//! c := f_c(relation(v))            (B.1d) — conditions
//! ```
//!
//! Where: `v := [p; t; ẋ; x; y; z; m; pre(z); pre(m)]`
//!
//! Variables are classified by role:
//! - Parameters (p), States (x), Algebraics (y)
//! - Discrete Reals (z), Discrete-valued (m: Boolean, Integer, enum)
//! - Inputs (u), Outputs (w), Constants
//!
//! Continuous equations are ONE implicit set (f_x). Equation classification
//! (which equation solves for which variable) is structural analysis work.

use indexmap::{IndexMap, IndexSet};
use rumoca_core::Span;
use rumoca_ir_flat as ir_flat;
use serde::{Deserialize, Serialize};

pub use rumoca_ir_flat::{
    BuiltinFunction, ComponentRefPart, ComponentReference, ComprehensionIndex, Expression,
    ExternalFunction, ForIndex, Function, FunctionParam, Literal, OpBinary, OpUnary, Statement,
    StatementBlock, Subscript, VarName, component_base_name,
};

/// Namespaced flat-expression surface used by DAE consumers.
///
/// This keeps caller code on `dae::flat::...` paths without direct
/// `rumoca-ir-flat` dependencies or crate-level re-export shortcuts.
pub mod flat {
    pub use rumoca_ir_flat::{
        Algorithm, BuiltinFunction, ComponentRefPart, ComponentReference, ComprehensionIndex,
        Expression, ExternalFunction, ForIndex, Function, FunctionParam, Literal, OpBinary,
        OpUnary, Statement, StatementBlock, Subscript, VarName, component_base_name,
    };
}

/// Detailed breakdown of balance calculation components.
#[derive(Debug, Clone)]
pub struct BalanceDetail {
    pub state_unknowns: usize,
    pub alg_unknowns: usize,
    pub output_unknowns: usize,
    pub f_x_scalar: usize,
    pub algorithm_outputs: usize,
    pub when_eq_scalar: usize,
    pub interface_flow_count: usize,
    pub overconstrained_interface_count: i64,
    pub oc_break_edge_scalar_count: usize,
}

impl std::fmt::Display for BalanceDetail {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let unknowns = (self.state_unknowns + self.alg_unknowns + self.output_unknowns) as i64;
        let brk = self.oc_break_edge_scalar_count as i64;
        let available_oc_interface = self.overconstrained_interface_count.max(0);
        let base_without_iflow =
            (self.f_x_scalar + self.algorithm_outputs + self.when_eq_scalar) as i64;
        // Interface-flow contribution only closes a remaining deficit.
        let iflow_needed = (unknowns - base_without_iflow).max(0);
        let effective_iflow = (self.interface_flow_count as i64).min(iflow_needed);
        let base_equations = base_without_iflow + effective_iflow;
        let oc_needed = (unknowns - base_equations).max(0);
        let effective_oc_interface = available_oc_interface.min(oc_needed);
        let raw_equations = base_equations + effective_oc_interface;
        let raw_balance = raw_equations - unknowns;
        let effective_brk = brk.min(raw_balance.max(0));
        let balance = raw_balance - effective_brk;
        writeln!(
            f,
            "  Unknowns: {} = states({}) + alg({}) + out({})",
            unknowns, self.state_unknowns, self.alg_unknowns, self.output_unknowns
        )?;
        writeln!(
            f,
            "  Equations: {} = f_x({}) + algo({}) + when({}) + iflow({}) + oc({}) - brk({})",
            raw_equations - effective_brk,
            self.f_x_scalar,
            self.algorithm_outputs,
            self.when_eq_scalar,
            effective_iflow,
            effective_oc_interface,
            effective_brk
        )?;
        write!(f, "  Balance: {}", balance)
    }
}

/// Scalar counts for canonical runtime variable partitions.
///
/// MLS Appendix B notation:
/// - `p`: parameters + constants
/// - `t`: independent time variable (always 1)
/// - `x`: continuous states
/// - `y`: continuous algebraics (including outputs)
/// - `z`: discrete Real variables
/// - `m`: discrete-valued variables (Boolean/Integer/enum)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct RuntimePartitionScalarCounts {
    pub p: usize,
    pub t: usize,
    pub x: usize,
    pub y: usize,
    pub z: usize,
    pub m: usize,
}

/// Solver-agnostic periodic clock schedule descriptor.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClockSchedule {
    /// Positive tick period in seconds.
    pub period_seconds: f64,
    /// Tick phase offset in seconds.
    pub phase_seconds: f64,
}

/// Hybrid DAE representation (MLS Appendix B).
///
/// Equations follow the MLS B.1 canonical form:
/// - `f_x`: 0 = f_x(v, c) — implicit continuous equations (B.1a)
/// - `f_z`: z = f_z(v, c) — discrete Real updates at events (B.1b)
/// - `f_m`: m := f_m(v, c) — discrete-valued updates (B.1c)
/// - `f_c`: c := f_c(relation(v)) — conditions (B.1d)
///
/// Runtime contract:
/// - Variable partitions are explicit and disjoint (`p`, `x`, `y`, `z`, `m`),
///   with `t` represented implicitly as the simulation independent variable.
/// - Runtime/equation partitions are explicit vectors and always present in
///   the schema: `f_x`, `f_z`, `f_m`, `f_c`, `relation`,
///   `synthetic_root_conditions`, `initial_equations`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Dae {
    // ── Variables ──────────────────────────────────────────────────────
    /// State variables (x) - continuous variables with derivatives.
    #[serde(rename = "x")]
    pub states: IndexMap<VarName, Variable>,
    /// Algebraic variables (y) - variables without derivatives.
    #[serde(rename = "y")]
    pub algebraics: IndexMap<VarName, Variable>,
    /// Input variables (u) - known externally provided values.
    #[serde(rename = "u")]
    pub inputs: IndexMap<VarName, Variable>,
    /// Output variables (w) - computed from states/algebraics.
    #[serde(rename = "w")]
    pub outputs: IndexMap<VarName, Variable>,
    /// Parameters (p) - fixed values during simulation.
    /// MLS Appendix B groups parameters/constants under p; constants remain
    /// separately tracked in `constants` for compiler/runtime behavior.
    #[serde(rename = "p")]
    pub parameters: IndexMap<VarName, Variable>,
    /// Constants - fixed values at compile time.
    #[serde(rename = "constants")]
    pub constants: IndexMap<VarName, Variable>,
    /// Discrete Real variables (z) - change only at events (MLS B.1b).
    #[serde(rename = "z")]
    pub discrete_reals: IndexMap<VarName, Variable>,
    /// Discrete-valued variables (m) - Boolean, Integer, enum (MLS B.1c).
    #[serde(rename = "m")]
    pub discrete_valued: IndexMap<VarName, Variable>,
    /// Derivative alias variables - defined by ODE equations but not states.
    /// e.g., `omega` in `omega = der(gamma)` is a derivative alias.
    /// These are not counted as algebraic unknowns because they're defined by ODEs.
    #[serde(rename = "x_dot_alias")]
    pub derivative_aliases: IndexMap<VarName, Variable>,

    // ── Equations (MLS B.1) ───────────────────────────────────────────
    /// Continuous implicit equations: 0 = f_x(v, c) (MLS B.1a).
    /// All continuous equations in one unified set — no ODE/algebraic/output split.
    #[serde(rename = "f_x")]
    pub f_x: Vec<Equation>,
    /// Discrete Real update equations: z = f_z(v, c) (MLS B.1b).
    /// Extracted from when-clauses that assign to Real variables.
    #[serde(rename = "f_z")]
    pub f_z: Vec<Equation>,
    /// Discrete-valued update equations: m := f_m(v, c) (MLS B.1c).
    /// Extracted from when-clauses that assign to Boolean/Integer/enum variables.
    #[serde(rename = "f_m")]
    pub f_m: Vec<Equation>,
    /// Condition equations: c := f_c(relation(v)) (MLS B.1d).
    /// Canonically populated during ToDAE from if/when conditions.
    #[serde(rename = "f_c")]
    pub f_c: Vec<Equation>,
    /// Relation expressions used by `f_c(relation(v))` (MLS B.1d).
    #[serde(default)]
    pub relation: Vec<Expression>,
    /// Extra root conditions synthesized from equation expressions beyond canonical `relation`.
    /// This is canonical runtime metadata (always present in DAE schema).
    pub synthetic_root_conditions: Vec<Expression>,
    /// Scheduled discontinuity instants derived at compile time.
    /// This is canonical runtime metadata (always present in DAE schema).
    pub scheduled_time_events: Vec<f64>,
    /// Clock constructor expressions extracted from discrete update equations.
    /// These are evaluated against simulation parameters to build tick schedules.
    /// This is canonical runtime metadata (always present in DAE schema).
    pub clock_constructor_exprs: Vec<Expression>,
    /// Lowered periodic clock schedules.
    /// This is canonical runtime metadata (always present in DAE schema).
    pub clock_schedules: Vec<ClockSchedule>,

    /// Initial equations.
    pub initial_equations: Vec<Equation>,

    /// True if the model is declared with the `partial` keyword.
    /// MLS §4.7: Partial models are incomplete and shouldn't be balance-checked.
    pub is_partial: bool,
    /// The class type of the root model.
    #[serde(default)]
    pub class_type: ir_flat::ClassType,

    /// User-defined functions used by this model (MLS §12).
    pub functions: IndexMap<VarName, Function>,
    /// Enumeration literal ordinal map (MLS §4.9.5, 1-based ordinals).
    ///
    /// Keys are canonical literal paths (e.g.
    /// `Modelica.Electrical.Digital.Interfaces.Logic.'1'`), values are
    /// integer ordinals used by runtime numeric evaluation.
    #[serde(default)]
    pub enum_literal_ordinals: IndexMap<String, i64>,

    /// Count of interface flow variables (MLS §4.7).
    /// Per MLS §4.7, flow variables in top-level public connectors count toward
    /// the local equation size, not as unknowns. This is because they will receive
    /// their defining equations from external connections when the component is used.
    /// Interface connectors are identified by containing flow variables but not
    /// contributing any behavioral equations (only connectors, not model components).
    #[serde(default)]
    pub interface_flow_count: usize,

    /// Overconstrained interface balance correction (MLS §4.8, §9.4).
    ///
    /// For overconstrained connector types (e.g., QuasiStatic Reference), this is:
    ///   N_vars - N_linking_equations - N_explicit_roots
    /// where N_vars is the number of overconstrained unknowns, N_linking_equations
    /// is the number of equations already linking them (connection equalities +
    /// component equations), and N_explicit_roots is the number of explicit root
    /// equations (from Connections.root/isRoot).
    ///
    /// This can be negative when the connection graph has cycles (redundant
    /// equations), which is common in polyphase models with adapter components.
    #[serde(default)]
    pub overconstrained_interface_count: i64,

    /// Scalar count of excess equations from VCG break edges (MLS §9.4).
    /// Break edges in the overconstrained connection graph generate equality equations
    /// that should be replaced by `equalityConstraint()` calls. This correction
    /// tracks the number of excess equation scalars.
    #[serde(default)]
    pub oc_break_edge_scalar_count: usize,
}

impl Dae {
    /// Create a new empty DAE.
    pub fn new() -> Self {
        Self::default()
    }

    /// Get total number of variables.
    pub fn num_variables(&self) -> usize {
        self.states.len()
            + self.algebraics.len()
            + self.inputs.len()
            + self.outputs.len()
            + self.parameters.len()
            + self.constants.len()
            + self.discrete_reals.len()
            + self.discrete_valued.len()
    }

    /// Compute scalar sizes for runtime partitions `(p, t, x, y, z, m)`.
    pub fn runtime_partition_scalar_counts(&self) -> RuntimePartitionScalarCounts {
        RuntimePartitionScalarCounts {
            p: self.parameters.values().map(|v| v.size()).sum::<usize>()
                + self.constants.values().map(|v| v.size()).sum::<usize>(),
            t: 1,
            x: self.states.values().map(|v| v.size()).sum(),
            y: self
                .algebraics
                .values()
                .chain(self.outputs.values())
                .map(|v| v.size())
                .sum(),
            z: self.discrete_reals.values().map(|v| v.size()).sum(),
            m: self.discrete_valued.values().map(|v| v.size()).sum(),
        }
    }

    /// Get total number of continuous equations (f_x).
    pub fn num_equations(&self) -> usize {
        self.f_x.len()
    }

    /// Check if the system is balanced (equations match unknowns).
    ///
    /// Per SPEC_0020, algorithm outputs count as equations.
    pub fn is_balanced(&self) -> bool {
        self.balance() == 0
    }

    /// Get the balance: equations - unknowns.
    /// Positive means over-determined, negative means under-determined.
    ///
    /// Per MLS §8.4.1, the number of equations must equal the number of unknowns.
    /// Per SPEC_0020, algorithm outputs count as equations.
    /// Per SPEC_0019, array variables count as size() scalar unknowns.
    /// Per Flatten Phase Roadmap, output variables are unknowns (they need defining equations).
    /// Per MLS §4.7, flow variables in interface connectors count toward equation size.
    pub fn balance(&self) -> i64 {
        // Count scalar unknowns (arrays expand to size() elements)
        let state_unknowns: usize = self.states.values().map(|v| v.size()).sum();
        let alg_unknowns: usize = self.algebraics.values().map(|v| v.size()).sum();
        let output_unknowns: usize = self.outputs.values().map(|v| v.size()).sum();
        let unknowns = (state_unknowns + alg_unknowns + output_unknowns) as i64;

        // f_x: unified continuous equations (MLS B.1a).
        // Only equations that constrain at least one continuous unknown belong
        // to local continuous balance accounting.
        let f_x_scalar = self.count_f_x_scalars_with_continuous_unknowns();
        let algorithm_outputs = 0usize;
        let when_eq_scalar = 0usize;

        // Per MLS §4.7: interface flow variables count as equations
        // Per MLS §4.8/§9.4: overconstrained correction
        //
        // OC interface correction is applied only to close an existing deficit
        // (oc_needed), so it cannot over-correct. Break-edge correction is then
        // applied at the end only when the system is over-determined.
        let brk = self.oc_break_edge_scalar_count as i64;
        let available_oc_interface = self.overconstrained_interface_count.max(0);
        let base_without_iflow = (f_x_scalar + algorithm_outputs + when_eq_scalar) as i64;
        // Interface-flow equations should only compensate a remaining local deficit.
        // This prevents double-counting when explicit unconnected-flow equations
        // already close top-level connector flows in standalone models.
        let iflow_needed = (unknowns - base_without_iflow).max(0);
        let effective_iflow = (self.interface_flow_count as i64).min(iflow_needed);
        let base_equations = base_without_iflow + effective_iflow;
        // OC interface correction must never over-correct a model that is already
        // balanced (or over-determined) before OC terms are applied.
        let oc_needed = (unknowns - base_equations).max(0);
        let effective_oc_interface = available_oc_interface.min(oc_needed);

        let raw_equations = base_equations + effective_oc_interface;
        let raw_balance = raw_equations - unknowns;

        // Per MLS §9.4: subtract break edge excess (cycles in OC graph).
        // Cap the correction so it only reduces positive balance toward zero.
        // For models where VCG-conditional component equations already compensate
        // (e.g., MultiBody Orientation), the raw balance is already 0 and no
        // correction should be applied.
        let effective_brk = brk.min(raw_balance.max(0));
        raw_balance - effective_brk
    }

    /// Return detailed breakdown of the balance calculation components.
    /// Useful for diagnostics when investigating balance mismatches.
    pub fn balance_detail(&self) -> BalanceDetail {
        let state_unknowns: usize = self.states.values().map(|v| v.size()).sum();
        let alg_unknowns: usize = self.algebraics.values().map(|v| v.size()).sum();
        let output_unknowns: usize = self.outputs.values().map(|v| v.size()).sum();
        let algorithm_outputs = 0usize;
        let when_eq_scalar = 0usize;
        let f_x_scalar = self.count_f_x_scalars_with_continuous_unknowns();
        BalanceDetail {
            state_unknowns,
            alg_unknowns,
            output_unknowns,
            f_x_scalar,
            algorithm_outputs,
            when_eq_scalar,
            interface_flow_count: self.interface_flow_count,
            overconstrained_interface_count: self.overconstrained_interface_count,
            oc_break_edge_scalar_count: self.oc_break_edge_scalar_count,
        }
    }

    /// Names of unknowns defined at runtime by event/clock evaluation.
    ///
    /// Includes direct targets and expanded record fields.
    pub fn runtime_defined_unknown_names(&self) -> std::collections::HashSet<String> {
        self.runtime_defined_unknown_names_impl(true)
    }

    /// Names of continuous unknowns defined at runtime by event/clock evaluation.
    ///
    /// These unknowns are not safe candidates for purely symbolic elimination or
    /// synthetic orphan pinning, because their values are produced by runtime
    /// execution semantics (MLS Appendix B event and clock operators).
    ///
    /// Includes direct targets and expanded record fields in `algebraics`/`outputs`.
    pub fn runtime_defined_continuous_unknown_names(&self) -> std::collections::HashSet<String> {
        self.runtime_defined_unknown_names_impl(false)
    }

    fn runtime_defined_unknown_names_impl(
        &self,
        include_discrete: bool,
    ) -> std::collections::HashSet<String> {
        let mut defined = std::collections::HashSet::new();

        // Discrete partitions (f_z/f_m/f_c + relation) can reference unknowns
        // that must remain available at runtime for event and clocked
        // evaluation.
        for eq in self.f_z.iter().chain(self.f_m.iter()) {
            if let Some(lhs) = eq.lhs.as_ref() {
                self.extend_runtime_defined_target(&mut defined, lhs, include_discrete);
            }
            for target in runtime_assignment_target_names(&eq.rhs) {
                self.extend_runtime_defined_target(&mut defined, target, include_discrete);
            }
        }

        for expr in self
            .f_z
            .iter()
            .map(|eq| &eq.rhs)
            .chain(self.f_m.iter().map(|eq| &eq.rhs))
            .chain(self.f_c.iter().map(|eq| &eq.rhs))
            .chain(self.relation.iter())
        {
            self.extend_runtime_defined_refs_from_expr(&mut defined, expr, include_discrete);
        }

        // Continuous equations that use event/clock operators define values that
        // are produced by runtime semantics (MLS Appendix B pre()/sample()/clocked
        // evaluation). Keep their assignment targets available at runtime.
        for eq in &self.f_x {
            let Some(target) = runtime_assignment_target_name(&eq.rhs) else {
                continue;
            };
            let Some(solution) = runtime_assignment_solution_expr(&eq.rhs) else {
                continue;
            };
            if expression_contains_clocked_or_event_operators(solution) {
                self.extend_runtime_defined_target(&mut defined, target, include_discrete);
            }
        }

        defined
    }

    fn extend_runtime_defined_refs_from_expr(
        &self,
        defined: &mut std::collections::HashSet<String>,
        expr: &Expression,
        include_discrete: bool,
    ) {
        let mut refs = std::collections::HashSet::new();
        expr.collect_var_refs(&mut refs);
        for name in refs {
            self.extend_runtime_defined_target(defined, &name, include_discrete);
        }
    }

    fn extend_runtime_defined_target(
        &self,
        defined: &mut std::collections::HashSet<String>,
        target: &VarName,
        include_discrete: bool,
    ) {
        if !include_discrete
            && (self.discrete_reals.contains_key(target)
                || self.discrete_valued.contains_key(target))
        {
            return;
        }

        let raw_target = target.as_str();
        let mut candidates = std::collections::VecDeque::from([raw_target.to_string()]);
        if let Some(base) = component_base_name(raw_target)
            && base != raw_target
        {
            candidates.push_back(base);
        }

        while let Some(candidate) = candidates.pop_front() {
            let prefix = format!("{candidate}.");
            if include_discrete {
                insert_matching_runtime_targets(
                    defined,
                    &candidate,
                    &prefix,
                    self.states
                        .keys()
                        .chain(self.algebraics.keys())
                        .chain(self.outputs.keys())
                        .chain(self.discrete_reals.keys())
                        .chain(self.discrete_valued.keys()),
                );
            } else {
                insert_matching_runtime_targets(
                    defined,
                    &candidate,
                    &prefix,
                    self.algebraics.keys().chain(self.outputs.keys()),
                );
            }
        }
    }

    fn count_f_x_scalars_with_continuous_unknowns(&self) -> usize {
        let continuous_unknowns = self.collect_continuous_unknown_names();
        let input_names = self.collect_input_names();
        self.f_x
            .iter()
            .filter(|eq| self.equation_counts_for_balance(eq, &continuous_unknowns, &input_names))
            .map(|eq| eq.scalar_count)
            .sum()
    }

    fn equation_counts_for_balance(
        &self,
        eq: &Equation,
        continuous_unknowns: &std::collections::HashSet<VarName>,
        input_names: &std::collections::HashSet<VarName>,
    ) -> bool {
        if self.equation_references_continuous_unknown(eq, continuous_unknowns) {
            return true;
        }
        // Binding equations for internal promoted inputs/discrete partitions can
        // be input-only aliases and should not inflate continuous balance.
        if eq.origin.starts_with("binding equation for") {
            return false;
        }
        // Preserve explicit user equations constraining interface inputs
        // (e.g. inverse-block style constraints `u1 = u2`).
        self.equation_references_input(eq, input_names)
    }

    fn collect_continuous_unknown_names(&self) -> std::collections::HashSet<VarName> {
        self.states
            .keys()
            .chain(self.algebraics.keys())
            .chain(self.outputs.keys())
            .cloned()
            .collect()
    }

    fn collect_input_names(&self) -> std::collections::HashSet<VarName> {
        self.inputs.keys().cloned().collect()
    }

    fn equation_references_continuous_unknown(
        &self,
        eq: &Equation,
        continuous_unknowns: &std::collections::HashSet<VarName>,
    ) -> bool {
        if eq
            .lhs
            .as_ref()
            .is_some_and(|name| self.name_matches_continuous_unknown(name, continuous_unknowns))
        {
            return true;
        }

        let mut refs = std::collections::HashSet::new();
        eq.rhs.collect_var_refs(&mut refs);
        refs.into_iter()
            .any(|name| self.name_matches_continuous_unknown(&name, continuous_unknowns))
    }

    fn equation_references_input(
        &self,
        eq: &Equation,
        input_names: &std::collections::HashSet<VarName>,
    ) -> bool {
        if eq
            .lhs
            .as_ref()
            .is_some_and(|name| self.name_matches_input(name, input_names))
        {
            return true;
        }

        let mut refs = std::collections::HashSet::new();
        eq.rhs.collect_var_refs(&mut refs);
        refs.into_iter()
            .any(|name| self.name_matches_input(&name, input_names))
    }

    fn name_matches_continuous_unknown(
        &self,
        name: &VarName,
        continuous_unknowns: &std::collections::HashSet<VarName>,
    ) -> bool {
        if continuous_unknowns.contains(name) {
            return true;
        }
        if let Some(base_name) = component_base_name(name.as_str())
            && base_name != name.as_str()
        {
            let base = VarName::new(base_name.clone());
            if continuous_unknowns.contains(&base) {
                return true;
            }
            let base_prefix = format!("{base_name}.");
            if continuous_unknowns
                .iter()
                .any(|unknown| unknown.as_str().starts_with(&base_prefix))
            {
                return true;
            }
        }
        let prefix = format!("{}.", name.as_str());
        continuous_unknowns
            .iter()
            .any(|unknown| unknown.as_str().starts_with(&prefix))
    }

    fn name_matches_input(
        &self,
        name: &VarName,
        inputs: &std::collections::HashSet<VarName>,
    ) -> bool {
        if inputs.contains(name) {
            return true;
        }
        if let Some(base_name) = component_base_name(name.as_str())
            && base_name != name.as_str()
        {
            let base = VarName::new(base_name.clone());
            if inputs.contains(&base) {
                return true;
            }
            let base_prefix = format!("{base_name}.");
            if inputs
                .iter()
                .any(|input| input.as_str().starts_with(&base_prefix))
            {
                return true;
            }
        }
        let prefix = format!("{}.", name.as_str());
        inputs
            .iter()
            .any(|input| input.as_str().starts_with(&prefix))
    }
}

fn insert_matching_runtime_targets<'a, I>(
    defined: &mut std::collections::HashSet<String>,
    candidate: &str,
    prefix: &str,
    names: I,
) where
    I: Iterator<Item = &'a VarName>,
{
    for text in names
        .map(VarName::as_str)
        .filter(|text| *text == candidate || text.starts_with(prefix))
    {
        defined.insert(text.to_string());
    }
}

fn runtime_assignment_target_names(expr: &Expression) -> Vec<&VarName> {
    let Expression::Binary {
        op: OpBinary::Sub(_),
        lhs,
        rhs,
    } = expr
    else {
        return Vec::new();
    };

    let mut names = Vec::with_capacity(2);
    if let Expression::VarRef { name, .. } = lhs.as_ref() {
        names.push(name);
    }
    if let Expression::VarRef { name, .. } = rhs.as_ref() {
        names.push(name);
    }
    names
}

fn runtime_assignment_target_name(expr: &Expression) -> Option<&VarName> {
    runtime_assignment_target_names(expr).into_iter().next()
}

fn runtime_assignment_solution_expr(expr: &Expression) -> Option<&Expression> {
    let Expression::Binary {
        op: OpBinary::Sub(_),
        lhs,
        rhs,
    } = expr
    else {
        return None;
    };

    if matches!(lhs.as_ref(), Expression::VarRef { .. }) {
        return Some(rhs.as_ref());
    }
    if matches!(rhs.as_ref(), Expression::VarRef { .. }) {
        return Some(lhs.as_ref());
    }
    None
}

fn expression_contains_clocked_or_event_operators(expr: &Expression) -> bool {
    match expr {
        Expression::BuiltinCall { function, args } => {
            if matches!(
                function,
                BuiltinFunction::Pre
                    | BuiltinFunction::Sample
                    | BuiltinFunction::Edge
                    | BuiltinFunction::Change
            ) {
                return true;
            }
            args.iter()
                .any(expression_contains_clocked_or_event_operators)
        }
        Expression::FunctionCall { name, args, .. } => {
            let short_name = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            if matches!(
                short_name,
                "previous"
                    | "Clock"
                    | "hold"
                    | "subSample"
                    | "superSample"
                    | "shiftSample"
                    | "backSample"
                    | "noClock"
                    | "firstTick"
                    | "interval"
            ) {
                return true;
            }
            args.iter()
                .any(expression_contains_clocked_or_event_operators)
        }
        Expression::Binary { lhs, rhs, .. } => {
            expression_contains_clocked_or_event_operators(lhs)
                || expression_contains_clocked_or_event_operators(rhs)
        }
        Expression::Unary { rhs, .. } => expression_contains_clocked_or_event_operators(rhs),
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expression_contains_clocked_or_event_operators(cond)
                    || expression_contains_clocked_or_event_operators(value)
            }) || expression_contains_clocked_or_event_operators(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => elements
            .iter()
            .any(expression_contains_clocked_or_event_operators),
        Expression::Range { start, step, end } => {
            expression_contains_clocked_or_event_operators(start)
                || step
                    .as_deref()
                    .is_some_and(expression_contains_clocked_or_event_operators)
                || expression_contains_clocked_or_event_operators(end)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expression_contains_clocked_or_event_operators(expr)
                || indices
                    .iter()
                    .any(|idx| expression_contains_clocked_or_event_operators(&idx.range))
                || filter
                    .as_deref()
                    .is_some_and(expression_contains_clocked_or_event_operators)
        }
        Expression::Index { base, subscripts } => {
            expression_contains_clocked_or_event_operators(base)
                || subscripts.iter().any(|sub| match sub {
                    Subscript::Expr(expr) => expression_contains_clocked_or_event_operators(expr),
                    Subscript::Index(_) | Subscript::Colon => false,
                })
        }
        Expression::FieldAccess { base, .. } => {
            expression_contains_clocked_or_event_operators(base)
        }
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => false,
    }
}

/// A variable in the DAE system.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Variable {
    /// Variable name.
    pub name: VarName,
    /// Array dimensions (empty for scalars).
    pub dims: Vec<i64>,
    /// Start value.
    pub start: Option<Expression>,
    /// Fixed attribute (for initial conditions).
    pub fixed: Option<bool>,
    /// Minimum value.
    pub min: Option<Expression>,
    /// Maximum value.
    pub max: Option<Expression>,
    /// Nominal value (for scaling).
    pub nominal: Option<Expression>,
    /// Physical unit.
    pub unit: Option<String>,
    /// State selection hint.
    pub state_select: ir_flat::StateSelect,
    /// Description string.
    pub description: Option<String>,
}

impl Variable {
    /// Create a new variable with the given name.
    pub fn new(name: VarName) -> Self {
        Self {
            name,
            ..Default::default()
        }
    }

    /// Check if this is a scalar (0-dimensional).
    pub fn is_scalar(&self) -> bool {
        self.dims.is_empty()
    }

    /// Get the total size (product of dimensions, 1 for scalars).
    pub fn size(&self) -> usize {
        if self.dims.is_empty() {
            1
        } else {
            self.dims.iter().map(|&d| d.max(0) as usize).product()
        }
    }
}

/// An equation in the DAE system.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Equation {
    /// Left-hand side (variable being defined, if any).
    pub lhs: Option<VarName>,
    /// Right-hand side expression.
    pub rhs: Expression,
    /// Source span for error reporting. Never loses source location.
    pub span: Span,
    /// Human-readable origin description (for debugging).
    pub origin: String,
    /// Number of scalar equations this represents (MLS §8.4).
    /// For array equations like `x[n] = expr`, this is n.
    /// For scalar equations, this is 1.
    #[serde(default = "default_scalar_count")]
    pub scalar_count: usize,
}

/// Default scalar count for equations (1 for serde deserialization).
fn default_scalar_count() -> usize {
    1
}

#[cfg(test)]
mod tests {
    use super::{Algorithm, Dae, Equation, VarName, Variable};
    use rumoca_core::Span;
    use rumoca_ir_flat as flat;

    fn scalar_eq(count: usize) -> Equation {
        Equation {
            lhs: Some(VarName::new("x")),
            rhs: flat::Expression::Binary {
                op: flat::OpBinary::Sub(Default::default()),
                lhs: Box::new(flat::Expression::VarRef {
                    name: VarName::new("x"),
                    subscripts: vec![],
                }),
                rhs: Box::new(flat::Expression::Literal(flat::Literal::Integer(0))),
            },
            span: Span::DUMMY,
            origin: "test".to_string(),
            scalar_count: count,
        }
    }

    fn dae_with_unknown_scalars(unknown_scalars: i64) -> Dae {
        let mut dae = Dae::default();
        dae.algebraics.insert(
            VarName::new("x"),
            Variable {
                name: VarName::new("x"),
                dims: vec![unknown_scalars],
                ..Default::default()
            },
        );
        dae
    }

    fn assignment_stmt(name: &str) -> flat::Statement {
        flat::Statement::Assignment {
            comp: flat::ComponentReference {
                local: false,
                parts: vec![flat::ComponentRefPart {
                    ident: name.to_string(),
                    subs: Vec::new(),
                }],
                def_id: None,
            },
            value: flat::Expression::Literal(flat::Literal::Integer(1)),
        }
    }

    #[test]
    fn test_dae_json_uses_mls_symbol_keys() {
        let dae = Dae::default();
        let value = serde_json::to_value(&dae).expect("DAE should serialize");
        let obj = value
            .as_object()
            .expect("serialized DAE should be a JSON object");

        for key in [
            "x", "y", "u", "w", "p", "z", "m", "f_x", "f_z", "f_m", "f_c",
        ] {
            assert!(obj.contains_key(key), "expected MLS key `{key}`");
        }

        for legacy in [
            "states",
            "algebraics",
            "inputs",
            "outputs",
            "parameters",
            "discrete_reals",
            "discrete_valued",
            "derivative_aliases",
        ] {
            assert!(
                !obj.contains_key(legacy),
                "legacy key `{legacy}` must not be serialized"
            );
        }
    }

    #[test]
    fn test_runtime_partition_scalar_counts() {
        let mut dae = Dae::default();
        dae.parameters.insert(
            VarName::new("p"),
            Variable {
                name: VarName::new("p"),
                ..Default::default()
            },
        );
        dae.constants.insert(
            VarName::new("c"),
            Variable {
                name: VarName::new("c"),
                ..Default::default()
            },
        );
        dae.states.insert(
            VarName::new("x"),
            Variable {
                name: VarName::new("x"),
                dims: vec![2],
                ..Default::default()
            },
        );
        dae.algebraics.insert(
            VarName::new("y"),
            Variable {
                name: VarName::new("y"),
                ..Default::default()
            },
        );
        dae.outputs.insert(
            VarName::new("w"),
            Variable {
                name: VarName::new("w"),
                ..Default::default()
            },
        );
        dae.discrete_reals.insert(
            VarName::new("z"),
            Variable {
                name: VarName::new("z"),
                dims: vec![3],
                ..Default::default()
            },
        );
        dae.discrete_valued.insert(
            VarName::new("m"),
            Variable {
                name: VarName::new("m"),
                ..Default::default()
            },
        );

        let counts = dae.runtime_partition_scalar_counts();
        assert_eq!(counts.p, 2);
        assert_eq!(counts.t, 1);
        assert_eq!(counts.x, 2);
        assert_eq!(counts.y, 2);
        assert_eq!(counts.z, 3);
        assert_eq!(counts.m, 1);
    }

    #[test]
    fn test_balance_clamps_overconstrained_interface_to_deficit() {
        let mut dae = dae_with_unknown_scalars(4);
        dae.f_x.push(scalar_eq(4));
        dae.overconstrained_interface_count = 9;

        // Base equations already match unknowns, so OC interface terms must not
        // over-correct into positive imbalance.
        assert_eq!(dae.balance(), 0);
    }

    #[test]
    fn test_balance_uses_only_needed_overconstrained_interface() {
        let mut dae = dae_with_unknown_scalars(4);
        dae.f_x.push(scalar_eq(3));
        dae.overconstrained_interface_count = 9;

        // Base deficit is exactly 1 equation, so only 1 OC scalar is needed.
        assert_eq!(dae.balance(), 0);
    }

    #[test]
    fn test_balance_applies_oc_interface_even_with_break_edges() {
        let mut dae = dae_with_unknown_scalars(10);
        dae.f_x.push(scalar_eq(1));
        dae.overconstrained_interface_count = 9;
        dae.oc_break_edge_scalar_count = 12;

        // OC interface equations are needed to close the deficit.
        // Break-edge correction should only remove excess equations when
        // the system is over-determined, not suppress needed OC correction.
        assert_eq!(dae.balance(), 0);
    }

    #[test]
    fn test_balance_clamps_interface_flow_to_remaining_deficit() {
        let mut dae = dae_with_unknown_scalars(4);
        dae.f_x.push(scalar_eq(4));
        dae.interface_flow_count = 3;

        // Base equations already close all unknowns, so interface-flow terms
        // must be clamped to zero to avoid double-counting.
        assert_eq!(dae.balance(), 0);
    }

    #[test]
    fn test_balance_uses_interface_flow_to_close_deficit_only() {
        let mut dae = dae_with_unknown_scalars(5);
        dae.f_x.push(scalar_eq(3));
        dae.interface_flow_count = 9;

        // Only 2 interface-flow equations are needed to close the deficit.
        assert_eq!(dae.balance(), 0);
    }

    #[test]
    fn test_balance_counts_continuous_equations_only() {
        let mut dae = Dae::default();
        dae.algebraics.insert(
            VarName::new("y"),
            Variable {
                name: VarName::new("y"),
                ..Default::default()
            },
        );
        dae.f_x.push(Equation::explicit(
            VarName::new("y"),
            flat::Expression::Literal(flat::Literal::Integer(1)),
            Span::DUMMY,
            "test",
        ));

        assert_eq!(dae.balance(), 0);
    }

    #[test]
    fn test_dae_algorithm_from_flat_fills_outputs_from_statements() {
        let flat_alg =
            flat::Algorithm::new(vec![assignment_stmt("y")], Span::DUMMY, "algorithm section");

        let dae_alg = Algorithm::from_flat(&flat_alg);
        assert_eq!(dae_alg.outputs, vec![VarName::new("y")]);
    }

    #[test]
    fn test_explicit_with_scalar_count_clamps_zero_to_one() {
        let eq = Equation::explicit_with_scalar_count(
            VarName::new("x"),
            flat::Expression::Literal(flat::Literal::Integer(1)),
            Span::DUMMY,
            "test",
            0,
        );
        assert_eq!(eq.scalar_count, 1);
    }

    #[test]
    fn test_explicit_with_scalar_count_preserves_nonzero_count() {
        let eq = Equation::explicit_with_scalar_count(
            VarName::new("x"),
            flat::Expression::Literal(flat::Literal::Integer(1)),
            Span::DUMMY,
            "test",
            3,
        );
        assert_eq!(eq.scalar_count, 3);
    }

    #[test]
    fn test_runtime_defined_unknown_names_include_discrete_targets() {
        let mut dae = Dae::default();
        dae.algebraics.insert(
            VarName::new("a"),
            Variable {
                name: VarName::new("a"),
                ..Default::default()
            },
        );
        dae.discrete_valued.insert(
            VarName::new("enable"),
            Variable {
                name: VarName::new("enable"),
                ..Default::default()
            },
        );
        dae.f_m.push(Equation::explicit(
            VarName::new("a"),
            flat::Expression::Literal(flat::Literal::Real(1.0)),
            Span::DUMMY,
            "runtime-defined-a",
        ));
        dae.f_m.push(Equation::explicit(
            VarName::new("enable"),
            flat::Expression::Literal(flat::Literal::Boolean(true)),
            Span::DUMMY,
            "runtime-defined-enable",
        ));

        let all = dae.runtime_defined_unknown_names();
        assert!(all.contains("a"));
        assert!(all.contains("enable"));
    }

    #[test]
    fn test_runtime_defined_continuous_unknown_names_exclude_discrete_targets() {
        let mut dae = Dae::default();
        dae.algebraics.insert(
            VarName::new("a"),
            Variable {
                name: VarName::new("a"),
                ..Default::default()
            },
        );
        dae.discrete_valued.insert(
            VarName::new("enable"),
            Variable {
                name: VarName::new("enable"),
                ..Default::default()
            },
        );
        dae.f_m.push(Equation::explicit(
            VarName::new("a"),
            flat::Expression::Literal(flat::Literal::Real(1.0)),
            Span::DUMMY,
            "runtime-defined-a",
        ));
        dae.f_m.push(Equation::explicit(
            VarName::new("enable"),
            flat::Expression::Literal(flat::Literal::Boolean(true)),
            Span::DUMMY,
            "runtime-defined-enable",
        ));

        let continuous = dae.runtime_defined_continuous_unknown_names();
        assert!(continuous.contains("a"));
        assert!(!continuous.contains("enable"));
    }

    #[test]
    fn test_runtime_defined_continuous_unknown_names_include_fx_pre_assignment_targets() {
        let mut dae = Dae::default();
        dae.algebraics.insert(
            VarName::new("gate.y"),
            Variable {
                name: VarName::new("gate.y"),
                ..Default::default()
            },
        );
        dae.algebraics.insert(
            VarName::new("gate.aux"),
            Variable {
                name: VarName::new("gate.aux"),
                ..Default::default()
            },
        );
        dae.f_x.push(Equation::residual(
            flat::Expression::Binary {
                op: flat::OpBinary::Sub(Default::default()),
                lhs: Box::new(flat::Expression::VarRef {
                    name: VarName::new("gate.y"),
                    subscripts: vec![],
                }),
                rhs: Box::new(flat::Expression::BuiltinCall {
                    function: flat::BuiltinFunction::Pre,
                    args: vec![flat::Expression::VarRef {
                        name: VarName::new("gate.aux"),
                        subscripts: vec![],
                    }],
                }),
            },
            Span::DUMMY,
            "equation from gate",
        ));

        let continuous = dae.runtime_defined_continuous_unknown_names();
        assert!(
            continuous.contains("gate.y"),
            "f_x assignment targets using pre() must remain runtime-defined"
        );
    }

    #[test]
    fn test_runtime_defined_unknown_names_include_discrete_partition_targets() {
        let mut dae = Dae::default();
        dae.discrete_valued.insert(
            VarName::new("logic.qn"),
            Variable {
                name: VarName::new("logic.qn"),
                ..Default::default()
            },
        );
        dae.discrete_valued.insert(
            VarName::new("logic.src"),
            Variable {
                name: VarName::new("logic.src"),
                ..Default::default()
            },
        );
        dae.f_m.push(Equation::residual(
            flat::Expression::Binary {
                op: flat::OpBinary::Sub(Default::default()),
                lhs: Box::new(flat::Expression::VarRef {
                    name: VarName::new("logic.qn"),
                    subscripts: vec![],
                }),
                rhs: Box::new(flat::Expression::VarRef {
                    name: VarName::new("logic.src"),
                    subscripts: vec![],
                }),
            },
            Span::DUMMY,
            "discrete assignment",
        ));

        let all = dae.runtime_defined_unknown_names();
        assert!(
            all.contains("logic.qn"),
            "f_m assignment target must be runtime-defined"
        );
    }

    #[test]
    fn test_runtime_defined_unknown_names_include_both_sides_of_discrete_alias() {
        let mut dae = Dae::default();
        dae.discrete_valued.insert(
            VarName::new("logic.qn"),
            Variable {
                name: VarName::new("logic.qn"),
                ..Default::default()
            },
        );
        dae.discrete_valued.insert(
            VarName::new("logic.y"),
            Variable {
                name: VarName::new("logic.y"),
                ..Default::default()
            },
        );
        dae.f_m.push(Equation::residual(
            flat::Expression::Binary {
                op: flat::OpBinary::Sub(Default::default()),
                lhs: Box::new(flat::Expression::VarRef {
                    name: VarName::new("logic.y"),
                    subscripts: vec![],
                }),
                rhs: Box::new(flat::Expression::VarRef {
                    name: VarName::new("logic.qn"),
                    subscripts: vec![],
                }),
            },
            Span::DUMMY,
            "discrete alias",
        ));

        let all = dae.runtime_defined_unknown_names();
        assert!(all.contains("logic.y"));
        assert!(all.contains("logic.qn"));
    }
}

impl Equation {
    /// Create a new equation in residual form (0 = rhs).
    pub fn residual(rhs: Expression, span: Span, origin: impl Into<String>) -> Self {
        Self {
            lhs: None,
            rhs,
            span,
            origin: origin.into(),
            scalar_count: 1,
        }
    }

    /// Create a new equation in residual form with explicit scalar count.
    pub fn residual_array(
        rhs: Expression,
        span: Span,
        origin: impl Into<String>,
        scalar_count: usize,
    ) -> Self {
        Self {
            lhs: None,
            rhs,
            span,
            origin: origin.into(),
            scalar_count,
        }
    }

    /// Create a new equation in explicit form (lhs = rhs).
    pub fn explicit(lhs: VarName, rhs: Expression, span: Span, origin: impl Into<String>) -> Self {
        Self::explicit_with_scalar_count(lhs, rhs, span, origin, 1)
    }

    /// Create a new explicit equation with a scalarized equation count.
    ///
    /// The scalar count is clamped to at least 1 so callers cannot accidentally
    /// construct an explicit equation that contributes zero scalars to balance.
    pub fn explicit_with_scalar_count(
        lhs: VarName,
        rhs: Expression,
        span: Span,
        origin: impl Into<String>,
        scalar_count: usize,
    ) -> Self {
        Self {
            lhs: Some(lhs),
            rhs,
            span,
            origin: origin.into(),
            scalar_count: scalar_count.max(1),
        }
    }
}

/// A when clause for discrete event handling.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WhenClause {
    /// The trigger condition.
    pub condition: Expression,
    /// Equations active when triggered.
    pub equations: Vec<Equation>,
    /// Source span for error reporting.
    pub span: Span,
    /// Human-readable origin description (for debugging).
    pub origin: String,
}

impl WhenClause {
    /// Create a new when clause with span information.
    pub fn new(condition: Expression, span: Span, origin: impl Into<String>) -> Self {
        Self {
            condition,
            equations: Vec::new(),
            span,
            origin: origin.into(),
        }
    }
}

/// An algorithm section in the DAE system.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Algorithm {
    /// Statements in this algorithm.
    pub statements: Vec<Statement>,
    /// Output variables (left-hand sides of assignments).
    /// Used for balance checking per SPEC_0020.
    pub outputs: Vec<VarName>,
    /// Source span for error reporting.
    pub span: Span,
    /// Human-readable origin description (for debugging).
    pub origin: String,
}

impl Algorithm {
    /// Create a new algorithm section from a Algorithm.
    pub fn from_flat(alg: &rumoca_ir_flat::Algorithm) -> Self {
        let mut outputs = IndexSet::new();
        outputs.extend(alg.outputs.iter().cloned());
        outputs.extend(rumoca_ir_flat::extract_algorithm_outputs(&alg.statements));
        Self {
            statements: alg.statements.clone(),
            outputs: outputs.into_iter().collect(),
            span: alg.span,
            origin: alg.origin.clone(),
        }
    }

    /// Get the number of outputs (equations contributed).
    pub fn num_outputs(&self) -> usize {
        self.outputs.len()
    }
}

/// Classification of a variable for DAE analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum VariableKind {
    /// State variable (has derivative).
    State,
    /// Derivative of a state variable.
    Derivative,
    /// Algebraic variable (no derivative).
    Algebraic,
    /// Input variable (known externally).
    Input,
    /// Output variable (computed result).
    Output,
    /// Parameter (fixed during simulation).
    Parameter,
    /// Constant (fixed at compile time).
    Constant,
    /// Discrete variable (changes at events).
    Discrete,
}
