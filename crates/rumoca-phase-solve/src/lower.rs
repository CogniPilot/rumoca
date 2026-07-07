//! Lower flat expressions and DAE residual rows to linear ops.
//!
//! SPEC_0021 file-size exception: this facade coordinates solve lowering
//! submodules and still owns shared lowering dispatch. split plan: continue
//! moving projection and branch logic into focused submodules.

use std::sync::Arc;

use indexmap::{IndexMap, IndexSet};
use rumoca_core::VarName;
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{
    BinaryOp, CompareOp, ComponentReferenceKey, ComputeBlock, LinearOp, Reg, UnaryOp,
};
use rumoca_ir_solve::{ScalarSlot, VarLayout};

mod array_values;
mod builtin_methods;
mod clock;
mod compile_time;
#[cfg(test)]
mod complex_operator_tests;
mod complex_projection;
mod cse;
mod derivative_rhs;
mod discrete_updates;
mod emit;
mod error;
mod expression_rows;
mod fft;
mod function_calls;
mod function_dispatch;
mod function_projection;
mod helpers;
mod initial_residual;
mod misc_helpers;
mod root_conditions;
mod scalar_ops;
mod scope;
mod source_refs;
mod statements;
#[cfg(test)]
#[path = "lower/tests/test_fixtures.rs"]
mod test_fixtures;
#[cfg(test)]
mod tests;

use cse::RowCse;
pub(crate) use discrete_updates::{
    initial_condition_update_equations, lower_initial_update_rhs,
    normalized_discrete_update_equations,
};
pub use error::LowerError;
use error::unsupported_at;
pub use expression_rows::{
    lower_expression_rows_from_expressions,
    lower_expression_rows_from_expressions_with_runtime_metadata,
    lower_initial_expression_rows_from_expressions,
    lower_initial_expression_rows_from_expressions_with_runtime_metadata,
};
use function_projection::format_subscript_binding_key;
use helpers::*;
pub use initial_residual::{initial_residual_equations, lower_initial_residual};
use misc_helpers::*;
use scope::*;
use source_refs::*;

const MAX_FUNCTION_INLINE_DEPTH: usize = 64;
/// Projected function-call scalars larger than this many expression nodes are
/// not inlined; the call is kept for runtime evaluation instead. Textual
/// inlining duplicates argument expressions per use, which grows
/// exponentially across nested array-valued calls without a size cutoff.
const MAX_FUNCTION_PROJECTION_NODES: usize = 4096;
pub(crate) use rumoca_core::NAMED_FUNCTION_ARG_PREFIX;
pub(super) const SIZE_BINDING_PREFIX: &str = "__rumoca_size__.";
const RETURN_FLAG_BINDING: &str = "__rumoca_returned__";
pub(super) const BREAK_FLAG_BINDING: &str = "__rumoca_break__";

#[derive(Debug, Clone)]
pub(super) struct IndexedBinding {
    slot: ScalarSlot,
    indices: Vec<usize>,
}

pub(super) type IndexedBindingMap = Arc<IndexMap<ComponentReferenceKey, Vec<IndexedBinding>>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct LocalIndexedBinding {
    reg: Reg,
    indices: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoweredExpression {
    pub ops: Vec<LinearOp>,
    pub result: Reg,
}

pub fn lower_expression(
    expr: &rumoca_core::Expression,
    layout: &VarLayout,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> Result<LoweredExpression, LowerError> {
    let mut builder = LowerBuilder::new(layout, functions);
    let scope = Scope::new();
    let result = if let Some(span) = expr.span().filter(|span| !span.is_dummy()) {
        builder.lower_expr_with_source_context(expr, span, &scope, 0)?
    } else {
        builder.lower_expr(expr, &scope, 0)?
    };
    Ok(LoweredExpression {
        ops: builder.ops,
        result,
    })
}

pub fn lower_residual(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    expression_rows::lower_residual_rows_with_mode(dae_model, layout, false)
}

pub(crate) fn lower_residual_rows_and_targets_from_equations<'a>(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    equations: impl IntoIterator<Item = (usize, &'a dae::Equation)>,
    state_scalar_count: usize,
    target_rows_for_equation: impl FnMut(
        &dae::Equation,
        usize,
    ) -> Result<Vec<Option<ScalarSlot>>, LowerError>,
) -> Result<expression_rows::LoweredRowsAndTargets, LowerError> {
    expression_rows::lower_residual_rows_and_targets_from_equations_with_mode(
        dae_model,
        layout,
        equations,
        state_scalar_count,
        false,
        target_rows_for_equation,
    )
}

pub(crate) fn scalarized_record_field_binding_names(
    base: &str,
    layout: &VarLayout,
) -> Option<Vec<String>> {
    expression_rows::scalarized_record_field_binding_names(base, layout)
}

pub(crate) fn compile_time_subscript_indices_for_structured_access(
    subscripts: &[rumoca_core::Subscript],
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let mut indices = crate::lower_vec_with_capacity(
        subscripts.len(),
        "structured access subscript index count",
        subscript_span_with_owner(subscripts, owner_span),
    )?;
    for subscript in subscripts {
        indices.push(compile_time_subscript_index_with_owner(
            subscript,
            structural_bindings,
            owner_span,
        )?);
    }
    Ok(indices)
}

pub(crate) fn structural_bindings_for_structured_access(
    dae_model: &dae::Dae,
) -> Result<IndexMap<String, f64>, LowerError> {
    compile_time::structural_bindings(dae_model)
}

pub(crate) fn external_table_data_for_dae(
    dae_model: &dae::Dae,
) -> Result<Vec<rumoca_core::ExternalTableData>, LowerError> {
    compile_time::external_table_data(dae_model)
}

pub fn lower_derivative_rhs(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<ComputeBlock, LowerError> {
    derivative_rhs::lower_derivative_rhs(dae_model, layout)
}

pub(crate) fn analyze_derivative_rhs(
    dae_model: &dae::Dae,
) -> Result<derivative_rhs::DerivativeRhsAnalysis, LowerError> {
    derivative_rhs::analyze_derivative_rhs(dae_model)
}

pub(crate) fn lower_derivative_rhs_with_analysis(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    analysis: &derivative_rhs::DerivativeRhsAnalysis,
) -> Result<ComputeBlock, LowerError> {
    derivative_rhs::lower_derivative_rhs_with_analysis(dae_model, layout, analysis)
}

pub fn lower_derivative_rhs_scalar_programs(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    derivative_rhs::lower_derivative_rhs_scalar_programs(dae_model, layout)
}

pub(crate) fn state_derivative_equation_flags(
    dae_model: &dae::Dae,
) -> Result<Vec<bool>, LowerError> {
    derivative_rhs::state_derivative_equation_flags(dae_model)
}

pub fn lower_discrete_rhs(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let equations = normalized_discrete_update_equations(dae_model)?;
    lower_discrete_rhs_from_equations(dae_model, layout, &equations)
}

pub(crate) fn lower_discrete_rhs_from_equations(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    equations: &[dae::Equation],
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let structural_bindings = compile_time::structural_bindings(dae_model)?;
    Ok(rumoca_eval_solve::to_scalar_program_block(
        &expression_rows::lower_expression_rows_with_mode(
            equations.iter(),
            layout,
            &dae_model.symbols.functions,
            expression_rows::RuntimeRowMetadata {
                clock_intervals: &dae_model.clocks.intervals,
                clock_timings: &dae_model.clocks.timings,
                triggered_clock_conditions: &dae_model.clocks.triggered_conditions,
                discrete_valued_names: &dae_model.variables.discrete_valued,
                variable_starts: &dae_model.metadata.variable_starts,
                dae_variables: Some(&dae_model.variables),
                structural_bindings: Some(Arc::new(structural_bindings)),
                guard_target_start_before_first_clock_tick: true,
            },
            false,
        )?,
    )?
    .programs)
}

pub(crate) fn lower_runtime_assignment_rhs(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    equations: &[dae::Equation],
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let structural_bindings = compile_time::structural_bindings(dae_model)?;
    Ok(rumoca_eval_solve::to_scalar_program_block(
        &expression_rows::lower_expression_rows_with_mode(
            equations.iter(),
            layout,
            &dae_model.symbols.functions,
            expression_rows::RuntimeRowMetadata {
                clock_intervals: &dae_model.clocks.intervals,
                clock_timings: &dae_model.clocks.timings,
                triggered_clock_conditions: &dae_model.clocks.triggered_conditions,
                discrete_valued_names: &dae_model.variables.discrete_valued,
                variable_starts: &dae_model.metadata.variable_starts,
                dae_variables: Some(&dae_model.variables),
                structural_bindings: Some(Arc::new(structural_bindings)),
                guard_target_start_before_first_clock_tick: true,
            },
            false,
        )?,
    )?
    .programs)
}

pub(crate) fn lower_dynamic_time_event_rhs(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    expressions: &[rumoca_core::Expression],
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let structural_bindings = compile_time::structural_bindings(dae_model)?;
    expression_rows::lower_expression_rows_from_expressions_with_structural_bindings(
        expressions,
        layout,
        &dae_model.symbols.functions,
        expression_rows::RuntimeRowMetadata {
            clock_intervals: &dae_model.clocks.intervals,
            clock_timings: &dae_model.clocks.timings,
            triggered_clock_conditions: &[],
            discrete_valued_names: &dae_model.variables.discrete_valued,
            variable_starts: &dae_model.metadata.variable_starts,
            dae_variables: Some(&dae_model.variables),
            structural_bindings: Some(Arc::new(structural_bindings)),
            guard_target_start_before_first_clock_tick: false,
        },
    )
}

pub fn lower_observation_rhs(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    expressions: &[rumoca_core::Expression],
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let structural_bindings = Arc::new(compile_time::structural_bindings(dae_model)?);
    let indexed_bindings = indexed_bindings_for_layout(layout);
    lower_observation_rhs_with_structural_bindings(
        dae_model,
        layout,
        expressions,
        &structural_bindings,
        &indexed_bindings,
    )
}

/// Observation lowering with a caller-provided structural-bindings map, so
/// batch callers build the (potentially large) scalarized binding map once
/// instead of once per observation row.
pub(crate) fn structural_bindings_for_dae(
    dae_model: &dae::Dae,
) -> Result<Arc<IndexMap<String, f64>>, LowerError> {
    Ok(Arc::new(compile_time::structural_bindings(dae_model)?))
}

/// Prebuilt layout binding index for batch observation lowering.
pub(crate) fn indexed_bindings_for_layout(layout: &VarLayout) -> IndexedBindingMap {
    Arc::new(build_indexed_binding_map(layout))
}

pub(crate) fn lower_observation_rhs_with_structural_bindings(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    expressions: &[rumoca_core::Expression],
    structural_bindings: &Arc<IndexMap<String, f64>>,
    indexed_bindings: &IndexedBindingMap,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    expression_rows::lower_observation_rows_from_expressions_with_structural_bindings(
        expressions,
        layout,
        &dae_model.symbols.functions,
        expression_rows::RuntimeRowMetadata {
            clock_intervals: &dae_model.clocks.intervals,
            clock_timings: &dae_model.clocks.timings,
            triggered_clock_conditions: &[],
            discrete_valued_names: &dae_model.variables.discrete_valued,
            variable_starts: &dae_model.metadata.variable_starts,
            dae_variables: Some(&dae_model.variables),
            structural_bindings: Some(Arc::clone(structural_bindings)),
            guard_target_start_before_first_clock_tick: false,
        },
        Arc::clone(indexed_bindings),
    )
}

pub fn lower_root_conditions(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    root_conditions::lower_root_conditions(dae_model, layout)
}

pub fn lower_root_relation_memory_targets(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Option<ScalarSlot>>, LowerError> {
    root_conditions::lower_root_relation_memory_targets(dae_model, layout)
}

struct LowerBuilder<'a> {
    layout: &'a VarLayout,
    functions: &'a IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    clock_intervals: Option<&'a IndexMap<String, f64>>,
    clock_timings: Option<&'a IndexMap<String, dae::ClockSchedule>>,
    triggered_clock_conditions: Option<&'a [rumoca_core::Expression]>,
    discrete_valued_names: Option<&'a IndexMap<rumoca_core::VarName, dae::Variable>>,
    variable_starts: Option<&'a IndexMap<String, rumoca_core::Expression>>,
    dae_variables: Option<&'a dae::DaeVariables>,
    structural_bindings: Arc<IndexMap<String, f64>>,
    direct_assignments: Arc<IndexMap<String, DirectAssignmentValue>>,
    direct_assignment_stack: Vec<String>,
    indexed_bindings: IndexedBindingMap,
    local_indexed_bindings: IndexMap<String, Vec<LocalIndexedBinding>>,
    local_binding_dims: IndexMap<String, Vec<i64>>,
    known_empty_local_arrays: IndexSet<String>,
    guarded_uninitialized_locals: IndexSet<String>,
    local_const_bindings: IndexMap<String, f64>,
    function_closures: IndexMap<ComponentReferenceKey, FunctionClosure>,
    is_initial_mode: bool,
    value_mode: ValueMode,
    current_update_target: Option<ScalarSlot>,
    source_context_span: Option<rumoca_core::Span>,
    ops: Vec<LinearOp>,
    next_reg: Reg,
    call_site_namespace: u64,
    next_call_site: u64,
    cse: RowCse,
    param_slot_regs: IndexMap<Reg, usize>,
    dedup_access_ops: bool,
    /// Lazy per-key indexed-binding metadata (dims + indices lookup).
    /// `RefCell` because dims inference also runs from `&self` paths.
    indexed_meta_cache: std::cell::RefCell<IndexMap<ComponentReferenceKey, Arc<IndexedMeta>>>,
    /// Lazy index from a parent binding path to its direct scalarized
    /// children, mirroring `scope_key_direct_child_suffix` semantics.
    /// Built once on first use: the previous per-reference full scan of
    /// `layout.bindings()` was quadratic in model size and dominated
    /// solve lowering for large discretized models.
    scalarized_children_index: Option<IndexMap<String, Vec<(ComponentReferenceKey, String)>>>,
}

#[derive(Default)]
pub(super) struct LowerBuilderMetadata<'a> {
    pub(super) clock_intervals: Option<&'a IndexMap<String, f64>>,
    pub(super) clock_timings: Option<&'a IndexMap<String, dae::ClockSchedule>>,
    pub(super) triggered_clock_conditions: Option<&'a [rumoca_core::Expression]>,
    pub(super) discrete_valued_names: Option<&'a IndexMap<rumoca_core::VarName, dae::Variable>>,
    pub(super) variable_starts: Option<&'a IndexMap<String, rumoca_core::Expression>>,
    pub(super) dae_variables: Option<&'a dae::DaeVariables>,
    pub(super) indexed_bindings: Option<&'a IndexedBindingMap>,
    pub(super) is_initial_mode: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValueMode {
    Current,
    Pre,
}

impl<'a> LowerBuilder<'a> {
    fn new(
        layout: &'a VarLayout,
        functions: &'a IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    ) -> Self {
        Self::new_with_metadata(layout, functions, LowerBuilderMetadata::default())
    }

    fn new_with_runtime_metadata(
        layout: &'a VarLayout,
        functions: &'a IndexMap<rumoca_core::VarName, rumoca_core::Function>,
        clock_intervals: &'a IndexMap<String, f64>,
        clock_timings: &'a IndexMap<String, dae::ClockSchedule>,
        triggered_clock_conditions: &'a [rumoca_core::Expression],
        variable_starts: &'a IndexMap<String, rumoca_core::Expression>,
        dae_variables: &'a dae::DaeVariables,
        is_initial_mode: bool,
    ) -> Self {
        Self::new_with_metadata(
            layout,
            functions,
            LowerBuilderMetadata {
                clock_intervals: Some(clock_intervals),
                clock_timings: Some(clock_timings),
                triggered_clock_conditions: Some(triggered_clock_conditions),
                discrete_valued_names: None,
                variable_starts: Some(variable_starts),
                dae_variables: Some(dae_variables),
                indexed_bindings: None,
                is_initial_mode,
            },
        )
    }

    fn new_with_metadata(
        layout: &'a VarLayout,
        functions: &'a IndexMap<rumoca_core::VarName, rumoca_core::Function>,
        metadata: LowerBuilderMetadata<'a>,
    ) -> Self {
        Self {
            layout,
            functions,
            clock_intervals: metadata.clock_intervals,
            clock_timings: metadata.clock_timings,
            triggered_clock_conditions: metadata.triggered_clock_conditions,
            discrete_valued_names: metadata.discrete_valued_names,
            variable_starts: metadata.variable_starts,
            dae_variables: metadata.dae_variables,
            structural_bindings: Arc::default(),
            direct_assignments: Arc::default(),
            direct_assignment_stack: Vec::new(),
            indexed_bindings: metadata
                .indexed_bindings
                .cloned()
                .unwrap_or_else(|| Arc::new(build_indexed_binding_map(layout))),
            local_indexed_bindings: IndexMap::new(),
            local_binding_dims: IndexMap::new(),
            known_empty_local_arrays: IndexSet::new(),
            guarded_uninitialized_locals: IndexSet::new(),
            local_const_bindings: IndexMap::new(),
            function_closures: IndexMap::new(),
            is_initial_mode: metadata.is_initial_mode,
            value_mode: ValueMode::Current,
            current_update_target: None,
            source_context_span: None,
            ops: Vec::new(),
            next_reg: 0,
            call_site_namespace: 0,
            next_call_site: 0,
            cse: RowCse::default(),
            param_slot_regs: IndexMap::new(),
            dedup_access_ops: true,
            indexed_meta_cache: std::cell::RefCell::new(IndexMap::new()),
            scalarized_children_index: None,
        }
    }

    fn with_direct_assignments(
        mut self,
        direct_assignments: Arc<IndexMap<String, DirectAssignmentValue>>,
    ) -> Self {
        self.direct_assignments = direct_assignments;
        self
    }

    fn with_structural_bindings(mut self, structural_bindings: Arc<IndexMap<String, f64>>) -> Self {
        self.structural_bindings = structural_bindings;
        self
    }

    fn with_call_site_namespace(mut self, namespace: u64) -> Self {
        self.call_site_namespace = namespace;
        self
    }

    fn with_current_update_target(mut self, target: Option<ScalarSlot>) -> Self {
        self.current_update_target = target;
        self
    }

    fn with_dedup_access_ops(mut self, dedup_access_ops: bool) -> Self {
        self.dedup_access_ops = dedup_access_ops;
        self
    }

    /// Create an independent sub-builder that evaluates into a disjoint
    /// register space starting at `start_reg`.  Used by `build_matmul_node`
    /// to produce non-overlapping `lhs_ops` / `rhs_ops` register files.
    pub(super) fn fork_with_next_reg(&self, start_reg: Reg) -> LowerBuilder<'a> {
        LowerBuilder {
            layout: self.layout,
            functions: self.functions,
            clock_intervals: self.clock_intervals,
            clock_timings: self.clock_timings,
            triggered_clock_conditions: self.triggered_clock_conditions,
            discrete_valued_names: self.discrete_valued_names,
            variable_starts: self.variable_starts,
            dae_variables: self.dae_variables,
            structural_bindings: self.structural_bindings.clone(),
            direct_assignments: Arc::default(),
            direct_assignment_stack: Vec::new(),
            indexed_bindings: Arc::clone(&self.indexed_bindings),
            local_indexed_bindings: IndexMap::new(),
            local_binding_dims: IndexMap::new(),
            known_empty_local_arrays: IndexSet::new(),
            guarded_uninitialized_locals: self.guarded_uninitialized_locals.clone(),
            local_const_bindings: self.local_const_bindings.clone(),
            function_closures: self.function_closures.clone(),
            is_initial_mode: self.is_initial_mode,
            value_mode: self.value_mode,
            current_update_target: self.current_update_target,
            source_context_span: self.source_context_span,
            ops: Vec::new(),
            next_reg: start_reg,
            call_site_namespace: self.call_site_namespace,
            next_call_site: 0,
            cse: RowCse::default(),
            param_slot_regs: IndexMap::new(),
            dedup_access_ops: self.dedup_access_ops,
            indexed_meta_cache: std::cell::RefCell::new(IndexMap::new()),
            scalarized_children_index: None,
        }
    }

    fn lower_expr_in_mode(
        &mut self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
        mode: ValueMode,
    ) -> Result<Reg, LowerError> {
        let old_mode = self.value_mode;
        self.value_mode = mode;
        let result = self.lower_expr(expr, scope, call_depth);
        self.value_mode = old_mode;
        result
    }

    fn non_dummy_span(span: rumoca_core::Span) -> Option<rumoca_core::Span> {
        (!span.is_dummy()).then_some(span)
    }

    fn active_source_context_span(&self) -> Option<rumoca_core::Span> {
        self.source_context_span.filter(|span| !span.is_dummy())
    }

    fn span_or_source_context(&self, span: rumoca_core::Span) -> Option<rumoca_core::Span> {
        Self::non_dummy_span(span).or_else(|| self.active_source_context_span())
    }

    fn with_optional_source_context<T>(
        &mut self,
        source_context_span: Option<rumoca_core::Span>,
        f: impl FnOnce(&mut Self) -> Result<T, LowerError>,
    ) -> Result<T, LowerError> {
        let old_source_context_span = self.source_context_span;
        if let Some(span) = source_context_span {
            self.source_context_span = Some(span);
        }
        let result = f(self);
        self.source_context_span = old_source_context_span;
        result
    }

    pub(in crate::lower) fn lower_array_like_values_with_source_context(
        &mut self,
        expr: &rumoca_core::Expression,
        source_context_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        self.with_optional_source_context(Self::non_dummy_span(source_context_span), |this| {
            this.lower_array_like_values(expr, scope, call_depth)
        })
    }

    fn lower_expr_with_source_context(
        &mut self,
        expr: &rumoca_core::Expression,
        source_context_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        self.with_optional_source_context(Self::non_dummy_span(source_context_span), |this| {
            this.lower_expr(expr, scope, call_depth)
        })
    }

    fn lower_array_like_values_with_optional_source_context(
        &mut self,
        expr: &rumoca_core::Expression,
        source_context_span: Option<rumoca_core::Span>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        match source_context_span {
            Some(span) => {
                self.lower_array_like_values_with_source_context(expr, span, scope, call_depth)
            }
            None => self.lower_array_like_values(expr, scope, call_depth),
        }
    }

    pub(in crate::lower) fn scope_key_from_reference(
        &self,
        name: &rumoca_core::Reference,
        span: rumoca_core::Span,
    ) -> Result<ComponentReferenceKey, LowerError> {
        if self
            .lookup_function_output_projection(name, span)?
            .is_some()
        {
            return Ok(ComponentReferenceKey::generated(name.as_str()));
        }
        if self.scalarized_field_binding_available(name.as_str(), "re")
            || self.scalarized_field_binding_available(name.as_str(), "im")
        {
            return Ok(ComponentReferenceKey::generated(name.as_str()));
        }
        if name.is_generated() || self.dae_variables.is_none() {
            return scope_key_from_reference(name, span);
        }
        if let Some(component_ref) = name.component_ref() {
            match ComponentReferenceKey::from_component_reference(component_ref) {
                Ok(key) => return Ok(key),
                Err(err)
                    if err.kind
                        == rumoca_ir_solve::ComponentReferenceKeyErrorKind::MissingDefId =>
                {
                    if let Some(component_ref) = self
                        .dae_variables
                        .and_then(|variables| dae_variable(variables, name.var_name()))
                        .and_then(|variable| variable.component_ref.as_ref())
                        && component_ref.def_id.is_some()
                    {
                        return ComponentReferenceKey::from_component_reference(component_ref)
                            .map_err(|err| {
                                LowerError::contract_violation(
                                    format!(
                                        "Solve lowering requires static component-reference metadata for `{}`: {err}",
                                        name.as_str(),
                                    ),
                                    err.span,
                                )
                            });
                    }
                    if let Some(component_ref) =
                        self.scalarized_base_component_ref(name, err.span)?
                    {
                        return ComponentReferenceKey::from_component_reference(&component_ref)
                            .map_err(|err| {
                                LowerError::contract_violation(
                                    format!(
                                        "Solve lowering requires static component-reference metadata for `{}`: {err}",
                                        name.as_str(),
                                    ),
                                    err.span,
                                )
                            });
                    }
                    return Err(LowerError::contract_violation(
                        format!(
                            "Solve lowering requires static component-reference metadata for `{}`: {err}",
                            name.as_str(),
                        ),
                        err.span,
                    ));
                }
                Err(err) => {
                    return Err(LowerError::contract_violation(
                        format!(
                            "Solve lowering requires static component-reference metadata for `{}`: {err}",
                            name.as_str(),
                        ),
                        err.span,
                    ));
                }
            }
        }
        let Some(variable) = self
            .dae_variables
            .and_then(|variables| dae_variable(variables, name.var_name()))
        else {
            #[cfg(test)]
            if let Some(generated) = self.test_fixture_generated_scope_key(name, span) {
                return Ok(generated);
            }
            return Err(LowerError::contract_violation(
                format!(
                    "Solve lowering synthesized source reference `{}` that is not a DAE variable",
                    name.as_str()
                ),
                span,
            ));
        };
        match variable.origin {
            dae::VariableOrigin::Generated => Ok(ComponentReferenceKey::generated(name.as_str())),
            dae::VariableOrigin::Source => {
                #[cfg(test)]
                if let Some(key) =
                    crate::test_support::fixture_key_for_variable(name.as_str(), variable)
                {
                    return Ok(key);
                }
                let component_ref = variable.component_ref.as_ref().ok_or_else(|| {
                    LowerError::contract_violation(
                        format!(
                            "source DAE variable `{}` lost structured component-reference metadata before Solve lowering",
                            name.as_str()
                        ),
                        span,
                    )
                })?;
                ComponentReferenceKey::from_component_reference(component_ref).map_err(|err| {
                    LowerError::contract_violation(
                        format!(
                            "Solve lowering requires static component-reference metadata for `{}`: {err}",
                            name.as_str(),
                        ),
                        err.span,
                    )
                })
            }
        }
    }

    fn scalarized_base_component_ref(
        &self,
        name: &rumoca_core::Reference,
        span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::ComponentReference>, LowerError> {
        let Some(scalar) = rumoca_core::parse_scalar_name(name.as_str()) else {
            return Ok(None);
        };
        let Some(component_ref) = self
            .dae_variables
            .and_then(|variables| dae_variable(variables, &VarName::new(scalar.base)))
            .and_then(|variable| variable.component_ref.as_ref())
            .filter(|component_ref| component_ref.def_id.is_some())
        else {
            return Ok(None);
        };
        let mut component_ref = component_ref.clone();
        let Some(last) = component_ref.parts.last_mut() else {
            return Ok(None);
        };
        for index in scalar.indices {
            let subscript = rumoca_core::Subscript::try_generated_index(
                index,
                span,
                "scalarized source reference",
            )
            .map_err(|err| LowerError::contract_violation(err.to_string(), span))?;
            last.subs.push(subscript);
        }
        Ok(Some(component_ref))
    }

    fn lower_expr(
        &mut self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let context_span = expr.span().filter(|span| !span.is_dummy());
        self.with_optional_source_context(context_span, |this| {
            this.lower_expr_inner(expr, scope, call_depth)
        })
    }

    fn lower_expr_inner(
        &mut self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let result = match expr {
            rumoca_core::Expression::Literal { value: lit, span } => {
                self.emit_const_at(eval_literal(lit)?, *span)
            }
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
            } => self.lower_var_ref(name, subscripts, *span, scope, call_depth),
            rumoca_core::Expression::Binary { op, lhs, rhs, span } => {
                if matches!(op, rumoca_core::OpBinary::Mul) {
                    self.lower_multiplication_expr(lhs, rhs, *span, scope, call_depth)
                        .map_err(|err| err.with_fallback_span(*span))
                } else {
                    let l = self
                        .lower_expr(lhs, scope, call_depth)
                        .map_err(|err| err.with_fallback_span(*span))?;
                    let r = self
                        .lower_expr(rhs, scope, call_depth)
                        .map_err(|err| err.with_fallback_span(*span))?;
                    self.lower_binary(op.clone(), l, r, *span)
                        .map_err(|err| err.with_fallback_span(*span))
                }
            }
            rumoca_core::Expression::Unary { op, rhs, span } => {
                let r = self
                    .lower_expr(rhs, scope, call_depth)
                    .map_err(|err| err.with_fallback_span(*span))?;
                self.lower_unary(op.clone(), r, *span)
            }
            rumoca_core::Expression::BuiltinCall {
                function,
                args,
                span,
            } => self.lower_builtin(*function, args, *span, scope, call_depth),
            rumoca_core::Expression::If {
                branches,
                else_branch,
                span,
            } => self.with_optional_source_context(Self::non_dummy_span(*span), |this| {
                this.lower_if(branches, else_branch, scope, call_depth)
            }),
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor,
                span,
            } => self.lower_function_call(name, args, *is_constructor, *span, scope, call_depth),
            rumoca_core::Expression::FieldAccess { base, field, span } => {
                self.lower_field_access(base, field, *span, scope, call_depth)
            }
            rumoca_core::Expression::Index {
                base,
                subscripts,
                span,
            } => self.lower_index(
                base,
                subscripts,
                self.span_or_source_context(*span),
                scope,
                call_depth,
            ),
            rumoca_core::Expression::Empty { .. } => Err(LowerError::Unsupported {
                reason: "empty expression has no scalar value".to_string(),
            }),
            rumoca_core::Expression::Array { elements, .. } => match elements.as_slice() {
                [single] => self.lower_expr(single, scope, call_depth),
                _ => Err(LowerError::Unsupported {
                    reason: format!(
                        "array expression with {} elements in scalar position",
                        elements.len()
                    ),
                }),
            },
            rumoca_core::Expression::Tuple { elements, .. } => match elements.as_slice() {
                [single] => self.lower_expr(single, scope, call_depth),
                _ => Err(LowerError::Unsupported {
                    reason: format!(
                        "tuple expression with {} elements in scalar position",
                        elements.len()
                    ),
                }),
            },
            rumoca_core::Expression::Range { .. } => Err(LowerError::Unsupported {
                reason: "range expression in scalar position".to_string(),
            }),
            rumoca_core::Expression::ArrayComprehension { .. } => Err(LowerError::Unsupported {
                reason: "array comprehension in scalar position".to_string(),
            }),
        };
        if let Some(span) = expr.span() {
            result.map_err(|err| err.with_fallback_span(span))
        } else {
            result
        }
    }

    #[allow(clippy::too_many_lines)]
    fn lower_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if subscripts.is_empty()
            && let Some(reg) = scope.get(&generated_scope_key(name.as_str())).copied()
        {
            return Ok(reg);
        }

        if subscripts.is_empty()
            && let Some(slot) = self.pre_mode_slot_for_key(name.as_str())
        {
            return self.emit_slot_load(slot, span);
        }

        if subscripts.is_empty()
            && let Some(slot) = self.layout.binding(name.as_str())
        {
            return self.emit_slot_load(slot, span);
        }

        if let Some(slot) = self.non_variable_layout_slot(name, subscripts) {
            return self.emit_slot_load(slot, span);
        }

        if subscripts.is_empty()
            && let Some(reference) = self.singleton_record_array_field_reference(name)
        {
            return self.lower_var_ref(&reference, &[], span, scope, call_depth);
        }

        if subscripts.is_empty()
            && let Some(reg) =
                self.lower_var_ref_binding_key(name.as_str(), span, scope, call_depth)?
        {
            return Ok(reg);
        }

        if subscripts.is_empty() {
            let real_field_key = format!("{}.re", name.as_str());
            if self.scalarized_field_binding_available(name.as_str(), "re")
                && let Some(reg) =
                    self.lower_var_ref_binding_key(&real_field_key, span, scope, call_depth)?
            {
                return Ok(reg);
            }
        }

        let name_key = self.scope_key_from_reference(name, span)?;
        if subscripts.is_empty()
            && let Some(reg) = scope.get(&name_key).copied()
        {
            return Ok(reg);
        }

        if let Some(indices) =
            self.singleton_shape_subscript_indices(name.as_str(), subscripts, span)?
        {
            let key = format_subscript_binding_key(name.as_str(), &indices);
            let owner_span = reference_context_span(name, span);
            if let Some(reg) =
                self.lower_var_ref_binding_key(&key, owner_span, scope, call_depth)?
            {
                return Ok(reg);
            }
        }

        let owner_span = reference_context_span(name, span);
        if let Some(reg) =
            self.generated_local_static_subscript_reg(name, subscripts, owner_span, scope)?
        {
            return Ok(reg);
        }

        if !subscripts.is_empty()
            && scope.contains_key(&name_key)
            && !self.local_indexed_bindings.contains_key(name.as_str())
        {
            if let Some(dims) = self.local_binding_dims.get(name.as_str())
                && dims.iter().any(|dim| *dim < 0)
            {
                return Err(unsupported_at(
                    format!(
                        "subscripted local array `{}` has negative dimensions {}",
                        name.as_str(),
                        format_i64_dims(dims)
                    ),
                    owner_span,
                ));
            }
            return Err(LowerError::Unsupported {
                reason: format!(
                    "subscripted local variable references are unsupported: {}[...]",
                    name.as_str()
                ),
            });
        }

        let base_name = name.as_str().to_string();
        if let Some(indices) = static_subscript_indices_with_owner(subscripts, owner_span)? {
            let key = if indices.is_empty() {
                base_name.clone()
            } else if indices.len() == 1 {
                format!("{base_name}[{}]", indices[0])
            } else {
                let suffix = indices
                    .iter()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",");
                format!("{base_name}[{suffix}]")
            };
            if let Some(reg) =
                self.lower_var_ref_binding_key(&key, owner_span, scope, call_depth)?
            {
                return Ok(reg);
            }
        }

        if let Some(indices) = self.compile_time_subscript_indices(subscripts, span)? {
            let key = if indices.is_empty() {
                base_name.clone()
            } else {
                format_subscript_binding_key(base_name.as_str(), &indices)
            };
            let owner_span = reference_context_span(name, span);
            if let Some(reg) =
                self.lower_var_ref_binding_key(&key, owner_span, scope, call_depth)?
            {
                return Ok(reg);
            }
        }

        let target = DynamicBindingTarget::source_reference(name, span)?;
        self.lower_dynamic_subscripted_binding(
            target,
            subscripts,
            scope,
            call_depth,
            DynamicSubscriptSemantics::VarRef,
        )
    }

    /// Resolves a scalar binding key through the pre-mode slot, direct
    /// assignment value, and layout binding lookups, in that order.
    pub(in crate::lower) fn lower_var_ref_binding_key(
        &mut self,
        key: &str,
        owner_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        if let Some(reg) = scope.get(&generated_scope_key(key)).copied() {
            return Ok(Some(reg));
        }
        if let Some(slot) = self.pre_mode_slot_for_key(key) {
            return self.emit_slot_load(slot, owner_span).map(Some);
        }
        if let Some(values) = self.lower_direct_assignment_values_for_key(key, scope, call_depth)?
            && let Some(value) = values.first().copied()
        {
            return Ok(Some(value));
        }
        if let Some(slot) = self.layout.binding(key) {
            return self.emit_slot_load(slot, owner_span).map(Some);
        }
        Ok(None)
    }

    fn non_variable_layout_slot(
        &self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) -> Option<ScalarSlot> {
        if !subscripts.is_empty() {
            return None;
        }
        if self
            .dae_variables
            .and_then(|variables| dae_variable(variables, name.var_name()))
            .is_some()
        {
            return None;
        }
        self.layout.binding(name.as_str())
    }

    fn singleton_record_array_field_reference(
        &self,
        name: &rumoca_core::Reference,
    ) -> Option<rumoca_core::Reference> {
        let component_ref = name.component_ref()?;
        if component_ref.def_id.is_some() || component_ref.parts.len() < 2 {
            return None;
        }
        let mut candidate_ref = component_ref.clone();
        let base_index = candidate_ref.parts.len().checked_sub(2)?;
        let span = candidate_ref.parts[base_index].span;
        let subscript =
            rumoca_core::Subscript::try_generated_index(1, span, "singleton record array field")
                .ok()?;
        candidate_ref.parts[base_index].subs.push(subscript);
        let candidate = candidate_ref.to_var_name().to_string();
        let variable = self.dae_variables.and_then(|variables| {
            dae_variable(variables, &rumoca_core::VarName::new(candidate.as_str()))
        })?;
        let component_ref = variable.component_ref.clone()?;
        Some(rumoca_core::Reference::with_component_reference(
            candidate.as_str(),
            component_ref,
        ))
    }

    fn generated_local_static_subscript_reg(
        &self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        owner_span: rumoca_core::Span,
        scope: &Scope,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(indices) = static_subscript_indices_with_owner(subscripts, owner_span)?
            .and_then(|indices| (!indices.is_empty()).then_some(indices))
        else {
            return Ok(None);
        };
        let key = format_subscript_binding_key(name.as_str(), &indices);
        Ok(scope.get(&generated_scope_key(&key)).copied())
    }

    fn singleton_shape_subscript_indices(
        &self,
        name: &str,
        subscripts: &[rumoca_core::Subscript],
        owner_span: rumoca_core::Span,
    ) -> Result<Option<Vec<usize>>, LowerError> {
        if subscripts.is_empty() {
            return Ok(None);
        }
        let Some(shape) = self.layout.shape(name) else {
            return Ok(None);
        };
        if shape.len() != subscripts.len() {
            return Ok(None);
        }

        let mut indices = crate::lower_vec_with_capacity(
            subscripts.len(),
            "singleton subscript index count",
            subscript_span_with_owner(subscripts, owner_span),
        )?;
        for (subscript, dim) in subscripts.iter().zip(shape.iter().copied()) {
            let index = match subscript {
                rumoca_core::Subscript::Index { value, span } if *value > 0 => {
                    positive_i64_index(*value, span_or_owner(*span, owner_span))?
                }
                rumoca_core::Subscript::Expr { expr, span } => {
                    match static_singleton_subscript_index(expr, span_or_owner(*span, owner_span))?
                    {
                        Some(value) => value,
                        None => return Ok(None),
                    }
                }
                rumoca_core::Subscript::Colon { .. } if dim == 1 => 1,
                rumoca_core::Subscript::Colon { .. } => return Ok(None),
                _ => {
                    return Err(LowerError::Unsupported {
                        reason: "non-positive subscript is unsupported".to_string(),
                    });
                }
            };
            if index == 0 || index > dim {
                return Err(LowerError::Unsupported {
                    reason: format!("subscript index {index} exceeds dimension {dim}"),
                });
            }
            indices.push(index);
        }
        Ok(Some(indices))
    }

    fn pre_mode_slot_for_key(&self, key: &str) -> Option<ScalarSlot> {
        if self.value_mode != ValueMode::Pre || key.starts_with("__pre__.") {
            return None;
        }
        self.layout.binding(format!("__pre__.{key}").as_str())
    }

    fn pre_mode_base_key(&self, base_key: &str) -> Option<String> {
        if self.value_mode != ValueMode::Pre || base_key.starts_with("__pre__.") {
            return None;
        }
        let pre_key = format!("__pre__.{base_key}");
        (self.layout.binding(pre_key.as_str()).is_some()
            || self
                .indexed_bindings
                .contains_key(&ComponentReferenceKey::generated(&pre_key)))
        .then_some(pre_key)
    }

    fn lower_direct_assignment_values_for_key(
        &mut self,
        key: &str,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let Some(assignment) = self.direct_assignments.get(key).cloned() else {
            return Ok(None);
        };
        if self
            .direct_assignment_stack
            .iter()
            .any(|active| active == key)
        {
            return Ok(None);
        }

        self.direct_assignment_stack.push(key.to_string());
        let lowered = self.lower_array_like_values(&assignment.rhs, scope, call_depth + 1);
        self.direct_assignment_stack.pop();

        let values = lowered?;
        if let Some(flat_index) = assignment.flat_index {
            let selected =
                direct_assignment_component(&values, flat_index, assignment.repeat_period)
                    .ok_or_else(|| LowerError::Unsupported {
                        reason: format!(
                            "direct assignment for `{key}` did not produce component {}",
                            flat_index + 1
                        ),
                    })?;
            return Ok(Some(vec![selected]));
        }
        Ok(Some(values))
    }

    #[allow(clippy::too_many_lines, clippy::excessive_nesting)]
    fn lower_index(
        &mut self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        owner_span: Option<rumoca_core::Span>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            ..
        } = base
            && is_stream_passthrough_intrinsic(name.as_str())
            && let Some(arg) = args.first()
        {
            return self.lower_index(arg, subscripts, owner_span, scope, call_depth);
        }
        if matches!(
            base,
            rumoca_core::Expression::FieldAccess { .. }
                | rumoca_core::Expression::FunctionCall { .. }
        ) && index_owner_span(base, subscripts, owner_span).is_none()
        {
            return Err(LowerError::UnspannedContractViolation {
                reason: "structural array scalar index lowering requires a source span".to_string(),
            });
        }
        if let Some(reg) =
            self.lower_structural_index_expr(base, subscripts, scope, call_depth, None)?
        {
            return Ok(reg);
        }
        if let Some(reg) =
            self.lower_compile_time_indexed_local_value(base, subscripts, owner_span, scope)?
        {
            return Ok(reg);
        }
        if dynamic_binding_base_key(base).is_err()
            && let Some(span) = index_owner_span(base, subscripts, owner_span)
            && let Some(indices) = static_subscript_indices_with_owner(subscripts, span)?
        {
            let dims = self.infer_expr_dims(base, scope)?;
            if let Some(flat_index) = flat_index_from_one_based_usize_indices(&dims, &indices) {
                let values = self
                    .lower_array_like_values_with_source_context(base, span, scope, call_depth)?;
                if let Some(value) = values.get(flat_index).copied() {
                    return Ok(value);
                }
            }
        }
        if matches!(base, rumoca_core::Expression::FieldAccess { .. })
            && let Some(span) = index_owner_span(base, subscripts, owner_span)
            && let Some(indices) = static_subscript_indices_with_owner(subscripts, span)?
        {
            let dims = self.infer_expr_dims(base, scope)?;
            if let Some(flat_index) = flat_index_from_one_based_usize_indices(&dims, &indices) {
                let mut dae_model = dae::Dae::default();
                dae_model.symbols.functions = self.functions.clone();
                if let Some(variables) = self.dae_variables {
                    dae_model.variables = variables.clone();
                }
                if let Some(values) = derivative_rhs::function_call_projected_scalars_with_owner(
                    base,
                    &dae_model,
                    &self.structural_bindings,
                    span,
                )? && let Some(value) = values.get(flat_index).cloned()
                {
                    return self.lower_expr(&value, scope, call_depth + 1);
                }
                if let Some(value) = derivative_rhs::project_array_like_scalar_with_owner(
                    base,
                    flat_index,
                    &dae_model,
                    &self.structural_bindings,
                    span,
                )? {
                    return self.lower_expr(&value, scope, call_depth + 1);
                }
                let values = self
                    .lower_array_like_values_with_source_context(base, span, scope, call_depth)?;
                if let Some(value) = values.get(flat_index).copied() {
                    return Ok(value);
                }
            }
        }
        if let Some(reg) =
            self.lower_array_like_dynamic_index(base, subscripts, owner_span, scope, call_depth)?
        {
            return Ok(reg);
        }

        if let Ok(key) = indexed_binding_key(base, subscripts)
            && let Some(reg) = scope.get(&generated_scope_key(&key)).copied()
        {
            return Ok(reg);
        }

        if let Ok(key) = indexed_binding_key(base, subscripts)
            && let Some(slot) = self.pre_mode_slot_for_key(&key)
        {
            return self.emit_slot_load(
                slot,
                required_expression_span(base, "indexed pre slot load")?,
            );
        }

        if let Ok(key) = indexed_binding_key(base, subscripts)
            && let Some(slot) = self.layout.binding(&key)
        {
            return self.emit_slot_load(slot, required_expression_span(base, "indexed slot load")?);
        }

        if is_static_singleton_scalar_projection(base, subscripts)? {
            return self.lower_expr(base, scope, call_depth);
        }

        if scalar_literal_projection(base, subscripts, owner_span)? {
            return self.lower_expr(base, scope, call_depth);
        }

        if dynamic_binding_base_key(base).is_err()
            && self
                .infer_expr_dims(base, scope)
                .unwrap_or_default()
                .is_empty()
            && index_owner_span(base, subscripts, owner_span).is_some_and(|span| {
                static_subscript_indices_with_owner(subscripts, span)
                    .is_ok_and(|indices| indices.is_some())
            })
        {
            return self.lower_expr(base, scope, call_depth);
        }

        if let Some(span) = index_owner_span(base, subscripts, owner_span)
            && let Some(indices) = static_subscript_indices_with_owner(subscripts, span)?
            && dynamic_binding_base_key(base).is_err()
        {
            let dims = self.infer_expr_dims(base, scope).unwrap_or_default();
            let flat_index =
                flat_index_from_one_based_usize_indices(&dims, &indices).or_else(|| {
                    (indices.len() == 1)
                        .then(|| indices.first().and_then(|index| index.checked_sub(1)))
                        .flatten()
                });
            if let Some(flat_index) = flat_index {
                let mut dae_model = dae::Dae::default();
                dae_model.symbols.functions = self.functions.clone();
                if let Some(variables) = self.dae_variables {
                    dae_model.variables = variables.clone();
                }
                if let Some(value) = derivative_rhs::project_array_like_scalar_with_owner(
                    base,
                    flat_index,
                    &dae_model,
                    &self.structural_bindings,
                    span,
                )? {
                    return self.lower_expr(&value, scope, call_depth + 1);
                }
            }
            let values =
                self.lower_array_like_values_with_source_context(base, span, scope, call_depth)?;
            if let Some(index) =
                flat_index_from_one_based_usize_indices(&dims, &indices).or_else(|| {
                    (indices.len() == 1)
                        .then(|| indices.first().and_then(|index| index.checked_sub(1)))
                        .flatten()
                })
            {
                if let Some(value) = values.get(index).copied() {
                    return Ok(value);
                }
            }
        }

        let base_key = match dynamic_binding_base_key(base) {
            Ok(base_key) => base_key,
            Err(err @ LowerError::DynamicBindingBase { .. }) => {
                return Err(err);
            }
            Err(err) => return Err(err),
        };
        let source_key = component_reference_key_for_expr(base)?;
        let source_span = base.span();
        self.lower_dynamic_subscripted_binding(
            DynamicBindingTarget::field(base_key, source_key, source_span),
            subscripts,
            scope,
            call_depth,
            DynamicSubscriptSemantics::Index,
        )
    }

    fn lower_dynamic_subscripted_binding(
        &mut self,
        target: DynamicBindingTarget,
        subscripts: &[rumoca_core::Subscript],
        scope: &Scope,
        call_depth: usize,
        semantics: DynamicSubscriptSemantics,
    ) -> Result<Reg, LowerError> {
        if subscripts.is_empty() {
            return Err(LowerError::MissingBinding {
                name: target.display_key,
            });
        }
        let fallback_span = subscript_fallback_span(subscripts).or(target.source_span);
        let emit_span = target.emit_span(fallback_span)?;
        if let Some(indices) = self.compile_time_subscript_indices(subscripts, emit_span)? {
            return self.lower_static_subscripted_binding(
                &target.display_key,
                &indices,
                subscript_span_with_owner(subscripts, emit_span),
                scope,
            );
        }
        let selector_span = subscript_span_with_owner(subscripts, emit_span);
        let mut selectors = crate::lower_vec_with_capacity(
            subscripts.len(),
            "dynamic subscript selector count",
            selector_span,
        )?;
        for subscript in subscripts {
            selectors.push(self.lower_structural_index_selector(
                subscript,
                selector_span,
                scope,
                call_depth,
            )?);
        }
        if let Some(reg) = self.try_lower_indexed_param_load(&target, &selectors, fallback_span)? {
            return Ok(reg);
        }
        let candidates = self.lower_dynamic_indexed_binding_candidates(
            &target,
            fallback_span,
            emit_span,
            scope,
            call_depth,
        )?;
        let mut matched = false;
        let mut merged = self.emit_const_at(0.0, emit_span)?;
        for (indices, value) in candidates {
            if indices.len() != selectors.len() {
                continue;
            }
            let cond = self.emit_subscript_match_at(&selectors, &indices, emit_span)?;
            merged = self.emit_select_at(cond, value, merged, emit_span)?;
            matched = true;
        }
        if matched {
            return Ok(merged);
        }
        Err(dynamic_subscript_unsupported(
            &target.display_key,
            subscripts,
            semantics,
        ))
    }

    fn lower_dynamic_indexed_binding_candidates(
        &mut self,
        target: &DynamicBindingTarget,
        fallback_span: Option<rumoca_core::Span>,
        emit_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<(Vec<usize>, Reg)>, LowerError> {
        let mut candidates = Vec::new();
        if let Some(local) = self.local_indexed_bindings.get(&target.display_key) {
            candidates.extend(local.iter().map(|entry| (entry.indices.clone(), entry.reg)));
        }

        let (entry_display_key, entries) = self.dynamic_layout_entries(target, fallback_span)?;
        for entry in sorted_flat_entries(&entries) {
            if entry.indices.is_empty() {
                candidates.push((
                    entry.indices.clone(),
                    self.emit_slot_load(entry.slot, emit_span)?,
                ));
                continue;
            }
            let scalar_key = format_subscript_binding_key(&entry_display_key, &entry.indices);
            if let Some(values) =
                self.lower_direct_assignment_values_for_key(&scalar_key, scope, call_depth)?
                && let Some(value) = values.first().copied()
            {
                candidates.push((entry.indices.clone(), value));
                continue;
            }
            candidates.push((
                entry.indices.clone(),
                self.emit_slot_load(entry.slot, emit_span)?,
            ));
        }
        candidates.sort_by(|(lhs, _), (rhs, _)| lhs.cmp(rhs));
        Ok(candidates)
    }

    /// Attempt to lower a dynamic parameter-array subscript to a single
    /// [`LinearOp::LoadIndexedP`]. Returns `Ok(None)` (so the caller falls back
    /// to the select chain) unless every element resolves to a contiguous,
    /// row-major run of constant parameter slots with no computed override.
    fn try_lower_indexed_param_load(
        &mut self,
        target: &DynamicBindingTarget,
        selectors: &[Reg],
        fallback_span: Option<rumoca_core::Span>,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(grid) = self.indexed_param_grid(target, selectors.len(), fallback_span) else {
            return Ok(None);
        };
        let Some((base, count, strides)) = contiguous_param_grid_layout(&grid) else {
            return Ok(None);
        };
        // Flat 0-based offset register: `Σ_d (selector_d - 1) * stride_d`. The
        // load op rounds and clamps the accumulated offset, so the selectors do
        // not need per-term rounding here.
        let Some(span) = fallback_span.or(target.source_span) else {
            return Ok(None);
        };
        let mut offset: Option<Reg> = None;
        for (d, &selector) in selectors.iter().enumerate() {
            let one = self.emit_const_at(1.0, span)?;
            let zero_based = self.emit_binary_at(BinaryOp::Sub, selector, one, span)?;
            let term = if strides[d] == 1 {
                zero_based
            } else {
                let stride = self.emit_const_at(strides[d] as f64, span)?;
                self.emit_binary_at(BinaryOp::Mul, zero_based, stride, span)?
            };
            offset = Some(match offset {
                None => term,
                Some(acc) => self.emit_binary_at(BinaryOp::Add, acc, term, span)?,
            });
        }
        let offset = offset.ok_or_else(|| {
            LowerError::contract_violation(
                "indexed parameter load requested without dynamic subscript selectors",
                span,
            )
        })?;
        Ok(Some(self.emit_load_indexed_p(base, count, offset, span)?))
    }

    /// The `(indices, parameter-slot)` grid for a dynamic subscript target, but
    /// only when every element is a pure constant-parameter slot with no
    /// locally-computed binding or direct-assignment override. Non-emitting:
    /// it reads layout metadata so the fast-path check stays side-effect free.
    fn indexed_param_grid(
        &self,
        target: &DynamicBindingTarget,
        ndim: usize,
        fallback_span: Option<rumoca_core::Span>,
    ) -> Option<Vec<(Vec<usize>, usize)>> {
        // Accumulate `indices -> parameter-slot` from both the function-argument
        // (local) and model-layout sources; either helper returning `None`
        // aborts the fast path.
        let mut grid: IndexMap<Vec<usize>, usize> = IndexMap::new();
        self.collect_local_param_slots(target, ndim, &mut grid)?;
        self.collect_layout_param_slots(target, ndim, fallback_span, &mut grid)?;
        if grid.is_empty() {
            return None;
        }
        let mut out: Vec<(Vec<usize>, usize)> = grid.into_iter().collect();
        out.sort_by(|a, b| a.0.cmp(&b.0));
        Some(out)
    }

    /// Add the function-argument array elements (local indexed bindings) to
    /// `grid`, recovering each element's parameter slot from builder-owned
    /// load provenance. `None` if any element is not a stored parameter slot.
    fn collect_local_param_slots(
        &self,
        target: &DynamicBindingTarget,
        ndim: usize,
        grid: &mut IndexMap<Vec<usize>, usize>,
    ) -> Option<()> {
        // No local bindings for this target: nothing to add, not a failure.
        let Some(locals) = self.local_indexed_bindings.get(&target.display_key) else {
            return Some(());
        };
        for entry in locals {
            if entry.indices.len() != ndim {
                return None;
            }
            let slot = self.param_slot_regs.get(&entry.reg).copied()?;
            if grid
                .insert(entry.indices.clone(), slot)
                .is_some_and(|prev| prev != slot)
            {
                return None;
            }
        }
        Some(())
    }

    /// Add model-level parameter-array slots from the solve layout to `grid`.
    /// `None` if any element has a direct-assignment override or is not a pure
    /// parameter slot. Layout-lookup errors leave `grid` unchanged (the
    /// select-chain fallback re-runs the same lookup authoritatively).
    fn collect_layout_param_slots(
        &self,
        target: &DynamicBindingTarget,
        ndim: usize,
        fallback_span: Option<rumoca_core::Span>,
        grid: &mut IndexMap<Vec<usize>, usize>,
    ) -> Option<()> {
        let Ok((entry_display_key, entries)) = self.dynamic_layout_entries(target, fallback_span)
        else {
            return Some(());
        };
        for entry in sorted_flat_entries(&entries) {
            if entry.indices.len() != ndim {
                return None;
            }
            // A direct assignment would shadow the stored parameter value.
            let scalar_key = format_subscript_binding_key(&entry_display_key, &entry.indices);
            if self.direct_assignments.contains_key(&scalar_key) {
                return None;
            }
            let ScalarSlot::P { index, .. } = entry.slot else {
                return None;
            };
            if grid
                .insert(entry.indices.clone(), index)
                .is_some_and(|prev| prev != index)
            {
                return None;
            }
        }
        Some(())
    }

    fn dynamic_layout_entries(
        &self,
        target: &DynamicBindingTarget,
        fallback_span: Option<rumoca_core::Span>,
    ) -> Result<(String, Vec<IndexedBinding>), LowerError> {
        if let Some(pre_key) = self.pre_mode_base_key(&target.display_key)
            && let Some(entries) = self
                .indexed_bindings
                .get(&ComponentReferenceKey::generated(&pre_key))
        {
            return Ok((pre_key, entries.clone()));
        }
        if let Some(source_key) = &target.source_key {
            let span = target.source_span.or(fallback_span).ok_or_else(|| {
                LowerError::UnspannedContractViolation {
                    reason: format!(
                        "indexed solve-layout lookup for `{}` requires source span metadata",
                        target.display_key
                    ),
                }
            })?;
            if self
                .local_indexed_bindings
                .contains_key(&target.display_key)
                && !self.indexed_bindings.contains_key(source_key)
            {
                return Ok((target.display_key.clone(), Vec::new()));
            }
            let entries = self.indexed_bindings.get(source_key).cloned().ok_or_else(|| {
                LowerError::contract_violation(
                    format!(
                        "indexed solve-layout lookup for `{}` resolved source metadata but no indexed binding group",
                        target.display_key
                    ),
                    span,
                )
            })?;
            return Ok((target.display_key.clone(), entries));
        }
        let generated_key = ComponentReferenceKey::generated(&target.display_key);
        if let Some(entries) = self.indexed_bindings.get(&generated_key) {
            #[cfg(test)]
            if self.test_fixture_generated_binding_available(&target.display_key, &generated_key) {
                return Ok((target.display_key.clone(), entries.clone()));
            }
            if !target.generated {
                let span = target
                    .source_span
                    .or(fallback_span)
                    .ok_or_else(|| LowerError::Unsupported {
                        reason: format!(
                            "indexed solve-layout lookup for `{}` lost structured component-reference metadata",
                            target.display_key
                        ),
                    })?;
                return Err(LowerError::contract_violation(
                    format!(
                        "indexed solve-layout lookup for `{}` lost structured component-reference metadata",
                        target.display_key
                    ),
                    span,
                ));
            }
            return Ok((target.display_key.clone(), entries.clone()));
        }
        Ok((target.display_key.clone(), Vec::new()))
    }

    fn lower_static_subscripted_binding(
        &mut self,
        base_key: &str,
        indices: &[usize],
        span: rumoca_core::Span,
        scope: &Scope,
    ) -> Result<Reg, LowerError> {
        let binding_base_key = self
            .pre_mode_base_key(base_key)
            .unwrap_or_else(|| base_key.to_string());
        let indexed_key = dae::format_subscript_key(&binding_base_key, indices);
        let indexed_scope_key = generated_scope_key(&indexed_key);
        if let Some(reg) = scope.get(&indexed_scope_key) {
            return Ok(*reg);
        }
        if let Some(reg) = self
            .local_indexed_bindings
            .get(base_key)
            .and_then(|entries| {
                entries
                    .iter()
                    .find(|entry| entry.indices == indices)
                    .map(|entry| entry.reg)
            })
        {
            return Ok(reg);
        }
        if let Some(slot) = self.pre_mode_slot_for_key(&indexed_key) {
            return self.emit_slot_load(slot, span);
        }
        if let Some(slot) = self.layout.binding(&indexed_key) {
            return self.emit_slot_load(slot, span);
        }
        Err(LowerError::MissingBinding { name: indexed_key })
    }

    fn emit_subscript_match_at(
        &mut self,
        lhs: &[Reg],
        rhs: &[usize],
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        debug_assert_eq!(lhs.len(), rhs.len());
        let mut cond = self.emit_const_at(1.0, span)?;
        for (reg, index) in lhs.iter().zip(rhs.iter()) {
            let rhs_const = self.emit_const_at(*index as f64, span)?;
            let eq = self.emit_compare_at(CompareOp::Eq, *reg, rhs_const, span)?;
            cond = self.emit_binary_at(BinaryOp::And, cond, eq, span)?;
        }
        Ok(cond)
    }

    fn emit_round_at(&mut self, arg: Reg, span: rumoca_core::Span) -> Result<Reg, LowerError> {
        let sign = self.emit_unary_at(UnaryOp::Sign, arg, span)?;
        let half = self.emit_const_at(0.5, span)?;
        let bias = self.emit_binary_at(BinaryOp::Mul, sign, half, span)?;
        let shifted = self.emit_binary_at(BinaryOp::Add, arg, bias, span)?;
        self.emit_unary_at(UnaryOp::Trunc, shifted, span)
    }

    #[allow(clippy::too_many_lines, clippy::excessive_nesting)]
    fn lower_field_access(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        field_access_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if matches!(field, "re" | "im")
            && let rumoca_core::Expression::FunctionCall {
                name, args, span, ..
            } = base
        {
            let projected_name = format!("{}.{}", name.as_str(), field);
            if let Some(reg) = self.lower_complex_math_sum_projection(
                &projected_name,
                args,
                *span,
                scope,
                call_depth,
            )? {
                return Ok(reg);
            }
        }

        if let rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem,
            lhs,
            rhs,
            ..
        } = base
            && matches!(field, "re" | "im")
            && let Some(reg) =
                self.lower_complex_vector_dot_field(lhs, rhs, field, field_access_span, scope)?
        {
            return Ok(reg);
        }

        if matches!(field, "re" | "im")
            && let Some(reg) =
                self.lower_complex_operator_field_access(base, field, scope, call_depth)?
        {
            return Ok(reg);
        }

        if let rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } = base
        {
            return self.lower_if_field_access(
                branches,
                else_branch,
                field,
                field_access_span,
                scope,
                call_depth,
            );
        }

        if let rumoca_core::Expression::Index {
            base: indexed_base,
            subscripts,
            span,
        } = base
            && let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = indexed_base.as_ref()
            && matches!(op, rumoca_core::OpBinary::Add | rumoca_core::OpBinary::Sub)
        {
            let lhs = field_access_expr_with_owner(
                &rumoca_core::Expression::Index {
                    base: lhs.clone(),
                    subscripts: subscripts.clone(),
                    span: *span,
                },
                field,
                field_access_span,
            );
            let rhs = field_access_expr_with_owner(
                &rumoca_core::Expression::Index {
                    base: rhs.clone(),
                    subscripts: subscripts.clone(),
                    span: *span,
                },
                field,
                field_access_span,
            );
            let lhs_reg = self.lower_expr(&lhs, scope, call_depth)?;
            let rhs_reg = self.lower_expr(&rhs, scope, call_depth)?;
            return self.lower_binary(op.clone(), lhs_reg, rhs_reg, field_access_span);
        }

        if let rumoca_core::Expression::Index {
            base, subscripts, ..
        } = base
            && let Some(reg) =
                self.lower_structural_index_expr(base, subscripts, scope, call_depth, Some(field))?
        {
            return Ok(reg);
        }

        if let rumoca_core::Expression::FieldAccess {
            base: nested_base,
            field: nested_field,
            ..
        } = base
            && matches!(nested_base.as_ref(), rumoca_core::Expression::Index { .. })
        {
            let nested_path = format!("{nested_field}.{field}");
            if let Some(reg) =
                self.lower_indexed_field_access(nested_base, &nested_path, scope, call_depth)?
            {
                return Ok(reg);
            }
        }

        if let Some(reg) = self.lower_indexed_field_access(base, field, scope, call_depth)? {
            return Ok(reg);
        }

        if let Some(reg) = self.lower_constructor_field_access(base, field, scope, call_depth)? {
            return Ok(reg);
        }

        if let rumoca_core::Expression::FunctionCall { .. } = base {
            let expr = rumoca_core::Expression::FieldAccess {
                base: Box::new(base.clone()),
                field: field.to_string(),
                span: field_access_span,
            };
            let mut dae_model = dae::Dae::default();
            dae_model.symbols.functions = self.functions.clone();
            if let Some(variables) = self.dae_variables {
                dae_model.variables = variables.clone();
            }
            if let Some(mut values) = derivative_rhs::function_call_projected_scalars_with_owner(
                &expr,
                &dae_model,
                &self.structural_bindings,
                field_access_span,
            )? && values.len() == 1
            {
                let value = values.remove(0);
                if value != expr {
                    return self.lower_expr(&value, scope, call_depth + 1);
                }
            }
        }

        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            span,
        } = base
            && let Some(materialized) = self.materialize_single_record_function_call_components(
                name, args, *span, scope, call_depth,
            )?
            && let Some(component) = materialized
                .components
                .into_iter()
                .find(|component| component.suffix == field)
        {
            return Ok(component.reg);
        }

        if let Some(reg) =
            self.lower_function_output_name_field_access(base, field, scope, call_depth)?
        {
            return Ok(reg);
        }

        if let Some(mut values) = self.lower_indexed_record_field_values(
            base,
            field,
            Self::non_dummy_span(field_access_span),
            scope,
        )? {
            if values.len() == 1 {
                return Ok(values.remove(0));
            }
            return Err(LowerError::Unsupported {
                reason: format!(
                    "field `{field}` projection selected {} scalarized record values where one was required",
                    values.len()
                ),
            });
        }

        if let Some(values) = self.lower_structural_field_values(base, field, scope, call_depth)? {
            if let Some(first) = values.into_iter().next() {
                return Ok(first);
            }
            return self.emit_const_at(
                0.0,
                required_expression_span(base, "empty structural field projection")?,
            );
        }

        let key = field_access_binding_key(base, field)?;
        let span = Self::non_dummy_span(field_access_span)
            .or_else(|| base.span().filter(|span| !span.is_dummy()))
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: format!("field access `{key}` requires source span metadata"),
            })?;
        if let Some(reg) = self.lower_var_ref_binding_key(&key, span, scope, call_depth)? {
            return Ok(reg);
        }
        if let Some(reg) =
            self.lower_singleton_record_array_field_binding(base, field, span, scope, call_depth)?
        {
            return Ok(reg);
        }
        Err(LowerError::MissingBinding { name: key })
    }

    fn lower_singleton_record_array_field_binding(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let Ok(base_key) = binding_base_key(base) else {
            return Ok(None);
        };
        let singleton_key = format!("{base_key}[1].{field}");
        if self.layout.binding(&singleton_key).is_none() {
            return Ok(None);
        }
        let higher_prefix = format!("{base_key}[");
        let higher_suffix = format!("].{field}");
        let has_higher_member = self.layout.bindings().keys().any(|key| {
            key.strip_prefix(higher_prefix.as_str())
                .and_then(|rest| rest.strip_suffix(higher_suffix.as_str()))
                .and_then(|index| index.parse::<usize>().ok())
                .is_some_and(|index| index > 1)
        });
        if has_higher_member {
            return Ok(None);
        }
        self.lower_var_ref_binding_key(&singleton_key, span, scope, call_depth)
    }

    fn lower_function_output_name_field_access(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            span,
        } = base
        else {
            return Ok(None);
        };
        let Some(function) = self.lookup_function(name) else {
            return Ok(None);
        };
        let [output] = function.outputs.as_slice() else {
            return Ok(None);
        };
        if output.name != field
            || !output.dims.is_empty()
            || output.type_class == Some(rumoca_core::ClassType::Record)
        {
            return Ok(None);
        }
        self.lower_function_call(name, args, false, *span, scope, call_depth)
            .map(Some)
    }

    fn lower_indexed_field_access(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let rumoca_core::Expression::Index {
            base, subscripts, ..
        } = base
        else {
            return Ok(None);
        };
        let base_key = binding_base_key(base)?;
        let field_key = format!("{base_key}.{field}");
        if let Some(indices) = self.compile_time_subscript_indices(
            subscripts,
            required_expression_span(base, "indexed field access")?,
        )? && !indices.is_empty()
        {
            let key = format_subscript_binding_key(&field_key, &indices);
            let record_element_field_key = format!(
                "{}.{}",
                format_subscript_binding_key(&base_key, &indices),
                field
            );
            if let Some(reg) = scope
                .get(&generated_scope_key(&record_element_field_key))
                .or_else(|| scope.get(&generated_scope_key(&key)))
                .copied()
            {
                return Ok(Some(reg));
            }
            if let Some(slot) = self
                .pre_mode_slot_for_key(&record_element_field_key)
                .or_else(|| self.pre_mode_slot_for_key(&key))
            {
                return self
                    .emit_slot_load(
                        slot,
                        required_expression_span(base, "indexed field pre slot load")?,
                    )
                    .map(Some);
            }
            let values = match self.lower_direct_assignment_values_for_key(
                &record_element_field_key,
                scope,
                call_depth,
            )? {
                Some(values) => Some(values),
                None => self.lower_direct_assignment_values_for_key(&key, scope, call_depth)?,
            };
            if let Some(values) = values
                && let Some(value) = values.first().copied()
            {
                return Ok(Some(value));
            }
            if let Some(slot) = self
                .layout
                .binding(&record_element_field_key)
                .or_else(|| self.layout.binding(&key))
            {
                return self
                    .emit_slot_load(
                        slot,
                        required_expression_span(base, "indexed field slot load")?,
                    )
                    .map(Some);
            }
        }

        let source_field_key = component_reference_key_for_field_base(base, field)?;
        let source_field_span = base.span();
        let field_key_generated = ComponentReferenceKey::generated(&field_key);
        if !source_field_key
            .as_ref()
            .is_some_and(|key| self.indexed_bindings.contains_key(key))
            && !self.indexed_bindings.contains_key(&field_key_generated)
            && !self.local_indexed_bindings.contains_key(field_key.as_str())
        {
            return Ok(None);
        }
        self.lower_dynamic_subscripted_binding(
            DynamicBindingTarget::field(field_key, source_field_key, source_field_span),
            subscripts,
            scope,
            call_depth,
            DynamicSubscriptSemantics::Index,
        )
        .map(Some)
    }

    fn lower_complex_vector_dot_field(
        &mut self,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        field: &str,
        span: rumoca_core::Span,
        scope: &Scope,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(lhs_re) = self.lower_complex_field_array_values(lhs, "re", span, scope)? else {
            return Ok(None);
        };
        let Some(lhs_im) = self.lower_complex_field_array_values(lhs, "im", span, scope)? else {
            return Ok(None);
        };
        let Some(rhs_re) = self.lower_complex_field_array_values(rhs, "re", span, scope)? else {
            return Ok(None);
        };
        let Some(rhs_im) = self.lower_complex_field_array_values(rhs, "im", span, scope)? else {
            return Ok(None);
        };
        let len = [lhs_re.len(), lhs_im.len(), rhs_re.len(), rhs_im.len()]
            .into_iter()
            .max()
            .unwrap_or(0);
        if len == 0
            || ![lhs_re.len(), lhs_im.len(), rhs_re.len(), rhs_im.len()]
                .into_iter()
                .all(|size| size == 1 || size == len)
        {
            return Ok(None);
        }
        let mut acc = self.emit_const_at(0.0, span)?;
        for idx in 0..len {
            let ar = lhs_re[if lhs_re.len() == 1 { 0 } else { idx }];
            let ai = lhs_im[if lhs_im.len() == 1 { 0 } else { idx }];
            let br = rhs_re[if rhs_re.len() == 1 { 0 } else { idx }];
            let bi = rhs_im[if rhs_im.len() == 1 { 0 } else { idx }];
            let term = if field == "re" {
                let arbr = self.emit_binary_at(BinaryOp::Mul, ar, br, span)?;
                let aibi = self.emit_binary_at(BinaryOp::Mul, ai, bi, span)?;
                self.emit_binary_at(BinaryOp::Sub, arbr, aibi, span)?
            } else {
                let arbi = self.emit_binary_at(BinaryOp::Mul, ar, bi, span)?;
                let aibr = self.emit_binary_at(BinaryOp::Mul, ai, br, span)?;
                self.emit_binary_at(BinaryOp::Add, arbi, aibr, span)?
            };
            acc = self.emit_binary_at(BinaryOp::Add, acc, term, span)?;
        }
        Ok(Some(acc))
    }

    fn lower_complex_field_array_values(
        &mut self,
        expr: &rumoca_core::Expression,
        field: &str,
        span: rumoca_core::Span,
        scope: &Scope,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        if let rumoca_core::Expression::VarRef { name, .. } = expr {
            let key = format!("{}.{}", name.as_str(), field);
            if let Some(values) = self.lower_record_field_array_values(&key, span)? {
                return Ok(Some(values));
            }
        }
        let projected = field_access_expr_with_owner(expr, field, span);
        match self.lower_array_like_values(&projected, scope, 0) {
            Ok(values) if !values.is_empty() => Ok(Some(values)),
            Ok(_) => Ok(None),
            Err(_) => Ok(None),
        }
    }

    fn lower_if_field_access(
        &mut self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
        field: &str,
        field_access_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let projected_branches = branches
            .iter()
            .map(|(cond, value)| {
                (
                    cond.clone(),
                    field_access_expr_with_owner(value, field, field_access_span),
                )
            })
            .collect::<Vec<_>>();
        let projected_else = field_access_expr_with_owner(else_branch, field, field_access_span);
        self.with_optional_source_context(Self::non_dummy_span(field_access_span), |this| {
            this.lower_if(&projected_branches, &projected_else, scope, call_depth)
        })
    }

    fn lower_complex_operator_field_access(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let (re, im) = match base {
            // MLS operator overloading for Complex numbers is flattened into
            // ordinary expression trees. Projected `re/im` access must recover
            // the selected component from the complex arithmetic result.
            rumoca_core::Expression::Binary { op, lhs, rhs, span } => {
                let (lhs_re, lhs_im) =
                    self.lower_complex_operand_parts(lhs, *span, scope, call_depth)?;
                let (rhs_re, rhs_im) =
                    self.lower_complex_operand_parts(rhs, *span, scope, call_depth)?;
                let op = match op {
                    rumoca_core::OpBinary::Add => BinaryOp::Add,
                    rumoca_core::OpBinary::Sub => BinaryOp::Sub,
                    rumoca_core::OpBinary::Mul => BinaryOp::Mul,
                    rumoca_core::OpBinary::Div => BinaryOp::Div,
                    _ => return Ok(None),
                };
                self.lower_complex_binary_parts(op, lhs_re, lhs_im, rhs_re, rhs_im, *span)?
            }
            rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs,
                span,
            } => {
                let (rhs_re, rhs_im) =
                    self.lower_complex_operand_parts(rhs, *span, scope, call_depth)?;
                (
                    self.emit_unary_at(UnaryOp::Neg, rhs_re, *span)?,
                    self.emit_unary_at(UnaryOp::Neg, rhs_im, *span)?,
                )
            }
            rumoca_core::Expression::FunctionCall {
                name, args, span, ..
            } => {
                let Some(op) = complex_operator_call_op(name.as_str()) else {
                    return Ok(None);
                };
                let lhs = args.first().ok_or_else(|| {
                    LowerError::contract_violation(
                        format!("{} requires lhs for complex operator call", name.as_str()),
                        *span,
                    )
                })?;
                let rhs = args.get(1).ok_or_else(|| {
                    LowerError::contract_violation(
                        format!("{} requires rhs for complex operator call", name.as_str()),
                        *span,
                    )
                })?;
                let (lhs_re, lhs_im) =
                    self.lower_complex_operand_parts(lhs, *span, scope, call_depth)?;
                let (rhs_re, rhs_im) =
                    self.lower_complex_operand_parts(rhs, *span, scope, call_depth)?;
                self.lower_complex_binary_parts(op, lhs_re, lhs_im, rhs_re, rhs_im, *span)?
            }
            _ => return Ok(None),
        };
        Ok(Some(if field == "re" { re } else { im }))
    }

    fn lower_complex_binary_parts(
        &mut self,
        op: BinaryOp,
        lhs_re: Reg,
        lhs_im: Reg,
        rhs_re: Reg,
        rhs_im: Reg,
        span: rumoca_core::Span,
    ) -> Result<(Reg, Reg), LowerError> {
        match op {
            BinaryOp::Add => Ok((
                self.emit_binary_at(BinaryOp::Add, lhs_re, rhs_re, span)?,
                self.emit_binary_at(BinaryOp::Add, lhs_im, rhs_im, span)?,
            )),
            BinaryOp::Sub => Ok((
                self.emit_binary_at(BinaryOp::Sub, lhs_re, rhs_re, span)?,
                self.emit_binary_at(BinaryOp::Sub, lhs_im, rhs_im, span)?,
            )),
            BinaryOp::Mul => {
                let ac = self.emit_binary_at(BinaryOp::Mul, lhs_re, rhs_re, span)?;
                let bd = self.emit_binary_at(BinaryOp::Mul, lhs_im, rhs_im, span)?;
                let ad = self.emit_binary_at(BinaryOp::Mul, lhs_re, rhs_im, span)?;
                let bc = self.emit_binary_at(BinaryOp::Mul, lhs_im, rhs_re, span)?;
                Ok((
                    self.emit_binary_at(BinaryOp::Sub, ac, bd, span)?,
                    self.emit_binary_at(BinaryOp::Add, ad, bc, span)?,
                ))
            }
            BinaryOp::Div => {
                let rr2 = self.emit_binary_at(BinaryOp::Mul, rhs_re, rhs_re, span)?;
                let ri2 = self.emit_binary_at(BinaryOp::Mul, rhs_im, rhs_im, span)?;
                let denom = self.emit_binary_at(BinaryOp::Add, rr2, ri2, span)?;
                let lhs_rr = self.emit_binary_at(BinaryOp::Mul, lhs_re, rhs_re, span)?;
                let lhs_ri = self.emit_binary_at(BinaryOp::Mul, lhs_re, rhs_im, span)?;
                let li_rr = self.emit_binary_at(BinaryOp::Mul, lhs_im, rhs_re, span)?;
                let li_ri = self.emit_binary_at(BinaryOp::Mul, lhs_im, rhs_im, span)?;
                let re_num = self.emit_binary_at(BinaryOp::Add, lhs_rr, li_ri, span)?;
                let im_num = self.emit_binary_at(BinaryOp::Sub, li_rr, lhs_ri, span)?;
                Ok((
                    self.emit_binary_at(BinaryOp::Div, re_num, denom, span)?,
                    self.emit_binary_at(BinaryOp::Div, im_num, denom, span)?,
                ))
            }
            _ => Err(LowerError::contract_violation(
                format!(
                    "complex operator call mapped to unsupported binary op {}",
                    op.kind_name()
                ),
                span,
            )),
        }
    }
}
fn subscript_span(subscripts: &[rumoca_core::Subscript]) -> Option<rumoca_core::Span> {
    subscripts
        .iter()
        .map(rumoca_core::Subscript::span)
        .find(|span| !span.is_dummy())
}

fn subscript_span_with_owner(
    subscripts: &[rumoca_core::Subscript],
    owner_span: rumoca_core::Span,
) -> rumoca_core::Span {
    subscript_span(subscripts).unwrap_or(owner_span)
}

fn index_owner_span(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    owner_span: Option<rumoca_core::Span>,
) -> Option<rumoca_core::Span> {
    subscripts
        .iter()
        .find_map(subscript_source_provenance)
        .or_else(|| base.span().filter(|span| !span.is_dummy()))
        .or_else(|| owner_span.filter(|span| !span.is_dummy()))
}

fn flat_index_from_one_based_usize_indices(dims: &[usize], indices: &[usize]) -> Option<usize> {
    if dims.len() != indices.len() || dims.is_empty() {
        return None;
    }
    let mut flat = 0usize;
    for (axis, index) in indices.iter().copied().enumerate() {
        let dim = dims[axis];
        if index == 0 || index > dim {
            return None;
        }
        let stride = dims[axis + 1..].iter().product::<usize>();
        flat = flat.checked_add((index - 1).checked_mul(stride)?)?;
    }
    Some(flat)
}

fn subscript_source_provenance(subscript: &rumoca_core::Subscript) -> Option<rumoca_core::Span> {
    let span = subscript.span();
    if !span.is_dummy() {
        return Some(span);
    }
    let rumoca_core::Subscript::Expr { expr, .. } = subscript else {
        return None;
    };
    expr.span()
}

fn required_expression_span(
    expr: &rumoca_core::Expression,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    Ok(expr.require_span(context)?.span())
}
