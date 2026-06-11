//! Lower flat expressions and DAE residual rows to linear ops.
//!
//! SPEC_0021 file-size exception: this facade coordinates solve lowering
//! submodules and still owns shared lowering dispatch. split plan: continue
//! moving projection, branch, and row-family logic into focused submodules.

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
mod root_conditions;
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
    let result = builder.lower_expr(expr, &scope, 0)?;
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
    let structural_bindings = compile_time::structural_bindings(dae_model);
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
                structural_bindings: Some(&structural_bindings),
                guard_target_start_before_first_clock_tick: true,
            },
            false,
        )?,
    )
    .programs)
}

pub(crate) fn lower_runtime_assignment_rhs(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    equations: &[dae::Equation],
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let structural_bindings = compile_time::structural_bindings(dae_model);
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
                structural_bindings: Some(&structural_bindings),
                guard_target_start_before_first_clock_tick: true,
            },
            false,
        )?,
    )
    .programs)
}

pub(crate) fn lower_dynamic_time_event_rhs(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    expressions: &[rumoca_core::Expression],
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let structural_bindings = compile_time::structural_bindings(dae_model);
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
            structural_bindings: Some(&structural_bindings),
            guard_target_start_before_first_clock_tick: false,
        },
    )
}

pub fn lower_observation_rhs(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    expressions: &[rumoca_core::Expression],
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let structural_bindings = compile_time::structural_bindings(dae_model);
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
            structural_bindings: Some(&structural_bindings),
            guard_target_start_before_first_clock_tick: false,
        },
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
    structural_bindings: IndexMap<String, f64>,
    direct_assignments: IndexMap<String, DirectAssignmentValue>,
    direct_assignment_stack: Vec<String>,
    indexed_bindings: IndexedBindingMap,
    local_indexed_bindings: IndexMap<String, Vec<LocalIndexedBinding>>,
    local_binding_dims: IndexMap<String, Vec<i64>>,
    known_empty_local_arrays: IndexSet<String>,
    local_const_bindings: IndexMap<String, f64>,
    function_closures: IndexMap<ComponentReferenceKey, FunctionClosure>,
    is_initial_mode: bool,
    value_mode: ValueMode,
    current_update_target: Option<ScalarSlot>,
    ops: Vec<LinearOp>,
    next_reg: Reg,
    call_site_namespace: u64,
    next_call_site: u64,
    cse: RowCse,
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
                dae_variables: None,
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
            structural_bindings: IndexMap::new(),
            direct_assignments: IndexMap::new(),
            direct_assignment_stack: Vec::new(),
            indexed_bindings: metadata
                .indexed_bindings
                .cloned()
                .unwrap_or_else(|| Arc::new(build_indexed_binding_map(layout))),
            local_indexed_bindings: IndexMap::new(),
            local_binding_dims: IndexMap::new(),
            known_empty_local_arrays: IndexSet::new(),
            local_const_bindings: IndexMap::new(),
            function_closures: IndexMap::new(),
            is_initial_mode: metadata.is_initial_mode,
            value_mode: ValueMode::Current,
            current_update_target: None,
            ops: Vec::new(),
            next_reg: 0,
            call_site_namespace: 0,
            next_call_site: 0,
            cse: RowCse::default(),
        }
    }

    fn with_direct_assignments(
        mut self,
        direct_assignments: IndexMap<String, DirectAssignmentValue>,
    ) -> Self {
        self.direct_assignments = direct_assignments;
        self
    }

    fn with_structural_bindings(mut self, structural_bindings: IndexMap<String, f64>) -> Self {
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
            direct_assignments: IndexMap::new(),
            direct_assignment_stack: Vec::new(),
            indexed_bindings: Arc::clone(&self.indexed_bindings),
            local_indexed_bindings: IndexMap::new(),
            local_binding_dims: IndexMap::new(),
            known_empty_local_arrays: IndexSet::new(),
            local_const_bindings: self.local_const_bindings.clone(),
            function_closures: self.function_closures.clone(),
            is_initial_mode: self.is_initial_mode,
            value_mode: self.value_mode,
            current_update_target: self.current_update_target,
            ops: Vec::new(),
            next_reg: start_reg,
            call_site_namespace: self.call_site_namespace,
            next_call_site: 0,
            cse: RowCse::default(),
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

    pub(in crate::lower) fn scope_key_from_reference(
        &self,
        name: &rumoca_core::Reference,
        span: rumoca_core::Span,
    ) -> Result<ComponentReferenceKey, LowerError> {
        if self.lookup_function_output_projection(name).is_some() {
            return Ok(ComponentReferenceKey::generated(name.as_str()));
        }
        if name.is_generated() || name.component_ref().is_some() || self.dae_variables.is_none() {
            return scope_key_from_reference(name, span);
        }
        let Some(variable) = self
            .dae_variables
            .and_then(|variables| dae_variable(variables, name.var_name()))
        else {
            #[cfg(test)]
            if let Some(generated) = self.test_fixture_generated_scope_key(name, span) {
                return Ok(generated);
            }
            return Err(LowerError::ContractViolation {
                reason: format!(
                    "Solve lowering synthesized source reference `{}` that is not a DAE variable",
                    name.as_str()
                ),
                span,
            });
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
                let component_ref =
                    variable
                        .component_ref
                        .as_ref()
                        .ok_or_else(|| LowerError::ContractViolation {
                            reason: format!(
                                "source DAE variable `{}` lost structured component-reference metadata before Solve lowering",
                                name.as_str()
                            ),
                            span,
                        })?;
                ComponentReferenceKey::from_component_reference(component_ref).map_err(|err| {
                    LowerError::ContractViolation {
                        reason: format!(
                            "Solve lowering requires static component-reference metadata for `{}`",
                            name.as_str()
                        ),
                        span: err.span,
                    }
                })
            }
        }
    }

    fn lower_expr(
        &mut self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let result = match expr {
            rumoca_core::Expression::Literal { value: lit, .. } => {
                Ok(self.emit_const(eval_literal(lit)))
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
                    self.lower_binary(op.clone(), l, r)
                        .map_err(|err| err.with_fallback_span(*span))
                }
            }
            rumoca_core::Expression::Unary { op, rhs, .. } => {
                let r = self.lower_expr(rhs, scope, call_depth)?;
                self.lower_unary(op.clone(), r)
            }
            rumoca_core::Expression::BuiltinCall {
                function,
                args,
                span,
            } => self.lower_builtin(*function, args, *span, scope, call_depth),
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => self.lower_if(branches, else_branch, scope, call_depth),
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor,
                span,
            } => self.lower_function_call(name, args, *is_constructor, *span, scope, call_depth),
            rumoca_core::Expression::FieldAccess { base, field, .. } => {
                self.lower_field_access(base, field, scope, call_depth)
            }
            rumoca_core::Expression::Index {
                base, subscripts, ..
            } => self.lower_index(base, subscripts, scope, call_depth),
            // SPEC_0008: aggregates in scalar position must fail loudly.
            // Lowering them to a constant 0.0 (or silently taking the first
            // element) turns upstream type errors into wrong simulations.
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
            return self.emit_slot_load(slot);
        }

        if let Some(slot) = self.non_variable_layout_slot(name, subscripts) {
            return self.emit_slot_load(slot);
        }

        let name_key = self.scope_key_from_reference(name, span)?;
        if subscripts.is_empty()
            && let Some(reg) = scope.get(&name_key).copied()
        {
            return Ok(reg);
        }

        if let Some(indices) = self.singleton_shape_subscript_indices(name.as_str(), subscripts)? {
            let key = format_subscript_binding_key(name.as_str(), &indices);
            if let Some(reg) = self.lower_var_ref_binding_key(&key, scope, call_depth)? {
                return Ok(reg);
            }
        }

        if let Some(reg) = self.generated_local_static_subscript_reg(name, subscripts, scope)? {
            return Ok(reg);
        }

        if !subscripts.is_empty()
            && scope.contains_key(&name_key)
            && !self.local_indexed_bindings.contains_key(name.as_str())
        {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "subscripted local variable references are unsupported: {}[...]",
                    name.as_str()
                ),
            });
        }

        let base_name = name.as_str().to_string();
        if let Some(indices) = static_subscript_indices(subscripts)? {
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
            if let Some(reg) = self.lower_var_ref_binding_key(&key, scope, call_depth)? {
                return Ok(reg);
            }
        }

        if let Some(indices) = self.compile_time_subscript_indices(subscripts)? {
            let key = if indices.is_empty() {
                base_name.clone()
            } else {
                format_subscript_binding_key(base_name.as_str(), &indices)
            };
            if let Some(reg) = self.lower_var_ref_binding_key(&key, scope, call_depth)? {
                return Ok(reg);
            }
        }

        let target = DynamicBindingTarget::source_reference(name)?;
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
    fn lower_var_ref_binding_key(
        &mut self,
        key: &str,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        if let Some(slot) = self.pre_mode_slot_for_key(key) {
            return self.emit_slot_load(slot).map(Some);
        }
        if let Some(values) = self.lower_direct_assignment_values_for_key(key, scope, call_depth)?
            && let Some(value) = values.first().copied()
        {
            return Ok(Some(value));
        }
        if let Some(slot) = self.layout.binding(key) {
            return self.emit_slot_load(slot).map(Some);
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

    fn generated_local_static_subscript_reg(
        &self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        scope: &Scope,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(indices) = static_subscript_indices(subscripts)?
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

        let mut indices = Vec::with_capacity(subscripts.len());
        for (subscript, dim) in subscripts.iter().zip(shape.iter().copied()) {
            let index = match subscript {
                rumoca_core::Subscript::Index { value, .. } if *value > 0 => *value as usize,
                rumoca_core::Subscript::Expr { expr, .. } => {
                    match static_singleton_subscript_index(expr)? {
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

    fn lower_index(
        &mut self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if let Some(reg) =
            self.lower_structural_index_expr(base, subscripts, scope, call_depth, None)?
        {
            return Ok(reg);
        }
        if let Some(reg) = self.lower_compile_time_indexed_local_value(base, subscripts, scope)? {
            return Ok(reg);
        }
        if let Some(reg) =
            self.lower_array_like_dynamic_index(base, subscripts, scope, call_depth)?
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
            return self.emit_slot_load(slot);
        }

        if let Ok(key) = indexed_binding_key(base, subscripts)
            && let Some(slot) = self.layout.binding(&key)
        {
            return self.emit_slot_load(slot);
        }

        if is_static_singleton_scalar_projection(base, subscripts)? {
            return self.lower_expr(base, scope, call_depth);
        }

        let base_key = dynamic_binding_base_key(base)?;
        let source_key = component_reference_key_for_expr(base)?;
        self.lower_dynamic_subscripted_binding(
            DynamicBindingTarget::field(base_key, source_key),
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
        if let Some(indices) = self.compile_time_subscript_indices(subscripts)? {
            return self.lower_static_subscripted_binding(&target.display_key, &indices, scope);
        }
        let selectors = subscripts
            .iter()
            .map(|subscript| self.lower_structural_index_selector(subscript, scope, call_depth))
            .collect::<Result<Vec<_>, _>>()?;
        let fallback_span = subscript_fallback_span(subscripts);
        let candidates = self.lower_dynamic_indexed_binding_candidates(
            &target,
            fallback_span,
            scope,
            call_depth,
        )?;
        let mut matched = false;
        let mut merged = self.emit_const(0.0);
        for (indices, value) in candidates {
            if indices.len() != selectors.len() {
                continue;
            }
            let cond = self.emit_subscript_match(&selectors, &indices);
            merged = self.emit_select(cond, value, merged);
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
                candidates.push((entry.indices.clone(), self.emit_slot_load(entry.slot)?));
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
            candidates.push((entry.indices.clone(), self.emit_slot_load(entry.slot)?));
        }
        candidates.sort_by(|(lhs, _), (rhs, _)| lhs.cmp(rhs));
        Ok(candidates)
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
            return Ok((
                target.display_key.clone(),
                self.indexed_bindings
                    .get(source_key)
                    .cloned()
                    .unwrap_or_default(),
            ));
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
                return Err(LowerError::ContractViolation {
                    reason: format!(
                        "indexed solve-layout lookup for `{}` lost structured component-reference metadata",
                        target.display_key
                    ),
                    span,
                });
            }
            return Ok((target.display_key.clone(), entries.clone()));
        }
        Ok((target.display_key.clone(), Vec::new()))
    }

    fn lower_static_subscripted_binding(
        &mut self,
        base_key: &str,
        indices: &[usize],
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
            return self.emit_slot_load(slot);
        }
        if let Some(slot) = self.layout.binding(&indexed_key) {
            return self.emit_slot_load(slot);
        }
        Err(LowerError::MissingBinding { name: indexed_key })
    }

    fn emit_subscript_match(&mut self, lhs: &[Reg], rhs: &[usize]) -> Reg {
        debug_assert_eq!(lhs.len(), rhs.len());
        let mut cond = self.emit_const(1.0);
        for (reg, index) in lhs.iter().zip(rhs.iter()) {
            let rhs_const = self.emit_const(*index as f64);
            let eq = self.emit_compare(CompareOp::Eq, *reg, rhs_const);
            cond = self.emit_binary(BinaryOp::And, cond, eq);
        }
        cond
    }

    fn emit_round(&mut self, arg: Reg) -> Reg {
        let sign = self.emit_unary(UnaryOp::Sign, arg);
        let half = self.emit_const(0.5);
        let bias = self.emit_binary(BinaryOp::Mul, sign, half);
        let shifted = self.emit_binary(BinaryOp::Add, arg, bias);
        self.emit_unary(UnaryOp::Trunc, shifted)
    }

    fn lower_field_access(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if matches!(field, "re" | "im")
            && let rumoca_core::Expression::FunctionCall { name, args, .. } = base
        {
            let projected_name = format!("{}.{}", name.as_str(), field);
            if let Some(reg) =
                self.lower_complex_math_sum_projection(&projected_name, args, scope, call_depth)?
            {
                return Ok(reg);
            }
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
            return self.lower_if_field_access(branches, else_branch, field, scope, call_depth);
        }

        if let rumoca_core::Expression::Index {
            base, subscripts, ..
        } = base
            && let Some(reg) =
                self.lower_structural_index_expr(base, subscripts, scope, call_depth, Some(field))?
        {
            return Ok(reg);
        }

        if let Some(reg) = self.lower_indexed_field_access(base, field, scope, call_depth)? {
            return Ok(reg);
        }

        if let Some(reg) = self.lower_constructor_field_access(base, field, scope, call_depth)? {
            return Ok(reg);
        }

        if let Some(reg) =
            self.lower_function_output_name_field_access(base, field, scope, call_depth)?
        {
            return Ok(reg);
        }

        if let Some(mut values) = self.lower_indexed_record_field_values(base, field, scope)? {
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
            return Ok(self.emit_const(0.0));
        }

        let key = field_access_binding_key(base, field)?;
        if let Some(reg) = scope.get(&generated_scope_key(&key)).copied() {
            return Ok(reg);
        }
        let slot = self
            .layout
            .binding(&key)
            .ok_or(LowerError::MissingBinding { name: key })?;
        self.emit_slot_load(slot)
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

        if let Some(indices) = static_subscript_indices(subscripts)?
            && !indices.is_empty()
        {
            let key = format_subscript_binding_key(&field_key, &indices);
            if let Some(reg) = scope.get(&generated_scope_key(&key)).copied() {
                return Ok(Some(reg));
            }
            if let Some(slot) = self.pre_mode_slot_for_key(&key) {
                return self.emit_slot_load(slot).map(Some);
            }
            if let Some(values) =
                self.lower_direct_assignment_values_for_key(&key, scope, call_depth)?
                && let Some(value) = values.first().copied()
            {
                return Ok(Some(value));
            }
            if let Some(slot) = self.layout.binding(&key) {
                return self.emit_slot_load(slot).map(Some);
            }
        }

        let source_field_key = component_reference_key_for_field_base(base, field)?;
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
            DynamicBindingTarget::field(field_key, source_field_key),
            subscripts,
            scope,
            call_depth,
            DynamicSubscriptSemantics::Index,
        )
        .map(Some)
    }

    fn lower_if_field_access(
        &mut self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
        field: &str,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let projected_branches = branches
            .iter()
            .map(|(cond, value)| (cond.clone(), field_access_expr(value, field)))
            .collect::<Vec<_>>();
        let projected_else = field_access_expr(else_branch, field);
        self.lower_if(&projected_branches, &projected_else, scope, call_depth)
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
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
                let (lhs_re, lhs_im) = self.lower_complex_operand_parts(lhs, scope, call_depth)?;
                let (rhs_re, rhs_im) = self.lower_complex_operand_parts(rhs, scope, call_depth)?;
                match op {
                    rumoca_core::OpBinary::Add => (
                        self.emit_binary(BinaryOp::Add, lhs_re, rhs_re),
                        self.emit_binary(BinaryOp::Add, lhs_im, rhs_im),
                    ),
                    rumoca_core::OpBinary::Sub => (
                        self.emit_binary(BinaryOp::Sub, lhs_re, rhs_re),
                        self.emit_binary(BinaryOp::Sub, lhs_im, rhs_im),
                    ),
                    rumoca_core::OpBinary::Mul => {
                        let ac = self.emit_binary(BinaryOp::Mul, lhs_re, rhs_re);
                        let bd = self.emit_binary(BinaryOp::Mul, lhs_im, rhs_im);
                        let ad = self.emit_binary(BinaryOp::Mul, lhs_re, rhs_im);
                        let bc = self.emit_binary(BinaryOp::Mul, lhs_im, rhs_re);
                        (
                            self.emit_binary(BinaryOp::Sub, ac, bd),
                            self.emit_binary(BinaryOp::Add, ad, bc),
                        )
                    }
                    rumoca_core::OpBinary::Div => {
                        let rr2 = self.emit_binary(BinaryOp::Mul, rhs_re, rhs_re);
                        let ri2 = self.emit_binary(BinaryOp::Mul, rhs_im, rhs_im);
                        let denom = self.emit_binary(BinaryOp::Add, rr2, ri2);
                        let lhs_rr = self.emit_binary(BinaryOp::Mul, lhs_re, rhs_re);
                        let lhs_ri = self.emit_binary(BinaryOp::Mul, lhs_re, rhs_im);
                        let li_rr = self.emit_binary(BinaryOp::Mul, lhs_im, rhs_re);
                        let li_ri = self.emit_binary(BinaryOp::Mul, lhs_im, rhs_im);
                        let re_num = self.emit_binary(BinaryOp::Add, lhs_rr, li_ri);
                        let im_num = self.emit_binary(BinaryOp::Sub, li_rr, lhs_ri);
                        (
                            self.emit_binary(BinaryOp::Div, re_num, denom),
                            self.emit_binary(BinaryOp::Div, im_num, denom),
                        )
                    }
                    _ => return Ok(None),
                }
            }
            rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs,
                ..
            } => {
                let (rhs_re, rhs_im) = self.lower_complex_operand_parts(rhs, scope, call_depth)?;
                (
                    self.emit_unary(UnaryOp::Neg, rhs_re),
                    self.emit_unary(UnaryOp::Neg, rhs_im),
                )
            }
            rumoca_core::Expression::FunctionCall { name, args, .. } => {
                let Some(op) = complex_operator_call_op(name.as_str()) else {
                    return Ok(None);
                };
                let lhs = args.first().ok_or_else(|| LowerError::InvalidFunction {
                    name: name.as_str().to_string(),
                    reason: "missing lhs for complex operator call".to_string(),
                })?;
                let rhs = args.get(1).ok_or_else(|| LowerError::InvalidFunction {
                    name: name.as_str().to_string(),
                    reason: "missing rhs for complex operator call".to_string(),
                })?;
                let (lhs_re, lhs_im) = self.lower_complex_operand_parts(lhs, scope, call_depth)?;
                let (rhs_re, rhs_im) = self.lower_complex_operand_parts(rhs, scope, call_depth)?;
                self.lower_complex_binary_parts(op, lhs_re, lhs_im, rhs_re, rhs_im)
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
    ) -> (Reg, Reg) {
        match op {
            BinaryOp::Add => (
                self.emit_binary(BinaryOp::Add, lhs_re, rhs_re),
                self.emit_binary(BinaryOp::Add, lhs_im, rhs_im),
            ),
            BinaryOp::Sub => (
                self.emit_binary(BinaryOp::Sub, lhs_re, rhs_re),
                self.emit_binary(BinaryOp::Sub, lhs_im, rhs_im),
            ),
            BinaryOp::Mul => {
                let ac = self.emit_binary(BinaryOp::Mul, lhs_re, rhs_re);
                let bd = self.emit_binary(BinaryOp::Mul, lhs_im, rhs_im);
                let ad = self.emit_binary(BinaryOp::Mul, lhs_re, rhs_im);
                let bc = self.emit_binary(BinaryOp::Mul, lhs_im, rhs_re);
                (
                    self.emit_binary(BinaryOp::Sub, ac, bd),
                    self.emit_binary(BinaryOp::Add, ad, bc),
                )
            }
            BinaryOp::Div => {
                let rr2 = self.emit_binary(BinaryOp::Mul, rhs_re, rhs_re);
                let ri2 = self.emit_binary(BinaryOp::Mul, rhs_im, rhs_im);
                let denom = self.emit_binary(BinaryOp::Add, rr2, ri2);
                let lhs_rr = self.emit_binary(BinaryOp::Mul, lhs_re, rhs_re);
                let lhs_ri = self.emit_binary(BinaryOp::Mul, lhs_re, rhs_im);
                let li_rr = self.emit_binary(BinaryOp::Mul, lhs_im, rhs_re);
                let li_ri = self.emit_binary(BinaryOp::Mul, lhs_im, rhs_im);
                let re_num = self.emit_binary(BinaryOp::Add, lhs_rr, li_ri);
                let im_num = self.emit_binary(BinaryOp::Sub, li_rr, lhs_ri);
                (
                    self.emit_binary(BinaryOp::Div, re_num, denom),
                    self.emit_binary(BinaryOp::Div, im_num, denom),
                )
            }
            _ => unreachable!("complex operator call should map to arithmetic binary op"),
        }
    }

    fn lower_complex_operand_parts(
        &mut self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<(Reg, Reg), LowerError> {
        if self.requires_complex_projection(expr, scope)? {
            let re = self.lower_field_access(expr, "re", scope, call_depth)?;
            let im = self.lower_field_access(expr, "im", scope, call_depth)?;
            return Ok((re, im));
        }

        let re = self.lower_expr(expr, scope, call_depth)?;
        let im = self.emit_const(0.0);
        Ok((re, im))
    }

    fn requires_complex_projection(
        &self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
    ) -> Result<bool, LowerError> {
        match expr {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => {
                let key = self.scope_key_from_reference(name, expr.span().unwrap_or_default())?;
                Ok(self.component_field_available(&key, name, "re")
                    || self.component_field_available(&key, name, "im")
                    || scope_field_available(scope, &key, "re")
                    || scope_field_available(scope, &key, "im"))
            }
            rumoca_core::Expression::FieldAccess { base, field, .. } => {
                Ok(field_access_binding_key(base, field)
                    .ok()
                    .map(|key| {
                        let source_ref = component_reference_for_field_base(base, field)
                            .ok()
                            .flatten();
                        let scope_key = source_ref
                            .as_ref()
                            .map(|component_ref| component_reference_key(component_ref.clone()))
                            .transpose()
                            .ok()
                            .flatten()
                            .unwrap_or_else(|| generated_scope_key(&key));
                        self.component_field_key_available(&scope_key, "re")
                            || self
                                .component_reference_field_available_opt(source_ref.as_ref(), "re")
                            || self.component_field_key_available(&scope_key, "im")
                            || self
                                .component_reference_field_available_opt(source_ref.as_ref(), "im")
                            || scope_field_available(scope, &scope_key, "re")
                            || scope_field_available(scope, &scope_key, "im")
                    })
                    .unwrap_or(false))
            }
            rumoca_core::Expression::Binary { lhs, rhs, .. } => Ok(self
                .requires_complex_projection(lhs, scope)?
                || self.requires_complex_projection(rhs, scope)?),
            rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs,
                ..
            } => self.requires_complex_projection(rhs, scope),
            rumoca_core::Expression::FunctionCall {
                name,
                is_constructor,
                ..
            } => Ok(complex_operator_call_op(name.as_str()).is_some()
                || self.function_call_returns_complex_parts(name, *is_constructor)),
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => Ok(self.branches_require_complex_projection(branches, scope)?
                || self.requires_complex_projection(else_branch, scope)?),
            _ => Ok(false),
        }
    }

    fn branches_require_complex_projection(
        &self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        scope: &Scope,
    ) -> Result<bool, LowerError> {
        for (_, value) in branches {
            if self.requires_complex_projection(value, scope)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn component_field_available(
        &self,
        base_key: &ComponentReferenceKey,
        reference: &rumoca_core::Reference,
        field: &str,
    ) -> bool {
        self.component_field_key_available(base_key, field)
            || self.component_reference_field_available(reference, field)
    }

    fn component_field_key_available(&self, base_key: &ComponentReferenceKey, field: &str) -> bool {
        let Some(field_key) = component_field_key(base_key, field) else {
            return false;
        };
        let generated_name = match &field_key {
            ComponentReferenceKey::Generated { name } => Some(name.as_str()),
            ComponentReferenceKey::Source { .. } => None,
        };
        generated_name.is_some_and(|name| self.layout.binding(name).is_some())
            || generated_name.is_some_and(|name| self.direct_assignments.contains_key(name))
            || self.indexed_bindings.contains_key(&field_key)
    }

    fn component_reference_field_available(
        &self,
        reference: &rumoca_core::Reference,
        field: &str,
    ) -> bool {
        let Some(source_ref) = component_reference_for_source_reference(reference) else {
            return false;
        };
        self.component_reference_field_available_opt(Some(&source_ref), field)
    }

    fn component_reference_field_available_opt(
        &self,
        reference: Option<&rumoca_core::ComponentReference>,
        field: &str,
    ) -> bool {
        let Some(mut component_ref) = reference.cloned() else {
            return false;
        };
        component_ref.parts.push(rumoca_core::ComponentRefPart {
            ident: field.to_string(),
            span: component_ref.span,
            subs: Vec::new(),
        });
        #[cfg(test)]
        {
            let display_name = rumoca_core::ComponentPath::from_component_reference(&component_ref)
                .to_flat_string();
            if let Some(key) =
                crate::test_support::fixture_key_for_component_ref(&component_ref, &display_name)
            {
                return self.indexed_bindings.contains_key(&key);
            }
        }
        ComponentReferenceKey::from_component_reference(&component_ref)
            .ok()
            .is_some_and(|key| self.indexed_bindings.contains_key(&key))
    }

    fn function_call_returns_complex_parts(
        &self,
        name: &rumoca_core::Reference,
        is_constructor: bool,
    ) -> bool {
        if is_constructor && name.last_segment() == "Complex" {
            return true;
        }
        let Some(function) = self.lookup_function(name) else {
            return false;
        };
        if self.is_record_constructor_call(name, is_constructor) {
            return function.inputs.iter().any(|input| input.name == "re")
                && function.inputs.iter().any(|input| input.name == "im");
        }
        function.outputs.first().is_some_and(|output| {
            output.type_class == Some(rumoca_core::ClassType::Record)
                && rumoca_core::qualified_type_name_matches(&output.type_name, "Complex")
        })
    }

    fn lower_constructor_field_access(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span: _,
        } = base
        else {
            return Ok(None);
        };

        if !self.is_record_constructor_call(name, *is_constructor) {
            return Ok(None);
        }

        if let Some(index) = constructor_positional_field_index(field)
            && let Some(expr) = args.get(index)
        {
            return self.lower_expr(expr, caller_scope, call_depth).map(Some);
        }

        let Some(constructor) = self.lookup_function(name).cloned() else {
            return Ok(None);
        };

        let mut local_scope = Scope::new();
        let mut input_regs = IndexMap::<String, Reg>::new();
        for (idx, input) in constructor.inputs.iter().enumerate() {
            let reg = if let Some(arg_expr) = args.get(idx) {
                self.lower_expr(arg_expr, caller_scope, call_depth + 1)?
            } else if let Some(default_expr) = input.default.as_ref() {
                self.lower_expr(default_expr, &local_scope, call_depth + 1)?
            } else {
                return Err(LowerError::MissingActualArgument {
                    function: name.as_str().to_string(),
                    what: "constructor input",
                    input: input.name.clone(),
                });
            };
            local_scope.insert(generated_scope_key(&input.name), reg);
            input_regs.insert(input.name.clone(), reg);
        }

        if let Some(reg) = input_regs.get(field).copied() {
            return Ok(Some(reg));
        }

        if let Some(output) = constructor
            .outputs
            .iter()
            .find(|output| output.name == field)
        {
            if let Some(default_expr) = output.default.as_ref() {
                let reg = self.lower_expr(default_expr, &local_scope, call_depth + 1)?;
                return Ok(Some(reg));
            }
            if let Some(reg) = local_scope.get(&generated_scope_key(&output.name)).copied() {
                return Ok(Some(reg));
            }
        }

        Ok(None)
    }

    fn lower_binary(
        &mut self,
        op: rumoca_core::OpBinary,
        lhs: Reg,
        rhs: Reg,
    ) -> Result<Reg, LowerError> {
        let reg = match op {
            rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => {
                self.emit_binary(BinaryOp::Add, lhs, rhs)
            }
            rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => {
                self.emit_binary(BinaryOp::Sub, lhs, rhs)
            }
            rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => {
                self.emit_binary(BinaryOp::Mul, lhs, rhs)
            }
            rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => {
                self.emit_binary(BinaryOp::Div, lhs, rhs)
            }
            rumoca_core::OpBinary::Exp | rumoca_core::OpBinary::ExpElem => {
                self.emit_binary(BinaryOp::Pow, lhs, rhs)
            }
            rumoca_core::OpBinary::And => self.emit_binary(BinaryOp::And, lhs, rhs),
            rumoca_core::OpBinary::Or => self.emit_binary(BinaryOp::Or, lhs, rhs),
            rumoca_core::OpBinary::Lt => self.emit_compare(CompareOp::Lt, lhs, rhs),
            rumoca_core::OpBinary::Le => self.emit_compare(CompareOp::Le, lhs, rhs),
            rumoca_core::OpBinary::Gt => self.emit_compare(CompareOp::Gt, lhs, rhs),
            rumoca_core::OpBinary::Ge => self.emit_compare(CompareOp::Ge, lhs, rhs),
            rumoca_core::OpBinary::Eq => self.emit_compare(CompareOp::Eq, lhs, rhs),
            rumoca_core::OpBinary::Neq => self.emit_compare(CompareOp::Ne, lhs, rhs),
            rumoca_core::OpBinary::Assign | rumoca_core::OpBinary::Empty => {
                return Err(LowerError::Unsupported {
                    reason: format!("binary operator {:?} is unsupported", op),
                });
            }
        };
        Ok(reg)
    }

    fn lower_unary(&mut self, op: rumoca_core::OpUnary, rhs: Reg) -> Result<Reg, LowerError> {
        let reg = match op {
            rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => {
                self.emit_unary(UnaryOp::Neg, rhs)
            }
            rumoca_core::OpUnary::Not => self.emit_unary(UnaryOp::Not, rhs),
            rumoca_core::OpUnary::Plus
            | rumoca_core::OpUnary::DotPlus
            | rumoca_core::OpUnary::Empty => rhs,
        };
        Ok(reg)
    }

    fn lower_simple_builtin(
        &mut self,
        function: rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        span: rumoca_core::Span,
    ) -> Option<Result<Reg, LowerError>> {
        let arg = |builder: &mut Self, idx: usize| -> Result<Reg, LowerError> {
            let expr = args.get(idx).ok_or_else(|| LowerError::ContractViolation {
                reason: format!(
                    "builtin {:?} requires argument at index {}, but only {} were provided",
                    function,
                    idx,
                    args.len()
                ),
                span: rumoca_core::Span::DUMMY,
            })?;
            builder.lower_expr(expr, scope, call_depth)
        };
        let unary = |builder: &mut Self, op: UnaryOp| -> Result<Reg, LowerError> {
            let x = arg(builder, 0)?;
            Ok(builder.emit_unary(op, x))
        };
        let binary = |builder: &mut Self, op: BinaryOp| -> Result<Reg, LowerError> {
            let x = arg(builder, 0)?;
            let y = arg(builder, 1)?;
            Ok(builder.emit_binary(op, x, y))
        };

        let result = match function {
            rumoca_core::BuiltinFunction::Abs => unary(self, UnaryOp::Abs),
            rumoca_core::BuiltinFunction::Sign => unary(self, UnaryOp::Sign),
            rumoca_core::BuiltinFunction::Sqrt => unary(self, UnaryOp::Sqrt),
            rumoca_core::BuiltinFunction::Floor | rumoca_core::BuiltinFunction::Integer => {
                unary(self, UnaryOp::Floor)
            }
            rumoca_core::BuiltinFunction::Ceil => unary(self, UnaryOp::Ceil),
            rumoca_core::BuiltinFunction::Sin => unary(self, UnaryOp::Sin),
            rumoca_core::BuiltinFunction::Cos => unary(self, UnaryOp::Cos),
            rumoca_core::BuiltinFunction::Tan => unary(self, UnaryOp::Tan),
            rumoca_core::BuiltinFunction::Asin => unary(self, UnaryOp::Asin),
            rumoca_core::BuiltinFunction::Acos => unary(self, UnaryOp::Acos),
            rumoca_core::BuiltinFunction::Atan => unary(self, UnaryOp::Atan),
            rumoca_core::BuiltinFunction::Sinh => unary(self, UnaryOp::Sinh),
            rumoca_core::BuiltinFunction::Cosh => unary(self, UnaryOp::Cosh),
            rumoca_core::BuiltinFunction::Tanh => unary(self, UnaryOp::Tanh),
            rumoca_core::BuiltinFunction::Exp => unary(self, UnaryOp::Exp),
            rumoca_core::BuiltinFunction::Log => unary(self, UnaryOp::Log),
            rumoca_core::BuiltinFunction::Log10 => unary(self, UnaryOp::Log10),
            rumoca_core::BuiltinFunction::Atan2 => binary(self, BinaryOp::Atan2),
            rumoca_core::BuiltinFunction::Div => self.lower_div_builtin(args, scope, call_depth),
            rumoca_core::BuiltinFunction::Mod => {
                self.lower_remainder_builtin(args, scope, call_depth, UnaryOp::Floor, "mod")
            }
            rumoca_core::BuiltinFunction::Rem => {
                self.lower_remainder_builtin(args, scope, call_depth, UnaryOp::Trunc, "rem")
            }
            rumoca_core::BuiltinFunction::NoEvent => arg(self, 0),
            rumoca_core::BuiltinFunction::Homotopy => {
                arg(self, usize::from(self.is_initial_mode && args.len() > 1))
            }
            rumoca_core::BuiltinFunction::Smooth => arg(self, 1),
            rumoca_core::BuiltinFunction::Zeros => Ok(self.emit_const(0.0)),
            rumoca_core::BuiltinFunction::Ones => Ok(self.emit_const(1.0)),
            rumoca_core::BuiltinFunction::Fill
            | rumoca_core::BuiltinFunction::Scalar
            | rumoca_core::BuiltinFunction::Vector
            | rumoca_core::BuiltinFunction::Matrix
            | rumoca_core::BuiltinFunction::Diagonal
            | rumoca_core::BuiltinFunction::Transpose
            | rumoca_core::BuiltinFunction::Linspace
            | rumoca_core::BuiltinFunction::Cat
            | rumoca_core::BuiltinFunction::Cross
            | rumoca_core::BuiltinFunction::Skew
            | rumoca_core::BuiltinFunction::OuterProduct
            | rumoca_core::BuiltinFunction::Symmetric => {
                self.lower_builtin_first_array_like_value(function, args, scope, call_depth, span)
            }
            rumoca_core::BuiltinFunction::Ndims => {
                let dims = match self.infer_expr_dims(
                    args.first()
                        .expect("ndims() requires exactly 1 argument — malformed Solve-IR"),
                    scope,
                ) {
                    Ok(dims) => dims,
                    Err(err) => return Some(Err(err)),
                };
                Ok(self.emit_const(dims.len() as f64))
            }
            rumoca_core::BuiltinFunction::Identity => Ok(self.emit_const(1.0)),
            _ => return None,
        };
        Some(result)
    }

    fn lower_div_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if args.len() != 2 {
            return Err(LowerError::ContractViolation {
                reason: format!("div() requires exactly 2 arguments, got {}", args.len()),
                span: rumoca_core::Span::DUMMY,
            });
        }
        let x = self.lower_expr(&args[0], scope, call_depth)?;
        let y = self.lower_expr(&args[1], scope, call_depth)?;
        let q = self.emit_binary(BinaryOp::Div, x, y);
        Ok(self.emit_unary(UnaryOp::Trunc, q))
    }

    fn lower_remainder_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        quotient_rounding: UnaryOp,
        function_name: &str,
    ) -> Result<Reg, LowerError> {
        if args.len() != 2 {
            return Err(LowerError::ContractViolation {
                reason: format!(
                    "{function_name}() requires exactly 2 arguments, got {}",
                    args.len()
                ),
                span: rumoca_core::Span::DUMMY,
            });
        }
        let x = self.lower_expr(&args[0], scope, call_depth)?;
        let y = self.lower_expr(&args[1], scope, call_depth)?;
        let q = self.emit_binary(BinaryOp::Div, x, y);
        let q_rounded = self.emit_unary(quotient_rounding, q);
        let product = self.emit_binary(BinaryOp::Mul, q_rounded, y);
        Ok(self.emit_binary(BinaryOp::Sub, x, product))
    }

    fn lower_builtin(
        &mut self,
        function: rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if let Some(result) = self.lower_simple_builtin(function, args, scope, call_depth, span) {
            return result;
        }

        let arg = |builder: &mut Self, idx: usize| -> Result<Reg, LowerError> {
            let expr = args.get(idx).ok_or_else(|| LowerError::ContractViolation {
                reason: format!(
                    "builtin {:?} requires argument at index {}, but only {} were provided",
                    function,
                    idx,
                    args.len()
                ),
                span,
            })?;
            builder.lower_expr(expr, scope, call_depth)
        };

        match function {
            rumoca_core::BuiltinFunction::Min => {
                self.lower_min_max_builtin(args, scope, call_depth, BinaryOp::Min, f64::INFINITY)
            }
            rumoca_core::BuiltinFunction::Max => self.lower_min_max_builtin(
                args,
                scope,
                call_depth,
                BinaryOp::Max,
                f64::NEG_INFINITY,
            ),
            // MLS §3.7.2: delay(expr, delayTime) reads expr at a previous
            // time instant. During event iteration it must not feed the
            // current unknown value back into the same discrete solve.
            rumoca_core::BuiltinFunction::Delay => {
                if args.is_empty() {
                    return Err(LowerError::ContractViolation {
                        reason: "delay() requires at least 1 argument".to_string(),
                        span,
                    });
                }
                self.lower_expr_in_mode(&args[0], scope, call_depth, ValueMode::Pre)
            }
            rumoca_core::BuiltinFunction::SemiLinear => {
                let x = arg(self, 0)?;
                let k1 = arg(self, 1)?;
                let k2 = arg(self, 2)?;
                let zero = self.emit_const(0.0);
                let cond = self.emit_compare(CompareOp::Ge, x, zero);
                let pos = self.emit_binary(BinaryOp::Mul, k1, x);
                let neg = self.emit_binary(BinaryOp::Mul, k2, x);
                Ok(self.emit_select(cond, pos, neg))
            }
            rumoca_core::BuiltinFunction::Der => Ok(self.emit_const(0.0)),
            rumoca_core::BuiltinFunction::Pre => Err(unsupported_at(
                "pre() must be lowered to __pre__ parameters before Solve-IR lowering",
                span,
            )),
            rumoca_core::BuiltinFunction::Edge => self.lower_edge_builtin(args, scope, call_depth),
            rumoca_core::BuiltinFunction::Change => {
                self.lower_change_builtin(args, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Initial => self.lower_initial_builtin(),
            // MLS §8.6: terminal() is false during ordinary simulation
            // evaluation; terminal-event handling can override this phase
            // marker explicitly when that event is implemented.
            rumoca_core::BuiltinFunction::Terminal => Ok(self.emit_const(0.0)),
            rumoca_core::BuiltinFunction::Sum => self.lower_sum_builtin(args, scope, call_depth),
            rumoca_core::BuiltinFunction::Product => {
                self.lower_product_builtin(args, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Size => self.lower_size_builtin(args, scope, call_depth),
            rumoca_core::BuiltinFunction::Sample => {
                self.lower_sample_builtin(args, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Reinit => Err(unsupported_at(
                "reinit() must be converted to event update equations before Solve-IR row lowering",
                span,
            )),
            _ => unreachable!("simple builtin handled before detailed lowering"),
        }
    }
}

mod misc_helpers;
use misc_helpers::*;
