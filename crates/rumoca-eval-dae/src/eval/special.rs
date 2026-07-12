use super::builtin_table::{
    eval_external_table_function, resolve_function_closure, resolve_user_function,
};
use super::distribution_clock::{
    eval_clock_special_function, eval_distribution_function, eval_qualified_distribution_function,
    eval_qualified_math_special_function, is_qualified_distribution_function_name,
    is_qualified_math_special_function_name,
};
use super::*;

mod input_binding;
mod runtime_specials;
mod state_accessors;
pub(in crate::eval) use input_binding::infer_array_arg_dims;
use input_binding::*;
pub(super) use runtime_specials::*;
pub use runtime_specials::{
    deterministic_automatic_global_seed, is_runtime_special_function_name,
    is_runtime_special_function_short_name, modelica_strings_hash_string,
};
pub(super) use state_accessors::*;

#[derive(Clone)]
pub(in crate::eval) struct OutputSelection {
    output_name: String,
    indices: Vec<i64>,
    complex_component: Option<&'static str>,
}

impl OutputSelection {
    fn new(output_name: impl Into<String>, indices: &[i64]) -> Self {
        Self {
            output_name: output_name.into(),
            indices: indices.to_vec(),
            complex_component: None,
        }
    }

    pub(in crate::eval) fn output_name(&self) -> &str {
        &self.output_name
    }

    fn complex_component(&self) -> Option<&'static str> {
        self.complex_component
    }
}

fn declared_output_selection(
    function: &rumoca_core::Function,
    suffix: &str,
) -> Option<OutputSelection> {
    if !function.outputs.iter().any(|output| {
        suffix == output.name
            || suffix
                .strip_prefix(&output.name)
                .is_some_and(|rest| rest.starts_with('.'))
    }) {
        return None;
    }
    let mut selection = OutputSelection::new(suffix, &[]);
    selection.complex_component = complex_field_selection_from_path(suffix);
    Some(selection)
}

pub(in crate::eval) fn resolve_user_function_target<T: SimFloat>(
    requested_name: &str,
    env: &VarEnv<T>,
) -> Option<(VarName, Option<OutputSelection>)> {
    if resolve_user_function(requested_name, env).is_some() {
        return Some((VarName::new(requested_name), None));
    }

    rumoca_core::find_map_top_level_splits_rev(requested_name, |base_name, suffix| {
        let function = resolve_user_function(base_name, env)?;
        let selection = declared_output_selection(function, suffix)?;
        Some((VarName::new(base_name), Some(selection)))
    })
}

/// Resolve a function call target and return its output names in declaration order.
///
/// This is used by algorithm statement evaluation for multi-output assignments.
pub fn resolve_function_call_outputs_pub<T: SimFloat>(
    name: &VarName,
    env: &VarEnv<T>,
) -> Option<(VarName, Vec<String>)> {
    if let Some((resolved_name, selection)) = resolve_runtime_special_target(name.as_str()) {
        if selection.is_some() {
            return None;
        }
        let output_names = runtime_special_output_names(resolved_name.as_str())?
            .iter()
            .map(|name| (*name).to_string())
            .collect();
        return Some((resolved_name, output_names));
    }

    let (resolved_name, selection) = resolve_user_function_target(name.as_str(), env)?;
    if selection.is_some() {
        return None;
    }
    let func = resolve_user_function(resolved_name.as_str(), env)?;
    let output_names = func
        .outputs
        .iter()
        .map(|param| param.name.clone())
        .collect();
    Some((resolved_name, output_names))
}

/// DAE-IR wrapper for `resolve_function_call_outputs_pub`.
pub fn resolve_function_call_outputs_pub_dae<T: SimFloat>(
    name: &rumoca_core::Reference,
    env: &VarEnv<T>,
) -> Option<(rumoca_core::VarName, Vec<String>)> {
    let flat_name = VarName::new(name.as_str());
    let (resolved, outputs) = resolve_function_call_outputs_pub(&flat_name, env)?;
    Some((rumoca_core::VarName::new(resolved.as_str()), outputs))
}

struct RuntimeRecursionDepthGuard {
    runtime: Arc<EvalRuntimeState>,
}

impl Drop for RuntimeRecursionDepthGuard {
    fn drop(&mut self) {
        self.runtime
            .function_recursion_depth
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |depth| {
                Some(depth.saturating_sub(1))
            })
            .ok();
    }
}

fn try_enter_function_recursion_for(
    runtime: &Arc<EvalRuntimeState>,
) -> Option<RuntimeRecursionDepthGuard> {
    let depth = runtime
        .function_recursion_depth
        .fetch_add(1, Ordering::Relaxed);
    if depth >= MAX_FUNC_RECURSION {
        runtime
            .function_recursion_depth
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |depth| {
                Some(depth.saturating_sub(1))
            })
            .ok();
        return None;
    }
    Some(RuntimeRecursionDepthGuard {
        runtime: runtime.clone(),
    })
}

fn build_local_function_env<T: SimFloat>(env: &VarEnv<T>) -> VarEnv<T> {
    let mut local_env = VarEnv::<T>::new();
    local_env.vars = crate::eval::VarScope::child_of(&env.vars);
    local_env.runtime = env.runtime.clone();
    local_env.functions = env.functions.clone();
    local_env.dims = env.dims.clone();
    local_env.start_exprs = env.start_exprs.clone();
    local_env.clock_intervals = env.clock_intervals.clone();
    local_env.enum_literal_ordinals = env.enum_literal_ordinals.clone();
    local_env.is_initial = env.is_initial;
    local_env.function_closures = env.function_closures.clone();
    local_env
}

fn seed_static_function_scope_dims<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    inputs: &[FunctionParam],
    outputs: &[FunctionParam],
    locals: &[FunctionParam],
) {
    let dims = std::sync::Arc::make_mut(&mut local_env.dims);
    for param in inputs.iter().chain(outputs.iter()).chain(locals.iter()) {
        if param.dims.is_empty()
            || (!param.shape_expr.is_empty() && param.dims.iter().any(|dim| *dim <= 0))
            || param.dims.iter().any(|dim| *dim < 0)
        {
            continue;
        }
        dims.insert(param.name.clone(), param.dims.clone());
    }
}

fn seed_resolved_function_scope_dims<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    params: &[FunctionParam],
) -> Result<(), EvalError> {
    for param in params {
        let Some(dims) = resolved_function_param_dims(param, local_env)? else {
            continue;
        };
        std::sync::Arc::make_mut(&mut local_env.dims).insert(param.name.clone(), dims);
    }
    seed_record_field_dims(local_env, params);
    Ok(())
}

fn seed_record_field_dims<T: SimFloat>(local_env: &mut VarEnv<T>, params: &[FunctionParam]) {
    let mut field_dims = Vec::new();
    for param in params {
        collect_record_field_dims(
            &param.name,
            param,
            local_env,
            &mut Vec::new(),
            &mut field_dims,
        );
    }
    let dims = std::sync::Arc::make_mut(&mut local_env.dims);
    dims.extend(field_dims);
}

fn collect_record_field_dims<T: SimFloat>(
    prefix: &str,
    param: &FunctionParam,
    env: &VarEnv<T>,
    type_stack: &mut Vec<String>,
    field_dims: &mut Vec<(String, Vec<i64>)>,
) {
    if param.type_class != Some(rumoca_core::ClassType::Record)
        || type_stack.iter().any(|name| name == &param.type_name)
    {
        return;
    }
    let Some(constructor) = env.functions.get(param.type_name.as_str()).or_else(|| {
        env.functions
            .values()
            .find(|candidate| candidate.name.last_segment() == param.type_name)
    }) else {
        return;
    };
    type_stack.push(param.type_name.clone());
    for field in &constructor.inputs {
        let field_path = format!("{prefix}.{}", field.name);
        if !field.dims.is_empty() {
            field_dims.push((field_path.clone(), field.dims.clone()));
        }
        collect_record_field_dims(&field_path, field, env, type_stack, field_dims);
    }
    type_stack.pop();
}

pub(in crate::eval) fn resolved_function_param_dims<T: SimFloat>(
    param: &FunctionParam,
    env: &VarEnv<T>,
) -> Result<Option<Vec<i64>>, EvalError> {
    if !param.shape_expr.is_empty() {
        return eval_function_shape_expr(&param.shape_expr, env).map(Some);
    }
    if param.dims.is_empty() {
        return Ok(None);
    }
    if param.dims.iter().any(|dim| *dim < 0) {
        return Err(EvalError::InvalidShape {
            context: "function parameter dimensions",
            reason: format!("dimension must be non-negative: {:?}", param.dims),
        }
        .with_span_if_missing(param.span));
    }
    Ok(Some(param.dims.clone()))
}

fn eval_function_shape_expr<T: SimFloat>(
    shape_expr: &[Subscript],
    env: &VarEnv<T>,
) -> Result<Vec<i64>, EvalError> {
    let mut dims = Vec::with_capacity(shape_expr.len());
    for subscript in shape_expr {
        let dim = match subscript {
            Subscript::Index { value, .. } => *value,
            Subscript::Expr { expr, .. } => eval_shape_expr_dim(expr, env)?,
            Subscript::Colon { .. } => {
                return Err(EvalError::UnsupportedExpression {
                    kind: "dynamic function shape colon",
                });
            }
        };
        if dim < 0 {
            return Err(EvalError::UnsupportedExpression {
                kind: "negative function shape dimension",
            });
        }
        dims.push(dim);
    }
    Ok(dims)
}

fn eval_shape_expr_dim<T: SimFloat>(expr: &Expression, env: &VarEnv<T>) -> Result<i64, EvalError> {
    let value = eval_expr::<T>(expr, env)?.real().round();
    if !value.is_finite() || value < 0.0 || value > i64::MAX as f64 {
        return Err(EvalError::UnsupportedExpression {
            kind: "invalid function shape dimension",
        });
    }
    Ok(value as i64)
}

fn initialize_user_function_scope_values<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    outputs: &[FunctionParam],
    locals: &[FunctionParam],
) -> Result<(), EvalError> {
    for param in outputs.iter().chain(locals.iter()) {
        let dims = local_env
            .dims
            .get(param.name.as_str())
            .cloned()
            .unwrap_or_else(|| param.dims.clone());
        let Some(size) = concrete_param_size(&dims) else {
            if let Some(default) = param.default.as_ref() {
                let val = eval_expr::<T>(default, local_env)?;
                local_env.set(&param.name, val);
            }
            continue;
        };
        if let Some(default) = param.default.as_ref() {
            let values = eval_shaped_array_values::<T>(default, local_env, size)?;
            set_array_entries(local_env, &param.name, &dims, &values);
        }
    }
    Ok(())
}

fn selected_output_name(selection: &OutputSelection) -> Result<String, EvalError> {
    if selection.indices.is_empty() {
        return Ok(selection.output_name.clone());
    }
    let indices = selection
        .indices
        .iter()
        .map(|index| {
            usize::try_from(*index)
                .ok()
                .filter(|index| *index > 0)
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "invalid function output subscript",
                })
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(dae::format_subscript_key(&selection.output_name, &indices))
}

fn requested_function_output_name(
    selection: Option<&OutputSelection>,
    outputs: &[FunctionParam],
) -> Result<Option<String>, EvalError> {
    match selection {
        Some(selection) => selected_output_name(selection).map(Some),
        None => Ok(outputs.first().map(|output| output.name.clone())),
    }
}

fn function_output_cache_key(
    function: &VarName,
    args: &[Expression],
    output_name: impl Into<String>,
    env: &VarEnv<impl SimFloat>,
) -> FunctionOutputCacheKey {
    FunctionOutputCacheKey::new(function, args, output_name, env.is_initial)
}

fn cache_user_function_outputs<T: SimFloat>(
    env: &VarEnv<T>,
    function: &VarName,
    args: &[Expression],
    outputs: &[FunctionParam],
    local_env: &VarEnv<T>,
) {
    for output in outputs {
        let output_name = output.name.as_str();
        if let Some(value) = local_env.vars.get(output_name).copied() {
            let key = function_output_cache_key(function, args, output_name, env);
            env.insert_function_output_cache(key, value);
        }

        let field_prefix = format!("{output_name}.");
        let indexed_prefix = format!("{output_name}[");
        for (name, value) in local_env.vars.local_iter() {
            if name.starts_with(&field_prefix) || name.starts_with(&indexed_prefix) {
                let key = function_output_cache_key(function, args, name, env);
                env.insert_function_output_cache(key, *value);
            }
        }
    }
}

struct FunctionCallSummary {
    args_len: usize,
    inputs_len: usize,
    outputs_len: usize,
    locals_len: usize,
    body_len: usize,
    is_external: bool,
}

fn trace_function_call_summary(
    trace_call: bool,
    requested_name: &VarName,
    resolved_name: &VarName,
    summary: &FunctionCallSummary,
) {
    if !trace_call {
        return;
    }
    tracing::debug!(
        target: "rumoca_eval_dae::function_match",
        "function-call name='{}' resolved='{}' args={} inputs={} outputs={} locals={} body={} external={}",
        requested_name.as_str(),
        resolved_name.as_str(),
        summary.args_len,
        summary.inputs_len,
        summary.outputs_len,
        summary.locals_len,
        summary.body_len,
        summary.is_external
    );
}

fn trace_function_call_body_once(
    trace_call: bool,
    resolved_name: &VarName,
    body: &[rumoca_core::Statement],
) {
    if !trace_call {
        return;
    }
    static PRINT_FUNCTION_BODY_ONCE: std::sync::Once = std::sync::Once::new();
    PRINT_FUNCTION_BODY_ONCE.call_once(|| {
        tracing::debug!(
            target: "rumoca_eval_dae::function_match",
            "function-call first traced body for '{}': {:?}",
            resolved_name.as_str(),
            body
        );
    });
}

fn trace_function_call_inputs<T: SimFloat>(
    trace_call: bool,
    local_env: &VarEnv<T>,
    inputs: &[FunctionParam],
) {
    if !trace_call || !crate::trace::function_inputs_enabled() {
        return;
    }
    for input in inputs {
        let name = input.name.as_str();
        let dim = local_env.dims.get(name).cloned();
        let scalar = local_env.vars.get(name).copied();
        let first = env_array_sample(local_env, name, &[1]);
        let second = env_array_sample(local_env, name, &[2]);
        let first_base = dae::format_subscript_key(name, &[1]);
        let second_base = dae::format_subscript_key(name, &[2]);
        let first_re = local_env.vars.get(&format!("{first_base}.re")).copied();
        let second_re = local_env.vars.get(&format!("{second_base}.re")).copied();
        let first_im = local_env.vars.get(&format!("{first_base}.im")).copied();
        let second_im = local_env.vars.get(&format!("{second_base}.im")).copied();
        tracing::debug!(
            target: "rumoca_eval_dae::function_inputs",
            "function-call input {} value={:?} dims={:?} {}[1]={:?} {}[2]={:?} {}[1].re={:?} {}[2].re={:?} {}[1].im={:?} {}[2].im={:?}",
            name,
            scalar.map(|value| value.real()),
            dim,
            name,
            first,
            name,
            second,
            name,
            first_re,
            name,
            second_re,
            name,
            first_im,
            name,
            second_im
        );
    }
}

fn trace_function_call_outputs<T: SimFloat>(
    trace_call: bool,
    local_env: &VarEnv<T>,
    outputs: &[FunctionParam],
) {
    if !trace_call {
        return;
    }
    for output in outputs {
        let base = output.name.as_str();
        tracing::debug!(
            target: "rumoca_eval_dae::function_match",
            "function-call output {} = {} | {}.re = {} | {}.im = {}",
            base,
            trace_env_value(local_env, base),
            base,
            trace_env_value(local_env, &format!("{base}.re")),
            base,
            trace_env_value(local_env, &format!("{base}.im"))
        );
    }
}

fn trace_env_value<T: SimFloat>(env: &VarEnv<T>, name: &str) -> String {
    env.get_optional(name)
        .map(|value| value.real().to_string())
        .unwrap_or_else(|| "<missing>".to_string())
}

fn maybe_trace_interpolation_coefficients_state<T: SimFloat>(
    resolved_name: &VarName,
    local_env: &VarEnv<T>,
    body: &[rumoca_core::Statement],
) {
    if !crate::trace::sim_or_introspect_enabled()
        || !resolved_name
            .as_str()
            .ends_with("Modelica.Blocks.Sources.TimeTable.getInterpolationCoefficients")
    {
        return;
    }
    static PRINT_GETINTERP_BODY: std::sync::Once = std::sync::Once::new();
    PRINT_GETINTERP_BODY.call_once(|| {
        tracing::debug!(target: "rumoca_eval_dae::sim", "getInterpolationCoefficients body={:?}", body);
    });
    tracing::debug!(
        target: "rumoca_eval_dae::sim",
        "getInterpolationCoefficients state: startTimeScaled={} shiftTimeScaled={} timeScaled={} last_in={} next={} nrow={} tp={} dt={} a={} b={} nextEventScaled={}",
        trace_env_value(local_env, "startTimeScaled"),
        trace_env_value(local_env, "shiftTimeScaled"),
        trace_env_value(local_env, "timeScaled"),
        trace_env_value(local_env, "last"),
        trace_env_value(local_env, "next"),
        trace_env_value(local_env, "nrow"),
        trace_env_value(local_env, "tp"),
        trace_env_value(local_env, "dt"),
        trace_env_value(local_env, "a"),
        trace_env_value(local_env, "b"),
        trace_env_value(local_env, "nextEventScaled")
    );
}

fn resolve_selection_value<T: SimFloat>(
    local_env: &VarEnv<T>,
    selection: &OutputSelection,
) -> Result<T, EvalError> {
    let selected_name = selected_output_name(selection)?;
    if let Some(value) = local_env.vars.get(&selected_name).copied() {
        return Ok(value);
    }
    if let Some(base_output) = rumoca_core::ComponentPath::from_flat_path(&selection.output_name)
        .parts()
        .first()
        && let Some(value) = local_env.vars.get(base_output.as_str()).copied()
    {
        // Scalar evaluator executes user functions in selection context
        // (`*.re` / `*.im` caller names). If field materialization is absent
        // in the local env, use the base output bound in that context.
        return Ok(value);
    }
    Err(EvalError::MissingBinding {
        name: selection.output_name.clone(),
    })
}

fn eval_user_function_call<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let Some((resolved_name, selection)) = resolve_user_function_target(name.as_str(), env) else {
        return Ok(None);
    };
    let func = resolve_user_function(resolved_name.as_str(), env).ok_or_else(|| {
        EvalError::MissingFunction {
            name: resolved_name.to_string(),
        }
    })?;

    // External/empty stubs cannot be evaluated here; allow special handlers
    // (e.g. BooleanVectors/Clock wrappers) to resolve after this path.
    if func.external.is_some() || func.body.is_empty() {
        return Ok(None);
    }

    // Clone function data to release borrow on env.functions
    let inputs = func.inputs.clone();
    let outputs = func.outputs.clone();
    let locals = func.locals.clone();
    let body = func.body.clone();
    if let Some(output_name) = requested_function_output_name(selection.as_ref(), &outputs)? {
        let key = function_output_cache_key(&resolved_name, args, output_name, env);
        if let Some(value) = env.get_function_output_cache(&key) {
            return Ok(Some(value));
        }
    }

    let Some(_recursion_depth_guard) = try_enter_function_recursion_for(&env.runtime) else {
        return Err(EvalError::UnsupportedExpression {
            kind: "function recursion limit",
        });
    };

    let trace_call = function_trace_match_enabled(name.as_str(), resolved_name.as_str());
    let summary = FunctionCallSummary {
        args_len: args.len(),
        inputs_len: inputs.len(),
        outputs_len: outputs.len(),
        locals_len: locals.len(),
        body_len: body.len(),
        is_external: func.external.is_some(),
    };
    trace_function_call_summary(trace_call, name, &resolved_name, &summary);
    trace_function_call_body_once(trace_call, &resolved_name, &body);

    let mut local_env = build_local_function_env(env);
    seed_static_function_scope_dims(&mut local_env, &inputs, &outputs, &locals);
    // Bind arguments/defaults and execute the body under the call-stack context
    // so selected function calls (`*.re` / `*.im`) propagate correctly.
    let complex_component = selection
        .as_ref()
        .and_then(OutputSelection::complex_component);
    let eval_result = with_function_call_context(&env.runtime, complex_component, || {
        bind_user_function_inputs(&mut local_env, name.as_str(), &inputs, args, env)?;
        seed_resolved_function_scope_dims(&mut local_env, &outputs)?;
        seed_resolved_function_scope_dims(&mut local_env, &locals)?;
        trace_function_call_inputs(trace_call, &local_env, &inputs);
        initialize_user_function_scope_values(&mut local_env, &outputs, &locals)?;
        crate::statement::eval_statements(&body, &mut local_env)
    });
    eval_result?;
    trace_function_call_outputs(trace_call, &local_env, &outputs);
    maybe_trace_interpolation_coefficients_state(&resolved_name, &local_env, &body);
    cache_user_function_outputs(env, &resolved_name, args, &outputs, &local_env);

    if let Some(ref selection) = selection {
        return Ok(Some(resolve_selection_value(&local_env, selection)?));
    }

    let output = outputs.first().ok_or(EvalError::UnsupportedExpression {
        kind: "function output",
    })?;
    Ok(Some(local_env.require(&output.name)?))
}

pub fn eval_user_function_output_path_pub<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    output_path: &str,
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let (_, _, local_env) = eval_user_function_local_env(name, args, env)?;
    let value =
        local_env
            .vars
            .get(output_path)
            .copied()
            .ok_or_else(|| EvalError::MissingBinding {
                name: output_path.to_string(),
            })?;
    Ok(value)
}

/// Evaluate every scalar element of an array-valued path below a user-function
/// output.  Record outputs may be nested to arbitrary depth; `output_path` is
/// the fully qualified path in the function-local environment (for example,
/// `result.pose.position`).
pub fn eval_user_function_output_array_path_pub<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    output_path: &str,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    let (_, _, local_env) = eval_user_function_local_env(name, args, env)?;
    if let Some(values) =
        super::array_helpers::array_values_from_env_name_generic(output_path, &local_env)?
    {
        return Ok(values);
    }
    local_env
        .vars
        .get(output_path)
        .copied()
        .map(|value| vec![value])
        .ok_or_else(|| EvalError::MissingBinding {
            name: output_path.to_string(),
        })
}

/// Record function output: `(field suffix, value)` pairs plus per-field dims.
pub type RecordOutputFields<T> = (Vec<(String, T)>, Vec<(String, Vec<i64>)>);

pub fn eval_user_function_record_output_pub<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<RecordOutputFields<T>>, EvalError> {
    let Some(function) = resolve_user_function(name.as_str(), env) else {
        return Ok(None);
    };
    let Some(output) = function.outputs.first() else {
        return Ok(None);
    };
    if output.type_class != Some(rumoca_core::ClassType::Record) {
        return Ok(None);
    }
    let (_, _, local_env) = eval_user_function_local_env(name, args, env)?;
    Ok(Some(record_output_fields_from_local_env(
        &output.name,
        &local_env,
    )))
}

/// One materialized function output: flat values or record field entries.
pub enum MaterializedOutput<T> {
    Values(Vec<T>),
    Record(RecordOutputFields<T>),
}

/// Materialized function outputs as `(output name, materialized value)` pairs.
pub type MaterializedOutputs<T> = Vec<(String, MaterializedOutput<T>)>;

pub fn eval_user_function_outputs_pub<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<MaterializedOutputs<T>>, EvalError> {
    let Some(function) = resolve_user_function(name.as_str(), env) else {
        return Ok(None);
    };
    if function.external.is_some() || function.body.is_empty() {
        return Ok(None);
    }
    let (_, outputs, local_env) = eval_user_function_local_env(name, args, env)?;
    let mut materialized = Vec::with_capacity(outputs.len());
    for output in outputs {
        if output.type_class == Some(rumoca_core::ClassType::Record) {
            let fields = record_output_fields_from_local_env(&output.name, &local_env);
            if fields.0.is_empty() {
                return Err(EvalError::MissingBinding {
                    name: output.name.clone(),
                });
            }
            materialized.push((output.name, MaterializedOutput::Record(fields)));
            continue;
        }
        let values = if local_env
            .dims
            .get(output.name.as_str())
            .is_some_and(|dims| !dims.is_empty())
        {
            array_values_from_env_name_generic(output.name.as_str(), &local_env)?.ok_or_else(
                || EvalError::MissingBinding {
                    name: output.name.clone(),
                },
            )?
        } else {
            vec![local_env.require(output.name.as_str())?]
        };
        materialized.push((output.name, MaterializedOutput::Values(values)));
    }
    Ok(Some(materialized))
}

/// Record output field values and dims bound under `{output_name}.` in the
/// function's local evaluation scope.
fn record_output_fields_from_local_env<T: SimFloat>(
    output_name: &str,
    local_env: &VarEnv<T>,
) -> RecordOutputFields<T> {
    let prefix = format!("{output_name}.");
    let values = local_env
        .vars
        .iter()
        .filter_map(|(key, value)| {
            key.strip_prefix(prefix.as_str())
                .map(|suffix| (suffix.to_string(), *value))
        })
        .collect::<Vec<_>>();
    let dims = local_env
        .dims
        .iter()
        .filter_map(|(key, dims)| {
            key.strip_prefix(prefix.as_str())
                .map(|suffix| (suffix.to_string(), dims.clone()))
        })
        .collect::<Vec<_>>();
    (values, dims)
}

fn eval_user_function_local_env<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<(VarName, Vec<FunctionParam>, VarEnv<T>), EvalError> {
    let (resolved_name, selection) =
        resolve_user_function_target(name.as_str(), env).ok_or_else(|| {
            EvalError::MissingFunction {
                name: name.to_string(),
            }
        })?;
    if selection.is_some() {
        return Err(EvalError::UnsupportedExpression {
            kind: "selected function output path",
        });
    }
    let func = resolve_user_function(resolved_name.as_str(), env).ok_or_else(|| {
        EvalError::MissingFunction {
            name: resolved_name.to_string(),
        }
    })?;
    if func.external.is_some() || func.body.is_empty() {
        return Err(EvalError::UnsupportedExpression {
            kind: "external function output path",
        });
    }

    let inputs = func.inputs.clone();
    let outputs = func.outputs.clone();
    let locals = func.locals.clone();
    let body = func.body.clone();
    let Some(_recursion_depth_guard) = try_enter_function_recursion_for(&env.runtime) else {
        return Err(EvalError::UnsupportedExpression {
            kind: "function recursion limit",
        });
    };

    let mut local_env = build_local_function_env(env);
    seed_static_function_scope_dims(&mut local_env, &inputs, &outputs, &locals);
    let trace_call = function_trace_match_enabled(name.as_str(), resolved_name.as_str());
    trace_function_call_summary(
        trace_call,
        name,
        &resolved_name,
        &FunctionCallSummary {
            args_len: args.len(),
            inputs_len: inputs.len(),
            outputs_len: outputs.len(),
            locals_len: locals.len(),
            body_len: body.len(),
            is_external: false,
        },
    );
    with_function_call_context(&env.runtime, None, || {
        bind_user_function_inputs(&mut local_env, name.as_str(), &inputs, args, env)?;
        seed_resolved_function_scope_dims(&mut local_env, &outputs)?;
        seed_resolved_function_scope_dims(&mut local_env, &locals)?;
        trace_function_call_inputs(trace_call, &local_env, &inputs);
        initialize_user_function_scope_values(&mut local_env, &outputs, &locals)?;
        crate::statement::eval_statements(&body, &mut local_env)
    })?;
    trace_function_call_outputs(trace_call, &local_env, &outputs);
    cache_user_function_outputs(env, &resolved_name, args, &outputs, &local_env);
    Ok((resolved_name, outputs, local_env))
}

pub fn eval_user_function_array_output_pub<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    let (resolved_name, selection) =
        resolve_user_function_target(name.as_str(), env).ok_or_else(|| {
            EvalError::MissingFunction {
                name: name.to_string(),
            }
        })?;
    let func = resolve_user_function(resolved_name.as_str(), env).ok_or_else(|| {
        EvalError::MissingFunction {
            name: resolved_name.to_string(),
        }
    })?;
    if func.external.is_some() || func.body.is_empty() {
        return Err(EvalError::UnsupportedExpression {
            kind: "external function array output",
        });
    }

    let inputs = func.inputs.clone();
    let outputs = func.outputs.clone();
    let locals = func.locals.clone();
    let body = func.body.clone();
    let output = selected_array_output_param(selection.as_ref(), &outputs)?;
    let Some(_recursion_depth_guard) = try_enter_function_recursion_for(&env.runtime) else {
        return Err(EvalError::UnsupportedExpression {
            kind: "function recursion limit",
        });
    };

    let mut local_env = build_local_function_env(env);
    seed_static_function_scope_dims(&mut local_env, &inputs, &outputs, &locals);
    let complex_component = selection
        .as_ref()
        .and_then(OutputSelection::complex_component);
    with_function_call_context(&env.runtime, complex_component, || {
        bind_user_function_inputs(&mut local_env, name.as_str(), &inputs, args, env)?;
        seed_resolved_function_scope_dims(&mut local_env, &outputs)?;
        seed_resolved_function_scope_dims(&mut local_env, &locals)?;
        initialize_user_function_scope_values(&mut local_env, &outputs, &locals)?;
        crate::statement::eval_statements(&body, &mut local_env)
    })?;
    cache_user_function_outputs(env, &resolved_name, args, &outputs, &local_env);
    collect_function_array_output(&output, &local_env)
}

pub fn resolve_user_function_output_dims_pub<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    output_name: Option<&str>,
    env: &VarEnv<T>,
) -> Result<Option<Vec<i64>>, EvalError> {
    let (resolved_name, selection) =
        resolve_user_function_target(name.as_str(), env).ok_or_else(|| {
            EvalError::MissingFunction {
                name: name.to_string(),
            }
        })?;
    let function = resolve_user_function(resolved_name.as_str(), env).ok_or_else(|| {
        EvalError::MissingFunction {
            name: resolved_name.to_string(),
        }
    })?;
    let selected_name = output_name.or_else(|| selection.as_ref().map(|item| item.output_name()));
    let output = match selected_name {
        Some(selected_name) => function
            .outputs
            .iter()
            .find(|output| output.name == selected_name),
        None => function.outputs.first(),
    }
    .ok_or(EvalError::UnsupportedExpression {
        kind: "function output shape",
    })?;
    if output.dims.is_empty() && output.shape_expr.is_empty() {
        return Ok(None);
    }

    let mut local_env = build_local_function_env(env);
    seed_static_function_scope_dims(
        &mut local_env,
        &function.inputs,
        &function.outputs,
        &function.locals,
    );
    bind_user_function_inputs(
        &mut local_env,
        resolved_name.as_str(),
        &function.inputs,
        args,
        env,
    )?;
    resolved_function_param_dims(output, &local_env)
}

fn selected_array_output_param(
    selection: Option<&OutputSelection>,
    outputs: &[FunctionParam],
) -> Result<FunctionParam, EvalError> {
    match selection {
        Some(selection) => outputs
            .iter()
            .find(|output| output.name == selection.output_name)
            .cloned()
            .ok_or(EvalError::UnsupportedExpression {
                kind: "selected function array output",
            }),
        None => outputs
            .first()
            .cloned()
            .ok_or(EvalError::UnsupportedExpression {
                kind: "function array output",
            }),
    }
}

fn collect_function_array_output<T: SimFloat>(
    output: &FunctionParam,
    local_env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    let declared_dims = local_env.dims.get(output.name.as_str()).cloned();
    let Some(dims) = declared_dims.filter(|dims| !dims.is_empty()) else {
        if let Some(values) = array_values_from_env_name_generic(output.name.as_str(), local_env)?
            && values.len() > 1
        {
            return Ok(values);
        }
        return local_env
            .vars
            .get(output.name.as_str())
            .copied()
            .map(|value| vec![value])
            .ok_or_else(|| EvalError::MissingBinding {
                name: output.name.clone(),
            });
    };

    let expected = concrete_param_size(&dims).ok_or(EvalError::UnsupportedExpression {
        kind: "function array output shape",
    })?;
    let values =
        array_values_from_env_name_generic(output.name.as_str(), local_env)?.ok_or_else(|| {
            EvalError::MissingBinding {
                name: output.name.clone(),
            }
        })?;
    if values.len() != expected {
        return Err(EvalError::ShapeMismatch {
            context: "function array output",
            expected,
            actual: values.len(),
        });
    }
    Ok(values)
}

fn function_trace_match_enabled(_name: &str, _resolved_name: &str) -> bool {
    // Gated by the `rumoca_eval_dae::function_match` trace target. (The former
    // env var took a comma-separated name filter; the boolean trace target now
    // traces all function calls when enabled.)
    crate::trace::function_match_enabled()
}

fn copy_selected_input_fields<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param_name: &str,
    arg_path: &str,
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    let src_prefix = format!("{arg_path}.");
    let dst_prefix = format!("{param_name}.");
    for (field_name, field_value) in selected_input_fields(env, &src_prefix, &dst_prefix) {
        local_env.set(&field_name, field_value);
    }
    copy_selected_input_start_fields(local_env, env, &src_prefix, &dst_prefix)?;
    Ok(())
}

fn selected_input_fields<T: SimFloat>(
    env: &VarEnv<T>,
    src_prefix: &str,
    dst_prefix: &str,
) -> Vec<(String, T)> {
    let mut selected = Vec::new();
    for (key, value) in &env.vars {
        if let Some(suffix) = key.strip_prefix(src_prefix) {
            selected.push((format!("{dst_prefix}{suffix}"), *value));
        }
    }
    selected
}

fn copy_selected_input_start_fields<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    env: &VarEnv<T>,
    src_prefix: &str,
    dst_prefix: &str,
) -> Result<(), EvalError> {
    for (key, start_expr) in env.start_exprs.iter() {
        let Some(suffix) = key.strip_prefix(src_prefix) else {
            continue;
        };
        let dst = format!("{dst_prefix}{suffix}");
        if local_env.vars.contains_key(dst.as_str()) {
            continue;
        }
        match eval_expr::<T>(start_expr, env) {
            Ok(value) => local_env.set(&dst, value),
            Err(err) if err.missing_binding_name().is_some() => {}
            Err(err) => return Err(err),
        }
    }
    Ok(())
}

pub(in crate::eval) fn copy_record_function_output_fields<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    arg_expr: &Expression,
    env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    if param.type_class != Some(rumoca_core::ClassType::Record) {
        return Ok(false);
    }
    let Expression::FunctionCall { name, args, .. } = arg_expr else {
        return Ok(false);
    };
    let Some(function) = resolve_user_function(name.as_str(), env) else {
        return Ok(false);
    };
    let Some(output) = function.outputs.first() else {
        return Ok(false);
    };
    if output.type_class != Some(rumoca_core::ClassType::Record) {
        return Ok(false);
    }
    if copy_materialized_record_function_output_fields(
        local_env,
        param,
        name.var_name(),
        args,
        env,
    )? {
        return Ok(true);
    }

    if let Some(fields) =
        super::array_helpers::record_constructor_fields_for_output(function, output, env)
    {
        for field in fields {
            copy_record_function_output_field(
                local_env,
                param,
                name.var_name(),
                args,
                output,
                field,
                env,
            )?;
        }
        return Ok(true);
    }
    Err(EvalError::UnsupportedExpression {
        kind: "record function output fields",
    })
}

fn copy_record_function_output_field<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    function_name: &VarName,
    args: &[Expression],
    output: &FunctionParam,
    field: &FunctionParam,
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    let total = super::array_helpers::function_param_size(field).ok_or(
        EvalError::UnsupportedExpression {
            kind: "record function output field shape",
        },
    )?;
    let output_name = format!("{}.{}", output.name, field.name);
    let input_name = format!("{}.{}", param.name, field.name);
    for flat_index in 0..total {
        let output_path =
            super::array_helpers::function_output_path(&output_name, &field.dims, flat_index)
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "record function output field index",
                })?;
        let input_path =
            super::array_helpers::function_output_path(&input_name, &field.dims, flat_index)
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "record function input field index",
                })?;
        let value = eval_user_function_output_path_pub(function_name, args, &output_path, env)?;
        local_env.set(&input_path, value);
    }
    Ok(())
}

fn copy_materialized_record_function_output_fields<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    function_name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    let (_, outputs, function_env) = eval_user_function_local_env(function_name, args, env)?;
    let Some(output) = outputs
        .iter()
        .find(|output| output.type_class == Some(rumoca_core::ClassType::Record))
    else {
        return Err(EvalError::UnsupportedExpression {
            kind: "record function output fields",
        });
    };
    let src_prefix = format!("{}.", output.name);
    let dst_prefix = format!("{}.", param.name);
    let mut copied = false;
    for (key, value) in &function_env.vars {
        let Some(suffix) = key.strip_prefix(&src_prefix) else {
            continue;
        };
        local_env.set(&format!("{dst_prefix}{suffix}"), *value);
        copied = true;
    }
    Ok(copied)
}

fn bind_function_input_alias<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    current_function_name: &str,
    param: &FunctionParam,
    arg_expr: &Expression,
    env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    // Only function-typed inputs can be invoked as callable values.
    if !param.type_name.to_ascii_lowercase().contains("function") {
        return Ok(false);
    }
    let closure =
        function_closure_from_arg(arg_expr, env).ok_or_else(|| EvalError::MissingBinding {
            name: param.name.clone(),
        })?;
    local_env
        .function_closures
        .insert(param.name.clone(), closure.clone());
    local_env
        .function_closures
        .insert(format!("{current_function_name}.{}", param.name), closure);
    Ok(true)
}

pub(super) fn function_closure_from_arg<T: SimFloat>(
    arg_expr: &Expression,
    env: &VarEnv<T>,
) -> Option<FunctionClosure> {
    match arg_expr {
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } if !is_constructor && resolve_user_function(name.as_str(), env).is_some() => {
            Some(FunctionClosure {
                target_name: name.var_name().clone(),
                bound_args: args.clone(),
            })
        }
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => resolve_function_closure(name.as_str(), env).cloned(),
        _ => None,
    }
}

fn eval_function_closure_call<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let Some(closure) = resolve_function_closure(name.as_str(), env) else {
        return Ok(None);
    };
    let target = resolve_user_function(closure.target_name.as_str(), env).ok_or_else(|| {
        EvalError::MissingFunction {
            name: closure.target_name.to_string(),
        }
    })?;
    let mut merged_args = Vec::with_capacity(args.len().saturating_add(closure.bound_args.len()));
    // Modelica partial function application leaves at least one open argument
    // (e.g., `f(u)`), so runtime invocation supplies those first.
    merged_args.extend(args.iter().cloned());
    merged_args.extend(closure.bound_args.iter().cloned());
    if merged_args.len() > target.inputs.len() {
        return Ok(None);
    }
    eval_user_function_call(&closure.target_name, &merged_args, env)
}

fn eval_constructor_call<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    if args.is_empty() {
        return Err(EvalError::UnsupportedExpression {
            kind: "constructor arity",
        });
    }
    if args.len() == 1 {
        if current_function_complex_component(&env.runtime) == Some("im") {
            // Constructors like Complex(1) imply zero imaginary part.
            return Ok(T::zero());
        }
        return eval_expr::<T>(&args[0], env);
    }

    // When constructor calls survive into scalar evaluation, prefer component
    // selection by caller suffix if available (e.g., generated `*.im` helpers).
    if let Some(component) = current_function_complex_component(&env.runtime) {
        match component {
            "im" => return eval_expr::<T>(&args[1], env),
            "re" => return eval_expr::<T>(&args[0], env),
            _ => {}
        }
    }

    let _ = name;
    eval_expr::<T>(&args[0], env)
}

pub(super) fn eval_function_call<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    is_constructor: bool,
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let short_name = name.last_segment();
    if let Some(result) = eval_external_table_function(short_name, args, env)? {
        return Ok(result);
    }
    if is_constructor {
        return eval_constructor_call(name, args, env);
    }
    if name.as_str() == "Complex" {
        // MLS §6.7.1: Complex is the built-in operator-record constructor.
        // Flattened/runtime paths may still surface it as a plain function call,
        // so preserve constructor semantics even when `is_constructor` is lost.
        return eval_constructor_call(name, args, env);
    }
    if let Some(result) = eval_function_closure_call(name, args, env)? {
        return Ok(result);
    }

    if is_runtime_special_function_name(name)
        && let Some(result) = eval_special_function_call(name, args, env)?
    {
        return Ok(result);
    }

    // Runtime special functions must bypass structured user-function bodies so
    // standard library helpers like BooleanVectors.firstTrueIndex keep their
    // declared runtime semantics on the simulation path.
    if let Some(result) = eval_user_function_call(name, args, env)? {
        return Ok(result);
    }
    // Try qualified builtin resolution:
    // "Modelica.Math.sin" → "sin" → BuiltinFunction::Sin. This must run
    // after exact user-function lookup so `Pkg.vector` is not captured by the
    // builtin `vector` operator.
    if let Some(builtin) = BuiltinFunction::from_name(short_name) {
        return eval_builtin(builtin, args, env);
    }
    if let Some(builtin) = BuiltinFunction::from_name(&short_name.to_ascii_lowercase()) {
        return eval_builtin(builtin, args, env);
    }
    if let Some(result) = eval_special_function_call(name, args, env)? {
        return Ok(result);
    }
    trace_unresolved_user_function(name, env);

    Err(EvalError::MissingFunction {
        name: name.to_string(),
    })
}

fn trace_unresolved_user_function<T: SimFloat>(name: &VarName, env: &VarEnv<T>) {
    if !crate::trace::sim_or_introspect_enabled() {
        return;
    }
    let short = name.last_segment();
    let direct_hit = env.functions.contains_key(name.as_str());
    let short_hits: Vec<String> = env
        .functions
        .values()
        .filter(|candidate| candidate.name.last_segment() == short)
        .map(|candidate| candidate.name.as_str().to_string())
        .take(16)
        .collect();
    tracing::debug!(
        target: "rumoca_eval_dae::sim",
        "unresolved user function: name='{}' direct_hit={} total_functions={} short='{}' short_hits={:?}",
        name,
        direct_hit,
        env.functions.len(),
        short,
        short_hits
    );
}

/// Public wrapper for `eval_builtin`, used by the statement evaluator.
pub fn eval_builtin_pub<T: SimFloat>(
    function: BuiltinFunction,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    eval_builtin(function, args, env)
}

/// Public wrapper for `eval_function_call`, used by the statement evaluator.
pub fn eval_function_call_pub<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    eval_function_call(name, args, false, env)
}

/// DAE-IR wrapper for `eval_function_call_pub`.
pub fn eval_function_call_pub_dae<T: SimFloat>(
    name: &rumoca_core::VarName,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let flat_name = VarName::new(name.as_str());
    let flat_args: Vec<Expression> = args.to_vec();
    eval_function_call(&flat_name, &flat_args, false, env)
}

/// Evaluate a specific selected function output via its resolved base function name.
pub fn eval_selected_function_output_pub<T: SimFloat>(
    resolved_name: &VarName,
    output_name: &str,
    indices: &[i64],
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let selection = OutputSelection::new(output_name, indices);
    if let Some(value) =
        eval_selected_runtime_special_function(resolved_name.as_str(), &selection, args, env)?
    {
        return Ok(value);
    }
    let (_, _, local_env) = eval_user_function_local_env(resolved_name, args, env)?;
    resolve_selection_value(&local_env, &selection)
}

/// DAE-IR wrapper for `eval_selected_function_output_pub`.
pub fn eval_selected_function_output_pub_dae<T: SimFloat>(
    resolved_name: &rumoca_core::VarName,
    output_name: &str,
    indices: &[i64],
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let flat_name = VarName::new(resolved_name.as_str());
    let flat_args: Vec<Expression> = args.to_vec();
    eval_selected_function_output_pub(&flat_name, output_name, indices, &flat_args, env)
}

/// Evaluate a condition expression as a smooth zero-crossing function for root finding (f64-only).
pub fn eval_condition_as_root(expr: &Expression, env: &VarEnv<f64>) -> Result<f64, EvalError> {
    match expr {
        Expression::Binary { op, lhs, rhs, .. } => {
            let l = eval_expr::<f64>(lhs, env)?;
            let r = eval_expr::<f64>(rhs, env)?;
            match op {
                OpBinary::Lt | OpBinary::Le => Ok(l - r),
                OpBinary::Gt | OpBinary::Ge => Ok(r - l),
                _ => {
                    let val = eval_expr::<f64>(expr, env)?;
                    Ok(if val.to_bool() { -1.0 } else { 1.0 })
                }
            }
        }
        _ => {
            let val = eval_expr::<f64>(expr, env)?;
            Ok(if val.to_bool() { -1.0 } else { 1.0 })
        }
    }
}
