//! Expression-rendering configuration shared by template helper modules.

use minijinja::Value;

/// Configuration for expression rendering.
#[derive(Clone)]
pub(crate) struct ExprConfig {
    pub(crate) prefix: String,
    pub(crate) power: String,
    pub(crate) and_op: String,
    pub(crate) or_op: String,
    pub(crate) not_op: String,
    pub(crate) true_val: String,
    pub(crate) false_val: String,
    pub(crate) array_start: String,
    pub(crate) array_end: String,
    pub(crate) if_style: IfStyle,
    /// When false, keep dots in variable/function names instead of replacing with underscores.
    pub(crate) sanitize_dots: bool,
    /// When true, use 1-based indexing (Modelica) instead of 0-based (Python).
    pub(crate) one_based_index: bool,
    /// When true, use Modelica builtin names (abs, min, max) instead of Python (fabs, fmin, fmax).
    pub(crate) modelica_builtins: bool,
    /// Optional function for element-wise multiply (e.g., `ca.times` for CasADi).
    pub(crate) mul_elem_fn: Option<String>,
    /// Optional function-call form for power (e.g., `ca.power` for CasADi).
    /// When set, `a^b` renders as `power_fn(a, b)` instead of `a ** b`.
    pub(crate) power_fn: Option<String>,
    /// Subscript rendering style: "bracket" (default: `x[0]`) or "underscore" (`x_1`, 1-based).
    /// The "underscore" style matches the C template's unpack_vars naming convention.
    pub(crate) subscript_underscore: bool,
    /// Override function name for `IfStyle::Function` (default: `"if_else"`).
    /// E.g., set to `"IfElse.ifelse"` for Julia ModelingToolkit.
    pub(crate) if_else_fn: Option<String>,
    /// When true, render Modelica range `start:end` as Python `range(start, end + 1)`
    /// and array comprehensions with `[...]` instead of `{...}`.
    pub(crate) python_range: bool,
    /// Override function name for `sum()` calls on non-literal arrays.
    /// Default is `"sum1"` (CasADi convention, rendered as `prefix + sum1`).
    /// Templates can set this to a runtime helper name.
    pub(crate) sum_fn: String,
    /// When true, render all numeric literals as float constants with `f` suffix.
    /// E.g., `8` → `8.0f`, `3.14` → `3.14f`.
    pub(crate) float_literals: bool,
    /// Optional source-reference to emitted-symbol map provided by a template.
    pub(crate) symbols: Option<Value>,
    /// Optional aliases from Appendix-B condition memory (`c[i]`) to live
    /// relation expressions for backends that do not run event iteration.
    pub(crate) condition_aliases: Option<Value>,
    /// Render-time substitutions for expression-level unrolling.
    pub(crate) substitutions: Vec<(String, String)>,
}

#[derive(Clone, Copy)]
pub(crate) enum IfStyle {
    /// Python-style: ca.if_else(cond, then, else)
    Function,
    /// Ternary: cond ? then : else
    Ternary,
    /// Modelica-style: if cond then expr elseif cond2 then expr2 else expr3
    Modelica,
}

impl Default for ExprConfig {
    fn default() -> Self {
        Self {
            prefix: String::new(),
            power: "**".to_string(),
            and_op: "and".to_string(),
            or_op: "or".to_string(),
            not_op: "not ".to_string(),
            true_val: "True".to_string(),
            false_val: "False".to_string(),
            array_start: "[".to_string(),
            array_end: "]".to_string(),
            if_style: IfStyle::Function,
            sanitize_dots: true,
            one_based_index: false,
            modelica_builtins: false,
            mul_elem_fn: None,
            power_fn: None,
            subscript_underscore: false,
            if_else_fn: None,
            python_range: false,
            sum_fn: "sum1".to_string(),
            float_literals: false,
            symbols: None,
            condition_aliases: None,
            substitutions: Vec::new(),
        }
    }
}

/// Helper to get a string attribute from a Value.
pub(crate) fn get_str_attr(v: &Value, attr: &str) -> Option<String> {
    v.get_attr(attr)
        .ok()
        .and_then(|val| val.as_str().map(|s| s.to_string()))
}

fn get_present_attr(v: &Value, attr: &str) -> Option<Value> {
    let val = v.get_attr(attr).ok()?;
    (!val.is_undefined() && !val.is_none()).then_some(val)
}

fn get_bool_attr(v: &Value, attr: &str) -> Option<bool> {
    Some(get_present_attr(v, attr)?.is_true())
}

fn get_non_empty_str_attr(v: &Value, attr: &str) -> Option<String> {
    get_str_attr(v, attr).filter(|s| !s.is_empty())
}

impl ExprConfig {
    pub(crate) fn from_value(v: &Value) -> Self {
        let mut cfg = Self::default();

        if let Some(s) = get_str_attr(v, "prefix") {
            cfg.prefix = s;
        }
        if let Some(s) = get_str_attr(v, "power") {
            cfg.power = s;
        }
        if let Some(s) = get_str_attr(v, "and_op") {
            cfg.and_op = s;
        }
        if let Some(s) = get_str_attr(v, "or_op") {
            cfg.or_op = s;
        }
        if let Some(s) = get_str_attr(v, "not_op") {
            cfg.not_op = s;
        }
        if let Some(s) = get_str_attr(v, "true_val") {
            cfg.true_val = s;
        }
        if let Some(s) = get_str_attr(v, "false_val") {
            cfg.false_val = s;
        }
        if let Some(s) = get_str_attr(v, "array_start") {
            cfg.array_start = s;
        }
        if let Some(s) = get_str_attr(v, "array_end") {
            cfg.array_end = s;
        }
        if let Some(s) = get_str_attr(v, "if_style") {
            cfg.if_style = match s.as_str() {
                "ternary" => IfStyle::Ternary,
                "modelica" => IfStyle::Modelica,
                _ => IfStyle::Function,
            };
        }
        if let Some(enabled) = get_bool_attr(v, "sanitize_dots") {
            cfg.sanitize_dots = enabled;
        }
        if let Some(enabled) = get_bool_attr(v, "one_based_index") {
            cfg.one_based_index = enabled;
        }
        if let Some(enabled) = get_bool_attr(v, "modelica_builtins") {
            cfg.modelica_builtins = enabled;
        }
        if let Some(s) = get_non_empty_str_attr(v, "mul_elem_fn") {
            cfg.mul_elem_fn = Some(s);
        }
        if let Some(s) = get_non_empty_str_attr(v, "power_fn") {
            cfg.power_fn = Some(s);
        }
        if let Some(enabled) = get_bool_attr(v, "subscript_underscore") {
            cfg.subscript_underscore = enabled;
        }
        if let Some(s) = get_non_empty_str_attr(v, "if_else_fn") {
            cfg.if_else_fn = Some(s);
        }
        if let Some(enabled) = get_bool_attr(v, "python_range") {
            cfg.python_range = enabled;
        }
        if let Some(s) = get_non_empty_str_attr(v, "sum_fn") {
            cfg.sum_fn = s;
        }
        if let Some(enabled) = get_bool_attr(v, "float_literals") {
            cfg.float_literals = enabled;
        }
        if let Some(val) = get_present_attr(v, "symbols") {
            cfg.symbols = Some(val);
        }
        if let Some(val) = get_present_attr(v, "condition_aliases") {
            cfg.condition_aliases = Some(val);
        }

        cfg
    }
}
