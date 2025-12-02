//! Constants used throughout the IR processing pipeline.
//!
//! This module defines standard prefixes, built-in function names,
//! and other constants to avoid magic strings scattered throughout the codebase.

/// Prefix for derivative variables (e.g., der_x for derivative of x)
pub const DERIVATIVE_PREFIX: &str = "der_";

/// Prefix for previous-value variables (e.g., pre_x for previous value of x)
pub const PREVIOUS_VALUE_PREFIX: &str = "pre_";

/// Prefix for condition variables (e.g., c0, c1, c2)
pub const CONDITION_PREFIX: &str = "c";

/// Built-in function: derivative operator
pub const BUILTIN_DER: &str = "der";

/// Built-in function: previous value operator
pub const BUILTIN_PRE: &str = "pre";

/// Built-in function: reinit (for when clauses)
pub const BUILTIN_REINIT: &str = "reinit";

/// Built-in function: time variable
pub const BUILTIN_TIME: &str = "time";

/// Built-in function: noEvent - prevents event generation
/// noEvent(expr) returns expr but suppresses event generation during zero-crossing detection
pub const BUILTIN_NO_EVENT: &str = "noEvent";

/// Built-in function: smooth - indicates smoothness for event handling
/// smooth(p, expr) asserts that expr is p times continuously differentiable
pub const BUILTIN_SMOOTH: &str = "smooth";

/// Built-in function: sample - periodic event generation
/// sample(start, interval) returns true at time=start and then every interval seconds
pub const BUILTIN_SAMPLE: &str = "sample";

/// Built-in function: edge - rising edge detection
/// edge(b) returns true when b changes from false to true
pub const BUILTIN_EDGE: &str = "edge";

/// Built-in function: change - value change detection
/// change(v) returns true when v changes value
pub const BUILTIN_CHANGE: &str = "change";

/// Built-in function: initial - simulation start detection
/// initial() returns true during the initial equation evaluation
pub const BUILTIN_INITIAL: &str = "initial";

/// Built-in function: terminal - simulation end detection
/// terminal() returns true during the terminal equation evaluation
pub const BUILTIN_TERMINAL: &str = "terminal";

/// Built-in math functions - Trigonometric
pub const BUILTIN_SIN: &str = "sin";
pub const BUILTIN_COS: &str = "cos";
pub const BUILTIN_TAN: &str = "tan";
pub const BUILTIN_ASIN: &str = "asin";
pub const BUILTIN_ACOS: &str = "acos";
pub const BUILTIN_ATAN: &str = "atan";
pub const BUILTIN_ATAN2: &str = "atan2";
pub const BUILTIN_SINH: &str = "sinh";
pub const BUILTIN_COSH: &str = "cosh";
pub const BUILTIN_TANH: &str = "tanh";

/// Built-in math functions - Exponential/Logarithmic
pub const BUILTIN_EXP: &str = "exp";
pub const BUILTIN_LOG: &str = "log";
pub const BUILTIN_LOG10: &str = "log10";

/// Built-in math functions - Power/Root
pub const BUILTIN_SQRT: &str = "sqrt";

/// Built-in math functions - Rounding/Sign
pub const BUILTIN_ABS: &str = "abs";
pub const BUILTIN_SIGN: &str = "sign";
pub const BUILTIN_FLOOR: &str = "floor";
pub const BUILTIN_CEIL: &str = "ceil";
pub const BUILTIN_MOD: &str = "mod";
pub const BUILTIN_REM: &str = "rem";

/// Built-in math functions - Min/Max (scalar versions)
pub const BUILTIN_MIN: &str = "min";
pub const BUILTIN_MAX: &str = "max";

/// Built-in math functions - Integer conversion
pub const BUILTIN_INTEGER: &str = "integer";
pub const BUILTIN_DIV: &str = "div";

/// Built-in array functions - Construction
pub const BUILTIN_ZEROS: &str = "zeros";
pub const BUILTIN_ONES: &str = "ones";
pub const BUILTIN_FILL: &str = "fill";
pub const BUILTIN_IDENTITY: &str = "identity";
pub const BUILTIN_DIAGONAL: &str = "diagonal";
pub const BUILTIN_LINSPACE: &str = "linspace";

/// Built-in array functions - Information
pub const BUILTIN_SIZE: &str = "size";
pub const BUILTIN_NDIMS: &str = "ndims";

/// Built-in array functions - Reduction
pub const BUILTIN_SUM: &str = "sum";
pub const BUILTIN_PRODUCT: &str = "product";

/// Built-in array functions - Transformation
pub const BUILTIN_TRANSPOSE: &str = "transpose";
pub const BUILTIN_SYMMETRIC: &str = "symmetric";
pub const BUILTIN_CROSS: &str = "cross";
pub const BUILTIN_SKEW: &str = "skew";
pub const BUILTIN_OUTER_PRODUCT: &str = "outerProduct";

/// Built-in array functions - Scalar conversion (for vectors)
pub const BUILTIN_SCALAR: &str = "scalar";
pub const BUILTIN_VECTOR: &str = "vector";
pub const BUILTIN_MATRIX: &str = "matrix";

/// Default type names
pub const TYPE_REAL: &str = "Real";
pub const TYPE_BOOL: &str = "Bool";
pub const TYPE_INTEGER: &str = "Integer";
pub const TYPE_STRING: &str = "String";

/// Helper to create derivative variable name
pub fn derivative_name(var: &str) -> String {
    format!("{}{}", DERIVATIVE_PREFIX, var)
}

/// Helper to create previous value variable name
pub fn previous_value_name(var: &str) -> String {
    format!("{}{}", PREVIOUS_VALUE_PREFIX, var)
}

/// Helper to create condition variable name
pub fn condition_name(index: usize) -> String {
    format!("{}{}", CONDITION_PREFIX, index)
}

/// List of global built-in symbols that should not be scoped
pub fn global_builtins() -> Vec<String> {
    vec![
        // Core operators
        BUILTIN_TIME.to_string(),
        BUILTIN_DER.to_string(),
        BUILTIN_PRE.to_string(),
        BUILTIN_REINIT.to_string(),
        BUILTIN_NO_EVENT.to_string(),
        BUILTIN_SMOOTH.to_string(),
        BUILTIN_SAMPLE.to_string(),
        BUILTIN_EDGE.to_string(),
        BUILTIN_CHANGE.to_string(),
        BUILTIN_INITIAL.to_string(),
        BUILTIN_TERMINAL.to_string(),
        // Trigonometric
        BUILTIN_SIN.to_string(),
        BUILTIN_COS.to_string(),
        BUILTIN_TAN.to_string(),
        BUILTIN_ASIN.to_string(),
        BUILTIN_ACOS.to_string(),
        BUILTIN_ATAN.to_string(),
        BUILTIN_ATAN2.to_string(),
        BUILTIN_SINH.to_string(),
        BUILTIN_COSH.to_string(),
        BUILTIN_TANH.to_string(),
        // Exponential/Logarithmic
        BUILTIN_EXP.to_string(),
        BUILTIN_LOG.to_string(),
        BUILTIN_LOG10.to_string(),
        // Power/Root
        BUILTIN_SQRT.to_string(),
        // Rounding/Sign
        BUILTIN_ABS.to_string(),
        BUILTIN_SIGN.to_string(),
        BUILTIN_FLOOR.to_string(),
        BUILTIN_CEIL.to_string(),
        BUILTIN_MOD.to_string(),
        BUILTIN_REM.to_string(),
        // Min/Max
        BUILTIN_MIN.to_string(),
        BUILTIN_MAX.to_string(),
        // Integer conversion
        BUILTIN_INTEGER.to_string(),
        BUILTIN_DIV.to_string(),
        // Array construction
        BUILTIN_ZEROS.to_string(),
        BUILTIN_ONES.to_string(),
        BUILTIN_FILL.to_string(),
        BUILTIN_IDENTITY.to_string(),
        BUILTIN_DIAGONAL.to_string(),
        BUILTIN_LINSPACE.to_string(),
        // Array information
        BUILTIN_SIZE.to_string(),
        BUILTIN_NDIMS.to_string(),
        // Array reduction
        BUILTIN_SUM.to_string(),
        BUILTIN_PRODUCT.to_string(),
        // Array transformation
        BUILTIN_TRANSPOSE.to_string(),
        BUILTIN_SYMMETRIC.to_string(),
        BUILTIN_CROSS.to_string(),
        BUILTIN_SKEW.to_string(),
        BUILTIN_OUTER_PRODUCT.to_string(),
        // Scalar conversion
        BUILTIN_SCALAR.to_string(),
        BUILTIN_VECTOR.to_string(),
        BUILTIN_MATRIX.to_string(),
    ]
}

/// Check if a function name is a built-in function
pub fn is_builtin_function(name: &str) -> bool {
    global_builtins().contains(&name.to_string())
}

/// Check if a type name is a primitive/built-in type
pub fn is_primitive_type(name: &str) -> bool {
    matches!(
        name,
        TYPE_REAL | TYPE_BOOL | TYPE_INTEGER | TYPE_STRING | "Boolean"
    )
}
