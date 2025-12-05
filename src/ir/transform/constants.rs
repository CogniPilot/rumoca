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

/// Built-in special functions
/// cardinality(c) - returns number of connections to connector c (deprecated but needed for MSL)
pub const BUILTIN_CARDINALITY: &str = "cardinality";
/// semiLinear(x, k1, k2) - piecewise linear: if x >= 0 then k1*x else k2*x
pub const BUILTIN_SEMI_LINEAR: &str = "semiLinear";
/// String(value, ...) - converts value to string representation
pub const BUILTIN_STRING: &str = "String";
/// delay(expr, delayTime) - time delay function
pub const BUILTIN_DELAY: &str = "delay";
/// spatialDistribution(...) - transport delay for fluid flow
pub const BUILTIN_SPATIAL_DISTRIBUTION: &str = "spatialDistribution";
/// getInstanceName() - returns model instance path
pub const BUILTIN_GET_INSTANCE_NAME: &str = "getInstanceName";
/// homotopy(actual, simplified) - continuation method for initialization
pub const BUILTIN_HOMOTOPY: &str = "homotopy";
/// assert(condition, message) - runtime assertion
pub const BUILTIN_ASSERT: &str = "assert";
/// terminate(message) - graceful simulation termination
pub const BUILTIN_TERMINATE: &str = "terminate";

/// Default type names
pub const TYPE_REAL: &str = "Real";
pub const TYPE_BOOL: &str = "Bool";
pub const TYPE_INTEGER: &str = "Integer";
pub const TYPE_STRING: &str = "String";

// =============================================================================
// Modelica.Constants - Standard mathematical and physical constants
// Values from MSL 4.1.0 (CODATA 2018 / SI 2019)
// =============================================================================

/// Mathematical constant pi (π)
pub const MODELICA_PI: f64 = std::f64::consts::PI;

/// Mathematical constant e (Euler's number)
pub const MODELICA_E: f64 = std::f64::consts::E;

/// Degree to Radian conversion factor (π/180)
pub const MODELICA_D2R: f64 = std::f64::consts::PI / 180.0;

/// Radian to Degree conversion factor (180/π)
pub const MODELICA_R2D: f64 = 180.0 / std::f64::consts::PI;

/// Euler-Mascheroni constant (γ)
pub const MODELICA_GAMMA: f64 = 0.577_215_664_901_532_9;

/// Machine epsilon - difference between 1 and next representable float
pub const MODELICA_EPS: f64 = f64::EPSILON;

/// Smallest positive normalized floating-point number
pub const MODELICA_SMALL: f64 = f64::MIN_POSITIVE;

/// Maximum representable finite floating-point number
pub const MODELICA_INF: f64 = f64::MAX;

/// Speed of light in vacuum [m/s] (exact)
pub const MODELICA_C: f64 = 299792458.0;

/// Standard acceleration of gravity [m/s²]
pub const MODELICA_G_N: f64 = 9.80665;

/// Newtonian constant of gravitation [m³/(kg·s²)]
pub const MODELICA_G: f64 = 6.67430e-11;

/// Elementary charge \[C\] (exact)
pub const MODELICA_Q: f64 = 1.602176634e-19;

/// Planck constant [J·s] (exact)
pub const MODELICA_H: f64 = 6.62607015e-34;

/// Boltzmann constant [J/K] (exact)
pub const MODELICA_K: f64 = 1.380649e-23;

/// Avogadro constant [1/mol] (exact)
pub const MODELICA_N_A: f64 = 6.02214076e23;

/// Faraday constant [C/mol] = q * N_A
pub const MODELICA_F: f64 = MODELICA_Q * MODELICA_N_A;

/// Molar gas constant [J/(mol·K)] = k * N_A
pub const MODELICA_R: f64 = MODELICA_K * MODELICA_N_A;

/// Magnetic constant (vacuum permeability) [H/m]
pub const MODELICA_MU_0: f64 = 1.25663706212e-6;

/// Electric constant (vacuum permittivity) [F/m] = 1/(μ₀·c²)
pub const MODELICA_EPSILON_0: f64 = 1.0 / (MODELICA_MU_0 * MODELICA_C * MODELICA_C);

/// Stefan-Boltzmann constant [W/(m²·K⁴)]
/// σ = 2π⁵k⁴/(15h³c²)
pub const MODELICA_SIGMA: f64 = 5.670374419e-8;

/// Absolute zero temperature [°C]
pub const MODELICA_T_ZERO: f64 = -273.15;

/// Map of Modelica.Constants names to their values
/// Supports both short names (pi) and fully qualified names (Modelica.Constants.pi)
pub fn get_modelica_constant(name: &str) -> Option<f64> {
    // Strip "Modelica.Constants." prefix if present
    let short_name = name.strip_prefix("Modelica.Constants.").unwrap_or(name);

    match short_name {
        // Mathematical constants
        "pi" => Some(MODELICA_PI),
        "e" => Some(MODELICA_E),
        "D2R" => Some(MODELICA_D2R),
        "R2D" => Some(MODELICA_R2D),
        "gamma" => Some(MODELICA_GAMMA),

        // Machine-dependent constants
        "eps" => Some(MODELICA_EPS),
        "small" => Some(MODELICA_SMALL),
        "inf" => Some(MODELICA_INF),

        // Physical constants (SI 2019 / CODATA 2018)
        "c" => Some(MODELICA_C),
        "g_n" => Some(MODELICA_G_N),
        "G" => Some(MODELICA_G),
        "q" => Some(MODELICA_Q),
        "h" => Some(MODELICA_H),
        "k" => Some(MODELICA_K),
        "N_A" => Some(MODELICA_N_A),
        "F" => Some(MODELICA_F),
        "R" => Some(MODELICA_R),
        "mu_0" => Some(MODELICA_MU_0),
        "epsilon_0" => Some(MODELICA_EPSILON_0),
        "sigma" => Some(MODELICA_SIGMA),
        "T_zero" => Some(MODELICA_T_ZERO),

        _ => None,
    }
}

/// List of "safe" Modelica.Constants short names that won't conflict with variable names.
/// Single-letter constants (e, c, h, k, q, G, F, R) are excluded as they're common variable names.
pub fn modelica_constant_names() -> Vec<String> {
    vec![
        // Safe short names (unlikely to be variable names)
        "pi".to_string(),
        "mu_0".to_string(),
        "epsilon_0".to_string(),
        "sigma".to_string(),
        "g_n".to_string(),
        "N_A".to_string(),
        "D2R".to_string(),
        "R2D".to_string(),
        "gamma".to_string(),
        "eps".to_string(),
        "small".to_string(),
        "inf".to_string(),
        "T_zero".to_string(),
    ]
}

/// List of all Modelica.Constants names including ambiguous single-letter ones.
/// Use this only for fully qualified references.
pub fn all_modelica_constant_names() -> Vec<String> {
    vec![
        // Safe short names
        "pi".to_string(),
        "mu_0".to_string(),
        "epsilon_0".to_string(),
        "sigma".to_string(),
        "g_n".to_string(),
        "N_A".to_string(),
        "D2R".to_string(),
        "R2D".to_string(),
        "gamma".to_string(),
        "eps".to_string(),
        "small".to_string(),
        "inf".to_string(),
        "T_zero".to_string(),
        // Ambiguous names (only substitute with full qualification)
        "e".to_string(),
        "c".to_string(),
        "G".to_string(),
        "q".to_string(),
        "h".to_string(),
        "k".to_string(),
        "F".to_string(),
        "R".to_string(),
    ]
}

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
    let mut builtins = vec![
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
        // Special functions
        BUILTIN_CARDINALITY.to_string(),
        BUILTIN_SEMI_LINEAR.to_string(),
        BUILTIN_STRING.to_string(),
        BUILTIN_DELAY.to_string(),
        BUILTIN_SPATIAL_DISTRIBUTION.to_string(),
        BUILTIN_GET_INSTANCE_NAME.to_string(),
        BUILTIN_HOMOTOPY.to_string(),
        BUILTIN_ASSERT.to_string(),
        BUILTIN_TERMINATE.to_string(),
    ];

    // Add safe Modelica.Constants short names (won't conflict with variable names)
    for name in modelica_constant_names() {
        builtins.push(name.clone());
    }

    // Add ALL Modelica.Constants as fully qualified names (including ambiguous ones)
    for name in all_modelica_constant_names() {
        builtins.push(format!("Modelica.Constants.{}", name));
    }

    // Add built-in enumeration types and their literals
    for name in builtin_enumeration_names() {
        builtins.push(name);
    }

    builtins
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

// =============================================================================
// Built-in Enumeration Types
// =============================================================================

/// StateSelect enumeration - controls state variable selection in DAE solvers
/// enumeration(never, avoid, default, prefer, always)
pub mod state_select {
    pub const NEVER: i64 = 1;
    pub const AVOID: i64 = 2;
    pub const DEFAULT: i64 = 3;
    pub const PREFER: i64 = 4;
    pub const ALWAYS: i64 = 5;
}

/// Init enumeration - initialization types for blocks
/// enumeration(NoInit, SteadyState, InitialState, InitialOutput)
pub mod init {
    pub const NO_INIT: i64 = 1;
    pub const STEADY_STATE: i64 = 2;
    pub const INITIAL_STATE: i64 = 3;
    pub const INITIAL_OUTPUT: i64 = 4;
}

/// Dynamics enumeration - for fluid component initialization
/// enumeration(DynamicFreeInitial, FixedInitial, SteadyStateInitial, SteadyState)
pub mod dynamics {
    pub const DYNAMIC_FREE_INITIAL: i64 = 1;
    pub const FIXED_INITIAL: i64 = 2;
    pub const STEADY_STATE_INITIAL: i64 = 3;
    pub const STEADY_STATE: i64 = 4;
}

/// GravityTypes enumeration - for multibody mechanics
/// enumeration(NoGravity, UniformGravity, PointGravity)
pub mod gravity_types {
    pub const NO_GRAVITY: i64 = 1;
    pub const UNIFORM_GRAVITY: i64 = 2;
    pub const POINT_GRAVITY: i64 = 3;
}

/// AssertionLevel enumeration - for assert statement
/// enumeration(warning, error)
pub mod assertion_level {
    pub const WARNING: i64 = 1;
    pub const ERROR: i64 = 2;
}

/// AnalogFilter enumeration - analog filter characteristics
/// enumeration(CriticalDamping, Bessel, Butterworth, ChebyshevI)
pub mod analog_filter {
    pub const CRITICAL_DAMPING: i64 = 1;
    pub const BESSEL: i64 = 2;
    pub const BUTTERWORTH: i64 = 3;
    pub const CHEBYSHEV_I: i64 = 4;
}

/// FilterType enumeration - type of analog filter
/// enumeration(LowPass, HighPass, BandPass, BandStop)
pub mod filter_type {
    pub const LOW_PASS: i64 = 1;
    pub const HIGH_PASS: i64 = 2;
    pub const BAND_PASS: i64 = 3;
    pub const BAND_STOP: i64 = 4;
}

/// SimpleController enumeration - simple controller type
/// enumeration(P, PI, PD, PID)
pub mod simple_controller {
    pub const P: i64 = 1;
    pub const PI: i64 = 2;
    pub const PD: i64 = 3;
    pub const PID: i64 = 4;
}

/// Look up a built-in enumeration value by qualified name
/// Returns the integer value for enumeration literals like "StateSelect.prefer"
pub fn get_enumeration_value(name: &str) -> Option<i64> {
    // Handle fully qualified names (e.g., Modelica.Blocks.Types.Init.NoInit)
    // and short names (e.g., StateSelect.prefer)

    // StateSelect enumeration
    if name == "StateSelect.never" || name.ends_with(".StateSelect.never") {
        return Some(state_select::NEVER);
    }
    if name == "StateSelect.avoid" || name.ends_with(".StateSelect.avoid") {
        return Some(state_select::AVOID);
    }
    if name == "StateSelect.default" || name.ends_with(".StateSelect.default") {
        return Some(state_select::DEFAULT);
    }
    if name == "StateSelect.prefer" || name.ends_with(".StateSelect.prefer") {
        return Some(state_select::PREFER);
    }
    if name == "StateSelect.always" || name.ends_with(".StateSelect.always") {
        return Some(state_select::ALWAYS);
    }

    // Init enumeration (Modelica.Blocks.Types.Init)
    if name == "Init.NoInit" || name.ends_with(".Init.NoInit") {
        return Some(init::NO_INIT);
    }
    if name == "Init.SteadyState" || name.ends_with(".Init.SteadyState") {
        return Some(init::STEADY_STATE);
    }
    if name == "Init.InitialState" || name.ends_with(".Init.InitialState") {
        return Some(init::INITIAL_STATE);
    }
    if name == "Init.InitialOutput" || name.ends_with(".Init.InitialOutput") {
        return Some(init::INITIAL_OUTPUT);
    }

    // Dynamics enumeration (Modelica.Fluid.Types.Dynamics)
    if name == "Dynamics.DynamicFreeInitial" || name.ends_with(".Dynamics.DynamicFreeInitial") {
        return Some(dynamics::DYNAMIC_FREE_INITIAL);
    }
    if name == "Dynamics.FixedInitial" || name.ends_with(".Dynamics.FixedInitial") {
        return Some(dynamics::FIXED_INITIAL);
    }
    if name == "Dynamics.SteadyStateInitial" || name.ends_with(".Dynamics.SteadyStateInitial") {
        return Some(dynamics::STEADY_STATE_INITIAL);
    }
    if name == "Dynamics.SteadyState" || name.ends_with(".Dynamics.SteadyState") {
        return Some(dynamics::STEADY_STATE);
    }

    // GravityTypes enumeration (Modelica.Mechanics.MultiBody.Types.GravityTypes)
    if name == "GravityTypes.NoGravity" || name.ends_with(".GravityTypes.NoGravity") {
        return Some(gravity_types::NO_GRAVITY);
    }
    if name == "GravityTypes.UniformGravity" || name.ends_with(".GravityTypes.UniformGravity") {
        return Some(gravity_types::UNIFORM_GRAVITY);
    }
    if name == "GravityTypes.PointGravity" || name.ends_with(".GravityTypes.PointGravity") {
        return Some(gravity_types::POINT_GRAVITY);
    }

    // AssertionLevel enumeration
    if name == "AssertionLevel.warning" || name.ends_with(".AssertionLevel.warning") {
        return Some(assertion_level::WARNING);
    }
    if name == "AssertionLevel.error" || name.ends_with(".AssertionLevel.error") {
        return Some(assertion_level::ERROR);
    }

    // AnalogFilter enumeration (Modelica.Blocks.Types.AnalogFilter)
    if name == "AnalogFilter.CriticalDamping" || name.ends_with(".AnalogFilter.CriticalDamping") {
        return Some(analog_filter::CRITICAL_DAMPING);
    }
    if name == "AnalogFilter.Bessel" || name.ends_with(".AnalogFilter.Bessel") {
        return Some(analog_filter::BESSEL);
    }
    if name == "AnalogFilter.Butterworth" || name.ends_with(".AnalogFilter.Butterworth") {
        return Some(analog_filter::BUTTERWORTH);
    }
    if name == "AnalogFilter.ChebyshevI" || name.ends_with(".AnalogFilter.ChebyshevI") {
        return Some(analog_filter::CHEBYSHEV_I);
    }

    // FilterType enumeration (Modelica.Blocks.Types.FilterType)
    if name == "FilterType.LowPass" || name.ends_with(".FilterType.LowPass") {
        return Some(filter_type::LOW_PASS);
    }
    if name == "FilterType.HighPass" || name.ends_with(".FilterType.HighPass") {
        return Some(filter_type::HIGH_PASS);
    }
    if name == "FilterType.BandPass" || name.ends_with(".FilterType.BandPass") {
        return Some(filter_type::BAND_PASS);
    }
    if name == "FilterType.BandStop" || name.ends_with(".FilterType.BandStop") {
        return Some(filter_type::BAND_STOP);
    }

    // SimpleController enumeration (Modelica.Blocks.Types.SimpleController)
    if name == "SimpleController.P" || name.ends_with(".SimpleController.P") {
        return Some(simple_controller::P);
    }
    if name == "SimpleController.PI" || name.ends_with(".SimpleController.PI") {
        return Some(simple_controller::PI);
    }
    if name == "SimpleController.PD" || name.ends_with(".SimpleController.PD") {
        return Some(simple_controller::PD);
    }
    if name == "SimpleController.PID" || name.ends_with(".SimpleController.PID") {
        return Some(simple_controller::PID);
    }

    None
}

/// List of built-in enumeration type names (for global builtins)
pub fn builtin_enumeration_names() -> Vec<String> {
    vec![
        // StateSelect
        "StateSelect".to_string(),
        "StateSelect.never".to_string(),
        "StateSelect.avoid".to_string(),
        "StateSelect.default".to_string(),
        "StateSelect.prefer".to_string(),
        "StateSelect.always".to_string(),
        // Init
        "Init".to_string(),
        "Init.NoInit".to_string(),
        "Init.SteadyState".to_string(),
        "Init.InitialState".to_string(),
        "Init.InitialOutput".to_string(),
        // Dynamics
        "Dynamics".to_string(),
        "Dynamics.DynamicFreeInitial".to_string(),
        "Dynamics.FixedInitial".to_string(),
        "Dynamics.SteadyStateInitial".to_string(),
        "Dynamics.SteadyState".to_string(),
        // GravityTypes
        "GravityTypes".to_string(),
        "GravityTypes.NoGravity".to_string(),
        "GravityTypes.UniformGravity".to_string(),
        "GravityTypes.PointGravity".to_string(),
        // AssertionLevel
        "AssertionLevel".to_string(),
        "AssertionLevel.warning".to_string(),
        "AssertionLevel.error".to_string(),
        // AnalogFilter
        "AnalogFilter".to_string(),
        "AnalogFilter.CriticalDamping".to_string(),
        "AnalogFilter.Bessel".to_string(),
        "AnalogFilter.Butterworth".to_string(),
        "AnalogFilter.ChebyshevI".to_string(),
        // FilterType
        "FilterType".to_string(),
        "FilterType.LowPass".to_string(),
        "FilterType.HighPass".to_string(),
        "FilterType.BandPass".to_string(),
        "FilterType.BandStop".to_string(),
        // SimpleController
        "SimpleController".to_string(),
        "SimpleController.P".to_string(),
        "SimpleController.PI".to_string(),
        "SimpleController.PD".to_string(),
        "SimpleController.PID".to_string(),
    ]
}
