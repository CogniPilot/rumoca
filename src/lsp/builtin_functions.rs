//! Built-in Modelica function definitions for completion and signature help.

/// Information about a function for completion and signature help
pub struct FunctionInfo {
    pub name: &'static str,
    pub signature: &'static str,
    pub documentation: &'static str,
    pub parameters: Vec<(&'static str, &'static str)>, // (name, description)
}

/// Get built-in Modelica function information
pub fn get_builtin_functions() -> Vec<FunctionInfo> {
    vec![
        FunctionInfo {
            name: "der",
            signature: "der(x: Real) -> Real",
            documentation: "Time derivative of x",
            parameters: vec![("x", "Variable to differentiate")],
        },
        FunctionInfo {
            name: "pre",
            signature: "pre(x) -> typeof(x)",
            documentation: "Value of x immediately before the current event",
            parameters: vec![("x", "Variable to get previous value of")],
        },
        FunctionInfo {
            name: "noEvent",
            signature: "noEvent(expr) -> typeof(expr)",
            documentation: "Disable event generation for the expression",
            parameters: vec![("expr", "Expression to evaluate without events")],
        },
        FunctionInfo {
            name: "smooth",
            signature: "smooth(p: Integer, expr) -> typeof(expr)",
            documentation: "Indicate that expr is p times continuously differentiable",
            parameters: vec![
                ("p", "Order of continuous differentiability"),
                ("expr", "Expression that is smooth"),
            ],
        },
        FunctionInfo {
            name: "sample",
            signature: "sample(start: Real, interval: Real) -> Boolean",
            documentation: "Generate events at regular intervals",
            parameters: vec![
                ("start", "Start time for sampling"),
                ("interval", "Time interval between samples"),
            ],
        },
        FunctionInfo {
            name: "edge",
            signature: "edge(b: Boolean) -> Boolean",
            documentation: "True when b changes from false to true",
            parameters: vec![("b", "Boolean variable to detect rising edge")],
        },
        FunctionInfo {
            name: "change",
            signature: "change(v) -> Boolean",
            documentation: "True when v changes value",
            parameters: vec![("v", "Variable to detect changes")],
        },
        FunctionInfo {
            name: "reinit",
            signature: "reinit(x: Real, expr: Real)",
            documentation: "Reinitialize x to expr during an event",
            parameters: vec![
                ("x", "State variable to reinitialize"),
                ("expr", "New value for x"),
            ],
        },
        FunctionInfo {
            name: "sin",
            signature: "sin(x: Real) -> Real",
            documentation: "Sine function (x in radians)",
            parameters: vec![("x", "Angle in radians")],
        },
        FunctionInfo {
            name: "cos",
            signature: "cos(x: Real) -> Real",
            documentation: "Cosine function (x in radians)",
            parameters: vec![("x", "Angle in radians")],
        },
        FunctionInfo {
            name: "tan",
            signature: "tan(x: Real) -> Real",
            documentation: "Tangent function (x in radians)",
            parameters: vec![("x", "Angle in radians")],
        },
        FunctionInfo {
            name: "asin",
            signature: "asin(x: Real) -> Real",
            documentation: "Inverse sine function",
            parameters: vec![("x", "Value in range [-1, 1]")],
        },
        FunctionInfo {
            name: "acos",
            signature: "acos(x: Real) -> Real",
            documentation: "Inverse cosine function",
            parameters: vec![("x", "Value in range [-1, 1]")],
        },
        FunctionInfo {
            name: "atan",
            signature: "atan(x: Real) -> Real",
            documentation: "Inverse tangent function",
            parameters: vec![("x", "Value")],
        },
        FunctionInfo {
            name: "atan2",
            signature: "atan2(y: Real, x: Real) -> Real",
            documentation: "Two-argument inverse tangent",
            parameters: vec![("y", "Y coordinate"), ("x", "X coordinate")],
        },
        FunctionInfo {
            name: "sinh",
            signature: "sinh(x: Real) -> Real",
            documentation: "Hyperbolic sine function",
            parameters: vec![("x", "Value")],
        },
        FunctionInfo {
            name: "cosh",
            signature: "cosh(x: Real) -> Real",
            documentation: "Hyperbolic cosine function",
            parameters: vec![("x", "Value")],
        },
        FunctionInfo {
            name: "tanh",
            signature: "tanh(x: Real) -> Real",
            documentation: "Hyperbolic tangent function",
            parameters: vec![("x", "Value")],
        },
        FunctionInfo {
            name: "exp",
            signature: "exp(x: Real) -> Real",
            documentation: "Exponential function e^x",
            parameters: vec![("x", "Exponent")],
        },
        FunctionInfo {
            name: "log",
            signature: "log(x: Real) -> Real",
            documentation: "Natural logarithm ln(x)",
            parameters: vec![("x", "Value (must be positive)")],
        },
        FunctionInfo {
            name: "log10",
            signature: "log10(x: Real) -> Real",
            documentation: "Base-10 logarithm",
            parameters: vec![("x", "Value (must be positive)")],
        },
        FunctionInfo {
            name: "sqrt",
            signature: "sqrt(x: Real) -> Real",
            documentation: "Square root",
            parameters: vec![("x", "Value (must be non-negative)")],
        },
        FunctionInfo {
            name: "abs",
            signature: "abs(x) -> typeof(x)",
            documentation: "Absolute value",
            parameters: vec![("x", "Value")],
        },
        FunctionInfo {
            name: "sign",
            signature: "sign(x: Real) -> Integer",
            documentation: "Sign of x: -1, 0, or 1",
            parameters: vec![("x", "Value")],
        },
        FunctionInfo {
            name: "min",
            signature: "min(x, y) -> typeof(x)",
            documentation: "Minimum of two values",
            parameters: vec![("x", "First value"), ("y", "Second value")],
        },
        FunctionInfo {
            name: "max",
            signature: "max(x, y) -> typeof(x)",
            documentation: "Maximum of two values",
            parameters: vec![("x", "First value"), ("y", "Second value")],
        },
        FunctionInfo {
            name: "sum",
            signature: "sum(A) -> scalar",
            documentation: "Sum of all elements in array A",
            parameters: vec![("A", "Array to sum")],
        },
        FunctionInfo {
            name: "product",
            signature: "product(A) -> scalar",
            documentation: "Product of all elements in array A",
            parameters: vec![("A", "Array to multiply")],
        },
        FunctionInfo {
            name: "transpose",
            signature: "transpose(A) -> matrix",
            documentation: "Transpose of matrix A",
            parameters: vec![("A", "Matrix to transpose")],
        },
        FunctionInfo {
            name: "cross",
            signature: "cross(x: Real[3], y: Real[3]) -> Real[3]",
            documentation: "Cross product of 3-vectors",
            parameters: vec![("x", "First 3-vector"), ("y", "Second 3-vector")],
        },
        FunctionInfo {
            name: "skew",
            signature: "skew(x: Real[3]) -> Real[3,3]",
            documentation: "Skew-symmetric matrix from 3-vector",
            parameters: vec![("x", "3-vector")],
        },
        FunctionInfo {
            name: "identity",
            signature: "identity(n: Integer) -> Real[n,n]",
            documentation: "n×n identity matrix",
            parameters: vec![("n", "Size of the identity matrix")],
        },
        FunctionInfo {
            name: "diagonal",
            signature: "diagonal(v: Real[:]) -> Real[size(v,1), size(v,1)]",
            documentation: "Diagonal matrix from vector",
            parameters: vec![("v", "Vector for diagonal elements")],
        },
        FunctionInfo {
            name: "zeros",
            signature: "zeros(n1, n2, ...) -> Real[n1, n2, ...]",
            documentation: "Array of zeros with given dimensions",
            parameters: vec![("n1, n2, ...", "Dimensions of the array")],
        },
        FunctionInfo {
            name: "ones",
            signature: "ones(n1, n2, ...) -> Real[n1, n2, ...]",
            documentation: "Array of ones with given dimensions",
            parameters: vec![("n1, n2, ...", "Dimensions of the array")],
        },
        FunctionInfo {
            name: "fill",
            signature: "fill(s, n1, n2, ...) -> typeof(s)[n1, n2, ...]",
            documentation: "Array filled with value s",
            parameters: vec![
                ("s", "Value to fill with"),
                ("n1, n2, ...", "Dimensions of the array"),
            ],
        },
        FunctionInfo {
            name: "size",
            signature: "size(A, i: Integer) -> Integer",
            documentation: "Size of array A in dimension i",
            parameters: vec![("A", "Array"), ("i", "Dimension (1-based)")],
        },
        FunctionInfo {
            name: "ndims",
            signature: "ndims(A) -> Integer",
            documentation: "Number of dimensions of array A",
            parameters: vec![("A", "Array")],
        },
        FunctionInfo {
            name: "floor",
            signature: "floor(x: Real) -> Real",
            documentation: "Largest integer not greater than x",
            parameters: vec![("x", "Value to floor")],
        },
        FunctionInfo {
            name: "ceil",
            signature: "ceil(x: Real) -> Real",
            documentation: "Smallest integer not less than x",
            parameters: vec![("x", "Value to ceil")],
        },
        FunctionInfo {
            name: "mod",
            signature: "mod(x: Real, y: Real) -> Real",
            documentation: "Modulus: x - floor(x/y)*y",
            parameters: vec![("x", "Dividend"), ("y", "Divisor")],
        },
        FunctionInfo {
            name: "rem",
            signature: "rem(x: Real, y: Real) -> Real",
            documentation: "Remainder: x - div(x,y)*y",
            parameters: vec![("x", "Dividend"), ("y", "Divisor")],
        },
        FunctionInfo {
            name: "div",
            signature: "div(x: Real, y: Real) -> Integer",
            documentation: "Integer division: truncate(x/y)",
            parameters: vec![("x", "Dividend"), ("y", "Divisor")],
        },
        FunctionInfo {
            name: "integer",
            signature: "integer(x: Real) -> Integer",
            documentation: "Convert to integer (floor)",
            parameters: vec![("x", "Value to convert")],
        },
        FunctionInfo {
            name: "String",
            signature: "String(x, significantDigits, minimumLength, leftJustified) -> String",
            documentation: "Convert to string representation",
            parameters: vec![
                ("x", "Value to convert"),
                ("significantDigits", "Number of significant digits (optional)"),
                ("minimumLength", "Minimum string length (optional)"),
                ("leftJustified", "Left justify in field (optional)"),
            ],
        },
        FunctionInfo {
            name: "delay",
            signature: "delay(expr, delayTime, delayMax) -> typeof(expr)",
            documentation: "Return expr evaluated at time - delayTime",
            parameters: vec![
                ("expr", "Expression to delay"),
                ("delayTime", "Delay time"),
                ("delayMax", "Maximum delay (optional, for fixed-size buffer)"),
            ],
        },
        FunctionInfo {
            name: "initial",
            signature: "initial() -> Boolean",
            documentation: "True during initialization",
            parameters: vec![],
        },
        FunctionInfo {
            name: "terminal",
            signature: "terminal() -> Boolean",
            documentation: "True at end of successful simulation",
            parameters: vec![],
        },
        FunctionInfo {
            name: "assert",
            signature: "assert(condition: Boolean, message: String, level)",
            documentation: "Check condition, report message if false",
            parameters: vec![
                ("condition", "Condition to check"),
                ("message", "Error message if condition is false"),
                ("level", "AssertionLevel.error or .warning (optional)"),
            ],
        },
        FunctionInfo {
            name: "connect",
            signature: "connect(a, b)",
            documentation: "Connect two connectors",
            parameters: vec![("a", "First connector"), ("b", "Second connector")],
        },
    ]
}
