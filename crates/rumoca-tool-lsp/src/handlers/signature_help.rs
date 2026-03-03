//! Signature help handler for Modelica files.

use lsp_types::{
    ParameterInformation, ParameterLabel, Position, SignatureHelp, SignatureInformation,
};

use crate::helpers::get_text_before_cursor;

/// Handle signature help request - show function parameter info.
pub fn handle_signature_help(source: &str, line: u32, character: u32) -> Option<SignatureHelp> {
    let position = Position { line, character };
    let text_before = get_text_before_cursor(source, position)?;

    // Find the function name and active parameter index
    let (func_name, active_param) = parse_function_context(&text_before)?;

    // Look up function signature
    let sig = get_function_signature(&func_name)?;

    Some(SignatureHelp {
        signatures: vec![sig],
        active_signature: Some(0),
        active_parameter: Some(active_param),
    })
}

fn parse_function_context(text: &str) -> Option<(String, u32)> {
    // Walk backwards to find matching open paren
    let mut depth = 0i32;
    let mut comma_count = 0u32;
    let chars: Vec<char> = text.chars().collect();

    for i in (0..chars.len()).rev() {
        match chars[i] {
            ')' => depth += 1,
            '(' if depth > 0 => depth -= 1,
            '(' => return extract_func_name(text, i, comma_count),
            ',' if depth == 0 => comma_count += 1,
            _ => {}
        }
    }
    None
}

fn extract_func_name(text: &str, paren_pos: usize, comma_count: u32) -> Option<(String, u32)> {
    let before_paren = text[..paren_pos].trim_end();
    let name: String = before_paren
        .chars()
        .rev()
        .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '.')
        .collect::<String>()
        .chars()
        .rev()
        .collect();
    if name.is_empty() {
        return None;
    }
    Some((name, comma_count))
}

fn get_function_signature(name: &str) -> Option<SignatureInformation> {
    let (label, params) = builtin_signature(name)?;
    let parameters = params
        .into_iter()
        .map(|p| ParameterInformation {
            label: ParameterLabel::Simple(p.to_string()),
            documentation: None,
        })
        .collect();
    Some(SignatureInformation {
        label: label.to_string(),
        documentation: None,
        parameters: Some(parameters),
        active_parameter: None,
    })
}

/// Look up the signature (label, parameter list) for a Modelica built-in function.
fn builtin_signature(name: &str) -> Option<(&'static str, Vec<&'static str>)> {
    let sig = match name {
        "der" => ("der(x)", vec!["x: Real"]),
        "abs" => ("abs(v)", vec!["v: Real"]),
        "sign" => ("sign(v)", vec!["v: Real"]),
        "sqrt" => ("sqrt(v)", vec!["v: Real"]),
        "sin" => ("sin(u)", vec!["u: Real"]),
        "cos" => ("cos(u)", vec!["u: Real"]),
        "tan" => ("tan(u)", vec!["u: Real"]),
        "asin" => ("asin(u)", vec!["u: Real"]),
        "acos" => ("acos(u)", vec!["u: Real"]),
        "atan" => ("atan(u)", vec!["u: Real"]),
        "atan2" => ("atan2(y, x)", vec!["y: Real", "x: Real"]),
        "exp" => ("exp(u)", vec!["u: Real"]),
        "log" => ("log(u)", vec!["u: Real"]),
        "log10" => ("log10(u)", vec!["u: Real"]),
        "sinh" => ("sinh(u)", vec!["u: Real"]),
        "cosh" => ("cosh(u)", vec!["u: Real"]),
        "tanh" => ("tanh(u)", vec!["u: Real"]),
        "floor" => ("floor(u)", vec!["u: Real"]),
        "ceil" => ("ceil(u)", vec!["u: Real"]),
        "mod" => ("mod(x, y)", vec!["x: Real", "y: Real"]),
        "rem" => ("rem(x, y)", vec!["x: Real", "y: Real"]),
        "div" => ("div(x, y)", vec!["x: Real", "y: Real"]),
        "min" => ("min(x, y)", vec!["x: Real", "y: Real"]),
        "max" => ("max(x, y)", vec!["x: Real", "y: Real"]),
        "pre" => ("pre(y)", vec!["y: discrete variable"]),
        "edge" => ("edge(b)", vec!["b: Boolean"]),
        "change" => ("change(v)", vec!["v: discrete variable"]),
        "initial" => ("initial()", vec![]),
        "terminal" => ("terminal()", vec![]),
        "noEvent" => ("noEvent(expr)", vec!["expr: Real"]),
        "smooth" => ("smooth(p, expr)", vec!["p: Integer", "expr: Real"]),
        "sample" => (
            "sample(start, interval)",
            vec!["start: Real", "interval: Real"],
        ),
        "delay" => (
            "delay(expr, delayTime, delayMax)",
            vec!["expr: Real", "delayTime: Real", "delayMax: Real"],
        ),
        "reinit" => ("reinit(x, expr)", vec!["x: Real", "expr: Real"]),
        "assert" => (
            "assert(condition, message, level)",
            vec![
                "condition: Boolean",
                "message: String",
                "level: AssertionLevel",
            ],
        ),
        "terminate" => ("terminate(message)", vec!["message: String"]),
        "connect" => ("connect(a, b)", vec!["a: connector", "b: connector"]),
        "zeros" => ("zeros(n)", vec!["n: Integer"]),
        "ones" => ("ones(n)", vec!["n: Integer"]),
        "fill" => ("fill(s, n1, n2, ...)", vec!["s: scalar", "n1: Integer"]),
        "identity" => ("identity(n)", vec!["n: Integer"]),
        "diagonal" => ("diagonal(v)", vec!["v: Vector"]),
        "linspace" => (
            "linspace(x1, x2, n)",
            vec!["x1: Real", "x2: Real", "n: Integer"],
        ),
        "transpose" => ("transpose(A)", vec!["A: Matrix"]),
        "size" => ("size(A, i)", vec!["A: Array", "i: Integer"]),
        "ndims" => ("ndims(A)", vec!["A: Array"]),
        "sum" => ("sum(A)", vec!["A: Array"]),
        "product" => ("product(A)", vec!["A: Array"]),
        "cat" => ("cat(k, A, B, ...)", vec!["k: Integer", "A: Array"]),
        "scalar" => ("scalar(A)", vec!["A: Array"]),
        "vector" => ("vector(A)", vec!["A: Array"]),
        "matrix" => ("matrix(A)", vec!["A: Array"]),
        "cross" => ("cross(x, y)", vec!["x: Vector[3]", "y: Vector[3]"]),
        "skew" => ("skew(x)", vec!["x: Vector[3]"]),
        "outerProduct" => ("outerProduct(v1, v2)", vec!["v1: Vector", "v2: Vector"]),
        "symmetric" => ("symmetric(A)", vec!["A: Matrix"]),
        "homotopy" => (
            "homotopy(actual, simplified)",
            vec!["actual: Real", "simplified: Real"],
        ),
        "semiLinear" => (
            "semiLinear(x, k+, k-)",
            vec!["x: Real", "k+: Real", "k-: Real"],
        ),
        "inStream" => ("inStream(v)", vec!["v: stream variable"]),
        "actualStream" => ("actualStream(v)", vec!["v: stream variable"]),
        "getInstanceName" => ("getInstanceName()", vec![]),
        "integer" => ("integer(x)", vec!["x: Real"]),
        "cardinality" => ("cardinality(c)", vec!["c: connector"]),
        _ => return None,
    };
    Some(sig)
}
