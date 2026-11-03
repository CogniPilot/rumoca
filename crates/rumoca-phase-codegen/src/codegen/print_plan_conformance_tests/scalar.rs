//! Reviewed decoders for the closed scalar print items. Each issued token
//! has one spelling here, independent of the template; a printed item
//! decodes to every token whose spelling it is, and conforms only when the
//! issued token, operands, extent and checks are among those decodings.
//! Tokens sharing one spelling (`and`/`boolean_min`, `or`/`boolean_max`)
//! are the same Boolean operation, so their decodings coincide by design.

use rumoca_ir_solve as solve;
use solve::{
    SolveBinaryToken as Binary, SolveBodyItem as Body, SolveCheckedToken as Checked,
    SolveCompareOperator as Compare, SolveConversionToken as Conversion,
    SolveDomainCheck as Domain, SolveElementOperand as Operand, SolveIntegerLiteral as Integer,
    SolveKernel as Kernel, SolveLiteral as Literal, SolveUnaryToken as Unary,
};

const UNARY: [(Unary, &str); 21] = [
    (Unary::Negate, "(-{a})"),
    (Unary::Not, "(!{a})"),
    (Unary::Sign, "({a}>0 ? 1 : ({a}<0 ? -1 : 0))"),
    (Unary::IntegerAbs, "({a}<0 ? -{a} : {a})"),
    (Unary::RealAbs, "fabs({a})"),
    (Unary::Ceiling, "ceil({a})"),
    (Unary::Truncate, "trunc({a})"),
    (Unary::Sqrt, "sqrt({a})"),
    (Unary::Floor, "floor({a})"),
    (Unary::Sin, "sin({a})"),
    (Unary::Cos, "cos({a})"),
    (Unary::Tan, "tan({a})"),
    (Unary::Asin, "asin({a})"),
    (Unary::Acos, "acos({a})"),
    (Unary::Atan, "atan({a})"),
    (Unary::Sinh, "sinh({a})"),
    (Unary::Cosh, "cosh({a})"),
    (Unary::Tanh, "tanh({a})"),
    (Unary::Exp, "exp({a})"),
    (Unary::Log, "log({a})"),
    (Unary::Log10, "log10({a})"),
];

const BINARY: [(Binary, &str); 12] = [
    (Binary::BooleanMin, "({a} && {b})"),
    (Binary::BooleanMax, "({a} || {b})"),
    (Binary::Power, "pow({a},{b})"),
    (Binary::Atan2, "atan2({a},{b})"),
    (Binary::RealMin, "rumoca_real_min({a},{b})"),
    (Binary::RealMax, "rumoca_real_max({a},{b})"),
    (Binary::Add, "({a} + {b})"),
    (Binary::Subtract, "({a} - {b})"),
    (Binary::Multiply, "({a} * {b})"),
    (Binary::Divide, "({a} / {b})"),
    (Binary::And, "({a} && {b})"),
    (Binary::Or, "({a} || {b})"),
];

const COMPARE: [(Compare, &str); 6] = [
    (Compare::Equal, "=="),
    (Compare::NotEqual, "!="),
    (Compare::Less, "<"),
    (Compare::LessEqual, "<="),
    (Compare::Greater, ">"),
    (Compare::GreaterEqual, ">="),
];

/// Each checked token as its complete lines over `{d}`, `{a}` and `{b}`.
const CHECKED: [(Checked, &str); 8] = [
    (
        Checked::Add,
        "if (({b}>0 && {a}>INT64_MAX-{b}) || ({b}<0 && {a}<INT64_MIN-{b})) return 1;\n{d} = {a}+{b};\n",
    ),
    (
        Checked::Subtract,
        "if (({b}<0 && {a}>INT64_MAX+{b}) || ({b}>0 && {a}<INT64_MIN+{b})) return 1;\n{d} = {a}-{b};\n",
    ),
    (
        Checked::Multiply,
        "if ({a}>0 ? ({b}>0 ? {a}>INT64_MAX/{b} : {b}<INT64_MIN/{a}) : ({a}<0 ? ({b}>0 ? {a}<INT64_MIN/{b} : ({b}<0 && {a}<INT64_MAX/{b})) : 0)) return 1;\n{d} = {a}*{b};\n",
    ),
    (
        Checked::Quotient,
        "if ({b} == 0) return 1;\nif ({a} == INT64_MIN && {b} == -1) return 1;\n{d} = {a}/{b};\n",
    ),
    (
        Checked::Modulo,
        "if ({b} == 0) return 1;\n{d} = {b} == -1 ? 0 : {a}%{b};\nif ({d} != 0 && (({d} < 0) != ({b} < 0))) {d} += {b};\n",
    ),
    (
        Checked::Remainder,
        "if ({b} == 0) return 1;\n{d} = {b} == -1 ? 0 : {a}%{b};\n",
    ),
    (Checked::Min, "{d} = {a} < {b} ? {a} : {b};\n"),
    (Checked::Max, "{d} = {a} > {b} ? {a} : {b};\n"),
];

fn fill(pattern: &str, d: &str, a: &str, b: &str) -> String {
    pattern
        .replace("{d}", d)
        .replace("{a}", a)
        .replace("{b}", b)
}

fn operand(value: Operand) -> String {
    let element = if value.is_broadcast() { "0" } else { "k" };
    format!("r{}[{element}]", value.register())
}

/// Every `r<n>[k]` or `r<n>[0]` read in `text`, in first-appearance order.
fn operands(text: &str) -> Vec<String> {
    let mut found: Vec<String> = Vec::new();
    let mut rest = text;
    while let Some(at) = rest.find('r') {
        let tail = &rest[at + 1..];
        let digits = tail.bytes().take_while(u8::is_ascii_digit).count();
        let after = &tail[digits..];
        let read = (digits > 0)
            .then(|| {
                ["[k]", "[0]"]
                    .into_iter()
                    .find(|index| after.starts_with(index))
            })
            .flatten();
        if let Some(index) = read {
            let name = format!("r{}{index}", &tail[..digits]);
            if !found.contains(&name) {
                found.push(name);
            }
        }
        rest = tail;
    }
    found
}

fn integer(value: Integer) -> String {
    match value {
        Integer::Minimum => "INT64_MIN".to_owned(),
        Integer::Negative { magnitude } => format!("(-INT64_C({magnitude}))"),
        Integer::Nonnegative { magnitude } => format!("INT64_C({magnitude})"),
    }
}

fn domain_line(destination: usize, extent: u32, domain: Domain) -> String {
    format!(
        "for (size_t k=0; k<{extent}; ++k) if (r{destination}[k]<{} || r{destination}[k]>{}) return 1;\n",
        integer(domain.minimum()),
        integer(domain.maximum())
    )
}

/// Whether `text` decodes, through some token's spelling over its own
/// operands, to the issued `expected` expression.
fn decodes_to<T: PartialEq + Copy>(
    text: &str,
    table: &[(T, &str)],
    expected: (T, &str, &str),
) -> bool {
    let reads = operands(text);
    let pick = |i: usize| reads.get(i).map_or("", String::as_str);
    let pairs = [(pick(0), pick(1)), (pick(0), pick(0)), (pick(1), pick(0))];
    table.iter().any(|&(token, pattern)| {
        token == expected.0
            && pairs.iter().any(|&(a, b)| {
                a == expected.1 && b == expected.2 && fill(pattern, "", a, b) == text
            })
    })
}

/// The next line, when it starts with `prefix`.
fn take(text: &mut &str, prefix: &str, item: &Body) -> Result<String, String> {
    let (line, rest) = text
        .split_once('\n')
        .ok_or_else(|| format!("{item:?}: body ended"))?;
    if !line.starts_with(prefix) {
        return Err(format!("{item:?} printed as {line:?}"));
    }
    *text = rest;
    Ok(line.to_owned())
}

/// Consumes the printed lines of one scalar item from `text`, or returns
/// why they are not the issued item.
pub(super) fn check(text: &mut &str, item: &Body) -> Result<(), String> {
    match *item {
        Body::Constant {
            destination, value, ..
        } => {
            let line = take(text, &format!("r{destination}[0] = "), item)?;
            let printed = &line[format!("r{destination}[0] = ").len()..];
            let literal = match value {
                Literal::Real64 { bits } => format!("rumoca_real64(UINT64_C({bits}))"),
                Literal::Int64 { value } => integer(value),
                Literal::Boolean { value } => value.to_string(),
            };
            (printed == format!("{literal};"))
                .then_some(())
                .ok_or_else(|| format!("{item:?} printed as {line:?}"))
        }
        Body::Select {
            destination,
            condition,
            if_true,
            if_false,
            ..
        } => {
            let line = take(text, "memcpy(", item)?;
            let expected = format!(
                "memcpy(r{destination}, (r{condition}[0] ? r{if_true} : r{if_false}), sizeof(r{destination}));"
            );
            (line == expected)
                .then_some(())
                .ok_or_else(|| format!("{item:?} printed as {line:?}"))
        }
        Body::Elementwise {
            destination,
            extent,
            kernel,
            domain,
            ..
        } => {
            let opening = take(text, "for (", item)?;
            if opening != format!("for (size_t k=0; k<{extent}; ++k) {{") {
                return Err(format!("{item:?} opened as {opening:?}"));
            }
            let close = text
                .find("\n}\n")
                .ok_or_else(|| format!("{item:?}: unclosed loop"))?;
            let kernel_text = format!("{}\n", &text[..close]);
            *text = &text[close + 3..];
            check_kernel(destination, kernel, &kernel_text)
                .ok_or_else(|| format!("{item:?} kernel printed as {kernel_text:?}"))?;
            check_domain(text, destination, extent, domain)
                .ok_or_else(|| format!("{item:?}: domain check differs"))
        }
        Body::Convert {
            destination,
            operand: source,
            extent,
            token,
            domain,
            ..
        } => {
            let line = take(text, "for (", item)?;
            let expected = match token {
                Conversion::IntegerToReal => format!(
                    "for (size_t k=0; k<{extent}; ++k) r{destination}[k] = (double)r{source}[k];"
                ),
                rounding => format!(
                    "for (size_t k=0; k<{extent}; ++k) {{ double v={}(r{source}[k]); if (!isfinite(v) || v < -9223372036854775808.0 || v >= 9223372036854775808.0) return 1; r{destination}[k]=(int64_t)v; }}",
                    if rounding == Conversion::RealToIntegerTowardZero {
                        "trunc"
                    } else {
                        "floor"
                    }
                ),
            };
            if line != expected {
                return Err(format!("{item:?} printed as {line:?}"));
            }
            check_domain(text, destination, extent, domain)
                .ok_or_else(|| format!("{item:?}: domain check differs"))
        }
        _ => Err(format!("{item:?} is not a scalar item")),
    }
}

/// The kernel lines between the loop opening and its closing brace.
fn check_kernel(destination: usize, kernel: Kernel, text: &str) -> Option<()> {
    let assignment = format!("r{destination}[k] = ");
    match kernel {
        Kernel::Unary {
            token,
            operand: value,
            min_operand_check,
        } => {
            let a = operand(value);
            let guard = format!("if ({a} == INT64_MIN) return 1;\n");
            let rest = if min_operand_check {
                text.strip_prefix(guard.as_str())?
            } else {
                (!text.starts_with("if (")).then_some(text)?
            };
            let expression = rest
                .strip_prefix(assignment.as_str())?
                .strip_suffix(";\n")?;
            decodes_to(expression, &UNARY, (token, a.as_str(), "")).then_some(())
        }
        Kernel::Binary { token, lhs, rhs } => {
            let expression = text
                .strip_prefix(assignment.as_str())?
                .strip_suffix(";\n")?;
            decodes_to(
                expression,
                &BINARY,
                (token, operand(lhs).as_str(), operand(rhs).as_str()),
            )
            .then_some(())
        }
        Kernel::Checked { token, lhs, rhs } => {
            let d = format!("r{destination}[k]");
            let (a, b) = (operand(lhs), operand(rhs));
            CHECKED
                .iter()
                .any(|&(candidate, pattern)| {
                    candidate == token && fill(pattern, &d, &a, &b) == text
                })
                .then_some(())
        }
        Kernel::Compare { token, lhs, rhs } => {
            let expression = text
                .strip_prefix(assignment.as_str())?
                .strip_suffix(";\n")?;
            let [a, spelling, b] = expression.split(' ').collect::<Vec<_>>()[..] else {
                return None;
            };
            let decoded = COMPARE
                .iter()
                .find(|&&(_, candidate)| candidate == spelling)?
                .0;
            (decoded == token && a == operand(lhs) && b == operand(rhs)).then_some(())
        }
    }
}

/// The optional domain line after an integer result.
fn check_domain(
    text: &mut &str,
    destination: usize,
    extent: u32,
    domain: Option<Domain>,
) -> Option<()> {
    let prefix = format!("for (size_t k=0; k<{extent}; ++k) if (r{destination}[k]<");
    match domain {
        Some(domain) => {
            *text = text.strip_prefix(domain_line(destination, extent, domain).as_str())?;
            Some(())
        }
        None => (!text.starts_with(prefix.as_str())).then_some(()),
    }
}
