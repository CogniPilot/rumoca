//! Program generation over the evaluable subset, plus a readable rendering.
//!
//! The generator is seeded rather than sampled: program `seed` is the same
//! program on every machine and every run, so a divergence the differential
//! reports is reproducible from the seed printed with it.

use super::preservation_corpus::*;
use super::*;
use rumoca_core::OpBinary;

/// Deepest statement nesting a generated program contains.
const STATEMENT_DEPTH: usize = 2;

/// Deepest operator nesting a generated expression contains.
const EXPRESSION_DEPTH: usize = 2;

/// Longest loop or branch body a generated program contains.
///
/// Three is the shortest body that can hold a definition, a conditional
/// redefinition and a use of the same local, which is the shape a substitution
/// that ignores a later write to its target changes the meaning of.
const NESTED_BODY_LENGTH: usize = 3;

/// A reproducible draw sequence, from one seed.
pub(super) struct Draw {
    state: u64,
}

impl Draw {
    pub(super) fn new(seed: u64) -> Self {
        Self {
            state: seed
                .wrapping_mul(0x9E37_79B9_7F4A_7C15)
                .wrapping_add(0xD1B5_4A32_D192_ED03),
        }
    }

    fn next(&mut self) -> u64 {
        self.state = self.state.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut mixed = self.state;
        mixed = (mixed ^ (mixed >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        mixed = (mixed ^ (mixed >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        mixed ^ (mixed >> 31)
    }

    fn below(&mut self, bound: usize) -> usize {
        (self.next() % bound as u64) as usize
    }

    fn one_in(&mut self, bound: usize) -> bool {
        self.below(bound) == 0
    }
}

/// One generated program body, including its output epilogue.
pub(super) fn generated_program(seed: u64) -> Vec<rumoca_core::Statement> {
    let mut draw = Draw::new(seed);
    let count = 1 + draw.below(3);
    let mut body = statements(&mut draw, 0, &[], count);
    body.extend(epilogue(&mut draw));
    body
}

fn statements(
    draw: &mut Draw,
    depth: usize,
    binders: &[&'static str],
    count: usize,
) -> Vec<rumoca_core::Statement> {
    (0..count)
        .map(|_| statement(draw, depth, binders))
        .collect()
}

fn statement(draw: &mut Draw, depth: usize, binders: &[&'static str]) -> rumoca_core::Statement {
    if depth >= STATEMENT_DEPTH {
        return assignment(draw, binders);
    }
    match draw.below(6) {
        0..=2 => assignment(draw, binders),
        3 | 4 => loop_statement(draw, depth, binders),
        _ => branch_statement(draw, depth, binders),
    }
}

fn loop_statement(
    draw: &mut Draw,
    depth: usize,
    binders: &[&'static str],
) -> rumoca_core::Statement {
    let Some(binder) = BINDERS.get(binders.len()).copied() else {
        return assignment(draw, binders);
    };
    let count = 2 + draw.below(2) as i64;
    let mut inner = binders.to_vec();
    inner.push(binder);
    let length = 1 + draw.below(NESTED_BODY_LENGTH);
    let body = statements(draw, depth + 1, &inner, length);
    for_loop(binder, count, body)
}

fn branch_statement(
    draw: &mut Draw,
    depth: usize,
    binders: &[&'static str],
) -> rumoca_core::Statement {
    let arms = 1 + draw.below(2);
    let conditions = (0..arms)
        .map(|_| {
            let condition = boolean_expression(draw, binders, 0);
            let length = 1 + draw.below(NESTED_BODY_LENGTH);
            let taken = statements(draw, depth + 1, binders, length);
            (condition, taken)
        })
        .collect();
    let fallback = draw
        .one_in(2)
        .then(|| statements(draw, depth + 1, binders, 1));
    branch(conditions, fallback)
}

fn assignment(draw: &mut Draw, binders: &[&'static str]) -> rumoca_core::Statement {
    match draw.below(10) {
        0 | 1 => assign(REAL_LOCALS[0], real_expression(draw, binders, 0)),
        2 => assign(REAL_LOCALS[1], real_expression(draw, binders, 0)),
        3 => assign(INTEGER_LOCAL, integer_expression(draw, binders, 0)),
        4 => assign(BOOLEAN_LOCAL, boolean_expression(draw, binders, 0)),
        5 => assign(REAL_OUTPUT, real_expression(draw, binders, 0)),
        6 => assign_element(
            REAL_ARRAY_LOCAL,
            subscript(draw, binders),
            real_expression(draw, binders, 0),
        ),
        7 => assign_element(
            REAL_ARRAY_OUTPUT,
            subscript(draw, binders),
            real_expression(draw, binders, 0),
        ),
        8 => assign(REAL_ARRAY_LOCAL, array_expression(draw)),
        _ => assign(REAL_ARRAY_OUTPUT, array_expression(draw)),
    }
}

/// A whole-array value, for the unsubscripted array assignments and reads.
///
/// Compaction treats array locals as substitution targets exactly as it treats
/// scalar ones, but an array local is only ever *defined* by an unsubscripted
/// assignment. Without this form the predicates that ask whether a later
/// statement writes one coordinate of the target, or reads one coordinate of
/// it, have nothing to answer about.
fn array_expression(draw: &mut Draw) -> Expression {
    match draw.below(5) {
        0 => var(REAL_ARRAY_INPUTS[0]),
        1 => var(REAL_ARRAY_INPUTS[1]),
        2 => var(REAL_ARRAY_LOCAL),
        3 => binary(
            OpBinary::Add,
            var(REAL_ARRAY_INPUTS[draw.below(2)]),
            var(REAL_ARRAY_LOCAL),
        ),
        _ => binary(
            OpBinary::Sub,
            var(REAL_ARRAY_INPUTS[0]),
            var(REAL_ARRAY_INPUTS[1]),
        ),
    }
}

/// An in-bounds subscript: a binder, whose `1:N` range never exceeds the
/// declared extent, or a literal coordinate inside it.
fn subscript(draw: &mut Draw, binders: &[&'static str]) -> Expression {
    match binders {
        [] => integer(1 + draw.below(ARRAY_EXTENT as usize) as i64),
        _ if draw.one_in(3) => integer(1 + draw.below(ARRAY_EXTENT as usize) as i64),
        _ => var(binders[draw.below(binders.len())]),
    }
}

/// Addition and subtraction outweigh multiplication so that repeated
/// accumulation stays inside the exactly representable range.
fn arithmetic(draw: &mut Draw) -> OpBinary {
    match draw.below(6) {
        0..=2 => OpBinary::Add,
        3 | 4 => OpBinary::Sub,
        _ => OpBinary::Mul,
    }
}

fn real_expression(draw: &mut Draw, binders: &[&'static str], depth: usize) -> Expression {
    if depth >= EXPRESSION_DEPTH || draw.one_in(3) {
        return real_leaf(draw, binders);
    }
    let op = arithmetic(draw);
    let lhs = real_expression(draw, binders, depth + 1);
    let rhs = real_expression(draw, binders, depth + 1);
    binary(op, lhs, rhs)
}

fn real_leaf(draw: &mut Draw, binders: &[&'static str]) -> Expression {
    match draw.below(8) {
        0 => real(draw.below(7) as f64 - 3.0),
        1 => var(REAL_INPUTS[0]),
        2 => var(REAL_INPUTS[1]),
        3 => var(REAL_LOCALS[0]),
        4 => var(REAL_LOCALS[1]),
        5 => element(REAL_ARRAY_INPUTS[0], subscript(draw, binders)),
        6 => element(REAL_ARRAY_INPUTS[1], subscript(draw, binders)),
        _ => element(REAL_ARRAY_LOCAL, subscript(draw, binders)),
    }
}

fn integer_expression(draw: &mut Draw, binders: &[&'static str], depth: usize) -> Expression {
    if depth >= EXPRESSION_DEPTH || draw.one_in(3) {
        return integer_leaf(draw, binders);
    }
    let op = arithmetic(draw);
    let lhs = integer_expression(draw, binders, depth + 1);
    let rhs = integer_expression(draw, binders, depth + 1);
    binary(op, lhs, rhs)
}

fn integer_leaf(draw: &mut Draw, binders: &[&'static str]) -> Expression {
    match draw.below(4) {
        0 => integer(draw.below(4) as i64),
        1 => var(INTEGER_INPUT),
        2 => var(INTEGER_LOCAL),
        _ if binders.is_empty() => integer(draw.below(4) as i64),
        _ => var(binders[draw.below(binders.len())]),
    }
}

fn boolean_expression(draw: &mut Draw, binders: &[&'static str], depth: usize) -> Expression {
    if depth >= EXPRESSION_DEPTH {
        return var(BOOLEAN_LOCAL);
    }
    match draw.below(8) {
        0 => var(BOOLEAN_LOCAL),
        1 => boolean(draw.one_in(2)),
        2 => Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs: Box::new(boolean_expression(draw, binders, depth + 1)),
            span: span(),
        },
        3 => {
            let lhs = boolean_expression(draw, binders, depth + 1);
            let rhs = boolean_expression(draw, binders, depth + 1);
            binary(OpBinary::And, lhs, rhs)
        }
        4 => {
            let lhs = boolean_expression(draw, binders, depth + 1);
            let rhs = boolean_expression(draw, binders, depth + 1);
            binary(OpBinary::Or, lhs, rhs)
        }
        5 => {
            let lhs = integer_expression(draw, binders, depth + 1);
            let rhs = integer_expression(draw, binders, depth + 1);
            binary(relation(draw), lhs, rhs)
        }
        _ => {
            let lhs = real_expression(draw, binders, depth + 1);
            let rhs = real_expression(draw, binders, depth + 1);
            binary(relation(draw), lhs, rhs)
        }
    }
}

fn relation(draw: &mut Draw) -> OpBinary {
    match draw.below(4) {
        0 => OpBinary::Lt,
        1 => OpBinary::Le,
        2 => OpBinary::Gt,
        _ => OpBinary::Ge,
    }
}

/// Statements that route local values into the outputs.
///
/// An empty epilogue leaves every local dead at exit, which is the case the
/// pass is allowed to optimize hardest; a non-empty one makes named locals
/// observable, which is the case a deleted live store shows up in.
fn epilogue(draw: &mut Draw) -> Vec<rumoca_core::Statement> {
    let count = draw.below(3);
    (0..count).map(|_| output_statement(draw)).collect()
}

fn output_statement(draw: &mut Draw) -> rumoca_core::Statement {
    let coordinate = integer(1 + draw.below(ARRAY_EXTENT as usize) as i64);
    match draw.below(8) {
        0 => assign(REAL_OUTPUT, var(REAL_LOCALS[0])),
        1 => assign(REAL_OUTPUT, var(REAL_LOCALS[1])),
        2 => assign(
            REAL_OUTPUT,
            binary(OpBinary::Add, var(REAL_LOCALS[0]), var(REAL_LOCALS[1])),
        ),
        3 => assign(REAL_OUTPUT, element(REAL_ARRAY_LOCAL, coordinate)),
        4 => assign_element(REAL_ARRAY_OUTPUT, coordinate, var(REAL_LOCALS[0])),
        5 => assign(REAL_OUTPUT, var(INTEGER_LOCAL)),
        // A whole-array read of the array local, so a wrongly deleted
        // whole-array store to it is observable in an output.
        6 => assign(REAL_ARRAY_OUTPUT, var(REAL_ARRAY_LOCAL)),
        _ => assign(
            REAL_OUTPUT,
            Expression::If {
                branches: vec![(var(BOOLEAN_LOCAL), var(REAL_LOCALS[0]))],
                else_branch: Box::new(var(REAL_LOCALS[1])),
                span: span(),
            },
        ),
    }
}

/// A readable one-line rendering, so a reported divergence names a program.
pub(super) fn render(statements: &[rumoca_core::Statement]) -> String {
    statements
        .iter()
        .map(render_statement)
        .collect::<Vec<_>>()
        .join(" ")
}

fn render_statement(statement: &rumoca_core::Statement) -> String {
    match statement {
        rumoca_core::Statement::Assignment { comp, value, .. } => {
            format!(
                "{} := {};",
                render_component(comp),
                render_expression(value)
            )
        }
        rumoca_core::Statement::For {
            indices, equations, ..
        } => {
            let bindings = indices
                .iter()
                .map(|index| format!("{} in {}", index.ident, render_expression(&index.range)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("for {bindings} loop {} end for;", render(equations))
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => render_branch(cond_blocks, else_block.as_deref()),
        other => format!("{other:?}"),
    }
}

fn render_branch(
    cond_blocks: &[rumoca_core::StatementBlock],
    else_block: Option<&[rumoca_core::Statement]>,
) -> String {
    let mut rendered = String::new();
    for (ordinal, block) in cond_blocks.iter().enumerate() {
        let keyword = if ordinal == 0 { "if" } else { "elseif" };
        rendered.push_str(&format!(
            "{keyword} {} then {} ",
            render_expression(&block.cond),
            render(&block.stmts)
        ));
    }
    if let Some(statements) = else_block {
        rendered.push_str(&format!("else {} ", render(statements)));
    }
    rendered.push_str("end if;");
    rendered
}

fn render_component(comp: &rumoca_core::ComponentReference) -> String {
    comp.parts()
        .iter()
        .map(|part| format!("{}{}", part.ident, render_subscripts(&part.subs)))
        .collect::<Vec<_>>()
        .join(".")
}

fn render_subscripts(subs: &[rumoca_core::Subscript]) -> String {
    if subs.is_empty() {
        return String::new();
    }
    let rendered = subs
        .iter()
        .map(render_subscript)
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{rendered}]")
}

fn render_subscript(subscript: &rumoca_core::Subscript) -> String {
    match subscript {
        rumoca_core::Subscript::Index { value, .. } => value.to_string(),
        rumoca_core::Subscript::Colon { .. } => ":".to_string(),
        rumoca_core::Subscript::Expr { expr, .. } => render_expression(expr),
    }
}

fn render_expression(expression: &Expression) -> String {
    match expression {
        Expression::Literal { value, .. } => value.to_string(),
        Expression::VarRef {
            name, subscripts, ..
        } => format!("{}{}", name.as_str(), render_subscripts(subscripts)),
        Expression::Binary { op, lhs, rhs, .. } => format!(
            "({} {op} {})",
            render_expression(lhs),
            render_expression(rhs)
        ),
        Expression::Unary { op, rhs, .. } => format!("({op}{})", render_expression(rhs)),
        Expression::Range {
            start, step, end, ..
        } => {
            let step = step
                .as_ref()
                .map(|step| format!("{}:", render_expression(step)))
                .unwrap_or_default();
            format!(
                "{}:{step}{}",
                render_expression(start),
                render_expression(end)
            )
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => render_if_expression(branches, else_branch),
        Expression::Array { elements, .. } => {
            format!("{{{}}}", render_expression_list(elements))
        }
        Expression::BuiltinCall { function, args, .. } => {
            format!("{function:?}({})", render_expression_list(args))
        }
        Expression::ArrayComprehension { expr, indices, .. } => {
            let bindings = indices
                .iter()
                .map(|index| format!("{} in {}", index.name, render_expression(&index.range)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{} for {bindings}}}", render_expression(expr))
        }
        other => format!("{other:?}"),
    }
}

fn render_if_expression(branches: &[(Expression, Expression)], else_branch: &Expression) -> String {
    let arms = branches
        .iter()
        .map(|(condition, value)| {
            format!(
                "if {} then {}",
                render_expression(condition),
                render_expression(value)
            )
        })
        .collect::<Vec<_>>()
        .join(" else");
    format!("({arms} else {})", render_expression(else_branch))
}

fn render_expression_list(expressions: &[Expression]) -> String {
    expressions
        .iter()
        .map(render_expression)
        .collect::<Vec<_>>()
        .join(", ")
}
