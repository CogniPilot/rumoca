//! GALEC AST → C99 printer for the `embedded-c-galec` export (SPEC_0034
//! GAL-024).
//!
//! Scope: exactly the AST shape [`crate::lower`] emits today — sequential
//! [`Statement::Assignment`]s over single-part `self.` state references,
//! with expressions built from literals, references, the emittable §3.2.6
//! builtin calls ([`crate::lower::emittable_builtin_targets`]),
//! parentheses, if-expressions, `not`, unary minus over references, binary
//! operations, and whole-array start literals. Anything outside that shape
//! fails with a typed `ET023` — never silently dropped (GAL-007).
//!
//! Semantics preserved in C:
//!
//! - `and`/`or`/`not` → `&&`/`||`/`!`; `<>` → `!=`; `^` → `pow(…)`
//!   (GALEC `^` returns Real for numeric operands);
//! - every composite subexpression is parenthesized, so the AST shape — the
//!   normative GALEC evaluation order (trap T6) — survives verbatim and
//!   nested unary/binary forms can never re-associate;
//! - Real literals reuse the strict GALEC formatter (trap T7); its output
//!   (`1.0e+5`) is a valid C `double` literal;
//! - GALEC subscripts are 1-based, C subscripts 0-based: literal indices
//!   shift at print time, expression indices print as `(… - 1)`;
//! - Integer is `int32_t`, so literals outside its range are rejected
//!   rather than truncated;
//! - builtins map per the table below; the few without a direct C99
//!   counterpart call `static inline` helpers owned by the
//!   `embedded-c-galec` templates (`rumoca_ir_galec_sign`/`min`/`max`/
//!   `imin`/`imax` — a fixed name contract, compile-checked in CI per
//!   GAL-012).

use rumoca_ir_galec::ast::{
    BinaryOp, Expression, FunctionCall, IfExpression, Name, RefPart, Reference, Statement,
};

use crate::c_mangle::CNameTable;
use crate::diagnostic::GalecTargetError;

/// How one GALEC builtin prints in C.
enum CBuiltin {
    /// Plain C function call (libm or a template-owned helper).
    Function(&'static str),
    /// `real(i)` → `((double)(i))` explicit widening cast.
    RealCast,
    /// `divisionTowardsZero(a, b)` → `(a / b)`; C99 integer division
    /// truncates toward zero, matching the catalog semantics.
    IntegerDivision,
}

/// The GALEC §3.2.6 → C mapping for every catalog name the lowering can
/// emit, with the call arity. Parity with
/// [`crate::lower::emittable_builtin_targets`] is pinned by a unit test
/// (GAL-005: accept/lower/render stay anchored to one catalog).
static C_BUILTIN_MAP: &[(&str, usize, CBuiltin)] = &[
    ("real", 1, CBuiltin::RealCast),
    ("absolute", 1, CBuiltin::Function("fabs")),
    ("sign", 1, CBuiltin::Function("rumoca_ir_galec_sign")),
    ("sqrt", 1, CBuiltin::Function("sqrt")),
    ("exp", 1, CBuiltin::Function("exp")),
    ("ln", 1, CBuiltin::Function("log")),
    ("lg", 1, CBuiltin::Function("log10")),
    ("roundDown", 1, CBuiltin::Function("floor")),
    ("roundUp", 1, CBuiltin::Function("ceil")),
    ("sin", 1, CBuiltin::Function("sin")),
    ("cos", 1, CBuiltin::Function("cos")),
    ("tan", 1, CBuiltin::Function("tan")),
    ("asin", 1, CBuiltin::Function("asin")),
    ("acos", 1, CBuiltin::Function("acos")),
    ("atan", 1, CBuiltin::Function("atan")),
    ("atan2", 2, CBuiltin::Function("atan2")),
    ("sinh", 1, CBuiltin::Function("sinh")),
    ("cosh", 1, CBuiltin::Function("cosh")),
    ("tanh", 1, CBuiltin::Function("tanh")),
    // GALEC min/max are the relational two-argument forms (`u1 < u2`
    // selects), NOT C `fmin`/`fmax` (which drop a qNaN operand instead of
    // taking the `else` branch a false comparison implies, traps T9/T14).
    ("min", 2, CBuiltin::Function("rumoca_ir_galec_min")),
    ("max", 2, CBuiltin::Function("rumoca_ir_galec_max")),
    ("imin", 2, CBuiltin::Function("rumoca_ir_galec_imin")),
    ("imax", 2, CBuiltin::Function("rumoca_ir_galec_imax")),
    ("divisionTowardsZero", 2, CBuiltin::IntegerDivision),
];

/// GALEC-statement/-expression → C printer over one package's collision
/// checked C name table.
pub struct CPrinter<'a> {
    names: &'a CNameTable,
}

impl<'a> CPrinter<'a> {
    /// Printer over the package's C name table.
    #[must_use]
    pub fn new(names: &'a CNameTable) -> Self {
        Self { names }
    }

    /// Print one statement as C source lines (one line per emitted C
    /// statement; whole-array assignments expand element-wise, row-major).
    ///
    /// # Errors
    ///
    /// `ET023` for statement kinds the current lowering never emits
    /// (module docs); expression errors propagate.
    pub fn statement_lines(&self, statement: &Statement) -> Result<Vec<String>, GalecTargetError> {
        match statement {
            Statement::Assignment { target, value } => match value {
                Expression::Array(elements) => {
                    let mut lines = Vec::new();
                    self.array_assignment(&self.reference(target)?, elements, &mut lines)?;
                    Ok(lines)
                }
                // A whole-array copy `x := y` (array target, array-valued
                // reference, e.g. the `'previous(x)' := x` pre-commit for an
                // array discrete state): a C array is not assignable with `=`,
                // so copy its contiguous storage with `memcpy`. Both operands
                // have equal dimensions (the GALEC type validator enforces it),
                // so `sizeof(dst)` is the exact copy length.
                Expression::Ref(source)
                    if self.is_whole_array_reference(target)
                        && self.is_whole_array_reference(source) =>
                {
                    let dst = self.reference(target)?;
                    let src = self.reference(source)?;
                    Ok(vec![format!("memcpy({dst}, {src}, sizeof({dst}));")])
                }
                scalar => {
                    if let Some(dimensions) = self.whole_array_dimensions(target) {
                        let dimensions = dimensions.to_vec();
                        let mut lines = Vec::new();
                        self.array_expression_assignment(
                            &self.reference(target)?,
                            &dimensions,
                            scalar,
                            &mut Vec::new(),
                            &mut lines,
                        )?;
                        return Ok(lines);
                    }
                    Ok(vec![format!(
                        "{} = {};",
                        self.reference(target)?,
                        self.expression(scalar)?
                    )])
                }
            },
            Statement::MultiAssignment { .. } => {
                Err(unsupported_statement("a multi-assignment statement"))
            }
            Statement::Call(_) => Err(unsupported_statement("a bare call statement")),
            Statement::If(_) => Err(unsupported_statement("an if statement")),
            Statement::For(_) => Err(unsupported_statement("a for loop")),
            Statement::Limit(_) => Err(unsupported_statement("a limit statement")),
            Statement::Signal(_) => Err(unsupported_statement("a signal statement")),
        }
    }

    /// `target[i][j]… = element;` lines for a whole-array literal, indices
    /// 0-based in nesting order (GALEC constructors and C arrays are both
    /// row-major).
    fn array_assignment(
        &self,
        target: &str,
        elements: &[Expression],
        lines: &mut Vec<String>,
    ) -> Result<(), GalecTargetError> {
        for (index, element) in elements.iter().enumerate() {
            let path = format!("{target}[{index}]");
            match element {
                Expression::Array(nested) => self.array_assignment(&path, nested, lines)?,
                scalar => lines.push(format!("{path} = {};", self.expression(scalar)?)),
            }
        }
        Ok(())
    }

    /// `target[i][j]… = indexed(value);` lines for a whole-array expression
    /// whose GALEC AST stays array-native but whose C expression must be scalar
    /// at each assignment site.
    fn array_expression_assignment(
        &self,
        target: &str,
        dimensions: &[i64],
        value: &Expression,
        indices: &mut Vec<i64>,
        lines: &mut Vec<String>,
    ) -> Result<(), GalecTargetError> {
        let Some((first, rest)) = dimensions.split_first() else {
            let element = self.indexed_expression(value, indices)?;
            lines.push(format!("{target} = {};", self.expression(&element)?));
            return Ok(());
        };
        let size = usize::try_from(*first)
            .ok()
            .filter(|size| *size >= 1)
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!("C export saw non-positive array dimension {first}"),
            })?;
        for index in 0..size {
            let path = format!("{target}[{index}]");
            let one_based =
                i64::try_from(index + 1).map_err(|_| GalecTargetError::LoweringInternal {
                    detail: "C export array index exceeds i64".to_owned(),
                })?;
            indices.push(one_based);
            self.array_expression_assignment(&path, rest, value, indices, lines)?;
            indices.pop();
        }
        Ok(())
    }

    fn indexed_expression(
        &self,
        expression: &Expression,
        indices: &[i64],
    ) -> Result<Expression, GalecTargetError> {
        if indices.is_empty() {
            return Ok(expression.clone());
        }
        match expression {
            Expression::Ref(reference) if self.is_whole_array_reference(reference) => Ok(
                Expression::Ref(self.reference_with_static_subscripts(reference, indices)?),
            ),
            Expression::Ref(_) => Ok(expression.clone()),
            Expression::Neg(reference) if self.is_whole_array_reference(reference) => Ok(
                Expression::Neg(self.reference_with_static_subscripts(reference, indices)?),
            ),
            Expression::Neg(_) => Ok(expression.clone()),
            Expression::Array(elements) => self.indexed_array_element(elements, indices),
            Expression::If(if_expression) => Ok(Expression::If(IfExpression {
                branches: if_expression
                    .branches
                    .iter()
                    .map(|(condition, value)| {
                        Ok((
                            condition.clone(),
                            self.index_value_if_array(value, indices)?,
                        ))
                    })
                    .collect::<Result<Vec<_>, GalecTargetError>>()?,
                else_value: Box::new(
                    self.index_value_if_array(&if_expression.else_value, indices)?,
                ),
            })),
            Expression::Paren(inner) if self.expression_needs_indexing(inner) => Ok(
                Expression::Paren(Box::new(self.indexed_expression(inner, indices)?)),
            ),
            Expression::Binary { op, lhs, rhs } => Ok(Expression::Binary {
                op: *op,
                lhs: Box::new(self.index_value_if_array(lhs, indices)?),
                rhs: Box::new(self.index_value_if_array(rhs, indices)?),
            }),
            Expression::Bool(_)
            | Expression::Integer(_)
            | Expression::Real(_)
            | Expression::Call(_)
            | Expression::Paren(_)
            | Expression::Not(_)
            | Expression::Size { .. } => Ok(expression.clone()),
        }
    }

    fn index_value_if_array(
        &self,
        expression: &Expression,
        indices: &[i64],
    ) -> Result<Expression, GalecTargetError> {
        if self.expression_needs_indexing(expression) {
            self.indexed_expression(expression, indices)
        } else {
            Ok(expression.clone())
        }
    }

    fn indexed_array_element(
        &self,
        elements: &[Expression],
        indices: &[i64],
    ) -> Result<Expression, GalecTargetError> {
        let Some((first, rest)) = indices.split_first() else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "C export array element selection called without indices".to_owned(),
            });
        };
        let index = usize::try_from(*first)
            .ok()
            .filter(|index| *index >= 1 && *index <= elements.len())
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!(
                    "C export array constructor index {first} is outside 1..{}",
                    elements.len()
                ),
            })?;
        let selected = &elements[index - 1];
        if rest.is_empty() {
            return Ok(selected.clone());
        }
        if matches!(selected, Expression::Array(_)) || self.expression_needs_indexing(selected) {
            return self.indexed_expression(selected, rest);
        }
        Err(GalecTargetError::LoweringInternal {
            detail: "C export array constructor rank does not match target dimensions".to_owned(),
        })
    }

    fn expression_needs_indexing(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Ref(reference) | Expression::Neg(reference) => {
                self.is_whole_array_reference(reference)
            }
            Expression::Array(_) => true,
            Expression::If(if_expression) => {
                if_expression
                    .branches
                    .iter()
                    .any(|(_, value)| self.expression_needs_indexing(value))
                    || self.expression_needs_indexing(&if_expression.else_value)
            }
            Expression::Paren(inner) | Expression::Not(inner) => {
                self.expression_needs_indexing(inner)
            }
            Expression::Binary { lhs, rhs, .. } => {
                self.expression_needs_indexing(lhs) || self.expression_needs_indexing(rhs)
            }
            Expression::Bool(_)
            | Expression::Integer(_)
            | Expression::Real(_)
            | Expression::Call(_)
            | Expression::Size { .. } => false,
        }
    }

    fn reference_with_static_subscripts(
        &self,
        reference: &Reference,
        indices: &[i64],
    ) -> Result<Reference, GalecTargetError> {
        let Reference::State(parts) = reference else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "C export can only index whole-array state references".to_owned(),
            });
        };
        let [part] = parts.as_slice() else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "C export can only index single-part state references".to_owned(),
            });
        };
        let mut part = part.clone();
        part.subscripts = indices
            .iter()
            .copied()
            .map(Expression::Integer)
            .collect::<Vec<_>>();
        Ok(Reference::State(vec![part]))
    }

    /// Print one expression as C, fully parenthesized (module docs).
    ///
    /// # Errors
    ///
    /// `ET023` for constructs outside the lowering's emitted shape;
    /// `ET018` for catalog names the lowering cannot emit.
    pub fn expression(&self, expression: &Expression) -> Result<String, GalecTargetError> {
        match expression {
            Expression::Bool(value) => Ok(if *value { "true" } else { "false" }.to_owned()),
            Expression::Integer(value) => integer_literal(*value),
            Expression::Real(value) => real_literal(*value),
            Expression::Ref(reference) => self.reference(reference),
            Expression::Call(call) => self.call(call),
            Expression::Paren(inner) => Ok(format!("({})", self.expression(inner)?)),
            Expression::If(if_expression) => self.ternary(if_expression),
            Expression::Neg(reference) => Ok(format!("(-{})", self.reference(reference)?)),
            Expression::Not(inner) => Ok(format!("(!({}))", self.expression(inner)?)),
            Expression::Binary { op, lhs, rhs } => self.binary(*op, lhs, rhs),
            Expression::Size { .. } => Err(GalecTargetError::CExportUnsupported {
                construct: "a `size(…)` expression",
                detail: "the current DAE lowering never emits dimension queries".to_owned(),
            }),
            Expression::Array(_) => Err(GalecTargetError::CExportUnsupported {
                construct: "an array constructor outside a whole-array assignment",
                detail: "C has no array-valued expressions; only direct \
                         `target := {…}` assignments expand element-wise"
                    .to_owned(),
            }),
        }
    }

    fn binary(
        &self,
        op: BinaryOp,
        lhs: &Expression,
        rhs: &Expression,
    ) -> Result<String, GalecTargetError> {
        let left = self.expression(lhs)?;
        let right = self.expression(rhs)?;
        if op == BinaryOp::Pow {
            return Ok(format!("pow({left}, {right})"));
        }
        let token = match op {
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::Ne => "!=",
            // `+ - * / < > <= >= ==` share their C spelling.
            other => other.token(),
        };
        Ok(format!("({left} {token} {right})"))
    }

    /// If-expression → right-nested C conditional, one `?:` per branch.
    fn ternary(&self, if_expression: &IfExpression) -> Result<String, GalecTargetError> {
        let mut out = self.expression(&if_expression.else_value)?;
        for (condition, value) in if_expression.branches.iter().rev() {
            out = format!(
                "({} ? {} : {})",
                self.expression(condition)?,
                self.expression(value)?,
                out
            );
        }
        Ok(out)
    }

    fn call(&self, call: &FunctionCall) -> Result<String, GalecTargetError> {
        let Name::Ident(function, _) = &call.function else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "C export met a call to a quoted function name; the lowering \
                         only emits plain-identifier catalog builtins"
                    .to_owned(),
            });
        };
        let Some((_, arity, form)) = C_BUILTIN_MAP
            .iter()
            .find(|(name, _, _)| *name == function.as_str())
        else {
            return Err(GalecTargetError::LoweringInternal {
                detail: format!(
                    "C export has no mapping for the call target `{}`; the lowering \
                     only emits the emittable §3.2.6 catalog subset",
                    function.as_str()
                ),
            });
        };
        if call.arguments.len() != *arity {
            return Err(GalecTargetError::LoweringInternal {
                detail: format!(
                    "C export met `{}` with {} argument(s), expected {arity}",
                    function.as_str(),
                    call.arguments.len()
                ),
            });
        }
        let arguments = call
            .arguments
            .iter()
            .map(|argument| self.expression(argument))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(match form {
            CBuiltin::Function(c_name) => format!("{c_name}({})", arguments.join(", ")),
            CBuiltin::RealCast => format!("((double)({}))", arguments[0]),
            CBuiltin::IntegerDivision => format!("({} / {})", arguments[0], arguments[1]),
        })
    }

    /// Literal dimensions when `reference` is a whole-array state reference:
    /// a single-part `self.x` reference with NO subscripts whose declaration is
    /// an array. Indexed elements and scalars return `None`.
    fn whole_array_dimensions(&self, reference: &Reference) -> Option<&[i64]> {
        let Reference::State(parts) = reference else {
            return None;
        };
        let [part] = parts.as_slice() else {
            return None;
        };
        if part.subscripts.is_empty() {
            self.names.array_dimensions(&part.name)
        } else {
            None
        }
    }

    /// Whether `reference` is a whole-array state reference.
    fn is_whole_array_reference(&self, reference: &Reference) -> bool {
        self.whole_array_dimensions(reference).is_some()
    }

    /// `self.x[i]` → `self->x[i-1]` struct member access on the block-state
    /// pointer.
    fn reference(&self, reference: &Reference) -> Result<String, GalecTargetError> {
        let Reference::State(parts) = reference else {
            return Err(GalecTargetError::CExportUnsupported {
                construct: "a local (non-`self.`) reference",
                detail: "the current DAE lowering emits no method locals or loop \
                         iterators"
                    .to_owned(),
            });
        };
        let [part] = parts.as_slice() else {
            return Err(GalecTargetError::CExportUnsupported {
                construct: "a multi-part state reference",
                detail: "the current DAE lowering emits no state compartments".to_owned(),
            });
        };
        self.ref_part(part)
    }

    fn ref_part(&self, part: &RefPart) -> Result<String, GalecTargetError> {
        let mut out = format!("self->{}", self.names.c_name(&part.name)?);
        for subscript in &part.subscripts {
            out.push('[');
            out.push_str(&self.zero_based(subscript)?);
            out.push(']');
        }
        Ok(out)
    }

    /// One GALEC 1-based subscript as a C 0-based index.
    fn zero_based(&self, subscript: &Expression) -> Result<String, GalecTargetError> {
        match subscript {
            Expression::Integer(value) => {
                if *value < 1 {
                    return Err(GalecTargetError::LoweringInternal {
                        detail: format!(
                            "C export met the GALEC subscript {value}; valid subscripts \
                             are 1-based positive integers"
                        ),
                    });
                }
                Ok((value - 1).to_string())
            }
            other => Ok(format!("({} - 1)", self.expression(other)?)),
        }
    }
}

/// GALEC Integer is C `int32_t`: literals outside its range are rejected,
/// never truncated (SPEC_0008).
fn integer_literal(value: i64) -> Result<String, GalecTargetError> {
    if i32::try_from(value).is_err() {
        return Err(GalecTargetError::CExportUnsupported {
            construct: "an Integer literal beyond int32_t",
            detail: format!("literal {value} does not fit the C Integer type int32_t"),
        });
    }
    if value < 0 {
        Ok(format!("({value})"))
    } else {
        Ok(value.to_string())
    }
}

/// Strict GALEC Real spelling (trap T7) doubles as the C literal; negative
/// values are parenthesized for operand safety.
fn real_literal(value: f64) -> Result<String, GalecTargetError> {
    let text = rumoca_ir_galec::format_real_literal(value).map_err(|error| {
        GalecTargetError::LoweringInternal {
            detail: format!("C export met an unprintable Real literal: {error}"),
        }
    })?;
    if value.is_sign_negative() {
        Ok(format!("({text})"))
    } else {
        Ok(text)
    }
}

fn unsupported_statement(construct: &'static str) -> GalecTargetError {
    GalecTargetError::CExportUnsupported {
        construct,
        detail: "the current DAE lowering emits sequential assignments only \
                 (crate::lower); this statement kind cannot have come from it"
            .to_owned(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_galec::ast::{
        Block, Dimension, ProtectedEntity, ProtectedKind, ScalarType, VariableDeclaration,
    };

    fn table(names: &[&str]) -> CNameTable {
        table_with_dims(
            &names
                .iter()
                .map(|name| (*name, Vec::new()))
                .collect::<Vec<_>>(),
        )
    }

    fn array_table(names: &[(&str, &[i64])]) -> CNameTable {
        table_with_dims(
            &names
                .iter()
                .map(|(name, dims)| (*name, dims.to_vec()))
                .collect::<Vec<_>>(),
        )
    }

    fn table_with_dims(names: &[(&str, Vec<i64>)]) -> CNameTable {
        let mut block = Block::new(Name::ident("M"));
        block.protected = names
            .iter()
            .map(|(name, dims)| {
                let mut decl = VariableDeclaration::scalar(
                    ScalarType::Real,
                    crate::mangle::galec_variable_name(name).unwrap(),
                );
                decl.dimensions = dims
                    .iter()
                    .copied()
                    .map(|size| Dimension::Expr(Expression::Integer(size)))
                    .collect();
                ProtectedEntity {
                    kind: ProtectedKind::State,
                    decl,
                    start: None,
                }
            })
            .collect();
        CNameTable::build(&block).unwrap()
    }

    fn state(name: &str) -> Expression {
        Expression::Ref(Reference::State(vec![RefPart::plain(
            crate::mangle::galec_variable_name(name).unwrap(),
        )]))
    }

    fn print(names: &[&str], expression: &Expression) -> String {
        let table = table(names);
        CPrinter::new(&table).expression(expression).unwrap()
    }

    #[test]
    fn operators_map_and_stay_fully_parenthesized() {
        // (a + b) * c, as the lowering shapes it (AST order is normative).
        let expr = Expression::binary(
            BinaryOp::Mul,
            Expression::binary(BinaryOp::Add, state("a"), state("b")),
            state("c"),
        );
        assert_eq!(
            print(&["a", "b", "c"], &expr),
            "((self->a + self->b) * self->c)"
        );

        let logic = Expression::binary(
            BinaryOp::Or,
            Expression::binary(BinaryOp::Ne, state("a"), state("b")),
            Expression::Not(Box::new(Expression::binary(
                BinaryOp::And,
                Expression::Bool(true),
                state("c"),
            ))),
        );
        assert_eq!(
            print(&["a", "b", "c"], &logic),
            "((self->a != self->b) || (!((true && self->c))))"
        );
    }

    #[test]
    fn power_prints_as_pow_call() {
        let expr = Expression::binary(BinaryOp::Pow, state("a"), Expression::Real(2.0));
        assert_eq!(print(&["a"], &expr), "pow(self->a, 2.0)");
    }

    #[test]
    fn nested_negation_forms_stay_grouped() {
        // The trap-T4 rewrite `0.0 - (expr)` around a negated reference.
        let expr = Expression::negated_real(Expression::binary(
            BinaryOp::Sub,
            Expression::negated_real(state("x")),
            Expression::Real(1.0),
        ));
        assert_eq!(print(&["x"], &expr), "(0.0 - (((-self->x) - 1.0)))");
    }

    #[test]
    fn if_expression_prints_as_right_nested_ternary() {
        let expr = Expression::If(IfExpression {
            branches: vec![
                (Expression::Bool(true), Expression::Real(1.0)),
                (state("c"), Expression::Real(2.0)),
            ],
            else_value: Box::new(Expression::Real(3.0)),
        });
        assert_eq!(print(&["c"], &expr), "(true ? 1.0 : (self->c ? 2.0 : 3.0))");
    }

    #[test]
    fn literals_print_strictly() {
        assert_eq!(print(&[], &Expression::Real(0.1)), "0.1");
        assert_eq!(print(&[], &Expression::Real(-1.5)), "(-1.5)");
        assert_eq!(print(&[], &Expression::Real(1.0e21)), "1.0e+21");
        assert_eq!(print(&[], &Expression::Integer(7)), "7");
        assert_eq!(print(&[], &Expression::Integer(-7)), "(-7)");
        assert_eq!(print(&[], &Expression::Bool(false)), "false");
    }

    #[test]
    fn integer_literals_beyond_int32_are_rejected() {
        let table = table(&[]);
        let error = CPrinter::new(&table)
            .expression(&Expression::Integer(i64::from(i32::MAX) + 1))
            .unwrap_err();
        assert_eq!(error.code(), "ET023", "{error}");
    }

    #[test]
    fn subscripts_shift_to_zero_based() {
        let expr = Expression::Ref(Reference::State(vec![RefPart {
            name: Name::ident("x"),
            subscripts: vec![Expression::Integer(2)],
            span: rumoca_core::Span::DUMMY,
        }]));
        assert_eq!(print(&["x"], &expr), "self->x[1]");

        let dynamic = Expression::Ref(Reference::State(vec![RefPart {
            name: Name::ident("x"),
            subscripts: vec![state("i")],
            span: rumoca_core::Span::DUMMY,
        }]));
        assert_eq!(print(&["x", "i"], &dynamic), "self->x[(self->i - 1)]");
    }

    #[test]
    fn quoted_names_print_through_the_mangled_table() {
        // A scalarized hierarchical name declares as a quoted identifier
        // and prints through its collision-checked C mangling.
        assert_eq!(
            print(&["motor.emf.v"], &state("motor.emf.v")),
            "self->motor_emf_v"
        );

        let previous_name = crate::mangle::pre_state_name("y").unwrap();
        let mut block = Block::new(Name::ident("M"));
        block.protected = vec![ProtectedEntity {
            kind: ProtectedKind::State,
            decl: VariableDeclaration::scalar(ScalarType::Real, previous_name.clone()),
            start: None,
        }];
        let table = CNameTable::build(&block).unwrap();
        let previous = Expression::Ref(Reference::State(vec![RefPart::plain(previous_name)]));
        assert_eq!(
            CPrinter::new(&table).expression(&previous).unwrap(),
            "self->previous_y"
        );
    }

    #[test]
    fn every_emittable_builtin_has_a_c_mapping_with_matching_arity() {
        let table = table(&["a", "b"]);
        let printer = CPrinter::new(&table);
        for (name, arity) in crate::lower::expr::emittable_builtin_targets() {
            let (_, mapped_arity, _) = C_BUILTIN_MAP
                .iter()
                .find(|(mapped, _, _)| *mapped == name)
                .unwrap_or_else(|| panic!("emittable builtin `{name}` has no C mapping"));
            assert_eq!(*mapped_arity, arity, "arity drift for `{name}`");
            let call = Expression::Call(FunctionCall {
                function: Name::ident(name),
                arguments: (0..arity).map(|_| state("a")).collect(),
            });
            let printed = printer.expression(&call).unwrap();
            assert!(!printed.is_empty());
        }
    }

    #[test]
    fn specific_builtins_take_their_c99_spelling() {
        let one_arg = |name: &str| {
            Expression::Call(FunctionCall {
                function: Name::ident(name),
                arguments: vec![state("a")],
            })
        };
        assert_eq!(print(&["a"], &one_arg("absolute")), "fabs(self->a)");
        assert_eq!(print(&["a"], &one_arg("ln")), "log(self->a)");
        assert_eq!(print(&["a"], &one_arg("lg")), "log10(self->a)");
        assert_eq!(print(&["a"], &one_arg("roundDown")), "floor(self->a)");
        assert_eq!(print(&["a"], &one_arg("roundUp")), "ceil(self->a)");
        assert_eq!(print(&["a"], &one_arg("real")), "((double)(self->a))");
        assert_eq!(
            print(&["a"], &one_arg("sign")),
            "rumoca_ir_galec_sign(self->a)"
        );
        let division = Expression::Call(FunctionCall {
            function: Name::ident("divisionTowardsZero"),
            arguments: vec![state("a"), state("b")],
        });
        assert_eq!(print(&["a", "b"], &division), "(self->a / self->b)");
    }

    #[test]
    fn unmapped_call_targets_are_a_loud_projection_bug() {
        let table = table(&["a"]);
        let call = Expression::Call(FunctionCall {
            function: Name::ident("luFactorize"),
            arguments: vec![state("a")],
        });
        let error = CPrinter::new(&table).expression(&call).unwrap_err();
        assert_eq!(error.code(), "ET018", "{error}");
    }

    #[test]
    fn whole_array_assignments_expand_row_major() {
        let table = table(&["x"]);
        let statement = Statement::Assignment {
            target: Reference::State(vec![RefPart::plain(Name::ident("x"))]),
            value: Expression::Array(vec![
                Expression::Array(vec![Expression::Real(1.0), Expression::Real(2.0)]),
                Expression::Array(vec![Expression::Real(3.0), Expression::Real(4.0)]),
            ]),
        };
        assert_eq!(
            CPrinter::new(&table).statement_lines(&statement).unwrap(),
            vec![
                "self->x[0][0] = 1.0;",
                "self->x[0][1] = 2.0;",
                "self->x[1][0] = 3.0;",
                "self->x[1][1] = 4.0;",
            ]
        );
    }

    #[test]
    fn whole_array_binary_assignments_expand_by_target_dimensions() {
        let table = array_table(&[("y", &[3]), ("a", &[3]), ("b", &[3])]);
        let statement = Statement::Assignment {
            target: Reference::state(Name::ident("y")),
            value: Expression::binary(BinaryOp::Sub, state("a"), state("b")),
        };
        assert_eq!(
            CPrinter::new(&table).statement_lines(&statement).unwrap(),
            vec![
                "self->y[0] = (self->a[0] - self->b[0]);",
                "self->y[1] = (self->a[1] - self->b[1]);",
                "self->y[2] = (self->a[2] - self->b[2]);",
            ]
        );
    }

    #[test]
    fn whole_array_if_assignments_index_branch_values() {
        let table = array_table(&[("y", &[2]), ("a", &[2]), ("b", &[2]), ("c", &[])]);
        let statement = Statement::Assignment {
            target: Reference::state(Name::ident("y")),
            value: Expression::If(IfExpression {
                branches: vec![(state("c"), state("a"))],
                else_value: Box::new(state("b")),
            }),
        };
        assert_eq!(
            CPrinter::new(&table).statement_lines(&statement).unwrap(),
            vec![
                "self->y[0] = (self->c ? self->a[0] : self->b[0]);",
                "self->y[1] = (self->c ? self->a[1] : self->b[1]);",
            ]
        );
    }

    #[test]
    fn non_assignment_statements_are_rejected_with_et023() {
        let table = table(&[]);
        let statement = Statement::Signal(vec![rumoca_ir_galec::ast::Identifier::new("NAN")]);
        let error = CPrinter::new(&table)
            .statement_lines(&statement)
            .unwrap_err();
        assert_eq!(error.code(), "ET023");
        assert!(error.to_string().contains("signal statement"), "{error}");
    }

    #[test]
    fn array_constructors_outside_assignments_are_rejected() {
        let table = table(&[]);
        let error = CPrinter::new(&table)
            .expression(&Expression::Array(vec![Expression::Real(1.0)]))
            .unwrap_err();
        assert_eq!(error.code(), "ET023", "{error}");
    }
}
