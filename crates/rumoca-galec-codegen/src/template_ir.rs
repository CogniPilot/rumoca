//! Language-neutral, template-walkable context over a validated GALEC block
//! (SPEC_0034 D16/D17): the target template — not a typed printer —
//! generates the code by walking this tree with recursive minijinja macros,
//! exactly like the generic IR targets walk their serialized IR. Every
//! GALEC-rendering target consumes this one context: the `.alg` text
//! itself, the embedded C track, and the embedded Rust track.
//!
//! The pass owns everything semantic, so templates stay token-level:
//!
//! - names carry BOTH spellings: `galec_name` is the exact `.alg` token
//!   (quoted identifiers keep their quotes, e.g. `'previous(x)'`) and
//!   `base_name` is the collision-checked identifier stem for
//!   curly-brace languages (non-alphanumeric → `_`, trailing `_` trimmed,
//!   distinctness enforced). Keyword escaping is the template's job (each
//!   language appends `_` against its own keyword list); because base
//!   names never end in `_`, a template-appended `_` cannot re-collide;
//! - subscripts and projection indices are the GALEC truth — **1-based**;
//!   0-based languages subtract in the template (`[{{ i - 1 }}]`);
//! - Real literals arrive as T7-strict text (`1.0e+5` — valid in GALEC, C,
//!   and Rust alike) plus a `negative` flag for operand parenthesization;
//!   Integer literals are validated against the GALEC Integer range (i32);
//! - expressions are `kind`-tagged nodes (`binary` ops are abstract names —
//!   `add`/`pow`/… — each template maps its own spellings and precedence);
//! - whole-array assignments carry BOTH the structural value tree and a
//!   pre-projected element list (`indices` + scalar expression per
//!   element), so a language with array values (GALEC, Rust) assigns
//!   wholesale while a language without them (C) expands element-wise —
//!   no projection logic in any template;
//! - the GAL-029 `solve` statement is its own kind, with the same dual
//!   value/element forms for its matrix and vector operands (the `.alg`
//!   template reconstructs the catalog call; embedded templates
//!   materialize scratch and accumulate the status word);
//! - variables walk the block's declaration order and carry their section
//!   (`interface`/`protected`) and GALEC declaration `prefix`
//!   (`input `/`output `/`parameter `/`constant `/``) so the `.alg`
//!   template reproduces the declaration lists exactly.
//!
//! Statement kinds: `assign` (scalar target), `assign_whole` (whole-array
//! target), `solve`. Expression kinds: `bool`, `int`, `real`, `ref`,
//! `neg`, `not`, `paren`, `binary`, `if`, `call` (GALEC §3.2.6 catalog
//! name), `array`.

use std::collections::HashMap;

use serde_json::{Value, json};

use rumoca_ir_galec::ast::{
    BinaryOp, Block, Expression, IfExpression, InterfaceKind, Name, ProtectedKind, Reference,
    ScalarType, Spanned, Statement, TypeRef, VariableDeclaration,
};

use crate::diagnostic::GalecTargetError;
use crate::emit::{c_comment_text, status_abi, validate_block};
use crate::mangle::manifest_name;
use crate::package::AlgorithmCodePackage;

/// Serialize the template-walkable context for one Algorithm Code package
/// (D16), joining the authoritative manifest ids/causalities. The block is
/// re-validated first (GAL-004: no rendering path consumes an un-validated
/// package).
///
/// # Errors
///
/// `ET022` on base-identifier collisions, `ET023` for GALEC constructs the
/// export shape does not cover, `ET018` for validator rejections.
pub fn galec_template_context(
    package: &AlgorithmCodePackage,
    model_name: &str,
) -> Result<Value, GalecTargetError> {
    let mut manifest_info = HashMap::new();
    for variable in &package.manifest.variables {
        let common = variable.common();
        manifest_info.insert(
            common.name.as_str().to_owned(),
            (
                common.id.as_str().to_owned(),
                common.block_causality.as_str(),
            ),
        );
    }
    context_with_info(&package.block, model_name, |spelling| {
        manifest_info
            .get(spelling)
            .cloned()
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!("declared variable `{spelling}` has no manifest entry to join"),
            })
    })
}

/// [`galec_template_context`] directly from a (possibly hand-written,
/// already-parsed) GALEC block — the editor path: positional `V1…` ids and
/// declaration-kind causalities are synthesized exactly like the manifest
/// builder would.
///
/// # Errors
///
/// Those of [`galec_template_context`].
pub fn galec_template_context_for_block(
    block: &Block,
    model_name: &str,
) -> Result<Value, GalecTargetError> {
    let mut ordinal = 0usize;
    let mut synthesized = HashMap::new();
    for (decl, _, causality) in declared_entities(block) {
        ordinal += 1;
        synthesized.insert(
            manifest_name(&decl.name).to_owned(),
            (format!("V{ordinal}"), causality),
        );
    }
    context_with_info(block, model_name, |spelling| {
        synthesized
            .get(spelling)
            .cloned()
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!("declared variable `{spelling}` lost its synthesized id"),
            })
    })
}

/// Declared entities in block order with their `.alg` declaration prefix
/// and manifest `blockCausality` literal.
fn declared_entities(
    block: &Block,
) -> impl Iterator<Item = (&VariableDeclaration, DeclarationPlace, &'static str)> {
    let interface = block.interface.iter().map(|variable| {
        let (prefix, causality) = match variable.kind {
            InterfaceKind::Input => ("input ", "input"),
            InterfaceKind::Output => ("output ", "output"),
            InterfaceKind::TunableParameter => ("parameter ", "tunableParameter"),
        };
        (
            &variable.decl,
            DeclarationPlace {
                section: "interface",
                prefix,
            },
            causality,
        )
    });
    let protected = block.protected.iter().map(|entity| {
        let (prefix, causality) = match entity.kind {
            ProtectedKind::DependentParameter => ("parameter ", "dependentParameter"),
            ProtectedKind::Constant => ("constant ", "constant"),
            ProtectedKind::State => ("", "state"),
        };
        (
            &entity.decl,
            DeclarationPlace {
                section: "protected",
                prefix,
            },
            causality,
        )
    });
    interface.chain(protected)
}

#[derive(Clone, Copy)]
struct DeclarationPlace {
    section: &'static str,
    prefix: &'static str,
}

fn context_with_info(
    block: &Block,
    model_name: &str,
    info: impl Fn(&str) -> Result<(String, &'static str), GalecTargetError>,
) -> Result<Value, GalecTargetError> {
    validate_block(block)?;
    reject_unrepresented(block)?;
    let names = BaseNames::build(block)?;
    let mut variables = Vec::new();
    for (decl, place, _) in declared_entities(block) {
        let spelling = manifest_name(&decl.name);
        let TypeRef::Primitive(scalar) = &decl.ty else {
            return Err(GalecTargetError::CExportUnsupported {
                construct: "a compartment-typed declaration",
                detail: "the current DAE lowering (crate::lower) never emits this construct"
                    .to_owned(),
            });
        };
        let (id, causality) = info(spelling)?;
        variables.push(json!({
            "name": c_comment_text(spelling),
            "galec_name": galec_token(&decl.name),
            "base_name": names.base_by_spelling(spelling)?,
            "id": id,
            "causality": causality,
            "section": place.section,
            "prefix": place.prefix,
            "scalar": scalar_name(*scalar),
            "dimensions": crate::c_mangle::literal_dimensions(&decl.dimensions)?,
        }));
    }
    let walker = StatementWalker { names: &names };
    Ok(json!({
        "model_name": model_name,
        "block_name": c_comment_text(&crate::emit::block_display_name(&block.name)),
        "galec_name": galec_token(&block.name),
        "base_name": base_identifier_of(&block.name),
        "status_abi": status_abi(block),
        "variables": variables,
        "methods": {
            "startup": walker.method(&block.startup.statements, &block.startup.signals)?,
            "recalibrate":
                walker.method(&block.recalibrate.statements, &block.recalibrate.signals)?,
            "do_step": walker.method(&block.do_step.statements, &block.do_step.signals)?,
        },
    }))
}

/// Loud rejections for block features the context does not (yet) model —
/// the current lowering never emits them, and silently dropping them would
/// miscompile (GAL-007).
pub(crate) fn reject_unrepresented(block: &Block) -> Result<(), GalecTargetError> {
    let unsupported = |construct: &'static str| GalecTargetError::CExportUnsupported {
        construct,
        detail: "the current DAE lowering (crate::lower) never emits this construct".to_owned(),
    };
    if !block.compartments.is_empty() {
        return Err(unsupported("record state compartments"));
    }
    if !block.error_signals.is_empty() {
        return Err(unsupported("user-defined error signals"));
    }
    if !block.protected_functions.is_empty() || !block.public_functions.is_empty() {
        return Err(unsupported("user-defined functions"));
    }
    for method in [&block.startup, &block.recalibrate, &block.do_step] {
        if !method.locals.is_empty() {
            return Err(unsupported("method-local variables"));
        }
    }
    for (decl, _, _) in declared_entities(block) {
        if !decl.range.is_empty() {
            return Err(unsupported("declaration (min, max) range attributes"));
        }
    }
    Ok(())
}

fn scalar_name(scalar: ScalarType) -> &'static str {
    match scalar {
        ScalarType::Real => "Real",
        ScalarType::Integer => "Integer",
        ScalarType::Boolean => "Boolean",
    }
}

/// The exact `.alg` token of a GALEC name: plain identifiers verbatim,
/// quoted identifiers with their quotes.
fn galec_token(name: &Name) -> String {
    match name {
        Name::Ident(ident, _) => ident.as_str().to_owned(),
        Name::Quoted(content, _) => format!("'{content}'"),
    }
}

/// The language-neutral half of name mangling: normalize the manifest
/// spelling to a base identifier, or `None` when the spelling cannot begin
/// one (quoted GALEC names have looser lexical rules than identifiers —
/// such names print fine in `.alg` but embedded templates must `fail()` on
/// a `null` base name instead of emitting an illegal identifier). Keyword
/// escaping is deliberately absent (template-owned, module docs).
fn base_identifier_of(name: &Name) -> Option<String> {
    let spelling = manifest_name(name);
    let first_is_letter = spelling
        .chars()
        .next()
        .is_some_and(|first| first.is_ascii_alphabetic());
    if !first_is_letter {
        return None;
    }
    let mut base: String = spelling
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect();
    while base.ends_with('_') {
        base.pop();
    }
    Some(base)
}

/// Collision-checked base identifiers plus declared array dimensions over
/// one block's variables.
struct BaseNames {
    by_spelling: HashMap<String, Option<String>>,
    array_dimensions: HashMap<String, Vec<i64>>,
}

impl BaseNames {
    fn build(block: &Block) -> Result<Self, GalecTargetError> {
        let mut by_spelling = HashMap::new();
        let mut array_dimensions = HashMap::new();
        let mut owners: HashMap<String, String> = HashMap::new();
        for (decl, _, _) in declared_entities(block) {
            let spelling = manifest_name(&decl.name).to_owned();
            let base = base_identifier_of(&decl.name);
            if let Some(base) = &base {
                claim_base(&mut owners, base, &spelling)?;
            }
            if !decl.dimensions.is_empty() {
                array_dimensions.insert(
                    spelling.clone(),
                    crate::c_mangle::literal_dimensions(&decl.dimensions)?,
                );
            }
            by_spelling.insert(spelling, base);
        }
        Ok(Self {
            by_spelling,
            array_dimensions,
        })
    }

    fn base_by_spelling(&self, spelling: &str) -> Result<Option<&str>, GalecTargetError> {
        self.by_spelling
            .get(spelling)
            .map(|base| base.as_deref())
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!(
                    "template context met a reference to `{spelling}`, which the block \
                     never declared"
                ),
            })
    }

    fn base(&self, name: &Name) -> Result<Option<&str>, GalecTargetError> {
        self.base_by_spelling(manifest_name(name))
    }

    fn dimensions(&self, name: &Name) -> Option<&[i64]> {
        self.array_dimensions
            .get(manifest_name(name))
            .map(Vec::as_slice)
    }
}

/// Record `base` as owned by `spelling`, failing on a cross-spelling
/// collision (ET022).
fn claim_base(
    owners: &mut HashMap<String, String>,
    base: &str,
    spelling: &str,
) -> Result<(), GalecTargetError> {
    if let Some(first) = owners.get(base)
        && first != spelling
    {
        return Err(GalecTargetError::CNameCollision {
            first: first.clone(),
            second: spelling.to_owned(),
            c_name: base.to_owned(),
        });
    }
    owners.insert(base.to_owned(), spelling.to_owned());
    Ok(())
}

struct StatementWalker<'a> {
    names: &'a BaseNames,
}

impl StatementWalker<'_> {
    fn method(
        &self,
        statements: &[Spanned<Statement>],
        signals: &[rumoca_ir_galec::ast::PredefinedSignal],
    ) -> Result<Value, GalecTargetError> {
        let statements = statements
            .iter()
            .map(|statement| self.statement(&statement.node))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(json!({
            "signals": signals.iter().map(|signal| signal.name()).collect::<Vec<_>>(),
            "statements": statements,
        }))
    }

    fn statement(&self, statement: &Statement) -> Result<Value, GalecTargetError> {
        match statement {
            Statement::Assignment { target, value } => {
                if let Expression::Call(call) = value
                    && matches!(&call.function, Name::Ident(f, _) if f.as_str() == "solveLinearEquations")
                {
                    return self.solve(target, &call.arguments);
                }
                match self.whole_array_dimensions(target) {
                    Some(dimensions) => {
                        let dimensions = dimensions.to_vec();
                        Ok(json!({
                            "kind": "assign_whole",
                            "target": self.reference(target)?,
                            "dimensions": dimensions,
                            "value": self.expression(value)?,
                            // Whole-array reference copy (never a scalar
                            // broadcast): languages with array values assign
                            // directly, C memcpys.
                            "copy": self.is_whole_array_copy(value),
                            "elements": self.projected_elements(&dimensions, value)?,
                        }))
                    }
                    None => Ok(json!({
                        "kind": "assign",
                        "target": self.reference(target)?,
                        "value": self.expression(value)?,
                    })),
                }
            }
            Statement::MultiAssignment { .. } => Err(unsupported("a multi-assignment statement")),
            Statement::Call(_) => Err(unsupported("a bare call statement")),
            Statement::If(_) => Err(unsupported("an if statement")),
            Statement::For(_) => Err(unsupported("a for loop")),
            Statement::Limit(_) => Err(unsupported("a limit statement")),
            Statement::Signal(_) => Err(unsupported("a signal statement")),
        }
    }

    /// `x := solveLinearEquations(A, b);` (GAL-029): both operands carry
    /// the dual value/element forms so embedded templates can materialize
    /// scratch copies without projection logic, while the `.alg` template
    /// reconstructs the catalog call from the value trees.
    fn solve(
        &self,
        target: &Reference,
        arguments: &[Expression],
    ) -> Result<Value, GalecTargetError> {
        let [a, b] = arguments else {
            return Err(GalecTargetError::LoweringInternal {
                detail: format!(
                    "template context met `solveLinearEquations` with {} argument(s), \
                     expected 2",
                    arguments.len()
                ),
            });
        };
        let Some(&[n]) = self.whole_array_dimensions(target) else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "template context needs a whole-vector target for \
                         `solveLinearEquations`"
                    .to_owned(),
            });
        };
        Ok(json!({
            "kind": "solve",
            "n": n,
            "target": self.reference(target)?,
            "a": self.expression(a)?,
            "a_copy": self.is_whole_array_copy(a),
            "a_elements": self.projected_elements(&[n, n], a)?,
            "b": self.expression(b)?,
            "b_copy": self.is_whole_array_copy(b),
            "b_elements": self.projected_elements(&[n], b)?,
        }))
    }

    /// Row-major element projection of a whole-array value: one
    /// `{ indices: [1-based…], value: <scalar expr> }` entry per element.
    fn projected_elements(
        &self,
        dimensions: &[i64],
        value: &Expression,
    ) -> Result<Vec<Value>, GalecTargetError> {
        let mut elements = Vec::new();
        self.project(dimensions, value, &mut Vec::new(), &mut elements)?;
        Ok(elements)
    }

    fn project(
        &self,
        dimensions: &[i64],
        value: &Expression,
        indices: &mut Vec<i64>,
        elements: &mut Vec<Value>,
    ) -> Result<(), GalecTargetError> {
        let Some((first, rest)) = dimensions.split_first() else {
            let projected = self.indexed_expression(value, indices)?;
            elements.push(json!({
                "indices": indices.clone(),
                "value": self.expression(&projected)?,
            }));
            return Ok(());
        };
        let size = usize::try_from(*first)
            .ok()
            .filter(|size| *size >= 1)
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!("template context saw non-positive array dimension {first}"),
            })?;
        for index in 1..=size {
            indices.push(
                i64::try_from(index).map_err(|_| GalecTargetError::LoweringInternal {
                    detail: "template context array index exceeds i64".to_owned(),
                })?,
            );
            self.project(rest, value, indices, elements)?;
            indices.pop();
        }
        Ok(())
    }

    /// Scalar projection of an array-native expression at 1-based `indices`
    /// (the same rules the lowering uses: whole-array references subscript,
    /// constructors select, binaries/ifs distribute, scalars pass through).
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
                Expression::Ref(reference_with_static_subscripts(reference, indices)?),
            ),
            Expression::Ref(_) => Ok(expression.clone()),
            Expression::Neg(reference) if self.is_whole_array_reference(reference) => Ok(
                Expression::Neg(reference_with_static_subscripts(reference, indices)?),
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
                detail: "template context array element selection called without indices"
                    .to_owned(),
            });
        };
        let element = usize::try_from(*first)
            .ok()
            .and_then(|index| index.checked_sub(1))
            .and_then(|index| elements.get(index))
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!(
                    "template context array element index {first} is outside the constructor"
                ),
            })?;
        if rest.is_empty() && !self.expression_needs_indexing(element) {
            return Ok(element.clone());
        }
        if matches!(element, Expression::Array(_)) || self.expression_needs_indexing(element) {
            return self.indexed_expression(element, rest);
        }
        Err(GalecTargetError::LoweringInternal {
            detail: "template context array constructor rank does not match target dimensions"
                .to_owned(),
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

    fn whole_array_dimensions(&self, reference: &Reference) -> Option<&[i64]> {
        let Reference::State(parts) = reference else {
            return None;
        };
        let [part] = parts.as_slice() else {
            return None;
        };
        if part.subscripts.is_empty() {
            self.names.dimensions(&part.name)
        } else {
            None
        }
    }

    fn is_whole_array_reference(&self, reference: &Reference) -> bool {
        self.whole_array_dimensions(reference).is_some()
    }

    /// Whether a whole-assignment value is a whole-array reference (a
    /// direct copy — never a scalar broadcast).
    fn is_whole_array_copy(&self, value: &Expression) -> bool {
        matches!(value, Expression::Ref(reference) if self.is_whole_array_reference(reference))
    }

    // -----------------------------------------------------------------
    // Expression nodes
    // -----------------------------------------------------------------

    fn expression(&self, expression: &Expression) -> Result<Value, GalecTargetError> {
        match expression {
            Expression::Bool(value) => Ok(json!({ "kind": "bool", "value": value })),
            Expression::Integer(value) => {
                // GALEC Integer is 32-bit (§3.1.6); reject over-range
                // literals once for every target language (SPEC_0008:
                // never truncated).
                if i32::try_from(*value).is_err() {
                    return Err(GalecTargetError::CExportUnsupported {
                        construct: "an Integer literal beyond the GALEC Integer range",
                        detail: format!("literal {value} does not fit 32 bits"),
                    });
                }
                Ok(json!({ "kind": "int", "value": value }))
            }
            Expression::Real(value) => {
                let text = rumoca_ir_galec::format_real_literal(*value).map_err(|error| {
                    GalecTargetError::LoweringInternal {
                        detail: format!(
                            "template context met an unprintable Real literal: {error}"
                        ),
                    }
                })?;
                Ok(json!({
                    "kind": "real",
                    "text": text,
                    "negative": value.is_sign_negative(),
                }))
            }
            Expression::Ref(reference) => self.reference(reference),
            Expression::Neg(reference) => Ok(json!({
                "kind": "neg",
                "value": self.reference(reference)?,
            })),
            Expression::Not(inner) => Ok(json!({
                "kind": "not",
                "value": self.expression(inner)?,
            })),
            Expression::Paren(inner) => Ok(json!({
                "kind": "paren",
                "value": self.expression(inner)?,
            })),
            Expression::Binary { op, lhs, rhs } => Ok(json!({
                "kind": "binary",
                "op": op_name(*op),
                "lhs": self.expression(lhs)?,
                "rhs": self.expression(rhs)?,
            })),
            Expression::If(if_expression) => Ok(json!({
                "kind": "if",
                "branches": if_expression
                    .branches
                    .iter()
                    .map(|(condition, value)| {
                        Ok(json!({
                            "condition": self.expression(condition)?,
                            "value": self.expression(value)?,
                        }))
                    })
                    .collect::<Result<Vec<_>, GalecTargetError>>()?,
                "else": self.expression(&if_expression.else_value)?,
            })),
            Expression::Call(call) => {
                let Name::Ident(function, _) = &call.function else {
                    return Err(GalecTargetError::LoweringInternal {
                        detail: "template context met a call to a quoted function name".to_owned(),
                    });
                };
                Ok(json!({
                    "kind": "call",
                    "builtin": function.as_str(),
                    "args": call
                        .arguments
                        .iter()
                        .map(|argument| self.expression(argument))
                        .collect::<Result<Vec<_>, GalecTargetError>>()?,
                }))
            }
            Expression::Array(elements) => Ok(json!({
                "kind": "array",
                "elements": elements
                    .iter()
                    .map(|element| self.expression(element))
                    .collect::<Result<Vec<_>, GalecTargetError>>()?,
            })),
            Expression::Size { .. } => Err(unsupported("a `size(…)` expression")),
        }
    }

    /// `self.x[i]` → `{ kind: "ref", base_name, galec_name, indices: [i] }`
    /// (indices 1-based, the GALEC truth).
    fn reference(&self, reference: &Reference) -> Result<Value, GalecTargetError> {
        let Reference::State(parts) = reference else {
            return Err(unsupported("a local (non-`self.`) reference"));
        };
        let [part] = parts.as_slice() else {
            return Err(unsupported("a multi-part state reference"));
        };
        let indices = part
            .subscripts
            .iter()
            .map(|subscript| match subscript {
                Expression::Integer(value) if *value >= 1 => Ok(*value),
                other => Err(GalecTargetError::LoweringInternal {
                    detail: format!(
                        "template context met a non-literal GALEC subscript {other:?}; \
                         the lowering emits literal 1-based subscripts only"
                    ),
                }),
            })
            .collect::<Result<Vec<_>, GalecTargetError>>()?;
        Ok(json!({
            "kind": "ref",
            "base_name": self.names.base(&part.name)?,
            "galec_name": galec_token(&part.name),
            "indices": indices,
        }))
    }
}

/// Abstract operator names — each template maps its own spellings and
/// precedence classes.
fn op_name(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "add",
        BinaryOp::Sub => "sub",
        BinaryOp::Mul => "mul",
        BinaryOp::Div => "div",
        BinaryOp::Pow => "pow",
        BinaryOp::Lt => "lt",
        BinaryOp::Le => "le",
        BinaryOp::Gt => "gt",
        BinaryOp::Ge => "ge",
        BinaryOp::Eq => "eq",
        BinaryOp::Ne => "ne",
        BinaryOp::And => "and",
        BinaryOp::Or => "or",
    }
}

fn reference_with_static_subscripts(
    reference: &Reference,
    indices: &[i64],
) -> Result<Reference, GalecTargetError> {
    let Reference::State(parts) = reference else {
        return Err(GalecTargetError::LoweringInternal {
            detail: "template context can only index whole-array state references".to_owned(),
        });
    };
    let [part] = parts.as_slice() else {
        return Err(GalecTargetError::LoweringInternal {
            detail: "template context can only index single-part state references".to_owned(),
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

fn unsupported(construct: &'static str) -> GalecTargetError {
    GalecTargetError::CExportUnsupported {
        construct,
        detail: "the current DAE lowering (crate::lower) never emits this construct".to_owned(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn base_identifiers_never_end_in_underscore() {
        let name = crate::mangle::pre_state_name("y").unwrap();
        assert_eq!(base_identifier_of(&name).as_deref(), Some("previous_y"));
        assert_eq!(
            base_identifier_of(&Name::quoted("a.b[2]")).as_deref(),
            Some("a_b_2")
        );
        assert_eq!(base_identifier_of(&Name::quoted("__shadow")), None);
    }

    #[test]
    fn galec_tokens_keep_their_quotes() {
        assert_eq!(galec_token(&Name::ident("y")), "y");
        assert_eq!(galec_token(&Name::quoted("previous(y)")), "'previous(y)'");
    }
}
