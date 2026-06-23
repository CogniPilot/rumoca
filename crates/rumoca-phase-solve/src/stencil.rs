//! Structured tensor preservation for Solve IR.
//!
//! Tensor nodes are emitted only when structured rows carry DAE structured
//! family provenance and access proofs whose operands vary affinely across the
//! family domain. Scalar rows remain a fallback view; they are not the owner of
//! tensor shape.

use indexmap::{IndexMap, IndexSet};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::lower::LowerError;

mod access_proof;

pub(crate) use access_proof::{
    StructuredAccessOperand, StructuredAccessProof, StructuredAccessProofBuilder,
    structured_access_operand_for_slot,
};

#[derive(Debug, Clone)]
pub(crate) struct StructuredProgram {
    pub(crate) ops: Vec<solve::LinearOp>,
    pub(crate) output_index: usize,
    pub(crate) pointwise_output_y_index: Option<usize>,
    pub(crate) span: rumoca_core::Span,
    pub(crate) output_y_range: std::ops::Range<usize>,
    pub(crate) load_y_ranges: Vec<StructuredLoadYRange>,
    pub(crate) dae_equation_index: Option<usize>,
    pub(crate) access_proof: Option<StructuredAccessProof>,
}

#[derive(Debug, Clone)]
pub(crate) struct StructuredLoadYRange {
    pub(crate) op_position: usize,
    pub(crate) y_range: std::ops::Range<usize>,
}

pub(crate) fn structured_load_y_ranges(
    row: &[solve::LinearOp],
    y_slot_ranges: &IndexMap<usize, std::ops::Range<usize>>,
    span: rumoca_core::Span,
) -> Result<Vec<StructuredLoadYRange>, LowerError> {
    let mut ranges =
        stencil_vec_with_capacity(row.len(), "structured LoadY range metadata count", span)?;
    for (op_position, op) in row.iter().enumerate() {
        if let solve::LinearOp::LoadY { index, .. } = op
            && let Some(y_range) = y_slot_ranges.get(index).cloned()
        {
            ranges.push(StructuredLoadYRange {
                op_position,
                y_range,
            });
        }
    }
    Ok(ranges)
}

pub(crate) fn structured_y_slot_ranges(
    layout: &solve::VarLayout,
) -> Result<IndexMap<usize, std::ops::Range<usize>>, LowerError> {
    let mut ranges_by_base: IndexMap<String, std::ops::Range<usize>> =
        stencil_source_free_index_map_with_capacity(
            layout.bindings().len(),
            "structured Y-slot base range count",
        )?;
    for (name, slot) in layout.bindings() {
        let solve::ScalarSlot::Y { index, .. } = slot else {
            continue;
        };
        let end = checked_y_slot_range_end(*index)?;
        let base = dae::component_base_name(name).unwrap_or_else(|| name.clone());
        ranges_by_base
            .entry(base)
            .and_modify(|range| {
                range.start = range.start.min(*index);
                range.end = range.end.max(end);
            })
            .or_insert(*index..end);
    }

    let mut ranges_by_slot: IndexMap<usize, std::ops::Range<usize>> =
        stencil_source_free_index_map_with_capacity(
            layout.bindings().len(),
            "structured Y-slot range lookup count",
        )?;
    for (name, slot) in layout.bindings() {
        let solve::ScalarSlot::Y { index, .. } = slot else {
            continue;
        };
        let base = dae::component_base_name(name).unwrap_or_else(|| name.clone());
        if let Some(range) = ranges_by_base.get(&base) {
            ranges_by_slot.insert(*index, range.clone());
        }
    }
    Ok(ranges_by_slot)
}

fn checked_y_slot_range_end(index: usize) -> Result<usize, LowerError> {
    index.checked_add(1).ok_or_else(|| {
        stencil_unspanned_contract_violation(format!(
            "Y-slot range end overflows for slot index {index}"
        ))
    })
}

pub(crate) fn push_structured_programs(
    nodes: &mut Vec<solve::ComputeNode>,
    rows: &mut Vec<StructuredProgram>,
    structured_equations: &[dae::StructuredEquationFamily],
    dae_equations: &[dae::Equation],
) -> Result<(), LowerError> {
    if rows.is_empty() {
        return Ok(());
    }
    let span = first_structured_program_span(rows);
    let mut scalar_fallback_rows =
        stencil_vec_with_capacity(rows.len(), "scalar fallback structured row count", span)?;
    let mut consumed =
        stencil_vec_with_capacity(rows.len(), "structured row consumed flag count", span)?;
    consumed.extend(std::iter::repeat_n(false, rows.len()));
    let slots = structured_slots_for_rows(rows, structured_equations)?;
    for row in 0..rows.len() {
        if consumed[row] {
            continue;
        }
        match structured_tensor_at(
            rows,
            &slots,
            row,
            structured_equations,
            dae_equations,
            consumed.as_slice(),
        )? {
            StructuredTensorDecision::Preserve(candidate) => {
                flush_scalar_fallback_rows(nodes, &mut scalar_fallback_rows)?;
                for row_index in candidate.row_indices {
                    consumed[row_index] = true;
                }
                nodes.push(candidate.node);
            }
            StructuredTensorDecision::Scalar(_decline) => {
                scalar_fallback_rows.push(rows[row].clone());
                consumed[row] = true;
            }
        }
    }
    flush_scalar_fallback_rows(nodes, &mut scalar_fallback_rows)?;
    rows.clear();
    Ok(())
}

fn flush_scalar_fallback_rows(
    nodes: &mut Vec<solve::ComputeNode>,
    rows: &mut Vec<StructuredProgram>,
) -> Result<(), LowerError> {
    if rows.is_empty() {
        return Ok(());
    }
    let span = first_structured_program_span(rows);
    let mut programs =
        stencil_vec_with_capacity(rows.len(), "scalar fallback structured program count", span)?;
    let mut program_spans =
        stencil_vec_with_capacity(rows.len(), "scalar fallback span count", span)?;
    let mut output_indices =
        stencil_vec_with_capacity(rows.len(), "scalar fallback output index count", span)?;
    for row in rows.iter() {
        programs.push(row.ops.clone());
        program_spans.push(row.span);
        output_indices.push(row.output_index);
    }
    nodes.push(solve::ComputeNode::ScalarPrograms(
        solve::ScalarProgramBlock::with_output_indices(programs, program_spans, output_indices)?,
    ));
    rows.clear();
    Ok(())
}

#[derive(Debug)]
struct StructuredTensorCandidate {
    node: solve::ComputeNode,
    row_indices: Vec<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StructuredTensorDecline {
    MissingStructuredSlot,
    MissingStructuredFamily,
    TooFewStructuredRows,
    NonCompactCandidateDomain,
    MismatchedDaeBodyShape,
    MissingAffineAccessProof,
    NonAffineOutputMap,
}

#[derive(Debug)]
enum StructuredTensorDecision {
    Preserve(StructuredTensorCandidate),
    Scalar(StructuredTensorDecline),
}

#[derive(Debug)]
enum StructuredDomainDecision {
    Preserve(Vec<usize>, rumoca_core::StructuredIndexDomain),
    Scalar(StructuredTensorDecline),
}

fn structured_slots_for_rows(
    rows: &[StructuredProgram],
    structured_equations: &[dae::StructuredEquationFamily],
) -> Result<Vec<Option<dae::StructuredEquationSlot>>, LowerError> {
    if rows.is_empty() {
        return Ok(Vec::new());
    }
    let mut slots = stencil_vec_with_capacity(
        rows.len(),
        "structured row slot metadata count",
        rows[0].span,
    )?;
    for row in rows {
        slots.push(row.dae_equation_index.and_then(|equation_index| {
            dae::structured_equation_slot(structured_equations, equation_index)
        }));
    }
    Ok(slots)
}

fn first_structured_program_span(rows: &[StructuredProgram]) -> rumoca_core::Span {
    rows[0].span
}

fn stencil_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    values.try_reserve_exact(capacity).map_err(|_| {
        stencil_contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })?;
    Ok(values)
}

fn copied_row_indices(
    row_indices: &[usize],
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let mut values =
        stencil_vec_with_capacity(row_indices.len(), "structured row index copy count", span)?;
    values.extend(row_indices.iter().copied());
    Ok(values)
}

fn structured_domain_index_tuples(
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<Vec<Vec<i64>>, LowerError> {
    domain.index_tuples().map_err(|err| {
        stencil_contract_violation(format!("structured index domain is invalid: {err}"), span)
    })
}

fn stencil_index_map_with_capacity<K, V>(
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<IndexMap<K, V>, LowerError>
where
    K: std::hash::Hash + Eq,
{
    let mut values = IndexMap::new();
    values.try_reserve(capacity).map_err(|_| {
        stencil_contract_violation(format!("{context} exceeds host memory limits"), span)
    })?;
    Ok(values)
}

fn stencil_source_free_index_map_with_capacity<K, V>(
    capacity: usize,
    context: &'static str,
) -> Result<IndexMap<K, V>, LowerError>
where
    K: std::hash::Hash + Eq,
{
    let mut values = IndexMap::new();
    values.try_reserve(capacity).map_err(|_| {
        stencil_unspanned_contract_violation(format!("{context} exceeds host memory limits"))
    })?;
    Ok(values)
}

fn structured_tensor_at(
    rows: &[StructuredProgram],
    slots: &[Option<dae::StructuredEquationSlot>],
    start: usize,
    structured_equations: &[dae::StructuredEquationFamily],
    dae_equations: &[dae::Equation],
    consumed: &[bool],
) -> Result<StructuredTensorDecision, LowerError> {
    let slot_rows =
        structured_slot_row_lookup(slots, consumed, first_structured_program_span(rows))?;
    let Some(slot) = slots.get(start).and_then(|slot| *slot) else {
        return Ok(StructuredTensorDecision::Scalar(
            StructuredTensorDecline::MissingStructuredSlot,
        ));
    };
    let Some(family) = structured_equations.get(slot.family_index) else {
        return Ok(StructuredTensorDecision::Scalar(
            StructuredTensorDecline::MissingStructuredFamily,
        ));
    };
    let row_indices = structured_row_indices_for_family(
        &slot_rows,
        slot,
        family.equation_counts.len(),
        family.span,
    )?;
    if row_indices.len() < 2 {
        return Ok(StructuredTensorDecision::Scalar(
            StructuredTensorDecline::TooFewStructuredRows,
        ));
    }
    let domain_decision = max_structured_affine_domain(
        rows,
        &row_indices,
        family,
        slot.iteration_index,
        dae_equations,
    )?;
    let (row_indices, domain) = match domain_decision {
        StructuredDomainDecision::Preserve(row_indices, domain) => (row_indices, domain),
        StructuredDomainDecision::Scalar(decline) => {
            return Ok(StructuredTensorDecision::Scalar(decline));
        }
    };
    let Some(strides) =
        affine_strides_from_access_proofs(rows, &row_indices, &domain, family.span)?
    else {
        return Ok(StructuredTensorDecision::Scalar(
            StructuredTensorDecline::MissingAffineAccessProof,
        ));
    };
    let Some(output_map) = output_map_for_rows(rows, &row_indices, &domain, family.span)? else {
        return Ok(StructuredTensorDecision::Scalar(
            StructuredTensorDecline::NonAffineOutputMap,
        ));
    };
    let Some(first_row) = row_indices
        .first()
        .and_then(|row_index| rows.get(*row_index))
    else {
        return Ok(StructuredTensorDecision::Scalar(
            StructuredTensorDecline::TooFewStructuredRows,
        ));
    };
    let base_ops = first_row.ops.clone();
    let load_strides = strides.load_strides;
    let const_strides = strides.const_strides;
    let metadata = solve::TensorNodeMetadata::default();
    let span = family.span;
    let node = if is_pointwise_map(
        &base_ops,
        &load_strides,
        &output_map.strides,
        first_row
            .pointwise_output_y_index
            .unwrap_or(output_map.start),
        &first_row.output_y_range,
        &first_row.load_y_ranges,
    ) {
        solve::ComputeNode::Map {
            domain,
            output_map,
            base_ops,
            load_strides,
            const_strides,
            metadata,
            span,
        }
    } else {
        solve::ComputeNode::AffineStencil {
            domain,
            output_map,
            base_ops,
            load_strides,
            const_strides,
            metadata,
            span,
        }
    };
    Ok(StructuredTensorDecision::Preserve(
        StructuredTensorCandidate { node, row_indices },
    ))
}

fn structured_row_indices_for_family(
    slot_rows: &IndexMap<dae::StructuredEquationSlot, usize>,
    start_slot: dae::StructuredEquationSlot,
    iteration_count: usize,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    if start_slot.iteration_index >= iteration_count {
        return Ok(Vec::new());
    }
    let mut row_indices = stencil_vec_with_capacity(
        iteration_count - start_slot.iteration_index,
        "structured row index count",
        span,
    )?;
    for iteration_index in start_slot.iteration_index..iteration_count {
        let expected = dae::StructuredEquationSlot {
            iteration_index,
            ..start_slot
        };
        let Some(row_index) = slot_rows.get(&expected).copied() else {
            break;
        };
        row_indices.push(row_index);
    }
    Ok(row_indices)
}

fn structured_slot_row_lookup(
    slots: &[Option<dae::StructuredEquationSlot>],
    consumed: &[bool],
    span: rumoca_core::Span,
) -> Result<IndexMap<dae::StructuredEquationSlot, usize>, LowerError> {
    if consumed.len() != slots.len() {
        return Err(stencil_contract_violation(
            format!(
                "structured consumed flag count {} does not match slot count {}",
                consumed.len(),
                slots.len()
            ),
            span,
        ));
    }
    let mut slot_rows =
        stencil_index_map_with_capacity(slots.len(), "structured slot row lookup count", span)?;
    for (row_index, slot) in slots.iter().enumerate() {
        if consumed[row_index] {
            continue;
        }
        if let Some(slot) = slot {
            slot_rows.entry(*slot).or_insert(row_index);
        }
    }
    Ok(slot_rows)
}

fn structured_dae_body_shapes_match(
    rows: &[StructuredProgram],
    row_indices: &[usize],
    dae_equations: &[dae::Equation],
) -> Result<bool, LowerError> {
    let mut first = None;
    for row_index in row_indices {
        let Some(equation_index) = rows.get(*row_index).and_then(|row| row.dae_equation_index)
        else {
            return Ok(false);
        };
        let Some(equation) = dae_equations.get(equation_index) else {
            return Ok(false);
        };
        let signature = expression_body_shape(&equation.rhs, equation.span)?;
        let Some(first_signature) = &first else {
            first = Some(signature);
            continue;
        };
        if signature != *first_signature {
            return Ok(false);
        }
    }
    if first.is_none() {
        return Ok(false);
    }
    Ok(true)
}

#[derive(Debug, Clone, PartialEq)]
enum ExpressionBodyShape {
    Binary {
        op: rumoca_core::OpBinary,
        lhs: Box<ExpressionBodyShape>,
        rhs: Box<ExpressionBodyShape>,
    },
    Unary {
        op: rumoca_core::OpUnary,
        rhs: Box<ExpressionBodyShape>,
    },
    VarRef {
        name: String,
        subscripts: Vec<SubscriptBodyShape>,
    },
    BuiltinCall {
        function: String,
        args: Vec<ExpressionBodyShape>,
    },
    FunctionCall {
        name: String,
        args: Vec<ExpressionBodyShape>,
        is_constructor: bool,
    },
    Literal(LiteralBodyShape),
    If {
        branches: Vec<(ExpressionBodyShape, ExpressionBodyShape)>,
        else_branch: Box<ExpressionBodyShape>,
    },
    Array {
        elements: Vec<ExpressionBodyShape>,
        is_matrix: bool,
    },
    Tuple(Vec<ExpressionBodyShape>),
    Range {
        start: Box<ExpressionBodyShape>,
        step: Option<Box<ExpressionBodyShape>>,
        end: Box<ExpressionBodyShape>,
    },
    ArrayComprehension,
    Index {
        base: Box<ExpressionBodyShape>,
        subscripts: Vec<SubscriptBodyShape>,
    },
    FieldAccess {
        base: Box<ExpressionBodyShape>,
        field: String,
    },
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
enum SubscriptBodyShape {
    ScalarizedIndex,
    Colon,
    Expr(ExpressionBodyShape),
}

#[derive(Debug, Clone, PartialEq)]
enum LiteralBodyShape {
    Real,
    Integer,
    Boolean,
    String,
}

fn expression_body_shape(
    expr: &rumoca_core::Expression,
    context_span: rumoca_core::Span,
) -> Result<ExpressionBodyShape, LowerError> {
    use rumoca_core::Expression;
    let span = expr.span().unwrap_or(context_span);
    match expr {
        Expression::Binary { op, lhs, rhs, .. } => Ok(ExpressionBodyShape::Binary {
            op: op.clone(),
            lhs: Box::new(expression_body_shape(lhs, span)?),
            rhs: Box::new(expression_body_shape(rhs, span)?),
        }),
        Expression::Unary { op, rhs, .. } => Ok(ExpressionBodyShape::Unary {
            op: op.clone(),
            rhs: Box::new(expression_body_shape(rhs, span)?),
        }),
        Expression::VarRef {
            name, subscripts, ..
        } => Ok(ExpressionBodyShape::VarRef {
            name: name.to_string(),
            subscripts: subscript_body_shapes(subscripts, span)?,
        }),
        Expression::BuiltinCall { function, args, .. } => Ok(ExpressionBodyShape::BuiltinCall {
            function: function.name().to_string(),
            args: expression_body_shapes(args, "builtin-call body-shape argument count", span)?,
        }),
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } => Ok(ExpressionBodyShape::FunctionCall {
            name: name.to_string(),
            args: expression_body_shapes(args, "function-call body-shape argument count", span)?,
            is_constructor: *is_constructor,
        }),
        Expression::Literal { value, .. } => {
            Ok(ExpressionBodyShape::Literal(literal_body_shape(value)))
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => Ok(ExpressionBodyShape::If {
            branches: branch_body_shapes(branches, span)?,
            else_branch: Box::new(expression_body_shape(else_branch, span)?),
        }),
        Expression::Array {
            elements,
            is_matrix,
            ..
        } => Ok(ExpressionBodyShape::Array {
            elements: expression_body_shapes(elements, "array body-shape element count", span)?,
            is_matrix: *is_matrix,
        }),
        Expression::Tuple { elements, .. } => Ok(ExpressionBodyShape::Tuple(
            expression_body_shapes(elements, "tuple body-shape element count", span)?,
        )),
        Expression::Range {
            start, step, end, ..
        } => Ok(ExpressionBodyShape::Range {
            start: Box::new(expression_body_shape(start, span)?),
            step: step
                .as_ref()
                .map(|step| expression_body_shape(step, span).map(Box::new))
                .transpose()?,
            end: Box::new(expression_body_shape(end, span)?),
        }),
        Expression::ArrayComprehension { .. } => Ok(ExpressionBodyShape::ArrayComprehension),
        Expression::Index {
            base, subscripts, ..
        } => Ok(ExpressionBodyShape::Index {
            base: Box::new(expression_body_shape(base, span)?),
            subscripts: subscript_body_shapes(subscripts, span)?,
        }),
        Expression::FieldAccess { base, field, .. } => Ok(ExpressionBodyShape::FieldAccess {
            base: Box::new(expression_body_shape(base, span)?),
            field: field.clone(),
        }),
        Expression::Empty { .. } => Ok(ExpressionBodyShape::Empty),
    }
}

fn literal_body_shape(value: &rumoca_core::Literal) -> LiteralBodyShape {
    match value {
        rumoca_core::Literal::Real(_) => LiteralBodyShape::Real,
        rumoca_core::Literal::Integer(_) => LiteralBodyShape::Integer,
        rumoca_core::Literal::Boolean(_) => LiteralBodyShape::Boolean,
        rumoca_core::Literal::String(_) => LiteralBodyShape::String,
    }
}

fn expression_body_shapes(
    expressions: &[rumoca_core::Expression],
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<ExpressionBodyShape>, LowerError> {
    let mut shapes = stencil_vec_with_capacity(expressions.len(), context, span)?;
    for expression in expressions {
        shapes.push(expression_body_shape(expression, span)?);
    }
    Ok(shapes)
}

fn branch_body_shapes(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    span: rumoca_core::Span,
) -> Result<Vec<(ExpressionBodyShape, ExpressionBodyShape)>, LowerError> {
    let mut shapes = stencil_vec_with_capacity(branches.len(), "if body-shape branch count", span)?;
    for (condition, branch) in branches {
        shapes.push((
            expression_body_shape(condition, span)?,
            expression_body_shape(branch, span)?,
        ));
    }
    Ok(shapes)
}

fn subscript_body_shapes(
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
) -> Result<Vec<SubscriptBodyShape>, LowerError> {
    let mut shapes =
        stencil_vec_with_capacity(subscripts.len(), "body-shape subscript count", span)?;
    for subscript in subscripts {
        shapes.push(match subscript {
            rumoca_core::Subscript::Index { .. } => SubscriptBodyShape::ScalarizedIndex,
            rumoca_core::Subscript::Colon { .. } => SubscriptBodyShape::Colon,
            rumoca_core::Subscript::Expr { expr, .. } => {
                SubscriptBodyShape::Expr(expression_body_shape(expr, span)?)
            }
        });
    }
    Ok(shapes)
}

fn max_structured_affine_domain(
    rows: &[StructuredProgram],
    row_indices: &[usize],
    family: &dae::StructuredEquationFamily,
    iteration_start: usize,
    dae_equations: &[dae::Equation],
) -> Result<StructuredDomainDecision, LowerError> {
    let index_tuples = structured_domain_index_tuples(&family.domain, family.span)?;
    let mut decline = StructuredTensorDecline::NonCompactCandidateDomain;
    for count in (2..=row_indices.len()).rev() {
        let row_indices = &row_indices[..count];
        let Some(end) = iteration_start.checked_add(count) else {
            decline = StructuredTensorDecline::NonCompactCandidateDomain;
            continue;
        };
        let Some(candidate_tuples) = index_tuples.get(iteration_start..end) else {
            decline = StructuredTensorDecline::NonCompactCandidateDomain;
            continue;
        };
        let Some(domain) =
            compact_domain_from_tuples(&family.domain, candidate_tuples, family.span)?
        else {
            decline = StructuredTensorDecline::NonCompactCandidateDomain;
            continue;
        };
        if !structured_dae_body_shapes_match(rows, row_indices, dae_equations)? {
            decline = StructuredTensorDecline::MismatchedDaeBodyShape;
            continue;
        }
        if affine_strides_from_access_proofs(rows, row_indices, &domain, family.span)?.is_none() {
            decline = StructuredTensorDecline::MissingAffineAccessProof;
            continue;
        }
        if output_map_for_rows(rows, row_indices, &domain, family.span)?.is_some() {
            return Ok(StructuredDomainDecision::Preserve(
                copied_row_indices(row_indices, family.span)?,
                domain,
            ));
        }
        decline = StructuredTensorDecline::NonAffineOutputMap;
    }
    Ok(StructuredDomainDecision::Scalar(decline))
}

fn affine_strides_from_access_proofs(
    rows: &[StructuredProgram],
    row_indices: &[usize],
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<Option<AffineStrides>, LowerError> {
    let mut proof_rows =
        stencil_vec_with_capacity(row_indices.len(), "affine proof row count", span)?;
    for row_index in row_indices {
        let Some(proof) = rows
            .get(*row_index)
            .and_then(|row| row.access_proof.as_ref())
        else {
            return Ok(None);
        };
        proof_rows.push(proof);
    }
    let Some(base_row) = row_indices
        .first()
        .and_then(|row_index| rows.get(*row_index))
    else {
        return Ok(None);
    };
    let Some(base_proof) = proof_rows.first().copied() else {
        return Ok(None);
    };
    let index_tuples = structured_domain_index_tuples(domain, span)?;
    if proof_rows.len() != index_tuples.len() {
        return Ok(None);
    }
    let Some(()) = proof_operand_kinds_match(base_proof, &proof_rows) else {
        return Ok(None);
    };
    let Some(operand_deltas) =
        proof_operand_deltas(base_proof, &proof_rows, domain, &index_tuples, span)?
    else {
        return Ok(None);
    };
    proof_strides_for_base_ops(base_row.ops.as_slice(), base_proof, operand_deltas, span)
}

fn proof_operand_kinds_match(
    base: &StructuredAccessProof,
    rows: &[&StructuredAccessProof],
) -> Option<()> {
    for row in rows {
        if row.operands.len() != base.operands.len() {
            return None;
        }
        for (lhs, rhs) in base.operands.iter().zip(&row.operands) {
            if !proof_operand_kind_matches(lhs, rhs) {
                return None;
            }
        }
    }
    Some(())
}

fn proof_operand_kind_matches(
    lhs: &StructuredAccessOperand,
    rhs: &StructuredAccessOperand,
) -> bool {
    matches!(
        (lhs, rhs),
        (
            StructuredAccessOperand::Const(_),
            StructuredAccessOperand::Const(_)
        ) | (
            StructuredAccessOperand::LoadY(_),
            StructuredAccessOperand::LoadY(_)
        ) | (
            StructuredAccessOperand::LoadP(_),
            StructuredAccessOperand::LoadP(_)
        )
    )
}

#[derive(Debug, Clone, PartialEq)]
enum ProofOperandDelta {
    Const(Vec<solve::AffineStencilConstStrideTerm>),
    Load(Vec<solve::AffineStencilIndexStrideTerm>),
}

fn proof_operand_deltas(
    base: &StructuredAccessProof,
    rows: &[&StructuredAccessProof],
    domain: &rumoca_core::StructuredIndexDomain,
    index_tuples: &[Vec<i64>],
    span: rumoca_core::Span,
) -> Result<Option<Vec<ProofOperandDelta>>, LowerError> {
    let mut deltas = stencil_vec_with_capacity(
        base.operands.len(),
        "affine proof operand delta count",
        span,
    )?;
    for (operand_index, operand) in base.operands.iter().enumerate() {
        let Some(delta) = (match operand {
            StructuredAccessOperand::Const(base_value) => {
                let Some(values) = proof_const_values(rows, operand_index, span)? else {
                    return Ok(None);
                };
                let Some(terms) =
                    infer_const_terms_with_span(*base_value, &values, domain, index_tuples, span)?
                else {
                    return Ok(None);
                };
                Some(ProofOperandDelta::Const(terms))
            }
            StructuredAccessOperand::LoadY(base_value)
            | StructuredAccessOperand::LoadP(base_value) => {
                let Some(values) = proof_load_values(rows, operand_index, span)? else {
                    return Ok(None);
                };
                let Some(terms) =
                    infer_index_terms_with_span(*base_value, &values, domain, index_tuples, span)?
                else {
                    return Ok(None);
                };
                Some(ProofOperandDelta::Load(terms))
            }
        }) else {
            return Ok(None);
        };
        deltas.push(delta);
    }
    Ok(Some(deltas))
}

fn proof_const_values(
    rows: &[&StructuredAccessProof],
    operand_index: usize,
    span: rumoca_core::Span,
) -> Result<Option<Vec<f64>>, LowerError> {
    let mut values = stencil_vec_with_capacity(rows.len(), "affine proof const value count", span)?;
    for row in rows {
        let Some(StructuredAccessOperand::Const(value)) = row.operands.get(operand_index) else {
            return Ok(None);
        };
        values.push(*value);
    }
    Ok(Some(values))
}

fn proof_load_values(
    rows: &[&StructuredAccessProof],
    operand_index: usize,
    span: rumoca_core::Span,
) -> Result<Option<Vec<usize>>, LowerError> {
    let mut values = stencil_vec_with_capacity(rows.len(), "affine proof load value count", span)?;
    for row in rows {
        let Some(StructuredAccessOperand::LoadY(value) | StructuredAccessOperand::LoadP(value)) =
            row.operands.get(operand_index)
        else {
            return Ok(None);
        };
        values.push(*value);
    }
    Ok(Some(values))
}

fn proof_strides_for_base_ops(
    base_ops: &[solve::LinearOp],
    base_proof: &StructuredAccessProof,
    operand_deltas: Vec<ProofOperandDelta>,
    span: rumoca_core::Span,
) -> Result<Option<AffineStrides>, LowerError> {
    let mut proof_operands = base_proof.operands.iter().zip(operand_deltas);
    let access_operand_count = base_ops
        .iter()
        .filter(|op| is_access_operand_op(op))
        .count();
    let mut load_strides =
        stencil_vec_with_capacity(access_operand_count, "affine proof load stride count", span)?;
    let mut const_strides = stencil_vec_with_capacity(
        access_operand_count,
        "affine proof const stride count",
        span,
    )?;
    for (op_position, op) in base_ops.iter().enumerate() {
        if !is_access_operand_op(op) {
            continue;
        }
        let Some((operand, delta)) = next_matching_proof_operand(&mut proof_operands, op) else {
            return Ok(None);
        };
        match (operand, delta) {
            (StructuredAccessOperand::Const(_), ProofOperandDelta::Const(terms)) => {
                if !terms.is_empty() {
                    const_strides.push(solve::AffineStencilConstStride { op_position, terms });
                }
            }
            (
                StructuredAccessOperand::LoadY(_) | StructuredAccessOperand::LoadP(_),
                ProofOperandDelta::Load(terms),
            ) => {
                if !terms.is_empty() {
                    load_strides.push(solve::AffineStencilLoadStride { op_position, terms });
                }
            }
            _ => return Ok(None),
        }
    }
    for (operand, delta) in proof_operands {
        if !can_skip_unemitted_proof_operand(operand, &delta) {
            return Ok(None);
        }
    }
    Ok(Some(AffineStrides {
        load_strides,
        const_strides,
    }))
}

fn is_access_operand_op(op: &solve::LinearOp) -> bool {
    matches!(
        op,
        solve::LinearOp::Const { .. }
            | solve::LinearOp::LoadY { .. }
            | solve::LinearOp::LoadP { .. }
    )
}

fn next_matching_proof_operand<'a>(
    proof_operands: &mut impl Iterator<Item = (&'a StructuredAccessOperand, ProofOperandDelta)>,
    op: &solve::LinearOp,
) -> Option<(&'a StructuredAccessOperand, ProofOperandDelta)> {
    for (operand, delta) in proof_operands {
        if proof_operand_matches_op(operand, op) {
            return Some((operand, delta));
        }
        if !can_skip_unemitted_proof_operand(operand, &delta) {
            return None;
        }
    }
    None
}

fn can_skip_unemitted_proof_operand(
    operand: &StructuredAccessOperand,
    delta: &ProofOperandDelta,
) -> bool {
    matches!(
        (operand, delta),
        (StructuredAccessOperand::Const(_), ProofOperandDelta::Const(terms)) if terms.is_empty()
    ) || matches!(
        (operand, delta),
        (StructuredAccessOperand::LoadP(_), ProofOperandDelta::Load(terms)) if terms.is_empty()
    )
}

fn proof_operand_matches_op(operand: &StructuredAccessOperand, op: &solve::LinearOp) -> bool {
    matches!(
        (operand, op),
        (
            StructuredAccessOperand::Const(_),
            solve::LinearOp::Const { .. }
        ) | (
            StructuredAccessOperand::LoadY(_),
            solve::LinearOp::LoadY { .. }
        ) | (
            StructuredAccessOperand::LoadP(_),
            solve::LinearOp::LoadP { .. }
        )
    )
}

fn output_map_for_rows(
    rows: &[StructuredProgram],
    row_indices: &[usize],
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<Option<solve::TensorOutputMap>, LowerError> {
    let Some(first_row) = row_indices
        .first()
        .and_then(|row_index| rows.get(*row_index))
    else {
        return Ok(None);
    };
    let mut values =
        stencil_vec_with_capacity(row_indices.len(), "tensor output-map value count", span)?;
    for row_index in row_indices {
        let Some(row) = rows.get(*row_index) else {
            return Ok(None);
        };
        values.push(row.output_index);
    }
    let index_tuples = structured_domain_index_tuples(domain, span)?;
    let Some(strides) =
        infer_index_terms_with_span(first_row.output_index, &values, domain, &index_tuples, span)?
    else {
        return Ok(None);
    };
    Ok(Some(solve::TensorOutputMap {
        start: first_row.output_index,
        strides,
    }))
}

pub(crate) fn tensor_output_map_from_values(
    domain: &rumoca_core::StructuredIndexDomain,
    values: &[usize],
    span: rumoca_core::Span,
) -> Result<Option<solve::TensorOutputMap>, LowerError> {
    let Some(start) = values.first().copied() else {
        return Ok(None);
    };
    let index_tuples = structured_domain_index_tuples(domain, span)?;
    let Some(strides) = infer_index_terms_with_span(start, values, domain, &index_tuples, span)?
    else {
        return Ok(None);
    };
    Ok(Some(solve::TensorOutputMap { start, strides }))
}

fn compact_domain_from_tuples(
    source_domain: &rumoca_core::StructuredIndexDomain,
    index_tuples: &[Vec<i64>],
    span: rumoca_core::Span,
) -> Result<Option<rumoca_core::StructuredIndexDomain>, LowerError> {
    if index_tuples.is_empty() {
        return Ok(None);
    }
    let dimension_count = source_domain.binders.len();
    if index_tuples
        .iter()
        .any(|index_tuple| index_tuple.len() != dimension_count)
    {
        return Ok(None);
    }
    let mut binders =
        stencil_vec_with_capacity(dimension_count, "compact domain binder count", span)?;
    for dimension in 0..dimension_count {
        let Some(binder) = compact_binder(source_domain, index_tuples, dimension, span)? else {
            return Ok(None);
        };
        binders.push(binder);
    }
    let domain = rumoca_core::StructuredIndexDomain { binders };
    Ok((structured_domain_index_tuples(&domain, span)? == index_tuples).then_some(domain))
}

fn compact_binder(
    source_domain: &rumoca_core::StructuredIndexDomain,
    index_tuples: &[Vec<i64>],
    dimension: usize,
    span: rumoca_core::Span,
) -> Result<Option<rumoca_core::StructuredIndexBinder>, LowerError> {
    let mut values = IndexSet::new();
    values.try_reserve(index_tuples.len()).map_err(|_| {
        stencil_contract_violation(
            "compact domain binder value count exceeds host memory limits",
            span,
        )
    })?;
    for index_tuple in index_tuples {
        let value = index_tuple[dimension];
        values.insert(value);
    }
    let Some(first) = values.first().copied() else {
        return Ok(None);
    };
    let step = if let Some(second) = values.get_index(1).copied() {
        let step = second.checked_sub(first).ok_or_else(|| {
            stencil_contract_violation("compact domain binder step overflows i64", span)
        })?;
        if step == 0 {
            return Ok(None);
        }
        for value in values.iter().skip(2) {
            let delta = value.checked_sub(first).ok_or_else(|| {
                stencil_contract_violation("compact domain binder delta overflows i64", span)
            })?;
            let remainder = delta.checked_rem(step).ok_or_else(|| {
                stencil_contract_violation("compact domain binder remainder overflows i64", span)
            })?;
            if remainder != 0 {
                return Ok(None);
            }
        }
        step
    } else {
        1
    };
    let Some(upper) = values.last().copied() else {
        return Ok(None);
    };
    Ok(Some(rumoca_core::StructuredIndexBinder {
        id: source_domain.binders[dimension].id,
        display_name: source_domain.binders[dimension].display_name.clone(),
        lower: first,
        upper,
        step,
    }))
}

#[derive(Clone, Debug, PartialEq)]
struct AffineStrides {
    load_strides: Vec<solve::AffineStencilLoadStride>,
    const_strides: Vec<solve::AffineStencilConstStride>,
}

fn is_pointwise_map(
    base_ops: &[solve::LinearOp],
    load_strides: &[solve::AffineStencilLoadStride],
    output_terms: &[solve::AffineStencilIndexStrideTerm],
    pointwise_output_y_index: usize,
    output_y_range: &std::ops::Range<usize>,
    load_y_ranges: &[StructuredLoadYRange],
) -> bool {
    for stride in load_strides {
        let Some(op) = base_ops.get(stride.op_position) else {
            return false;
        };
        if let solve::LinearOp::LoadY { index, .. } = op
            && !stride.terms.is_empty()
        {
            if stride.terms != output_terms {
                return false;
            }
            if !is_same_component_load(
                *index,
                stride.op_position,
                pointwise_output_y_index,
                output_y_range,
                load_y_ranges,
            ) {
                return false;
            }
        }
    }
    true
}

fn is_same_component_load(
    load_index: usize,
    op_position: usize,
    output_start: usize,
    output_y_range: &std::ops::Range<usize>,
    load_y_ranges: &[StructuredLoadYRange],
) -> bool {
    if load_index == output_start {
        return true;
    }
    if output_y_range.contains(&load_index) {
        return false;
    }
    let Some(load_y_range) = load_y_range_for_op(load_y_ranges, op_position) else {
        return false;
    };
    let Some(load_len) = range_len(load_y_range) else {
        return false;
    };
    let Some(output_len) = range_len(output_y_range) else {
        return false;
    };
    if load_len != output_len {
        return false;
    }
    load_index.checked_sub(load_y_range.start) == output_start.checked_sub(output_y_range.start)
}

fn load_y_range_for_op(
    load_y_ranges: &[StructuredLoadYRange],
    op_position: usize,
) -> Option<&std::ops::Range<usize>> {
    load_y_ranges
        .iter()
        .find(|range| range.op_position == op_position)
        .map(|range| &range.y_range)
}

fn range_len(range: &std::ops::Range<usize>) -> Option<usize> {
    range.end.checked_sub(range.start)
}

fn infer_index_terms_with_span(
    base_value: usize,
    values: &[usize],
    domain: &rumoca_core::StructuredIndexDomain,
    index_tuples: &[Vec<i64>],
    span: rumoca_core::Span,
) -> Result<Option<Vec<solve::AffineStencilIndexStrideTerm>>, LowerError> {
    let Some(base_tuple) = index_tuples.first() else {
        return Ok(None);
    };
    let mut strides =
        stencil_vec_with_capacity(base_tuple.len(), "affine index stride term count", span)?;
    for dimension in 0..base_tuple.len() {
        let Some(stride) = infer_integer_dimension_stride_with_span(
            base_value,
            values,
            domain,
            index_tuples,
            dimension,
            span,
        )?
        else {
            return Ok(None);
        };
        strides.push(stride);
    }
    for (value, tuple) in values.iter().zip(index_tuples) {
        let Some(expected) =
            apply_integer_terms_with_span(base_value, &strides, domain, base_tuple, tuple, span)?
        else {
            return Ok(None);
        };
        if expected != *value {
            return Ok(None);
        }
    }
    Ok(Some(index_stride_terms(strides, span)?))
}

fn infer_const_terms_with_span(
    base_value: f64,
    values: &[f64],
    domain: &rumoca_core::StructuredIndexDomain,
    index_tuples: &[Vec<i64>],
    span: rumoca_core::Span,
) -> Result<Option<Vec<solve::AffineStencilConstStrideTerm>>, LowerError> {
    let Some(base_tuple) = index_tuples.first() else {
        return Ok(None);
    };
    let mut strides =
        stencil_vec_with_capacity(base_tuple.len(), "affine const stride term count", span)?;
    for dimension in 0..base_tuple.len() {
        let Some(stride) = infer_float_dimension_stride_with_span(
            base_value,
            values,
            domain,
            index_tuples,
            dimension,
            span,
        )?
        else {
            return Ok(None);
        };
        strides.push(stride);
    }
    for (value, tuple) in values.iter().zip(index_tuples) {
        let Some(expected) =
            apply_float_terms_with_span(base_value, &strides, domain, base_tuple, tuple, span)?
        else {
            return Ok(None);
        };
        if (expected - value).abs() > 1e-9 {
            return Ok(None);
        }
    }
    Ok(Some(const_stride_terms(strides, span)?))
}

fn infer_integer_dimension_stride_with_span(
    base_value: usize,
    values: &[usize],
    domain: &rumoca_core::StructuredIndexDomain,
    index_tuples: &[Vec<i64>],
    dimension: usize,
    span: rumoca_core::Span,
) -> Result<Option<isize>, LowerError> {
    let Some(base_tuple) = index_tuples.first() else {
        return Ok(None);
    };
    let base_value = checked_usize_to_isize(base_value, "affine index base value", span)?;
    for (value, tuple) in values.iter().zip(index_tuples).skip(1) {
        if !only_dimension_changes(base_tuple, tuple, dimension) {
            continue;
        }
        let Some(ordinal) = ordinal_delta_with_span(dimension, domain, base_tuple, tuple, span)?
        else {
            return Ok(None);
        };
        if ordinal == 0 {
            continue;
        }
        let value = checked_usize_to_isize(*value, "affine index value", span)?;
        let ordinal = checked_i64_to_isize(ordinal, "affine index ordinal", span)?;
        let delta = value.checked_sub(base_value).ok_or_else(|| {
            stencil_contract_violation("affine index delta overflows host index range", span)
        })?;
        let remainder = delta.checked_rem(ordinal).ok_or_else(|| {
            stencil_contract_violation(
                "affine index delta remainder overflows host index range",
                span,
            )
        })?;
        if remainder != 0 {
            return Ok(None);
        }
        let stride = delta.checked_div(ordinal).ok_or_else(|| {
            stencil_contract_violation("affine index stride overflows host index range", span)
        })?;
        return Ok(Some(stride));
    }
    Ok(Some(0))
}

fn infer_float_dimension_stride_with_span(
    base_value: f64,
    values: &[f64],
    domain: &rumoca_core::StructuredIndexDomain,
    index_tuples: &[Vec<i64>],
    dimension: usize,
    span: rumoca_core::Span,
) -> Result<Option<f64>, LowerError> {
    let Some(base_tuple) = index_tuples.first() else {
        return Ok(None);
    };
    for (value, tuple) in values.iter().zip(index_tuples).skip(1) {
        if !only_dimension_changes(base_tuple, tuple, dimension) {
            continue;
        }
        let Some(ordinal) = ordinal_delta_with_span(dimension, domain, base_tuple, tuple, span)?
        else {
            return Ok(None);
        };
        if ordinal == 0 {
            continue;
        }
        let stride = (*value - base_value) / ordinal as f64;
        if !stride.is_finite() {
            return Err(stencil_contract_violation(
                "affine const stride is not finite",
                span,
            ));
        }
        return Ok(Some(stride));
    }
    Ok(Some(0.0))
}

fn only_dimension_changes(base_tuple: &[i64], tuple: &[i64], dimension: usize) -> bool {
    tuple
        .iter()
        .enumerate()
        .all(|(idx, value)| idx == dimension || *value == base_tuple[idx])
        && tuple[dimension] != base_tuple[dimension]
}

fn apply_integer_terms_with_span(
    base_value: usize,
    strides: &[isize],
    domain: &rumoca_core::StructuredIndexDomain,
    base_tuple: &[i64],
    tuple: &[i64],
    span: rumoca_core::Span,
) -> Result<Option<usize>, LowerError> {
    let mut value = checked_usize_to_isize(base_value, "affine index base value", span)?;
    for (dimension, stride) in strides.iter().enumerate() {
        let Some(ordinal) = ordinal_delta_with_span(dimension, domain, base_tuple, tuple, span)?
        else {
            return Ok(None);
        };
        let ordinal = checked_i64_to_isize(ordinal, "affine index ordinal", span)?;
        let term = ordinal.checked_mul(*stride).ok_or_else(|| {
            stencil_contract_violation("affine index stride term overflows host index range", span)
        })?;
        value = value.checked_add(term).ok_or_else(|| {
            stencil_contract_violation("affine index term sum overflows host index range", span)
        })?;
    }
    Ok(usize::try_from(value).ok())
}

fn apply_float_terms_with_span(
    base_value: f64,
    strides: &[f64],
    domain: &rumoca_core::StructuredIndexDomain,
    base_tuple: &[i64],
    tuple: &[i64],
    span: rumoca_core::Span,
) -> Result<Option<f64>, LowerError> {
    let mut value = base_value;
    for (dimension, stride) in strides.iter().enumerate() {
        if let Some(ordinal) = ordinal_delta_with_span(dimension, domain, base_tuple, tuple, span)?
        {
            let term = ordinal as f64 * stride;
            if !term.is_finite() {
                return Err(stencil_contract_violation(
                    "affine const stride term is not finite",
                    span,
                ));
            }
            value += term;
            if !value.is_finite() {
                return Err(stencil_contract_violation(
                    "affine const term sum is not finite",
                    span,
                ));
            }
        }
    }
    Ok(Some(value))
}

fn ordinal_delta_with_span(
    dimension: usize,
    domain: &rumoca_core::StructuredIndexDomain,
    base_tuple: &[i64],
    tuple: &[i64],
    span: rumoca_core::Span,
) -> Result<Option<i64>, LowerError> {
    let Some(binder) = domain.binders.get(dimension) else {
        return Ok(None);
    };
    let step = binder.step;
    if step == 0 {
        return Err(stencil_contract_violation(
            "structured affine domain binder has zero step",
            span,
        ));
    }
    let delta = tuple[dimension]
        .checked_sub(base_tuple[dimension])
        .ok_or_else(|| {
            stencil_contract_violation("structured affine ordinal delta overflows i64", span)
        })?;
    if delta % step != 0 {
        return Ok(None);
    }
    delta
        .checked_div(step)
        .map(Some)
        .ok_or_else(|| stencil_contract_violation("structured affine ordinal overflows i64", span))
}

fn checked_usize_to_isize(
    value: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<isize, LowerError> {
    isize::try_from(value).map_err(|_| {
        stencil_contract_violation(
            format!("{context} does not fit in host signed index range"),
            span,
        )
    })
}

fn checked_i64_to_isize(
    value: i64,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<isize, LowerError> {
    isize::try_from(value).map_err(|_| {
        stencil_contract_violation(
            format!("{context} does not fit in host signed index range"),
            span,
        )
    })
}

fn stencil_contract_violation(reason: impl Into<String>, span: rumoca_core::Span) -> LowerError {
    let reason = reason.into();
    if span.is_dummy() {
        stencil_unspanned_contract_violation(reason)
    } else {
        LowerError::ContractViolation { reason, span }
    }
}

fn stencil_unspanned_contract_violation(reason: impl Into<String>) -> LowerError {
    LowerError::UnspannedContractViolation {
        reason: reason.into(),
    }
}

fn index_stride_terms(
    strides: Vec<isize>,
    span: rumoca_core::Span,
) -> Result<Vec<solve::AffineStencilIndexStrideTerm>, LowerError> {
    let mut terms = stencil_vec_with_capacity(
        strides.len(),
        "affine index stride metadata term count",
        span,
    )?;
    for (dimension, stride) in strides.into_iter().enumerate() {
        if stride != 0 {
            terms.push(solve::AffineStencilIndexStrideTerm { dimension, stride });
        }
    }
    Ok(terms)
}

fn const_stride_terms(
    strides: Vec<f64>,
    span: rumoca_core::Span,
) -> Result<Vec<solve::AffineStencilConstStrideTerm>, LowerError> {
    let mut terms = stencil_vec_with_capacity(
        strides.len(),
        "affine const stride metadata term count",
        span,
    )?;
    for (dimension, stride) in strides.into_iter().enumerate() {
        if stride != 0.0 {
            terms.push(solve::AffineStencilConstStrideTerm { dimension, stride });
        }
    }
    Ok(terms)
}

#[cfg(test)]
mod tests;
