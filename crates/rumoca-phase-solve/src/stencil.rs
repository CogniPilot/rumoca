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

mod affine_terms;
use affine_terms::*;

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
    y_slot_ranges: &YSlotRanges,
    span: rumoca_core::Span,
) -> Result<Vec<StructuredLoadYRange>, LowerError> {
    let mut ranges =
        stencil_vec_with_capacity(row.len(), "structured LoadY range metadata count", span)?;
    for (op_position, op) in row.iter().enumerate() {
        if let solve::LinearOp::LoadY { index, .. } = op
            && let Some(y_range) = y_slot_ranges.get(*index)
        {
            ranges.push(StructuredLoadYRange {
                op_position,
                y_range,
            });
        }
    }
    Ok(ranges)
}

/// Contiguous Y-slot ranges of the structured array bases, one entry per base (NOT
/// per scalar), sorted and disjoint by start so a slot index maps to its base range by
/// binary search. Replaces a per-scalar `IndexMap<usize, Range>` so neither building
/// nor holding it is O(scalars) — the array element slots are contiguous, so a base's
/// range is `base_slot .. base_slot + product(shape)`.
#[derive(Debug, Clone, Default)]
pub(crate) struct YSlotRanges {
    ranges: Vec<std::ops::Range<usize>>,
}

impl YSlotRanges {
    /// Range of the array base owning slot `index`, or `None` if no base covers it.
    pub(crate) fn get(&self, index: usize) -> Option<std::ops::Range<usize>> {
        self.ranges
            .binary_search_by(|range| {
                if index < range.start {
                    std::cmp::Ordering::Greater
                } else if index >= range.end {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Equal
                }
            })
            .ok()
            .map(|position| self.ranges[position].clone())
    }
}

pub(crate) fn structured_y_slot_ranges(
    layout: &solve::VarLayout,
) -> Result<YSlotRanges, LowerError> {
    // Accumulate one contiguous Y-slot range per array base. The base binding seeds
    // the full range from its shape (`base .. base + product(shape)`); any per-element
    // bindings still present only extend it (a no-op for a shaped array), so this is
    // correct whether or not element slots are materialized.
    let mut ranges_by_base: IndexMap<String, std::ops::Range<usize>> = IndexMap::new();
    for (name, slot) in layout.bindings() {
        let solve::ScalarSlot::Y { index, .. } = slot else {
            continue;
        };
        let (base, end) = if rumoca_core::parse_scalar_name(name).is_some() {
            // Element `u[i,j]`: contributes its own slot to the base's range.
            let base = dae::component_base_name(name).unwrap_or_else(|| name.clone());
            (base, checked_y_slot_range_end(*index)?)
        } else if let Some(shape) = layout.shape(name) {
            // Array base `u` with shape: seeds the whole contiguous range.
            (
                name.clone(),
                checked_y_slot_range_end_for_shape(*index, shape)?,
            )
        } else {
            // Scalar Y variable: a singleton range.
            (name.clone(), checked_y_slot_range_end(*index)?)
        };
        ranges_by_base
            .entry(base)
            .and_modify(|range| {
                range.start = range.start.min(*index);
                range.end = range.end.max(end);
            })
            .or_insert(*index..end);
    }

    let mut ranges: Vec<std::ops::Range<usize>> = ranges_by_base.into_values().collect();
    ranges.sort_by_key(|range| range.start);
    Ok(YSlotRanges { ranges })
}

fn checked_y_slot_range_end_for_shape(index: usize, shape: &[usize]) -> Result<usize, LowerError> {
    let count = shape
        .iter()
        .try_fold(1usize, |count, dim| count.checked_mul(*dim))
        .ok_or_else(|| {
            stencil_unspanned_contract_violation(format!(
                "Y-slot array element count overflows for base slot index {index}"
            ))
        })?;
    index.checked_add(count).ok_or_else(|| {
        stencil_unspanned_contract_violation(format!(
            "Y-slot range end overflows for base slot index {index} with {count} elements"
        ))
    })
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
            StructuredTensorDecision::Scalar {
                decline,
                family_rows: Some(family_rows),
            } => {
                // A structured family that could not be preserved as one tensor node.
                // Surfacing the reason makes a missed stencil (and the slower scalar
                // lowering it implies) diagnosable via `--trace` without a rebuild.
                tracing::debug!(
                    reason_code = decline.code(),
                    rows = family_rows.len(),
                    "structured family scalarized"
                );
                // Scalarize the whole declining family at once. Consuming every row
                // here means the outer loop skips them instead of re-entering
                // `structured_tensor_at` per row, each call re-scanning the entire
                // remaining family (the O(N^2)/O(N^3) compile blowup on large grids).
                scalarize_rows(
                    &family_rows,
                    rows.as_slice(),
                    &mut consumed,
                    &mut scalar_fallback_rows,
                );
            }
            StructuredTensorDecision::Scalar {
                family_rows: None, ..
            } => {
                scalar_fallback_rows.push(rows[row].clone());
                consumed[row] = true;
            }
        }
    }
    flush_scalar_fallback_rows(nodes, &mut scalar_fallback_rows)?;
    rows.clear();
    Ok(())
}

/// Append every not-yet-consumed row in `row_indices` to the scalar fallback and mark
/// it consumed -- scalarizing a whole declining regular family in a single pass.
fn scalarize_rows(
    row_indices: &[usize],
    rows: &[StructuredProgram],
    consumed: &mut [bool],
    scalar_fallback_rows: &mut Vec<StructuredProgram>,
) {
    for &row_index in row_indices {
        if !consumed[row_index] {
            scalar_fallback_rows.push(rows[row_index].clone());
            consumed[row_index] = true;
        }
    }
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
    /// A `regular` family failed full-domain corner validation. Regularity is
    /// all-or-nothing (every cell shares one affine body, so the whole domain is the
    /// only candidate stencil); the shrinking-prefix search below cannot recover a
    /// smaller valid stencil and would only cost O(N^2). Decline straight to scalar.
    RegularFamilyNotPreservable,
}

impl StructuredTensorDecline {
    const fn code(self) -> &'static str {
        match self {
            Self::MissingStructuredSlot => "solve:missing-structured-slot",
            Self::MissingStructuredFamily => "solve:missing-structured-family",
            Self::TooFewStructuredRows => "solve:too-few-structured-rows",
            Self::NonCompactCandidateDomain => "solve:non-compact-candidate-domain",
            Self::MismatchedDaeBodyShape => "solve:mismatched-dae-body-shape",
            Self::MissingAffineAccessProof => "solve:missing-affine-access-proof",
            Self::NonAffineOutputMap => "solve:non-affine-output-map",
            Self::RegularFamilyNotPreservable => "solve:regular-family-not-preservable",
        }
    }
}

#[derive(Debug)]
enum StructuredTensorDecision {
    Preserve(StructuredTensorCandidate),
    Scalar {
        decline: StructuredTensorDecline,
        /// Every row of the structured family that declines together. The caller
        /// scalarizes them all in one pass, so a family that can't preserve is never
        /// re-evaluated row-by-row (each re-evaluation re-scans the whole remaining
        /// family -- the O(N^2)/O(N^3) blowup on large grids). `None` when no family
        /// was resolved (only the single starting row scalarizes).
        family_rows: Option<Vec<usize>>,
    },
}

impl StructuredTensorDecision {
    fn scalar(decline: StructuredTensorDecline) -> Self {
        StructuredTensorDecision::Scalar {
            decline,
            family_rows: None,
        }
    }

    fn scalar_family(decline: StructuredTensorDecline, family_rows: Vec<usize>) -> Self {
        StructuredTensorDecision::Scalar {
            decline,
            family_rows: Some(family_rows),
        }
    }
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
    let tuples = domain.index_tuple_iter().map_err(|err| {
        stencil_contract_violation(format!("structured index domain is invalid: {err}"), span)
    })?;
    let mut materialized =
        stencil_vec_with_capacity(tuples.len(), "explicit scalar-view index tuple count", span)?;
    materialized.extend(tuples);
    Ok(materialized)
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
        return Ok(StructuredTensorDecision::scalar(
            StructuredTensorDecline::MissingStructuredSlot,
        ));
    };
    let Some(family) = structured_equations.get(slot.family_index) else {
        return Ok(StructuredTensorDecision::scalar(
            StructuredTensorDecline::MissingStructuredFamily,
        ));
    };
    // Batch-scalarize the whole family only when it is `regular` (every cell shares
    // one affine body): there a decline is all-or-nothing, so consuming every row at
    // once is correct and avoids the per-row rescan blowup. A NON-regular family must
    // keep single-row scalarization so the outer loop can still preserve a SUBDOMAIN
    // after a non-affine boundary iteration (the shrinking-prefix search).
    let decline_decision = |decline, family_row_indices: Vec<usize>| {
        if family.regular.is_some() {
            StructuredTensorDecision::scalar_family(decline, family_row_indices)
        } else {
            StructuredTensorDecision::scalar(decline)
        }
    };
    let family_row_indices = structured_row_indices_for_family(
        &slot_rows,
        slot,
        family.point_count().map_err(|err| {
            stencil_contract_violation(
                format!("structured index domain is invalid: {err}"),
                family.span,
            )
        })?,
        family.span,
    )?;
    if family_row_indices.len() < 2 {
        return Ok(StructuredTensorDecision::scalar(
            StructuredTensorDecline::TooFewStructuredRows,
        ));
    }
    let domain_decision = max_structured_affine_domain(
        rows,
        &family_row_indices,
        family,
        slot.iteration_index,
        dae_equations,
    )?;
    let (row_indices, domain) = match domain_decision {
        StructuredDomainDecision::Preserve(row_indices, domain) => (row_indices, domain),
        StructuredDomainDecision::Scalar(decline) => {
            return Ok(decline_decision(decline, family_row_indices));
        }
    };
    let Some(strides) = affine_strides_for_family(
        rows,
        &row_indices,
        &domain,
        family.span,
        family.interiors_materialized,
    )?
    else {
        return Ok(decline_decision(
            StructuredTensorDecline::MissingAffineAccessProof,
            family_row_indices,
        ));
    };
    let Some(output_map) = output_map_for_family(
        rows,
        &row_indices,
        &domain,
        family.span,
        family.interiors_materialized,
    )?
    else {
        return Ok(decline_decision(
            StructuredTensorDecline::NonAffineOutputMap,
            family_row_indices,
        ));
    };
    structured_tensor_node(rows, row_indices, strides, output_map, domain, family.span)
}

/// Assemble the family's `ComputeNode` (a pointwise `Map` or an `AffineStencil`)
/// from its base row, the corner-derived strides, and the output map, preserving
/// the consumed `row_indices`.
fn structured_tensor_node(
    rows: &[StructuredProgram],
    row_indices: Vec<usize>,
    strides: AffineStrides,
    output_map: solve::TensorOutputMap,
    domain: rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<StructuredTensorDecision, LowerError> {
    let Some(first_row) = row_indices
        .first()
        .and_then(|row_index| rows.get(*row_index))
    else {
        return Ok(StructuredTensorDecision::scalar_family(
            StructuredTensorDecline::TooFewStructuredRows,
            row_indices,
        ));
    };
    let base_ops = first_row.ops.clone();
    let load_strides = strides.load_strides;
    let const_strides = strides.const_strides;
    let metadata = solve::TensorNodeMetadata::default();
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
    // A `regular` family is affine over its whole domain by construction (flatten
    // only classifies it regular when every cell shares one affine body), so the
    // entire candidate range is the stencil -- no need to search shrinking prefixes,
    // and validation reads only the corner rows (base + one neighbor per binder).
    // Leaving the interior rows unread is the contract that lets flatten stop
    // materializing them. A non-regular family keeps the full prefix search below.
    if family.regular.is_some() {
        // Regularity is all-or-nothing: the whole domain is the only candidate
        // stencil. Either the corner model validates the full domain, or the family
        // scalarizes -- the shrinking-prefix search below can never recover a smaller
        // valid stencil for a regular family, and running it is the O(N^2) (per
        // single-row rescan -> O(N^3)) cost behind the large-grid compile blowup.
        return Ok(
            match regular_family_full_domain(
                rows,
                row_indices,
                family,
                iteration_start,
                dae_equations,
            )? {
                Some(decision) => decision,
                None => StructuredDomainDecision::Scalar(
                    StructuredTensorDecline::RegularFamilyNotPreservable,
                ),
            },
        );
    }

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

/// Preserve a `regular` family's entire candidate range as one stencil domain,
/// validated from corner rows only (body shape, strides, output map). Returns
/// `None` to fall back to the prefix search when the corner model does not apply
/// (e.g. an extent-1 binder, or a corner that fails to validate) -- the same
/// conservative fallback the per-part corner helpers use.
fn regular_family_full_domain(
    rows: &[StructuredProgram],
    row_indices: &[usize],
    family: &dae::StructuredEquationFamily,
    iteration_start: usize,
    dae_equations: &[dae::Equation],
) -> Result<Option<StructuredDomainDecision>, LowerError> {
    let point_count = family.point_count().map_err(|err| {
        stencil_contract_violation(
            format!("structured index domain is invalid: {err}"),
            family.span,
        )
    })?;
    if iteration_start != 0 || row_indices.len() != point_count {
        return Ok(None);
    }
    let domain = family.domain.clone();
    if !corner_dae_body_shapes_match(rows, row_indices, dae_equations, &domain, family.span)? {
        return Ok(None);
    }
    if affine_strides_for_family(
        rows,
        row_indices,
        &domain,
        family.span,
        family.interiors_materialized,
    )?
    .is_none()
    {
        return Ok(None);
    }
    if output_map_for_family(
        rows,
        row_indices,
        &domain,
        family.span,
        family.interiors_materialized,
    )?
    .is_none()
    {
        return Ok(None);
    }
    Ok(Some(StructuredDomainDecision::Preserve(
        copied_row_indices(row_indices, family.span)?,
        domain,
    )))
}

/// Like [`structured_dae_body_shapes_match`] but reads only the family's corner
/// rows (base + one neighbor per binder). For a regular family every cell shares
/// one body, so the corners are representative -- this avoids reading the interior
/// rows' DAE bodies. Singleton binders need no neighbor because they do not vary.
fn corner_dae_body_shapes_match(
    rows: &[StructuredProgram],
    row_indices: &[usize],
    dae_equations: &[dae::Equation],
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<bool, LowerError> {
    let point_count = domain.scalar_count().map_err(|err| {
        stencil_contract_violation(format!("structured index domain is invalid: {err}"), span)
    })?;
    if row_indices.len() != point_count {
        return Ok(false);
    }
    let corner_positions = structured_domain_corner_ordinals(domain, span)?;
    let mut corner_rows =
        stencil_vec_with_capacity(corner_positions.len(), "corner body-shape row count", span)?;
    for &position in &corner_positions {
        corner_rows.push(row_indices[position]);
    }
    structured_dae_body_shapes_match(rows, &corner_rows, dae_equations)
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
    let full =
        proof_strides_for_base_ops(base_row.ops.as_slice(), base_proof, operand_deltas, span)?;
    // Family-native validation: whenever the full-domain derivation succeeds,
    // the representative-subset derivation (base cell + one unit step per
    // dimension, trusting the DAE structured family) must produce identical
    // strides. This proves the O(rank) subset path correct as the foundation
    // for family-native lowering. `if cfg!(debug_assertions)` keeps the check
    // out of release builds entirely (the subset derivation is itself O(cells)
    // today) while leaving the function compiled (no dead-code).
    if cfg!(debug_assertions) {
        let representative =
            affine_strides_from_representative_proofs(rows, row_indices, domain, span)?;
        debug_assert!(
            full.is_none() || full == representative,
            "representative-subset affine strides diverged from the full-domain \
             derivation:\n  full={full:?}\n  representative={representative:?}"
        );
    }
    Ok(full)
}

/// Derive affine strides from a representative SUBSET of the family's rows — the
/// base cell plus the first unit-step row in each dimension — instead of
/// differencing every cell. The DAE structured family guarantees affineness, so
/// each per-dimension stride is fixed by these O(rank) points (a dimension with
/// no unit-step row contributes stride 0). This is the family-native path that
/// avoids materializing every cell; today it is only cross-checked against
/// `affine_strides_from_access_proofs`.
fn affine_strides_from_representative_proofs(
    rows: &[StructuredProgram],
    row_indices: &[usize],
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<Option<AffineStrides>, LowerError> {
    let index_tuples = structured_domain_index_tuples(domain, span)?;
    if row_indices.len() != index_tuples.len() {
        return Ok(None);
    }
    let Some(base_tuple) = index_tuples.first() else {
        return Ok(None);
    };
    let rank = base_tuple.len();
    // Base cell (position 0) plus the first row that steps only one dimension.
    let mut positions =
        stencil_vec_with_capacity(rank + 1, "representative proof position count", span)?;
    positions.push(0usize);
    for dimension in 0..rank {
        if let Some(position) = (1..index_tuples.len()).find(|&candidate| {
            only_dimension_changes(base_tuple, &index_tuples[candidate], dimension)
        }) {
            positions.push(position);
        }
    }
    let mut proof_rows =
        stencil_vec_with_capacity(positions.len(), "representative proof row count", span)?;
    let mut sub_tuples =
        stencil_vec_with_capacity(positions.len(), "representative proof tuple count", span)?;
    for &position in &positions {
        let Some(row_index) = row_indices.get(position) else {
            return Ok(None);
        };
        let Some(proof) = rows
            .get(*row_index)
            .and_then(|row| row.access_proof.as_ref())
        else {
            return Ok(None);
        };
        proof_rows.push(proof);
        sub_tuples.push(index_tuples[position].clone());
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
    let Some(()) = proof_operand_kinds_match(base_proof, &proof_rows) else {
        return Ok(None);
    };
    let Some(operand_deltas) =
        proof_operand_deltas(base_proof, &proof_rows, domain, &sub_tuples, span)?
    else {
        return Ok(None);
    };
    proof_strides_for_base_ops(base_row.ops.as_slice(), base_proof, operand_deltas, span)
}

/// Load/const strides for a regular family, corner-first (P1). The strides depend
/// only on the access pattern, so the family's corner rows (base + one neighbor
/// per dimension) yield the same result as diffing every row -- this is the
/// production path, so the interior rows are not read when the corner model
/// applies. Falls back to the full-row derivation when the corners cannot be
/// isolated, keeping scalar fallback for families the corner model does not cover.
/// In debug builds, small materialized families are checked against the full-row
/// oracle -- the equivalence that lets flatten (P3) stop materializing interiors.
fn affine_strides_for_family(
    rows: &[StructuredProgram],
    row_indices: &[usize],
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
    interiors_materialized: bool,
) -> Result<Option<AffineStrides>, LowerError> {
    let corner = affine_strides_from_corner_rows(rows, row_indices, domain, span)?;
    if !interiors_materialized {
        // The interior rows carry only placeholder bodies, so the full-row scan
        // would read garbage -- the corner rows alone determine the strides, and
        // there is no full-row fallback to take.
        return Ok(corner);
    }
    // Debug: the corner model must never be *less* conservative than the full scan.
    // The corner path is row-blind (it reads only the corners), so if it builds a
    // stencil, diffing every row must build the same one -- otherwise a misclassified
    // family could slip a wrong stencil past it where the full scan would have declined
    // to scalar. A bounded full-row fallback below remains the correctness oracle.
    if cfg!(debug_assertions)
        && should_cross_check_scalar_view(domain, span)?
        && let Some(corner) = &corner
    {
        let full = affine_strides_from_access_proofs(rows, row_indices, domain, span)?;
        let Some(full) = &full else {
            return Ok(None);
        };
        if corner != full {
            return Ok(None);
        }
    }
    match corner {
        Some(strides) => Ok(Some(strides)),
        None if should_cross_check_scalar_view(domain, span)? => {
            affine_strides_from_access_proofs(rows, row_indices, domain, span)
        }
        None => Ok(None),
    }
}

/// Output map for a regular family, corner-first (P1) with full-row fallback and a
/// debug-build equivalence assert -- see [`affine_strides_for_family`].
fn output_map_for_family(
    rows: &[StructuredProgram],
    row_indices: &[usize],
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
    interiors_materialized: bool,
) -> Result<Option<solve::TensorOutputMap>, LowerError> {
    let corner = output_map_from_corner_rows(rows, row_indices, domain, span)?;
    if !interiors_materialized {
        // Interior bodies are placeholders; the corners pin the output map.
        return Ok(corner);
    }
    // Debug: corners must not be less conservative than the full scan -- see
    // `affine_strides_for_family` for the rationale.
    if cfg!(debug_assertions)
        && should_cross_check_scalar_view(domain, span)?
        && let Some(corner) = &corner
    {
        let full = output_map_for_rows(rows, row_indices, domain, span)?;
        let Some(full) = &full else {
            return Ok(None);
        };
        if corner != full {
            return Ok(None);
        }
    }
    match corner {
        Some(output_map) => Ok(Some(output_map)),
        None if should_cross_check_scalar_view(domain, span)? => {
            output_map_for_rows(rows, row_indices, domain, span)
        }
        None => Ok(None),
    }
}

/// Affine strides computed from only the family's CORNER rows: the base
/// iteration plus one neighbor per domain dimension. For a regular family (affine
/// accesses are guaranteed by construction) these O(ndim) rows determine the same
/// strides the full-row inference produces -- this is the basis for building the
/// stencil node without materializing every iteration. Handles `LoadP`
/// (derived-parameter `c`) loads identically to `LoadY`, via the same per-tuple
/// index fit. Returns `None` (caller falls back to the full-row path) when the
/// corners cannot be located or a proof is missing.
fn affine_strides_from_corner_rows(
    rows: &[StructuredProgram],
    row_indices: &[usize],
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<Option<AffineStrides>, LowerError> {
    let Some((corner_rows, corner_tuples)) =
        corner_rows_and_tuples(rows, row_indices, domain, span)?
    else {
        return Ok(None);
    };
    let mut corner_proofs =
        stencil_vec_with_capacity(corner_rows.len(), "corner proof row count", span)?;
    for row in &corner_rows {
        let Some(proof) = row.access_proof.as_ref() else {
            return Ok(None);
        };
        corner_proofs.push(proof);
    }
    let (Some(base_row), Some(base_proof)) = (corner_rows.first(), corner_proofs.first().copied())
    else {
        return Ok(None);
    };
    let Some(()) = proof_operand_kinds_match(base_proof, &corner_proofs) else {
        return Ok(None);
    };
    let Some(operand_deltas) =
        proof_operand_deltas(base_proof, &corner_proofs, domain, &corner_tuples, span)?
    else {
        return Ok(None);
    };
    proof_strides_for_base_ops(base_row.ops.as_slice(), base_proof, operand_deltas, span)
}

/// The family's corner rows (base iteration + one neighbor per domain dimension)
/// paired with their index tuples, in `[base, +dim0, +dim1, ...]` order. This is
/// the shared selection both corner-row builders (strides and output map) consume.
/// `None` when the rows do not cover the full domain or a corner row is absent.
fn corner_rows_and_tuples<'a>(
    rows: &'a [StructuredProgram],
    row_indices: &[usize],
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<Option<CornerSelection<'a>>, LowerError> {
    let point_count = domain.scalar_count().map_err(|err| {
        stencil_contract_violation(format!("structured index domain is invalid: {err}"), span)
    })?;
    if point_count == 0 || row_indices.len() != point_count {
        return Ok(None);
    }
    let corner_positions = structured_domain_corner_ordinals(domain, span)?;
    let mut corner_rows =
        stencil_vec_with_capacity(corner_positions.len(), "corner row count", span)?;
    let mut corner_tuples =
        stencil_vec_with_capacity(corner_positions.len(), "corner index tuple count", span)?;
    for &position in &corner_positions {
        let Some(row) = rows.get(row_indices[position]) else {
            return Ok(None);
        };
        let Some(index_tuple) = domain.index_tuple_at(position).map_err(|err| {
            stencil_contract_violation(format!("structured index domain is invalid: {err}"), span)
        })?
        else {
            return Ok(None);
        };
        corner_rows.push(row);
        corner_tuples.push(index_tuple);
    }
    Ok(Some((corner_rows, corner_tuples)))
}

fn structured_domain_corner_ordinals(
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    domain.corner_ordinals().map_err(|err| {
        stencil_contract_violation(format!("structured index domain is invalid: {err}"), span)
    })
}

fn should_cross_check_scalar_view(
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<bool, LowerError> {
    const MAX_DEBUG_SCALAR_VIEW_POINTS: usize = 256;
    domain
        .scalar_count()
        .map(|count| count <= MAX_DEBUG_SCALAR_VIEW_POINTS)
        .map_err(|err| {
            stencil_contract_violation(format!("structured index domain is invalid: {err}"), span)
        })
}

/// The tensor output map computed from only the family's corner rows (base + one
/// neighbor per dimension), mirroring `output_map_for_rows` on that subset. For a
/// regular family the output index is affine in the binders, so the corners pin
/// it exactly. `None` (caller falls back) when the corners cannot be located.
fn output_map_from_corner_rows(
    rows: &[StructuredProgram],
    row_indices: &[usize],
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<Option<solve::TensorOutputMap>, LowerError> {
    let Some((corner_rows, corner_tuples)) =
        corner_rows_and_tuples(rows, row_indices, domain, span)?
    else {
        return Ok(None);
    };
    let Some(first_row) = corner_rows.first() else {
        return Ok(None);
    };
    let mut values =
        stencil_vec_with_capacity(corner_rows.len(), "corner output-map value count", span)?;
    for row in &corner_rows {
        values.push(row.output_index);
    }
    let Some(strides) = infer_index_terms_with_span(
        first_row.output_index,
        &values,
        domain,
        &corner_tuples,
        span,
    )?
    else {
        return Ok(None);
    };
    Ok(Some(solve::TensorOutputMap {
        start: first_row.output_index,
        strides,
    }))
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

/// A regular family's corner rows paired with their index tuples, in
/// `[base, +dim0, +dim1, ...]` order -- the shared selection consumed by both the
/// stride and output-map corner builders.
type CornerSelection<'a> = (Vec<&'a StructuredProgram>, Vec<Vec<i64>>);

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
