//! Lean, backend-neutral initial-condition settlement for GPU preparation.
//!
//! This path intentionally accepts only lowering-proven direct assignments.
//! General nonlinear or coupled initialization remains unsupported here rather
//! than silently using a finite-difference CPU projection.

use rumoca_ir_solve as solve;

const INITIAL_RESIDUAL_TOLERANCE: f64 = 1.0e-9;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct GpuInitializationMetrics {
    pub residual_evaluations: usize,
    pub passes: usize,
    pub temporary_values: usize,
}

#[derive(Debug, thiserror::Error)]
pub enum GpuInitializationError {
    #[error("GPU initial projection does not support {feature} (row={row}, span={span:?})")]
    Unsupported {
        feature: &'static str,
        row: usize,
        span: Option<rumoca_core::Span>,
    },
    #[error("GPU initial projection is malformed: {message} (row={row}, span={span:?})")]
    Malformed {
        message: String,
        row: usize,
        span: Option<rumoca_core::Span>,
    },
    #[error(
        "GPU initial projection {kind} did not settle (row={row}, value={value:.6e}, span={span:?})"
    )]
    NonConverged {
        kind: &'static str,
        row: usize,
        value: f64,
        span: Option<rumoca_core::Span>,
    },
    #[error("GPU initial projection evaluation failed: {message} (span={span:?})")]
    Evaluation {
        message: String,
        span: Option<rumoca_core::Span>,
    },
}

#[derive(Debug)]
pub struct GpuInitializationResult {
    pub y0: Vec<f64>,
    pub p0: Vec<f64>,
    pub metrics: GpuInitializationMetrics,
}

/// Settle a GPU-prepared model without introducing a continuous solver/JVP
/// payload.  The artifact is complete-or-error: input vectors are cloned and
/// never exposed after a failed evaluation or residual check.
pub fn settle_gpu_initial_conditions(
    model: &solve::SolveModel,
    t_start: f64,
) -> Result<GpuInitializationResult, GpuInitializationError> {
    let initialization = &model.problem.initialization;
    reject_unsupported_runtime_features(model)?;
    let mut y0 = model.initial_y.clone();
    let p0 = model.parameters.clone();
    ensure_finite(&y0, "initial y", None)?;
    ensure_finite(&p0, "initial p", None)?;
    validate_assignment_shape(initialization, model.initial_y.len())?;
    if initialization.residual.is_empty() {
        return Ok(GpuInitializationResult {
            y0,
            p0,
            metrics: GpuInitializationMetrics::default(),
        });
    }
    let runtime_state = rumoca_eval_solve::SimulationRuntimeState::new();
    let eval_context = rumoca_eval_solve::RowEvalContext {
        external_tables: Some(model.external_tables.as_slice()),
        runtime_state: Some(&runtime_state),
        ..Default::default()
    };
    let mut worst = (0usize, 0.0f64, None);
    let mut native_metrics = rumoca_eval_solve::MapEvaluationMetrics::default();
    for family in &initialization.direct_families {
        execute_direct_family(
            family,
            DirectFamilyExecution {
                initialization,
                y: &mut y0,
                p: &p0,
                t: t_start,
                context: eval_context,
                apply: true,
                worst: &mut worst,
                metrics: &mut native_metrics,
            },
        )?;
    }
    ensure_finite(&y0, "settled y", None)?;
    worst = (0usize, 0.0f64, None);
    for family in &initialization.direct_families {
        execute_direct_family(
            family,
            DirectFamilyExecution {
                initialization,
                y: &mut y0,
                p: &p0,
                t: t_start,
                context: eval_context,
                apply: false,
                worst: &mut worst,
                metrics: &mut native_metrics,
            },
        )?;
    }
    if !worst.1.is_finite() || worst.1.abs() > INITIAL_RESIDUAL_TOLERANCE {
        return Err(GpuInitializationError::NonConverged {
            kind: "residual",
            row: worst.0,
            value: worst.1,
            span: worst.2,
        });
    }
    Ok(GpuInitializationResult {
        y0,
        p0,
        metrics: GpuInitializationMetrics {
            residual_evaluations: 2,
            passes: 1,
            temporary_values: native_metrics
                .temporary_values
                .saturating_add(initialization.direct_families.len()),
        },
    })
}

fn validate_assignment_shape(
    initialization: &solve::InitializationSolveSystem,
    y_len: usize,
) -> Result<(), GpuInitializationError> {
    let required = normalize_target_ranges(&initialization.required_target_ranges, y_len)?;
    let fixed = normalize_target_ranges(&initialization.fixed_target_ranges, y_len)?;
    let expected = if y_len == 0 {
        Vec::new()
    } else {
        vec![solve::InitializationTargetRange {
            start: 0,
            end: y_len,
            span: required.first().and_then(|range| range.span),
        }]
    };
    if initialization.residual.is_empty() {
        return validate_empty_assignment_shape(initialization, &required, &fixed, &expected);
    }
    if initialization.direct_families.is_empty() {
        return Err(GpuInitializationError::Unsupported {
            feature: "non-direct or incomplete initial residual system",
            row: 0,
            span: None,
        });
    }
    if !initialization.row_targets.is_empty() {
        return Err(GpuInitializationError::Malformed {
            message: "compact GPU initialization must not materialize scalar row targets"
                .to_string(),
            row: 0,
            span: None,
        });
    }
    let mut actual_ranges = fixed;
    actual_ranges.extend(validate_direct_node_ownership(initialization)?);
    let actual = normalize_target_ranges(&actual_ranges, y_len)?;
    if !same_target_coverage(&required, &expected) || !same_target_coverage(&actual, &required) {
        return Err(GpuInitializationError::Malformed {
            message: "incomplete direct plus fixed-start target union".to_string(),
            row: 0,
            span: actual
                .first()
                .and_then(|range| range.span)
                .or_else(|| required.first().and_then(|range| range.span)),
        });
    }
    Ok(())
}

fn validate_empty_assignment_shape(
    initialization: &solve::InitializationSolveSystem,
    required: &[solve::InitializationTargetRange],
    fixed: &[solve::InitializationTargetRange],
    expected: &[solve::InitializationTargetRange],
) -> Result<(), GpuInitializationError> {
    if !initialization.direct_families.is_empty() || !initialization.row_targets.is_empty() {
        return Err(GpuInitializationError::Malformed {
            message: "empty initial residual cannot own assignment rows".to_string(),
            row: 0,
            span: initialization
                .direct_families
                .first()
                .map(|family| family.span),
        });
    }
    if required.is_empty() && fixed.is_empty() {
        return Ok(());
    }
    if !same_target_coverage(required, expected) || !same_target_coverage(fixed, required) {
        return Err(GpuInitializationError::Malformed {
            message: "incomplete fixed-start target union".to_string(),
            row: 0,
            span: fixed
                .first()
                .and_then(|range| range.span)
                .or_else(|| required.first().and_then(|range| range.span)),
        });
    }
    Ok(())
}

fn validate_direct_node_ownership(
    initialization: &solve::InitializationSolveSystem,
) -> Result<Vec<solve::InitializationTargetRange>, GpuInitializationError> {
    let mut ranges = Vec::with_capacity(initialization.direct_families.len());
    let mut node_owners = vec![None; initialization.residual.nodes.len()];
    for family in &initialization.direct_families {
        let Some(owner) = node_owners.get_mut(family.node_index) else {
            return Err(GpuInitializationError::Malformed {
                message: "direct initial family references a missing residual node".to_string(),
                row: family.node_index,
                span: Some(family.span),
            });
        };
        if owner.replace(family.span).is_some() {
            return Err(GpuInitializationError::Malformed {
                message: "duplicate direct ownership of one residual node".to_string(),
                row: family.node_index,
                span: Some(family.span),
            });
        }
        if !matches!(family.residual_sign, -1 | 1) {
            return Err(GpuInitializationError::Malformed {
                message: "direct initial family must have a unit residual sign".to_string(),
                row: 0,
                span: Some(family.span),
            });
        }
        let Some(solve::ComputeNode::Map {
            domain, base_ops, ..
        }) = initialization.residual.nodes.get(family.node_index)
        else {
            return Err(GpuInitializationError::Unsupported {
                feature: "non-Map direct initial family",
                row: 0,
                span: Some(family.span),
            });
        };
        if has_random_or_impure_ops(base_ops) {
            return Err(GpuInitializationError::Unsupported {
                feature: "random or impure direct initial operations",
                row: 0,
                span: Some(family.span),
            });
        }
        let dense = solve::TensorOutputMap::dense_contiguous(family.targets.start, domain)
            .map_err(|error| GpuInitializationError::Malformed {
                message: format!("invalid direct target map: {error:?}"),
                row: 0,
                span: Some(family.span),
            })?;
        if family.targets.strides != dense.strides {
            return Err(GpuInitializationError::Malformed {
                message: "direct target map must be dense and contiguous".to_string(),
                row: 0,
                span: Some(family.span),
            });
        }
        let count = domain
            .scalar_count()
            .map_err(|error| GpuInitializationError::Malformed {
                message: format!("invalid direct target domain: {error}"),
                row: 0,
                span: Some(family.span),
            })?;
        let end = family.targets.start.checked_add(count).ok_or_else(|| {
            GpuInitializationError::Malformed {
                message: "direct target range overflow".to_string(),
                row: 0,
                span: Some(family.span),
            }
        })?;
        ranges.push(solve::InitializationTargetRange {
            start: family.targets.start,
            end,
            span: Some(family.span),
        });
    }
    if let Some(node_index) = node_owners.iter().position(Option::is_none) {
        let span = initialization
            .residual
            .nodes
            .get(node_index)
            .and_then(|node| match node {
                solve::ComputeNode::Map { span, .. }
                | solve::ComputeNode::AffineStencil { span, .. }
                | solve::ComputeNode::MatMul { span, .. }
                | solve::ComputeNode::LinSolve { span, .. } => Some(*span),
                solve::ComputeNode::ScalarPrograms(block) => block.first_source_span(),
            });
        return Err(GpuInitializationError::Malformed {
            message: "direct initial families must own every residual node".to_string(),
            row: node_index,
            span,
        });
    }
    Ok(ranges)
}

fn normalize_target_ranges(
    ranges: &[solve::InitializationTargetRange],
    upper_bound: usize,
) -> Result<Vec<solve::InitializationTargetRange>, GpuInitializationError> {
    let mut ranges = ranges.to_vec();
    ranges.sort_unstable_by_key(|range| (range.start, range.end));
    let mut normalized: Vec<solve::InitializationTargetRange> = Vec::with_capacity(ranges.len());
    for range in ranges {
        if range.start >= range.end || range.end > upper_bound {
            return Err(GpuInitializationError::Malformed {
                message: "initial target range is empty or out of bounds".to_string(),
                row: 0,
                span: range.span,
            });
        }
        if let Some(last) = normalized.last_mut() {
            if range.start < last.end {
                return Err(GpuInitializationError::Malformed {
                    message: "initial target ranges overlap".to_string(),
                    row: 0,
                    span: range.span.or(last.span),
                });
            }
            if range.start == last.end {
                last.end = range.end;
                continue;
            }
        }
        normalized.push(range);
    }
    Ok(normalized)
}

fn same_target_coverage(
    left: &[solve::InitializationTargetRange],
    right: &[solve::InitializationTargetRange],
) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(left, right)| left.start == right.start && left.end == right.end)
}

struct DirectFamilyExecution<'a> {
    initialization: &'a solve::InitializationSolveSystem,
    y: &'a mut [f64],
    p: &'a [f64],
    t: f64,
    context: rumoca_eval_solve::RowEvalContext<'a>,
    apply: bool,
    worst: &'a mut (usize, f64, Option<rumoca_core::Span>),
    metrics: &'a mut rumoca_eval_solve::MapEvaluationMetrics,
}

fn execute_direct_family(
    family: &solve::InitializationDirectFamily,
    execution: DirectFamilyExecution<'_>,
) -> Result<(), GpuInitializationError> {
    let Some(node @ solve::ComputeNode::Map { .. }) = execution
        .initialization
        .residual
        .nodes
        .get(family.node_index)
    else {
        return Err(GpuInitializationError::Unsupported {
            feature: "non-Map direct initial family",
            row: 0,
            span: Some(family.span),
        });
    };
    let evaluation = rumoca_eval_solve::eval_map_elements_with_context(
        node,
        execution.y,
        execution.p,
        execution.t,
        execution.context,
        |ordinal, value, y| {
            let row = direct_map_index(&family.targets, ordinal, family.span).map_err(|error| {
                rumoca_eval_solve::EvalSolveError::InvalidRow {
                    message: error.to_string(),
                    span: Some(family.span),
                }
            })?;
            if !value.is_finite() {
                return Err(rumoca_eval_solve::EvalSolveError::InvalidRow {
                    message: format!("non-finite direct initial residual at y[{row}]"),
                    span: Some(family.span),
                });
            }
            if execution.apply {
                *y.get_mut(row).ok_or_else(|| {
                    rumoca_eval_solve::EvalSolveError::InvalidRow {
                        message: format!("direct target y[{row}] is outside the state vector"),
                        span: Some(family.span),
                    }
                })? -= f64::from(family.residual_sign) * value;
            }
            if value.abs() > execution.worst.1.abs() {
                *execution.worst = (row, value, Some(family.span));
            }
            Ok(())
        },
    )
    .map_err(|error| GpuInitializationError::Evaluation {
        message: error.to_string(),
        span: error.source_span().or(Some(family.span)),
    })?;
    execution.metrics.elements = execution
        .metrics
        .elements
        .saturating_add(evaluation.elements);
    execution.metrics.temporary_values = execution
        .metrics
        .temporary_values
        .max(evaluation.temporary_values);
    Ok(())
}

fn has_random_or_impure_ops(ops: &[solve::LinearOp]) -> bool {
    ops.iter().any(|op| {
        matches!(
            op,
            solve::LinearOp::RandomInitialState { .. }
                | solve::LinearOp::RandomResult { .. }
                | solve::LinearOp::RandomState { .. }
                | solve::LinearOp::ImpureRandomInit { .. }
                | solve::LinearOp::ImpureRandom { .. }
                | solve::LinearOp::ImpureRandomInteger { .. }
        )
    })
}

fn direct_map_index(
    map: &solve::TensorOutputMap,
    ordinal: &[usize],
    span: rumoca_core::Span,
) -> Result<usize, GpuInitializationError> {
    let offset = map
        .strides
        .iter()
        .try_fold(0isize, |total, term| {
            total.checked_add(
                term.stride
                    .checked_mul(isize::try_from(*ordinal.get(term.dimension)?).ok()?)?,
            )
        })
        .ok_or_else(|| GpuInitializationError::Malformed {
            message: "direct target map overflow".to_string(),
            row: 0,
            span: Some(span),
        })?;
    map.start
        .checked_add_signed(offset)
        .ok_or_else(|| GpuInitializationError::Malformed {
            message: "direct target map overflow".to_string(),
            row: 0,
            span: Some(span),
        })
}

fn reject_unsupported_runtime_features(
    model: &solve::SolveModel,
) -> Result<(), GpuInitializationError> {
    let problem = &model.problem;
    let has_events = !problem.events.root_conditions.is_empty()
        || !problem.events.root_relation_memory_targets.is_empty()
        || !problem.events.scheduled_root_conditions.is_empty()
        || !problem.events.scheduled_time_events.is_empty()
        || !problem.events.dynamic_time_event_names.is_empty()
        || !problem.events.dynamic_time_event_rhs.is_empty()
        || !problem.events.action_conditions.is_empty()
        || !problem.events.actions.is_empty();
    let has_discrete = !problem.discrete.runtime_assignment_rhs.is_empty()
        || !problem.discrete.rhs.is_empty()
        || !problem.discrete.update_targets.is_empty()
        || !problem.discrete.pre_modes.is_empty();
    let has_memory = !problem
        .solve_layout
        .relation_memory_parameter_indices
        .is_empty()
        || !problem.solve_layout.pre_param_bindings.is_empty();
    if has_events
        || has_discrete
        || has_memory
        || !problem.clocks.periodic_event_schedules.is_empty()
    {
        return Err(GpuInitializationError::Unsupported {
            feature: "event, discrete, pre, relation-memory, or clock initialization",
            row: 0,
            span: None,
        });
    }
    Ok(())
}

fn ensure_finite(
    values: &[f64],
    kind: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), GpuInitializationError> {
    if let Some((row, value)) = values
        .iter()
        .copied()
        .enumerate()
        .find(|(_, value)| !value.is_finite())
    {
        return Err(GpuInitializationError::NonConverged {
            kind,
            row,
            value,
            span,
        });
    }
    let _ = kind;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_solve::{
        AffineStencilIndexStrideTerm, BinaryOp, ComputeBlock, ComputeNode, LinearOp,
        TensorNodeMetadata, TensorOutputMap,
    };

    fn span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("gpu_initialization_test.mo"),
            1,
            2,
        )
    }

    fn direct_model() -> solve::SolveModel {
        let span = span();
        let mut rows = vec![
            vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::Const { dst: 1, value: 2.0 },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ],
            vec![
                LinearOp::LoadY { dst: 0, index: 1 },
                LinearOp::Const { dst: 1, value: 0.0 },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ],
        ];
        let domain = rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        };
        let residual = ComputeNode::Map {
            domain: domain.clone(),
            output_map: TensorOutputMap::dense_contiguous(0, &domain).unwrap(),
            base_ops: rows.remove(0),
            load_strides: vec![rumoca_ir_solve::AffineStencilLoadStride {
                op_position: 0,
                terms: vec![AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 1,
                }],
            }],
            const_strides: vec![rumoca_ir_solve::AffineStencilConstStride {
                op_position: 1,
                terms: vec![rumoca_ir_solve::AffineStencilConstStrideTerm {
                    dimension: 0,
                    stride: -2.0,
                }],
            }],
            metadata: TensorNodeMetadata::default(),
            span,
        };
        let initialization = solve::InitializationSolveSystem {
            residual: ComputeBlock {
                nodes: vec![residual.clone()],
            },
            direct_families: vec![solve::InitializationDirectFamily {
                node_index: 0,
                targets: TensorOutputMap::dense_contiguous(0, &domain).unwrap(),
                residual_sign: 1,
                span,
            }],
            required_target_ranges: vec![solve::InitializationTargetRange {
                start: 0,
                end: 2,
                span: Some(span),
            }],
            ..Default::default()
        };
        solve::SolveModel {
            problem: solve::SolveProblem {
                initialization,
                ..Default::default()
            },
            initial_y: vec![0.0, 0.0],
            ..Default::default()
        }
    }

    fn singleton_domain(active_upper: i64) -> rumoca_core::StructuredIndexDomain {
        rumoca_core::StructuredIndexDomain {
            binders: vec![
                rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 1,
                    step: 1,
                },
                rumoca_core::StructuredIndexBinder {
                    id: 1,
                    display_name: "j".to_string(),
                    lower: 1,
                    upper: active_upper,
                    step: 1,
                },
            ],
        }
    }

    #[test]
    fn direct_initial_assignment_is_one_pass_with_linear_temporary_storage() {
        let result = settle_gpu_initial_conditions(&direct_model(), 0.0)
            .expect("proven direct rows should settle");
        assert_eq!(result.y0, vec![2.0, 0.0]);
        assert_eq!(result.metrics.residual_evaluations, 2);
        assert_eq!(result.metrics.passes, 1);
        assert!(result.metrics.temporary_values <= result.y0.len() * 3);
    }

    #[test]
    fn settlement_executes_mixed_singleton_domain_without_scalar_rows() {
        let mut model = direct_model();
        let domain = singleton_domain(2);
        let solve::ComputeNode::Map {
            domain: residual_domain,
            output_map,
            load_strides,
            const_strides,
            ..
        } = &mut model.problem.initialization.residual.nodes[0]
        else {
            unreachable!()
        };
        *residual_domain = domain.clone();
        *output_map = TensorOutputMap::dense_contiguous(0, &domain).unwrap();
        load_strides[0].terms[0].dimension = 1;
        const_strides[0].terms[0].dimension = 1;
        model.problem.initialization.direct_families[0].targets =
            TensorOutputMap::dense_contiguous(0, &domain).unwrap();

        assert!(model.problem.initialization.row_targets.is_empty());
        assert_eq!(
            model.problem.initialization.direct_families[0]
                .targets
                .strides,
            vec![AffineStencilIndexStrideTerm {
                dimension: 1,
                stride: 1,
            }]
        );
        let result = settle_gpu_initial_conditions(&model, 0.0)
            .expect("mixed-singleton direct rows should settle without scalar fallback");
        assert_eq!(result.y0, vec![2.0, 0.0]);
        assert_eq!(result.metrics.residual_evaluations, 2);
        assert_eq!(result.metrics.passes, 1);
    }

    #[test]
    fn settlement_executes_all_singleton_domain_with_empty_strides() {
        let mut model = direct_model();
        let domain = singleton_domain(1);
        let solve::ComputeNode::Map {
            domain: residual_domain,
            output_map,
            base_ops,
            load_strides,
            const_strides,
            ..
        } = &mut model.problem.initialization.residual.nodes[0]
        else {
            unreachable!()
        };
        *residual_domain = domain.clone();
        *output_map = TensorOutputMap::dense_contiguous(0, &domain).unwrap();
        let LinearOp::Const { value, .. } = &mut base_ops[1] else {
            unreachable!()
        };
        *value = 7.0;
        load_strides.clear();
        const_strides.clear();
        model.problem.initialization.direct_families[0].targets =
            TensorOutputMap::dense_contiguous(0, &domain).unwrap();
        model.problem.initialization.required_target_ranges[0].end = 1;
        model.initial_y.truncate(1);

        assert!(model.problem.initialization.row_targets.is_empty());
        assert!(
            model.problem.initialization.direct_families[0]
                .targets
                .strides
                .is_empty()
        );
        let result = settle_gpu_initial_conditions(&model, 0.0)
            .expect("all-singleton direct row should settle with empty strides");
        assert_eq!(result.y0, vec![7.0]);
        assert_eq!(result.metrics.residual_evaluations, 2);
        assert_eq!(result.metrics.passes, 1);
    }

    #[test]
    fn direct_initial_assignment_uses_model_external_tables_for_apply_and_verify() {
        let mut model = direct_model();
        let table_id = 515_151_u64;
        model.external_tables = solve::ExternalTables::new(vec![rumoca_core::ExternalTableData {
            id: table_id,
            data: vec![vec![1.0, 10.0], vec![3.0, 30.0]],
            columns: vec![2],
            smoothness: 3,
            extrapolation: 1,
        }]);
        let solve::ComputeNode::Map {
            base_ops,
            const_strides,
            ..
        } = &mut model.problem.initialization.residual.nodes[0]
        else {
            unreachable!()
        };
        *base_ops = vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::Const {
                dst: 1,
                value: table_id as f64,
            },
            LinearOp::Const { dst: 2, value: 1.0 },
            LinearOp::Const { dst: 3, value: 2.0 },
            LinearOp::TableLookup {
                dst: 4,
                table_id: 1,
                column: 2,
                input: 3,
            },
            LinearOp::Binary {
                dst: 5,
                op: BinaryOp::Sub,
                lhs: 0,
                rhs: 4,
            },
            LinearOp::StoreOutput { src: 5 },
        ];
        const_strides.clear();

        let settled = settle_gpu_initial_conditions(&model, 0.0)
            .expect("table-backed direct initialization should settle and verify");
        assert_eq!(settled.y0, vec![10.0, 10.0]);
    }

    #[test]
    fn settlement_admission_rejects_random_direct_initial_operations_with_span() {
        let mut model = direct_model();
        let solve::ComputeNode::Map { base_ops, .. } =
            &mut model.problem.initialization.residual.nodes[0]
        else {
            unreachable!()
        };
        base_ops.insert(0, LinearOp::ImpureRandomInit { dst: 6, seed: 1 });

        let error = settle_gpu_initial_conditions(&model, 0.0)
            .expect_err("random initialization cannot be replayed for residual verification");
        assert!(matches!(
            error,
            GpuInitializationError::Unsupported {
                feature: "random or impure direct initial operations",
                span: Some(actual),
                ..
            } if actual == span()
        ));
    }

    #[test]
    fn settlement_rejects_partial_required_target_union_independently() {
        let mut model = direct_model();
        model.problem.initialization.required_target_ranges[0].end = 1;
        let error = settle_gpu_initial_conditions(&model, 0.0)
            .expect_err("settlement must reject a partial hand-built artifact");
        assert!(error.to_string().contains("incomplete"));
    }

    #[test]
    fn settlement_rejects_partial_fixed_only_metadata_before_empty_residual_return() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("fixed_only_partial.mo"),
            20,
            30,
        );
        let mut model = solve::SolveModel {
            initial_y: vec![1.0, 2.0],
            ..Default::default()
        };
        model.problem.initialization.required_target_ranges =
            vec![solve::InitializationTargetRange {
                start: 0,
                end: 2,
                span: Some(span),
            }];
        model.problem.initialization.fixed_target_ranges = vec![solve::InitializationTargetRange {
            start: 0,
            end: 1,
            span: Some(span),
        }];

        let error = settle_gpu_initial_conditions(&model, 0.0)
            .expect_err("partial fixed-only metadata must be validated before early return");
        assert!(error.to_string().contains("incomplete"));
        assert!(matches!(
            error,
            GpuInitializationError::Malformed { span: Some(actual), .. } if actual == span
        ));
    }

    #[test]
    fn settlement_rejects_duplicate_direct_node_ownership() {
        let mut model = direct_model();
        let node = model.problem.initialization.residual.nodes[0].clone();
        model.problem.initialization.residual.nodes.push(node);
        let mut duplicate = model.problem.initialization.direct_families[0].clone();
        duplicate.targets.start = 2;
        model.problem.initialization.direct_families.push(duplicate);
        model.problem.initialization.required_target_ranges[0].end = 4;
        model.initial_y.resize(4, 0.0);

        let error = settle_gpu_initial_conditions(&model, 0.0)
            .expect_err("every residual node must have one unique direct owner");
        assert!(error.to_string().contains("duplicate"));
    }

    #[test]
    fn settlement_rejects_unowned_residual_node() {
        let mut model = direct_model();
        model
            .problem
            .initialization
            .residual
            .nodes
            .push(model.problem.initialization.residual.nodes[0].clone());

        let error = settle_gpu_initial_conditions(&model, 0.0)
            .expect_err("every residual node must have a direct owner");
        assert!(error.to_string().contains("own every residual node"));
    }

    #[test]
    fn settlement_rejects_direct_fixed_overlap_at_fixed_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("direct_fixed_overlap.mo"),
            40,
            50,
        );
        let mut model = direct_model();
        model.problem.initialization.fixed_target_ranges = vec![solve::InitializationTargetRange {
            start: 1,
            end: 2,
            span: Some(span),
        }];

        let error = settle_gpu_initial_conditions(&model, 0.0)
            .expect_err("simulation must reject direct/fixed ownership overlap");
        assert!(error.to_string().contains("overlap"));
        assert!(matches!(
            error,
            GpuInitializationError::Malformed { span: Some(actual), .. } if actual == span
        ));
    }

    #[test]
    fn settlement_accepts_adjacent_complete_fixed_only_ranges() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("adjacent_fixed.mo"),
            10,
            20,
        );
        let mut model = solve::SolveModel {
            initial_y: vec![-0.0, 2.0],
            ..Default::default()
        };
        model.problem.initialization.required_target_ranges =
            vec![solve::InitializationTargetRange {
                start: 0,
                end: 2,
                span: Some(span),
            }];
        model.problem.initialization.fixed_target_ranges = vec![
            solve::InitializationTargetRange {
                start: 0,
                end: 1,
                span: Some(span),
            },
            solve::InitializationTargetRange {
                start: 1,
                end: 2,
                span: Some(span),
            },
        ];

        let settled = settle_gpu_initial_conditions(&model, 0.0)
            .expect("adjacent fixed ranges form exact complete coverage");
        assert_eq!(settled.y0[0].to_bits(), (-0.0f64).to_bits());
        assert_eq!(settled.y0[1], 2.0);
    }

    #[test]
    fn settlement_invalid_range_reports_span_after_json_and_bincode() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("invalid_range_roundtrip.mo"),
            70,
            80,
        );
        let range = solve::InitializationTargetRange {
            start: 0,
            end: 3,
            span: Some(span),
        };
        let json = serde_json::to_string(&range).expect("serialize invalid range JSON");
        let from_json: solve::InitializationTargetRange =
            serde_json::from_str(&json).expect("deserialize invalid range JSON");
        let bytes = bincode::serialize(&range).expect("serialize invalid range bincode");
        let from_bincode: solve::InitializationTargetRange =
            bincode::deserialize(&bytes).expect("deserialize invalid range bincode");

        for decoded in [from_json, from_bincode] {
            let mut model = solve::SolveModel {
                initial_y: vec![0.0, 0.0],
                ..Default::default()
            };
            model.problem.initialization.required_target_ranges = vec![decoded];
            let error = settle_gpu_initial_conditions(&model, 0.0)
                .expect_err("out-of-bounds range must fail closed");
            assert!(matches!(
                error,
                GpuInitializationError::Malformed { span: Some(actual), .. } if actual == span
            ));
        }
    }

    #[test]
    fn event_system_is_rejected_before_returning_initial_vectors() {
        let mut model = solve::SolveModel::default();
        model.problem.events.scheduled_time_events.push(0.0);
        let error = settle_gpu_initial_conditions(&model, 0.0)
            .expect_err("GPU preparation must reject event systems");
        assert!(matches!(error, GpuInitializationError::Unsupported { .. }));
    }

    #[test]
    fn nonfinite_initial_vector_is_rejected_without_partial_settlement() {
        let mut model = direct_model();
        model.initial_y[0] = f64::NAN;

        let error = settle_gpu_initial_conditions(&model, 0.0)
            .expect_err("non-finite GPU initialization input must fail closed");
        assert!(matches!(error, GpuInitializationError::NonConverged { .. }));
        assert!(error.to_string().contains("initial y"));
    }

    #[test]
    fn no_initial_equations_preserve_finite_vectors_exactly() {
        let model = solve::SolveModel {
            initial_y: vec![-0.0, 3.25],
            parameters: vec![7.5, -2.0],
            ..Default::default()
        };
        let result = settle_gpu_initial_conditions(&model, 0.0).expect("finite vectors pass");
        assert_eq!(
            result.y0.iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
            model
                .initial_y
                .iter()
                .map(|v| v.to_bits())
                .collect::<Vec<_>>()
        );
        assert_eq!(
            result.p0.iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
            model
                .parameters
                .iter()
                .map(|v| v.to_bits())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn no_initial_equations_still_reject_nonfinite_vectors() {
        let model = solve::SolveModel {
            initial_y: vec![f64::INFINITY],
            ..Default::default()
        };
        assert!(settle_gpu_initial_conditions(&model, 0.0).is_err());
    }
}
