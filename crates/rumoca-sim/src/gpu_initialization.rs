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
    if initialization.residual.is_empty() {
        return Ok(GpuInitializationResult {
            y0: model.initial_y.clone(),
            p0: model.parameters.clone(),
            metrics: GpuInitializationMetrics::default(),
        });
    }
    validate_assignment_shape(initialization, model.initial_y.len())?;
    let mut y0 = model.initial_y.clone();
    let p0 = model.parameters.clone();
    ensure_finite(&y0, "initial y", None)?;
    ensure_finite(&p0, "initial p", None)?;
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
    if initialization.direct_families.len() != initialization.residual.nodes.len() {
        return Err(GpuInitializationError::Malformed {
            message: "direct initial families must own every residual Map".to_string(),
            row: 0,
            span: None,
        });
    }
    for family in &initialization.direct_families {
        if !matches!(family.residual_sign, -1 | 1) {
            return Err(GpuInitializationError::Malformed {
                message: "direct initial family must have a unit residual sign".to_string(),
                row: 0,
                span: Some(family.span),
            });
        }
        let Some(solve::ComputeNode::Map { domain, .. }) =
            initialization.residual.nodes.get(family.node_index)
        else {
            return Err(GpuInitializationError::Unsupported {
                feature: "non-Map direct initial family",
                row: 0,
                span: Some(family.span),
            });
        };
        let _ = (domain, y_len);
    }
    Ok(())
}

struct DirectFamilyExecution<'a> {
    initialization: &'a solve::InitializationSolveSystem,
    y: &'a mut [f64],
    p: &'a [f64],
    t: f64,
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
        rumoca_eval_solve::RowEvalContext::default(),
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
}
