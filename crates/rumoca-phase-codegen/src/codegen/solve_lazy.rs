//! Lazy minijinja [`Object`] wrappers over the Solve IR.
//!
//! `Value::from_serialize` of the `SolveProblem` materializes every `LinearOp`
//! into ~2 `IndexMap`s each (~64x the JSON size) — gigabytes for a 150k-op
//! model. These wrappers expose the IR to templates *lazily*: structural fields
//! are produced on demand and op lists materialize one op at a time during
//! iteration, so peak memory is O(one program) instead of O(whole problem).
//! Targets that never access a field (e.g. c-solve never touches
//! `solve.continuous`) pay nothing for it. The Rust render functions can also
//! `downcast_object_ref` to [`SolveProgramsObject`] / [`SolveOpListObject`] to
//! iterate the typed ops directly with zero materialization.

use std::fmt;
use std::sync::Arc;

use minijinja::Value;
use minijinja::value::{Enumerator, Object, ObjectRepr};
use rumoca_ir_solve as solve;

// ── Generic lazy Map / Seq ───────────────────────────────────────────────────

type MapGet = Arc<dyn Fn(&str) -> Option<Value> + Send + Sync>;

struct LazyMap {
    keys: &'static [&'static str],
    get: MapGet,
}

impl fmt::Debug for LazyMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("LazyMap")
    }
}

impl Object for LazyMap {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Map
    }
    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Iter(Box::new(self.keys.iter().copied().map(Value::from)))
    }
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        (self.get)(key.as_str()?)
    }
}

fn lazy_map(
    keys: &'static [&'static str],
    get: impl Fn(&str) -> Option<Value> + Send + Sync + 'static,
) -> Value {
    Value::from_object(LazyMap {
        keys,
        get: Arc::new(get),
    })
}

type SeqGet = Arc<dyn Fn(usize) -> Value + Send + Sync>;

struct LazySeq {
    len: usize,
    get: SeqGet,
}

impl fmt::Debug for LazySeq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("LazySeq")
    }
}

impl Object for LazySeq {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Seq
    }
    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Seq(self.len)
    }
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let idx = key.as_usize()?;
        (idx < self.len).then(|| (self.get)(idx))
    }
}

fn lazy_seq(len: usize, get: impl Fn(usize) -> Value + Send + Sync + 'static) -> Value {
    Value::from_object(LazySeq {
        len,
        get: Arc::new(get),
    })
}

// ── Op-list leaves (downcast-able by the render functions) ───────────────────

/// The `programs` list of a `ScalarProgramBlock` (a Seq of op-lists). Render
/// functions downcast this to iterate the typed programs/ops directly.
#[derive(Debug)]
pub(super) struct SolveProgramsObject {
    pub(super) block: Arc<solve::ScalarProgramBlock>,
}

impl Object for SolveProgramsObject {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Seq
    }
    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Seq(self.block.programs.len())
    }
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let idx = key.as_usize()?;
        (idx < self.block.programs.len()).then(|| {
            Value::from_object(SolveOpListObject {
                ops: OpListSource::Program(self.block.clone(), idx),
            })
        })
    }
}

/// A single op list — either a program inside a `ScalarProgramBlock` or a raw
/// `Vec<LinearOp>` (matmul/linsolve operand ops). Materializes one op at a time.
#[derive(Debug)]
pub(super) struct SolveOpListObject {
    ops: OpListSource,
}

#[derive(Debug)]
enum OpListSource {
    Program(Arc<solve::ScalarProgramBlock>, usize),
    Raw(Arc<Vec<solve::LinearOp>>),
}

impl SolveOpListObject {
    fn ops(&self) -> &[solve::LinearOp] {
        match &self.ops {
            OpListSource::Program(block, idx) => &block.programs[*idx],
            OpListSource::Raw(ops) => ops,
        }
    }
}

impl Object for SolveOpListObject {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Seq
    }
    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Seq(self.ops().len())
    }
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let idx = key.as_usize()?;
        self.ops().get(idx).map(Value::from_serialize)
    }
}

fn raw_op_list_value(ops: Arc<Vec<solve::LinearOp>>) -> Value {
    Value::from_object(SolveOpListObject {
        ops: OpListSource::Raw(ops),
    })
}

// ── IR hierarchy ─────────────────────────────────────────────────────────────

fn scalar_program_block_value(block: Arc<solve::ScalarProgramBlock>) -> Value {
    lazy_map(&["programs", "program_spans"], move |k| match k {
        "programs" => Some(Value::from_object(SolveProgramsObject {
            block: block.clone(),
        })),
        "program_spans" => Some(Value::from_serialize(&block.program_spans)),
        _ => None,
    })
}

fn compute_node_value(node: Arc<solve::ComputeNode>) -> Value {
    // Serialized as a tagged enum: { "MatMul": {...} } / { "ScalarPrograms": ... }
    // / { "LinSolve": {...} }. Only the active variant key is present.
    match node.as_ref() {
        solve::ComputeNode::ScalarPrograms(_) => lazy_map(&["ScalarPrograms"], move |k| {
            (k == "ScalarPrograms").then(|| {
                let solve::ComputeNode::ScalarPrograms(block) = node.as_ref() else {
                    unreachable!()
                };
                scalar_program_block_value(Arc::new(block.clone()))
            })
        }),
        solve::ComputeNode::MatMul { .. } => lazy_map(&["MatMul"], move |k| {
            (k == "MatMul").then(|| matmul_value(node.clone()))
        }),
        solve::ComputeNode::LinSolve { .. } => lazy_map(&["LinSolve"], move |k| {
            (k == "LinSolve").then(|| linsolve_value(node.clone()))
        }),
    }
}

fn matmul_value(node: Arc<solve::ComputeNode>) -> Value {
    lazy_map(
        &[
            "lhs_ops",
            "lhs_start",
            "rhs_ops",
            "rhs_start",
            "m",
            "k",
            "n",
            "lhs_sparsity",
            "rhs_sparsity",
            "metadata",
            "span",
        ],
        move |k| {
            let solve::ComputeNode::MatMul {
                lhs_ops,
                lhs_start,
                rhs_ops,
                rhs_start,
                m,
                k: kk,
                n,
                lhs_sparsity,
                rhs_sparsity,
                metadata,
                span,
            } = node.as_ref()
            else {
                unreachable!()
            };
            match k {
                "lhs_ops" => Some(raw_op_list_value(Arc::new(lhs_ops.clone()))),
                "rhs_ops" => Some(raw_op_list_value(Arc::new(rhs_ops.clone()))),
                "lhs_start" => Some(Value::from(*lhs_start)),
                "rhs_start" => Some(Value::from(*rhs_start)),
                "m" => Some(Value::from(*m)),
                "k" => Some(Value::from(*kk)),
                "n" => Some(Value::from(*n)),
                "lhs_sparsity" => Some(Value::from_serialize(lhs_sparsity)),
                "rhs_sparsity" => Some(Value::from_serialize(rhs_sparsity)),
                "metadata" => Some(Value::from_serialize(metadata)),
                "span" => Some(Value::from_serialize(span)),
                _ => None,
            }
        },
    )
}

fn linsolve_value(node: Arc<solve::ComputeNode>) -> Value {
    lazy_map(
        &[
            "setup_ops",
            "matrix_start",
            "rhs_start",
            "n",
            "next_reg",
            "metadata",
            "span",
        ],
        move |k| {
            let solve::ComputeNode::LinSolve {
                setup_ops,
                matrix_start,
                rhs_start,
                n,
                next_reg,
                metadata,
                span,
            } = node.as_ref()
            else {
                unreachable!()
            };
            match k {
                "setup_ops" => Some(raw_op_list_value(Arc::new(setup_ops.clone()))),
                "matrix_start" => Some(Value::from(*matrix_start)),
                "rhs_start" => Some(Value::from(*rhs_start)),
                "n" => Some(Value::from(*n)),
                "next_reg" => Some(Value::from(*next_reg)),
                "metadata" => Some(Value::from_serialize(metadata)),
                "span" => Some(Value::from_serialize(span)),
                _ => None,
            }
        },
    )
}

/// Lazy view of a `ComputeBlock` exposing `nodes` (structured) plus the derived
/// `scalar_programs` fallback and counts — matching `solve_template_blocks_value`.
pub(super) fn compute_block_value(block: Arc<solve::ComputeBlock>) -> Value {
    lazy_map(
        &[
            "nodes",
            "scalar_programs",
            "output_count",
            "tensor_node_count",
            "scalar_programs_use_linear_solve_component",
        ],
        move |k| match k {
            "nodes" => {
                let block = block.clone();
                Some(lazy_seq(block.nodes.len(), move |i| {
                    compute_node_value(Arc::new(block.nodes[i].clone()))
                }))
            }
            "scalar_programs" => {
                let scalar = Arc::new(rumoca_eval_solve::to_scalar_program_block(&block));
                Some(scalar_program_block_value(scalar))
            }
            "output_count" => Some(Value::from(block.len())),
            "tensor_node_count" => Some(Value::from(block.tensor_node_count())),
            "scalar_programs_use_linear_solve_component" => {
                let scalar = rumoca_eval_solve::to_scalar_program_block(&block);
                Some(Value::from(
                    super::scalar_program_block_uses_linear_solve_component(&scalar),
                ))
            }
            _ => None,
        },
    )
}

fn continuous_value(problem: Arc<solve::SolveProblem>) -> Value {
    lazy_map(
        &[
            "implicit_rhs",
            "implicit_row_targets",
            "algebraic_projection_plan",
            "residual",
            "derivative_rhs",
        ],
        move |k| {
            let c = &problem.continuous;
            match k {
                "implicit_rhs" => Some(compute_block_value(Arc::new(c.implicit_rhs.clone()))),
                "derivative_rhs" => Some(compute_block_value(Arc::new(c.derivative_rhs.clone()))),
                "residual" => Some(scalar_program_block_value(Arc::new(c.residual.clone()))),
                "implicit_row_targets" => Some(Value::from_serialize(&c.implicit_row_targets)),
                "algebraic_projection_plan" => {
                    Some(Value::from_serialize(&c.algebraic_projection_plan))
                }
                _ => None,
            }
        },
    )
}

fn discrete_value(problem: Arc<solve::SolveProblem>) -> Value {
    lazy_map(
        &[
            "runtime_assignment_rhs",
            "runtime_assignment_targets",
            "rhs",
            "update_targets",
            "pre_modes",
            "observation_refresh",
        ],
        move |k| {
            let d = &problem.discrete;
            match k {
                "rhs" => Some(scalar_program_block_value(Arc::new(d.rhs.clone()))),
                "runtime_assignment_rhs" => Some(scalar_program_block_value(Arc::new(
                    d.runtime_assignment_rhs.clone(),
                ))),
                "runtime_assignment_targets" => {
                    Some(Value::from_serialize(&d.runtime_assignment_targets))
                }
                "update_targets" => Some(Value::from_serialize(&d.update_targets)),
                "pre_modes" => Some(Value::from_serialize(&d.pre_modes)),
                "observation_refresh" => Some(Value::from_serialize(&d.observation_refresh)),
                _ => None,
            }
        },
    )
}

fn events_value(problem: Arc<solve::SolveProblem>) -> Value {
    lazy_map(
        &[
            "root_conditions",
            "root_relation_memory_targets",
            "scheduled_time_events",
            "dynamic_time_event_names",
            "dynamic_time_event_rhs",
            "action_conditions",
            "actions",
        ],
        move |k| {
            let e = &problem.events;
            match k {
                "root_conditions" => {
                    Some(scalar_program_block_value(Arc::new(e.root_conditions.clone())))
                }
                "dynamic_time_event_rhs" => Some(scalar_program_block_value(Arc::new(
                    e.dynamic_time_event_rhs.clone(),
                ))),
                "action_conditions" => {
                    Some(scalar_program_block_value(Arc::new(e.action_conditions.clone())))
                }
                "root_relation_memory_targets" => {
                    Some(Value::from_serialize(&e.root_relation_memory_targets))
                }
                "scheduled_time_events" => Some(Value::from_serialize(&e.scheduled_time_events)),
                "dynamic_time_event_names" => {
                    Some(Value::from_serialize(&e.dynamic_time_event_names))
                }
                "actions" => Some(Value::from_serialize(&e.actions)),
                _ => None,
            }
        },
    )
}

pub(super) fn artifacts_value(artifacts: Arc<solve::SolveArtifacts>) -> Value {
    lazy_map(&["continuous"], move |k| {
        (k == "continuous").then(|| {
            let artifacts = artifacts.clone();
            lazy_map(
                &["mass_matrix", "implicit_jacobian_v", "full_jacobian_v"],
                move |k| {
                    let c = &artifacts.continuous;
                    match k {
                        "implicit_jacobian_v" => {
                            Some(compute_block_value(Arc::new(c.implicit_jacobian_v.clone())))
                        }
                        "full_jacobian_v" => {
                            Some(scalar_program_block_value(Arc::new(c.full_jacobian_v.clone())))
                        }
                        "mass_matrix" => Some(Value::from_serialize(&c.mass_matrix)),
                        _ => None,
                    }
                },
            )
        })
    })
}

/// Lazy `solve` context object: the `SolveProblem` fields plus an embedded
/// `artifacts` field (templates access `solve.artifacts.*`). Structural fields
/// (`layout`, `solve_layout`, targets) serialize eagerly (small); op-heavy
/// sub-systems are produced lazily.
pub(super) fn solve_value(
    problem: Arc<solve::SolveProblem>,
    artifacts: Arc<solve::SolveArtifacts>,
) -> Value {
    lazy_map(
        &[
            "schema_version",
            "layout",
            "solve_layout",
            "continuous",
            "initialization",
            "discrete",
            "events",
            "clocks",
            "artifacts",
        ],
        move |k| match k {
            "schema_version" => Some(Value::from(problem.schema_version)),
            "layout" => Some(Value::from_serialize(&problem.layout)),
            "solve_layout" => Some(Value::from_serialize(&problem.solve_layout)),
            "continuous" => Some(continuous_value(problem.clone())),
            "discrete" => Some(discrete_value(problem.clone())),
            "events" => Some(events_value(problem.clone())),
            "initialization" => Some(Value::from_serialize(&problem.initialization)),
            "clocks" => Some(Value::from_serialize(&problem.clocks)),
            "artifacts" => Some(artifacts_value(artifacts.clone())),
            _ => None,
        },
    )
}

/// Lazy `nodes` Seq of a `ComputeBlock` (each `ComputeNode` materialized on
/// demand, with its op lists lazy underneath).
pub(super) fn nodes_value(block: Arc<solve::ComputeBlock>) -> Value {
    let len = block.nodes.len();
    lazy_seq(len, move |i| {
        compute_node_value(Arc::new(block.nodes[i].clone()))
    })
}

/// Lazy `solve_derivative_nodes`: the `nodes` of the continuous derivative block.
pub(super) fn derivative_nodes_value(problem: Arc<solve::SolveProblem>) -> Value {
    let len = problem.continuous.derivative_rhs.nodes.len();
    lazy_seq(len, move |i| {
        compute_node_value(Arc::new(problem.continuous.derivative_rhs.nodes[i].clone()))
    })
}

