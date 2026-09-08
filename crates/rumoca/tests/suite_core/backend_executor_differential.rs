//! Differential oracle over the two Solve-block executors.
//!
//! `rumoca-eval-solve` interprets a `ScalarProgramBlock`; `rumoca-exec-cranelift`
//! compiles the same block through the entry point `rumoca-sim`'s native backend
//! uses. Both read identical `(y, p, t)` inputs, so any disagreement is a
//! meaning change in one of them.
//!
//! Every case pins the row opcode it exists to cover to a block that produced
//! at least one finite compared value. Presence of an opcode anywhere in the
//! model proves nothing: a case whose opcode never lands in a compared block is
//! red, and the failure names every block with its opcode census.

use std::collections::{BTreeMap, BTreeSet};

use rumoca::Compiler;
use rumoca_eval_solve::{PreparedScalarProgramBlock, RowEvalContext, to_scalar_program_block};
use rumoca_exec_cranelift::{
    CompiledExpressionRows, CompiledPureCallTable, compile_expression_scalar_program_block,
    compile_expression_scalar_program_block_with_pure_calls,
};
use rumoca_ir_solve::{ComputeBlock, LinearOp, ScalarProgramBlock, SolveModel, SolvePureCallTable};
use rumoca_sim::SimOptions;

/// Agreement both executors must reach on a finite value, scaled by the larger
/// magnitude and floored at one so slots near zero carry an absolute bound.
const AGREEMENT_TOLERANCE: f64 = 1.0e-12;

/// Probe points evaluated per block.
const PROBE_COUNT: usize = 4;

/// What one Solve block contributed to the differential across every probe.
#[derive(Default)]
struct BlockOutcome {
    opcodes: BTreeSet<&'static str>,
    finite_compared: usize,
    values_compared: usize,
    notes: Vec<String>,
}

/// The whole differential for one model: what each block contributed, and every
/// disagreement observed.
struct Differential {
    model: String,
    outcomes: BTreeMap<String, BlockOutcome>,
    divergences: Vec<String>,
}

impl Differential {
    fn finite_compared(&self) -> usize {
        self.outcomes
            .values()
            .map(|outcome| outcome.finite_compared)
            .sum()
    }

    /// Every block, its opcode census, and what it compared. This is the
    /// diagnostic a vacuity failure prints.
    fn census(&self) -> String {
        self.outcomes
            .iter()
            .map(|(label, outcome)| {
                let opcodes = outcome
                    .opcodes
                    .iter()
                    .copied()
                    .collect::<Vec<_>>()
                    .join(", ");
                let notes = if outcome.notes.is_empty() {
                    String::new()
                } else {
                    format!("\n      notes: {}", outcome.notes.join("; "))
                };
                format!(
                    "  {label}: finite_compared={} values_compared={}\n      opcodes: [{opcodes}]{notes}",
                    outcome.finite_compared, outcome.values_compared
                )
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Fail unless `opcode` occurs in a block that produced a finite compared
    /// value. Counting opcodes that merely occur somewhere in the model is what
    /// lets a differential pass while comparing nothing.
    fn require_compared_opcode(&self, opcode: &str) {
        let covered = self.outcomes.values().any(|outcome| {
            outcome.finite_compared > 0 && outcome.opcodes.iter().any(|kind| *kind == opcode)
        });
        assert!(
            covered,
            "`{}` compared no value produced by a block containing `{opcode}`;\n{}",
            self.model,
            self.census()
        );
    }

    fn require_agreement(&self) {
        assert!(
            self.divergences.is_empty(),
            "`{}`: the interpreter and the native executor disagree:\n{}\n{}",
            self.model,
            self.divergences.join("\n"),
            self.census()
        );
    }
}

/// One block handed to both executors, named for the failure message. A block
/// the evaluator boundary cannot even scalarize keeps its place in the report
/// carrying the reason, so it can never vanish from the census unremarked.
struct NamedBlock {
    label: String,
    block: ScalarProgramBlock,
    unavailable: Option<String>,
}

fn scalarized(label: &str, block: &ComputeBlock, into: &mut Vec<NamedBlock>) {
    let (block, unavailable) = match to_scalar_program_block(block) {
        Ok(scalar) => (scalar, None),
        Err(error) => (
            ScalarProgramBlock::default(),
            Some(format!(
                "the evaluator boundary cannot scalarize it: {error}"
            )),
        ),
    };
    into.push(NamedBlock {
        label: label.to_string(),
        block,
        unavailable,
    });
}

fn scalar(label: &str, block: &ScalarProgramBlock, into: &mut Vec<NamedBlock>) {
    into.push(NamedBlock {
        label: label.to_string(),
        block: block.clone(),
        unavailable: None,
    });
}

/// The value-producing blocks a runtime evaluates from `(y, p, t)` alone.
/// Jacobian blocks are excluded: they consume a seed vector and are compiled
/// through a different entry point.
fn blocks_under_test(model: &SolveModel) -> Vec<NamedBlock> {
    let mut blocks = Vec::new();
    let continuous = model.problem().continuous();
    scalarized(
        "continuous.implicit_rhs",
        continuous.implicit_rhs(),
        &mut blocks,
    );
    scalarized("continuous.residual", continuous.residual(), &mut blocks);
    scalarized(
        "continuous.derivative_rhs",
        continuous.derivative_rhs(),
        &mut blocks,
    );
    scalarized(
        "continuous.manifold_residual",
        continuous.manifold_residual(),
        &mut blocks,
    );
    let initialization = model.problem().initialization();
    scalarized(
        "initialization.residual",
        initialization.residual(),
        &mut blocks,
    );
    scalar(
        "initialization.update_rhs",
        initialization.update_rhs(),
        &mut blocks,
    );
    let discrete = model.problem().discrete();
    scalar("discrete.rhs", &discrete.rhs, &mut blocks);
    scalar(
        "discrete.runtime_assignment_rhs",
        &discrete.runtime_assignment_rhs,
        &mut blocks,
    );
    scalarized(
        "discrete.structured_rhs",
        &discrete.structured_rhs,
        &mut blocks,
    );
    scalar(
        "events.root_conditions",
        &model.problem().events().root_conditions,
        &mut blocks,
    );
    scalar(
        "visible_value_rows",
        model.visible_value_rows(),
        &mut blocks,
    );
    blocks.retain(|named| !named.block.programs().is_empty() || named.unavailable.is_some());
    blocks
}

fn census(ops: &[LinearOp], into: &mut BTreeSet<&'static str>) {
    for op in ops {
        into.insert(op.kind_name());
        match op {
            LinearOp::FunctionFold { program, .. }
            | LinearOp::GuardedFunctionFold { program, .. }
            | LinearOp::StoreOutputFunctionFold { program, .. } => census(program.update(), into),
            LinearOp::FunctionConditional { program, .. } => {
                for arm in program.arms() {
                    census(arm.condition(), into);
                    census(arm.result(), into);
                }
                census(program.fallback(), into);
            }
            _ => {}
        }
    }
}

fn block_census(block: &ScalarProgramBlock) -> BTreeSet<&'static str> {
    let mut kinds = BTreeSet::new();
    for row in block.programs() {
        census(row, &mut kinds);
    }
    kinds
}

/// Deterministic probe points around the model's own start vector. The jitter
/// is a fixed function of the slot and probe ordinals, so a failure reproduces
/// from the message alone.
fn probe_points(model: &SolveModel) -> Vec<(Vec<f64>, f64)> {
    (0..PROBE_COUNT)
        .map(|probe| {
            let y = model
                .initial_y()
                .iter()
                .enumerate()
                .map(|(slot, value)| {
                    let ordinal = (slot * 31 + probe * 17 + 1) as f64;
                    value + 0.4 * ordinal.sin()
                })
                .collect::<Vec<_>>();
            (y, 0.125 * probe as f64)
        })
        .collect()
}

/// Agreement on one output slot. `NaN` matches `NaN` and an infinity matches
/// the same-signed infinity; finite values must agree to `AGREEMENT_TOLERANCE`.
fn agrees(interpreted: f64, native: f64) -> bool {
    if interpreted.is_nan() || native.is_nan() {
        return interpreted.is_nan() && native.is_nan();
    }
    if interpreted.is_infinite() || native.is_infinite() {
        return interpreted == native;
    }
    let scale = interpreted.abs().max(native.abs()).max(1.0);
    (interpreted - native).abs() <= AGREEMENT_TOLERANCE * scale
}

struct ProbeInputs<'a> {
    y: &'a [f64],
    p: &'a [f64],
    t: f64,
}

struct ProbeRequest<'a> {
    label: &'a str,
    prepared: &'a PreparedScalarProgramBlock,
    compiled: &'a CompiledExpressionRows,
    inputs: ProbeInputs<'a>,
    pure_calls: &'a SolvePureCallTable,
}

/// Compare one block at one probe point, folding the result into `outcome` and
/// appending any disagreement to `divergences`.
fn compare_probe(
    request: ProbeRequest<'_>,
    outcome: &mut BlockOutcome,
    divergences: &mut Vec<String>,
) {
    let ProbeRequest {
        label,
        prepared,
        compiled,
        inputs,
        pure_calls,
    } = request;
    let output_count = prepared.len();
    let mut interpreted_out = vec![0.0; output_count];
    let mut native_out = vec![0.0; output_count];
    let context = RowEvalContext {
        pure_calls: Some(pure_calls),
        ..Default::default()
    };
    let interpreted =
        prepared.eval_with_context(inputs.y, inputs.p, inputs.t, context, &mut interpreted_out);
    let native = compiled.call(inputs.y, inputs.p, inputs.t, &mut native_out);
    let at = format!("{label} at t={}", inputs.t);
    match (interpreted, native) {
        (Ok(()), Ok(())) => {
            for (slot, (left, right)) in interpreted_out.iter().zip(&native_out).enumerate() {
                outcome.values_compared += 1;
                if left.is_finite() && right.is_finite() {
                    outcome.finite_compared += 1;
                }
                if !agrees(*left, *right) {
                    divergences.push(format!(
                        "  {at}: output[{slot}] interpreter={left:?} native={right:?}"
                    ));
                }
            }
        }
        (Err(interpreted), Err(native)) => outcome.notes.push(format!(
            "both executors refused at t={}: interpreter={interpreted}, native={native}",
            inputs.t
        )),
        (Err(interpreted), Ok(())) => {
            let shape = if native_out.iter().all(|value| value.is_nan()) {
                "an all-NaN result"
            } else {
                "a result that is not all-NaN"
            };
            divergences.push(format!(
                "  {at}: the interpreter refused ({interpreted}) while the native \
                 executor returned Ok with {shape}: {native_out:?}"
            ));
        }
        (Ok(()), Err(native)) => divergences.push(format!(
            "  {at}: the native executor refused ({native}) while the interpreter \
             returned Ok: {interpreted_out:?}"
        )),
    }
}

fn differential(source: &str, model_name: &str) -> Differential {
    let compiled = Compiler::new()
        .model(model_name)
        .compile_str(source, &format!("{model_name}.mo"))
        .unwrap_or_else(|error| panic!("compile {model_name}: {error:?}"));
    let opts = SimOptions::default();
    let model = rumoca_sim::lower_for_simulation_with_overrides(compiled.dae(), &opts)
        .unwrap_or_else(|error| panic!("lower {model_name}: {error:?}"));

    let pure_calls = rumoca_exec_cranelift::compile_pure_call_table(model.pure_calls()).ok();
    let probes = probe_points(&model);
    let mut outcomes = BTreeMap::new();
    let mut divergences = Vec::new();
    for named in blocks_under_test(&model) {
        let mut outcome = BlockOutcome {
            opcodes: block_census(&named.block),
            ..BlockOutcome::default()
        };
        evaluate_block(
            &named,
            &model,
            pure_calls.as_ref(),
            &probes,
            &mut outcome,
            &mut divergences,
        );
        outcomes.insert(named.label, outcome);
    }
    Differential {
        model: model_name.to_string(),
        outcomes,
        divergences,
    }
}

fn evaluate_block(
    named: &NamedBlock,
    model: &SolveModel,
    pure_calls: Option<&CompiledPureCallTable>,
    probes: &[(Vec<f64>, f64)],
    outcome: &mut BlockOutcome,
    divergences: &mut Vec<String>,
) {
    if let Some(reason) = &named.unavailable {
        outcome.notes.push(reason.clone());
        return;
    }
    let requirements =
        match rumoca_eval_solve::scalar_program_block_input_requirements(&named.block) {
            Ok(requirements) => requirements,
            Err(error) => {
                outcome
                    .notes
                    .push(format!("input requirements unavailable: {error}"));
                return;
            }
        };
    if requirements.seed_len > 0 {
        outcome.notes.push(format!(
            "reads {} seed slots; the expression entry point supplies none",
            requirements.seed_len
        ));
        return;
    }
    if requirements.y_len > model.initial_y().len() || requirements.p_len > model.parameters().len()
    {
        outcome.notes.push(format!(
            "requires y_len={} p_len={} beyond the model's y_len={} p_len={}",
            requirements.y_len,
            requirements.p_len,
            model.initial_y().len(),
            model.parameters().len()
        ));
        return;
    }
    let prepared = match PreparedScalarProgramBlock::new(named.block.clone()) {
        Ok(prepared) => prepared,
        Err(error) => {
            outcome
                .notes
                .push(format!("the interpreter refused the block: {error}"));
            return;
        }
    };
    let compiled = match pure_calls {
        Some(table) => compile_expression_scalar_program_block_with_pure_calls(&named.block, table),
        None => compile_expression_scalar_program_block(&named.block),
    };
    let compiled = match compiled {
        Ok(compiled) => compiled,
        Err(error) => {
            outcome
                .notes
                .push(format!("the native backend refused the block: {error}"));
            return;
        }
    };
    for (y, t) in probes {
        compare_probe(
            ProbeRequest {
                label: &named.label,
                prepared: &prepared,
                compiled: &compiled,
                inputs: ProbeInputs {
                    y,
                    p: model.parameters(),
                    t: *t,
                },
                pure_calls: model.pure_calls(),
            },
            outcome,
            divergences,
        );
    }
}

/// Both concatenation shapes reach the same row opcode: `[x, v]` carries
/// rank-one operands through the MLS 10.4.2.1 promotion, `cat(2, ...)`
/// concatenates rank-two operands with none.
const CONCAT_AXES: &str = r#"
model ConcatAxes
  Real x[2](start = {0.5, -0.25}, each fixed = true);
  Real columns[2, 2];
  Real widened[2, 4];
  parameter Real gain[2, 2] = [1.5, 0.25; -0.75, 2.0];
equation
  columns = [x, {1.0 + time, 2.0 - time}];
  widened = cat(2, gain, columns);
  der(x) = {columns[1, 2] - x[1], widened[2, 4] - x[2]};
end ConcatAxes;
"#;

/// A non-square operand keeps the contraction and the transpose distinguishable:
/// a wrong index order in either one changes the result rather than reproducing
/// it.
const MATRIX_PRODUCTS: &str = r#"
model MatrixProducts
  parameter Real a[3, 2] = [1.0, 0.5; -0.25, 2.0; 0.75, -1.5];
  Real x[2](start = {0.75, -0.5}, each fixed = true);
  Real projected[3];
  Real gram[2, 2];
equation
  projected = a * x;
  gram = transpose(a) * a;
  der(x) = {projected[1] - gram[1, 1] * x[1], projected[3] - gram[2, 2] * x[2]};
end MatrixProducts;
"#;

/// `A(x) * der(x) = b(x, time)` is the implicit linear-state form the runtime
/// evaluates, so its solve lands in an evaluated block rather than in an
/// unreachable one. Each diagonal entry exceeds the absolute sum of its row's
/// off-diagonal entries for every real `x`, so `A(x)` is strictly diagonally
/// dominant and therefore nonsingular at every probe point, not merely at the
/// chosen ones.
const TENSOR_LINEAR_SOLVE: &str = r#"
model TensorLinearSolve
  Real x[3](start = {0.5, -0.25, 0.75}, each fixed = true);
equation
  [4 + x[1]^2, 0.3, 0.1;
   0.2, 5 + x[2]^2, 0.4;
   0.1, 0.25, 6 + x[3]^2] * der(x) = {1 - x[1], 2 - x[2], 3 + time - x[3]};
end TensorLinearSolve;
"#;

/// The function body stays an aggregate typed pure-call owner, so its tensor
/// algebra runs through the pure-call table on both sides rather than through
/// the enclosing row.
const FUNCTION_TENSOR_KERNEL: &str = r#"
function stackColumns
  input Real u[3];
  input Real v[3];
  input Real weight;
  output Real m[3, 2];
algorithm
  m := [weight * u, v];
end stackColumns;

model FunctionTensorKernel
  Real x[3](start = {0.5, -0.75, 0.25}, each fixed = true);
  Real stacked[3, 2];
equation
  stacked = stackColumns(x, {1.0 + time, 2.0 - time, 0.5}, 0.375);
  der(x) = stacked[:, 1] - stacked[:, 2];
end FunctionTensorKernel;
"#;

/// `lane` is algebraic, so the runtime index projects out of a packed register
/// range rather than out of the parameter vector. `slot` starts inside `lane`'s
/// extent, which is what keeps the projected value finite and therefore
/// genuinely compared at every probe.
const TENSOR_INDEXING: &str = r#"
model TensorIndexing
  Real x(start = 0.5, fixed = true);
  Real lane[4];
  Real picked;
  Integer slot(start = 2, fixed = true);
equation
  lane = {1.0 + x, 2.5 - x, -3.0 * x, 4.25 + time};
  slot = 1 + integer(2 + 1.5 * sin(time));
  picked = lane[slot];
  der(x) = picked - x;
end TensorIndexing;
"#;

#[test]
fn concat_axes_agree_between_executors() {
    let report = differential(CONCAT_AXES, "ConcatAxes");
    report.require_agreement();
    report.require_compared_opcode("TensorConcatenate");
}

#[test]
fn matrix_products_agree_between_executors() {
    let report = differential(MATRIX_PRODUCTS, "MatrixProducts");
    report.require_agreement();
    report.require_compared_opcode("MatrixMultiply");
    report.require_compared_opcode("TensorTranspose");
}

#[test]
fn tensor_linear_solve_agrees_between_executors() {
    let report = differential(TENSOR_LINEAR_SOLVE, "TensorLinearSolve");
    report.require_agreement();
    report.require_compared_opcode("LinearSolveComponent");
}

#[test]
fn function_tensor_kernel_agrees_between_executors() {
    let report = differential(FUNCTION_TENSOR_KERNEL, "FunctionTensorKernel");
    report.require_agreement();
    report.require_compared_opcode("PureCall");
}

#[test]
fn tensor_indexing_agrees_between_executors() {
    let report = differential(TENSOR_INDEXING, "TensorIndexing");
    report.require_agreement();
    report.require_compared_opcode("LoadIndexedRegister");
}

#[test]
fn every_case_compares_values() {
    for (source, name) in [
        (CONCAT_AXES, "ConcatAxes"),
        (MATRIX_PRODUCTS, "MatrixProducts"),
        (TENSOR_LINEAR_SOLVE, "TensorLinearSolve"),
        (FUNCTION_TENSOR_KERNEL, "FunctionTensorKernel"),
        (TENSOR_INDEXING, "TensorIndexing"),
    ] {
        let report = differential(source, name);
        assert!(
            report.finite_compared() > 0,
            "`{name}` compared no finite value:\n{}",
            report.census()
        );
    }
}
