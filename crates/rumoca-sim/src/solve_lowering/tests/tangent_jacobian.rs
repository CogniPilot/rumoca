//! Tangent-lane Jacobians of projection blocks against an exact dense
//! reference.
//!
//! The reference differentiates every implicit row of a block in one
//! direction per block unknown through the solver-Y JVP rows, giving the dense
//! block Jacobian. For a torn block it then applies the implicit function
//! theorem by a pivoted dense solve over the causal rows and targets: the
//! recovered sensitivities are `-A^-1 B` for the causal-row slopes `A` in the
//! causal targets and `B` in the tears, and the reduced tear Jacobian is
//! `R_t + R_c (-A^-1 B)` over the residual rows. The tangent plan, which
//! instead sweeps the causal steps in order, must agree to 1e-9 relative at
//! random points. Each block's colored Jacobian from one multi-lane
//! evaluation must match the dense reference to 1e-9 relative and the
//! one-direction colored JVP passes bit for bit.

use std::path::PathBuf;

use rumoca_compile::compile::{Session, SessionConfig, SourceRootKind};
use rumoca_eval_solve::{
    ColoredTangentEvaluator, PreparedScalarProgramBlock, RowEvalContext, TangentPoint,
    TornTangentEvaluator, to_scalar_program_block,
};
use rumoca_ir_solve::{self as solve, ColoredTangentPlan, TornTangentPlan};

use super::super::entry::lower_dae_for_simulation;
use crate::SimOptions;

const TOLERANCE: f64 = 1e-9;

/// Two coupled nonlinear loops torn at one or two unknowns each.
pub(super) const LOOPS: &str = "model TangentLoops
  Real x(start=1, fixed=true);
  Real a(start=1);
  Real b(start=1);
  Real c(start=1);
  Real u(start=0.5);
  Real v(start=0.5);
  Real w(start=0.5);
  Real q(start=0.2);
equation
  der(x) = -0.5*a + 0.1*w;
  a = x + 0.2*sin(c);
  b = a*a + 0.1*a;
  c = 1 + b - 0.3*cos(b) + 0.1*sin(c);
  u = 0.4*cos(v) + 0.1*x*w;
  v = u*u - 0.2*q + 0.3;
  w = exp(-0.5*v) + 0.2*u*q;
  q = 0.3*sin(w) + 0.1*v*v;
end TangentLoops;";

/// An affine chain solved as one block.
pub(super) fn affine_chain() -> String {
    let mut source = String::from("model TangentChain\n  Real x(start=1, fixed=true);\n");
    for k in 1..=12 {
        source.push_str(&format!("  Real v{k};\n"));
    }
    source.push_str("equation\n  der(x) = -0.1*x - 0.01*v6;\n  v1 = 1 + x;\n");
    for k in 2..=11 {
        source.push_str(&format!(
            "  v{} - (2 + x*x)*v{k} + v{} = 0.1*x;\n",
            k - 1,
            k + 1
        ));
    }
    source.push_str("  v12 + 0.5*v1 = x;\nend TangentChain;\n");
    source
}

/// The checked DAE of `model` in `source` over the source roots `roots`.
pub(super) fn compile_with_roots(
    source: &str,
    model: &str,
    roots: &[PathBuf],
) -> std::sync::Arc<rumoca_ir_dae::Dae> {
    let mut session = Session::new(SessionConfig::default());
    for root in roots {
        let parsed = rumoca_compile::source_roots::parse_source_root_with_cache(root)
            .expect("parse the source root");
        let key = rumoca_compile::source_roots::source_root_source_set_key(
            root.to_string_lossy().as_ref(),
        );
        session.replace_parsed_source_set(&key, SourceRootKind::External, parsed.documents, None);
    }
    session
        .add_document(&format!("{model}.mo"), source)
        .expect("fixture parses");
    match session.compile_model_dae_strict_reachable_uncached_with_recovery(model) {
        Ok(compiled) => compiled.dae,
        Err(error) => panic!("compile {model}: {error}"),
    }
}

fn lower_source(source: &str, model: &str, roots: &[PathBuf]) -> solve::SolveModel {
    let dae = compile_with_roots(source, model, roots);
    match lower_dae_for_simulation(&dae, &SimOptions::default()) {
        Ok(model) => model,
        Err(error) => panic!("lower {model}: {error:?}"),
    }
}

/// Deterministic uniform numbers in `[-1, 1)`.
struct Random(u64);

impl Random {
    fn next(&mut self) -> f64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        (self.0 % 2_000_000) as f64 / 1_000_000.0 - 1.0
    }
}

struct Rows<'a> {
    model: &'a solve::SolveModel,
    /// The solver-Y JVP rows the runtime projection differentiates.
    jvp: solve::ScalarProgramBlock,
    prepared_jvp: PreparedScalarProgramBlock,
}

impl Rows<'_> {
    fn context(&self) -> RowEvalContext<'_> {
        RowEvalContext {
            pure_calls: Some(&self.model.pure_calls),
            external_tables: Some(self.model.external_tables.as_slice()),
            ..RowEvalContext::default()
        }
    }

    /// The derivative of implicit row `row` in unknown `column` at `y`.
    fn slope(&self, row: usize, column: usize, y: &[f64]) -> f64 {
        let mut seed = vec![0.0; y.len() + self.model.parameters.len()];
        seed[column] = 1.0;
        let (program, offset) = self
            .prepared_jvp
            .row_output_position(row)
            .expect("a scalar JVP row");
        self.prepared_jvp
            .eval_row_output_unchecked_with_context(
                program,
                offset,
                y,
                &self.model.parameters,
                0.0,
                RowEvalContext {
                    seed: Some(&seed),
                    ..self.context()
                },
            )
            .expect("evaluate the JVP row")
    }

    /// The dense Jacobian of `rows` in `columns`, row-major.
    fn dense(&self, rows: &[usize], columns: &[usize], y: &[f64]) -> Vec<f64> {
        rows.iter()
            .flat_map(|&row| columns.iter().map(move |&column| (row, column)))
            .map(|(row, column)| self.slope(row, column, y))
            .collect()
    }
}

fn assert_close(label: &str, exact: &[f64], reference: &[f64]) {
    let scale = reference
        .iter()
        .fold(1e-3, |max, value| value.abs().max(max));
    for (index, (exact, reference)) in exact.iter().zip(reference).enumerate() {
        assert!(
            (exact - reference).abs() <= TOLERANCE * scale,
            "{label} entry {index}: tangent {exact:e} vs dense {reference:e} (scale {scale:e})"
        );
    }
}

/// The model start point with every coordinate moved at random, away from the
/// exact zeros where start values put vector norms and similar kinks.
fn random_point(model: &solve::SolveModel, random: &mut Random) -> Vec<f64> {
    model
        .initial_y
        .iter()
        .map(|value| value + 1e-2 * random.next() * value.abs().max(1.0))
        .collect()
}

fn prepared_rows(model: &solve::SolveModel) -> Rows<'_> {
    let jvp = to_scalar_program_block(&model.artifacts.continuous.implicit_jacobian_v)
        .expect("scalarize the solver-Y JVP");
    Rows {
        model,
        prepared_jvp: PreparedScalarProgramBlock::new(jvp.clone()).expect("prepare the JVP rows"),
        jvp,
    }
}

fn point<'a>(rows: &'a Rows<'a>, y: &'a [f64]) -> TangentPoint<'a> {
    TangentPoint {
        y,
        p: &rows.model.parameters,
        t: 0.0,
        context: rows.context(),
    }
}

/// Solve `a x = b` for the `n` by `n` row-major `a` and the `n` by `m`
/// row-major `b` by Gaussian elimination with partial pivoting.
fn dense_solve(mut a: Vec<f64>, mut b: Vec<f64>, n: usize, m: usize) -> Vec<f64> {
    for pivot in 0..n {
        let best = (pivot..n)
            .max_by(|&i, &j| a[i * n + pivot].abs().total_cmp(&a[j * n + pivot].abs()))
            .expect("a pivot row");
        for column in 0..n {
            a.swap(pivot * n + column, best * n + column);
        }
        for column in 0..m {
            b.swap(pivot * m + column, best * m + column);
        }
        for row in pivot + 1..n {
            let factor = a[row * n + pivot] / a[pivot * n + pivot];
            for column in pivot..n {
                a[row * n + column] -= factor * a[pivot * n + column];
            }
            for column in 0..m {
                b[row * m + column] -= factor * b[pivot * m + column];
            }
        }
    }
    for row in (0..n).rev() {
        for column in 0..m {
            let tail: f64 = (row + 1..n)
                .map(|k| a[row * n + k] * b[k * m + column])
                .sum();
            b[row * m + column] = (b[row * m + column] - tail) / a[row * n + row];
        }
    }
    b
}

/// The reduced tear Jacobian and the recovered sensitivities, both row-major
/// over the tears, by the implicit function theorem on the dense block
/// Jacobian at `y`.
fn dense_reference(
    rows: &Rows<'_>,
    tearing: &solve::BlockTearing,
    y: &[f64],
) -> (Vec<f64>, Vec<f64>) {
    let tears = &tearing.tear_y_indices;
    let causal_rows: Vec<usize> = tearing.causal_steps.iter().map(|step| step.row).collect();
    let targets: Vec<usize> = tearing
        .causal_steps
        .iter()
        .map(|step| step.y_index)
        .collect();
    let (k, n) = (tears.len(), targets.len());
    let a = rows.dense(&causal_rows, &targets, y);
    let b = rows.dense(&causal_rows, tears, y);
    let recovered: Vec<f64> = dense_solve(a, b, n, k)
        .into_iter()
        .map(|value| -value)
        .collect();
    let direct = rows.dense(&tearing.residual_rows, tears, y);
    let through = rows.dense(&tearing.residual_rows, &targets, y);
    let residual = (0..tearing.residual_rows.len())
        .flat_map(|row| (0..k).map(move |column| (row, column)))
        .map(|(row, column)| {
            let chained: f64 = (0..n)
                .map(|step| through[row * n + step] * recovered[step * k + column])
                .sum();
            direct[row * k + column] + chained
        })
        .collect();
    (residual, recovered)
}

/// Check one torn block at `points` random points; `false` when the plan
/// declines at a point with a vanished causal pivot.
fn check_torn_block(
    label: &str,
    rows: &Rows<'_>,
    tearing: &solve::BlockTearing,
    random: &mut Random,
    points: usize,
) -> bool {
    let plan = TornTangentPlan::derive(tearing, &rows.jvp).expect("the tear Jacobian plan");
    let evaluator =
        TornTangentEvaluator::new(plan, &rows.jvp).expect("prepare the tear Jacobian plan");
    for _ in 0..points {
        let y = random_point(rows.model, random);
        let exact = evaluator
            .eval(point(rows, &y))
            .expect("evaluate the tangent Jacobian");
        let Some(exact) = exact else {
            // The plan declines only at a vanished causal pivot: some causal
            // row has a zero or undefined slope in its own target.
            let vanished = tearing.causal_steps.iter().any(|step| {
                let slope = rows.slope(step.row, step.y_index, &y);
                !slope.is_finite() || slope == 0.0
            });
            assert!(
                vanished,
                "{label}: the tangent plan declined at regular pivots"
            );
            return false;
        };
        let (residual, recovered) = dense_reference(rows, tearing, &y);
        assert_close(&format!("{label} reduced"), &exact.residual, &residual);
        assert_close(&format!("{label} recovered"), &exact.recovered, &recovered);
    }
    true
}

/// Check every torn block of `model` at `points` random points; returns the
/// number of torn blocks and of those checked at every point (the others
/// declined at a vanished causal pivot).
fn check_torn_blocks(label: &str, model: &solve::SolveModel, points: usize) -> (usize, usize) {
    let rows = prepared_rows(model);
    let mut random = Random(0x9e37_79b9_7f4a_7c15);
    let blocks = &model.problem.continuous.algebraic_projection_plan.blocks;
    let torn = blocks
        .iter()
        .filter(|block| block.tearing.is_some())
        .count();
    let checked = blocks
        .iter()
        .enumerate()
        .filter_map(|(index, block)| Some((index, block.tearing.as_ref()?)))
        .filter(|(index, tearing)| {
            check_torn_block(
                &format!("{label} block {index}"),
                &rows,
                tearing,
                &mut random,
                points,
            )
        })
        .count();
    (torn, checked)
}

/// The one-direction values of every placement of `application`: each color's
/// calls with that color's seed, as the colored application evaluates them.
fn one_direction_values(
    rows: &Rows<'_>,
    application: &solve::ProjectionJacobianApplication,
    y: &[f64],
) -> Vec<Option<f64>> {
    let source = PreparedScalarProgramBlock::new(application.source().clone())
        .expect("prepare the application programs");
    let mut values = vec![None; application.output_len()];
    let mut outputs = Vec::new();
    for color in application.colors() {
        let mut seed = vec![0.0; y.len() + rows.model.parameters.len()];
        for &index in color.seed_indices() {
            seed[index] = 1.0;
        }
        for call in color.outputs().programs() {
            source
                .eval_row_outputs_unchecked_with_context(
                    call.program(),
                    y,
                    &rows.model.parameters,
                    0.0,
                    RowEvalContext {
                        seed: Some(&seed),
                        ..rows.context()
                    },
                    &mut outputs,
                )
                .expect("evaluate the one-direction call");
            for &(offset, destination) in call.placements() {
                values[destination] = Some(outputs[offset]);
            }
        }
    }
    values
}

/// Check the colored Jacobian of one block at a random point.
fn check_colored_block(
    label: &str,
    rows: &Rows<'_>,
    block: &solve::AlgebraicProjectionBlock,
    application: &solve::ProjectionJacobianApplication,
) {
    let plan = ColoredTangentPlan::derive(application).expect("the colored Jacobian plan");
    let evaluator = ColoredTangentEvaluator::new(plan);
    let y = random_point(rows.model, &mut Random(0x2545_f491_4f6c_dd1d));
    let mut lanes = vec![f64::NAN; application.output_len()];
    evaluator
        .eval(
            (&y, &rows.model.parameters, 0.0),
            rows.context(),
            y.len() + rows.model.parameters.len(),
            &mut lanes,
        )
        .expect("evaluate the colored tangents");
    let n = block.rows.len();
    let (mut values, mut dense) = (Vec::new(), Vec::new());
    for (destination, dual) in one_direction_values(rows, application, &y)
        .into_iter()
        .enumerate()
        .filter_map(|(destination, dual)| Some((destination, dual?)))
    {
        let value = lanes[destination];
        assert!(
            value.to_bits() == dual.to_bits(),
            "{label} colored entry {destination}: lanes {value:e} vs one-direction {dual:e}"
        );
        let (row, column) = (
            block.rows[destination % n],
            block.y_indices[destination / n],
        );
        values.push(value);
        dense.push(rows.slope(row, column, &y));
    }
    assert!(
        !values.is_empty(),
        "{label}: the application places entries"
    );
    assert_close(&format!("{label} colored"), &values, &dense);
}

/// The number of multi-color block applications the lowered plan issues.
fn multi_color_applications(model: &solve::SolveModel) -> usize {
    model
        .artifacts
        .continuous
        .structural
        .algebraic_projection()
        .iter()
        .filter_map(solve::JacobianStructure::jacobian_application)
        .filter(|application| application.colors().len() >= 2)
        .count()
}

/// Check every multi-color block application of `model`; returns the blocks
/// checked.
fn check_colored_blocks(label: &str, model: &solve::SolveModel) -> usize {
    let rows = prepared_rows(model);
    let structures = model.artifacts.continuous.structural.algebraic_projection();
    let blocks = &model.problem.continuous.algebraic_projection_plan.blocks;
    let applications =
        blocks
            .iter()
            .zip(structures)
            .enumerate()
            .filter_map(|(index, (block, structure))| {
                let application = structure.jacobian_application()?;
                (application.colors().len() >= 2).then_some((index, block, application))
            });
    let mut checked = 0;
    for (index, block, application) in applications {
        check_colored_block(&format!("{label} block {index}"), &rows, block, application);
        checked += 1;
    }
    checked
}

#[test]
fn torn_tangent_jacobians_match_the_dense_implicit_function_reference() {
    let model = lower_source(LOOPS, "TangentLoops", &[]);
    assert_eq!(check_torn_blocks("TangentLoops", &model, 8), (2, 2));
}

#[test]
fn colored_tangent_jacobians_match_the_dense_reference_and_the_one_direction_passes() {
    let model = lower_source(&affine_chain(), "TangentChain", &[]);
    assert!(check_colored_blocks("TangentChain", &model) >= 1);
}

pub(super) fn msl_root() -> Option<PathBuf> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/msl/ModelicaStandardLibrary-4.1.0");
    root.is_dir().then_some(root)
}

#[test]
fn fourbar1_tangent_jacobians_match_the_dense_reference() {
    let Some(root) = msl_root() else {
        return;
    };
    let model = lower_source(
        "model TangentFourbar1\n  extends Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1;\nend TangentFourbar1;\n",
        "TangentFourbar1",
        &[root],
    );
    // The block table moves with structural levers, so the expected counts
    // come from the lowered plan. Every torn block is either checked against
    // the dense reference at every point or declined at a vanished causal
    // pivot, which `check_torn_block` asserts; every multi-color application
    // is checked.
    let (torn, checked) = check_torn_blocks("Fourbar1", &model, 3);
    let colored = multi_color_applications(&model);
    eprintln!(
        "Fourbar1: {torn} torn blocks ({checked} checked, {} declined at a vanished pivot), \
         {colored} colored blocks",
        torn - checked
    );
    assert!(
        torn > 0 && checked > 0,
        "Fourbar1 exercises the torn tangent plan"
    );
    assert!(colored > 0, "Fourbar1 exercises the colored tangent plan");
    assert_eq!(check_colored_blocks("Fourbar1", &model), colored);
}
