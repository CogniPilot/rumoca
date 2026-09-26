//! Tangent-lane Jacobians of projection blocks against finite differences.
//!
//! The reduced tear Jacobian of each torn block, evaluated from multi-lane
//! tangent programs, must match a central finite difference of the block's
//! reduced residual through an independent causal sweep (each causal row
//! solved for its target by a scalar Newton iteration on the primal row), and
//! the recovered-coordinate sensitivities must match the sweep's causal
//! coordinates, both to 1e-6 relative at random points. Each block's colored
//! Jacobian from one multi-lane evaluation must match a central difference of
//! its rows per column to 1e-6 relative and the one-direction colored JVP
//! passes bit for bit.

use std::path::PathBuf;

use rumoca_compile::compile::{Session, SessionConfig, SourceRootKind};
use rumoca_eval_solve::{
    ColoredTangentEvaluator, PreparedScalarProgramBlock, RowEvalContext, TangentPoint,
    TornTangentEvaluator, to_scalar_program_block,
};
use rumoca_ir_solve::{self as solve, ColoredTangentPlan, TornTangentPlan};

use super::super::entry::lower_dae_for_simulation;
use crate::SimOptions;

const TOLERANCE: f64 = 1e-6;

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
    primal: PreparedScalarProgramBlock,
    /// The solver-Y JVP rows the runtime projection differentiates.
    jvp: solve::ScalarProgramBlock,
}

impl Rows<'_> {
    fn context(&self) -> RowEvalContext<'_> {
        RowEvalContext {
            pure_calls: Some(&self.model.pure_calls),
            external_tables: Some(self.model.external_tables.as_slice()),
            ..RowEvalContext::default()
        }
    }

    fn value(&self, row: usize, y: &[f64]) -> f64 {
        let (program, offset) = self.primal.row_output_position(row).expect("a scalar row");
        self.primal
            .eval_row_output_unchecked_with_context(
                program,
                offset,
                y,
                &self.model.parameters,
                0.0,
                self.context(),
            )
            .expect("evaluate the primal row")
    }

    /// Solve row `row` for `y[target]` by Newton with a difference slope.
    fn isolate(&self, row: usize, target: usize, y: &mut [f64]) {
        for _ in 0..50 {
            let residual = self.value(row, y);
            let scale = y[target].abs().max(1.0);
            let step = 1e-7 * scale;
            y[target] += step;
            let slope = (self.value(row, y) - residual) / step;
            y[target] -= step;
            let correction = residual / slope;
            y[target] -= correction;
            if correction.abs() <= 1e-15 * scale {
                return;
            }
        }
    }

    /// The reduced residual after the causal sweep of `tearing` from `y`.
    fn sweep(&self, tearing: &solve::BlockTearing, y: &mut [f64]) -> Vec<f64> {
        for step in &tearing.causal_steps {
            self.isolate(step.row, step.y_index, y);
        }
        tearing
            .residual_rows
            .iter()
            .map(|&row| self.value(row, y))
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
            "{label} entry {index}: tangent {exact:e} vs difference {reference:e} (scale {scale:e})"
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
    Rows {
        model,
        primal: PreparedScalarProgramBlock::new(
            to_scalar_program_block(&model.problem.continuous.implicit_rhs)
                .expect("scalarize the implicit rows"),
        )
        .expect("prepare the implicit rows"),
        jvp: to_scalar_program_block(&model.artifacts.continuous.implicit_jacobian_v)
            .expect("scalarize the solver-Y JVP"),
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

/// Central differences of the reduced residual and the causal coordinates
/// through the sweep, one column per tear, both row-major.
fn sweep_difference(
    rows: &Rows<'_>,
    tearing: &solve::BlockTearing,
    y: &[f64],
) -> (Vec<f64>, Vec<f64>) {
    let tears = tearing.tear_y_indices.len();
    let mut residual = vec![0.0; tears * tears];
    let mut recovered = vec![0.0; tearing.causal_steps.len() * tears];
    for (column, &tear) in tearing.tear_y_indices.iter().enumerate() {
        let step = 1e-7 * y[tear].abs().max(1.0);
        let mut plus = y.to_vec();
        plus[tear] += step;
        let plus_residual = rows.sweep(tearing, &mut plus);
        let mut minus = y.to_vec();
        minus[tear] -= step;
        let minus_residual = rows.sweep(tearing, &mut minus);
        let quotient = |plus: f64, minus: f64| (plus - minus) / (2.0 * step);
        let targets = tearing.causal_steps.iter().map(|causal| causal.y_index);
        let causal = targets.map(|index| quotient(plus[index], minus[index]));
        let reduced = plus_residual
            .iter()
            .zip(&minus_residual)
            .map(|(plus, minus)| quotient(*plus, *minus));
        for (row, value) in reduced.enumerate() {
            residual[row * tears + column] = value;
        }
        for (row, value) in causal.enumerate() {
            recovered[row * tears + column] = value;
        }
    }
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
    let evaluator = TornTangentEvaluator::new(plan);
    for _ in 0..points {
        let mut y = random_point(rows.model, random);
        rows.sweep(tearing, &mut y);
        let exact = evaluator
            .eval(point(rows, &y))
            .expect("evaluate the tangent Jacobian");
        let Some(exact) = exact else {
            // The plan declines only at a vanished causal pivot, which the
            // torn solve cannot isolate either: some causal row has a
            // vanishing slope in its target (a vanished slope leaves the
            // sweep undefined, not a number, as well).
            let mut vanished = false;
            for step in &tearing.causal_steps {
                let mut plus = y.to_vec();
                let delta = 1e-7 * y[step.y_index].abs().max(1.0);
                plus[step.y_index] += delta;
                let slope = (rows.value(step.row, &plus) - rows.value(step.row, &y)) / delta;
                vanished |= slope.is_nan() || slope.abs() <= 1e-9;
            }
            assert!(
                vanished,
                "{label}: the tangent plan declined at regular pivots"
            );
            return false;
        };
        let (residual, recovered) = sweep_difference(rows, tearing, &y);
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
    let (mut values, mut difference) = (Vec::new(), Vec::new());
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
        let step = 1e-7 * y[column].abs().max(1.0);
        let mut plus = y.clone();
        plus[column] += step;
        let mut minus = y.clone();
        minus[column] -= step;
        values.push(value);
        difference.push((rows.value(row, &plus) - rows.value(row, &minus)) / (2.0 * step));
    }
    assert!(
        !values.is_empty(),
        "{label}: the application places entries"
    );
    assert_close(&format!("{label} colored"), &values, &difference);
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
fn torn_tangent_jacobians_match_finite_differences_of_the_causal_sweep() {
    let model = lower_source(LOOPS, "TangentLoops", &[]);
    assert_eq!(check_torn_blocks("TangentLoops", &model, 8), (2, 2));
}

#[test]
fn colored_tangent_jacobians_match_finite_differences_and_the_one_direction_passes() {
    let model = lower_source(&affine_chain(), "TangentChain", &[]);
    assert!(check_colored_blocks("TangentChain", &model) >= 1);
}

pub(super) fn msl_root() -> Option<PathBuf> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/msl/ModelicaStandardLibrary-4.1.0");
    root.is_dir().then_some(root)
}

#[test]
fn fourbar1_tangent_jacobians_match_finite_differences() {
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
    // the finite difference at every point or declined at a vanished causal
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
