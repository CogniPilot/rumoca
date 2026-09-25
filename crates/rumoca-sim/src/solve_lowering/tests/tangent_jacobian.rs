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
const LOOPS: &str = "model TangentLoops
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
fn affine_chain() -> String {
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

fn lower_source(source: &str, model: &str, roots: &[PathBuf]) -> solve::SolveModel {
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
    let dae = session
        .compile_model_dae_strict_reachable_uncached_with_recovery(model)
        .unwrap_or_else(|error| panic!("compile {model}: {error}"))
        .dae;
    lower_dae_for_simulation(&dae, &SimOptions::default())
        .unwrap_or_else(|error| panic!("lower {model}: {error:?}"))
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

/// Check every torn block of `model` at `points` random points; returns the
/// number of blocks checked.
fn check_torn_blocks(label: &str, model: &solve::SolveModel, points: usize) -> usize {
    let jvp = to_scalar_program_block(&model.artifacts.continuous.implicit_jacobian_v)
        .expect("scalarize the solver-Y JVP");
    let rows = Rows {
        model,
        primal: PreparedScalarProgramBlock::new(
            to_scalar_program_block(&model.problem.continuous.implicit_rhs)
                .expect("scalarize the implicit rows"),
        )
        .expect("prepare the implicit rows"),
    };
    let mut random = Random(0x9e37_79b9_7f4a_7c15);
    let mut checked = 0;
    for (block_index, block) in model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .iter()
        .enumerate()
    {
        let Some(tearing) = &block.tearing else {
            continue;
        };
        let plan = TornTangentPlan::derive(tearing, &jvp).expect("the tear Jacobian plan");
        let evaluator = TornTangentEvaluator::new(plan);
        let tears = tearing.tear_y_indices.len();
        let mut regular = true;
        for _ in 0..points {
            let mut y = random_point(model, &mut random);
            rows.sweep(tearing, &mut y);
            let Some(exact) = evaluator
                .eval(TangentPoint {
                    y: &y,
                    p: &model.parameters,
                    t: 0.0,
                    context: rows.context(),
                    primal: Some(&rows.primal),
                    fd_step: 1e-7,
                })
                .expect("evaluate the tangent Jacobian")
            else {
                // The plan declines only at a vanished causal pivot, which the
                // torn solve cannot isolate either.
                assert!(
                    has_vanished_pivot(&rows, tearing, &y),
                    "{label} block {block_index}: the tangent plan declined at regular pivots"
                );
                regular = false;
                break;
            };
            let mut residual = vec![0.0; tears * tears];
            let mut recovered = vec![0.0; tearing.causal_steps.len() * tears];
            for (column, &tear) in tearing.tear_y_indices.iter().enumerate() {
                let step = 1e-7 * y[tear].abs().max(1.0);
                let mut plus = y.clone();
                plus[tear] += step;
                let plus_residual = rows.sweep(tearing, &mut plus);
                let mut minus = y.clone();
                minus[tear] -= step;
                let minus_residual = rows.sweep(tearing, &mut minus);
                for row in 0..tears {
                    residual[row * tears + column] =
                        (plus_residual[row] - minus_residual[row]) / (2.0 * step);
                }
                for (index, causal) in tearing.causal_steps.iter().enumerate() {
                    recovered[index * tears + column] =
                        (plus[causal.y_index] - minus[causal.y_index]) / (2.0 * step);
                }
            }
            let block_label = format!("{label} block {block_index}");
            assert_close(
                &format!("{block_label} reduced"),
                &exact.residual,
                &residual,
            );
            assert_close(
                &format!("{block_label} recovered"),
                &exact.recovered,
                &recovered,
            );
        }
        checked += usize::from(regular);
    }
    checked
}

/// Whether some causal row of `tearing` has a vanishing slope in its target.
fn has_vanished_pivot(rows: &Rows<'_>, tearing: &solve::BlockTearing, y: &[f64]) -> bool {
    tearing.causal_steps.iter().any(|step| {
        let mut plus = y.to_vec();
        let delta = 1e-7 * y[step.y_index].abs().max(1.0);
        plus[step.y_index] += delta;
        let slope = (rows.value(step.row, &plus) - rows.value(step.row, y)) / delta;
        // A vanished slope leaves the sweep undefined (not a number) as well.
        !(slope.abs() > 1e-9)
    })
}

/// Check every block's colored Jacobian of `model`; returns the blocks checked.
fn check_colored_blocks(label: &str, model: &solve::SolveModel) -> usize {
    let jvp = to_scalar_program_block(&model.artifacts.continuous.implicit_jacobian_v)
        .expect("scalarize the solver-Y JVP");
    let single = PreparedScalarProgramBlock::new(jvp.clone()).expect("prepare the JVP rows");
    let rows = Rows {
        model,
        primal: PreparedScalarProgramBlock::new(
            to_scalar_program_block(&model.problem.continuous.implicit_rhs)
                .expect("scalarize the implicit rows"),
        )
        .expect("prepare the implicit rows"),
    };
    let structures = model.artifacts.continuous.structural.algebraic_projection();
    let mut checked = 0;
    for (block, structure) in model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .iter()
        .zip(structures)
    {
        let pattern = structure.pattern().nonzero_coordinates();
        let groups = structure.coloring().groups();
        if groups.len() < 2 {
            continue;
        }
        let plan =
            ColoredTangentPlan::derive(&block.rows, &block.y_indices, &pattern, groups, &jvp)
                .expect("the colored Jacobian plan");
        let evaluator = ColoredTangentEvaluator::new(plan);
        let y = &random_point(model, &mut Random(0x2545_f491_4f6c_dd1d));
        let point = TangentPoint {
            y,
            p: &model.parameters,
            t: 0.0,
            context: rows.context(),
            primal: Some(&rows.primal),
            fd_step: 1e-7,
        };
        let values = evaluator
            .eval(point, &block.rows)
            .expect("evaluate the colored tangents")
            .expect("every entry evaluates");
        let mut dual = Vec::with_capacity(values.len());
        let mut difference = Vec::with_capacity(values.len());
        for entry in evaluator.plan().entries() {
            let row = block.rows[entry.row];
            let mut seed = vec![0.0; y.len()];
            for (column, color) in evaluator.plan().colors().iter().enumerate() {
                if *color == entry.lane {
                    seed[block.y_indices[column]] = 1.0;
                }
            }
            let (program, offset) = single.row_output_position(row).expect("a JVP row");
            dual.push(
                single
                    .eval_row_output_unchecked_with_context(
                        program,
                        offset,
                        y,
                        &model.parameters,
                        0.0,
                        RowEvalContext {
                            seed: Some(&seed),
                            ..rows.context()
                        },
                    )
                    .expect("evaluate the one-direction JVP"),
            );
            let column = block.y_indices[entry.column];
            let step = 1e-7 * y[column].abs().max(1.0);
            let mut plus = y.clone();
            plus[column] += step;
            let mut minus = y.clone();
            minus[column] -= step;
            difference.push((rows.value(row, &plus) - rows.value(row, &minus)) / (2.0 * step));
        }
        for (index, (lanes, dual)) in values.iter().zip(&dual).enumerate() {
            assert!(
                (lanes - dual).abs() <= 1e-12 * dual.abs().max(1.0),
                "{label} colored entry {index}: lanes {lanes:e} vs one-direction {dual:e}"
            );
        }
        assert_close(&format!("{label} colored"), &values, &difference);
        checked += 1;
    }
    checked
}

#[test]
fn torn_tangent_jacobians_match_finite_differences_of_the_causal_sweep() {
    let model = lower_source(LOOPS, "TangentLoops", &[]);
    assert!(check_torn_blocks("TangentLoops", &model, 8) == 2);
}

#[test]
fn colored_tangent_jacobians_match_finite_differences_and_the_one_direction_passes() {
    let model = lower_source(&affine_chain(), "TangentChain", &[]);
    assert!(check_colored_blocks("TangentChain", &model) >= 1);
}

fn msl_root() -> Option<PathBuf> {
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
    assert!(check_torn_blocks("Fourbar1", &model, 3) >= 5);
    assert!(check_colored_blocks("Fourbar1", &model) >= 5);
}
