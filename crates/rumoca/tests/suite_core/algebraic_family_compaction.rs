//! A regular family of continuous algebraic equations keeps one owner
//! (SPEC_0043 §6c).
//!
//! The Harris-style kernel below defines grayscale, image gradients, and
//! a windowed `sum` with model-level `for` equations. Flatten keeps full
//! bodies only at the corners of each family and issues placeholder rows for
//! every other cell; the DAE lowers each family from its template. The number
//! of full Flat bodies therefore follows the source, not the image, and the
//! scores agree bit for bit with the kernel evaluated in source order.

use rumoca::Compiler;
use rumoca_core::{Expression, Literal, OpBinary};
use rumoca_ir_flat::FamilyInteriors;

fn source(height: usize, width: usize) -> String {
    format!(
        r#"
model Kernel
  parameter Integer height = 4;
  parameter Integer width = 4;
  parameter Real k = 0.07;
  input Real rgb[height,width,3] = {{{{{{
    if r > div(height,2) and c > div(width,2) then 7 else mod(r*37 + c*101 + ch*17 + r*c*7, 256)
    for ch in 1:3}} for c in 1:width}} for r in 1:height}};
  output Real score[height-4,width-4];
protected
  Real gray[height,width];
  Real dx[height-2,width-2];
  Real gxx[height-2,width-2];
equation
  for y in 1:height loop
    for x in 1:width loop
      gray[y,x] = ((rgb[y,x,1]+rgb[y,x,2])+rgb[y,x,3])/3/255.0;
    end for;
  end for;
  for y in 1:height-2 loop
    for x in 1:width-2 loop
      dx[y,x] = (gray[y+1,x+2]-gray[y+1,x])/2.0;
      gxx[y,x] = dx[y,x]*dx[y,x];
    end for;
  end for;
  for y in 1:height-4 loop
    for x in 1:width-4 loop
      score[y,x] = sum(gxx[y+row,x+column] for row in 0:2, column in 0:2)/9 - k;
    end for;
  end for;
end Kernel;
model Probe extends Kernel(height={height}, width={width}); end Probe;
"#
    )
}

fn compile(height: usize, width: usize) -> rumoca::CompilationResult {
    Compiler::new()
        .model("Probe")
        .compile_str(&source(height, width), "algebraic_family_compaction.mo")
        .unwrap_or_else(|error| panic!("{height}x{width} kernel compiles: {error:?}"))
}

/// A placeholder row `target - 0.0` of a family whose template owns its body.
fn placeholder(residual: &Expression) -> bool {
    matches!(
        residual,
        Expression::Binary { op: OpBinary::Sub, rhs, .. }
            if matches!(rhs.as_ref(), Expression::Literal { value: Literal::Real(value), .. } if *value == 0.0)
    )
}

/// Full-body Flat rows and the interiors of every family.
fn flat_bodies(height: usize, width: usize) -> (usize, Vec<FamilyInteriors>) {
    let compiled = compile(height, width);
    let bodies = compiled
        .flat
        .equations
        .iter()
        .filter(|equation| !placeholder(&equation.residual))
        .count();
    let interiors = compiled
        .flat
        .structured_equations
        .iter()
        .map(|family| family.interiors)
        .collect();
    (bodies, interiors)
}

#[test]
fn algebraic_family_bodies_follow_the_source_not_the_image() {
    let (small, small_interiors) = flat_bodies(9, 11);
    let (large, large_interiors) = flat_bodies(30, 41);
    assert!(
        small_interiors
            .iter()
            .all(|interiors| *interiors == FamilyInteriors::ContinuousAlgebraic),
        "every family is a continuous algebraic owner: {small_interiors:?}"
    );
    assert_eq!(small_interiors, large_interiors);
    assert_eq!(small, large, "full Flat bodies are the families' corners");
}

fn pattern(height: usize, width: usize, r: usize, c: usize, ch: usize) -> f64 {
    if r > height / 2 && c > width / 2 {
        7.0
    } else {
        ((r * 37 + c * 101 + ch * 17 + r * c * 7) % 256) as f64
    }
}

/// The kernel in source order; `sum` adds its comprehension row-major.
fn reference(height: usize, width: usize) -> Vec<Vec<f64>> {
    let gray = |y: usize, x: usize| {
        ((pattern(height, width, y, x, 1) + pattern(height, width, y, x, 2))
            + pattern(height, width, y, x, 3))
            / 3.0
            / 255.0
    };
    let gxx = |y: usize, x: usize| {
        let dx = (gray(y + 1, x + 2) - gray(y + 1, x)) / 2.0;
        dx * dx
    };
    (1..=height - 4)
        .map(|y| {
            (1..=width - 4)
                .map(|x| {
                    let window = (0..=2)
                        .flat_map(|row| (0..=2).map(move |column| (row, column)))
                        .map(|(row, column)| gxx(y + row, x + column))
                        .reduce(|sum, value| sum + value)
                        .expect("a nonempty window");
                    window / 9.0 - 0.07
                })
                .collect()
        })
        .collect()
}

#[test]
fn compact_algebraic_family_scores_are_bit_identical_to_source_order() {
    let (height, width) = (10, 12);
    let compiled = compile(height, width);
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 0.0,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("kernel simulates: {error:?}"));
    for (y, row) in reference(height, width).iter().enumerate() {
        for (x, expected) in row.iter().enumerate() {
            let name = format!("score[{},{}]", y + 1, x + 1);
            let column = result
                .names
                .iter()
                .position(|candidate| *candidate == name)
                .unwrap_or_else(|| panic!("{name} in the result"));
            let actual = *result.data[column].last().expect("a recorded score");
            assert_eq!(
                actual.to_bits(),
                expected.to_bits(),
                "{name}: {actual:e} != {expected:e}"
            );
        }
    }
}

/// Residual programs, operations, and calls of the lowered kernel.
fn solve_size(height: usize, width: usize) -> (usize, usize, usize) {
    let compiled = compile(height, width);
    let model =
        rumoca_sim::lower_dae_for_simulation(&compiled.dae, &rumoca_sim::SimOptions::default())
            .unwrap_or_else(|error| panic!("{height}x{width} kernel lowers: {error:?}"));
    let mut size = (0, 0, 0);
    for node in &model.problem.continuous.implicit_rhs.nodes {
        let rumoca_ir_solve::ComputeNode::ScalarPrograms(block) = node else {
            panic!("the kernel residual is scalar-program blocks");
        };
        for program in block.programs() {
            size.0 += 1;
            size.1 += program.len();
            size.2 += program
                .iter()
                .filter(|op| matches!(op, rumoca_ir_solve::LinearOp::PureCall { .. }))
                .count();
        }
    }
    size
}

/// Every family is one call program (SPEC_0040 SOLVE-C70): the residual has
/// one program per family whatever the image size.
#[test]
fn algebraic_family_residual_does_not_grow_with_the_image() {
    let small = solve_size(9, 11);
    let large = solve_size(30, 41);
    assert_eq!(
        small.2, 4,
        "gray, gradient, energy, and window families: {small:?}"
    );
    assert_eq!(
        small, large,
        "residual programs, operations, and calls follow the source"
    );
}

/// The synthesized owners travel in the checked serialized call table, and
/// the replayed model computes the same bits.
#[test]
fn algebraic_family_owners_round_trip_through_the_solve_wire() {
    let (height, width) = (10, 12);
    let compiled = compile(height, width);
    let opts = rumoca_sim::SimOptions {
        t_end: 0.0,
        ..Default::default()
    };
    let lowered = rumoca_sim::lower_correlated_for_simulation_with_overrides(&compiled.dae, &opts)
        .expect("lower correlated kernel");
    let wire = rumoca_phase_solve::fmi::fmi_component_wire(&lowered).expect("component wire");
    let json = serde_json::to_string(&wire).expect("serialize component");
    let mut deserializer = serde_json::Deserializer::from_str(&json);
    let replayed = rumoca_phase_solve::fmi::deserialize_fmi_component(&mut deserializer)
        .expect("replay component");
    assert!(
        replayed.runtime_view().model().pure_calls == lowered.model().pure_calls,
        "the replayed call table is the constructed one"
    );
    let original = rumoca_phase_solve::fmi::finish_fmi_component(lowered).expect("finish");
    let before = rumoca_sim::simulate_fmi_component(original, &opts).expect("simulate original");
    let after = rumoca_sim::simulate_fmi_component(replayed, &opts).expect("simulate replay");
    assert_eq!(before.names, after.names);
    for (lhs, rhs) in before.data.iter().zip(&after.data) {
        assert!(
            lhs.iter().zip(rhs).all(|(a, b)| a.to_bits() == b.to_bits()),
            "replay changed a value"
        );
    }
}
