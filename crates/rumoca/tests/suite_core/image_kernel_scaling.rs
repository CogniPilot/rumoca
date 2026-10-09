//! A readable image kernel keeps the size of the authored source.
//!
//! The kernel computes grayscale from an RGB frame and scores every interior
//! pixel through a function call on its window slice
//! `gray[row - radius:row + radius, column - radius:column + radius]`
//! (MLS §10.4.1, §12.4). Solve keeps the whole frame one pure call with a
//! tensor result, so its residual program does not grow with the frame; the
//! refresh rows that read that call grow linearly, and every construction
//! analysis over them must too. The numerical check reproduces the kernel in
//! source order, so every score, including a signed zero chosen by a strict
//! comparison tie, is bit-identical.

use rumoca::Compiler;
use rumoca_ir_solve::{ComputeNode, LinearOp};

const RADIUS: usize = 2;

fn source(height: usize, width: usize) -> String {
    format!(
        r#"
package ImageKernel
  function PatchPeak "Largest negated difference from the center; ties keep the second operand"
    input Real gray[5,5];
    output Real peak;
  protected
    Real difference;
  algorithm
    peak := 0.0;
    for i in 1:5 loop
      for j in 1:5 loop
        difference := -(gray[i,j]-gray[3,3]);
        peak := if noEvent(peak > difference) then peak else difference;
      end for;
    end for;
  end PatchPeak;

  function FrameScores
    input Real rgb[:,:,3];
    output Real scores[size(rgb,1)*size(rgb,2)];
  protected
    constant Integer radius = {RADIUS};
    Real gray[size(rgb,1),size(rgb,2)];
  algorithm
    scores := zeros(size(rgb,1)*size(rgb,2));
    for row in 1:size(rgb,1) loop
      for column in 1:size(rgb,2) loop
        gray[row,column] := ((rgb[row,column,1]+rgb[row,column,2])+rgb[row,column,3])/3.0;
      end for;
    end for;
    for row in radius+1:size(rgb,1)-radius loop
      for column in radius+1:size(rgb,2)-radius loop
        scores[(row-1)*size(rgb,2)+column] :=
          PatchPeak(gray[row-radius:row+radius,column-radius:column+radius]);
      end for;
    end for;
  end FrameScores;

  model Frame
    parameter Integer height = 4;
    parameter Integer width = 4;
    input Real rgb[height,width,3] = {{{{{{
      if r > div(height,2) and c > div(width,2) then 7 else mod(r*37 + c*101 + ch*17 + r*c*7, 256)
      for ch in 1:3}} for c in 1:width}} for r in 1:height}};
    output Real scores[height*width] = FrameScores(rgb);
  end Frame;

  model Probe extends Frame(height={height}, width={width}); end Probe;
end ImageKernel;
"#
    )
}

fn compile(height: usize, width: usize) -> rumoca::CompilationResult {
    Compiler::new()
        .model("ImageKernel.Probe")
        .compile_str(&source(height, width), "image_kernel.mo")
        .unwrap_or_else(|error| panic!("{height}x{width} kernel compiles: {error:?}"))
}

/// Residual programs and their total operation count.
fn residual_size(height: usize, width: usize) -> (usize, usize, usize) {
    let compiled = compile(height, width);
    let model =
        rumoca_sim::lower_dae_for_simulation(&compiled.dae, &rumoca_sim::SimOptions::default())
            .unwrap_or_else(|error| panic!("{height}x{width} kernel lowers: {error:?}"));
    let (programs, operations) = model
        .problem
        .continuous
        .implicit_rhs
        .nodes
        .iter()
        .map(|node| match node {
            ComputeNode::ScalarPrograms(block) => (
                block.programs().len(),
                block.programs().iter().map(Vec::len).sum::<usize>(),
            ),
            _ => (1, 1),
        })
        .fold((0, 0), |(programs, operations), (p, o)| {
            (programs + p, operations + o)
        });
    let calls = model
        .problem
        .continuous
        .implicit_rhs
        .nodes
        .iter()
        .filter_map(|node| match node {
            ComputeNode::ScalarPrograms(block) => Some(block.programs().iter().flatten()),
            _ => None,
        })
        .flatten()
        .filter(|op| matches!(op, LinearOp::PureCall { .. }))
        .count();
    (programs, operations, calls)
}

#[test]
fn the_residual_of_a_window_kernel_does_not_grow_with_the_frame() {
    let small = residual_size(8, 10);
    let large = residual_size(40, 56);
    assert_eq!(small.2, 1, "the whole frame is one call: {small:?}");
    assert_eq!(
        small, large,
        "residual programs, operations, and calls follow the source, not the pixels"
    );
}

/// A frame large enough that a refresh analysis quadratic in its rows takes
/// minutes still lowers promptly.
#[test]
fn a_wide_window_kernel_lowers_in_time_linear_in_its_rows() {
    let started = std::time::Instant::now();
    let (_, _, calls) = residual_size(96, 128);
    assert_eq!(calls, 1);
    assert!(
        started.elapsed() < std::time::Duration::from_secs(120),
        "lowering 96x128 took {:?}",
        started.elapsed()
    );
}

fn pattern(height: usize, width: usize, r: usize, c: usize, ch: usize) -> f64 {
    if r > height / 2 && c > width / 2 {
        7.0
    } else {
        ((r * 37 + c * 101 + ch * 17 + r * c * 7) % 256) as f64
    }
}

/// The kernel in source order: the same IEEE operations as the Modelica
/// functions, so the scores agree bit for bit.
fn reference_scores(height: usize, width: usize) -> Vec<f64> {
    let gray = |r: usize, c: usize| {
        ((pattern(height, width, r, c, 1) + pattern(height, width, r, c, 2))
            + pattern(height, width, r, c, 3))
            / 3.0
    };
    // PatchPeak: row-major over the window, a strict comparison keeping the
    // second operand on ties.
    let patch_peak = |row: usize, column: usize| {
        let center = gray(row, column);
        (row - RADIUS..=row + RADIUS)
            .flat_map(|i| (column - RADIUS..=column + RADIUS).map(move |j| (i, j)))
            .fold(0.0_f64, |peak, (i, j)| {
                let difference = -(gray(i, j) - center);
                if peak > difference { peak } else { difference }
            })
    };
    let mut scores = vec![0.0; height * width];
    for row in RADIUS + 1..=height - RADIUS {
        for column in RADIUS + 1..=width - RADIUS {
            scores[(row - 1) * width + column - 1] = patch_peak(row, column);
        }
    }
    scores
}

#[test]
fn window_kernel_scores_are_bit_identical_to_source_order() {
    let (height, width) = (12, 14);
    let compiled = compile(height, width);
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 0.0,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("kernel simulates: {error:?}"));
    let expected = reference_scores(height, width);
    let signed_zeros = expected
        .iter()
        .filter(|score| score.to_bits() == (-0.0_f64).to_bits())
        .count();
    assert!(signed_zeros > 0, "the uniform corner ties to a signed zero");
    for (index, expected) in expected.iter().enumerate() {
        let name = format!("scores[{}]", index + 1);
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
