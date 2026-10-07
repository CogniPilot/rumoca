//! A call owner whose result is a whole image refreshes as one unit, and
//! each refresh evaluates the call once (SPEC_0043 §6a, SPEC_0040 SOLVE-C51).
//!
//! The model writes every pixel of its output from one user-function call.
//! Solve issues one pure-call owner for it, and the exact refresh of its
//! pixels is one assignment program. Every refresh schedule runs it as one
//! shared-value segment holding one call operation, whatever the image size:
//! the unit count and the evaluations per refresh follow the source, and
//! only the rows (one per output scalar) grow.

use rumoca::Compiler;
use rumoca_ir_solve::LinearOp;
use rumoca_sim::SimOptions;

fn source(height: usize, width: usize) -> String {
    format!(
        r#"
function Scale
  input Real image[:,:];
  output Real scaled[size(image,1)*size(image,2)];
algorithm
  for row in 1:size(image,1) loop
    for column in 1:size(image,2) loop
      scaled[(row-1)*size(image,2)+column] := 2.0*image[row,column] + 1.0;
    end for;
  end for;
end Scale;

model Kernel
  parameter Integer height = 4;
  parameter Integer width = 4;
  input Real image[height,width] = {{{{
    mod(r*37 + c*101, 256) for c in 1:width}} for r in 1:height}};
  output Real scaled[height*width];
equation
  scaled = Scale(image);
end Kernel;
model Probe extends Kernel(height={height}, width={width}); end Probe;
"#
    )
}

/// What the Solve model issues for the call owner of one image size.
#[derive(Debug, PartialEq, Eq)]
struct Units {
    owners: usize,
    rows: usize,
    programs: usize,
    /// Per refresh schedule: its shared-value segments and the call
    /// operations they hold.
    schedules: Vec<(usize, usize)>,
}

fn units(height: usize, width: usize) -> Units {
    let compiled = Compiler::new()
        .model("Probe")
        .compile_str(&source(height, width), "call_owner_refresh_units.mo")
        .unwrap_or_else(|error| panic!("{height}x{width} kernel compiles: {error:?}"));
    let opts = SimOptions {
        t_end: 0.0,
        ..Default::default()
    };
    let lowered = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &opts)
        .unwrap_or_else(|error| panic!("{height}x{width} kernel lowers: {error:?}"));
    let continuous = &lowered.problem.continuous;
    let owners = &continuous.refresh_owners;
    let mut programs = std::collections::BTreeSet::new();
    let mut schedules = Vec::new();
    for schedule in owners.exact_assignment_schedules() {
        programs.extend(schedule.program_ids().iter().map(|id| format!("{id:?}")));
        let shared = schedule
            .shared_segments(&continuous.implicit_rhs, owners)
            .expect("the exact schedule's shared-value segments prove");
        let segments = shared.segments().segments();
        let calls = segments
            .iter()
            .flat_map(|segment| segment.ops())
            .filter(|op| matches!(op, LinearOp::PureCall { .. }))
            .count();
        schedules.push((segments.len(), calls));
    }
    Units {
        owners: lowered.pure_calls.owners().len(),
        rows: owners.algebraic().causal_rows().len(),
        programs: programs.len(),
        schedules,
    }
}

#[test]
fn a_call_owner_refreshes_as_one_unit_whatever_the_image_size() {
    let small = units(5, 7);
    let large = units(30, 41);
    assert_eq!(small.rows, 35, "one refresh row per output scalar");
    assert_eq!(large.rows, 30 * 41);
    assert_eq!(small.owners, 1, "one pure-call owner");
    assert_eq!(small.programs, 1, "one exact assignment program");
    assert!(!small.schedules.is_empty());
    assert!(
        small.schedules.iter().all(|&unit| unit == (1, 1)),
        "every refresh schedule runs one segment that evaluates the call once: {:?}",
        small.schedules
    );
    assert_eq!(
        (small.owners, small.programs, &small.schedules),
        (large.owners, large.programs, &large.schedules),
        "units and evaluations per refresh do not grow with the image"
    );
}
