//! The C targets render a synthesized family owner's maps as loop nests whose
//! trip counts follow MLS §10.4.1 for every Integer step (SPEC_0040 SOLVE-C70).
//!
//! The count is computed in the template from the binder's lower bound, upper
//! bound, and step, so the cases below cover a negative step, a bound the step
//! does not divide, and an empty range. The check reads the rendered C and
//! needs no C toolchain or FMI importer.

use rumoca::Compiler;

const SOURCE: &str = r#"
model MapLoops
  function descending
    input Real x;
    output Real y[3];
  algorithm
    y := {x*k for k in 8:-3:1};
  end descending;
  function ascending
    input Real x;
    output Real y[2];
  algorithm
    y := {x*k for k in 0:4:7};
  end ascending;
  function grid
    input Real x;
    output Real y[2,3];
  algorithm
    y := {{x*(10*r + c) for c in 9:-4:1} for r in 3:-2:1};
  end grid;
  Real x(start = 1, fixed = true);
  Real d[3] = descending(x);
  Real a[2] = ascending(x);
  Real g[2,3] = grid(x);
equation
  der(x) = -x;
end MapLoops;
"#;

/// The `(trip count, step)` of every `map` loop in the rendered functions, as
/// the template emits them.
fn map_loops(target: &str) -> Vec<(usize, i64)> {
    let compiled = Compiler::new()
        .model("MapLoops")
        .compile_str(SOURCE, "fmi_family_map_loops.mo")
        .unwrap_or_else(|error| panic!("MapLoops compiles: {error:?}"));
    let files = rumoca::render_target_files(&compiled, "MapLoops", target, None)
        .unwrap_or_else(|error| panic!("render {target}: {error:#}"));
    let functions = files
        .iter()
        .find(|file| file.path.ends_with("rmc_functions.c"))
        .unwrap_or_else(|| panic!("{target} renders its function sources"));
    let lines = functions.content.lines().collect::<Vec<_>>();
    let mut loops = Vec::new();
    for (position, line) in lines.iter().enumerate() {
        let Some(count) = line
            .trim()
            .strip_prefix("for (size_t i0=0; i0<")
            .and_then(|rest| rest.split(';').next())
        else {
            continue;
        };
        let Some(step) = lines.get(position + 1).and_then(|next| {
            next.trim()
                .strip_prefix("if (i0) b0[0]+=")
                .map(|rest| rest.trim_end_matches(';'))
        }) else {
            continue;
        };
        let step = step
            .trim_start_matches('(')
            .trim_end_matches(')')
            .replace("INT64_C(", "")
            .replace(')', "");
        loops.push((
            count.parse().expect("a literal trip count"),
            step.parse().expect("a literal step"),
        ));
    }
    loops.sort_unstable();
    loops
}

/// `8:-3:1` visits 8, 5, 2; `0:4:7` visits 0, 4; the nested grid visits
/// rows 3, 1 and columns 9, 5, 1.
#[test]
fn map_loops_count_negative_and_non_divisible_steps_exactly() {
    for target in ["fmi2", "fmi3"] {
        assert_eq!(
            map_loops(target),
            [(2, -2), (2, 4), (3, -4), (3, -3)],
            "{target}"
        );
    }
}
