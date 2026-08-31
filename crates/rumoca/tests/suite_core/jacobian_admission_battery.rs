//! The finite-difference battery, generated from the admission table.
//!
//! The Jacobian synthesis specification states what synthesis does with every
//! construct at every shape of operands, and `rumoca_phase_autodiff::admission`
//! is that statement as data. This gate reads it and writes the probes: an
//! admitted pair gets a
//! probe whose synthesized Jacobian is compared against central differences, a
//! refused pair gets a probe whose refusal must fire in the compiler *and* in
//! `--emit-standard-modelica` citing the same rule, and a pair that is not a
//! Modelica program gets a probe proving the engine is never asked for it.
//!
//! The point of generating the battery rather than writing it is that a
//! construct cannot be admitted quietly. It either has a row here, and then it
//! has a finite-difference check, or it has no row, and then
//! `every_construct_the_engine_reaches_is_stated_exactly_once` fails in the
//! crate that owns the rules. There is no third state, which is what the
//! specification's dichotomy asks for.
//!
//! Each probe carries its result out component by component, never reduced, so
//! nothing between the construct and the Jacobian can absorb a rank a wrong
//! tangent collapsed.

use rumoca::Compiler;
use rumoca_phase_autodiff::admission::{Family, Row, Verdict, table};
use rumoca_sim::{SimOptions, SimResult, simulate_dae};

use super::jacobian_finite_difference::Points;
use super::required_tool_markers::omc_differential_is_required;

/// The name of the differentiated function inside the probe at `index`.
///
/// Names are per row because a whole family of probes is loaded into one
/// OpenModelica session at a time, where two functions of the same name are
/// one function declared twice.
fn probe_function(index: usize) -> String {
    format!("pair{index}")
}

/// The finite-difference step every generated probe is written at.
const STEP: f64 = 1.0e-5;

/// The tolerance a gap is held to, relative to the Jacobian's own magnitude.
///
/// A central difference at this step is accurate to about `1e-10` relative
/// plus the step's own truncation, so this is the same tolerance the named
/// battery uses and the one its negative controls are measured against.
const TOLERANCE: f64 = 2.0e-5;

/// The probe model's name for the row at `index`.
///
/// Two constructs can share a spelling, and a spelling is not a class name, so
/// the position in the table is what names the model. The row's own `Display`
/// carries the construct and its shapes into every failure message.
fn probe_name(index: usize) -> String {
    format!("Pair{index}")
}

/// The differentiated point for the row at `index`.
///
/// Coordinates land in `[-1, -0.25] ∪ [0.25, 1]`, which is the range every
/// operand expression in the table is written for: each keeps the value it
/// builds inside the domain of every rule and away from the kinks.
fn point(index: usize) -> Vec<f64> {
    let mut points = Points::new(0x51ec_0000 + index as u64 * 7 + 1);
    (0..3).map(|_| points.next()).collect()
}

/// The complete probe model for an admitted row: its Jacobian, the central
/// differences of the same function, their largest gap, and its magnitude.
fn admitted_source(row: &Row, index: usize) -> String {
    let name = probe_name(index);
    let call = probe_function(index);
    let coordinates: Vec<String> = point(index)
        .iter()
        .map(|value| format!("{value:.12}"))
        .collect();
    let mut equations = String::new();
    for column in 1..=3 {
        equations.push_str(&format!(
            "  Jfd[:, {column}] = ({call}({}) - {call}({}))/(2*h);\n",
            perturbed(column, true),
            perturbed(column, false)
        ));
    }
    format!(
        "{}\n\nmodel {name}\n  parameter Real x[3] = {{{}}};\n  parameter Real h = {STEP:.12};\n  \
         Real J[{width}, 3] = jacobian({call}(x), x);\n  Real Jfd[{width}, 3];\n  \
         Real gap;\n  Real size_of_jacobian;\n  Real clock(start = 0, fixed = true);\nequation\n  \
         der(clock) = 0;\n{equations}  gap = max(abs(J - Jfd));\n  \
         size_of_jacobian = max(abs(J));\nend {name};\n",
        row.function(&call),
        coordinates.join(", "),
        width = row.width(),
    )
}

/// The differentiated argument, perturbed forward or backward in one coordinate.
fn perturbed(column: usize, forward: bool) -> String {
    let sign = if forward { "+" } else { "-" };
    let entries: Vec<&str> = (1..=3)
        .map(|index| if index == column { "h" } else { "0.0" })
        .collect();
    format!("(x {sign} {{{}}})", entries.join(", "))
}

/// The probe model for a refused row: the same call, with nothing to compare,
/// because the expansion never produces a program.
fn refused_source(row: &Row, index: usize) -> String {
    let name = probe_name(index);
    let call = probe_function(index);
    format!(
        "{}\n\nmodel {name}\n  parameter Real x[3] = {{0.37, -0.51, 0.83}};\n  \
         Real J[{}, 3] = jacobian({call}(x), x);\n  \
         Real clock(start = 0, fixed = true);\nequation\n  der(clock) = 0;\nend {name};\n",
        row.function(&call),
        row.width(),
    )
}

/// The probe model for an untypable row: the primal alone, with no `jacobian`
/// call anywhere, so nothing wakes the expander.
fn untypable_source(row: &Row, index: usize) -> String {
    let name = probe_name(index);
    let call = probe_function(index);
    format!(
        "{}\n\nmodel {name}\n  parameter Real x[3] = {{0.37, -0.51, 0.83}};\n  \
         Real y[{}] = {call}(x);\n  Real clock(start = 0, fixed = true);\nequation\n  \
         der(clock) = 0;\nend {name};\n",
        row.function(&call),
        row.width(),
    )
}

fn first_value(result: &SimResult, name: &str) -> f64 {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("probe result is missing column {name}"));
    *result.data[index]
        .first()
        .unwrap_or_else(|| panic!("column {name} has no samples"))
}

/// Compile and run one admitted probe in this compiler, reporting whatever
/// diagnostic stopped it.
///
/// Both stages are folded into one result because a pair that cannot be
/// checked here can fail at either: a primal this canonical DAE does not
/// carry never compiles, and an expansion it declines compiles and then does
/// not run.
fn run_here(row: &Row, index: usize) -> Result<(f64, f64), String> {
    let name = probe_name(index);
    let source = admitted_source(row, index);
    let compiled = Compiler::new()
        .model(&name)
        .compile_str(&source, &format!("{name}.mo"))
        .map_err(|error| format!("{error:#}"))?;
    let result = simulate_dae(
        compiled.dae(),
        &SimOptions {
            t_end: 0.01,
            ..SimOptions::default()
        },
    )
    .map_err(|error| format!("{error:#}"))?;
    Ok((
        first_value(&result, "gap"),
        first_value(&result, "size_of_jacobian"),
    ))
}

/// Compile and run one admitted probe, returning `(gap, size_of_jacobian)`.
fn run(row: &Row, index: usize) -> (f64, f64) {
    run_here(row, index).unwrap_or_else(|error| {
        panic!(
            "{row} must compile and run: {error}\n{}",
            admitted_source(row, index)
        )
    })
}

/// Every row of one family that is checked here, against central differences.
fn check_family(family: Family) {
    let mut checked = 0usize;
    for (index, row) in table().iter().enumerate() {
        if row.family != family || row.verdict != Verdict::Differentiated {
            continue;
        }
        let (gap, size) = run(row, index);
        let tolerance = TOLERANCE * size.max(1.0);
        assert!(
            gap <= tolerance,
            "{row}: the synthesized Jacobian and central differences differ by {gap:e} \
             (tolerance {tolerance:e}, |J|max {size:e})\n{}",
            admitted_source(row, index)
        );
        // A tangent that collapsed to nothing would match nothing, so the
        // magnitude is a claim the comparison cannot make on its own.
        assert!(
            size > 1.0e-6,
            "{row}: the synthesized Jacobian is all zeros, so the comparison proves nothing\n{}",
            admitted_source(row, index)
        );
        checked += 1;
    }
    assert!(
        checked > 0,
        "{family:?} has no admitted rows, so this gate proves nothing"
    );
}

#[test]
fn admitted_elementary_pairs_match_central_differences() {
    check_family(Family::Elementary);
}

#[test]
fn admitted_shaping_pairs_match_central_differences() {
    check_family(Family::Shaping);
}

#[test]
fn admitted_constant_pairs_match_central_differences() {
    check_family(Family::Constant);
}

#[test]
fn admitted_arithmetic_pairs_match_central_differences() {
    check_family(Family::Arithmetic);
}

#[test]
fn admitted_power_pairs_match_central_differences() {
    check_family(Family::Power);
}

#[test]
fn admitted_unary_pairs_match_central_differences() {
    check_family(Family::Unary);
}

#[test]
fn admitted_composite_pairs_match_central_differences() {
    check_family(Family::Composite);
}

#[test]
fn admitted_statement_pairs_match_central_differences() {
    check_family(Family::Statement);
}

#[test]
fn admitted_declaration_pairs_match_central_differences() {
    check_family(Family::Declaration);
}

/// Every family with an admitted row is run by a gate above.
///
/// A family added to the table without a gate would be admitted and unchecked,
/// which is the state the whole construct exists to remove.
#[test]
fn every_admitted_family_has_a_gate() {
    const GATED: &[Family] = &[
        Family::Elementary,
        Family::Shaping,
        Family::Constant,
        Family::Arithmetic,
        Family::Power,
        Family::Unary,
        Family::Declaration,
        Family::Composite,
        Family::Statement,
    ];
    for family in Family::ALL {
        let admitted = table()
            .iter()
            .any(|row| row.family == *family && row.verdict == Verdict::Differentiated);
        assert_eq!(
            admitted,
            GATED.contains(family),
            "{family:?} has admitted rows but no finite-difference gate above, or a gate with \
             nothing to run"
        );
    }
}

/// A refused pair must be refused by the compiler and by the portable writer,
/// citing the same rule.
///
/// Both artifacts reach the expansion through one `expand_source`, so a form
/// only one of them refused would put a derivative nothing checked into a file
/// another tool runs. That is the one-artifact rule of JAC-E3, and this is
/// where it is measured over the whole refusal surface rather than four cases.
#[test]
fn refused_pairs_refuse_identically_in_both_artifacts() {
    let mut checked = 0usize;
    for (index, row) in table().iter().enumerate() {
        let Verdict::Refused { rule, says } = row.verdict else {
            continue;
        };
        let name = probe_name(index);
        let file = format!("{name}.mo");
        let source = refused_source(row, index);

        let compiler = format!(
            "{:?}",
            Compiler::new()
                .model(&name)
                .compile_str(&source, &file)
                .err()
                .unwrap_or_else(|| panic!(
                    "{row} must not compile: it is stated refused\n{source}"
                ))
        );
        let portable = format!(
            "{:#}",
            rumoca_compile::parsing::expand_source_to_standard_modelica(&source, &file)
                .err()
                .unwrap_or_else(|| panic!(
                    "{row} must not expand: a refusal the portable writer skipped would put an \
                     unchecked derivative in a file another tool runs\n{source}"
                ))
        );
        for (path, rendered) in [("compiler", &compiler), ("portable writer", &portable)] {
            assert!(
                rendered.contains(rule),
                "{row} must cite {rule} in the {path}: {rendered}"
            );
            assert!(
                rendered.contains(says),
                "{row} must say what it refused ({says}) in the {path}: {rendered}"
            );
        }
        checked += 1;
    }
    assert!(checked > 0, "the refusal gate must cover the table");
}

/// A pair the table calls untypable must be rejected as ordinary Modelica,
/// with no `jacobian` call in the probe at all, *and* this compiler's expander
/// must not mint an expansion for it either.
///
/// The verdict states two things. Whether a construct is Modelica is not this
/// compiler's opinion to hold, so OpenModelica is the authority the first half
/// is measured against: it must refuse to instantiate the pair's primal. That
/// is what makes "untypable" a statement about the language rather than a
/// place to put an inconvenient construct. The second half is about this
/// compiler, and it is the half a verdict phrased as "the engine is never
/// asked" actually claims: a pair no Modelica program contains must have no
/// expansion here, because an expansion is a rule stated for a construct the
/// table says the engine never meets, checked by nothing and decided by
/// whichever reader meets the generated text first.
#[test]
fn untypable_pairs_are_not_modelica_programs() {
    let rows: Vec<(usize, Row)> = table()
        .into_iter()
        .enumerate()
        .filter(|(_, row)| row.verdict == Verdict::Untypable)
        .collect();
    assert!(!rows.is_empty(), "the untypable gate must cover the table");
    for (index, row) in &rows {
        assert!(
            !untypable_source(row, *index).contains("jacobian("),
            "{row}: the primal probe must not wake the expander"
        );
        let name = probe_name(*index);
        let asked = refused_source(row, *index);
        if let Ok(expanded) = rumoca_compile::parsing::expand_source_to_standard_modelica(
            &asked,
            &format!("{name}.mo"),
        ) {
            panic!(
                "{row} is stated untypable, so the engine is never asked for its rule; this \
                 compiler's expander was asked and answered:\n{expanded}"
            );
        }
    }

    if !omc_available() {
        assert!(
            !omc_differential_is_required(),
            "the OpenModelica typing row is required in this lane, but no working `omc` is on \
             PATH"
        );
        eprintln!("skipping the OpenModelica typing rows: omc not available");
        return;
    }

    let directory = tempfile::tempdir().expect("temporary typing directory");
    let mut script = String::new();
    for (index, row) in &rows {
        let name = probe_name(*index);
        std::fs::write(
            directory.path().join(format!("{name}.mo")),
            untypable_source(row, *index),
        )
        .expect("write untypable probe");
        script.push_str(&format!(
            "loadFile(\"{name}.mo\");\n\
             print(\"OMC_ROW {index}\\n\");\n\
             instantiateModel({name});\n\
             print(getErrorString());\n\
             print(\"\\nOMC_END {index}\\n\");\n"
        ));
    }
    std::fs::write(directory.path().join("typing.mos"), &script).expect("write typing script");
    let output = std::process::Command::new("omc")
        .arg("typing.mos")
        .current_dir(directory.path())
        .output()
        .expect("run omc");
    let transcript = String::from_utf8_lossy(&output.stdout).into_owned();

    let accepted: Vec<String> = rows
        .iter()
        .filter(|(index, _)| !omc_rejected(&transcript, *index))
        .map(|(_, row)| format!("{row}"))
        .collect();
    assert!(
        accepted.is_empty(),
        "these pairs are stated untypable, but OpenModelica instantiates them, so they are \
         Modelica programs this engine owes a rule for:\n{}",
        accepted.join("\n")
    );
}

/// Every generated probe checked here must also elaborate in OpenModelica.
///
/// The in-process comparison proves the derivative is right. This row proves
/// the artifact carrying it is portable, which is the other half of the
/// one-artifact rule: a construct only this compiler accepts would elaborate
/// here and nowhere else, and the exported file would be one no other tool
/// reads.
#[test]
fn the_generated_battery_elaborates_under_omc() {
    let rows: Vec<(usize, Row)> = table()
        .into_iter()
        .enumerate()
        .filter(|(_, row)| row.verdict == Verdict::Differentiated)
        .collect();
    assert!(!rows.is_empty(), "the elaboration row must cover the table");

    if !omc_available() {
        assert!(
            !omc_differential_is_required(),
            "the OpenModelica elaboration row is required in this lane, but no working `omc` is \
             on PATH"
        );
        eprintln!("skipping the OpenModelica elaboration row: omc not available");
        return;
    }

    let directory = tempfile::tempdir().expect("temporary elaboration directory");
    let mut script = String::new();
    for (index, row) in &rows {
        let name = probe_name(*index);
        let source = admitted_source(row, *index);
        let expanded = rumoca_compile::parsing::expand_source_to_standard_modelica(
            &source,
            &format!("{name}.mo"),
        )
        .unwrap_or_else(|error| panic!("{row} must expand: {error:#}\n{source}"));
        assert!(
            !expanded.contains("= jacobian("),
            "{row} kept a surface call in its expansion:\n{expanded}"
        );
        std::fs::write(directory.path().join(format!("{name}.mo")), &expanded)
            .expect("write expanded probe");
        script.push_str(&format!(
            "loadFile(\"{name}.mo\");\n\
             print(\"OMC_ROW {index}\\n\");\n\
             instantiateModel({name});\n\
             print(getErrorString());\n\
             print(\"\\nOMC_END {index}\\n\");\n"
        ));
    }
    std::fs::write(directory.path().join("elaborate.mos"), &script)
        .expect("write elaboration script");
    let output = std::process::Command::new("omc")
        .arg("elaborate.mos")
        .current_dir(directory.path())
        .output()
        .expect("run omc");
    let transcript = String::from_utf8_lossy(&output.stdout).into_owned();

    let rejected: Vec<String> = rows
        .iter()
        .filter(|(index, _)| omc_rejected(&transcript, *index))
        .map(|(_, row)| format!("{row}"))
        .collect();
    assert!(
        rejected.is_empty(),
        "OpenModelica refused to elaborate the expansion of these admitted pairs, so their \
         artifact is not the portable Modelica the expansion claims to be:\n{}",
        rejected.join("\n")
    );
}

/// Whether OpenModelica reported an error while instantiating one probe.
///
/// The transcript brackets each probe between its own markers, so a row's
/// verdict is read from its own block and never from a neighbour's.
fn omc_rejected(transcript: &str, index: usize) -> bool {
    let opened = format!("OMC_ROW {index}\n");
    let closed = format!("OMC_END {index}\n");
    transcript
        .split_once(&opened)
        .and_then(|(_, tail)| tail.split_once(&closed))
        .is_some_and(|(block, _)| block.contains("Error"))
}

/// Every row is claimed by one of the gates above.
#[test]
fn every_row_is_claimed_by_a_gate() {
    for row in table() {
        match row.verdict {
            Verdict::Differentiated | Verdict::DifferentiatedInOpenModelica => assert!(
                Family::ALL.contains(&row.family),
                "{row} names a family no gate runs"
            ),
            Verdict::Refused { .. } | Verdict::Untypable => {}
        }
    }
}

/// Whether a working `omc` is on PATH.
fn omc_available() -> bool {
    std::process::Command::new("omc")
        .arg("--version")
        .output()
        .is_ok_and(|output| output.status.success())
}

/// A number the run script printed under a named marker.
fn marked_value(transcript: &str, marker: &str) -> Option<f64> {
    transcript
        .lines()
        .find_map(|line| line.strip_prefix(marker))
        .and_then(|text| text.trim().parse::<f64>().ok())
}

/// A pair this compiler's own canonical DAE cannot run is still owed a
/// finite-difference row; OpenModelica is where that row is taken.
///
/// Both halves of the row's claim are checked. First this compiler must fail
/// on the row's own probe, the very program the in-process battery would run,
/// and the diagnostic it fails with is printed rather than assumed: that is
/// what stops the verdict from becoming a way to move an inconvenient
/// construct out of reach of the in-process battery. Then the exported
/// expansion must simulate in OpenModelica and agree with the central
/// differences the probe computes there, at the tolerance the in-process gate
/// uses.
///
/// The failure is demanded at either stage, because a pair can be out of reach
/// two ways: its primal may be a program this canonical DAE does not carry at
/// all, or its primal may run while the *expansion* is the program this
/// compiler declines. Both leave no in-process row to take, and both are
/// reported here with the diagnostic that put them there.
#[test]
fn pairs_this_compiler_cannot_run_agree_with_central_differences_under_omc() {
    let rows: Vec<(usize, Row)> = table()
        .into_iter()
        .enumerate()
        .filter(|(_, row)| row.verdict == Verdict::DifferentiatedInOpenModelica)
        .collect();
    assert!(
        !rows.is_empty(),
        "the OpenModelica battery must have rows, or its verdict is unused and should go"
    );

    for (index, row) in &rows {
        let source = admitted_source(row, *index);
        let refused = run_here(row, *index).err().unwrap_or_else(|| {
            panic!(
                "{row} is stated to need OpenModelica, but this compiler runs its probe, so \
                 its finite-difference row belongs in the in-process battery\n{source}"
            )
        });
        eprintln!("{row} is taken in OpenModelica because this compiler said: {refused}");
    }

    if !omc_available() {
        assert!(
            !omc_differential_is_required(),
            "the OpenModelica differential row is required in this lane, but no working `omc` \
             is on PATH"
        );
        eprintln!("skipping the OpenModelica admission rows: omc not available");
        return;
    }

    let directory = tempfile::tempdir().expect("temporary differential directory");
    for (index, row) in &rows {
        let name = probe_name(*index);
        let source = admitted_source(row, *index);
        let expanded = rumoca_compile::parsing::expand_source_to_standard_modelica(
            &source,
            &format!("{name}.mo"),
        )
        .unwrap_or_else(|error| panic!("{row} must expand: {error:#}\n{source}"));
        let (gap, size) = simulate_under_omc(directory.path(), &name, &expanded, row);
        let tolerance = TOLERANCE * size.max(1.0);
        assert!(
            gap <= tolerance,
            "the expanded {row} disagrees with central differences under omc by {gap:e} \
             (tolerance {tolerance:e}, |J|max {size:e})\n{expanded}"
        );
        assert!(
            size > 1.0e-6,
            "the expanded {row} has an all-zero Jacobian under omc, so the comparison proves \
             nothing\n{expanded}"
        );
    }
}

/// Simulate one expanded probe in OpenModelica, returning `(gap, magnitude)`.
fn simulate_under_omc(
    directory: &std::path::Path,
    name: &str,
    expanded: &str,
    row: &Row,
) -> (f64, f64) {
    let model_file = format!("{name}.mo");
    std::fs::write(directory.join(&model_file), expanded).expect("write expanded model");
    let script = format!("run_{name}.mos");
    std::fs::write(
        directory.join(&script),
        format!(
            "loadFile(\"{model_file}\");\n\
             simulate({name}, startTime = 0, stopTime = 0.001, numberOfIntervals = 1);\n\
             print(\"OMC_ERRORS:\" + getErrorString() + \"\\n\");\n\
             print(\"OMC_GAP:\" + String(val(gap, 0.001), significantDigits = 12) + \"\\n\");\n\
             print(\"OMC_SIZE:\" + String(val(size_of_jacobian, 0.001), \
             significantDigits = 12) + \"\\n\");\n"
        ),
    )
    .expect("write differential script");

    let output = std::process::Command::new("omc")
        .arg(&script)
        .current_dir(directory)
        .output()
        .expect("run omc");
    let transcript = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        transcript.contains("The simulation finished successfully"),
        "omc did not simulate the expanded {row}:\n{transcript}\n{expanded}"
    );
    assert_eq!(
        transcript
            .lines()
            .find_map(|line| line.strip_prefix("OMC_ERRORS:")),
        Some(""),
        "omc reported errors simulating the expanded {row}:\n{transcript}\n{expanded}"
    );
    (
        marked_value(&transcript, "OMC_GAP:")
            .unwrap_or_else(|| panic!("{row} must report a gap:\n{transcript}")),
        marked_value(&transcript, "OMC_SIZE:")
            .unwrap_or_else(|| panic!("{row} must report a magnitude:\n{transcript}")),
    )
}
