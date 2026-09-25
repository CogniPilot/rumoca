//! The torn affine elimination promotes vanished causal pivots to tears in
//! place, identically in the linked ME kernel and the generated C kernel
//! (SPEC_0044 ME-PROJ-001; SPEC_0043 affine elimination row).
//!
//! `AffinePromote` is a 21-unknown affine block: a tridiagonal chain `v1..v20`
//! coupled to `w1..w3`, each `w` read by one chain row and by one closing
//! equation. The issued causal order solves `w2` from `v13 + e*w2 = 0.2*x`
//! with the parameter `e = 0`, so every refresh promotes that step; it solves
//! `w3` from `v10 + (x - 0.5)*w3 = 0.1*x`, a pivot that vanishes exactly at
//! `x = 0.5` and is met later in the same elimination. The full block stays
//! regular through the chain rows. The trajectory starts on `x = 0.5`, so the
//! initial refresh promotes both steps and the rest promote one.

use rumoca_eval_solve::projection_policy::torn_promotion_capacity;

use super::projection::{
    DRIVER, FIXED_STATE_DRIVER, assert_projection_trace, compile_packaged_sources, in_process_trace,
};
use super::*;

pub(super) const MODEL: &str = "AffinePromote";
const OUTPUTS: &[&str] = &["x", "v2", "v10", "v13", "v19", "w1", "w2", "w3"];

fn affine_promote() -> String {
    let mut source = format!("model {MODEL}\n  parameter Real e = 0;\n");
    source.push_str("  output Real x(start=0.5, fixed=true);\n");
    for k in 1..=20 {
        source.push_str(&format!("  output Real v{k};\n"));
    }
    for k in 1..=3 {
        source.push_str(&format!("  output Real w{k};\n"));
    }
    source.push_str("equation\n  der(x) = -0.1*x - 0.01*v10 - 0.01*w1;\n  v1 = 1 + x;\n");
    for k in 2..=19 {
        let coupling = match k {
            8 => " + w1",
            11 => " + w3",
            16 => " + w2",
            _ => "",
        };
        source.push_str(&format!(
            "  v{} - (2 + x*x)*v{k} + v{}{coupling} = 0.1*x;\n",
            k - 1,
            k + 1
        ));
    }
    source.push_str(
        "  v5 + w1 = 0.3*x;\n  v13 + e*w2 = 0.2*x;\n  v10 + (x - 0.5)*w3 = 0.1*x;\n  \
         v20 + 0.5*v1 = x;\nend AffinePromote;\n",
    );
    source
}

pub(super) fn compile() -> rumoca::CompilationResult {
    rumoca::Compiler::new()
        .model(MODEL)
        .compile_str(&affine_promote(), &format!("{MODEL}.mo"))
        .unwrap_or_else(|error| panic!("compile {MODEL}: {error:?}"))
}

#[test]
fn packaged_fmi_promotes_vanished_affine_pivots_like_the_linked_kernel() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("affine promotion work directory");
    let driver = work.path().join("projection_trace.py");
    fs::write(&driver, DRIVER).expect("write projection trace driver");
    let compiled = compile();
    let reference = in_process_trace(&compiled, OUTPUTS);
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(work.path(), &compiled, target, MODEL);
        let source =
            fs::read_to_string(fmu.root.join("sources/model.c")).expect("read generated model");
        assert_promoted_block_descriptor(target, &source);
        validate_source_package(&fmu, standard);
        for interface in ["ModelExchange", "CoSimulation"] {
            let csv = work
                .path()
                .join(format!("{MODEL}-{target}-{interface}.csv"));
            checked_output(
                Command::new("python3")
                    .arg(&driver)
                    .arg(&fmu.archive)
                    .arg(interface)
                    .arg(OUTPUTS.join(","))
                    .arg(&csv),
                &format!("{MODEL} {target} {interface} trace"),
            );
            assert_projection_trace(MODEL, &csv, &reference);
        }
    }
}

/// States at which the refresh promotes: `0.5` meets both vanished pivots,
/// the others only the parameter pivot.
const PROMOTION_STATES: [f64; 4] = [0.5, 0.25, 1.0, 1.7];

/// Both kernels solve the same promoted torn arithmetic from the same state,
/// so the refreshed coordinates agree to the last bits the colored forward
/// Jacobian and the linked Jacobian share.
#[test]
fn packaged_fmi_promoted_refresh_matches_the_linked_kernel_at_fixed_states() {
    const VALUE_TOLERANCE: f64 = 1.0e-12;
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let work = tempdir().expect("affine promotion fixed-state work directory");
    let driver = work.path().join("fixed_state_values.py");
    fs::write(&driver, FIXED_STATE_DRIVER).expect("write fixed-state driver");
    let compiled = compile();
    let names = &OUTPUTS[1..];
    let linked = linked_values(&compiled, names);
    for target in ["fmi2", "fmi3"] {
        let fmu = build_named_fmu(work.path(), &compiled, target, MODEL);
        compile_packaged_sources(&fmu);
        let generated = generated_values(&driver, &fmu, names);
        assert_eq!(generated.len(), PROMOTION_STATES.len(), "{target}");
        for ((state, generated), linked) in PROMOTION_STATES.iter().zip(&generated).zip(&linked) {
            for ((name, actual), expected) in names.iter().zip(generated).zip(linked) {
                assert!(
                    (actual - expected).abs() <= VALUE_TOLERANCE * expected.abs().max(1.0),
                    "{target} {name} at x={state}: FMU {actual:.17e} vs linked {expected:.17e}"
                );
            }
        }
    }
}

fn linked_values(compiled: &rumoca::CompilationResult, names: &[&str]) -> Vec<Vec<f64>> {
    PROMOTION_STATES
        .iter()
        .map(|&state| {
            let probe = rumoca_sim::eval_dae_at(
                &compiled.dae,
                &rumoca_sim::SimOptions::default(),
                &[("x".to_string(), state)],
                0.0,
            )
            .unwrap_or_else(|error| panic!("{MODEL} eval at x={state}: {error:?}"));
            names
                .iter()
                .map(|name| {
                    probe
                        .report
                        .solver_y
                        .iter()
                        .find(|slot| slot.name == *name)
                        .unwrap_or_else(|| panic!("linked eval lacks {name}"))
                        .value
                })
                .collect()
        })
        .collect()
}

/// The promoted block's descriptor row, not generic kernel text, is what the
/// FMU must carry: at least one `rmc_blocks[]` initializer row is an
/// elimination block, and every such row's `elim_capacity` is exactly the
/// promotion capacity the shared policy issues for its tear count, so the
/// generated kernel promotes vanished pivots up to the same bound the linked
/// kernel does. Fields are decoded by name against the generated `RmcBlock`
/// struct, so an inserted field (for instance a `causal_offset` after
/// `causal_col`) shifts both the struct and the initializer together and the
/// decode still lands on the right value.
fn assert_promoted_block_descriptor(target: &str, source: &str) {
    let fields = rmc_block_fields(source);
    let index = |name: &str| {
        fields
            .iter()
            .position(|field| field == name)
            .unwrap_or_else(|| panic!("{target}: RmcBlock struct has no `{name}` field"))
    };
    let elimination = index("elimination");
    let nelim_tear = index("nelim_tear");
    let elim_capacity = index("elim_capacity");
    let mut promoted = 0usize;
    for row in rmc_block_rows(source) {
        let values = split_initializer_fields(&row);
        assert_eq!(
            values.len(),
            fields.len(),
            "{target}: block descriptor carries {} values for {} struct fields:\n{row}",
            values.len(),
            fields.len()
        );
        if values[elimination] != "true" {
            continue;
        }
        promoted += 1;
        let base = values[nelim_tear].parse::<usize>().unwrap_or_else(|_| {
            panic!(
                "{target}: nelim_tear `{}` is not a count",
                values[nelim_tear]
            )
        });
        let capacity = values[elim_capacity].parse::<usize>().unwrap_or_else(|_| {
            panic!(
                "{target}: elim_capacity `{}` is not a count",
                values[elim_capacity]
            )
        });
        assert!(
            base > 0,
            "{target}: a promoting elimination block issues no tears"
        );
        let expected = torn_promotion_capacity(base).unwrap_or_else(|| {
            panic!("{target}: the shared policy issues no promotion capacity for {base} tears")
        });
        assert_eq!(
            capacity, expected,
            "{target}: promoted block elim_capacity {capacity} is not \
             torn_promotion_capacity({base}) = {expected}"
        );
    }
    assert!(
        promoted >= 1,
        "{target}: the generated model emits no promoting elimination block"
    );
}

/// Field names of the generated C `RmcBlock` struct, in declaration order.
fn rmc_block_fields(source: &str) -> Vec<String> {
    let close = source
        .find("} RmcBlock;")
        .expect("generated source declares the RmcBlock struct");
    let open = source[..close]
        .rfind("typedef struct {")
        .expect("RmcBlock struct opens with a typedef")
        + "typedef struct {".len();
    source[open..close]
        .split(';')
        .filter_map(|declaration| declaration.split_whitespace().last())
        .map(|name| name.trim_start_matches('*').to_string())
        .collect()
}

/// The brace-delimited `rmc_blocks[]` initializer rows, one string per block.
fn rmc_block_rows(source: &str) -> Vec<String> {
    let marker = "static const RmcBlock rmc_blocks[";
    let start = source
        .find(marker)
        .expect("generated source initializes rmc_blocks");
    let open = start
        + source[start..]
            .find('{')
            .expect("rmc_blocks initializer opens with a brace");
    let mut rows = Vec::new();
    let mut depth = 0i32;
    let mut row_start = 0usize;
    for (offset, character) in source[open..].char_indices() {
        match character {
            '{' => {
                depth += 1;
                if depth == 2 {
                    row_start = open + offset + 1;
                }
            }
            '}' => {
                if depth == 2 {
                    rows.push(source[row_start..open + offset].to_string());
                }
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            _ => {}
        }
    }
    rows
}

/// One initializer row split at its top-level commas, so `RMC_P(offset)` and
/// other parenthesized values stay whole.
fn split_initializer_fields(row: &str) -> Vec<String> {
    let mut fields = Vec::new();
    let mut depth = 0i32;
    let mut last = 0usize;
    for (offset, character) in row.char_indices() {
        match character {
            '(' => depth += 1,
            ')' => depth -= 1,
            ',' if depth == 0 => {
                fields.push(row[last..offset].trim().to_string());
                last = offset + 1;
            }
            _ => {}
        }
    }
    let tail = row[last..].trim();
    if !tail.is_empty() {
        fields.push(tail.to_string());
    }
    fields
}

fn generated_values(driver: &Path, fmu: &BuiltFmu, names: &[&str]) -> Vec<Vec<f64>> {
    let states = PROMOTION_STATES.map(|value| value.to_string()).join(",");
    let output = checked_output(
        Command::new("python3")
            .arg(driver)
            .arg(&fmu.archive)
            .arg(fmu.version)
            .arg(&states)
            .arg(names.join(",")),
        &format!("{} fixed-state values", fmu.version),
    );
    String::from_utf8_lossy(&output.stdout)
        .lines()
        .map(|line| {
            line.split(',')
                .map(|value| value.parse::<f64>().expect("numeric refresh value"))
                .collect()
        })
        .collect()
}
