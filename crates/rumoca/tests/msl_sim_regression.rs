use std::env;
use std::path::PathBuf;

use rumoca::Compiler;
use rumoca_session::runtime::{SimOptions, SimResult, simulate_dae};

fn example_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../examples")
        .join(name)
}

fn cached_msl_root() -> Option<PathBuf> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/msl/ModelicaStandardLibrary-4.1.0");
    root.is_dir().then_some(root)
}

fn compiler_with_msl() -> Option<Compiler> {
    if let Some(raw) = env::var_os("MODELICAPATH") {
        return Some(
            env::split_paths(&raw).fold(Compiler::new(), |compiler, path| {
                compiler.library(path.to_string_lossy().as_ref())
            }),
        );
    }
    if let Some(msl_root) = cached_msl_root() {
        return Some(Compiler::new().library(msl_root.to_string_lossy().as_ref()));
    }
    None
}

fn column_value(result: &SimResult, names: &[&str]) -> f64 {
    let idx = result
        .names
        .iter()
        .position(|candidate| names.iter().any(|name| candidate == name))
        .unwrap_or_else(|| panic!("simulation result missing columns {:?}", names));
    *result
        .data
        .last()
        .and_then(|row| row.get(idx))
        .unwrap_or_else(|| panic!("simulation result missing final sample for {:?}", names))
}

fn variable_is_state(result: &SimResult, name: &str) -> bool {
    result
        .variable_meta
        .iter()
        .find(|meta| meta.name == name)
        .is_some_and(|meta| meta.is_state)
}

#[test]
fn switched_rlc_msl_simulates_like_handwritten_example() {
    let Some(msl_compiler) = compiler_with_msl() else {
        eprintln!(
            "skipping switched_rlc_msl_simulates_like_handwritten_example: \
             requires MODELICAPATH or cached MSL at target/msl/ModelicaStandardLibrary-4.1.0"
        );
        return;
    };
    let simple = Compiler::new()
        .model("SwitchedRLC")
        .compile_file(example_path("SwitchedRLC.mo").to_string_lossy().as_ref())
        .expect("handwritten switched RLC example should compile");
    let msl = msl_compiler
        .model("SwitchedRLC_MSL")
        .compile_file(example_path("SwitchedRLCMSL.mo").to_string_lossy().as_ref())
        .expect("MSL switched RLC example should compile");

    let opts = SimOptions {
        t_end: 10.0,
        ..SimOptions::default()
    };

    let simple_result =
        simulate_dae(&simple.dae, &opts).expect("handwritten switched RLC example should simulate");
    let msl_result =
        simulate_dae(&msl.dae, &opts).expect("MSL switched RLC example should simulate");

    // MLS Appendix B / SPEC_0003: variables appearing differentiated remain
    // states. The MSL capacitor voltage and inductor current are both physical
    // storage states and must survive simulator preparation.
    assert_eq!(simple_result.n_states, 2);
    assert_eq!(
        msl_result.n_states, 2,
        "expected SwitchedRLC_MSL to retain both storage states"
    );
    assert!(
        variable_is_state(&msl_result, "capacitor.v"),
        "expected capacitor.v to remain a reported state"
    );
    assert!(
        variable_is_state(&msl_result, "inductor.i"),
        "expected inductor.i to remain a reported state"
    );

    let simple_v = column_value(&simple_result, &["V"]);
    let msl_v = column_value(&msl_result, &["capacitor.v", "capacitor.p.v"]);
    let simple_i = column_value(&simple_result, &["i_L"]);
    let msl_i = column_value(&msl_result, &["inductor.i"]);

    assert!(
        (simple_v - msl_v).abs() <= 1.0e-6,
        "expected capacitor voltage to match handwritten example: simple={simple_v} msl={msl_v}"
    );
    assert!(
        (simple_i - msl_i).abs() <= 1.0e-6,
        "expected inductor current to match handwritten example: simple={simple_i} msl={msl_i}"
    );
}
