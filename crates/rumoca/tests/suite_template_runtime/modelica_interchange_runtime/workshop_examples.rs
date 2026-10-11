//! Actual workshop models, independently executed before and after interchange.

use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::Path,
    process::Command,
};

use rumoca::{Compiler, render_target_files};

struct Example {
    name: &'static str,
    observables: &'static [&'static str],
    initial: &'static [(&'static str, f64)],
}

const EXAMPLES: &[Example] = &[
    Example {
        name: "Circuit.Test",
        observables: &["cap.v", "res.v", "src.v"],
        initial: &[("cap.v", 0.0), ("src.v", 5.0)],
    },
    Example {
        name: "SympyDecay",
        observables: &["x"],
        initial: &[("x", 1.0)],
    },
    Example {
        name: "FmiTensorDecay",
        observables: &["x[1]", "x[2]"],
        initial: &[("x[1]", 1.0), ("x[2]", 1.0)],
    },
    Example {
        name: "NeuralODETensor",
        observables: &["x[1]", "x[2]", "dx[1]", "dx[2]", "radius"],
        initial: &[("x[1]", 1.0), ("x[2]", 0.0), ("radius", 1.0)],
    },
    Example {
        name: "NeuralPredatorPrey",
        observables: &[
            "population[1]",
            "population[2]",
            "growth[1]",
            "growth[2]",
            "interactionIndex",
        ],
        initial: &[("population[1]", 1.35), ("population[2]", 0.72)],
    },
    Example {
        name: "NeuralODEBackprop",
        observables: &[
            "W1[1,1]",
            "Wmid[1,1]",
            "b2[1]",
            "loss",
            "rmsError",
            "targetFirst[1]",
        ],
        initial: &[("b2[1]", 0.0), ("targetFirst[1]", -0.18)],
    },
];

struct OmcResult {
    equations: usize,
    variables: usize,
    samples: Vec<BTreeMap<String, f64>>,
}

impl OmcResult {
    fn settled_trace(&self) -> &[BTreeMap<String, f64>] {
        let initial = self
            .samples
            .iter()
            .rposition(|sample| sample["time"] == 0.0)
            .expect("settled sample at exact common start time");
        &self.samples[initial..]
    }
}

fn example_source(example: &Example) -> String {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../examples/models")
        .join(format!(
            "{}.mo",
            example.name.split('.').next().expect("model file stem")
        ));
    fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("required example {}: {error}", path.display()))
}

fn omc_is_available() -> bool {
    let timeout_available = Command::new("timeout")
        .arg("--version")
        .output()
        .is_ok_and(|output| output.status.success());
    let omc_available = timeout_available
        && Command::new("timeout")
            .args(["--kill-after=2s", "5s", "omc", "--version"])
            .output()
            .is_ok_and(|output| output.status.success());
    super::super::template_runtime_policy::prerequisites_are_available(
        "actual workshop Modelica roundtrip",
        &[("GNU timeout", timeout_available), ("OMC", omc_available)],
    )
}

fn check_count(transcript: &str, label: &str) -> usize {
    let words: Vec<_> = transcript.split_whitespace().collect();
    words
        .windows(2)
        .find(|words| words[1].starts_with(label))
        .and_then(|words| words[0].parse().ok())
        .unwrap_or_else(|| panic!("missing OMC {label} count: {transcript}"))
}

fn run_omc(example: &Example, source: &str, target: &str) -> OmcResult {
    let model = if target == "original" {
        example.name.to_owned()
    } else {
        example.name.replace('.', "_")
    };
    let directory = tempfile::tempdir().expect("OMC products directory");
    fs::write(directory.path().join("model.mo"), source).expect("write Modelica source");
    fs::write(
        directory.path().join("check.mos"),
        format!(
            "loadFile(\"model.mo\");\ncheckModel({0});\ngetErrorString();\n\
             simulate({0}, stopTime=0.002, numberOfIntervals=2, tolerance=1e-10, \
             outputFormat=\"csv\", fileNamePrefix=\"trace\");\ngetErrorString();\n",
            model,
        ),
    )
    .expect("write OMC script");
    // GNU timeout bounds compilation and execution together and terminates the
    // process group, including OMC's generated compiler/simulation children.
    let output = Command::new("timeout")
        .args(["--kill-after=5s", "60s", "omc", "check.mos"])
        .current_dir(directory.path())
        .output()
        .expect("execute required OMC");
    let transcript = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        output.status.success()
            && transcript.contains(&format!("Check of {model} completed successfully"))
            && !transcript.contains("Error:"),
        "OMC rejected {target}/{}:\n{transcript}\n{source}",
        example.name,
    );
    let trace =
        fs::read_to_string(directory.path().join("trace_res.csv")).unwrap_or_else(|error| {
            panic!(
                "missing {target}/{} simulation: {error}\n{transcript}",
                example.name
            )
        });
    OmcResult {
        equations: check_count(&transcript, "equation(s)"),
        variables: check_count(&transcript, "variable(s)"),
        samples: omc_csv_samples(&trace),
    }
}

// OMC quotes every CSV header; commas within tensor coordinates stay inside
// those quotes. Data rows contain only numeric values.
fn omc_csv_samples(trace: &str) -> Vec<BTreeMap<String, f64>> {
    let mut lines = trace.lines();
    let names: Vec<_> = lines
        .next()
        .expect("OMC CSV header")
        .trim_matches('"')
        .split("\",\"")
        .collect();
    assert!(
        names.iter().all(|name| !name.is_empty()),
        "empty OMC channel"
    );
    assert_eq!(
        names.iter().copied().collect::<BTreeSet<_>>().len(),
        names.len(),
        "duplicate OMC channels must not overwrite data"
    );
    let samples: Vec<_> = lines
        .filter(|line| !line.trim().is_empty())
        .map(|line| {
            let values: Vec<f64> = line
                .split(',')
                .map(|value| {
                    let value: f64 = value.parse().expect("numeric OMC sample");
                    assert!(value.is_finite(), "nonfinite OMC sample");
                    value
                })
                .collect();
            assert_eq!(names.len(), values.len(), "OMC CSV column count");
            names
                .iter()
                .map(|name| (*name).to_owned())
                .zip(values)
                .collect()
        })
        .collect();
    assert!(
        samples.len() >= 3,
        "OMC must produce initial and short-step samples"
    );
    assert_eq!(samples[0]["time"], 0.0);
    assert!(
        samples
            .windows(2)
            .all(|pair| pair[0]["time"] <= pair[1]["time"]),
        "OMC time must be nondecreasing"
    );
    assert_eq!(samples.last().expect("last sample")["time"], 0.002);
    samples
}

fn assert_close(actual: f64, expected: f64, context: &str) {
    assert!(
        (actual - expected).abs() <= 1e-8 * (1.0 + expected.abs()),
        "{context}: expected {expected}, got {actual}",
    );
}

fn observable(sample: &BTreeMap<String, f64>, name: &str) -> f64 {
    // Flat scalar component paths become one quoted Modelica identifier.
    *sample
        .get(name)
        .or_else(|| sample.get(&format!("'{name}'")))
        .unwrap_or_else(|| panic!("missing observable {name}; columns: {:?}", sample.keys()))
}

fn assert_roundtrip(example: &Example, target: &str, reference: &OmcResult, actual: &OmcResult) {
    assert_eq!(
        actual.equations, reference.equations,
        "{target}/{} equations",
        example.name
    );
    assert_eq!(
        actual.variables, reference.variables,
        "{target}/{} variables",
        example.name
    );
    let reference_trace = reference.settled_trace();
    let actual_trace = actual.settled_trace();
    for &(name, expected) in example.initial {
        assert_close(
            observable(&actual_trace[0], name),
            expected,
            &format!("{target}/{} initial {name}", example.name),
        );
    }
    assert_eq!(
        actual_trace.len(),
        reference_trace.len(),
        "same OMC sample grid"
    );
    for (expected, actual) in reference_trace.iter().zip(actual_trace) {
        assert_eq!(
            actual["time"], expected["time"],
            "same exact OMC sample times"
        );
        for &name in example.observables {
            assert_close(
                observable(actual, name),
                observable(expected, name),
                &format!("{target}/{} {name} at {}", example.name, actual["time"]),
            );
        }
    }
}

#[test]
fn actual_workshop_exports_preserve_omc_structure_initialization_and_short_trace() {
    if !omc_is_available() {
        return;
    }
    for example in EXAMPLES {
        let source = example_source(example);
        let compiled = Compiler::new()
            .model(example.name)
            .compile_str(&source, &format!("{}.mo", example.name))
            .unwrap_or_else(|error| panic!("compile actual {}: {error:#}", example.name));
        let reference = run_omc(example, &source, "original");
        for &(name, expected) in example.initial {
            assert_close(
                observable(&reference.settled_trace()[0], name),
                expected,
                &format!("original/{} initial {name}", example.name),
            );
        }
        for target in ["base-modelica", "flat-modelica", "dae-modelica"] {
            let mut files = render_target_files(&compiled, example.name, target, None)
                .unwrap_or_else(|error| panic!("render {target}/{}: {error:#}", example.name));
            assert_eq!(files.len(), 1, "one complete Modelica document");
            let file = files.remove(0);
            Compiler::new()
                .model(&example.name.replace('.', "_"))
                .compile_str(&file.content, &file.path)
                .unwrap_or_else(|error| {
                    panic!("Rumoca reload {target}/{}: {error:#}", example.name)
                });
            let actual = run_omc(example, &file.content, target);
            assert_roundtrip(example, target, &reference, &actual);
        }
    }
}
