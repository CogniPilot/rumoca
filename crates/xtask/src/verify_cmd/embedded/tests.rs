//! Unit coverage for the parts of the embedded budget gate that decide things.
//!
//! Measuring a row needs the flight models and an ARM cross toolchain, so what
//! is tested here is everything that turns a measurement into a verdict: the
//! symbol policy, the ceiling comparison, reading a state type out of a header,
//! parsing an `nm` listing, refusing an inadmissible manifest, and refusing a
//! toolchain root that cannot run the gate. Plus one test that the checked-in
//! budget is the roster it claims to be.
//!
//! Two of these are the gate's required negative controls, and both are here
//! rather than in a manual demonstration precisely because they need no cross
//! compiler: [`a_ceiling_below_the_measured_size_fails_naming_both_numbers`]
//! and [`a_planted_double_precision_call_is_caught_by_the_symbol_check`].

use std::path::{Path, PathBuf};

use super::emit::{artifact_stem, state_type_names, strip_ansi};
use super::manifest::{Budget, BudgetEntry, BudgetManifest, Measured, validate};
use super::symbols::{
    DOUBLE_LIBM_FUNCTIONS, ObjectSymbols, Reason, classify, parse_undefined, scan,
};
use super::toolchain::{ArmToolchain, REQUIRED_TOOLS, TOOLCHAIN_UNUSABLE_HEADLINE};
use super::verdict::{Sizes, judge};

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("workspace root")
}

fn budget(text_ceiling: u64, state_ceiling: u64) -> Budget {
    Budget {
        text_bytes: text_ceiling,
        state_bytes: state_ceiling,
        measured: Measured {
            text_bytes: 63_478,
            state_bytes: 59_560,
            comment: "coverage".to_string(),
        },
    }
}

fn entry(budget: Budget) -> BudgetEntry {
    BudgetEntry {
        id: "row".to_string(),
        model: "Vehicles.Rdd2.NavigationEstimator".to_string(),
        entry_point: "Vehicles/package.mo".to_string(),
        target: "embedded-c-galec".to_string(),
        why: "coverage".to_string(),
        budget,
    }
}

/// One translation unit weighing `text_bytes`. The judgement layer only ever
/// sums the breakdown, so a single synthetic unit says everything these tests
/// need to say about size.
fn sized(text_bytes: u64) -> Vec<(String, u64)> {
    vec![("NavigationEstimator.o".to_string(), text_bytes)]
}

fn objects(undefined: &[&str]) -> Vec<ObjectSymbols> {
    vec![ObjectSymbols {
        object: "NavigationEstimator.o".to_string(),
        undefined: undefined.iter().map(|name| (*name).to_string()).collect(),
    }]
}

/// Negative control: a ceiling under what the artifact actually weighs is a
/// failure that names the measured size and the ceiling, so a reader can tell
/// whether the artifact grew or the ceiling was set too tight.
#[test]
fn a_ceiling_below_the_measured_size_fails_naming_both_numbers() {
    let entry = entry(budget(60_000, 61_900));
    let verdict = judge(
        &entry,
        Sizes {
            units: sized(63_478),
            state_bytes: 59_560,
        },
        &objects(&["sqrtf", "memcpy"]),
        0.0,
        "arm-none-eabi-gcc ...".to_string(),
    );
    assert!(
        !verdict.passed(),
        "a 63478 byte artifact must not fit 60000"
    );
    let finding = verdict.findings.join("\n");
    assert!(finding.contains("63478"), "{finding}");
    assert!(finding.contains("60000"), "{finding}");
    // The recorded measurement travels with the failure, so the report says
    // whether this is growth or a ceiling that was always too tight.
    assert!(finding.contains("over the 60000 byte ceiling"), "{finding}");
    assert!(finding.contains("3478"), "{finding}");
}

/// The same artifact under a ceiling that fits is green, and the state ceiling
/// is judged independently of the text ceiling.
#[test]
fn an_artifact_inside_both_ceilings_passes() {
    let verdict = judge(
        &entry(budget(66_000, 61_900)),
        Sizes {
            units: sized(63_478),
            state_bytes: 59_560,
        },
        &objects(&["sqrtf", "sinf", "cosf", "memcpy"]),
        0.0,
        "arm-none-eabi-gcc ...".to_string(),
    );
    assert!(verdict.passed(), "{:?}", verdict.findings);
}

#[test]
fn a_state_struct_over_its_ceiling_fails_on_its_own() {
    let verdict = judge(
        &entry(budget(66_000, 40_000)),
        Sizes {
            units: sized(63_478),
            state_bytes: 59_560,
        },
        &objects(&["sqrtf"]),
        0.0,
        "arm-none-eabi-gcc ...".to_string(),
    );
    assert_eq!(verdict.findings.len(), 1, "{:?}", verdict.findings);
    let finding = &verdict.findings[0];
    assert!(finding.starts_with("state is 59560 bytes"), "{finding}");
    assert!(finding.contains("40000 byte ceiling"), "{finding}");
}

/// Negative control: a `double` that survives lowering leaves the
/// double-precision entry point undefined, and the symbol check reports it even
/// though every ceiling is met.
#[test]
fn a_planted_double_precision_call_is_caught_by_the_symbol_check() {
    let verdict = judge(
        &entry(budget(66_000, 61_900)),
        Sizes {
            units: sized(1),
            state_bytes: 1,
        },
        &objects(&["sqrt", "__aeabi_dmul", "sinf"]),
        0.0,
        "arm-none-eabi-gcc ...".to_string(),
    );
    assert!(!verdict.passed());
    let findings = verdict.findings.join("\n");
    assert!(findings.contains("`__aeabi_dmul`"), "{findings}");
    assert!(findings.contains("`sqrt`"), "{findings}");
    assert!(findings.contains("`sqrtf`"), "{findings}");
    // The single-precision sibling in the same object is not a finding.
    assert!(!findings.contains("`sinf`"), "{findings}");
}

#[test]
fn allocation_is_forbidden_even_when_the_artifact_is_small() {
    let verdict = judge(
        &entry(budget(66_000, 61_900)),
        Sizes {
            units: sized(10),
            state_bytes: 10,
        },
        &objects(&["malloc", "free"]),
        0.0,
        "arm-none-eabi-gcc ...".to_string(),
    );
    let findings = verdict.findings.join("\n");
    assert!(findings.contains("carries no heap"), "{findings}");
    assert_eq!(verdict.findings.len(), 2, "{findings}");
}

#[test]
fn single_precision_siblings_are_all_permitted() {
    for name in DOUBLE_LIBM_FUNCTIONS {
        assert_eq!(
            classify(name),
            Some(Reason::DoubleLibm),
            "`{name}` is the double-precision form and must be forbidden"
        );
        let single = format!("{name}f");
        assert_eq!(
            classify(&single),
            None,
            "`{single}` is the single-precision form and must be permitted"
        );
    }
}

#[test]
fn double_helpers_are_recognized_under_both_naming_schemes() {
    for name in [
        "__aeabi_dadd",
        "__aeabi_dcmplt",
        "__aeabi_d2iz",
        "__aeabi_f2d",
        "__aeabi_i2d",
        "__adddf3",
        "__muldf3",
        "__extendsfdf2",
        "__truncdfsf2",
        "__floatsidf",
    ] {
        assert_eq!(
            classify(name),
            Some(Reason::DoubleHelper),
            "`{name}` is a double-precision helper"
        );
    }
    // The single-precision EABI helpers and the ordinary runtime are fine.
    for name in [
        "__aeabi_fadd",
        "__aeabi_fmul",
        "__aeabi_f2iz",
        "__aeabi_idiv",
        "__aeabi_ldivmod",
        "memcpy",
        "memset",
    ] {
        assert_eq!(classify(name), None, "`{name}` must be permitted");
    }
}

#[test]
fn a_reserved_alias_is_judged_like_the_plain_name() {
    assert_eq!(classify("_malloc"), Some(Reason::Heap));
    assert_eq!(classify("_sqrt"), Some(Reason::DoubleLibm));
    assert_eq!(classify("_sqrtf"), None);
}

/// `nm --undefined-only` prints a blank address column, a type letter, and the
/// name. Strong (`U`) and weak (`w`) undefined references both link to nothing,
/// so both are read; a file-name banner line carries no type letter.
#[test]
fn an_nm_listing_yields_exactly_the_undefined_names() {
    let listing = "\
NavigationEstimator.o:
                 U __aeabi_dmul
                 U memcpy
                 w __gxx_personality_v0
0000000000000000 T NavigationEstimator_dostep
";
    assert_eq!(
        parse_undefined(listing),
        vec![
            "__aeabi_dmul".to_string(),
            "__gxx_personality_v0".to_string(),
            "memcpy".to_string()
        ]
    );
}

#[test]
fn the_same_symbol_in_two_objects_is_one_finding_naming_both() {
    let readings = vec![
        ObjectSymbols {
            object: "model.o".to_string(),
            undefined: vec!["sqrt".to_string()],
        },
        ObjectSymbols {
            object: "kernels.o".to_string(),
            undefined: vec!["sqrt".to_string(), "sqrtf".to_string()],
        },
    ];
    let hits = scan(&readings);
    assert_eq!(hits.len(), 1);
    assert_eq!(hits[0].symbol, "sqrt");
    assert_eq!(hits[0].objects, vec!["model.o", "kernels.o"]);
}

#[test]
fn the_state_type_is_read_out_of_the_emitted_header() {
    let header = "\
typedef struct NavigationEstimatorScratchGroup0Tag {
    float slot[4];
} NavigationEstimatorScratchGroup0;
typedef struct NavigationEstimatorStateTag {
    float x[3];
} NavigationEstimatorState;
";
    assert_eq!(
        state_type_names(header),
        vec!["NavigationEstimatorState".to_string()]
    );
    // A closing brace that is not a typedef terminator yields nothing.
    assert!(state_type_names("}\n} not an identifier;\n").is_empty());
}

/// A refusal is quoted into the failure report and read out of CI logs, so the
/// compiler's colouring is removed before it gets there.
#[test]
fn compiler_colouring_is_stripped_before_it_reaches_the_report() {
    assert_eq!(
        strip_ansi("\u{1b}[31mrumoca::compiler::E012\u{1b}[0m failed"),
        "rumoca::compiler::E012 failed"
    );
    assert_eq!(strip_ansi("no escapes here"), "no escapes here");
}

#[test]
fn a_row_id_becomes_a_safe_artifact_stem() {
    assert_eq!(
        artifact_stem("navigation-estimator/embedded-c-galec"),
        "navigation-estimator_embedded-c-galec"
    );
}

/// Negative control: a toolchain root that is not a gcc-arm-embedded
/// installation refuses, naming the root, the directory searched, and every
/// tool that is missing.
#[test]
fn a_toolchain_root_without_the_tools_refuses() {
    let empty = workspace_root().join("target/verification/embedded-toolchain-probe");
    std::fs::create_dir_all(&empty).expect("probe dir");
    let error = ArmToolchain::resolve(&empty).expect_err("an empty root cannot run the gate");
    let text = format!("{error:#}");
    assert!(text.contains(TOOLCHAIN_UNUSABLE_HEADLINE), "{text}");
    assert!(text.contains(&empty.display().to_string()), "{text}");
    for tool in REQUIRED_TOOLS {
        assert!(text.contains(tool), "{text} does not name {tool}");
    }
}

#[test]
fn an_inadmissible_manifest_is_refused() {
    let base = BudgetManifest {
        schema_version: 1,
        ceiling_policy: "fall-only".to_string(),
        entries: vec![entry(budget(66_000, 61_900))],
    };
    validate(&base).expect("the base manifest is admissible");

    let mut wrong_version = base.clone();
    wrong_version.schema_version = 2;
    assert!(validate(&wrong_version).is_err(), "schema version");

    let mut no_policy = base.clone();
    no_policy.ceiling_policy = "  ".to_string();
    assert!(validate(&no_policy).is_err(), "empty ceiling policy");

    let mut empty = base.clone();
    empty.entries.clear();
    assert!(
        validate(&empty).is_err(),
        "an empty manifest certifies nothing"
    );

    let mut duplicate = base.clone();
    duplicate.entries.push(entry(budget(66_000, 61_900)));
    assert!(validate(&duplicate).is_err(), "duplicate id");

    let mut zero_ceiling = base.clone();
    zero_ceiling.entries[0].budget.text_bytes = 0;
    assert!(
        validate(&zero_ceiling).is_err(),
        "a zero ceiling is not a budget"
    );

    let mut absolute = base.clone();
    absolute.entries[0].entry_point = "/abs/Vehicles/package.mo".to_string();
    assert!(validate(&absolute).is_err(), "absolute entry point");

    let mut unwitnessed = base;
    unwitnessed.entries[0].budget.measured.comment = String::new();
    assert!(
        validate(&unwitnessed).is_err(),
        "a ceiling with no recorded measurement states no headroom"
    );
}

/// The checked-in budget loads, gates the artifacts the flight track is about,
/// and carries a ceiling at or above the measurement it was set from. A ceiling
/// recorded below its own measurement would be permanently red.
#[test]
fn the_checked_in_budget_gates_the_flight_artifacts() {
    let root = workspace_root();
    let manifest = super::manifest::load(&super::manifest::manifest_path(&root))
        .expect("the checked-in embedded budget must load");
    for (model, target) in [
        ("Vehicles.Rdd2.NavigationEstimator", "embedded-c-galec"),
        ("Vehicles.Rdd2.NavigationEstimator", "galec-production"),
        ("Estimation.StrapdownINS.UKF.Estimator", "galec-production"),
    ] {
        assert!(
            manifest
                .entries
                .iter()
                .any(|entry| entry.model == model && entry.target == target),
            "the embedded budget does not gate {model} at {target}"
        );
    }
    for entry in &manifest.entries {
        let Budget {
            text_bytes,
            state_bytes,
            measured,
        } = &entry.budget;
        assert!(
            *text_bytes >= measured.text_bytes,
            "{}: text ceiling {text_bytes} is below its own recorded measurement {}",
            entry.id,
            measured.text_bytes
        );
        assert!(
            *state_bytes >= measured.state_bytes,
            "{}: state ceiling {state_bytes} is below its own recorded measurement {}",
            entry.id,
            measured.state_bytes
        );
    }
}

/// Every single-precision libm call the embedded-c-galec templates can emit
/// must have its double sibling in [`DOUBLE_LIBM_FUNCTIONS`], so that losing
/// the `f` on any template arm is caught by the symbol check. Unlike
/// [`single_precision_siblings_are_all_permitted`], which only checks the
/// names already listed, this test derives the required names from the
/// templates themselves and therefore fails when a builtin is added without a
/// matching forbidden entry (`tanhf` was emitted while `tanh` was absent).
#[test]
fn every_emitted_libm_float_has_its_double_sibling_forbidden() {
    let template_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../rumoca-phase-codegen/src/templates/embedded-c-galec");
    let mut emitted = std::collections::BTreeSet::new();
    for entry in std::fs::read_dir(&template_dir).expect("read template directory") {
        let path = entry.expect("read template entry").path();
        if path.extension().and_then(|extension| extension.to_str()) != Some("jinja") {
            continue;
        }
        let text = std::fs::read_to_string(&path).expect("read template");
        for word in f_suffixed_words(&text) {
            let stem = &word[..word.len() - 1];
            if DOUBLE_LIBM_FUNCTIONS.contains(&stem) {
                emitted.insert(word.clone());
            } else if LIBM_FLOAT_SHAPES.contains(&word.as_str()) {
                panic!(
                    "{} emits `{word}` but `{stem}` is not in \
                     DOUBLE_LIBM_FUNCTIONS; add it so a dropped `f` \
                     suffix cannot pass the symbol check",
                    path.display()
                );
            }
        }
    }
    for required in ["sqrtf", "sinf", "cosf", "atan2f", "tanhf", "powf"] {
        assert!(
            emitted.contains(required),
            "template scan no longer sees `{required}`; the scan or the \
             templates changed shape and this test must follow"
        );
    }
}

/// Lowercase alphanumeric words ending in `f`, at least two characters, not
/// preceded by an underscore: the shape of a single-precision libm call in the
/// templates, and nothing a `rumoca_galec_*` helper name can produce.
fn f_suffixed_words(text: &str) -> Vec<String> {
    let bytes = text.as_bytes();
    let mut words = Vec::new();
    let mut start = None;
    for (index, byte) in bytes.iter().chain(std::iter::once(&b' ')).enumerate() {
        if byte.is_ascii_lowercase() || byte.is_ascii_digit() {
            start.get_or_insert(index);
            continue;
        }
        let Some(word_start) = start.take() else {
            continue;
        };
        let word = &text[word_start..index];
        let boundary = word_start == 0 || bytes[word_start - 1] != b'_';
        if boundary && word.len() > 1 && word.ends_with('f') {
            words.push(word.to_string());
        }
    }
    words
}

/// The libm float spellings the templates are known to use. The scan above
/// only consults this list for words whose double stem is missing from
/// [`DOUBLE_LIBM_FUNCTIONS`], so a genuinely new libm builtin must be added
/// both here and there, while non-libm identifiers ending in `f` stay exempt.
const LIBM_FLOAT_SHAPES: [&str; 22] = [
    "acosf", "asinf", "atan2f", "atanf", "cbrtf", "ceilf", "cosf", "coshf", "expf", "fabsf",
    "floorf", "fmodf", "hypotf", "log10f", "log2f", "logf", "powf", "sinf", "sinhf", "sqrtf",
    "tanf", "tanhf",
];
