//! Unit coverage for the parts of the embedded budget gate that decide things.
//!
//! Measuring a row needs the flight models and an ARM cross toolchain, so what
//! is tested here is everything that turns a measurement into a verdict: the
//! symbol policy, the ceiling comparisons, counting arithmetic out of a
//! disassembly, reading a state type out of a header, parsing an `nm` listing,
//! refusing an inadmissible manifest, and refusing a toolchain root that cannot
//! run the gate. Plus one test that the checked-in budget is the roster it
//! claims to be.
//!
//! Four of these are the gate's negative controls, and all four are here rather
//! than in a manual demonstration precisely because they need no cross
//! compiler: [`a_ceiling_below_the_measured_size_fails_naming_both_numbers`],
//! [`a_floating_point_ceiling_below_the_measured_count_fails_naming_both_numbers`],
//! [`a_planted_double_precision_call_is_caught_by_the_symbol_check`] and
//! [`an_empty_disassembly_is_a_failure_rather_than_a_count_of_zero`].
//!
//! The floating-point counter is covered from a fixture disassembly rather than
//! from a compiled object, so the counted set is pinned by a test that runs on
//! any machine: see [`OBJDUMP_FIXTURE`].

use std::path::{Path, PathBuf};

use super::emit::{artifact_stem, state_type_names, strip_ansi};
use super::fp_ops::{self, COUNTED_ARITHMETIC, Class, EXCLUDED_TRAFFIC};
use super::manifest::{
    Budget, BudgetEntry, BudgetManifest, MANIFEST_SCHEMA_VERSION, Measured, validate,
};
use super::symbols::{
    DOUBLE_LIBM_FUNCTIONS, ObjectSymbols, Reason, classify, parse_undefined, scan,
};
use super::toolchain::{ArmToolchain, REQUIRED_TOOLS, TOOLCHAIN_UNUSABLE_HEADLINE};
use super::verdict::{Metrics, judge};

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("workspace root")
}

fn budget(text_ceiling: u64, state_ceiling: u64, fp_ceiling: u64) -> Budget {
    Budget {
        text_bytes: text_ceiling,
        state_bytes: state_ceiling,
        fp_ops: fp_ceiling,
        measured: Measured {
            text_bytes: 63_478,
            state_bytes: 59_560,
            fp_ops: 1_120,
            comment: "coverage".to_string(),
        },
    }
}

fn entry(budget: Budget) -> BudgetEntry {
    BudgetEntry {
        id: "row".to_string(),
        model: "Vehicles.Rdd2.NavigationEstimator".to_string(),
        entry_point: "Vehicles/package.mo".to_string(),
        target: "fixture-c-target".to_string(),
        why: "coverage".to_string(),
        budget,
    }
}

/// One translation unit weighing `text_bytes` and carrying `fp_ops` arithmetic
/// instructions, beside a state struct of `state_bytes`. The judgement layer
/// only ever sums the per-unit breakdowns, so a single synthetic unit says
/// everything these tests need to say.
fn measured(text_bytes: u64, state_bytes: u64, fp_ops: u64) -> Metrics {
    Metrics {
        text_units: vec![("NavigationEstimator.o".to_string(), text_bytes)],
        state_bytes,
        fp_units: vec![("NavigationEstimator.o".to_string(), fp_ops)],
    }
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
    let entry = entry(budget(60_000, 61_900, 1_200));
    let verdict = judge(
        &entry,
        measured(63_478, 59_560, 1_120),
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

/// The same artifact under ceilings that fit is green, and each ceiling is
/// judged independently of the others.
#[test]
fn an_artifact_inside_every_ceiling_passes() {
    let verdict = judge(
        &entry(budget(66_000, 61_900, 1_200)),
        measured(63_478, 59_560, 1_120),
        &objects(&["sqrtf", "sinf", "cosf", "memcpy"]),
        0.0,
        "arm-none-eabi-gcc ...".to_string(),
    );
    assert!(verdict.passed(), "{:?}", verdict.findings);
}

#[test]
fn a_state_struct_over_its_ceiling_fails_on_its_own() {
    let verdict = judge(
        &entry(budget(66_000, 40_000, 1_200)),
        measured(63_478, 59_560, 1_120),
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
        &entry(budget(66_000, 61_900, 1_200)),
        measured(1, 1, 1),
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
        &entry(budget(66_000, 61_900, 1_200)),
        measured(10, 10, 10),
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
        artifact_stem("navigation-estimator/fixture-c-target"),
        "navigation-estimator_fixture-c-target"
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
        schema_version: MANIFEST_SCHEMA_VERSION,
        ceiling_policy: "fall-only".to_string(),
        fp_ops_policy: "counts arithmetic, fall-only".to_string(),
        entries: vec![entry(budget(66_000, 61_900, 1_200))],
    };
    validate(&base).expect("the base manifest is admissible");

    let mut wrong_version = base.clone();
    wrong_version.schema_version = MANIFEST_SCHEMA_VERSION + 1;
    assert!(validate(&wrong_version).is_err(), "schema version");

    let mut no_policy = base.clone();
    no_policy.ceiling_policy = "  ".to_string();
    assert!(validate(&no_policy).is_err(), "empty ceiling policy");

    let mut no_fp_policy = base.clone();
    no_fp_policy.fp_ops_policy = "  ".to_string();
    assert!(
        validate(&no_fp_policy).is_err(),
        "the rule about arithmetic must travel with the numbers it governs"
    );

    let mut empty = base.clone();
    empty.entries.clear();
    assert!(
        validate(&empty).is_err(),
        "an empty manifest certifies nothing"
    );

    let mut duplicate = base.clone();
    duplicate.entries.push(entry(budget(66_000, 61_900, 1_200)));
    assert!(validate(&duplicate).is_err(), "duplicate id");

    let mut zero_ceiling = base.clone();
    zero_ceiling.entries[0].budget.text_bytes = 0;
    assert!(
        validate(&zero_ceiling).is_err(),
        "a zero ceiling is not a budget"
    );

    let mut zero_fp_ceiling = base.clone();
    zero_fp_ceiling.entries[0].budget.fp_ops = 0;
    assert!(
        validate(&zero_fp_ceiling).is_err(),
        "a zero arithmetic ceiling is a permanently red row, not a budget"
    );

    let mut uncounted = base.clone();
    uncounted.entries[0].budget.measured.fp_ops = 0;
    assert!(
        validate(&uncounted).is_err(),
        "a zero recorded count is what an unreadable disassembly looks like"
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

/// With every former Embedded/Production C product retired, the checked-in
/// roster is empty and therefore deliberately inadmissible. The command must
/// fail closed until the `efmu` `SolveAlgorithmProduct` has fresh authenticated
/// measurements; an empty green budget would certify nothing.
#[test]
fn the_checked_in_budget_refuses_to_certify_zero_active_artifacts() {
    let root = workspace_root();
    let path = super::manifest::manifest_path(&root);
    let raw = std::fs::read_to_string(&path).expect("read checked-in embedded budget");
    let manifest: BudgetManifest =
        serde_json::from_str(&raw).expect("checked-in embedded budget schema");
    assert!(
        manifest.entries.is_empty(),
        "no retired row may remain active"
    );
    let error = validate(&manifest).expect_err("an empty budget must fail closed");
    assert!(
        error.to_string().contains("no entries"),
        "unexpected empty-budget refusal: {error:#}"
    );
}

/// A disassembly in the exact shape `arm-none-eabi-objdump -d` writes it,
/// carrying every mnemonic [`COUNTED_ARITHMETIC`] names, one predicated
/// spelling of a counted mnemonic, and a spread of the single-precision traffic
/// and integer work that must not be counted.
///
/// Fifteen lines here are arithmetic: the fourteen counted mnemonics plus the
/// predicated `vmulgt.f32`. Everything else is a move, a load, a store, a
/// comparison, a conversion, a sign flip, a selection, a rounding, a
/// double-precision operation, an integer instruction, or not an instruction at
/// all.
const OBJDUMP_FIXTURE: &str = concat!(
    "\n",
    "step.o:     file format elf32-littlearm\n",
    "\n",
    "\n",
    "Disassembly of section .text.step:\n",
    "\n",
    "00000000 <step>:\n",
    "   0:\tee30 0a20 \tvadd.f32\ts0, s0, s1\n",
    "   4:\tee30 0a60 \tvsub.f32\ts0, s0, s1\n",
    "   8:\tee20 0a20 \tvmul.f32\ts0, s0, s1\n",
    "   c:\tee20 0a60 \tvnmul.f32\ts0, s0, s1\n",
    "  10:\tee80 0a20 \tvdiv.f32\ts0, s0, s1\n",
    "  14:\teeb1 0ac0 \tvsqrt.f32\ts0, s0\n",
    "  18:\tee00 1a20 \tvmla.f32\ts2, s0, s1\n",
    "  1c:\tee00 1a60 \tvmls.f32\ts2, s0, s1\n",
    "  20:\tee10 1ac0 \tvnmla.f32\ts2, s1, s0\n",
    "  24:\tee10 1a80 \tvnmls.f32\ts2, s1, s0\n",
    "  28:\teee0 7a20 \tvfma.f32\ts15, s0, s1\n",
    "  2c:\teee0 7a60 \tvfms.f32\ts15, s0, s1\n",
    "  30:\teed0 7a20 \tvfnma.f32\ts15, s0, s1\n",
    "  34:\teed0 7a60 \tvfnms.f32\ts15, s0, s1\n",
    "  38:\tbfc8      \tit\tgt\n",
    "  3a:\tee20 0a20 \tvmulgt.f32\ts0, s0, s1\n",
    "  3e:\teeb0 0a41 \tvmov.f32\ts0, s2\n",
    "  42:\teddf 7a07 \tvldr\ts15, [pc, #28]\t@ 60 <step+0x60>\n",
    "  46:\ted8d 0a00 \tvstr\ts0, [sp]\n",
    "  4a:\teeb4 0a60 \tvcmp.f32\ts0, s1\n",
    "  4e:\teeb4 0ae0 \tvcmpe.f32\ts0, s1\n",
    "  52:\teeb8 0ae7 \tvcvt.f32.s32\ts0, s15\n",
    "  56:\teebd 7ac0 \tvcvt.s32.f32\ts15, s0\n",
    "  5a:\teeb0 0ac0 \tvabs.f32\ts0, s0\n",
    "  5e:\teeb1 0a40 \tvneg.f32\ts0, s0\n",
    "  62:\tfe30 0a20 \tvselgt.f32\ts0, s0, s1\n",
    "  66:\tfe80 0a20 \tvmaxnm.f32\ts0, s0, s1\n",
    "  6a:\tfe80 0a60 \tvminnm.f32\ts0, s0, s1\n",
    "  6e:\teebd 0ac0 \tvrintz.f32\ts0, s0\n",
    "  72:\tee20 7b01 \tvmul.f64\td7, d0, d1\n",
    "  76:\t4413      \tadd\tr3, r2\n",
    "  78:\t6819      \tldr\tr1, [r3, #0]\n",
    "  7a:\t4770      \tbx\tlr\n",
    "  7c:\t00000000 \t.word\t0x00000000\n",
);

/// The counter counts exactly [`COUNTED_ARITHMETIC`] and nothing else, from a
/// listing in the shape the disassembler writes.
#[test]
fn the_counter_counts_the_documented_set_and_nothing_else() {
    let tally = fp_ops::tally(OBJDUMP_FIXTURE, "the fixture").expect("the fixture is countable");
    assert_eq!(
        tally.arithmetic, 15,
        "fourteen counted mnemonics plus the predicated spelling"
    );
    // Every instruction line, arithmetic or not: the banner, the symbol header
    // and the blank lines are not instructions.
    assert_eq!(tally.instructions, 34);
}

/// Each mnemonic in the counted set classifies as arithmetic, each mnemonic in
/// the traffic set does not, and both hold for the `.f32` spelling the
/// disassembler writes.
#[test]
fn the_two_documented_sets_classify_as_they_say() {
    for stem in COUNTED_ARITHMETIC {
        assert_eq!(
            fp_ops::classify(&format!("{stem}.f32")),
            Class::Arithmetic,
            "`{stem}.f32` is in COUNTED_ARITHMETIC"
        );
    }
    for stem in EXCLUDED_TRAFFIC {
        assert_eq!(
            fp_ops::classify(&format!("{stem}.f32")),
            Class::Traffic,
            "`{stem}.f32` is in EXCLUDED_TRAFFIC"
        );
    }
    // A conversion names two types; the `f32` component is enough to make it a
    // single-precision instruction, and it is still traffic.
    assert_eq!(fp_ops::classify("vcvt.f32.s32"), Class::Traffic);
    // Double-precision arithmetic is not this counter's business: the symbol
    // policy owns `double`, and counting `.f64` here would mix two budgets.
    assert_eq!(fp_ops::classify("vmul.f64"), Class::Untyped);
    // Loads and stores disassemble without a type suffix.
    assert_eq!(fp_ops::classify("vldr"), Class::Untyped);
    assert_eq!(fp_ops::classify("bx"), Class::Untyped);
    // A predicated arithmetic mnemonic is the same work; a predicated selection
    // is still a selection.
    assert_eq!(fp_ops::classify("vaddne.f32"), Class::Arithmetic);
    assert_eq!(fp_ops::classify("vselvs.f32"), Class::Traffic);
    // `vmls` ends in the letters of a condition code and is judged as itself.
    assert_eq!(fp_ops::classify("vmls.f32"), Class::Arithmetic);
}

/// A single-precision mnemonic in neither documented set stops the row instead
/// of being silently uncounted, and names itself so that classifying it is a
/// one-line change in the one place the sets live.
#[test]
fn an_unclassified_single_precision_mnemonic_stops_the_row() {
    assert_eq!(fp_ops::classify("vjoin.f32"), Class::Unclassified);
    let listing = concat!(
        "00000000 <step>:\n",
        "   0:\tee30 0a20 \tvadd.f32\ts0, s0, s1\n",
        "   4:\tffff ffff \tvjoin.f32\ts0, s0, s1\n",
    );
    let error = fp_ops::tally(listing, "step.o").expect_err("an unlisted mnemonic is a refusal");
    let text = format!("{error:#}");
    assert!(text.contains("`vjoin.f32`"), "{text}");
    assert!(text.contains("step.o"), "{text}");
    assert!(text.contains("COUNTED_ARITHMETIC"), "{text}");
}

/// Negative control: a listing with no instruction in it is a failed
/// measurement, not an artifact that does no arithmetic. A zero here would read
/// as the whole step path having vanished.
#[test]
fn an_empty_disassembly_is_a_failure_rather_than_a_count_of_zero() {
    let listing = concat!(
        "\n",
        "step.o:     file format elf32-littlearm\n",
        "\n",
        "Disassembly of section .text:\n",
        "\n",
    );
    let error = fp_ops::tally(listing, "`arm-none-eabi-objdump -d step.o`")
        .expect_err("an empty listing measures nothing");
    let text = format!("{error:#}");
    assert!(text.contains("no instruction"), "{text}");
    assert!(text.contains("arm-none-eabi-objdump -d step.o"), "{text}");
}

/// Negative control: an `fp_ops` ceiling under the measured count fails, naming
/// the count, the ceiling, and the excess, and it fails on its own when both
/// size ceilings are met.
#[test]
fn a_floating_point_ceiling_below_the_measured_count_fails_naming_both_numbers() {
    let verdict = judge(
        &entry(budget(66_000, 61_900, 1_000)),
        measured(63_478, 59_560, 1_120),
        &objects(&["sqrtf"]),
        0.0,
        "arm-none-eabi-gcc ...".to_string(),
    );
    assert!(!verdict.passed(), "1120 instructions must not fit 1000");
    assert_eq!(verdict.findings.len(), 1, "{:?}", verdict.findings);
    let finding = &verdict.findings[0];
    assert!(finding.starts_with("fp is 1120 instructions"), "{finding}");
    assert!(finding.contains("1000 instruction ceiling"), "{finding}");
    // The excess, so the reader knows how far over the row is without doing the
    // subtraction.
    assert!(finding.contains("by 120"), "{finding}");
    // And the recorded measurement, so the reader can tell growth from a
    // ceiling that was always too tight.
    assert!(finding.contains("records 1120 instructions"), "{finding}");
}

/// The arithmetic count is a sum over translation units, exactly like `.text`:
/// arithmetic moved from one unit into another is arithmetic still executed.
#[test]
fn the_floating_point_count_sums_over_translation_units() {
    let metrics = Metrics {
        text_units: vec![
            ("model.o".to_string(), 60_000),
            ("kernels.o".to_string(), 168),
        ],
        state_bytes: 1,
        fp_units: vec![
            ("model.o".to_string(), 1_100),
            ("kernels.o".to_string(), 20),
        ],
    };
    assert_eq!(metrics.text_bytes(), 60_168);
    assert_eq!(metrics.fp_ops(), 1_120);
}
