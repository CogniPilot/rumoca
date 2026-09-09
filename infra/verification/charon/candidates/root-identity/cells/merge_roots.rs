//! Selected roots cross the multi-target merge path by identity: the artifact carries them, the
//! merge remaps them like every other reference, and the merged order is seeded from them. A
//! mutually recursive pair of selected functions has no vertex with zero incoming edges, so an
//! order inferred from the dependency graph would lose both; the carried identities keep them.

use assert_cmd::cargo::CommandCargoExt;
use charon_lib::ast::*;
use charon_lib::export::CrateData;
use charon_lib::options::SerializationFormat;
use std::path::Path;
use std::process::Command;

const CYCLE: &str = r#"
pub fn even(value: u32) -> bool {
    if value == 0 { true } else { odd(value - 1) }
}

pub fn odd(value: u32) -> bool {
    if value == 0 { false } else { even(value - 1) }
}
"#;

const CHAIN: &str = r#"
pub fn leaf(value: u32) -> u32 {
    value + 1
}

pub fn top(value: u32) -> u32 {
    leaf(value) * 2
}
"#;

struct Emitted {
    functions: Vec<String>,
    globals: Vec<String>,
    roots: Vec<String>,
    explicit_roots: Vec<String>,
    consumed_roots: Vec<String>,
    dispositions: Vec<String>,
}

fn ident(krate: &TranslatedCrate, id: ItemId) -> String {
    match krate.item_name(id).as_slice_uninstantiated().last() {
        Some(PathElem::Ident(s, _)) => s.clone(),
        other => format!("{other:?}"),
    }
}

fn run(dir: &Path, name: &str, code: &str, extra: &[&str]) -> Emitted {
    let source = dir.join(format!("{name}.rs"));
    std::fs::write(&source, code).unwrap();
    let out = dir.join(format!("{name}.llbc"));
    let mut cmd = Command::cargo_bin("charon").unwrap();
    cmd.arg("rustc")
        .arg("--preset=aeneas")
        .arg("--error-on-warnings")
        .args(extra)
        .arg(format!("--dest-file={}", out.display()))
        .arg("--")
        .arg("--edition=2024")
        .arg("--crate-name")
        .arg("cycle")
        .arg("--crate-type")
        .arg("lib")
        .arg(&source);
    let status = cmd.status().unwrap();
    assert!(status.success(), "charon failed for {name}");
    let krate: TranslatedCrate = CrateData::deserialize_from_file(&out, SerializationFormat::Json)
        .unwrap()
        .translated;
    let mut functions: Vec<String> = krate
        .fun_decls
        .iter()
        .map(|f| ident(&krate, ItemId::Fun(f.def_id)))
        .collect();
    functions.sort();
    let mut globals: Vec<String> = krate
        .global_decls
        .iter()
        .map(|g| ident(&krate, ItemId::Global(g.def_id)))
        .collect();
    globals.sort();
    let mut consumed_roots: Vec<String> = krate
        .consumed_roots
        .iter()
        .map(|c| ident(&krate, c.root()))
        .collect();
    consumed_roots.sort();
    let mut dispositions: Vec<String> = krate
        .consumed_roots
        .iter()
        .map(|c| match c {
            ConsumedRoot::AnonConstToCall { root, init_fun } => format!(
                "AnonConstToCall {} -> {}",
                ident(&krate, ItemId::Global(*root)),
                ident(&krate, ItemId::Fun(*init_fun))
            ),
            ConsumedRoot::Inlined { root, into } => format!(
                "Inlined {} -> {}",
                ident(&krate, ItemId::Fun(*root)),
                ident(&krate, ItemId::Fun(*into))
            ),
            ConsumedRoot::Invisible { root } => format!("Invisible {}", ident(&krate, *root)),
        })
        .collect();
    dispositions.sort();
    let mut roots: Vec<String> = krate.roots.iter().map(|&id| ident(&krate, id)).collect();
    roots.sort();
    let mut explicit_roots: Vec<String> = krate
        .explicit_roots
        .iter()
        .map(|&id| ident(&krate, id))
        .collect();
    explicit_roots.sort();
    Emitted {
        functions,
        globals,
        roots,
        explicit_roots,
        consumed_roots,
        dispositions,
    }
}

const MERGE: &[&str] = &["--targets=x86_64-unknown-linux-gnu"];

#[test]
fn mutually_recursive_roots_survive_the_merge() {
    let dir = tempfile::tempdir().unwrap();
    let dir = dir.path().to_path_buf();
    let single = run(&dir, "single", CYCLE, &[]);
    let merged = run(&dir, "merged", CYCLE, MERGE);
    assert_eq!(single.functions, ["even", "odd"]);
    assert_eq!(
        merged.functions, single.functions,
        "the merge path lost selected roots"
    );
    assert_eq!(
        single.roots,
        ["even", "odd"],
        "both public functions are roots"
    );
    assert_eq!(
        merged.roots, single.roots,
        "the merge changed the carried roots"
    );
    assert!(
        single.explicit_roots.is_empty(),
        "no exact selection was named"
    );
    assert!(merged.explicit_roots.is_empty());
}

#[test]
fn nonrecursive_roots_survive_the_merge() {
    let dir = tempfile::tempdir().unwrap();
    let dir = dir.path().to_path_buf();
    let single = run(&dir, "single", CHAIN, &[]);
    let merged = run(&dir, "merged", CHAIN, MERGE);
    assert_eq!(single.functions, ["leaf", "top"]);
    assert_eq!(merged.functions, single.functions);
    assert_eq!(single.roots, ["leaf", "top"]);
    assert_eq!(merged.roots, single.roots);
}

#[test]
fn explicit_selection_is_carried_and_its_dependency_is_not_a_root() {
    let dir = tempfile::tempdir().unwrap();
    let dir = dir.path().to_path_buf();
    let select = ["--start-from=cycle::even"];
    let single = run(&dir, "single", CYCLE, &select);
    let merged = run(&dir, "merged", CYCLE, &[&select[..], MERGE].concat());
    assert_eq!(
        single.functions,
        ["even", "odd"],
        "odd is reachable from even"
    );
    assert_eq!(
        single.roots,
        ["even"],
        "only the selected function is a root"
    );
    assert_eq!(single.explicit_roots, ["even"]);
    assert_eq!(merged.functions, single.functions);
    assert_eq!(merged.roots, single.roots);
    assert_eq!(merged.explicit_roots, single.explicit_roots);
}

const DEFAULT_METHOD: &str = r#"
pub trait T {
    fn value() -> u32 {
        7
    }
}
"#;

/// A selected default trait method is a root by selection: post-merge cleanup must keep it even
/// though nothing mentions it, and both routes must carry it.
#[test]
fn selected_default_method_survives_the_merge() {
    let dir = tempfile::tempdir().unwrap();
    let dir = dir.path().to_path_buf();
    let select = ["--start-from=cycle::T::value"];
    let single = run(&dir, "single", DEFAULT_METHOD, &select);
    let merged = run(
        &dir,
        "merged",
        DEFAULT_METHOD,
        &[&select[..], MERGE].concat(),
    );
    assert_eq!(single.functions, ["value"]);
    assert_eq!(single.roots, ["value"]);
    assert_eq!(single.explicit_roots, ["value"]);
    assert_eq!(
        merged.functions, single.functions,
        "the merge cleanup dropped the selected method"
    );
    assert_eq!(merged.roots, single.roots);
    assert_eq!(merged.explicit_roots, single.explicit_roots);
}

/// The merge reports the errors of its own cleanup: an explicitly selected root that the merged
/// crate no longer holds, with no recorded disposition, makes the merged artifact erroneous.
#[test]
fn merge_reports_a_lost_selected_root() {
    use charon_lib::export::multi_target::merge;
    let dir = tempfile::tempdir().unwrap();
    let dir = dir.path().to_path_buf();
    let source = dir.join("single.rs");
    std::fs::write(&source, CHAIN).unwrap();
    let out = dir.join("single.llbc");
    let mut cmd = Command::cargo_bin("charon").unwrap();
    cmd.args([
        "rustc",
        "--preset=aeneas",
        "--targets=x86_64-unknown-linux-gnu",
    ])
    .arg(format!("--dest-file={}", out.display()))
    .args([
        "--",
        "--edition=2024",
        "--crate-name",
        "cycle",
        "--crate-type",
        "lib",
    ])
    .arg(&source);
    assert!(cmd.status().unwrap().success());
    let mut krate = CrateData::deserialize_from_file(&out, SerializationFormat::Json).unwrap();
    assert!(!krate.has_errors);
    let options = krate.translated.options.clone();
    // Name an absent function as the exact selection, with no disposition recorded.
    let absent = FunDeclId::from_raw(krate.translated.fun_decls.slot_count() + 7);
    krate.translated.explicit_roots.push(ItemId::Fun(absent));
    let merged = merge(options, vec![krate]);
    assert!(merged.has_errors, "the lost selected root was not reported");
}

/// A root a pass consumed is accounted for by its recorded disposition, on both routes: the
/// promoted constant under the selected function is taken by the constant-to-call pass into its
/// init_fun, and the init_fun by inlining.
#[test]
fn consumed_roots_carry_their_disposition() {
    let code = r#"
pub fn bound() -> &'static u32 {
    &7
}
"#;
    let dir = tempfile::tempdir().unwrap();
    let dir = dir.path().to_path_buf();
    let select = ["--start-from=cycle::bound"];
    let single = run(&dir, "single", code, &select);
    let merged = run(&dir, "merged", code, &[&select[..], MERGE].concat());
    for (name, emitted) in [("single", &single), ("merged", &merged)] {
        assert_eq!(emitted.functions, ["bound"], "{name}");
        assert!(
            emitted.globals.is_empty(),
            "{name}: the promoted constant was consumed"
        );
        assert!(
            !emitted.consumed_roots.is_empty(),
            "{name}: no disposition recorded"
        );
        for root in &emitted.roots {
            let present = emitted.functions.contains(root) || emitted.globals.contains(root);
            let consumed = emitted.consumed_roots.contains(root);
            assert!(
                present || consumed,
                "{name}: root {root} is neither present nor accounted for"
            );
        }
    }
    // The dispositions name their identities: the promoted constant went into its init_fun,
    // and the initializer was inlined into the selected function.
    let init_fun = single
        .dispositions
        .iter()
        .find_map(|d| {
            d.strip_prefix("AnonConstToCall ")
                .and_then(|s| s.split(" -> ").nth(1))
        })
        .expect("the promoted constant's disposition names its initializer")
        .to_owned();
    assert!(
        single
            .dispositions
            .contains(&format!("Inlined {init_fun} -> bound")),
        "{:?}",
        single.dispositions
    );
    assert_eq!(merged.roots, single.roots);
    assert_eq!(merged.consumed_roots, single.consumed_roots);
    assert_eq!(merged.dispositions, single.dispositions);
}
