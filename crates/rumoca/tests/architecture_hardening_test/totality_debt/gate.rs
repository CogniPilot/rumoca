//! Ratchet on run-time totality assertions in the five back-end crates.
//!
//! SPEC_0008 sanctions `panic!`/`expect("invariant")` for internal contract
//! violations, and SPEC_0037 requires each phase to prove its target
//! well-formed. Every such assertion is therefore a live proof obligation whose
//! justification is a string, and the set of obligations must only shrink.

use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

use crate::architecture_hardening_support::{collect_rs_files, workspace_root};
use crate::totality_debt::scan::{
    cfg_test_module_names, declared_module_names, scan_tracks_file_to_its_end,
    silent_totality_debt_sites, totality_debt_sites,
};

/// Ceilings on `.expect(`, `panic!(`, `unreachable!(`, `todo!(` and
/// `unimplemented!(` in production source, per crate.
const TOTALITY_DEBT_CEILINGS: &[(&str, usize)] = &[
    ("rumoca-phase-solve", 272),
    ("rumoca-phase-galec", 240),
    ("rumoca-phase-dae", 232),
    ("rumoca-ir-dae", 60),
    ("rumoca-ir-solve", 15),
];

/// Ceilings on `.unwrap()`, which carries the same obligation with no message.
/// Pinned separately so that dropping a message cannot make the debt look
/// smaller.
const SILENT_TOTALITY_DEBT_CEILINGS: &[(&str, usize)] = &[
    ("rumoca-phase-solve", 0),
    ("rumoca-phase-galec", 0),
    ("rumoca-phase-dae", 2),
    ("rumoca-ir-dae", 0),
    ("rumoca-ir-solve", 0),
];

const GUIDANCE: &str = "\
Each of these is a proof obligation whose justification lives in a string. \
Before adding one, discharge the fact instead: give the value a branded or \
witness type with a private constructor when the fact is local, or widen the \
handover type so the phase that already proved the fact carries the proof \
forward. A genuinely fallible input belongs in a `Result` at a trust boundary, \
not in an assertion. The ranked type-away-first worklist is \
docs/dev-guide/src/architecture/totality-debt-audit-2026-08-22.md.";

/// Measured production totality debt for one crate.
struct CrateDebt {
    asserted: usize,
    silent: usize,
}

#[test]
fn test_backend_totality_debt_does_not_increase() {
    let mut violations = Vec::new();
    let mut measured = Vec::new();

    for (crate_name, ceiling) in TOTALITY_DEBT_CEILINGS {
        let debt = measure_crate(crate_name);
        measured.push(format!(
            "  {crate_name}: {} asserted (ceiling {ceiling}), {} silent",
            debt.asserted, debt.silent
        ));
        if debt.asserted > *ceiling {
            violations.push(format!(
                "{crate_name}: {} run-time totality assertions, ceiling {ceiling}",
                debt.asserted
            ));
        }
    }

    assert!(
        violations.is_empty(),
        "production totality debt rose above its pinned ceiling:\n{}\n\n{GUIDANCE}\n\nMeasured:\n{}",
        violations.join("\n"),
        measured.join("\n")
    );
}

#[test]
fn test_backend_silent_totality_debt_does_not_increase() {
    let mut violations = Vec::new();

    for (crate_name, ceiling) in SILENT_TOTALITY_DEBT_CEILINGS {
        let debt = measure_crate(crate_name);
        if debt.silent > *ceiling {
            violations.push(format!(
                "{crate_name}: {} production `.unwrap()` calls, ceiling {ceiling}",
                debt.silent
            ));
        }
    }

    assert!(
        violations.is_empty(),
        "production `.unwrap()` rose above its pinned ceiling:\n{}\n\n{GUIDANCE}",
        violations.join("\n")
    );
}

/// The two ceiling tables must name exactly the same crates, so that adding a
/// crate to one and forgetting the other cannot leave a counter unpinned.
#[test]
fn test_totality_ceiling_tables_cover_the_same_crates() {
    let asserted: BTreeSet<&str> = TOTALITY_DEBT_CEILINGS
        .iter()
        .map(|(name, _)| *name)
        .collect();
    let silent: BTreeSet<&str> = SILENT_TOTALITY_DEBT_CEILINGS
        .iter()
        .map(|(name, _)| *name)
        .collect();
    assert_eq!(asserted, silent);
}

/// Every pinned crate must contribute production source, so a crate rename can
/// never silently reduce the measured debt to zero.
#[test]
fn test_pinned_crates_have_production_source() {
    for (crate_name, _) in TOTALITY_DEBT_CEILINGS {
        let files = production_source_files(crate_name);
        assert!(
            !files.is_empty(),
            "{crate_name} contributed no production source files; the ceiling table names a \
crate that no longer exists or no longer has a src directory"
        );
    }
}

/// The counts above are only meaningful if the scan read each file to its end.
/// A file the scan loses track of contributes a silently lower number, so the
/// tracking is asserted rather than assumed.
#[test]
fn test_totality_scan_tracks_every_production_file_to_its_end() {
    let mut untracked = Vec::new();
    let mut scanned = 0usize;

    for (crate_name, _) in TOTALITY_DEBT_CEILINGS {
        for path in production_source_files(crate_name) {
            let Ok(content) = fs::read_to_string(&path) else {
                continue;
            };
            scanned += 1;
            if !scan_tracks_file_to_its_end(&content) {
                untracked.push(path.display().to_string());
            }
        }
    }

    assert!(scanned > 0, "no production source was scanned at all");
    assert!(
        untracked.is_empty(),
        "the totality scan ended inside a `#[cfg(test)]` item, a block comment or a string literal in \
these files, so every line below that point was dropped from the count: {untracked:#?}"
    );
}

fn measure_crate(crate_name: &str) -> CrateDebt {
    let mut debt = CrateDebt {
        asserted: 0,
        silent: 0,
    };
    for path in production_source_files(crate_name) {
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        debt.asserted += totality_debt_sites(&content).len();
        debt.silent += silent_totality_debt_sites(&content).len();
    }
    debt
}

/// Source files of a crate that reach a release build.
fn production_source_files(crate_name: &str) -> Vec<PathBuf> {
    let src = workspace_root().join("crates").join(crate_name).join("src");
    production_source_files_under(&src)
}

/// Every `.rs` file under a library's `src` directory except those the compiler
/// itself drops from a release build.
///
/// No part of a path excludes a file. Neither a file called `tests.rs` nor a
/// directory called `tests` is a compiler fact: both compile into the library
/// target like any other module, so reading either name as an exclusion would
/// let a rename or a move hide a shipped obligation while the count fell. A
/// file earns its exclusion only by being reachable from a `#[cfg(test)]`
/// declaration, which is the same fact the compiler acts on.
pub(crate) fn production_source_files_under(src: &Path) -> Vec<PathBuf> {
    if !src.is_dir() {
        return Vec::new();
    }
    let mut files = Vec::new();
    collect_rs_files(src, &mut files);
    files.sort();
    let test_only = cfg_test_module_closure(&files);
    files.retain(|path| !test_only.contains(path));
    files
}

/// Files reachable from a `#[cfg(test)] mod <name>;` declaration, transitively.
/// Such a file is absent from a release build, so it carries no shipped
/// obligation even when its name does not mark it as a test.
fn cfg_test_module_closure(files: &[PathBuf]) -> BTreeSet<PathBuf> {
    let mut frontier = Vec::new();
    for path in files {
        let Ok(content) = fs::read_to_string(path) else {
            continue;
        };
        for name in cfg_test_module_names(&content) {
            frontier.extend(resolve_module_file(path, &name));
        }
    }

    let mut closure = BTreeSet::new();
    while let Some(path) = frontier.pop() {
        if !closure.insert(path.clone()) {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        for name in declared_module_names(&content) {
            frontier.extend(resolve_module_file(&path, &name));
        }
    }
    closure
}

/// Resolves `mod <name>;` declared in `declaring` to the file that backs it.
fn resolve_module_file(declaring: &Path, name: &str) -> Option<PathBuf> {
    let directory = match declaring.file_stem().and_then(|stem| stem.to_str()) {
        Some("mod" | "lib" | "main") => declaring.parent()?.to_path_buf(),
        _ => declaring.with_extension(""),
    };
    let flat = directory.join(format!("{name}.rs"));
    if flat.is_file() {
        return Some(flat);
    }
    let nested = directory.join(name).join("mod.rs");
    nested.is_file().then_some(nested)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A path never excludes a file, in any of its parts. A module under a
    /// directory called `tests`, and a module whose own file is called
    /// `tests.rs`, both compile into the library target unless separately
    /// declared `#[cfg(test)]`, so both are counted.
    ///
    /// The `tests/mod.rs` case is the one a directory-name rule hides: it takes
    /// a whole subtree out of the count while every line of it still ships.
    #[test]
    fn path_names_never_exclude_a_shipped_module() {
        let fixture = Fixture::new("path-names");
        fixture.write("lib.rs", "mod tensor;\n");
        fixture.write(
            "tensor.rs",
            "pub(crate) mod tests;\nmod generated;\nfn call() { tests::shipped(None); }\n",
        );
        fixture.write(
            "tensor/tests/mod.rs",
            "fn shipped(v: Option<u32>) -> u32 { v.expect(\"this ships in a release build\") }\n",
        );
        fixture.write(
            "tensor/generated/mod.rs",
            "fn emitted() { unreachable!(\"emitted, and shipped\"); }\n",
        );

        let files = production_source_files_under(&fixture.root);
        assert_eq!(
            fixture.relative_names(&files),
            vec![
                "lib.rs",
                "tensor.rs",
                "tensor/generated/mod.rs",
                "tensor/tests/mod.rs",
            ],
            "a directory called `tests` or `generated` is not a compiler fact and must not \
exclude the modules under it"
        );

        let counted: usize = files
            .iter()
            .map(|path| {
                let content = fs::read_to_string(path).unwrap_or_default();
                totality_debt_sites(&content).len()
            })
            .sum();
        assert_eq!(counted, 2, "both shipped obligations must reach the count");
    }

    /// Every file the five crates keep under a directory called `tests` earns
    /// its exclusion from a `#[cfg(test)]` declaration, not from the directory
    /// name. The same redundancy the file-name pin below asserts, for the
    /// directory that would otherwise hide a whole subtree at once.
    #[test]
    fn test_directories_are_excluded_by_their_cfg_test_declaration() {
        for (crate_name, _) in TOTALITY_DEBT_CEILINGS {
            for path in production_source_files(crate_name) {
                let relative = path.to_string_lossy().replace('\\', "/");
                assert!(
                    !relative.contains("/tests/"),
                    "{} is counted as production source and sits under a directory called \
`tests`; a release build compiles it in, so either declare its module \
`#[cfg(test)]` or move it under a production name",
                    path.display()
                );
            }
        }
    }

    /// Every test-named file the five crates actually contain earns its
    /// exclusion from a `#[cfg(test)]` declaration, not from its name. If that
    /// ever stops holding, the count rises and the ceiling catches it.
    #[test]
    fn test_named_files_are_excluded_by_their_cfg_test_declaration() {
        for (crate_name, _) in TOTALITY_DEBT_CEILINGS {
            for path in production_source_files(crate_name) {
                let name = path
                    .file_name()
                    .map(|name| name.to_string_lossy().into_owned())
                    .unwrap_or_default();
                assert!(
                    name != "tests.rs" && !name.ends_with("_tests.rs"),
                    "{} is counted as production source but is named like a test; either \
declare it `#[cfg(test)]` or give it a production name",
                    path.display()
                );
            }
        }
    }

    /// A file declared as `#[cfg(test)] mod <name>;` is dropped even when its
    /// name does not mark it as a test, and the exclusion follows the module
    /// tree transitively rather than stopping at the declared file.
    #[test]
    fn cfg_test_modules_are_excluded_transitively() {
        let fixture = Fixture::new("closure");
        fixture.write(
            "lib.rs",
            "#[cfg(test)]\nmod corpus;\nmod shipped;\nfn run() { one().expect(\"lib\"); }\n",
        );
        fixture.write(
            "corpus.rs",
            "mod deeper;\nfn fixture() { two().expect(\"corpus\"); }\n",
        );
        fixture.write(
            "corpus/deeper.rs",
            "fn d() { three().expect(\"deeper\"); }\n",
        );
        fixture.write("shipped.rs", "fn s() { four().expect(\"shipped\"); }\n");

        let files = production_source_files_under(&fixture.root);
        let names = fixture.relative_names(&files);
        assert_eq!(names, vec!["lib.rs", "shipped.rs"]);
    }

    /// Module resolution must follow both the flat and the directory layout.
    #[test]
    fn module_files_resolve_in_both_layouts() {
        let fixture = Fixture::new("resolve");
        fixture.write("lib.rs", "mod flat;\nmod nested;\n");
        fixture.write("flat.rs", "");
        fixture.write("nested/mod.rs", "mod child;\n");
        fixture.write("nested/child.rs", "");

        let lib = fixture.root.join("lib.rs");
        assert_eq!(
            resolve_module_file(&lib, "flat"),
            Some(fixture.root.join("flat.rs"))
        );
        assert_eq!(
            resolve_module_file(&lib, "nested"),
            Some(fixture.root.join("nested/mod.rs"))
        );
        assert_eq!(resolve_module_file(&lib, "absent"), None);

        let nested = fixture.root.join("nested/mod.rs");
        assert_eq!(
            resolve_module_file(&nested, "child"),
            Some(fixture.root.join("nested/child.rs"))
        );
    }

    /// A source tree with known contents must produce the counts the gate
    /// claims: assertions outside `#[cfg(test)]` are counted in both the
    /// semicolon and the brace form, and assertions inside are not.
    #[test]
    fn measured_counts_match_a_known_tree() {
        let fixture = Fixture::new("counts");
        fixture.write(
            "lib.rs",
            "#[cfg(test)]\nmod tests;\nfn a() { one().expect(\"a\"); panic!(\"b\"); }\n",
        );
        fixture.write("tests.rs", "fn t() { two().expect(\"hidden\"); }\n");
        fixture.write(
            "inline.rs",
            "fn c() { todo!(\"c\"); }\n#[cfg(test)]\nmod tests {\n    fn t() { unreachable!(\"hidden\"); }\n}\nfn d() { three().unwrap(); }\n",
        );

        let files = production_source_files_under(&fixture.root);
        assert_eq!(fixture.relative_names(&files), vec!["inline.rs", "lib.rs"]);

        let mut asserted = 0;
        let mut silent = 0;
        for path in files {
            let content = fs::read_to_string(&path).unwrap_or_default();
            asserted += totality_debt_sites(&content).len();
            silent += silent_totality_debt_sites(&content).len();
        }
        assert_eq!(asserted, 3, "expect + panic in lib.rs, todo in inline.rs");
        assert_eq!(silent, 1, "one production unwrap in inline.rs");
    }

    /// A tree whose production file carries a multi-line string holding braces.
    /// The string body is not structure, so the `#[cfg(test)]` block above it
    /// still ends where its own brace ends and the obligations below the string
    /// still reach the count.
    #[test]
    fn a_multi_line_string_does_not_hide_the_obligations_below_it() {
        let fixture = Fixture::new("multi-line-string");
        fixture.write(
            "lib.rs",
            r#"#[cfg(test)]
mod tests {
    const TEMPLATE: &str = "opens {
an unbalanced brace { inside the string body
end";
    fn hidden() {
        one().expect("hidden");
    }
}

fn shipped() {
    two().expect("shipped");
    three().unwrap();
}
"#,
        );

        let files = production_source_files_under(&fixture.root);
        assert_eq!(fixture.relative_names(&files), vec!["lib.rs"]);

        let content = fs::read_to_string(fixture.root.join("lib.rs")).unwrap_or_default();
        assert_eq!(
            totality_debt_sites(&content).len(),
            1,
            "the obligation below a multi-line string must reach the count"
        );
        assert_eq!(silent_totality_debt_sites(&content).len(), 1);
        assert!(
            scan_tracks_file_to_its_end(&content),
            "the file is well formed, so the tracking assertion is not what protects this count"
        );
    }

    /// A `#[cfg(test)] mod <name>;` written inside a multi-line string is text,
    /// not a declaration. Reading it as one takes a shipped file out of the
    /// count entirely, which is the largest loss the scan can suffer.
    #[test]
    fn a_module_declaration_inside_a_multi_line_string_is_not_a_declaration() {
        let fixture = Fixture::new("string-declaration");
        fixture.write(
            "lib.rs",
            r#"const SNIPPET: &str = "
#[cfg(test)]
mod shipped;
";
mod shipped;
"#,
        );
        fixture.write(
            "shipped.rs",
            "fn s() { four().expect(\"this ships in a release build\"); }\n",
        );

        let files = production_source_files_under(&fixture.root);
        assert_eq!(
            fixture.relative_names(&files),
            vec!["lib.rs", "shipped.rs"],
            "a declaration quoted inside a string must not pull a shipped file out of the count"
        );

        let counted: usize = files
            .iter()
            .map(|path| {
                let content = fs::read_to_string(path).unwrap_or_default();
                totality_debt_sites(&content).len()
            })
            .sum();
        assert_eq!(counted, 1);
    }

    /// A scratch source tree, removed when the test ends.
    struct Fixture {
        root: PathBuf,
    }

    impl Fixture {
        fn new(label: &str) -> Self {
            let root = std::env::temp_dir()
                .join(format!("rumoca-totality-{label}-{}", std::process::id()));
            let _stale_fixture_removal_error = fs::remove_dir_all(&root);
            fs::create_dir_all(&root).expect("create fixture root");
            Self { root }
        }

        fn write(&self, relative: &str, content: &str) {
            let path = self.root.join(relative);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).expect("create fixture directory");
            }
            fs::write(&path, content).expect("write fixture file");
        }

        /// Fixture-relative paths in a fixed order, so an expectation reads as
        /// a set rather than depending on how `Path` orders its components.
        fn relative_names(&self, files: &[PathBuf]) -> Vec<String> {
            let mut names: Vec<String> = files
                .iter()
                .filter_map(|path| path.strip_prefix(&self.root).ok())
                .map(|path| path.to_string_lossy().replace('\\', "/"))
                .collect();
            names.sort();
            names
        }
    }

    impl Drop for Fixture {
        fn drop(&mut self) {
            let _fixture_removal_error = fs::remove_dir_all(&self.root);
        }
    }
}
