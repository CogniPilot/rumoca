//! Exact Markdown fixtures owned by the guide browser-binding tests.

use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};

pub(super) struct GuideFence {
    pub page: PathBuf,
    pub line: usize,
    pub source_line_count: usize,
    pub source: String,
    pub language: String,
    flags: Vec<String>,
}

impl GuideFence {
    pub(super) fn has_flag(&self, flag: &str) -> bool {
        self.flags.iter().any(|value| value == flag)
    }

    pub(super) fn fragment_context(&self) -> Option<&str> {
        self.flags
            .iter()
            .find_map(|flag| flag.strip_prefix("context-"))
    }

    pub(super) fn model_name(&self) -> Option<String> {
        self.source.lines().rev().find_map(|line| {
            let rest = line.strip_prefix("model ")?;
            let name: String = rest
                .chars()
                .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
                .collect();
            (!name.is_empty()).then_some(name)
        })
    }

    pub(super) fn included_library_path(&self) -> Option<PathBuf> {
        let path = self
            .source
            .trim()
            .strip_prefix("{{#include ")?
            .strip_suffix("}}")?;
        let path = path.strip_prefix("../../../../examples/reinforcement_learning/")?;
        match path {
            "NeuralNetwork.mo" | "cartpole/PolicySearch.mo" => {
                Some(Path::new("examples/reinforcement_learning").join(path))
            }
            _ => None,
        }
    }

    pub(super) fn scenario_path(&self) -> Option<PathBuf> {
        self.source.lines().find_map(|line| {
            let path = line.strip_prefix("// rumoca-live-scenario: ")?;
            let relative = path
                .strip_prefix("../repo-examples/")
                .expect("guide scenarios refer to staged repository examples");
            Some(Path::new("examples").join(relative))
        })
    }
}

pub(super) fn repository_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn markdown_pages(dir: &Path, pages: &mut Vec<PathBuf>) {
    let mut entries: Vec<PathBuf> = std::fs::read_dir(dir)
        .unwrap_or_else(|error| panic!("read {}: {error}", dir.display()))
        .map(|entry| entry.expect("guide directory entry").path())
        .collect();
    entries.sort();
    for path in entries {
        if path.is_dir() {
            markdown_pages(&path, pages);
        } else if path.extension().is_some_and(|ext| ext == "md") {
            pages.push(path);
        }
    }
}

pub(super) fn markdown_fences() -> Vec<GuideFence> {
    let mut pages = Vec::new();
    markdown_pages(&repository_root().join("docs/user-guide/src"), &mut pages);
    let mut fences = Vec::new();
    for page in pages {
        let text = std::fs::read_to_string(&page)
            .unwrap_or_else(|error| panic!("read {}: {error}", page.display()));
        let mut lines = text.lines().enumerate();
        while let Some((start, line)) = lines.next() {
            let Some(info) = line.strip_prefix("```") else {
                continue;
            };
            let body: Vec<_> = lines
                .by_ref()
                .take_while(|(_, line)| !line.starts_with("```"))
                .map(|(_, line)| line)
                .collect();
            fences.push(GuideFence {
                page: page.clone(),
                line: start + 2,
                source_line_count: body.len(),
                source: body.join("\n") + "\n",
                language: info.split(',').next().unwrap().to_owned(),
                flags: info.split(',').map(|flag| flag.trim().to_owned()).collect(),
            });
        }
    }
    fences
}

pub(super) fn guide_fences() -> Vec<GuideFence> {
    markdown_fences()
        .into_iter()
        .filter(|fence| fence.language == "modelica")
        .collect()
}

#[test]
fn user_guide_fence_inventory_preserves_every_embedded_source() {
    let fences = guide_fences();
    assert_eq!(fences.len(), 48, "review new embedded examples explicitly");
    assert_eq!(
        fences.iter().filter(|f| f.model_name().is_some()).count(),
        17
    );
    assert_eq!(fences.iter().filter(|f| f.has_flag("fragment")).count(), 8);
    let contexts: std::collections::BTreeSet<_> = fences
        .iter()
        .filter(|fence| fence.has_flag("fragment"))
        .map(|fence| fence.fragment_context().expect("fragment context"))
        .collect();
    assert_eq!(
        contexts.len(),
        8,
        "each exact fragment needs its own context"
    );
    let scenarios: Vec<_> = fences
        .iter()
        .filter_map(GuideFence::scenario_path)
        .collect();
    assert_eq!(scenarios.len(), 22);
    let issued: Vec<PathBuf> =
        serde_json::from_str(
            &std::fs::read_to_string(repository_root().join(
                "crates/rumoca-bind-wasm/src/tests/fixtures/user_guide/linked-scenarios.json",
            ))
            .expect("mandatory browser scenario roster"),
        )
        .expect("scenario path array");
    let actual: std::collections::BTreeSet<_> = scenarios.iter().cloned().collect();
    assert_eq!(actual.len(), 17);
    assert_eq!(issued.len(), actual.len(), "no duplicate scenario cells");
    assert_eq!(
        issued
            .into_iter()
            .collect::<std::collections::BTreeSet<_>>(),
        actual
    );
    for path in scenarios {
        assert!(
            repository_root().join(&path).is_file(),
            "{}",
            path.display()
        );
    }
    for fence in fences {
        let classified = if fence.has_flag("fragment") {
            matches!(
                fence.fragment_context(),
                Some(
                    "neural-tensor"
                        | "neural-width"
                        | "neural-population"
                        | "neural-observed"
                        | "neural-dynamics"
                        | "state"
                        | "experiment"
                        | "tanks"
                )
            )
        } else {
            fence.model_name().is_some()
                || fence.scenario_path().is_some()
                || fence.included_library_path().is_some()
        };
        assert!(
            classified,
            "unclassified guide example {}:{}",
            fence.page.display(),
            fence.line
        );
        assert!(fence.line > 0, "{}", fence.page.display());
        assert!(fence.source.ends_with('\n'));
    }
}

fn included_library_sources() -> std::collections::BTreeMap<String, String> {
    let records: Vec<serde_json::Value> = serde_json::from_str(
        &std::fs::read_to_string(repository_root().join(
            "crates/rumoca-bind-wasm/src/tests/fixtures/user_guide/included-rl-sources.json",
        ))
        .unwrap(),
    )
    .unwrap();
    let fences = guide_fences();
    let actual: std::collections::BTreeSet<_> = fences
        .iter()
        .filter_map(GuideFence::included_library_path)
        .collect();
    let mut sources = std::collections::BTreeMap::new();
    for record in records
        .iter()
        .filter(|record| record["language"] == "modelica")
    {
        let page = repository_root()
            .join("docs/user-guide/src")
            .join(record["page"].as_str().unwrap());
        let fence = fences
            .iter()
            .find(|fence| fence.page == page && Some(fence.line as u64) == record["line"].as_u64())
            .unwrap();
        let path = fence
            .included_library_path()
            .expect("registered library include");
        let key = record["path"].as_str().unwrap();
        assert_eq!(path, PathBuf::from(key));
        assert_eq!(
            format!("{:x}", Sha256::digest(fence.source.as_bytes())),
            record["directive_sha256"]
        );
        let source = std::fs::read_to_string(repository_root().join(&path)).unwrap();
        assert_eq!(
            format!("{:x}", Sha256::digest(source.as_bytes())),
            record["source_sha256"]
        );
        assert!(sources.insert(key.to_owned(), source).is_none());
    }
    assert_eq!(sources.len(), 2, "both exact displayed Modelica libraries");
    assert_eq!(
        actual,
        sources
            .keys()
            .map(PathBuf::from)
            .collect::<std::collections::BTreeSet<_>>()
    );
    sources
}

#[test]
fn user_guide_library_includes_refuse_unregistered_paths() {
    let mut fence = GuideFence {
        page: PathBuf::from("simulation/neural-network.md"),
        line: 1,
        source_line_count: 1,
        source: String::new(),
        language: "modelica".to_owned(),
        flags: Vec::new(),
    };
    for path in [
        "../../../../outside.mo",
        "../../../../examples/reinforcement_learning/../secret.mo",
        "../../../../examples/reinforcement_learning/Unregistered.mo",
    ] {
        fence.source = format!("{{{{#include {path}}}}}\n");
        assert!(fence.included_library_path().is_none());
    }
}

#[test]
fn user_guide_included_libraries_compile_through_the_actual_loader() {
    let _guard = super::session_test_guard();
    super::clear_source_root_cache().unwrap();
    let loaded =
        super::load_source_roots(&serde_json::to_string(&included_library_sources()).unwrap())
            .unwrap();
    let loaded: serde_json::Value = serde_json::from_str(&loaded).unwrap();
    assert_eq!(loaded["error_count"], 0);
    let source = "model GuideIncludedPolicy\n Real y[1];\n Real mean[2];\n Real deviation[2];\n equation\n y=NeuralNetwork.activate(NeuralNetwork.dense({{1,2}},{0},{3,4}));\n (mean,deviation)=PolicySearch.fit({{1,2},{3,4}});\n end GuideIncludedPolicy;\n";
    let checked = super::compile(source, "GuideIncludedPolicy").unwrap();
    let checked: serde_json::Value = serde_json::from_str(&checked).unwrap();
    assert_eq!(checked["balance"]["is_balanced"], true);
    super::clear_source_root_cache().unwrap();
}
