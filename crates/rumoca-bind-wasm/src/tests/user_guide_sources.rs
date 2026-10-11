//! Exact Markdown fixtures owned by the guide browser-binding tests.

use std::path::{Path, PathBuf};

pub(super) struct GuideFence {
    pub page: PathBuf,
    pub line: usize,
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
    assert_eq!(fences.len(), 45, "review new embedded examples explicitly");
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
    assert_eq!(scenarios.len(), 21);
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
            fence.model_name().is_some() || fence.scenario_path().is_some()
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

#[cfg(feature = "native-assignments")]
#[test]
fn user_guide_other_language_inventory_rejects_unclassified_or_changed_examples() {
    use sha2::{Digest, Sha256};
    let inventory = std::fs::read_to_string(
        repository_root().join("docs/verification/user-guide-ci/non-modelica-inventory.json"),
    )
    .unwrap();
    let inventory: serde_json::Value = serde_json::from_str(&inventory).unwrap();
    assert_eq!(inventory["schema"], 1);
    let entries = inventory["entries"].as_array().unwrap();
    let fences: Vec<_> = markdown_fences()
        .into_iter()
        .filter(|fence| fence.language != "modelica")
        .collect();
    assert_eq!(fences.len(), 115);
    assert_eq!(entries.len(), fences.len());
    for (fence, entry) in fences.into_iter().zip(entries) {
        let source_root = repository_root().join("docs/user-guide/src");
        let page = fence
            .page
            .strip_prefix(&source_root)
            .unwrap()
            .to_str()
            .unwrap();
        assert_eq!(entry["page"], page);
        assert_eq!(entry["line"], fence.line);
        assert_eq!(entry["language"], fence.language);
        let digest = format!("{:x}", Sha256::digest(fence.source.as_bytes()));
        assert_eq!(entry["source_sha256"], digest, "{page}:{}", fence.line);
        for field in ["classification", "owner", "scope"] {
            assert!(entry[field].as_str().is_some_and(|value| !value.is_empty()));
        }
    }
}
