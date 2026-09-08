use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;

use proc_macro2::{TokenStream, TokenTree};

const OMC_DIFFERENTIAL_MARKER: &str = "omc-differential-required";

fn workspace_root() -> PathBuf {
    rumoca_core::workspace_root_from_manifest_dir(env!("CARGO_MANIFEST_DIR"))
}

fn omc_differential_marker_path() -> PathBuf {
    rumoca_core::msl_cache_dir_from_manifest(env!("CARGO_MANIFEST_DIR"))
        .join(OMC_DIFFERENTIAL_MARKER)
}

pub(super) fn omc_differential_is_required() -> bool {
    omc_differential_marker_path().is_file()
}

#[derive(Default)]
struct MarkerUseInventory {
    owner_identifiers: usize,
    relative_marker_literals: usize,
}

fn inventory_marker_uses(tokens: TokenStream, inventory: &mut MarkerUseInventory) {
    for token in tokens {
        match token {
            TokenTree::Group(group) => inventory_marker_uses(group.stream(), inventory),
            TokenTree::Ident(ident) if ident == "omc_differential_is_required" => {
                inventory.owner_identifiers += 1;
            }
            TokenTree::Literal(literal)
                if literal.to_string().contains(OMC_DIFFERENTIAL_MARKER) =>
            {
                inventory.relative_marker_literals += 1;
            }
            _ => {}
        }
    }
}

#[test]
fn marker_path_is_workspace_scoped_probe() {
    let marker = omc_differential_marker_path();
    assert!(
        marker.is_absolute(),
        "marker path must not depend on process CWD"
    );
    assert_eq!(
        marker,
        workspace_root()
            .join("target/msl")
            .join(OMC_DIFFERENTIAL_MARKER)
    );
}

#[test]
fn marker_path_is_independent_of_process_cwd() {
    let unrelated_cwd = tempfile::tempdir().expect("unrelated child CWD");
    let output = Command::new(std::env::current_exe().expect("suite_core test binary"))
        .arg("required_tool_markers::marker_path_is_workspace_scoped_probe")
        .arg("--exact")
        .arg("--nocapture")
        .current_dir(unrelated_cwd.path())
        .output()
        .expect("run marker-path probe from unrelated CWD");
    assert!(
        output.status.success(),
        "workspace marker changed meaning under a different CWD:\n{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn every_omc_required_check_uses_the_workspace_owner() {
    let _marker_is_present = omc_differential_is_required();
    let consumers = [
        ("jacobian_admission_battery.rs", 4),
        ("jacobian_standard_modelica.rs", 3),
        ("omc_differential_semantics.rs", 2),
    ];
    let source_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/suite_core");
    for (consumer, expected_owner_identifiers) in consumers {
        let source = std::fs::read_to_string(source_root.join(consumer))
            .unwrap_or_else(|error| panic!("read {consumer}: {error}"));
        let tokens = TokenStream::from_str(&source)
            .unwrap_or_else(|error| panic!("tokenize {consumer}: {error}"));
        let mut inventory = MarkerUseInventory::default();
        inventory_marker_uses(tokens, &mut inventory);
        assert_eq!(
            inventory.relative_marker_literals, 0,
            "{consumer} still embeds the CWD-relative OMC marker"
        );
        assert_eq!(
            inventory.owner_identifiers, expected_owner_identifiers,
            "{consumer} must route every reviewed OMC-required decision through the workspace \
             marker owner; the count includes its one import"
        );
    }
}

#[test]
fn required_omc_parent_owns_one_marker_transaction() {
    let workflow = std::fs::read_to_string(workspace_root().join(".github/workflows/ci.yml"))
        .expect("read CI workflow");
    let step = workflow
        .split_once("- name: Run required OMC differential semantics test")
        .and_then(|(_, suffix)| suffix.split_once("# Report-only survey"))
        .map(|(step, _)| step)
        .expect("required OMC workflow step");
    let commands = step
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty() && *line != "shell: bash" && *line != "run: |")
        .collect::<Vec<_>>();
    let expected_transaction = [
        "set -euo pipefail",
        "mkdir -p target/msl",
        "exec 9>target/msl/omc-differential-required.lock",
        "flock 9",
        "trap 'rm -f target/msl/omc-differential-required' EXIT",
        "touch target/msl/omc-differential-required",
        "cargo test --package rumoca --test suite_core \\",
        "omc_differential_semantics::encapsulated_scope_rejection_matches_omc \\",
        "-- --nocapture",
    ];
    assert_eq!(
        commands, expected_transaction,
        "required OMC step must contain exactly one fail-closed marker transaction; extra \
         commands can create an early-success or stale-marker path"
    );
}
