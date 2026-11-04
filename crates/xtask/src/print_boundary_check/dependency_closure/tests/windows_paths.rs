use super::*;

fn fixture_at(root: &Path) -> Value {
    let mut value = fixture();
    for package in value["packages"].as_array_mut().unwrap() {
        if package["source"].is_null() {
            let relative = package["manifest_path"]
                .as_str()
                .unwrap()
                .strip_prefix("/repo/")
                .unwrap()
                .replace('/', "\\");
            package["manifest_path"] = json!(root.join(relative));
        }
    }
    value
}

fn capture_at(value: &Value, root: &Path, entry_root: &Path) -> Checked<ProductionClosure> {
    collect(
        &value.to_string(),
        root,
        &entry_root.join(r"crates\printer\Cargo.toml"),
    )
}

#[test]
fn verbatim_and_ordinary_disk_paths_preserve_the_whole_dependency_identity() {
    let ordinary = Path::new(r"C:\checkout with spaces\repo");
    let verbatim = Path::new(r"\\?\C:\checkout with spaces\repo");
    let expected = capture_at(&fixture_at(ordinary), ordinary, ordinary).unwrap();
    for package_root in [ordinary, verbatim] {
        for root in [ordinary, verbatim] {
            for entry_root in [ordinary, verbatim] {
                assert_eq!(
                    capture_at(&fixture_at(package_root), root, entry_root).unwrap(),
                    expected
                );
            }
        }
    }
}

#[test]
fn verbatim_and_ordinary_unc_paths_preserve_the_whole_dependency_identity() {
    let ordinary = Path::new(r"\\server\share\repo");
    let verbatim = Path::new(r"\\?\UNC\server\share\repo");
    let expected = capture_at(&fixture_at(ordinary), ordinary, ordinary).unwrap();
    for package_root in [ordinary, verbatim] {
        for root in [ordinary, verbatim] {
            assert_eq!(
                capture_at(&fixture_at(package_root), root, verbatim).unwrap(),
                expected
            );
        }
    }
}

#[test]
fn compatible_prefixes_do_not_grant_an_external_root_or_device_authority() {
    let root = Path::new(r"\\?\C:\repo");
    for external in [
        r"D:\repo\plan\Cargo.toml",
        r"C:\repo-neighbor\plan\Cargo.toml",
        r"C:\outside\plan\Cargo.toml",
        r"C:repo\plan\Cargo.toml",
        r"\\server\share\repo\plan\Cargo.toml",
        r"\\.\C:\repo\plan\Cargo.toml",
    ] {
        let mut value = fixture_at(root);
        value["packages"][1]["manifest_path"] = json!(external);
        assert_eq!(
            capture_at(&value, root, root).unwrap_err().rule,
            "external-path-dependency"
        );
    }
    let root = Path::new(r"\\?\UNC\server\share\repo");
    for external in [
        r"\\other-server\share\repo\plan\Cargo.toml",
        r"\\server\other-share\repo\plan\Cargo.toml",
    ] {
        let mut value = fixture_at(root);
        value["packages"][1]["manifest_path"] = json!(external);
        assert_eq!(
            capture_at(&value, root, root).unwrap_err().rule,
            "external-path-dependency"
        );
    }
}

#[test]
fn parent_traversal_still_refuses_in_both_windows_representations() {
    let root = Path::new(r"\\?\C:\repo");
    for outside in [
        r"C:\repo\..\outside\Cargo.toml",
        r"\\?\C:\repo\..\outside\Cargo.toml",
    ] {
        let mut value = fixture_at(root);
        value["packages"][1]["manifest_path"] = json!(outside);
        assert_eq!(
            capture_at(&value, root, root).unwrap_err().rule,
            "dependency-path"
        );
    }
}

#[test]
fn entry_aliases_cannot_hide_a_duplicate_entry_manifest() {
    let ordinary = Path::new(r"C:\repo");
    let verbatim = Path::new(r"\\?\C:\repo");
    let mut value = fixture_at(ordinary);
    let mut duplicate = value["packages"][0].clone();
    duplicate["id"] = json!("another-entry");
    duplicate["manifest_path"] = json!(verbatim.join(r"crates\printer\Cargo.toml"));
    value["packages"].as_array_mut().unwrap().push(duplicate);
    assert_eq!(
        capture_at(&value, verbatim, ordinary).unwrap_err().rule,
        "duplicate-entry"
    );
}
