use super::*;
use rumoca_compile::codegen::targets::parse_target_manifest;

fn manifest_files(toml: &str) -> Vec<TargetFile> {
    parse_target_manifest(toml)
        .expect("manifest should parse")
        .files
}

fn fixture_manifest() -> rumoca_compile::codegen::targets::TargetManifest {
    parse_target_manifest(
        r#"
version = 1
ir = "dae"
name = "package-fixture"
readiness_level = 2

[capabilities]

[[assets]]
source = "fixture-assets"
dest = "assets/"

[[files]]
id = "marker"
path = "marker.txt"
template = "marker-template"
"#,
    )
    .expect("fixture manifest parses")
}

fn build_fixture(
    out_dir: &Path,
    archive_path: Option<&Path>,
    marker: &str,
    fail_render: bool,
    fail_assets: bool,
) -> Result<()> {
    let manifest = fixture_manifest();
    let package = PackageSpec {
        required_files: vec!["marker.txt".to_owned()],
        zip: archive_path.map(|path| ZipPackage {
            archive_path: path.to_owned(),
        }),
    };
    render_and_package(
        &manifest.files,
        |template, _| {
            if fail_render && template == "marker-template" {
                return Err(anyhow::anyhow!("injected render failure"));
            }
            Ok(if template == "marker-template" {
                marker.to_owned()
            } else {
                template.to_owned()
            })
        },
        &manifest.assets,
        |_| {
            if fail_assets {
                return Err(anyhow::anyhow!("injected asset failure"));
            }
            Ok(vec![TargetAssetFile {
                relative_path: "payload.bin".to_owned(),
                bytes: b"asset bytes".to_vec(),
            }])
        },
        &package,
        out_dir,
    )
}

fn output_snapshot(out_dir: &Path, archive_path: Option<&Path>) -> Vec<Vec<u8>> {
    let mut snapshot = vec![
        fs::read(out_dir.join("marker.txt")).expect("marker exists"),
        fs::read(out_dir.join("assets/payload.bin")).expect("asset exists"),
    ];
    if let Some(archive_path) = archive_path {
        snapshot.push(fs::read(archive_path).expect("archive exists"));
    }
    snapshot
}

/// A multi-level checksum web is ordered producer-first, with the catalog
/// emitted only after every index it references.
#[test]
fn topo_sort_orders_producers_before_consumers() {
    let files = manifest_files(
        r#"
version = 1
ir = "dae"
name = "web"
readiness_level = 2

[capabilities]

[[files]]
id = "source_data"
path = "data/source.bin"
template = "source.jinja"

[[files]]
id = "api"
path = "include/api.txt"
template = "api.jinja"

[[files]]
id = "implementation"
path = "lib/implementation.txt"
template = "implementation.jinja"

[[files]]
id = "data_index"
path = "indexes/data.txt"
template = "data-index.jinja"
  [[files.checksums]]
  of = "source_data"
  algorithm = "sha1"
  as = "source_sha1"

[[files]]
id = "library_index"
path = "indexes/library.txt"
template = "library-index.jinja"
  [[files.checksums]]
  of = "data_index"
  algorithm = "sha1"
  as = "data_index_sha1"
  [[files.checksums]]
  of = "api"
  algorithm = "sha1"
  as = "api_sha1"
  [[files.checksums]]
  of = "implementation"
  algorithm = "sha1"
  as = "implementation_sha1"

[[files]]
id = "catalog"
path = "catalog.txt"
template = "catalog.jinja"
  [[files.checksums]]
  of = "data_index"
  algorithm = "sha1"
  as = "data_index_sha1"
  [[files.checksums]]
  of = "library_index"
  algorithm = "sha1"
  as = "library_index_sha1"
"#,
    );
    let order = topo_sort(&files).expect("DAG should topo-sort");
    let position: HashMap<&str, usize> = order
        .iter()
        .enumerate()
        .map(|(rank, &index)| (files[index].id.as_deref().unwrap(), rank))
        .collect();
    assert!(position["source_data"] < position["data_index"]);
    assert!(position["data_index"] < position["library_index"]);
    assert!(position["api"] < position["library_index"]);
    assert!(position["implementation"] < position["library_index"]);
    assert!(position["data_index"] < position["catalog"]);
    assert!(position["library_index"] < position["catalog"]);
    assert_eq!(position["catalog"], files.len() - 1);
}

/// A mutual A<->B cycle renders nothing (parse allows it — both ids exist,
/// no self edge — so the topo sort is the guard).
#[test]
fn topo_sort_rejects_a_cycle() {
    let files = manifest_files(
        r#"
version = 1
ir = "dae"
name = "cycle"
readiness_level = 2

[capabilities]

[[files]]
id = "a"
path = "a.xml"
template = "a.jinja"
  [[files.checksums]]
  of = "b"
  algorithm = "sha1"
  as = "b_sha1"

[[files]]
id = "b"
path = "b.xml"
template = "b.jinja"
  [[files.checksums]]
  of = "a"
  algorithm = "sha1"
  as = "a_sha1"
"#,
    );
    let err = topo_sort(&files).expect_err("a mutual cycle must render nothing");
    assert!(err.to_string().contains("cycle"), "{err}");
}

/// A self-hash edge is rejected at parse time (no file can embed its own
/// hash — the DAG-by-construction invariant).
#[test]
fn self_hash_edge_is_rejected_at_parse() {
    let err = parse_target_manifest(
        r#"
version = 1
ir = "dae"
name = "self"
readiness_level = 2

[capabilities]

[[files]]
id = "m"
path = "m.xml"
template = "m.jinja"
  [[files.checksums]]
  of = "m"
  algorithm = "sha1"
  as = "m_sha1"
"#,
    )
    .expect_err("a self-hash edge must be refused");
    assert!(err.to_string().contains("checksums itself"), "{err}");
}

/// A checksum `of` naming no declared id is rejected at parse time.
#[test]
fn dangling_checksum_of_is_rejected_at_parse() {
    let err = parse_target_manifest(
        r#"
version = 1
ir = "dae"
name = "dangling"
readiness_level = 2

[capabilities]

[[files]]
id = "c"
path = "c.xml"
template = "c.jinja"
  [[files.checksums]]
  of = "missing"
  algorithm = "sha1"
  as = "missing_sha1"
"#,
    )
    .expect_err("a dangling checksum `of` must be refused");
    assert!(err.to_string().contains("names no [[files]] id"), "{err}");
}

/// End-to-end: render in dependency order, inject each producer's real
/// SHA-1 downstream under its `as` key, and write the exact hashed bytes.
/// The consumer's rendered content carries the SHA-1 of the producer's
/// on-disk bytes — the no-placeholder guarantee, black-box.
#[test]
fn render_and_package_threads_real_producer_hashes() {
    let files = manifest_files(
        r#"
version = 1
ir = "dae"
name = "e2e"
readiness_level = 2

[capabilities]

[[assets]]
source = "fake-assets"
dest = "schemas/"

[[files]]
id = "leaf"
path = "leaf.txt"
template = "leaf-template"

[[files]]
id = "root"
path = "root.txt"
template = "root-template"
  [[files.checksums]]
  of = "leaf"
  algorithm = "sha1"
  as = "leaf_sha1"
"#,
    );
    // Fake renderer: `leaf-template` -> fixed content; `root-template` ->
    // text embedding the injected `leaf_sha1`; path templates -> the path.
    let render = |template: &str, artifact: &ArtifactRenderContext<'_>| -> Result<String> {
        Ok(match template {
            "leaf-template" => "LEAF-BODY".to_string(),
            "root-template" => format!(
                "root sees leaf={}",
                artifact
                    .checksums
                    .get("leaf_sha1")
                    .expect("leaf_sha1 injected")
            ),
            other => other.to_string(), // path templates render to themselves
        })
    };
    let asset_source = |source: &str| -> Result<Vec<TargetAssetFile>> {
        assert_eq!(source, "fake-assets");
        Ok(vec![TargetAssetFile {
            relative_path: "LICENSE".to_string(),
            bytes: b"license bytes".to_vec(),
        }])
    };
    let dir = tempfile::tempdir().expect("temp dir");
    let out_dir = dir.path().join("product");
    let package = PackageSpec {
        required_files: vec!["root.txt".to_string()],
        zip: None,
    };
    let manifest = parse_target_manifest(
        r#"
version = 1
ir = "dae"
name = "e2e"
readiness_level = 2

[capabilities]

[[assets]]
source = "fake-assets"
dest = "schemas/"

[[files]]
id = "leaf"
path = "leaf.txt"
template = "leaf-template"

[[files]]
id = "root"
path = "root.txt"
template = "root-template"
  [[files.checksums]]
  of = "leaf"
  algorithm = "sha1"
  as = "leaf_sha1"
"#,
    )
    .expect("manifest parses");

    render_and_package(
        &files,
        render,
        &manifest.assets,
        asset_source,
        &package,
        &out_dir,
    )
    .expect("declarative package build should succeed");

    let leaf_bytes = std::fs::read(out_dir.join("leaf.txt")).expect("leaf written");
    let expected = sha1_hex(&leaf_bytes);
    let root = std::fs::read_to_string(out_dir.join("root.txt")).expect("root written");
    assert_eq!(root, format!("root sees leaf={expected}"));
    let license = std::fs::read_to_string(out_dir.join("schemas/LICENSE")).expect("asset copied");
    assert_eq!(license, "license bytes");
}

#[test]
fn render_failure_preserves_previous_directory_and_archive() {
    let temp = tempfile::tempdir().expect("temp dir");
    let out_dir = temp.path().join("product");
    let archive = temp.path().join("product.zip");
    build_fixture(&out_dir, Some(&archive), "old marker", false, false)
        .expect("initial package succeeds");
    let before = output_snapshot(&out_dir, Some(&archive));

    let error = build_fixture(&out_dir, Some(&archive), "new marker", true, false)
        .expect_err("render failure must abort replacement");

    assert!(
        error.to_string().contains("Render target template"),
        "{error}"
    );
    assert_eq!(output_snapshot(&out_dir, Some(&archive)), before);
}

#[test]
fn asset_resolution_failure_preserves_previous_directory_and_archive() {
    let temp = tempfile::tempdir().expect("temp dir");
    let out_dir = temp.path().join("product");
    let archive = temp.path().join("product.zip");
    build_fixture(&out_dir, Some(&archive), "old marker", false, false)
        .expect("initial package succeeds");
    let before = output_snapshot(&out_dir, Some(&archive));

    let error = build_fixture(&out_dir, Some(&archive), "new marker", false, true)
        .expect_err("asset failure must abort replacement");

    assert!(
        error.to_string().contains("Resolve target asset source"),
        "{error}"
    );
    assert_eq!(output_snapshot(&out_dir, Some(&archive)), before);
}

#[test]
fn missing_required_marker_preserves_previous_directory_and_archive() {
    let temp = tempfile::tempdir().expect("temp dir");
    let out_dir = temp.path().join("product");
    let archive = temp.path().join("product.zip");
    build_fixture(&out_dir, Some(&archive), "old marker", false, false)
        .expect("initial package succeeds");
    let before = output_snapshot(&out_dir, Some(&archive));
    let manifest = fixture_manifest();
    let package = PackageSpec {
        required_files: vec!["missing.txt".to_owned()],
        zip: Some(ZipPackage {
            archive_path: archive.clone(),
        }),
    };

    let error = render_and_package(
        &manifest.files,
        |template, _| {
            Ok(if template == "marker-template" {
                "new marker".to_owned()
            } else {
                template.to_owned()
            })
        },
        &manifest.assets,
        |_| {
            Ok(vec![TargetAssetFile {
                relative_path: "payload.bin".to_owned(),
                bytes: b"new asset bytes".to_vec(),
            }])
        },
        &package,
        &out_dir,
    )
    .expect_err("missing required marker must abort replacement");

    assert!(error.to_string().contains("is not produced"), "{error}");
    assert_eq!(output_snapshot(&out_dir, Some(&archive)), before);
}

#[test]
fn invalid_roots_are_rejected_before_render_or_asset_resolution() {
    use std::cell::Cell;

    let temp = tempfile::tempdir().expect("temp dir");
    let prior_root = temp.path().join("prior");
    build_fixture(&prior_root, None, "old marker", false, false).expect("initial package succeeds");
    let before = output_snapshot(&prior_root, None);
    let manifest = fixture_manifest();
    let package = PackageSpec {
        required_files: vec!["marker.txt".to_owned()],
        zip: None,
    };
    for invalid in [PathBuf::new(), PathBuf::from("."), prior_root.join("..")] {
        let render_calls = Cell::new(0);
        let asset_calls = Cell::new(0);
        let error = render_and_package(
            &manifest.files,
            |template, _| {
                render_calls.set(render_calls.get() + 1);
                Ok(template.to_owned())
            },
            &manifest.assets,
            |_| {
                asset_calls.set(asset_calls.get() + 1);
                Ok(Vec::new())
            },
            &package,
            &invalid,
        )
        .expect_err("invalid root must fail");
        assert!(
            error.to_string().contains("Package root"),
            "{invalid:?}: {error}"
        );
        assert_eq!(render_calls.get(), 0);
        assert_eq!(asset_calls.get(), 0);
        assert_eq!(output_snapshot(&prior_root, None), before);
    }
}

#[test]
fn unrecognized_archive_preserves_previous_directory_and_archive_bytes() {
    let temp = tempfile::tempdir().expect("temp dir");
    let out_dir = temp.path().join("product");
    build_fixture(&out_dir, None, "old marker", false, false).expect("initial directory succeeds");
    let archive = temp.path().join("foreign.zip");
    fs::write(&archive, b"not a package archive").expect("foreign archive written");
    let before_root = output_snapshot(&out_dir, None);
    let before_archive = fs::read(&archive).expect("foreign archive readable");

    let error = build_fixture(&out_dir, Some(&archive), "new marker", false, false)
        .expect_err("foreign archive must not be replaced");

    assert!(
        error
            .to_string()
            .contains("not a recognized previous product"),
        "{error}"
    );
    assert_eq!(output_snapshot(&out_dir, None), before_root);
    assert_eq!(
        fs::read(&archive).expect("archive survives"),
        before_archive
    );
}

#[cfg(unix)]
#[test]
fn archive_staging_failure_preserves_previous_directory_and_archive() {
    use std::os::unix::fs::PermissionsExt as _;

    let temp = tempfile::tempdir().expect("temp dir");
    let product_parent = temp.path().join("products");
    let archive_parent = temp.path().join("archives");
    fs::create_dir_all(&product_parent).expect("product parent");
    fs::create_dir_all(&archive_parent).expect("archive parent");
    let out_dir = product_parent.join("product");
    let archive = archive_parent.join("product.zip");
    build_fixture(&out_dir, Some(&archive), "old marker", false, false)
        .expect("initial package succeeds");
    let before = output_snapshot(&out_dir, Some(&archive));
    let original_mode = fs::metadata(&archive_parent)
        .expect("archive parent metadata")
        .permissions()
        .mode();
    fs::set_permissions(&archive_parent, fs::Permissions::from_mode(0o555))
        .expect("make archive parent read-only");

    let result = build_fixture(&out_dir, Some(&archive), "new marker", false, false);

    fs::set_permissions(&archive_parent, fs::Permissions::from_mode(original_mode))
        .expect("restore archive parent permissions");
    let error = result.expect_err("archive staging must fail in a read-only parent");
    assert!(
        error
            .to_string()
            .contains("Create archive staging directory"),
        "{error}"
    );
    assert_eq!(output_snapshot(&out_dir, Some(&archive)), before);
}

#[test]
fn incomplete_rollback_retains_every_recovery_directory() {
    let parent = tempfile::tempdir().expect("parent temp dir");
    let staged_root = tempfile::Builder::new()
        .prefix("staged-root-")
        .tempdir_in(parent.path())
        .expect("staged root");
    fs::write(staged_root.path().join("new-marker"), b"new").expect("new product");
    let transaction = tempfile::Builder::new()
        .prefix("transaction-")
        .tempdir_in(parent.path())
        .expect("transaction");
    let previous_root = transaction.path().join("previous-product");
    fs::create_dir(&previous_root).expect("previous product");
    fs::write(previous_root.join("old-marker"), b"old").expect("old product");
    let archive_directory = tempfile::Builder::new()
        .prefix("archive-")
        .tempdir_in(parent.path())
        .expect("archive transaction");
    let staged_file = archive_directory.path().join("package.zip");
    let previous_archive = archive_directory.path().join("previous");
    fs::write(&staged_file, b"new archive").expect("new archive");
    fs::write(&previous_archive, b"old archive").expect("old archive");
    let staged_root_path = staged_root.path().to_owned();
    let transaction_path = transaction.path().to_owned();
    let archive_directory_path = archive_directory.path().to_owned();

    let error = abort_install(
        anyhow::anyhow!("injected publication failure"),
        vec![anyhow::anyhow!("injected rollback failure")],
        staged_root,
        transaction,
        Some(StagedArchive {
            directory: archive_directory,
            file: staged_file,
        }),
    );

    assert!(
        error.to_string().contains("recovery directories retained"),
        "{error:#}"
    );
    assert_eq!(
        fs::read(staged_root_path.join("new-marker")).unwrap(),
        b"new"
    );
    assert_eq!(
        fs::read(transaction_path.join("previous-product/old-marker")).unwrap(),
        b"old"
    );
    assert_eq!(
        fs::read(archive_directory_path.join("previous")).unwrap(),
        b"old archive"
    );
}

#[test]
fn deterministic_archive_contains_exact_staged_bytes() {
    use std::io::Read as _;

    let temp = tempfile::tempdir().expect("temp dir");
    let first_root = temp.path().join("first");
    let second_root = temp.path().join("second");
    let first_archive = temp.path().join("first.zip");
    let second_archive = temp.path().join("second.zip");
    build_fixture(
        &first_root,
        Some(&first_archive),
        "fixed marker",
        false,
        false,
    )
    .expect("first package succeeds");
    build_fixture(
        &second_root,
        Some(&second_archive),
        "fixed marker",
        false,
        false,
    )
    .expect("second package succeeds");

    assert_eq!(
        fs::read(&first_archive).expect("first archive"),
        fs::read(&second_archive).expect("second archive")
    );
    let file = fs::File::open(&first_archive).expect("archive opens");
    let mut archive = zip::ZipArchive::new(file).expect("archive parses");
    let mut marker = Vec::new();
    archive
        .by_name("marker.txt")
        .expect("marker entry")
        .read_to_end(&mut marker)
        .expect("marker bytes");
    assert_eq!(marker, fs::read(first_root.join("marker.txt")).unwrap());
    let mut asset = Vec::new();
    archive
        .by_name("assets/payload.bin")
        .expect("asset entry")
        .read_to_end(&mut asset)
        .expect("asset bytes");
    assert_eq!(
        asset,
        fs::read(first_root.join("assets/payload.bin")).unwrap()
    );
}
