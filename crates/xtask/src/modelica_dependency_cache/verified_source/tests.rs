use super::*;
use std::io::Write;

fn fixture_archive(root: &Path) -> LibrarySpec {
    let mut writer = zip::ZipWriter::new(Cursor::new(Vec::new()));
    let options = zip::write::SimpleFileOptions::default();
    for (path, text) in [
        (
            "models-sha/Vehicles/package.mo",
            "package Vehicles end Vehicles;",
        ),
        (
            "models-sha/Vehicles/Plant.mo",
            "model Plant parameter Real z=0.1; end Plant;",
        ),
    ] {
        writer.start_file(path, options).expect("archive file");
        writer.write_all(text.as_bytes()).expect("archive bytes");
    }
    let bytes = writer.finish().expect("archive").into_inner();
    let spec = LibrarySpec {
        display_name: "Pinned flight fixture".to_string(),
        version: "immutable-revision".to_string(),
        url: "https://example.invalid/unneeded.zip".to_string(),
        sha256: format!("{:x}", Sha256::digest(&bytes)),
        cache_dir: PathBuf::from("target/verification/flight-fixture"),
        zip_layout: ZipLayout::AutoStripCommonRoot,
        required_files: vec![PathBuf::from("Vehicles/package.mo")],
        write_revision_marker: false,
    };
    let archive = library_dir(root, &spec).with_extension("zip");
    fs::create_dir_all(archive.parent().expect("archive parent")).expect("parent");
    fs::write(archive, bytes).expect("verified archive fixture");
    spec
}

#[test]
fn default_source_is_provisioned_from_the_verified_archive() {
    let temp = tempfile::tempdir().expect("tempdir");
    let spec = fixture_archive(temp.path());
    let source = qualify_source(temp.path(), &spec, None).expect("default source");
    assert_eq!(source.root(), library_dir(temp.path(), &spec));
    assert_eq!(source.revision(), spec.version);
    assert_eq!(source.archive_sha256(), spec.sha256);
    assert_eq!(
        fs::read_to_string(source.root().join("Vehicles/Plant.mo")).expect("plant"),
        "model Plant parameter Real z=0.1; end Plant;"
    );
    qualify_source(temp.path(), &spec, None).expect("verified cache reuse");
}

#[test]
fn wrong_explicit_revision_is_not_replaced_by_the_default_source() {
    let temp = tempfile::tempdir().expect("tempdir");
    let spec = fixture_archive(temp.path());
    let canonical = qualify_source(temp.path(), &spec, None).expect("canonical source");
    let supplied = temp.path().join("a41-compatible-packages");
    fs::create_dir_all(supplied.join("Vehicles")).expect("supplied root");
    fs::copy(
        canonical.root().join("Vehicles/package.mo"),
        supplied.join("Vehicles/package.mo"),
    )
    .expect("same package entry");
    fs::write(
        supplied.join("Vehicles/Plant.mo"),
        "model Plant parameter Real z=0; end Plant;",
    )
    .expect("wrong revision");
    fs::write(supplied.join(REVISION_FILE), revision_marker(&spec)).expect("forged marker");
    let error = qualify_source(temp.path(), &spec, Some(&supplied)).expect_err("wrong input");
    assert!(format!("{error:#}").contains("pinned source differs"));
    qualify_source(temp.path(), &spec, None).expect("canonical source remains intact");
}

#[test]
fn dirty_cached_source_is_refused_instead_of_silently_repaired() {
    let temp = tempfile::tempdir().expect("tempdir");
    let spec = fixture_archive(temp.path());
    let source = qualify_source(temp.path(), &spec, None).expect("default source");
    let path = source.root().join("Vehicles/Plant.mo");
    fs::write(&path, "edited").expect("edit");
    assert!(qualify_source(temp.path(), &spec, None).is_err());
    assert_eq!(fs::read_to_string(path).expect("retained edit"), "edited");
}

#[test]
fn an_extra_source_file_cannot_extend_the_pinned_input() {
    let temp = tempfile::tempdir().expect("tempdir");
    let spec = fixture_archive(temp.path());
    let source = qualify_source(temp.path(), &spec, None).expect("default source");
    fs::write(
        source.root().join("Vehicles/Extra.mo"),
        "model Extra end Extra;",
    )
    .expect("extra");
    let error = qualify_source(temp.path(), &spec, Some(source.root())).expect_err("extra source");
    assert!(format!("{error:#}").contains("unexpected pinned source file"));
}

#[test]
fn forged_archive_bytes_are_rejected_before_source_provisioning() {
    let temp = tempfile::tempdir().expect("tempdir");
    let spec = fixture_archive(temp.path());
    fs::write(
        library_dir(temp.path(), &spec).with_extension("zip"),
        "forged",
    )
    .expect("archive edit");
    let error = qualify_source(temp.path(), &spec, None).expect_err("checksum");
    assert!(format!("{error:#}").contains("SHA-256 mismatch"));
    assert!(!library_dir(temp.path(), &spec).exists());
}

#[test]
fn git_metadata_does_not_change_verified_source_bytes() {
    let temp = tempfile::tempdir().expect("tempdir");
    let spec = fixture_archive(temp.path());
    let source = qualify_source(temp.path(), &spec, None).expect("default source");
    fs::create_dir(source.root().join(".git")).expect("git metadata");
    fs::write(source.root().join(".git/HEAD"), "ref: refs/heads/main").expect("HEAD");
    qualify_source(temp.path(), &spec, Some(source.root())).expect("byte-identical checkout");
}

#[cfg(unix)]
#[test]
fn a_source_symlink_cannot_redirect_the_verified_inventory() {
    let temp = tempfile::tempdir().expect("tempdir");
    let spec = fixture_archive(temp.path());
    let source = qualify_source(temp.path(), &spec, None).expect("default source");
    let original = source.root().join("Vehicles/Plant.mo");
    let outside = temp.path().join("Plant.mo");
    fs::rename(&original, &outside).expect("move source");
    std::os::unix::fs::symlink(outside, original).expect("source symlink");
    let error =
        qualify_source(temp.path(), &spec, Some(source.root())).expect_err("source symlink");
    assert!(format!("{error:#}").contains("pinned source must be regular"));
}

#[test]
fn numerical_flight_pin_is_distinct_from_example_and_compatibility_inputs() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let manifest = load_manifest_file(&root.join(CORPUS_MANIFEST)).expect("corpus manifest");
    let flight = required_library(&manifest, "flight_models").expect("flight spec");
    assert_eq!(flight.version, "3d6204e7a907e452e08c46cd63352d6365089a38");
    assert_eq!(
        flight.sha256,
        "9475fdc2e27df233a1c8b8debddbbc56765c9559c69938a1583df2210c3aec6f"
    );
    assert!(flight.url.ends_with(&format!("/{}.zip", flight.version)));
    assert!(
        flight
            .cache_dir
            .starts_with("target/verification/corpus-dependencies")
    );
    let examples = load_manifest(&root).expect("examples manifest");
    assert_ne!(
        flight.cache_dir,
        required_library(&examples, CMM_KEY).expect("CMM").cache_dir
    );
    assert_ne!(flight.cache_dir, Path::new("target/modelica-models"));
}
