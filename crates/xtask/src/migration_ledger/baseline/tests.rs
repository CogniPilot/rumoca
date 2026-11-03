use super::*;

const PATH: &str = "crates/rumoca-phase-codegen/src/templates/fmi-ls-wasm/model.c.jinja";
// A tiny baseline blob, independently checked by the immutable ledger. This
// literal is test data, not a copied renderer or an alternative source of truth.
const BYTES: &[u8] = b"{% include \"fmi3-model.c.jinja\" %}\n";

#[test]
fn exact_old_bytes_remain_borrowed_and_bound_to_one_owner() {
    let baseline = FrozenBaseline::checked().unwrap();
    let input = BYTES.to_vec();
    let source = baseline.source(PATH, &input).unwrap();
    assert_eq!(source.path(), PATH);
    assert!(std::ptr::eq(source.bytes(), input.as_slice()));
    source.check_owner(&baseline).unwrap();
    let other = FrozenBaseline::checked().unwrap();
    assert!(source.check_owner(&other).is_err());
}

#[test]
fn replacement_bytes_cannot_become_the_baseline() {
    let baseline = FrozenBaseline::checked().unwrap();
    for bytes in [
        b"".as_slice(),
        b"changed",
        b"{% include \"fmi3-model.c.jinja\" %}\r\n",
    ] {
        assert!(baseline.source(PATH, bytes).is_err());
    }
    assert!(
        baseline
            .source(PATH, &vec![0; 16 * 1024 * 1024 + 1])
            .is_err()
    );
}

#[test]
fn path_substitution_and_unknown_files_cannot_grant_a_baseline() {
    let baseline = FrozenBaseline::checked().unwrap();
    for path in [
        "",
        "../escape",
        "/absolute",
        "./x",
        "crates/unknown.rs",
        "crates/xtask/Cargo.toml",
    ] {
        assert!(baseline.source(path, BYTES).is_err(), "{path}");
    }
    let different = "crates/rumoca-phase-codegen/src/templates/fmi-ls-wasm/rmc_rows.c.jinja";
    assert!(baseline.source(different, BYTES).is_err());
}
