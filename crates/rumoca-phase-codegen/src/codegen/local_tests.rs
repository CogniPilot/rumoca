//! Local unit tests for the codegen module, kept in a sibling file (like the
//! other `*_tests` modules) so `mod.rs` stays under the file-size limit.

use super::*;

#[test]
fn enum_type_names_use_top_level_parent_scope() {
    let mut dae = dae::Dae::default();
    dae.symbols.enum_literal_ordinals.insert(
        "Modelica.Blocks.Types.Smoothness.LinearSegments".to_string(),
        1,
    );
    dae.symbols
        .enum_literal_ordinals
        .insert("Pkg.Enum[index.with.dot].Choice".to_string(), 1);

    assert_eq!(
        enum_type_names_from_ordinals(&dae),
        vec![
            "Modelica.Blocks.Types.Smoothness".to_string(),
            "Pkg.Enum[index.with.dot]".to_string(),
        ]
    );
}

/// The manifest-template filter copies in this crate (`xs_double_str` /
/// `xml_escape_str`) must byte-for-byte match the canonical
/// `rumoca-galec-codegen` implementations the shipping `galec` render path
/// uses. These copies exist only to render the shared eFMI manifest
/// templates in tests, so a silent divergence would let those tests validate
/// against bytes the CLI never writes (SPEC_0034 contract §6 sanctions the
/// duplication; this keeps the two owners in lock-step).
#[test]
fn manifest_filter_copies_match_canonical_galec_codegen() {
    // Finite values only: non-finite Reals are rejected at model
    // construction and both implementations `debug_assert!` finiteness, so
    // they are out of contract for this filter.
    for value in [0.0, -0.0, 1.0, -1.0, 2.0, 0.5, 1e300, 1e-7, 1234.5678] {
        assert_eq!(
            xs_double_str(value),
            rumoca_galec_codegen::xs_double(value),
            "xs_double divergence for {value}"
        );
    }
    for text in ["", "plain", "a<b>&c\"'d", "<>&\"'", "no entities", "π & Ω"] {
        assert_eq!(
            xml_escape_str(text),
            rumoca_galec_codegen::xml_escape(text),
            "xml_escape divergence for {text:?}"
        );
    }
}
