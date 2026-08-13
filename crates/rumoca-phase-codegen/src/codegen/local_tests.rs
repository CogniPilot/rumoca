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
