//! eFMI packaging data model + product-agnostic context views (SPEC_0034 D3
//! amended; contract §6 relocation).
//!
//! Formerly the standalone `rumoca-efmi` crate; dissolved into this projection
//! crate so the id/reference graph and the data-integrity validators live
//! beside the projection that produces them, and the `rumoca` build step imports
//! the [`checksum::Sha1Hex`] primitive and the id newtypes downward through the
//! `rumoca-compile` facade. This module owns only *validated typed data*: the
//! `__content.xml` model ([`content`]), the shared manifest attribute group
//! ([`manifest_common`]), the Algorithm Code manifest
//! ([`algorithm_code_manifest`]), the Production Code manifest
//! ([`production_code_manifest`], including the cross-manifest validator
//! [`production_code_manifest::validate_against_algorithm_code`]), SHA-1
//! checksums over exact raw bytes ([`checksum`]), UUID/timestamp/id discipline
//! ([`ids`]), the one typed error enum ([`diagnostic`]), and the
//! `#[derive(Serialize)]` context [`views`] the eFMI packaging templates
//! consume.
//!
//! There is **no XML text** here anymore (the typed quick-xml serializers and
//! the container-write re-parse were deleted): element/attribute names, fixed
//! `efmiVersion`/`xsdVersion`/`kind` literals, escaping, and wrapper
//! cardinality all live in the minijinja templates. Construction validates
//! eagerly (SPEC_0008: fail early, no silent defaults), so the views reflect
//! exactly what those validated models carry (GAL-021: no placeholder
//! checksums, ever). See `spec/SPEC_0034_GALEC_EFMI_EXPORT.md`.

pub mod algorithm_code_manifest;
pub mod checksum;
pub mod content;
pub mod diagnostic;
pub mod ids;
pub mod manifest_common;
pub mod production_code_manifest;
pub mod views;

#[cfg(test)]
mod tests;

/// eFMI profile string for the pinned Beta-1 schema set (GAL-022).
pub const EFMI_PROFILE: &str = "efmi-1.0.0-beta-1";

pub use checksum::Sha1Hex;
pub use diagnostic::EfmiError;
pub use ids::{
    FilePath, IdRegistry, Identifier, ManifestId, NameWithoutSlashes, NormalizedText, UtcTimestamp,
};
pub use views::{
    AcManifestCtx, ContentCtx, EfmiManifestContext, ModelExport, PcManifestCtx, UnitCtx,
    xml_escape, xs_double,
};

#[cfg(test)]
mod format_real_tests {
    use super::views::xs_double;

    /// Every rendering must be a valid `xs:double` lexical form: an optional
    /// sign, decimal digits, and exactly one `.` — never exponent notation,
    /// `inf`, or `NaN` (formerly `xml::format_real_has_explicit_decimal_point`).
    fn assert_xs_double_lexical(rendered: &str) {
        let digits = rendered.strip_prefix('-').unwrap_or(rendered);
        assert_eq!(
            digits.matches('.').count(),
            1,
            "`{rendered}` must contain exactly one decimal point"
        );
        assert!(
            digits.chars().all(|c| c.is_ascii_digit() || c == '.'),
            "`{rendered}` must contain only digits and a decimal point"
        );
    }

    #[test]
    fn xs_double_has_explicit_decimal_point() {
        assert_eq!(xs_double(2.0), "2.0");
        assert_eq!(xs_double(0.1), "0.1");
        assert_eq!(xs_double(-10.0), "-10.0");
        // Extreme magnitudes: Rust's f64 Display never emits exponent
        // notation, so the rendering must stay a plain decimal literal. This
        // catches any future switch to an exponent-producing formatter.
        for value in [1.0e300, -1.0e300, 5.0e-324, f64::MAX, f64::MIN_POSITIVE] {
            assert_xs_double_lexical(&xs_double(value));
        }
        assert!(xs_double(1.0e300).ends_with(".0"));
    }
}
