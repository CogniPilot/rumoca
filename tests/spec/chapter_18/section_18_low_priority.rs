//! MLS §18: Low Priority Annotation Tests
//!
//! This module tests low priority normative requirements:
//! - §18.8: Vendor-specific annotations
//! - §18.1: Annotation restrictions
//!
//! Reference: https://specification.modelica.org/master/annotations.html

use crate::spec::{expect_failure, expect_parse_success};

// ============================================================================
// §18.8 VENDOR ANNOTATIONS
// ============================================================================

/// MLS §18.8: Vendor-specific annotation handling
mod vendor_annotations {
    use super::*;

    /// MLS: "Vendor annotation with __VendorName prefix"
    #[test]
    fn mls_18_8_vendor_basic() {
        expect_parse_success(
            r#"
            model Test
                Real x;
            equation
                x = 1;
            annotation(__Dymola_experimentSetupOutput);
            end Test;
            "#,
        );
    }

    /// MLS: "Vendor annotation with value"
    #[test]
    fn mls_18_8_vendor_with_value() {
        expect_parse_success(
            r#"
            model Test
                Real x;
            equation
                x = 1;
            annotation(__Dymola_Commands(file = "script.mos"));
            end Test;
            "#,
        );
    }

    /// MLS: "Multiple vendor annotations"
    #[test]
    fn mls_18_8_vendor_multiple() {
        expect_parse_success(
            r#"
            model Test
                Real x;
            equation
                x = 1;
            annotation(
                __Dymola_experimentSetupOutput,
                __OpenModelica_commandLineOptions = "-d=newInst",
                __Wolfram_mathLinkOptions(timeout = 30)
            );
            end Test;
            "#,
        );
    }

    /// MLS: "Vendor annotation with complex structure"
    #[test]
    fn mls_18_8_vendor_complex() {
        expect_parse_success(
            r#"
            model Test
                Real x;
            equation
                x = 1;
            annotation(
                __Dymola_selections(
                    selection1(name = "Test", match = "*.x")
                )
            );
            end Test;
            "#,
        );
    }

    /// MLS: "Vendor annotations must be preserved in output"
    #[test]
    fn mls_18_8_vendor_preservation() {
        expect_parse_success(
            r#"
            model Test
                Real x annotation(__MyTool_special = true);
            equation
                x = 1;
            end Test;
            "#,
        );
    }

    /// MLS: "Vendor annotation on component"
    #[test]
    fn mls_18_8_vendor_on_component() {
        expect_parse_success(
            r#"
            model Test
                Real x annotation(__Dymola_BoundaryCondition = true);
            equation
                x = 1;
            end Test;
            "#,
        );
    }

    /// MLS: "Vendor annotation on equation"
    #[test]
    fn mls_18_8_vendor_on_equation() {
        expect_parse_success(
            r#"
            model Test
                Real x;
            equation
                x = 1 annotation(__Dymola_label = "main equation");
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §18.1 ANNOTATION RESTRICTIONS
// ============================================================================

/// MLS §18.1: Annotation modifier restrictions
mod annotation_restrictions {
    use super::*;

    /// MLS: "No 'final' modifier in annotations"
    #[test]
    #[ignore = "Annotation final restriction not yet enforced"]
    fn mls_18_1_no_final_in_annotation() {
        expect_failure(
            r#"
            model Test
                Real x;
            equation
                x = 1;
            annotation(final Documentation(info = "test"));
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "No 'each' modifier in annotations"
    #[test]
    #[ignore = "Annotation each restriction not yet enforced"]
    fn mls_18_1_no_each_in_annotation() {
        expect_failure(
            r#"
            model Test
                Real x;
            equation
                x = 1;
            annotation(each Placement(visible = true));
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "No 'redeclare' modifier in annotations"
    #[test]
    fn mls_18_1_no_redeclare_in_annotation() {
        expect_failure(
            r#"
            model Test
                Real x;
            equation
                x = 1;
            annotation(redeclare Icon(coordinateSystem(extent = {{-100,-100},{100,100}})));
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Normal annotation without restricted modifiers
    #[test]
    fn mls_18_1_valid_annotation() {
        expect_parse_success(
            r#"
            model Test
                Real x;
            equation
                x = 1;
            annotation(
                Documentation(info = "<html>Test model</html>"),
                Icon(coordinateSystem(extent = {{-100,-100},{100,100}}))
            );
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §18.7 TEST CASE ANNOTATION
// ============================================================================

/// MLS §18.7: TestCase annotation
mod test_case_annotation {
    use super::*;

    /// MLS: "TestCase annotation marks model for testing"
    #[test]
    fn mls_18_7_test_case_basic() {
        expect_parse_success(
            r#"
            model Test
                Real x(start = 0, fixed = true);
            equation
                der(x) = 1;
            annotation(__Dymola_TestCase(expect = "success"));
            end Test;
            "#,
        );
    }

    /// MLS: "TestCase with expected result"
    #[test]
    fn mls_18_7_test_case_expected() {
        expect_parse_success(
            r#"
            model Test
                Real x(start = 0, fixed = true);
            equation
                der(x) = 1;
            annotation(
                experiment(StopTime = 1),
                __Dymola_TestCase(
                    expect = "success",
                    result = {x(finalValue = 1)}
                )
            );
            end Test;
            "#,
        );
    }
}
