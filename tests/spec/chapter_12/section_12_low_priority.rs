//! MLS §12: Low Priority Function Tests
//!
//! This module tests low priority normative requirements:
//! - §12.8: FORTRAN 77 external interface
//!
//! Reference: https://specification.modelica.org/master/functions.html

use crate::spec::expect_parse_success;

// ============================================================================
// §12.8 FORTRAN 77 EXTERNAL INTERFACE
// ============================================================================

/// MLS §12.8: FORTRAN 77 external function interface
mod fortran_interface {
    use super::*;

    /// MLS: "External FORTRAN 77 function declaration"
    #[test]
    fn mls_12_8_fortran_basic() {
        expect_parse_success(
            r#"
            function FortranFunc
                input Real x;
                output Real y;
            external "FORTRAN 77";
            end FortranFunc;
            "#,
        );
    }

    /// MLS: "FORTRAN 77 with explicit function name"
    #[test]
    fn mls_12_8_fortran_explicit_name() {
        expect_parse_success(
            r#"
            function MyFunc
                input Real x;
                output Real y;
            external "FORTRAN 77" y = fortran_func(x);
            end MyFunc;
            "#,
        );
    }

    /// MLS: "FORTRAN 77 with Library annotation"
    #[test]
    fn mls_12_8_fortran_library() {
        expect_parse_success(
            r#"
            function BLASFunc
                input Real x[3];
                output Real y;
            external "FORTRAN 77" y = ddot(3, x, 1, x, 1)
                annotation(Library = "blas");
            end BLASFunc;
            "#,
        );
    }

    /// MLS: "FORTRAN 77 array passing convention"
    #[test]
    fn mls_12_8_fortran_arrays() {
        expect_parse_success(
            r#"
            function FortranArray
                input Real A[:,:];
                input Integer m;
                input Integer n;
                output Real result;
            external "FORTRAN 77";
            end FortranArray;
            "#,
        );
    }

    /// MLS: "FORTRAN 77 string passing"
    #[test]
    fn mls_12_8_fortran_strings() {
        expect_parse_success(
            r#"
            function FortranString
                input String s;
                output Integer len;
            external "FORTRAN 77";
            end FortranString;
            "#,
        );
    }

    /// MLS: "FORTRAN 77 subroutine (no return value)"
    #[test]
    fn mls_12_8_fortran_subroutine() {
        expect_parse_success(
            r#"
            function FortranSub
                input Real x;
                output Real y;
            external "FORTRAN 77";
            end FortranSub;
            "#,
        );
    }

    /// MLS: "FORTRAN 77 with multiple outputs"
    #[test]
    fn mls_12_8_fortran_multi_output() {
        expect_parse_success(
            r#"
            function FortranMultiOut
                input Real x;
                output Real y1;
                output Real y2;
            external "FORTRAN 77";
            end FortranMultiOut;
            "#,
        );
    }
}
