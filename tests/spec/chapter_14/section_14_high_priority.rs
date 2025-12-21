//! MLS Chapter 14: High-Priority Edge Case Tests
//!
//! This module contains critical tests for operator record edge cases
//! and advanced scenarios in operator overloading.
//!
//! Reference: https://specification.modelica.org/master/overloaded-operators.html

use crate::spec::expect_parse_success;

// ============================================================================
// CRITICAL: COMPLEX NUMBER PATTERNS
// ============================================================================

/// Critical complex number patterns
mod complex_critical {
    use super::*;

    /// Critical: Full complex number type
    #[test]
    fn critical_complex_full() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;

                encapsulated operator 'constructor'
                    function fromReal
                        input Real re;
                        input Real im = 0;
                        output Complex result;
                    algorithm
                        result.re := re;
                        result.im := im;
                    end fromReal;
                end 'constructor';

                encapsulated operator '0'
                    function zero
                        output Complex result;
                    algorithm
                        result.re := 0;
                        result.im := 0;
                    end zero;
                end '0';

                encapsulated operator '+'
                    function add
                        input Complex c1;
                        input Complex c2;
                        output Complex result;
                    algorithm
                        result.re := c1.re + c2.re;
                        result.im := c1.im + c2.im;
                    end add;
                end '+';

                encapsulated operator '-'
                    function negate
                        input Complex c;
                        output Complex result;
                    algorithm
                        result.re := -c.re;
                        result.im := -c.im;
                    end negate;

                    function subtract
                        input Complex c1;
                        input Complex c2;
                        output Complex result;
                    algorithm
                        result.re := c1.re - c2.re;
                        result.im := c1.im - c2.im;
                    end subtract;
                end '-';

                encapsulated operator '*'
                    function multiply
                        input Complex c1;
                        input Complex c2;
                        output Complex result;
                    algorithm
                        result.re := c1.re * c2.re - c1.im * c2.im;
                        result.im := c1.re * c2.im + c1.im * c2.re;
                    end multiply;

                    function scaleReal
                        input Real r;
                        input Complex c;
                        output Complex result;
                    algorithm
                        result.re := r * c.re;
                        result.im := r * c.im;
                    end scaleReal;
                end '*';

                encapsulated operator '=='
                    function equal
                        input Complex c1;
                        input Complex c2;
                        output Boolean result;
                    algorithm
                        result := c1.re == c2.re and c1.im == c2.im;
                    end equal;
                end '==';
            end Complex;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: VECTOR/MATRIX PATTERNS
// ============================================================================

/// Critical vector/matrix patterns
mod vector_critical {
    use super::*;

    /// Critical: 3D Vector type
    #[test]
    fn critical_vector3() {
        expect_parse_success(
            r#"
            operator record Vector3
                Real x;
                Real y;
                Real z;

                encapsulated operator 'constructor'
                    function fromComponents
                        input Real x;
                        input Real y;
                        input Real z;
                        output Vector3 result;
                    algorithm
                        result.x := x;
                        result.y := y;
                        result.z := z;
                    end fromComponents;
                end 'constructor';

                encapsulated operator '0'
                    function zero
                        output Vector3 result;
                    algorithm
                        result.x := 0;
                        result.y := 0;
                        result.z := 0;
                    end zero;
                end '0';

                encapsulated operator '+'
                    function add
                        input Vector3 a;
                        input Vector3 b;
                        output Vector3 result;
                    algorithm
                        result.x := a.x + b.x;
                        result.y := a.y + b.y;
                        result.z := a.z + b.z;
                    end add;
                end '+';

                encapsulated operator '-'
                    function negate
                        input Vector3 v;
                        output Vector3 result;
                    algorithm
                        result.x := -v.x;
                        result.y := -v.y;
                        result.z := -v.z;
                    end negate;

                    function subtract
                        input Vector3 a;
                        input Vector3 b;
                        output Vector3 result;
                    algorithm
                        result.x := a.x - b.x;
                        result.y := a.y - b.y;
                        result.z := a.z - b.z;
                    end subtract;
                end '-';

                encapsulated operator '*'
                    function scale
                        input Real s;
                        input Vector3 v;
                        output Vector3 result;
                    algorithm
                        result.x := s * v.x;
                        result.y := s * v.y;
                        result.z := s * v.z;
                    end scale;

                    function dot
                        input Vector3 a;
                        input Vector3 b;
                        output Real result;
                    algorithm
                        result := a.x * b.x + a.y * b.y + a.z * b.z;
                    end dot;
                end '*';
            end Vector3;
            "#,
        );
    }

    /// Critical: 2x2 Matrix type
    #[test]
    fn critical_matrix2x2() {
        expect_parse_success(
            r#"
            operator record Matrix2
                Real m[2, 2];

                encapsulated operator 'constructor'
                    function fromRows
                        input Real r1[2];
                        input Real r2[2];
                        output Matrix2 result;
                    algorithm
                        result.m[1, :] := r1;
                        result.m[2, :] := r2;
                    end fromRows;

                    function identity
                        output Matrix2 result;
                    algorithm
                        result.m := {{1, 0}, {0, 1}};
                    end identity;
                end 'constructor';

                encapsulated operator '0'
                    function zero
                        output Matrix2 result;
                    algorithm
                        result.m := zeros(2, 2);
                    end zero;
                end '0';

                encapsulated operator '+'
                    function add
                        input Matrix2 a;
                        input Matrix2 b;
                        output Matrix2 result;
                    algorithm
                        result.m := a.m + b.m;
                    end add;
                end '+';

                encapsulated operator '*'
                    function multiply
                        input Matrix2 a;
                        input Matrix2 b;
                        output Matrix2 result;
                    algorithm
                        for i in 1:2 loop
                            for j in 1:2 loop
                                result.m[i, j] := 0;
                                for k in 1:2 loop
                                    result.m[i, j] := result.m[i, j] + a.m[i, k] * b.m[k, j];
                                end for;
                            end for;
                        end for;
                    end multiply;
                end '*';
            end Matrix2;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: PHYSICAL UNIT PATTERNS
// ============================================================================

/// Critical physical unit patterns
mod units_critical {
    use super::*;

    /// Critical: Length with unit
    #[test]
    fn critical_length_type() {
        expect_parse_success(
            r#"
            operator record Length
                Real value(unit = "m");

                encapsulated operator 'constructor'
                    function fromMeters
                        input Real meters;
                        output Length result;
                    algorithm
                        result.value := meters;
                    end fromMeters;
                end 'constructor';

                encapsulated operator '0'
                    function zero
                        output Length result;
                    algorithm
                        result.value := 0;
                    end zero;
                end '0';

                encapsulated operator '+'
                    function add
                        input Length a;
                        input Length b;
                        output Length result;
                    algorithm
                        result.value := a.value + b.value;
                    end add;
                end '+';

                encapsulated operator '-'
                    function subtract
                        input Length a;
                        input Length b;
                        output Length result;
                    algorithm
                        result.value := a.value - b.value;
                    end subtract;
                end '-';

                encapsulated operator '*'
                    function scale
                        input Real factor;
                        input Length l;
                        output Length result;
                    algorithm
                        result.value := factor * l.value;
                    end scale;
                end '*';

                encapsulated operator '<'
                    function lessThan
                        input Length a;
                        input Length b;
                        output Boolean result;
                    algorithm
                        result := a.value < b.value;
                    end lessThan;
                end '<';
            end Length;
            "#,
        );
    }

    /// Critical: Angle type
    #[test]
    fn critical_angle_type() {
        expect_parse_success(
            r#"
            operator record Angle
                Real rad(unit = "rad");

                encapsulated operator 'constructor'
                    function fromRadians
                        input Real radians;
                        output Angle result;
                    algorithm
                        result.rad := radians;
                    end fromRadians;

                    function fromDegrees
                        input Real degrees;
                        output Angle result;
                    algorithm
                        result.rad := degrees * 3.14159265358979 / 180;
                    end fromDegrees;
                end 'constructor';

                encapsulated operator '+'
                    function add
                        input Angle a;
                        input Angle b;
                        output Angle result;
                    algorithm
                        result.rad := a.rad + b.rad;
                    end add;
                end '+';

                encapsulated operator '-'
                    function negate
                        input Angle a;
                        output Angle result;
                    algorithm
                        result.rad := -a.rad;
                    end negate;

                    function subtract
                        input Angle a;
                        input Angle b;
                        output Angle result;
                    algorithm
                        result.rad := a.rad - b.rad;
                    end subtract;
                end '-';
            end Angle;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: MULTIPLE OVERLOADS
// ============================================================================

/// Critical multiple overload patterns
mod overload_critical {
    use super::*;

    /// Critical: Multiple function overloads in one operator
    #[test]
    fn critical_multiple_overloads() {
        expect_parse_success(
            r#"
            operator record Value
                Real x;

                encapsulated operator '+'
                    function addValues
                        input Value a;
                        input Value b;
                        output Value result;
                    algorithm
                        result.x := a.x + b.x;
                    end addValues;

                    function addReal
                        input Value v;
                        input Real r;
                        output Value result;
                    algorithm
                        result.x := v.x + r;
                    end addReal;

                    function addRealLeft
                        input Real r;
                        input Value v;
                        output Value result;
                    algorithm
                        result.x := r + v.x;
                    end addRealLeft;
                end '+';

                encapsulated operator '*'
                    function multiplyValues
                        input Value a;
                        input Value b;
                        output Value result;
                    algorithm
                        result.x := a.x * b.x;
                    end multiplyValues;

                    function scaleLeft
                        input Real s;
                        input Value v;
                        output Value result;
                    algorithm
                        result.x := s * v.x;
                    end scaleLeft;

                    function scaleRight
                        input Value v;
                        input Real s;
                        output Value result;
                    algorithm
                        result.x := v.x * s;
                    end scaleRight;
                end '*';
            end Value;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: OPERATOR RECORD IN PACKAGE
// ============================================================================

/// Critical package patterns
mod package_critical {
    use super::*;

    /// Critical: Operator record in package
    #[test]
    fn critical_in_package() {
        expect_parse_success(
            r#"
            package ComplexMath
                operator record Complex
                    Real re;
                    Real im;

                    encapsulated operator 'constructor'
                        function create
                            input Real re;
                            input Real im = 0;
                            output Complex result;
                        algorithm
                            result.re := re;
                            result.im := im;
                        end create;
                    end 'constructor';

                    encapsulated operator '+'
                        function add
                            input Complex a;
                            input Complex b;
                            output Complex result;
                        algorithm
                            result.re := a.re + b.re;
                            result.im := a.im + b.im;
                        end add;
                    end '+';
                end Complex;

                function magnitude
                    input Complex c;
                    output Real m;
                algorithm
                    m := sqrt(c.re^2 + c.im^2);
                end magnitude;

                function phase
                    input Complex c;
                    output Real p;
                algorithm
                    p := atan2(c.im, c.re);
                end phase;
            end ComplexMath;
            "#,
        );
    }
}
