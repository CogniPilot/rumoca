//! Chapter 12: Function Examples
//!
//! Complete function examples demonstrating various function features.
//!
//! Reference: https://specification.modelica.org/master/functions.html

use crate::spec::expect_parse_success;

// ============================================================================
// COMPLETE FUNCTION EXAMPLES
// ============================================================================

/// Complex mathematical function examples
mod mathematical_examples {
    use super::*;

    #[test]
    fn example_quadratic_roots() {
        expect_parse_success(
            r#"
            function QuadraticRoots
                "Compute roots of ax^2 + bx + c = 0"
                input Real a;
                input Real b;
                input Real c;
                output Real x1;
                output Real x2;
            protected
                Real discriminant;
            algorithm
                discriminant := b*b - 4*a*c;
                if discriminant >= 0 then
                    x1 := (-b + sqrt(discriminant)) / (2*a);
                    x2 := (-b - sqrt(discriminant)) / (2*a);
                else
                    x1 := 0;
                    x2 := 0;
                end if;
            end QuadraticRoots;
            "#,
        );
    }

    #[test]
    fn example_vector_norm() {
        expect_parse_success(
            r#"
            function VectorNorm
                "Compute Euclidean norm of a vector"
                input Real v[:];
                output Real norm;
            protected
                Real sumSquares;
            algorithm
                sumSquares := 0;
                for i in 1:size(v, 1) loop
                    sumSquares := sumSquares + v[i]^2;
                end for;
                norm := sqrt(sumSquares);
            end VectorNorm;
            "#,
        );
    }

    #[test]
    fn example_dot_product() {
        expect_parse_success(
            r#"
            function DotProduct
                "Compute dot product of two vectors"
                input Real a[:];
                input Real b[size(a, 1)];
                output Real result;
            algorithm
                result := 0;
                for i in 1:size(a, 1) loop
                    result := result + a[i] * b[i];
                end for;
            end DotProduct;
            "#,
        );
    }

    #[test]
    fn example_cross_product() {
        expect_parse_success(
            r#"
            function CrossProduct
                "Compute cross product of two 3D vectors"
                input Real a[3];
                input Real b[3];
                output Real c[3];
            algorithm
                c[1] := a[2]*b[3] - a[3]*b[2];
                c[2] := a[3]*b[1] - a[1]*b[3];
                c[3] := a[1]*b[2] - a[2]*b[1];
            end CrossProduct;
            "#,
        );
    }

    #[test]
    fn example_interpolate() {
        expect_parse_success(
            r#"
            function LinearInterpolate
                "Linear interpolation between two values"
                input Real x1;
                input Real y1;
                input Real x2;
                input Real y2;
                input Real x;
                output Real y;
            algorithm
                y := y1 + (y2 - y1) * (x - x1) / (x2 - x1);
            end LinearInterpolate;
            "#,
        );
    }

    #[test]
    fn example_clamp() {
        expect_parse_success(
            r#"
            function Clamp
                "Clamp a value to a range"
                input Real x;
                input Real minVal;
                input Real maxVal;
                output Real y;
            algorithm
                if x < minVal then
                    y := minVal;
                elseif x > maxVal then
                    y := maxVal;
                else
                    y := x;
                end if;
            end Clamp;
            "#,
        );
    }

    #[test]
    fn example_polynomial_eval() {
        expect_parse_success(
            r#"
            function EvalPolynomial
                "Evaluate polynomial using Horner's method"
                input Real coeffs[:];
                input Real x;
                output Real y;
            protected
                Integer n;
            algorithm
                n := size(coeffs, 1);
                y := coeffs[n];
                for i in n-1:-1:1 loop
                    y := y * x + coeffs[i];
                end for;
            end EvalPolynomial;
            "#,
        );
    }
}

/// Matrix and linear algebra examples
mod linear_algebra_examples {
    use super::*;

    #[test]
    fn example_matrix_trace() {
        expect_parse_success(
            r#"
            function MatrixTrace
                input Real A[:,:];
                output Real trace;
            protected
                Integer n;
            algorithm
                n := min(size(A, 1), size(A, 2));
                trace := 0;
                for i in 1:n loop
                    trace := trace + A[i, i];
                end for;
            end MatrixTrace;
            "#,
        );
    }

    #[test]
    fn example_frobenius_norm() {
        expect_parse_success(
            r#"
            function FrobeniusNorm
                "Compute Frobenius norm of a matrix"
                input Real A[:,:];
                output Real norm;
            protected
                Real sumSq;
            algorithm
                sumSq := 0;
                for i in 1:size(A, 1) loop
                    for j in 1:size(A, 2) loop
                        sumSq := sumSq + A[i, j]^2;
                    end for;
                end for;
                norm := sqrt(sumSq);
            end FrobeniusNorm;
            "#,
        );
    }

    #[test]
    fn example_matrix_is_symmetric() {
        expect_parse_success(
            r#"
            function IsSymmetric
                "Check if matrix is symmetric"
                input Real A[:,:];
                input Real tol = 1e-10;
                output Boolean symmetric;
            algorithm
                symmetric := true;
                if size(A, 1) <> size(A, 2) then
                    symmetric := false;
                    return;
                end if;
                for i in 1:size(A, 1) loop
                    for j in i+1:size(A, 2) loop
                        if abs(A[i, j] - A[j, i]) > tol then
                            symmetric := false;
                            return;
                        end if;
                    end for;
                end for;
            end IsSymmetric;
            "#,
        );
    }
}

/// Utility function examples
mod utility_examples {
    use super::*;

    #[test]
    fn example_find_max_index() {
        expect_parse_success(
            r#"
            function FindMaxIndex
                "Find index of maximum element"
                input Real x[:];
                output Integer idx;
            protected
                Real maxVal;
            algorithm
                maxVal := x[1];
                idx := 1;
                for i in 2:size(x, 1) loop
                    if x[i] > maxVal then
                        maxVal := x[i];
                        idx := i;
                    end if;
                end for;
            end FindMaxIndex;
            "#,
        );
    }

    #[test]
    fn example_count_positive() {
        expect_parse_success(
            r#"
            function CountPositive
                "Count positive elements in array"
                input Real x[:];
                output Integer count;
            algorithm
                count := 0;
                for i in 1:size(x, 1) loop
                    if x[i] > 0 then
                        count := count + 1;
                    end if;
                end for;
            end CountPositive;
            "#,
        );
    }

    #[test]
    fn example_all_positive() {
        expect_parse_success(
            r#"
            function AllPositive
                "Check if all elements are positive"
                input Real x[:];
                output Boolean result;
            algorithm
                result := true;
                for i in 1:size(x, 1) loop
                    if x[i] <= 0 then
                        result := false;
                        return;
                    end if;
                end for;
            end AllPositive;
            "#,
        );
    }

    #[test]
    fn example_weighted_average() {
        expect_parse_success(
            r#"
            function WeightedAverage
                input Real values[:];
                input Real weights[size(values, 1)];
                output Real avg;
            protected
                Real sumWeights;
                Real sumWeightedValues;
            algorithm
                sumWeights := sum(weights);
                sumWeightedValues := 0;
                for i in 1:size(values, 1) loop
                    sumWeightedValues := sumWeightedValues + values[i] * weights[i];
                end for;
                avg := sumWeightedValues / sumWeights;
            end WeightedAverage;
            "#,
        );
    }
}

/// Coordinate transformation examples
mod coordinate_examples {
    use super::*;

    #[test]
    fn example_cartesian_to_polar() {
        expect_parse_success(
            r#"
            function CartesianToPolar
                input Real x;
                input Real y;
                output Real r;
                output Real theta;
            algorithm
                r := sqrt(x*x + y*y);
                theta := atan2(y, x);
            end CartesianToPolar;
            "#,
        );
    }

    #[test]
    fn example_polar_to_cartesian() {
        expect_parse_success(
            r#"
            function PolarToCartesian
                input Real r;
                input Real theta;
                output Real x;
                output Real y;
            algorithm
                x := r * cos(theta);
                y := r * sin(theta);
            end PolarToCartesian;
            "#,
        );
    }

    #[test]
    fn example_spherical_to_cartesian() {
        expect_parse_success(
            r#"
            function SphericalToCartesian
                input Real r;
                input Real theta;
                input Real phi;
                output Real x;
                output Real y;
                output Real z;
            algorithm
                x := r * sin(phi) * cos(theta);
                y := r * sin(phi) * sin(theta);
                z := r * cos(phi);
            end SphericalToCartesian;
            "#,
        );
    }
}
