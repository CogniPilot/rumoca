"""Generate the CasADi 3.7.2 SX comparator for the exp_mixed operation."""

import argparse
import os
from pathlib import Path

import casadi as ca

EPS = 1e-2


def wedge(vector):
    matrix = ca.SX(3, 3)
    matrix[0, 0] = 0
    matrix[0, 1] = -vector[2]
    matrix[0, 2] = vector[1]
    matrix[1, 0] = vector[2]
    matrix[1, 1] = 0
    matrix[1, 2] = -vector[0]
    matrix[2, 0] = -vector[1]
    matrix[2, 1] = vector[0]
    matrix[2, 2] = 0
    return matrix


def coefficients_series(theta_sq):
    return (
        0.5 - theta_sq / 24.0,
        1.0 / 6.0 - theta_sq / 120.0,
        1.0 / 24.0 - theta_sq / 720.0,
    )


def coefficients_closed(theta_sq):
    theta = ca.sqrt(theta_sq)
    return (
        (1.0 - ca.cos(theta)) / theta_sq,
        (theta - ca.sin(theta)) / (theta_sq * theta),
        (theta_sq / 2.0 + ca.cos(theta) - 1.0) / (theta_sq * theta_sq),
    )


def coefficients(theta_sq):
    series = coefficients_series(theta_sq)
    closed = coefficients_closed(theta_sq)
    condition = theta_sq < EPS
    return tuple(ca.if_else(condition, left, right) for left, right in zip(series, closed))


def exp_map(vector):
    theta_sq = sum(vector[index] ** 2 for index in range(3))
    series_b = 1.0 - theta_sq / 8.0
    series_a = 0.5 - theta_sq / 48.0
    half = ca.sqrt(theta_sq) / 2.0
    closed_b = ca.cos(half)
    closed_a = ca.sin(half) / ca.sqrt(theta_sq)
    condition = theta_sq < 1e-8
    scale = ca.if_else(condition, series_a, closed_a)
    scalar = ca.if_else(condition, series_b, closed_b)
    return ca.vertcat(scalar, scale * vector[0], scale * vector[1], scale * vector[2])


def quaternion_product(left, right):
    return ca.vertcat(
        left[0] * right[0] - left[1] * right[1] - left[2] * right[2] - left[3] * right[3],
        left[1] * right[0] + left[0] * right[1] - left[3] * right[2] + left[2] * right[3],
        left[2] * right[0] + left[3] * right[1] + left[0] * right[2] - left[1] * right[3],
        left[3] * right[0] - left[2] * right[1] + left[1] * right[2] + left[0] * right[3],
    )


def to_dcm(quaternion):
    a, b, c, d = quaternion[0], quaternion[1], quaternion[2], quaternion[3]
    aa, ab, ac, ad = a * a, a * b, a * c, a * d
    bb, bc, bd = b * b, b * c, b * d
    cc, cd, dd = c * c, c * d, d * d
    matrix = ca.SX(3, 3)
    matrix[0, 0] = aa + bb - cc - dd
    matrix[0, 1] = 2 * (bc - ad)
    matrix[0, 2] = 2 * (bd + ac)
    matrix[1, 0] = 2 * (bc + ad)
    matrix[1, 1] = aa - bb + cc - dd
    matrix[1, 2] = 2 * (cd - ab)
    matrix[2, 0] = 2 * (bd - ac)
    matrix[2, 1] = 2 * (cd + ab)
    matrix[2, 2] = aa - bb - cc + dd
    return matrix


def build():
    state = ca.SX.sym("X0", 10)
    left = ca.SX.sym("l", 9)
    right = ca.SX.sym("r", 9)
    coupling = ca.SX.sym("B", 2, 2)
    omega_left = left[6:9]
    omega_right = right[6:9]
    identity = ca.SX.eye(2)

    theta_left = sum(omega_left[index] ** 2 for index in range(3))
    c1, c2, c3 = coefficients(theta_left)
    wedge_left = wedge(omega_left)
    wedge_left_sq = ca.mtimes(wedge_left, wedge_left)
    algebra_left = ca.horzcat(left[3:6], left[0:3])
    algebra_right = ca.horzcat(right[3:6], right[0:3])
    propagated_left = (
        algebra_left
        + 0.5 * ca.mtimes(algebra_left, coupling)
        + ca.mtimes(ca.mtimes(wedge_left, algebra_left), c1 * identity + c2 * coupling)
        + ca.mtimes(ca.mtimes(wedge_left_sq, algebra_left), c2 * identity + c3 * coupling)
    )

    theta_right = sum(omega_right[index] ** 2 for index in range(3))
    d1, d2, d3 = coefficients(theta_right)
    wedge_right = wedge(omega_right)
    wedge_right_sq = ca.mtimes(wedge_right, wedge_right)
    propagated_right = (
        algebra_right
        - 0.5 * ca.mtimes(algebra_right, coupling)
        + ca.mtimes(ca.mtimes(wedge_right, algebra_right), d1 * identity - d2 * coupling)
        + ca.mtimes(ca.mtimes(wedge_right_sq, algebra_right), d2 * identity - d3 * coupling)
    )

    quaternion_left = exp_map(omega_left)
    quaternion_right = exp_map(omega_right)
    right_state = quaternion_product(quaternion_right, state[6:10])
    output_quaternion = quaternion_product(right_state, quaternion_left)
    initial_matrix = ca.horzcat(state[3:6], state[0:3])
    output_matrix = (
        ca.mtimes(to_dcm(right_state), propagated_left)
        + ca.mtimes(ca.mtimes(to_dcm(quaternion_right), initial_matrix) + propagated_right, identity + coupling)
    )
    output = ca.vertcat(
        output_matrix[0, 1], output_matrix[1, 1], output_matrix[2, 1],
        output_matrix[0, 0], output_matrix[1, 0], output_matrix[2, 0],
        output_quaternion,
    )
    return state, left, right, coupling, output


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--out", required=True, type=Path)
    parser.add_argument("--expected-version", required=True)
    args = parser.parse_args()
    if ca.__version__ != args.expected_version:
        raise SystemExit(f"CasADi version {ca.__version__} != required {args.expected_version}")
    args.out.mkdir(parents=True, exist_ok=True)
    os.chdir(args.out)
    state, left, right, coupling, output = build()
    function = ca.Function(
        "exp_mixed_full",
        [state, left, right, coupling],
        [output],
        ["X0", "l", "r", "B"],
        ["X1"],
        {"cse": True},
    )
    generator = ca.CodeGenerator(
        "casadi_exp_mixed", {"casadi_real": "float", "casadi_int": "int", "with_header": True}
    )
    generator.add(function)
    generator.generate()


if __name__ == "__main__":
    main()
