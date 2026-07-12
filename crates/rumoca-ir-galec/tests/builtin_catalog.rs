//! §3.2.6 builtin catalog integrity: exact name set, signaling set (trap
//! T14), lifting rules, and the reserved-name collision surface (GAL-015).

use rumoca_ir_galec::ast::PredefinedSignal;
use rumoca_ir_galec::builtins::{
    BUILTINS, find_builtin, find_lifted_base, is_appendix_c_reserved, is_builtin_name,
    is_reserved_name,
};

/// The complete mandatory §3.2.6 base catalog, alphabetized.
const EXPECTED_NAMES: &[&str] = &[
    "absolute",
    "acos",
    "asin",
    "atan",
    "atan2",
    "cos",
    "cosh",
    "divisionTowardsZero",
    "epsReal",
    "euler",
    "exp",
    "fractional",
    "hasNaN1D",
    "hasNaN2D",
    "imax",
    "imin",
    "integer",
    "interpolation1D",
    "interpolation2D",
    "interpolation3D",
    "isFinite",
    "isInfinite",
    "isNaN",
    "lg",
    "ln",
    "luFactorize",
    "luSolve",
    "max",
    "maxInteger",
    "maxReal",
    "min",
    "minInteger",
    "minReal",
    "minusInfinite",
    "nan",
    "pi",
    "plusInfinite",
    "posMinReal",
    "real",
    "realRemainderTowardsZero",
    "remainderTowardsZero",
    "roundDown",
    "roundHalfToEven",
    "roundUp",
    "safe_acos",
    "safe_asin",
    "safe_lg",
    "safe_ln",
    "safe_posdiv",
    "safe_sqrt",
    "safe_tan",
    "sign",
    "sin",
    "sinh",
    "solveLinearEquations",
    "sqrt",
    "tan",
    "tanh",
];

#[test]
fn base_catalog_matches_normative_name_set() {
    let mut names: Vec<&str> = BUILTINS.iter().map(|b| b.name).collect();
    names.sort_unstable();
    assert_eq!(names, EXPECTED_NAMES);
}

#[test]
fn only_four_builtins_signal() {
    let signaling: Vec<&str> = BUILTINS
        .iter()
        .filter(|b| !b.signals.is_empty())
        .map(|b| b.name)
        .collect();
    assert_eq!(
        signaling,
        ["integer", "solveLinearEquations", "luFactorize", "luSolve"]
    );
    let integer = find_builtin("integer").expect("integer is a builtin");
    assert_eq!(
        integer.signals,
        [PredefinedSignal::Nan, PredefinedSignal::Overflow]
    );
    for solver in ["solveLinearEquations", "luFactorize", "luSolve"] {
        let builtin = find_builtin(solver).expect("solver is a builtin");
        assert_eq!(
            builtin.signals,
            [PredefinedSignal::SolveLinearEquationsFailed]
        );
    }
}

#[test]
fn exact_signatures_spot_checks() {
    let atan2 = find_builtin("atan2").expect("atan2 exists");
    assert_eq!(atan2.inputs.len(), 2);
    assert_eq!(atan2.inputs[0].name, "y", "argument order is (y, x)");
    assert_eq!(atan2.inputs[1].name, "x");

    let lu_factorize = find_builtin("luFactorize").expect("luFactorize exists");
    assert_eq!(lu_factorize.outputs.len(), 2, "multi-output builtin");

    let interpolation3d = find_builtin("interpolation3D").expect("interpolation3D exists");
    assert_eq!(interpolation3d.inputs.len(), 12);

    let safe_posdiv = find_builtin("safe_posdiv").expect("safe_posdiv exists");
    assert_eq!(safe_posdiv.inputs.len(), 3);
}

#[test]
fn lifting_follows_scalar_signature_rule() {
    // One or two scalar inputs, one scalar output: lifted.
    for lifted in ["sqrt", "integer", "min", "imax", "atan2", "sign", "isNaN"] {
        let builtin = find_builtin(lifted).expect("base builtin exists");
        assert!(builtin.is_lifted(), "{lifted} must be lifted");
        assert!(is_builtin_name(&format!("{lifted}1D")));
        assert!(is_builtin_name(&format!("{lifted}2D")));
        assert!(find_lifted_base(&format!("{lifted}2D")).is_some());
    }
    // Nullary, three-input, and array-parameter builtins: not lifted.
    for unlifted in [
        "pi",
        "nan",
        "safe_posdiv",
        "hasNaN1D",
        "solveLinearEquations",
    ] {
        let builtin = find_builtin(unlifted).expect("base builtin exists");
        assert!(!builtin.is_lifted(), "{unlifted} must not be lifted");
        assert!(!is_builtin_name(&format!("{unlifted}1D")));
    }
}

#[test]
fn reserved_name_surface() {
    // Keywords and future-reserved keywords.
    for name in ["block", "self", "size", "while", "return", "enumeration"] {
        assert!(is_reserved_name(name), "{name} must be reserved");
    }
    // The `__` prefix space.
    assert!(is_reserved_name("__anything"));
    // Builtins including lifted variants.
    assert!(is_reserved_name("absolute"));
    assert!(is_reserved_name("sin2D"));
    // Appendix C including its lifted variants and referenced-undefined names.
    assert!(is_appendix_c_reserved("remainderDown"));
    assert!(is_appendix_c_reserved("roundTowardsZero1D"));
    assert!(is_appendix_c_reserved("abs"));
    assert!(is_reserved_name("divisionEuclidean"));
    // Predefined error signals.
    assert!(is_reserved_name("NAN"));
    assert!(is_reserved_name("SOLVE_LINEAR_EQUATIONS_FAILED"));
    // Ordinary user names stay legal.
    for name in ["myVar", "firstTick", "integralGain", "sinBlend"] {
        assert!(!is_reserved_name(name), "{name} must not be reserved");
    }
}

#[test]
fn all_builtin_names_contains_base_and_lifted() {
    let names: Vec<String> = rumoca_ir_galec::builtins::all_builtin_names().collect();
    assert!(names.iter().any(|n| n == "sqrt"));
    assert!(names.iter().any(|n| n == "sqrt1D"));
    assert!(names.iter().any(|n| n == "atan22D"));
    assert!(!names.iter().any(|n| n == "pi1D"));
    // No duplicates.
    let mut sorted = names.clone();
    sorted.sort_unstable();
    sorted.dedup();
    assert_eq!(sorted.len(), names.len());
}
