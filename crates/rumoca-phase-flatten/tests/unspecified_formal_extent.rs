//! Regression coverage for function inputs declared with an unspecified
//! dimension (MLS §12.4.5, §10.3.1): `input Real x[:]` takes its extent from
//! the actual argument, so a call with a three-element vector is accepted
//! and `size(x, 1)` inside the function reads 3.
//!
//! The MSL shapes that exercise this are
//! `Modelica.Electrical.Polyphase.Functions.quasiRMS` (`input Real x[:]`),
//! `Modelica.Electrical.Polyphase.Functions.activePower` (`input Real v[:]`,
//! `input Real i[:]`) and `Modelica.Math.BooleanVectors.anyTrue`
//! (`input Boolean b[:]`), all called with polyphase vectors from the
//! machine examples. Every one of them regressed to
//! `EF034 ... argument has shape [3], expected [0]` when the written `:` was
//! treated as an exact zero extent.
//!
//! The rule is per dimension and must not blind the exact check: a literal
//! extent (`x[2]`) still refuses a different actual extent, a mixed shape
//! (`x[3, :]`) still refuses a different leading extent, and an unspecified
//! extent still declares a dimension, so rank stays exact.

use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

/// Parse, resolve, instantiate, typecheck and flatten `source`; every
/// refusal after resolution is reported as its rendered message so the tests
/// can name the layer that refused.
fn try_flatten_source(source: &str, model: &str) -> Result<flat::Model, String> {
    let file_name = "<unspecified_formal_extent>";
    let stored = rumoca_phase_parse::parse_to_ast(source, file_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let overlay =
        match rumoca_phase_instantiate::instantiate_model_with_outcome(resolved.inner(), model) {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                return Err(format!("instantiate: {error}"));
            }
        };
    let typed = rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, model)
        .map_err(|error| format!("typecheck: {error:?}"))?;
    rumoca_phase_flatten::flatten_typed(typed, rumoca_phase_flatten::FlattenOptions::default())
        .map_err(|error| format!("flatten: {error}"))
}

/// Resolved dimensions of the flat variable named `name`.
fn dims_of(model: &flat::Model, name: &str) -> Vec<i64> {
    model
        .variables
        .iter()
        .find(|(var_name, _)| var_name.as_str() == name)
        .map(|(_, var)| var.dims.clone())
        .unwrap_or_else(|| {
            let known: Vec<&str> = model.variables.keys().map(|key| key.as_str()).collect();
            panic!("no flat variable `{name}`; got {known:?}")
        })
}

const LIB: &str = r#"
    package Lib
        function vectorLength "quasiRMS shape: one unspecified extent"
            input Real x[:];
            output Integer n;
        algorithm
            n := size(x, 1);
        end vectorLength;

        function pairLength "a literal extent is an exact obligation"
            input Real x[2];
            output Integer n;
        algorithm
            n := size(x, 1);
        end pairLength;

        function columnCount "every dimension unspecified"
            input Real m[:, :];
            output Integer n;
        algorithm
            n := size(m, 2);
        end columnCount;

        function trailingCount "mixed literal and unspecified extents"
            input Real m[3, :];
            output Integer n;
        algorithm
            n := size(m, 2);
        end trailingCount;

        model Accepts
            parameter Integer n = vectorLength({1.0, 2.0, 3.0});
            Real y[n];
            Real a = vectorLength({1.0, 2.0, 3.0});
            Real d = columnCount({{1.0, 2.0, 3.0, 4.0}, {5.0, 6.0, 7.0, 8.0}});
            Real e = trailingCount({{1.0, 2.0}, {3.0, 4.0}, {5.0, 6.0}});
        end Accepts;

        model RefusesFixed
            Real y = pairLength({1.0, 2.0, 3.0});
        end RefusesFixed;

        model RefusesMixed
            Real y = trailingCount({{1.0, 2.0}, {3.0, 4.0}});
        end RefusesMixed;

        model RefusesScalarRank
            Real y = vectorLength(1.0);
        end RefusesScalarRank;

        model RefusesVectorRank
            Real y = columnCount({1.0, 2.0, 3.0});
        end RefusesVectorRank;
    end Lib;
"#;

/// Every binding here is evaluated by the flatten phase: `y[n]` sizes the
/// variable from the call's result, and the scalar bindings have their
/// dimensions inferred from the call, which binds each formal's shape. Each
/// of them was refused as `has shape [3], expected [0]` before the written
/// `:` was read as an unspecified extent.
#[test]
fn unspecified_formal_extent_takes_the_actual_extent() {
    let model = try_flatten_source(LIB, "Lib.Accepts").expect("model flattens");
    assert_eq!(
        dims_of(&model, "y"),
        vec![3],
        "size(x, 1) inside `vectorLength` reads the actual's extent"
    );
    for scalar in ["a", "d", "e"] {
        assert!(dims_of(&model, scalar).is_empty(), "`{scalar}` is a scalar");
    }
}

/// A literal extent stays an exact obligation. `x[2]` against a 3-vector is
/// refused by the typecheck phase before flatten sees it (the flatten-phase
/// refusal of that exact shape is covered by the `rumoca-eval-flat` unit
/// tests); the mixed `m[3, :]` reaches flatten and is refused there on its
/// literal leading extent while its trailing extent stays unspecified.
#[test]
fn fixed_formal_extent_still_refuses_a_different_actual() {
    let error = try_flatten_source(LIB, "Lib.RefusesFixed")
        .expect_err("`x[2]` refuses a three-element actual");
    assert!(error.starts_with("typecheck:"), "{error}");
    assert!(
        error.contains("array dimension mismatch: expected `[2]`, found `[3]`"),
        "{error}"
    );

    let error = try_flatten_source(LIB, "Lib.RefusesMixed")
        .expect_err("`m[3, :]` refuses a two-row actual");
    assert!(error.starts_with("flatten:"), "{error}");
    assert!(
        error.contains("argument `m` has shape [2, 2], expected [3, :]"),
        "{error}"
    );
}

/// An unspecified extent still declares a dimension, so rank is exact: a
/// scalar is not a vector and a vector is not a matrix. Both reach flatten
/// and are refused there.
#[test]
fn unspecified_formal_extent_still_refuses_a_rank_mismatch() {
    let error = try_flatten_source(LIB, "Lib.RefusesScalarRank")
        .expect_err("`x[:]` refuses a scalar actual");
    assert!(error.starts_with("flatten:"), "{error}");
    assert!(
        error.contains("argument `x` has shape [], expected [:]"),
        "{error}"
    );

    let error = try_flatten_source(LIB, "Lib.RefusesVectorRank")
        .expect_err("`m[:, :]` refuses a vector actual");
    assert!(error.starts_with("flatten:"), "{error}");
    assert!(
        error.contains("argument `m` has shape [3], expected [:, :]"),
        "{error}"
    );
}
