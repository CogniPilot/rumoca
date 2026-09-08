//! Callable identity in the declaration-level structural folds.
//!
//! The dimension folds (`init_array_dimensions`, `infer_dims_from_literals`)
//! and the structural binding and Boolean-equation folds run while the flatten
//! context is being prepared, before `finalize_flat_model` collects the Flat
//! callable catalog. The evaluator resolves a user-function call only through
//! the exact occurrence that collected-call canonicalization attaches, so the
//! pre-collected callables are issued their Flat identity, and the fold's
//! expression is canonicalized against that catalog, before the first fold
//! runs. These cases pin both directions of that contract: a pure user
//! function and a record constructor now fold in those positions, while an
//! `external` function, an `impure` function, and a synchronous operator are
//! still refused or deferred and never fold to a value.

use rumoca_core::Expression;
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

/// Where a fixture stopped when it did not flatten.
#[derive(Debug)]
enum Refusal {
    Typecheck(String),
    Flatten(rumoca_phase_flatten::FlattenError),
}

fn try_flatten_source(source: &str, model: &str) -> Result<flat::Model, Box<Refusal>> {
    let file_name = "<structural_fold_callable_identity>";
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
                panic!("fixture instantiation failed: {error}")
            }
        };
    let typed = rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, model)
        .map_err(|diagnostics| Box::new(Refusal::Typecheck(format!("{diagnostics:?}"))))?;
    rumoca_phase_flatten::flatten_typed(typed, rumoca_phase_flatten::FlattenOptions::default())
        .map_err(|error| Box::new(Refusal::Flatten(error)))
}

fn flatten_source(source: &str, model: &str) -> flat::Model {
    match try_flatten_source(source, model).map_err(|refusal| *refusal) {
        Ok(model) => model,
        Err(Refusal::Typecheck(diagnostics)) => panic!("model typechecks: {diagnostics}"),
        Err(Refusal::Flatten(error)) => panic!("model flattens: {error}"),
    }
}

fn variable<'a>(model: &'a flat::Model, name: &str) -> &'a flat::Variable {
    model
        .variables
        .iter()
        .find(|(var_name, _)| var_name.as_str() == name)
        .map(|(_, var)| var)
        .unwrap_or_else(|| {
            let known: Vec<&str> = model.variables.keys().map(|key| key.as_str()).collect();
            panic!("no flat variable `{name}`; got {known:?}")
        })
}

fn is_call_to(expr: &Expression, callee: &str) -> bool {
    matches!(expr, Expression::FunctionCall { name, .. } if name.as_str().ends_with(callee))
}

/// `Modelica.Electrical.Polyphase.Functions.symmetricOrientation` shape: an
/// array parameter declared with unspecified dimensions and bound to a pure
/// function whose output extent is its own input.
const PURE_FUNCTION_DIMENSION_FOLD: &str = r"
    package Lib
        function sym
            input Integer m;
            output Real y[m];
        algorithm
            for k in 1:m loop
                y[k] := (k - 1) * 2 / m;
            end for;
        end sym;

        function count
            input Integer m;
            output Integer n;
        algorithm
            n := 2 * m;
        end count;

        model Top
            parameter Integer m = 3;
            parameter Real orientation[:] = sym(m);
            parameter Integer n = count(m);
            Real z[n];
            Real x;
        equation
            der(x) = -sum(orientation) * x;
            for i in 1:n loop
                z[i] = i * x;
            end for;
        end Top;
    end Lib;
";

#[test]
fn pure_user_function_folds_in_declared_dimension_and_structural_binding_positions() {
    let model = flatten_source(PURE_FUNCTION_DIMENSION_FOLD, "Lib.Top");
    // `orientation[:]` has no declared extent; the fold reads `y[m]` from the
    // resolved callee with the structural `m = 3`.
    assert_eq!(variable(&model, "orientation").dims, vec![3]);
    // `n = count(m)` is a structural binding whose only source of a value is
    // executing the pure body, and `z[n]` is sized from it.
    assert_eq!(variable(&model, "z").dims, vec![6]);
}

/// `Modelica.Thermal.FluidHeatFlow.Media.Medium()` shape: a record constructor
/// bound to a parameter, and a constructor value consumed by a pure function
/// whose result selects an if-equation branch. The call is written with the
/// use-site spelling a model inside the package would use.
const RECORD_CONSTRUCTOR_BINDING_FOLD: &str = r"
    package Lib
        record Medium
            parameter Real rho = 1;
            parameter Real cp = 4;
        end Medium;

        function halfCp
            input Medium medium;
            output Integer n;
        algorithm
            n := integer(medium.cp / 2);
        end halfCp;

        model Top
            parameter Medium medium = Medium(rho = 2);
            parameter Integer n = halfCp(Medium(rho = 2));
            Real x;
        equation
            if n == 2 then
                der(x) = -medium.rho * x;
            else
                der(x) = -7 * x;
            end if;
        end Top;
    end Lib;
";

#[test]
fn record_constructor_folds_in_a_parameter_binding() {
    let model = flatten_source(RECORD_CONSTRUCTOR_BINDING_FOLD, "Lib.Top");
    // `halfCp(Medium(rho = 2))` selects the branch only if the constructor
    // call folded to the record value whose default `cp = 4` the body reads;
    // a retained conditional or the `-7` branch would mean it did not.
    assert_eq!(model.equations.len(), 1, "one branch survives selection");
    let residual = &model.equations[0].residual;
    assert!(
        !residual.contains_subexpression(|expr| matches!(expr, Expression::If { .. })),
        "the if-equation must be selected structurally, got {residual:?}"
    );
    assert!(
        !residual.contains_subexpression(|expr| matches!(
            expr,
            Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                ..
            } if *value == 7.0
        )),
        "the else branch must not survive, got {residual:?}"
    );
}

/// `Modelica.Blocks.Tables.Internal.getTimeTableTmin` shape: an `external "C"`
/// function bound to a structural parameter.
const EXTERNAL_FUNCTION_BINDING: &str = r#"
    package Lib
        function ext
            input Real x;
            output Real y;
            external "C" y = ext_c(x);
        end ext;

        model Top
            parameter Real t = ext(1.0);
            Real x;
        equation
            der(x) = -t * x;
        end Top;
    end Lib;
"#;

#[test]
fn external_function_in_a_structural_binding_is_deferred_and_never_folded() {
    let model = flatten_source(EXTERNAL_FUNCTION_BINDING, "Lib.Top");
    let binding = variable(&model, "t")
        .binding
        .as_ref()
        .expect("the parameter keeps its declaration binding");
    // The occurrence now resolves, so the evaluator reaches the callee and
    // refuses it as external (a typed `NotConstant` deferral); the binding
    // stays the call for the runtime owner and is never a substituted value.
    assert!(
        is_call_to(binding, "ext"),
        "external call must remain unevaluated, got {binding:?}"
    );
}

/// `Modelica.Math.Random.Utilities.initializeImpureRandom` shape: an `impure`
/// function whose result would size an array if it were folded.
const IMPURE_FUNCTION_BINDING: &str = r"
    package Lib
        impure function seed
            input Integer s;
            output Integer r;
        algorithm
            r := s;
        end seed;

        model Sized
            parameter Integer n = seed(3);
            Real z[n];
        equation
            for i in 1:n loop
                z[i] = i;
            end for;
        end Sized;

        model Scalar
            parameter Integer n = seed(3);
            Real x;
        equation
            der(x) = -n * x;
        end Scalar;
    end Lib;
";

#[test]
fn impure_function_in_a_structural_binding_never_folds() {
    // A folded `seed(3)` would size `z` as `[3]` and compile cleanly; the
    // impure refusal leaves `n` without a translation-time value, so no phase
    // can settle the dimension.
    assert!(
        try_flatten_source(IMPURE_FUNCTION_BINDING, "Lib.Sized").is_err(),
        "an impure call must not supply a structural dimension"
    );
    let model = flatten_source(IMPURE_FUNCTION_BINDING, "Lib.Scalar");
    let binding = variable(&model, "n")
        .binding
        .as_ref()
        .expect("the parameter keeps its declaration binding");
    assert!(
        is_call_to(binding, "seed"),
        "impure call must remain unevaluated, got {binding:?}"
    );
}

/// A synchronous operator in a structural Boolean position, with a user
/// callable present so the fold runs against an identified catalog.
const SAMPLE_STRUCTURAL_BOOLEAN: &str = r"
    package Lib
        function twice
            input Real a;
            output Real b;
        algorithm
            b := 2 * a;
        end twice;

        model Top
            parameter Real p = twice(1.0);
            Boolean b;
            Real x;
        equation
            b = sample(0.0, 0.5);
            der(x) = if b then -x else -p * x;
        end Top;
    end Lib;
";

#[test]
fn sample_in_a_structural_boolean_position_is_refused_or_deferred_never_folded() {
    match try_flatten_source(SAMPLE_STRUCTURAL_BOOLEAN, "Lib.Top").map_err(|refusal| *refusal) {
        // Refusal: `sample` is not a collected callable, so no occurrence is
        // attached and the evaluator declines the call by identity.
        Err(Refusal::Flatten(rumoca_phase_flatten::FlattenError::ConstantEvaluationFailed {
            reason,
            ..
        })) => {
            assert!(
                reason.contains("sample"),
                "refusal must name the operator: {reason}"
            );
        }
        Err(refusal) => panic!("unexpected failure: {refusal:?}"),
        // Deferral: the equation must still carry the operator call, never a
        // Boolean literal the runtime event indicator was replaced by.
        Ok(model) => {
            let folded = model.equations.iter().any(|equation| {
                equation.residual.contains_subexpression(|expr| {
                    matches!(
                        expr,
                        Expression::Literal {
                            value: rumoca_core::Literal::Boolean(_),
                            ..
                        }
                    )
                })
            });
            assert!(!folded, "sample(...) must not fold to a Boolean literal");
            let retained = model.equations.iter().any(|equation| {
                equation.residual.contains_subexpression(|expr| {
                    matches!(
                        expr,
                        Expression::FunctionCall { name, .. } if name.as_str() == "sample"
                    ) || matches!(
                        expr,
                        Expression::BuiltinCall {
                            function: rumoca_core::BuiltinFunction::Sample,
                            ..
                        }
                    )
                })
            });
            assert!(retained, "sample(...) must remain in the equations");
        }
    }
}
