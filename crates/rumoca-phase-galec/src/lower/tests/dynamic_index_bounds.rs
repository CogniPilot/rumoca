//! Proven-range tests for a dynamic index whose local is written under a
//! conditional.
//!
//! The fixture is the shape a dense linear solve reaches: a scalar Integer
//! local written on one arm of a conditional and left alone on the other, then
//! used as the subscript of an element write. An element write has no runtime
//! guard to fall back on, so it is admitted only when every reaching definition
//! of the local is proven inside the extent; the union of the arms is exactly
//! what decides these cases.

use super::*;

/// The value one arm of the fixture's conditional writes to `pivotRow`.
enum GuardedPivot {
    /// A literal inside the subscripted extent.
    InRange(i64),
    /// A literal outside the subscripted extent.
    OutOfRange(i64),
    /// The function's Integer parameter, which carries no declared range.
    Unbounded,
}

/// Build `pick(rhs, accepted, hint)`:
///
/// ```modelica
/// x := {0.0, 0.0, 0.0, 0.0};
/// pivotRow := 1;
/// if accepted then
///   pivotRow := <guarded>;
/// end if;
/// x[pivotRow] := rhs[1];
/// ```
fn conditional_pivot_fixture(guarded: &GuardedPivot) -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "function pick input Real rhs[4]; input Boolean accepted; input Integer hint; \
                output Real x[4]; protected Integer pivotRow; algorithm x := zeros(4); \
                pivotRow := 1; if accepted then pivotRow := hint; end if; \
                x[pivotRow] := rhs[1]; end pick;";
    let source = sources.add("conditional-pivot.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let at = dae::DaeProvenance::source(span).unwrap();
    dae::Dae::construct(sources, |dae| {
        let (vector, boolean, integer) = dae.types(|types| {
            Ok((
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [4]), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
            ))
        })?;
        let signature = dae::FunctionSignature::new(
            VarName::new("pick"),
            [vector, boolean, integer],
            [vector],
            at,
        );
        dae.function(signature, |dae, reservation| {
            let (rhs, accepted, hint) = dae.functions(|functions| {
                Ok((
                    functions.parameter(&reservation, VarName::new("rhs"), 0, at)?,
                    functions.parameter(&reservation, VarName::new("accepted"), 1, at)?,
                    functions.parameter(&reservation, VarName::new("hint"), 2, at)?,
                ))
            })?;
            let x =
                dae.functions(|functions| functions.output(&reservation, VarName::new("x"), 0, at))?;
            let pivot_row = dae.functions(|functions| {
                functions.local(&reservation, VarName::new("pivotRow"), integer, at)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, at))?;

            let zeros = dae.expressions(|expressions| {
                let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                expressions.at(at).array([zero, zero, zero, zero])
            })?;
            dae.functions(|functions| functions.assign(&mut body, x, zeros, at))?;

            let one = dae.expressions(|expressions| {
                expressions.at(at).literal(dae::DaeLiteral::Integer(1))
            })?;
            dae.functions(|functions| functions.assign(&mut body, pivot_row, one, at))?;

            let condition =
                dae.expressions(|expressions| expressions.at(at).function_parameter(accepted))?;
            let guarded = match guarded {
                GuardedPivot::InRange(value) | GuardedPivot::OutOfRange(value) => dae
                    .expressions(|expressions| {
                        expressions.at(at).literal(dae::DaeLiteral::Integer(*value))
                    })?,
                GuardedPivot::Unbounded => {
                    dae.expressions(|expressions| expressions.at(at).function_parameter(hint))?
                }
            };
            let kept = dae.functions(|functions| functions.read(&body, pivot_row, at))?;
            dae.functions(|functions| {
                functions.assign_conditional_all(
                    &mut body,
                    &[pivot_row],
                    &[condition],
                    &[vec![guarded]],
                    &[kept],
                    at,
                )
            })?;

            let index = dae.functions(|functions| functions.read(&body, pivot_row, at))?;
            let current = dae.functions(|functions| functions.read(&body, x, at))?;
            let written = dae.expressions(|expressions| {
                let base = expressions.at(at).function_parameter(rhs)?;
                let first = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
                expressions.at(at).index(
                    base,
                    [dae::Subscript::Index {
                        expression: first,
                        provenance: at,
                    }],
                )
            })?;
            let updated = dae.expressions(|expressions| {
                expressions.at(at).array_update(
                    current,
                    written,
                    [dae::Subscript::Index {
                        expression: index,
                        provenance: at,
                    }],
                )
            })?;
            dae.functions(|functions| functions.assign(&mut body, x, updated, at))?;
            dae.functions(|functions| functions.define(body, at))
        })
        .map(|_| ())
    })
    .unwrap()
}

fn lower_conditional_pivot(guarded: &GuardedPivot) -> Result<Vec<gast::UserFunction>, String> {
    let model = conditional_pivot_fixture(guarded);
    model.inspect(|view| {
        let pick = view.function_id(0).unwrap();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        user_functions::lower_reachable(view, &definitions, HashSet::from([pick.index()]))
            .map_err(|error| format!("{error:?}"))
    })
}

#[test]
fn a_guarded_pivot_inside_the_extent_proves_through_the_union_of_its_arms() {
    // The taken arm writes 4 and the untaken arm keeps the 1 written before
    // the conditional, so every reaching value is inside 1:4.
    let lowered = lower_conditional_pivot(&GuardedPivot::InRange(4))
        .expect("a pivot proven on both reaching definitions lowers");
    assert_eq!(lowered.len(), 1);
}

#[test]
fn a_guarded_pivot_outside_the_extent_is_refused() {
    // The kept value is inside 1:4 but the written one is not, so the union
    // leaves the subscript outside the extent.
    let error = lower_conditional_pivot(&GuardedPivot::OutOfRange(5))
        .expect_err("a pivot outside the extent on one arm is refused");
    assert!(error.contains("dynamic-array-index"), "{error}");
}

#[test]
fn a_guarded_pivot_with_one_unproven_arm_is_refused() {
    // The kept value is proven and the written one is not. An unproven
    // reaching definition makes the union unproven: the subscript is refused
    // rather than admitted on the strength of the arm that does prove.
    let error = lower_conditional_pivot(&GuardedPivot::Unbounded)
        .expect_err("a pivot with an unproven reaching definition is refused");
    assert!(error.contains("dynamic-array-index"), "{error}");
}
