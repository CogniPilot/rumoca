//! MLS §8.6 discrete initial values lowered to initialization update rows.

use super::*;

/// MLS §8.6: an `initial algorithm` that assigns a discrete-time variable determines
/// the value that variable holds when initialization finishes. Solve lowers that DAE
/// definition to an initialization update row, and it writes the coordinate's `pre`
/// slot too, because `pre(v) = v` holds at the initialization instant. Without the
/// `pre` row a `when` whose trigger reads `pre(v)` — the
/// `Modelica.Blocks.Sources.Pulse`/`SawTooth`/`Trapezoid` period counter — would be
/// scheduled against the declared `start` value the algorithm has already replaced.
#[test]
fn discrete_initial_value_becomes_an_initialization_update_of_the_coordinate_and_its_pre_slot() {
    let source = TestSource::new("Integer count; initial algorithm count := 3;");
    let declaration = source.at(0, 13);
    let owner = source.at(34, 43);
    let model = dae::Dae::construct(source.map, |model| {
        let integer = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Integer),
                declaration,
            )
        })?;
        let count = model.variables(|variables| {
            variables.discrete_value(
                VarName::new("count"),
                integer,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let (three, previous) = model.expressions(|expressions| {
            Ok((
                expressions.at(owner).literal(dae::DaeLiteral::Integer(3))?,
                expressions
                    .at(declaration)
                    .coordinate(dae::CoordinateInput::PreDiscreteValue(count))?,
            ))
        })?;
        model.initialization(|initialization| {
            initialization
                .discrete_value_initial_value(count, three, owner)
                .map(|_| ())
        })?;
        model.b1c([count], |topology| {
            topology.owner(declaration, [count], |staged| {
                staged.always(declaration, [(previous, declaration)])
            })?;
            Ok(())
        })
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    solve
        .validate()
        .expect("the initialization update satisfies the Solve shape contract");
    let ScalarSlot::P { index, .. } = solve
        .layout
        .binding("count")
        .expect("a discrete coordinate keeps parameter storage")
    else {
        panic!("a discrete coordinate occupies P storage");
    };
    let [current, pre] = solve.initialization.update_targets.as_slice() else {
        panic!(
            "one current and one pre update target expected, got {:?}",
            solve.initialization.update_targets
        );
    };
    assert_eq!(*current, rumoca_ir_solve::scalar_slot_p(index));
    let pre_index = solve
        .solve_layout
        .pre_param_bindings
        .iter()
        .find(|binding| {
            matches!(
                binding.source,
                rumoca_ir_solve::PreParamSource::P { index: source } if source == index
            )
        })
        .expect("a discrete coordinate has a lowered pre slot")
        .dest_p_index;
    assert_eq!(*pre, rumoca_ir_solve::scalar_slot_p(pre_index));
    assert_eq!(
        solve.initialization.update_rhs.len(),
        2,
        "each update target has its own row"
    );
}
