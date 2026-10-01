//! MLS §4.9, §10.1: one evaluated scalar attribute or binding value covers
//! every element of an array declaration, including the empty element set of
//! a zero-size array (MLS §10.3.1); any other value count is left for the
//! caller's shape check.

use super::*;

#[test]
fn one_value_spreads_over_every_scalar_including_none() {
    for count in [0, 2, 5] {
        let mut reals = vec![1.5_f64];
        broadcast_scalar_values(&mut reals, count);
        assert_eq!(reals, vec![1.5; count]);

        let mut names = vec!["start".to_string()];
        broadcast_scalar_values(&mut names, count);
        assert_eq!(names, vec!["start".to_string(); count]);
    }
}

#[test]
fn a_scalar_value_and_other_lengths_are_left_alone() {
    let mut scalar = vec![2.0_f64];
    broadcast_scalar_values(&mut scalar, 1);
    assert_eq!(scalar, vec![2.0]);

    for (values, count) in [(vec![1.0, 2.0], 3), (vec![1.0, 2.0, 3.0], 3), (vec![], 2)] {
        let mut copy = values.clone();
        broadcast_scalar_values(&mut copy, count);
        assert_eq!(copy, values, "{count}");
    }

    let mut flags = vec![true, false];
    broadcast_scalar_values(&mut flags, 4);
    assert_eq!(flags, vec![true, false]);
}

#[test]
fn a_variable_view_spreads_one_value_over_its_array_scalars() {
    let source = TestSource::new("parameter Real p[3]; parameter Real q[0];");
    let p_declaration = source.source("parameter Real p[3]", 0);
    let q_declaration = source.source("parameter Real q[0]", 0);
    let dae = Dae::construct(source.map, |dae| {
        for (id, extent, name, declaration) in
            [(0, 3, "p", p_declaration), (1, 0, "q", q_declaration)]
        {
            let array = dae.types(|types| {
                types.intern(
                    TypeId::new(id),
                    ValueType::array(ScalarType::Real, [extent]),
                    declaration,
                )
            })?;
            dae.variables(|variables| {
                variables.parameter(
                    VarName::new(name),
                    array,
                    declaration,
                    VariableAttributes::default(),
                )
            })?;
        }
        Ok(())
    })
    .expect("two parameter arrays construct");
    dae.inspect(|view| {
        let p = view.variable(view.variable_id(0).unwrap()).unwrap();
        let mut values = vec![4.0_f64];
        p.broadcast_values(&mut values);
        assert_eq!(values, vec![4.0; 3]);
        let mut labels = vec!["nominal"];
        p.broadcast_values(&mut labels);
        assert_eq!(labels, vec!["nominal"; 3]);
        let mut full = vec![1.0, 2.0, 3.0];
        p.broadcast_values(&mut full);
        assert_eq!(full, vec![1.0, 2.0, 3.0]);

        let q = view.variable(view.variable_id(1).unwrap()).unwrap();
        let mut empty = vec![4.0_f64];
        q.broadcast_values(&mut empty);
        assert!(empty.is_empty());
    });
}
