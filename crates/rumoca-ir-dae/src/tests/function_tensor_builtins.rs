//! Compact typed array/matrix constructor owners.
//!
//! Split out of `function_owners.rs` at the module seam between semantic
//! function-owner construction and the aggregate builtin constructors, to keep
//! both files under the SPEC_0021 file-size threshold.

use super::*;

#[test]
fn ones_and_fill_are_compact_typed_array_operations() {
    let source = TestSource::new("ones(2, 2); fill(0.5, 3)");
    let ones_at = source.source("ones(2, 2)", 0);
    let fill_at = source.source("fill(0.5, 3)", 0);
    let first_two_at = source.source("2", 0);
    let second_two_at = source.source("2", 1);
    let value_at = source.source("0.5", 0);
    let extent_at = source.source("3", 0);
    let dae = Dae::construct(source.map, |dae| {
        let first_two = dae.expressions(|expressions| {
            expressions.at(first_two_at).literal(DaeLiteral::Integer(2))
        })?;
        let second_two = dae.expressions(|expressions| {
            expressions
                .at(second_two_at)
                .literal(DaeLiteral::Integer(2))
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(ones_at)
                .builtin(PureBuiltin::Ones, [first_two, second_two])
        })?;
        let value =
            dae.expressions(|expressions| expressions.at(value_at).literal(DaeLiteral::Real(0.5)))?;
        let extent = dae
            .expressions(|expressions| expressions.at(extent_at).literal(DaeLiteral::Integer(3)))?;
        dae.expressions(|expressions| {
            expressions
                .at(fill_at)
                .builtin(PureBuiltin::Fill, [value, extent])
        })?;
        Ok(())
    })
    .unwrap();

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        for (index, builtin, dimensions, text) in [
            (2, PureBuiltin::Ones, &[2, 2][..], "ones(2, 2)"),
            (5, PureBuiltin::Fill, &[3][..], "fill(0.5, 3)"),
        ] {
            let expression = view.expression(view.expression_id(index).unwrap()).unwrap();
            assert_eq!(expression.value_type().scalar_type(), ScalarType::Real);
            assert_eq!(expression.value_type().dimensions(), dimensions);
            assert_eq!(view.source_text(expression.provenance()), Some(text));
            assert!(matches!(
                expression.operation(),
                ExpressionOperation::Builtin { builtin: found, .. } if found == builtin
            ));
        }
    });
}

#[test]
fn identity_is_a_checked_compact_integer_matrix_operation() {
    let source = TestSource::new("Integer i[2,2] = identity(2)");
    let extent_at = source.source("2", 2);
    let identity_at = source.source("identity(2)", 0);
    let dae = Dae::construct(source.map, |dae| {
        let extent = dae
            .expressions(|expressions| expressions.at(extent_at).literal(DaeLiteral::Integer(2)))?;
        dae.expressions(|expressions| {
            expressions
                .at(identity_at)
                .builtin(PureBuiltin::Identity, [extent])
        })?;
        Ok(())
    })
    .expect("one non-negative Integer extent constructs identity compactly");

    let assert_identity = |view: DaeView<'_>| {
        assert_eq!(view.expression_count(), 2, "identity stores no n² payload");
        let expression = view.expression(view.expression_id(1).unwrap()).unwrap();
        assert_eq!(expression.value_type().scalar_type(), ScalarType::Integer);
        assert_eq!(expression.value_type().dimensions(), &[2, 2]);
        assert!(matches!(
            expression.operation(),
            ExpressionOperation::Builtin {
                builtin: PureBuiltin::Identity,
                arguments,
            } if arguments.len() == 1
        ));
    };
    dae.inspect(assert_identity);
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(assert_identity);
}

#[test]
fn identity_rejects_non_integer_negative_and_extra_extents() {
    let invalid = |literal: DaeLiteral, argument_count: usize| {
        let source = TestSource::new("identity(extent)");
        let at = source.source("identity(extent)", 0);
        Dae::construct(source.map, |dae| {
            let extent =
                dae.expressions(|expressions| expressions.at(at).literal(literal.clone()))?;
            let arguments = vec![extent; argument_count];
            dae.expressions(|expressions| {
                expressions.at(at).builtin(PureBuiltin::Identity, arguments)
            })?;
            Ok(())
        })
    };

    assert!(matches!(
        invalid(DaeLiteral::Real(2.0), 1),
        Err(DaeConstructionError::InvalidArrayExtent { .. })
    ));
    assert!(matches!(
        invalid(DaeLiteral::Integer(-1), 1),
        Err(DaeConstructionError::InvalidArrayExtent { .. })
    ));
    assert!(matches!(
        invalid(DaeLiteral::Integer(2), 2),
        Err(DaeConstructionError::InvalidArity {
            expected: 1,
            found: 2,
            ..
        })
    ));
}

#[test]
fn vector_is_one_compact_constructor_derived_view_and_round_trips() {
    let source = TestSource::new("parameter Real p[1,3,1]; vector(p); vector(k)");
    let declaration = source.source("parameter Real p[1,3,1]", 0);
    let parameter_use = source.source("p", 1);
    let parameter_vector = source.source("vector(p)", 0);
    let binder_use = source.source("k", 0);
    let binder_vector = source.source("vector(k)", 0);
    let domain_input = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(0),
            display_name: "k".to_string(),
            lower: 1,
            upper: 1,
            step: 1,
        }],
    };
    let dae = Dae::construct(source.map, |dae| {
        let parameter_type = dae.types(|types| {
            types.intern(
                TypeId::new(80),
                ValueType::array(ScalarType::Real, [1, 3, 1]),
                declaration,
            )
        })?;
        let parameter = dae.variables(|variables| {
            variables.parameter(
                VarName::new("p"),
                rumoca_core::InstanceId::new(1),
                parameter_type,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let domain = dae.domains(|domains| domains.structured(domain_input.clone(), binder_use))?;
        let binder = dae.domains(|domains| domains.binder(domain, 0, binder_use))?;
        dae.expressions(|expressions| {
            let parameter = expressions
                .at(parameter_use)
                .coordinate(CoordinateInput::Parameter(parameter))?;
            expressions
                .at(parameter_vector)
                .builtin(PureBuiltin::Vector, [parameter])?;
            let binder = expressions.at(binder_use).binder(binder)?;
            expressions
                .at(binder_vector)
                .builtin(PureBuiltin::Vector, [binder])?;
            Ok(())
        })
    })
    .expect("ARR-015 operand shapes construct vector views without scalarization");

    let assert_vector = |view: DaeView<'_>| {
        assert_eq!(view.expression_count(), 4, "each vector remains one node");
        let parameter = view.expression(view.expression_id(0).unwrap()).unwrap();
        let parameter_vector = view.expression(view.expression_id(1).unwrap()).unwrap();
        assert_eq!(
            parameter_vector.value_type().scalar_type(),
            ScalarType::Real
        );
        assert_eq!(parameter_vector.value_type().dimensions(), &[3]);
        assert_eq!(parameter_vector.variability(), parameter.variability());
        assert_eq!(parameter_vector.binder_domain(), parameter.binder_domain());
        assert!(matches!(
            parameter_vector.operation(),
            ExpressionOperation::Builtin {
                builtin: PureBuiltin::Vector,
                arguments,
            } if arguments.len() == 1
        ));

        let binder = view.expression(view.expression_id(2).unwrap()).unwrap();
        let binder_vector = view.expression(view.expression_id(3).unwrap()).unwrap();
        assert_eq!(
            binder_vector.value_type().scalar_type(),
            ScalarType::Integer
        );
        assert_eq!(binder_vector.value_type().dimensions(), &[1]);
        assert_eq!(binder_vector.variability(), binder.variability());
        assert_eq!(binder_vector.binder_domain(), binder.binder_domain());
    };
    dae.inspect(assert_vector);

    assert_eq!(
        bincode::serialize(&PureBuiltin::Vector).unwrap(),
        38_u32.to_le_bytes()
    );
    assert_eq!(DAE_SCHEMA_VERSION, 36);
    let json = serde_json::to_string(&dae).unwrap();
    assert!(json.contains("\"builtin\":\"vector\""));
    let decoded: Dae = serde_json::from_str(&json).unwrap();
    decoded.inspect(assert_vector);
    let binary = bincode::serialize(&dae).unwrap();
    let decoded: Dae = bincode::deserialize(&binary).unwrap();
    assert_eq!(bincode::serialize(&decoded).unwrap(), binary);
}

#[test]
fn vector_rejects_forged_arity_and_two_non_unit_dimensions() {
    let invalid_arity = |count: usize| {
        let source = TestSource::new("vector(A)");
        let at = source.source("vector(A)", 0);
        Dae::construct(source.map, |dae| {
            let value =
                dae.expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(1.0)))?;
            dae.expressions(|expressions| {
                expressions
                    .at(at)
                    .builtin(PureBuiltin::Vector, vec![value; count])
            })?;
            Ok(())
        })
    };
    assert!(matches!(
        invalid_arity(0),
        Err(DaeConstructionError::InvalidArity {
            expected: 1,
            found: 0,
            ..
        })
    ));
    assert!(matches!(
        invalid_arity(2),
        Err(DaeConstructionError::InvalidArity {
            expected: 1,
            found: 2,
            ..
        })
    ));

    let source = TestSource::new("vector(A)");
    let at = source.source("vector(A)", 0);
    let error = Dae::construct(source.map, |dae| {
        let matrix =
            dae.types(|types| types.derived(ValueType::array(ScalarType::Real, [2, 3]), at))?;
        let input = dae.variables(|variables| {
            variables.input(
                VarName::new("A"),
                rumoca_core::InstanceId::new(1),
                matrix,
                InputVariability::Continuous,
                at,
                VariableAttributes::default(),
            )
        })?;
        let input = dae.expressions(|expressions| {
            expressions.at(at).coordinate(CoordinateInput::Input(input))
        })?;
        dae.expressions(|expressions| expressions.at(at).builtin(PureBuiltin::Vector, [input]))?;
        Ok(())
    });
    assert!(matches!(
        error,
        Err(DaeConstructionError::ShapeMismatch { .. })
    ));
}

#[test]
fn vector_rejects_record_roots_until_aggregate_dimensions_are_explicit() {
    let source = TestSource::new("vector(Pair(1.0, 2))");
    let at = source.source("vector(Pair(1.0, 2))", 0);
    let error = Dae::construct(source.map, |dae| {
        let (real, integer) = dae.types(|types| {
            Ok((
                types.derived(ValueType::scalar(ScalarType::Real), at)?,
                types.derived(ValueType::scalar(ScalarType::Integer), at)?,
            ))
        })?;
        let pair = dae.types(|types| {
            types.record(
                VarName::new("Pair"),
                [(VarName::new("x"), real), (VarName::new("n"), integer)],
                at,
            )
        })?;
        let pair = dae.expressions(|expressions| {
            let x = expressions.at(at).literal(DaeLiteral::Real(1.0))?;
            let n = expressions.at(at).literal(DaeLiteral::Integer(2))?;
            expressions.at(at).record(pair, [x, n])
        })?;
        dae.expressions(|expressions| expressions.at(at).builtin(PureBuiltin::Vector, [pair]))?;
        Ok(())
    });

    assert!(matches!(
        error,
        Err(DaeConstructionError::ShapeMismatch { .. })
    ));
}

#[test]
fn transpose_swaps_only_the_first_two_axes_and_round_trips() {
    let source = TestSource::new(
        "input Real m[2,3]; input Real t[2,3,4]; input Real z[0,3]; transpose(m); transpose(t); transpose(z)",
    );
    let at = source.source("transpose(m)", 0);
    let dae = Dae::construct(source.map, |dae| {
        let (matrix, tensor, empty) = dae.types(|types| {
            Ok((
                types.derived(ValueType::array(ScalarType::Real, [2, 3]), at)?,
                types.derived(ValueType::array(ScalarType::Real, [2, 3, 4]), at)?,
                types.derived(ValueType::array(ScalarType::Real, [0, 3]), at)?,
            ))
        })?;
        let (m, t, z) = dae.variables(|variables| {
            Ok((
                variables.input(
                    VarName::new("m"),
                    rumoca_core::InstanceId::new(1),
                    matrix,
                    InputVariability::Continuous,
                    at,
                    VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("t"),
                    rumoca_core::InstanceId::new(2),
                    tensor,
                    InputVariability::Continuous,
                    at,
                    VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("z"),
                    rumoca_core::InstanceId::new(3),
                    empty,
                    InputVariability::Continuous,
                    at,
                    VariableAttributes::default(),
                )?,
            ))
        })?;
        dae.expressions(|expressions| {
            for input in [
                CoordinateInput::Input(m),
                CoordinateInput::Input(t),
                CoordinateInput::Input(z),
            ] {
                let input = expressions.at(at).coordinate(input)?;
                expressions
                    .at(at)
                    .builtin(PureBuiltin::Transpose, [input])?;
            }
            Ok(())
        })
    })
    .expect("ARR-038 admits compact primitive arrays of rank two or greater");

    let assert_layout = |view: DaeView<'_>| {
        assert_eq!(view.expression_count(), 6);
        for (index, expected) in [(1, &[3, 2][..]), (3, &[3, 2, 4]), (5, &[3, 0])] {
            let transpose = view.expression(view.expression_id(index).unwrap()).unwrap();
            assert_eq!(transpose.value_type().scalar_type(), ScalarType::Real);
            assert_eq!(transpose.value_type().dimensions(), expected);
            assert!(matches!(
                transpose.operation(),
                ExpressionOperation::Builtin {
                    builtin: PureBuiltin::Transpose,
                    arguments,
                } if arguments.len() == 1
            ));
        }
    };
    dae.inspect(assert_layout);

    assert_eq!(
        bincode::serialize(&PureBuiltin::Transpose).unwrap(),
        39_u32.to_le_bytes()
    );
    assert_eq!(DAE_SCHEMA_VERSION, 36);
    let json = serde_json::to_string(&dae).unwrap();
    assert!(json.contains("\"builtin\":\"transpose\""));
    let decoded: Dae = serde_json::from_str(&json).unwrap();
    decoded.inspect(assert_layout);
    let binary = bincode::serialize(&dae).unwrap();
    let decoded: Dae = bincode::deserialize(&binary).unwrap();
    decoded.inspect(assert_layout);
}

#[test]
fn transpose_rejects_forged_arity_rank_and_record_roots() {
    let invalid_arity = |count: usize| {
        let source = TestSource::new("transpose(A)");
        let at = source.source("transpose(A)", 0);
        Dae::construct(source.map, |dae| {
            let value =
                dae.expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(1.0)))?;
            dae.expressions(|expressions| {
                expressions
                    .at(at)
                    .builtin(PureBuiltin::Transpose, vec![value; count])
            })?;
            Ok(())
        })
    };
    for (count, found) in [(0, 0), (2, 2)] {
        assert!(matches!(
            invalid_arity(count),
            Err(DaeConstructionError::InvalidArity {
                expected: 1,
                found: actual,
                ..
            }) if actual == found
        ));
    }

    for dimensions in [Vec::new(), vec![3]] {
        let source = TestSource::new("transpose(A)");
        let at = source.source("transpose(A)", 0);
        let error = Dae::construct(source.map, |dae| {
            let ty = dae.types(|types| {
                types.derived(ValueType::array(ScalarType::Real, dimensions.clone()), at)
            })?;
            let input = dae.variables(|variables| {
                variables.input(
                    VarName::new("A"),
                    rumoca_core::InstanceId::new(1),
                    ty,
                    InputVariability::Continuous,
                    at,
                    VariableAttributes::default(),
                )
            })?;
            let input = dae.expressions(|expressions| {
                expressions.at(at).coordinate(CoordinateInput::Input(input))
            })?;
            dae.expressions(|expressions| {
                expressions.at(at).builtin(PureBuiltin::Transpose, [input])
            })?;
            Ok(())
        });
        assert!(matches!(
            error,
            Err(DaeConstructionError::ShapeMismatch { .. })
        ));
    }

    let source = TestSource::new("transpose(Pair(1.0, 2.0))");
    let at = source.source("transpose(Pair(1.0, 2.0))", 0);
    let error = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        let pair = dae.types(|types| {
            types.record(
                VarName::new("Pair"),
                [(VarName::new("x"), real), (VarName::new("y"), real)],
                at,
            )
        })?;
        let pair = dae.expressions(|expressions| {
            let x = expressions.at(at).literal(DaeLiteral::Real(1.0))?;
            let y = expressions.at(at).literal(DaeLiteral::Real(2.0))?;
            expressions.at(at).record(pair, [x, y])
        })?;
        dae.expressions(|expressions| expressions.at(at).builtin(PureBuiltin::Transpose, [pair]))?;
        Ok(())
    });
    assert!(matches!(
        error,
        Err(DaeConstructionError::ShapeMismatch { .. })
    ));
}

#[test]
fn diagonal_and_outer_product_are_checked_compact_matrix_operations() {
    let source = TestSource::new(
        "input Real d[3]; input Integer lhs[2]; input Real rhs[5]; diagonal(d); outerProduct(lhs, rhs)",
    );
    let at = source.source("outerProduct(lhs, rhs)", 0);
    let dae = Dae::construct(source.map, |dae| {
        let (real3, integer2, real5) = dae.types(|types| {
            Ok((
                types.derived(ValueType::array(ScalarType::Real, [3]), at)?,
                types.derived(ValueType::array(ScalarType::Integer, [2]), at)?,
                types.derived(ValueType::array(ScalarType::Real, [5]), at)?,
            ))
        })?;
        let (d, lhs, rhs) = dae.variables(|variables| {
            Ok((
                variables.input(
                    VarName::new("d"),
                    rumoca_core::InstanceId::new(1),
                    real3,
                    InputVariability::Continuous,
                    at,
                    VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("lhs"),
                    rumoca_core::InstanceId::new(2),
                    integer2,
                    InputVariability::Discrete,
                    at,
                    VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("rhs"),
                    rumoca_core::InstanceId::new(3),
                    real5,
                    InputVariability::Continuous,
                    at,
                    VariableAttributes::default(),
                )?,
            ))
        })?;
        dae.expressions(|expressions| {
            let d = expressions.at(at).coordinate(CoordinateInput::Input(d))?;
            expressions.at(at).builtin(PureBuiltin::Diagonal, [d])?;
            let lhs = expressions.at(at).coordinate(CoordinateInput::Input(lhs))?;
            let rhs = expressions.at(at).coordinate(CoordinateInput::Input(rhs))?;
            expressions
                .at(at)
                .builtin(PureBuiltin::OuterProduct, [lhs, rhs])?;
            Ok(())
        })
    })
    .expect("ARR-041/042 construct exact matrix types from compact vectors");

    let assert_layout = |view: DaeView<'_>| {
        assert_eq!(view.expression_count(), 5, "neither operation scalarizes");
        let diagonal = view.expression(view.expression_id(1).unwrap()).unwrap();
        assert_eq!(diagonal.value_type().scalar_type(), ScalarType::Real);
        assert_eq!(diagonal.value_type().dimensions(), &[3, 3]);
        assert!(matches!(
            diagonal.operation(),
            ExpressionOperation::Builtin {
                builtin: PureBuiltin::Diagonal,
                arguments,
            } if arguments.len() == 1
        ));
        let outer = view.expression(view.expression_id(4).unwrap()).unwrap();
        assert_eq!(outer.value_type().scalar_type(), ScalarType::Real);
        assert_eq!(outer.value_type().dimensions(), &[2, 5]);
        assert!(matches!(
            outer.operation(),
            ExpressionOperation::Builtin {
                builtin: PureBuiltin::OuterProduct,
                arguments,
            } if arguments.len() == 2
        ));
    };
    dae.inspect(assert_layout);

    assert_eq!(
        bincode::serialize(&PureBuiltin::Diagonal).unwrap(),
        40_u32.to_le_bytes()
    );
    assert_eq!(
        bincode::serialize(&PureBuiltin::OuterProduct).unwrap(),
        41_u32.to_le_bytes()
    );
    assert_eq!(DAE_SCHEMA_VERSION, 36);
    let json = serde_json::to_string(&dae).unwrap();
    assert!(json.contains("\"builtin\":\"diagonal\""));
    assert!(json.contains("\"builtin\":\"outer_product\""));
    let decoded: Dae = serde_json::from_str(&json).unwrap();
    decoded.inspect(assert_layout);
    let binary = bincode::serialize(&dae).unwrap();
    let decoded: Dae = bincode::deserialize(&binary).unwrap();
    decoded.inspect(assert_layout);
}

fn invalid_matrix_builtin_arity(
    builtin: PureBuiltin,
    count: usize,
) -> Result<Dae, DaeConstructionError> {
    let source = TestSource::new("builtin(A)");
    let at = source.source("builtin(A)", 0);
    Dae::construct(source.map, |dae| {
        let value =
            dae.expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(1.0)))?;
        dae.expressions(|expressions| expressions.at(at).builtin(builtin, vec![value; count]))?;
        Ok(())
    })
}

fn invalid_matrix_builtin_shape(
    builtin: PureBuiltin,
    lhs_dimensions: Vec<u32>,
    rhs_dimensions: Option<Vec<u32>>,
) -> Result<Dae, DaeConstructionError> {
    let source = TestSource::new("builtin(A, B)");
    let at = source.source("builtin(A, B)", 0);
    Dae::construct(source.map, |dae| {
        let (lhs_type, rhs_type) = dae.types(|types| {
            let lhs = types.derived(ValueType::array(ScalarType::Real, lhs_dimensions), at)?;
            let rhs = match rhs_dimensions {
                Some(dimensions) => {
                    Some(types.derived(ValueType::array(ScalarType::Real, dimensions), at)?)
                }
                None => None,
            };
            Ok((lhs, rhs))
        })?;
        let (lhs, rhs) = dae.variables(|variables| {
            let lhs = variables.input(
                VarName::new("A"),
                rumoca_core::InstanceId::new(1),
                lhs_type,
                InputVariability::Continuous,
                at,
                VariableAttributes::default(),
            )?;
            let rhs = match rhs_type {
                Some(ty) => Some(variables.input(
                    VarName::new("B"),
                    rumoca_core::InstanceId::new(2),
                    ty,
                    InputVariability::Continuous,
                    at,
                    VariableAttributes::default(),
                )?),
                None => None,
            };
            Ok((lhs, rhs))
        })?;
        dae.expressions(|expressions| {
            let lhs = expressions.at(at).coordinate(CoordinateInput::Input(lhs))?;
            let arguments = match rhs {
                Some(rhs) => vec![
                    lhs,
                    expressions.at(at).coordinate(CoordinateInput::Input(rhs))?,
                ],
                None => vec![lhs],
            };
            expressions.at(at).builtin(builtin, arguments)
        })?;
        Ok(())
    })
}

#[test]
fn diagonal_and_outer_product_reject_forged_arity_rank_and_element_type() {
    let assert_invalid_arity = |builtin, expected, count| {
        let result = invalid_matrix_builtin_arity(builtin, count);
        assert!(matches!(
            result,
            Err(DaeConstructionError::InvalidArity {
                expected: actual,
                found,
                ..
            }) if actual == expected && found == count
        ));
    };
    assert_invalid_arity(PureBuiltin::Diagonal, 1, 0);
    assert_invalid_arity(PureBuiltin::Diagonal, 1, 2);
    assert_invalid_arity(PureBuiltin::OuterProduct, 2, 0);
    assert_invalid_arity(PureBuiltin::OuterProduct, 2, 1);

    assert!(matches!(
        invalid_matrix_builtin_shape(PureBuiltin::Diagonal, vec![2, 2], None),
        Err(DaeConstructionError::ShapeMismatch { .. })
    ));
    assert!(matches!(
        invalid_matrix_builtin_shape(PureBuiltin::OuterProduct, vec![2, 2], Some(vec![3])),
        Err(DaeConstructionError::ShapeMismatch { .. })
    ));
    assert!(matches!(
        invalid_matrix_builtin_shape(PureBuiltin::OuterProduct, vec![2], Some(vec![3, 1])),
        Err(DaeConstructionError::ShapeMismatch { .. })
    ));

    let source = TestSource::new("diagonal({true, false})");
    let at = source.source("diagonal({true, false})", 0);
    let error = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let yes = expressions.at(at).literal(DaeLiteral::Boolean(true))?;
            let no = expressions.at(at).literal(DaeLiteral::Boolean(false))?;
            let vector = expressions.at(at).array([yes, no])?;
            expressions
                .at(at)
                .builtin(PureBuiltin::Diagonal, [vector])?;
            Ok(())
        })
    });
    assert!(matches!(
        error,
        Err(DaeConstructionError::ExpectedNumeric { .. })
    ));
}

#[test]
fn skew_is_one_checked_compact_real_matrix_operation_and_round_trips() {
    let source = TestSource::new("input Real x[3]; skew(x)");
    let at = source.source("skew(x)", 0);
    let dae = Dae::construct(source.map, |dae| {
        let vector =
            dae.types(|types| types.derived(ValueType::array(ScalarType::Real, [3]), at))?;
        let x = dae.variables(|variables| {
            variables.input(
                VarName::new("x"),
                rumoca_core::InstanceId::new(1),
                vector,
                InputVariability::Continuous,
                at,
                VariableAttributes::default(),
            )
        })?;
        dae.expressions(|expressions| {
            let x = expressions.at(at).coordinate(CoordinateInput::Input(x))?;
            expressions.at(at).builtin(PureBuiltin::Skew, [x])?;
            Ok(())
        })
    })
    .expect("ARR-037 constructs skew from exactly one Real 3-vector");

    let assert_skew = |view: DaeView<'_>| {
        assert_eq!(view.expression_count(), 2, "skew stores no 3x3 payload");
        let skew = view.expression(view.expression_id(1).unwrap()).unwrap();
        assert_eq!(skew.value_type().scalar_type(), ScalarType::Real);
        assert_eq!(skew.value_type().dimensions(), &[3, 3]);
        assert!(matches!(
            skew.operation(),
            ExpressionOperation::Builtin {
                builtin: PureBuiltin::Skew,
                arguments,
            } if arguments.len() == 1
        ));
    };
    dae.inspect(assert_skew);
    assert_eq!(
        bincode::serialize(&PureBuiltin::Skew).unwrap(),
        42_u32.to_le_bytes()
    );
    assert_eq!(DAE_SCHEMA_VERSION, 36);
    let json = serde_json::to_string(&dae).unwrap();
    assert!(json.contains("\"builtin\":\"skew\""));
    let decoded: Dae = serde_json::from_str(&json).unwrap();
    decoded.inspect(assert_skew);
    let binary = bincode::serialize(&dae).unwrap();
    let decoded: Dae = bincode::deserialize(&binary).unwrap();
    decoded.inspect(assert_skew);
}

#[test]
fn skew_rejects_forged_arity_rank_and_non_real_elements() {
    for count in [0, 2] {
        assert!(matches!(
            invalid_matrix_builtin_arity(PureBuiltin::Skew, count),
            Err(DaeConstructionError::InvalidArity {
                expected: 1,
                found,
                ..
            }) if found == count
        ));
    }
    for dimensions in [vec![2], vec![3, 1]] {
        assert!(matches!(
            invalid_matrix_builtin_shape(PureBuiltin::Skew, dimensions, None),
            Err(DaeConstructionError::ShapeMismatch { .. })
        ));
    }

    let invalid_elements = |literals: [DaeLiteral; 3]| {
        let source = TestSource::new("skew({a,b,c})");
        let at = source.source("skew({a,b,c})", 0);
        Dae::construct(source.map, |dae| {
            dae.expressions(|expressions| {
                let values = literals
                    .into_iter()
                    .map(|value| expressions.at(at).literal(value))
                    .collect::<Result<Vec<_>, _>>()?;
                let vector = expressions.at(at).array(values)?;
                expressions.at(at).builtin(PureBuiltin::Skew, [vector])?;
                Ok(())
            })
        })
    };
    assert!(matches!(
        invalid_elements([
            DaeLiteral::Integer(1),
            DaeLiteral::Integer(2),
            DaeLiteral::Integer(3),
        ]),
        Err(DaeConstructionError::TypeMismatch { .. })
    ));
    assert!(matches!(
        invalid_elements([
            DaeLiteral::Boolean(true),
            DaeLiteral::Boolean(false),
            DaeLiteral::Boolean(true),
        ]),
        Err(DaeConstructionError::ExpectedNumeric { .. })
    ));
}

#[test]
fn linspace_and_cross_are_checked_compact_vector_operations() {
    let source = TestSource::new("linspace(2.0, 4.0, 3); cross({1.0,2.0,3.0},{4.0,5.0,6.0})");
    let linspace_at = source.source("linspace(2.0, 4.0, 3)", 0);
    let cross_at = source.source("cross({1.0,2.0,3.0},{4.0,5.0,6.0})", 0);
    let provenances = [
        source.source("2.0", 0),
        source.source("4.0", 0),
        source.source("3", 0),
        source.source("1.0", 0),
        source.source("2.0", 1),
        source.source("3.0", 0),
        source.source("4.0", 1),
        source.source("5.0", 0),
        source.source("6.0", 0),
    ];
    let dae = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let start = expressions
                .at(provenances[0])
                .literal(DaeLiteral::Real(2.0))?;
            let stop = expressions
                .at(provenances[1])
                .literal(DaeLiteral::Real(4.0))?;
            let count = expressions
                .at(provenances[2])
                .literal(DaeLiteral::Integer(3))?;
            expressions
                .at(linspace_at)
                .builtin(PureBuiltin::Linspace, [start, stop, count])?;
            let lhs = provenances[3..6]
                .iter()
                .zip([1.0, 2.0, 3.0])
                .map(|(at, value)| expressions.at(*at).literal(DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            let lhs = expressions.at(cross_at).array(lhs)?;
            let rhs = provenances[6..]
                .iter()
                .zip([4.0, 5.0, 6.0])
                .map(|(at, value)| expressions.at(*at).literal(DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            let rhs = expressions.at(cross_at).array(rhs)?;
            expressions
                .at(cross_at)
                .builtin(PureBuiltin::Cross, [lhs, rhs])?;
            Ok(())
        })
    })
    .unwrap();

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        for (index, builtin, text) in [
            (3, PureBuiltin::Linspace, "linspace(2.0, 4.0, 3)"),
            (12, PureBuiltin::Cross, "cross({1.0,2.0,3.0},{4.0,5.0,6.0})"),
        ] {
            let expression = view.expression(view.expression_id(index).unwrap()).unwrap();
            assert_eq!(expression.value_type().dimensions(), &[3]);
            assert_eq!(view.source_text(expression.provenance()), Some(text));
            assert!(matches!(
                expression.operation(),
                ExpressionOperation::Builtin { builtin: found, .. } if found == builtin
            ));
        }
    });

    let source = TestSource::new("cross({true,true,true},{true,true,true})");
    let at = source.source("cross({true,true,true},{true,true,true})", 0);
    let rejected = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let values = (0..6)
                .map(|_| expressions.at(at).literal(DaeLiteral::Boolean(true)))
                .collect::<Result<Vec<_>, _>>()?;
            let lhs = expressions.at(at).array(values[..3].iter().copied())?;
            let rhs = expressions.at(at).array(values[3..].iter().copied())?;
            expressions.at(at).builtin(PureBuiltin::Cross, [lhs, rhs])?;
            Ok(())
        })
    });
    assert!(matches!(
        rejected,
        Err(DaeConstructionError::ExpectedNumeric { .. })
    ));
}
