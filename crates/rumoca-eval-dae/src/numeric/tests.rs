use rumoca_core::{
    ClockLattice, ClockRational, SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain,
    VarName,
};
use rumoca_ir_dae::{
    BinaryOperator, CoordinateInput, Dae, DaeConstructionError, DaeLiteral, DaeProvenance, ExprId,
    ExpressionOperation, Expressions, PureBuiltin, ScalarType, Subscript, ValueType,
};

use super::NumericEvaluator;

#[test]
fn integer_builtin_floors_negative_fractional_dae_values() {
    let mut source_map = SourceMap::new();
    let source = source_map.add("integer.mo", "integer(-1.8)");
    let at = DaeProvenance::source(Span::from_offsets(source, 0, 13)).unwrap();
    let dae = Dae::construct(source_map, |dae| {
        dae.expressions(|expressions| {
            let argument = expressions.at(at).literal(DaeLiteral::Real(-1.8))?;
            expressions
                .at(at)
                .builtin(PureBuiltin::Integer, [argument])?;
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        let integer = view.expression_id(1).unwrap();
        assert_eq!(
            NumericEvaluator::new(view).expression(integer).unwrap(),
            [-2.0]
        );
    });
}

fn real_literals<'dae>(
    expressions: &mut Expressions<'_, 'dae>,
    at: DaeProvenance,
    values: impl IntoIterator<Item = i32>,
) -> Result<Vec<ExprId<'dae>>, DaeConstructionError> {
    values
        .into_iter()
        .map(|value| {
            expressions
                .at(at)
                .literal(DaeLiteral::Real(f64::from(value)))
        })
        .collect()
}

#[test]
fn array_update_evaluates_whole_and_slice_selections_row_major() {
    let text = "matrix[:, 2] := {10, 20}; matrix[2, {1, 3}] := {40, 60}";
    let mut source_map = SourceMap::new();
    let source = source_map.add("array_update.mo", text);
    let at = DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let dae = Dae::construct(source_map, |dae| {
        dae.expressions(|expressions| {
            let values = real_literals(expressions, at, 1..=6)?;
            let rows = values
                .chunks_exact(3)
                .map(|row| expressions.at(at).array(row.iter().copied()))
                .collect::<Result<Vec<_>, _>>()?;
            let matrix = expressions.at(at).array(rows)?;

            let two = expressions.at(at).literal(DaeLiteral::Integer(2))?;
            expressions.at(at).index(
                matrix,
                [
                    Subscript::Index {
                        expression: two,
                        provenance: at,
                    },
                    Subscript::Whole { provenance: at },
                ],
            )?;
            let column_values = real_literals(expressions, at, [10, 20])?;
            let column_values = expressions.at(at).array(column_values)?;
            let column_update = expressions.at(at).array_update(
                matrix,
                column_values,
                [
                    Subscript::Whole { provenance: at },
                    Subscript::Index {
                        expression: two,
                        provenance: at,
                    },
                ],
            )?;

            let columns = [1, 3]
                .into_iter()
                .map(|value| expressions.at(at).literal(DaeLiteral::Integer(value)))
                .collect::<Result<Vec<_>, _>>()?;
            let columns = expressions.at(at).array(columns)?;
            let row_values = real_literals(expressions, at, [40, 60])?;
            let row_values = expressions.at(at).array(row_values)?;
            expressions.at(at).array_update(
                column_update,
                row_values,
                [
                    Subscript::Index {
                        expression: two,
                        provenance: at,
                    },
                    Subscript::Slice {
                        expression: columns,
                        provenance: at,
                    },
                ],
            )?;
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        let selection = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    ExpressionOperation::Index { .. }
                )
            })
            .unwrap();
        assert_eq!(
            NumericEvaluator::new(view).expression(selection).unwrap(),
            [4.0, 5.0, 6.0]
        );
        let update = view.expression_id(view.expression_count() - 1).unwrap();
        assert_eq!(
            NumericEvaluator::new(view).expression(update).unwrap(),
            [1.0, 10.0, 3.0, 40.0, 20.0, 60.0]
        );
    });
}

#[test]
fn periodic_clock_interval_evaluates_to_its_exact_period() {
    let mut source_map = SourceMap::new();
    let source = source_map.add("interval.mo", "Clock(0.1); interval()");
    let clock_at = DaeProvenance::source(Span::from_offsets(source, 0, 10)).unwrap();
    let interval_at = DaeProvenance::source(Span::from_offsets(source, 12, 22)).unwrap();
    let dae = Dae::construct(source_map, |dae| {
        let clock = dae.clocks(|clocks| {
            clocks.periodic(
                ClockLattice::new(ClockRational::new(1, 10).unwrap(), ClockRational::ZERO).unwrap(),
                clock_at,
            )
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(interval_at)
                .coordinate(CoordinateInput::ClockInterval(clock))
                .map(|_| ())
        })
    })
    .unwrap();
    dae.inspect(|view| {
        assert_eq!(
            NumericEvaluator::new(view)
                .expression(view.expression_id(0).unwrap())
                .unwrap(),
            vec![0.1]
        );
    });
}

#[test]
fn checked_quotients_preserve_modelica_sign_semantics() {
    let text = "div(-7, 3); mod(-7, 3); rem(-7, 3)";
    let mut source_map = SourceMap::new();
    let source = source_map.add("mod.mo", text);
    let at = |start, end| DaeProvenance::source(Span::from_offsets(source, start, end)).unwrap();
    let dae = Dae::construct(source_map, |dae| {
        let minus_seven = dae
            .expressions(|expressions| expressions.at(at(4, 6)).literal(DaeLiteral::Integer(-7)))?;
        let three = dae
            .expressions(|expressions| expressions.at(at(8, 9)).literal(DaeLiteral::Integer(3)))?;
        dae.expressions(|expressions| {
            expressions
                .at(at(0, 10))
                .builtin(PureBuiltin::Div, [minus_seven, three])
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(at(12, 22))
                .builtin(PureBuiltin::Mod, [minus_seven, three])
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(at(24, text.len()))
                .builtin(PureBuiltin::Rem, [minus_seven, three])
        })?;
        Ok(())
    })
    .unwrap();
    dae.inspect(|view| {
        let mut evaluator = NumericEvaluator::new(view);
        assert_eq!(
            evaluator
                .expression(view.expression_id(2).unwrap())
                .unwrap(),
            vec![-2.0]
        );
        assert_eq!(
            evaluator
                .expression(view.expression_id(3).unwrap())
                .unwrap(),
            vec![2.0]
        );
        assert_eq!(
            evaluator
                .expression(view.expression_id(4).unwrap())
                .unwrap(),
            vec![-1.0]
        );
    });
}

#[test]
fn checked_zeros_evaluates_to_its_constructor_derived_shape() {
    let text = "zeros(2, 3)";
    let mut source_map = SourceMap::new();
    let source = source_map.add("zeros.mo", text);
    let at = |start, end| DaeProvenance::source(Span::from_offsets(source, start, end)).unwrap();
    let dae = Dae::construct(source_map, |dae| {
        let two = dae
            .expressions(|expressions| expressions.at(at(6, 7)).literal(DaeLiteral::Integer(2)))?;
        let three = dae
            .expressions(|expressions| expressions.at(at(9, 10)).literal(DaeLiteral::Integer(3)))?;
        dae.expressions(|expressions| {
            expressions
                .at(at(0, text.len()))
                .builtin(PureBuiltin::Zeros, [two, three])
        })?;
        Ok(())
    })
    .unwrap();

    dae.inspect(|view| {
        let zeros = view.expression_id(2).unwrap();
        assert!(matches!(
            view.expression(zeros).unwrap().operation(),
            ExpressionOperation::Builtin {
                builtin: PureBuiltin::Zeros,
                ..
            }
        ));
        assert_eq!(
            NumericEvaluator::new(view).expression(zeros).unwrap(),
            vec![0.0; 6]
        );
    });
}

#[test]
fn checked_identity_derives_only_its_requested_scalar_view() {
    let text = "identity(3)";
    let mut source_map = SourceMap::new();
    let source = source_map.add("identity.mo", text);
    let at = DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let dae = Dae::construct(source_map, |dae| {
        let extent =
            dae.expressions(|expressions| expressions.at(at).literal(DaeLiteral::Integer(3)))?;
        dae.expressions(|expressions| expressions.at(at).builtin(PureBuiltin::Identity, [extent]))?;
        Ok(())
    })
    .unwrap();

    dae.inspect(|view| {
        let identity = view.expression_id(1).unwrap();
        assert_eq!(
            NumericEvaluator::new(view).expression(identity).unwrap(),
            vec![1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0]
        );
    });
}

#[test]
fn checked_vector_reuses_the_compact_operand_row_major_values() {
    let text = "vector([{{1.0},{2.0},{3.0}}])";
    let mut source_map = SourceMap::new();
    let source = source_map.add("vector.mo", text);
    let at = DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let dae = Dae::construct(source_map, |dae| {
        dae.expressions(|expressions| {
            let values = [1.0, 2.0, 3.0]
                .into_iter()
                .map(|value| expressions.at(at).literal(DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            let columns = values
                .into_iter()
                .map(|value| expressions.at(at).array([value]))
                .collect::<Result<Vec<_>, _>>()?;
            let row = expressions.at(at).array(columns)?;
            let tensor = expressions.at(at).array([row])?;
            expressions.at(at).builtin(PureBuiltin::Vector, [tensor])?;
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        let vector = view.expression_id(8).unwrap();
        let node = view.expression(vector).unwrap();
        assert_eq!(node.value_type().dimensions(), &[3]);
        assert_eq!(
            NumericEvaluator::new(view).expression(vector).unwrap(),
            [1.0, 2.0, 3.0]
        );
    });
}

#[test]
fn checked_transpose_permutes_nonsquare_and_rank_three_row_major_values() {
    let text = "transpose([1,2,3;4,5,6]); transpose(tensor)";
    let mut source_map = SourceMap::new();
    let source = source_map.add("transpose.mo", text);
    let at = DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let dae = Dae::construct(source_map, |dae| {
        dae.expressions(|expressions| {
            let matrix_values = real_literals(expressions, at, 1..=6)?;
            let matrix_rows = matrix_values
                .chunks_exact(3)
                .map(|row| expressions.at(at).array(row.iter().copied()))
                .collect::<Result<Vec<_>, _>>()?;
            let matrix = expressions.at(at).array(matrix_rows)?;
            expressions
                .at(at)
                .builtin(PureBuiltin::Transpose, [matrix])?;

            let tensor_values = real_literals(expressions, at, 1..=12)?;
            let vectors = tensor_values
                .chunks_exact(2)
                .map(|values| expressions.at(at).array(values.iter().copied()))
                .collect::<Result<Vec<_>, _>>()?;
            let matrices = vectors
                .chunks_exact(3)
                .map(|rows| expressions.at(at).array(rows.iter().copied()))
                .collect::<Result<Vec<_>, _>>()?;
            let tensor = expressions.at(at).array(matrices)?;
            expressions
                .at(at)
                .builtin(PureBuiltin::Transpose, [tensor])?;
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        let transposes = (0..view.expression_count())
            .filter_map(|index| {
                let id = view.expression_id(index).unwrap();
                matches!(
                    view.expression(id).unwrap().operation(),
                    ExpressionOperation::Builtin {
                        builtin: PureBuiltin::Transpose,
                        ..
                    }
                )
                .then_some(id)
            })
            .collect::<Vec<_>>();
        let mut evaluator = NumericEvaluator::new(view);
        assert_eq!(
            evaluator.expression(transposes[0]).unwrap(),
            [1.0, 4.0, 2.0, 5.0, 3.0, 6.0]
        );
        assert_eq!(
            evaluator.expression(transposes[1]).unwrap(),
            [
                1.0, 2.0, 7.0, 8.0, 3.0, 4.0, 9.0, 10.0, 5.0, 6.0, 11.0, 12.0
            ]
        );
    });
}

#[test]
fn checked_diagonal_and_outer_product_evaluate_compact_operands_row_major() {
    let text = "diagonal({2.0,3.0}); outerProduct({1.0,2.0},{4.0,5.0})";
    let mut source_map = SourceMap::new();
    let source = source_map.add("matrix_products.mo", text);
    let at = DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let dae = Dae::construct(source_map, |dae| {
        dae.expressions(|expressions| {
            let diagonal_values = [2.0, 3.0]
                .into_iter()
                .map(|value| expressions.at(at).literal(DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            let diagonal_values = expressions.at(at).array(diagonal_values)?;
            expressions
                .at(at)
                .builtin(PureBuiltin::Diagonal, [diagonal_values])?;
            let lhs = [1.0, 2.0]
                .into_iter()
                .map(|value| expressions.at(at).literal(DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            let lhs = expressions.at(at).array(lhs)?;
            let rhs = [4.0, 5.0]
                .into_iter()
                .map(|value| expressions.at(at).literal(DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            let rhs = expressions.at(at).array(rhs)?;
            expressions
                .at(at)
                .builtin(PureBuiltin::OuterProduct, [lhs, rhs])?;
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        let builtins = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    ExpressionOperation::Builtin {
                        builtin: PureBuiltin::Diagonal | PureBuiltin::OuterProduct,
                        ..
                    }
                )
            })
            .collect::<Vec<_>>();
        let mut evaluator = NumericEvaluator::new(view);
        assert_eq!(
            evaluator.expression(builtins[0]).unwrap(),
            [2.0, 0.0, 0.0, 3.0]
        );
        assert_eq!(
            evaluator.expression(builtins[1]).unwrap(),
            [4.0, 5.0, 8.0, 10.0]
        );
    });
}

#[test]
fn checked_skew_evaluates_one_compact_real_three_vector_row_major() {
    let text = "skew({1.0,2.0,3.0})";
    let mut source_map = SourceMap::new();
    let source = source_map.add("skew.mo", text);
    let at = DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let dae = Dae::construct(source_map, |dae| {
        dae.expressions(|expressions| {
            let values = [1.0, 2.0, 3.0]
                .into_iter()
                .map(|value| expressions.at(at).literal(DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            let vector = expressions.at(at).array(values)?;
            expressions.at(at).builtin(PureBuiltin::Skew, [vector])?;
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        let skew = view.expression_id(4).unwrap();
        assert_eq!(
            NumericEvaluator::new(view).expression(skew).unwrap(),
            [0.0, -3.0, 2.0, 3.0, 0.0, -1.0, -2.0, 1.0, 0.0]
        );
    });
}

#[test]
fn promoted_concatenation_evaluates_in_result_row_major_order() {
    let text = "[a, b]";
    let mut source_map = SourceMap::new();
    let source = source_map.add("cat.mo", text);
    let at = DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let dae = Dae::construct(source_map, |dae| {
        dae.expressions(|expressions| {
            let one = expressions.at(at).literal(DaeLiteral::Real(1.0))?;
            let two = expressions.at(at).literal(DaeLiteral::Real(2.0))?;
            let three = expressions.at(at).literal(DaeLiteral::Real(3.0))?;
            let four = expressions.at(at).literal(DaeLiteral::Real(4.0))?;
            let a = expressions.at(at).array([one, two])?;
            let b = expressions.at(at).array([three, four])?;
            expressions
                .at(at)
                .builtin(PureBuiltin::PromotedCat2, [a, b])?;
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        let concatenation = view.expression_id(6).unwrap();
        assert_eq!(
            NumericEvaluator::new(view)
                .expression(concatenation)
                .unwrap(),
            vec![1.0, 3.0, 2.0, 4.0]
        );
    });
}

#[test]
fn checked_ones_and_fill_evaluate_without_materialized_dae_arrays() {
    let text = "ones(2); fill(0.5, 3)";
    let mut source_map = SourceMap::new();
    let source = source_map.add("constructors.mo", text);
    let at = |needle: &str, occurrence: usize| {
        let start = text.match_indices(needle).nth(occurrence).unwrap().0;
        DaeProvenance::source(Span::from_offsets(source, start, start + needle.len())).unwrap()
    };
    let dae = Dae::construct(source_map, |dae| {
        let two = dae.expressions(|expressions| {
            expressions.at(at("2", 0)).literal(DaeLiteral::Integer(2))
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(at("ones(2)", 0))
                .builtin(PureBuiltin::Ones, [two])
        })?;
        let value = dae.expressions(|expressions| {
            expressions.at(at("0.5", 0)).literal(DaeLiteral::Real(0.5))
        })?;
        let three = dae.expressions(|expressions| {
            expressions.at(at("3", 0)).literal(DaeLiteral::Integer(3))
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(at("fill(0.5, 3)", 0))
                .builtin(PureBuiltin::Fill, [value, three])
        })?;
        Ok(())
    })
    .unwrap();

    dae.inspect(|view| {
        let mut evaluator = NumericEvaluator::new(view);
        assert_eq!(
            evaluator
                .expression(view.expression_id(1).unwrap())
                .unwrap(),
            vec![1.0, 1.0]
        );
        assert_eq!(
            evaluator
                .expression(view.expression_id(4).unwrap())
                .unwrap(),
            vec![0.5, 0.5, 0.5]
        );
    });
}

#[test]
fn checked_linspace_and_cross_evaluate_their_vector_semantics() {
    let text = "linspace(0.0, 2.0, 3); cross({1.0,0.0,0.0},{0.0,1.0,0.0})";
    let mut source_map = SourceMap::new();
    let source = source_map.add("vectors.mo", text);
    let at = |needle: &str, occurrence: usize| {
        let start = text.match_indices(needle).nth(occurrence).unwrap().0;
        DaeProvenance::source(Span::from_offsets(source, start, start + needle.len())).unwrap()
    };
    let dae = Dae::construct(source_map, |dae| {
        dae.expressions(|expressions| {
            let start = expressions
                .at(at("0.0", 0))
                .literal(DaeLiteral::Real(0.0))?;
            let stop = expressions
                .at(at("2.0", 0))
                .literal(DaeLiteral::Real(2.0))?;
            let count = expressions.at(at("3", 0)).literal(DaeLiteral::Integer(3))?;
            expressions
                .at(at("linspace(0.0, 2.0, 3)", 0))
                .builtin(PureBuiltin::Linspace, [start, stop, count])?;
            let lhs_values = [
                expressions
                    .at(at("1.0", 0))
                    .literal(DaeLiteral::Real(1.0))?,
                expressions
                    .at(at("0.0", 1))
                    .literal(DaeLiteral::Real(0.0))?,
                expressions
                    .at(at("0.0", 2))
                    .literal(DaeLiteral::Real(0.0))?,
            ];
            let lhs = expressions.at(at("{1.0,0.0,0.0}", 0)).array(lhs_values)?;
            let rhs_values = [
                expressions
                    .at(at("0.0", 3))
                    .literal(DaeLiteral::Real(0.0))?,
                expressions
                    .at(at("1.0", 1))
                    .literal(DaeLiteral::Real(1.0))?,
                expressions
                    .at(at("0.0", 4))
                    .literal(DaeLiteral::Real(0.0))?,
            ];
            let rhs = expressions.at(at("{0.0,1.0,0.0}", 0)).array(rhs_values)?;
            expressions
                .at(at("cross({1.0,0.0,0.0},{0.0,1.0,0.0})", 0))
                .builtin(PureBuiltin::Cross, [lhs, rhs])?;
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        let mut evaluator = NumericEvaluator::new(view);
        assert_eq!(
            evaluator
                .expression(view.expression_id(3).unwrap())
                .unwrap(),
            vec![0.0, 1.0, 2.0]
        );
        assert_eq!(
            evaluator
                .expression(view.expression_id(12).unwrap())
                .unwrap(),
            vec![0.0, 0.0, 1.0]
        );
    });
}

#[test]
fn zero_cardinality_variables_have_the_unique_empty_value() {
    let text = "parameter Real p[0]; Real z[0];";
    let mut source_map = SourceMap::new();
    let source = source_map.add("empty.mo", text);
    let at = |needle: &str| {
        let start = text.find(needle).unwrap();
        DaeProvenance::source(Span::from_offsets(source, start, start + needle.len())).unwrap()
    };
    let dae = Dae::construct(source_map, |dae| {
        let empty = dae.types(|types| {
            types.derived(ValueType::array(ScalarType::Real, [0]), at("Real p[0]"))
        })?;
        dae.variables(|variables| {
            variables.parameter(
                VarName::new("p"),
                empty,
                at("parameter Real p[0]"),
                rumoca_ir_dae::VariableAttributes::default(),
            )?;
            variables.algebraic(
                VarName::new("z"),
                empty,
                at("Real z[0]"),
                rumoca_ir_dae::VariableAttributes::default(),
            )?;
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        let mut evaluator = NumericEvaluator::new(view);
        for index in 0..2 {
            let variable = view.variable_id(index).unwrap();
            assert_eq!(
                evaluator.initial_value(variable).unwrap(),
                Vec::<f64>::new()
            );
        }
    });
}

#[test]
fn runtime_inputs_require_an_override_or_use_their_checked_default() {
    let text = "input Real defaulted = 2.5; input Real supplied;";
    let mut source_map = SourceMap::new();
    let source = source_map.add("inputs.mo", text);
    let at = |needle: &str| {
        let start = text.find(needle).unwrap();
        DaeProvenance::source(Span::from_offsets(source, start, start + needle.len())).unwrap()
    };
    let dae = Dae::construct(source_map, |dae| {
        let real = dae.types(|types| {
            types.derived(ValueType::scalar(ScalarType::Real), at("Real defaulted"))
        })?;
        let default = dae
            .expressions(|expressions| expressions.at(at("2.5")).literal(DaeLiteral::Real(2.5)))?;
        dae.variables(|variables| {
            variables.input(
                VarName::new("defaulted"),
                real,
                rumoca_ir_dae::InputVariability::Continuous,
                at("input Real defaulted"),
                rumoca_ir_dae::VariableAttributes {
                    binding: Some(default),
                    causality: rumoca_ir_dae::VariableCausality::Input,
                    ..Default::default()
                },
            )?;
            variables.input(
                VarName::new("supplied"),
                real,
                rumoca_ir_dae::InputVariability::Continuous,
                at("input Real supplied"),
                rumoca_ir_dae::VariableAttributes {
                    causality: rumoca_ir_dae::VariableCausality::Input,
                    ..Default::default()
                },
            )?;
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        let defaulted = view.variable_id(0).unwrap();
        let supplied = view.variable_id(1).unwrap();
        let mut without_provider = NumericEvaluator::new(view);
        assert_eq!(
            without_provider.initial_value(defaulted).unwrap(),
            vec![2.5]
        );
        assert_eq!(
            without_provider.initial_value(supplied).unwrap_err().kind(),
            super::NumericEvaluationErrorKind::MissingValue
        );

        let mut with_provider =
            NumericEvaluator::with_overrides(view, |variable, _| match variable.name().as_str() {
                "defaulted" => Some(3.5),
                "supplied" => Some(4.5),
                _ => None,
            });
        assert_eq!(with_provider.initial_value(defaulted).unwrap(), vec![3.5]);
        assert_eq!(with_provider.initial_value(supplied).unwrap(), vec![4.5]);
    });
}

#[test]
fn nested_comprehensions_evaluate_with_lexically_scoped_binders() {
    let text = "{{i + j for j in 1:3} for i in 1:2}";
    let mut source_map = SourceMap::new();
    let source = source_map.add("nested.mo", text);
    let at = |needle: &str, occurrence: usize| {
        let start = text.match_indices(needle).nth(occurrence).unwrap().0;
        DaeProvenance::source(Span::from_offsets(source, start, start + needle.len())).unwrap()
    };
    let singleton_domain = |name: &str, upper| StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: name.to_string(),
            lower: 1,
            upper,
            step: 1,
        }],
    };
    let dae = Dae::construct(source_map, |dae| {
        let outer =
            dae.domains(|domains| domains.structured(singleton_domain("i", 2), at("1:2", 0)))?;
        let i = dae.domains(|domains| domains.binder(outer, 0, at("i", 0)))?;
        let inner = dae.domains(|domains| {
            domains.nested_in_scope([i], singleton_domain("j", 3), at("1:3", 0))
        })?;
        let j = dae.domains(|domains| domains.binder(inner, 0, at("j", 0)))?;
        dae.expressions(|expressions| {
            let i = expressions.at(at("i", 0)).binder(i)?;
            let j = expressions.at(at("j", 0)).binder(j)?;
            let sum = expressions
                .at(at("i + j", 0))
                .binary(BinaryOperator::Add, i, j)?;
            let inner = expressions
                .at(at("{i + j for j in 1:3}", 0))
                .comprehension(inner, sum)?;
            expressions.at(at(text, 0)).comprehension(outer, inner)?;
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        let nested = view.expression_id(4).unwrap();
        assert_eq!(
            NumericEvaluator::new(view).expression(nested).unwrap(),
            vec![2.0, 3.0, 4.0, 3.0, 4.0, 5.0]
        );
    });
}

#[test]
fn function_assertions_execute_for_each_call_without_numeric_messages() {
    let text = "function checked input Boolean ok; output Real y; algorithm \
                assert(ok, \"message\"); y := 1.0; end checked; \
                checked(true); checked(false)";
    let mut source_map = SourceMap::new();
    let source = source_map.add("checked.mo", text);
    let at = |needle: &str, occurrence: usize| {
        let start = text.match_indices(needle).nth(occurrence).unwrap().0;
        DaeProvenance::source(Span::from_offsets(source, start, start + needle.len())).unwrap()
    };
    let assertion_at = at("assert(ok, \"message\")", 0);
    let dae = Dae::construct(source_map, |dae| {
        let boolean = dae.types(|types| {
            types.derived(ValueType::scalar(ScalarType::Boolean), at("Boolean", 0))
        })?;
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at("Real", 0)))?;
        let function = construct_asserting_function(dae, boolean, real, at)?;
        let true_value = dae.expressions(|expressions| {
            expressions
                .at(at("true", 0))
                .literal(DaeLiteral::Boolean(true))
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(at("checked(true)", 0))
                .call(function, 0, [true_value])
        })?;
        let false_value = dae.expressions(|expressions| {
            expressions
                .at(at("false", 0))
                .literal(DaeLiteral::Boolean(false))
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(at("checked(false)", 0))
                .call(function, 0, [false_value])
        })?;
        Ok(())
    })
    .unwrap();

    dae.inspect(|view| {
        let calls = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    ExpressionOperation::Call { .. }
                )
            })
            .collect::<Vec<_>>();
        assert_eq!(calls.len(), 2);
        let mut evaluator = NumericEvaluator::new(view);
        assert_eq!(evaluator.expression(calls[0]).unwrap(), vec![1.0]);
        let error = evaluator.expression(calls[1]).unwrap_err();
        assert_eq!(
            error.kind(),
            super::NumericEvaluationErrorKind::AssertionFailed
        );
        assert_eq!(error.span(), assertion_at.span());
    });
}

fn construct_asserting_function<'dae>(
    dae: &mut rumoca_ir_dae::DaeConstruction<'dae>,
    boolean: rumoca_ir_dae::ValueTypeId<'dae>,
    real: rumoca_ir_dae::ValueTypeId<'dae>,
    at: impl Copy + Fn(&str, usize) -> DaeProvenance,
) -> Result<rumoca_ir_dae::FunctionId<'dae>, rumoca_ir_dae::DaeConstructionError> {
    let signature = rumoca_ir_dae::FunctionSignature::new(
        VarName::new("checked"),
        [boolean],
        [real],
        at("function checked", 0),
    );
    dae.function(signature, |dae, reservation| {
        let input = dae.functions(|functions| {
            functions.parameter(&reservation, VarName::new("ok"), 0, at("Boolean ok", 0))
        })?;
        let output = dae.functions(|functions| {
            functions.output(&reservation, VarName::new("y"), 0, at("Real y", 0))
        })?;
        let mut body =
            dae.functions(|functions| functions.begin(reservation, at("function checked", 0)))?;
        let condition =
            dae.expressions(|expressions| expressions.at(at("ok", 1)).function_parameter(input))?;
        let message = dae.expressions(|expressions| {
            expressions
                .at(at("\"message\"", 0))
                .literal(DaeLiteral::String("message".to_string()))
        })?;
        dae.functions(|functions| {
            functions.assertion(
                &mut body,
                condition,
                message,
                at("assert(ok, \"message\")", 0),
            )
        })?;
        let one = dae.expressions(|expressions| {
            expressions.at(at("1.0", 0)).literal(DaeLiteral::Real(1.0))
        })?;
        dae.functions(|functions| functions.assign(&mut body, output, one, at("y := 1.0", 0)))?;
        dae.functions(|functions| functions.define(body, at("function checked", 0)))
    })
    .map(|(function, ())| function)
}

#[test]
fn checked_function_fold_evaluates_its_compact_transition() {
    let text = "function sum3 output Integer y; algorithm y := 0; \
                for k in 1:3 loop y := y + k; end for; end sum3; sum3()";
    let mut source_map = SourceMap::new();
    let source = source_map.add("sum3.mo", text);
    let at = |needle: &str, occurrence: usize| {
        let start = text.match_indices(needle).nth(occurrence).unwrap().0;
        DaeProvenance::source(Span::from_offsets(source, start, start + needle.len())).unwrap()
    };
    let dae = Dae::construct(source_map, |dae| {
        let integer = dae.types(|types| {
            types.derived(ValueType::scalar(ScalarType::Integer), at("Integer", 0))
        })?;
        let function = construct_sum3_function(dae, integer, at)?;
        dae.expressions(|expressions| expressions.at(at("sum3()", 0)).call(function, 0, []))?;
        Ok(())
    })
    .unwrap();

    dae.inspect(|view| {
        let call = view.expression_id(view.expression_count() - 1).unwrap();
        assert_eq!(
            NumericEvaluator::new(view).expression(call).unwrap(),
            vec![6.0]
        );
    });
}

fn construct_sum3_function<'dae>(
    dae: &mut rumoca_ir_dae::DaeConstruction<'dae>,
    integer: rumoca_ir_dae::ValueTypeId<'dae>,
    at: impl Copy + Fn(&str, usize) -> DaeProvenance,
) -> Result<rumoca_ir_dae::FunctionId<'dae>, rumoca_ir_dae::DaeConstructionError> {
    let signature = rumoca_ir_dae::FunctionSignature::new(
        VarName::new("sum3"),
        [],
        [integer],
        at("function sum3", 0),
    );
    dae.function(signature, |dae, reservation| {
        let output = dae.functions(|functions| {
            functions.output(&reservation, VarName::new("y"), 0, at("Integer y", 0))
        })?;
        let mut body =
            dae.functions(|functions| functions.begin(reservation, at("function sum3", 0)))?;
        let zero = dae.expressions(|expressions| {
            expressions.at(at("0", 0)).literal(DaeLiteral::Integer(0))
        })?;
        dae.functions(|functions| functions.assign(&mut body, output, zero, at("y := 0", 0)))?;
        let domain = dae.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        id: 0,
                        display_name: "k".to_string(),
                        lower: 1,
                        upper: 3,
                        step: 1,
                    }],
                },
                at("1:3", 0),
            )
        })?;
        let binder = dae.domains(|domains| domains.binder(domain, 0, at("k", 0)))?;
        let mut loop_body = dae.functions(|functions| {
            functions.begin_loop(
                body,
                domain,
                [output],
                at("for k in 1:3 loop y := y + k; end for", 0),
            )
        })?;
        let current =
            dae.functions(|functions| functions.read(loop_body.body(), output, at("y", 3)))?;
        let k = dae.expressions(|expressions| expressions.at(at("k", 1)).binder(binder))?;
        let update = dae.expressions(|expressions| {
            expressions
                .at(at("y + k", 0))
                .binary(BinaryOperator::Add, current, k)
        })?;
        dae.functions(|functions| {
            functions.assign_loop(&mut loop_body, output, update, at("y := y + k", 0))
        })?;
        let body = dae.functions(|functions| {
            functions.finish_loop(loop_body, at("for k in 1:3 loop y := y + k; end for", 0))
        })?;
        dae.functions(|functions| functions.define(body, at("function sum3", 0)))
    })
    .map(|(function, ())| function)
}
