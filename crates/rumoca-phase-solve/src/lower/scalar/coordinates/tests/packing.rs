//! Whole TensorLoad/register/cache equality against the original range loop.
use super::*;

fn model() -> dae::Dae {
    let mut sources = rumoca_core::SourceMap::new();
    let source = sources.add("tensor_load.mo", "Real x[14400]; parameter Real p[14400];");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 38)).unwrap();
    dae::Dae::construct(sources, |model| {
        let vector = model.types(|types| {
            types.derived(dae::ValueType::array(dae::ScalarType::Real, [14400]), at)
        })?;
        let (state, parameter) = model.variables(|variables| {
            Ok((
                variables.state(
                    rumoca_core::VarName::new("x"),
                    vector,
                    at,
                    Default::default(),
                )?,
                variables.parameter(
                    rumoca_core::VarName::new("p"),
                    vector,
                    at,
                    dae::VariableAttributes {
                        is_tunable: true,
                        ..Default::default()
                    },
                )?,
            ))
        })?;
        model.expressions(|expressions| {
            expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(state))?;
            expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(parameter))?;
            expressions
                .at(at)
                .coordinate(dae::CoordinateInput::PreState(state))?;
            Ok(())
        })
    })
    .unwrap()
}

fn reference_pack<'dae>(
    compiler: &mut ScalarCompiler<'_, 'dae>,
    expression: dae::ExprId<'dae>,
    coordinate: dae::CoordinateView<'dae>,
    span: Span,
) -> solve::Reg {
    let (variable, pre_variable) = if let Some(variable) = coordinate_variable(coordinate) {
        (variable, false)
    } else {
        (pre_coordinate_variable(coordinate).unwrap(), true)
    };
    let sampled_base = if !pre_variable && compiler.sampled_source {
        compiler.layout.pre_variables[variable as usize]
    } else {
        None
    };
    let first = compiler
        .coordinate_scalar_slot(variable, 0, pre_variable, sampled_base, span)
        .unwrap();
    let (input, input_start) = match first {
        solve::ScalarSlot::Y { index, .. } => (solve::TensorInputKind::Y, index),
        solve::ScalarSlot::P { index, .. } => (solve::TensorInputKind::P, index),
        _ => unreachable!(),
    };
    original_loop(
        compiler,
        CoordinateExtent {
            variable,
            count: 14400,
            pre_variable,
            sampled_base,
            input_start,
        },
        span,
    )
    .unwrap();
    let dst_start = compiler.next_register;
    for _ in 0..14400 {
        compiler.register(span).unwrap();
    }
    compiler
        .emit(solve::LinearOp::TensorLoad {
            dst_start,
            input,
            input_start,
            count: 14400,
            seed_start: None,
            lanes: 1,
        })
        .unwrap();
    compiler
        .tensor_load_cache
        .insert((compiler.context_id, expression), (dst_start, 14400));
    dst_start
}

#[test]
fn full_capacity_tensor_load_program_and_cache_equal_original_loop() {
    let model = model();
    model.inspect(|view| {
        let mut layout = crate::layout::lower_layout(view).unwrap();
        layout.pre_variables[0] = Some(14400);
        compare_expressions(view, &layout);
    });
}

fn compare_expressions<'dae>(view: dae::DaeView<'dae>, layout: &LoweredLayout<'dae>) {
    for sampled in [false, true] {
        compare_sampled(view, layout, sampled);
    }
}

fn compare_sampled<'dae>(view: dae::DaeView<'dae>, layout: &LoweredLayout<'dae>, sampled: bool) {
    for index in 0..view.expression_count() {
        let expression = view.expression_id(index).unwrap();
        let node = view.expression(expression).unwrap();
        let dae::ExpressionOperation::Coordinate(coordinate) = node.operation() else {
            unreachable!()
        };
        let span = node.provenance().span();
        let mut actual = ScalarCompiler::new(view, layout, None);
        actual.sampled_source = sampled;
        let mut reference = ScalarCompiler::new(view, layout, None);
        reference.sampled_source = sampled;
        let expected = reference_pack(&mut reference, expression, coordinate, span);
        assert_eq!(
            actual
                .pack_coordinate(expression, coordinate, span)
                .unwrap(),
            Some(expected)
        );
        assert_eq!(actual.ops, reference.ops);
        assert_eq!(actual.next_register, reference.next_register);
        assert_eq!(actual.tensor_load_cache, reference.tensor_load_cache);
        let before = actual.ops.clone();
        assert_eq!(
            actual
                .pack_coordinate(expression, coordinate, span)
                .unwrap(),
            Some(expected)
        );
        assert_eq!(actual.ops, before);
        assert_eq!(actual.next_register, 14400);
    }
}

#[test]
fn register_overflow_does_not_publish_tensor_load_or_cache() {
    let model = model();
    model.inspect(|view| {
        let layout = crate::layout::lower_layout(view).unwrap();
        let mut compiler = ScalarCompiler::new(view, &layout, None);
        let expression = view.expression_id(0).unwrap();
        let node = view.expression(expression).unwrap();
        let dae::ExpressionOperation::Coordinate(coordinate) = node.operation() else {
            unreachable!()
        };
        compiler.next_register = solve::Reg::MAX - 1;
        let error = compiler
            .pack_coordinate(expression, coordinate, node.provenance().span())
            .unwrap_err();
        assert!(error.to_string().contains("Solve register index overflow"));
        assert_eq!(error.source_span(), Some(node.provenance().span()));
        assert_eq!(compiler.next_register, solve::Reg::MAX - 1);
        assert!(compiler.ops.is_empty());
        assert!(compiler.tensor_load_cache.is_empty());
    });
}
