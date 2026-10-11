//! One checked coordinate cannot reuse a register from the other read lane.

use super::*;
mod lazy;

fn alias_model() -> (dae::Dae, Span) {
    let mut sources = rumoca_core::SourceMap::new();
    let source = sources.add(
        "current_definition.mo",
        "Real alias; discrete Real producer; alias=producer;",
    );
    let span = Span::from_offsets(source, 0, 49);
    let at = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let real = model.types(|types| {
            types.intern(
                rumoca_core::TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                at,
            )
        })?;
        let (alias, producer) = model.variables(|variables| {
            let alias = variables.algebraic(
                rumoca_core::VarName::new("alias"),
                real,
                at,
                dae::VariableAttributes::default(),
            )?;
            let producer = variables.discrete_real(
                rumoca_core::VarName::new("producer"),
                real,
                at,
                dae::VariableAttributes::default(),
            )?;
            Ok((alias, producer))
        })?;
        let residual = model.expressions(|expressions| {
            let alias = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(alias))?;
            let producer = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::DiscreteReal(producer))?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, alias, producer)
        })?;
        model.continuous(|continuous| continuous.value_equation(at, residual))
    })
    .unwrap();
    (model, span)
}

#[test]
fn same_expression_entry_current_entry_reads_keep_distinct_registers() {
    let (model, span) = alias_model();
    model.inspect(|view| {
        let dae::ContinuousOwnerView::Residual { equation, .. } =
            view.continuous_owners().next().unwrap()
        else {
            panic!("residual owner");
        };
        let dae::ExpressionOperation::Binary {
            lhs: alias,
            rhs: producer,
            ..
        } = view.expression(equation.residual()).unwrap().operation()
        else {
            panic!("defining equation");
        };
        let dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteReal(producer)) =
            view.expression(producer).unwrap().operation()
        else {
            panic!("issued producer");
        };
        let producer = dae::VariableId::from(producer).index();
        let plan = rumoca_phase_structural::CausalDiscretePlan::derive(view).unwrap();
        let definitions = Rc::new(rumoca_phase_structural::SameTickDefinitions::derive(
            view, &plan,
        ));
        let layout = crate::layout::lower_layout(view).unwrap();
        let mut compiler = ScalarCompiler::new(view, &layout, None)
            .with_same_tick_definitions(definitions, Rc::new(BTreeSet::from([producer])));
        let entry = compiler.expression(alias, 0).unwrap();
        compiler
            .ops
            .push(solve::LinearOp::StoreOutput { src: entry });
        compiler.project_current_definitions = true;
        compiler.enter_context(ScalarContextFrame::CurrentDefinitions {
            parent: compiler.context_id,
        });
        let current = compiler.expression(alias, 0).unwrap();
        compiler
            .ops
            .push(solve::LinearOp::StoreOutput { src: current });
        compiler.project_current_definitions = false;
        compiler.enter_context(ScalarContextFrame::EntryDefinitions {
            parent: compiler.context_id,
        });
        let next_entry = compiler.expression(alias, 0).unwrap();
        compiler
            .ops
            .push(solve::LinearOp::StoreOutput { src: next_entry });
        let block = solve::ScalarProgramBlock::with_source_span(vec![compiler.ops], span).unwrap();
        let mut actual = [0.0; 3];
        rumoca_eval_solve::eval_scalar_program_block(
            &block,
            &[7.0],
            &[3.0],
            0.0,
            None,
            &mut actual,
        )
        .unwrap();
        assert_eq!(actual, [7.0, 3.0, 7.0]);
    });
}
