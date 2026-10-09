//! Nested lowerers borrow the enclosing function's issued metadata inventory.

use super::*;
use std::sync::Arc;

#[test]
fn nested_region_lowerers_share_one_immutable_owner_context() {
    let mut sources = SourceMap::new();
    let source = sources.add("shared_context.mo", "model Shared end Shared;");
    let at = Span::from_offsets(source, 0, 23);
    let model = dae::Dae::construct(sources, |_| Ok(())).unwrap();
    model.inspect(|view| {
        let context = RegionContext {
            view,
            callees: Default::default(),
            predicate_ranges: Default::default(),
            conditional_groups: Default::default(),
            assertion_slots: Arc::from([]),
            direct_assertion_count: 0,
            direct_assertions: std::sync::Arc::from([]),
            assertion_output_base: 0,
            loop_statements: Default::default(),
        };
        let environment = EnvironmentLayout {
            model_coordinates: vec![],
            parameters: vec![],
            values: vec![],
            expressions: vec![],
            fold_parameters: vec![],
            binders: vec![],
        };
        let value_type =
            solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic_profile()));
        let mut table = solve::SolvePureCallTable::builder(arithmetic_profile());
        table
            .add_owner(
                solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
                vec![value_type.clone()],
                vec![solve::SolvePureCallOutput::result(value_type)],
                at,
                |builder, inputs, outputs| {
                    let mut nested = context.clone();
                    for _ in 0..32 {
                        let lowerer = load_region_lowerer(builder, &[], &environment, &nested, at)?;
                        assert!(Arc::ptr_eq(
                            &context.conditional_groups,
                            &lowerer.conditional_groups
                        ));
                        assert!(Arc::ptr_eq(&context.callees, &lowerer.callees));
                        assert!(Arc::ptr_eq(
                            &context.predicate_ranges,
                            &lowerer.predicate_ranges
                        ));
                        nested = lowerer.region_context();
                    }
                    let value = builder.load(inputs[0], at)?;
                    builder.store(outputs[0], value, at)
                },
            )
            .unwrap();
    });
}
