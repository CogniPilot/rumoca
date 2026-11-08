//! Call-level printing consumes the checked block's own issued table.

use std::num::NonZeroU64;

use rumoca_core::{SourceId, SourceMap, Span};
use rumoca_ir_solve as solve;

fn span(index: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("call_levels.solve"),
        index,
        index + 1,
    )
}

/// Edges point to earlier owners, and methods call the selected roots.
fn block(edges: &[&[usize]], roots: &[usize]) -> solve::SolveAlgorithmBlock {
    let profile = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary32,
        solve::SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
    );
    let mut table = solve::SolvePureCallTable::builder(profile);
    let mut ids = Vec::new();
    for (index, callees) in edges.iter().enumerate() {
        let id = table
            .add_owner(
                solve::SolvePureCallIdentity::issued(NonZeroU64::new(index as u64 + 1).unwrap()),
                Vec::new(),
                vec![solve::SolvePureCallOutput::result(
                    solve::SolveValueType::scalar(solve::SolveScalarType::real(profile)),
                )],
                span(index + 1),
                |builder, _, outputs| {
                    for &callee in *callees {
                        builder.call(ids[callee], &[], span(index + 1))?;
                    }
                    let zero =
                        builder.constant(solve::SolveValue::real(profile, 0.0), span(index + 1))?;
                    builder.store(outputs[0], zero, span(index + 1))
                },
            )
            .expect("acyclic owner");
        ids.push(id);
    }
    let table = table.finish();
    let functions = solve::SolveBlockFunctions {
        status_bindings: table
            .owners()
            .iter()
            .map(|owner| solve::SolveStatusBinding::new(owner.id(), Vec::new()))
            .collect(),
        sources: ids
            .iter()
            .enumerate()
            .map(|(index, _)| {
                solve::SolveAlgorithmCodeRef::construct(
                    solve::SolveAlgorithmCodeEntityKind::Function,
                    format!("function{index}"),
                    span(index + 1),
                )
            })
            .collect(),
        table,
    };
    solve::SolveAlgorithmBlock::construct_with_functions(
        profile,
        Vec::new(),
        functions,
        |_, _| Ok(Vec::new()),
        |_, _| Ok(Vec::new()),
        |builder, _| {
            for &root in roots {
                builder.call(ids[root], &[], span(edges.len() + 1))?;
            }
            Ok(Vec::new())
        },
    )
    .expect("checked block")
}

fn check(edges: &[&[usize]], roots: &[usize], expected: &[usize]) {
    let block = block(edges, roots);
    let issued = block.call_levels();
    assert_eq!(issued.levels(), expected);
    let view = super::solve_block_value(&block, &SourceMap::new()).unwrap();
    let json = serde_json::to_value(view).unwrap();
    assert_eq!(json["call_levels"], serde_json::json!(expected));
    assert_eq!(json["function_levels"], issued.deepest());
}

#[test]
fn nested_call_chain_prints_issued_levels() {
    check(&[&[], &[0], &[1]], &[2], &[3, 2, 1]);
}

#[test]
fn diamond_uses_the_deepest_path_to_a_shared_callee() {
    check(&[&[], &[0], &[0], &[1, 2]], &[3], &[3, 2, 2, 1]);
}

#[test]
fn unequal_paths_keep_the_deepest_level_of_a_shared_callee() {
    check(&[&[], &[0], &[0, 1]], &[2], &[3, 2, 1]);
}

#[test]
fn an_unreachable_owner_stays_at_level_one() {
    check(&[&[], &[0], &[]], &[1], &[2, 1, 1]);
}

#[test]
fn no_functions_prints_no_function_level() {
    check(&[], &[], &[]);
}
