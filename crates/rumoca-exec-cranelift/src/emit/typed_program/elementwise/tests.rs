//! A fused broadcast `Map` (a row repeated over a domain) read by a binary64
//! elementwise loop: an even-width row is read two lanes at a time, an
//! odd-width row keeps the scalar loop. Both must give the interpreter's bits.

use super::super::*;

fn at() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("broadcast_pairs.mo"),
        0,
        1,
    )
}

/// `x .+ broadcast(row)` with `x` of `rows × width` and `row` of `width`.
fn broadcast_sum_table(rows: u32, width: u32) -> solve::SolvePureCallTable {
    let profile = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let real = solve::SolveScalarType::real(profile);
    let matrix = solve::SolveValueType::tensor(real, vec![rows, width]).unwrap();
    let row = solve::SolveValueType::tensor(real, vec![width]).unwrap();
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: i64::from(rows),
            step: 1,
        }],
    };
    solve::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(971).unwrap()),
            vec![matrix.clone(), row.clone()],
            vec![solve::SolvePureCallOutput::result(matrix)],
            at(),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], at())?;
                let bias = b.load(inputs[1], at())?;
                let repeated = b.map(domain, &[bias], row, at(), |r, captures, _, output| {
                    let value = r.load(captures[0], at())?;
                    r.store(output, value, at())
                })?;
                let sum = b.binary(solve::SolveBinaryOperator::Add, x, repeated, at())?;
                b.store(outputs[0], sum, at())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn a_fused_broadcast_row_gives_the_interpreters_bits_at_even_and_odd_widths() {
    use rumoca_eval_solve::{PureCallInvocation, TypedValue, eval_pure_call};
    for (rows, width) in [(3u32, 4u32), (5, 8), (4, 3), (7, 5)] {
        let table = broadcast_sum_table(rows, width);
        let owner = &table.owners()[0];
        let x = (0..rows * width)
            .map(|i| [1e16, -0.0, 0.25, -3.5, 1e-300][i as usize % 5] * f64::from(i + 1))
            .collect::<Vec<_>>();
        let bias = (0..width)
            .map(|j| [-1e16, 0.0, -0.0, 2.5][j as usize % 4])
            .collect::<Vec<_>>();
        let arguments = owner
            .inputs()
            .iter()
            .zip([&x, &bias])
            .map(|(ty, cells)| {
                let kinds = cells
                    .iter()
                    .map(|v| solve::SolveValueKind::Real64(v.to_bits()));
                TypedValue::construct(ty.clone(), kinds.collect()).unwrap()
            })
            .collect::<Vec<_>>();
        let expected = eval_pure_call(&table, owner.id(), &arguments).unwrap()[0]
            .elements()
            .iter()
            .map(|cell| match cell {
                solve::SolveValueKind::Real64(bits) => *bits,
                _ => panic!("expected binary64"),
            })
            .collect::<Vec<_>>();
        let input = x
            .iter()
            .chain(&bias)
            .map(|v| v.to_bits())
            .collect::<Vec<_>>();
        let mut output = vec![0; (rows * width) as usize];
        CompiledPureCallTable::compile(&table)
            .unwrap()
            .call_cells(
                PureCallInvocation::Primal(&owner.call_site()),
                &input,
                &mut output,
            )
            .unwrap();
        assert_eq!(output, expected, "{rows} x {width}");
    }
}
