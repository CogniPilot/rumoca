use super::*;

fn site() -> crate::SolvePureCallSite {
    let arithmetic = crate::SolveArithmeticProfile::construct(
        crate::SolveRealFormat::Binary64,
        crate::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
    );
    let scalar = crate::SolveScalarType::real(arithmetic);
    let vector = crate::SolveValueType::tensor(scalar, vec![14400]).unwrap();
    let real = crate::SolveValueType::scalar(scalar);
    let table = crate::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            crate::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![vector.clone(), vector.clone()],
            vec![
                crate::SolvePureCallOutput::result(vector),
                crate::SolvePureCallOutput::result(real),
            ],
            span(),
            |builder, inputs, outputs| {
                let tensor = builder.load(inputs[0], span())?;
                builder.store(outputs[0], tensor, span())?;
                let sum = builder.reduce(crate::SolveReductionOperator::Sum, tensor, span())?;
                builder.store(outputs[1], sum, span())
            },
        )?;
        Ok(())
    })
    .unwrap();
    table.owners()[0].call_site()
}

fn load(input: crate::TensorInputKind, start: Reg, input_start: usize) -> LinearOp {
    LinearOp::TensorLoad {
        dst_start: start,
        input,
        input_start,
        count: 14400,
        seed_start: Some(input_start),
        lanes: 1,
    }
}

#[test]
fn issued_per_element_and_whole_input_summaries_preserve_full_capacity() {
    let site = site();
    assert!(!site.output_dependencies()[0][0].is_whole_input());
    assert!(site.output_dependencies()[1][0].is_whole_input());
    compare_program(&[
        load(crate::TensorInputKind::Y, 0, 0),
        load(crate::TensorInputKind::P, 14400, 14400),
        LinearOp::PureCall {
            dst_start: 28800,
            input_starts: vec![0, 14400].into_boxed_slice(),
            site,
        },
        LinearOp::StoreOutputRange {
            start: 28800,
            count: 14401,
            stride: 1,
        },
    ]);
}

#[test]
fn unused_input_and_interface_errors_are_still_traversed() {
    let site = site();
    // Input1 is unused by both issued outputs; the original eager range walk
    // still validates it and must refuse its uninitialized first register.
    compare_program(&[
        load(crate::TensorInputKind::Y, 0, 0),
        LinearOp::PureCall {
            dst_start: 28800,
            input_starts: vec![0, 14400].into_boxed_slice(),
            site: site.clone(),
        },
    ]);
    compare_program(&[LinearOp::PureCall {
        dst_start: 0,
        input_starts: vec![0].into_boxed_slice(),
        site,
    }]);
}
