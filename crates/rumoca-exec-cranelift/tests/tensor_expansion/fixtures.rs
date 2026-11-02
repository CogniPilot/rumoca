use rumoca_core::{IndexBinderProvenance, Span, StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_solve::*;
use std::num::NonZeroU64;

pub fn span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("tensor_expansion_fixture.mo"),
        1,
        2,
    )
}

pub fn arithmetic() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL)
}

pub fn tensor(shape: Vec<u32>) -> SolveValueType {
    SolveValueType::tensor(SolveScalarType::real(arithmetic()), shape).unwrap()
}

fn axis(n: u32) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: i64::from(n),
            step: 1,
        }],
    }
}

fn table(
    inputs: Vec<SolveValueType>,
    output: SolveValueType,
    build: impl for<'p> FnOnce(
        &mut TypedProgramBuilder<'p>,
        &[ProgramSlot<'p>],
        &[ProgramSlot<'p>],
    ) -> Result<(), SolveProgramConstructionError>,
) -> SolvePureCallTable {
    SolvePureCallTable::construct(arithmetic(), |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            inputs,
            vec![SolvePureCallOutput::result(output)],
            span(),
            build,
        )?;
        Ok(())
    })
    .unwrap()
}

pub struct TypedFixture {
    pub table: SolvePureCallTable,
    pub input: Vec<f64>,
    pub expected: Vec<f64>,
}

pub fn literal(n: u32) -> TypedFixture {
    let at = span();
    TypedFixture {
        table: table(vec![], tensor(vec![n]), |b, _, outputs| {
            let values = (0..n)
                .map(|i| b.constant(SolveValue::real(arithmetic(), f64::from(i) * 0.5), at))
                .collect::<Result<Vec<_>, _>>()?;
            let aggregate = b.construct_aggregate(&values, vec![n], at)?;
            b.store(outputs[0], aggregate, at)
        }),
        input: vec![],
        expected: (0..n).map(|i| f64::from(i) * 0.5).collect(),
    }
}

pub fn map(n: u32) -> TypedFixture {
    let at = span();
    TypedFixture {
        table: table(
            vec![tensor(vec![n, 32])],
            tensor(vec![n]),
            |b, inputs, outputs| {
                let matrix = b.load(inputs[0], at)?;
                let mapped = b.map(
                    axis(n),
                    &[matrix],
                    SolveValueType::scalar(SolveScalarType::real(arithmetic())),
                    at,
                    |r, captures, _, output| {
                        let matrix = r.load(captures[0], at)?;
                        let first = r.project_element(matrix, vec![0, 0], at)?;
                        r.store(output, first, at)
                    },
                )?;
                b.store(outputs[0], mapped, at)
            },
        ),
        input: (0..n * 32).map(|i| 0.25 + f64::from(i)).collect(),
        expected: vec![0.25; n as usize],
    }
}

pub fn fold(n: u32) -> TypedFixture {
    let at = span();
    TypedFixture {
        table: table(
            vec![tensor(vec![n])],
            tensor(vec![n]),
            |b, inputs, outputs| {
                let vector = b.load(inputs[0], at)?;
                let folded = b.fold(
                    axis(n),
                    &[vector],
                    &[],
                    at,
                    |r, carried, _, binders, next| {
                        let vector = r.load(carried[0], at)?;
                        let index = r.load(binders[0], at)?;
                        let one = r.constant(SolveValue::real(arithmetic(), 1.0), at)?;
                        let updated = r.update_element(vector, one, &[index], at)?;
                        r.store(next[0], updated, at)
                    },
                )?;
                b.store(outputs[0], folded[0], at)
            },
        ),
        input: vec![-0.0; n as usize],
        expected: vec![1.0; n as usize],
    }
}

pub fn row(n: u32, stride: usize) -> ScalarProgramBlock {
    let count = n as usize;
    ScalarProgramBlock::with_output_indices(
        vec![vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::Y,
                input_start: 0,
                count: count * stride,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 0,
                count,
                stride,
            },
        ]],
        vec![span()],
        (0..count).collect(),
    )
    .unwrap()
}

pub fn small_product() -> TypedFixture {
    let at = span();
    TypedFixture {
        table: table(
            vec![tensor(vec![17, 1]), tensor(vec![1, 3])],
            tensor(vec![17, 3]),
            |b, inputs, outputs| {
                let lhs = b.load(inputs[0], at)?;
                let rhs = b.load(inputs[1], at)?;
                let product = b.matrix_multiply(lhs, rhs, at)?;
                b.store(outputs[0], product, at)
            },
        ),
        input: vec![1.0; 20],
        expected: vec![1.0; 51],
    }
}

pub fn fused(n: u32) -> TypedFixture {
    let at = span();
    TypedFixture {
        table: table(
            vec![tensor(vec![n]); 3],
            tensor(vec![n]),
            |b, inputs, outputs| {
                let a = b.load(inputs[0], at)?;
                let b_value = b.load(inputs[1], at)?;
                let c = b.load(inputs[2], at)?;
                let first = b.binary(SolveBinaryOperator::Add, a, b_value, at)?;
                let second = b.binary(SolveBinaryOperator::Add, first, c, at)?;
                b.store(outputs[0], second, at)
            },
        ),
        input: [
            vec![1e16; n as usize],
            vec![1.0; n as usize],
            vec![-1e16; n as usize],
        ]
        .concat(),
        expected: vec![0.0; n as usize],
    }
}
