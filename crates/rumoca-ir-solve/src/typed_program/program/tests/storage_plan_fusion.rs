use super::*;

fn binary64() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        crate::SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
    )
}

fn real(arithmetic: SolveArithmeticProfile, dimensions: Vec<u32>) -> SolveValueType {
    SolveValueType::tensor(SolveScalarType::real(arithmetic), dimensions).unwrap()
}

/// The register operation `index` defines.
fn defined(program: &TypedProgram, index: usize) -> SolveRegisterId {
    let mut defined = None;
    program.operations()[index]
        .operation()
        .visit_output_registers(|register| defined = Some(register));
    defined.unwrap()
}

fn fused(program: &TypedProgram, index: usize) -> Option<SolveFusedRegister> {
    program.storage_plan().fusion().registers()[defined(program, index).index()]
}

/// Two read-only inputs of `extent` and one output; `body` issues the
/// operations after the two loads (operations 0 and 1) and returns the
/// value stored.
fn two_input_program(
    arithmetic: SolveArithmeticProfile,
    extent: u32,
    body: impl for<'p> FnOnce(
        &mut TypedProgramBuilder<'p>,
        ProgramRegister<'p>,
        ProgramRegister<'p>,
    ) -> Result<ProgramRegister<'p>, SolveProgramConstructionError>,
) -> TypedProgram {
    TypedProgram::construct(arithmetic, |builder| {
        let value_type = real(arithmetic, vec![extent]);
        let lhs = builder.declare_slot(
            value_type.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let rhs = builder.declare_slot(
            value_type.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(1),
        )?;
        let output = builder.declare_slot(
            value_type,
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(2),
        )?;
        let x = builder.load(lhs, span(3))?;
        let y = builder.load(rhs, span(4))?;
        let result = body(builder, x, y)?;
        builder.store(output, result, span(9))
    })
    .unwrap()
}

/// A value one later elementwise operation of its extent reads once is
/// evaluated inside that operation's loop, and its operands live until then.
#[test]
fn a_single_use_elementwise_value_is_fused_into_its_reader() {
    let program = two_input_program(binary64(), 4, |builder, x, y| {
        let product = builder.binary(SolveBinaryOperator::Multiply, x, y, span(5))?;
        builder.binary(SolveBinaryOperator::Add, product, y, span(6))
    });
    let fusion = program.storage_plan().fusion();
    assert_eq!(
        fused(&program, 2),
        Some(SolveFusedRegister {
            definition: 2,
            point: 3
        })
    );
    assert_eq!(fused(&program, 3), None, "a stored value is not fused");
    let x = defined(&program, 0);
    assert_eq!(program.register_last_reads()[x.index()], Some(2));
    assert_eq!(fusion.last_reads()[x.index()], Some(3));
    // Fusion is a refinement: the register placement is unchanged.
    assert_eq!(
        program.storage_plan().registers()[defined(&program, 2).index()],
        SolveRegisterPlacement::Frame { extent: 4 }
    );
}

/// `shared = x op y`, read by two values that one final sum reads.
fn shared_operand_program(operator: SolveBinaryOperator) -> TypedProgram {
    two_input_program(binary64(), 4, move |builder, x, y| {
        let shared = builder.binary(operator, x, y, span(5))?;
        let left = builder.binary(SolveBinaryOperator::Add, shared, x, span(6))?;
        let right = builder.binary(SolveBinaryOperator::Multiply, shared, y, span(7))?;
        builder.binary(SolveBinaryOperator::Add, left, right, span(8))
    })
}

/// A value read twice is stored unless it is cheap and every read is inside
/// the one loop that evaluates them all.
#[test]
fn a_value_read_twice_is_recomputed_only_when_cheap() {
    let divided = shared_operand_program(SolveBinaryOperator::Divide);
    assert_eq!(fused(&divided, 2), None, "a division is not recomputed");
    for reader in [3, 4] {
        assert_eq!(
            fused(&divided, reader),
            Some(SolveFusedRegister {
                definition: reader,
                point: 5
            })
        );
    }
    let multiplied = shared_operand_program(SolveBinaryOperator::Multiply);
    assert_eq!(
        fused(&multiplied, 2),
        Some(SolveFusedRegister {
            definition: 2,
            point: 5
        })
    );
}

/// A single element gains nothing from fusion.
#[test]
fn a_single_element_value_is_not_fused() {
    let program = two_input_program(binary64(), 1, |builder, x, y| {
        let product = builder.binary(SolveBinaryOperator::Multiply, x, y, span(5))?;
        builder.binary(SolveBinaryOperator::Add, product, y, span(6))
    });
    assert!(
        program
            .storage_plan()
            .fusion()
            .registers()
            .iter()
            .all(Option::is_none)
    );
}

/// A reduction reads its operand element by element, in order.
#[test]
fn a_reduced_value_is_fused_into_the_reduction() {
    let program = TypedProgram::construct(binary64(), |builder| {
        let arithmetic = binary64();
        let input = builder.declare_slot(
            real(arithmetic, vec![4]),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let output = builder.declare_slot(
            SolveValueType::scalar(SolveScalarType::real(arithmetic)),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(1),
        )?;
        let x = builder.load(input, span(2))?;
        let squared = builder.binary(SolveBinaryOperator::Multiply, x, x, span(3))?;
        let sum = builder.reduce(SolveReductionOperator::Sum, squared, span(4))?;
        builder.store(output, sum, span(5))
    })
    .unwrap();
    assert_eq!(
        fused(&program, 1),
        Some(SolveFusedRegister {
            definition: 1,
            point: 2
        })
    );
}

/// A transpose read once by a binary64 product above the static bound is
/// read in place by the product; at or below the bound it is stored.
#[test]
fn a_transpose_is_fused_only_into_a_host_kernel_product() {
    let product = |extent: u32| {
        TypedProgram::construct(binary64(), |builder| {
            let matrix = real(binary64(), vec![extent, extent]);
            let lhs = builder.declare_slot(
                matrix.clone(),
                SolveStorageClass::Input,
                SolveSlotAccess::ReadOnly,
                span(0),
            )?;
            let rhs = builder.declare_slot(
                matrix.clone(),
                SolveStorageClass::Input,
                SolveSlotAccess::ReadOnly,
                span(1),
            )?;
            let output = builder.declare_slot(
                matrix,
                SolveStorageClass::Output,
                SolveSlotAccess::ReadWrite,
                span(2),
            )?;
            let a = builder.load(lhs, span(3))?;
            let b = builder.load(rhs, span(4))?;
            let transposed = builder.transpose(a, span(5))?;
            let product = builder.matrix_multiply(transposed, b, span(6))?;
            builder.store(output, product, span(7))
        })
        .unwrap()
    };
    // 4 × 4 × 4 = 64 is the bound itself.
    assert_eq!(SOLVE_STATIC_MATRIX_WORK, 64);
    let at_bound = product(4);
    assert_eq!(fused(&at_bound, 2), None);
    let above = product(5);
    assert_eq!(
        fused(&above, 2),
        Some(SolveFusedRegister {
            definition: 2,
            point: 3
        })
    );
    let a = defined(&above, 0);
    assert_eq!(
        above.storage_plan().fusion().last_reads()[a.index()],
        Some(3)
    );
}
