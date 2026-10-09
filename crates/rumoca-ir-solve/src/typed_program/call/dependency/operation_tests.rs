use super::*;
use crate::{
    ProgramSlot, SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat, SolveScalarType,
    SolveSlotAccess, SolveStorageClass, SolveValue, TypedProgramBuilder,
};
use rumoca_core::{SourceId, StructuredIndexBinder, StructuredIndexDomain};
use std::collections::BTreeSet;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("whole_union.alg"), 0, 1)
}

fn profile() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL)
}

fn copy_outputs<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    inputs: &[ProgramSlot<'program>],
    outputs: &[ProgramSlot<'program>],
) -> Result<(), SolveProgramConstructionError> {
    for (&input, &output) in inputs.iter().zip(outputs) {
        let value = builder.load(input, span())?;
        builder.store(output, value, span())?;
    }
    Ok(())
}

fn constant_boolean_output<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    _inputs: &[ProgramSlot<'program>],
    outputs: &[ProgramSlot<'program>],
) -> Result<(), SolveProgramConstructionError> {
    let value = builder.constant(SolveValue::boolean(false), span())?;
    builder.store(outputs[0], value, span())
}

fn domain() -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: 3,
            step: 1,
        }],
    }
}

fn mixed_region(fold: bool) -> TypedProgram {
    let arithmetic = profile();
    let types = vec![
        SolveValueType::scalar(SolveScalarType::Boolean),
        SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3]).unwrap(),
        SolveValueType::scalar(SolveScalarType::integer(arithmetic)),
        SolveValueType::tensor(SolveScalarType::Boolean, vec![2, 2]).unwrap(),
    ];
    TypedProgram::construct(arithmetic, |builder| {
        let mut values = Vec::new();
        for value_type in &types {
            let slot = builder.declare_slot(
                value_type.clone(),
                SolveStorageClass::Input,
                SolveSlotAccess::ReadOnly,
                span(),
            )?;
            values.push(builder.load(slot, span())?);
        }
        if fold {
            builder.fold(
                domain(),
                &[values[1], values[2], values[3], values[1]],
                &[values[1], values[0], values[0]],
                span(),
                |region, carried, _captures, _binders, outputs| {
                    copy_outputs(region, carried, outputs)
                },
            )?;
        } else {
            let captures = [values[1], values[2], values[3], values[0], values[1]];
            let outputs = vec![
                types[1].clone(),
                types[2].clone(),
                types[3].clone(),
                types[0].clone(),
                types[1].clone(),
            ];
            builder.conditional(
                values[0],
                &captures,
                outputs,
                span(),
                copy_outputs,
                copy_outputs,
            )?;
        }
        Ok(())
    })
    .unwrap()
}

fn source_summaries(
    program: &TypedProgram,
    input: usize,
    shift: usize,
    active: bool,
) -> Vec<SolveCallDependency> {
    if !active {
        return Vec::new();
    }
    let first = (input + shift) % 7;
    let second = (first + 1) % 7;
    let coordinate = SolveCallDependency {
        input: first,
        coordinates: Some(Coordinates::identity(
            program.register_types()[input].dimensions().len(),
        )),
    };
    let mut sources = vec![
        coordinate.clone(),
        SolveCallDependency::whole(second),
        coordinate,
    ];
    if shift >= 7 {
        sources.push(SolveCallDependency::whole(first));
    }
    sources
}

// The oracle takes the checked region's explicit operand lists, independently
// of the production visitor, access classification and dependency accumulator.
fn compare_with_whole_oracle(program: &TypedProgram) {
    let operation = program.operations().last().unwrap().operation();
    let (inputs, outputs) = match operation {
        SolveOperation::Conditional {
            condition,
            captures,
            destinations,
            ..
        } => (
            std::iter::once(*condition)
                .chain(captures.iter().copied())
                .collect::<Vec<_>>(),
            destinations,
        ),
        SolveOperation::Fold {
            initial,
            captures,
            destinations,
            ..
        } => (
            initial.iter().chain(captures.iter()).copied().collect(),
            destinations,
        ),
        _ => panic!("checked conditional or fold expected"),
    };
    let unique: BTreeSet<_> = inputs.iter().map(|input| input.index()).collect();
    for mask in 0..(1 << unique.len()) {
        for shift in 0..14 {
            let mut registers =
                vec![vec![SolveCallDependency::whole(99)]; program.register_types().len()];
            for (ordinal, &input) in unique.iter().enumerate() {
                registers[input] =
                    source_summaries(program, input, shift, mask & (1 << ordinal) != 0);
            }
            let expected = inputs
                .iter()
                .flat_map(|input| registers[input.index()].iter())
                .map(|source| source.input)
                .collect::<BTreeSet<_>>()
                .into_iter()
                .map(SolveCallDependency::whole)
                .collect::<Vec<_>>();
            let before = registers.clone();
            operations::derive(program, operation, &mut registers, span()).unwrap();
            for output in outputs.iter() {
                assert_eq!(
                    registers[output.index()],
                    expected,
                    "mask {mask}, shift {shift}"
                );
            }
            for &input in &unique {
                assert_eq!(
                    registers[input], before[input],
                    "operand summaries must survive every output"
                );
            }
        }
    }
}

#[test]
fn conditional_whole_union_matches_oracle_for_mixed_ranks_and_repeated_captures() {
    compare_with_whole_oracle(&mixed_region(false));
}

#[test]
fn fold_whole_union_matches_oracle_for_mixed_ranks_and_overlapping_initial_captures() {
    compare_with_whole_oracle(&mixed_region(true));
}

#[test]
fn whole_regions_admit_no_input_slots_or_captures() {
    for fold in [false, true] {
        let program = TypedProgram::construct(profile(), |builder| {
            let initial = builder.constant(SolveValue::boolean(true), span())?;
            if fold {
                builder.fold(
                    domain(),
                    &[initial],
                    &[],
                    span(),
                    |region, carried, _, _, outputs| copy_outputs(region, carried, outputs),
                )?;
            } else {
                builder.conditional(
                    initial,
                    &[],
                    vec![SolveValueType::scalar(SolveScalarType::Boolean)],
                    span(),
                    constant_boolean_output,
                    constant_boolean_output,
                )?;
            }
            Ok(())
        })
        .unwrap();
        assert!(program.slots().is_empty());
        let operation = program.operations().last().unwrap().operation();
        let mut registers = vec![Vec::new(); program.register_types().len()];
        operations::derive(&program, operation, &mut registers, span()).unwrap();
        assert!(registers.iter().all(Vec::is_empty));
    }
}

#[test]
fn whole_regions_refuse_empty_output_tuples_before_building_regions() {
    for fold in [false, true] {
        let result = empty_output_region(fold);
        let expected = if fold {
            SolveProgramConstructionError::InvalidFold { provenance: span() }
        } else {
            SolveProgramConstructionError::InvalidRegion { provenance: span() }
        };
        assert_eq!(result.unwrap_err(), expected);
    }
}

fn empty_output_region(fold: bool) -> Result<TypedProgram, SolveProgramConstructionError> {
    TypedProgram::construct(profile(), |builder| {
        let condition = builder.constant(SolveValue::boolean(true), span())?;
        if fold {
            builder.fold(domain(), &[], &[], span(), |_, _, _, _, _| {
                panic!("invalid fold must fail first")
            })?;
        } else {
            builder.conditional(
                condition,
                &[],
                Vec::new(),
                span(),
                |_, _, _| panic!("invalid conditional must fail first"),
                |_, _, _| panic!("invalid conditional must fail first"),
            )?;
        }
        Ok(())
    })
}
