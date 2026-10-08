//! Exact reference-loop controls for the private coordinate extent certificate.
use super::*;
use crate::layout::{StorageClass, VariableSlot};
mod packing;

fn with_compiler(test: impl FnOnce(&mut ScalarCompiler<'_, '_>, Span)) {
    with_layout(
        VariableSlot {
            storage: StorageClass::Y,
            base: 0,
            count: 14400,
        },
        Some(19),
        test,
    );
}

fn with_layout(
    slot: VariableSlot,
    pre: Option<usize>,
    test: impl FnOnce(&mut ScalarCompiler<'_, '_>, Span),
) {
    let mut sources = rumoca_core::SourceMap::new();
    let source = sources.add("coordinate_extent.mo", "Real x[14400];");
    let span = Span::from_offsets(source, 0, 14);
    let model = dae::Dae::construct(sources, |_| Ok(())).unwrap();
    model.inspect(|view| {
        let mut layout = crate::layout::lower_layout(view).unwrap();
        layout.variables.push(slot);
        layout.pre_variables.push(pre);
        test(&mut ScalarCompiler::new(view, &layout, None), span);
    });
}

// Kept literally equivalent to the production pre-optimization loop.
fn original_loop(
    compiler: &ScalarCompiler<'_, '_>,
    extent: CoordinateExtent,
    span: Span,
) -> Result<(), LowerError> {
    let first = compiler.coordinate_scalar_slot(
        extent.variable,
        0,
        extent.pre_variable,
        extent.sampled_base,
        span,
    )?;
    let (input, input_start) = match first {
        solve::ScalarSlot::Y { index, .. } => (solve::TensorInputKind::Y, index),
        solve::ScalarSlot::P { index, .. } => (solve::TensorInputKind::P, index),
        _ => unreachable!(),
    };
    for scalar in 1..extent.count {
        let expected = input_start.checked_add(scalar).ok_or_else(|| {
            LowerError::contract("tensor coordinate storage extent overflow", span)
        })?;
        let slot = compiler.coordinate_scalar_slot(
            extent.variable,
            scalar,
            extent.pre_variable,
            extent.sampled_base,
            span,
        )?;
        let contiguous = match slot {
            solve::ScalarSlot::Y { index, .. } => {
                input == solve::TensorInputKind::Y && index == expected
            }
            solve::ScalarSlot::P { index, .. } => {
                input == solve::TensorInputKind::P && index == expected
            }
            _ => false,
        };
        assert!(contiguous);
    }
    Ok(())
}

fn optimized(
    compiler: &ScalarCompiler<'_, '_>,
    mut extent: CoordinateExtent,
    span: Span,
) -> Result<(), LowerError> {
    let first = compiler.coordinate_scalar_slot(
        extent.variable,
        0,
        extent.pre_variable,
        extent.sampled_base,
        span,
    )?;
    extent.input_start = match first {
        solve::ScalarSlot::Y { index, .. } | solve::ScalarSlot::P { index, .. } => index,
        _ => unreachable!(),
    };
    compiler.validate_coordinate_extent(extent, span)
}

fn compare(compiler: &ScalarCompiler<'_, '_>, extent: CoordinateExtent, span: Span) {
    let reference = original_loop(compiler, extent, span);
    let actual = optimized(compiler, extent, span);
    match (reference, actual) {
        (Ok(()), Ok(())) => {}
        (Err(reference), Err(actual)) => {
            assert_eq!(reference.code(), actual.code());
            assert_eq!(reference.to_string(), actual.to_string());
            assert_eq!(reference.source_span(), actual.source_span());
        }
        other => panic!("range/reference divergence: {other:?}"),
    }
}

#[test]
fn full_capacity_ordinary_pre_and_sampled_ranges_match() {
    with_compiler(|compiler, span| {
        for pre_variable in [false, true] {
            for sampled_base in [None, Some(37)] {
                compare(
                    compiler,
                    CoordinateExtent {
                        variable: 0,
                        count: 14400,
                        pre_variable,
                        sampled_base,
                        input_start: 0,
                    },
                    span,
                );
            }
        }
    });
}

#[test]
fn missing_variable_and_sampled_exemption_match() {
    with_compiler(|compiler, span| {
        for pre_variable in [false, true] {
            for sampled_base in [None, Some(usize::MAX - 2), Some(23)] {
                compare(
                    compiler,
                    CoordinateExtent {
                        variable: 99,
                        count: 7,
                        pre_variable,
                        sampled_base,
                        input_start: 0,
                    },
                    span,
                );
            }
        }
    });
}

#[test]
fn register_ranges_are_dense_and_overflow_leaves_the_file_unchanged() {
    with_compiler(|compiler, span| {
        for expected in 0..14400 {
            assert_eq!(compiler.register(span).unwrap(), expected);
        }
        assert_eq!(compiler.register_range(5, span).unwrap(), 14400);
        assert_eq!(compiler.next_register, 14405);
        compiler.next_register = solve::Reg::MAX;
        let error = compiler.register(span).unwrap_err();
        assert!(error.to_string().contains("Solve register index overflow"));
        assert_eq!(error.source_span(), Some(span));
        assert_eq!(compiler.next_register, solve::Reg::MAX);
    });
}

fn compare_extents(slot: VariableSlot, pre: Option<usize>) {
    with_layout(slot, pre, |compiler, span| {
        for count in [2, 3, 7, 14400] {
            compare_selections(compiler, count, span);
        }
        assert!(compiler.ops.is_empty());
        assert!(compiler.tensor_load_cache.is_empty());
        assert_eq!(compiler.next_register, 0);
    });
}

fn compare_selections(compiler: &ScalarCompiler<'_, '_>, count: usize, span: Span) {
    for pre_variable in [false, true] {
        for sampled_base in [None, Some(23), Some(usize::MAX - 2)] {
            compare(
                compiler,
                CoordinateExtent {
                    variable: 0,
                    count,
                    pre_variable,
                    sampled_base,
                    input_start: 0,
                },
                span,
            );
        }
    }
}

#[test]
fn y_p_short_extreme_and_missing_pre_layouts_match() {
    for storage in [StorageClass::Y, StorageClass::P] {
        compare_storage(storage);
    }
}

fn compare_storage(storage: StorageClass) {
    for base in [0, 17, usize::MAX / 8 + 1, usize::MAX - 2, usize::MAX] {
        compare_boundaries(storage, base);
    }
}

fn compare_boundaries(storage: StorageClass, base: usize) {
    for count in [0, 1, 2, 7, 14400] {
        let slot = VariableSlot {
            storage,
            base,
            count,
        };
        compare_extents(slot, None);
        compare_extents(slot, Some(base));
    }
}

#[test]
fn saturated_byte_offsets_are_not_newly_refused() {
    let base = usize::MAX / 8 + 1;
    with_layout(
        VariableSlot {
            storage: StorageClass::Y,
            base,
            count: 14400,
        },
        Some(base),
        |compiler, span| {
            let first = compiler
                .coordinate_scalar_slot(0, 0, false, None, span)
                .unwrap();
            assert_eq!(
                first,
                solve::ScalarSlot::Y {
                    index: base,
                    byte_offset: usize::MAX
                }
            );
            compare(
                compiler,
                CoordinateExtent {
                    variable: 0,
                    count: 14400,
                    pre_variable: false,
                    sampled_base: None,
                    input_start: base,
                },
                span,
            );
        },
    );
}

#[test]
fn a_range_past_the_register_budget_is_refused_before_it_is_recorded() {
    with_compiler(|compiler, span| {
        compiler.ledger = register_ledger::RegisterLedger::with_budget(100);
        assert_eq!(compiler.register_range(60, span).unwrap(), 0);
        let error = compiler.register_range(41, span).unwrap_err();
        assert!(matches!(error, LowerError::BudgetExceeded { .. }));
        assert_eq!(
            error.code(),
            crate::diagnostic_codes::EL006_SOLVE_RESOURCE_BUDGET
        );
        assert_eq!(error.source_span(), Some(span));
        let message = error.to_string();
        assert!(message.contains("the model expression"), "{message}");
        assert!(message.contains("needs 101 registers"), "{message}");
        assert!(message.contains("60 registers, 960 bytes"), "{message}");
        assert_eq!(
            compiler.next_register, 60,
            "a refused range allocates nothing"
        );
        assert_eq!(compiler.register_range(40, span).unwrap(), 60);
    });
}

#[test]
fn a_register_range_carries_no_per_register_facts() {
    with_compiler(|compiler, span| {
        compiler.register_range(14400, span).unwrap();
        assert_eq!(compiler.ledger.fact_count(), 0);
        let literal = compiler.constant(2.0, span).unwrap();
        assert_eq!(compiler.ledger.fact_count(), 1);
        assert_eq!(compiler.real_register(literal), Some(2.0));
        assert_eq!(compiler.integer_register(literal), Some(2));
        assert_eq!(compiler.integer_register(0), None);
    });
}

#[test]
fn issued_operations_and_facts_are_charged_to_the_budget_before_their_memory_is_spent() {
    with_compiler(|compiler, span| {
        // A register file of 1000 entries is 16000 bytes. Each literal also
        // issues an operation and a fact, so the program is refused long
        // before its register file alone would be full.
        compiler.ledger = register_ledger::RegisterLedger::with_budget(1000);
        let mut issued = 0;
        let error = loop {
            match compiler.constant(1.0, span) {
                Ok(_) => issued += 1,
                Err(error) => break error,
            }
            assert!(issued < 1000, "the register file alone was filled");
        };
        assert!(matches!(error, LowerError::BudgetExceeded { .. }));
        let message = error.to_string();
        assert!(message.contains("register facts"), "{message}");
        assert!(message.contains("operations"), "{message}");
        assert!(issued < 200, "{issued}");
        assert_eq!(compiler.next_register as usize, issued);
    });
}
