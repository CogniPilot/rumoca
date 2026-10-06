//! Synchronous read-only captures cannot become consumed private update storage.
use super::*;

fn branch_value<'region>(
    r: &mut solve::TypedProgramBuilder<'region>,
    inputs: &[solve::ProgramSlot<'region>],
    outputs: &[solve::ProgramSlot<'region>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let raster = r.load(inputs[0], span(761))?;
    let index = r.load(inputs[1], span(762))?;
    let value = r.project_element_dynamic(raster, &[index], span(763))?;
    r.store(outputs[0], value, span(764))
}

#[test]
fn full_raster_lazy_captures_borrow_without_iteration_copies_and_keep_late_faults() {
    let p = profile();
    let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let raster =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![14400]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(76),
            vec![raster, integer],
            vec![solve::SolvePureCallOutput::result(scalar.clone())],
            span(750),
            |b, inputs, outputs| {
                let raster = b.load(inputs[0], span(751))?;
                let late = b.load(inputs[1], span(752))?;
                let zero = b.constant(solve::SolveValue::real(p, -0.0), span(753))?;
                let result = b.fold(
                    domain(1, 14400, 1),
                    &[zero],
                    &[raster, late],
                    span(754),
                    |r, carried, captures, binders, outputs| {
                        let old = r.load(carried[0], span(755))?;
                        let raster = r.load(captures[0], span(756))?;
                        let late = r.load(captures[1], span(757))?;
                        let index = r.load(binders[0], span(758))?;
                        let end =
                            r.constant(solve::SolveValue::integer(p, 14400).unwrap(), span(759))?;
                        let is_last =
                            r.compare(solve::SolveCompareOperator::Equal, index, end, span(760))?;
                        let selected = r.select(is_last, late, index, span(765))?;
                        let value = r.conditional(
                            is_last,
                            &[raster, selected],
                            vec![scalar.clone()],
                            span(766),
                            branch_value,
                            branch_value,
                        )?;
                        let sum =
                            r.binary(solve::SolveBinaryOperator::Add, old, value[0], span(767))?;
                        r.store(outputs[0], sum, span(768))
                    },
                )?;
                b.store(outputs[0], result[0], span(769))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    // The raster stays in the immutable input interface; no region allocates
    // a raster-sized input snapshot. Required scalar scratch remains bounded.
    assert!(compiled.layout().scratch_bytes < 1024);
    let mut runner = Runner::new(&compiled);
    for pattern in [[1e16, 1.0, -1e16, 3.0], [-0.0; 4]] {
        let values = (0..14400).map(|i| pattern[i % 4]).collect::<Vec<_>>();
        let mut expected = -0.0;
        for value in &values {
            expected += value;
        }
        for target in [14400, 14401, 14400] {
            let inputs = vec![
                values.iter().copied().map(real).collect(),
                vec![solve::SolveValueKind::Integer(target)],
            ];
            let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
            if target == 14400 {
                assert_eq!(status, 0);
                assert_eq!(actual, cells([real(expected)]));
                assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
            } else {
                assert!(oracle(&table, &site, &inputs).is_err());
                assert_eq!(actual, [0xa5; 8]);
                let fault = compiled
                    .faults()
                    .iter()
                    .find(|fault| fault.status == status as u32)
                    .unwrap();
                assert_eq!(fault.kind, TypedCallFaultKind::IndexBounds);
                assert_eq!(fault.provenance, span(763));
                assert_eq!(fault.region_path, [(3, 0), (7, 0)]);
            }
        }
    }
}
