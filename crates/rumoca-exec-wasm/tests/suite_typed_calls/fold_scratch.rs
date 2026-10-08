//! A read-only image read by a windowed loop adds no scratch (SOLVE-C78).
//!
//! The owner mirrors a raster pass: it carries one raster-sized aggregate
//! through a bounded `while` fold, rewrites one element per pass behind a
//! conditional, and reads the raster input, a read-only capture, in the
//! predicate and in both conditional arms.
use super::*;

const RASTER: u32 = 1000;
const BYTES: u32 = RASTER * 8;

fn raster_type() -> solve::SolveValueType {
    solve::SolveValueType::tensor(solve::SolveScalarType::real(profile()), vec![RASTER]).unwrap()
}

fn copy_element<'region>(
    r: &mut solve::TypedProgramBuilder<'region>,
    inputs: &[solve::ProgramSlot<'region>],
    outputs: &[solve::ProgramSlot<'region>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let old = r.load(inputs[0], span(801))?;
    let raster = r.load(inputs[1], span(802))?;
    let index = r.load(inputs[2], span(803))?;
    let value = r.project_element_dynamic(raster, &[index], span(804))?;
    let next = r.update_element(old, value, &[index], span(805))?;
    r.store(outputs[0], next, span(806))
}

fn keep<'region>(
    r: &mut solve::TypedProgramBuilder<'region>,
    inputs: &[solve::ProgramSlot<'region>],
    outputs: &[solve::ProgramSlot<'region>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let old = r.load(inputs[0], span(807))?;
    r.store(outputs[0], old, span(808))
}

fn windowed_pass() -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(78),
            vec![raster_type(), boolean],
            vec![solve::SolvePureCallOutput::result(raster_type())],
            span(800),
            |b, inputs, outputs| {
                let raster = b.load(inputs[0], span(810))?;
                let enabled = b.load(inputs[1], span(811))?;
                let zero = b.constant(solve::SolveValue::real(p, 0.0), span(812))?;
                let initial = b.fill(zero, vec![RASTER], span(813))?;
                let result = b.fold_while(
                    domain(1, i64::from(RASTER), 1),
                    &[initial],
                    &[raster, enabled],
                    span(814),
                    |r, carried, captures, outputs| {
                        r.load(carried[0], span(815))?;
                        r.load(captures[0], span(816))?;
                        let enabled = r.load(captures[1], span(817))?;
                        r.store(outputs[0], enabled, span(818))
                    },
                    |r, carried, captures, binders, outputs| {
                        let old = r.load(carried[0], span(819))?;
                        let raster = r.load(captures[0], span(820))?;
                        let enabled = r.load(captures[1], span(821))?;
                        let index = r.load(binders[0], span(822))?;
                        let next = r.conditional(
                            enabled,
                            &[old, raster, index],
                            vec![raster_type()],
                            span(823),
                            copy_element,
                            keep,
                        )?;
                        r.store(outputs[0], next[0], span(824))
                    },
                )?;
                b.store(outputs[0], result[0], span(825))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn a_read_only_raster_read_by_a_windowed_loop_adds_no_scratch() {
    let (table, site) = windowed_pass();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    // Output 8000, the zero constant 8 and the carried raster 8000 (its initial
    // value, rewritten in place), then one pass of private storage: the binder
    // 8, the element read in the copying arm 8 and the loop counter 8. The raster input, the
    // conditional result, the transition output and the predicate's inputs and
    // output are all borrowed ranges.
    assert_eq!(compiled.layout().scratch_bytes, 2 * BYTES + 8 + 8 + 8 + 8);
    let mut runner = Runner::new(&compiled);
    for enabled in [true, false] {
        let values = (0..RASTER).map(|i| f64::from(i) * 0.5 - 7.0);
        let inputs = vec![
            values.clone().map(real).collect::<Vec<_>>(),
            vec![solve::SolveValueKind::Boolean(enabled)],
        ];
        let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
        assert_eq!(status, 0);
        assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
        let expected = values.map(|v| if enabled { v } else { 0.0 });
        assert_eq!(actual, cells(expected.map(real)));
    }
}
