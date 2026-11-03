use super::*;

/// Owners whose interfaces take every scalar crossing under a narrowed
/// integer profile: an integer, a Boolean array, a binary64 array and an
/// integer array in and out; an owner with no inputs; an observed predicate
/// with a converted input. Full-domain integers cross in the model controls.
fn crossing_table() -> solve::SolvePureCallTable {
    let profile = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::construct(-100, 100).unwrap(),
    );
    let narrow = solve::SolveValueType::scalar(solve::SolveScalarType::integer(profile));
    let flags = solve::SolveValueType::tensor(solve::SolveScalarType::Boolean, vec![2]).unwrap();
    let reals =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(profile), vec![3]).unwrap();
    let integers =
        solve::SolveValueType::tensor(solve::SolveScalarType::integer(profile), vec![2]).unwrap();
    let interface = vec![narrow, flags, reals, integers];
    solve::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            identity(800),
            interface.clone(),
            interface
                .iter()
                .cloned()
                .map(solve::SolvePureCallOutput::result)
                .collect(),
            span(800),
            |builder, inputs, outputs| {
                for (input, output) in inputs.iter().zip(outputs) {
                    let value = builder.load(*input, span(801))?;
                    builder.store(*output, value, span(802))?;
                }
                Ok(())
            },
        )?;
        table.add_owner(
            identity(810),
            vec![],
            vec![solve::SolvePureCallOutput::result(
                solve::SolveValueType::scalar(solve::SolveScalarType::real(profile)),
            )],
            span(810),
            |builder, _, outputs| {
                let one = builder.constant(solve::SolveValue::real(profile, 1.0), span(811))?;
                builder.store(outputs[0], one, span(812))
            },
        )?;
        table.add_owner(
            identity(820),
            vec![solve::SolveValueType::scalar(
                solve::SolveScalarType::Boolean,
            )],
            vec![solve::SolvePureCallOutput::assertion_predicate()],
            span(820),
            |builder, inputs, outputs| {
                let condition = builder.load(inputs[0], span(821))?;
                let assertion = builder.assertion_output(0, span(822))?;
                builder.check_assertion(assertion, condition, &[], span(823), |_, _, _| Ok(()))?;
                builder.store(outputs[0], condition, span(824))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn wrapper(table: &solve::SolvePureCallTable, owner: usize) -> String {
    let families = PureCallFamilies::new(table).unwrap();
    let mut environment = super::super::super::create_environment();
    environment
        .add_template(
            "wrapper_control",
            "{% from \"fmi-typed-functions.jinja\" import wrapper %}{{ wrapper(table.owners[owner]) }}",
        )
        .unwrap();
    environment
        .get_template("wrapper_control")
        .unwrap()
        .render(minijinja::context! { table => families.owners_value(), owner => owner })
        .unwrap()
}

/// The wrapper spells the issued crossings: binary64 values pass through,
/// others convert with their input checks, and each output lands at its
/// issued offset.
#[test]
fn the_scalar_wrapper_prints_the_issued_boundary() {
    let table = crossing_table();
    assert_eq!(wrapper(&table, 0), EXPECTED_CROSSINGS);
    assert_eq!(wrapper(&table, 1), EXPECTED_NO_INPUTS);
}

const EXPECTED_CROSSINGS: &str = r#"RMC_API int rumoca_scalar_pure_0(const double* const in[], double* out, const RmcAssertionObserver* observer) {
(void)in;
if (!out) return 1;
int64_t a0[1]; (void)a0;
for (size_t k=0; k<1; ++k) if (trunc(in[0][k]) != in[0][k]) return 1;
for (size_t k=0; k<1; ++k) { double v=in[0][k]; if (!isfinite(v) || v < -9223372036854775808.0 || v >= 9223372036854775808.0) return 1; a0[k]=(int64_t)v; }
for (size_t k=0; k<1; ++k) if (a0[k]<(-INT64_C(100)) || a0[k]>INT64_C(100)) return 1;
bool a1[2]; (void)a1;
for (size_t k=0; k<2; ++k) { if (in[1][k] != 0.0 && in[1][k] != 1.0) return 1; a1[k] = in[1][k] != 0.0; }
int64_t a3[2]; (void)a3;
for (size_t k=0; k<2; ++k) if (trunc(in[3][k]) != in[3][k]) return 1;
for (size_t k=0; k<2; ++k) { double v=in[3][k]; if (!isfinite(v) || v < -9223372036854775808.0 || v >= 9223372036854775808.0) return 1; a3[k]=(int64_t)v; }
for (size_t k=0; k<2; ++k) if (a3[k]<(-INT64_C(100)) || a3[k]>INT64_C(100)) return 1;
int64_t b0[1]; (void)b0;bool b1[2]; (void)b1;int64_t b3[2]; (void)b3;
{ int status=rumoca_pure_0((const void*[]){ a0,a1,in[2],a3, }, (void*[]){ b0,b1,out+3,b3, }, observer); if (status) return status; }
for (size_t k=0; k<1; ++k) out[0+k]=(double)b0[k];
for (size_t k=0; k<2; ++k) out[1+k]=(double)b1[k];
for (size_t k=0; k<2; ++k) out[6+k]=(double)b3[k];
return 0;
}
"#;

const EXPECTED_NO_INPUTS: &str = r#"RMC_API int rumoca_scalar_pure_1(const double* const in[], double* out, const RmcAssertionObserver* observer) {
(void)in;
if (!out) return 1;

{ int status=rumoca_pure_1((const void*[]){ NULL }, (void*[]){ out+0, }, observer); if (status) return status; }
return 0;
}
"#;
