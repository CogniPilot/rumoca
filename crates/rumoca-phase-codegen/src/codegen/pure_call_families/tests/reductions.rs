use super::*;
use rumoca_eval_solve::{TypedValue, eval_pure_call};

fn reduction_table(
    scalar: solve::SolveScalarType,
    dimensions: Vec<u32>,
    operator: solve::SolveReductionOperator,
) -> solve::SolvePureCallTable {
    let tensor = solve::SolveValueType::tensor(scalar, dimensions).unwrap();
    let domain = match scalar {
        solve::SolveScalarType::Integer(domain) => domain,
        _ => solve::SolveIntegerDomain::FULL,
    };
    let arithmetic =
        solve::SolveArithmeticProfile::construct(solve::SolveRealFormat::Binary64, domain);
    solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(1100),
            vec![tensor],
            vec![solve::SolvePureCallOutput::result(
                solve::SolveValueType::scalar(scalar),
            )],
            span(1100),
            |b, inputs, outputs| {
                let input = b.load(inputs[0], span(1101))?;
                let value = b.reduce(operator, input, span(1102))?;
                b.store(outputs[0], value, span(1103))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn c_value(value: solve::SolveValueKind) -> String {
    match value {
        solve::SolveValueKind::Real64(bits) => format!("rumoca_real64(UINT64_C({bits}))"),
        solve::SolveValueKind::Integer(i64::MIN) => "INT64_MIN".into(),
        solve::SolveValueKind::Integer(value) if value < 0 => format!("(-INT64_C({}))", -value),
        solve::SolveValueKind::Integer(value) => format!("INT64_C({value})"),
        solve::SolveValueKind::Boolean(value) => value.to_string(),
        _ => panic!("the C profile supports binary64, Integer and Boolean"),
    }
}

const CONTROL: &str = r#"{% from "fmi-typed-functions.jinja" import functions, function_prototypes %}
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <math.h>
#define RMC_API
typedef struct ModelInstance ModelInstance;
{{ real_extremum_prelude("c") }}
{{ function_prototypes(table, directional if directional is defined else []) }}
{{ functions(table, directional if directional is defined else []) }}
{{ declarations if declarations is defined else "" }}
int main(void) { {{ checks }} return 0; }
"#;

fn compiled_control(table: &solve::SolvePureCallTable, inputs: &[Vec<solve::SolveValueKind>]) {
    let owner = &table.owners()[0];
    let ty = &owner.inputs()[0];
    let ctype = match ty.element_type() {
        solve::SolveScalarType::Real { .. } => "double",
        solve::SolveScalarType::Integer(_) => "int64_t",
        solve::SolveScalarType::Boolean => "bool",
    };
    let mut checks = String::new();
    for values in inputs {
        let input = TypedValue::construct(ty.clone(), values.clone()).unwrap();
        let expected = eval_pure_call(table, owner.id(), &[input]);
        let values = values
            .iter()
            .copied()
            .map(c_value)
            .collect::<Vec<_>>()
            .join(",");
        checks.push_str(&format!(
            "{{ {ctype} input[]={{{values}}}, original[sizeof(input)/sizeof(input[0])];\n\
             memcpy(original,input,sizeof(input));\n\
             {ctype} output=1, sentinel=output; (void)sentinel;\n\
             int status=rumoca_pure_0((const void*[]){{input}},(void*[]){{&output}},NULL);\n\
             assert(memcmp(input,original,sizeof(input))==0);\n"
        ));
        match expected {
            Ok(expected) => {
                let value = expected[0].elements()[0];
                checks.push_str(&format!(
                    "assert(status==0); {ctype} expected={}; (void)expected;\n",
                    c_value(value)
                ));
                if matches!(value, solve::SolveValueKind::Real64(bits) if f64::from_bits(bits).is_nan())
                {
                    checks.push_str("assert(isnan(output));\n");
                } else {
                    checks.push_str("assert(memcmp(&output,&expected,sizeof(output))==0);\n");
                }
            }
            Err(_) => checks
                .push_str("assert(status==1 && memcmp(&output,&sentinel,sizeof(output))==0);\n"),
        }
        checks.push_str("}\n");
    }
    let families = PureCallFamilies::new(table).unwrap();
    let mut environment = super::super::super::create_environment();
    environment
        .add_template("reduction_control", CONTROL)
        .unwrap();
    let source = environment
        .get_template("reduction_control")
        .unwrap()
        .render(minijinja::context! { table => families.owners_value(), checks => checks })
        .unwrap();
    super::assertion_carrier::compile_and_execute(&source);
}

#[test]
fn compiled_real_reductions_preserve_order_nan_ties_and_tensor_inputs() {
    use solve::SolveReductionOperator::{Maximum, Minimum, Product, Sum};
    let cases = [
        [1e16, 1.0, -1e16, 3.0],
        [3.0, -2.0, 0.5, 4.0],
        [-0.0, 0.0, -0.0, 0.0],
        [f64::NAN, 3.0, f64::NAN, 2.0],
        [f64::INFINITY, f64::NEG_INFINITY, 2.0, 3.0],
    ]
    .map(|values| {
        values
            .map(|v| solve::SolveValueKind::Real64(v.to_bits()))
            .to_vec()
    });
    for operator in [Sum, Product, Minimum, Maximum] {
        compiled_control(
            &reduction_table(
                solve::SolveScalarType::real(profile()),
                vec![2, 2],
                operator,
            ),
            &cases,
        );
        compiled_control(
            &reduction_table(solve::SolveScalarType::real(profile()), vec![1], operator),
            &[vec![solve::SolveValueKind::Real64((-0.0_f64).to_bits())]],
        );
    }
}

#[test]
fn compiled_integer_reductions_check_each_intermediate_without_publication() {
    use solve::SolveReductionOperator::{Maximum, Minimum, Product, Sum};
    let cases = [[2, -3, 4], [i64::MAX, 1, -1], [i64::MIN, -1, 1]]
        .map(|values| values.map(solve::SolveValueKind::Integer).to_vec());
    for operator in [Sum, Product, Minimum, Maximum] {
        compiled_control(
            &reduction_table(
                solve::SolveScalarType::integer(profile()),
                vec![3],
                operator,
            ),
            &cases,
        );
        compiled_control(
            &reduction_table(
                solve::SolveScalarType::integer(profile()),
                vec![1],
                operator,
            ),
            &[vec![solve::SolveValueKind::Integer(i64::MIN)]],
        );
    }
    let scalar =
        solve::SolveScalarType::Integer(solve::SolveIntegerDomain::construct(-10, 10).unwrap());
    compiled_control(
        &reduction_table(scalar, vec![3], Sum),
        &[vec![8, 5, -5]
            .into_iter()
            .map(solve::SolveValueKind::Integer)
            .collect()],
    );
    compiled_control(
        &reduction_table(scalar, vec![3], Product),
        &[vec![10, 2, 0]
            .into_iter()
            .map(solve::SolveValueKind::Integer)
            .collect()],
    );
}

#[test]
fn compiled_maximum_directional_reduction_preserves_the_winning_seed() {
    let table = reduction_table(
        solve::SolveScalarType::real(profile()),
        vec![3],
        solve::SolveReductionOperator::Maximum,
    );
    let ty = &table.owners()[0].inputs()[0];
    let typed = |values: &[f64]| {
        TypedValue::construct(
            ty.clone(),
            values
                .iter()
                .map(|v| solve::SolveValueKind::Real64(v.to_bits()))
                .collect(),
        )
        .unwrap()
    };
    let expected = rumoca_eval_solve::eval_pure_call_directional(
        &table,
        table.owners()[0].id(),
        &[typed(&[-3.0, 7.0, -2.0]), typed(&[0.1, 0.4, 0.8])],
    )
    .unwrap();
    assert_eq!(
        expected[0].elements(),
        &[solve::SolveValueKind::Real64(7.0_f64.to_bits())]
    );
    assert_eq!(
        expected[1].elements(),
        &[solve::SolveValueKind::Real64(0.4_f64.to_bits())]
    );
    let families = PureCallFamilies::new(&table).unwrap();
    let mut environment = super::super::super::create_environment();
    environment
        .add_template("directional_reduce_control", CONTROL)
        .unwrap();
    let source = environment.get_template("directional_reduce_control").unwrap().render(minijinja::context! {
        table => families.owners_value(), directional => families.directional_value(),
        checks => "double x[]={-3.0,7.0,-2.0}, seed[]={0.1,0.4,0.8}, out[2]={91.0,92.0}; const double* in[]={x,seed}; assert(rumoca_scalar_pure_directional_0(in,out,NULL)==0); assert(out[0]==7.0 && out[1]==0.4);",
    }).unwrap();
    super::assertion_carrier::compile_and_execute(&source);
}

#[test]
fn compiled_boolean_reductions_preserve_all_and_false_before_true_order() {
    use solve::SolveReductionOperator::{All, Maximum, Minimum};
    let cases = [
        [false, true, false],
        [true, true, true],
        [false, false, false],
    ]
    .map(|values| values.map(solve::SolveValueKind::Boolean).to_vec());
    for operator in [All, Minimum, Maximum] {
        compiled_control(
            &reduction_table(solve::SolveScalarType::Boolean, vec![3], operator),
            &cases,
        );
        compiled_control(
            &reduction_table(solve::SolveScalarType::Boolean, vec![1], operator),
            &[vec![solve::SolveValueKind::Boolean(false)]],
        );
    }
}

fn guarded_table() -> solve::SolvePureCallTable {
    let p = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let tensor = solve::SolveValueType::tensor(integer.element_type(), vec![3]).unwrap();
    solve::SolvePureCallTable::construct(p, |table| {
        table.add_owner(
            identity(1110),
            vec![boolean.clone(), tensor.clone()],
            vec![
                solve::SolvePureCallOutput::result(integer.clone()),
                solve::SolvePureCallOutput::assertion_predicate(),
            ],
            span(1110),
            |b, inputs, outputs| {
                let valid = b.load(inputs[0], span(1111))?;
                let assertion = b.assertion_output(1, span(1112))?;
                b.check_assertion(assertion, valid, &[], span(1113), |_, _, _| Ok(()))?;
                let input = b.load(inputs[1], span(1114))?;
                let value = b.reduce(solve::SolveReductionOperator::Sum, input, span(1115))?;
                b.store(outputs[0], value, span(1116))?;
                b.store(outputs[1], valid, span(1117))
            },
        )?;
        table.add_owner(
            identity(1120),
            vec![boolean, tensor],
            vec![solve::SolvePureCallOutput::result(integer.clone())],
            span(1120),
            |b, inputs, outputs| {
                let active = b.load(inputs[0], span(1121))?;
                let input = b.load(inputs[1], span(1122))?;
                let selected = b.conditional(
                    active,
                    &[input],
                    vec![integer],
                    span(1123),
                    |region, inputs, outputs| {
                        let input = region.load(inputs[0], span(1124))?;
                        let value =
                            region.reduce(solve::SolveReductionOperator::Sum, input, span(1125))?;
                        region.store(outputs[0], value, span(1126))
                    },
                    |region, _, outputs| {
                        let five = region
                            .constant(solve::SolveValue::integer(p, 5).unwrap(), span(1127))?;
                        region.store(outputs[0], five, span(1128))
                    },
                )?;
                b.store(outputs[0], selected[0], span(1129))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn compiled_reductions_respect_first_fault_and_inactive_branches() {
    let table = guarded_table();
    let families = PureCallFamilies::new(&table).unwrap();
    let mut environment = super::super::super::create_environment();
    environment
        .add_template("guarded_reduce_control", CONTROL)
        .unwrap();
    let source = environment.get_template("guarded_reduce_control").unwrap().render(minijinja::context! {
        table => families.owners_value(),
        declarations => "static int observed(void* context,size_t predicate,const void* const captures[],size_t count) { (void)captures; assert(predicate==1 && count==0); ++*(size_t*)context; return 0; }",
        checks => r#"bool valid=false, predicate=true; int64_t input[]={INT64_MAX,1,-1}, result=91; size_t notices=0;
RmcAssertionObserver observer={&notices,observed};
const void* in[]={&valid,input}; void* out[]={&result,&predicate};
assert(rumoca_pure_0(in,out,&observer)==2);
assert(notices==1 && result==91 && predicate);
valid=true;
assert(rumoca_pure_0(in,out,&observer)==1);
assert(notices==1 && result==91 && predicate);
valid=false;
assert(rumoca_pure_1(in,(void*[]){&result},NULL)==0 && result==5);
result=91; valid=true;
assert(rumoca_pure_1(in,(void*[]){&result},NULL)==1 && result==91);
assert(input[0]==INT64_MAX && input[1]==1 && input[2]==-1);"#,
    }).unwrap();
    super::assertion_carrier::compile_and_execute(&source);
}

#[test]
fn compiled_boolean_binary_and_broadcast_extrema_require_no_real_helpers() {
    let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let tensor = solve::SolveValueType::tensor(solve::SolveScalarType::Boolean, vec![3]).unwrap();
    for operator in [
        solve::SolveBinaryOperator::Min,
        solve::SolveBinaryOperator::Max,
    ] {
        let table = solve::SolvePureCallTable::construct(profile(), |table| {
            table.add_owner(
                identity(1130),
                vec![tensor.clone(), tensor.clone(), scalar.clone()],
                vec![
                    solve::SolvePureCallOutput::result(tensor.clone()),
                    solve::SolvePureCallOutput::result(tensor.clone()),
                ],
                span(1130),
                |b, inputs, outputs| {
                    let a = b.load(inputs[0], span(1131))?;
                    let other = b.load(inputs[1], span(1132))?;
                    let scalar = b.load(inputs[2], span(1133))?;
                    let pair = b.binary(operator, a, other, span(1134))?;
                    let broadcast = b.broadcast_binary(operator, a, scalar, false, span(1135))?;
                    b.store(outputs[0], pair, span(1136))?;
                    b.store(outputs[1], broadcast, span(1137))
                },
            )?;
            Ok(())
        })
        .unwrap();
        let families = PureCallFamilies::new(&table).unwrap();
        let checks = if operator == solve::SolveBinaryOperator::Min {
            "assert(!pair[0] && !pair[1] && !pair[2]); assert(!broadcast[0] && broadcast[1] && !broadcast[2]);"
        } else {
            "assert(pair[0] && pair[1] && !pair[2]); assert(broadcast[0] && broadcast[1] && broadcast[2]);"
        };
        let mut environment = super::super::super::create_environment();
        environment
            .add_template("boolean_extremum_control", CONTROL)
            .unwrap();
        let source = environment.get_template("boolean_extremum_control").unwrap().render(minijinja::context! {
            table => families.owners_value(), uses_real_extremum => false,
            checks => format!("bool a[]={{false,true,false}}, other[]={{true,false,false}}, scalar=true, pair[3], broadcast[3]; assert(rumoca_pure_0((const void*[]){{a,other,&scalar}},(void*[]){{pair,broadcast}},NULL)==0); {checks}"),
        }).unwrap();
        assert!(!source.contains("rumoca_real_min") && !source.contains("rumoca_real_max"));
        super::assertion_carrier::compile_and_execute(&source);
    }
}
