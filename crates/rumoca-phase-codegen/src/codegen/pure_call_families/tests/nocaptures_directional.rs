use super::*;

fn square_table(level: solve::SolveAssertionLevel) -> solve::SolvePureCallTable {
    let p = profile();
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    solve::SolvePureCallTable::construct(p, |table| {
        table.add_owner(
            identity(980),
            vec![
                real.clone(),
                solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
                solve::SolveValueType::scalar(solve::SolveScalarType::integer(p)),
            ],
            vec![
                solve::SolvePureCallOutput::result(real),
                solve::SolvePureCallOutput::assertion_predicate_at_level(level),
            ],
            span(980),
            |b, inputs, outputs| {
                let valid = b.load(inputs[1], span(981))?;
                let assertion = b.assertion_output(1, span(982))?;
                b.check_assertion(assertion, valid, &[], span(983), |_, _, _| Ok(()))?;
                let x = b.load(inputs[0], span(984))?;
                let values = b.construct_aggregate(&[x, x], vec![2], span(985))?;
                let index = b.load(inputs[2], span(986))?;
                let value = b.project_element_dynamic(values, &[index], span(987))?;
                let square = b.binary(solve::SolveBinaryOperator::Multiply, value, x, span(988))?;
                b.store(outputs[0], square, span(989))?;
                b.store(outputs[1], valid, span(990))
            },
        )?;
        Ok(())
    })
    .unwrap()
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
{{ function_prototypes(table, directional) }}
{{ functions(table, directional) }}
static int observed(void* context, size_t predicate, const void* const captures[], size_t count) {
    (void)captures;
    assert(predicate==1 && count==0);
    ++*(size_t*)context;
    return 0;
}
int main(void) {
    double x=3.0, tangent=2.0, valid=1.0, index=1.0;
    double output[3]={91.0, 92.0, 93.0};
    const double* arguments[]={&x, &tangent, &valid, &index};
    size_t notices=0;
    RmcAssertionObserver observer={&notices, observed};
    assert(rumoca_scalar_pure_directional_0(arguments, output, &observer)==0);
    assert(notices==0 && output[0]==9.0 && output[1]==12.0 && output[2]==1.0);
    valid=0.0;
    {% if fatal %}
    index=3.0; output[0]=91.0; output[1]=92.0; output[2]=93.0;
    assert(rumoca_scalar_pure_directional_0(arguments, output, &observer)==2);
    assert(notices==1 && output[0]==91.0 && output[1]==92.0 && output[2]==93.0);
    {% else %}
    assert(rumoca_scalar_pure_directional_0(arguments, output, &observer)==0);
    assert(notices==1 && output[0]==9.0 && output[1]==12.0 && output[2]==0.0);
    index=3.0; output[0]=91.0; output[1]=92.0; output[2]=93.0;
    assert(rumoca_scalar_pure_directional_0(arguments, output, &observer)==1);
    assert(notices==2 && output[0]==91.0 && output[1]==92.0 && output[2]==93.0);
    {% endif %}
    size_t reached=notices;
    valid=1.0; index=3.0; output[0]=91.0; output[1]=92.0; output[2]=93.0;
    assert(rumoca_scalar_pure_directional_0(arguments, output, &observer)==1);
    assert(notices==reached && output[0]==91.0 && output[1]==92.0 && output[2]==93.0);
    valid=0.0;
    assert(rumoca_scalar_pure_directional_0(arguments, output, NULL)==1);
    assert(output[0]==91.0 && output[1]==92.0 && output[2]==93.0);
    return 0;
}
"#;

#[test]
fn compiled_directional_checks_keep_source_identity_and_atomic_outputs() {
    for level in [
        solve::SolveAssertionLevel::Error,
        solve::SolveAssertionLevel::Warning,
    ] {
        let families = PureCallFamilies::new(&square_table(level)).unwrap();
        let mut environment = super::super::super::create_environment();
        environment
            .add_template("directional_assertion", CONTROL)
            .unwrap();
        let source = environment
            .get_template("directional_assertion")
            .unwrap()
            .render(minijinja::context! {
                table => families.owners_value(), directional => families.directional_value(),
                fatal => level == solve::SolveAssertionLevel::Error,
            })
            .unwrap();
        super::assertion_carrier::compile_and_execute(&source);
    }
}
