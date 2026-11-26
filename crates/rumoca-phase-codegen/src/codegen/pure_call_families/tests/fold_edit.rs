//! Execute generated C with an incoming array that is also a read-only capture.
use super::*;

/// The array the fold body edits: the read-only capture, or the carried one.
fn edited_array<T>(use_capture: bool, original: T, old: T) -> T {
    if use_capture { original } else { old }
}

fn scatter(use_capture: bool, last: i64) -> solve::SolvePureCallTable {
    let ty =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(profile()), vec![4]).unwrap();
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: last,
            step: 1,
        }],
    };
    solve::SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1500),
            vec![ty.clone()],
            vec![solve::SolvePureCallOutput::result(ty)],
            span(1),
            |b, inputs, outputs| {
                let start = b.load(inputs[0], span(2))?;
                let result = b.fold(
                    domain,
                    &[start],
                    &[start],
                    span(3),
                    |b, carried, captures, binders, out| {
                        let old = b.load(carried[0], span(4))?;
                        let original = b.load(captures[0], span(5))?;
                        let i = b.load(binders[0], span(6))?;
                        let x = b.project_element_dynamic(original, &[i], span(7))?;
                        let value = b.unary(solve::SolveUnaryOperator::Negate, x, span(8))?;
                        let updated = b.update_element(
                            edited_array(use_capture, original, old),
                            value,
                            &[i],
                            span(9),
                        )?;
                        b.store(out[0], updated, span(10))
                    },
                )?;
                b.store(outputs[0], result[0], span(11))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

const EMIT: &str = r#"{% from "fmi-typed-functions.jinja" import functions, function_prototypes %}
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <math.h>
#define RMC_API
typedef struct ModelInstance ModelInstance;
{{ function_prototypes(table) }}
{{ functions(table) }}
"#;

#[test]
fn scatter_updates_only_the_carried_array_after_a_successful_pass() {
    for (capture, last) in [(false, 4), (true, 4), (false, 5)] {
        let families = PureCallFamilies::new(&scatter(capture, last)).unwrap();
        let mut env = super::super::super::create_environment();
        env.add_template("fold_test", EMIT).unwrap();
        let mut source = env
            .get_template("fold_test")
            .unwrap()
            .render(minijinja::context! { table => families.owners_value() })
            .unwrap();
        assert_eq!(source.contains("delta->index=index"), !capture);
        source.push_str(&format!(
            r#"
int main(void) {{
    double original[]={{2,3,5,7}}, out[]={{31,32,33,34}};
    const double* inputs[]={{original}};
    int status=rumoca_scalar_pure_0(inputs,out,NULL);
    assert((status!=0)=={fault});
    double expected[]={{{expected}}};
    double saved[]={{2,3,5,7}};
    assert(memcmp(expected,out,sizeof(out))==0);
    assert(memcmp(saved,original,sizeof(saved))==0);
    return 0;
}}
"#,
            fault = i32::from(last > 4),
            expected = if last > 4 {
                "31,32,33,34"
            } else if capture {
                "2,3,5,-7"
            } else {
                "-2,-3,-5,-7"
            }
        ));
        super::assertion_carrier::compile_and_execute_with_flags(&source, &["-O2"]);
    }
}
