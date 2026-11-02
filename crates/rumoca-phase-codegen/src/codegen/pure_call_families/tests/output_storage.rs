//! The scalar ABI requires the issued owner's complete output range.

use super::*;

fn slice_table() -> solve::SolvePureCallTable {
    let scalar = solve::SolveScalarType::real(profile());
    let matrix = solve::SolveValueType::tensor(scalar, vec![3, 4]).unwrap();
    let replacement = solve::SolveValueType::tensor(scalar, vec![2, 2]).unwrap();
    solve::SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1200),
            vec![matrix.clone(), replacement],
            vec![
                solve::SolvePureCallOutput::result(matrix.clone()),
                solve::SolvePureCallOutput::result(matrix),
            ],
            span(1200),
            |builder, inputs, outputs| {
                let base = builder.load(inputs[0], span(1201))?;
                let replacement = builder.load(inputs[1], span(1202))?;
                let updated = builder.update_slice(base, replacement, vec![1, 1], span(1203))?;
                builder.store(outputs[0], base, span(1204))?;
                builder.store(outputs[1], updated, span(1205))
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
int main(void) {
    double base[12], tangent[12];
    for (size_t k=0; k<12; ++k) { base[k]=(double)k+1; tangent[k]=0.125*((double)k+1); }
    double replacement[]={-1,20,30,40}, direction[]={-0.5,0.25,-0.75,1.5};
    double original[12], original_tangent[12];
    memcpy(original,base,sizeof(base)); memcpy(original_tangent,tangent,sizeof(tangent));
    double primal[26], result[50];
    primal[0]=primal[25]=12345; result[0]=result[49]=54321;
    const double* in[]={base,replacement};
    const double* directional_in[]={base,tangent,replacement,direction};
    assert(rumoca_scalar_pure_0(in,primal+1,NULL)==0);
    assert(rumoca_scalar_pure_directional_0(directional_in,result+1,NULL)==0);
    assert(primal[0]==12345 && primal[25]==12345 && result[0]==54321 && result[49]==54321);
    for (size_t k=0; k<12; ++k) {
        double value=base[k], seed=tangent[k];
        for (size_t row=0; row<2; ++row) for (size_t col=0; col<2; ++col) {
            if (k==(row+1)*4+col+1) { value=replacement[row*2+col]; seed=direction[row*2+col]; }
        }
        assert(primal[1+k]==base[k] && primal[13+k]==value);
        assert(result[1+k]==base[k] && result[13+k]==tangent[k]);
        assert(result[25+k]==value && result[37+k]==seed);
    }
    assert(memcmp(base,original,sizeof(base))==0);
    assert(memcmp(tangent,original_tangent,sizeof(tangent))==0);
    assert(rumoca_scalar_pure_0(NULL,NULL,NULL)!=0);
    assert(rumoca_scalar_pure_directional_0(NULL,NULL,NULL)!=0);
    return 0;
}
"#;

#[test]
fn scalar_output_storage_survives_fortify_and_wrapping_pointer_arithmetic() {
    let families = PureCallFamilies::new(&slice_table()).unwrap();
    let mut environment = super::super::super::create_environment();
    environment
        .add_template("scalar_output_storage", CONTROL)
        .unwrap();
    let source = environment
        .get_template("scalar_output_storage")
        .unwrap()
        .render(minijinja::context! {
            table => families.owners_value(), directional => families.directional_value(),
        })
        .unwrap();
    super::assertion_carrier::compile_and_execute_with_flags(
        &source,
        &[
            "-O3",
            "-fno-strict-overflow",
            "-U_FORTIFY_SOURCE",
            "-D_FORTIFY_SOURCE=3",
        ],
    );
}
