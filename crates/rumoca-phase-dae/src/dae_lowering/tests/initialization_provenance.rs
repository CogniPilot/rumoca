use super::*;

#[test]
fn scalarization_repeats_typed_initialization_provenance() {
    let mut dae = Dae::new();
    let mut target = dae::Variable::new(rumoca_core::VarName::new("target"), test_span());
    target.dims = vec![3];
    dae.variables
        .algebraics
        .insert(rumoca_core::VarName::new("target"), target);
    for k in 1..=3 {
        let name = format!("connector.pin[{k}].v");
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(&name),
            dae::Variable::new(rumoca_core::VarName::new(&name), test_span()),
        );
    }
    dae.initialization
        .equations
        .push(dae::Equation::residual_array(
            sub(var_ref("target"), var_ref("connector.pin.v")),
            test_span(),
            "phantom initial equation",
            3,
        ));
    dae.initialization
        .equation_provenance
        .push(dae::InitializationEquationProvenance::FixedStart);

    scalarize_phantom_vector_equations(&mut dae).unwrap();

    assert_eq!(dae.initialization.equations.len(), 3);
    assert_eq!(
        dae.initialization.equation_provenance,
        vec![dae::InitializationEquationProvenance::FixedStart; 3]
    );
}
