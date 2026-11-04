//! Compile-only STRUCT-T07 / MLS 4.9.7.1 preference refusal and order refinement.

use rumoca::Compiler;
use rumoca_phase_structural::construct_formal_derivatives;

#[test]
fn nondifferentiable_preferences_keep_the_original_formal_coordinates() {
    let source = r#"
model Withheld
  Real x(start=1, fixed=true);
  Real y(stateSelect=StateSelect.prefer);
  Integer k;
equation
  der(x) = -x;
  k = integer(3*x);
  y = x + k;
end Withheld;
"#;
    let compiled = Compiler::new()
        .model("Withheld")
        .compile_str(source, "Withheld.mo")
        .unwrap();
    let before = serde_json::to_vec(compiled.dae.as_ref()).unwrap();
    let formal = construct_formal_derivatives(&compiled.dae).unwrap();
    formal.inspect(|system| {
        assert_eq!(system.formal_dimension(), 1);
        let successors = system
            .source
            .variables()
            .filter_map(|(id, variable)| {
                system
                    .coordinate(id, 1)
                    .map(|_| variable.name().to_string())
            })
            .collect::<Vec<_>>();
        assert_eq!(successors, ["x"]);
    });
    assert_eq!(serde_json::to_vec(compiled.dae.as_ref()).unwrap(), before);
}

#[test]
fn newly_required_rate_bounds_are_reanalyzed() {
    let compiled = Compiler::new()
        .model("NoDerivativeRateLoop")
        .compile_str(
            include_str!("../fixtures/index_reduction/NoDerivativeRateLoop.mo"),
            "NoDerivativeRateLoop.mo",
        )
        .unwrap();
    let formal = construct_formal_derivatives(&compiled.dae).unwrap();
    formal.inspect(|system| {
        assert_eq!(system.formal_dimension(), 2);
        for name in ["R1.w", "R2.w"] {
            let (id, _) = system
                .source
                .variables()
                .find(|(_, variable)| variable.name().as_str() == name)
                .unwrap();
            assert!(system.coordinate(id, 1).is_some(), "{name} needs its rate");
            let level = system.stages().find_map(|stage| {
                stage
                    .coordinates()
                    .any(|coordinate| coordinate.source() == id && coordinate.order() == 0)
                    .then_some(stage.level())
            });
            assert_eq!(
                level,
                Some(-1),
                "the supplied derivative's rate must join its required stage"
            );
        }
    });
}
