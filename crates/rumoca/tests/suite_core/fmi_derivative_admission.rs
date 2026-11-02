//! Exercise derivative admission through normal source lowering and rendering.
//! These metadata checks need no external FMI tools or C compiler.

#![cfg(feature = "fmi")]

#[test]
fn source_lowered_refresh_assignments_keep_the_complete_derivative_profile() {
    for (model, source) in [
        (
            "P",
            include_str!("../fixtures/fmi-partial-derivatives/P.mo"),
        ),
        (
            "ArrayP",
            include_str!("../fixtures/fmi-partial-derivatives/ArrayP.mo"),
        ),
        (
            "InitialP",
            include_str!("../fixtures/fmi-partial-derivatives/InitialP.mo"),
        ),
        (
            "TimeP",
            include_str!("../fixtures/fmi-partial-derivatives/TimeP.mo"),
        ),
    ] {
        let compiled = rumoca::Compiler::new()
            .model(model)
            .compile_str(source, &format!("{model}.mo"))
            .expect("lower the actual Modelica fixture");
        for (version, capabilities) in [
            ("fmi2", &["providesDirectionalDerivative"][..]),
            (
                "fmi3",
                &[
                    "providesDirectionalDerivatives",
                    "providesAdjointDerivatives",
                ][..],
            ),
        ] {
            let files = rumoca::render_target_files(&compiled, model, version, None)
                .expect("render from the normal source-owned component");
            let xml = &files
                .iter()
                .find(|file| file.path == "modelDescription.xml")
                .expect("the standard target emits model metadata")
                .content;
            for capability in capabilities {
                assert_eq!(
                    xml.matches(&format!("{capability}=\"true\"")).count(),
                    2,
                    "{model}/{version}: both interfaces retain {capability}"
                );
            }
        }
    }
}
