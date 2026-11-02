//! Initialization-solved parameters and explicit time through exported APIs.

use super::*;

const PROFILE_DRIVER: &str =
    include_str!("../../../fixtures/fmi-partial-derivatives/profile-driver.c");

#[test]
fn initialization_parameter_is_differentiated_then_frozen_by_the_exported_api() {
    execute_profile(
        "InitialP",
        include_str!("../../../fixtures/fmi-partial-derivatives/InitialP.mo"),
        1,
    );
}

#[test]
fn explicit_time_has_its_own_api_seed_and_retains_mode_refusals() {
    execute_profile(
        "TimeP",
        include_str!("../../../fixtures/fmi-partial-derivatives/TimeP.mo"),
        2,
    );
}

fn execute_profile(model: &str, source: &str, profile: u8) {
    if !conformance_prerequisites_are_available() {
        return;
    }
    let standards = standard_roots();
    let compiled = rumoca::Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .expect("compile the source-owned derivative profile");
    for (version, headers) in [("fmi2", &standards.0.root), ("fmi3", &standards.1.root)] {
        let files = rumoca::render_target_files(&compiled, model, version, None)
            .expect("render the actual derivative profile");
        let work = tempdir().unwrap();
        for file in &files {
            let path = work.path().join(&file.path);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, &file.content).unwrap();
        }
        let xml = &files
            .iter()
            .find(|f| f.path == "modelDescription.xml")
            .unwrap()
            .content;
        let vars = metadata(xml);
        let scalarized = version == "fmi2";
        let token = lifecycle::attribute(
            xml,
            if scalarized {
                "guid"
            } else {
                "instantiationToken"
            },
        );
        let mut refs = format!(
            "#define FMI_VERSION {}\n#define PROFILE {profile}\n#define TOKEN {token:?}\n",
            if scalarized { 2 } else { 3 }
        );
        for (symbol, name) in [
            ("STATE_VR", "x"),
            ("INPUT_VR", "u"),
            ("OUTPUT_VR", "y"),
            ("DERIVATIVE_VR", "der(x)"),
        ] {
            refs.push_str(&format!("#define {symbol} {}\n", vars[name]));
        }
        if profile == 1 {
            for (symbol, name) in [("GAIN_VR", "gain"), ("OFFSET_VR", "offset")] {
                refs.push_str(&format!("#define {symbol} {}\n", vars[name]));
            }
        }
        let capability = if scalarized {
            "providesDirectionalDerivative"
        } else {
            "providesDirectionalDerivatives"
        };
        assert_eq!(xml.matches(&format!("{capability}=\"true\"")).count(), 2);
        if !scalarized {
            assert_eq!(
                xml.matches("providesAdjointDerivatives=\"true\"").count(),
                2
            );
        }
        run_rendered(work.path(), &headers.join("headers"), PROFILE_DRIVER, &refs);
    }
}
