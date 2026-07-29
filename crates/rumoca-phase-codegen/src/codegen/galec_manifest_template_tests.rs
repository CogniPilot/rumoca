//! Architecture checks for the checked Algorithm Code template surface.

use super::create_environment;
use crate::templates;

#[test]
fn galec_templates_parse_in_the_strict_shared_environment() {
    let mut env = create_environment();
    for target in ["galec", "galec-production", "embedded-c-galec"] {
        let bundle = templates::builtin_target(target).expect("built-in GALEC target");
        for template in bundle.templates {
            env.add_template_owned(
                format!("{target}/{}", template.path),
                template.source.to_owned(),
            )
            .unwrap_or_else(|error| panic!("{target}/{}: {error}", template.path));
        }
    }
}

#[test]
fn galec_templates_consume_checked_algorithm_code_and_artifact_facts() {
    for target in ["galec", "galec-production", "embedded-c-galec"] {
        let bundle = templates::builtin_target(target).expect("built-in GALEC target");
        for template in bundle.templates {
            assert!(
                !template.source.contains("ctx."),
                "{target}/{} retains the removed dynamic manifest context",
                template.path
            );
            assert!(
                !template.source.contains("galec_alg_source")
                    && !template.source.contains("galec_c_source")
                    && !template.source.contains("galec_c_header"),
                "{target}/{} retains a pre-rendered target-language passthrough",
                template.path
            );
        }
    }
}
