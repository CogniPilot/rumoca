use rumoca_core::SourceMap;

use super::*;

fn empty_checked_dae() -> dae::Dae {
    dae::Dae::construct(SourceMap::new(), |_| Ok(())).expect("empty checked DAE is valid")
}

#[test]
fn ordinary_dae_template_error_stays_anchored_to_template_source() {
    let dae = empty_checked_dae();
    let template = "prefix\n{{ dae.missing_field.value }}\n";

    let error = render_template(&dae, template).expect_err("strict undefined access must fail");

    match error {
        CodegenError::TemplateRenderError { src, span, .. } => {
            assert_eq!(src.name(), "inline");
            assert_eq!(src.inner(), template);
            assert_eq!(span.offset(), "prefix\n".len());
            assert_eq!(span.len(), "{{ dae.missing_field.value }}".len());
        }
        other => panic!("expected template-source diagnostic, got {other:?}"),
    }
}
