//! Parser regression: short classes must retain source-written metadata.
use rumoca_ir_ast as ast;
use rumoca_phase_parse::parse_to_ast;

#[test]
fn short_class_specifiers_retain_descriptions_and_annotations() {
    let source = r#"
package ShortForms
  connector Input = input Real "signal hint" annotation(__Signal(value=1));
  type Color = enumeration(red, green) "enum hint" annotation(__Enum(value=2));
  function Bound = function Scale(gain=2) "bound hint" annotation(__Bound(value=3));
  function ScaleDer = der(Scale, gain) "der hint" annotation(__Der(value=4));
end ShortForms;
"#;
    let parsed =
        parse_to_ast(source, "ShortForms.mo").expect("short class definitions should parse");
    let classes = &parsed.classes["ShortForms"].classes;

    for (name, description, annotation) in [
        ("Input", "signal hint", "__Signal"),
        ("Color", "enum hint", "__Enum"),
        ("Bound", "bound hint", "__Bound"),
        ("ScaleDer", "der hint", "__Der"),
    ] {
        let class = &classes[name];
        let written = class
            .description
            .iter()
            .map(|token| token.text.as_ref())
            .collect::<Vec<_>>();
        assert_eq!(written, [description], "description for {name}");
        assert!(
            class.annotation.iter().any(|expr| matches!(
                expr,
                ast::Expression::ClassModification { target, .. } if target.to_string() == annotation
            )),
            "annotation {annotation} missing from {name}"
        );
    }
}

#[test]
fn short_class_annotations_reject_forbidden_modifiers() {
    // MLS §18.2 applies to annotations on every short-class form.
    for definition in [
        "connector Input = input Real",
        "type Color = enumeration(red, green)",
        "function Bound = function Scale(gain=2)",
        "function ScaleDer = der(Scale, gain)",
    ] {
        for modifier in ["each", "final"] {
            let source = format!("{definition} annotation({modifier} value=1);");
            let error = parse_to_ast(&source, "InvalidShortForm.mo")
                .expect_err("forbidden annotation modifiers must be rejected");
            assert!(
                error.to_string().contains("MLS §18.2"),
                "expected annotation validation for {source}: {error}"
            );
        }
    }
}
