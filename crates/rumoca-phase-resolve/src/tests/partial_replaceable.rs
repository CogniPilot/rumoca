//! Partial and replaceable declarations: what resolve must defer to
//! instantiation and what it must reject outright.

use super::*;

#[test]
fn test_partial_member_under_replaceable_package_is_not_rejected_in_resolve() {
    let source = r#"
package PartialMedium
  replaceable partial model BaseProperties
Real p;
  end BaseProperties;
end PartialMedium;

model UsesReplaceableMedium
  replaceable package Medium = PartialMedium;
  Medium.BaseProperties medium;
end UsesReplaceableMedium;
"#;
    resolve_test_source(source).expect("resolve must defer replaceable package member partiality");
}
#[test]
fn test_non_replaceable_partial_type_path_is_unresolved() {
    let source = r#"
model M
  package P
  end P;
  P.Missing x;
equation
  x = 0;
end M;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(
        result.is_err(),
        "resolution should fail for non-replaceable partial type path"
    );

    let diags = result.expect_err("expected resolve diagnostics");
    assert!(diags.iter().any(|d| {
        d.code.as_deref() == Some("ER002")
            && d.message.contains("unresolved type reference")
            && d.message.contains("P.Missing")
    }));
}

#[test]
fn test_partial_model_can_declare_replaceable_partial_component() {
    let source = r#"
partial block PartialBooleanMISO
  input Boolean u;
  output Boolean y;
end PartialBooleanMISO;

partial block PartialLogical
  replaceable PartialBooleanMISO combinator constrainedby PartialBooleanMISO;
end PartialLogical;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(
        result.is_ok(),
        "partial classes may contain replaceable components constrained by partial classes"
    );
}

#[test]
fn test_concrete_model_can_declare_replaceable_partial_component() {
    let source = r#"
partial block PartialBooleanMISO
  input Boolean u;
  output Boolean y;
end PartialBooleanMISO;

block Concrete
  replaceable PartialBooleanMISO combinator constrainedby PartialBooleanMISO;
end Concrete;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(
        result.is_ok(),
        "replaceable partial-typed components must remain legal until instantiation"
    );
}

#[test]
fn test_concrete_model_cannot_instantiate_partial_component() {
    let source = r#"
partial block PartialBooleanMISO
  input Boolean u;
  output Boolean y;
end PartialBooleanMISO;

block Concrete
  PartialBooleanMISO combinator;
end Concrete;
"#;
    let result = resolve_test_source(source);
    assert!(result.is_err(), "resolution should fail");

    let diags = result.expect_err("expected resolve diagnostics");
    assert!(diags.iter().any(|d| {
        d.code.as_deref() == Some("ER005")
            && d.message
                .contains("component 'combinator' instantiates partial block")
    }));
}
