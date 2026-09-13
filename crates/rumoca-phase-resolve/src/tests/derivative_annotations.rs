use super::*;

#[test]
fn derivative_annotations_resolve_targets_and_excluded_inputs_in_lexical_scope() {
    let source = r#"
package P
  function f
    input Real x;
    input Real v;
    output Real y;
  algorithm
    y := x*x;
    annotation(derivative(noDerivative=x,zeroDerivative=v)=df);
  end f;
  function df
    input Real x;
    input Real v;
    output Real dy;
  algorithm
    dy := v;
  end df;
end P;
function df
  input Real x;
  output Real y;
algorithm
  y := x;
end df;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let package = &tree.definitions.classes["P"];
    let function = &package.classes["f"];
    let rumoca_ir_ast::Expression::Binary { lhs, rhs, .. } = &function.annotation[0] else {
        panic!("annotated function assignment");
    };
    let rumoca_ir_ast::Expression::ComponentReference(target) = rhs.as_ref() else {
        panic!("annotation target");
    };
    assert_eq!(target.target_def_id(), package.classes["df"].def_id);
    assert_ne!(
        target.target_def_id(),
        tree.definitions.classes["df"].def_id
    );
    let rumoca_ir_ast::Expression::ClassModification { modifications, .. } = lhs.as_ref() else {
        panic!("derivative restrictions");
    };
    for (modifier, input) in modifications.iter().zip(["x", "v"]) {
        let rumoca_ir_ast::Expression::Modification { value, .. } = modifier else {
            panic!("input restriction");
        };
        let rumoca_ir_ast::Expression::ComponentReference(reference) = value.as_ref() else {
            panic!("restricted input");
        };
        assert_eq!(reference.target_def_id(), function.components[input].def_id);
    }
}
