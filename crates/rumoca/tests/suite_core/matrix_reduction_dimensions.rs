//! MSL KinematicPTP2 derives output extents with a matrix max reduction.

use rumoca_ir_ast as ast;

const SOURCE: &str = r#"
model MatrixReductionDimensions
  parameter Real q_begin[:] = {0};
  parameter Real q_end[:] = {1};
  parameter Real qd_max[:] = {2};
  parameter Real qdd_max[:] = {3};
  final parameter Integer nout = max([size(q_begin, 1); size(q_end, 1); size(qd_max, 1); size(qdd_max, 1)]);
  Real q[nout] = fill(time, nout);
end MatrixReductionDimensions;
"#;

fn typechecked(source: &str) -> (ast::ClassTree, ast::InstanceOverlay) {
    let parsed = rumoca_phase_parse::parse_to_ast(source, "matrix_reduction_dimensions.mo")
        .expect("the reduced source must parse");
    let mut tree = ast::ClassTree::from_parsed(parsed);
    tree.source_map
        .add("matrix_reduction_dimensions.mo", source);
    let tree = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .expect("the reduced source must resolve")
        .into_inner();
    let mut overlay =
        rumoca_phase_instantiate::instantiate_model(&tree, "MatrixReductionDimensions")
            .expect("the reduced source must instantiate");
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "MatrixReductionDimensions")
        .expect("the binding must establish its shape before strict typecheck");
    (tree, overlay)
}

fn check(source: &str, extent: usize) {
    let (_, overlay) = typechecked(source);
    let q = overlay
        .components
        .values()
        .find(|component| {
            let path = component.qualified_name.to_flat_string();
            path == "q" || path == "path.q"
        })
        .expect("the instantiated output must exist");
    assert_eq!(q.dims, vec![extent as i64]);
}

#[test]
fn scalar_string_constant_proves_medium_composition_shape_through_flatten() {
    let source = r#"
package BaseMedium
  constant String mediumName = "air";
  constant String substanceNames[:] = {mediumName};
  constant Integer nX = size(substanceNames, 1);
  model BaseProperties
    Real X[nX];
  equation
    X = fill(time, nX);
  end BaseProperties;
end BaseMedium;
model MatrixReductionDimensions
  replaceable package Medium = BaseMedium(mediumName="water");
  Medium.BaseProperties medium;
end MatrixReductionDimensions;
"#;
    let (tree, overlay) = typechecked(source);
    let flat = rumoca_phase_flatten::flatten_ref(&tree, &overlay, "MatrixReductionDimensions")
        .expect("the medium composition dimension must survive flattening");
    let variable = flat
        .variables
        .iter()
        .find(|(name, _)| name.to_string() == "medium.X")
        .map(|(_, variable)| variable)
        .expect("composition variable");
    assert_eq!(variable.dims, vec![1]);
}

#[test]
fn nested_scalar_modifier_binding_proves_array_extent() {
    let source = SOURCE
        .replace("model MatrixReductionDimensions", "block Kinematic")
        .replace("end MatrixReductionDimensions;", "end Kinematic;");
    let source = format!(
        "{source}\nmodel MatrixReductionDimensions\n parameter Real angle = 2;\n Kinematic path(q_end={{angle}});\nend MatrixReductionDimensions;"
    );
    check(&source, 1);
}

#[test]
fn nested_scalar_function_result_proves_array_extent_without_its_value() {
    let source = SOURCE
        .replace("model MatrixReductionDimensions", "block Kinematic")
        .replace("end MatrixReductionDimensions;", "end Kinematic;");
    let source = format!(
        r#"{source}
function convert
  input Real u;
  output Real y;
algorithm
  y := asin(u);
end convert;
model MatrixReductionDimensions
  final parameter Real angle = convert(0.5);
  Kinematic path(q_end={{angle}});
end MatrixReductionDimensions;
"#
    );
    check(&source, 1);
}

#[test]
fn qualified_scalar_constant_proves_array_extent_without_its_value() {
    let source = SOURCE
        .replace("model MatrixReductionDimensions", "block Kinematic")
        .replace("end MatrixReductionDimensions;", "end Kinematic;");
    let source = format!(
        r#"{source}
package Constants
  constant Real pi = 2*asin(1.0);
end Constants;
model MatrixReductionDimensions
  Kinematic path(q_end={{Constants.pi}});
end MatrixReductionDimensions;
"#
    );
    check(&source, 1);
}

#[test]
fn column_matrix_max_proves_a_single_element_output_extent() {
    check(SOURCE, 1);
}

#[test]
fn column_matrix_max_proves_a_larger_output_extent() {
    check(&SOURCE.replace("q_end[:] = {1}", "q_end[:] = {1,2,3}"), 3);
}

#[test]
fn scalar_powers_prove_array_operand_shapes() {
    check(
        &SOURCE.replace("q_end[:] = {1}", "q_end[:] = {2^2, 3^2}"),
        2,
    );
}

#[test]
fn scalar_record_field_proves_array_extent_without_its_value() {
    check(&drive_data_source(), 1);
}

#[test]
fn replaceable_scalar_record_field_proves_array_extent_without_its_value() {
    let source = drive_data_source().replace(
        "parameter DriveData driveData;",
        "replaceable parameter DriveData driveData constrainedby DriveData;",
    );
    check(&source, 1);
}

#[test]
fn redeclared_scalar_record_field_proves_array_extent_without_its_value() {
    let source = drive_data_source()
        .replace("model MatrixReductionDimensions", "model Holder")
        .replace("end MatrixReductionDimensions;", "end Holder;")
        .replace(
            "parameter DriveData driveData;",
            "replaceable parameter DriveData driveData constrainedby DriveData;",
        );
    let source = format!(
        r#"{source}
record AlternateDriveData
  extends DriveData(wMax=acos(0.5));
end AlternateDriveData;
model MatrixReductionDimensions
  extends Holder(redeclare AlternateDriveData driveData);
end MatrixReductionDimensions;
"#
    );
    check(&source, 1);
}

fn drive_data_source() -> String {
    let source = SOURCE
        .replace("model MatrixReductionDimensions", "block Kinematic")
        .replace("end MatrixReductionDimensions;", "end Kinematic;");
    format!(
        r#"{source}
record DriveData
  parameter Real wMax = asin(0.5);
end DriveData;
model MatrixReductionDimensions
  parameter DriveData driveData;
  Kinematic path(qd_max={{driveData.wMax}});
end MatrixReductionDimensions;
"#
    )
}

#[test]
fn element_array_max_control_proves_the_same_extent() {
    let source = SOURCE
        .replace("max([", "max({")
        .replace("size(qdd_max, 1)])", "size(qdd_max, 1)})")
        .replace("; size(", ", size(");
    check(&source, 1);
}
