//! Replaceable component member tails are proved by Instantiate before Flat.

use rumoca_ir_ast as ast;

fn typed_flat_model(source: &str, model: &str) -> rumoca_ir_flat::Model {
    let source_name = "<selected_component_member_identity>";
    let stored = rumoca_phase_parse::parse_to_ast(source, source_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(source_name, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let ast::InstancedTree { tree, mut overlay } =
        rumoca_phase_instantiate::instantiate(resolved, model).expect("model instantiates");
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, model)
        .expect("instanced model typechecks");
    rumoca_phase_flatten::flatten_ref(&tree, &overlay, model).expect("typed model flattens")
}

fn assert_all_variables_have_exact_occurrence_identity(model: &rumoca_ir_flat::Model) {
    for (name, variable) in &model.variables {
        assert!(
            variable.component_ref.is_some(),
            "flat variable `{name}` must retain structured occurrence identity"
        );
    }
}

#[test]
fn nested_drive_modifier_and_selected_media_fields_cross_flat_exactly() {
    let drive = typed_flat_model(
        r"
record DriveData
  parameter Real JL = 2;
end DriveData;
model LoadInertia
  parameter Real J = 1;
end LoadInertia;
partial model PartialDrive
  replaceable parameter DriveData driveData constrainedby DriveData;
  LoadInertia loadInertia(J = driveData.JL);
end PartialDrive;
model DriveTest
  extends PartialDrive;
end DriveTest;
",
        "DriveTest",
    );
    assert_all_variables_have_exact_occurrence_identity(&drive);

    let media = typed_flat_model(
        r"
record StateBase
  Real p;
  Real T;
end StateBase;
record StateConcrete
  extends StateBase;
end StateConcrete;
model BaseProperties
  replaceable StateBase state constrainedby StateBase;
end BaseProperties;
model ConcreteProperties
  extends BaseProperties(redeclare StateConcrete state);
  Real localPressure = state.p;
end ConcreteProperties;
model MediaTest
  replaceable ConcreteProperties medium constrainedby BaseProperties;
  Real pressure = medium.state.p;
  Real temperature = medium.state.T;
end MediaTest;
",
        "MediaTest",
    );
    assert_all_variables_have_exact_occurrence_identity(&media);
}
