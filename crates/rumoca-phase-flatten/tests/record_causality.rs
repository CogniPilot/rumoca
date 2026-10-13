use rumoca_core::{Causality, VarName};
use rumoca_ir_ast as ast;
use rumoca_ir_dae::{DeclaredCausality, VariableCausality, VariableRole};

const SOURCE: &str = include_str!("fixtures/RecordCausality.mo");

#[test]
fn public_nested_record_causality_survives_dae_lowering() {
    for model in ["NestedRecord", "InheritedRecord", "ProtectedRecord"] {
        let (flat, dae) = lower(model);
        assert_eq!(flat.top_level_input_components.len(), 1);
        assert!(flat.top_level_input_components.contains("incoming"));
        assert_eq!(flat.top_level_output_components.len(), 1);
        assert!(flat.top_level_output_components.contains("outgoing"));
        check_fields(
            &dae,
            "incoming",
            VariableCausality::Input,
            DeclaredCausality::Input,
        );
        check_fields(
            &dae,
            "outgoing",
            VariableCausality::Output,
            DeclaredCausality::Output,
        );
        check_fields(
            &dae,
            "local",
            VariableCausality::Local,
            DeclaredCausality::None,
        );
        check_fields(
            &dae,
            "child.incoming",
            VariableCausality::Local,
            DeclaredCausality::Input,
        );
        check_fields(
            &dae,
            "child.outgoing",
            VariableCausality::Local,
            DeclaredCausality::Output,
        );
        if model == "ProtectedRecord" {
            check_fields(
                &dae,
                "hidden",
                VariableCausality::Local,
                DeclaredCausality::Input,
            );
            check_fields(
                &dae,
                "hiddenResult",
                VariableCausality::Local,
                DeclaredCausality::Output,
            );
        }
        let expected = if model == "InheritedRecord" {
            3.5
        } else {
            1.25
        };
        assert!(
            matches!(flat.variables[&VarName::new("incoming.leaf.values")].start,
            Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(value), ..
            }) if value == expected)
        );
    }
}

#[test]
fn public_record_array_elements_retain_issued_root_causality() {
    let (flat, dae) = lower("RecordArray");
    assert!(flat.top_level_input_components.contains("incoming"));
    assert!(flat.top_level_output_components.contains("outgoing"));
    for index in 1..=2 {
        check_fields(
            &dae,
            &format!("incoming[{index}]"),
            VariableCausality::Input,
            DeclaredCausality::Input,
        );
        check_fields(
            &dae,
            &format!("outgoing[{index}]"),
            VariableCausality::Output,
            DeclaredCausality::Output,
        );
        check_fields(
            &dae,
            &format!("local[{index}]"),
            VariableCausality::Local,
            DeclaredCausality::None,
        );
    }
}

fn check_fields(
    dae: &rumoca_ir_dae::Dae,
    root: &str,
    exported: VariableCausality,
    declared: DeclaredCausality,
) {
    dae.inspect(|view| {
        for field in ["leaf.values", "leaf.count", "leaf.valid", "scalar"] {
            let name = VarName::new(format!("{root}.{field}"));
            let (_, variable) = view.variables().find(|(_, v)| v.name() == &name).unwrap();
            assert_eq!(variable.causality(), exported, "{name}");
            assert_eq!(variable.declared_causality(), declared, "{name}");
            let expected_shape: &[u32] = if field == "leaf.values" { &[2] } else { &[] };
            assert_eq!(variable.value_type().dimensions(), expected_shape, "{name}");
            if exported == VariableCausality::Input {
                assert_eq!(variable.role(), VariableRole::Input, "{name}");
            }
        }
    });
}

fn lower(model: &str) -> (rumoca_ir_flat::Model, rumoca_ir_dae::Dae) {
    let model = format!("RecordCausality.{model}");
    let parsed = rumoca_phase_parse::parse_to_ast(SOURCE, "RecordCausality.mo").unwrap();
    let mut tree = ast::ClassTree::from_parsed(parsed);
    tree.source_map.add("RecordCausality.mo", SOURCE);
    let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).unwrap();
    let instanced = rumoca_phase_instantiate::instantiate(resolved, &model).unwrap();
    let ast::InstancedTree { tree, mut overlay } = instanced;
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, &model).unwrap();
    let flat =
        rumoca_phase_flatten::flatten_ref_with_options(&tree, &overlay, &model, Default::default())
            .unwrap();
    for (name, variable) in &flat.variables {
        if flat.top_level_input_components.contains("incoming")
            && variable
                .component_ref
                .as_ref()
                .is_some_and(|r| r.parts()[0].ident == "incoming")
        {
            assert!(matches!(variable.causality, Causality::Input(_)), "{name}");
        }
    }
    let dae = rumoca_phase_dae::to_dae(&flat, tree.source_map).unwrap();
    (flat, dae)
}
