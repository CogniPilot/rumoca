use super::*;

fn flatten_index(expression: &str) -> flat::Model {
    let source = format!(
        "model Selection
           constant Integer c1=1;
           constant Integer c2=c1;
           parameter Integer k=1;
           parameter Real a[3,2]=fill(0.0,3,2);
           Boolean n[4](each start=false,each fixed=true);
           Boolean y(start=false,fixed=true);
         equation
           when time>0.1 then y=n[{expression}]; end when;
           when time>0.2 then n={{true,true,true,true}}; end when;
         end Selection;"
    );
    let file = "discrete_selection_parameter_reads.mo";
    let parsed = rumoca_phase_parse::parse_to_ast(&source, file).expect("source parses");
    let mut tree = rumoca_ir_ast::ClassTree::from_parsed(parsed);
    tree.source_map.add(file, &source);
    let resolved = rumoca_phase_resolve::resolve(rumoca_ir_ast::ParsedTree::new(tree))
        .expect("source resolves");
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, "Selection").expect("source instantiates");
    let rumoca_ir_ast::InstancedTree { tree, mut overlay } = instanced;
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "Selection")
        .expect("source typechecks");
    crate::flatten_ref(&tree, &overlay, "Selection").expect("source flattens")
}

fn selected_index(model: &flat::Model) -> i64 {
    let equation = &model.when_chains[0].branches().next().unwrap().equations[0];
    let flat::WhenEquation::Assign { value, .. } = equation else {
        panic!("scalar receiver keeps its source assignment")
    };
    let Expression::VarRef {
        name, subscripts, ..
    } = value
    else {
        panic!("selected source read remains a resolved reference: {value:?}")
    };
    assert_eq!(name.var_name().as_str(), "n");
    assert_eq!(
        name.instance_id(),
        Some(model.variables[name.var_name()].instance_id)
    );
    let [Subscript::Index { value, .. }] = subscripts.as_slice() else {
        panic!("selection is fixed together with its dependency proof")
    };
    *value
}

fn structural_parameters(model: &flat::Model) -> BTreeSet<String> {
    model
        .parameter_branch_selections
        .iter()
        .filter(|selection| {
            selection.kind == flat::StructuralParameterUse::DiscreteDependencySubscript
        })
        .flat_map(|selection| selection.references.iter().flatten().cloned())
        .collect()
}

#[test]
fn constant_chain_keeps_only_the_adjustable_index_structural() {
    let model = flatten_index("c2+k");
    assert_eq!(selected_index(&model), 2);
    assert_eq!(
        structural_parameters(&model),
        BTreeSet::from(["k".to_owned()])
    );
    assert!(
        model
            .variables
            .contains_key(&rumoca_core::VarName::new("c1"))
    );
    assert!(
        model
            .variables
            .contains_key(&rumoca_core::VarName::new("c2"))
    );
}

#[test]
fn builtin_indices_distinguish_parameter_values_from_fixed_array_shape() {
    for (expression, index, parameters) in [
        ("min(abs(k),2)", 1, vec!["k"]),
        ("size(a,k)", 3, vec!["k"]),
        ("ndims(a)", 2, Vec::new()),
    ] {
        let model = flatten_index(expression);
        assert_eq!(selected_index(&model), index, "{expression}");
        assert_eq!(
            structural_parameters(&model),
            parameters.into_iter().map(str::to_owned).collect(),
            "the array value is not a structural dimension: {expression}"
        );
    }
}
