//! MLS 9.1 / 10.5: evaluating a structural index must not widen a connect.
use crate::instantiate_model;
use rumoca_ir_ast as ast;
use std::collections::BTreeSet;

fn instantiate(declarations: &str, index: &str) -> Result<ast::InstanceOverlay, String> {
    let source = format!(
        "connector Port Real x; flow Real f; end Port;
         package Data constant Integer n=2; constant Integer parent[3]={{0,n,1}}; end Data;
         model Plant
           {declarations}
           Port source[3], sink[2];
         equation
           for i in 1:2 loop connect(source[{index}], sink[i]); end for;
         end Plant;"
    );
    instantiate_source(&source, "Plant")
}

fn instantiate_source(source: &str, target: &str) -> Result<ast::InstanceOverlay, String> {
    let file = "connection_indices.mo";
    let parsed = rumoca_phase_parse::parse_to_ast(source, file).expect("parse");
    let mut tree = ast::ClassTree::from_parsed(parsed);
    tree.source_map.add(file, source);
    let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .map_err(|error| format!("{error:?}"))?;
    instantiate_model(&resolved, target).map_err(|error| error.to_string())
}

fn assert_elements(declarations: &str, index: &str) {
    let overlay = instantiate(declarations, index).expect("structural connect should instantiate");
    assert_pairs(&overlay, "");
}

fn assert_pairs(overlay: &ast::InstanceOverlay, prefix: &str) {
    let actual: BTreeSet<_> = overlay
        .classes
        .values()
        .flat_map(|class| {
            rumoca_eval_ast::connection::scalar_connection_view(&class.connections).map(
                |connection| {
                    let connection = connection.expect("scalar view");
                    (connection.a.to_flat_string(), connection.b.to_flat_string())
                },
            )
        })
        .collect();
    assert_eq!(
        actual,
        BTreeSet::from([
            (format!("{prefix}source[2]"), format!("{prefix}sink[1]")),
            (format!("{prefix}source[1]"), format!("{prefix}sink[2]")),
        ])
    );
}

#[test]
fn direct_structural_index_retains_each_connector_element() {
    assert_elements("", "3-i");
}

#[test]
fn boolean_parameter_if_index_selects_one_connector() {
    assert_elements(
        "parameter Boolean choose=false;",
        "if choose then 3 else 3-i",
    );
}

#[test]
fn parameter_table_index_selects_one_connector() {
    assert_elements("parameter Integer parent[3]={0,2,1};", "parent[i+1]");
}

#[test]
fn constant_table_index_selects_one_connector() {
    assert_elements("constant Integer parent[3]={0,2,1};", "parent[i+1]");
}

#[test]
fn package_constant_table_index_selects_one_connector() {
    assert_elements("import D=Data;", "D.parent[i+1]");
}

#[test]
fn unevaluated_subscript_refuses_instead_of_connecting_the_whole_array() {
    let error = instantiate("parameter Integer dynamicIndex;", "dynamicIndex")
        .expect_err("a non-structural subscript must never disappear");
    let message = error;
    assert!(message.contains("subscript"), "{message}");
    assert!(message.contains("source"), "{message}");
}

#[test]
fn package_binding_evaluates_in_its_declaration_scope() {
    assert_elements("import D=Data; parameter Integer n=3;", "D.parent[i+1]");
}

#[test]
fn array_alias_selects_the_requested_element() {
    assert_elements(
        "parameter Integer values[3]={0,2,1}; parameter Integer parent[3]=values;",
        "parent[i+1]",
    );
}

#[test]
fn nested_array_binding_selects_each_dimension() {
    assert_elements(
        "parameter Integer parent[2,2]={{2,3},{3,1}};",
        "parent[i,i]",
    );
}

#[test]
fn out_of_bounds_table_selection_refuses() {
    let error = instantiate("parameter Integer parent[3]={0,2,1};", "parent[i+9]")
        .expect_err("out-of-bounds selection has no scalar value");
    assert!(error.contains("subscript"));
}

#[test]
fn cyclic_array_binding_refuses() {
    let error = instantiate(
        "parameter Integer a[3]=b; parameter Integer b[3]=a;",
        "a[i+1]",
    )
    .expect_err("cyclic selection has no scalar value");
    assert!(error.contains("cyclic"), "{error}");
}

#[test]
fn instance_modifier_supplies_the_structural_table() {
    let overlay = instantiate_source(
        "connector Port Real x; flow Real f; end Port;
         model Plant
           parameter Integer parent[3]={0,1,2};
           Port source[3], sink[2];
         equation
           for i in 1:2 loop connect(source[parent[i+1]],sink[i]); end for;
         end Plant;
         model Test Plant p(parent={0,2,1}); end Test;",
        "Test",
    )
    .expect("modified table");
    assert_pairs(&overlay, "p.");
}

#[test]
fn package_redeclare_selects_the_effective_table() {
    let overlay = instantiate_source(
        "connector Port Real x; flow Real f; end Port;
         package Base constant Integer parent[3]={0,1,2}; end Base;
         package Selected extends Base(parent={0,2,1}); end Selected;
         model Plant
           replaceable package D=Base;
           Port source[3], sink[2];
         equation
           for i in 1:2 loop connect(source[D.parent[i+1]],sink[i]); end for;
         end Plant;
         model Test Plant p(redeclare package D=Selected); end Test;",
        "Test",
    )
    .expect("selected package");
    assert_pairs(&overlay, "p.");
}

fn assert_whole_connection(declarations: &str, lhs: &str, rhs: &str, expected: (&str, &str)) {
    let source = format!(
        "connector Port Real x; flow Real f; end Port;
         model Holder Port p; end Holder;
         model Plant {declarations} equation connect({lhs},{rhs}); end Plant;"
    );
    let overlay = instantiate_source(&source, "Plant").expect("whole-dimension connect");
    let connections: Vec<_> = overlay
        .classes
        .values()
        .flat_map(|class| &class.connections)
        .collect();
    assert_eq!(connections.len(), 1);
    assert_eq!(connections[0].a.to_flat_string(), expected.0);
    assert_eq!(connections[0].b.to_flat_string(), expected.1);
    assert!(connections[0].family.is_none());
    assert!(!overlay.array_parent_dims.is_empty());
}

#[test]
fn colon_on_one_endpoint_retains_the_whole_array() {
    assert_whole_connection(
        "Port source[2],sink[2];",
        "source[:]",
        "sink",
        ("source", "sink"),
    );
}

#[test]
fn colon_on_both_endpoints_retains_the_whole_arrays() {
    assert_whole_connection(
        "Port source[2],sink[2];",
        "source[:]",
        "sink[:]",
        ("source", "sink"),
    );
}

#[test]
fn colon_on_an_inner_path_part_retains_the_whole_array() {
    assert_whole_connection(
        "Holder source[2],sink[2];",
        "source[:].p",
        "sink.p",
        ("source.p", "sink.p"),
    );
}

#[test]
fn token_range_and_empty_colon_forms_keep_the_whole_dimension() {
    for empty in [false, true] {
        let source = "model Plant equation connect(a[:].p,b.p); end Plant;";
        let parsed = rumoca_phase_parse::parse_to_ast(source, "colon.mo").expect("parse");
        let mut tree = ast::ClassTree::from_parsed(parsed);
        tree.source_map.add("colon.mo", source);
        let class = tree.definitions.classes.get_mut("Plant").expect("Plant");
        let ast::Equation::Connect { lhs, .. } = &mut class.equations[0] else {
            panic!("connect");
        };
        let token = lhs.parts[0].ident.clone();
        lhs.parts[0].subs = Some(vec![if empty {
            ast::Subscript::Empty
        } else {
            ast::Subscript::Range { token }
        }]);
        let connections = crate::connections::extract_connections(
            &class.equations,
            &ast::QualifiedName::new(),
            &crate::connections::ConnectionParams::new(),
            &tree.source_map,
        )
        .expect("both colon representations select the whole component");
        assert_eq!(connections[0].a.to_flat_string(), "a.p");
        assert_eq!(connections[0].b.to_flat_string(), "b.p");
    }
}

#[test]
fn literal_array_projection_uses_the_same_structural_evaluator() {
    assert_elements("", "({0,2,1})[i+1]");
}

#[test]
fn trailing_colon_preserves_the_scalar_prefix_selection() {
    assert_whole_connection(
        "Port source[2,3],sink[3];",
        "source[2,:]",
        "sink",
        ("source[2]", "sink"),
    );
}

#[test]
fn colon_before_a_scalar_refuses_instead_of_selecting_a_different_dimension() {
    for selector in ["source[:,1]", "source[1,:,2]"] {
        let source = format!(
            "connector Port Real x; flow Real f; end Port;
             model Plant Port source[2,3,2],sink[2]; equation connect({selector},sink); end Plant;"
        );
        let error = instantiate_source(&source, "Plant").expect_err("no forged scalar prefix");
        assert!(
            error.contains("subscript") && error.contains("source"),
            "{error}"
        );
    }
}
