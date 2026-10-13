//! Original source types and variability of array target coordinates.

use super::*;

#[test]
fn integer_domains_reject_integral_real_source_expressions() {
    for (declarations, index) in [
        ("", "1.0"),
        ("parameter Real k = 1;", "k"),
        ("type Index = Real; parameter Index k = 1;", "k"),
        (
            "type Index = Real; type Alias = Index; parameter Alias k = 1;",
            "k",
        ),
        ("", "1 / 1"),
    ] {
        for equation in [
            format!("y = a[{index}];"),
            format!("when time > 1 then a[{index}] = 2; end when;"),
        ] {
            let source = format!(
                "model Test {declarations} discrete Real a[2]; Real y; equation {equation} end Test;"
            );
            let diagnostics = typecheck_diagnostics(&source);
            assert!(
                diagnostics.iter().any(|diagnostic| diagnostic
                    .message
                    .contains("subscript must have Integer type")),
                "source type must survive normalization: {source}; {diagnostics:?}"
            );
        }
    }
}

#[test]
fn integer_domain_accepts_integer_aliases_and_vector_slices() {
    let source = r#"
        model Test
            type Index = Integer;
            type Alias = Index;
            parameter Alias k = 1;
            parameter Integer indices[2] = {1, 2};
            Real a[2]; Real y; Real pair[2]; Real ranged[2]; Real selected[2];
        equation
            y = a[k];
            pair = a[{1, 2}];
            ranged = a[1:2];
            selected = a[indices];
        end Test;
    "#;
    typecheck(resolve(parse(source)).unwrap()).expect("Integer types and slices are valid");
}

#[test]
fn nonparameter_integer_indices_are_legal_reads_but_not_when_targets() {
    let source = r#"
        model Test
            Integer k = 1;
            discrete Real a[2]; Real y;
        equation
            y = a[k];
        end Test;
    "#;
    typecheck(resolve(parse(source)).unwrap()).expect("simulation-time Integer read index");
    let source = source.replace("y = a[k];", "when time > 1 then a[k] = 2; end when;");
    let diagnostics = typecheck_diagnostics(&source);
    assert!(
        diagnostics.iter().any(|diagnostic| diagnostic
            .message
            .contains("target subscripts must be parameter expressions")),
        "{diagnostics:?}"
    );
}

#[test]
fn when_targets_accept_nonparameter_array_shape_and_parameter_iterators() {
    let source = r#"
        model Test
            input Real values[2];
            parameter Integer dimension = 1;
            discrete Real a[2];
        equation
            when time > 1 then
                a[size(values, dimension)] = 1;
                for i in 1:2 loop
                    a[i] = 1;
                end for;
            end when;
        end Test;
    "#;
    typecheck(resolve(parse(source)).unwrap()).expect("fixed shape and equation iterators");
}

#[test]
fn size_dimension_argument_must_be_parameter_in_when_target() {
    let source = r#"
        model Test
            Real values[2]; Integer dimension = 1;
            discrete Real a[2];
        equation
            when time > 1 then a[size(values, dimension)] = 1; end when;
        end Test;
    "#;
    let diagnostics = typecheck_diagnostics(source);
    assert!(
        diagnostics.iter().any(|diagnostic| diagnostic
            .message
            .contains("target subscripts must be parameter expressions")),
        "{diagnostics:?}"
    );
}

#[test]
fn initialization_event_condition_does_not_make_a_parameter_coordinate() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model Test
            discrete Real a[2];
        equation
            when time > 1 then a[if initial() then 1 else 2] = 1; end when;
        end Test;
    "#,
    );
    assert!(
        diagnostics.iter().any(|diagnostic| diagnostic
            .message
            .contains("target subscripts must be parameter expressions")),
        "{diagnostics:?}"
    );
}

#[test]
fn boolean_and_enumeration_index_domains_preserve_semantic_types() {
    let source = r#"
        model Test
            type Domain = enumeration(first, second);
            type Alias = Domain;
            parameter Alias k = Domain.second;
            Real b[Boolean]; Real e[Domain]; Real y; Real bv[2]; Real ev[2];
        equation
            y = b[false] + e[k];
            bv = b[{false, true}];
            ev = e[{Domain.first, Domain.second}];
        end Test;
    "#;
    typecheck(resolve(parse(source)).unwrap()).expect("Boolean and matching enum domains");
    for source in [
        source.replace("b[false]", "b[1]"),
        source.replace("e[k]", "e[1]"),
    ] {
        let diagnostics = typecheck_diagnostics(&source);
        assert!(
            diagnostics
                .iter()
                .any(|diagnostic| diagnostic.message.contains("array subscript must have")),
            "{diagnostics:?}"
        );
    }
}

#[test]
fn matrix_indices_are_not_vector_slices() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model Test
            Real a[2]; Real y[2, 2];
        equation
            y = a[{{1, 2}, {1, 2}}];
        end Test;
    "#,
    );
    assert!(
        diagnostics.iter().any(|diagnostic| diagnostic
            .message
            .contains("subscript must be a scalar or vector")),
        "{diagnostics:?}"
    );
}

#[test]
fn boolean_and_enum_iterators_keep_their_domain_element_types() {
    let source = r#"
        model Test
            type Domain = enumeration(first, second);
            Real b[Boolean]; Real e[Domain];
        equation
            for k in false:true loop b[k] = 1; end for;
            for k in Domain.first:Domain.second loop e[k] = 1; end for;
        end Test;
    "#;
    typecheck(resolve(parse(source)).unwrap()).expect("typed non-Integer iterators");
}

#[test]
fn real_iterators_cannot_index_integer_domains() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model Test
            Real a[2];
        equation
            for k in 1.0:2.0 loop a[k] = 1; end for;
        end Test;
    "#,
    );
    assert!(
        diagnostics.iter().any(|diagnostic| diagnostic
            .message
            .contains("subscript must have Integer type")),
        "{diagnostics:?}"
    );
}

#[test]
fn end_remains_contextual_to_the_index_domain() {
    let source = r#"
        model Test
            type Domain = enumeration(first, second);
            Real a[2]; Real b[Boolean]; Real e[Domain]; Real y;
        equation
            y = a[end] + b[end] + e[end];
        end Test;
    "#;
    typecheck(resolve(parse(source)).unwrap()).expect("contextual end indices");
}

#[test]
fn function_result_indices_retain_output_domains() {
    let source = r#"
        function values
            output Real result[Boolean];
        algorithm
            result := {1, 2};
        end values;
        model Test
            Real y;
        equation
            y = values()[true] + values()[false];
        end Test;
    "#;
    typecheck(resolve(parse(source)).unwrap()).expect("function output index domains");
}

const QUALIFIED_ENUM_TABLE: &str = r#"
    package Digital
        package Interfaces
            type Logic = enumeration(low, high);
            type Other = enumeration(low, high);
        end Interfaces;
        package Tables
            import L = Digital.Interfaces.Logic;
            constant Digital.Interfaces.Logic AndTable[L, L] =
                [L.low, L.low; L.low, L.high];
        end Tables;
    end Digital;
    model Test
        Digital.Interfaces.Logic left = Digital.Interfaces.Logic.low;
        Digital.Interfaces.Logic right = Digital.Interfaces.Logic.high;
        Digital.Interfaces.Other other = Digital.Interfaces.Other.low;
        Digital.Interfaces.Logic outputValue;
    equation
        outputValue = Digital.Tables.AndTable[left, right];
    end Test;
"#;

#[test]
fn qualified_constant_table_retains_imported_enumeration_domains() {
    typecheck(resolve(parse(QUALIFIED_ENUM_TABLE)).unwrap()).expect("qualified enum-domain table");
    let mut instanced = rumoca_phase_instantiate::instantiate(
        resolve(parse(QUALIFIED_ENUM_TABLE)).unwrap(),
        "Test",
    )
    .expect("instantiate qualified enum table");
    typecheck_instanced(&instanced.tree, &mut instanced.overlay, "Test")
        .expect("instanced qualified enum-domain table");
}

#[test]
fn qualified_constant_table_rejects_a_different_enumeration_domain() {
    let source = QUALIFIED_ENUM_TABLE.replace("AndTable[left, right]", "AndTable[other, right]");
    let diagnostics = typecheck_diagnostics(&source);
    assert!(
        diagnostics.iter().any(|diagnostic| diagnostic
            .message
            .contains("array subscript must have")
            && diagnostic.message.contains("Logic")
            && diagnostic.message.contains("Other")),
        "{diagnostics:?}"
    );
    let source = QUALIFIED_ENUM_TABLE.replace("AndTable[left, right]", "AndTable[1, right]");
    let diagnostics = typecheck_diagnostics(&source);
    assert!(
        diagnostics.iter().any(|diagnostic| diagnostic
            .message
            .contains("array subscript must have")
            && diagnostic.message.contains("Logic")
            && diagnostic.message.contains("Integer")),
        "{diagnostics:?}"
    );
}

#[test]
fn member_index_domains_follow_inherited_declarations() {
    let source = r#"
        record Base
            Real a[Boolean];
        end Base;
        record Derived
            extends Base;
        end Derived;
        model Test
            Derived value; Real y;
        equation
            y = value.a[true];
        end Test;
    "#;
    typecheck(resolve(parse(source)).unwrap()).expect("inherited Boolean array domain");
}

#[test]
fn instanced_typecheck_rejects_real_parameter_before_evaluation() {
    let source = r#"
        model Test
            type Index = Real;
            parameter Index k = 1;
            discrete Real a[2];
        equation
            when time > 1 then a[k] = 1; end when;
        end Test;
    "#;
    let tree = resolve(parse(source)).unwrap().into_inner();
    let model = tree.get_class_by_qualified_name("Test").unwrap();
    let mut overlay = InstanceOverlay::new();
    for (name, component) in &model.components {
        add_test_instance(&mut overlay, name, component, component.binding.clone());
    }
    let diagnostics = typecheck_instanced(&tree, &mut overlay, "Test")
        .expect_err("source Real type survives value evaluation");
    assert!(
        diagnostics.iter().any(|diagnostic| diagnostic
            .message
            .contains("subscript must have Integer type")),
        "{diagnostics:?}"
    );
}
