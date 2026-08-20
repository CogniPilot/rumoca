//! Dimension evaluation driven by the instanced overlay: enum-alias
//! dependent extents, component-scoped extends modifiers, and table column
//! ranges.

use super::*;

#[test]
fn test_typecheck_instanced_evaluates_enum_alias_dependent_dimensions() {
    let source = r#"
        type ModelStructure = enumeration(av_vb, a_v_b);

        model Pipe
            parameter ModelStructure modelStructure = ModelStructure.av_vb;
            parameter Boolean useLumpedPressure = false;
            parameter Integer n = 2;
            final parameter Integer nFM =
                if useLumpedPressure then nFMLumped else nFMDistributed;
            final parameter Integer nFMDistributed =
                if modelStructure == ModelStructure.a_v_b then n + 1 else n;
            final parameter Integer nFMLumped =
                if modelStructure == ModelStructure.a_v_b then 2 else 1;
            Real pathLengths[nFM];
        end Pipe;

        model Network
            parameter ModelStructure pipeModelStructure = ModelStructure.av_vb;
            Pipe pipe1(final modelStructure = pipeModelStructure);
        end Network;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let pipe = tree
        .definitions
        .classes
        .get("Pipe")
        .expect("Pipe class should exist");
    let network = tree
        .definitions
        .classes
        .get("Network")
        .expect("Network class should exist");

    let mut overlay = InstanceOverlay::new();
    add_test_instance(
        &mut overlay,
        "pipeModelStructure",
        network.components.get("pipeModelStructure").unwrap(),
        Some(
            network
                .components
                .get("pipeModelStructure")
                .unwrap()
                .binding
                .clone()
                .expect("pipeModelStructure binding"),
        ),
    );
    add_test_instance(
        &mut overlay,
        "pipe1.modelStructure",
        pipe.components.get("modelStructure").unwrap(),
        Some(Expression::ComponentReference(make_comp_ref(
            "pipeModelStructure",
        ))),
    );
    for name in [
        "useLumpedPressure",
        "n",
        "nFM",
        "nFMDistributed",
        "nFMLumped",
        "pathLengths",
    ] {
        let component = pipe
            .components
            .get(name)
            .expect("Pipe component should exist");
        add_test_instance(
            &mut overlay,
            &format!("pipe1.{name}"),
            component,
            component.binding.clone(),
        );
    }

    typecheck_instanced(&tree, &mut overlay, "Network")
        .expect("typecheck_instanced should evaluate enum-dependent dimensions");

    let path_lengths = overlay
        .components
        .values()
        .find(|data| data.qualified_name.to_flat_string() == "pipe1.pathLengths")
        .expect("pathLengths instance");
    assert_eq!(
        path_lengths.dims,
        vec![2],
        "nFM should evaluate through the enum-valued modelStructure alias"
    );
}

#[test]
fn test_typecheck_instanced_evaluates_component_scoped_extends_modifier_dimensions() {
    let source = r#"
        partial block Base
            parameter Integer nout = 1;
            Real y[nout];
        end Base;

        block Table
            extends Base(final nout = size(columns, 1));
            parameter Integer columns[:] = 2:2;
        end Table;

        model Test
            Table table;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let base = tree
        .definitions
        .classes
        .get("Base")
        .expect("Base class should exist");
    let table = tree
        .definitions
        .classes
        .get("Table")
        .expect("Table class should exist");
    let test = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");
    let mut overlay = InstanceOverlay::new();
    add_test_instance(
        &mut overlay,
        "table",
        test.components.get("table").expect("table component"),
        None,
    );

    let columns = table.components.get("columns").expect("columns component");
    add_test_instance(
        &mut overlay,
        "table.columns",
        columns,
        columns.binding.clone(),
    );
    add_test_instance(
        &mut overlay,
        "table.y",
        base.components.get("y").expect("inherited y component"),
        None,
    );

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("typecheck_instanced should evaluate scoped extends modifier dimensions");

    let y = overlay
        .components
        .values()
        .find(|data| data.qualified_name.to_flat_string() == "table.y")
        .expect("table.y instance");
    assert_eq!(
        y.dims,
        vec![1],
        "inherited y[nout] should use nout from the component-scoped extends modifier"
    );
}

#[test]
fn test_typecheck_instanced_evaluates_table_column_range_dimensions() {
    let tree = table_column_range_tree();
    let mut overlay = table_column_range_overlay(&tree);

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("typecheck_instanced should evaluate table-driven dimensions");

    let u = overlay
        .components
        .values()
        .find(|data| data.qualified_name.to_flat_string() == "core.tabris.u")
        .expect("core.tabris.u instance");
    assert_eq!(u.dims, vec![1], "table inputs should use columns dimension");
}

fn table_column_range_tree() -> ClassTree {
    let parsed = parse(table_column_range_source());
    resolve(parsed)
        .expect("resolve should succeed")
        .into_inner()
}

fn table_column_range_source() -> &'static str {
    r#"
    partial block MIMOs
        parameter Integer n = 1;
        Real u[n];
        Real y[n];
    end MIMOs;

    block CombiTable1Dv
        extends MIMOs(final n = size(columns, 1));
        parameter Real table[:, :] = fill(0.0, 0, 2);
        parameter Integer columns[:] = 2:size(table, 2);
    end CombiTable1Dv;

    record Material
        parameter Real tabris[:, 2] = [1, 10; 2, 20; 3, 30];
    end Material;

    model GenericHystTellinenTable
        parameter Material mat = Material();
        CombiTable1Dv tabris(table = mat.tabris);
    end GenericHystTellinenTable;

    model Test
        GenericHystTellinenTable core;
    end Test;
    "#
}

fn table_column_range_overlay(tree: &ClassTree) -> InstanceOverlay {
    let mimos = tree.definitions.classes.get("MIMOs").expect("MIMOs class");
    let table = tree
        .definitions
        .classes
        .get("CombiTable1Dv")
        .expect("CombiTable1Dv class");
    let material = tree
        .definitions
        .classes
        .get("Material")
        .expect("Material class");
    let generic = tree
        .definitions
        .classes
        .get("GenericHystTellinenTable")
        .expect("GenericHystTellinenTable class");
    let test = tree.definitions.classes.get("Test").expect("Test class");
    let table_binding = generic
        .components
        .get("tabris")
        .expect("tabris component")
        .modifications
        .get("table")
        .cloned()
        .expect("tabris(table = ...) modifier");

    let mut overlay = InstanceOverlay::new();
    add_test_instance(
        &mut overlay,
        "core",
        test.components.get("core").expect("core component"),
        None,
    );
    add_test_instance(
        &mut overlay,
        "core.mat",
        generic.components.get("mat").expect("mat component"),
        generic
            .components
            .get("mat")
            .expect("mat component")
            .binding
            .clone(),
    );
    add_test_instance(
        &mut overlay,
        "core.mat.tabris",
        material
            .components
            .get("tabris")
            .expect("mat.tabris component"),
        material
            .components
            .get("tabris")
            .expect("mat.tabris component")
            .binding
            .clone(),
    );
    add_test_instance(
        &mut overlay,
        "core.tabris",
        generic.components.get("tabris").expect("tabris component"),
        None,
    );
    add_test_instance(
        &mut overlay,
        "core.tabris.table",
        table.components.get("table").expect("table component"),
        Some(table_binding),
    );
    add_test_instance(
        &mut overlay,
        "core.tabris.columns",
        table.components.get("columns").expect("columns component"),
        table
            .components
            .get("columns")
            .expect("columns component")
            .binding
            .clone(),
    );
    add_test_instance(
        &mut overlay,
        "core.tabris.u",
        mimos.components.get("u").expect("u component"),
        None,
    );
    add_test_instance(
        &mut overlay,
        "core.tabris.y",
        mimos.components.get("y").expect("y component"),
        None,
    );
    overlay
}
