//! MLS §7.2.5 and §10.5.1: non-each modifiers select the component-array
//! coordinate even when the value starts with a package namespace.
use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const ELEMENT: &str = "model Element parameter Real q[4]={1,0,0,0}; end Element;";
const DATA: &str = "package Data constant Real m[3,4]={{1,10,100,1000},{2,20,200,2000},{3,30,300,3000}}; end Data;";

fn derivatives(package: &str, body: &str) -> Vec<f64> {
    let source = format!(
        r#"{ELEMENT}
      {package}
      model Probe
        {body}
        Real state[8](each start=0,each fixed=true);
      equation
        der(state)={{e[1].q[1],e[1].q[2],e[1].q[3],e[1].q[4],
          e[2].q[1],e[2].q[2],e[2].q[3],e[2].q[4]}};
      end Probe;"#
    );
    let compiled = Compiler::new()
        .model("Probe")
        .compile_str(&source, "package-modifier.mo")
        .expect("a package-owned array modifier must select each component's row");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("evaluate initial derivatives");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    probe
        .report
        .derivatives
        .iter()
        .map(|slot| slot.value)
        .collect()
}

#[test]
fn qualified_package_slice_selects_distinct_rows() {
    assert_eq!(
        derivatives(DATA, "Element e[2](q=Data.m[2:3,:]);"),
        [2.0, 20.0, 200.0, 2000.0, 3.0, 30.0, 300.0, 3000.0]
    );
}

#[test]
fn package_alias_preserves_reversed_selection() {
    assert_eq!(
        derivatives(DATA, "import D=Data; Element e[2](q=D.m[3:-2:1,:]);"),
        [3.0, 30.0, 300.0, 3000.0, 1.0, 10.0, 100.0, 1000.0]
    );
}

#[test]
fn nested_package_namespace_is_not_an_instance_coordinate() {
    let package = format!("package Outer {DATA} end Outer;");
    assert_eq!(
        derivatives(&package, "Element e[2](q=Outer.Data.m[1:2,:]);"),
        [1.0, 10.0, 100.0, 1000.0, 2.0, 20.0, 200.0, 2000.0]
    );
}

#[test]
fn a_local_component_shadows_the_outer_package_namespace() {
    let declarations = [
        DATA,
        "model Container parameter Real m[2,4]={{4,40,400,4000},{5,50,500,5000}}; end Container;",
    ]
    .join("\n");
    assert_eq!(
        derivatives(&declarations, "Container Data; Element e[2](q=Data.m);"),
        [4.0, 40.0, 400.0, 4000.0, 5.0, 50.0, 500.0, 5000.0]
    );
}

#[test]
fn each_keeps_the_same_whole_row_for_both_components() {
    assert_eq!(
        derivatives(DATA, "Element e[2](each q=Data.m[2,:]);"),
        [2.0, 20.0, 200.0, 2000.0, 2.0, 20.0, 200.0, 2000.0]
    );
}

#[test]
fn an_out_of_bounds_package_selection_is_not_repaired() {
    let source = format!(
        "{ELEMENT} {DATA} model Probe Element e[2](q=Data.m[4:5,:]); Real x=e[1].q[1]; end Probe;"
    );
    assert!(
        Compiler::new()
            .model("Probe")
            .compile_str(&source, "invalid-package-modifier.mo")
            .is_err()
    );
}
