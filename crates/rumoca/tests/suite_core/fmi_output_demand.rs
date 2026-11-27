//! A single scalar observation of a tuple from a Modelica function may discard an
//! independent bounded fold. Both the full-tuple call and a fold whose assertion
//! can fire must retain the loop.
const SOURCE: &str = r#"
package DemandFixture
  function pair
    input Real x;
    output Real cheap;
    output Real expensive[24];
  algorithm
    cheap := if x > 0 then x*x+1 else 2*x+1;
    for j in 1:24 loop
      expensive[j] := sin(x*j);
      for k in 1:8 loop
        expensive[j] := sin(expensive[j]+x/(j+k));
      end for;
    end for;
  end pair;
  model First
    input Real x=1;
    output Real y=pair(x);
  end First;
  model Both
    input Real x=1;
    output Real y,z[24];
  equation
    (y,z)=pair(x);
  end Both;
  model Mixed
    input Real x=1;
    output Real y=pair(x), both, z[24];
  equation
    (both,z)=pair(x+1);
  end Mixed;
end DemandFixture;
"#;

fn generated(source: &str, model: &str) -> (String, String) {
    let class = format!("DemandFixture.{model}");
    let compiled = rumoca::Compiler::new()
        .model(&class)
        .compile_str(source, "DemandFixture.mo")
        .unwrap_or_else(|error| panic!("compile {class}: {error:#}"));
    let files = rumoca::render_target_files(&compiled, &class, "fmi3", None).unwrap();
    let file = |name| {
        files
            .iter()
            .find(|file| file.path == name)
            .unwrap()
            .content
            .clone()
    };
    (
        file("sources/rmc_assign.c"),
        file("sources/rmc_functions.c"),
    )
}

fn scalar_symbol(assign: &str) -> &str {
    assign
        .split("int status=")
        .nth(1)
        .unwrap()
        .split('(')
        .next()
        .unwrap()
}

#[test]
fn bounded_fold_is_pruned_only_for_an_independent_unread_result() {
    let (caller, body) = generated(SOURCE, "First");
    let leaf = scalar_symbol(&caller);
    assert!(leaf.contains("_reads_"));
    let typed = leaf.replace("scalar_", "");
    assert!(!body.lines().any(|line| line.starts_with("static int ")
        && line.contains(&format!("{typed}_region_"))
        && line.contains("_fold(")));
    let (caller, body) = generated(SOURCE, "Both");
    assert!(!scalar_symbol(&caller).contains("_reads_"));
    assert!(body.contains("_fold(") && body.contains("sin("));
    let (caller, _) = generated(SOURCE, "Mixed");
    let calls: Vec<_> = caller
        .split("int status=")
        .skip(1)
        .filter_map(|s| s.split_once('('))
        .map(|(name, _)| name)
        .collect();
    assert!(calls.iter().any(|name| name.contains("_reads_")));
    assert!(
        calls
            .iter()
            .any(|name| name.starts_with("rumoca_scalar_pure_") && !name.contains("_reads_"))
    );
}

#[test]
fn a_possible_assertion_inside_a_discarded_result_loop_is_observable() {
    let source = SOURCE.replace(
        "expensive[j] := sin(x*j);",
        "assert(x > -j, \"loop domain\"); expensive[j] := sin(x*j);",
    );
    let (caller, body) = generated(&source, "First");
    let typed = scalar_symbol(&caller).replace("scalar_", "");
    assert!(body.lines().any(|line| line.starts_with("static int ")
        && line.contains(&format!("{typed}_region_"))
        && line.contains("_fold(")));
    assert!(body.contains("observer->observe"));
}
