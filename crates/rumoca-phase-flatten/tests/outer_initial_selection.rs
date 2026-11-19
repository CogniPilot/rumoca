//! MLS §5.4: an `outer` element is the matching `inner` element, so an
//! if-equation selected at translation reads the `inner` element's values,
//! not the defaults of the class the `outer` declaration names.

use rumoca_ir_flat as flat;

const SOURCE_NAME: &str = "<outer_initial_selection>";
const SOURCE: &str = r#"
package P
  model System
    parameter Boolean fixInitial = false;
    parameter Integer mode = 1;
    parameter Real level = 0;
  end System;
  model Part
    outer System system;
    parameter Boolean fix = system.fixInitial;
    Real x(start = 0);
    Real y(start = 0);
    Real z(start = 0);
    Real w(start = 0);
  equation
    der(x) = -x;
    der(y) = -y;
    der(z) = -z;
    der(w) = -w;
  initial equation
    if fix then
      x = 5;
    end if;
    if system.mode == 2 then
      y = 5;
    end if;
    if system.level > 1 then
      z = 5;
    end if;
    if system.fixInitial then
      w = 5;
    end if;
  end Part;
  model Modified
    inner System system(fixInitial = true, mode = 2, level = 3);
    Part c;
  end Modified;
  model Defaults
    inner System system;
    Part c;
  end Defaults;
  model Nested
    inner System system(fixInitial = true, mode = 2, level = 3);
    model Middle
      Part c;
    end Middle;
    Middle m;
  end Nested;
  model Shadowed
    inner System system;
    model Middle
      inner System system(fixInitial = true, mode = 2, level = 3);
      Part c;
    end Middle;
    Middle m;
    Part d;
  end Shadowed;
end P;
"#;

fn flatten(model: &str) -> flat::Model {
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, SOURCE_NAME).expect("source parses");
    let mut tree = rumoca_ir_ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, SOURCE);
    let resolved = rumoca_phase_resolve::resolve(rumoca_ir_ast::ParsedTree::new(tree))
        .expect("source resolves");
    let mut instanced =
        rumoca_phase_instantiate::instantiate(resolved, model).expect("source instantiates");
    rumoca_phase_typecheck::typecheck_instanced(&instanced.tree, &mut instanced.overlay, model)
        .expect("source typechecks");
    rumoca_phase_flatten::flatten_ref(&instanced.tree, &instanced.overlay, model)
        .expect("source flattens")
}

/// The variables the model's initial equations constrain, sorted.
fn initialized(flat: &flat::Model) -> Vec<String> {
    let mut names = Vec::new();
    for equation in &flat.initial_equations {
        let mut refs = Vec::new();
        equation.residual.collect_var_refs(&mut refs);
        names.extend(refs.iter().map(|name| name.as_str().to_string()));
    }
    names.sort();
    names
}

#[test]
fn an_initial_selection_reads_the_inner_modification() {
    let flat = flatten("P.Modified");
    assert_eq!(initialized(&flat), ["c.w", "c.x", "c.y", "c.z"]);
}

#[test]
fn an_initial_selection_reads_the_inner_defaults_when_unmodified() {
    let flat = flatten("P.Defaults");
    assert!(initialized(&flat).is_empty(), "{:?}", initialized(&flat));
}

#[test]
fn an_initial_selection_reads_an_inner_above_the_enclosing_class() {
    let flat = flatten("P.Nested");
    assert_eq!(initialized(&flat), ["m.c.w", "m.c.x", "m.c.y", "m.c.z"]);
}

#[test]
fn an_initial_selection_reads_the_nearest_inner() {
    let flat = flatten("P.Shadowed");
    assert_eq!(initialized(&flat), ["m.c.w", "m.c.x", "m.c.y", "m.c.z"]);
}
