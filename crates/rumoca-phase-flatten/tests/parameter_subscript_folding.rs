//! Regression coverage for array subscripts written as a `parameter`/`constant`
//! reference (MLS §4.5: an array subscript must be evaluable at compile time).
//!
//! `Modelica.Electrical.QuasiStatic.Polyphase.Basic.PlugToPin_p` is the shape
//! that motivated this: it declares
//!
//! ```modelica
//! parameter Integer k = 1 annotation(Evaluate=true);
//! ...
//! pin_p.v = plug_p.pin[k].v;
//! ```
//!
//! Before the fix `collapse_index_refs_to_known_varrefs` only folded integer
//! *literal* subscripts, so `plug_p.pin[k].v` survived flattening verbatim.
//! Two things then went wrong downstream: a record-valued potential (`Complex
//! v`) was never scalarized into its `.re`/`.im` rows because the rendered name
//! matched no flat variable, and the `todae` scalar-size inference sized the
//! reference as the whole `pin` array, inflating the row count (`ED001`).
//!
//! The folding must stay restricted to `constant`/`parameter` variables with a
//! compile-time binding; a subscript that depends on a discrete or continuous
//! variable has no single value and must be left untouched.

use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

fn seed_exact_predefined_type_identities(
    tree: &ast::ClassTree,
    overlay: &mut ast::InstanceOverlay,
) {
    for (name, type_id) in [
        ("Real", tree.type_table.real()),
        ("Integer", tree.type_table.integer()),
        ("Boolean", tree.type_table.boolean()),
        ("String", tree.type_table.string()),
        (
            "Clock",
            tree.type_table
                .lookup("Clock")
                .expect("resolved tree owns predefined Clock"),
        ),
    ] {
        let def_id = tree
            .scope_tree
            .predefined_member(&rumoca_core::ComponentPath::from_flat_path(name))
            .expect("resolved tree owns exact predefined declaration identity");
        overlay.type_ids_by_def_id.insert(def_id, type_id);
        overlay.type_roots.insert(type_id, type_id);
    }
}

/// Parse, resolve, instantiate and flatten `source`, returning the flat model.
fn flatten_source(source: &str, model: &str) -> flat::Model {
    let file_name = "<parameter_subscript_folding>";
    let stored = rumoca_phase_parse::parse_to_ast(source, file_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, model).expect("model instantiates");
    let ast::InstancedTree { tree, mut overlay } = instanced;
    seed_exact_predefined_type_identities(&tree, &mut overlay);
    rumoca_phase_flatten::flatten_ref(&tree, &overlay, model).expect("model flattens")
}

/// Collects every variable reference an expression tree mentions, rendering a
/// still-subscripted reference as `name[..]` so an unfolded subscript is
/// distinguishable from a folded one.
#[derive(Default)]
struct ReferenceCollector {
    names: Vec<String>,
}

impl rumoca_core::ExpressionVisitor for ReferenceCollector {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        if subscripts.is_empty() {
            self.names.push(name.as_str().to_string());
        } else {
            self.names.push(format!("{}[..]", name.as_str()));
        }
        self.walk_var_ref(name, subscripts);
    }
}

/// Every variable reference reachable from the model's equations, rendered as
/// its flat name.
fn equation_reference_names(model: &flat::Model) -> Vec<String> {
    use rumoca_core::ExpressionVisitor;
    let mut collector = ReferenceCollector::default();
    for equation in &model.equations {
        collector.visit_expression(&equation.residual);
    }
    collector.names
}

/// Assert that `names` contains `expected` and no name mentioning `forbidden`.
fn assert_folded(names: &[String], expected: &str, forbidden: &str) {
    assert!(
        names.iter().any(|name| name == expected),
        "expected folded reference `{expected}`; equation references were {names:?}"
    );
    assert!(
        !names.iter().any(|name| name.contains(forbidden)),
        "reference still carries the unfolded subscript `{forbidden}`: {names:?}"
    );
}

/// `PlugToPin_p` shape: the parameter subscript sits on the right-hand side of
/// a connector-potential equation.
const PARAMETER_SUBSCRIPT_ON_RHS: &str = r#"
    package P
      connector Pin
        Real v;
        flow Real i;
      end Pin;
      connector Plug
        parameter Integer m = 2;
        Pin pin[m];
      end Plug;
      model Tap
        parameter Integer m = 2;
        parameter Integer k = 1;
        Plug plug_p(final m = m);
        Pin pin_p;
      equation
        pin_p.v = plug_p.pin[k].v;
        plug_p.pin[1].i = 0;
        plug_p.pin[2].i = 0;
        pin_p.i = 0;
      end Tap;
      model Net
        Tap t(m = 2, k = 2);
      end Net;
    end P;
"#;

#[test]
fn parameter_subscript_on_rhs_folds_to_the_bound_index() {
    let model = flatten_source(PARAMETER_SUBSCRIPT_ON_RHS, "P.Net");
    let names = equation_reference_names(&model);
    assert_folded(&names, "t.plug_p.pin[2].v", "t.k");
}

/// Same shape with the parameter subscript on the left-hand side: this is the
/// variant that inflated `todae` row counts, because the unfolded reference was
/// sized as the whole `pin` array instead of one element.
const PARAMETER_SUBSCRIPT_ON_LHS: &str = r#"
    package P
      connector Pin
        Real v;
        flow Real i;
      end Pin;
      connector Plug
        parameter Integer m = 2;
        Pin pin[m];
      end Plug;
      model Tap
        parameter Integer k = 1;
        Plug plug(final m = 2);
        Real y;
      equation
        plug.pin[k].v = y;
        y = 7;
        plug.pin[1].i = 0;
        plug.pin[2].i = 0;
      end Tap;
      model Net
        Tap t(k = 2);
      end Net;
    end P;
"#;

#[test]
fn parameter_subscript_on_lhs_folds_to_the_bound_index() {
    let model = flatten_source(PARAMETER_SUBSCRIPT_ON_LHS, "P.Net");
    let names = equation_reference_names(&model);
    assert_folded(&names, "t.plug.pin[2].v", "t.k");
}

/// Arithmetic over structural parameters is still a compile-time subscript.
const PARAMETER_SUBSCRIPT_ARITHMETIC: &str = r#"
    package P
      connector Pin
        Real v;
        flow Real i;
      end Pin;
      connector Plug
        parameter Integer m = 3;
        Pin pin[m];
      end Plug;
      model Tap
        constant Integer offset = 1;
        parameter Integer k = 1;
        Plug plug(final m = 3);
        Real y;
      equation
        y = plug.pin[k + offset].v;
        plug.pin[1].i = 0;
        plug.pin[2].i = 0;
        plug.pin[3].i = 0;
      end Tap;
      model Net
        Tap t(k = 2);
      end Net;
    end P;
"#;

#[test]
fn parameter_arithmetic_subscript_folds() {
    let model = flatten_source(PARAMETER_SUBSCRIPT_ARITHMETIC, "P.Net");
    let names = equation_reference_names(&model);
    assert_folded(&names, "t.plug.pin[3].v", "t.k");
}

/// `Modelica.Electrical.QuasiStatic.Polyphase.Basic.PlugToPins_p` shape: the
/// tap is itself an *array component*, so its own index sits in the middle of
/// the path (`taps.tap[2].plug.pin[k].v`). Flatten leaves such a reference as a
/// field-access chain rather than one dotted `VarRef`, which the collapse pass
/// has to render before it can fold the trailing subscript.
/// The MSL connector's potential is a *record* (`Complex v`), so the flat model
/// only holds the `.re`/`.im` leaves; the folded name has to be recovered as a
/// scalarized-record base rather than matched directly.
const PARAMETER_SUBSCRIPT_IN_COMPONENT_ARRAY: &str = r#"
    package P
      record Cplx
        Real re;
        Real im;
      end Cplx;
      connector Pin
        Cplx v;
        flow Cplx i;
      end Pin;
      connector Plug
        parameter Integer m = 2;
        Pin pin[m];
      end Plug;
      model Tap
        parameter Integer m = 2;
        parameter Integer k = 1;
        Plug plug(final m = m);
        Pin pin_p;
      equation
        pin_p.v = plug.pin[k].v;
        for j in 1:m loop
          plug.pin[j].i.re = if j == k then -pin_p.i.re else 0;
          plug.pin[j].i.im = if j == k then -pin_p.i.im else 0;
        end for;
      end Tap;
      model Taps
        parameter Integer m = 2;
        Tap tap[m](each final m = m, final k = {j for j in 1:m});
        Plug plug(final m = m);
        Pin pin_p[m];
      equation
        for j in 1:m loop
          connect(plug, tap[j].plug);
          connect(tap[j].pin_p, pin_p[j]);
        end for;
      end Taps;
      model Star
        parameter Integer m = 2;
        Taps taps(final m = m);
        Plug plug(final m = m);
        Pin pin_n;
      equation
        connect(plug, taps.plug);
        for j in 1:m loop
          connect(taps.pin_p[j], pin_n);
        end for;
      end Star;
      model Net
        Star star(m = 2);
      end Net;
    end P;
"#;

#[test]
fn parameter_subscript_inside_a_component_array_folds() {
    let model = flatten_source(PARAMETER_SUBSCRIPT_IN_COMPONENT_ARRAY, "P.Net");
    let names = equation_reference_names(&model);
    // `k` still legitimately appears in the `if j == k` guards; what must not
    // survive is a *subscript* that was never folded, which the collector
    // renders as a trailing `[..]`.
    assert!(
        !names.iter().any(|name| name.ends_with("pin[..]")),
        "the array-element tap still carries an unfolded subscript: {names:?}"
    );
    assert!(
        names
            .iter()
            .any(|name| name == "star.taps.tap[2].plug.pin[2].v"),
        "expected `star.taps.tap[2].plug.pin[2].v`; references were {names:?}"
    );
}

/// A subscript that depends on a *variable* has no compile-time value, so the
/// reference must survive folding unchanged.
const VARIABLE_SUBSCRIPT: &str = r#"
    package P
      connector Pin
        Real v;
        flow Real i;
      end Pin;
      connector Plug
        parameter Integer m = 2;
        Pin pin[m];
      end Plug;
      model Tap
        Plug plug(final m = 2);
        Integer j;
        Real y;
      equation
        j = 1 + integer(time);
        y = plug.pin[j].v;
        plug.pin[1].i = 0;
        plug.pin[2].i = 0;
      end Tap;
      model Net
        Tap t;
      end Net;
    end P;
"#;

#[test]
fn variable_subscript_is_not_folded() {
    let model = flatten_source(VARIABLE_SUBSCRIPT, "P.Net");
    let names = equation_reference_names(&model);
    assert!(
        names.iter().any(|name| name.contains("t.j")),
        "a variable subscript must be left symbolic; references were {names:?}"
    );
    assert!(
        !names.iter().any(|name| name == "t.plug.pin[1].v"),
        "a variable subscript must not be folded to an element: {names:?}"
    );
}
