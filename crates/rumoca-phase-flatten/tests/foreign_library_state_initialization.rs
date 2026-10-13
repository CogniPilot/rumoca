//! Library state initialized from a parameter binding or an initial equation
//! (MLS 3.7 §12.3; SPEC_0040 FLAT-C05). Both execute once at initialization,
//! so the state accesses they hold are ordered there; any other position
//! that reaches the state is refused.

use rumoca_core::{Expression, VarName};
use rumoca_ir_ast as ast;

const PACKAGE: &str = r#"
  impure function setInternalState
    input Integer[33] rngState;
    input Integer id;
  external "C" ModelicaRandom_setInternalState_xorshift1024star(rngState, size(rngState, 1), id);
  end setInternalState;
  function initialize
    input Integer seed;
    output Integer id;
  protected
    Integer rngState[33];
  algorithm
    rngState := fill(seed, 33);
    id := 7;
    setInternalState(rngState, id);
  end initialize;
  impure function impureRandom
    input Integer id;
    output Real y;
  external "C" y = ModelicaRandom_impureRandom_xorshift1024star(id);
  end impureRandom;
"#;

const DRAW: &str = "algorithm\n  when sample(0, 0.1) then\n    r := impureRandom(id);\n  end when;";

fn model(name: &str, declarations: &str, body: &str) -> String {
    format!("model {name}\n{PACKAGE}\n{declarations}\n{body}\nend {name};\n")
}

fn flatten(source: &str, name: &str) -> Result<rumoca_ir_flat::Model, String> {
    let file = format!("{name}.mo");
    let parsed = rumoca_phase_parse::parse_to_ast(source, &file).unwrap();
    let mut tree = ast::ClassTree::from_parsed(parsed);
    tree.source_map.add(&file, source);
    let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).unwrap();
    let instanced = rumoca_phase_instantiate::instantiate(resolved, name).unwrap();
    let ast::InstancedTree { tree, mut overlay } = instanced;
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, name).unwrap();
    rumoca_phase_flatten::flatten_ref_with_options(&tree, &overlay, name, Default::default())
        .map_err(|error| format!("{error:?}"))
}

/// The name of the function an expression calls, if it is a call.
fn callee(expression: &Expression) -> Option<String> {
    match expression {
        Expression::FunctionCall { name, .. } => Some(name.var_name().as_str().to_string()),
        _ => None,
    }
}

/// The calls on either side of an initial equation's residual.
fn residual_callees(equation: &rumoca_ir_flat::Equation) -> Vec<String> {
    match &equation.residual {
        Expression::Binary { lhs, rhs, .. } => [lhs, rhs]
            .into_iter()
            .filter_map(|side| callee(side))
            .collect(),
        other => callee(other).into_iter().collect(),
    }
}

fn refused(source: &str, name: &str) -> String {
    flatten(source, name).expect_err("the access order is unspecified")
}

#[test]
fn parameter_binding_initializes_the_library_state() {
    let source = model(
        "Binding",
        "  parameter Integer id = initialize(5);\n  discrete Real r(start = 0, fixed = true);",
        DRAW,
    );
    let flat = flatten(&source, "Binding").expect("a parameter binding is ordered");
    let binding = flat.variables[&VarName::new("id")]
        .binding
        .as_ref()
        .unwrap();
    assert!(
        callee(binding)
            .unwrap()
            .ends_with("initialize__foreign_state")
    );
}

#[test]
fn initial_equation_defining_a_parameter_initializes_the_library_state() {
    for equation in ["id = initialize(5);", "initialize(5) = id;"] {
        let source = model(
            "InitialEquation",
            "  parameter Integer id(fixed = false);\n  discrete Real r(start = 0, fixed = true);",
            &format!("initial equation\n  {equation}\n{DRAW}"),
        );
        let flat = flatten(&source, "InitialEquation").expect("an initial equation is ordered");
        let calls = flat
            .initial_equations
            .iter()
            .flat_map(residual_callees)
            .collect::<Vec<_>>();
        assert_eq!(calls.len(), 1, "{equation}: {calls:?}");
        assert!(calls[0].ends_with("initialize__foreign_state"), "{calls:?}");
        assert!(
            flat.functions
                .keys()
                .all(|name| name.as_str() != "InitialEquation.initialize"),
            "the unthreaded function is replaced"
        );
    }
}

#[test]
fn initial_equation_initializes_a_state_no_section_draws_from() {
    let source = model(
        "Undrawn",
        "  parameter Integer id(fixed = false);",
        "initial equation\n  id = initialize(5);",
    );
    flatten(&source, "Undrawn").expect("an undrawn state is still initialized in order");
}

#[test]
fn equation_reaching_the_library_state_is_refused() {
    let declarations =
        "  parameter Integer id = initialize(5);\n  discrete Real r(start = 0, fixed = true);";
    let source = model(
        "Equation",
        declarations,
        "equation\n  when sample(0, 0.1) then\n    r = impureRandom(id);\n  end when;",
    );
    assert!(
        refused(&source, "Equation").contains("UnorderedForeignState"),
        "when equation"
    );
}

#[test]
fn initial_equation_defining_a_variable_is_refused() {
    let source = model(
        "VariableInitial",
        "  parameter Integer id = initialize(5);\n  Real r(start = 0);",
        "initial equation\n  r = impureRandom(id);",
    );
    let error = refused(&source, "VariableInitial");
    assert!(error.contains("UnorderedForeignState"), "{error}");
}

#[test]
fn two_initializers_of_one_state_are_refused() {
    let cases = [
        (
            "BindingAndEquation",
            "  parameter Integer id = initialize(5);\n  parameter Integer other(fixed = false);",
            "initial equation\n  other = initialize(6);",
        ),
        (
            "TwoEquations",
            "  parameter Integer id(fixed = false);\n  parameter Integer other(fixed = false);",
            "initial equation\n  id = initialize(5);\n  other = initialize(6);",
        ),
    ];
    for (name, declarations, body) in cases {
        let error = refused(&model(name, declarations, body), name);
        assert!(
            error.contains("so their order is unspecified"),
            "{name}: {error}"
        );
    }
}
