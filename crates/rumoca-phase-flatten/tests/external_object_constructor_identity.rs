//! ExternalObject construction is an executable nested function call, whereas
//! record construction is structural.  The two source forms deliberately look
//! alike; Flat must retain their resolved declaration identities and must not
//! infer semantics from the rendered call spelling.

use rumoca_core::Expression;
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

const SOURCE: &str = r#"
class Handle
  extends ExternalObject;

  function constructor
    input Real seed;
    output Handle handle;
    external "C" handle = make_handle(seed);
  end constructor;

  function destructor
    input Handle handle;
    external "C" free_handle(handle);
  end destructor;
end Handle;

record Pair
  Real left;
  Real right;
end Pair;

function addPair
  input Pair pair;
  output Real total;
algorithm
  total := pair.left + pair.right;
end addPair;

model UsesBoth
  parameter Handle handle = Handle(1.0);
  parameter Real total = addPair(Pair(2.0, 3.0));
end UsesBoth;
"#;

struct Fixture {
    model: flat::Model,
    handle_class_def_id: rumoca_core::DefId,
    constructor_def_id: rumoca_core::DefId,
    constructor_span: rumoca_core::Span,
}

fn flatten_source() -> Fixture {
    let file_name = "<external_object_constructor_identity>";
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, file_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let handle = resolved
        .inner()
        .get_class_by_qualified_name("Handle")
        .expect("Handle resolves");
    let handle_class_def_id = handle.def_id.expect("Handle has declaration identity");
    let constructor = handle
        .classes
        .get("constructor")
        .expect("Handle constructor resolves");
    let constructor_def_id = constructor
        .def_id
        .expect("constructor has declaration identity");
    let constructor_span = constructor.location.span();
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, "UsesBoth").expect("model instantiates");
    let ast::InstancedTree { tree, mut overlay } = instanced;
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "UsesBoth")
        .expect("model typechecks");
    let model =
        rumoca_phase_flatten::flatten_ref(&tree, &overlay, "UsesBoth").expect("model flattens");
    Fixture {
        model,
        handle_class_def_id,
        constructor_def_id,
        constructor_span,
    }
}

fn binding<'model>(model: &'model flat::Model, variable: &str) -> &'model Expression {
    model
        .variables
        .get(&rumoca_core::VarName::new(variable))
        .and_then(|variable| variable.binding.as_ref())
        .unwrap_or_else(|| panic!("flat variable `{variable}` has no binding"))
}

fn source_text(span: rumoca_core::Span) -> &'static str {
    &SOURCE[span.start.0..span.end.0]
}

#[test]
fn external_object_constructor_is_executable_while_record_constructor_is_structural() {
    let fixture = flatten_source();
    let model = &fixture.model;

    let handle = model
        .functions
        .get(&rumoca_core::VarName::new("Handle"))
        .unwrap_or_else(|| {
            let names = model
                .functions
                .keys()
                .map(rumoca_core::VarName::as_str)
                .collect::<Vec<_>>();
            panic!(
                "ExternalObject constructor is exposed under its callable type name; got {names:?}"
            )
        });
    assert!(
        !handle.is_constructor,
        "ExternalObject construction must not synthesize structural record fields"
    );
    assert!(
        handle.external.is_some(),
        "ExternalObject construction must retain the nested external function body"
    );
    assert_eq!(handle.inputs.len(), 1);
    assert_eq!(handle.def_id, Some(fixture.constructor_def_id));
    assert_ne!(handle.def_id, Some(fixture.handle_class_def_id));
    assert_eq!(handle.span, fixture.constructor_span);
    let declaration_text = source_text(handle.span).trim_start();
    assert!(
        declaration_text.starts_with("constructor")
            && declaration_text.ends_with("end constructor"),
        "unexpected constructor declaration provenance: {declaration_text:?}"
    );

    let add_pair = model
        .functions
        .get(&rumoca_core::VarName::new("addPair"))
        .expect("ordinary Modelica function is collected");
    assert!(!add_pair.is_constructor);
    assert!(add_pair.external.is_none());
    assert_eq!(
        add_pair.inputs.len(),
        2,
        "record input is structurally scalarized to its two fields"
    );

    let Expression::FunctionCall {
        name,
        is_constructor,
        span,
        ..
    } = binding(model, "handle")
    else {
        panic!("Handle binding is a function call");
    };
    assert!(!is_constructor);
    assert_eq!(name.target_def_id(), Some(fixture.handle_class_def_id));
    assert_eq!(
        name.resolved_function()
            .map(|resolved| resolved.instance_id),
        handle.instance_id
    );
    assert_eq!(source_text(*span), "Handle(1.0)");

    let Expression::FunctionCall {
        args,
        is_constructor,
        ..
    } = binding(model, "total")
    else {
        panic!("addPair binding is a function call");
    };
    assert!(!is_constructor);
    assert_eq!(
        args.len(),
        2,
        "record construction is structural and supplies the scalarized fields"
    );
}
