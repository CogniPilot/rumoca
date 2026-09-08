//! An operator-record constructor selected through its constructor function is
//! still a structural constructor in final Flat IR.
//!
//! The source occurrence initially identifies `C.'constructor'.fromReal`, while
//! function collection represents the identity conversion as the synthetic
//! `C` constructor signature. Exact `FunctionInstanceId` canonicalization must
//! therefore complete the occurrence kind without consulting the name `C`.

use rumoca_ir_ast as ast;

const SOURCE_NAME: &str = "<operator_record_constructor_kind>";
const SOURCE: &str = r#"
operator record C
  replaceable Real re;
  replaceable Real im;

  encapsulated operator 'constructor'
    function fromReal
      import C;
      input Real re;
      input Real im = 0;
      output C result(re = re, im = im);
    algorithm
    end fromReal;
  end 'constructor';
end C;

function makeRe
  input Real u;
  output Real result;
protected
  C value;
algorithm
  value := C(u);
  result := value.re;
end makeRe;

model UsesC
  Real x;
equation
  x = makeRe(1);
end UsesC;
"#;

fn typed_flat_model() -> (rumoca_ir_flat::Model, rumoca_core::SourceMap) {
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, SOURCE_NAME).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let overlay =
        match rumoca_phase_instantiate::instantiate_model_with_outcome(resolved.inner(), "UsesC") {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
    let typed = rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, "UsesC")
        .expect("instanced model typechecks");
    let source_map = resolved.inner().source_map.clone();
    let flat =
        rumoca_phase_flatten::flatten_typed(typed, rumoca_phase_flatten::FlattenOptions::default())
            .expect("typed model flattens");
    (flat, source_map)
}

#[test]
fn selected_operator_constructor_is_canonical_structural_constructor() {
    let (flat, source_map) = typed_flat_model();
    let make = flat
        .functions
        .get(&rumoca_core::VarName::new("makeRe"))
        .expect("reachable source function is collected");
    let constructor_call = make.body.iter().find_map(|statement| {
        let rumoca_core::Statement::Assignment { value, .. } = statement else {
            return None;
        };
        let rumoca_core::Expression::FunctionCall {
            name,
            is_constructor,
            ..
        } = value
        else {
            return None;
        };
        Some((name, is_constructor))
    });
    let Some((name, is_constructor)) = constructor_call else {
        panic!("makeRe retains its structural constructor expression");
    };
    let resolved = name
        .resolved_function()
        .expect("constructor call carries exact callable instance identity");
    let constructor =
        rumoca_core::resolve_function_instance(flat.functions.values(), resolved.instance_id)
            .expect("exact constructor instance is present exactly once");
    assert!(constructor.is_constructor);
    assert!(*is_constructor);
    assert!(constructor.outputs.is_empty());

    let _product = rumoca_phase_dae::construct(&flat, source_map)
        .expect("the proof-bearing structural constructor lowers without a fabricated output");
}
