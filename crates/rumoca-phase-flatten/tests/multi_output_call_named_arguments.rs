//! Named arguments in a multi-output tuple-assignment call statement resolve to
//! declaration-order positional slots, exactly as they do for an expression-position
//! call. The named spelling flattens to the same executable argument vector as the
//! positional spelling, and the same slot-binding refusals (MLS §12.4.1) apply.
//!
//! Before the tuple-call actuals were routed through the shared named-argument marker
//! projection, a named actual in `(q, r) := f(a = p, b = 2)` was lowered as a plain
//! expression, which stranded an `Expression::NamedArgument` that the Flat construction
//! boundary rejects. These fixtures guard both the accepted parity and the refusals.

use rumoca_core::{Expression, Statement, VarName};
use rumoca_ir_ast as ast;
use rumoca_phase_flatten::FlattenError;

fn two_output_source(call: &str) -> String {
    format!(
        r#"
function h2
  input Real a;
  input Real b;
  output Real x;
  output Real y;
algorithm
  x := a;
  y := b;
end h2;

function caller
  input Real p;
  output Real q;
protected
  Real r;
algorithm
  {call}
end caller;

model TupleCall
  Real z;
equation
  z = caller(1.0);
end TupleCall;
"#
    )
}

fn flatten_two_output(call: &str) -> Result<rumoca_ir_flat::Model, Box<FlattenError>> {
    let source = two_output_source(call);
    let file_name = "<multi_output_call_named_arguments>";
    let stored = rumoca_phase_parse::parse_to_ast(&source, file_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, &source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(
        resolved.inner(),
        "TupleCall",
    ) {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        rumoca_phase_instantiate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
            panic!("fixture instantiation failed: {error}")
        }
    };
    let typed = rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, "TupleCall")
        .expect("model typechecks");
    rumoca_phase_flatten::flatten_typed(typed, rumoca_phase_flatten::FlattenOptions::default())
        .map_err(Box::new)
}

// Return the executable argument vector of the single multi-output call statement in
// the flattened `caller` body. Named actuals are already resolved to positional order
// by this point, so this vector is the parity witness between the two spellings.
fn caller_call_args(model: &rumoca_ir_flat::Model) -> Vec<Expression> {
    let caller = model
        .functions
        .get(&VarName::new("caller"))
        .expect("caller function is collected");
    let call = caller
        .body
        .iter()
        .find_map(|statement| match statement {
            Statement::FunctionCall { args, outputs, .. } if outputs.len() == 2 => Some(args),
            _ => None,
        })
        .expect("caller body holds the two-output call statement");
    call.clone()
}

#[test]
fn named_tuple_call_matches_positional_lowering() {
    let named = flatten_two_output("(q, r) := h2(a = p, b = 2.0);")
        .expect("named actuals in a multi-output call flatten cleanly");
    let positional =
        flatten_two_output("(q, r) := h2(p, 2.0);").expect("positional spelling flattens cleanly");

    let named_args = caller_call_args(&named);
    let positional_args = caller_call_args(&positional);

    // The named spelling resolves to the same two positional actuals as the explicit
    // positional spelling: the source-level formal `p` followed by the literal `2.0`.
    // No `__rumoca_named_arg__` marker and no `Expression::NamedArgument` remains.
    assert_eq!(named_args.len(), 2);
    assert_eq!(named_args.len(), positional_args.len());
    assert!(matches!(named_args[0], Expression::VarRef { .. }));
    assert!(matches!(named_args[1], Expression::Literal { .. }));
    for spelling in [&named_args, &positional_args] {
        for argument in spelling.iter() {
            if let Expression::FunctionCall { name, .. } = argument {
                assert!(
                    !name
                        .as_str()
                        .starts_with(rumoca_core::NAMED_FUNCTION_ARG_PREFIX),
                    "no named-argument marker survives slot resolution"
                );
            }
        }
    }
}

#[test]
fn named_tuple_call_rejects_unknown_formal() {
    let error = flatten_two_output("(q, r) := h2(a = p, c = 2.0);")
        .expect_err("a named actual naming no input slot is refused");
    assert!(
        matches!(*error, FlattenError::InvalidFunctionCallArgs { ref reason, .. } if reason.contains("does not match any input")),
        "unexpected error: {error:?}"
    );
}

#[test]
fn named_tuple_call_rejects_duplicate_formal() {
    let error = flatten_two_output("(q, r) := h2(a = p, a = 2.0);")
        .expect_err("a doubly-filled input slot is refused");
    assert!(
        matches!(*error, FlattenError::InvalidFunctionCallArgs { ref reason, .. } if reason.contains("filled more than once")),
        "unexpected error: {error:?}"
    );
}

#[test]
fn named_tuple_call_rejects_missing_formal() {
    let error = flatten_two_output("(q, r) := h2(a = p);")
        .expect_err("an unfilled input slot with no default is refused");
    assert!(
        matches!(*error, FlattenError::InvalidFunctionCallArgs { ref reason, .. } if reason.contains("no argument and no default")),
        "unexpected error: {error:?}"
    );
}
