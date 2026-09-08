//! Named actuals to a builtin operator resolve to declaration-order positional
//! slots during AST lowering, exactly as they do for a user function call. The
//! named spelling flattens to the same executable argument vector as the
//! positional spelling, so `homotopy(actual = a, simplified = b)` and
//! `homotopy(a, b)` produce the same `BuiltinCall`.
//!
//! Before the builtin call site projected named actuals into positional order,
//! a named actual stranded an `Expression::NamedArgument` that the Flat
//! construction boundary rejects. These fixtures guard both the accepted parity
//! and the slot-binding refusals.

use rumoca_core::{BuiltinFunction, Expression};
use rumoca_ir_ast as ast;
use rumoca_phase_flatten::FlattenError;

fn model_source(call: &str) -> String {
    format!(
        r#"
model HomotopyCall
  Real a;
  Real b;
  Real x;
equation
  a = 1.0;
  b = 2.0;
  x = {call};
end HomotopyCall;
"#
    )
}

fn flatten_model(call: &str) -> Result<rumoca_ir_flat::Model, Box<FlattenError>> {
    let source = model_source(call);
    let file_name = "<builtin_operator_named_arguments>";
    let stored = rumoca_phase_parse::parse_to_ast(&source, file_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, &source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(
        resolved.inner(),
        "HomotopyCall",
    ) {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        rumoca_phase_instantiate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
            panic!("fixture instantiation failed: {error}")
        }
    };
    let typed =
        rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, "HomotopyCall")
            .expect("model typechecks");
    rumoca_phase_flatten::flatten_typed(typed, rumoca_phase_flatten::FlattenOptions::default())
        .map_err(Box::new)
}

// Return the argument vector of the single homotopy BuiltinCall in the flattened
// model. Named actuals are already resolved to positional order by this point,
// so this vector is the parity witness between the two spellings.
fn homotopy_args(model: &rumoca_ir_flat::Model) -> Vec<Expression> {
    let mut found: Option<Vec<Expression>> = None;
    for equation in &model.equations {
        equation.residual.contains_subexpression(|expression| {
            if let Expression::BuiltinCall {
                function: BuiltinFunction::Homotopy,
                args,
                ..
            } = expression
            {
                found = Some(args.clone());
            }
            false
        });
    }
    found.expect("flattened model holds the homotopy call")
}

// The named and positional spellings occupy different source columns, so their
// argument expressions carry different use-site spans. Parity is a claim about
// which variable fills each slot, not about source position, so compare each
// argument by its resolved variable identity.
fn slot_identities(args: &[Expression]) -> Vec<String> {
    args.iter()
        .map(|argument| match argument {
            Expression::VarRef { name, .. } => name.as_str().to_string(),
            other => panic!("expected a variable reference in a homotopy slot, found {other:?}"),
        })
        .collect()
}

#[test]
fn named_homotopy_matches_positional_lowering() {
    let named = flatten_model("homotopy(actual = a, simplified = b)")
        .expect("named homotopy actuals flatten cleanly");
    let positional =
        flatten_model("homotopy(a, b)").expect("positional homotopy actuals flatten cleanly");

    let named_args = homotopy_args(&named);
    let positional_args = homotopy_args(&positional);

    assert_eq!(named_args.len(), 2);
    assert_eq!(
        slot_identities(&named_args),
        slot_identities(&positional_args)
    );
    assert_eq!(
        slot_identities(&named_args),
        vec!["a".to_string(), "b".to_string()]
    );
    for argument in &named_args {
        assert!(
            !matches!(argument, Expression::FunctionCall { name, .. }
                if name.as_str().starts_with(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)),
            "no named-argument marker survives builtin slot resolution"
        );
    }
}

#[test]
fn reordered_named_homotopy_binds_by_name() {
    // The named spelling reverses source order; binding by name must still place
    // `actual` first and `simplified` second, matching `homotopy(a, b)`.
    let reordered = flatten_model("homotopy(simplified = b, actual = a)")
        .expect("reordered named homotopy actuals flatten cleanly");
    let positional =
        flatten_model("homotopy(a, b)").expect("positional homotopy actuals flatten cleanly");

    assert_eq!(
        slot_identities(&homotopy_args(&reordered)),
        slot_identities(&homotopy_args(&positional)),
    );
    assert_eq!(
        slot_identities(&homotopy_args(&reordered)),
        vec!["a".to_string(), "b".to_string()],
    );
}

#[test]
fn unknown_named_formal_is_refused() {
    let error = flatten_model("homotopy(bogus = a, simplified = b)")
        .expect_err("an unknown formal name has no slot to bind");
    assert!(matches!(
        *error,
        FlattenError::InvalidFunctionCallArgs { ref function, .. } if function == "homotopy"
    ));
}

#[test]
fn duplicate_named_formal_is_refused() {
    let error = flatten_model("homotopy(actual = a, actual = b)")
        .expect_err("a formal filled twice is ambiguous");
    assert!(matches!(
        *error,
        FlattenError::InvalidFunctionCallArgs { ref function, .. } if function == "homotopy"
    ));
}
