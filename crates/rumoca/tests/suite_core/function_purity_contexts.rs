//! MLS 3.7 §12.3 call restrictions on impure functions inside function bodies.
//!
//! The restriction is stated of the written prefix: an impure function may be
//! called from "another function marked with the prefix impure", and a body
//! declared `pure` may not reach one. A function that writes no prefix is a
//! different case: "For a function without explicit purity, it is deprecated
//! to call any function declared impure", which is a deprecation, not an
//! error, and such a function is then treated as impure for transformations.

use rumoca::Compiler;

fn source(prefix: &str) -> String {
    format!(
        r#"
model PurityContext
  parameter Integer id = initialize(3);
  Real x(start = 0, fixed = true);
equation
  der(x) = id;
protected
  impure function store
    input Integer seed;
    output Integer y;
    external "C" y = rumoca_test_store(seed) annotation(Library = "rumoca_test");
  end store;
  {prefix}function initialize
    input Integer seed;
    output Integer id;
  algorithm
    id := store(seed);
  end initialize;
end PurityContext;
"#
    )
}

#[test]
fn a_function_without_explicit_purity_may_call_an_impure_function() {
    Compiler::new()
        .model("PurityContext")
        .compile_str(&source(""), "purity_context.mo")
        .expect("calling an impure function from an undeclared-purity body is only deprecated");
}

#[test]
fn a_function_declared_pure_may_not_call_an_impure_function() {
    let Err(error) = Compiler::new()
        .model("PurityContext")
        .compile_str(&source("pure "), "purity_context.mo")
    else {
        panic!("a body declared pure is not a context that admits an impure call");
    };
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("impure function `PurityContext.store`")
            && rendered.contains("declared `pure`"),
        "the rejection names the impure callee and the declared-pure context, got: {rendered}"
    );
}
