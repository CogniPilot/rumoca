//! MLS 10.1: `size(a, i)` reads only the declared dimensions of
//! `a`, so a function local that is being assigned may name itself in them.

use rumoca::Compiler;

fn compile(body: &str) -> Result<(), String> {
    let source = format!(
        r#"
model T
  function g
    input Integer n;
    output Integer y[n];
  algorithm
    for i in 1:n loop y[i] := i; end for;
  end g;
  function f
    input Integer s;
    output Integer id;
  protected
    Integer st[3];
  algorithm
{body}
    id := st[1] + s;
  end f;
  Integer k = f(1);
end T;
"#
    );
    Compiler::new()
        .model("T")
        .compile_str(&source, "T.mo")
        .map(|_| ())
        .map_err(|error| error.to_string())
}

#[test]
fn size_of_an_unassigned_local_is_not_an_element_read() {
    compile("    st := g(size(st, 1));").expect("size reads the declared dimension only");
}

#[test]
fn an_element_read_of_an_unassigned_local_is_still_refused() {
    let error = compile("    id := st[1];\n    st := g(3);").expect_err("the element is undefined");
    assert!(error.contains("do not all have a definition"), "{error}");
}
