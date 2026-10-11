use crate::{Session, SessionConfig};

#[test]
fn component_instances_have_distinct_when_call_owners() {
    let source = r#"
function sumAndProduct
  input Real u;
  input Real v;
  output Real s;
  output Real p;
algorithm
  s := u + v;
  p := u * v;
end sumAndProduct;
model Cell
  parameter Real offset = 1;
  discrete Real s(start = 0, fixed = true);
  discrete Real p(start = 0, fixed = true);
equation
  when sample(0, 0.1) then
    (s, p) = sumAndProduct(offset, time);
  end when;
end Cell;
model SharedSourceCalls
  Cell first(offset = 1);
  Cell second(offset = 2);
end SharedSourceCalls;
"#;
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("SharedSourceCalls.mo", source)
        .expect("the model parses");
    session
        .compile_model("SharedSourceCalls")
        .expect("instances of one declaration retain distinct semantic call owners");
}
