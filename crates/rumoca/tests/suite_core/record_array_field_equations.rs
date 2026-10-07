//! A whole-record equation whose record has an array-of-records field
//! (MLS 3.7 §8.3.1, §10.6.1): Flat holds the array of records as one column
//! per leaf field, and each column equals the matching field projection of
//! the record value.

use rumoca::Compiler;

const RECORD_ARRAY_FIELD_EQUATION: &str = r#"
within;
package P
  record Edge
    Boolean enabled;
    Integer id;
    Real r[2];
  end Edge;
  record Graph
    Integer generation;
    Edge edges[3];
  end Graph;
  function Seed
    input Integer g;
    output Graph result;
  algorithm
    result.generation := g;
    for slot in 1:3 loop
      result.edges[slot] := Edge(slot > 1, slot, {slot, 2 * slot});
    end for;
  end Seed;
  model M
    parameter Integer g = 2;
    Graph next = Seed(g);
  end M;
end P;
"#;

#[test]
fn record_equation_defines_every_record_array_column() {
    Compiler::new()
        .model("P.M")
        .compile_str(RECORD_ARRAY_FIELD_EQUATION, "RecordArrayFieldEquation.mo")
        .expect("a record equation reaches the columns of an array-of-records field");
}
