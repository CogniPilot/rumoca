//! Record values a function writes whole, updates field by field inside
//! branches, and reads whole (MLS §12.4.4, §12.6): the record is split into
//! field locals, a whole write is projected (or held once when its value is a
//! call), and a whole read is reassembled by the record constructor from the
//! field locals current at the read.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const WHOLE_VALUES: &str = r#"
within;
package W
  record Frame
    Integer generation;
    Real x[2];
    Real histogram[2];
  end Frame;
  function Empty
    output Frame f;
  algorithm
    f.generation := 0;
    f.x := zeros(2);
    f.histogram := {7, 8};
  end Empty;
  function Build
    input Boolean valid;
    input Integer g;
    input Real v;
    output Frame frame;
  protected
    Frame candidate;
  algorithm
    frame := Empty();
    if valid then
      candidate := Empty();
      candidate.x[1] := v;
      if v > 0 then
        candidate.generation := g;
        for k in 1:2 loop
          candidate.x[2] := candidate.x[2] + k * v;
        end for;
        frame := candidate;
      end if;
    end if;
  end Build;
  function Total
    input Frame f;
    output Real y;
  algorithm
    y := f.generation + 10 * f.x[1] + 100 * f.x[2] + 1000 * f.histogram[1];
  end Total;
  function Carry
    input Frame previous;
    input Real v;
    output Real y;
  protected
    Frame next;
  algorithm
    next := previous;
    if v > 0 then
      next.generation := next.generation + 1;
      next.x[2] := v;
    end if;
    y := Total(next);
  end Carry;
end W;

model ObserveWholeValues
  Real accepted = W.Total(W.Build(true, 3, 2));
  Real rejected = W.Total(W.Build(true, 3, -1));
  Real idle = W.Total(W.Build(false, 3, 2));
  Real carried = W.Carry(W.Frame(4, {1, 2}, {5, 6}), 9);
  Real kept = W.Carry(W.Frame(4, {1, 2}, {5, 6}), -9);
end ObserveWholeValues;
"#;

fn value(report: &rumoca_sim::EvalAtReport, name: &str) -> f64 {
    report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == name)
        .unwrap_or_else(|| panic!("missing solver value {name}"))
        .value
}

#[test]
fn a_record_written_whole_updated_in_branches_and_read_whole_is_split() {
    let compiled = Compiler::new()
        .model("ObserveWholeValues")
        .compile_str(WHOLE_VALUES, "ObserveWholeValues.mo")
        .expect("whole writes, branch updates and whole reads of a record compile");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the split record DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    // `Total` weighs generation, x[1], x[2] and histogram[1] by 1, 10, 100 and
    // 1000; the histogram keeps the value `Empty()` held.
    assert_eq!(
        value(&probe.report, "accepted"),
        3.0 + 20.0 + 600.0 + 7000.0
    );
    assert_eq!(value(&probe.report, "rejected"), 7000.0);
    assert_eq!(value(&probe.report, "idle"), 7000.0);
    assert_eq!(value(&probe.report, "carried"), 5.0 + 10.0 + 900.0 + 5000.0);
    assert_eq!(value(&probe.report, "kept"), 4.0 + 10.0 + 200.0 + 5000.0);
}

const ARRAY_FIELDS: &str = r#"
within;
package A
  record Edge
    Integer id;
    Real r;
  end Edge;
  record State
    Edge edges[2];
  end State;
  function Count
    input Edge edges[:];
    output Real y;
  algorithm
    y := sum(edges.r);
  end Count;
  function CountState
    input State s;
    output Real y;
  algorithm
    y := Count(s.edges);
  end CountState;
  function Edges
    input Real u;
    output Real y;
  protected
    State s;
  algorithm
    s.edges[1].id := 1;
    s.edges[1].r := 1;
    s.edges[2].id := 2;
    s.edges[2].r := 2;
    if u > 0 then
      s.edges[2].r := u;
    end if;
    y := Count(s.edges);
  end Edges;
  function Whole
    input Real u;
    output Real y;
  protected
    State s;
  algorithm
    s.edges[1].id := 1;
    s.edges[1].r := 1;
    s.edges[2].id := 2;
    s.edges[2].r := 2;
    if u > 0 then
      s.edges[2].r := u;
    end if;
    y := CountState(s);
  end Whole;
end A;
model ObserveEdges
  Real y = A.Edges(3);
end ObserveEdges;
model ObserveWhole
  Real y = A.Whole(3);
end ObserveWhole;
"#;

/// An array-of-records field read whole is not split, so it keeps one
/// record-array local whose element fields the branch writes.
#[test]
fn an_array_of_records_read_whole_keeps_its_local() {
    let compiled = Compiler::new()
        .model("ObserveEdges")
        .compile_str(ARRAY_FIELDS, "ObserveEdges.mo")
        .expect("an array of records read whole compiles from its record-array local");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the record-array DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    assert_eq!(value(&probe.report, "y"), 4.0);
}

/// A record read whole whose array-of-records field is written below keeps
/// its source form (no constructor call reassembles struct-of-arrays
/// columns), and the existing record-array element owners evaluate it.
#[test]
fn a_record_with_a_written_record_array_field_keeps_its_source_form() {
    let compiled = Compiler::new()
        .model("ObserveWhole")
        .compile_str(ARRAY_FIELDS, "ObserveWhole.mo")
        .expect("a record read whole over a written record-array field compiles");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the record DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    assert_eq!(value(&probe.report, "y"), 4.0);
}

/// MLS 3.7 §12.4.4: a split record whose only whole write is conditional
/// leaves its other fields unassigned on the untaken path, so reading it
/// whole is refused rather than reassembled from undefined field locals.
#[test]
fn a_conditionally_defined_split_record_read_whole_is_refused() {
    let source = r#"
within;
package U
  record Frame
    Integer generation;
    Real x[2];
  end Frame;
  function Empty
    output Frame f;
  algorithm
    f.generation := 0;
    f.x := zeros(2);
  end Empty;
  function Build
    input Real v;
    output Frame frame;
  protected
    Frame candidate;
  algorithm
    if v > 0 then
      candidate := Empty();
    end if;
    candidate.x[1] := v;
    if v > 1 then
      candidate.generation := 1;
    end if;
    frame := candidate;
  end Build;
end U;
model ObserveUndefined
  U.Frame f = U.Build(time + 2);
end ObserveUndefined;
"#;
    let error = Compiler::new()
        .model("ObserveUndefined")
        .compile_str(source, "ObserveUndefined.mo")
        .expect_err("a field unassigned on one path is not read");
    let message = error.to_string();
    assert!(
        message.contains("do not all have a definition"),
        "unexpected diagnostic: {message}"
    );
}

/// MLS 3.7 §11.2.1: a whole assignment evaluates its value before it writes
/// the record, so a value that reads the record being written sees its old
/// fields, not the fields an earlier part of the same write just stored.
#[test]
fn a_whole_write_reading_its_own_record_sees_the_old_fields() {
    let source = r#"
within;
package S
  record R
    Real x;
    Real y;
  end R;
  function Swap
    input Real v;
    output Real y;
  protected
    R r;
  algorithm
    r := R(v, 2 * v);
    if v > 0 then
      r.x := r.x + 1;
    end if;
    r := R(r.y, r.x);
    y := 100 * r.x + r.y;
  end Swap;
  function Named
    input Real v;
    output Real y;
  protected
    R r;
  algorithm
    r := R(v, v);
    if v > 0 then
      r.x := r.x + 1;
    end if;
    r := R(x = 1, y = r.x + 10);
    y := 100 * r.x + r.y;
  end Named;
end S;
model ObserveSelfReads
  Real swapped = S.Swap(3);
  Real named = S.Named(3);
end ObserveSelfReads;
"#;
    let compiled = Compiler::new()
        .model("ObserveSelfReads")
        .compile_str(source, "ObserveSelfReads.mo")
        .expect("a whole write reading its own record compiles");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the self-reading record DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    // r = (4, 6) before the swap, so the swap gives (6, 4).
    assert_eq!(value(&probe.report, "swapped"), 604.0);
    // r.x = 4 before the write, so y = 4 + 10.
    assert_eq!(value(&probe.report, "named"), 114.0);
}

/// MLS 3.7 section 12.4.4: a record copied whole from an input, one of its
/// record fields replaced whole, other fields updated in a branch and the
/// record read whole is split into field locals, also when a nested field is
/// an array of records that a whole read cannot reassemble from columns.
const NESTED_WHOLE_WRITE: &str = r#"
package D
  record Cat
    Real pos[2];
    Integer n;
  end Cat;
  record Edge
    Boolean enabled;
    Real w;
  end Edge;
  record Graph
    Edge edges[2];
    Integer rev;
  end Graph;
  record Loc
    Cat catalog;
    Graph graph;
    Real g;
  end Loc;
  record Poses
    Real p[2]; Real pos2[3,2];
    Integer rev;
  end Poses;
  record Est
    Loc localization;
    Poses poses;
  end Est;
  record S
    Est estimator;
    Real w;
  end S;
  record Res
    Loc next;
    Boolean accepted;
  end Res;
  function MkRes
    input Loc l;
    output Res r;
  algorithm
    r.next := l;
    r.accepted := true;
  end MkRes;
  function ValidView
    input Cat c;
    input Poses p;
    output Boolean ok;
  algorithm
    ok := c.n > 0 and p.rev > 0;
  end ValidView;
  function ValidS
    input S s;
    output Boolean ok;
  algorithm
    ok := s.w > 0;
  end ValidS;
  function Pub
    input S previous;
    input Loc proposed;
    input Boolean captured;
    output S next;
  protected
    S candidate;
    Res published;
    Boolean valid; Integer slot;
  algorithm
    next := previous;
    published := MkRes(proposed);
    if published.accepted then
      candidate := previous;
      candidate.estimator.localization := published.next;
      valid := true; slot := published.next.catalog.n;
      if captured then
        candidate.estimator.poses.rev := previous.estimator.poses.rev + 1;
        candidate.estimator.poses.p[1] := published.next.catalog.pos[1]; candidate.estimator.poses.pos2[slot,:] := published.next.catalog.pos;
      end if;
      if valid then
        valid := ValidView(candidate.estimator.localization.catalog, candidate.estimator.poses);
      end if;
      if valid then
        valid := ValidS(candidate);
        if valid then next := candidate; end if;
      end if;
    end if;
  end Pub;
end D;
model Accepted
  parameter Real u = 1;
  D.S r = D.Pub(D.S(D.Est(D.Loc(D.Cat({1, 2}, 1), D.Graph({D.Edge(true, 1), D.Edge(false, 2)}, 5), 3), D.Poses({4, 5}, zeros(3,2), 6)), 7), D.Loc(D.Cat({u, 2}, 2), D.Graph({D.Edge(true, 3), D.Edge(true, 4)}, 6), 8), true);
  Real y = r.estimator.poses.p[1];
  Real z = r.estimator.localization.g;
  Real w = r.estimator.localization.graph.edges[2].w;
end Accepted;
model Rejected
  parameter Real u = 1;
  D.S r = D.Pub(D.S(D.Est(D.Loc(D.Cat({1, 2}, 1), D.Graph({D.Edge(true, 1), D.Edge(false, 2)}, 5), 3), D.Poses({4, 5}, zeros(3,2), 0)), 7), D.Loc(D.Cat({u, 2}, 2), D.Graph({D.Edge(true, 3), D.Edge(true, 4)}, 6), 8), false);
  Real y = r.estimator.poses.p[1];
  Real z = r.estimator.localization.g;
  Real w = r.estimator.localization.graph.edges[2].w;
end Rejected;
"#;

fn nested_whole_write_values(model: &str) -> [f64; 3] {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(NESTED_WHOLE_WRITE, "NestedWholeWrite.mo")
        .expect("a nested whole write beside an array of records compiles");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the nested record DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    ["y", "z", "w"].map(|name| value(&probe.report, name))
}

#[test]
fn a_nested_record_replaced_whole_beside_an_array_of_records_is_split() {
    assert_eq!(nested_whole_write_values("Accepted"), [1.0, 8.0, 4.0]);
}

#[test]
fn a_rejected_nested_update_keeps_the_previous_record() {
    // The previous pose revision is 0, so the view check fails and the
    // result stays the previous record.
    assert_eq!(nested_whole_write_values("Rejected"), [4.0, 3.0, 2.0]);
}

/// MLS 3.7 section 12.4.4: a record local whose array-of-records field is
/// written element by element, replaced whole in the branches of a
/// conditional, passed to and returned from calls, and updated through a
/// nested record keeps one value per field.
const BRANCH_RECORD_ARRAY: &str = r#"
package G
  constant Integer cap = 3;
  record Edge
    Boolean enabled;
    Integer id;
    Real w;
  end Edge;
  record State
    Integer generation;
    Integer revision;
    Edge edges[cap];
  end State;
  record Insertion
    State state;
    Boolean accepted;
  end Insertion;
  function EmptyEdge
    output Edge result;
  algorithm
    result.enabled := false; result.id := 0; result.w := 0;
  end EmptyEdge;
  function Empty
    input Integer generation = 1;
    output State result;
  algorithm
    result.generation := generation; result.revision := 0;
    for i in 1:cap loop
      result.edges[i] := EmptyEdge();
    end for;
  end Empty;
  function Insert
    input State state;
    input Real w;
    output Insertion result;
  algorithm
    result.state := state;
    result.accepted := false;
    for i in 1:cap loop
      if not result.state.edges[i].enabled and not result.accepted then
        result.state.edges[i].enabled := true;
        result.state.edges[i].id := i;
        result.state.edges[i].w := w;
        result.accepted := true;
      end if;
    end for;
  end Insert;
  function Total
    input State s;
    output Real y;
  algorithm
    y := s.revision * 100 + s.generation * 10;
    for i in 1:cap loop
      if s.edges[i].enabled then y := y + s.edges[i].w; end if;
    end for;
  end Total;
  function Capture
    input State previous;
    input Boolean reset;
    input Real w;
    output Real y;
  protected
    State working; Insertion insertion; Boolean valid;
  algorithm
    y := -1;
    if reset then
      working := Empty(5);
    else
      working := previous;
    end if;
    valid := true;
    for slot in 1:cap loop
      if working.edges[slot].enabled and working.edges[slot].w > 100 then
        working.edges[slot] := EmptyEdge();
      end if;
    end for;
    insertion := Insert(working, w);
    working := insertion.state;
    valid := insertion.accepted;
    if valid then
      working.revision := if reset then 1 else previous.revision + 1;
      y := Total(working);
    end if;
  end Capture;
end G;
model M
  parameter Real w = 7;
  Real a = G.Capture(G.State(2, 3, {G.Edge(true, 1, 1.5), G.Edge(false, 0, 0), G.Edge(false, 0, 0)}), false, w);
  Real b = G.Capture(G.State(2, 3, {G.Edge(true, 1, 1.5), G.Edge(false, 0, 0), G.Edge(false, 0, 0)}), true, w);
end M;
"#;

#[test]
fn a_record_with_an_array_of_records_is_replaced_whole_in_branches_and_updated() {
    let compiled = Compiler::new()
        .model("M")
        .compile_str(BRANCH_RECORD_ARRAY, "BranchRecordArray.mo")
        .expect("a record local with an array-of-records field compiles");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the record-array DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    // Kept previous record: revision 3 + 1, generation 2, and the inserted
    // edge of weight 7 beside the retained 1.5.
    assert_eq!(value(&probe.report, "a"), 400.0 + 20.0 + 1.5 + 7.0);
    // Reset record: revision 1, generation 5, one inserted edge.
    assert_eq!(value(&probe.report, "b"), 100.0 + 50.0 + 7.0);
}

/// MLS 3.7 section 11.2.1: statements run in order and the last write wins, so
/// a record copied whole and then updated field by field in straight-line
/// code holds the updated field where it is read whole afterwards.
const STRAIGHT_LINE_OVERWRITE: &str = r#"
package O
  record Catalog
    Real positions[2];
    Real rotations[2];
    Integer count;
  end Catalog;
  function Score
    input Catalog catalog;
    output Real y;
  algorithm
    y := sum(catalog.positions) + 10 * sum(catalog.rotations) + 100 * catalog.count;
  end Score;
  function Policy
    input Catalog catalog;
    input Real nodePosition[2];
    input Real nodeRotation[2];
    output Real y;
  protected
    Catalog policy;
  algorithm
    policy := catalog;
    policy.positions := nodePosition;
    policy.rotations := nodeRotation;
    y := Score(policy);
  end Policy;
end O;
model Overwrite
  parameter Real u = 1;
  Real y = O.Policy(O.Catalog({100, 200}, {300, 400}, 5), {u, 2}, {3, 4});
end Overwrite;
"#;

#[test]
fn a_record_copied_whole_then_updated_in_straight_line_code_keeps_the_update() {
    let compiled = Compiler::new()
        .model("Overwrite")
        .compile_str(STRAIGHT_LINE_OVERWRITE, "Overwrite.mo")
        .expect("a straight-line overwrite of a copied record compiles");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the overwritten record DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    // The updated positions and rotations, and the copied count.
    assert_eq!(value(&probe.report, "y"), 3.0 + 70.0 + 500.0);
}

/// MLS 3.7 section 12.4.4: an output record whose array-of-records field is
/// filled element by element from calls and read through the fields of its
/// elements holds one column per element field.
const ELEMENT_CALLS: &str = r#"
package E
  record Proposal
    Real value;
    Boolean verified;
  end Proposal;
  record Batch
    Proposal proposals[3];
    Real nextSeeds[3];
    Real total;
  end Batch;
  function Verify
    input Real seed;
    output Proposal proposal;
  algorithm
    proposal.value := 2 * seed;
    proposal.verified := seed > 1.5;
  end Verify;
  function Run
    input Real base;
    output Batch result;
  algorithm
    result.total := 0;
    for rank in 1:3 loop
      result.proposals[rank] := Verify(base + rank);
      result.nextSeeds[rank] := result.proposals[rank].value + 1;
      result.total := result.total + (if result.proposals[rank].verified then 1 else 0);
    end for;
  end Run;
end E;
model Elements
  parameter Real base = 0.5;
  E.Batch b = E.Run(base);
  Real second = b.nextSeeds[2];
  Real total = b.total;
end Elements;
"#;

#[test]
fn an_array_of_records_filled_from_calls_and_read_by_fields_is_split() {
    let compiled = Compiler::new()
        .model("Elements")
        .compile_str(ELEMENT_CALLS, "Elements.mo")
        .expect("element-wise record calls with field reads compile");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the element-call DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    // Seeds 1.5, 2.5, 3.5: the second proposal is 5, so its next seed is 6;
    // seed 1.5 is not above 1.5, so two of three proposals verify.
    assert_eq!(value(&probe.report, "second"), 6.0);
    assert_eq!(value(&probe.report, "total"), 2.0);
}

/// MLS 3.7 section 12.4.4: a record local with an array-of-records field,
/// copied whole into a field of the output record, is split into columns and
/// the copy reads each column, since a constructor cannot reassemble an array
/// of records from them.
const RECORD_ARRAY_COPY: &str = r#"
package G
  constant Integer cap = 3;
  record Edge
    Boolean enabled;
    Integer id;
    Real w;
  end Edge;
  record State
    Integer generation;
    Integer revision;
    Edge edges[cap];
  end State;
  record Insertion
    State state;
    Boolean accepted;
  end Insertion;
  function EmptyEdge
    output Edge result;
  algorithm
    result.enabled := false; result.id := 0; result.w := 0;
  end EmptyEdge;
  function Empty
    input Integer generation = 1;
    output State result;
  algorithm
    result.generation := generation; result.revision := 0;
    for i in 1:cap loop
      result.edges[i] := EmptyEdge();
    end for;
  end Empty;
  function Insert
    input State state;
    input Real w;
    output Insertion result;
  algorithm
    result.state := state;
    result.accepted := false;
    for i in 1:cap loop
      if not result.state.edges[i].enabled and not result.accepted then
        result.state.edges[i].enabled := true;
        result.state.edges[i].id := i;
        result.state.edges[i].w := w;
        result.accepted := true;
      end if;
    end for;
  end Insert;
  function Total
    input State s;
    output Real y;
  algorithm
    y := s.revision * 100 + s.generation * 10;
    for i in 1:cap loop
      if s.edges[i].enabled then y := y + s.edges[i].w; end if;
    end for;
  end Total;
  record Update
    State state;
    Boolean accepted;
  end Update;
  function Capture
    input State previous;
    input Boolean reset;
    input Real w;
    output Update result;
  protected
    State working; Insertion insertion; Boolean valid; Real y;
  algorithm
    result.state := previous; result.accepted := false; y := -1;
    if reset then
      working := Empty(5);
    else
      working := previous;
    end if;
    valid := true;
    for slot in 1:cap loop
      if working.edges[slot].enabled and working.edges[slot].w > 100 then
        working.edges[slot] := EmptyEdge();
      end if;
    end for;
    insertion := Insert(working, w);
    working := insertion.state;
    valid := insertion.accepted;
    if valid then
      working.revision := if reset then 1 else previous.revision + 1;
      y := Total(working);
      result.state := working; result.accepted := true;
    end if;
  end Capture;
  function Probe
    input State previous;
    input Boolean reset;
    input Real w;
    output Real y;
  protected
    Update u;
  algorithm
    u := Capture(previous, reset, w);
    y := u.state.revision * 1000 + u.state.edges[2].w + (if u.accepted then 0.5 else 0);
  end Probe;
end G;
model M
  parameter Real w = 7;
  Real a = G.Probe(G.State(2, 3, {G.Edge(true, 1, 1.5), G.Edge(false, 0, 0), G.Edge(false, 0, 0)}), false, w);
  Real b = G.Probe(G.State(2, 3, {G.Edge(true, 1, 1.5), G.Edge(false, 0, 0), G.Edge(false, 0, 0)}), true, w);
end M;
"#;

#[test]
fn a_record_array_field_copied_whole_into_an_output_record_is_copied_by_column() {
    let compiled = Compiler::new()
        .model("M")
        .compile_str(RECORD_ARRAY_COPY, "RecordArrayCopy.mo")
        .expect("a whole copy of a split record with a record-array field compiles");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the copied record DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    // Revision 4 with the new edge of weight 7 beside the kept 1.5, accepted.
    assert_eq!(value(&probe.report, "a"), 4000.0 + 7.0 + 0.5);
    // Reset to revision 1, whose second edge is the empty one, accepted.
    assert_eq!(value(&probe.report, "b"), 1000.0 + 0.5);
}

/// MLS 3.7 section 8.3.1: a whole-record equation whose value is a record
/// constructor nested in another equates every leaf field with the matching
/// field of the nested constructor, through the scalar record-lane path.
const NESTED_RECORD_EQUATION: &str = r#"
record Part
  Real a;
  Real b[2];
end Part;
record Wrap
  Part part;
  Real c;
end Wrap;
model M
  parameter Real u = 2;
  Wrap o;
  Real y = o.part.b[1] + o.c + o.part.a;
  Real z(start = 0);
equation
  o = Wrap(Part(u, {u + 1, 5}), 7);
  der(z) = -z + o.part.b[2];
end M;
"#;

#[test]
fn a_record_equation_over_a_nested_constructor_projects_each_lane() {
    let compiled = Compiler::new()
        .model("M")
        .compile_str(NESTED_RECORD_EQUATION, "NestedRecordEquation.mo")
        .expect("a nested record constructor equation compiles");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the nested record equation DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    assert_eq!(value(&probe.report, "o.part.a"), 2.0);
    assert_eq!(value(&probe.report, "o.part.b[1]"), 3.0);
    assert_eq!(value(&probe.report, "o.part.b[2]"), 5.0);
    assert_eq!(value(&probe.report, "o.c"), 7.0);
    assert_eq!(value(&probe.report, "y"), 12.0);
}
