//! Source tuple occurrences remain shared across primitive and record receivers.

use super::Compiler;
use rumoca_ir_dae::{DaeView, ExprId, ExpressionOperation};
use rumoca_ir_solve::LinearOp;

fn call_owner<'dae>(view: DaeView<'dae>, mut expression: ExprId<'dae>) -> u32 {
    loop {
        match view.expression(expression).unwrap().operation() {
            ExpressionOperation::Field { base, .. } | ExpressionOperation::Index { base, .. } => {
                expression = base
            }
            ExpressionOperation::Call { owner, .. } => return owner.index(),
            _ => panic!("a tuple receiver retains its source call"),
        }
    }
}

#[test]
fn record_and_discrete_tuple_receivers_retain_one_source_occurrence() {
    let source = r"
record Payload
  Boolean flags[4];
  Integer count;
end Payload;
record Envelope
  Payload body;
end Envelope;
function split
  input Boolean on;
  output Envelope payload;
  output Boolean tail;
algorithm
  payload.body.flags := fill(on,4);
  payload.body.count := if on then 3 else 7;
  tail := not on;
end split;
model TupleCallIdentity
  input Boolean on;
  Envelope first;
  Boolean firstTail;
  Envelope second;
  Boolean secondTail;
equation
  (first, firstTail) = split(on);
  (second, secondTail) = split(on);
end TupleCallIdentity;
";
    let result = Compiler::new()
        .model("TupleCallIdentity")
        .compile_str(source, "TupleCallIdentity.mo")
        .unwrap();
    let owners = result.dae.inspect(|view| {
        assert_eq!(view.discrete_value_owner_count(), 2);
        (0..2)
            .map(|index| {
                let owner = view
                    .discrete_value_owner(view.discrete_value_owner_id(index).unwrap())
                    .unwrap();
                assert_eq!(owner.targets().len(), 3);
                let calls = owner
                    .branches()
                    .get(0)
                    .unwrap()
                    .values()
                    .iter()
                    .map(|(value, _)| call_owner(view, value))
                    .collect::<Vec<_>>();
                assert!(calls.iter().all(|call| *call == calls[0]));
                calls[0]
            })
            .collect::<Vec<_>>()
    });
    assert_ne!(
        owners[0], owners[1],
        "equal inputs do not merge occurrences"
    );
    let package = rumoca_phase_solve::lower_solve_package(&result.dae).unwrap();
    assert_eq!(package.problem.discrete.rhs.programs().len(), 2);
    assert_eq!(
        package
            .problem
            .discrete
            .rhs
            .programs()
            .iter()
            .flatten()
            .filter(|operation| matches!(operation, LinearOp::PureCall { .. }))
            .count(),
        2,
        "each complete six-scalar tuple evaluates its source call once"
    );
}
