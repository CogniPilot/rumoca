//! Aggregate record function signatures, nominal identity, and call ownership.

use rumoca::{Compiler, CompilerError};
use rumoca_compile::compile::FailedPhase;
use rumoca_core::ExpressionVisitor;
use rumoca_ir_dae as dae;

const NESTED_RECORD_ARGUMENT: &str = r#"
package RecordCallOwner
  record Pair
    Real left;
    Real right;
  end Pair;

  function makePair
    input Real value;
    output Pair pair;
  algorithm
    pair.left := value;
    pair.right := value + 1.0;
  end makePair;

  function consume
    input Pair pair;
    output Real total;
  algorithm
    total := pair.left + pair.right;
  end consume;

  model Model
    Real total;
  equation
    total = consume(makePair(time));
  end Model;
end RecordCallOwner;
"#;

const RECORD_OUTPUT_FIELD_REUSE: &str = r#"
package RecordOutputReuse
  record Pair
    Real left;
    Real right;
  end Pair;

  function expensive
    input Real value;
    output Real result;
  algorithm
    result := value + 1.0;
  end expensive;

  function makePair
    input Real value;
    output Pair pair;
  algorithm
    pair.left := expensive(value);
    pair.right := pair.left + pair.left;
  end makePair;

  model Model
    Pair pair;
  equation
    pair = makePair(time);
  end Model;
end RecordOutputReuse;
"#;

const SAME_SHAPE_DIFFERENT_RECORD_ARGUMENT: &str = r#"
package NominalRecordCall
  record Expected
    Real value;
  end Expected;

  record Foreign
    Real value;
  end Foreign;

  function consume
    input Expected item;
    output Real result;
  algorithm
    result := item.value;
  end consume;

  model Model
    Real result;
  equation
    result = consume(Foreign(time));
  end Model;
end NominalRecordCall;
"#;

/// `makePair(time)` is one source evaluation and `consume` has one aggregate
/// formal/actual slot. No phase may create one call projection per record leaf.
#[test]
fn aggregate_record_argument_retains_one_formal_actual_and_call_owner() {
    let compiled = Compiler::new()
        .model("RecordCallOwner.Model")
        .compile_str(NESTED_RECORD_ARGUMENT, "record_call_owner.mo")
        .expect("a nested record-valued function argument should compile");

    let consume = compiled
        .flat()
        .functions
        .values()
        .find(|function| function.name.as_str().ends_with("consume"))
        .expect("consume remains in Flat");
    assert_eq!(consume.inputs.len(), 1, "the record formal stays aggregate");
    assert_eq!(
        consume.inputs[0].type_class,
        Some(rumoca_core::ClassType::Record)
    );
    let mut calls = FlatCallCollector::default();
    for equation in &compiled.flat().equations {
        calls.visit_expression(&equation.residual);
    }
    assert_eq!(
        calls.consume_argument_counts,
        [1],
        "the source call retains one aggregate actual"
    );

    compiled.dae().inspect(|view| {
        let make_pair = (0..view.function_count())
            .filter_map(|index| view.function_id(index))
            .filter_map(|id| view.function(id))
            .find(|function| function.name().as_str().ends_with("makePair"))
            .expect("the record-valued callee remains in checked DAE")
            .id();
        let calls = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter_map(|expression| match expression.operation() {
                dae::ExpressionOperation::Call {
                    owner,
                    function,
                    output,
                    ..
                } if function == make_pair => Some((owner, output)),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert_eq!(
            calls.len(),
            1,
            "one aggregate result must not become cloned leaf calls"
        );
        assert_eq!(calls[0].1, 0, "the call projects its sole aggregate result");
    });
}

#[derive(Default)]
struct FlatCallCollector {
    consume_argument_counts: Vec<usize>,
}

impl ExpressionVisitor for FlatCallCollector {
    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
    ) {
        if name.as_str().ends_with("consume") {
            self.consume_argument_counts.push(args.len());
        }
        self.walk_function_call(name, args, is_constructor);
    }
}

#[test]
fn same_shape_different_nominal_record_actual_is_rejected() {
    let error = Compiler::new()
        .model("NominalRecordCall.Model")
        .compile_str(
            SAME_SHAPE_DIFFERENT_RECORD_ARGUMENT,
            "nominal_record_call.mo",
        )
        .expect_err("equal leaf shape cannot authorize a different nominal record actual");
    match error {
        CompilerError::CompileDiagnosticsError { failures, .. } => assert!(
            failures.iter().any(|failure| {
                failure.phase == Some(FailedPhase::Typecheck)
                    && failure.error_code.as_deref() == Some("ET002")
                    && failure.error.contains("Expected")
                    && failure.error.contains("Foreign")
                    && failure
                        .primary_label
                        .as_ref()
                        .and_then(|label| label.message.as_deref())
                        == Some("incompatible function argument")
            }),
            "nominal mismatch requires its exact Typecheck/ET002 owner: {failures:?}",
        ),
        other => panic!("expected structured nominal type refusal, got {other:?}"),
    }
}

/// A record field is a reaching definition, not a textual macro. Reusing the
/// field twice must read the one value established by `expensive(value)`;
/// Flat output coalescing must not clone that call into both reads.
#[test]
fn record_output_field_reuse_does_not_clone_its_defining_call() {
    let compiled = Compiler::new()
        .model("RecordOutputReuse.Model")
        .compile_str(RECORD_OUTPUT_FIELD_REUSE, "record_output_reuse.mo")
        .expect("a record output may reuse an earlier field definition");

    compiled.dae().inspect(|view| {
        let expensive = (0..view.function_count())
            .filter_map(|index| view.function_id(index))
            .filter_map(|id| view.function(id))
            .find(|function| function.name().as_str().ends_with("expensive"))
            .expect("the called function remains in checked DAE")
            .id();
        let calls = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter(|expression| {
                matches!(
                    expression.operation(),
                    dae::ExpressionOperation::Call { function, .. }
                        if function == expensive
                )
            })
            .count();

        assert_eq!(
            calls, 1,
            "one source call defines pair.left; later reads must not clone its work"
        );
    });
}
