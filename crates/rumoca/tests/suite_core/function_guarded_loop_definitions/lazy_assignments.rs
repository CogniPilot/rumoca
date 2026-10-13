//! Selected source arms retain their own reads and target update order.
use rumoca::Compiler;
const SOURCE: &str = include_str!("../../fixtures/function_guarded_loops/LazyAssignment.mo");

#[test]
fn selected_body_owner_is_admitted_independently() {
    compile(SOURCE, "LazyBodyOnly");
}
#[test]
fn selected_ordered_owner_is_admitted_independently() {
    compile(SOURCE, "LazyOrderedOnly");
}
#[test]
fn selected_fold_owner_is_admitted_independently() {
    compile(SOURCE, "LazyEmptyFold");
}
#[test]
fn selected_array_reads_and_later_conditions_keep_their_source_calls() {
    compile(SOURCE, "LazyAssignment");
}
#[test]
fn partial_targets_keep_condition_rhs_and_index_execution_owners() {
    compile(SOURCE, "LazyIndexOrder");
}
#[test]
fn an_empty_fold_does_not_require_its_unreached_arm_scratch() {
    let source = SOURCE.replace("for pass in 1:2 loop", "for pass in 1:0 loop");
    assert_ne!(source, SOURCE);
    Compiler::new()
        .model("LazyEmptyFold")
        .compile_str(&source, "EmptyLazyFold.mo")
        .unwrap_or_else(|error| panic!("an empty fold has no reached arm read: {error}"));
}
#[test]
fn an_unconditionally_selected_missing_arm_retains_its_ordered_assigned_check() {
    let source = SOURCE.replace(
        "y := if enabled then sum(scratch) else -1;",
        "y := sum(scratch);",
    );
    assert_ne!(source, SOURCE);
    let compiled = Compiler::new()
        .model("LazyBodyOnly")
        .compile_str(&source, "UnsafeLazyAssignment.mo")
        .unwrap_or_else(|error| {
            panic!("the top-level read has an ordered assigned-state owner: {error}")
        });
    super::assert_runtime_call_retained(&compiled.dae);
    assert_body_assigned_read(&compiled.dae, &source);
}

fn assert_body_assigned_read(dae: &rumoca_ir_dae::Dae, source: &str) {
    use rumoca_ir_dae::{ExpressionOperation, FunctionAssignedState, FunctionStatementView};
    dae.inspect(|view| {
        let function = (0..view.function_count())
            .filter_map(|index| view.function_id(index).and_then(|id| view.function(id)))
            .find(|function| function.name().as_str() == "Lazy.Body")
            .expect("the original called function remains present");
        let scratch = function
            .values()
            .find(|value| value.name().as_str() == "scratch")
            .expect("the declared mutable scratch retains its identity")
            .id();
        let output = function
            .values()
            .find(|value| value.name().as_str() == "y")
            .expect("the declared output retains its identity")
            .id();
        let statements = function.statements().collect::<Vec<_>>();
        let [
            FunctionStatementView::Conditional { conditional },
            FunctionStatementView::RequireAssigned {
                conditional: checked,
                value,
                definition,
                provenance,
            },
            FunctionStatementView::Assignment { definition: result },
        ] = &statements[..]
        else {
            panic!("source conditional, assigned-state check and output read must remain in order");
        };
        assert_eq!(conditional.id(), checked.id());
        assert_eq!(*value, scratch);
        assert_eq!(result.target(), output);
        assert!(matches!(
            conditional.outgoing(scratch),
            Some(FunctionAssignedState::Conditional { .. })
        ));
        assert!(
            matches!(view.expression(definition.rhs()).unwrap().operation(),
            ExpressionOperation::FunctionConditionalOutput { conditional: owner, value: target, .. }
                if owner == checked.id() && target == scratch)
        );
        let read = source.find("y := sum(scratch);").unwrap();
        assert_eq!(provenance.span().start.0, read);
    });
}

fn compile(source: &str, model: &str) {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, "LazyAssignment.mo")
        .unwrap_or_else(|error| panic!("selected source admission: {error}"));
    super::assert_runtime_call_retained(&compiled.dae);
}
