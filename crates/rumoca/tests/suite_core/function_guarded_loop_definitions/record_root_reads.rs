//! Constructor-owned record paths require their exact readable mutable root.
use super::assert_runtime_call_retained;
use rumoca::{Compiler, CompilerError};

const SOURCE: &str = include_str!("../../fixtures/function_guarded_loops/RecordRootReads.mo");

#[test]
fn nested_and_quoted_record_fields_retain_their_selected_whole_root_reads() {
    let compiled = Compiler::new()
        .model("RecordRootReads")
        .compile_str(SOURCE, "RecordRootReads.mo")
        .expect("nested and quoted paths retain their original declaration owner");
    assert_runtime_call_retained(&compiled.dae);
}

#[test]
fn reenabled_record_root_cannot_borrow_a_skipped_definition() {
    let source = SOURCE.replacen(
        "    end if;\n    if valid then",
        "    end if;\n    valid := true;\n    if valid then",
        1,
    );
    assert_ne!(source, SOURCE);
    require_source_refusal(
        &source,
        "ED020",
        "missing function value definition for identity 1",
        (5, "tree.branch.weight"),
    );
}

#[test]
fn an_uninitialized_record_sibling_cannot_issue_a_whole_root_read_receipt() {
    let source = SOURCE.replacen(
        "tree := Tree(Leaf(values[1],values[2]),2);",
        "tree.branch.weight := values[1];",
        1,
    );
    assert_ne!(source, SOURCE);
    require_source_refusal(
        &source,
        "ED019",
        "reads elements of `tree__scale` that do not all have a definition",
        (0, "y"),
    );
}

#[test]
fn a_written_field_remains_readable_without_unused_record_siblings() {
    let compiled = Compiler::new()
        .model("RecordFieldOnly")
        .compile_str(SOURCE, "InitializedRecordField.mo")
        .expect("the reached scale field has its own definition, without a whole Tree");
    assert_runtime_call_retained(&compiled.dae);
}

#[test]
fn a_whole_consumer_after_a_partial_update_observes_the_current_root() {
    let source = SOURCE
        .replace(
            "    fieldRecord.scale := value;",
            "    fieldRecord := Tree(Leaf(10,20),99);\n    fieldRecord.scale := value;",
        )
        .replace(
            "    y := fieldRecord.scale;",
            "    y := InputOnly(fieldRecord);",
        );
    let compiled = Compiler::new()
        .model("RecordFieldOnly")
        .compile_str(&source, "UpdatedWholeRecord.mo")
        .expect("the whole consumer retains all current constructor fields");
    let evaluated =
        rumoca_sim::eval_dae_at(&compiled.dae, &rumoca_sim::SimOptions::default(), &[], 0.0)
            .expect("the whole updated record evaluates");
    assert!(evaluated.report.error.is_none());
    let output = evaluated
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "fieldOnly")
        .expect("the authored scalar output remains present");
    assert_eq!(output.value.to_bits(), 32.0_f64.to_bits());
}

#[test]
fn an_unwritten_field_read_cannot_use_an_unrelated_written_field() {
    let source = SOURCE.replace(
        "    y := fieldRecord.scale;",
        "    y := fieldRecord.branch.weight;",
    );
    let error = match Compiler::new()
        .model("RecordFieldOnly")
        .compile_str(&source, "UnwrittenRecordField.mo")
    {
        Err(error) => error,
        Ok(_) => panic!("an unassigned branch field cannot become a value from scale"),
    };
    let CompilerError::CompileDiagnosticsError {
        failures,
        source_map,
        ..
    } = &error
    else {
        panic!("expected checked construction refusal: {error}");
    };
    let failure = failures
        .iter()
        .find(|failure| failure.model_name == "RecordFieldOnly")
        .unwrap_or_else(|| panic!("missing original model refusal: {error}"));
    assert_eq!(
        failure
            .phase
            .as_ref()
            .map(|phase| phase.to_string())
            .as_deref(),
        Some("ToDae")
    );
    let label = failure
        .primary_label
        .as_ref()
        .expect("source-owned read refusal");
    let (_, actual_source) = source_map
        .as_ref()
        .expect("source map")
        .get_source(label.span.source)
        .expect("original source identity");
    assert_eq!(actual_source, source);
}

#[test]
fn a_whole_declaration_default_retains_its_original_record_owner() {
    let source = SOURCE.replace(
        "    Tree fieldRecord;",
        "    Tree fieldRecord = Tree(Leaf(10,20),99);",
    );
    let flat = Compiler::new()
        .model("RecordFieldOnly")
        .compile_str_flat(&source, "DefaultRecordLocal.mo")
        .expect("the source declaration binding reaches the original Flat owner");
    let local = flat
        .functions
        .values()
        .flat_map(|function| &function.locals)
        .find(|local| local.name == "fieldRecord")
        .expect("field-only admission cannot remove a bound original root");
    assert!(
        local.default.is_some(),
        "the whole source binding is retained"
    );
}

fn require_source_refusal(
    source: &str,
    code: &str,
    detail: &str,
    (read_offset, primary_fragment): (usize, &str),
) {
    let error = match Compiler::new()
        .model("RecordRootReads")
        .compile_str(source, "UndefinedRecordRoot.mo")
    {
        Err(error) => error,
        Ok(_) => panic!(
            "expected static record-root refusal; checked runtime admission needs qualification"
        ),
    };
    let CompilerError::CompileDiagnosticsError {
        failures,
        source_map,
        ..
    } = &error
    else {
        panic!("expected source construction refusal: {error}");
    };
    let failure = failures
        .iter()
        .find(|failure| failure.model_name == "RecordRootReads")
        .unwrap_or_else(|| panic!("missing original model failure: {error}"));
    assert_eq!(
        failure
            .phase
            .as_ref()
            .map(|phase| phase.to_string())
            .as_deref(),
        Some("ToDae")
    );
    assert!(
        failure
            .error_code
            .as_deref()
            .is_some_and(|actual| actual.ends_with(code)),
        "{error}"
    );
    let label = failure.primary_label.as_ref().unwrap();
    assert!(label.primary);
    let (_, issued) = source_map
        .as_ref()
        .unwrap()
        .get_source(label.span.source)
        .unwrap();
    assert_eq!(issued, source);
    let read = source
        .find("y := tree.branch.weight + tree.scale;")
        .expect("the original reached read remains in the negative source");
    assert_eq!(label.span.start.0, read + read_offset);
    assert_eq!(
        label.span.end.0,
        read + read_offset + primary_fragment.len()
    );
    assert_eq!(
        issued.get(label.span.start.0..label.span.end.0),
        Some(primary_fragment)
    );
    assert!(failure.error.contains(detail), "{error}");
}
