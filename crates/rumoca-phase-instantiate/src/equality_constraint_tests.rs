use crate::{InstantiateError, InstantiationOutcome, instantiate_model_with_outcome};
use rumoca_ir_ast as ast;
use rumoca_phase_parse::parse_to_ast;
use rumoca_phase_resolve::resolve;

fn resolved_tree(source: &str) -> ast::ClassTree {
    let file = "equality_constraint_instantiate_test.mo";
    let stored = parse_to_ast(source, file).expect("fixture parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file, source);
    resolve(ast::ParsedTree::new(tree))
        .expect("fixture resolves")
        .inner()
        .clone()
}

fn component<'a>(overlay: &'a ast::InstanceOverlay, path: &str) -> &'a ast::InstanceData {
    overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == path)
        .unwrap_or_else(|| panic!("missing component occurrence {path}"))
}

const EXACT_OCCURRENCES: &str = r#"
record R
  Real x;
  replaceable function equalityConstraint
    input R a;
    input R b;
    output Real residue[2];
  end equalityConstraint;
end R;

function AlternateConstraint
  extends R.equalityConstraint;
end AlternateConstraint;

model M
  R ordinary;
  R alternate(redeclare function equalityConstraint = AlternateConstraint);
end M;
"#;

#[test]
fn exact_occurrences_remain_pending_until_effective_typecheck() {
    let tree = resolved_tree(EXACT_OCCURRENCES);
    let overlay = match instantiate_model_with_outcome(&tree, "M") {
        InstantiationOutcome::Success(overlay) => overlay,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        InstantiationOutcome::Error(error) => panic!("valid exposure failed: {error}"),
    };
    assert_eq!(
        overlay.finalized_overconstrained().err(),
        Some(ast::EqualityConstraintOccurrenceError::EffectiveTypeCatalogNotFinalized),
        "Instantiate must not expose declaration-only proofs as typed semantics",
    );
    assert_eq!(overlay.overconstrained_construction_counts().0, 2);

    assert_eq!(
        component(&overlay, "ordinary.x")
            .qualified_name
            .to_flat_string(),
        "ordinary.x"
    );
    assert_eq!(
        component(&overlay, "alternate.x")
            .qualified_name
            .to_flat_string(),
        "alternate.x"
    );
}

#[test]
fn compact_record_arrays_fall_back_to_exact_per_occurrence_exposures() {
    let source = format!("{EXACT_OCCURRENCES}\nmodel ArrayM\n  R records[2];\nend ArrayM;");
    let tree = resolved_tree(&source);
    let overlay = match instantiate_model_with_outcome(&tree, "ArrayM") {
        InstantiationOutcome::Success(overlay) => overlay,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        InstantiationOutcome::Error(error) => panic!("record array failed: {error}"),
    };
    assert_eq!(overlay.overconstrained_construction_counts().0, 2);
    assert_eq!(
        overlay.finalized_overconstrained().err(),
        Some(ast::EqualityConstraintOccurrenceError::EffectiveTypeCatalogNotFinalized),
    );
}

#[test]
fn same_spelling_from_another_package_cannot_satisfy_exact_record_identity() {
    let source = r#"
package A
  record R
    Real x;
    replaceable function equalityConstraint
      input R a;
      input R b;
      output Real residue[1];
    end equalityConstraint;
  end R;
end A;
package B
  record R
    Real x;
  end R;
end B;
function WrongConstraint
  input B.R a;
  input B.R b;
  output Real residue[1];
end WrongConstraint;
model M
  A.R r(redeclare function equalityConstraint = WrongConstraint);
end M;
"#;
    let tree = resolved_tree(source);
    let error = match instantiate_model_with_outcome(&tree, "M") {
        InstantiationOutcome::Error(error) => error,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        InstantiationOutcome::Success(_) => panic!("B.R is not A.R"),
    };
    assert!(
        matches!(
            error.as_ref(),
            InstantiateError::InvalidEqualityConstraintExposure { reason, .. }
                if reason.contains("exact effective record type")
        ) || matches!(
            error.as_ref(),
            InstantiateError::RedeclareConstraintViolation { .. }
        )
    );
}

#[test]
fn malformed_selected_callable_is_ei036_not_absent_or_default_sized() {
    let source = r#"
record R
  Real x;
  replaceable function equalityConstraint
    input R a;
    input R b;
    output Real residue[1];
  end equalityConstraint;
end R;
function ScalarConstraint
  input R a;
  input R b;
  output Real residue;
end ScalarConstraint;
model M
  R r(redeclare function equalityConstraint = ScalarConstraint);
end M;
"#;
    let tree = resolved_tree(source);
    let error = match instantiate_model_with_outcome(&tree, "M") {
        InstantiationOutcome::Error(error) => error,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        InstantiationOutcome::Success(_) => panic!("scalar output is malformed"),
    };
    assert!(
        matches!(
            error.as_ref(),
            InstantiateError::InvalidEqualityConstraintExposure { reason, .. }
                if reason.contains("rank-1")
        ) || matches!(
            error.as_ref(),
            InstantiateError::RedeclareConstraintViolation { .. }
        )
    );
}

#[test]
fn symbolic_constant_extent_fails_ei036_without_a_replay_certificate() {
    let source = r#"
record R
  Real x;
  replaceable function equalityConstraint
    input R a;
    input R b;
  protected
    constant Integer n = 1;
  public
    output Real residue[n];
  end equalityConstraint;
end R;
model M
  R r;
end M;
"#;
    let tree = resolved_tree(source);
    let error = match instantiate_model_with_outcome(&tree, "M") {
        InstantiationOutcome::Error(error) => error,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        InstantiationOutcome::Success(_) => {
            panic!("symbolic extent has no independently replayable certificate")
        }
    };
    assert!(matches!(
        error.as_ref(),
        InstantiateError::InvalidEqualityConstraintExposure { reason, .. }
            if reason.contains("symbolic equalityConstraint extent certificate not implemented")
    ));
}
