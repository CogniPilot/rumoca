//! Regression for enum-parameter branch selection in mismatched if-equations.

use super::*;

const SOURCE: &str = r#"
type Init = enumeration(NoInit, InitialState, InitialOutput);

model FilterLike
  parameter Init initType = Init.NoInit;
  Real x;
  Real y;
equation
  if initType == Init.InitialState then
    x = 1;
  elseif initType == Init.InitialOutput then
    x = 2;
    y = 3;
  end if;
end FilterLike;

model UsesNestedEnumIfBranchSelection
  FilterLike filter(initType = Init.InitialOutput);
end UsesNestedEnumIfBranchSelection;

model UsesArrayEnumIfBranchSelection
  FilterLike filter[2](each initType = Init.InitialOutput);
end UsesArrayEnumIfBranchSelection;

record GenericChillerData
  final parameter Integer nCapFunT = 6;
  parameter Real capFunT[nCapFunT];
end GenericChillerData;

record ConcreteChillerData = GenericChillerData(
  capFunT = {1, 2, 3, 4, 5, 6});

model UsesArrayRecordFieldDimension
  parameter ConcreteChillerData per[2];
  Real y;
equation
  y = per[2].capFunT[6];
end UsesArrayRecordFieldDimension;
"#;

#[test]
fn test_mismatched_if_uses_scoped_enum_parameter_value() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", SOURCE)
        .expect("enum if fixture should parse");

    let result = session
        .compile_model("UsesNestedEnumIfBranchSelection")
        .expect("scoped enum parameter should select the matching if branch");

    assert!(
        rumoca_phase_dae::balance::is_balanced(&result.dae),
        "selected branch should keep the model balanced: {}",
        rumoca_phase_dae::balance::balance_detail(&result.dae)
    );
}

#[test]
fn test_array_record_field_dimension_uses_canonical_parameter_value() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", SOURCE)
        .expect("array record dimension fixture should parse");

    let result = session
        .compile_model("UsesArrayRecordFieldDimension")
        .expect("array record field dimension should resolve for second element");

    assert!(
        rumoca_phase_dae::balance::is_balanced(&result.dae),
        "array record field dimension model should remain balanced: {}",
        rumoca_phase_dae::balance::balance_detail(&result.dae)
    );
}

#[test]
fn test_mismatched_if_uses_canonical_array_enum_parameter_value() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", SOURCE)
        .expect("enum array if fixture should parse");

    let result = session
        .compile_model("UsesArrayEnumIfBranchSelection")
        .expect("array enum parameter should select the matching if branch");

    assert!(
        rumoca_phase_dae::balance::is_balanced(&result.dae),
        "selected array branches should keep the model balanced: {}",
        rumoca_phase_dae::balance::balance_detail(&result.dae)
    );
}
