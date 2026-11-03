use super::*;
use crate::typed_program::SolveElementToken;

fn real64(dimensions: Vec<u32>) -> SolveValueType {
    SolveValueType::tensor(
        SolveScalarType::Real {
            format: SolveRealFormat::Binary64,
        },
        dimensions,
    )
    .unwrap()
}

fn integer(domain: SolveIntegerDomain, dimensions: Vec<u32>) -> SolveValueType {
    SolveValueType::tensor(SolveScalarType::Integer(domain), dimensions).unwrap()
}

fn boolean() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::Boolean)
}

fn converted(crossing: SolveScalarCrossing) -> (SolveElementToken, u32, bool) {
    match crossing {
        SolveScalarCrossing::Converted {
            element,
            extent,
            domain,
        } => (element, extent, domain.is_some()),
        SolveScalarCrossing::Direct => panic!("expected a converted crossing"),
    }
}

#[test]
fn binary64_values_pass_through_and_others_convert_with_input_checks() {
    let narrow = SolveIntegerDomain::construct(1, 3).unwrap();
    let inputs = [
        real64(vec![3]),
        integer(SolveIntegerDomain::FULL, vec![2]),
        integer(narrow, vec![1]),
        boolean(),
    ];
    let outputs = [
        SolvePureCallOutput::result(integer(narrow, vec![2])),
        SolvePureCallOutput::result(real64(vec![2, 2])),
        SolvePureCallOutput::result(boolean()),
    ];
    let boundary = SolveScalarBoundary::of(&inputs, &outputs).unwrap();

    let crossings: Vec<_> = boundary
        .inputs()
        .iter()
        .map(|item| item.crossing())
        .collect();
    assert_eq!(crossings[0], SolveScalarCrossing::Direct);
    assert_eq!(
        converted(crossings[1]),
        (SolveElementToken::Int64, 2, false)
    );
    assert_eq!(converted(crossings[2]), (SolveElementToken::Int64, 1, true));
    assert_eq!(
        converted(crossings[3]),
        (SolveElementToken::Boolean, 1, false)
    );
    assert!(
        boundary
            .inputs()
            .iter()
            .enumerate()
            .all(|(n, item)| item.argument() == n)
    );

    // An output only widens: a narrowed integer result carries no check.
    let placed: Vec<_> = boundary
        .outputs()
        .iter()
        .map(|item| (item.output(), item.offset(), item.extent()))
        .collect();
    assert_eq!(placed, [(0, 0, 2), (1, 2, 4), (2, 6, 1)]);
    assert_eq!(
        converted(boundary.outputs()[0].crossing()),
        (SolveElementToken::Int64, 2, false)
    );
    assert_eq!(
        boundary.outputs()[1].crossing(),
        SolveScalarCrossing::Direct
    );
    assert_eq!(boundary.output_scalar_count(), 7);
}

#[test]
fn a_binary32_value_has_no_scalar_crossing() {
    let real32 = SolveValueType::scalar(SolveScalarType::Real {
        format: SolveRealFormat::Binary32,
    });
    let real = [SolvePureCallOutput::result(real64(vec![1]))];
    assert_eq!(
        SolveScalarBoundary::of(&[real64(vec![1]), real32.clone()], &real),
        Err(SolveScalarBoundaryError::UnsupportedInput { argument: 1 })
    );
    assert_eq!(
        SolveScalarBoundary::of(&[], &[real[0].clone(), SolvePureCallOutput::result(real32)]),
        Err(SolveScalarBoundaryError::UnsupportedOutput { output: 1 })
    );
}

#[test]
fn the_issued_boundary_serializes_closed_crossing_tags() {
    let narrow = SolveIntegerDomain::construct(-2, 5).unwrap();
    let boundary = SolveScalarBoundary::of(
        &[real64(vec![1]), integer(narrow, vec![2])],
        &[SolvePureCallOutput::result(boolean())],
    )
    .unwrap();
    let wire = serde_json::to_value(&boundary).unwrap();
    assert_eq!(wire["inputs"][0]["crossing"]["crossing"], "direct");
    assert_eq!(wire["inputs"][1]["crossing"]["crossing"], "converted");
    assert_eq!(wire["inputs"][1]["crossing"]["element"], "int64");
    assert_eq!(wire["inputs"][1]["crossing"]["extent"], 2);
    assert_eq!(
        wire["inputs"][1]["crossing"]["domain"]["minimum"]["sign"],
        "negative"
    );
    assert_eq!(wire["outputs"][0]["crossing"]["element"], "boolean");
    assert!(wire["outputs"][0]["crossing"]["domain"].is_null());
    assert_eq!(wire["outputs"][0]["offset"], 0);
}
