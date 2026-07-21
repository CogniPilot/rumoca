use super::*;

#[test]
fn bare_negative_dimensions_propagate_from_projection_entry() {
    let dimensions = HashMap::from([("A".to_string(), vec![-1]), ("B".to_string(), vec![-1])]);
    let expression = mul(var_ref("A"), var_ref("B"));
    let error = Projector(&dimensions, &IndexMap::new())
        .project(&expression, 0, &[])
        .expect_err("invalid bare operands must fail closed during projection entry");

    assert!(error.to_string().contains("negative dimension"));
    assert_eq!(error.source_span(), Some(test_span()));
}
