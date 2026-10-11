//! Temporary raw golden issuance from the existing canonical test fixture.
//! Revert this probe after capturing its output; retain the ordinary golden tests.

#[test]
fn emit_current_representative_solve_problem_golden() {
    let problem = super::representative_solve_problem_fixture();
    let json = serde_json::to_string_pretty(&problem)
        .expect("existing representative Solve fixture serializes");
    println!("RUMOCA_CURRENT_SOLVE_GOLDEN_BEGIN_7c9bce9856ef470d");
    println!("{json}");
    println!("RUMOCA_CURRENT_SOLVE_GOLDEN_END_7c9bce9856ef470d");
}
