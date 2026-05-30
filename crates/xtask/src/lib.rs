pub mod msl_tools;

#[path = "bin/rumoca-traversal-policy-check.rs"]
mod traversal_policy_check;

pub fn run_traversal_policy_check() -> anyhow::Result<()> {
    traversal_policy_check::run()
}
