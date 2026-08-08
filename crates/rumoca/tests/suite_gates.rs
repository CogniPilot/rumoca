//! Umbrella binary for the repo-inspection policy and budget gates.
//!
//! These members link no `rumoca` library, so this binary stays a few MB and
//! `cargo xtask verify quick` can run the fast gates without paying for a
//! whole-compiler link. Keep it that way: a member that needs the compiler
//! belongs in `suite_core.rs`, which also documents the grouping rules.

#[path = "code_size_budget_test.rs"]
mod code_size_budget_test;
#[path = "dae_loc_trigger_test.rs"]
mod dae_loc_trigger_test;
#[path = "history_policy_test.rs"]
mod history_policy_test;
#[path = "mls_formalization_manifest_test.rs"]
mod mls_formalization_manifest_test;
#[path = "spec_budget_test.rs"]
mod spec_budget_test;
