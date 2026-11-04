pub mod migration_ledger;
// Identity only; no migration authorization is issued here.
pub mod print_boundary_check;
pub mod web_assets;

mod traversal_policy_check;

pub fn run_traversal_policy_check() -> anyhow::Result<()> {
    traversal_policy_check::run()
}
