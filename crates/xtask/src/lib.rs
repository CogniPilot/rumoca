pub mod web_assets;

mod print_plan_template_check;
mod traversal_policy_check;

pub fn run_traversal_policy_check() -> anyhow::Result<()> {
    traversal_policy_check::run()
}

pub fn run_print_plan_template_check() -> anyhow::Result<()> {
    print_plan_template_check::run()
}

pub fn run_print_plan_template_check_cli(args: &[String]) -> anyhow::Result<()> {
    print_plan_template_check::run_cli(args)
}
