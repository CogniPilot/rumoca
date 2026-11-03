fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    xtask::run_print_plan_template_check_cli(&args)
}
