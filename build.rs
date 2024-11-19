use rerun_except::rerun_except;

fn main() {
    lalrpop::process_root().unwrap();
    rerun_except(&["src/generators/templates/*.tera"]).unwrap();
}
