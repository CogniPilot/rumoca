use rerun_except::rerun_except;

fn main() {
    lalrpop::process_root().unwrap();
    rerun_except(&["templates/*.tera"]).unwrap();
    rerun_except(&["models/*.mo"]).unwrap();
}
