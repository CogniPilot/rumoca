use rerun_except::rerun_except;
use std::process::Command;

fn main() {
    lalrpop::process_root().unwrap();
    rerun_except(&["*.mo", "*.jinja", "*.sdf", "*.py"]).unwrap();

    // create git hash
    let output = Command::new("git")
        .args(&["describe", "--dirty", "--tags"])
        .output()
        .unwrap();
    let git_hash = String::from_utf8(output.stdout).unwrap();
    println!("cargo:rerun-if-changed=.git/HEAD");
    println!("cargo:rustc-env=GIT_HASH={}", git_hash);
}
