use super::*;
use tempfile::tempdir;

/// A user need not mention a dependency that is used only by another library.
#[test]
fn source_roots_follow_transitive_library_references() {
    for reverse in [false, true] {
        let temp = tempdir().expect("tempdir");
        let a = temp.path().join("LibA");
        let b = temp.path().join("LibB");
        fs::create_dir_all(&a).unwrap();
        fs::create_dir_all(&b).unwrap();
        fs::write(a.join("package.mo"), "package LibA type T3 = Real[3]; function twice input Real x; output Real y; algorithm y := 2*x; end twice; end LibA;").unwrap();
        fs::write(b.join("package.mo"), "package LibB model M LibA.T3 v; Real x(start=1, fixed=true); equation v={1,2,3}; der(x)=LibA.twice(x); end M; end LibB;").unwrap();
        let paths = if reverse { [&b, &a] } else { [&a, &b] };
        let compiler = Compiler::new()
            .model("Top")
            .source_root(&paths[0].to_string_lossy())
            .source_root(&paths[1].to_string_lossy());
        let source = "model Top LibB.M m; end Top;";
        let result = compiler.compile_str(source, "Top.mo");
        assert!(
            result.is_ok(),
            "transitive library reference must compile: {:?}",
            result.err()
        );
    }
}

/// Three loading waves, a lexical library cycle, and an unrelated broken root
/// must not force eager loading of every configured library.
#[test]
fn source_roots_close_cycles_and_skip_unreferenced_roots() {
    let temp = tempdir().unwrap();
    let libraries = [
        ("LibA", "package LibA constant Real scale=LibC.k; end LibA;"),
        (
            "LibB",
            "package LibB model M Real x(start=1, fixed=true); equation der(x)=LibA.scale*x; end M; end LibB;",
        ),
        (
            "LibC",
            "package LibC constant Real k=2; model UnusedCycle LibB.M b; end UnusedCycle; end LibC;",
        ),
        ("Bad", "package Bad !!! intentionally invalid"),
    ];
    let mut compiler = Compiler::new().model("Top");
    for (name, text) in libraries {
        let dir = temp.path().join(name);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("package.mo"), text).unwrap();
        compiler = compiler.source_root(&dir.to_string_lossy());
    }
    let result = compiler.compile_str("model Top LibB.M b; end Top;", "Top.mo");
    assert!(
        result.is_ok(),
        "dependency closure must terminate: {:?}",
        result.err()
    );
}
