use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::time::Instant;

use rumoca_session::compile::{Session, SessionConfig, SourceRootKind};

fn default_msl_archive_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("workspace root")
        .join("target/msl/ModelicaStandardLibrary-4.1.0")
}

fn parse_args() -> PathBuf {
    let mut args = std::env::args().skip(1);
    match args.next() {
        Some(arg) if arg == "--library-root" => {
            let value = args.next().expect("--library-root requires a path");
            assert!(
                args.next().is_none(),
                "unexpected extra arguments after --library-root"
            );
            PathBuf::from(value)
        }
        Some(arg) => panic!("unknown arg: {arg}"),
        None => default_msl_archive_root(),
    }
}

fn namespace_class_names(session: &mut Session) -> Vec<String> {
    let mut stack = vec![String::new()];
    let mut seen = HashSet::new();
    let mut names = Vec::new();

    while let Some(prefix) = stack.pop() {
        let entries = session
            .namespace_index_query(&prefix)
            .expect("query namespace completion cache");
        for (_, full_name, has_children) in entries {
            if !seen.insert(full_name.clone()) {
                continue;
            }
            names.push(full_name.clone());
            if has_children {
                stack.push(format!("{full_name}."));
            }
        }
    }

    names.sort_unstable();
    names
}

fn main() {
    let archive_root = parse_args();
    let library_paths = [
        archive_root.join("Modelica 4.1.0"),
        archive_root.join("ModelicaServices 4.1.0"),
        archive_root.join("Complex.mo"),
    ];

    for path in &library_paths {
        assert!(
            path.exists(),
            "expected library path to exist: {}",
            path.display()
        );
    }

    let mut session = Session::new(SessionConfig::default());

    for library_path in &library_paths {
        let started = Instant::now();
        let report = session.index_library_tolerant(
            &library_path.display().to_string(),
            SourceRootKind::DurableLibrary,
            library_path,
            None,
        );
        assert!(
            report.diagnostics.is_empty(),
            "library load failed for {}: {:?}",
            library_path.display(),
            report.diagnostics
        );
        println!(
            "indexed {} in {:?} ({:?}, {} inserted)",
            library_path.display(),
            started.elapsed(),
            report.cache_status,
            report.inserted_file_count
        );
    }

    let completion_started = Instant::now();
    let class_names = namespace_class_names(&mut session);
    println!(
        "built library completion cache in {:?} ({} classes)",
        completion_started.elapsed(),
        class_names.len()
    );
}
