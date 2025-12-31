//! Benchmark for balance checking with MSL
//!
//! Run with: MSL_PATH=/path/to/MSL cargo run --release --example bench_balance
//! Profile with: MSL_PATH=/path/to/MSL cargo flamegraph --example bench_balance
//!
//! Set the MSL_PATH environment variable to point to your Modelica Standard Library installation.

use std::sync::Arc;

fn main() {
    // Setup: Load MSL from environment variable
    let msl_path = match std::env::var("MSL_PATH") {
        Ok(path) => std::path::PathBuf::from(path),
        Err(_) => {
            eprintln!("Error: MSL_PATH environment variable not set.");
            eprintln!(
                "Please set MSL_PATH to point to your Modelica Standard Library installation."
            );
            eprintln!(
                "Example: MSL_PATH=/path/to/ModelicaStandardLibrary_v4.1.0 cargo run --release --example bench_balance"
            );
            std::process::exit(1);
        }
    };

    if !msl_path.exists() {
        eprintln!("Error: MSL_PATH '{}' does not exist.", msl_path.display());
        std::process::exit(1);
    }

    println!("Loading MSL...");
    let start = std::time::Instant::now();

    // Discover and parse MSL files
    let files = rumoca::ir::transform::multi_file::discover_modelica_files(&msl_path).unwrap();
    println!("Found {} files", files.len());

    let definitions: Vec<_> = files
        .iter()
        .filter_map(|path| {
            let path_str = path.to_string_lossy().to_string();
            rumoca::compiler::parse_file_cached(path).map(|ast| (path_str, ast))
        })
        .collect();
    println!(
        "Parsed {} files in {:?}",
        definitions.len(),
        start.elapsed()
    );

    // Merge into single StoredDefinition
    let merge_start = std::time::Instant::now();
    let merged = rumoca::ir::transform::multi_file::merge_stored_definitions(definitions).unwrap();
    println!("Merged in {:?}", merge_start.elapsed());

    // Build class dictionary
    let dict_start = std::time::Instant::now();
    let lib_def = Arc::new(merged);
    let lib_dict = rumoca::ir::transform::flatten::get_or_build_library_dict("Modelica", &lib_def);
    println!(
        "Built library dict ({} entries) in {:?}",
        lib_dict.len(),
        dict_start.elapsed()
    );

    // User code - uses PID from MSL (version 1: k=2)
    let user_code_v1 = r#"
model Test
    import PID = Modelica.Blocks.Continuous.PID;
    PID pid(k=2, I.reset = true);
    Real x(start=0);
equation
    der(x) = pid.y;
end Test;
"#;

    // User code after edit (version 2: k=3)
    let user_code_v2 = r#"
model Test
    import PID = Modelica.Blocks.Continuous.PID;
    PID pid(k=3, I.reset = true);
    Real x(start=0);
equation
    der(x) = pid.y;
end Test;
"#;

    let library_dicts = vec![lib_dict];

    // First compile (cold)
    println!("\n=== First compile (k=2) ===");
    let user_def_v1 = {
        let mut grammar = rumoca::modelica_grammar::ModelicaGrammar::new();
        rumoca::modelica_parser::parse(user_code_v1, "user.mo", &mut grammar).unwrap();
        grammar.modelica.unwrap()
    };

    let start = std::time::Instant::now();
    let result = rumoca::compiler::pipeline::check_balance_with_library_dicts(
        &user_def_v1,
        &library_dicts,
        Some("Test"),
    );
    let elapsed = start.elapsed();
    match result {
        Ok(balance) => println!("  Time: {:?} - {:?}", elapsed, balance.status),
        Err(e) => println!("  Time: {:?} - Error: {}", elapsed, e),
    }

    // Simulate editing: change k=2 to k=3
    println!("\n=== Second compile after edit (k=3) ===");
    let user_def_v2 = {
        let mut grammar = rumoca::modelica_grammar::ModelicaGrammar::new();
        rumoca::modelica_parser::parse(user_code_v2, "user.mo", &mut grammar).unwrap();
        grammar.modelica.unwrap()
    };

    // Run multiple times to see variance
    for i in 0..5 {
        let start = std::time::Instant::now();
        let result = rumoca::compiler::pipeline::check_balance_with_library_dicts(
            &user_def_v2,
            &library_dicts,
            Some("Test"),
        );
        let elapsed = start.elapsed();
        match result {
            Ok(balance) => println!("  Run {}: {:?} - {:?}", i, elapsed, balance.status),
            Err(e) => println!("  Run {}: {:?} - Error: {}", i, elapsed, e),
        }
    }
}
