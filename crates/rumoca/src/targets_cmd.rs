//! `rumoca targets` — list built-in code-gen targets (the capability matrix)
//! plus the IR-dump (`--emit`) pseudo-targets. Split out of `main.rs` for the
//! SPEC_0021 file-size budget.

use anyhow::Result;

pub(crate) fn run(json: bool) -> Result<()> {
    let matrix = rumoca_compile::codegen::targets::builtin_target_compatibility_matrix()?;
    if json {
        println!("{}", serde_json::to_string_pretty(&matrix)?);
        return Ok(());
    }

    let target_width = matrix
        .iter()
        .map(|entry| entry.id.len())
        .max()
        .unwrap_or("target".len())
        .max("target".len());

    println!(
        "{:<target_width$} {:<6} {:<16} {:<12} {:<5} {:<8} {:<8} {:<8} {:<8} {:<8} {:<8} {:<8} {:<8}",
        "target",
        "ir",
        "mode",
        "deploy",
        "level",
        "scalar",
        "matmul",
        "linsolve",
        "sparse",
        "dyn-ctrl",
        "events",
        "fwd-ad",
        "rev-ad",
        target_width = target_width
    );
    for entry in matrix {
        println!(
            "{:<target_width$} {:<6} {:<16} {:<12} {:<5} {:<8} {:<8} {:<8} {:<8} {:<8} {:<8} {:<8} {:<8}",
            entry.id,
            format!("{:?}", entry.ir).to_ascii_lowercase(),
            entry.execution_mode.as_deref().unwrap_or("unknown"),
            entry.deployment_class.as_deref().unwrap_or("unknown"),
            target_readiness_label(entry.readiness_level),
            target_support_label(entry.scalar_programs),
            target_support_label(entry.matmul),
            target_support_label(entry.linsolve),
            target_support_label(entry.sparse),
            target_support_label(entry.dynamic_control_flow),
            target_support_label(entry.events),
            target_support_label(entry.forward_ad),
            target_support_label(entry.reverse_ad),
            target_width = target_width
        );
    }

    println!(
        "\nLegend:\n  \
         ir       compiler stage the target consumes (ast < flat < dae < solve)\n  \
         mode     code-gen style (symbolic / compiled / source-transform / packaged)\n  \
         deploy   deployment class (cpu / symbolic / fmu / browser / modelica)\n  \
         level    readiness 0=experimental .. 2=validated (? = unrated)\n  \
         (per-feature columns) scalar/matmul/linsolve/sparse/dyn-ctrl/events/fwd-ad/rev-ad\n           \
         feature support: native | scalar (scalarized) | no | unknown"
    );

    // IR dumps are not code-gen targets; they live behind `--emit`.
    println!("\nDump an IR stage with `rumoca compile --emit <value>`:");
    for (id, stage) in [
        ("ast-mo / ast-json", "abstract syntax tree"),
        ("flat-mo / flat-json", "flattened model"),
        ("dae-mo / dae-json", "DAE system"),
        ("solve-json", "solver IR (no Modelica form)"),
        ("galec", "eFMI GALEC Algorithm Code"),
    ] {
        println!("  --emit {id:<22} {stage}");
    }
    println!(
        "\n--target also accepts a directory containing target.toml, or a raw \
         <file>.jinja template (use --phase to pick the IR it receives)."
    );
    Ok(())
}

pub(crate) fn target_readiness_label(readiness_level: Option<u8>) -> String {
    readiness_level
        .map(|level| level.to_string())
        .unwrap_or_else(|| "?".to_string())
}

pub(crate) fn target_support_label(
    support: rumoca_compile::codegen::targets::TargetFeatureSupport,
) -> &'static str {
    match support {
        rumoca_compile::codegen::targets::TargetFeatureSupport::Native => "native",
        rumoca_compile::codegen::targets::TargetFeatureSupport::Scalar => "scalar",
        rumoca_compile::codegen::targets::TargetFeatureSupport::Unsupported => "no",
        rumoca_compile::codegen::targets::TargetFeatureSupport::Unknown => "unknown",
    }
}
