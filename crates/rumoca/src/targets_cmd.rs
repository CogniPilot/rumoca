//! `rumoca targets` — list built-in code-gen targets (the capability matrix)
//! plus the IR-dump (`--emit`) pseudo-targets. Split out of `main.rs` for the
//! SPEC_0021 file-size budget.

use anyhow::Result;
use rumoca_compile::codegen::targets::{TargetCompatibilityEntry, TargetFeatureSupport};
use std::fmt::Write as _;

pub(crate) fn run(json: bool) -> Result<()> {
    let matrix = rumoca_compile::codegen::targets::builtin_target_compatibility_matrix()?;
    if json {
        println!("{}", serde_json::to_string_pretty(&matrix)?);
        return Ok(());
    }

    print!("{}", render_human_targets(&matrix)?);
    Ok(())
}

fn render_human_targets(
    matrix: &[TargetCompatibilityEntry],
) -> std::result::Result<String, std::fmt::Error> {
    let target_width = matrix
        .iter()
        .map(|entry| entry.id.len())
        .max()
        .unwrap_or("target".len())
        .max("target".len());

    let mut output = String::new();
    writeln!(
        output,
        "{:<target_width$} {:<6} {:<16} {:<12} {:<5} {:<8} {:<8} {:<8} {:<8} {:<8} {:<8} {:<8} {:<8} {:<8} {:<8}",
        "target",
        "ir",
        "mode",
        "deploy",
        "level",
        "scalar",
        "matmul",
        "linsolve",
        "elem",
        "stencil",
        "sparse",
        "dyn-ctrl",
        "events",
        "fwd-ad",
        "rev-ad",
        target_width = target_width
    )?;
    for entry in matrix {
        writeln!(
            output,
            "{:<target_width$} {:<6} {:<16} {:<12} {:<5} {:<8} {:<8} {:<8} {:<8} {:<8} {:<8} {:<8} {:<8} {:<8} {:<8}",
            entry.id,
            format!("{:?}", entry.ir).to_ascii_lowercase(),
            entry.execution_mode.as_deref().unwrap_or("unknown"),
            entry.deployment_class.as_deref().unwrap_or("unknown"),
            target_readiness_label(entry.readiness_level),
            target_support_label(entry.scalar_programs),
            target_support_label(entry.matmul),
            target_support_label(entry.linsolve),
            target_support_label(entry.elementwise),
            target_support_label(entry.stencil),
            target_support_label(entry.sparse),
            target_support_label(entry.dynamic_control_flow),
            target_support_label(entry.events),
            target_support_label(entry.forward_ad),
            target_support_label(entry.reverse_ad),
            target_width = target_width
        )?;
    }

    writeln!(
        output,
        "\nLegend:\n  \
         ir       representation consumed (ast < flat < dae; dae branches to solve or galec)\n  \
         mode     code-gen style (symbolic / compiled / source-transform / packaged)\n  \
         deploy   deployment class (cpu / symbolic / fmu / efmi / browser / modelica)\n  \
         level    readiness 0=experimental .. 2=validated (? = unrated)\n  \
         (per-feature columns) scalar/matmul/linsolve/elem/stencil/sparse/dyn-ctrl/events/fwd-ad/rev-ad\n           \
         feature support: native | scalar (scalarized) | no | unknown"
    )?;

    // IR dumps are not code-gen targets; they live behind `--emit`.
    writeln!(
        output,
        "\nDump an IR stage with `rumoca compile --emit <value>`:"
    )?;
    for (id, stage) in [
        ("ast-mo / ast-json", "abstract syntax tree"),
        ("flat-mo / flat-json", "flattened model"),
        ("dae-mo / dae-json", "DAE system"),
        ("solve-json", "solver IR (no Modelica form)"),
    ] {
        writeln!(output, "  --emit {id:<22} {stage}")?;
    }
    writeln!(
        output,
        "\n--target also accepts a directory containing target.toml, or a raw \
         <file>.jinja template (use --phase to pick the IR it receives)."
    )?;
    Ok(output)
}

pub(crate) fn target_readiness_label(readiness_level: Option<u8>) -> String {
    readiness_level
        .map(|level| level.to_string())
        .unwrap_or_else(|| "?".to_string())
}

pub(crate) fn target_support_label(support: TargetFeatureSupport) -> &'static str {
    match support {
        TargetFeatureSupport::Native => "native",
        TargetFeatureSupport::Scalar => "scalar",
        TargetFeatureSupport::Unsupported => "no",
        TargetFeatureSupport::Unknown => "unknown",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn human_targets_table_reports_elementwise_and_stencil_support() {
        let matrix = rumoca_compile::codegen::targets::builtin_target_compatibility_matrix()
            .expect("target compatibility matrix should build");
        let output = render_human_targets(&matrix).expect("human target table should render");
        let wgsl_row = output
            .lines()
            .find(|line| line.starts_with("wgsl-solve "))
            .unwrap_or_else(|| panic!("wgsl-solve row should be present:\n{output}"));

        assert!(output.lines().next().is_some_and(|line| {
            line.split_whitespace().collect::<Vec<_>>()
                == vec![
                    "target", "ir", "mode", "deploy", "level", "scalar", "matmul", "linsolve",
                    "elem", "stencil", "sparse", "dyn-ctrl", "events", "fwd-ad", "rev-ad",
                ]
        }));
        assert!(
            output.contains(
                "scalar/matmul/linsolve/elem/stencil/sparse/dyn-ctrl/events/fwd-ad/rev-ad"
            ),
            "legend should explain elem and stencil columns"
        );
        assert_eq!(
            wgsl_row.split_whitespace().collect::<Vec<_>>(),
            vec![
                "wgsl-solve",
                "solve",
                "jit",
                "gpu",
                "0",
                "native",
                "scalar",
                "unknown",
                "native",
                "native",
                "no",
                "no",
                "native",
                "no",
                "no",
            ]
        );
    }
}
