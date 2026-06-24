use super::*;
use rumoca_ir_solve::{ComputeBlock, ComputeNode, SolveProblem};
use rumoca_phase_codegen::{render_solve_template_with_name, templates};

pub(super) fn cached_cmm_root() -> Option<PathBuf> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../target/cmm/CMM-v0.0.2");
    root.join("LieGroup/package.mo").is_file().then_some(root)
}

fn max_scalar_row_ops(block: &ComputeBlock) -> usize {
    block
        .nodes
        .iter()
        .filter_map(|node| match node {
            ComputeNode::ScalarPrograms(rows) => rows.programs.iter().map(Vec::len).max(),
            ComputeNode::MatMul { .. }
            | ComputeNode::LinSolve { .. }
            | ComputeNode::Map { .. }
            | ComputeNode::AffineStencil { .. } => None,
        })
        .max()
        .unwrap_or(0)
}

fn affine_stencil_count(block: &ComputeBlock) -> usize {
    block
        .nodes
        .iter()
        .filter(|node| matches!(node, ComputeNode::AffineStencil { .. }))
        .count()
}

fn map_count(block: &ComputeBlock) -> usize {
    block
        .nodes
        .iter()
        .filter(|node| matches!(node, ComputeNode::Map { .. }))
        .count()
}

fn native_tensor_count(block: &ComputeBlock) -> usize {
    map_count(block) + affine_stencil_count(block)
}

fn native_family_kind_count(families: &[WgslSolveNativeFamilyManifest], kind: &str) -> usize {
    families.iter().filter(|family| family.kind == kind).count()
}

fn render_wgsl_solve_layout_source(problem: &SolveProblem, model_name: &str) -> String {
    let artifacts = rumoca_ir_solve::SolveArtifacts::default();
    let layout_template =
        templates::builtin_template_source("wgsl-solve", "model_layout.json.jinja")
            .expect("wgsl-solve layout template should exist");
    render_solve_template_with_name(problem, &artifacts, layout_template, model_name)
        .unwrap_or_else(|error| panic!("{model_name} wgsl-solve layout should render: {error}"))
}

fn assert_wgsl_solve_layout_budget(model_name: &str, layout_source: &str, max_bytes: usize) {
    assert!(
        layout_source.len() <= max_bytes,
        "{model_name} wgsl-solve layout manifest should stay <= {max_bytes} bytes, got {}",
        layout_source.len()
    );
    let layout: serde_json::Value = serde_json::from_str(layout_source)
        .unwrap_or_else(|error| panic!("{model_name} layout should be valid JSON: {error}"));
    assert!(
        layout.get("bindings").is_none(),
        "{model_name} wgsl-solve dispatch layout should not carry per-scalar bindings"
    );
    assert!(
        layout.get("kernel_prefix").is_none(),
        "{model_name} wgsl-solve dispatch layout should not expose stale kernel_prefix metadata"
    );
}

fn native_tensor_rows(block: &ComputeBlock) -> usize {
    block
        .nodes
        .iter()
        .map(|node| match node {
            ComputeNode::Map { domain, .. } | ComputeNode::AffineStencil { domain, .. } => domain
                .scalar_count()
                .expect("example tensor domain should have a valid scalar count"),
            ComputeNode::ScalarPrograms(_)
            | ComputeNode::MatMul { .. }
            | ComputeNode::LinSolve { .. } => 0,
        })
        .sum()
}

fn scalar_fallback_rows(block: &ComputeBlock) -> usize {
    block
        .len()
        .expect("smoke-test Solve compute block should report output length")
        - native_tensor_rows(block)
}

fn scalar_fallback_output_indices(block: &ComputeBlock) -> Vec<usize> {
    block
        .nodes
        .iter()
        .flat_map(|node| match node {
            ComputeNode::ScalarPrograms(rows) => rows.output_indices.clone(),
            ComputeNode::MatMul { .. }
            | ComputeNode::LinSolve { .. }
            | ComputeNode::Map { .. }
            | ComputeNode::AffineStencil { .. } => Vec::new(),
        })
        .collect()
}

fn extract_doc_model(source: &str, model_name: &str) -> String {
    let start_marker = format!("model {model_name}");
    let start = source
        .find(&start_marker)
        .unwrap_or_else(|| panic!("docs should contain `{start_marker}`"));
    let end_marker = format!("end {model_name};");
    let end = source[start..]
        .find(&end_marker)
        .map(|offset| start + offset + end_marker.len())
        .unwrap_or_else(|| panic!("docs should contain `{end_marker}`"));
    source[start..end].to_string()
}

fn extract_doc_model_with_integer_parameter(
    source: &str,
    model_name: &str,
    parameter: &str,
    value: usize,
) -> String {
    let model = extract_doc_model(source, model_name);
    let needle = format!("parameter Integer {parameter} = ");
    let start = model
        .find(&needle)
        .unwrap_or_else(|| panic!("{model_name} should define integer parameter `{parameter}`"));
    let value_start = start + needle.len();
    let value_end = model[value_start..]
        .find(|ch: char| !ch.is_ascii_digit())
        .map(|offset| value_start + offset)
        .unwrap_or_else(|| panic!("{model_name}.{parameter} value should be terminated"));
    let mut edited = model;
    edited.replace_range(value_start..value_end, &value.to_string());
    edited
}

#[derive(serde::Deserialize)]
struct WgslSolveLayoutManifest {
    rows: usize,
    entry_prefixes: WgslSolveEntryPrefixesManifest,
    workgroup_size: usize,
    chunk_size: usize,
    chunks: usize,
    tensor_nodes: usize,
    tensor_families: usize,
    map_families: usize,
    stencil_families: usize,
    scalar_fallback_rows: usize,
    kernel_count: usize,
    workgroup_total: usize,
    kernels: Vec<WgslSolveKernelManifest>,
    native_families: Vec<WgslSolveNativeFamilyManifest>,
    implicit_rhs: WgslSolveImplicitManifest,
}

#[derive(serde::Deserialize)]
struct WgslSolveImplicitManifest {
    rows: usize,
    entry_prefixes: WgslSolveEntryPrefixesManifest,
    workgroup_size: usize,
    chunk_size: usize,
    tensor_nodes: usize,
    tensor_families: usize,
    map_families: usize,
    stencil_families: usize,
    scalar_fallback_rows: usize,
    chunks: usize,
    kernel_count: usize,
    workgroup_total: usize,
    kernels: Vec<WgslSolveKernelManifest>,
    native_families: Vec<WgslSolveNativeFamilyManifest>,
}

#[derive(Clone, Debug, PartialEq, Eq, serde::Deserialize)]
struct WgslSolveEntryPrefixesManifest {
    native: Vec<String>,
    scalar: String,
}

#[derive(serde::Deserialize)]
struct WgslSolveKernelManifest {
    entry: String,
    rows: usize,
    workgroup_size: usize,
    workgroups: usize,
    output_map: Option<WgslSolveOutputMap>,
    start_slot: Option<usize>,
    output_indices: Option<Vec<usize>>,
}

#[derive(serde::Deserialize)]
struct WgslSolveNativeFamilyManifest {
    kind: String,
    rows: usize,
    output_map: WgslSolveOutputMap,
    domain_shape: Vec<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq, serde::Deserialize)]
struct WgslSolveOutputMap {
    start: usize,
    strides: Vec<WgslSolveOutputStride>,
}

#[derive(Clone, Debug, PartialEq, Eq, serde::Deserialize)]
struct WgslSolveOutputStride {
    dimension: usize,
    stride: isize,
}

struct Wave2DWgslMetrics {
    rows: usize,
    derivative_maps: usize,
    derivative_stencils: usize,
    derivative_scalar_fallback_rows: usize,
    derivative_kernel_count: usize,
    implicit_maps: usize,
    implicit_stencils: usize,
    implicit_scalar_fallback_rows: usize,
    implicit_kernel_count: usize,
    layout_bytes: usize,
    wgsl_bytes: usize,
}

fn wave2d_wgsl_metrics(n: usize) -> Wave2DWgslMetrics {
    let docs = include_str!("../../../../docs/user-guide/src/language/arrays-pde.md");
    let source = extract_doc_model_with_integer_parameter(docs, "Wave2D", "N", n);
    let result = Compiler::new()
        .model("Wave2D")
        .compile_str(&source, "Wave2D.mo")
        .unwrap_or_else(|error| panic!("Wave2D N={n} docs model should compile: {error}"));
    let problem = rumoca_phase_solve::lower_solve_problem(&result.dae)
        .unwrap_or_else(|error| panic!("Wave2D N={n} should lower to Solve IR: {error}"));
    let layout_source = render_wgsl_solve_layout_source(&problem, "Wave2D");
    let layout: WgslSolveLayoutManifest = serde_json::from_str(&layout_source)
        .unwrap_or_else(|error| panic!("Wave2D N={n} layout should be valid JSON: {error}"));
    let wgsl_template = templates::builtin_template_source("wgsl-solve", "model_solve.wgsl.jinja")
        .expect("wgsl-solve WGSL template should exist");
    let artifacts = rumoca_ir_solve::SolveArtifacts::default();
    let wgsl = render_solve_template_with_name(&problem, &artifacts, wgsl_template, "Wave2D")
        .unwrap_or_else(|error| panic!("Wave2D N={n} wgsl-solve source should render: {error}"));

    Wave2DWgslMetrics {
        rows: layout.rows,
        derivative_maps: layout.map_families,
        derivative_stencils: layout.stencil_families,
        derivative_scalar_fallback_rows: layout.scalar_fallback_rows,
        derivative_kernel_count: layout.kernel_count,
        implicit_maps: layout.implicit_rhs.map_families,
        implicit_stencils: layout.implicit_rhs.stencil_families,
        implicit_scalar_fallback_rows: layout.implicit_rhs.scalar_fallback_rows,
        implicit_kernel_count: layout.implicit_rhs.kernel_count,
        layout_bytes: layout_source.len(),
        wgsl_bytes: wgsl.len(),
    }
}

fn assert_native_kernel_manifest_offsets(
    kernels: &[WgslSolveKernelManifest],
    families: &[WgslSolveNativeFamilyManifest],
    entry_prefix: &str,
    block_name: &str,
) {
    let native_kernels: Vec<_> = kernels
        .iter()
        .filter(|kernel| native_wgsl_kernel(&kernel.entry, entry_prefix))
        .collect();
    assert_eq!(
        native_kernels.len(),
        families.len(),
        "{block_name} native kernel count should match native family metadata"
    );
    for (index, (kernel, family)) in native_kernels.iter().zip(families).enumerate() {
        assert_eq!(
            kernel.entry,
            format!("{entry_prefix}_{}{}", family.kind, index),
            "{block_name} native kernel entry should match family order"
        );
        assert_eq!(
            kernel.rows, family.rows,
            "{block_name} native kernel rows should match family rows"
        );
        assert_eq!(
            kernel.output_map.as_ref(),
            Some(&family.output_map),
            "{block_name} native kernel output_map should match family metadata"
        );
        assert_eq!(
            kernel.start_slot, None,
            "{block_name} native kernels should not expose scalar chunk start_slot"
        );
        assert_eq!(
            kernel.output_indices, None,
            "{block_name} native kernels should not expose scalar chunk output_indices"
        );
    }
}

fn assert_wgsl_kernel_entry_kinds(
    kernels: &[WgslSolveKernelManifest],
    entry_prefix: &str,
    block_name: &str,
) {
    for kernel in kernels {
        let native_entry = native_wgsl_kernel(&kernel.entry, entry_prefix);
        let scalar_entry = kernel.entry.starts_with(&format!("{entry_prefix}_chunk"));
        assert!(
            native_entry || scalar_entry,
            "{block_name} kernel entry `{}` should be a native map/stencil or scalar chunk entry",
            kernel.entry
        );
        if native_entry {
            assert!(
                kernel.output_map.is_some(),
                "{block_name} native kernel `{}` should expose tensor output_map metadata",
                kernel.entry
            );
            assert_eq!(
                kernel.start_slot, None,
                "{block_name} native kernel `{}` should not expose scalar start_slot metadata",
                kernel.entry
            );
            assert_eq!(
                kernel.output_indices, None,
                "{block_name} native kernel `{}` should not expose scalar output_indices metadata",
                kernel.entry
            );
        } else {
            assert_eq!(
                kernel.output_map, None,
                "{block_name} scalar chunk `{}` should not expose tensor output_map metadata",
                kernel.entry
            );
            assert!(
                kernel.start_slot.is_some(),
                "{block_name} scalar chunk `{}` should expose scalar start_slot metadata",
                kernel.entry
            );
            assert!(
                kernel.output_indices.is_some(),
                "{block_name} scalar chunk `{}` should expose scalar output_indices metadata",
                kernel.entry
            );
        }
    }
}

fn assert_wgsl_entry_prefixes(
    prefixes: &WgslSolveEntryPrefixesManifest,
    native: &[&str],
    scalar: &str,
    block_name: &str,
) {
    assert_eq!(
        prefixes.native,
        native
            .iter()
            .map(|prefix| (*prefix).to_string())
            .collect::<Vec<_>>(),
        "{block_name} native entry prefixes should match generated WGSL entry families"
    );
    assert_eq!(
        prefixes.scalar, scalar,
        "{block_name} scalar entry prefix should match generated WGSL scalar chunks"
    );
}

fn assert_scalar_chunk_manifest_offsets(
    kernels: &[WgslSolveKernelManifest],
    expected_output_indices: &[usize],
    entry_prefix: &str,
    block_name: &str,
) {
    let mut expected_start = 0;
    for kernel in kernels
        .iter()
        .filter(|kernel| kernel.entry.starts_with(&format!("{entry_prefix}_chunk")))
    {
        assert_eq!(
            kernel.output_map, None,
            "{block_name} scalar chunks should not expose tensor output_map"
        );
        assert_eq!(
            kernel.start_slot,
            Some(expected_start),
            "{block_name} scalar chunk start_slot should cover scalar view order"
        );
        assert_eq!(
            kernel.output_indices.as_deref(),
            Some(&expected_output_indices[expected_start..expected_start + kernel.rows]),
            "{block_name} scalar chunk output_indices should expose exact output slots"
        );
        expected_start += kernel.rows;
    }
    assert_eq!(
        expected_start,
        expected_output_indices.len(),
        "{block_name} scalar chunks should cover every fallback output index"
    );
}

// SPEC_0021: Exception - this assertion checks native family output-map
// coverage, stride arithmetic, and duplicate slot detection together.
#[allow(clippy::excessive_nesting)]
fn assert_native_family_output_coverage(
    families: &[WgslSolveNativeFamilyManifest],
    rows: usize,
    block_name: &str,
) {
    let mut covered = vec![None; rows];
    let row_limit = isize::try_from(rows)
        .unwrap_or_else(|_| panic!("{block_name} row count {rows} should fit isize"));
    for (family_index, family) in families.iter().enumerate() {
        let rank = family.domain_shape.len();
        assert!(
            rank > 0,
            "{block_name} family {family_index} should have a rank"
        );
        assert!(
            family.domain_shape.iter().all(|extent| *extent > 0),
            "{block_name} family {family_index} domain_shape should have nonzero extents"
        );
        assert_eq!(
            family.domain_shape.iter().product::<usize>(),
            family.rows,
            "{block_name} family {family_index} domain_shape should match rows"
        );

        let mut strides = vec![0; rank];
        let mut seen = vec![false; rank];
        for (term_index, term) in family.output_map.strides.iter().enumerate() {
            assert!(
                term.dimension < rank,
                "{block_name} family {family_index} output_map.strides[{term_index}] should target \
                 a domain dimension"
            );
            assert!(
                !seen[term.dimension],
                "{block_name} family {family_index} output_map.strides[{term_index}] should not \
                 duplicate a domain dimension"
            );
            seen[term.dimension] = true;
            strides[term.dimension] = term.stride;
        }

        for row in 0..family.rows {
            let mut remainder = row;
            let mut slot = isize::try_from(family.output_map.start).unwrap_or_else(|_| {
                panic!("{block_name} family {family_index} output_map.start should fit isize")
            });
            for dim in (0..rank).rev() {
                let index = remainder % family.domain_shape[dim];
                remainder /= family.domain_shape[dim];
                let index = isize::try_from(index).unwrap_or_else(|_| {
                    panic!("{block_name} family {family_index} domain index should fit isize")
                });
                let offset = index.checked_mul(strides[dim]).unwrap_or_else(|| {
                    panic!("{block_name} family {family_index} output stride should not overflow")
                });
                slot = slot.checked_add(offset).unwrap_or_else(|| {
                    panic!("{block_name} family {family_index} output slot should not overflow")
                });
            }
            assert!(
                (0..row_limit).contains(&slot),
                "{block_name} family {family_index} writes output slot {slot} outside {rows} rows"
            );
            let slot = usize::try_from(slot).unwrap_or_else(|_| {
                panic!("{block_name} family {family_index} output slot should be non-negative")
            });
            assert_eq!(
                covered[slot], None,
                "{block_name} family {family_index} overlaps output slot {slot}"
            );
            covered[slot] = Some(family_index);
        }
    }

    if let Some(gap) = covered.iter().position(Option::is_none) {
        panic!("{block_name} native families do not cover output slot {gap}");
    }
}

fn native_wgsl_kernel(entry: &str, entry_prefix: &str) -> bool {
    entry.starts_with(&format!("{entry_prefix}_map"))
        || entry.starts_with(&format!("{entry_prefix}_stencil"))
}

#[test]
fn quadrotor_acro_solve_preserves_tensor_structure_when_cmm_available() {
    let Some(result) = compile_quadrotor_acro_if_cmm_available() else {
        eprintln!(
            "skipping QuadrotorAcro tensor regression: requires cached CMM at \
             target/cmm/CMM-v0.0.2; run `rum repo cmm ensure`"
        );
        return;
    };

    let problem = rumoca_phase_solve::lower_solve_problem(&result.dae)
        .expect("QuadrotorAcro should lower to Solve IR");
    let implicit_rhs = &problem.continuous.implicit_rhs;
    let linsolve_nodes = implicit_rhs.compute_node_counts().linsolve;

    assert!(
        linsolve_nodes > 0,
        "QuadrotorAcro should preserve rigid-body solves as LinSolve nodes"
    );
    let max_scalar_ops = max_scalar_row_ops(implicit_rhs);
    assert!(
        max_scalar_ops < 9_000,
        "QuadrotorAcro Solve IR scalar fallback rows should stay below the \
         pre-tensor explosion threshold; got {max_scalar_ops}"
    );
}

#[test]
// SPEC_0021: Exception - PDE docs regression validates multiple documented
// models against DAE structure, native tensors, and layout budgets.
#[allow(clippy::too_many_lines)]
fn pde_docs_examples_expose_structured_dae_and_native_stencils_when_supported() {
    const MAX_TURKEY_LAYOUT_BYTES: usize = 5 * 1024;
    const MAX_AIRFOIL_LAYOUT_BYTES: usize = 18 * 1024;

    let docs = include_str!("../../../../docs/user-guide/src/language/arrays-pde.md");
    for (model_name, min_stencils) in [("Turkey", 1), ("Wave2D", 1), ("AirfoilFlow", 6)] {
        let source = extract_doc_model(docs, model_name);
        let result = Compiler::new()
            .model(model_name)
            .compile_str(&source, &format!("{model_name}.mo"))
            .unwrap_or_else(|error| panic!("{model_name} docs model should compile: {error}"));
        let dae_json = result
            .to_json()
            .unwrap_or_else(|error| panic!("{model_name} DAE JSON should render: {error}"));
        assert!(
            dae_json.contains("\"structured_equations\""),
            "{model_name} DAE JSON should expose structured source equation families"
        );
        assert!(
            !dae_json.contains("\"for_equations\""),
            "{model_name} DAE JSON should not expose obsolete for_equations key"
        );
        let problem = rumoca_phase_solve::lower_solve_problem(&result.dae)
            .unwrap_or_else(|error| panic!("{model_name} should lower to Solve IR: {error}"));
        let derivative_rhs = &problem.continuous.derivative_rhs;
        let count = affine_stencil_count(derivative_rhs);
        if min_stencils > 0 {
            assert!(
                count >= min_stencils,
                "{model_name} should emit at least {min_stencils} AffineStencil nodes, got {count}"
            );
        }
        if model_name == "AirfoilFlow" {
            assert_eq!(
                map_count(derivative_rhs),
                7,
                "AirfoilFlow derivative RHS should preserve current pointwise tensor families"
            );
            assert_eq!(
                affine_stencil_count(derivative_rhs),
                8,
                "AirfoilFlow derivative RHS should preserve current native stencil families"
            );
            assert_eq!(
                scalar_fallback_rows(derivative_rhs),
                0,
                "AirfoilFlow derivative RHS should not scalarize structured boundary or interior rows"
            );
        }
        if model_name == "Turkey" {
            assert_eq!(
                affine_stencil_count(&problem.continuous.residual),
                5,
                "Turkey continuous residual should preserve the structured geometry and flux families"
            );
            assert_eq!(
                scalar_fallback_rows(&problem.continuous.residual),
                3,
                "Turkey continuous residual scalar fallback rows should stay limited to boundary/surface rows"
            );
            assert_eq!(
                scalar_fallback_output_indices(&problem.continuous.residual),
                vec![42, 52, 53],
                "Turkey residual scalar fallback rows should be only the source scalar center, outer-boundary, and surface equations"
            );
            assert_eq!(
                affine_stencil_count(&problem.continuous.implicit_rhs),
                6,
                "Turkey implicit RHS should carry remapped residual tensor families"
            );
            assert_eq!(
                scalar_fallback_rows(&problem.continuous.implicit_rhs),
                5,
                "Turkey implicit RHS scalar fallback rows should stay limited to derivative and boundary rows"
            );
            assert_eq!(
                scalar_fallback_output_indices(&problem.continuous.implicit_rhs),
                vec![0, 9, 52, 62, 63],
                "Turkey implicit RHS scalar fallback rows should be only the derivative and source scalar boundary/surface equations"
            );
            let layout_source = render_wgsl_solve_layout_source(&problem, "Turkey");
            assert_wgsl_solve_layout_budget("Turkey", &layout_source, MAX_TURKEY_LAYOUT_BYTES);
            let layout: WgslSolveLayoutManifest =
                serde_json::from_str(&layout_source).expect("Turkey layout should be valid JSON");
            assert_eq!(
                layout.map_families,
                map_count(derivative_rhs),
                "Turkey layout derivative Map inventory should match Solve IR"
            );
            assert_eq!(
                layout.stencil_families,
                affine_stencil_count(derivative_rhs),
                "Turkey layout derivative stencil inventory should match Solve IR"
            );
            assert_eq!(
                layout.scalar_fallback_rows,
                scalar_fallback_rows(derivative_rhs),
                "Turkey layout derivative scalar fallback rows should match Solve IR"
            );
            assert_eq!(
                layout.implicit_rhs.map_families,
                map_count(&problem.continuous.implicit_rhs),
                "Turkey layout implicit Map inventory should match Solve IR"
            );
            assert_eq!(
                layout.implicit_rhs.stencil_families, 6,
                "Turkey layout implicit RHS should preserve remapped residual stencil families"
            );
            assert_eq!(
                layout.implicit_rhs.scalar_fallback_rows, 5,
                "Turkey layout implicit RHS scalar fallback rows should stay limited to derivative and boundary rows"
            );
            assert_eq!(
                layout.implicit_rhs.kernel_count,
                layout.implicit_rhs.stencil_families + layout.implicit_rhs.chunks,
                "Turkey layout implicit kernel count should include native stencils and scalar chunks"
            );
        }
        if model_name == "Wave2D" {
            assert_eq!(
                scalar_fallback_rows(derivative_rhs),
                0,
                "Wave2D derivative RHS should not depend on scalar row-family recovery"
            );
            assert_eq!(
                scalar_fallback_rows(&problem.continuous.implicit_rhs),
                0,
                "Wave2D implicit RHS should not depend on scalar row-family recovery"
            );
            assert!(
                scalar_fallback_output_indices(derivative_rhs).is_empty(),
                "Wave2D derivative RHS should have no scalar fallback output slots"
            );
            assert!(
                scalar_fallback_output_indices(&problem.continuous.implicit_rhs).is_empty(),
                "Wave2D implicit RHS should have no scalar fallback output slots"
            );
        }
        if model_name == "AirfoilFlow" {
            assert_eq!(
                map_count(&problem.continuous.residual),
                0,
                "AirfoilFlow continuous residual should not need pointwise native tensor families"
            );
            assert_eq!(
                affine_stencil_count(&problem.continuous.residual),
                3,
                "AirfoilFlow continuous residual should preserve the structured geometry and mask families"
            );
            assert_eq!(
                scalar_fallback_rows(&problem.continuous.residual),
                0,
                "AirfoilFlow continuous residual should not scalarize structured geometry or mask rows"
            );
            assert_eq!(
                map_count(&problem.continuous.implicit_rhs),
                7,
                "AirfoilFlow implicit RHS should preserve remapped derivative Map families"
            );
            assert_eq!(
                affine_stencil_count(&problem.continuous.implicit_rhs),
                11,
                "AirfoilFlow implicit RHS should carry remapped residual tensor families"
            );
            assert_eq!(
                scalar_fallback_rows(&problem.continuous.implicit_rhs),
                0,
                "AirfoilFlow implicit RHS should not re-expand derivative or residual tensor families"
            );
            let layout_source = render_wgsl_solve_layout_source(&problem, "AirfoilFlow");
            assert_wgsl_solve_layout_budget(
                "AirfoilFlow",
                &layout_source,
                MAX_AIRFOIL_LAYOUT_BYTES,
            );
            let layout: WgslSolveLayoutManifest = serde_json::from_str(&layout_source)
                .expect("AirfoilFlow layout should be valid JSON");
            assert_eq!(
                layout.map_families, 7,
                "AirfoilFlow layout should preserve derivative Map family inventory"
            );
            assert_eq!(
                layout.stencil_families, 8,
                "AirfoilFlow layout should preserve derivative native stencil inventory"
            );
            assert_eq!(
                layout.scalar_fallback_rows, 0,
                "AirfoilFlow layout should expose zero derivative scalar fallback rows"
            );
            assert_eq!(
                layout.implicit_rhs.map_families, 7,
                "AirfoilFlow layout should preserve implicit Map family inventory"
            );
            assert_eq!(
                layout.implicit_rhs.stencil_families, 11,
                "AirfoilFlow layout should preserve implicit native stencil inventory"
            );
            assert_eq!(
                layout.implicit_rhs.scalar_fallback_rows, 0,
                "AirfoilFlow layout should expose zero implicit scalar fallback rows"
            );
        }
    }
}

#[test]
// SPEC_0021: Exception - Wave2D N=50 budget regression keeps compile, solve,
// layout, WGSL, and kernel inventory checks in one benchmark-shaped test.
#[allow(clippy::too_many_lines)]
fn wave2d_n50_wgsl_solve_stays_within_codegen_budget() {
    const EXPECTED_ROWS: usize = 5_000;
    const EXPECTED_DERIVATIVE_MAPS: usize = 5;
    const EXPECTED_DERIVATIVE_STENCILS: usize = 1;
    const EXPECTED_IMPLICIT_MAPS: usize = 5;
    const EXPECTED_IMPLICIT_STENCILS: usize = 1;
    const MAX_SCALAR_FALLBACK_ROWS: usize = 0;
    const MAX_IMPLICIT_SCALAR_FALLBACK_ROWS: usize = 0;
    const MAX_LAYOUT_BYTES: usize = 8 * 1024;
    const MAX_WGSL_BYTES: usize = 16 * 1024;
    const EXPECTED_KERNELS: usize = EXPECTED_DERIVATIVE_MAPS + EXPECTED_DERIVATIVE_STENCILS;
    const EXPECTED_IMPLICIT_KERNELS: usize = EXPECTED_IMPLICIT_MAPS + EXPECTED_IMPLICIT_STENCILS;

    let docs = include_str!("../../../../docs/user-guide/src/language/arrays-pde.md");
    let source = extract_doc_model_with_integer_parameter(docs, "Wave2D", "N", 50);
    let result = Compiler::new()
        .model("Wave2D")
        .compile_str(&source, "Wave2D.mo")
        .expect("Wave2D N=50 docs model should compile");
    let problem = rumoca_phase_solve::lower_solve_problem(&result.dae)
        .expect("Wave2D N=50 should lower to Solve IR");
    let derivative_rhs = &problem.continuous.derivative_rhs;
    let derivative_scalar_fallback_rows = scalar_fallback_rows(derivative_rhs);
    let native_count = native_tensor_count(derivative_rhs);
    let derivative_map_count = map_count(derivative_rhs);
    let stencil_count = affine_stencil_count(derivative_rhs);
    let derivative_rhs_len = derivative_rhs
        .len()
        .expect("Wave2D derivative RHS should report output length");
    let implicit_rhs = &problem.continuous.implicit_rhs;
    let implicit_scalar_fallback_rows = scalar_fallback_rows(implicit_rhs);
    let implicit_native_count = native_tensor_count(implicit_rhs);
    let implicit_map_count = map_count(implicit_rhs);
    let implicit_stencil_count = affine_stencil_count(implicit_rhs);
    let implicit_rhs_len = implicit_rhs
        .len()
        .expect("Wave2D implicit RHS should report output length");

    assert_eq!(derivative_rhs_len, EXPECTED_ROWS);
    assert_eq!(
        derivative_map_count, EXPECTED_DERIVATIVE_MAPS,
        "Wave2D N=50 should preserve the current pointwise derivative family inventory"
    );
    assert_eq!(
        stencil_count, EXPECTED_DERIVATIVE_STENCILS,
        "Wave2D N=50 should preserve the current derivative stencil inventory"
    );
    assert_eq!(
        implicit_map_count, EXPECTED_IMPLICIT_MAPS,
        "Wave2D N=50 implicit RHS should preserve remapped pointwise families"
    );
    assert_eq!(
        implicit_stencil_count, EXPECTED_IMPLICIT_STENCILS,
        "Wave2D N=50 implicit RHS should preserve remapped stencil families"
    );
    assert_eq!(
        derivative_scalar_fallback_rows, MAX_SCALAR_FALLBACK_ROWS,
        "Wave2D N=50 scalar fallback rows should stay at {MAX_SCALAR_FALLBACK_ROWS}, \
         got {derivative_scalar_fallback_rows} across {native_count} native tensor nodes"
    );
    assert_eq!(
        implicit_scalar_fallback_rows, MAX_IMPLICIT_SCALAR_FALLBACK_ROWS,
        "Wave2D N=50 implicit scalar fallback rows should stay at \
         {MAX_IMPLICIT_SCALAR_FALLBACK_ROWS}, got {implicit_scalar_fallback_rows} \
         across {implicit_native_count} native tensor nodes"
    );

    let layout_source = render_wgsl_solve_layout_source(&problem, "Wave2D");
    let layout: WgslSolveLayoutManifest =
        serde_json::from_str(&layout_source).expect("Wave2D N=50 layout should be valid JSON");
    let native_kernel_count = layout
        .kernels
        .iter()
        .filter(|kernel| {
            kernel.entry.starts_with("derivative_rhs_map")
                || kernel.entry.starts_with("derivative_rhs_stencil")
        })
        .count();
    let scalar_chunk_rows: usize = layout
        .kernels
        .iter()
        .filter(|kernel| kernel.entry.starts_with("derivative_rhs_chunk"))
        .map(|kernel| kernel.rows)
        .sum();
    let scalar_chunk_count = layout
        .kernels
        .iter()
        .filter(|kernel| kernel.entry.starts_with("derivative_rhs_chunk"))
        .count();
    let derivative_workgroup_sum: usize =
        layout.kernels.iter().map(|kernel| kernel.workgroups).sum();
    let implicit_native_kernel_count = layout
        .implicit_rhs
        .kernels
        .iter()
        .filter(|kernel| {
            kernel.entry.starts_with("implicit_rhs_map")
                || kernel.entry.starts_with("implicit_rhs_stencil")
        })
        .count();
    let implicit_scalar_chunk_rows: usize = layout
        .implicit_rhs
        .kernels
        .iter()
        .filter(|kernel| kernel.entry.starts_with("implicit_rhs_chunk"))
        .map(|kernel| kernel.rows)
        .sum();
    let implicit_scalar_chunk_count = layout
        .implicit_rhs
        .kernels
        .iter()
        .filter(|kernel| kernel.entry.starts_with("implicit_rhs_chunk"))
        .count();
    let implicit_workgroup_sum: usize = layout
        .implicit_rhs
        .kernels
        .iter()
        .map(|kernel| kernel.workgroups)
        .sum();
    let native_family_rows: usize = layout
        .native_families
        .iter()
        .map(|family| family.rows)
        .sum();
    let implicit_native_family_rows: usize = layout
        .implicit_rhs
        .native_families
        .iter()
        .map(|family| family.rows)
        .sum();

    assert_eq!(layout.rows, EXPECTED_ROWS);
    assert_wgsl_entry_prefixes(
        &layout.entry_prefixes,
        &["derivative_rhs_map", "derivative_rhs_stencil"],
        "derivative_rhs_chunk",
        "layout derivative RHS",
    );
    assert_eq!(
        layout.tensor_nodes, native_count,
        "layout derivative tensor node count should match Solve IR native nodes"
    );
    assert_eq!(
        layout.tensor_families, native_count,
        "layout derivative tensor family count should match native families"
    );
    assert_eq!(
        layout.map_families, EXPECTED_DERIVATIVE_MAPS,
        "layout derivative Map count should match the current Solve IR inventory"
    );
    assert_eq!(
        layout.stencil_families, EXPECTED_DERIVATIVE_STENCILS,
        "layout derivative stencil count should match the current Solve IR inventory"
    );
    assert_eq!(
        layout.scalar_fallback_rows, derivative_scalar_fallback_rows,
        "layout derivative scalar fallback rows should match Solve IR scalar fallback rows"
    );
    assert_eq!(layout.implicit_rhs.rows, implicit_rhs_len);
    assert_wgsl_entry_prefixes(
        &layout.implicit_rhs.entry_prefixes,
        &["implicit_rhs_map", "implicit_rhs_stencil"],
        "implicit_rhs_chunk",
        "layout implicit RHS",
    );
    assert_eq!(
        layout.chunk_size, layout.workgroup_size,
        "layout scalar chunk size should follow top-level workgroup size"
    );
    assert_eq!(
        layout.implicit_rhs.chunk_size, layout.implicit_rhs.workgroup_size,
        "layout implicit scalar chunk size should follow implicit workgroup size"
    );
    assert_eq!(
        layout.implicit_rhs.workgroup_size, layout.workgroup_size,
        "layout implicit workgroup size should match derivative workgroup size"
    );
    assert_eq!(
        layout.chunks, scalar_chunk_count,
        "layout chunks should match derivative scalar chunk entries"
    );
    assert_eq!(
        native_kernel_count, native_count,
        "layout native kernels should match Solve IR Map/AffineStencil nodes"
    );
    assert_eq!(
        layout.native_families.len(),
        native_count,
        "layout native families should match Solve IR Map/AffineStencil nodes"
    );
    assert_eq!(
        native_family_rows, EXPECTED_ROWS,
        "layout native families should cover derivative RHS rows"
    );
    assert_native_family_output_coverage(
        &layout.native_families,
        layout.rows,
        "layout derivative native families",
    );
    assert!(
        layout.native_families.iter().all(|family| {
            matches!(family.kind.as_str(), "map" | "stencil")
                && family.output_map.start < EXPECTED_ROWS
                && family.domain_shape.iter().product::<usize>() == family.rows
        }),
        "layout native families should carry derivative kind, output-offset, and domain-shape metadata"
    );
    assert_eq!(
        native_family_kind_count(&layout.native_families, "map"),
        EXPECTED_DERIVATIVE_MAPS,
        "layout derivative native families should report the current Map inventory"
    );
    assert_eq!(
        native_family_kind_count(&layout.native_families, "stencil"),
        EXPECTED_DERIVATIVE_STENCILS,
        "layout derivative native families should report the current stencil inventory"
    );
    assert_eq!(
        scalar_chunk_rows, derivative_scalar_fallback_rows,
        "layout scalar chunk rows should match Solve IR scalar fallback rows"
    );
    assert_eq!(
        layout.implicit_rhs.scalar_fallback_rows, implicit_scalar_fallback_rows,
        "layout implicit scalar fallback rows should match Solve IR scalar fallback rows"
    );
    assert_eq!(
        layout.implicit_rhs.tensor_nodes, implicit_native_count,
        "layout implicit tensor node count should match Solve IR native nodes"
    );
    assert_eq!(
        layout.implicit_rhs.tensor_families, implicit_native_count,
        "layout implicit tensor family count should match native families"
    );
    assert_eq!(
        layout.implicit_rhs.map_families, EXPECTED_IMPLICIT_MAPS,
        "layout implicit Map count should match the current Solve IR inventory"
    );
    assert_eq!(
        layout.implicit_rhs.stencil_families, EXPECTED_IMPLICIT_STENCILS,
        "layout implicit stencil count should match the current Solve IR inventory"
    );
    assert_eq!(
        implicit_native_kernel_count, implicit_native_count,
        "layout implicit native kernels should match Solve IR Map/AffineStencil nodes"
    );
    assert_eq!(
        layout.implicit_rhs.native_families.len(),
        implicit_native_count,
        "layout implicit native families should match Solve IR Map/AffineStencil nodes"
    );
    assert_eq!(
        implicit_native_family_rows, implicit_rhs_len,
        "layout implicit native families should cover implicit RHS rows"
    );
    assert_native_family_output_coverage(
        &layout.implicit_rhs.native_families,
        layout.implicit_rhs.rows,
        "layout implicit native families",
    );
    assert!(
        layout.implicit_rhs.native_families.iter().all(|family| {
            matches!(family.kind.as_str(), "map" | "stencil")
                && family.output_map.start < implicit_rhs_len
                && family.domain_shape.iter().product::<usize>() == family.rows
        }),
        "layout implicit native families should carry kind, output-offset, and domain-shape metadata"
    );
    assert_eq!(
        native_family_kind_count(&layout.implicit_rhs.native_families, "map"),
        EXPECTED_IMPLICIT_MAPS,
        "layout implicit native families should report the current Map inventory"
    );
    assert_eq!(
        native_family_kind_count(&layout.implicit_rhs.native_families, "stencil"),
        EXPECTED_IMPLICIT_STENCILS,
        "layout implicit native families should report the current stencil inventory"
    );
    assert_eq!(
        implicit_scalar_chunk_rows, implicit_scalar_fallback_rows,
        "layout implicit scalar chunk rows should match Solve IR scalar fallback rows"
    );
    assert_eq!(
        layout.implicit_rhs.chunks, implicit_scalar_chunk_count,
        "layout implicit chunks should match implicit scalar chunk entries"
    );
    assert_eq!(
        layout.kernels.len(),
        EXPECTED_KERNELS,
        "Wave2D N=50 wgsl-solve kernel count should match the current native inventory, got {}",
        layout.kernels.len()
    );
    assert_eq!(
        layout.kernel_count, EXPECTED_KERNELS,
        "Wave2D N=50 manifest kernel_count should match the current derivative native inventory"
    );
    assert_eq!(
        layout.kernel_count,
        layout.kernels.len(),
        "Wave2D N=50 manifest kernel_count should match derivative kernel entries"
    );
    assert_eq!(
        layout.workgroup_total, derivative_workgroup_sum,
        "Wave2D N=50 manifest workgroup_total should match derivative kernel entries"
    );
    assert!(
        layout.kernels.iter().all(|kernel| kernel.workgroups > 0),
        "Wave2D N=50 derivative kernels should report positive workgroup counts"
    );
    assert_eq!(
        layout.implicit_rhs.kernels.len(),
        EXPECTED_IMPLICIT_KERNELS,
        "Wave2D N=50 implicit RHS wgsl-solve kernel count should match the current native \
         inventory, got {}",
        layout.implicit_rhs.kernels.len()
    );
    assert_eq!(
        layout.implicit_rhs.kernel_count, EXPECTED_IMPLICIT_KERNELS,
        "Wave2D N=50 manifest implicit kernel_count should match the current native inventory"
    );
    assert_eq!(
        layout.implicit_rhs.kernel_count,
        layout.implicit_rhs.kernels.len(),
        "Wave2D N=50 manifest implicit kernel_count should match implicit kernel entries"
    );
    assert_eq!(
        layout.implicit_rhs.workgroup_total, implicit_workgroup_sum,
        "Wave2D N=50 manifest implicit workgroup_total should match implicit kernel entries"
    );
    assert!(
        layout
            .implicit_rhs
            .kernels
            .iter()
            .all(|kernel| kernel.workgroups > 0),
        "Wave2D N=50 implicit kernels should report positive workgroup counts"
    );
    assert_wgsl_solve_layout_budget("Wave2D N=50", &layout_source, MAX_LAYOUT_BYTES);
    assert!(
        layout
            .kernels
            .iter()
            .all(|kernel| kernel.workgroup_size == layout.workgroup_size),
        "derivative kernel manifest should match top-level workgroup size"
    );
    assert!(
        layout
            .implicit_rhs
            .kernels
            .iter()
            .all(|kernel| kernel.workgroup_size == layout.workgroup_size),
        "implicit kernel manifest should match top-level workgroup size"
    );
    assert_wgsl_kernel_entry_kinds(&layout.kernels, "derivative_rhs", "derivative RHS");
    assert_wgsl_kernel_entry_kinds(&layout.implicit_rhs.kernels, "implicit_rhs", "implicit RHS");
    assert_native_kernel_manifest_offsets(
        &layout.kernels,
        &layout.native_families,
        "derivative_rhs",
        "derivative RHS",
    );
    assert_native_kernel_manifest_offsets(
        &layout.implicit_rhs.kernels,
        &layout.implicit_rhs.native_families,
        "implicit_rhs",
        "implicit RHS",
    );
    assert_scalar_chunk_manifest_offsets(
        &layout.kernels,
        &scalar_fallback_output_indices(derivative_rhs),
        "derivative_rhs",
        "derivative RHS",
    );
    assert_scalar_chunk_manifest_offsets(
        &layout.implicit_rhs.kernels,
        &scalar_fallback_output_indices(implicit_rhs),
        "implicit_rhs",
        "implicit RHS",
    );

    let wgsl_template = templates::builtin_template_source("wgsl-solve", "model_solve.wgsl.jinja")
        .expect("wgsl-solve WGSL template should exist");
    let artifacts = rumoca_ir_solve::SolveArtifacts::default();
    let wgsl = render_solve_template_with_name(&problem, &artifacts, wgsl_template, "Wave2D")
        .expect("Wave2D N=50 wgsl-solve source should render");
    assert!(
        wgsl.len() <= MAX_WGSL_BYTES,
        "Wave2D N=50 wgsl-solve source should stay <= {MAX_WGSL_BYTES} bytes, got {}",
        wgsl.len()
    );
    assert!(
        wgsl.contains("stencil_families="),
        "wgsl-solve source should report stencil family inventory"
    );
    assert!(
        wgsl.contains("scalar_fallback_rows="),
        "wgsl-solve source should report scalar fallback inventory"
    );
    assert!(
        wgsl.contains("scalar_chunks="),
        "wgsl-solve source should report scalar chunk inventory"
    );
    let workgroup_size_comment = format!("workgroup_size={}", layout.workgroup_size);
    assert_eq!(
        wgsl.matches(&workgroup_size_comment).count(),
        2,
        "wgsl-solve source should report derivative and implicit workgroup sizes"
    );
    let chunk_size_comment = format!("chunk_size={}", layout.chunk_size);
    assert_eq!(
        wgsl.matches(&chunk_size_comment).count(),
        2,
        "wgsl-solve source should report derivative and implicit scalar chunk sizes"
    );
    let kernel_count_comment = format!("kernel_count={EXPECTED_KERNELS}");
    assert_eq!(
        wgsl.matches(&kernel_count_comment).count(),
        2,
        "wgsl-solve source should report derivative and implicit kernel inventory"
    );
    assert!(
        !wgsl.contains("residual_rows="),
        "wgsl-solve source should use scalar fallback terminology"
    );
}

#[test]
fn wave2d_wgsl_solve_kernel_inventory_is_grid_size_stable() {
    const EXPECTED_DERIVATIVE_MAPS: usize = 5;
    const EXPECTED_DERIVATIVE_STENCILS: usize = 1;
    const EXPECTED_IMPLICIT_MAPS: usize = 5;
    const EXPECTED_IMPLICIT_STENCILS: usize = 1;
    const MAX_LAYOUT_BYTE_GROWTH: usize = 512;
    const MAX_WGSL_BYTE_GROWTH: usize = 512;

    let small = wave2d_wgsl_metrics(10);
    let large = wave2d_wgsl_metrics(50);

    assert!(
        large.rows > small.rows,
        "test should compare larger Wave2D grids: small={} large={}",
        small.rows,
        large.rows
    );
    assert_eq!(small.derivative_maps, EXPECTED_DERIVATIVE_MAPS);
    assert_eq!(large.derivative_maps, EXPECTED_DERIVATIVE_MAPS);
    assert_eq!(small.derivative_stencils, EXPECTED_DERIVATIVE_STENCILS);
    assert_eq!(large.derivative_stencils, EXPECTED_DERIVATIVE_STENCILS);
    assert_eq!(small.implicit_maps, EXPECTED_IMPLICIT_MAPS);
    assert_eq!(large.implicit_maps, EXPECTED_IMPLICIT_MAPS);
    assert_eq!(small.implicit_stencils, EXPECTED_IMPLICIT_STENCILS);
    assert_eq!(large.implicit_stencils, EXPECTED_IMPLICIT_STENCILS);
    assert_eq!(small.derivative_scalar_fallback_rows, 0);
    assert_eq!(large.derivative_scalar_fallback_rows, 0);
    assert_eq!(small.implicit_scalar_fallback_rows, 0);
    assert_eq!(large.implicit_scalar_fallback_rows, 0);
    assert_eq!(
        small.derivative_kernel_count, large.derivative_kernel_count,
        "derivative kernel count should be independent of Wave2D grid size"
    );
    assert_eq!(
        small.implicit_kernel_count, large.implicit_kernel_count,
        "implicit kernel count should be independent of Wave2D grid size"
    );
    assert!(
        large.layout_bytes <= small.layout_bytes + MAX_LAYOUT_BYTE_GROWTH,
        "layout manifest should not grow materially with Wave2D grid size: small={} large={}",
        small.layout_bytes,
        large.layout_bytes
    );
    assert!(
        large.wgsl_bytes <= small.wgsl_bytes + MAX_WGSL_BYTE_GROWTH,
        "WGSL source should not grow materially with Wave2D grid size: small={} large={}",
        small.wgsl_bytes,
        large.wgsl_bytes
    );
}
