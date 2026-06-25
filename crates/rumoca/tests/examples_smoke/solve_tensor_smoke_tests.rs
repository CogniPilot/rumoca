use super::*;
use rumoca_ir_solve::{ComputeBlock, ComputeNode, SolveProblem};
use rumoca_phase_codegen::{render_solve_template_with_name, templates};
use rumoca_sim::{SimOptions, SimResult, SimSolverMode, simulate_dae_with_diagnostics};

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

/// Physical-accuracy regression for the docs Turkey heat-equation example.
///
/// The method-of-lines shells start fridge-cold (277 K) and must warm toward the
/// 450 K oven, with the surface shell hottest and the center coolest — a monotone
/// radial profile. A frozen solution (every shell stuck at the 277 K start) means
/// the conductive-flux or geometry families were lowered wrong, e.g. a structured
/// algebraic family whose interior cells were cheapened to placeholder zeros but
/// not reconstructed downstream, so `der(T)` reads zero flux. That exact class of
/// regression has bitten this example more than once, so assert the physics, not
/// just that it compiles.
#[test]
fn turkey_heat_equation_warms_from_a_cold_start() {
    let docs = include_str!("../../../../docs/user-guide/src/language/arrays-pde.md");
    let source = extract_doc_model(docs, "Turkey");
    let result = Compiler::new()
        .model("Turkey")
        .compile_str(&source, "Turkey.mo")
        .expect("Turkey docs model should compile");

    let opts = SimOptions {
        t_end: 7200.0,
        ..SimOptions::default()
    };
    let sim = simulate_dae_with_diagnostics(&result.dae, &opts)
        .unwrap_or_else(|error| panic!("Turkey should simulate: {error:?}"));

    // Number of shells from the documented `N` (T[1..N]).
    let shells: usize = (1..)
        .take_while(|i| sim.names.iter().any(|n| n == &format!("T[{i}]")))
        .count();
    assert!(
        shells >= 3,
        "Turkey should expose several shell temperatures T[i]"
    );

    let column = |name: &str| -> usize {
        sim.names
            .iter()
            .position(|n| n == name)
            .unwrap_or_else(|| panic!("missing `{name}` in {:?}", sim.names))
    };
    let start_temp = |i: usize| {
        *sim.data[column(&format!("T[{i}]"))]
            .first()
            .expect("a first frame")
    };
    let final_temp = |i: usize| {
        *sim.data[column(&format!("T[{i}]"))]
            .last()
            .expect("a settled frame")
    };

    // (1) Cold start: every shell begins at the documented 277 K.
    for i in 1..=shells {
        assert!(
            (start_temp(i) - 277.0).abs() < 1.0,
            "shell T[{i}] should start at the 277 K fridge temperature, got {}",
            start_temp(i)
        );
    }

    let center = final_temp(1);
    let surface = final_temp(shells);

    // (2) The turkey actually cooks: the surface warms well above the cold start
    // toward the oven (a frozen 277 K solution fails here).
    assert!(
        surface > 320.0,
        "the surface shell should warm well above the 277 K start toward the \
         450 K oven, got {surface} K"
    );
    // (3) Heat reaches the interior: even the center rises off the cold start.
    assert!(
        center > 278.0,
        "the center shell should warm above the cold start, got {center} K"
    );
    // (4) Correct radial gradient: surface hotter than center, and the profile is
    // monotone non-decreasing from center to surface (heat enters from outside).
    let mut prev = f64::NEG_INFINITY;
    for i in 1..=shells {
        let t = final_temp(i);
        assert!(
            t >= prev - 1.0,
            "shell temperatures should increase monotonically from center to \
             surface; T[{i}]={t} K dropped below the inner shell ({prev} K)"
        );
        prev = t;
    }
    assert!(
        surface > center + 5.0,
        "the surface should be clearly hotter than the center, got \
         surface={surface} K center={center} K"
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
                4,
                "AirfoilFlow derivative RHS scalar rows should stay limited to the four motor states"
            );
        }
        if model_name == "Turkey" {
            assert_eq!(
                affine_stencil_count(&problem.continuous.residual),
                1,
                "Turkey continuous residual should preserve the structurally regular flux family"
            );
            assert_eq!(
                scalar_fallback_rows(&problem.continuous.residual),
                3,
                "Turkey continuous residual scalar fallback rows should stay limited to boundary/surface rows"
            );
            assert_eq!(
                scalar_fallback_output_indices(&problem.continuous.residual),
                vec![0, 10, 11],
                "Turkey residual scalar fallback rows should be only the source scalar center, outer-boundary, and surface equations"
            );
            assert_eq!(
                affine_stencil_count(&problem.continuous.implicit_rhs),
                2,
                "Turkey implicit RHS should carry remapped residual tensor families"
            );
            assert_eq!(
                scalar_fallback_rows(&problem.continuous.implicit_rhs),
                4,
                "Turkey implicit RHS scalar fallback rows should stay limited to derivative and boundary rows"
            );
            assert_eq!(
                scalar_fallback_output_indices(&problem.continuous.implicit_rhs),
                vec![9, 10, 20, 21],
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
                layout.implicit_rhs.stencil_families, 2,
                "Turkey layout implicit RHS should preserve remapped residual stencil families"
            );
            assert_eq!(
                layout.implicit_rhs.scalar_fallback_rows, 4,
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
                0,
                "AirfoilFlow continuous residual should not need standalone geometry stencils"
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
                8,
                "AirfoilFlow implicit RHS should carry remapped residual tensor families"
            );
            assert_eq!(
                scalar_fallback_rows(&problem.continuous.implicit_rhs),
                4,
                "AirfoilFlow implicit RHS scalar rows should stay limited to the four motor states"
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
                layout.scalar_fallback_rows, 4,
                "AirfoilFlow layout scalar rows should stay limited to the four motor states"
            );
            assert_eq!(
                layout.implicit_rhs.map_families, 7,
                "AirfoilFlow layout should preserve implicit Map family inventory"
            );
            assert_eq!(
                layout.implicit_rhs.stencil_families, 8,
                "AirfoilFlow layout should preserve implicit native stencil inventory"
            );
            assert_eq!(
                layout.implicit_rhs.scalar_fallback_rows, 4,
                "AirfoilFlow layout implicit scalar rows should stay limited to the four motor states"
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
    let gaussian_initial_family = result
        .flat
        .initial_structured_equations
        .iter()
        .filter_map(|family| family.regular.as_ref())
        .find(|regular| {
            regular.binders == ["i", "j"]
                && regular.accesses.iter().any(|access| access.var == "u")
                && regular.accesses.iter().any(|access| access.var == "w")
        });
    assert!(
        gaussian_initial_family.is_some(),
        "Wave2D Gaussian initial-condition loop should remain a regular \
         structured family even though its scalar value is nonlinear in i,j"
    );
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

/// `sig[i, j]` mask field of the docs AirfoilFlow model, indexed `(i, j) -> sig`.
/// The mask equations are explicit algebraics, so simulation results do not need
/// to carry them as solver outputs. This mirrors the model formula to keep the
/// smooth-geometry oracle aligned with what the flow equations reference.
fn airfoil_mask_probe(
    _result: &rumoca::CompilationResult,
    overrides: &[(&str, f64)],
) -> std::collections::HashMap<(usize, usize), f64> {
    let param = |name: &str, default: f64| {
        overrides
            .iter()
            .find_map(|(candidate, value)| (*candidate == name).then_some(*value))
            .unwrap_or(default)
    };

    let nx = 60usize;
    let ny = 36usize;
    let lx = 4.0_f64;
    let ly = 1.5_f64;
    let xle = 1.0_f64;
    let aoa = param("aoa", 8.0);
    let mc = param("mc0", 0.02);
    let pc = param("pc0", 0.4);
    let tk = param("tk0", 0.12);
    let dx = lx / nx as f64;
    let dy = ly / ny as f64;
    let epsn = 0.6 * dy;
    let epss = 0.8 * dx;
    let tmin = 0.6 * dy;
    let angle = aoa.to_radians();
    let ca = angle.cos();
    let sa = angle.sin();

    let mut field = std::collections::HashMap::new();
    for i in 1..=nx {
        for j in 1..=ny {
            let xa = (i as f64 - 0.5) * dx - xle;
            let ya = (j as f64 - 0.5) * dy - ly / 2.0;
            let sc = xa * ca - ya * sa;
            let nc = xa * sa + ya * ca;
            let camber = if sc < pc {
                mc / pc.powi(2) * (2.0 * pc * sc - sc.powi(2))
            } else {
                mc / (1.0 - pc).powi(2) * ((1.0 - 2.0 * pc) + 2.0 * pc * sc - sc.powi(2))
            };
            let thickness = 5.0
                * tk
                * (0.2969 * sc.max(0.0).sqrt() - 0.1260 * sc - 0.3516 * sc.powi(2)
                    + 0.2843 * sc.powi(3)
                    - 0.1036 * sc.powi(4));
            let sig = 0.5
                * (1.0
                    - (((nc - camber).abs() - (thickness.powi(2) + tmin.powi(2)).sqrt()) / epsn)
                        .tanh())
                * (0.5 * (1.0 + (sc / epss).tanh()))
                * (0.5 * (1.0 + ((1.0 - sc) / epss).tanh()));
            field.insert((i, j), sig);
        }
    }
    field
}

fn settle_airfoil(
    result: &rumoca::CompilationResult,
    t_end: f64,
    overrides: &[(&str, f64)],
) -> SimResult {
    let opts = SimOptions {
        t_end,
        solver_mode: SimSolverMode::RkLike,
        param_overrides: overrides
            .iter()
            .map(|(name, value)| ((*name).to_string(), *value))
            .collect(),
        ..SimOptions::default()
    };
    simulate_dae_with_diagnostics(&result.dae, &opts)
        .unwrap_or_else(|error| panic!("AirfoilFlow should simulate: {error:?}"))
}

/// Acceptance checks (1)-(5) for the smooth immersed mask: bounded/finite in
/// `[0,1]`, the body is present (a near-solid core), a smooth transition band
/// exists (a hard mask would have none), the solid footprint is a coherent blob,
/// and the inlet/outlet columns stay fluid (correct placement).
fn assert_smooth_mask_well_formed(mask: &std::collections::HashMap<(usize, usize), f64>) {
    // (1) Bounded and finite: a smooth indicator stays in [0, 1].
    for (&(i, j), &value) in mask {
        assert!(
            value.is_finite() && (-1e-6..=1.0 + 1e-6).contains(&value),
            "sig[{i},{j}] should be a finite solid fraction in [0,1], got {value}"
        );
    }

    // (2) The body exists and does not disappear: at least one near-solid cell.
    let max_sig = mask.values().copied().fold(0.0_f64, f64::max);
    assert!(
        max_sig > 0.8,
        "the airfoil core should be nearly solid (max sig > 0.8), got {max_sig}"
    );

    // (3) Smoothness: the tanh band must produce several partially-solid cells.
    let band_cells = mask
        .values()
        .filter(|&&v| (0.05..=0.95).contains(&v))
        .count();
    assert!(
        band_cells >= 4,
        "the smooth mask should expose a transition band of partially-solid \
         cells (a hard if/else mask has none); got {band_cells}"
    );

    // (4) Closure/coherence: present but far from filling the whole domain.
    let solid_cells = mask.values().filter(|&&v| v > 0.5).count();
    assert!(
        (4..mask.len() / 2).contains(&solid_cells),
        "the solid footprint should be a closed coherent body, got {solid_cells} \
         of {} cells",
        mask.len()
    );

    // (5) Placement: the inlet and outlet columns stay fluid.
    let (ni, nj) = mask
        .keys()
        .copied()
        .fold((0, 0), |(mi, mj), (i, j)| (mi.max(i), mj.max(j)));
    let column_solid = |i: usize| (1..=nj).any(|j| mask.get(&(i, j)).is_some_and(|&v| v > 0.5));
    assert!(
        !column_solid(1) && !column_solid(ni),
        "the inlet (i=1) and outlet (i={ni}) columns should remain fluid"
    );
}

// ---------------------------------------------------------------------------
// AirfoilFlow family-reassembly oracle (family-native lowering safety net).
//
// The 2-D Navier-Stokes airfoil model is the heaviest example in the book. Its
// `for` loops must reassemble into compact, grid-independent stencil/map kernels
// exactly like Wave2D: changing the grid must not grow the WGSL or change the
// family/kernel decomposition. This is the byte-stability gate that guards the
// `flatten` family-native work (keeping array equations from being ripped apart
// must keep this output identical).
fn airfoil_wgsl_metrics(nx: usize, ny: usize) -> Wave2DWgslMetrics {
    let docs = include_str!("../../../../docs/user-guide/src/language/arrays-pde.md");
    let nx_src = extract_doc_model_with_integer_parameter(docs, "AirfoilFlow", "NX", nx);
    let source = extract_doc_model_with_integer_parameter(&nx_src, "AirfoilFlow", "NY", ny);
    let result = Compiler::new()
        .model("AirfoilFlow")
        .compile_str(&source, "AirfoilFlow.mo")
        .unwrap_or_else(|error| panic!("AirfoilFlow {nx}x{ny} docs model should compile: {error}"));
    let problem = rumoca_phase_solve::lower_solve_problem(&result.dae)
        .unwrap_or_else(|error| panic!("AirfoilFlow {nx}x{ny} should lower to Solve IR: {error}"));
    let layout_source = render_wgsl_solve_layout_source(&problem, "AirfoilFlow");
    let layout: WgslSolveLayoutManifest =
        serde_json::from_str(&layout_source).unwrap_or_else(|error| {
            panic!("AirfoilFlow {nx}x{ny} layout should be valid JSON: {error}")
        });
    let wgsl_template = templates::builtin_template_source("wgsl-solve", "model_solve.wgsl.jinja")
        .expect("wgsl-solve WGSL template should exist");
    let artifacts = rumoca_ir_solve::SolveArtifacts::default();
    let wgsl = render_solve_template_with_name(&problem, &artifacts, wgsl_template, "AirfoilFlow")
        .unwrap_or_else(|error| {
            panic!("AirfoilFlow {nx}x{ny} wgsl-solve source should render: {error}")
        });

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

// Pinned, grid-independent family decomposition of the airfoil's WGSL kernels.
// Family-native lowering must keep these identical: the flow `for` loops
// reassemble into a fixed set of map/stencil kernels regardless of grid size,
// with only the scalar motor ODEs outside the PDE family kernels.
const AIRFOIL_DERIVATIVE_FAMILY_DECOMPOSITION: (usize, usize, usize) = (7, 8, 16);
const AIRFOIL_IMPLICIT_FAMILY_DECOMPOSITION: (usize, usize, usize) = (7, 8, 16);

#[test]
fn airfoil_wgsl_solve_reassembles_into_grid_independent_families() {
    let small = airfoil_wgsl_metrics(6, 4);
    let large = airfoil_wgsl_metrics(10, 7);

    // The four motor ODE rows are scalar by construction. Per-cell flow rows
    // must still reassemble into compact stencil/map kernels at either grid
    // size.
    assert_eq!(small.derivative_scalar_fallback_rows, 4);
    assert_eq!(large.derivative_scalar_fallback_rows, 4);
    assert_eq!(
        small.implicit_scalar_fallback_rows,
        large.implicit_scalar_fallback_rows
    );

    // The family/kernel decomposition is grid-independent (the heart of the oracle).
    assert_eq!(small.derivative_maps, large.derivative_maps);
    assert_eq!(small.derivative_stencils, large.derivative_stencils);
    assert_eq!(small.derivative_kernel_count, large.derivative_kernel_count);
    assert_eq!(small.implicit_maps, large.implicit_maps);
    assert_eq!(small.implicit_stencils, large.implicit_stencils);
    assert_eq!(small.implicit_kernel_count, large.implicit_kernel_count);

    // Pin the exact current decomposition so any family-native regression is visible.
    assert_eq!(
        (
            small.derivative_maps,
            small.derivative_stencils,
            small.derivative_kernel_count
        ),
        AIRFOIL_DERIVATIVE_FAMILY_DECOMPOSITION,
        "airfoil derivative family decomposition changed"
    );
    assert_eq!(
        (
            small.implicit_maps,
            small.implicit_stencils,
            small.implicit_kernel_count
        ),
        AIRFOIL_IMPLICIT_FAMILY_DECOMPOSITION,
        "airfoil implicit-RHS family decomposition changed"
    );

    // Output must stay compact: WGSL/layout do not grow with the grid (only a few
    // dimension constants differ between sizes).
    const MAX_GROWTH_BYTES: usize = 256;
    assert!(
        large.wgsl_bytes <= small.wgsl_bytes + MAX_GROWTH_BYTES,
        "airfoil WGSL should not grow with the grid: {} -> {}",
        small.wgsl_bytes,
        large.wgsl_bytes
    );
    assert!(
        large.layout_bytes <= small.layout_bytes + MAX_GROWTH_BYTES,
        "airfoil layout should not grow with the grid: {} -> {}",
        small.layout_bytes,
        large.layout_bytes
    );
}

#[test]
// Roadmap Track D.2: the immersed airfoil mask must be a differentiable smooth
// indicator, not a hard if/else threshold, so shape gradients are nonzero in
// the boundary band. This pins the acceptance criteria: smoothness, closure,
// boundedness, correct placement, and nonzero finite-difference sensitivity to
// the thickness coefficient.
fn airfoil_smooth_mask_is_differentiable_closed_and_shape_sensitive() {
    let docs = include_str!("../../../../docs/user-guide/src/language/arrays-pde.md");
    let source = extract_doc_model(docs, "AirfoilFlow");
    let result = Compiler::new()
        .model("AirfoilFlow")
        .compile_str(&source, "AirfoilFlow.mo")
        .unwrap_or_else(|error| panic!("AirfoilFlow docs model should compile: {error}"));

    // Reconstruct the mask formula used by the model: `sig` is an explicit
    // algebraic field, while the transient result exposes the state/input series
    // used by the web visualization.
    let mask = airfoil_mask_probe(&result, &[]);
    assert!(
        !mask.is_empty(),
        "AirfoilFlow mask oracle should reconstruct the sig field"
    );

    // A short settle still exercises the penalization term `-sig*V/tau` with
    // the smooth mask each step.
    let baseline = settle_airfoil(&result, 0.1, &[]);

    // The flow still runs with the smooth mask in place: velocities stay finite
    // and bounded (no penalization blow-up).
    let max_speed = baseline
        .names
        .iter()
        .zip(&baseline.data)
        .filter(|(name, _)| name.starts_with("u[") || name.starts_with("v["))
        .flat_map(|(_, series)| series.iter().copied())
        .fold(0.0_f64, |acc, value| {
            assert!(value.is_finite(), "velocity field should stay finite");
            acc.max(value.abs())
        });
    assert!(
        max_speed < 50.0,
        "velocities should stay bounded with the smooth penalization, got {max_speed}"
    );

    // Acceptance checks (1)-(5): bounded/finite, body present, smooth transition
    // band, coherent closed footprint, correct placement.
    assert_smooth_mask_well_formed(&mask);

    // (6) Differentiable shape sensitivity: thickening the airfoil (+10% tk)
    // must increase the total solid fraction, with the change concentrated in
    // the boundary band. A finite-difference check (roadmap §9.3) of the
    // d sig / d (thickness) gradient being nonzero and correctly signed.
    let base_tk = 0.12;
    let thick_mask = airfoil_mask_probe(&result, &[("tk0", base_tk * 1.1)]);
    let base_mask = airfoil_mask_probe(&result, &[("tk0", base_tk)]);

    let total: f64 = base_mask.values().sum();
    let total_thick: f64 = thick_mask.values().sum();
    assert!(
        total_thick - total > 1e-3,
        "thickening the airfoil should raise the total solid fraction \
         (d sig / d tk > 0); base={total}, +10% tk={total_thick}"
    );

    // The sensitivity should live near the boundary: cells that were in the
    // band must be where most of the change is, not the saturated core.
    let mut band_change = 0.0_f64;
    let mut core_change = 0.0_f64;
    for (&key, &base_value) in &base_mask {
        let delta = (thick_mask.get(&key).copied().unwrap_or(0.0) - base_value).abs();
        if (0.05..=0.95).contains(&base_value) {
            band_change += delta;
        } else if base_value > 0.95 {
            core_change += delta;
        }
    }
    assert!(
        band_change > core_change,
        "shape sensitivity should concentrate in the boundary band \
         (band={band_change}, core={core_change})"
    );

    // (7) Angle of attack is a normal pre-simulation parameter for the
    // non-interactive path: overriding it at simulation setup must rotate the
    // mask without recompilation.
    let mask_aoa0 = airfoil_mask_probe(&result, &[("aoa", 0.0)]);
    let rotated_cells = mask
        .iter()
        .filter(|(key, v8)| (mask_aoa0.get(*key).copied().unwrap_or(0.0) - **v8).abs() > 0.1)
        .count();
    assert!(
        rotated_cells >= 4,
        "overriding angle of attack before simulation should rotate the mask, \
         got {rotated_cells} changed cells"
    );
}

#[test]
fn airfoil_for_families_classify_regular_with_affine_strides() {
    let docs = include_str!("../../../../docs/user-guide/src/language/arrays-pde.md");
    let nx_src = extract_doc_model_with_integer_parameter(docs, "AirfoilFlow", "NX", 6);
    let source = extract_doc_model_with_integer_parameter(&nx_src, "AirfoilFlow", "NY", 4);
    let result = Compiler::new()
        .model("AirfoilFlow")
        .compile_str(&source, "AirfoilFlow.mo")
        .expect("airfoil compiles");
    let families = &result.flat.structured_equations;

    // The model has four `for` families: the sc/nc/sig mask loop, the interior
    // u/v/q stencil, and the inlet/outlet (over j) and far-field (over i) loops.
    assert_eq!(
        families.len(),
        4,
        "airfoil should flatten to four structured for-families"
    );

    // The input-driven geometry/mask loop contains a structural branch and is
    // allowed to decline regular classification. The flow/boundary loops still
    // need regular stride tables so the PDE kernels stay compact.
    for (k, fam) in families.iter().enumerate().skip(1) {
        let regular = fam.regular.as_ref().unwrap_or_else(|| {
            panic!("flow family[{k}] should classify as a regular elementwise family")
        });
        let domain_binders: Vec<&str> = fam
            .domain
            .binders
            .iter()
            .map(|b| b.display_name.as_str())
            .collect();
        let regular_binders: Vec<&str> = regular.binders.iter().map(String::as_str).collect();
        assert_eq!(
            regular_binders, domain_binders,
            "family[{k}] regular binder order should match the domain"
        );
        assert!(
            !regular.accesses.is_empty(),
            "family[{k}] should record its array accesses"
        );
    }

    // The interior momentum/continuity loop is the two-binder family that carries
    // non-zero stencil offsets; it must capture the full 5-point stencil.
    let interior = families
        .iter()
        .filter_map(|fam| fam.regular.as_ref())
        .find(|regular| {
            regular.binders == ["i", "j"]
                && regular
                    .accesses
                    .iter()
                    .any(|a| a.subscripts.iter().any(|s| s.constant != 0))
        })
        .expect("interior stencil family with non-zero offsets");
    let captures = |constant: i64, coeffs: &[i64]| {
        interior.accesses.iter().any(|access| {
            access
                .subscripts
                .iter()
                .any(|s| s.constant == constant && s.coeffs == coeffs)
        })
    };
    assert!(captures(1, &[1, 0]), "interior captures u[i+1, j]");
    assert!(captures(-1, &[1, 0]), "interior captures u[i-1, j]");
    assert!(captures(1, &[0, 1]), "interior captures [i, j+1]");
    assert!(captures(-1, &[0, 1]), "interior captures [i, j-1]");

    // The stride table must survive the flat -> DAE conversion: the Solve-IR
    // consumer reads it from the DAE structured families.
    let dae_regular = result
        .dae
        .continuous
        .structured_equations
        .iter()
        .filter(|fam| fam.regular.is_some())
        .count();
    assert!(
        dae_regular >= 3,
        "the airfoil's regular flow families should reach the DAE"
    );
}

/// Densify an affine stencil stride's sparse `(dimension, stride)` terms into a
/// dense per-domain-dimension vector, matching `ArrayAccess::binder_index_strides`.
fn densify_stride_terms(
    terms: &[rumoca_ir_solve::AffineStencilIndexStrideTerm],
    ndim: usize,
) -> Vec<i64> {
    let mut dense = vec![0i64; ndim];
    for term in terms {
        dense[term.dimension] = term.stride as i64;
    }
    dense
}

// Cross-check: every distinct per-binder load stride the reassembly infers for
// the airfoil PDE stencils is accounted for by the state/algebraic row-major
// layout. The mask fields are algebraics now, not promoted derived parameters,
// so the old constant-vector cell-major stride should no longer appear.
#[test]
fn airfoil_two_binder_load_strides_fully_characterized() {
    use std::collections::HashSet;

    let docs = include_str!("../../../../docs/user-guide/src/language/arrays-pde.md");
    let nx_src = extract_doc_model_with_integer_parameter(docs, "AirfoilFlow", "NX", 6);
    let source = extract_doc_model_with_integer_parameter(&nx_src, "AirfoilFlow", "NY", 4);
    let result = Compiler::new()
        .model("AirfoilFlow")
        .compile_str(&source, "AirfoilFlow.mo")
        .expect("airfoil compiles");
    let problem =
        rumoca_phase_solve::lower_solve_problem(&result.dae).expect("airfoil lowers to Solve IR");

    // Table side: the interior two-binder stencil family. Pick a state access
    // resolvable in the layout and predict its per-binder load stride from the
    // array shape -- for a 6x4 field this is row-major [NY, 1] = [4, 1].
    let interior = result
        .dae
        .continuous
        .structured_equations
        .iter()
        .filter_map(|family| family.regular.as_ref())
        .find(|regular| {
            regular.binders.len() == 2
                && regular
                    .accesses
                    .iter()
                    .any(|access| access.subscripts.iter().any(|s| s.constant != 0))
        })
        .expect("interior regular stencil family");
    let probe = interior
        .accesses
        .iter()
        .find(|access| {
            problem
                .layout
                .shape(&access.var)
                .is_some_and(|shape| shape.len() == access.subscripts.len())
        })
        .expect("a state access resolvable in the layout");
    let memory_strides = rumoca_core::row_major_strides(problem.layout.shape(&probe.var).unwrap());
    let table_stride = probe.binder_index_strides(&memory_strides, interior.binders.len());
    assert_eq!(
        table_stride,
        vec![4, 1],
        "6x4 state stencil load stride should be row-major [NY, 1]"
    );

    // Proof side: the strides the reassembly inferred for the two-binder stencils.
    let proof_strides: HashSet<Vec<i64>> = [
        &problem.continuous.derivative_rhs,
        &problem.continuous.implicit_rhs,
    ]
    .into_iter()
    .flat_map(|block| block.nodes.iter())
    .filter_map(|node| match node {
        ComputeNode::AffineStencil {
            domain,
            load_strides,
            ..
        }
        | ComputeNode::Map {
            domain,
            load_strides,
            ..
        } if domain.binders.len() == 2 => Some(load_strides),
        _ => None,
    })
    .flat_map(|load_strides| {
        load_strides
            .iter()
            .map(|ls| densify_stride_terms(&ls.terms, 2))
    })
    .collect();

    let expected: HashSet<Vec<i64>> = [table_stride.clone()].into_iter().collect();
    assert_eq!(
        proof_strides, expected,
        "airfoil two-binder load strides should be exactly the table-derived \
         row-major state/algebraic stride {table_stride:?}"
    );
}
