//! Descriptor tables of the shared C projection kernel, as rendered.
//!
//! The fmi2 and fmi3 targets settle coupled algebraic blocks through one C
//! kernel that reads per-block descriptors (`RmcBlock`) over one integer pool.
//! These cases render the generated C sources (`model.c` and the program
//! translation units beside it) for one fixture per projection
//! path (torn, affine, dense with seeded isolation, seed rescue, and the torn
//! affine elimination with promotion capacity) and check the descriptors
//! against the structure each path requires, without compiling or running the
//! emitted C.

use rumoca::Compiler;

const TORN: &str = "model TornLoop
  output Real x(start=1, fixed=true);
  output Real a(start=1);
  output Real b(start=1);
  output Real c(start=1);
equation
  der(x) = -0.5*a;
  a = x + 0.2*sin(c);
  b = a*a + 0.1*a;
  c = 1 + b - 0.3*cos(b) + 0.1*sin(c);
end TornLoop;";

const AFFINE: &str = "model AffineLoop
  output Real x(start=1, fixed=true);
  output Real i1;
  output Real i2;
equation
  der(x) = -i1 - 0.5*i2;
  (2 + x*x)*i1 + i2 = 1 + x;
  i1 - (3 + x*x)*i2 = x;
end AffineLoop;";

const DENSE: &str = "model DenseSeeded
  output Real x(start=1, fixed=true);
  output Real u(start=-1);
  output Real v(start=0.5);
equation
  der(x) = -u - v;
  u + 0.1*der(x) = 0.2*sin(v) + x;
  log(u) + v*v*v + v = x;
end DenseSeeded;";

const SINGULAR: &str = "model SingularSeed
  Real x(start=0, fixed=true);
  output Real y(start=1, fixed=true);
  output Real t(start=1);
  output Real s(start=1);
  output Real u(start=1);
equation
  der(x) = 0;
  der(y) = -0.2*t - 0.1*y;
  sin(x)*t + s = 1 + 0.5*y;
  t + u*u*u = 3;
  s - u*u = 0.5;
end SingularSeed;";

fn affine_promote() -> String {
    let mut source = String::from("model AffinePromote\n  parameter Real e = 0;\n");
    source.push_str("  output Real x(start=0.5, fixed=true);\n");
    for k in 1..=20 {
        source.push_str(&format!("  output Real v{k};\n"));
    }
    for k in 1..=3 {
        source.push_str(&format!("  output Real w{k};\n"));
    }
    source.push_str("equation\n  der(x) = -0.1*x - 0.01*v10 - 0.01*w1;\n  v1 = 1 + x;\n");
    for k in 2..=19 {
        let coupling = match k {
            8 => " + w1",
            11 => " + w3",
            16 => " + w2",
            _ => "",
        };
        source.push_str(&format!(
            "  v{} - (2 + x*x)*v{k} + v{}{coupling} = 0.1*x;\n",
            k - 1,
            k + 1
        ));
    }
    source.push_str(
        "  v5 + w1 = 0.3*x;\n  v13 + e*w2 = 0.2*x;\n  v10 + (x - 0.5)*w3 = 0.1*x;\n  \
         v20 + 0.5*v1 = x;\nend AffinePromote;\n",
    );
    source
}

fn model_c(model: &str, source: &str, target: &str) -> String {
    let compiled = match Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
    {
        Ok(compiled) => compiled,
        Err(error) => panic!("compile {model}: {error:#}"),
    };
    let files = match rumoca::render_target_files(&compiled, model, target, None) {
        Ok(files) => files,
        Err(error) => panic!("render {model} {target}: {error:#}"),
    };
    assert!(
        files.iter().any(|file| file.path == "sources/model.c"),
        "{model} {target} emits sources/model.c"
    );
    // Every C translation unit, `model.c` first; the shared header only
    // declares what the units define.
    let mut units = files
        .into_iter()
        .filter(|file| file.path.starts_with("sources/") && file.path.ends_with(".c"))
        .collect::<Vec<_>>();
    units.sort_by_key(|file| (file.path != "sources/model.c", file.path.clone()));
    units
        .into_iter()
        .map(|file| file.content)
        .collect::<Vec<_>>()
        .join("\n")
}

/// `RmcBlock` field names in their C initializer order.
const FIELDS: [&str; 55] = [
    "canonical",
    "n",
    "y",
    "row_target",
    "row_program",
    "row_offset",
    "max_outputs",
    "affine",
    "singleton_exact",
    "has_tearing",
    "row_ptr",
    "col_idx",
    "nnz",
    "ncolors",
    "colors",
    "color_seeds",
    "color_calls",
    "placements",
    "jvp_max_outputs",
    "torn",
    "k",
    "tear_col",
    "residual_row",
    "ncausal",
    "ncruns",
    "cruns",
    "causal_target",
    "causal_col",
    "elimination",
    "nelim",
    "elim_row",
    "elim_col",
    "nelim_tear",
    "elim_residual",
    "elim_tear",
    "nguards",
    "guards",
    "guard_step",
    "elim_capacity",
    "iso_default",
    "iso_start",
    "iso_entries",
    "nlane_calls",
    "lane_calls",
    "lane_max_outputs",
    "lane_max",
    "nsplit",
    "split",
    "row_split",
    "inv_len",
    "tangent_lanes",
    "tangent_directional",
    "ntangent_steps",
    "tangent_steps",
    "tangent_residuals",
];

const ISOLATION_KINDS: std::ops::RangeInclusive<usize> = 0..=2;
const ISOLATION_PROGRAM: usize = 2;
const RUN_CHAIN: usize = 0;
const RUN_ISOLATOR: usize = 1;

/// The rendered projection tables of one component.
#[derive(Debug, PartialEq)]
struct Tables {
    pool: Vec<usize>,
    blocks: Vec<Block>,
    isolator_count: usize,
    causal_count: usize,
}

#[derive(Debug, PartialEq)]
struct Block(Vec<usize>);

impl Block {
    fn get(&self, field: &str) -> usize {
        let Some(index) = FIELDS.iter().position(|name| *name == field) else {
            panic!("unknown descriptor field `{field}`");
        };
        self.0[index]
    }

    fn flag(&self, field: &str) -> bool {
        self.get(field) == 1
    }

    fn range<'a>(&self, tables: &'a Tables, field: &str, len: usize) -> &'a [usize] {
        let start = self.get(field);
        let Some(values) = tables.pool.get(start..start + len) else {
            panic!("`{field}` range {start}+{len} exceeds the pool");
        };
        values
    }
}

fn braced_body<'a>(source: &'a str, opener: &str) -> Option<&'a str> {
    let start = source.find(opener)?;
    let body = &source[start..];
    let open = body.find('{')? + 1;
    let close = body.find("\n};")?;
    Some(&body[open..close])
}

fn parse_value(token: &str) -> usize {
    let token = token.trim();
    match token {
        "true" => 1,
        "false" => 0,
        _ => {
            let digits = token
                .strip_prefix("RMC_P(")
                .and_then(|rest| rest.strip_suffix(')'))
                .unwrap_or(token);
            match digits.parse() {
                Ok(value) => value,
                Err(_) => panic!("descriptor value `{token}` is not an integer"),
            }
        }
    }
}

fn parse_tables(source: &str) -> Option<Tables> {
    let blocks = braced_body(source, "static const RmcBlock rmc_blocks[")?
        .lines()
        .filter_map(|line| {
            let line = line.trim().trim_end_matches(',');
            let inner = line.strip_prefix('{')?.strip_suffix('}')?;
            Some(Block(inner.split(',').map(parse_value).collect()))
        })
        .collect::<Vec<_>>();
    for block in &blocks {
        assert_eq!(block.0.len(), FIELDS.len(), "descriptor arity: {block:?}");
    }
    let pool = braced_body(source, "static const size_t rmc_pool[")?
        .split(',')
        .filter(|token| !token.trim().is_empty())
        .map(parse_value)
        .collect();
    // One-line initializers `opener ... = { a, b, };` of the program tables.
    let entries = |opener: &str| {
        source.find(opener).map_or(0, |start| {
            let body = &source[start..];
            let open = body.find('{').map_or(body.len(), |open| open + 1);
            let close = body.find("};").unwrap_or(body.len()).max(open);
            body[open..close]
                .split(',')
                .filter(|t| !t.trim().is_empty())
                .count()
        })
    };
    let isolator_count = entries("RMC_API const size_t rmc_iso_group[");
    let causal_count = entries("RMC_API const RmcCausalProgram rmc_causal_fn[");
    Some(Tables {
        pool,
        blocks,
        isolator_count,
        causal_count,
    })
}

fn rendered(model: &str, source: &str) -> (String, Tables) {
    let fmi3 = model_c(model, source, "fmi3");
    let (Some(tables), Some(fmi2)) = (
        parse_tables(&fmi3),
        parse_tables(&model_c(model, source, "fmi2")),
    ) else {
        panic!("{model}: fmi2 and fmi3 emit projection block descriptors");
    };
    assert_eq!(
        fmi2, tables,
        "{model}: fmi2 and fmi3 execute the same descriptor tables"
    );
    (fmi3, tables)
}

/// Structural invariants every descriptor satisfies whatever its path.
fn assert_well_formed(model: &str, tables: &Tables) {
    for block in &tables.blocks {
        assert_pattern(model, tables, block);
        assert_jacobian(model, tables, block);
        assert_tearing(model, tables, block);
        assert_elimination(model, block);
        assert_isolation(model, tables, block);
    }
}

/// Distinct unknowns and a compressed-row pattern over the block's columns.
fn assert_pattern(model: &str, tables: &Tables, block: &Block) {
    let n = block.get("n");
    assert!(n > 0, "{model}: a projection block is non-empty");
    let mut unknowns = block.range(tables, "y", n).to_vec();
    unknowns.sort_unstable();
    unknowns.dedup();
    assert_eq!(unknowns.len(), n, "{model}: block unknowns are distinct");
    let nnz = block.get("nnz");
    let row_ptr = block.range(tables, "row_ptr", n + 1);
    assert_eq!((row_ptr[0], row_ptr[n]), (0, nnz), "{model}: CSR bounds");
    assert!(row_ptr.windows(2).all(|pair| pair[0] <= pair[1]));
    assert!(block.range(tables, "col_idx", nnz).iter().all(|&c| c < n));
}

/// Colored forward Jacobian calls place every entry inside the pattern.
fn assert_jacobian(model: &str, tables: &Tables, block: &Block) {
    let ncolors = block.get("ncolors");
    let colors = block.range(tables, "colors", 4 * ncolors);
    assert!(
        colors
            .chunks(4)
            .all(|color| color[0] <= color[1] && color[2] <= color[3])
    );
    let calls = colors.chunks(4).map(|color| color[3]).max().unwrap_or(0);
    let call_table = block.range(tables, "color_calls", 3 * calls);
    let placement_count = call_table.chunks(3).map(|call| call[2]).max().unwrap_or(0);
    let placements = block.range(tables, "placements", 2 * placement_count);
    assert!(
        placements.chunks(2).all(|pair| pair[1] < block.get("nnz")),
        "{model}: every Jacobian placement lands inside the pattern"
    );
    if block.flag("singleton_exact") {
        assert_eq!(
            (block.get("n"), ncolors),
            (1, 0),
            "{model}: exact singletons skip the JVP"
        );
    }
}

/// Tears and causal steps partition the block; causal steps name isolators.
fn assert_tearing(model: &str, tables: &Tables, block: &Block) {
    if !block.flag("torn") {
        return;
    }
    let (n, k, ncausal) = (block.get("n"), block.get("k"), block.get("ncausal"));
    assert!(block.flag("has_tearing"));
    assert_eq!(
        k + ncausal,
        n,
        "{model}: tears and causal steps cover the block"
    );
    assert!(block.range(tables, "tear_col", k).iter().all(|&c| c < n));
    assert!(
        block
            .range(tables, "residual_row", k)
            .iter()
            .all(|&r| r < n)
    );
    assert!(
        block
            .range(tables, "causal_col", ncausal)
            .iter()
            .all(|&c| c < n)
    );
    // Runs of (kind, function, first step, step count) cover the causal
    // steps in order; a single step names an emitted isolator, a longer run
    // an emitted causal chain.
    let runs = block.range(tables, "cruns", 4 * block.get("ncruns"));
    let mut next = 0;
    for run in runs.chunks(4) {
        let [kind, function, first, count] = [run[0], run[1], run[2], run[3]];
        assert_eq!(first, next, "{model}: causal runs cover the steps in order");
        assert!(count > 0, "{model}: a causal run is non-empty");
        let emitted = match kind {
            RUN_ISOLATOR => count == 1 && function < tables.isolator_count,
            RUN_CHAIN => count > 1 && function < tables.causal_count,
            _ => false,
        };
        assert!(
            emitted,
            "{model}: causal run {run:?} names an emitted program"
        );
        next += count;
    }
    assert_eq!(
        next, ncausal,
        "{model}: causal runs cover every causal step"
    );
    // The tear Jacobian comes from the block's tangent plan: one step per
    // causal step, in the issued order, and one reduced row per tear.
    assert_eq!(
        block.get("tangent_lanes"),
        k + 1,
        "{model}: tears plus the coefficient lane"
    );
    assert_eq!(
        block.get("ntangent_steps"),
        ncausal,
        "{model}: one tangent step per causal step"
    );
    let steps = block.range(tables, "tangent_steps", 5 * ncausal);
    let targets = block.range(tables, "causal_target", ncausal);
    assert_eq!(
        steps.chunks(5).map(|step| step[3]).collect::<Vec<_>>(),
        targets,
        "{model}: the tangent steps follow the causal order"
    );
    assert_groups(model, steps.chunks(5).map(|step| step[4]), "steps");
    let residuals = block.range(tables, "tangent_residuals", 4 * k);
    assert_groups(
        model,
        residuals.chunks(4).map(|entry| entry[3]),
        "residuals",
    );
}

/// Evaluation groups partition their entries in order: each group leader
/// counts itself and its followers, and every follower counts zero.
fn assert_groups(model: &str, groups: impl Iterator<Item = usize>, what: &str) {
    let mut pending = 0;
    for group in groups {
        if pending == 0 {
            assert!(group > 0, "{model}: the first {what} entry leads a group");
            pending = group;
        } else {
            assert_eq!(group, 0, "{model}: a {what} follower counts zero");
        }
        pending -= 1;
    }
    assert_eq!(pending, 0, "{model}: the last {what} group is complete");
}

/// The elimination covers the block and carries the shared capacity.
fn assert_elimination(model: &str, block: &Block) {
    if !block.flag("elimination") {
        assert_eq!(block.get("elim_capacity"), 0);
        return;
    }
    let tears = block.get("nelim_tear");
    assert!(block.flag("affine"));
    assert_eq!(block.get("nelim") + tears, block.get("n"));
    assert_eq!(
        Some(block.get("elim_capacity")),
        rumoca_eval_solve::projection_policy::torn_promotion_capacity(tears),
        "{model}: the descriptor carries the shared promotion capacity"
    );
}

/// Each row lists only the isolation kinds that differ from its default.
fn assert_isolation(model: &str, tables: &Tables, block: &Block) {
    let n = block.get("n");
    let defaults = block.range(tables, "iso_default", n);
    let starts = block.range(tables, "iso_start", n + 1);
    assert_eq!(starts[0], 0);
    assert!(starts.windows(2).all(|pair| pair[0] <= pair[1]));
    let entries = block.range(tables, "iso_entries", 3 * starts[n]);
    for (row, default) in defaults.iter().enumerate() {
        assert!(ISOLATION_KINDS.contains(default) && *default != ISOLATION_PROGRAM);
        let exceptions = &entries[3 * starts[row]..3 * starts[row + 1]];
        assert!(
            exceptions.chunks(3).all(|entry| {
                entry[0] < n
                    && ISOLATION_KINDS.contains(&entry[1])
                    && entry[1] != *default
                    && (entry[1] != ISOLATION_PROGRAM || entry[2] < tables.isolator_count)
            }),
            "{model}: row {row} lists only valid exceptions to its default"
        );
    }
}

fn single_block<'a>(model: &str, tables: &'a Tables) -> &'a Block {
    assert_eq!(tables.blocks.len(), 1, "{model} has one projection block");
    &tables.blocks[0]
}

fn isolator_programs(tables: &Tables, block: &Block) -> usize {
    let n = block.get("n");
    let starts = block.range(tables, "iso_start", n + 1);
    block
        .range(tables, "iso_entries", 3 * starts[n])
        .chunks(3)
        .filter(|entry| entry[1] == ISOLATION_PROGRAM)
        .count()
}

#[test]
fn torn_loop_descriptor_carries_its_tearing_and_colored_jacobian() {
    let (_, tables) = rendered("TornLoop", TORN);
    assert_well_formed("TornLoop", &tables);
    let block = single_block("TornLoop", &tables);
    assert_eq!(block.get("n"), 3);
    assert!(block.flag("torn") && !block.flag("affine") && !block.flag("elimination"));
    assert!(
        block.get("nlane_calls") > 0 && block.get("ncolors") == 0,
        "the torn block keeps its JVP, emitted once as multi-lane calls"
    );
    assert_eq!(block.get("nnz"), 6);
}

#[test]
fn small_affine_loop_is_torn_without_the_elimination() {
    let (_, tables) = rendered("AffineLoop", AFFINE);
    assert_well_formed("AffineLoop", &tables);
    let block = single_block("AffineLoop", &tables);
    assert_eq!(block.get("n"), 2);
    assert!(block.flag("affine") && !block.flag("singleton_exact"));
    assert!(
        !block.flag("elimination"),
        "a two-unknown block is not a sparse candidate for the elimination"
    );
    assert_eq!(
        isolator_programs(&tables, block),
        0,
        "a coupled affine block is settled by its affine solve, never by isolation"
    );
}

#[test]
fn seeded_blocks_emit_isolators_and_the_block_rescue() {
    for (model, source) in [("DenseSeeded", DENSE), ("SingularSeed", SINGULAR)] {
        let (model_c, tables) = rendered(model, source);
        assert_well_formed(model, &tables);
        let block = single_block(model, &tables);
        assert!(!block.flag("affine"));
        assert!(
            isolator_programs(&tables, block) > 0,
            "{model}: the dense path seeds through row isolators"
        );
        assert!(
            has_seeded_projection_step(&model_c),
            "{model}: a seeded projection stage restores its targets on failure"
        );
    }
}

#[test]
fn affine_elimination_descriptor_carries_the_promotion_capacity() {
    let source = affine_promote();
    let (_, tables) = rendered("AffinePromote", &source);
    assert_well_formed("AffinePromote", &tables);
    let eliminated = tables
        .blocks
        .iter()
        .filter(|block| block.flag("elimination"))
        .collect::<Vec<_>>();
    assert_eq!(
        eliminated.len(),
        1,
        "the coupled chain takes the elimination"
    );
    let block = eliminated[0];
    assert_eq!(block.get("n"), 21);
    let tears = block.get("nelim_tear");
    assert!(tears > 0 && block.get("elim_capacity") > tears);
    assert_eq!(
        block.get("nguards"),
        0,
        "no causal step of this chain is guarded by a structural zero"
    );
    let singletons = tables
        .blocks
        .iter()
        .filter(|block| block.flag("singleton_exact"))
        .count();
    assert_eq!(singletons + 1, tables.blocks.len());
}

/// Whether a refresh step table holds a projection step with seeds: rows are
/// `{ kind, first, count, block, seed_first, seed_count, rescue, nrescue }`
/// with kind 2 for a projection step. The same reading as
/// `suite_template_runtime/cli_target_fmi/projection.rs`, which is a separate
/// test binary.
fn has_seeded_projection_step(source: &str) -> bool {
    source.lines().any(|line| {
        let fields = line
            .trim()
            .strip_prefix('{')
            .and_then(|rest| rest.strip_suffix("},"))
            .map(|row| row.split(',').map(str::trim).collect::<Vec<_>>());
        matches!(fields.as_deref(), Some([kind, _, _, _, _, seeds, _, _]) if *kind == "2" && *seeds != "0")
    })
}

/// Two coupled loops torn into one block with two tears.
const TWO_TEARS: &str = "model TwoTears
  Real x(start=1, fixed=true);
  Real u(start=1);
  Real v(start=1);
  Real w(start=1);
  Real z(start=1);
  Real s(start=1);
equation
  der(x) = -0.1*u - 0.1*z;
  u = x + 0.2*sin(w) + 0.1*cos(z);
  v = u*u + 0.1*u;
  w = 1 + v - 0.3*cos(v) + 0.1*sin(w);
  s = 0.5*z + 0.2*sin(z);
  z = 0.3*s*s + 0.1*u + 0.2*sin(z);
end TwoTears;";

/// With the colored lanes scoped off, a block's Jacobian renders as the
/// one-direction colored calls: colors and placements, no lane calls.
#[test]
fn torn_loop_descriptor_renders_one_direction_colors_without_lanes() {
    use rumoca_eval_solve::projection_policy::{JacobianSources, with_jacobian_sources};
    let sources = JacobianSources {
        colored_lanes: false,
    };
    let (_, tables) = with_jacobian_sources(sources, || rendered("TornLoop", TORN));
    assert_well_formed("TornLoop", &tables);
    let block = single_block("TornLoop", &tables);
    assert!(
        block.get("ncolors") > 0 && block.get("nlane_calls") == 0,
        "the torn block keeps its JVP as one-direction colors"
    );
    assert_eq!(block.get("nnz"), 6);
}

const SPLIT_CIRCLE_CHART: &str = include_str!("../fixtures/index_reduction/SplitCircleChart.mo");

/// A reduced chart set with an executable alternate renders both charts:
/// their switching descriptors, keep references, and state nominals, one step
/// table per chart and refresh plan, and one derivative kernel per chart
/// (SPEC_0040 STRUCT-T07 constraint-fold chart rows; SPEC_0044 ME-PROJ-001).
#[test]
fn a_split_circle_renders_both_charts_for_switching() {
    for target in ["fmi2", "fmi3"] {
        let source = model_c("SplitCircleChart", SPLIT_CIRCLE_CHART, target);
        assert!(source.contains("#define RMC_NCHARTS 2"), "{target}");
        let charts = braced_body(&source, "static const RmcChart rmc_charts[RMC_NCHARTS] = {")
            .expect("the chart descriptors");
        assert_eq!(
            charts.matches("RMC_P(").count(),
            6,
            "{target}: three pool ranges per chart"
        );
        let nominals = braced_body(
            &source,
            "static const double rmc_chart_nominals[RMC_NCHARTS][2] = {",
        )
        .expect("the chart state nominals");
        assert_eq!(
            nominals.matches('{').count(),
            2,
            "{target}: one nominal row per chart"
        );
        for plan in ["refresh_derivative_values", "refresh_algebraics"] {
            for chart in 0..2 {
                assert!(
                    source.contains(&format!("case {chart}: return {plan}_c{chart}(m);")),
                    "{target} {plan} {chart}"
                );
            }
        }
        assert!(
            source.contains("rmc_derivative_c1(ModelInstance* m)"),
            "{target}: the alternate's derivative kernel"
        );
    }
}

/// The fixed-step drive of the linked kernel switches charts before each
/// quarter-turn fold and stays on the physical branch.
#[test]
fn the_linked_fixed_step_drive_switches_the_split_circle() {
    let compiled = Compiler::new()
        .model("SplitCircleChart")
        .compile_str(SPLIT_CIRCLE_CHART, "SplitCircleChart.mo")
        .expect("compile SplitCircleChart");
    let component = rumoca_sim::lower_fmi_component(&compiled.dae).expect("lower SplitCircleChart");
    let artifact = rumoca_solver::fmi_me::MeModelArtifact::new(component);
    let step = 1.0e-2;
    let stop = 679.0 * step;
    let run = rumoca_solver::fmi_me::fixed_step::fixed_step_rk4(&artifact, step, stop)
        .expect("fixed-step drive");
    assert_eq!(run.states.len(), 679);
    assert_eq!(run.switches.len(), 4, "{:?}", run.switches);
    for (index, switch) in run.switches.iter().enumerate() {
        assert_eq!(
            (switch.from, switch.to),
            if index % 2 == 0 { (0, 1) } else { (1, 0) }
        );
        assert!(
            switch.sigma_target > 1.5 * switch.sigma_active,
            "{switch:?}"
        );
        let quarter = (switch.time / std::f64::consts::FRAC_PI_2).fract();
        assert!(
            quarter > 0.5 && quarter < 0.9,
            "a switch precedes its fold: {switch:?}"
        );
    }
}

/// The two-tear block takes its tear Jacobian from the tangent plan the
/// linked kernel evaluates: the rendered steps name the issued causal targets
/// and each reduced row reads one lane function.
#[test]
fn two_tears_render_the_torn_tangent_plan() {
    let (_, tables) = rendered("TwoTears", TWO_TEARS);
    assert_well_formed("TwoTears", &tables);
    let block = tables
        .blocks
        .iter()
        .find(|block| block.flag("torn"))
        .expect("the coupled loops are torn");
    let k = block.get("k");
    assert!(k >= 2, "two loops tear two coordinates");
    assert_eq!(block.get("tangent_lanes"), k + 1);
    assert!(
        !block.flag("tangent_directional"),
        "every row's JVP program widens"
    );
    assert!(
        block.get("lane_max") > k,
        "the lane seed holds the torn lanes"
    );
}
