//! A constant-filled aggregate lowers to one compact fill at its declared
//! shape, so the issued program does not grow with the cell count.

use rumoca::Compiler;
use rumoca_ir_solve::{ComputeBlock, ComputeNode, LinearOp};
use rumoca_sim::{SimOptions, simulate_dae};

fn model_source(name: &str, declaration: &str, equation: &str, rows: usize, cols: usize) -> String {
    format!(
        "model {name}\n  constant Integer rows = {rows};\n  constant Integer cols = {cols};\n  \
         {declaration}\nequation\n  {equation}\nend {name};\n"
    )
}

fn operations(block: &ComputeBlock) -> usize {
    block
        .nodes
        .iter()
        .map(|node| match node {
            ComputeNode::ScalarPrograms(programs) => {
                programs.programs().iter().map(Vec::len).sum::<usize>()
            }
            _ => 0,
        })
        .sum()
}

fn issued_operations(name: &str, source: &str) -> usize {
    let compiled = Compiler::new()
        .model(name)
        .compile_str(source, &format!("{name}.mo"))
        .unwrap_or_else(|error| panic!("compile {name}: {error:#}"));
    let solve = rumoca_sim::lower_solve_problem(&compiled.dae)
        .unwrap_or_else(|error| panic!("lower {name}: {error:#}"));
    let fills = [&solve.continuous.residual, &solve.continuous.implicit_rhs]
        .into_iter()
        .flat_map(|block| &block.nodes)
        .filter_map(|node| match node {
            ComputeNode::ScalarPrograms(programs) => Some(programs.programs()),
            _ => None,
        })
        .flatten()
        .flatten()
        .filter(|op| matches!(op, LinearOp::LoadY { .. }))
        .count();
    assert_eq!(fills, 0, "{name} issues no per-cell load of its output");
    operations(&solve.continuous.residual) + operations(&solve.continuous.implicit_rhs)
}

const SHAPES: [(&str, &str, &str); 4] = [
    (
        "ZeroMatrix",
        "output Real y[rows, cols];",
        "y = zeros(rows, cols);",
    ),
    (
        "ZeroVector",
        "output Real y[rows * cols];",
        "y = zeros(rows * cols);",
    ),
    (
        "FillMatrix",
        "output Real y[rows, cols];",
        "y = fill(0.0, rows, cols);",
    ),
    (
        "OnesMatrix",
        "output Real y[rows, cols];",
        "y = ones(rows, cols);",
    ),
];

/// MLS 10.3.3: `zeros`, `ones`, and `fill` are constant aggregates; the issued
/// program size is independent of the declared extents.
#[test]
fn a_constant_fill_issues_the_same_program_at_every_extent() {
    for (name, declaration, equation) in SHAPES {
        let small = issued_operations(name, &model_source(name, declaration, equation, 10, 10));
        let large = issued_operations(name, &model_source(name, declaration, equation, 350, 49));
        assert_eq!(small, large, "{name}: 10x10 and 350x49 differ");
        assert!(large <= 16, "{name}: {large} operations for one fill");
    }
}

fn simulated(name: &str, source: &str) -> rumoca_sim::SimResult {
    let compiled = Compiler::new()
        .model(name)
        .compile_str(source, &format!("{name}.mo"))
        .unwrap_or_else(|error| panic!("compile {name}: {error:#}"));
    simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.1,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("simulate {name}: {error:#}"))
}

/// Every cell of the compact fill is written with its exact value.
#[test]
fn a_constant_fill_writes_every_cell() {
    for (name, declaration, equation) in SHAPES {
        let result = simulated(name, &model_source(name, declaration, equation, 7, 5));
        let expected = if name == "OnesMatrix" { 1.0 } else { 0.0 };
        let cells = result
            .names
            .iter()
            .enumerate()
            .filter(|(_, column)| column.starts_with("y["))
            .collect::<Vec<_>>();
        assert_eq!(cells.len(), 35, "{name}");
        for (index, column) in cells {
            for value in &result.data[index] {
                assert_eq!(value.to_bits(), f64::to_bits(expected), "{name} {column}");
            }
        }
    }
}

/// A non-identity fill value keeps its exact value in every cell.
#[test]
fn a_literal_fill_keeps_its_value() {
    let source = model_source(
        "TwoFill",
        "output Real y[rows, cols];",
        "y = fill(2.5, rows, cols);",
        6,
        4,
    );
    let result = simulated("TwoFill", &source);
    let cells = result
        .names
        .iter()
        .enumerate()
        .filter(|(_, column)| column.starts_with("y["))
        .collect::<Vec<_>>();
    assert_eq!(cells.len(), 24);
    for (index, _) in cells {
        assert!(result.data[index].iter().all(|&value| value == 2.5));
    }
}
