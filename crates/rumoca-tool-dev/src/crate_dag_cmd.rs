use anyhow::{Context, Result, bail};
use clap::{Args as ClapArgs, ValueEnum};
use serde::Serialize;
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const DEFAULT_OUTPUT_PREFIX_REL: &str = "target/crate-dag/workspace-crate-dag";
const DEFAULT_SHARED_VIEW_REL: &str = ".rumoca/crate-dag-default-view.json";

#[derive(Debug, Clone, ClapArgs)]
pub(crate) struct CrateDagArgs {
    /// Output format (`html`, `dot`, `svg`, or `png`)
    #[arg(long, value_enum, default_value_t = CrateDagFormat::Html)]
    format: CrateDagFormat,
    /// Output path (default: `target/crate-dag/workspace-crate-dag.{format}`)
    #[arg(long)]
    output: Option<PathBuf>,
    /// Include workspace dev-dependencies in the graph
    #[arg(long)]
    include_dev: bool,
    /// Include workspace build-dependencies in the graph
    #[arg(long)]
    include_build: bool,
    /// Graph presentation style
    #[arg(long, value_enum, default_value_t = PresentationLayout::Force)]
    layout: PresentationLayout,
    /// Shared default view JSON (default: .rumoca/crate-dag-default-view.json when present)
    #[arg(long)]
    shared_view: Option<PathBuf>,
    /// Open the generated output with the system viewer
    #[arg(long)]
    display: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum CrateDagFormat {
    Html,
    Dot,
    Svg,
    Png,
}

impl CrateDagFormat {
    fn extension(self) -> &'static str {
        match self {
            Self::Html => "html",
            Self::Dot => "dot",
            Self::Svg => "svg",
            Self::Png => "png",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum PresentationLayout {
    TopDown,
    LeftRight,
    Force,
}

impl PresentationLayout {
    fn as_key(self) -> &'static str {
        match self {
            Self::TopDown => "top-down",
            Self::LeftRight => "left-right",
            Self::Force => "force",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum DependencyKind {
    Normal,
    Build,
    Dev,
}

impl DependencyKind {
    fn from_metadata_value(value: &Value) -> Self {
        match value.as_str() {
            Some("build") => Self::Build,
            Some("dev") => Self::Dev,
            _ => Self::Normal,
        }
    }

    fn priority(self) -> usize {
        match self {
            Self::Normal => 0,
            Self::Build => 1,
            Self::Dev => 2,
        }
    }

    fn dot_edge_attrs(self) -> &'static str {
        match self {
            Self::Normal => "",
            Self::Build => {
                r##" [style=dotted, color="#64748b", penwidth=1.1, label="build", fontsize=9, fontcolor="#334155"]"##
            }
            Self::Dev => {
                r##" [style=dashed, color="#94a3b8", penwidth=1.1, label="dev", fontsize=9, fontcolor="#334155"]"##
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum CrateKind {
    EntryPoint,
    Core,
    Ir,
    Phase,
    Eval,
    Session,
    Sim,
    Tool,
    Binding,
    Contracts,
    Test,
    Other,
}

impl CrateKind {
    fn from_name(name: &str) -> Self {
        if name == "rumoca" {
            Self::EntryPoint
        } else if name == "rumoca-core" {
            Self::Core
        } else if name.starts_with("rumoca-ir-") {
            Self::Ir
        } else if name.starts_with("rumoca-phase-") {
            Self::Phase
        } else if name.starts_with("rumoca-eval-") {
            Self::Eval
        } else if name == "rumoca-compile" {
            Self::Session
        } else if name == "rumoca-sim" || name.starts_with("rumoca-sim-") {
            Self::Sim
        } else if name.starts_with("rumoca-tool-") {
            Self::Tool
        } else if name.starts_with("rumoca-bind-") || name.starts_with("rumoca-wasm-") {
            Self::Binding
        } else if name == "rumoca-contracts" {
            Self::Contracts
        } else if name.starts_with("rumoca-test-") {
            Self::Test
        } else {
            Self::Other
        }
    }

    fn label(self) -> &'static str {
        match self {
            Self::EntryPoint => "entry",
            Self::Core => "core",
            Self::Ir => "ir",
            Self::Phase => "phase",
            Self::Eval => "eval",
            Self::Session => "session",
            Self::Sim => "sim",
            Self::Tool => "tool",
            Self::Binding => "binding",
            Self::Contracts => "contracts",
            Self::Test => "test",
            Self::Other => "other",
        }
    }

    fn colors(self) -> NodeColors {
        match self {
            Self::EntryPoint => NodeColors {
                fill: "#4c6ef5",
                font: "#ffffff",
            },
            Self::Core => NodeColors {
                fill: "#495057",
                font: "#ffffff",
            },
            Self::Ir => NodeColors {
                fill: "#74c69d",
                font: "#111827",
            },
            Self::Phase => NodeColors {
                fill: "#ffd166",
                font: "#111827",
            },
            Self::Eval => NodeColors {
                fill: "#f4a261",
                font: "#111827",
            },
            Self::Session => NodeColors {
                fill: "#8d99ae",
                font: "#ffffff",
            },
            Self::Sim => NodeColors {
                fill: "#ffadad",
                font: "#111827",
            },
            Self::Tool => NodeColors {
                fill: "#90e0ef",
                font: "#111827",
            },
            Self::Binding => NodeColors {
                fill: "#cdb4db",
                font: "#111827",
            },
            Self::Contracts => NodeColors {
                fill: "#ff8fab",
                font: "#111827",
            },
            Self::Test => NodeColors {
                fill: "#bde0fe",
                font: "#111827",
            },
            Self::Other => NodeColors {
                fill: "#dee2e6",
                font: "#111827",
            },
        }
    }

    fn legend_order() -> &'static [Self] {
        &[
            Self::EntryPoint,
            Self::Core,
            Self::Ir,
            Self::Phase,
            Self::Eval,
            Self::Session,
            Self::Sim,
            Self::Tool,
            Self::Binding,
            Self::Contracts,
            Self::Test,
            Self::Other,
        ]
    }
}

impl fmt::Display for CrateKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.label())
    }
}

#[derive(Debug, Clone, Copy)]
struct NodeColors {
    fill: &'static str,
    font: &'static str,
}

#[derive(Debug, Clone)]
struct WorkspaceNode {
    name: String,
    kind: CrateKind,
}

#[derive(Debug, Clone)]
struct WorkspaceEdge {
    from: String,
    to: String,
    kind: DependencyKind,
}

#[derive(Debug, Clone)]
struct WorkspaceGraph {
    nodes: Vec<WorkspaceNode>,
    edges: Vec<WorkspaceEdge>,
}

pub(crate) fn run(root: &Path, args: CrateDagArgs) -> Result<()> {
    let metadata = workspace_metadata(root)?;
    let graph = build_workspace_graph(&metadata, args.include_dev, args.include_build)?;
    let shared_default_view = load_shared_default_view(root, args.shared_view.as_deref())?;
    let output_path = resolve_output_path(root, args.output, args.format);
    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }

    let mut dot_path: Option<PathBuf> = None;
    match args.format {
        CrateDagFormat::Html => {
            let html_text = render_html_graph(&graph, args.layout, shared_default_view.as_ref())?;
            fs::write(&output_path, html_text)
                .with_context(|| format!("failed to write {}", output_path.display()))?;
        }
        CrateDagFormat::Dot => {
            let dot_text = render_dot_graph(&graph, args.layout);
            fs::write(&output_path, dot_text)
                .with_context(|| format!("failed to write {}", output_path.display()))?;
        }
        CrateDagFormat::Svg | CrateDagFormat::Png => {
            let dot_text = render_dot_graph(&graph, args.layout);
            let path = output_path.with_extension("dot");
            fs::write(&path, dot_text)
                .with_context(|| format!("failed to write {}", path.display()))?;
            dot_path = Some(path.clone());

            ensure_dot_available(root)?;
            let layout_engine = graphviz_engine(args.layout);
            let mut command = Command::new("dot");
            command
                .arg(format!("-K{layout_engine}"))
                .arg(format!("-T{}", args.format.extension()))
                .arg(&path)
                .arg("-o")
                .arg(&output_path)
                .current_dir(root);
            super::run_status(command)?;
        }
    }

    println!("Crate DAG: {}", output_path.display());
    if let Some(dot_path) = dot_path {
        println!("DOT source: {}", dot_path.display());
    }
    println!("Crates: {}", graph.nodes.len());
    println!("Dependencies: {}", graph.edges.len());
    if args.display {
        display_output(root, &output_path, args.format)?;
    }
    Ok(())
}

fn load_shared_default_view(root: &Path, shared_view: Option<&Path>) -> Result<Option<Value>> {
    let candidate = match shared_view {
        Some(path) if path.is_absolute() => path.to_path_buf(),
        Some(path) => root.join(path),
        None => root.join(DEFAULT_SHARED_VIEW_REL),
    };
    if !candidate.is_file() {
        return Ok(None);
    }
    let text = fs::read_to_string(&candidate)
        .with_context(|| format!("failed to read {}", candidate.display()))?;
    let value: Value = serde_json::from_str(&text)
        .with_context(|| format!("failed to parse {}", candidate.display()))?;
    if !value.is_object() {
        bail!(
            "shared crate-dag view must be a JSON object: {}",
            candidate.display()
        );
    }
    Ok(Some(value))
}

fn workspace_metadata(root: &Path) -> Result<Value> {
    let mut command = Command::new("cargo");
    command
        .arg("metadata")
        .arg("--no-deps")
        .arg("--format-version=1")
        .current_dir(root);
    let json = super::run_capture(command).context("failed to run cargo metadata")?;
    serde_json::from_str(&json).context("failed to parse cargo metadata JSON")
}

fn build_workspace_graph(
    metadata: &Value,
    include_dev: bool,
    include_build: bool,
) -> Result<WorkspaceGraph> {
    let workspace_members = metadata
        .get("workspace_members")
        .and_then(Value::as_array)
        .context("cargo metadata missing workspace_members")?
        .iter()
        .filter_map(Value::as_str)
        .collect::<BTreeSet<_>>();
    let packages = metadata
        .get("packages")
        .and_then(Value::as_array)
        .context("cargo metadata missing packages")?;

    let mut workspace_package_names = BTreeSet::new();
    let mut workspace_packages = Vec::new();
    for package in packages {
        let Some(id) = package.get("id").and_then(Value::as_str) else {
            continue;
        };
        if !workspace_members.contains(id) {
            continue;
        }
        let Some(name) = package.get("name").and_then(Value::as_str) else {
            continue;
        };
        workspace_package_names.insert(name.to_string());
        workspace_packages.push(package);
    }

    let mut nodes = workspace_package_names
        .iter()
        .map(|name| WorkspaceNode {
            name: name.clone(),
            kind: CrateKind::from_name(name),
        })
        .collect::<Vec<_>>();
    nodes.sort_by(|a, b| a.name.cmp(&b.name));

    let mut edges = Vec::new();
    for package in workspace_packages {
        let Some(from_name) = package.get("name").and_then(Value::as_str) else {
            continue;
        };
        let dependencies = package
            .get("dependencies")
            .and_then(Value::as_array)
            .cloned()
            .unwrap_or_default();
        let mut dependencies_by_name = BTreeMap::<String, DependencyKind>::new();
        for dependency in dependencies {
            let Some(dependency_name) = dependency.get("name").and_then(Value::as_str) else {
                continue;
            };
            if !workspace_package_names.contains(dependency_name) {
                continue;
            }
            let dependency_kind = DependencyKind::from_metadata_value(&dependency["kind"]);
            match dependency_kind {
                DependencyKind::Dev if !include_dev => continue,
                DependencyKind::Build if !include_build => continue,
                _ => {}
            }
            record_dependency_kind(
                &mut dependencies_by_name,
                dependency_name.to_string(),
                dependency_kind,
            );
        }
        edges.extend(
            dependencies_by_name
                .into_iter()
                .map(|(to, kind)| WorkspaceEdge {
                    from: from_name.to_string(),
                    to,
                    kind,
                }),
        );
    }
    edges.sort_by(|a, b| {
        a.from
            .cmp(&b.from)
            .then(a.to.cmp(&b.to))
            .then(a.kind.cmp(&b.kind))
    });

    Ok(WorkspaceGraph { nodes, edges })
}

fn record_dependency_kind(
    dependencies_by_name: &mut BTreeMap<String, DependencyKind>,
    dependency_name: String,
    dependency_kind: DependencyKind,
) {
    let entry = dependencies_by_name
        .entry(dependency_name)
        .or_insert(dependency_kind);
    if dependency_kind.priority() < entry.priority() {
        *entry = dependency_kind;
    }
}

fn resolve_output_path(root: &Path, output: Option<PathBuf>, format: CrateDagFormat) -> PathBuf {
    let output_path = output.unwrap_or_else(|| {
        PathBuf::from(format!(
            "{DEFAULT_OUTPUT_PREFIX_REL}.{}",
            format.extension()
        ))
    });
    if output_path.is_absolute() {
        output_path
    } else {
        root.join(output_path)
    }
}

fn ensure_dot_available(root: &Path) -> Result<()> {
    let mut command = Command::new("dot");
    command.arg("-V").current_dir(root);
    match command.output() {
        Ok(output) if output.status.success() || !output.stderr.is_empty() => Ok(()),
        _ => bail!(
            "graphviz `dot` is required for rendered output. Install graphviz or use `--format dot`."
        ),
    }
}

fn graphviz_engine(layout: PresentationLayout) -> &'static str {
    match layout {
        PresentationLayout::TopDown | PresentationLayout::LeftRight => "dot",
        PresentationLayout::Force => "fdp",
    }
}

fn display_output(root: &Path, output_path: &Path, format: CrateDagFormat) -> Result<()> {
    if format == CrateDagFormat::Dot {
        println!(
            "`--display` with `--format dot` does not render an interactive view. Use `--format html`, `--format svg`, or `--format png`."
        );
        return Ok(());
    }

    #[cfg(target_os = "macos")]
    let mut command = {
        let mut command = Command::new("open");
        command.arg(output_path);
        command
    };

    #[cfg(target_os = "linux")]
    let mut command = {
        let mut command = Command::new("xdg-open");
        command.arg(output_path);
        command
    };

    #[cfg(target_os = "windows")]
    let mut command = {
        let mut command = Command::new("cmd");
        command.arg("/C").arg("start").arg("").arg(output_path);
        command
    };

    #[cfg(not(any(target_os = "macos", target_os = "linux", target_os = "windows")))]
    {
        bail!("`--display` is not supported on this platform");
    }

    command.current_dir(root);
    let rendered = format!("{command:?}");
    let status = command
        .status()
        .with_context(|| format!("failed to launch output viewer: {rendered}"))?;
    if !status.success() {
        bail!(
            "failed to open {} with system viewer (status={status}): {rendered}",
            output_path.display()
        );
    }
    Ok(())
}

#[derive(Debug, Serialize)]
struct HtmlNode {
    id: String,
    label: String,
    kind: String,
    fill: String,
    text: String,
}

#[derive(Debug, Serialize)]
struct HtmlEdge {
    from: String,
    to: String,
    kind: String,
}

#[derive(Debug, Serialize)]
struct HtmlKind {
    key: String,
    label: String,
    fill: String,
    text: String,
    count: usize,
}

fn render_html_graph(
    graph: &WorkspaceGraph,
    initial_layout: PresentationLayout,
    shared_default_view: Option<&Value>,
) -> Result<String> {
    let nodes = graph
        .nodes
        .iter()
        .map(|node| {
            let colors = node.kind.colors();
            HtmlNode {
                id: node.name.clone(),
                label: display_crate_name(&node.name),
                kind: node.kind.label().to_string(),
                fill: colors.fill.to_string(),
                text: colors.font.to_string(),
            }
        })
        .collect::<Vec<_>>();
    let edges = graph
        .edges
        .iter()
        .map(|edge| HtmlEdge {
            from: edge.from.clone(),
            to: edge.to.clone(),
            kind: match edge.kind {
                DependencyKind::Normal => "normal".to_string(),
                DependencyKind::Build => "build".to_string(),
                DependencyKind::Dev => "dev".to_string(),
            },
        })
        .collect::<Vec<_>>();

    let counts_by_kind = graph
        .nodes
        .iter()
        .fold(BTreeMap::new(), |mut counts, node| {
            *counts.entry(node.kind).or_insert(0usize) += 1;
            counts
        });
    let kinds = CrateKind::legend_order()
        .iter()
        .filter_map(|kind| {
            let count = counts_by_kind.get(kind).copied().unwrap_or(0);
            if count == 0 {
                return None;
            }
            let colors = kind.colors();
            Some(HtmlKind {
                key: kind.label().to_string(),
                label: kind.label().to_string(),
                fill: colors.fill.to_string(),
                text: colors.font.to_string(),
                count,
            })
        })
        .collect::<Vec<_>>();

    let graph_json = serde_json::to_string(&serde_json::json!({
        "nodes": nodes,
        "edges": edges,
        "kinds": kinds,
        "initial_layout": initial_layout.as_key(),
        "layouts": ["top-down", "left-right", "force"],
        "shared_default_view": shared_default_view,
    }))
    .context("failed to serialize crate-dag html payload")?;

    let html_template = include_str!("crate_dag_template.html");

    Ok(html_template.replace("__GRAPH_JSON__", graph_json.as_str()))
}

fn render_dot_graph(graph: &WorkspaceGraph, layout: PresentationLayout) -> String {
    let mut dot = String::new();
    let rankdir = match layout {
        PresentationLayout::TopDown | PresentationLayout::Force => "TB",
        PresentationLayout::LeftRight => "LR",
    };
    let graph_layout = match layout {
        PresentationLayout::TopDown | PresentationLayout::LeftRight => "dot",
        PresentationLayout::Force => "fdp",
    };
    dot.push_str("digraph workspace_crate_dag {\n");
    dot.push_str(&format!("  rankdir={rankdir};\n"));
    dot.push_str(
        &format!(
            r#"  graph [layout="{graph_layout}", fontname="Helvetica", bgcolor="white", ranksep=1.0, nodesep=0.30, splines=true, overlap=false, labelloc=t, fontsize=14, label="Rumoca Workspace Crate Dependency DAG\n(edge direction: crate -> dependency, layout: {})"];"#,
            layout.as_key()
        ),
    );
    dot.push('\n');
    dot.push_str(
        r##"  node [shape=box, style="rounded,filled", fontname="Helvetica", fontsize=10, color="#334155", penwidth=1.1];"##,
    );
    dot.push('\n');
    dot.push_str(r##"  edge [color="#64748b", arrowhead=normal, arrowsize=1.0, penwidth=1.1];"##);
    dot.push('\n');

    for node in &graph.nodes {
        let colors = node.kind.colors();
        dot.push_str(&format!(
            r#"  "{}" [label="{}", fillcolor="{}", fontcolor="{}", tooltip="{}"];"#,
            dot_quote(&node.name),
            dot_quote(&display_crate_name(&node.name)),
            colors.fill,
            colors.font,
            node.kind
        ));
        dot.push('\n');
    }

    for edge in &graph.edges {
        dot.push_str(&format!(
            r#"  "{}" -> "{}"{};"#,
            dot_quote(&edge.from),
            dot_quote(&edge.to),
            edge.kind.dot_edge_attrs()
        ));
        dot.push('\n');
    }

    render_legend(&mut dot, graph);
    dot.push_str("}\n");
    dot
}

fn render_legend(dot: &mut String, graph: &WorkspaceGraph) {
    let used_kinds = graph
        .nodes
        .iter()
        .map(|node| node.kind)
        .collect::<BTreeSet<_>>();
    if used_kinds.is_empty() {
        return;
    }

    dot.push_str(r#"  subgraph cluster_legend {"#);
    dot.push('\n');
    dot.push_str(r#"    label="Crate Types";"#);
    dot.push('\n');
    dot.push_str(r#"    fontsize=11;"#);
    dot.push('\n');
    dot.push_str(r##"    color="#cbd5e1";"##);
    dot.push('\n');
    dot.push_str(r#"    style="rounded,dashed";"#);
    dot.push('\n');

    let mut previous_legend_node: Option<String> = None;
    let mut legend_index = 0usize;
    for kind in CrateKind::legend_order() {
        if !used_kinds.contains(kind) {
            continue;
        }
        let colors = kind.colors();
        let legend_node_name = format!("legend_{legend_index}");
        dot.push_str(&format!(
            r##"    "{}" [label="{}", shape=box, style="rounded,filled", fillcolor="{}", fontcolor="{}", color="#64748b"];"##,
            legend_node_name, kind, colors.fill, colors.font
        ));
        dot.push('\n');
        if let Some(previous) = previous_legend_node {
            dot.push_str(&format!(
                r#"    "{}" -> "{}" [style=invis];"#,
                previous, legend_node_name
            ));
            dot.push('\n');
        }
        previous_legend_node = Some(legend_node_name);
        legend_index += 1;
    }
    dot.push_str("  }\n");
}

fn display_crate_name(name: &str) -> String {
    name.strip_prefix("rumoca-").unwrap_or(name).to_string()
}

fn dot_quote(input: &str) -> String {
    input.replace('\\', "\\\\").replace('"', "\\\"")
}

#[cfg(test)]
mod tests {
    use super::{
        CrateKind, DependencyKind, PresentationLayout, WorkspaceEdge, WorkspaceGraph,
        WorkspaceNode, display_crate_name, record_dependency_kind, render_dot_graph,
        render_html_graph,
    };
    use std::collections::BTreeMap;

    #[test]
    fn crate_kind_classification_matches_workspace_conventions() {
        assert_eq!(CrateKind::from_name("rumoca"), CrateKind::EntryPoint);
        assert_eq!(CrateKind::from_name("rumoca-core"), CrateKind::Core);
        assert_eq!(CrateKind::from_name("rumoca-ir-ast"), CrateKind::Ir);
        assert_eq!(CrateKind::from_name("rumoca-phase-parse"), CrateKind::Phase);
        assert_eq!(CrateKind::from_name("rumoca-eval-ast"), CrateKind::Eval);
        assert_eq!(CrateKind::from_name("rumoca-compile"), CrateKind::Session);
        assert_eq!(CrateKind::from_name("rumoca-sim"), CrateKind::Sim);
        assert_eq!(CrateKind::from_name("rumoca-tool-dev"), CrateKind::Tool);
        assert_eq!(
            CrateKind::from_name("rumoca-bind-python"),
            CrateKind::Binding
        );
        assert_eq!(CrateKind::from_name("rumoca-bind-wasm"), CrateKind::Binding);
        assert_eq!(
            CrateKind::from_name("rumoca-contracts"),
            CrateKind::Contracts
        );
        assert_eq!(CrateKind::from_name("rumoca-test-msl"), CrateKind::Test);
        assert_eq!(CrateKind::from_name("unclassified"), CrateKind::Other);
    }

    #[test]
    fn dependency_kind_prefers_normal_over_dev_or_build() {
        let mut dependencies = BTreeMap::new();
        record_dependency_kind(
            &mut dependencies,
            "rumoca-core".to_string(),
            DependencyKind::Dev,
        );
        record_dependency_kind(
            &mut dependencies,
            "rumoca-core".to_string(),
            DependencyKind::Build,
        );
        record_dependency_kind(
            &mut dependencies,
            "rumoca-core".to_string(),
            DependencyKind::Normal,
        );
        assert_eq!(
            dependencies.get("rumoca-core"),
            Some(&DependencyKind::Normal)
        );
    }

    #[test]
    fn dot_render_includes_dev_and_build_edge_styles() {
        let graph = WorkspaceGraph {
            nodes: vec![
                WorkspaceNode {
                    name: "rumoca-tool-dev".to_string(),
                    kind: CrateKind::Tool,
                },
                WorkspaceNode {
                    name: "rumoca-core".to_string(),
                    kind: CrateKind::Core,
                },
            ],
            edges: vec![
                WorkspaceEdge {
                    from: "rumoca-tool-dev".to_string(),
                    to: "rumoca-core".to_string(),
                    kind: DependencyKind::Build,
                },
                WorkspaceEdge {
                    from: "rumoca-tool-dev".to_string(),
                    to: "rumoca-core".to_string(),
                    kind: DependencyKind::Dev,
                },
            ],
        };
        let dot = render_dot_graph(&graph, PresentationLayout::TopDown);
        assert!(dot.contains(r#"label="build""#));
        assert!(dot.contains(r#"label="dev""#));
        assert!(dot.contains(r#"label="tool-dev""#));
        assert!(dot.contains(r#"label="core""#));
    }

    #[test]
    fn display_name_strips_rumoca_prefix() {
        assert_eq!(display_crate_name("rumoca-phase-parse"), "phase-parse");
        assert_eq!(display_crate_name("rumoca"), "rumoca");
    }

    #[test]
    fn html_render_includes_kind_controls_and_payload() {
        let graph = WorkspaceGraph {
            nodes: vec![WorkspaceNode {
                name: "rumoca-tool-dev".to_string(),
                kind: CrateKind::Tool,
            }],
            edges: Vec::new(),
        };
        let html = render_html_graph(&graph, PresentationLayout::TopDown, None)
            .expect("html render should succeed");
        assert!(html.contains("Arrows show dependency direction"));
        assert!(html.contains("Auto Layout"));
        assert!(html.contains("\"kind\":\"tool\""));
        assert!(html.contains("\"label\":\"tool-dev\""));
        assert!(html.contains("\"initial_layout\":\"top-down\""));
    }

    #[test]
    fn html_render_is_offline_only() {
        let graph = WorkspaceGraph {
            nodes: vec![WorkspaceNode {
                name: "rumoca-tool-dev".to_string(),
                kind: CrateKind::Tool,
            }],
            edges: Vec::new(),
        };
        let html = render_html_graph(&graph, PresentationLayout::Force, None).expect("html render");

        let lowercase = html.to_ascii_lowercase();
        assert!(
            !lowercase.contains("http://"),
            "crate-dag html must not embed insecure remote URLs"
        );
        assert!(
            !lowercase.contains("https://"),
            "crate-dag html must not embed remote URLs"
        );
        assert!(
            !lowercase.contains("<script src="),
            "crate-dag html must inline scripts for offline use"
        );
        assert!(
            !lowercase.contains("<link rel=\"stylesheet\""),
            "crate-dag html must inline stylesheet assets for offline use"
        );
    }
}
