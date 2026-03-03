//! VCG extraction and side-effect function classification for equation flattening.

use super::{FlattenedEquations, build_qualified_name};
use rumoca_ir_ast as ast;

/// Extract VCG data from a Connections.* function call equation.
///
/// Handles `Connections.root(a)`, `Connections.branch(a, b)`, and
/// `Connections.potentialRoot(a, priority)` (MLS §9.4).
pub(super) fn extract_vcg_data_from_function_call(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
) -> FlattenedEquations {
    let mut result = FlattenedEquations::default();
    if is_connections_root_call(comp)
        && let Some(ref_arg) = args.first()
        && let ast::Expression::ComponentReference(cr) = ref_arg
    {
        result.definite_roots.push(build_qualified_name(prefix, cr));
    }
    if is_connections_branch_call(comp)
        && args.len() >= 2
        && let ast::Expression::ComponentReference(cr_a) = &args[0]
        && let ast::Expression::ComponentReference(cr_b) = &args[1]
    {
        let path_a = build_qualified_name(prefix, cr_a);
        let path_b = build_qualified_name(prefix, cr_b);
        result.branches.push((path_a, path_b));
    }
    if is_connections_potential_root_call(comp)
        && let Some(ref_arg) = args.first()
        && let ast::Expression::ComponentReference(cr) = ref_arg
    {
        let priority = extract_potential_root_priority(args);
        result
            .potential_roots
            .push((build_qualified_name(prefix, cr), priority));
    }
    result
}

/// Check if a function call is side-effect-only (doesn't contribute equations).
///
/// MLS §8.3.5 explicitly allows these in when-equations:
/// - assert() - runtime condition checking
/// - terminate() - simulation termination
///
/// Additionally, I/O functions like print() and Streams functions are side-effects:
/// - print() - console output
/// - Modelica.Utilities.Streams.* - file I/O
///
/// Connection graph functions (MLS §9.4) establish structure but don't contribute equations:
/// - Connections.branch() - creates a required edge in the virtual connection graph
/// - Connections.root() - defines a definite root node
/// - Connections.potentialRoot() - defines a potential root with priority
pub(super) fn is_side_effect_only_function(comp: &ast::ComponentReference) -> bool {
    // Get the last part of the component reference (the actual function name).
    let func_name = comp
        .parts
        .last()
        .map(|p| p.ident.text.as_ref())
        .unwrap_or("");

    // Check for specific side-effect-only functions.
    matches!(
        func_name,
        "assert" | "terminate" | "print" | "close" | "readLine" | "error"
    ) || is_streams_utility_function(comp)
        || is_connections_graph_function(comp)
}

/// Check if a function is from Modelica.Utilities.Streams (file I/O).
fn is_streams_utility_function(comp: &ast::ComponentReference) -> bool {
    // Check if the path contains "Streams" as a component.
    comp.parts
        .iter()
        .any(|p| p.ident.text.as_ref() == "Streams")
}

/// Check if a function is a Connections.* graph function (MLS §9.4).
/// These establish the virtual connection graph structure for overconstrained connectors
/// but don't contribute equations to the DAE system.
fn is_connections_graph_function(comp: &ast::ComponentReference) -> bool {
    // Check if the path is Connections.branch, Connections.root, Connections.potentialRoot,
    // Connections.isRoot, or Connections.rooted.
    if comp.parts.len() >= 2 {
        let parent = comp
            .parts
            .first()
            .map(|p| p.ident.text.as_ref())
            .unwrap_or("");
        let func = comp
            .parts
            .last()
            .map(|p| p.ident.text.as_ref())
            .unwrap_or("");
        parent == "Connections"
            && matches!(
                func,
                "branch" | "root" | "potentialRoot" | "isRoot" | "rooted"
            )
    } else {
        false
    }
}

/// Check if a function call is Connections.root() (MLS §9.4.1).
/// This declares the connector as a definite root for overconstrained types.
fn is_connections_root_call(comp: &ast::ComponentReference) -> bool {
    is_connections_call(comp, "root")
}

/// Check if a function call is Connections.branch(a, b) (MLS §9.4).
/// This creates a required edge in the virtual connection graph.
fn is_connections_branch_call(comp: &ast::ComponentReference) -> bool {
    is_connections_call(comp, "branch")
}

/// Check if a function call is Connections.potentialRoot(a, priority) (MLS §9.4).
/// This declares the connector as a potential root with a given priority.
fn is_connections_potential_root_call(comp: &ast::ComponentReference) -> bool {
    is_connections_call(comp, "potentialRoot")
}

/// Check if a function call is Connections.<func_name>().
fn is_connections_call(comp: &ast::ComponentReference, func_name: &str) -> bool {
    if comp.parts.len() >= 2 {
        let parent = comp
            .parts
            .first()
            .map(|p| p.ident.text.as_ref())
            .unwrap_or("");
        let func = comp
            .parts
            .last()
            .map(|p| p.ident.text.as_ref())
            .unwrap_or("");
        parent == "Connections" && func == func_name
    } else {
        false
    }
}

/// Extract priority from Connections.potentialRoot(a, priority) call.
/// Default priority is 0 if not specified (MLS §9.4).
fn extract_potential_root_priority(args: &[ast::Expression]) -> i64 {
    if args.len() >= 2
        && let ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
        } = &args[1]
    {
        return token.text.parse().unwrap_or(0);
    }
    0
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_ast as ast;
    use std::sync::Arc;

    fn cref(path: &str) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: crate::path_utils::parse_path_with_indices(path)
                .into_iter()
                .map(|part| ast::ComponentRefPart {
                    ident: ast::Token {
                        text: Arc::from(part),
                        location: ast::Location::default(),
                        token_number: 0,
                        token_type: 0,
                    },
                    subs: None,
                })
                .collect(),
            def_id: None,
        }
    }

    fn cref_expr(path: &str) -> ast::Expression {
        ast::Expression::ComponentReference(cref(path))
    }

    fn uint_expr(value: i64) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: ast::Token {
                text: Arc::from(value.to_string()),
                location: ast::Location::default(),
                token_number: 0,
                token_type: 0,
            },
        }
    }

    #[test]
    fn test_is_side_effect_only_function_handles_connections_streams_and_print() {
        assert!(is_side_effect_only_function(&cref("Connections.root")));
        assert!(is_side_effect_only_function(&cref(
            "Modelica.Utilities.Streams.print"
        )));
        assert!(is_side_effect_only_function(&cref("print")));
        assert!(!is_side_effect_only_function(&cref("sin")));
    }

    #[test]
    fn test_extract_vcg_data_from_connections_calls() {
        let prefix = ast::QualifiedName::new();

        let root = extract_vcg_data_from_function_call(
            &cref("Connections.root"),
            &[cref_expr("a.p")],
            &prefix,
        );
        assert_eq!(root.definite_roots, vec!["a.p".to_string()]);

        let branch = extract_vcg_data_from_function_call(
            &cref("Connections.branch"),
            &[cref_expr("a.p"), cref_expr("b.n")],
            &prefix,
        );
        assert_eq!(
            branch.branches,
            vec![("a.p".to_string(), "b.n".to_string())]
        );

        let potential = extract_vcg_data_from_function_call(
            &cref("Connections.potentialRoot"),
            &[cref_expr("a.p"), uint_expr(7)],
            &prefix,
        );
        assert_eq!(potential.potential_roots, vec![("a.p".to_string(), 7)]);
    }

    #[test]
    fn test_extract_vcg_data_default_priority_is_zero() {
        let prefix = ast::QualifiedName::new();
        let potential = extract_vcg_data_from_function_call(
            &cref("Connections.potentialRoot"),
            &[cref_expr("a.p")],
            &prefix,
        );
        assert_eq!(potential.potential_roots, vec![("a.p".to_string(), 0)]);
    }
}
