//! Document Links handler for Modelica files.
//!
//! Provides clickable links for:
//! - File paths in annotations (e.g., Modelica.Icons, documentation URIs)
//! - Import statements (link to imported packages)
//! - Within statements (link to parent package)

use std::collections::HashMap;

use lsp_types::{DocumentLink, DocumentLinkParams, Position, Range, Uri};

use crate::lsp::utils::parse_document;

/// Handle document links request
pub fn handle_document_links(
    documents: &HashMap<Uri, String>,
    params: DocumentLinkParams,
) -> Option<Vec<DocumentLink>> {
    let uri = &params.text_document.uri;
    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let mut links = Vec::new();

    // Parse the document to find imports and other linkable elements
    if let Some(ast) = parse_document(text, path) {
        // Add links for within clause
        if let Some(ref within) = ast.within {
            let within_str = within.to_string();
            // Find the within statement in the text
            for (line_num, line) in text.lines().enumerate() {
                if line.trim().starts_with("within ") {
                    if let Some(start) = line.find(&within_str) {
                        links.push(DocumentLink {
                            range: Range {
                                start: Position {
                                    line: line_num as u32,
                                    character: start as u32,
                                },
                                end: Position {
                                    line: line_num as u32,
                                    character: (start + within_str.len()) as u32,
                                },
                            },
                            target: None, // Would need workspace support to resolve
                            tooltip: Some(format!("Go to package {}", within_str)),
                            data: None,
                        });
                    }
                    break;
                }
            }
        }

        // Add links for imports
        for class in ast.class_list.values() {
            collect_import_links(class, text, &mut links);
        }
    }

    // Find documentation URIs in annotations
    collect_annotation_links(text, &mut links);

    // Find file path references in strings
    collect_file_path_links(text, uri, &mut links);

    Some(links)
}

/// Collect links from imports in a class
fn collect_import_links(
    class: &crate::ir::ast::ClassDefinition,
    text: &str,
    links: &mut Vec<DocumentLink>,
) {
    for import in &class.imports {
        let import_path = match import {
            crate::ir::ast::Import::Qualified { path, .. } => path.to_string(),
            crate::ir::ast::Import::Renamed { path, .. } => path.to_string(),
            crate::ir::ast::Import::Unqualified { path, .. } => path.to_string(),
            crate::ir::ast::Import::Selective { path, .. } => path.to_string(),
        };

        // Find the import in the text
        for (line_num, line) in text.lines().enumerate() {
            if line.trim().starts_with("import ")
                && line.contains(&import_path)
                && let Some(start) = line.find(&import_path)
            {
                links.push(DocumentLink {
                    range: Range {
                        start: Position {
                            line: line_num as u32,
                            character: start as u32,
                        },
                        end: Position {
                            line: line_num as u32,
                            character: (start + import_path.len()) as u32,
                        },
                    },
                    target: None, // Would need workspace support to resolve
                    tooltip: Some(format!("Go to {}", import_path)),
                    data: None,
                });
            }
        }
    }

    // Recursively check nested classes
    for nested in class.classes.values() {
        collect_import_links(nested, text, links);
    }
}

/// Collect links from annotations (documentation URIs, file references)
fn collect_annotation_links(text: &str, links: &mut Vec<DocumentLink>) {
    for (line_num, line) in text.lines().enumerate() {
        // Look for Documentation annotation with URI
        // Pattern: Documentation(info="<html>...</html>", revisions="...")
        if line.contains("Documentation") {
            // Find URIs in the documentation
            collect_uris_in_line(line, line_num, links);
        }

        // Look for Diagram/Icon annotations with file references
        // Pattern: fileName="path/to/file.svg"
        if line.contains("fileName")
            && let Some(file_link) = extract_filename_link(line, line_num)
        {
            links.push(file_link);
        }

        // Look for uses annotation
        // Pattern: uses(Modelica(version="4.0.0"))
        if line.contains("uses(") {
            collect_uses_links(line, line_num, links);
        }
    }
}

/// Collect URI references in a line
fn collect_uris_in_line(line: &str, line_num: usize, links: &mut Vec<DocumentLink>) {
    // Find http:// and https:// URLs
    let patterns = ["http://", "https://", "file://"];

    for pattern in &patterns {
        let mut search_pos = 0;
        while let Some(start) = line[search_pos..].find(pattern) {
            let abs_start = search_pos + start;

            // Find the end of the URL (space, quote, or angle bracket)
            let url_start = abs_start;
            let remaining = &line[url_start..];
            let url_end = remaining
                .find(|c: char| c.is_whitespace() || c == '"' || c == '\'' || c == '>' || c == '<')
                .unwrap_or(remaining.len());

            let url = &line[url_start..url_start + url_end];

            if is_valid_url(url) {
                links.push(DocumentLink {
                    range: Range {
                        start: Position {
                            line: line_num as u32,
                            character: url_start as u32,
                        },
                        end: Position {
                            line: line_num as u32,
                            character: (url_start + url_end) as u32,
                        },
                    },
                    target: url.parse().ok(),
                    tooltip: Some("Open URL".to_string()),
                    data: None,
                });
            }

            search_pos = url_start + url_end;
            if search_pos >= line.len() {
                break;
            }
        }
    }
}

/// Extract a filename link from a line
fn extract_filename_link(line: &str, line_num: usize) -> Option<DocumentLink> {
    // Look for fileName="..." pattern
    let pattern = "fileName=\"";
    let start = line.find(pattern)?;
    let value_start = start + pattern.len();
    let remaining = &line[value_start..];
    let value_end = remaining.find('"')?;
    let filename = &remaining[..value_end];

    // Only create link for relative paths or absolute file paths
    if !filename.is_empty() && !filename.starts_with("http") {
        return Some(DocumentLink {
            range: Range {
                start: Position {
                    line: line_num as u32,
                    character: value_start as u32,
                },
                end: Position {
                    line: line_num as u32,
                    character: (value_start + value_end) as u32,
                },
            },
            target: None, // Would need to resolve relative to document
            tooltip: Some(format!("Open {}", filename)),
            data: None,
        });
    }

    None
}

/// Collect links from uses() annotation
fn collect_uses_links(line: &str, line_num: usize, links: &mut Vec<DocumentLink>) {
    // Simple pattern matching for uses(LibraryName(...))
    if let Some(uses_start) = line.find("uses(") {
        let remaining = &line[uses_start + 5..];
        if let Some(paren_end) = remaining.find(')') {
            // Extract library names (simplified - doesn't handle nested parens well)
            let uses_content = &remaining[..paren_end];

            // Split by comma to get individual library references
            for lib_ref in uses_content.split(',') {
                let lib_ref = lib_ref.trim();
                // Get the library name (before the version paren)
                let lib_name = if let Some(paren) = lib_ref.find('(') {
                    lib_ref[..paren].trim()
                } else {
                    lib_ref
                };

                if !lib_name.is_empty()
                    && let Some(lib_pos) = line.find(lib_name)
                {
                    links.push(DocumentLink {
                        range: Range {
                            start: Position {
                                line: line_num as u32,
                                character: lib_pos as u32,
                            },
                            end: Position {
                                line: line_num as u32,
                                character: (lib_pos + lib_name.len()) as u32,
                            },
                        },
                        target: None,
                        tooltip: Some(format!("Go to library {}", lib_name)),
                        data: None,
                    });
                }
            }
        }
    }
}

/// Collect file path links in string literals
fn collect_file_path_links(text: &str, _base_uri: &Uri, links: &mut Vec<DocumentLink>) {
    // Look for string literals that look like file paths
    for (line_num, line) in text.lines().enumerate() {
        let mut in_string = false;
        let mut string_start = 0;
        let chars: Vec<char> = line.chars().collect();

        for (i, &c) in chars.iter().enumerate() {
            if c == '"' && (i == 0 || chars[i - 1] != '\\') {
                if in_string {
                    // End of string
                    let content = &line[string_start + 1..i];
                    if looks_like_file_path(content) {
                        links.push(DocumentLink {
                            range: Range {
                                start: Position {
                                    line: line_num as u32,
                                    character: (string_start + 1) as u32,
                                },
                                end: Position {
                                    line: line_num as u32,
                                    character: i as u32,
                                },
                            },
                            target: None, // Would need to resolve relative to document
                            tooltip: Some(format!("Open {}", content)),
                            data: None,
                        });
                    }
                } else {
                    // Start of string
                    string_start = i;
                }
                in_string = !in_string;
            }
        }
    }
}

/// Check if a string looks like a file path
fn looks_like_file_path(s: &str) -> bool {
    // Check for common file path patterns
    let file_extensions = [
        ".mo", ".csv", ".mat", ".txt", ".json", ".xml", ".svg", ".png", ".jpg",
    ];

    // Check if it ends with a known extension
    if file_extensions.iter().any(|ext| s.ends_with(ext)) {
        return true;
    }

    // Check for path separators and common patterns
    if (s.contains('/') || s.contains('\\')) && !s.contains(' ') && s.len() < 200 {
        return true;
    }

    false
}

/// Check if a string is a valid URL
fn is_valid_url(s: &str) -> bool {
    (s.starts_with("http://") || s.starts_with("https://") || s.starts_with("file://"))
        && s.len() > 10
        && !s.contains(char::is_whitespace)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_looks_like_file_path() {
        assert!(looks_like_file_path("path/to/file.mo"));
        assert!(looks_like_file_path("data.csv"));
        assert!(looks_like_file_path("resources/icon.svg"));
        assert!(!looks_like_file_path("hello world"));
        assert!(!looks_like_file_path("justtext"));
    }

    #[test]
    fn test_is_valid_url() {
        assert!(is_valid_url("https://example.com/path"));
        assert!(is_valid_url("http://localhost:8080"));
        assert!(!is_valid_url("not a url"));
        assert!(!is_valid_url("http://"));
    }

    #[test]
    fn test_collect_uris() {
        let line = r#"info="See <a href=\"https://example.com\">docs</a>""#;
        let mut links = Vec::new();
        collect_uris_in_line(line, 0, &mut links);
        assert!(!links.is_empty());
    }
}
