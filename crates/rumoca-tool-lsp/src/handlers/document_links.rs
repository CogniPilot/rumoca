//! Document links handler for Modelica files.
//!
//! Provides clickable links for:
//! - HTTP/HTTPS URLs.
//! - Quoted file paths (relative or absolute).

use std::path::{Path, PathBuf};

use lsp_types::{DocumentLink, Position, Range, Url};

/// Handle document links request.
pub fn handle_document_links(source: &str, document_uri: &Url) -> Vec<DocumentLink> {
    let mut links = Vec::new();

    for (line_index, line) in source.lines().enumerate() {
        let line_u32 = line_index as u32;
        collect_url_links(line, line_u32, &mut links);
        collect_quoted_path_links(line, line_u32, document_uri, &mut links);
    }

    links
}

fn collect_url_links(line: &str, line_u32: u32, links: &mut Vec<DocumentLink>) {
    let mut cursor = 0usize;
    while let Some(start) = find_next_url_start(line, cursor) {
        let tail = &line[start..];
        let end_rel = tail
            .find(|c: char| c.is_whitespace() || matches!(c, ')' | ']' | '}' | '"' | '\'' | '<'))
            .unwrap_or(tail.len());
        let raw = &tail[..end_rel];
        let token = raw.trim_end_matches(['.', ',', ';', ':', '!', '?']);
        if token.is_empty() {
            cursor = start
                .saturating_add(end_rel.saturating_add(1))
                .min(line.len());
            continue;
        }
        if let Ok(target) = Url::parse(token) {
            let start_char = byte_to_character(line, start);
            let end_char = byte_to_character(line, start + token.len());
            links.push(DocumentLink {
                range: Range {
                    start: Position {
                        line: line_u32,
                        character: start_char,
                    },
                    end: Position {
                        line: line_u32,
                        character: end_char,
                    },
                },
                target: Some(target),
                tooltip: Some("Open link".to_string()),
                data: None,
            });
        }
        cursor = start
            .saturating_add(end_rel.saturating_add(1))
            .min(line.len());
    }
}

fn find_next_url_start(line: &str, from: usize) -> Option<usize> {
    if from >= line.len() {
        return None;
    }
    let http = line[from..].find("http://").map(|i| from + i);
    let https = line[from..].find("https://").map(|i| from + i);
    match (http, https) {
        (Some(a), Some(b)) => Some(a.min(b)),
        (Some(a), None) => Some(a),
        (None, Some(b)) => Some(b),
        (None, None) => None,
    }
}

fn collect_quoted_path_links(
    line: &str,
    line_u32: u32,
    document_uri: &Url,
    links: &mut Vec<DocumentLink>,
) {
    let bytes = line.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() {
        if bytes[i] != b'"' {
            i += 1;
            continue;
        }
        let start_quote = i;
        i += 1;
        while i < bytes.len() {
            if bytes[i] == b'\\' && i + 1 < bytes.len() {
                i += 2;
                continue;
            }
            if bytes[i] == b'"' {
                break;
            }
            i += 1;
        }
        if i >= bytes.len() {
            break;
        }
        let end_quote = i;
        let value = &line[start_quote + 1..end_quote];
        if looks_like_file_path(value)
            && let Some(target) = resolve_file_target(value, document_uri)
        {
            links.push(DocumentLink {
                range: Range {
                    start: Position {
                        line: line_u32,
                        character: byte_to_character(line, start_quote + 1),
                    },
                    end: Position {
                        line: line_u32,
                        character: byte_to_character(line, end_quote),
                    },
                },
                target: Some(target),
                tooltip: Some("Open file".to_string()),
                data: None,
            });
        }
        i = end_quote + 1;
    }
}

fn looks_like_file_path(value: &str) -> bool {
    if value.is_empty() || value.contains("://") {
        return false;
    }
    if value.starts_with("./")
        || value.starts_with("../")
        || value.starts_with("~/")
        || value.starts_with('/')
        || value.contains('\\')
        || value.contains('/')
    {
        return true;
    }
    let lower = value.to_ascii_lowercase();
    [
        ".mo", ".json", ".mat", ".csv", ".txt", ".xml", ".yaml", ".yml", ".toml",
    ]
    .iter()
    .any(|ext| lower.ends_with(ext))
}

fn resolve_file_target(value: &str, document_uri: &Url) -> Option<Url> {
    let path = if let Some(rest) = value.strip_prefix("~/") {
        PathBuf::from(std::env::var("HOME").ok()?).join(rest)
    } else {
        PathBuf::from(value)
    };

    let absolute = if path.is_absolute() {
        path
    } else {
        let base = file_path_from_url(document_uri)?;
        let parent = Path::new(&base).parent()?;
        parent.join(path)
    };

    url_from_file_path(absolute)
}

#[cfg(not(target_arch = "wasm32"))]
fn file_path_from_url(uri: &Url) -> Option<PathBuf> {
    uri.to_file_path().ok()
}

#[cfg(target_arch = "wasm32")]
fn file_path_from_url(uri: &Url) -> Option<PathBuf> {
    if uri.scheme() != "file" {
        return None;
    }
    let path = uri.path();
    if path.is_empty() {
        return None;
    }
    Some(PathBuf::from(path))
}

#[cfg(not(target_arch = "wasm32"))]
fn url_from_file_path(path: impl AsRef<Path>) -> Option<Url> {
    Url::from_file_path(path).ok()
}

#[cfg(target_arch = "wasm32")]
fn url_from_file_path(path: impl AsRef<Path>) -> Option<Url> {
    let raw = path.as_ref().to_string_lossy();
    if raw.is_empty() {
        return None;
    }
    let mut normalized = raw.replace('\\', "/");
    if !normalized.starts_with('/') {
        normalized.insert(0, '/');
    }
    Url::parse(&format!("file://{}", normalized)).ok()
}

fn byte_to_character(line: &str, byte_index: usize) -> u32 {
    line[..byte_index.min(line.len())].chars().count() as u32
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn detects_url_and_file_links() {
        let source = r#"
model M
  String docs = "https://example.com/docs";
  String local = "./Modelica/package.mo";
end M;
"#;
        let base = std::env::temp_dir().join("rumoca_lsp_document_links_test");
        let uri = Url::from_file_path(base.join("test.mo")).expect("uri");
        let expected_local =
            Url::from_file_path(PathBuf::from(&base).join("Modelica").join("package.mo"))
                .expect("expected local uri")
                .to_string();
        let links = handle_document_links(source, &uri);
        assert!(
            links.iter().any(|l| {
                l.target
                    .as_ref()
                    .is_some_and(|t| t.as_str().starts_with("https://example.com/docs"))
            }),
            "missing https link: {links:?}"
        );
        assert!(
            links.iter().any(|l| {
                l.target
                    .as_ref()
                    .is_some_and(|t| t.as_str() == expected_local)
            }),
            "missing file link: {links:?}"
        );
    }

    #[test]
    fn detects_plain_comment_url_without_panicking() {
        let source = "// https://example.com/docs\n";
        let uri = Url::from_file_path(std::env::temp_dir().join("comment-links.mo")).expect("uri");
        let links = handle_document_links(source, &uri);
        assert_eq!(links.len(), 1, "expected one link from the comment URL");
        assert!(
            links[0]
                .target
                .as_ref()
                .is_some_and(|target| target.as_str().starts_with("https://example.com/docs")),
            "comment URL should become a document link: {links:?}"
        );
    }
}
