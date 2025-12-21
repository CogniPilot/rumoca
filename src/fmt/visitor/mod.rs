//! Format visitor for AST traversal.

mod components;
mod equations;
mod expressions;
mod visitor_impl;

use super::FormatOptions;
use crate::ir::ast::Import;

/// A comment with its location for reinsertion during formatting
#[derive(Debug, Clone)]
pub struct CommentInfo {
    pub text: String,
    pub line: u32,
}

/// AST-based formatter that implements the Visitor pattern
pub struct FormatVisitor {
    pub options: FormatOptions,
    pub indent_str: String,
    pub output: String,
    pub indent_level: usize,
    /// Comments to be inserted, sorted by line number
    comments: Vec<CommentInfo>,
    /// Index of next comment to potentially insert
    next_comment_idx: usize,
    /// Current output line number (1-based to match source)
    current_line: u32,
    /// Original source text for extracting exact token text
    pub source: Option<String>,
}

impl FormatVisitor {
    pub fn new(options: &FormatOptions) -> Self {
        let indent_str = if options.use_tabs {
            "\t".to_string()
        } else {
            " ".repeat(options.indent_size)
        };
        Self {
            options: options.clone(),
            indent_str,
            output: String::new(),
            indent_level: 0,
            comments: Vec::new(),
            next_comment_idx: 0,
            current_line: 1,
            source: None,
        }
    }

    pub fn with_comments_and_source(
        options: &FormatOptions,
        comments: Vec<CommentInfo>,
        source: &str,
    ) -> Self {
        let indent_str = if options.use_tabs {
            "\t".to_string()
        } else {
            " ".repeat(options.indent_size)
        };
        Self {
            options: options.clone(),
            indent_str,
            output: String::new(),
            indent_level: 0,
            comments,
            next_comment_idx: 0,
            current_line: 1,
            source: Some(source.to_string()),
        }
    }

    /// Emit any comments that should appear before the given source line
    pub fn emit_comments_before_line(&mut self, target_line: u32) {
        while self.next_comment_idx < self.comments.len() {
            let comment = &self.comments[self.next_comment_idx];
            if comment.line < target_line {
                // Emit this comment (trim to remove any trailing newlines from the token)
                self.output.push_str(&self.indent());
                self.output.push_str(comment.text.trim_end());
                self.output.push('\n');
                self.current_line += 1;
                self.next_comment_idx += 1;
            } else {
                break;
            }
        }
    }

    /// Get trailing comments for a specific line (same line, after the code)
    /// Returns the comments as a string to append, and marks them as consumed
    fn get_trailing_comments(&mut self, source_line: u32) -> String {
        let mut trailing = String::new();
        while self.next_comment_idx < self.comments.len() {
            let comment = &self.comments[self.next_comment_idx];
            if comment.line == source_line {
                // This is a trailing comment on the same line
                trailing.push(' ');
                trailing.push_str(comment.text.trim_end());
                self.next_comment_idx += 1;
            } else if comment.line > source_line {
                break;
            } else {
                // Comments before this line should have been emitted already
                self.next_comment_idx += 1;
            }
        }
        trailing
    }

    /// Emit any remaining comments at the end of output
    pub fn emit_remaining_comments(&mut self) {
        while self.next_comment_idx < self.comments.len() {
            let comment = &self.comments[self.next_comment_idx];
            self.output.push_str(&self.indent());
            self.output.push_str(comment.text.trim_end());
            self.output.push('\n');
            self.next_comment_idx += 1;
        }
    }

    pub fn indent(&self) -> String {
        self.indent_str.repeat(self.indent_level)
    }

    pub fn write(&mut self, s: &str) {
        self.output.push_str(s);
    }

    pub fn writeln(&mut self, s: &str) {
        self.output.push_str(&self.indent());
        self.output.push_str(s);
        self.output.push('\n');
    }

    /// Write a line with trailing comments from the source line
    pub fn writeln_with_trailing(&mut self, s: &str, source_line: u32) {
        self.output.push_str(&self.indent());
        self.output.push_str(s);
        // Check for and append any trailing comments from the same source line
        let trailing = self.get_trailing_comments(source_line);
        self.output.push_str(&trailing);
        self.output.push('\n');
    }

    /// Write a pre-formatted string that may end with newline, inserting trailing comments
    /// before the final newline. Used for equations/statements that already include formatting.
    pub fn write_with_trailing(&mut self, formatted: &str, source_line: u32) {
        // Check if the formatted string ends with newline
        if let Some(without_newline) = formatted.strip_suffix('\n') {
            self.output.push_str(without_newline);
            let trailing = self.get_trailing_comments(source_line);
            self.output.push_str(&trailing);
            self.output.push('\n');
        } else {
            self.output.push_str(formatted);
            let trailing = self.get_trailing_comments(source_line);
            self.output.push_str(&trailing);
        }
    }

    pub fn format_import(&self, import: &Import) -> String {
        match import {
            Import::Qualified { path, .. } => format!("import {};", path),
            Import::Renamed { alias, path, .. } => {
                format!("import {} = {};", alias.text, path)
            }
            Import::Unqualified { path, .. } => format!("import {}.*;", path),
            Import::Selective { path, names, .. } => {
                let name_list: Vec<&str> = names.iter().map(|t| t.text.as_str()).collect();
                format!("import {}.{{{}}};", path, name_list.join(", "))
            }
        }
    }
}
