use crate::architecture_hardening_support::{collect_rs_files, workspace_root};
use std::fs;
use std::path::Path;

fn is_generated(path: &Path) -> bool {
    path.components()
        .any(|component| component.as_os_str() == "generated")
}

fn comment_text(line: &str) -> Option<&str> {
    let trimmed = line.trim_start();
    let comment = trimmed.strip_prefix("//")?;
    Some(comment.trim_start_matches(['/', '!', ' ']))
}

fn contains_decimal_marker(comment: &str, label: &str) -> bool {
    comment.match_indices(label).any(|(at, _)| {
        let suffix = comment[at + label.len()..].trim_start();
        let integer_digits = suffix.chars().take_while(char::is_ascii_digit).count();
        if integer_digits == 0 {
            return false;
        }
        let suffix = &suffix[integer_digits..];
        let Some(fraction) = suffix.strip_prefix('.') else {
            return false;
        };
        fraction
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_digit())
    })
}

fn is_delivery_narration(comment: &str) -> bool {
    let lower = comment.to_ascii_lowercase();
    let checklist = comment
        .strip_prefix("- [")
        .is_some_and(|rest| matches!(rest.chars().next(), Some(' ' | 'x' | 'X')));
    contains_decimal_marker(comment, "Task ")
        || contains_decimal_marker(comment, "Phase ")
        || lower.contains("todo(phase")
        || lower.contains("roadmap")
        || checklist
}

#[test]
fn source_comments_describe_invariants_not_delivery_history() {
    let root = workspace_root();
    let mut files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut files);
    files.retain(|path| !is_generated(path));
    files.sort();

    let mut offenders = Vec::new();
    for path in files {
        let source = fs::read_to_string(&path).expect("read Rust source for comment audit");
        for (line_index, line) in source.lines().enumerate() {
            let Some(comment) = comment_text(line) else {
                continue;
            };
            if is_delivery_narration(comment) {
                let relative = path.strip_prefix(&root).unwrap_or(&path);
                offenders.push(format!(
                    "{}:{}: {}",
                    relative.display(),
                    line_index + 1,
                    comment
                ));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "source comments must state current invariants and ownership, not delivery history:\n{}",
        offenders.join("\n")
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detector_has_negative_controls_for_each_forbidden_shape() {
        for comment in [
            "Task 2.4: qualify names",
            "Phase 6.3: emit sparse rows",
            "TODO(phase 4b): add buffering",
            "Mirrors the implementation roadmap",
            "- [x] feature complete",
        ] {
            assert!(is_delivery_narration(comment), "missed `{comment}`");
        }
        for comment in [
            "Resolver Phase 2c owns contents",
            "Phase 2: select a tear variable",
            "MLS §10.4.2.1 array concatenation",
            "- typed failure classification",
        ] {
            assert!(!is_delivery_narration(comment), "rejected `{comment}`");
        }
    }
}
