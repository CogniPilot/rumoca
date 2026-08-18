use std::process::Command;

use super::architecture_hardening_support::workspace_root;

fn forbidden_commit_message_reason(message: &str) -> Option<&'static str> {
    let lowercase = message.to_ascii_lowercase();
    if lowercase.contains("codex") {
        return Some("named assistant reference");
    }
    if lowercase.contains("claude") {
        return Some("named assistant reference");
    }

    message.lines().find_map(|line| {
        let lowercase = line.trim_start().to_ascii_lowercase();
        let is_coauthor = lowercase.starts_with("co-authored-by:");
        let names_ai_provider = ["anthropic", "openai", "ai assistant"]
            .iter()
            .any(|term| lowercase.contains(term));
        (is_coauthor && names_ai_provider).then_some("AI co-author attribution")
    })
}

fn reachable_commit_messages() -> Vec<(String, String)> {
    let root = workspace_root();
    let shallow = Command::new("git")
        .args(["rev-parse", "--is-shallow-repository"])
        .current_dir(&root)
        .output()
        .expect("run git rev-parse for commit-message policy");
    assert!(
        shallow.status.success(),
        "commit-message architecture policy requires a Git worktree: {}",
        String::from_utf8_lossy(&shallow.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&shallow.stdout).trim(),
        "false",
        "commit-message architecture policy requires full history; configure checkout with fetch-depth: 0"
    );

    let output = Command::new("git")
        .args(["log", "--format=%H%x1f%B%x1e", "HEAD"])
        .current_dir(root)
        .output()
        .expect("run git log for commit-message policy");
    assert!(
        output.status.success(),
        "git log failed for commit-message architecture policy: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    String::from_utf8_lossy(&output.stdout)
        .split('\x1e')
        .filter_map(|record| {
            let record = record.trim_matches('\n');
            let (commit, message) = record.split_once('\x1f')?;
            Some((commit.to_string(), message.to_string()))
        })
        .collect()
}

/// SPEC_0025 §6: commit history attributes the work to human authors and must
/// not contain named assistant references or AI co-author trailers.
#[test]
fn test_reachable_commit_messages_have_no_ai_attribution() {
    let offenders = reachable_commit_messages()
        .into_iter()
        .filter_map(|(commit, message)| {
            forbidden_commit_message_reason(&message).map(|reason| {
                let subject = message.lines().next().unwrap_or("<empty subject>");
                format!("{} ({reason}): {subject}", &commit[..12])
            })
        })
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "SPEC_0025 §6 forbids AI attribution in commit messages: {offenders:#?}"
    );
}

#[test]
fn test_commit_message_policy_detects_named_references_and_ai_trailers() {
    assert!(forbidden_commit_message_reason("Ordinary human-authored change").is_none());
    assert!(forbidden_commit_message_reason("Ask Codex to review").is_some());
    assert!(forbidden_commit_message_reason("Claude-Session: example").is_some());
    assert!(
        forbidden_commit_message_reason(
            "Change\n\nCo-Authored-By: Example <noreply@anthropic.com>"
        )
        .is_some()
    );
}
