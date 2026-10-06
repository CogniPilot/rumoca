//! The baked identity matches the checkout the crate is tested from.
use std::process::Command;

fn git(args: &[&str]) -> Option<String> {
    let output = Command::new("git")
        .args(args)
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .output()
        .ok()?;
    output
        .status
        .success()
        .then(|| String::from_utf8(output.stdout).ok())
        .flatten()
        .map(|text| text.trim().to_owned())
}

#[test]
fn build_identity_and_merge_parents_track_the_current_checkout() {
    // Outside a git checkout (a source package) the build cannot know its
    // commit and must say so rather than invent one.
    let Some(commit) = git(&["rev-parse", "--short=12", "HEAD"]) else {
        assert_eq!(super::build_identity(), None);
        assert_eq!(super::build_merge_parents(), None);
        return;
    };
    let identity = super::build_identity().expect("a git checkout has an identity");
    assert_eq!(identity.trim_end_matches("-dirty"), commit);
    // The commit object names its parents even in a shallow checkout.
    let object = git(&["cat-file", "commit", "HEAD"]).unwrap();
    let parents = object
        .lines()
        .take_while(|line| !line.is_empty())
        .filter_map(|line| line.strip_prefix("parent "))
        .map(|parent| &parent[..12])
        .collect::<Vec<_>>();
    let expected: &[&str] = if parents.len() > 1 { &parents } else { &[] };
    assert_eq!(super::build_merge_parents(), Some(expected));
}
