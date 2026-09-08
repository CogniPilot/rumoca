use super::*;

#[cfg(target_os = "linux")]
#[test]
fn executing_image_identity_survives_path_replacement() {
    const CHILD: &str = "CHARON_FACT_IMAGE_REPLACEMENT_CHILD";
    if std::env::var_os(CHILD).is_some() {
        let before = executing_driver_digest().expect("executing image before rename");
        let current = std::env::current_exe().expect("child executable path");
        let replacement = current.with_extension("replacement");
        std::fs::write(&replacement, b"different executable bytes").expect("owned replacement");
        std::fs::rename(&replacement, current).expect("replace only the child executable copy");
        assert_eq!(
            executing_driver_digest().expect("executing image after rename"),
            before
        );
        println!("RUNNING_IMAGE_IDENTITY_CHECKED");
        return;
    }
    let directory = tempfile::tempdir().expect("owned child directory");
    let child = directory.path().join("image-test");
    std::fs::copy(std::env::current_exe().expect("test executable"), &child)
        .expect("copy test image");
    let output = std::process::Command::new(child)
        .env(CHILD, "1")
        .args([
            "--exact",
            "artifact::tests::executing_image_identity_survives_path_replacement",
            "--nocapture",
        ])
        .output()
        .expect("replacement child");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        String::from_utf8_lossy(&output.stdout).contains("RUNNING_IMAGE_IDENTITY_CHECKED"),
        "the exact child check did not execute: {}",
        String::from_utf8_lossy(&output.stdout)
    );
}
