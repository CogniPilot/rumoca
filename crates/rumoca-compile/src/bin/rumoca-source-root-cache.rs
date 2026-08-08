//! Build a portable source-root cache for a detached Rumoca client.

use std::ffi::OsString;
use std::path::PathBuf;

use anyhow::{Context, Result, bail};
use rumoca_compile::source_roots::{
    PortableSourceRoot, PortableSourceRootCacheIssue, write_portable_source_root_cache,
};

fn main() -> Result<()> {
    let (output, roots) = parse_args(std::env::args_os().skip(1))?;
    let report = write_portable_source_root_cache(&output, &roots)?;
    for issue in report.issues {
        match issue {
            PortableSourceRootCacheIssue::MissingRoot { key, path } => {
                eprintln!(
                    "source-root cache: skipping missing root {key}={}",
                    path.display()
                );
            }
            PortableSourceRootCacheIssue::ParseFailed { uri, message } => {
                eprintln!("source-root cache: skipping {uri}: {message}");
            }
        }
    }
    if report.wrote_cache {
        eprintln!(
            "source-root cache: wrote {} definitions to {}",
            report.definition_count,
            output.display()
        );
    } else {
        eprintln!(
            "source-root cache: no Modelica sources parsed; did not write {}",
            output.display()
        );
    }
    Ok(())
}

fn parse_args(
    mut args: impl Iterator<Item = OsString>,
) -> Result<(PathBuf, Vec<PortableSourceRoot>)> {
    let output = args
        .next()
        .map(PathBuf::from)
        .context("usage: rumoca-source-root-cache OUTPUT [KEY PATH]...")?;
    let mut roots = Vec::new();
    while let Some(key) = args.next() {
        let path = args
            .next()
            .context("each source-root KEY must be followed by PATH")?;
        let key = key
            .into_string()
            .map_err(|_| anyhow::anyhow!("source-root KEY must be valid UTF-8"))?;
        roots.push(PortableSourceRoot::new(key, PathBuf::from(path))?);
    }
    if roots.is_empty() {
        bail!("at least one source-root KEY PATH pair is required");
    }
    Ok((output, roots))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_output_and_source_root_pairs() {
        let (output, roots) = parse_args(
            ["cache.bin.gz", "msl", "/tmp/msl", "cmm", "/tmp/cmm"]
                .into_iter()
                .map(OsString::from),
        )
        .expect("parse args");
        assert_eq!(output, PathBuf::from("cache.bin.gz"));
        assert_eq!(roots.len(), 2);
        assert_eq!(roots[0].key(), "msl");
        assert_eq!(roots[1].path(), PathBuf::from("/tmp/cmm"));
    }

    #[test]
    fn rejects_unpaired_source_root() {
        let error = parse_args(["cache.bin.gz", "msl"].into_iter().map(OsString::from))
            .expect_err("missing path must fail");
        assert!(error.to_string().contains("followed by PATH"));
    }
}
