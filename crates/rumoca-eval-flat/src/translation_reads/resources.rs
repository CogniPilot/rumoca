//! MLS §13.5 resource URIs and the C meaning of other file names.

use std::path::{Path, PathBuf};

use rustc_hash::FxHashMap;

/// The resource directory of each package a translation loaded, by its fully
/// qualified name.
///
/// MLS §13.5 interprets the authority of `modelica://A.B/path` as a fully
/// qualified package name and the path as relative to that package's
/// resource directory.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ResourceRoots {
    packages: FxHashMap<String, PathBuf>,
}

impl ResourceRoots {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record the resource directory of the package named `package`.
    pub fn insert(&mut self, package: impl Into<String>, directory: impl Into<PathBuf>) {
        self.packages.insert(package.into(), directory.into());
    }

    /// The path a foreign file-name argument names.
    ///
    /// A `modelica://` URI names its resource and a `file://` URI its path;
    /// both schemes compare case-insensitively and their paths are
    /// percent-decoded (RFC 3986). Any other name is a file name.
    pub fn file_name(&self, name: &str) -> Result<PathBuf, String> {
        if let Some(rest) = strip_scheme(name, "modelica://") {
            return self.modelica_resource(name, rest);
        }
        if let Some(rest) = strip_scheme(name, "file://") {
            let path = rest
                .strip_prefix("localhost")
                .filter(|path| path.starts_with('/'))
                .unwrap_or(rest);
            return percent_decoded(path)
                .map(PathBuf::from)
                .ok_or_else(|| format!("malformed URI \"{name}\""));
        }
        Ok(PathBuf::from(name))
    }

    fn modelica_resource(&self, uri: &str, rest: &str) -> Result<PathBuf, String> {
        let Some((authority, path)) = rest.split_once('/') else {
            return Err(format!("URI \"{uri}\" names a class, not a resource"));
        };
        let Some(root) = self.packages.get(authority) else {
            return Err(format!(
                "URI \"{uri}\" names package `{authority}`, which this translation did not load"
            ));
        };
        let path = percent_decoded(path).ok_or_else(|| format!("malformed URI \"{uri}\""))?;
        let mut resolved = root.clone();
        resolved.extend(path.split('/').filter(|segment| !segment.is_empty()));
        Ok(resolved)
    }
}

fn strip_scheme<'a>(name: &'a str, scheme: &str) -> Option<&'a str> {
    let prefix = name.get(..scheme.len())?;
    prefix
        .eq_ignore_ascii_case(scheme)
        .then(|| &name[scheme.len()..])
}

fn percent_decoded(text: &str) -> Option<String> {
    let mut bytes = Vec::with_capacity(text.len());
    let mut rest = text.as_bytes();
    while let Some((&byte, tail)) = rest.split_first() {
        if byte == b'%' {
            let hex = std::str::from_utf8(tail.get(..2)?).ok()?;
            bytes.push(u8::from_str_radix(hex, 16).ok()?);
            rest = &tail[2..];
        } else {
            bytes.push(byte);
            rest = tail;
        }
    }
    String::from_utf8(bytes).ok()
}

/// Windows canonicalization returns verbatim paths (`\\?\D:\x`, `\\?\UNC\host\x`),
/// which are not Modelica path names; map them back to the drive and UNC forms
/// a user would write. Input uses `/` separators.
pub(super) fn strip_verbatim_prefix(path: String) -> String {
    if let Some(unc) = path.strip_prefix("//?/UNC/") {
        format!("//{unc}")
    } else if let Some(rest) = path.strip_prefix("//?/") {
        rest.to_string()
    } else {
        path
    }
}

/// The Modelica path name of a host path: `/` separators and no Windows
/// verbatim prefix.
pub(super) fn path_name_text(path: &Path) -> String {
    strip_verbatim_prefix(path.to_string_lossy().replace('\\', "/"))
}

/// `ModelicaInternal_fullPathName`: the canonical absolute path of an
/// existing file or directory, otherwise the name joined to the current
/// directory unless it is absolute. A trailing separator of the name is kept.
pub(super) fn full_path_name(path: &Path, name: &str) -> String {
    let full = std::fs::canonicalize(path).unwrap_or_else(|_| {
        if path.is_absolute() {
            path.to_path_buf()
        } else {
            std::env::current_dir()
                .map(|cwd| cwd.join(path))
                .unwrap_or_else(|_| path.to_path_buf())
        }
    });
    let mut full = path_name_text(&full);
    if name.ends_with('/') && !full.ends_with('/') {
        full.push('/');
    }
    full
}
