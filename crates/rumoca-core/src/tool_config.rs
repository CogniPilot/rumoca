//! Filesystem-neutral ownership shared by source tools' configuration loaders.

use std::error::Error;
use std::fmt;
use std::io;
use std::path::{Path, PathBuf};

/// The common read/parse failure shape for one tool-owned config schema.
#[derive(Debug)]
pub enum ToolConfigError<ParseError> {
    /// The selected configuration file could not be read.
    ReadError(io::Error),
    /// The selected configuration file did not decode as the tool's schema.
    ParseError(ParseError),
}

impl<ParseError: fmt::Display> fmt::Display for ToolConfigError<ParseError> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ReadError(error) => write!(formatter, "failed to read config file: {error}"),
            Self::ParseError(error) => write!(formatter, "failed to parse config file: {error}"),
        }
    }
}

impl<ParseError> Error for ToolConfigError<ParseError>
where
    ParseError: Error + 'static,
{
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::ReadError(error) => Some(error),
            Self::ParseError(error) => Some(error),
        }
    }
}

impl<ParseError> From<io::Error> for ToolConfigError<ParseError> {
    fn from(error: io::Error) -> Self {
        Self::ReadError(error)
    }
}

/// Find the nearest ancestor file whose name appears in caller priority order.
#[must_use]
pub fn find_nearest_named_config(start_dir: &Path, names: &[&str]) -> Option<PathBuf> {
    let mut current = start_dir.to_path_buf();
    loop {
        for name in names {
            let candidate = current.join(name);
            if candidate.is_file() {
                return Some(candidate);
            }
        }
        if !current.pop() {
            return None;
        }
    }
}
