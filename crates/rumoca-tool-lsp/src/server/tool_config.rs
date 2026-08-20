//! SPEC_0018 editor tool configuration.
//!
//! The CLI resolves `.rumoca_fmt.toml` / `.rumoca_lint.toml` by walking up from
//! the target file (`rumoca fmt`, `rumoca lint`). Before this module the editor
//! did not: it hardcoded `FormatOptions::default()` and `LintOptions::default()`,
//! so formatting a file in a canonical-profile project through the editor
//! produced different bytes than formatting it from the shell, and lint rules
//! disabled for a project still fired as editor diagnostics.
//!
//! Resolution is per *directory* and cached, because it hits the filesystem and
//! diagnostics run on every keystroke. Two entry points express the cost
//! difference: [`ModelicaLanguageServer::tool_options_for_document`] never stats
//! (keystroke path), while
//! [`ModelicaLanguageServer::refreshed_tool_options_for_document`] revalidates
//! the recorded config paths and mtimes first (user-initiated formatting).
//! Editing a config file through the editor also drops the cache — see
//! [`is_tool_config_uri`].

use super::*;
use rumoca_tool_fmt::PartialFormatOptions;
use rumoca_tool_lint::LintOptions;
use std::time::SystemTime;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct ToolConfigError {
    path: PathBuf,
    message: String,
}

impl ToolConfigError {
    fn load(path: &Path, tool: &str, error: impl std::fmt::Display) -> Self {
        Self {
            path: path.to_path_buf(),
            message: format!(
                "failed to load {tool} configuration '{}': {error}",
                path.display()
            ),
        }
    }
}

impl std::fmt::Display for ToolConfigError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.message)
    }
}

/// Effective tool configuration for one directory.
#[derive(Debug, Clone, Default)]
pub(super) struct EditorToolOptions {
    /// Formatter overrides from the config file, still partial so the client's
    /// `FormattingOptions` can act as the lower-precedence layer underneath.
    pub(super) format_overrides: PartialFormatOptions,
    /// Fully resolved linter options (rules have no client-side counterpart).
    pub(super) lint: LintOptions,
}

/// Identity of a config file as it was when its options were loaded.
#[derive(Debug, Clone, PartialEq, Eq)]
struct ConfigStamp {
    path: PathBuf,
    modified: SystemTime,
}

impl ConfigStamp {
    fn of(path: PathBuf, tool: &str) -> std::result::Result<Self, ToolConfigError> {
        let modified = std::fs::metadata(&path)
            .and_then(|metadata| metadata.modified())
            .map_err(|error| ToolConfigError::load(&path, tool, error))?;
        Ok(Self { path, modified })
    }
}

#[derive(Debug, Clone)]
pub(super) struct ToolConfigEntry {
    options: Arc<EditorToolOptions>,
    fmt_config: Option<ConfigStamp>,
    lint_config: Option<ConfigStamp>,
}

impl ToolConfigEntry {
    /// Whether the config files this entry was built from are unchanged.
    fn is_current_for(&self, dir: &Path) -> std::result::Result<bool, ToolConfigError> {
        let fmt_config = config_stamp(rumoca_tool_fmt::find_config(dir), "formatter")?;
        let lint_config = config_stamp(rumoca_tool_lint::find_config(dir), "linter")?;
        Ok(self.fmt_config == fmt_config && self.lint_config == lint_config)
    }
}

fn config_stamp(
    path: Option<PathBuf>,
    tool: &str,
) -> std::result::Result<Option<ConfigStamp>, ToolConfigError> {
    path.map(|path| ConfigStamp::of(path, tool)).transpose()
}

fn load_format_overrides(
    path: &Path,
) -> std::result::Result<PartialFormatOptions, ToolConfigError> {
    rumoca_tool_fmt::load_config_overrides(path)
        .map_err(|error| ToolConfigError::load(path, "formatter", error))
}

fn load_lint_options(path: &Path) -> std::result::Result<LintOptions, ToolConfigError> {
    rumoca_tool_lint::load_config(path)
        .map_err(|error| ToolConfigError::load(path, "linter", error))
}

/// Load the formatter/linter configuration that applies to `dir`.
pub(super) fn resolve_editor_tool_options(
    dir: &Path,
) -> std::result::Result<ToolConfigEntry, ToolConfigError> {
    let fmt_config_path = rumoca_tool_fmt::find_config(dir);
    let format_overrides = match fmt_config_path.as_deref() {
        Some(path) => load_format_overrides(path)?,
        None => PartialFormatOptions::default(),
    };
    let lint_config_path = rumoca_tool_lint::find_config(dir);
    let lint = match lint_config_path.as_deref() {
        Some(path) => load_lint_options(path)?,
        None => LintOptions::default(),
    };

    Ok(ToolConfigEntry {
        options: Arc::new(EditorToolOptions {
            format_overrides,
            lint,
        }),
        fmt_config: config_stamp(fmt_config_path, "formatter")?,
        lint_config: config_stamp(lint_config_path, "linter")?,
    })
}

/// The directory a document's tool config is resolved from.
fn config_dir_for_document(uri_path: &str) -> PathBuf {
    Path::new(uri_path)
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("."))
}

impl ModelicaLanguageServer {
    /// Cached tool options for a document. Never touches the filesystem when
    /// the directory is already cached — this is on the per-keystroke
    /// diagnostics path.
    pub(super) async fn tool_options_for_document(
        &self,
        uri_path: &str,
    ) -> std::result::Result<Arc<EditorToolOptions>, ToolConfigError> {
        let dir = config_dir_for_document(uri_path);
        if let Some(entry) = self.tool_config_cache.read().await.get(&dir) {
            return Ok(Arc::clone(&entry.options));
        }
        self.resolve_and_cache_tool_options(dir).await
    }

    /// Tool options for a document, revalidating the recorded config files
    /// first. Used by user-initiated requests (formatting), where one `stat`
    /// per config file is far cheaper than serving a stale profile.
    pub(super) async fn refreshed_tool_options_for_document(
        &self,
        uri_path: &str,
    ) -> std::result::Result<Arc<EditorToolOptions>, ToolConfigError> {
        let dir = config_dir_for_document(uri_path);
        let cached = self.tool_config_cache.read().await.get(&dir).cloned();
        if let Some(entry) = cached
            && entry.is_current_for(&dir)?
        {
            return Ok(Arc::clone(&entry.options));
        }
        self.resolve_and_cache_tool_options(dir).await
    }

    async fn resolve_and_cache_tool_options(
        &self,
        dir: PathBuf,
    ) -> std::result::Result<Arc<EditorToolOptions>, ToolConfigError> {
        let entry = resolve_editor_tool_options(&dir)?;
        self.clear_tool_config_diagnostics(&entry).await;
        let options = Arc::clone(&entry.options);
        self.tool_config_cache.write().await.insert(dir, entry);
        Ok(options)
    }

    /// Continue lint/compile diagnostics with defaults only after publishing
    /// the configuration failure. Failed loads are never cached, so repairing
    /// the file takes effect on the next request.
    pub(super) async fn tool_options_for_document_or_default(
        &self,
        uri_path: &str,
    ) -> Arc<EditorToolOptions> {
        match self.tool_options_for_document(uri_path).await {
            Ok(options) => options,
            Err(error) => {
                self.publish_tool_config_error(&error).await;
                Arc::new(EditorToolOptions::default())
            }
        }
    }

    async fn publish_tool_config_error(&self, error: &ToolConfigError) {
        self.client
            .log_message(MessageType::ERROR, error.to_string())
            .await;
        let Ok(uri) = Url::from_file_path(&error.path) else {
            return;
        };
        self.client
            .publish_diagnostics(
                uri,
                vec![Diagnostic {
                    range: Range::new(Position::new(0, 0), Position::new(0, 1)),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String("rumoca-config".to_string())),
                    source: Some("Rumoca Config".to_string()),
                    message: error.to_string(),
                    ..Diagnostic::default()
                }],
                None,
            )
            .await;
    }

    async fn clear_tool_config_diagnostics(&self, entry: &ToolConfigEntry) {
        for stamp in [&entry.fmt_config, &entry.lint_config]
            .into_iter()
            .flatten()
        {
            let Ok(uri) = Url::from_file_path(&stamp.path) else {
                continue;
            };
            self.client.publish_diagnostics(uri, Vec::new(), None).await;
        }
    }

    /// Drop cached tool options when `uri` names a config file, so a config
    /// edited in the editor takes effect on the very next request.
    pub(super) async fn invalidate_tool_config_for_uri(&self, uri: &Url) {
        if is_tool_config_uri(uri) {
            self.tool_config_cache.write().await.clear();
        }
    }

    /// Formatter options for a document: the client's `FormattingOptions` form
    /// the base layer and the project's `.rumoca_fmt.toml` overrides them, so a
    /// project that pins a profile wins over per-editor preferences (SPEC_0018
    /// CLI override pattern, with the editor in the CLI's place).
    pub(super) async fn effective_format_options(
        &self,
        uri_path: &str,
        client_options: &lsp_types::FormattingOptions,
    ) -> std::result::Result<rumoca_tool_fmt::FormatOptions, ToolConfigError> {
        let tool_options = self.refreshed_tool_options_for_document(uri_path).await?;
        let client = handlers::partial_format_options_from_client(client_options);
        Ok(rumoca_tool_fmt::FormatOptions::from_partial(
            client.overlay(tool_options.format_overrides.clone()),
        ))
    }
}

/// Whether `uri` names a formatter or linter configuration file.
pub(super) fn is_tool_config_uri(uri: &Url) -> bool {
    let matches = |name: &str| {
        rumoca_tool_fmt::CONFIG_FILE_NAMES.contains(&name)
            || rumoca_tool_lint::CONFIG_FILE_NAMES.contains(&name)
    };
    if let Ok(path) = uri.to_file_path() {
        return path
            .file_name()
            .and_then(|name| name.to_str())
            .is_some_and(matches);
    }
    uri.path().rsplit('/').next().is_some_and(matches)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tool_config_uris_are_recognized() {
        let cases = [
            (".rumoca_fmt.toml", true),
            ("rumoca_fmt.toml", true),
            (".rumoca_lint.toml", true),
            ("rumoca_lint.toml", true),
            ("Ball.mo", false),
            ("rumoca-scenario.toml", false),
        ];
        for (name, expected) in cases {
            let uri = Url::parse(&format!("file:///workspace/{name}")).expect("uri");
            assert_eq!(is_tool_config_uri(&uri), expected, "for {name}");
        }
    }

    #[test]
    fn resolution_reads_formatter_profile_and_lint_rules_from_config_files() {
        let dir = std::env::temp_dir().join("rumoca_lsp_tool_config_resolution");
        std::fs::create_dir_all(&dir).expect("temp dir");
        std::fs::write(dir.join(".rumoca_fmt.toml"), "profile = \"canonical\"\n")
            .expect("write fmt config");
        std::fs::write(
            dir.join(".rumoca_lint.toml"),
            "disabled_rules = [\"naming-convention\"]\n",
        )
        .expect("write lint config");

        let entry = resolve_editor_tool_options(&dir).expect("valid tool configs");
        assert_eq!(
            entry.options.format_overrides.profile,
            Some(rumoca_tool_fmt::FormatProfile::Canonical)
        );
        assert!(
            entry
                .options
                .lint
                .disabled_rules
                .iter()
                .any(|rule| rule == "naming-convention"),
            "lint config should disable naming-convention: {:?}",
            entry.options.lint.disabled_rules
        );
        assert!(
            entry.is_current_for(&dir).expect("config stamps"),
            "freshly resolved entry is current"
        );

        std::fs::remove_file(dir.join(".rumoca_fmt.toml")).expect("cleanup fmt");
        std::fs::remove_file(dir.join(".rumoca_lint.toml")).expect("cleanup lint");
        assert!(
            !entry.is_current_for(&dir).expect("removed configs"),
            "removing the config files must invalidate the entry"
        );
    }

    #[test]
    fn malformed_formatter_config_is_an_error() {
        let dir = std::env::temp_dir().join("rumoca_lsp_tool_config_malformed");
        std::fs::create_dir_all(&dir).expect("temp dir");
        let path = dir.join(".rumoca_fmt.toml");
        std::fs::write(&path, "profile = [\n").expect("write malformed config");

        let error = resolve_editor_tool_options(&dir)
            .expect_err("malformed formatter config must not become cached defaults");

        assert!(error.to_string().contains("formatter configuration"));
        assert!(error.to_string().contains(&path.display().to_string()));
        std::fs::remove_file(path).expect("cleanup config");
    }

    #[test]
    fn formatter_config_read_failure_is_an_error() {
        let dir = std::env::temp_dir().join("rumoca_lsp_tool_config_read_error");
        std::fs::create_dir_all(&dir).expect("temp dir");

        let error =
            load_format_overrides(&dir).expect_err("unreadable config path must be surfaced");

        assert!(error.to_string().contains("formatter configuration"));
        assert!(error.to_string().contains("failed to read config file"));
    }
}
