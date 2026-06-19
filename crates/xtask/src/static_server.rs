use anyhow::{Context, Result};
use std::ffi::OsStr;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Component, Path, PathBuf};

pub(crate) fn serve(
    root: &Path,
    explicit_port: Option<u16>,
    editor_pkg_subdir: Option<&str>,
) -> Result<()> {
    serve_with_options(
        root,
        ServeOptions {
            explicit_port,
            editor_pkg_subdir,
            default_path: "packages/playground/index.html",
            urls: &[("Editor", "/packages/playground/index.html")],
        },
    )
}

pub(crate) struct ServeOptions<'a> {
    pub(crate) explicit_port: Option<u16>,
    pub(crate) editor_pkg_subdir: Option<&'a str>,
    pub(crate) default_path: &'a str,
    pub(crate) urls: &'a [(&'a str, &'a str)],
}

pub(crate) fn serve_with_options(root: &Path, options: ServeOptions<'_>) -> Result<()> {
    let port = options
        .explicit_port
        .or_else(|| std::env::var("PORT").ok().and_then(|raw| raw.parse().ok()))
        .unwrap_or(8080);
    let listener = TcpListener::bind(("0.0.0.0", port))
        .with_context(|| format!("failed to bind 0.0.0.0:{port}"))?;

    println!("Serving on http://localhost:{port}");
    for &(label, url) in options.urls {
        println!("{label} URL: http://localhost:{port}{url}");
    }
    if let Some(pkg_subdir) = options.editor_pkg_subdir {
        println!("WASM package subdir: {pkg_subdir}");
    }
    println!("Press Ctrl+C to stop.");

    for stream in listener.incoming() {
        match stream {
            Ok(mut stream) => {
                if let Err(error) = handle_http_request(root, &options, &mut stream) {
                    eprintln!("serve error: {error:#}");
                }
            }
            Err(error) => eprintln!("accept error: {error}"),
        }
    }
    Ok(())
}

fn handle_http_request(
    root: &Path,
    options: &ServeOptions<'_>,
    stream: &mut TcpStream,
) -> Result<()> {
    let mut reader = BufReader::new(
        stream
            .try_clone()
            .context("failed to clone TCP stream for request parsing")?,
    );
    let mut request_line = String::new();
    if reader
        .read_line(&mut request_line)
        .context("failed to read request line")?
        == 0
    {
        return Ok(());
    }
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).is_err() || line == "\r\n" || line.is_empty() {
            break;
        }
    }

    let mut parts = request_line.split_whitespace();
    let method = parts.next().unwrap_or_default();
    let target = parts.next().unwrap_or("/");
    let include_body = match method {
        "GET" => true,
        "HEAD" => false,
        _ => {
            return write_http_response(
                stream,
                405,
                "Method Not Allowed",
                "text/plain",
                b"method not allowed",
            );
        }
    };

    let url = target.split('?').next().unwrap_or("/");
    if url.starts_with("/proxy/") {
        return write_http_response_with_body_policy(
            stream,
            501,
            "Not Implemented",
            "text/plain",
            b"proxy endpoint is not supported in xtask",
            include_body,
        );
    }

    match resolve_static_path(root, url, options.default_path) {
        Some(path) if path.is_file() => {
            if is_wasm_editor_index(root, &path) {
                let body = wasm_editor_index_body(&path, options.editor_pkg_subdir)?;
                return write_http_response_with_body_policy(
                    stream,
                    200,
                    "OK",
                    "text/html",
                    body.as_bytes(),
                    include_body,
                );
            }
            let body =
                fs::read(&path).with_context(|| format!("failed to read {}", path.display()))?;
            write_http_response_with_body_policy(
                stream,
                200,
                "OK",
                mime_for_path(&path),
                &body,
                include_body,
            )
        }
        _ => write_http_response_with_body_policy(
            stream,
            404,
            "Not Found",
            "text/plain",
            b"not found",
            include_body,
        ),
    }
}

fn write_http_response(
    stream: &mut TcpStream,
    code: u16,
    status: &str,
    content_type: &str,
    body: &[u8],
) -> Result<()> {
    write_http_response_with_body_policy(stream, code, status, content_type, body, true)
}

fn write_http_response_with_body_policy(
    stream: &mut TcpStream,
    code: u16,
    status: &str,
    content_type: &str,
    body: &[u8],
    include_body: bool,
) -> Result<()> {
    let headers = format!(
        "HTTP/1.1 {code} {status}\r\nContent-Type: {content_type}\r\nContent-Length: {}\r\nConnection: close\r\nCross-Origin-Opener-Policy: same-origin\r\nCross-Origin-Embedder-Policy: require-corp\r\nAccess-Control-Allow-Origin: *\r\nCache-Control: no-store, no-cache, must-revalidate, max-age=0\r\nPragma: no-cache\r\nExpires: 0\r\n\r\n",
        body.len()
    );
    stream
        .write_all(headers.as_bytes())
        .context("failed writing HTTP headers")?;
    if include_body {
        stream.write_all(body).context("failed writing HTTP body")?;
    }
    stream.flush().context("failed flushing HTTP response")?;
    Ok(())
}

fn is_wasm_editor_index(root: &Path, path: &Path) -> bool {
    path == root.join("packages/playground/index.html")
}

fn wasm_editor_index_body(path: &Path, editor_pkg_subdir: Option<&str>) -> Result<String> {
    let mut body =
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    if let Some(pkg_subdir) = editor_pkg_subdir {
        let replacement = format!("window.rumocaWasmPkgSubdir = '{pkg_subdir}';");
        body = body
            .replace(
                "window.rumocaWasmPkgSubdir = 'release-full-web';",
                &replacement,
            )
            .replace(
                "window.rumocaWasmPkgSubdir = 'release-full-web-rayon';",
                &replacement,
            );
    }
    Ok(body)
}

fn resolve_static_path(root: &Path, url: &str, default_path: &str) -> Option<PathBuf> {
    if url == "/" {
        return Some(root.join(default_path));
    }
    let relative = url.trim_start_matches('/');
    let mut safe = PathBuf::new();
    for component in Path::new(relative).components() {
        if let Component::Normal(segment) = component {
            safe.push(segment);
        }
    }
    if safe.as_os_str().is_empty() {
        return Some(root.join(default_path));
    }
    let mut path = root.join(safe);
    if path.is_dir() {
        path = path.join("index.html");
    }
    Some(path)
}

fn mime_for_path(path: &Path) -> &'static str {
    match path.extension().and_then(OsStr::to_str).unwrap_or_default() {
        "html" => "text/html",
        "js" | "mjs" => "application/javascript",
        "json" => "application/json",
        "wasm" => "application/wasm",
        "css" => "text/css",
        "svg" => "image/svg+xml",
        "png" => "image/png",
        "jpg" | "jpeg" => "image/jpeg",
        _ => "application/octet-stream",
    }
}
