use anyhow::{Context, Result};
use std::ffi::OsStr;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Component, Path, PathBuf};

pub(crate) fn serve(root: &Path, explicit_port: Option<u16>) -> Result<()> {
    let port = explicit_port
        .or_else(|| std::env::var("PORT").ok().and_then(|raw| raw.parse().ok()))
        .unwrap_or(8080);
    let listener = TcpListener::bind(("0.0.0.0", port))
        .with_context(|| format!("failed to bind 0.0.0.0:{port}"))?;

    println!("Serving on http://localhost:{port}");
    println!("Editor URL: http://localhost:{port}/editors/wasm/index.html");
    println!("Press Ctrl+C to stop.");

    for stream in listener.incoming() {
        match stream {
            Ok(mut stream) => {
                if let Err(error) = handle_http_request(root, &mut stream) {
                    eprintln!("serve error: {error:#}");
                }
            }
            Err(error) => eprintln!("accept error: {error}"),
        }
    }
    Ok(())
}

fn handle_http_request(root: &Path, stream: &mut TcpStream) -> Result<()> {
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
    if method != "GET" {
        return write_http_response(
            stream,
            405,
            "Method Not Allowed",
            "text/plain",
            b"method not allowed",
        );
    }

    let url = target.split('?').next().unwrap_or("/");
    if url.starts_with("/proxy/") {
        return write_http_response(
            stream,
            501,
            "Not Implemented",
            "text/plain",
            b"proxy endpoint is not supported in rum",
        );
    }

    match resolve_static_path(root, url) {
        Some(path) if path.is_file() => {
            let body =
                fs::read(&path).with_context(|| format!("failed to read {}", path.display()))?;
            write_http_response(stream, 200, "OK", mime_for_path(&path), &body)
        }
        _ => write_http_response(stream, 404, "Not Found", "text/plain", b"not found"),
    }
}

fn write_http_response(
    stream: &mut TcpStream,
    code: u16,
    status: &str,
    content_type: &str,
    body: &[u8],
) -> Result<()> {
    let headers = format!(
        "HTTP/1.1 {code} {status}\r\nContent-Type: {content_type}\r\nContent-Length: {}\r\nConnection: close\r\nCross-Origin-Opener-Policy: same-origin\r\nCross-Origin-Embedder-Policy: require-corp\r\nAccess-Control-Allow-Origin: *\r\nCache-Control: no-store, no-cache, must-revalidate, max-age=0\r\nPragma: no-cache\r\nExpires: 0\r\n\r\n",
        body.len()
    );
    stream
        .write_all(headers.as_bytes())
        .context("failed writing HTTP headers")?;
    stream.write_all(body).context("failed writing HTTP body")?;
    stream.flush().context("failed flushing HTTP response")?;
    Ok(())
}

fn resolve_static_path(root: &Path, url: &str) -> Option<PathBuf> {
    if url == "/" {
        return Some(root.join("editors/wasm/index.html"));
    }
    let relative = url.trim_start_matches('/');
    let mut safe = PathBuf::new();
    for component in Path::new(relative).components() {
        if let Component::Normal(segment) = component {
            safe.push(segment);
        }
    }
    if safe.as_os_str().is_empty() {
        return Some(root.join("editors/wasm/index.html"));
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
