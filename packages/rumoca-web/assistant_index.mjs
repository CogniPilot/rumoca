// Builds the static retrieval index the playground assistant searches:
// the diagnostic catalogue (read from the `#[diagnostic(code(...), help(...))]`
// attributes in the Rust sources, the single place codes are defined) and the
// user guide, split by heading. Nothing here is hand-copied text.
import fs from 'node:fs/promises';
import path from 'node:path';

const CRATE_ERROR_FILES_DIR = 'crates';
const DOC_CHUNK_LIMIT = 1800;

async function walk(dir, accept) {
  const found = [];
  for (const entry of await fs.readdir(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      if (entry.name === 'target' || entry.name === 'node_modules' || entry.name === 'tests') continue;
      found.push(...(await walk(full, accept)));
    } else if (accept(entry.name)) {
      found.push(full);
    }
  }
  return found;
}

// Reads one Rust string literal starting at `start` (which points at the
// opening quote); returns [value, indexAfterClosingQuote].
function readStringLiteral(text, start) {
  let value = '';
  let index = start + 1;
  while (index < text.length && text[index] !== '"') {
    if (text[index] === '\\') {
      const next = text[index + 1];
      value += next === 'n' ? '\n' : next === 't' ? '\t' : next;
      index += 2;
    } else {
      value += text[index];
      index += 1;
    }
  }
  return [value, index + 1];
}

// Returns the text of the balanced parenthesis group opening at `open`.
function balancedGroup(text, open) {
  let depth = 0;
  for (let index = open; index < text.length; index += 1) {
    if (text[index] === '"') {
      index = readStringLiteral(text, index)[1] - 1;
    } else if (text[index] === '(') {
      depth += 1;
    } else if (text[index] === ')') {
      depth -= 1;
      if (depth === 0) return text.slice(open, index + 1);
    }
  }
  throw new Error('unbalanced attribute parentheses');
}

function firstStringAfter(text, marker) {
  const at = text.indexOf(marker);
  if (at < 0) return '';
  const quote = text.indexOf('"', at + marker.length);
  return quote < 0 ? '' : readStringLiteral(text, quote)[0];
}

function docCommentAbove(lines, lineIndex) {
  const doc = [];
  for (let index = lineIndex - 1; index >= 0; index -= 1) {
    const trimmed = lines[index].trim();
    if (trimmed.startsWith('///')) doc.unshift(trimmed.slice(3).trim());
    else if (!trimmed.startsWith('#[')) break;
  }
  return doc.join(' ');
}

export function diagnosticsFromRust(text, source) {
  const entries = [];
  const lines = text.split('\n');
  const pattern = /code\(rumoca::(\w+)::([A-Z]{2,3}\d{3})\)/gu;
  for (const match of text.matchAll(pattern)) {
    const before = text.slice(0, match.index);
    if (/\/\/[!/]?[^\n]*$/u.test(before)) continue;
    const attributeStart = before.lastIndexOf('#[diagnostic(');
    const attribute = balancedGroup(text, attributeStart + '#[diagnostic'.length);
    const errorAt = before.lastIndexOf('#[error(');
    const message = errorAt < 0 ? '' : firstStringAfter(text.slice(errorAt), '#[error(');
    const line = before.split('\n').length - 1;
    entries.push({
      code: match[2],
      phase: match[1],
      message,
      help: firstStringAfter(attribute, 'help('),
      description: docCommentAbove(lines, line),
      source,
    });
  }
  return entries;
}

function chunkMarkdown(relativePath, text) {
  const chunks = [];
  let title = relativePath;
  let heading = '';
  let body = [];
  const flush = () => {
    const joined = body.join('\n').trim();
    for (let start = 0; start < joined.length; start += DOC_CHUNK_LIMIT) {
      chunks.push({ path: relativePath, title, heading, text: joined.slice(start, start + DOC_CHUNK_LIMIT) });
    }
    body = [];
  };
  for (const line of text.split('\n')) {
    const match = line.match(/^(#{1,3})\s+(.*)$/u);
    if (match) {
      flush();
      if (match[1] === '#') title = match[2];
      heading = match[2];
    } else {
      body.push(line);
    }
  }
  flush();
  return chunks;
}

export async function buildAssistantIndex(repoRoot) {
  const diagnostics = [];
  for (const file of await walk(path.join(repoRoot, CRATE_ERROR_FILES_DIR), (name) => name.endsWith('.rs'))) {
    const text = await fs.readFile(file, 'utf8');
    if (!text.includes('code(rumoca::')) continue;
    diagnostics.push(...diagnosticsFromRust(text, path.relative(repoRoot, file)));
  }
  diagnostics.sort((a, b) => a.code.localeCompare(b.code));

  const guideRoot = path.join(repoRoot, 'docs', 'user-guide', 'src');
  const docs = [];
  for (const file of (await walk(guideRoot, (name) => name.endsWith('.md'))).sort()) {
    const relative = path.relative(guideRoot, file);
    if (relative === 'SUMMARY.md') continue;
    docs.push(...chunkMarkdown(relative, await fs.readFile(file, 'utf8')));
  }
  return { diagnostics, docs };
}
