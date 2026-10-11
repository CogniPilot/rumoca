// The real browser must load the exact shipped package, including its WASM.
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { createHash } from "node:crypto";
import path from "node:path";

function sha256(bytes) {
  return createHash("sha256").update(bytes).digest("hex");
}

export async function observeShippedArtifact(page, extraFiles = []) {
  const files = ["rumoca_bind_wasm.js", "rumoca_bind_wasm_bg.wasm", ...extraFiles];
  const expected = new Map(await Promise.all(files.map(async (name) => [name,
    sha256(await readFile(path.join("packages/rumoca/dist/release-full-web", name)))])));
  const loaded = new Set();
  const pending = [];
  const errors = [];
  page.on("response", (response) => {
    const url = new URL(response.url());
    const name = path.basename(url.pathname);
    if (!expected.has(name) || response.request().method() === "HEAD") return;
    pending.push((async () => {
      assert.ok(url.pathname.includes("/packages/rumoca/dist/release-full-web/")
        || url.pathname.includes("/rumoca/pkg/release-full-web/"),
      `unexpected package route: ${url.pathname}`);
      assert.equal(response.status(), 200, `loaded ${name}`);
      if (loaded.has(name)) return;
      assert.equal(sha256(await response.body()), expected.get(name), `shipped bytes: ${name}`);
      loaded.add(name);
    })().catch((error) => errors.push(error)));
  });
  return async () => {
    await Promise.all(pending);
    assert.equal(errors.length, 0, errors.map(String).join("\n"));
    assert.deepEqual([...loaded].sort(), [...expected.keys()].sort(), "all required shipped files loaded");
    console.log("PASS: actual browser loaded exact shipped FullWeb SHA256", Object.fromEntries(expected));
  };
}
