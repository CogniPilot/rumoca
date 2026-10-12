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
  const shipped = await Promise.all(files.map(async (name) => [name,
    await readFile(path.join("packages/rumoca/dist/release-full-web", name))]));
  const expected = new Map(shipped.map(([name, bytes]) => [name, sha256(bytes)]));
  const session = await page.context().newCDPSession(page);
  // CDP's default inspector payload cache cannot retain the current WASM.
  // Size this test-owned transport from the actual shipped files; application
  // requests and bodies remain unchanged and every retained body is hashed.
  await session.send("Network.enable", {
    maxResourceBufferSize: Math.max(...shipped.map(([, bytes]) => bytes.length)) * 2,
    maxTotalBufferSize: shipped.reduce((sum, [, bytes]) => sum + bytes.length, 0) * 4,
    enableDurableMessages: true,
  });
  const loaded = new Set();
  const pending = [];
  const errors = [];
  const requests = new Map();
  session.on("Network.requestWillBeSent", ({ requestId, request }) => {
    if (request.method !== "HEAD") requests.set(requestId, request.url);
  });
  session.on("Network.responseReceived", ({ requestId, response }) => {
    const url = new URL(response.url);
    const name = path.basename(url.pathname);
    if (!expected.has(name) || !requests.has(requestId)) return;
    requests.set(requestId, { name, url, status: response.status });
  });
  session.on("Network.loadingFinished", ({ requestId }) => {
    const response = requests.get(requestId);
    requests.delete(requestId);
    if (!response || typeof response === "string") return;
    const { name, url, status } = response;
    pending.push((async () => {
      assert.ok(url.pathname.includes("/packages/rumoca/dist/release-full-web/")
        || url.pathname.includes("/rumoca/pkg/release-full-web/"),
      `unexpected package route: ${url.pathname}`);
      assert.equal(status, 200, `loaded ${name}`);
      if (loaded.has(name)) return;
      const body = await session.send("Network.getResponseBody", { requestId });
      const bytes = Buffer.from(body.body, body.base64Encoded ? "base64" : "utf8");
      assert.equal(sha256(bytes), expected.get(name), `shipped bytes: ${name}`);
      loaded.add(name);
    })().catch((error) => errors.push(error)));
  });
  session.on("Network.loadingFailed", ({ requestId, errorText }) => {
    const response = requests.get(requestId);
    requests.delete(requestId);
    if (response && typeof response !== "string") errors.push(new Error(`${response.name}: ${errorText}`));
  });
  return async () => {
    await Promise.all(pending);
    assert.equal(errors.length, 0, errors.map(String).join("\n"));
    assert.deepEqual([...loaded].sort(), [...expected.keys()].sort(), "all required shipped files loaded");
    console.log("PASS: actual browser loaded exact shipped FullWeb SHA256", Object.fromEntries(expected));
  };
}
