import { createHash } from "node:crypto";
import fs from "node:fs/promises";
import path from "node:path";

const exists = async (file) => {
  try {
    await fs.access(file);
    return true;
  } catch {
    return false;
  }
};

const fileSignature = async (repoRoot, file) => {
  const stat = await fs.stat(file);
  return {
    path: path.relative(repoRoot, file),
    size: stat.size,
    mtimeMs: Math.trunc(stat.mtimeMs),
  };
};

export const outputsExist = async (files) =>
  (await Promise.all(files.map(exists))).every(Boolean);

export const outputsAreFresh = async (inputs, outputs) => {
  if (!(await outputsExist(outputs))) return false;
  const inputStats = await Promise.all(inputs.map((file) => fs.stat(file)));
  const outputStats = await Promise.all(outputs.map((file) => fs.stat(file)));
  const latestInput = Math.max(...inputStats.map((stat) => stat.mtimeMs));
  const oldestOutput = Math.min(...outputStats.map((stat) => stat.mtimeMs));
  return oldestOutput >= latestInput;
};

export const buildFingerprint = async ({
  repoRoot,
  files,
  configuration,
  toolVersion,
}) => {
  const signatures = await Promise.all(
    [...files].sort().map((file) => fileSignature(repoRoot, file)),
  );
  return createHash("sha256")
    .update(JSON.stringify({ configuration, signatures, toolVersion }))
    .digest("hex");
};

export const readFingerprint = async (file) => {
  try {
    const parsed = JSON.parse(await fs.readFile(file, "utf8"));
    return typeof parsed.fingerprint === "string" ? parsed.fingerprint : null;
  } catch {
    return null;
  }
};

export const writeFingerprint = async (file, fingerprint) => {
  await fs.writeFile(file, `${JSON.stringify({ fingerprint }, null, 2)}\n`, "utf8");
};
