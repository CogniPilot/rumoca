#!/usr/bin/env node
import fs from "node:fs/promises";
import path from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { patchWasmPackageJson } from "./patch-wasm-pkg.mjs";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const repoRoot = path.resolve(__dirname, "..", "..");
const pkgRoot = path.join(repoRoot, "pkg");

const truthy = (value) =>
  ["1", "true", "yes", "on"].includes(String(value || "").toLowerCase());

const parseArgs = (argv) => {
  const args = {
    profile: "release",
    variant: "full-web",
    rayon: false,
    patch: true,
    pack: false,
    editorAliases: false,
  };

  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === "--profile") args.profile = argv[++i];
    else if (arg === "--variant") args.variant = argv[++i];
    else if (arg === "--rayon") args.rayon = true;
    else if (arg === "--no-patch") args.patch = false;
    else if (arg === "--pack") args.pack = true;
    else if (arg === "--editor-aliases") args.editorAliases = true;
    else if (arg === "--help") {
      console.log(`Usage: node packaging/npm/build.mjs [options]

Options:
  --profile <dev|release>                  Build profile (default: release)
  --variant <core|sim-diffsol|sim-rk45|full-web>
                                           Feature preset (default: full-web)
  --rayon                                  Enable wasm-rayon
  --no-patch                               Skip package.json patching
  --pack                                   Run npm pack on pkg/
  --editor-aliases                         Create rumoca.js / rumoca_bg.wasm aliases
`);
      process.exit(0);
    } else {
      throw new Error(`Unknown argument: ${arg}`);
    }
  }

  if (!["dev", "release"].includes(args.profile)) {
    throw new Error(`Invalid --profile: ${args.profile}`);
  }
  if (!["core", "sim-diffsol", "sim-rk45", "full-web"].includes(args.variant)) {
    throw new Error(`Invalid --variant: ${args.variant}`);
  }

  return args;
};

const featureListFor = (variant, rayon) => {
  const features = [];
  if (variant === "sim-diffsol") features.push("sim-diffsol");
  if (variant === "sim-rk45") features.push("sim-rk45");
  if (variant === "full-web") features.push("full-web");
  if (rayon) features.push("wasm-rayon");
  return features;
};

const buildSubdirName = ({ profile, variant, rayon }) =>
  `${profile}-${variant}${rayon ? "-rayon" : ""}`;

const buildTimeUtcNow = () => new Date().toISOString().replace(/\.\d{3}Z$/, "Z");

const run = (cmd, args, options = {}) => {
  const result = spawnSync(cmd, args, {
    stdio: "inherit",
    cwd: repoRoot,
    ...options,
  });
  if (result.status !== 0) {
    throw new Error(`Command failed: ${cmd} ${args.join(" ")}`);
  }
};

const runCapture = (cmd, args, options = {}) => {
  const result = spawnSync(cmd, args, {
    stdio: ["ignore", "pipe", "pipe"],
    encoding: "utf8",
    cwd: repoRoot,
    ...options,
  });
  if (result.status !== 0) {
    throw new Error(`Command failed: ${cmd} ${args.join(" ")}\n${result.stderr || ""}`);
  }
  return result.stdout;
};

const ensureLicenseInBindCrate = async () => {
  const wasmLicense = path.join(repoRoot, "crates", "rumoca-bind-wasm", "LICENSE");
  try {
    await fs.access(wasmLicense);
    return async () => {};
  } catch {
    const rootLicense = path.join(repoRoot, "LICENSE");
    await fs.copyFile(rootLicense, wasmLicense);
    return async () => {
      await fs.rm(wasmLicense, { force: true });
    };
  }
};

const copyEditorWorkers = async (pkgDir) => {
  await fs.copyFile(
    path.join(repoRoot, "editors", "wasm", "rumoca_worker.js"),
    path.join(pkgDir, "rumoca_worker.js"),
  );
  await fs.copyFile(
    path.join(repoRoot, "editors", "wasm", "parse_worker.js"),
    path.join(pkgDir, "parse_worker.js"),
  );
};

const writeEditorAliases = async (pkgDir) => {
  const jsFrom = path.join(pkgDir, "rumoca_bind_wasm.js");
  const wasmFrom = path.join(pkgDir, "rumoca_bind_wasm_bg.wasm");
  const jsTo = path.join(pkgDir, "rumoca.js");
  const wasmTo = path.join(pkgDir, "rumoca_bg.wasm");
  await fs.copyFile(jsFrom, jsTo);
  await fs.copyFile(wasmFrom, wasmTo);
  const jsText = await fs.readFile(jsTo, "utf8");
  await fs.writeFile(jsTo, jsText.replaceAll("rumoca_bind_wasm_bg.wasm", "rumoca_bg.wasm"), "utf8");
};

const utcTimestamp = () => {
  const iso = new Date().toISOString(); // 2026-04-30T20:10:55.123Z
  return iso.replaceAll(":", "").replaceAll("-", "").replace(".", "").replace("T", "-").replace("Z", "Z");
};

const packTarball = async ({ cwd, packageDir, tarballDestDir, dev }) => {
  const raw = runCapture("npm", ["pack", "--json", packageDir], { cwd });
  const parsed = JSON.parse(raw);
  const filename = parsed?.[0]?.filename;
  if (!filename) {
    throw new Error("npm pack did not return a filename");
  }
  const packedAt = path.join(cwd, filename);
  const renamedFilename = dev
    ? filename.replace(/\.tgz$/, `-dev-${utcTimestamp()}.tgz`)
    : filename;
  const finalPath = path.join(tarballDestDir, renamedFilename);
  await fs.rename(packedAt, finalPath);
  if (dev) {
    console.log(`Renamed dev tarball: ${renamedFilename}`);
  }
  console.log(`Moved tarball to: ${finalPath}`);
  return finalPath;
};

const main = async () => {
  const args = parseArgs(process.argv.slice(2));
  const features = featureListFor(args.variant, args.rayon);
  const releaseFlag = args.profile === "dev" ? "--dev" : "--release";
  const subdir = buildSubdirName(args);
  const pkgDir = path.join(pkgRoot, subdir);
  const outDirArg = `../../pkg/${subdir}`;

  const stagedCleanup = await ensureLicenseInBindCrate();
  try {
    const wasmPackArgs = [
      "build",
      "crates/rumoca-bind-wasm",
      "--target",
      "web",
      "--out-dir",
      outDirArg,
      releaseFlag,
    ];

    if (!truthy(process.env.RUMOCA_WASM_OPT)) {
      wasmPackArgs.push("--no-opt");
    }
    if (features.length > 0) {
      wasmPackArgs.push("--", "--features", features.join(","));
    }

    const env = { ...process.env };
    const nowUtc = buildTimeUtcNow();
    if (args.rayon) {
      const threadFlags = "-C target-feature=+atomics,+bulk-memory,+mutable-globals";
      const current = String(env.RUSTFLAGS || "");
      env.RUSTFLAGS = current.includes("target-feature=+atomics")
        ? current
        : `${current}${current.trim() ? " " : ""}${threadFlags}`;
    }

    run("wasm-pack", wasmPackArgs, { env });
    await copyEditorWorkers(pkgDir);
    await fs.writeFile(
      path.join(pkgDir, "rumoca_package_meta.json"),
      `${JSON.stringify({ packageBuiltTimeUtc: nowUtc }, null, 2)}\n`,
      "utf8",
    );

    if (args.editorAliases) {
      await writeEditorAliases(pkgDir);
      // Keep editor smoke/test workflows stable: mirror alias artifacts to pkg/.
      for (const file of ["rumoca.js", "rumoca_bg.wasm", "rumoca_worker.js", "parse_worker.js"]) {
        await fs.copyFile(path.join(pkgDir, file), path.join(pkgRoot, file));
      }
    }
    if (args.patch) {
      await patchWasmPackageJson(pkgDir, args.variant);
    }
    if (args.pack) {
      await packTarball({
        cwd: __dirname,
        packageDir: path.relative(__dirname, pkgDir),
        tarballDestDir: pkgRoot,
        dev: args.profile === "dev",
      });
    }
  } finally {
    await stagedCleanup();
  }
};

main().catch((error) => {
  console.error(error);
  process.exit(1);
});
