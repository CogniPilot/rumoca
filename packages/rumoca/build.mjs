#!/usr/bin/env node
import fs from "node:fs/promises";
import path from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { patchWasmPackageJson } from "./patch-wasm-pkg.mjs";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const repoRoot = path.resolve(__dirname, "..", "..");
const pkgRoot = path.join(__dirname, "dist");
const pkgOutDirArg = (subdir) => `../../packages/rumoca/dist/${subdir}`;

const truthy = (value) =>
  ["1", "true", "yes", "on"].includes(String(value || "").toLowerCase());

const parseArgs = (argv) => {
  const args = {
    profile: "release",
    variant: "full-web",
    rayon: false,
    patch: true,
    pack: false,
    optimize: false,
  };

  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === "--profile") args.profile = argv[++i];
    else if (arg === "--variant") args.variant = argv[++i];
    else if (arg === "--rayon") args.rayon = true;
    else if (arg === "--no-patch") args.patch = false;
    else if (arg === "--pack") args.pack = true;
    else if (arg === "--optimize") args.optimize = true;
    else if (arg === "--help") {
      console.log(`Usage: node packages/rumoca/build.mjs [options]

Options:
  --profile <dev|release>                  Build profile (default: release)
  --variant <core|sim-diffsol|sim-rk45|full-web>
                                           Feature preset (default: full-web)
  --rayon                                  Enable wasm-rayon
  --no-patch                               Skip package.json patching
  --pack                                   Run npm pack on packages/rumoca/dist/
  --optimize                               Run wasm-opt (-Oz) — use for published builds
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

const appendRustflags = (env, flags) => {
  const current = String(env.RUSTFLAGS || "");
  const missingFlags = flags.filter((flag) => !current.includes(flag));
  if (missingFlags.length === 0) return;
  env.RUSTFLAGS = `${current}${current.trim() ? " " : ""}${missingFlags.join(" ")}`;
};

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

// All hand-written browser JS lives in the rumoca-web package
// (@cognipilot/rumoca-web). This is the single source; the build assembles it
// into the wasm package and the playground's vendor dir below.
const webDir = path.join(repoRoot, "packages", "rumoca-web");
const runtimeFiles = [
  "rumoca_runtime.js",
  "rumoca_diffsol.js",
  "rumoca_interactive.js",
  "modelica_language.js",
  "rumoca_worker.js",
  "parse_worker.js",
  "rumoca_gpu.js",
];

const ensureWebVendorAssets = async () => {
  try {
    await fs.access(path.join(webDir, "node_modules", "esbuild"));
  } catch {
    run("npm", ["ci"], { cwd: webDir });
  }
  run("npm", ["run", "build"], { cwd: webDir });
};

const copyEditorWorkers = async (pkgDir) => {
  for (const file of runtimeFiles) {
    await fs.copyFile(path.join(webDir, "runtime", file), path.join(pkgDir, file));
  }
};

// Stage the playground's vendor/ dir (served from the same dir as its
// index.html, in dev and on gh-pages) from the single-source web/ package.
// Generated, gitignored — never hand-maintained, so it cannot drift.
const generateEditorVendor = async () => {
  const vendorDir = path.join(repoRoot, "packages", "playground", "vendor");
  await fs.rm(vendorDir, { recursive: true, force: true });
  await fs.cp(path.join(webDir, "vendor"), vendorDir, { recursive: true });
};

// Build the diffsol (stiff/implicit) addon and ship it inside the same package.
// It is a SEPARATE wasm module carrying relaxed-SIMD (faer/pulp), loaded lazily
// by rumoca_diffsol.js only when the browser supports it — so the main module
// stays SIMD-free / universal. Only meaningful for `full-web` (the package that
// has the `lower_model_to_solve_json` export the addon consumes); `core` lacks
// it, so the addon is not shipped there.
const buildDiffsolAddon = async (pkgDir, args) => {
  const tmpSubdir = ".diffsol-build";
  const tmpDir = path.join(pkgRoot, tmpSubdir);
  const addonArgs = [
    "build",
    "crates/rumoca-bind-wasm-diffsol",
    "--target",
    "web",
    "--out-dir",
    pkgOutDirArg(tmpSubdir),
    args.profile === "dev" ? "--dev" : "--release",
  ];
  if (!args.optimize) {
    addonArgs.push("--no-opt");
  }
  run("wasm-pack", addonArgs);
  for (const file of [
    "rumoca_bind_wasm_diffsol.js",
    "rumoca_bind_wasm_diffsol_bg.wasm",
    "rumoca_bind_wasm_diffsol.d.ts",
  ]) {
    await fs.copyFile(path.join(tmpDir, file), path.join(pkgDir, file));
  }
  await fs.rm(tmpDir, { recursive: true, force: true });
};

const copyPackageReadme = async (pkgDir) => {
  await fs.copyFile(
    path.join(__dirname, "README.md"),
    path.join(pkgDir, "README.md"),
  );
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
  const outDirArg = pkgOutDirArg(subdir);

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

    // wasm-opt is slow, so skip it by default (fast dev/editor builds). For
    // published packages pass --optimize: wasm-pack then runs wasm-opt with the
    // `-Oz` level configured in crates/rumoca-bind-wasm/Cargo.toml's
    // [package.metadata.wasm-pack.profile.release], stripping debug names and
    // dead code (~25% smaller raw).
    if (!args.optimize) {
      wasmPackArgs.push("--no-opt");
    }
    if (features.length > 0) {
      wasmPackArgs.push("--", "--features", features.join(","));
    }

    const env = { ...process.env };
    const nowUtc = buildTimeUtcNow();
    if (args.rayon) {
      const threadFlags = "-C target-feature=+atomics,+bulk-memory,+mutable-globals";
      appendRustflags(env, [threadFlags]);
    }

    await ensureWebVendorAssets();
    run("wasm-pack", wasmPackArgs, { env });
    await copyEditorWorkers(pkgDir);
    await generateEditorVendor();
    await copyPackageReadme(pkgDir);
    // The published `@cognipilot/rumoca` package (full-web) also ships the lazy
    // diffsol addon; `core` does not (no lowering export to feed it).
    if (args.variant === "full-web") {
      await buildDiffsolAddon(pkgDir, args);
    }
    await fs.writeFile(
      path.join(pkgDir, "rumoca_package_meta.json"),
      `${JSON.stringify({ packageBuiltTimeUtc: nowUtc }, null, 2)}\n`,
      "utf8",
    );

    if (args.patch) {
      await patchWasmPackageJson(pkgDir, args.variant, runtimeFiles);
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
