import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const testDir = path.dirname(fileURLToPath(import.meta.url));
const extensionRoot = path.resolve(testDir, "..");
const packageJsonPath = path.join(extensionRoot, "package.json");
const languageConfigurationPath = path.join(extensionRoot, "language-configuration.json");

function readJson(filePath) {
  return JSON.parse(fs.readFileSync(filePath, "utf8"));
}

test("modelica language wiring keeps comment configuration enabled", () => {
  const packageJson = readJson(packageJsonPath);
  const languageConfiguration = readJson(languageConfigurationPath);
  const modelicaContribution = (packageJson.contributes?.languages ?? []).find(
    (entry) => entry?.id === "modelica",
  );

  assert.ok(modelicaContribution, "expected a modelica language contribution");
  assert.equal(
    modelicaContribution.configuration,
    "./language-configuration.json",
    "expected modelica to use language-configuration.json",
  );
  assert.deepEqual(
    languageConfiguration.comments,
    {
      lineComment: "//",
      blockComment: ["/*", "*/"],
    },
    "expected modelica comments to support editor toggle-comment commands",
  );
});

test("galec language wiring enables block-comment configuration", () => {
  const packageJson = readJson(packageJsonPath);
  const galecContribution = (packageJson.contributes?.languages ?? []).find(
    (entry) => entry?.id === "galec",
  );

  assert.ok(galecContribution, "expected a galec language contribution");
  assert.equal(
    galecContribution.configuration,
    "./galec-language-configuration.json",
    "expected galec to use galec-language-configuration.json",
  );
  const galecConfiguration = readJson(
    path.join(extensionRoot, "galec-language-configuration.json"),
  );
  assert.deepEqual(
    galecConfiguration.comments,
    { blockComment: ["/*", "*/"] },
    "GALEC uses /* */ block comments only (no line comments, GAL-019)",
  );
});
