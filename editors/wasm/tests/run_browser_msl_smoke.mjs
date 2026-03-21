import { writeFile } from "node:fs/promises";
import { spawnSync } from "node:child_process";
import path from "node:path";

import { chromium } from "playwright-core";

function requiredEnv(name) {
  const value = process.env[name];
  if (!value) {
    throw new Error(`missing required env var ${name}`);
  }
  return value;
}

function envMs(name, fallback) {
  const raw = process.env[name];
  if (!raw) {
    return fallback;
  }
  const parsed = Number.parseInt(raw, 10);
  if (!Number.isFinite(parsed) || parsed <= 0) {
    throw new Error(`invalid timeout env var ${name}=${raw}`);
  }
  return parsed;
}

async function main() {
  const browserBinary = requiredEnv("RUMOCA_BROWSER_BINARY");
  const smokeUrl = requiredEnv("RUMOCA_WASM_SMOKE_URL");
  const resultPath = requiredEnv("RUMOCA_WASM_SMOKE_RESULT");
  const timeoutMs = envMs("RUMOCA_WASM_SMOKE_TIMEOUT_MS", 330000);
  const browserExecutablePath = path.isAbsolute(browserBinary)
    ? browserBinary
    : spawnSync("which", [browserBinary], { encoding: "utf8" }).stdout.trim();
  if (!browserExecutablePath) {
    throw new Error(`failed to resolve browser executable path for ${browserBinary}`);
  }

  const browser = await chromium.launch({
    executablePath: browserExecutablePath,
    headless: true,
    args: ["--no-sandbox", "--disable-gpu"],
  });

  try {
    const page = await browser.newPage();
    page.on("console", (message) => {
      console.error(`[wasm-browser] ${message.type()}: ${message.text()}`);
    });
    page.on("pageerror", (error) => {
      console.error("[wasm-browser] pageerror:", error);
    });

    await page.goto(smokeUrl, {
      waitUntil: "domcontentloaded",
      timeout: timeoutMs,
    });
    await page.waitForFunction(
      () => {
        const status = document.body?.dataset?.rumocaSmokeStatus;
        return status === "pass" || status === "fail";
      },
      undefined,
      { timeout: timeoutMs },
    );
    await page.waitForFunction(
      () => {
        const host = document.getElementById("simPlot");
        return Boolean(host?.classList.contains("rumoca-results-root"));
      },
      undefined,
      { timeout: timeoutMs },
    );

    await page.evaluate(() => {
      const modelSelect = document.getElementById("modelSelect");
      if (!(modelSelect instanceof HTMLSelectElement)) {
        throw new Error("missing model select");
      }
      const preferredModel =
        new URL(window.location.href).searchParams.get("smoke_model") || "Model";
      if (!Array.from(modelSelect.options).some((option) => option.value === preferredModel)) {
        modelSelect.appendChild(new Option(preferredModel, preferredModel));
      }
      if (!modelSelect.value) {
        const nextOption =
          Array.from(modelSelect.options).find((option) => option.value === preferredModel) ||
          Array.from(modelSelect.options).find((option) => option.value);
        if (!nextOption) {
          throw new Error("no simulation model available for run-tab smoke");
        }
        modelSelect.value = nextOption.value;
      } else if (modelSelect.value !== preferredModel) {
        modelSelect.value = preferredModel;
      }
      if (typeof window.updateSelectedModel === "function") {
        window.updateSelectedModel();
      }
    });

    const initialRunTabs = await page.locator("#simRunTabs .results-run-tab").count();
    await page.evaluate(async () => {
      if (typeof window.runSimulation !== "function") {
        throw new Error("window.runSimulation is not available");
      }
      await window.runSimulation();
    });
    await page.waitForFunction(
      (expectedCount) =>
        document.querySelectorAll("#simRunTabs .results-run-tab").length >= expectedCount,
      initialRunTabs + 1,
      { timeout: timeoutMs },
    );
    const firstRunTabs = await page.locator("#simRunTabs .results-run-tab").count();
    await page.evaluate(async () => {
      await window.runSimulation();
    });
    await page.waitForFunction(
      (expectedCount) =>
        document.querySelectorAll("#simRunTabs .results-run-tab").length >= expectedCount,
      firstRunTabs + 1,
      { timeout: timeoutMs },
    );
    const secondRunTabs = await page.locator("#simRunTabs .results-run-tab").count();
    await page.locator("#simRunTabs .results-run-close").first().click();
    await page.waitForFunction(
      (expectedCount) =>
        document.querySelectorAll("#simRunTabs .results-run-tab").length === expectedCount,
      secondRunTabs - 1,
      { timeout: timeoutMs },
    );
    await page.locator("#simRunTabs .results-run-close").first().click();
    await page.waitForFunction(
      () => document.querySelectorAll("#simRunTabs .results-run-tab").length === 0,
      undefined,
      { timeout: timeoutMs },
    );
    const reopenPath = await page.evaluate(() => {
      const tab = document.querySelector("#editorTabs .editor-tab");
      if (tab instanceof HTMLElement && tab.dataset.path) {
        return tab.dataset.path;
      }
      const row = document.querySelector("#explorerTree .sidebar-row[data-path]");
      if (row instanceof HTMLElement && row.dataset.path) {
        return row.dataset.path;
      }
      throw new Error("no project document path available for reopen");
    });
    await page.evaluate(async () => {
      while (document.querySelectorAll("#editorTabs .editor-tab").length > 0) {
        if (typeof window.closeActiveProjectDocument !== "function") {
          throw new Error("window.closeActiveProjectDocument is not available");
        }
        await window.closeActiveProjectDocument();
      }
    });
    await page.waitForFunction(
      () => document.querySelectorAll("#editorTabs .editor-tab").length === 0,
      undefined,
      { timeout: timeoutMs },
    );
    await page.evaluate((path) => {
      if (typeof window.openProjectDocument !== "function") {
        throw new Error("window.openProjectDocument is not available");
      }
      window.openProjectDocument(path);
    }, reopenPath);
    await page.waitForFunction(
      () => {
        return (
          document.querySelectorAll("#editorTabs .editor-tab").length >= 1 &&
          !document.querySelector(".left-area")?.classList.contains("pane-hidden")
        );
      },
      undefined,
      { timeout: timeoutMs },
    );
    await page.waitForFunction(
      () => document.querySelectorAll("#outlineTree .sidebar-row").length >= 1,
      undefined,
      { timeout: timeoutMs },
    );
    await page.evaluate(() => {
      const originalPrompt = window.prompt;
      window.prompt = () => "SplitSmoke.mo";
      try {
        const button = document.getElementById("explorerNewFileBtn");
        if (!(button instanceof HTMLButtonElement)) {
          throw new Error("missing explorer new file button");
        }
        button.click();
      } finally {
        window.prompt = originalPrompt;
      }
    });
    await page.waitForFunction(
      () =>
        document.querySelectorAll("#editorTabs .editor-tab").length >= 2
        && Array.from(document.querySelectorAll("#editorTabs .editor-tab")).some(
          (node) => node instanceof HTMLElement && node.dataset.path === "SplitSmoke.mo",
        ),
      undefined,
      { timeout: timeoutMs },
    );
    await page.evaluate(() => {
      const tab = document.querySelector('#editorTabs .editor-tab[data-path="SplitSmoke.mo"]');
      const zone = document.querySelector('.editor-drop-zone[data-drop-position="bottom"]');
      if (!(tab instanceof HTMLElement) || !(zone instanceof HTMLElement)) {
        throw new Error("missing editor tab or bottom split zone");
      }
      const dataTransfer = new DataTransfer();
      const dragStart = new DragEvent("dragstart", {
        bubbles: true,
        cancelable: true,
        dataTransfer,
      });
      const dragOver = new DragEvent("dragover", {
        bubbles: true,
        cancelable: true,
        dataTransfer,
      });
      const drop = new DragEvent("drop", {
        bubbles: true,
        cancelable: true,
        dataTransfer,
      });
      const dragEnd = new DragEvent("dragend", {
        bubbles: true,
        cancelable: true,
        dataTransfer,
      });
      tab.dispatchEvent(dragStart);
      zone.dispatchEvent(dragOver);
      zone.dispatchEvent(drop);
      tab.dispatchEvent(dragEnd);
    });
    await page.waitForFunction(
      () =>
        document.getElementById("editorPaneArea")?.dataset?.split === "horizontal"
        && !document.getElementById("secondaryEditorStack")?.classList.contains("pane-hidden"),
      undefined,
      { timeout: timeoutMs },
    );
    await page.evaluate(() => {
      const tab = document.querySelector('#secondaryEditorTabs .editor-tab[data-path="SplitSmoke.mo"]')
        || document.querySelector('#editorTabs .editor-tab[data-path="SplitSmoke.mo"]');
      const zone = document.querySelector('.editor-drop-zone[data-drop-position="right"]');
      if (!(tab instanceof HTMLElement) || !(zone instanceof HTMLElement)) {
        throw new Error("missing editor tab or right split zone");
      }
      const dataTransfer = new DataTransfer();
      const dragStart = new DragEvent("dragstart", {
        bubbles: true,
        cancelable: true,
        dataTransfer,
      });
      const dragOver = new DragEvent("dragover", {
        bubbles: true,
        cancelable: true,
        dataTransfer,
      });
      const drop = new DragEvent("drop", {
        bubbles: true,
        cancelable: true,
        dataTransfer,
      });
      const dragEnd = new DragEvent("dragend", {
        bubbles: true,
        cancelable: true,
        dataTransfer,
      });
      tab.dispatchEvent(dragStart);
      zone.dispatchEvent(dragOver);
      zone.dispatchEvent(drop);
      tab.dispatchEvent(dragEnd);
    });
    await page.waitForFunction(
      () => document.getElementById("editorPaneArea")?.dataset?.split === "vertical",
      undefined,
      { timeout: timeoutMs },
    );

    const [status, rawPayload, workbench, layout, runTabs] = await Promise.all([
      page.evaluate(() => document.body?.dataset?.rumocaSmokeStatus || ""),
      page.locator("#rumocaSmokeResult").textContent(),
      page.evaluate(() => {
        const activityBrand = document.querySelector(".activity-brand");
        const sidepanel = document.getElementById("workbenchSidepanel");
        const sidebar = document.querySelector(".workbench-sidebar");
        const sidebarHeaders = Array.from(
          document.querySelectorAll(".sidebar-section-label"),
        ).map((node) => (node.textContent || "").trim());
        const explorerItems = document.querySelectorAll("#explorerTree .sidebar-row").length;
        const outlineItems = document.querySelectorAll("#outlineTree .sidebar-row").length;
        const editorTabs = document.querySelectorAll("#editorTabs .editor-tab").length;
        const editorRunBtn = document.getElementById("editorRunBtn");
        const editorCodegenBtn = document.getElementById("editorCodegenBtn");
        const editorSettingsBtn = document.getElementById("editorSimSettingsBtn");
        const editorTabCloseBtn = document.querySelector("#editorTabs .editor-tab-close");
        const outputTabs = document.getElementById("outputTabs");
        const codegenOutputTab = document.getElementById("codegenTab");
        const resultsSettingsBtn = document.getElementById("resultsSettingsBtn");
        const resultsCloseBtn = document.getElementById("resultsCloseBtn");
        const leftArea = document.querySelector(".left-area");
        const rightPanel = document.getElementById("rightPanel");
        const workbenchTop = document.querySelector(".workbench-top");
        const editorPaneArea = document.getElementById("editorPaneArea");
        const secondaryEditorStack = document.getElementById("secondaryEditorStack");
        const bottomTabs = Array.from(document.querySelectorAll(".bottom-tab")).map(
          (node) => node.textContent || "",
        );
        const runtimeStatus = document.getElementById("outputRuntimeStatus");
        return {
          ok:
            Boolean(activityBrand) &&
            Boolean(sidepanel) &&
            Boolean(sidebar) &&
            sidebarHeaders.includes("Explorer") &&
            sidebarHeaders.includes("Outline") &&
            explorerItems >= 1 &&
            outlineItems >= 1 &&
            editorTabs >= 1 &&
            Boolean(editorRunBtn) &&
            Boolean(editorCodegenBtn) &&
            Boolean(editorSettingsBtn) &&
            Boolean(editorTabCloseBtn) &&
            Boolean(outputTabs) &&
            Boolean(codegenOutputTab) &&
            Boolean(resultsSettingsBtn) &&
            Boolean(resultsCloseBtn) &&
            Boolean(leftArea) &&
            Boolean(rightPanel) &&
            Boolean(workbenchTop) &&
            editorPaneArea?.dataset?.split === "vertical" &&
            !secondaryEditorStack?.classList.contains("pane-hidden") &&
            bottomTabs.some((label) => label.includes("Output")) &&
            bottomTabs.some((label) => label.includes("Errors")) &&
            runtimeStatus?.textContent?.includes("Ready") &&
            runtimeStatus?.dataset?.tone === "ready",
          hasActivityBrand: Boolean(activityBrand),
          sidebarHeaders,
          explorerItems,
          outlineItems,
          editorTabs,
          codegenTabHidden: codegenOutputTab?.classList.contains("hidden") ?? null,
          outputActivePane: outputTabs?.dataset?.activePane || "",
          leftPaneHidden: leftArea?.classList.contains("pane-hidden") ?? null,
          rightPaneHidden: rightPanel?.classList.contains("pane-hidden") ?? null,
          workbenchLayout: workbenchTop?.dataset?.layout || "",
          editorPaneSplit: editorPaneArea?.dataset?.split || "",
          secondaryEditorHidden: secondaryEditorStack?.classList.contains("pane-hidden") ?? null,
          bottomTabs,
          runtimeStatus: runtimeStatus?.textContent || "",
          runtimeTone: runtimeStatus?.dataset?.tone || "",
        };
      }),
      page.evaluate(() => {
        const host = document.getElementById("simPlot");
        const sidepanel = document.getElementById("workbenchSidepanel");
        const bottomPanel = document.getElementById("bottomPanel");
        const rightPanel = document.getElementById("rightPanel");
        if (!host) {
          return { ok: false, reason: "missing-results-host" };
        }
        const hostStyle = getComputedStyle(host);
        const hostRect = host.getBoundingClientRect();
        const sidepanelRect = sidepanel?.getBoundingClientRect() || null;
        const bottomPanelRect = bottomPanel?.getBoundingClientRect() || null;
        const rightPaneHidden = rightPanel?.classList.contains("pane-hidden") ?? false;
        const title = host.querySelector(".rumoca-results-title");
        const tabs = host.querySelector(".rumoca-results-tabs");
        return {
          ok:
            host.classList.contains("rumoca-results-root") &&
            hostStyle.position === "relative" &&
            Boolean(sidepanelRect) &&
            Boolean(bottomPanelRect) &&
            bottomPanelRect.left >= sidepanelRect.right - 2 &&
            Boolean(title) &&
            Boolean(tabs) &&
            (rightPaneHidden || (hostRect.width > 0 && hostRect.height > 0)),
          hostPosition: hostStyle.position,
          hasResultsRootClass: host.classList.contains("rumoca-results-root"),
          hasTitle: Boolean(title),
          hasTabs: Boolean(tabs),
          rightPaneHidden,
          sidepanelRect: sidepanelRect
            ? {
                left: sidepanelRect.left,
                right: sidepanelRect.right,
                width: sidepanelRect.width,
              }
            : null,
          bottomPanelRect: bottomPanelRect
            ? {
                left: bottomPanelRect.left,
                right: bottomPanelRect.right,
                width: bottomPanelRect.width,
              }
            : null,
          hostRect: {
            width: hostRect.width,
            height: hostRect.height,
          },
        };
      }),
      page.evaluate(() => {
        const tabs = Array.from(document.querySelectorAll("#simRunTabs .results-run-tab")).map(
          (node) => node.textContent || "",
        );
        const closeButtons = document.querySelectorAll("#simRunTabs .results-run-close").length;
        return {
          ok: tabs.length === 0 && closeButtons === 0,
          labels: tabs,
          count: tabs.length,
          closeButtons,
        };
      }),
    ]);
    const payload = rawPayload ? JSON.parse(rawPayload) : {};
    await writeFile(
      resultPath,
      `${JSON.stringify({ status, payload, workbench, layout, runTabs }, null, 2)}\n`,
      "utf8",
    );

    if (status !== "pass") {
      throw new Error(`wasm editor smoke reported ${status}`);
    }
    if (!workbench?.ok) {
      throw new Error(`wasm workbench layout check failed: ${JSON.stringify(workbench)}`);
    }
    if (!layout?.ok) {
      throw new Error(`wasm results layout check failed: ${JSON.stringify(layout)}`);
    }
    if (!runTabs?.ok) {
      throw new Error(`wasm results run-tabs check failed: ${JSON.stringify(runTabs)}`);
    }
  } finally {
    await browser.close();
  }
}

main().catch((error) => {
  console.error("[wasm-browser] failed");
  console.error(error);
  process.exit(1);
});
