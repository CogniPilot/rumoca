import { writeFile } from "node:fs/promises";
import { spawnSync } from "node:child_process";
import path from "node:path";

import { chromium } from "playwright-core";

const SIMULATION_SMOKE_MODEL = "BrowserRunSmoke";
const SIMULATION_SMOKE_SOURCE = `
model ${SIMULATION_SMOKE_MODEL}
  Real x(start = 1.0);
equation
  der(x) = -x;
end ${SIMULATION_SMOKE_MODEL};
`;
const RESULT_TAB_SELECTOR = [
  '#editorTabs .editor-tab[data-path*="rumoca-result."][data-path$=".json"]',
  '#secondaryEditorTabs .editor-tab[data-path*="rumoca-result."][data-path$=".json"]',
].join(", ");

function isResultPath(pathValue) {
  return /(^|\/)rumoca-result\..*\.json$/.test(String(pathValue || ""));
}

function requiredArg(name) {
  const idx = process.argv.indexOf(name);
  const value = idx >= 0 ? process.argv[idx + 1] : undefined;
  if (!value) {
    throw new Error(`missing required arg ${name}`);
  }
  return value;
}

async function main() {
  const browserBinary = requiredArg("--browser-binary");
  const smokeUrl = requiredArg("--smoke-url");
  const resultPath = requiredArg("--smoke-result");
  const timeoutMs = 330000;
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
        const runtimeStatus = document.getElementById("outputRuntimeStatus");
        return (
          status === "pass" ||
          status === "fail" ||
          runtimeStatus?.dataset?.tone === "error"
        );
      },
      undefined,
      { timeout: timeoutMs },
    );
    const startupFailure = await page.evaluate(() => {
      const runtimeStatus = document.getElementById("outputRuntimeStatus");
      if (runtimeStatus?.dataset?.tone !== "error") {
        return null;
      }
      return {
        runtimeStatus: runtimeStatus.textContent || "",
        output: document.getElementById("terminalOutput")?.textContent || "",
      };
    });
    if (startupFailure) {
      throw new Error(`wasm runtime startup failed: ${JSON.stringify(startupFailure)}`);
    }

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

    await page.evaluate(
      ({ modelName, source }) => {
        if (!window.editor?.setValue) {
          throw new Error("editor unavailable for simulation run smoke");
        }
        window.editor.setValue(source);
        window.compiledModels = {};
        const modelSelect = document.getElementById("modelSelect");
        if (!(modelSelect instanceof HTMLSelectElement)) {
          throw new Error("missing model select");
        }
        if (!Array.from(modelSelect.options).some((option) => option.value === modelName)) {
          modelSelect.appendChild(new Option(modelName, modelName));
        }
        modelSelect.value = modelName;
        if (typeof window.updateSelectedModel === "function") {
          window.updateSelectedModel();
        }
      },
      {
        modelName: SIMULATION_SMOKE_MODEL,
        source: SIMULATION_SMOKE_SOURCE,
      },
    );

    const initialResultTabs = await page.locator(RESULT_TAB_SELECTOR).count();
    await page.evaluate(async () => {
      if (typeof window.runSimulation !== "function") {
        throw new Error("window.runSimulation is not available");
      }
      await window.runSimulation();
    });
    await page.waitForFunction(
      ({ expectedCount }) => {
        const tabs = Array.from(document.querySelectorAll("#editorTabs .editor-tab, #secondaryEditorTabs .editor-tab"));
        return tabs.filter((tab) => /(^|\/)rumoca-result\..*\.json$/.test(tab.dataset?.path || "")).length >= expectedCount;
      },
      { expectedCount: initialResultTabs + 1 },
      { timeout: timeoutMs },
    );
    await page.waitForFunction(
      () =>
        Array.from(document.querySelectorAll("#primaryResultFileView, #secondaryResultFileView"))
          .some((view) => !view.hidden && view.querySelector(".rumoca-results-root")),
      undefined,
      { timeout: timeoutMs },
    );
    const resultTabs = await page.evaluate(() => {
      const tabs = Array.from(document.querySelectorAll("#editorTabs .editor-tab, #secondaryEditorTabs .editor-tab"));
      const resultTabs = tabs
        .filter((tab) => /(^|\/)rumoca-result\..*\.json$/.test(tab.dataset?.path || ""))
        .map((tab) => ({
          path: tab.dataset?.path || "",
          active: tab.classList.contains("active"),
          label: tab.textContent || "",
        }));
      const visibleResultViews = Array.from(
        document.querySelectorAll("#primaryResultFileView, #secondaryResultFileView"),
      ).filter((view) => !view.hidden && view.querySelector(".rumoca-results-root"));
      return {
        ok:
          resultTabs.length >= 1 &&
          resultTabs.some((tab) => /^results\/rumoca-result\..*\.json$/.test(tab.path)) &&
          visibleResultViews.length >= 1,
        count: resultTabs.length,
        labels: resultTabs.map((tab) => tab.label),
        paths: resultTabs.map((tab) => tab.path),
        defaultResultsPaths: resultTabs
          .map((tab) => tab.path)
          .filter((tabPath) => /^results\/rumoca-result\..*\.json$/.test(tabPath)),
        activePaths: resultTabs.filter((tab) => tab.active).map((tab) => tab.path),
        visibleResultViews: visibleResultViews.length,
      };
    });
    const resultLayout = await page.evaluate(() => {
      const visibleView = Array.from(
        document.querySelectorAll("#primaryResultFileView, #secondaryResultFileView"),
      ).find((view) => !view.hidden && view.querySelector(".rumoca-results-root"));
      const sidepanel = document.getElementById("workbenchSidepanel");
      const bottomPanel = document.getElementById("bottomPanel");
      if (!visibleView) {
        return { ok: false, reason: "missing-visible-result-editor" };
      }
      const host = visibleView;
      const hostRect = host.getBoundingClientRect();
      const sidepanelRect = sidepanel?.getBoundingClientRect() || null;
      const bottomPanelRect = bottomPanel?.getBoundingClientRect() || null;
      const tabs = host.querySelector(".rumoca-results-tabs");
      return {
        ok:
          Boolean(sidepanelRect) &&
          Boolean(bottomPanelRect) &&
          bottomPanelRect.left >= sidepanelRect.right - 2 &&
          Boolean(tabs) &&
          hostRect.width > 0 &&
          hostRect.height > 0,
        hostPosition: getComputedStyle(host).position,
        hasTabs: Boolean(tabs),
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
    });
    const reopenPath = await page.evaluate(() => {
      const tab = document.querySelector('#editorTabs .editor-tab[data-path$=".mo"], #secondaryEditorTabs .editor-tab[data-path$=".mo"]');
      if (tab instanceof HTMLElement && tab.dataset.path) {
        return tab.dataset.path;
      }
      const row = document.querySelector('#explorerTree .sidebar-row[data-path$=".mo"]');
      if (row instanceof HTMLElement && row.dataset.path) {
        return row.dataset.path;
      }
      throw new Error("no workspace document path available for reopen");
    });
    await page.evaluate(async () => {
      while (document.querySelectorAll("#editorTabs .editor-tab").length > 0) {
        if (typeof window.closeActiveWorkspaceDocument !== "function") {
          throw new Error("window.closeActiveWorkspaceDocument is not available");
        }
        await window.closeActiveWorkspaceDocument();
      }
    });
    await page.waitForFunction(
      () => document.querySelectorAll("#editorTabs .editor-tab").length === 0,
      undefined,
      { timeout: timeoutMs },
    );
    await page.evaluate((path) => {
      if (typeof window.openWorkspaceDocument !== "function") {
        throw new Error("window.openWorkspaceDocument is not available");
      }
      window.openWorkspaceDocument(path);
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

    const [status, rawPayload, workbench] = await Promise.all([
      page.evaluate(() => document.body?.dataset?.rumocaSmokeStatus || ""),
      page.locator("#rumocaSmokeResult").textContent(),
      page.evaluate(() => {
        const sidebarBrandToggle = document.getElementById("sidebarBrandToggle");
        const sidepanel = document.getElementById("workbenchSidepanel");
        const sidebar = document.querySelector(".workbench-sidebar");
        const sidebarHeaders = Array.from(
          document.querySelectorAll(".sidebar-section-label"),
        ).map((node) => (node.textContent || "").trim());
        const explorerItems = document.querySelectorAll("#explorerTree .sidebar-row").length;
        const outlineItems = document.querySelectorAll("#outlineTree .sidebar-row").length;
        const editorTabs = document.querySelectorAll("#editorTabs .editor-tab").length;
        const editorCodegenBtn = document.getElementById("editorCodegenBtn");
        const editorTabCloseBtn = document.querySelector("#editorTabs .editor-tab-close");
        const leftArea = document.querySelector(".left-area");
        const workbenchTop = document.querySelector(".workbench-top");
        const editorPaneArea = document.getElementById("editorPaneArea");
        const secondaryEditorStack = document.getElementById("secondaryEditorStack");
        const bottomTabs = Array.from(document.querySelectorAll(".bottom-tab")).map(
          (node) => node.textContent || "",
        );
        const runtimeStatus = document.getElementById("outputRuntimeStatus");
        return {
          ok:
            Boolean(sidebarBrandToggle) &&
            Boolean(sidepanel) &&
            Boolean(sidebar) &&
            sidebarHeaders.includes("Explorer") &&
            sidebarHeaders.includes("Outline") &&
            explorerItems >= 1 &&
            outlineItems >= 1 &&
            editorTabs >= 1 &&
            Boolean(editorCodegenBtn) &&
            Boolean(editorTabCloseBtn) &&
            Boolean(leftArea) &&
            Boolean(workbenchTop) &&
            editorPaneArea?.dataset?.split === "vertical" &&
            !secondaryEditorStack?.classList.contains("pane-hidden") &&
            bottomTabs.some((label) => label.includes("Output")) &&
            bottomTabs.some((label) => label.includes("Errors")) &&
            runtimeStatus?.textContent?.includes("Ready") &&
            runtimeStatus?.dataset?.tone === "ready",
          hasSidebarBrandToggle: Boolean(sidebarBrandToggle),
          sidebarHeaders,
          explorerItems,
          outlineItems,
          editorTabs,
          leftPaneHidden: leftArea?.classList.contains("pane-hidden") ?? null,
          workbenchLayout: workbenchTop?.dataset?.layout || "",
          editorPaneSplit: editorPaneArea?.dataset?.split || "",
          secondaryEditorHidden: secondaryEditorStack?.classList.contains("pane-hidden") ?? null,
          bottomTabs,
          runtimeStatus: runtimeStatus?.textContent || "",
          runtimeTone: runtimeStatus?.dataset?.tone || "",
        };
      }),
    ]);
    const payload = rawPayload ? JSON.parse(rawPayload) : {};
    await writeFile(
      resultPath,
      `${JSON.stringify({ status, payload, workbench, resultLayout, resultTabs }, null, 2)}\n`,
      "utf8",
    );

    if (status !== "pass") {
      throw new Error(`playground smoke reported ${status}`);
    }
    if (!workbench?.ok) {
      throw new Error(`wasm workbench layout check failed: ${JSON.stringify(workbench)}`);
    }
    if (!resultLayout?.ok) {
      throw new Error(`wasm results layout check failed: ${JSON.stringify(resultLayout)}`);
    }
    if (!resultTabs?.ok) {
      throw new Error(`wasm results editor-tab check failed: ${JSON.stringify(resultTabs)}`);
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
