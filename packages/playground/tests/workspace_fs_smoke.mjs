import {
  createWorkspaceFilesystem,
  inferModelicaFileName,
} from "../src/modules/workspace_fs.js";

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function activeDocumentRoundTrip() {
  const workspaceFs = createWorkspaceFilesystem();
  const source = `model Ball\n  Real x;\nend Ball;\n`;
  workspaceFs.setActiveDocument(inferModelicaFileName(source, "Main.mo"), source);
  workspaceFs.setEditorState({
    rightTab: "codegen",
    bottomTab: "errors",
    bottomPanelCollapsed: true,
    sim: { tEnd: "5.0", dt: "0.1" },
    template: "{{ dae.model_name }}",
    selectedSimulationModel: "Ball",
    codegenSettings: { target: "rust" },
    sourceRootPaths: ["vendor/Modelica"],
  });

  const entries = workspaceFs.snapshotArchiveEntries();
  const restored = createWorkspaceFilesystem();
  const workspaceState = restored.loadArchiveEntries(entries);

  assert(
    workspaceState.activeDocumentPath === "Ball.mo",
    `expected active document Ball.mo, got ${workspaceState.activeDocumentPath}`,
  );
  assert(
    workspaceState.activeDocumentContent === source,
    "expected active document content to survive snapshot round-trip",
  );
  assert(
    workspaceState.editorState?.rightTab === "codegen",
    "expected editor right tab state to survive snapshot round-trip",
  );
  assert(
    workspaceState.editorState?.sim === undefined,
    "expected scenario simulation settings to stay out of editor state",
  );
  assert(
    workspaceState.editorState?.template === undefined,
    "expected codegen template settings to stay out of editor state",
  );
  assert(
    workspaceState.editorState?.selectedSimulationModel === undefined,
    "expected selected model to stay out of persisted editor state",
  );
  assert(
    workspaceState.editorState?.codegenSettings === undefined,
    "expected codegen settings to stay out of persisted editor state",
  );
  assert(
    workspaceState.editorState?.sourceRootPaths === undefined,
    "expected source-root settings to stay out of persisted editor state",
  );
}

function packageArchiveRoundTrip() {
  const workspaceFs = createWorkspaceFilesystem();
  workspaceFs.setActiveDocument("Main.mo", "model Main\nend Main;\n");
  workspaceFs.replacePackageArchive("msl", "Modelica.zip", {
    "Modelica/package.mo": "package Modelica\nend Modelica;\n",
    "Modelica/Icons.mo": "within Modelica;\npackage Icons\nend Icons;\n",
  });

  const entries = workspaceFs.snapshotArchiveEntries();
  const restored = createWorkspaceFilesystem();
  restored.loadArchiveEntries(entries);

  const archives = restored.listPackageArchives();
  assert(archives.length === 1, `expected 1 restored archive, got ${archives.length}`);
  assert(
    archives[0].fileName === "Modelica.zip",
    `expected restored archive filename, got ${archives[0].fileName}`,
  );

  const archiveFiles = restored.getPackageArchiveFiles("msl");
  assert(
    archiveFiles["Modelica/package.mo"]?.includes("package Modelica"),
    "expected restored archive to keep package.mo contents",
  );
  assert(
    archiveFiles["Modelica/Icons.mo"]?.includes("package Icons"),
    "expected restored archive to keep Icons.mo contents",
  );
}

function binaryArchiveRoundTrip() {
  const workspaceFs = createWorkspaceFilesystem();
  const cache = new Uint8Array([0, 1, 2, 3, 4, 5, 6, 255, 254, 253]);
  workspaceFs.replacePackageArchive("msl", "Modelica.zip", {
    "Modelica/cache.bin": cache,
    "Modelica/package.mo": "package Modelica\nend Modelica;\n",
  });

  const entries = workspaceFs.snapshotArchiveEntries();
  const restored = createWorkspaceFilesystem();
  restored.loadArchiveEntries(entries);

  const archiveFiles = restored.getPackageArchiveFiles("msl");
  const restoredCache = archiveFiles["Modelica/cache.bin"];
  assert(
    restoredCache instanceof Uint8Array,
    `expected cache archive entry to remain binary, got ${Object.prototype.toString.call(restoredCache)}`,
  );
  assert(
    restoredCache.length === cache.length,
    `expected cache archive entry length to match, got ${restoredCache.length} vs ${cache.length}`,
  );
  for (let index = 0; index < cache.length; index += 1) {
    assert(
      restoredCache[index] === cache[index],
      `expected cache archive byte ${index} to match, got ${restoredCache[index]} vs ${cache[index]}`,
    );
  }
}

function packageArchiveRemovalCleansFiles() {
  const workspaceFs = createWorkspaceFilesystem();
  workspaceFs.setActiveDocument("Main.mo", "model Main\nend Main;\n");
  workspaceFs.replacePackageArchive("lib-a", "LibA.zip", {
    "Lib/package.mo": "package Lib\nend Lib;\n",
  });
  workspaceFs.removePackageArchive("lib-a");

  assert(
    workspaceFs.listPackageArchives().length === 0,
    "expected no package archives after removal",
  );
  assert(
    !workspaceFs.listFiles().some((file) => file.path === "Lib/package.mo"),
    "expected package-archive files to be removed with archive removal",
  );
}

function folderEntryImportRestoresWorkspaceState() {
  const workspaceFs = createWorkspaceFilesystem();
  const workspaceState = workspaceFs.loadFileEntries([
    {
      path: "Demo/Plant.mo",
      content: "within Demo;\nmodel Plant\nend Plant;\n",
    },
    {
      path: "rumoca-editor-state.json",
      content: JSON.stringify({
        rightTab: "simulate",
        bottomTab: "errors",
        sim: { tEnd: "20", dt: "0" },
        selectedSimulationModel: "Plant",
        codegenSettings: { target: "python" },
        sourceRootPaths: ["../MSL"],
      }),
    },
    {
      path: "rumoca-cache/package-archive.bin",
      content: "stale cache data",
    },
  ]);

  assert(
    workspaceState.activeDocumentPath === "Demo/Plant.mo",
    `expected Demo/Plant.mo active document, got ${workspaceState.activeDocumentPath}`,
  );
  assert(
    workspaceState.editorState?.bottomTab === "errors",
    "expected editor generated state to load from file entries",
  );
  assert(
    workspaceState.editorState?.sim === undefined,
    "expected imported scenario simulation settings to be ignored",
  );
  assert(
    workspaceState.editorState?.selectedSimulationModel === undefined,
    "expected imported selected model state to be ignored",
  );
  assert(
    workspaceState.editorState?.codegenSettings === undefined,
    "expected imported codegen settings to be ignored",
  );
  assert(
    workspaceState.editorState?.sourceRootPaths === undefined,
    "expected imported source-root settings to be ignored",
  );
  assert(
    !workspaceFs.listFiles().some((file) => file.path.startsWith("rumoca-cache/")),
    "expected rumoca-cache entries to be excluded from imported workspace files",
  );
}

activeDocumentRoundTrip();
packageArchiveRoundTrip();
binaryArchiveRoundTrip();
packageArchiveRemovalCleansFiles();
folderEntryImportRestoresWorkspaceState();
