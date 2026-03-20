import {
  createProjectFilesystem,
  inferModelicaFileName,
} from "../src/modules/project_fs.js";

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function activeDocumentRoundTrip() {
  const projectFs = createProjectFilesystem();
  const source = `model Ball\n  Real x;\nend Ball;\n`;
  projectFs.setActiveDocument(inferModelicaFileName(source, "Main.mo"), source);
  projectFs.setEditorState({
    rightTab: "codegen",
    bottomTab: "libraries",
    bottomPanelCollapsed: true,
    sim: { tEnd: "5.0", dt: "0.1" },
    template: "{{ dae.model_name }}",
  });

  const entries = projectFs.snapshotArchiveEntries();
  const restored = createProjectFilesystem();
  const projectState = restored.loadArchiveEntries(entries);

  assert(
    projectState.activeDocumentPath === "Ball.mo",
    `expected active document Ball.mo, got ${projectState.activeDocumentPath}`,
  );
  assert(
    projectState.activeDocumentContent === source,
    "expected active document content to survive snapshot round-trip",
  );
  assert(
    projectState.editorState?.rightTab === "codegen",
    "expected editor right tab state to survive snapshot round-trip",
  );
  assert(
    projectState.editorState?.sim?.tEnd === "5.0",
    "expected simulation settings to survive snapshot round-trip",
  );
}

function libraryArchiveRoundTrip() {
  const projectFs = createProjectFilesystem();
  projectFs.setActiveDocument("Main.mo", "model Main\nend Main;\n");
  projectFs.replaceLibraryArchive("msl", "Modelica.zip", {
    "Modelica/package.mo": "package Modelica\nend Modelica;\n",
    "Modelica/Icons.mo": "within Modelica;\npackage Icons\nend Icons;\n",
  });

  const entries = projectFs.snapshotArchiveEntries();
  const restored = createProjectFilesystem();
  restored.loadArchiveEntries(entries);

  const archives = restored.listLibraryArchives();
  assert(archives.length === 1, `expected 1 restored archive, got ${archives.length}`);
  assert(
    archives[0].fileName === "Modelica.zip",
    `expected restored archive filename, got ${archives[0].fileName}`,
  );

  const archiveFiles = restored.getArchiveFiles("msl");
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
  const projectFs = createProjectFilesystem();
  const cache = new Uint8Array([0, 1, 2, 3, 4, 5, 6, 255, 254, 253]);
  projectFs.replaceLibraryArchive("msl", "Modelica.zip", {
    "Modelica/cache.bin": cache,
    "Modelica/package.mo": "package Modelica\nend Modelica;\n",
  });

  const entries = projectFs.snapshotArchiveEntries();
  const restored = createProjectFilesystem();
  restored.loadArchiveEntries(entries);

  const archiveFiles = restored.getArchiveFiles("msl");
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

function libraryRemovalCleansFiles() {
  const projectFs = createProjectFilesystem();
  projectFs.setActiveDocument("Main.mo", "model Main\nend Main;\n");
  projectFs.replaceLibraryArchive("lib-a", "LibA.zip", {
    "Lib/package.mo": "package Lib\nend Lib;\n",
  });
  projectFs.removeLibraryArchive("lib-a");

  assert(
    projectFs.listLibraryArchives().length === 0,
    "expected no library archives after removal",
  );
  assert(
    !projectFs.listFiles().some((file) => file.path === "Lib/package.mo"),
    "expected library files to be removed with archive removal",
  );
}

function folderEntryImportRestoresProjectState() {
  const projectFs = createProjectFilesystem();
  const projectState = projectFs.loadFileEntries([
    {
      path: "Demo/Plant.mo",
      content: "within Demo;\nmodel Plant\nend Plant;\n",
    },
    {
      path: ".rumoca/editor-state.json",
      content: JSON.stringify({
        rightTab: "simulate",
        bottomTab: "packages",
        sim: { tEnd: "20", dt: "0" },
      }),
    },
    {
      path: ".rumoca/cache/library.bin",
      content: "stale cache data",
    },
  ]);

  assert(
    projectState.activeDocumentPath === "Demo/Plant.mo",
    `expected Demo/Plant.mo active document, got ${projectState.activeDocumentPath}`,
  );
  assert(
    projectState.editorState?.bottomTab === "packages",
    "expected editor sidecar state to load from file entries",
  );
  assert(
    !projectFs.listFiles().some((file) => file.path.startsWith(".rumoca/cache/")),
    "expected .rumoca/cache entries to be excluded from imported project files",
  );
}

activeDocumentRoundTrip();
libraryArchiveRoundTrip();
binaryArchiveRoundTrip();
libraryRemovalCleansFiles();
folderEntryImportRestoresProjectState();
