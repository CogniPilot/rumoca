const DEFAULT_ACTIVE_DOCUMENT = 'examples/models/Ball.mo';

const DEFAULT_EXAMPLE_FILE_PATHS = [
    'examples/.gitignore',
    'examples/README.md',
    'examples/assets/models/README.md',
    'examples/assets/models/airplane.glb',
    'examples/assets/models/buggy.glb',
    'examples/assets/models/drone.glb',
    'examples/assets/sand_pbr/GroundSand005_AO_1K.jpg',
    'examples/assets/sand_pbr/GroundSand005_BUMP_1K.jpg',
    'examples/assets/sand_pbr/GroundSand005_COL_1K.jpg',
    'examples/assets/sand_pbr/GroundSand005_NRM_1K.jpg',
    'examples/assets/skybox/README_skiingpenguins_arid2.txt',
    'examples/assets/skybox/arid2_bk.jpg',
    'examples/assets/skybox/arid2_dn.jpg',
    'examples/assets/skybox/arid2_ft.jpg',
    'examples/assets/skybox/arid2_lf.jpg',
    'examples/assets/skybox/arid2_rt.jpg',
    'examples/assets/skybox/arid2_up.jpg',
    'examples/codegen/README.md',
    'examples/codegen/custom_casadi.jinja',
    'examples/codegen/rumoca-scenario.galec_counter_production.toml',
    'examples/codegen/rumoca-scenario.ball_jax.toml',
    'examples/codegen/rumoca-scenario.sympy_decay_custom_casadi.toml',
    'examples/codegen/rumoca-scenario.sympy_decay_standalone_web.toml',
    'examples/codegen/rumoca-scenario.sympy_decay_sympy.toml',
    'examples/codegen/standalone_web/README.md',
    'examples/codegen/standalone_web/javascript.jinja',
    'examples/codegen/standalone_web/standalone_html.jinja',
    'examples/codegen/standalone_web/target.toml',
    'examples/interactive/README.md',
    'examples/interactive/fixedwing/FixedWingSIL.mo',
    'examples/interactive/fixedwing/README.md',
    'examples/interactive/fixedwing/fixedwing_scene.js',
    'examples/interactive/fixedwing/rumoca-scenario.toml',
    'examples/interactive/quadrotor/QuadrotorSIL.mo',
    'examples/interactive/quadrotor/README.md',
    'examples/interactive/quadrotor/quadrotor_scene.js',
    'examples/interactive/quadrotor/rumoca-scenario.acro.toml',
    'examples/interactive/rover/README.md',
    'examples/interactive/rover/Rover.mo',
    'examples/interactive/rover/rover_scene.js',
    'examples/interactive/rover/rumoca-scenario.toml',
    'examples/modelica_dependencies.toml',
    'examples/models/Ball.mo',
    'examples/models/Circuit.mo',
    'examples/models/GalecCounter.mo',
    'examples/models/KalmanFilterStepTest.mo',
    'examples/models/PIDMSL.mo',
    'examples/models/README.md',
    'examples/models/SwitchedRLC.mo',
    'examples/models/SwitchedRLC_MSL.mo',
    'examples/models/SympyDecay.mo',
    'examples/rumoca-workspace.toml',
    'examples/simulation/README.md',
    'examples/simulation/rumoca-scenario.ball.toml',
    'examples/simulation/rumoca-scenario.kalman_filter_step_test.toml',
    'examples/simulation/rumoca-scenario.pidmsl.toml',
    'examples/simulation/rumoca-scenario.switched_rlc.toml',
    'examples/simulation/rumoca-scenario.switched_rlc_msl.toml',
    'examples/simulation/rumoca-scenario.sympy_decay.toml',
    'target/cmm/CMM-v0.0.2/LieGroup/package.mo',
    'target/cmm/CMM-v0.0.2/LieGroups/package.mo',
    'target/cmm/CMM-v0.0.2/RigidBody/package.mo',
];

const TEXT_EXTENSIONS = new Set([
    '.css',
    '.gitignore',
    '.html',
    '.jinja',
    '.js',
    '.json',
    '.md',
    '.mo',
    '.svg',
    '.toml',
    '.txt',
    '.yaml',
    '.yml',
]);

const DEFAULT_WORKSPACE_FOLDERS = [
    'target',
    'target/msl',
    'target/msl/ModelicaStandardLibrary-4.1.0',
    'target/cmm',
    'target/cmm/CMM-v0.0.2',
];

function extensionOf(path) {
    const leaf = String(path || '').split('/').pop() || '';
    if (leaf === '.gitignore') {
        return '.gitignore';
    }
    const dot = leaf.lastIndexOf('.');
    return dot >= 0 ? leaf.slice(dot).toLowerCase() : '';
}

function isTextPath(path) {
    return TEXT_EXTENSIONS.has(extensionOf(path));
}

function candidateUrlsFor(path) {
    const normalized = String(path || '').replace(/^\/+/, '');
    const assetBase = String(globalThis.rumocaRepoAssetBase || '').trim();
    const baseCandidates = assetBase ? [assetBase] : [];
    return [
        ...baseCandidates.map((base) => {
            const pageUrl = globalThis.location?.href || import.meta.url;
            return new URL(normalized, new URL(base, pageUrl)).href;
        }),
        `../../${normalized}`,
        `/${normalized}`,
    ];
}

async function fetchWorkspaceFile(path) {
    let lastError = null;
    for (const candidate of candidateUrlsFor(path)) {
        try {
            const response = await fetch(candidate, { cache: 'no-cache' });
            if (!response.ok) {
                lastError = new Error(`${candidate}: ${response.status}`);
                continue;
            }
            return {
                path,
                content: isTextPath(path)
                    ? await response.text()
                    : new Uint8Array(await response.arrayBuffer()),
            };
        } catch (error) {
            lastError = error;
        }
    }
    throw lastError || new Error(`Failed to fetch ${path}`);
}

function explorerBranchIdsForPaths(paths) {
    const branchIds = new Set();
    for (const path of paths) {
        const parts = String(path || '').split('/').filter(Boolean);
        const folderParts = parts.slice(0, -1);
        let prefix = '';
        for (const part of folderParts) {
            prefix = prefix ? `${prefix}/${part}` : part;
            branchIds.add(`explorer:${prefix}`);
        }
    }
    return [...branchIds].sort((lhs, rhs) => lhs.localeCompare(rhs));
}

export async function loadDefaultWorkspaceEntries() {
    const entries = [];
    const missing = [];
    const fetched = await Promise.allSettled(
        DEFAULT_EXAMPLE_FILE_PATHS.map((path) => fetchWorkspaceFile(path)),
    );
    for (let index = 0; index < fetched.length; index += 1) {
        const result = fetched[index];
        if (result.status === 'fulfilled') {
            entries.push(result.value);
        } else {
            missing.push(`${DEFAULT_EXAMPLE_FILE_PATHS[index]}: ${result.reason?.message || result.reason}`);
        }
    }
    if (!entries.some((entry) => entry.path === DEFAULT_ACTIVE_DOCUMENT)) {
        throw new Error(`Default example workspace is missing ${DEFAULT_ACTIVE_DOCUMENT}`);
    }
    entries.push({
        path: 'rumoca-workspace.json',
        content: `${JSON.stringify({
            schemaVersion: 1,
            activeDocument: DEFAULT_ACTIVE_DOCUMENT,
            packageArchives: [],
            folders: DEFAULT_WORKSPACE_FOLDERS,
        }, null, 2)}\n`,
    });
    entries.push({
        path: 'rumoca-editor-state.json',
        content: `${JSON.stringify({
            schemaVersion: 1,
            openDocuments: [
                DEFAULT_ACTIVE_DOCUMENT,
                'examples/simulation/rumoca-scenario.ball.toml',
            ],
            selectedExplorerPath: DEFAULT_ACTIVE_DOCUMENT,
            explorerCollapsedNodeIds: explorerBranchIdsForPaths([
                ...DEFAULT_EXAMPLE_FILE_PATHS,
                ...DEFAULT_WORKSPACE_FOLDERS.map((folder) => `${folder}/.folder`),
            ]),
            projectSectionCollapsed: true,
            explorerSectionCollapsed: false,
            outlineSectionCollapsed: false,
            sidebarCollapsed: false,
            bottomPanelCollapsed: false,
            bottomTab: 'output',
        }, null, 2)}\n`,
    });
    if (missing.length > 0) {
        console.warn('Default example workspace loaded with missing files:', missing);
    }
    return entries;
}
