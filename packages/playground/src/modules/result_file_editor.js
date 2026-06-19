import * as shared from '../../vendor/visualization_shared.js';
import * as appFactory from '../../vendor/results_app.js';
import * as threeViewer from '../../vendor/three_viewer.js';
import { renderMarkdownHtml } from '../../vendor/markdown_renderer.js';

const INTERACTIVE_RUN_PREFIX = 'rumoca-input/';

function trimMaybeString(value) {
    return typeof value === 'string' ? value.trim() : '';
}

function cloneJson(value) {
    return JSON.parse(JSON.stringify(value));
}

function hashText(value) {
    let hash = 2166136261;
    const text = String(value || '');
    for (let index = 0; index < text.length; index += 1) {
        hash ^= text.charCodeAt(index);
        hash = Math.imul(hash, 16777619);
    }
    return (hash >>> 0).toString(36);
}

function safePathSegment(value, fallback = 'scenario') {
    const cleaned = trimMaybeString(value)
        .replace(/\.[^.]+$/, '')
        .replace(/[^A-Za-z0-9_.-]+/g, '_')
        .replace(/^_+|_+$/g, '');
    return cleaned || fallback;
}

function emptyResultDocument(message) {
    const wrapper = document.createElement('div');
    wrapper.className = 'result-file-empty';
    wrapper.textContent = message;
    return wrapper;
}

function parseResultDocument(content) {
    let parsed;
    try {
        parsed = JSON.parse(String(content || ''));
    } catch {
        return null;
    }
    return shared.normalizePersistedSimulationRun(parsed) || null;
}

export function isRumocaResultPath(path) {
    const filename = trimMaybeString(path).split('/').pop() || '';
    return filename.startsWith('rumoca-result.') && filename.endsWith('.json');
}

export function isInteractiveRunPath(path) {
    return trimMaybeString(path).startsWith(INTERACTIVE_RUN_PREFIX);
}

export function interactiveRunPathForScenario(scenarioPath, modelName = '') {
    const filename = trimMaybeString(scenarioPath).split('/').pop() || 'scenario';
    const label = safePathSegment(modelName) || safePathSegment(filename);
    return `${INTERACTIVE_RUN_PREFIX}${hashText(scenarioPath)}/${label}.input`;
}

function fileExtension(path) {
    const filename = trimMaybeString(path).split('/').pop() || '';
    const dot = filename.lastIndexOf('.');
    return dot === -1 ? '' : filename.slice(dot).toLowerCase();
}

function isImageAssetPath(path) {
    return ['.png', '.jpg', '.jpeg', '.gif', '.webp', '.svg'].includes(fileExtension(path));
}

function isGlbAssetPath(path) {
    return fileExtension(path) === '.glb';
}

function isMarkdownPath(path) {
    return ['.md', '.markdown'].includes(fileExtension(path));
}

function isPreviewAssetPath(path) {
    return isImageAssetPath(path) || isGlbAssetPath(path) || isMarkdownPath(path);
}

function bytesForContent(content) {
    if (content instanceof Uint8Array) {
        return content;
    }
    if (ArrayBuffer.isView(content)) {
        return new Uint8Array(content.buffer, content.byteOffset, content.byteLength);
    }
    if (content instanceof ArrayBuffer) {
        return new Uint8Array(content);
    }
    return null;
}

function arrayBufferForBytes(bytes) {
    if (!bytes) {
        return null;
    }
    return bytes.buffer.slice(bytes.byteOffset, bytes.byteOffset + bytes.byteLength);
}

function fileSizeLabel(content) {
    const bytes = bytesForContent(content);
    const size = bytes ? bytes.byteLength : String(content || '').length;
    if (size < 1024) {
        return `${size} B`;
    }
    if (size < 1024 * 1024) {
        return `${(size / 1024).toFixed(1)} KiB`;
    }
    return `${(size / 1024 / 1024).toFixed(1)} MiB`;
}

function contentSignature(content) {
    const bytes = bytesForContent(content);
    if (!bytes) {
        return String(content || '');
    }
    let hash = 2166136261;
    const stride = Math.max(1, Math.floor(bytes.byteLength / 64));
    for (let index = 0; index < bytes.byteLength; index += stride) {
        hash ^= bytes[index];
        hash = Math.imul(hash, 16777619);
    }
    return `${bytes.byteLength}:${hash >>> 0}`;
}

function mimeTypeForPath(path) {
    switch (fileExtension(path)) {
        case '.png':
            return 'image/png';
        case '.jpg':
        case '.jpeg':
            return 'image/jpeg';
        case '.gif':
            return 'image/gif';
        case '.webp':
            return 'image/webp';
        case '.svg':
            return 'image/svg+xml';
        case '.glb':
            return 'model/gltf-binary';
        case '.md':
        case '.markdown':
            return 'text/markdown';
        default:
            return 'application/octet-stream';
    }
}

function assetShell(_path, _content, actions = []) {
    const root = document.createElement('div');
    root.className = 'asset-file-viewer';
    const body = document.createElement('div');
    body.className = 'asset-file-body';
    if (actions.length > 0) {
        const actionGroup = document.createElement('div');
        actionGroup.className = 'asset-file-actions';
        for (const action of actions) {
            actionGroup.appendChild(action);
        }
        root.appendChild(actionGroup);
    }
    root.append(body);
    return { root, body };
}

function disposeObject3d(object) {
    if (!object) {
        return;
    }
    object.traverse?.((node) => {
        node.geometry?.dispose?.();
        const materials = Array.isArray(node.material) ? node.material : [node.material];
        for (const material of materials) {
            if (!material) continue;
            for (const value of Object.values(material)) {
                if (value && typeof value.dispose === 'function') {
                    value.dispose();
                }
            }
            material.dispose?.();
        }
    });
}

function renderImageAsset(path, content) {
    const bytes = bytesForContent(content);
    const { root, body } = assetShell(path, content);
    if (!bytes && typeof content !== 'string') {
        body.appendChild(emptyResultDocument('This image file could not be loaded.'));
        return { element: root };
    }
    const img = document.createElement('img');
    img.className = 'asset-image-preview';
    let objectUrl = null;
    if (typeof content === 'string' && fileExtension(path) === '.svg') {
        img.src = `data:image/svg+xml;charset=utf-8,${encodeURIComponent(content)}`;
    } else {
        objectUrl = URL.createObjectURL(new Blob([bytes || content], { type: mimeTypeForPath(path) }));
        img.src = objectUrl;
    }
    img.alt = trimMaybeString(path).split('/').pop() || 'Image preview';
    body.appendChild(img);
    return {
        element: root,
        dispose() {
            if (objectUrl) {
                URL.revokeObjectURL(objectUrl);
            }
        },
    };
}

function createGlbScene(THREE, viewport) {
    const scene = new THREE.Scene();
    scene.add(new THREE.HemisphereLight(0xffffff, 0x444444, 1.4));
    const sun = new THREE.DirectionalLight(0xffffff, 1.6);
    sun.position.set(4, 8, 5);
    scene.add(sun);

    const camera = new THREE.PerspectiveCamera(45, 1, 0.01, 1000);
    const renderer = new THREE.WebGLRenderer({ antialias: true, alpha: true });
    renderer.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));
    renderer.setClearColor(0x000000, 0);
    viewport.appendChild(renderer.domElement);

    const modelRoot = new THREE.Group();
    scene.add(modelRoot);
    return { scene, camera, renderer, modelRoot };
}

function resizeGlbViewport(viewport, camera, renderer) {
    const rect = viewport.getBoundingClientRect();
    const width = Math.max(1, Math.floor(rect.width));
    const height = Math.max(1, Math.floor(rect.height));
    renderer.setSize(width, height, false);
    camera.aspect = width / height;
    camera.updateProjectionMatrix();
}

function fitGlbModel(THREE, camera, loadedScene, fallbackDistance = 4) {
    if (!loadedScene) {
        return fallbackDistance;
    }
    const box = new THREE.Box3().setFromObject(loadedScene);
    if (box.isEmpty()) {
        camera.position.set(0, 0, fallbackDistance);
        camera.lookAt(0, 0, 0);
        return fallbackDistance;
    }
    const center = box.getCenter(new THREE.Vector3());
    const size = box.getSize(new THREE.Vector3());
    loadedScene.position.sub(center);
    const distance = Math.max(size.x, size.y, size.z, 1) * 2.2;
    camera.position.set(0, distance * 0.35, distance);
    camera.lookAt(0, 0, 0);
    camera.near = Math.max(distance / 1000, 0.001);
    camera.far = distance * 20;
    camera.updateProjectionMatrix();
    return distance;
}

function glbNodeLabel(node, fallbackIndex = 0) {
    return trimMaybeString(node?.name) || trimMaybeString(node?.type) || `Part ${fallbackIndex + 1}`;
}

function formatCoordinate(value) {
    return Number.isFinite(value) ? value.toFixed(4).replace(/\.?0+$/, '') : '0';
}

function formatVector3(vector) {
    return [
        `x ${formatCoordinate(vector.x)}`,
        `y ${formatCoordinate(vector.y)}`,
        `z ${formatCoordinate(vector.z)}`,
    ].join(' · ');
}

function clampNumber(value, min, max) {
    const numeric = Number(value);
    if (!Number.isFinite(numeric)) {
        return min;
    }
    return Math.min(max, Math.max(min, numeric));
}

function formatPercent(value) {
    return `${Math.round(clampNumber(value, 0, 1) * 100)}%`;
}

function collectGlbNodes(root) {
    const nodes = [];
    root.traverse?.((node) => {
        nodes.push({
            node,
            depth: Math.max(0, nodes.length === 0 ? 0 : node.parent ? 1 : 0),
        });
    });
    const depths = new Map([[root, -1]]);
    for (const item of nodes) {
        const parentDepth = depths.get(item.node.parent) ?? -1;
        item.depth = Math.max(0, parentDepth + 1);
        depths.set(item.node, item.depth);
    }
    return nodes;
}

function glbMeshMaterials(node) {
    if (!node?.isMesh || !node.material) {
        return [];
    }
    return Array.isArray(node.material) ? node.material.filter(Boolean) : [node.material];
}

function createGlbOpacityController(modelRoot) {
    const originalState = new WeakMap();
    let opacity = 1;

    function remember(material) {
        if (!originalState.has(material)) {
            originalState.set(material, {
                opacity: Number.isFinite(material.opacity) ? material.opacity : 1,
                transparent: Boolean(material.transparent),
                depthWrite: material.depthWrite,
            });
        }
        return originalState.get(material);
    }

    function applyMaterial(material) {
        const original = remember(material);
        material.opacity = original.opacity * opacity;
        material.transparent = original.transparent || opacity < 0.999;
        material.depthWrite = opacity < 0.999 ? false : original.depthWrite;
        material.needsUpdate = true;
    }

    function setOpacity(nextOpacity) {
        opacity = clampNumber(nextOpacity, 0.08, 1);
        modelRoot.traverse?.((node) => {
            for (const material of glbMeshMaterials(node)) {
                applyMaterial(material);
            }
        });
    }

    return {
        get opacity() {
            return opacity;
        },
        refresh() {
            setOpacity(opacity);
        },
        restore() {
            modelRoot.traverse?.((node) => {
                for (const material of glbMeshMaterials(node)) {
                    const original = originalState.get(material);
                    if (!original) continue;
                    material.opacity = original.opacity;
                    material.transparent = original.transparent;
                    material.depthWrite = original.depthWrite;
                    material.needsUpdate = true;
                }
            });
        },
        setOpacity,
    };
}

function selectedNodeAxisSize(THREE, node) {
    if (!node) {
        return 0.35;
    }
    const box = new THREE.Box3().setFromObject(node);
    if (box.isEmpty()) {
        return 0.35;
    }
    const size = box.getSize(new THREE.Vector3());
    return clampNumber(Math.max(size.x, size.y, size.z) * 0.45, 0.18, 3);
}

function createSelectionHighlight(THREE, selectedNode) {
    const highlights = [];
    selectedNode?.traverse?.((node) => {
        if (!node.isMesh || !node.geometry) {
            return;
        }
        const edges = new THREE.LineSegments(
            new THREE.EdgesGeometry(node.geometry, 18),
            new THREE.LineBasicMaterial({
                color: 0x00a2ff,
                transparent: true,
                opacity: 0.95,
                depthTest: false,
                depthWrite: false,
            })
        );
        edges.name = '__rumoca_glb_selection_edges';
        edges.renderOrder = 1000;
        node.add(edges);
        highlights.push(edges);
    });
    return highlights;
}

function disposeSelectionHighlights(highlights) {
    for (const highlight of highlights) {
        highlight.parent?.remove(highlight);
        highlight.geometry?.dispose?.();
        highlight.material?.dispose?.();
    }
    highlights.length = 0;
}

function createGlbInspector(THREE, inspector, modelRoot, opacityController) {
    const axes = new THREE.AxesHelper(0.35);
    axes.visible = true;
    let selectedNode = null;
    let selectedButton = null;
    let selectionHighlights = [];
    const buttonByNode = new Map();

    const title = document.createElement('div');
    title.className = 'asset-glb-inspector-title';
    title.textContent = 'Model';

    const axesLabel = document.createElement('label');
    axesLabel.className = 'asset-glb-toggle';
    const axesCheckbox = document.createElement('input');
    axesCheckbox.type = 'checkbox';
    axesCheckbox.checked = true;
    axesLabel.append(axesCheckbox, document.createTextNode('Axes'));

    const opacityLabel = document.createElement('label');
    opacityLabel.className = 'asset-glb-opacity';
    const opacityText = document.createElement('span');
    opacityText.textContent = 'Opacity';
    const opacitySlider = document.createElement('input');
    opacitySlider.type = 'range';
    opacitySlider.min = '0.08';
    opacitySlider.max = '1';
    opacitySlider.step = '0.01';
    opacitySlider.value = String(opacityController.opacity);
    const opacityValue = document.createElement('output');
    opacityValue.value = formatPercent(opacityController.opacity);
    opacityValue.textContent = opacityValue.value;
    opacityLabel.append(opacityText, opacitySlider, opacityValue);

    const controls = document.createElement('div');
    controls.className = 'asset-glb-controls';
    controls.append(axesLabel, opacityLabel);

    const header = document.createElement('div');
    header.className = 'asset-glb-inspector-header';
    header.append(title, controls);

    const tree = document.createElement('div');
    tree.className = 'asset-glb-tree';
    const details = document.createElement('div');
    details.className = 'asset-glb-details';
    inspector.replaceChildren(header, tree, details);

    function removeAxes() {
        if (axes.parent) {
            axes.parent.remove(axes);
        }
    }

    function setSelected(node, button = null) {
        removeAxes();
        disposeSelectionHighlights(selectionHighlights);
        selectedNode = node;
        if (selectedButton) {
            selectedButton.classList.remove('selected');
        }
        selectedButton = button || buttonByNode.get(node) || null;
        selectedButton?.classList.add('selected');
        if (selectedNode && axesCheckbox.checked) {
            axes.scale.setScalar(selectedNodeAxisSize(THREE, selectedNode));
            selectedNode.add(axes);
        }
        if (selectedNode) {
            selectionHighlights = createSelectionHighlight(THREE, selectedNode);
        }
        updateDetails();
    }

    function updateDetails() {
        if (!selectedNode) {
            details.textContent = 'Select a part to inspect coordinates.';
            return;
        }
        selectedNode.updateWorldMatrix(true, false);
        const world = selectedNode.getWorldPosition(new THREE.Vector3());
        const local = selectedNode.position;
        const rotation = selectedNode.rotation;
        const scale = selectedNode.scale;
        details.innerHTML = '';
        for (const [label, value] of [
            ['Name', glbNodeLabel(selectedNode)],
            ['Type', selectedNode.type || 'Object3D'],
            ['Local', formatVector3(local)],
            ['World', formatVector3(world)],
            ['Rotation', formatVector3(rotation)],
            ['Scale', formatVector3(scale)],
        ]) {
            const row = document.createElement('div');
            row.className = 'asset-glb-detail-row';
            const key = document.createElement('span');
            key.textContent = label;
            const val = document.createElement('code');
            val.textContent = value;
            row.append(key, val);
            details.appendChild(row);
        }
    }

    axesCheckbox.addEventListener('change', () => {
        if (axesCheckbox.checked && selectedNode) {
            axes.scale.setScalar(selectedNodeAxisSize(THREE, selectedNode));
            selectedNode.add(axes);
        } else {
            removeAxes();
        }
    });

    opacitySlider.addEventListener('input', () => {
        const opacity = clampNumber(opacitySlider.value, 0.08, 1);
        opacityController.setOpacity(opacity);
        opacityValue.value = formatPercent(opacity);
        opacityValue.textContent = opacityValue.value;
    });

    function loadTree(sceneRoot) {
        const items = collectGlbNodes(sceneRoot);
        tree.innerHTML = '';
        buttonByNode.clear();
        items.forEach((item, index) => {
            const button = document.createElement('button');
            button.type = 'button';
            button.className = 'asset-glb-tree-node';
            button.style.paddingLeft = `${8 + item.depth * 12}px`;
            button.textContent = glbNodeLabel(item.node, index);
            button.title = `${button.textContent} (${item.node.type || 'Object3D'})`;
            button.addEventListener('click', () => setSelected(item.node, button));
            buttonByNode.set(item.node, button);
            tree.appendChild(button);
            if (index === 0) {
                setSelected(item.node, button);
            }
        });
    }

    return {
        loadTree,
        selectNode: setSelected,
        updateDetails,
        dispose() {
            removeAxes();
            disposeSelectionHighlights(selectionHighlights);
            axes.geometry?.dispose?.();
            axes.material?.dispose?.();
            opacityController.restore();
        },
    };
}

function pickGlbNodeAt(THREE, viewport, camera, modelRoot, clientX, clientY, raycaster, pointer) {
    const rect = viewport.getBoundingClientRect();
    if (rect.width <= 0 || rect.height <= 0) {
        return null;
    }
    pointer.x = ((clientX - rect.left) / rect.width) * 2 - 1;
    pointer.y = -((clientY - rect.top) / rect.height) * 2 + 1;
    raycaster.setFromCamera(pointer, camera);
    const hit = raycaster.intersectObjects(modelRoot.children, true)
        .find((intersection) => intersection.object?.isMesh);
    return hit?.object || null;
}

function attachGlbViewportControls(THREE, viewport, camera, modelRoot, distanceState, { onSelect } = {}) {
    let dragging = false;
    let lastX = 0;
    let lastY = 0;
    let startX = 0;
    let startY = 0;
    let moved = false;
    const raycaster = new THREE.Raycaster();
    const pointer = new THREE.Vector2();

    viewport.addEventListener('pointerdown', (event) => {
        dragging = true;
        lastX = event.clientX;
        lastY = event.clientY;
        startX = event.clientX;
        startY = event.clientY;
        moved = false;
        viewport.setPointerCapture?.(event.pointerId);
    });
    viewport.addEventListener('pointermove', (event) => {
        if (!dragging) return;
        const dx = event.clientX - lastX;
        const dy = event.clientY - lastY;
        lastX = event.clientX;
        lastY = event.clientY;
        if (Math.hypot(event.clientX - startX, event.clientY - startY) > 4) {
            moved = true;
        }
        if (!moved) {
            return;
        }
        modelRoot.rotation.y += dx * 0.01;
        modelRoot.rotation.x += dy * 0.01;
    });
    viewport.addEventListener('pointerup', (event) => {
        if (!moved) {
            onSelect?.(pickGlbNodeAt(THREE, viewport, camera, modelRoot, event.clientX, event.clientY, raycaster, pointer));
        }
        dragging = false;
        viewport.releasePointerCapture?.(event.pointerId);
    });
    viewport.addEventListener('wheel', (event) => {
        event.preventDefault();
        distanceState.value = Math.max(0.1, distanceState.value * (event.deltaY > 0 ? 1.08 : 0.92));
        camera.position.setLength(distanceState.value);
        camera.lookAt(0, 0, 0);
    }, { passive: false });
}

function loadGlbScene(THREE, bytes, modelRoot, camera, viewport, distanceState, onLoaded) {
    const loader = new threeViewer.GLTFLoader();
    loader.parse(arrayBufferForBytes(bytes), '', (gltf) => {
        modelRoot.add(gltf.scene);
        distanceState.value = fitGlbModel(THREE, camera, gltf.scene, distanceState.value);
        onLoaded?.(gltf.scene);
    }, (error) => {
        viewport.appendChild(emptyResultDocument(`Could not parse GLB asset: ${error?.message || error}`));
    });
}

function renderGlbAsset(path, content) {
    const bytes = bytesForContent(content);
    const { root, body } = assetShell(path, content);
    body.classList.add('asset-glb-body');
    const viewport = document.createElement('div');
    viewport.className = 'asset-glb-viewport';
    const inspector = document.createElement('aside');
    inspector.className = 'asset-glb-inspector';
    inspector.textContent = 'Load a model to inspect its parts.';
    body.append(viewport, inspector);
    if (!bytes) {
        viewport.appendChild(emptyResultDocument('This GLB file could not be loaded.'));
        return { element: root };
    }
    const THREE = threeViewer.THREE || threeViewer;
    if (!THREE?.Scene || !threeViewer.GLTFLoader) {
        viewport.appendChild(emptyResultDocument('The 3D viewer is not available in this build.'));
        return { element: root };
    }

    const { scene, camera, renderer, modelRoot } = createGlbScene(THREE, viewport);
    const opacityController = createGlbOpacityController(modelRoot);
    const glbInspector = createGlbInspector(THREE, inspector, modelRoot, opacityController);
    const distanceState = { value: 4 };
    const resize = () => resizeGlbViewport(viewport, camera, renderer);
    let animationFrame = null;
    function render() {
        resize();
        glbInspector.updateDetails();
        renderer.render(scene, camera);
        animationFrame = window.requestAnimationFrame(render);
    }
    const resizeObserver = new ResizeObserver(resize);
    resizeObserver.observe(viewport);
    attachGlbViewportControls(THREE, viewport, camera, modelRoot, distanceState, {
        onSelect(node) {
            glbInspector.selectNode(node);
        },
    });
    try {
        loadGlbScene(THREE, bytes, modelRoot, camera, viewport, distanceState, (loadedScene) => {
            opacityController.refresh();
            glbInspector.loadTree(loadedScene);
        });
    } catch (error) {
        viewport.appendChild(emptyResultDocument(`Could not parse GLB asset: ${error?.message || error}`));
    }
    render();

    return {
        element: root,
        dispose() {
            if (animationFrame !== null) {
                window.cancelAnimationFrame(animationFrame);
            }
            resizeObserver.disconnect();
            glbInspector.dispose();
            disposeObject3d(modelRoot);
            renderer.dispose();
        },
    };
}

function renderBinaryAsset(path, content) {
    const { root, body } = assetShell(path, content);
    body.appendChild(emptyResultDocument('Binary file preview is not available for this file type.'));
    return { element: root };
}

function renderMarkdownDocument(path, content, { onSave } = {}) {
    const editButton = document.createElement('button');
    editButton.type = 'button';
    editButton.className = 'asset-file-action';
    editButton.textContent = 'Edit';
    const previewButton = document.createElement('button');
    previewButton.type = 'button';
    previewButton.className = 'asset-file-action';
    previewButton.textContent = 'Preview';
    previewButton.hidden = true;
    const saveButton = document.createElement('button');
    saveButton.type = 'button';
    saveButton.className = 'asset-file-action primary';
    saveButton.textContent = 'Save';
    saveButton.hidden = true;
    const { root, body } = assetShell(path, content, [editButton, previewButton, saveButton]);
    const preview = document.createElement('div');
    preview.className = 'markdown-preview';
    const editor = document.createElement('textarea');
    editor.className = 'markdown-editor';
    editor.value = String(content || '');
    editor.hidden = true;

    function showPreview(markdown = editor.value) {
        preview.innerHTML = renderMarkdownHtml(markdown);
        preview.hidden = false;
        editor.hidden = true;
        editButton.hidden = false;
        previewButton.hidden = true;
        saveButton.hidden = true;
    }

    function showEditor() {
        preview.hidden = true;
        editor.hidden = false;
        editButton.hidden = true;
        previewButton.hidden = false;
        saveButton.hidden = false;
        editor.focus();
    }

    editButton.addEventListener('click', showEditor);
    previewButton.addEventListener('click', () => showPreview());
    saveButton.addEventListener('click', () => {
        if (typeof onSave === 'function') {
            onSave(editor.value);
        }
        showPreview(editor.value);
    });
    showPreview(String(content || ''));
    body.classList.add('markdown-file-body');
    body.append(preview, editor);
    return { element: root };
}

function renderAssetDocument(path, content, options = {}) {
    if (isMarkdownPath(path)) {
        return renderMarkdownDocument(path, content, options);
    }
    if (isImageAssetPath(path)) {
        return renderImageAsset(path, content);
    }
    if (isGlbAssetPath(path)) {
        return renderGlbAsset(path, content);
    }
    return renderBinaryAsset(path, content);
}

export function createResultFileEditorController({
    workspaceFs,
    getEditorPanes,
    renderExplorerPane,
    scheduleWorkspacePersistence,
    resultsAppFactory = appFactory,
    shouldUseOtherCustomEditor,
    resolveInteractiveRun,
    loadInteractiveRuntime,
    loadWasmModule,
    reportInteractiveRunError,
}) {
    const mountedApps = new Map();
    const renderedKeys = new Map();

    function panes() {
        return getEditorPanes?.() || {};
    }

    function viewForPane(pane) {
        return pane?.resultViewElId ? document.getElementById(pane.resultViewElId) : null;
    }

    function isCustomFilePath(path, entry = null) {
        const nextPath = trimMaybeString(path);
        if (!nextPath) {
            return false;
        }
        return isInteractiveRunPath(nextPath)
            || isRumocaResultPath(nextPath)
            || isPreviewAssetPath(nextPath)
            || entry?.isText === false;
    }

    function shouldUseCustomFileEditor(pane) {
        const path = pane?.activePath;
        return isCustomFilePath(path, workspaceFs.getFileEntry(path));
    }

    function shouldUseResultEditor(pane) {
        return isRumocaResultPath(pane?.activePath);
    }

    function shouldUseInteractiveEditor(pane) {
        return isInteractiveRunPath(pane?.activePath);
    }

    function disposePane(paneId) {
        const mounted = mountedApps.get(paneId);
        if (mounted && typeof mounted.dispose === 'function') {
            mounted.dispose();
        }
        mountedApps.delete(paneId);
        renderedKeys.delete(paneId);
    }

    function writeResultDocument(path, run) {
        workspaceFs.setFile(path, `${JSON.stringify({
            version: 1,
            runId: run.runId,
            model: run.model,
            savedAtUnixMs: run.savedAtUnixMs,
            payload: run.payload,
            metrics: run.metrics || null,
            views: run.views,
        }, null, 2)}\n`);
        renderExplorerPane?.();
        scheduleWorkspacePersistence?.();
    }

    async function resolveInteractiveRunForError(path) {
        try {
            return await resolveInteractiveRun?.(path) || null;
        } catch {
            return null;
        }
    }

    function renderInteractiveRun(path) {
        const status = document.createElement('div');
        status.className = 'asset-file-meta';
        status.textContent = 'starting';
        const { root, body } = assetShell(path, '', []);
        root.classList.add('interactive-run-viewer');

        const host = document.createElement('div');
        host.className = 'interactive-run-host';
        const help = document.createElement('div');
        help.className = 'interactive-run-help';
        help.textContent = 'Press Capture, then use the configured keyboard, mouse, or gamepad inputs. Esc releases capture. C changes camera, H toggles HUD, T toggles realtime/fast, F toggles fullscreen, R resets, Q stops.';
        body.classList.add('interactive-run-body');
        body.prepend(status);
        body.append(host, help);

        const mounted = {
            element: root,
            disposed: false,
            runner: null,
            dispose() {
                this.disposed = true;
                this.runner?.dispose?.();
                this.runner = null;
            },
        };

        (async () => {
            const run = await resolveInteractiveRun?.(path);
            if (mounted.disposed) {
                return;
            }
            if (!run) {
                throw new Error('Input simulation context is no longer available.');
            }
            const wasm = await loadWasmModule?.();
            if (mounted.disposed) {
                return;
            }
            const runtime = await loadInteractiveRuntime?.();
            const THREE = threeViewer.THREE || threeViewer;
            if (THREE && threeViewer.GLTFLoader && !THREE.GLTFLoader) {
                THREE.GLTFLoader = threeViewer.GLTFLoader;
            }
            mounted.runner = await runtime.createInteractiveSimulation({
                wasm,
                THREE,
                source: run.source,
                modelName: run.modelName,
                config: run.config,
                sourceRootCacheUrl: run.sourceRootCacheUrl || '',
                sourceRoots: run.sourceRoots || '{}',
                workspaceSources: run.workspaceSources || '{}',
                container: host,
                scriptText: run.scriptText || '',
                assetBaseUrl: run.assetBaseUrl || '',
                onStatus(text) {
                    status.textContent = text || 'running';
                },
                onError(error) {
                    reportInteractiveRunError?.(run, path, error);
                    status.textContent = 'failed';
                },
            });
            if (mounted.disposed) {
                mounted.runner?.dispose?.();
                return;
            }
            mounted.runner.start();
            status.textContent = 'running';
        })().catch(async (error) => {
            if (!mounted.disposed) {
                const run = await resolveInteractiveRunForError(path);
                reportInteractiveRunError?.(run, path, error);
                host.replaceChildren();
                status.textContent = 'failed';
            }
        });

        return mounted;
    }

    function renderPane(pane) {
        const view = viewForPane(pane);
        const path = trimMaybeString(pane?.activePath);
        if (!view || !shouldUseCustomFileEditor(pane)) {
            return;
        }
        const content = workspaceFs.getFileContent(path);
        const renderKey = isInteractiveRunPath(path)
            ? `${path}\ninteractive`
            : `${path}\n${contentSignature(content)}`;
        if (renderedKeys.get(pane.id) === renderKey) {
            return;
        }
        disposePane(pane.id);
        view.innerHTML = '';
        renderedKeys.set(pane.id, renderKey);
        if (isInteractiveRunPath(path)) {
            const mounted = renderInteractiveRun(path);
            view.appendChild(mounted.element);
            mountedApps.set(pane.id, mounted);
            return;
        }
        if (!shouldUseResultEditor(pane)) {
            const mounted = renderAssetDocument(path, content, {
                onSave(nextContent) {
                    workspaceFs.setFile(path, String(nextContent));
                    renderedKeys.delete(pane.id);
                    renderExplorerPane?.();
                    scheduleWorkspacePersistence?.();
                    renderPane(pane);
                },
            });
            view.appendChild(mounted.element);
            mountedApps.set(pane.id, mounted);
            return;
        }
        const run = parseResultDocument(content);
        if (!run) {
            view.appendChild(emptyResultDocument('This JSON file is not a Rumoca result document.'));
            return;
        }
        const views = Array.isArray(run.views) && run.views.length > 0
            ? cloneJson(run.views)
            : shared.defaultVisualizationViews();
        const mounted = resultsAppFactory.createResultsApp({
            root: view,
            model: run.model,
            modelRef: { model: run.model, path },
            payload: run.payload,
            metrics: run.metrics || null,
            views,
            showHeader: false,
            bridge: {
                persistState() {},
                notify(message) {
                    const status = document.getElementById('simStatus');
                    if (status) {
                        status.textContent = String(message || '');
                        status.style.color = '#888';
                    }
                },
                async loadViews() {
                    return { views: cloneJson(run.views || views) };
                },
                async saveViews(_modelRef, nextViews) {
                    const normalized = shared.normalizeVisualizationViews(nextViews);
                    run.views = normalized;
                    writeResultDocument(path, run);
                    return { views: cloneJson(normalized) };
                },
                async resetViews() {
                    const defaults = shared.defaultVisualizationViews();
                    run.views = defaults;
                    writeResultDocument(path, run);
                    return { views: cloneJson(defaults) };
                },
            },
        });
        mountedApps.set(pane.id, mounted);
    }

    function syncPane(paneId) {
        const pane = panes()[paneId];
        if (!pane) {
            return;
        }
        const editorEl = document.getElementById(pane.editorElId);
        const view = viewForPane(pane);
        const isEmpty = pane.paths.length === 0 || !trimMaybeString(pane.activePath);
        const useCustomFileEditor = !isEmpty && shouldUseCustomFileEditor(pane);
        const useOtherCustomEditor = !isEmpty
            && !useCustomFileEditor
            && typeof shouldUseOtherCustomEditor === 'function'
            && shouldUseOtherCustomEditor(pane);
        if (editorEl) {
            editorEl.hidden = isEmpty || useCustomFileEditor || useOtherCustomEditor;
        }
        if (view) {
            view.hidden = isEmpty || !useCustomFileEditor;
        }
        if (useCustomFileEditor) {
            renderPane(pane);
        } else {
            disposePane(pane.id);
            if (view) {
                view.innerHTML = '';
            }
        }
    }

    function syncAll() {
        for (const paneId of Object.keys(panes())) {
            syncPane(paneId);
        }
    }

    function invalidatePath(path) {
        const nextPath = trimMaybeString(path);
        for (const [paneId, pane] of Object.entries(panes())) {
            if (pane?.activePath === nextPath) {
                renderedKeys.delete(paneId);
            }
        }
    }

    return {
        isResultPath: isRumocaResultPath,
        isInteractiveRunPath,
        isCustomFilePath,
        shouldUseCustomFileEditor,
        shouldUseResultEditor,
        shouldUseInteractiveEditor,
        invalidatePath,
        syncAll,
        syncPane,
    };
}
