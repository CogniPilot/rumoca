use std::io;
use std::path::Path;

use rumoca_sim_diffsol::SimResult;
use serde_json::json;

const UPLOT_JS: &str = include_str!("../../rumoca-sim-diffsol/src/vendor/uplot.min.js");
const UPLOT_CSS: &str = include_str!("../../rumoca-sim-diffsol/src/vendor/uplot.min.css");

#[derive(Debug, Clone)]
pub struct SimulationHeaderSettings {
    pub solver: String,
    pub t_start: f64,
    pub t_end_requested: f64,
    pub dt: Option<f64>,
    pub rtol: f64,
    pub atol: f64,
    pub compile_seconds: Option<f64>,
    pub simulate_seconds: Option<f64>,
    pub compile_phase_instantiate_seconds: Option<f64>,
    pub compile_phase_typecheck_seconds: Option<f64>,
    pub compile_phase_flatten_seconds: Option<f64>,
    pub compile_phase_todae_seconds: Option<f64>,
}

fn details_json(
    t_start_actual: f64,
    t_end_actual: f64,
    n_points: usize,
    n_vars: usize,
    settings: Option<&SimulationHeaderSettings>,
) -> String {
    let requested = settings.map(|s| {
        json!({
            "solver": s.solver,
            "t_start": s.t_start,
            "t_end": s.t_end_requested,
            "dt": s.dt,
            "rtol": s.rtol,
            "atol": s.atol,
        })
    });
    let timing = settings.and_then(|s| {
        let has_any = s.compile_seconds.is_some()
            || s.simulate_seconds.is_some()
            || s.compile_phase_instantiate_seconds.is_some()
            || s.compile_phase_typecheck_seconds.is_some()
            || s.compile_phase_flatten_seconds.is_some()
            || s.compile_phase_todae_seconds.is_some();
        if !has_any {
            return None;
        }
        Some(json!({
            "compile_seconds": s.compile_seconds,
            "simulate_seconds": s.simulate_seconds,
            "compile_phase_seconds": {
                "instantiate": s.compile_phase_instantiate_seconds,
                "typecheck": s.compile_phase_typecheck_seconds,
                "flatten": s.compile_phase_flatten_seconds,
                "todae": s.compile_phase_todae_seconds,
            }
        }))
    });
    json!({
        "actual": {
            "t_start": t_start_actual,
            "t_end": t_end_actual,
            "points": n_points,
            "variables": n_vars,
        },
        "requested": requested,
        "timing": timing,
    })
    .to_string()
}

pub fn generate_html_report(
    result: &SimResult,
    model_name: &str,
    settings: Option<&SimulationHeaderSettings>,
) -> String {
    let data_json = build_data_json(result);
    let names_json = build_names_json(result);
    let variable_meta_json = build_variable_meta_json(result);
    let t_start = result.times.first().copied().unwrap_or(0.0);
    let t_end = result.times.last().copied().unwrap_or(0.0);
    let n_points = result.times.len();
    let n_vars = result.names.len();
    let n_states = result.n_states;
    let details_json = details_json(t_start, t_end, n_points, n_vars, settings);

    format!(
        "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n\
         <meta charset=\"utf-8\">\n\
         <title>{model_name} — Simulation Results</title>\n\
         <style>{UPLOT_CSS}</style>\n\
         <style>{app_css}</style>\n\
         </head>\n<body>\n\
         <div id=\"sidebar\">\n  <h2>Variables</h2>\n  <div id=\"sidebarControls\"><button id=\"selectAllBtn\" type=\"button\">Select All</button><button id=\"deselectAllBtn\" type=\"button\">Deselect All</button></div>\n  <div id=\"checks\"></div>\n</div>\n\
         <div id=\"main\">\n\
         <div id=\"header\"><span id=\"header-model\">{model_name}</span><span id=\"header-right\"><select id=\"viewSelect\" style=\"display:none\"></select><button id=\"savePngBtn\" title=\"Save plot as PNG\">Save PNG</button><button id=\"simDetailsBtn\" title=\"Show run details\">Run Details</button></span></div>\n\
         <div id=\"seriesLegend\"></div>\n\
         <div id=\"plot\"></div>\n</div>\n\
         <button id=\"sidebarToggle\" title=\"Show/Hide variables\">Show Variables</button>\n\
         <div id=\"cursorInfo\">Hover plot for values</div>\n\
         <div id=\"simDetailsModal\" class=\"hidden\"><div id=\"simDetailsCard\"><div id=\"simDetailsTitle\">Simulation Details</div><pre id=\"simDetailsContent\"></pre><div id=\"simDetailsActions\"><button id=\"simDetailsClose\">Close</button></div></div></div>\n\
         <script>{UPLOT_JS}</script>\n\
         <script>\n{app_js}\n</script>\n</body>\n</html>",
        model_name = model_name,
        UPLOT_CSS = UPLOT_CSS,
        UPLOT_JS = UPLOT_JS,
        app_css = app_css(),
        app_js = app_js(
            &names_json,
            &data_json,
            n_states,
            &details_json,
            &variable_meta_json
        ),
    )
}

pub fn write_html_report(
    result: &SimResult,
    model_name: &str,
    path: &Path,
    settings: Option<&SimulationHeaderSettings>,
) -> io::Result<()> {
    let html = generate_html_report(result, model_name, settings);
    std::fs::write(path, html)
}

#[allow(clippy::too_many_lines)]
fn app_css() -> &'static str {
    r#"* { margin: 0; padding: 0; box-sizing: border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, monospace;
       background: #1e1e1e; color: #d4d4d4; display: flex; height: 100vh; }
#sidebar { width: 240px; min-width: 180px; padding: 12px; overflow-y: auto;
            border-right: 1px solid #333; flex-shrink: 0; }
#sidebar h2 { font-size: 14px; margin-bottom: 8px; color: #569cd6; }
#sidebar h3 { font-size: 12px; margin: 10px 0 4px; color: #888; text-transform: uppercase; }
#sidebarControls { display: flex; gap: 6px; margin-bottom: 8px; }
#sidebarControls button {
  flex: 1;
  border: 1px solid #3a3a3a;
  background: #252526;
  color: #d4d4d4;
  border-radius: 6px;
  padding: 4px 6px;
  font-size: 12px;
  cursor: pointer;
}
#sidebar label { display: block; padding: 2px 0; font-size: 13px; cursor: pointer; }
#checks { font-size: 13px; }
.tree-group { margin: 2px 0; }
.tree-group > summary {
  cursor: pointer;
  list-style: none;
  color: #bbb;
  user-select: none;
  padding: 1px 0;
}
.tree-group > summary::-webkit-details-marker { display: none; }
.tree-group > summary::before {
  content: "▾";
  color: #888;
  display: inline-block;
  width: 12px;
}
.tree-group:not([open]) > summary::before { content: "▸"; }
.tree-children {
  margin-left: 10px;
  padding-left: 8px;
  border-left: 1px dotted #333;
}
.tree-leaf {
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 1px 0;
}
.var-select-box {
  width: 12px;
  height: 12px;
  border-radius: 4px;
  border: 1px solid #666;
  background: transparent;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  flex-shrink: 0;
  cursor: pointer;
}
.var-select-box.selected {
  border-color: var(--series-color, #888);
  background: var(--series-color, #888);
}
.tree-label.is-state {
  font-weight: 700;
}
.tree-label {
  flex: 1;
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
#main { flex: 1; display: flex; flex-direction: column; padding: 12px 12px 46px 12px; min-width: 0; position: relative; }
#header { display: flex; justify-content: space-between; align-items: baseline; gap: 12px; margin-bottom: 8px; }
#header-model { font-size: 16px; font-weight: 700; color: #569cd6; }
#header-right { display: flex; align-items: center; gap: 8px; }
#seriesLegend {
  display: flex;
  flex-wrap: wrap;
  gap: 6px 12px;
  position: absolute;
  right: 12px;
  bottom: 56px;
  z-index: 5;
  max-width: min(58vw, 540px);
  max-height: 24vh;
  overflow: auto;
  padding: 6px 8px;
  border: 1px solid #3a3a3a;
  border-radius: 6px;
  background: rgba(37, 37, 38, 0.92);
  color: #b8b8b8;
  font-size: 12px;
  pointer-events: none;
}
.legend-item {
  display: inline-flex;
  align-items: center;
  gap: 6px;
}
.legend-swatch {
  width: 10px;
  height: 10px;
  border-radius: 2px;
  display: inline-block;
}
#simDetailsBtn {
  border: 1px solid #3a3a3a; background: #252526; color: #d4d4d4;
  border-radius: 6px; padding: 4px 8px; font-size: 12px; cursor: pointer;
}
#savePngBtn {
  border: 1px solid #3a3a3a; background: #252526; color: #d4d4d4;
  border-radius: 6px; padding: 4px 8px; font-size: 12px; cursor: pointer;
}
#plot { flex: 1; min-height: 0; padding-bottom: 2px; }
#sidebarToggle {
  position: fixed; left: 12px; bottom: 12px; z-index: 100;
  border: 1px solid #3a3a3a; background: #252526; color: #d4d4d4;
  border-radius: 6px; padding: 6px 10px; font-size: 12px; cursor: pointer;
}
#cursorInfo {
  position: fixed; right: 12px; bottom: 12px; z-index: 100;
  border: 1px solid #3a3a3a; background: rgba(37,37,38,0.95); color: #d4d4d4;
  border-radius: 6px; padding: 6px 10px; font-size: 12px; max-width: 72vw;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
}
#simDetailsModal {
  position: fixed; inset: 0; z-index: 200;
  background: rgba(0,0,0,0.45);
  display: flex; align-items: center; justify-content: center;
}
#simDetailsCard {
  width: min(560px, 92vw);
  max-height: 82vh;
  border: 1px solid #3a3a3a;
  border-radius: 8px;
  background: #1f1f1f;
  padding: 12px;
  display: flex;
  flex-direction: column;
  gap: 8px;
}
#simDetailsTitle { font-size: 14px; font-weight: 700; color: #569cd6; }
#simDetailsContent {
  margin: 0;
  overflow: auto;
  border: 1px solid #333;
  background: #161616;
  border-radius: 6px;
  padding: 10px;
  font-size: 12px;
  line-height: 1.4;
}
#simDetailsActions { display: flex; justify-content: flex-end; }
#simDetailsClose {
  border: 1px solid #3a3a3a; background: #252526; color: #d4d4d4;
  border-radius: 6px; padding: 4px 10px; font-size: 12px; cursor: pointer;
}
.hidden { display: none !important; }
.sidebar-hidden #sidebar { width: 0; min-width: 0; padding: 0; border-right: 0; overflow: hidden; }
.u-wrap { background: #1e1e1e !important; }"#
}

#[allow(clippy::too_many_lines)]
fn app_js(
    names_json: &str,
    data_json: &str,
    n_states: usize,
    details_json: &str,
    variable_meta_json: &str,
) -> String {
    format!(
        r##"(function() {{
  var names = {names_json};
  var allData = {data_json};
  var variableMeta = {variable_meta_json};
  var nStates = {n_states};
  var simDetails = {details_json};
  var nVars = names.length;
  var palette = [
    "#4ec9b0","#569cd6","#ce9178","#dcdcaa","#c586c0",
    "#9cdcfe","#d7ba7d","#608b4e","#d16969","#b5cea8",
    "#6a9955","#c8c8c8","#e8c87a","#7fdbca","#f07178"
  ];
  var checksEl = document.getElementById("checks");
  var seriesLegendEl = document.getElementById("seriesLegend");
  var cursorInfoEl = document.getElementById("cursorInfo");
  var sidebarToggle = document.getElementById("sidebarToggle");
  var selectAllBtn = document.getElementById("selectAllBtn");
  var deselectAllBtn = document.getElementById("deselectAllBtn");
  var savePngBtn = document.getElementById("savePngBtn");
  var simDetailsBtn = document.getElementById("simDetailsBtn");
  var simDetailsModal = document.getElementById("simDetailsModal");
  var simDetailsContent = document.getElementById("simDetailsContent");
  var simDetailsClose = document.getElementById("simDetailsClose");
  var exportLegendItems = [];
  function computePlotSize() {{
    var plotEl = document.getElementById("plot");
    var mainEl = document.getElementById("main");
    var width = plotEl ? plotEl.clientWidth : 0;
    var height = plotEl ? plotEl.clientHeight : 0;
    if (!Number.isFinite(width) || width < 120) {{
      width = mainEl ? mainEl.clientWidth : window.innerWidth;
    }}
    if (!Number.isFinite(height) || height < 220) {{
      var viewportHeight = window.innerHeight - 120;
      var mainHeight = mainEl ? mainEl.clientHeight - 70 : 0;
      height = Math.max(height || 0, viewportHeight, mainHeight);
    }}
    if (!Number.isFinite(width) || width < 320) width = 320;
    if (!Number.isFinite(height) || height < 220) height = 220;
    return {{ width: Math.floor(width), height: Math.floor(height) }};
  }}
  function escapeHtml(v) {{
    return String(v)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }}
  function escapeAttr(v) {{
    return escapeHtml(v).replace(/\n/g, "&#10;");
  }}
  function getVariableMeta(idx, fallbackName, fallbackIsState) {{
    var meta = variableMeta[idx] || {{}};
    var isState = meta.is_state === true || fallbackIsState;
    return {{
      name: String(meta.name || fallbackName),
      role: String(meta.role || (isState ? "state" : "algebraic")),
      isState: isState,
      valueType: meta.value_type ?? null,
      variability: meta.variability ?? null,
      timeDomain: meta.time_domain ?? null,
      unit: meta.unit ?? null,
      start: meta.start ?? null,
      min: meta.min ?? null,
      max: meta.max ?? null,
      nominal: meta.nominal ?? null,
      fixed: meta.fixed ?? null,
      description: meta.description ?? null
    }};
  }}
  function variableDetailsLines(meta) {{
    var lines = [];
    lines.push(meta.name + " [" + meta.role + "]");
    if (meta.valueType) lines.push("type: " + meta.valueType);
    if (meta.variability) lines.push("variability: " + meta.variability);
    if (meta.timeDomain) lines.push("time-domain: " + meta.timeDomain);
    if (meta.unit) lines.push("unit: " + meta.unit);
    if (meta.start) lines.push("start: " + meta.start);
    if (meta.min) lines.push("min: " + meta.min);
    if (meta.max) lines.push("max: " + meta.max);
    if (meta.nominal) lines.push("nominal: " + meta.nominal);
    if (meta.fixed !== null && meta.fixed !== undefined) lines.push("fixed: " + String(meta.fixed));
    if (meta.description) lines.push("description: " + meta.description);
    return lines;
  }}
  function variableDetailsTooltip(meta) {{
    return variableDetailsLines(meta).join("\n");
  }}
  function variableDetailsInline(meta) {{
    var parts = [meta.name + " (" + meta.role + ")"];
    if (meta.valueType) parts.push("type=" + meta.valueType);
    if (meta.variability) parts.push("variability=" + meta.variability);
    if (meta.timeDomain) parts.push("time=" + meta.timeDomain);
    if (meta.unit) parts.push("unit=" + meta.unit);
    if (meta.start) parts.push("start=" + meta.start);
    if (meta.min) parts.push("min=" + meta.min);
    if (meta.max) parts.push("max=" + meta.max);
    return parts.join(" | ");
  }}
  function makeTreeNode() {{
    return {{ children: {{}}, leaves: [] }};
  }}
  function buildVariableTree() {{
    var root = makeTreeNode();
    for (var i = 0; i < nVars; i++) {{
      var fullName = names[i];
      var parts = String(fullName).split(".");
      var node = root;
      for (var j = 0; j < parts.length - 1; j++) {{
        var seg = parts[j];
        if (!node.children[seg]) node.children[seg] = makeTreeNode();
        node = node.children[seg];
      }}
      var leafLabel = parts.length > 0 ? parts[parts.length - 1] : fullName;
      node.leaves.push({{
        idx: i,
        label: leafLabel,
        fullName: fullName,
        isState: i < nStates
      }});
    }}
    return root;
  }}
  function renderTree(node) {{
    var out = "";
    var groups = Object.keys(node.children).sort(function(a, b) {{ return a.localeCompare(b); }});
    for (var i = 0; i < groups.length; i++) {{
      var groupName = groups[i];
      out += '<details class="tree-group">' +
             '<summary>' + escapeHtml(groupName) + '</summary>' +
             '<div class="tree-children">' + renderTree(node.children[groupName]) + '</div>' +
             '</details>';
    }}
    var leaves = node.leaves.slice().sort(function(a, b) {{ return a.label.localeCompare(b.label); }});
    for (var k = 0; k < leaves.length; k++) {{
      var leaf = leaves[k];
      var meta = getVariableMeta(leaf.idx, leaf.fullName, leaf.isState);
      var selectedClass = meta.isState ? " selected" : "";
      var labelClass = meta.isState ? "tree-label is-state" : "tree-label";
      var tooltip = variableDetailsTooltip(meta);
      var color = palette[leaf.idx % palette.length];
      out += '<label class="tree-leaf" data-idx="' + leaf.idx + '" title="' + escapeAttr(tooltip) + '">' +
             '<span class="var-select-box' + selectedClass + '" data-idx="' + leaf.idx + '" style="--series-color:' + escapeAttr(String(color)) + ';"></span>' +
             '<span class="' + labelClass + '">' + escapeHtml(leaf.label) + '</span>' +
             '</label>';
    }}
    return out;
  }}
  checksEl.innerHTML = renderTree(buildVariableTree());
  var plot = null;
  var sidebarVisible = false;
  var defaultCursorInfo = "Hover plot for values";
  var lastPlotCursorInfo = defaultCursorInfo;
  function formatNum(v) {{
    if (!Number.isFinite(v)) return String(v);
    if (Math.abs(v) >= 1000 || (Math.abs(v) > 0 && Math.abs(v) < 0.001)) return v.toExponential(3);
    return v.toFixed(4).replace(/\.?0+$/, "");
  }}
  function updateCursorInfo(idx, active, data, series) {{
    if (idx == null || idx < 0 || idx >= data[0].length || active.length === 0) {{
      lastPlotCursorInfo = defaultCursorInfo;
      cursorInfoEl.textContent = defaultCursorInfo;
      return;
    }}
    var parts = ["t=" + formatNum(data[0][idx])];
    for (var i = 0; i < active.length; i++) {{
      var label = series[i + 1].label;
      parts.push(label + "=" + formatNum(data[i + 1][idx]));
    }}
    lastPlotCursorInfo = parts.join(" | ");
    cursorInfoEl.textContent = lastPlotCursorInfo;
  }}
  function detailsText(details) {{
    var actual = details.actual || {{}};
    var requested = details.requested || null;
    var timing = details.timing || null;
    var lines = [];
    lines.push("Actual");
    lines.push("  t_start: " + formatNum(actual.t_start));
    lines.push("  t_end:   " + formatNum(actual.t_end));
    lines.push("  points:  " + String(actual.points));
    lines.push("  vars:    " + String(actual.variables));
    if (requested) {{
      lines.push("");
      lines.push("Requested");
      lines.push("  solver:  " + String(requested.solver ?? "unknown"));
      lines.push("  t_start: " + formatNum(requested.t_start));
      lines.push("  t_end:   " + formatNum(requested.t_end));
      lines.push("  dt:      " + (requested.dt == null ? "auto" : formatNum(requested.dt)));
      lines.push("  rtol:    " + formatNum(requested.rtol));
      lines.push("  atol:    " + formatNum(requested.atol));
    }}
    if (timing) {{
      var phases = timing.compile_phase_seconds || {{}};
      lines.push("");
      lines.push("Run Timing");
      if (timing.compile_seconds != null) {{
        lines.push("  compile: " + formatNum(timing.compile_seconds) + "s");
      }}
      if (timing.simulate_seconds != null) {{
        lines.push("  simulate:" + " " + formatNum(timing.simulate_seconds) + "s");
      }}
      var phaseLines = [];
      if (phases.instantiate != null) phaseLines.push("instantiate " + formatNum(phases.instantiate) + "s");
      if (phases.typecheck != null) phaseLines.push("typecheck " + formatNum(phases.typecheck) + "s");
      if (phases.flatten != null) phaseLines.push("flatten " + formatNum(phases.flatten) + "s");
      if (phases.todae != null) phaseLines.push("toDAE " + formatNum(phases.todae) + "s");
      if (phaseLines.length > 0) {{
        lines.push("  phases:  " + phaseLines.join(" | "));
      }}
    }}
    return lines.join("\n");
  }}
  function openDetails() {{
    simDetailsContent.textContent = detailsText(simDetails);
    simDetailsModal.classList.remove("hidden");
  }}
  function closeDetails() {{
    simDetailsModal.classList.add("hidden");
  }}
  function buildExportPngDataUrl() {{
    if (!plot || !plot.root) {{
      cursorInfoEl.textContent = "No plot available to export";
      return null;
    }}
    var canvas = plot.root.querySelector("canvas");
    if (!canvas || typeof canvas.toDataURL !== "function") {{
      cursorInfoEl.textContent = "Plot canvas unavailable for export";
      return null;
    }}
    try {{
      var exportCanvas = document.createElement("canvas");
      exportCanvas.width = canvas.width;
      exportCanvas.height = canvas.height;
      var ctx = exportCanvas.getContext("2d");
      if (!ctx) {{
        cursorInfoEl.textContent = "PNG export failed: missing 2D context";
        return null;
      }}

      // Ensure deterministic dark background even when source canvas has transparency.
      ctx.fillStyle = "#1e1e1e";
      ctx.fillRect(0, 0, exportCanvas.width, exportCanvas.height);
      ctx.drawImage(canvas, 0, 0);

      if (Array.isArray(exportLegendItems) && exportLegendItems.length > 0) {{
        ctx.save();
        ctx.font = "12px monospace";
        ctx.textBaseline = "middle";
        var swatch = 10;
        var gap = 6;
        var rowH = 16;
        var pad = 8;
        var maxLabel = 0;
        for (var i = 0; i < exportLegendItems.length; i++) {{
          var labelText = String(exportLegendItems[i].label || "");
          maxLabel = Math.max(maxLabel, ctx.measureText(labelText).width);
        }}
        var boxW = Math.ceil(pad * 2 + swatch + gap + maxLabel);
        var boxH = Math.ceil(pad * 2 + rowH * exportLegendItems.length);
        var boxX = Math.max(6, exportCanvas.width - boxW - 10);
        var boxY = Math.max(6, exportCanvas.height - boxH - 10);

        ctx.fillStyle = "rgba(37,37,38,0.92)";
        ctx.strokeStyle = "rgba(90,90,90,0.9)";
        ctx.lineWidth = 1;
        ctx.fillRect(boxX, boxY, boxW, boxH);
        ctx.strokeRect(boxX + 0.5, boxY + 0.5, boxW - 1, boxH - 1);

        for (var j = 0; j < exportLegendItems.length; j++) {{
          var item = exportLegendItems[j];
          var y = boxY + pad + j * rowH + rowH / 2;
          ctx.fillStyle = String(item.color || "#cccccc");
          ctx.fillRect(boxX + pad, y - swatch / 2, swatch, swatch);
          ctx.fillStyle = "#d4d4d4";
          ctx.fillText(String(item.label || ""), boxX + pad + swatch + gap, y);
        }}
        ctx.restore();
      }}

      return exportCanvas.toDataURL("image/png");
    }} catch (err) {{
      cursorInfoEl.textContent = "Failed to save PNG: " + String(err);
      return null;
    }}
  }}
  window.__rumocaExportPngDataUrl = buildExportPngDataUrl;
  function savePlotPng() {{
    try {{
      var dataUrl = buildExportPngDataUrl();
      if (!dataUrl) return;
      var link = document.createElement("a");
      link.href = dataUrl;
      var modelSafe = String(document.getElementById("header-model")?.textContent || "rumoca_plot")
        .trim()
        .replace(/[^a-zA-Z0-9._-]+/g, "_")
        .replace(/^_+|_+$/g, "") || "rumoca_plot";
      link.download = modelSafe + "_plot.png";
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
    }} catch (err) {{
      cursorInfoEl.textContent = "Failed to save PNG: " + String(err);
    }}
  }}
  function setSidebarVisible(visible) {{
    sidebarVisible = visible;
    if (sidebarVisible) {{
      document.body.classList.remove("sidebar-hidden");
      sidebarToggle.textContent = "Hide Variables";
    }} else {{
      document.body.classList.add("sidebar-hidden");
      sidebarToggle.textContent = "Show Variables";
    }}
    setTimeout(function() {{
      if (plot) {{
        plot.setSize(computePlotSize());
      }}
    }}, 180);
  }}
  function renderLegend(active, series) {{
    if (!seriesLegendEl) return;
    if (!active || active.length === 0) {{
      seriesLegendEl.innerHTML = "";
      seriesLegendEl.style.display = "none";
      exportLegendItems = [];
      return;
    }}
    var items = [];
    var exportItems = [];
    for (var i = 0; i < active.length; i++) {{
      var s = series[i + 1] || {{}};
      var color = s.stroke || "#cccccc";
      var label = s.label || names[active[i]] || "";
      exportItems.push({{ color: String(color), label: String(label) }});
      items.push(
        '<span class="legend-item">' +
        '<span class="legend-swatch" style="background:' + escapeAttr(String(color)) + ';"></span>' +
        '<span>' + escapeHtml(String(label)) + '</span>' +
        '</span>'
      );
    }}
    exportLegendItems = exportItems;
    seriesLegendEl.innerHTML = items.join("");
    seriesLegendEl.style.display = "flex";
  }}
  function rebuild() {{
    var active = [];
    var selected = checksEl.querySelectorAll(".var-select-box.selected");
    for (var j = 0; j < selected.length; j++) active.push(parseInt(selected[j].dataset.idx, 10));
    var data = [allData[0]];
    var series = [{{}}];
    var isEmpty = active.length === 0;
    if (isEmpty) {{
      // Keep axes visible even when no variables are selected.
      data.push(new Array(allData[0].length).fill(0));
      series.push({{
        label: "",
        stroke: "rgba(0,0,0,0)",
        width: 0,
        points: {{ show: false }}
      }});
    }}
    for (var k = 0; k < active.length; k++) {{
      data.push(allData[active[k] + 1]);
      series.push({{ label: names[active[k]], stroke: palette[active[k] % palette.length], width: 1.5 }});
    }}
    renderLegend(active, series);
    var plotEl = document.getElementById("plot");
    if (plot) plot.destroy();
    var initialSize = computePlotSize();
    plot = new uPlot({{
      width: initialSize.width, height: initialSize.height,
      padding: [8, 8, 28, 8],
      scales: {{ x: {{ time: false }} }},
      axes: [
        {{
          stroke: "#888",
          grid: {{ stroke: "#333" }},
          label: "time",
          labelGap: 2,
          size: 36,
          font: "11px monospace",
          labelFont: "12px monospace"
        }},
        {{ stroke: "#888", grid: {{ stroke: "#333" }}, font: "11px monospace" }}
      ],
      series: series, cursor: {{ drag: {{ x: true, y: true }} }},
      hooks: {{
        setCursor: [function(u) {{ updateCursorInfo(u.cursor.idx, active, data, series); }}]
      }},
      legend: {{ show: false }}
    }}, data, plotEl);
    if (isEmpty) {{
      lastPlotCursorInfo = "No variables selected";
      cursorInfoEl.textContent = lastPlotCursorInfo;
      return;
    }}
    updateCursorInfo(null, active, data, series);
  }}
  sidebarToggle.addEventListener("click", function() {{ setSidebarVisible(!sidebarVisible); }});
  selectAllBtn.addEventListener("click", function() {{
    var boxes = checksEl.querySelectorAll(".var-select-box");
    for (var i = 0; i < boxes.length; i++) boxes[i].classList.add("selected");
    rebuild();
  }});
  deselectAllBtn.addEventListener("click", function() {{
    var boxes = checksEl.querySelectorAll(".var-select-box");
    for (var i = 0; i < boxes.length; i++) boxes[i].classList.remove("selected");
    rebuild();
  }});
  savePngBtn.addEventListener("click", savePlotPng);
  simDetailsBtn.addEventListener("click", openDetails);
  simDetailsClose.addEventListener("click", closeDetails);
  simDetailsModal.addEventListener("click", function(e) {{ if (e.target === simDetailsModal) closeDetails(); }});
  checksEl.addEventListener("click", function(e) {{
    var box = e.target.closest(".var-select-box");
    if (!box || !checksEl.contains(box)) return;
    box.classList.toggle("selected");
    rebuild();
  }});
  checksEl.addEventListener("mouseover", function(e) {{
    var leaf = e.target.closest(".tree-leaf");
    if (!leaf || !checksEl.contains(leaf)) return;
    var idx = parseInt(leaf.getAttribute("data-idx"), 10);
    if (!Number.isFinite(idx) || idx < 0 || idx >= nVars) return;
    var meta = getVariableMeta(idx, names[idx], idx < nStates);
    cursorInfoEl.textContent = variableDetailsInline(meta);
  }});
  checksEl.addEventListener("mouseout", function(e) {{
    var leaf = e.target.closest(".tree-leaf");
    if (!leaf || !checksEl.contains(leaf)) return;
    var related = e.relatedTarget;
    if (related && leaf.contains(related)) return;
    cursorInfoEl.textContent = lastPlotCursorInfo;
  }});
  setSidebarVisible(false);
  rebuild();
  var plotResizeObserver = null;
  if (typeof ResizeObserver !== "undefined") {{
    var targetEl = document.getElementById("plot");
    if (targetEl) {{
      plotResizeObserver = new ResizeObserver(function() {{
        if (plot) {{
          plot.setSize(computePlotSize());
        }}
      }});
      plotResizeObserver.observe(targetEl);
    }}
  }}
  window.addEventListener("resize", function() {{
    if (plot) {{
      plot.setSize(computePlotSize());
    }}
  }});
}})();"##,
        names_json = names_json,
        data_json = data_json,
        variable_meta_json = variable_meta_json,
        n_states = n_states,
        details_json = details_json,
    )
}

fn build_names_json(result: &SimResult) -> String {
    let parts: Vec<String> = result.names.iter().map(|n| format!("\"{}\"", n)).collect();
    format!("[{}]", parts.join(","))
}

fn build_variable_meta_json(result: &SimResult) -> String {
    let payload = result
        .variable_meta
        .iter()
        .map(|meta| {
            json!({
                "name": meta.name,
                "role": meta.role,
                "is_state": meta.is_state,
                "value_type": meta.value_type,
                "variability": meta.variability,
                "time_domain": meta.time_domain,
                "unit": meta.unit,
                "start": meta.start,
                "min": meta.min,
                "max": meta.max,
                "nominal": meta.nominal,
                "fixed": meta.fixed,
                "description": meta.description,
            })
        })
        .collect::<Vec<_>>();
    serde_json::to_string(&payload).unwrap_or_else(|_| "[]".to_string())
}

fn build_data_json(result: &SimResult) -> String {
    let mut arrays: Vec<String> = Vec::with_capacity(1 + result.data.len());
    arrays.push(format_f64_array(&result.times));
    for col in &result.data {
        arrays.push(format_f64_array(col));
    }
    format!("[{}]", arrays.join(",\n"))
}

fn format_f64_array(vals: &[f64]) -> String {
    let parts: Vec<String> = vals.iter().map(|v| format!("{:.10e}", v)).collect();
    format!("[{}]", parts.join(","))
}
