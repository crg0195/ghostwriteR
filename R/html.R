
write_self_contained_html <- function(parsed, graph, out) {
  svg_text <- export_graph_svg(graph)
  html <- build_html_page(parsed, svg_text)
  writeLines(html, con = out, useBytes = TRUE)
  invisible(out)
}


write_graph_only_html <- function(parsed, graph, out) {
  svg_text <- export_graph_svg(graph)
  html <- build_html_graph_page(parsed, svg_text)
  writeLines(html, con = out, useBytes = TRUE)
  invisible(out)
}


build_html_page <- function(parsed, svg_text) {
  stats <- workflow_stats(parsed)
  steps <- parsed$steps
  script_name <- basename(parsed$script_path)
  report_date <- sub(" 0", " ", format(Sys.Date(), "%B %d, %Y"), fixed = TRUE)
  subtitle <- workflow_summary_line(stats)
  has_inventories <- workflow_has_inventories(steps)

  phase_groups <- workflow_phases(steps)
  timeline_groups <- workflow_timeline_groups(steps)
  config <- workflow_display_config(steps, phase_groups)

  timeline_markup <- if (length(timeline_groups) == 0) {
    '<p class="empty-state">No workflow steps were detected.</p>'
  } else {
    paste(vapply(seq_along(timeline_groups), function(i) {
      group_name <- names(timeline_groups)[[i]]
      build_html_timeline_section(group_name, timeline_groups[[i]], steps, config, open = i <= 2L)
    }, character(1)), collapse = "\n")
  }

  graph_markup <- paste0(
    '<div class="graph-toolbar" role="toolbar" aria-label="Graph zoom controls">',
    '<button class="graph-tool" type="button" id="graphZoomOutButton" aria-label="Zoom out">-</button>',
    '<button class="graph-tool" type="button" id="graphZoomInButton" aria-label="Zoom in">+</button>',
    '<button class="graph-tool graph-tool--wide" type="button" id="graphFitButton">Fit</button>',
    '<button class="graph-tool graph-tool--wide" type="button" id="graphActualButton">100%</button>',
    '<span class="graph-zoom-label" id="graphZoomLabel">100%</span>',
    '</div>',
    '<div class="graph-frame" id="graphFrame"><div class="graph-canvas">', svg_text, '</div></div>'
  )

  timeline_copy <- if (isTRUE(config$large)) {
    "This script is large, so each phase opens in a shortened view first. Use the phase controls to reveal additional steps only when you need them."
  } else {
    "Each phase can be collapsed. Select any step to see a cleaner explanation without squeezing every detail into the map itself."
  }

  paste0(
    '<!DOCTYPE html>\n',
    '<html lang="en">\n',
    '<head>\n',
    '  <meta charset="utf-8">\n',
    '  <meta name="viewport" content="width=device-width, initial-scale=1">\n',
    '  <title>', html_escape(paste0("GhostwriteR - ", script_name)), '</title>\n',
    '  <style>\n',
    html_page_css(),
    '  </style>\n',
    '</head>\n',
    '<body>\n',
    '  <div class="page-shell">\n',
    '    <header class="hero-card">\n',
    '      <div class="hero-copy">\n',
    '        <p class="script-kicker">The script titled:</p>\n',
    '        <h1>', html_escape(script_name), '</h1>\n',
    '        <p class="hero-date">as of ', html_escape(report_date), '</p>\n',
    '        <p class="hero-summary">', html_escape(subtitle), '</p>\n',
    '      </div>\n',
    '      <div class="stat-grid">\n',
    build_stat_card("Sources", stats$inputs, "#d0a021"),
    build_stat_card(stats$object_label, stats$datasets, "#4b84b6"),
    build_stat_card("Transformations", stats$transforms, "#6f9a43"),
    build_stat_card("Outputs", stats$outputs, "#b5663b"),
    '      </div>\n',
    '    </header>\n',
    '    <section class="mode-strip">\n',
    '      <div class="mode-toggle" role="tablist" aria-label="Workflow views">\n',
    '        <button class="mode-button is-active" type="button" data-view-target="timeline" aria-pressed="true">Workflow timeline</button>\n',
    if (isTRUE(has_inventories)) '        <button class="mode-button" type="button" data-view-target="inventories" aria-pressed="false">Column inventories</button>\n' else '',
    '        <button class="mode-button" type="button" data-view-target="graph" aria-pressed="false">Full dependency graph</button>\n',
    '      </div>\n',
    '      <p class="mode-copy">Start with the timeline for a readable process story. Open the full graph only when you need the technical dependency map.</p>\n',
    '    </section>\n',
    '    <main class="view-stack">\n',
    '      <section class="view-panel is-visible" data-view="timeline">\n',
    '        <div class="timeline-layout">\n',
    '          <section class="panel timeline-panel">\n',
    '            <div class="panel-head panel-head--stacked">\n',
    '              <div>\n',
    '                <p class="section-label">Overview</p>\n',
    '                <h2>Workflow timeline</h2>\n',
    '              </div>\n',
    '              <p class="panel-copy">', html_escape(timeline_copy), '</p>\n',
    '            </div>\n',
    '            <div class="phase-list', if (isTRUE(config$large)) ' is-large' else '', '">\n',
    timeline_markup,
    '            </div>\n',
    '          </section>\n',
    build_html_detail_panel(steps),
    '        </div>\n',
    '      </section>\n',
    if (isTRUE(has_inventories)) build_html_inventory_view(steps, parsed$nodes) else '',
    '      <section class="view-panel" data-view="graph">\n',
    '        <div class="panel graph-panel">\n',
    '          <div class="panel-head">\n',
    '            <div>\n',
    '              <p class="section-label">Technical view</p>\n',
    '              <h2>Full dependency graph</h2>\n',
    '            </div>\n',
    '            <button class="focus-button" type="button" id="graphResetButton">Reset graph view</button>\n',
    '          </div>\n',
    '          <p class="panel-copy">This is the complete technical dependency map. It stays separate from the timeline so the story view remains easier to read.</p>\n',
    graph_markup,
    '        </div>\n',
    '      </section>\n',
    '    </main>\n',
    '  </div>\n',
    '  <script>\n',
    html_page_js(),
    '  </script>\n',
    '</body>\n',
    '</html>\n'
  )
}

build_html_graph_page <- function(parsed, svg_text) {
  stats <- workflow_stats(parsed)
  steps <- parsed$steps
  script_name <- basename(parsed$script_path)
  subtitle <- workflow_summary_line(stats)
  phase_groups <- workflow_phases(steps)
  timeline_groups <- workflow_timeline_groups(steps)
  overview_groups <- workflow_overview_groups(steps, phase_groups, timeline_groups)
  config <- workflow_display_config(steps, phase_groups)

  graph_toolbar <- paste0(
    '<div class="graph-toolbar" role="toolbar" aria-label="Graph zoom controls">',
    '<button class="graph-tool" type="button" id="graphOnlyZoomOutButton" aria-label="Zoom out">-</button>',
    '<button class="graph-tool" type="button" id="graphOnlyZoomInButton" aria-label="Zoom in">+</button>',
    '<button class="graph-tool graph-tool--wide" type="button" id="graphOnlyFitButton">Fit</button>',
    '<button class="graph-tool graph-tool--wide" type="button" id="graphOnlyActualButton">100%</button>',
    '<span class="graph-zoom-label" id="graphOnlyZoomLabel">100%</span>',
    '</div>'
  )

  graph_markup <- if (isTRUE(config$graph_overview)) {
    paste0(
      '<div class="graph-subviews">',
      '<div class="graph-mode-toggle" role="tablist" aria-label="Graph views">',
      '<button class="graph-mode-button is-active" type="button" data-graph-view-target="overview" aria-pressed="true">Workflow sections</button>',
      '<button class="graph-mode-button" type="button" data-graph-view-target="full" aria-pressed="false">Full technical graph</button>',
      '</div>',
      '<section class="graph-subview is-visible" data-graph-view="overview">',
      '<p class="graph-panel__copy">This standalone page opens in overview mode first so the full graph stays readable. Switch to the technical graph when you need the exact dependency chain.</p>',
      build_phase_overview(overview_groups, steps),
      '</section>',
      '<section class="graph-subview" data-graph-view="full">',
      graph_toolbar,
      '<div class="graph-frame graph-frame--only" id="graphOnlyFrame"><div class="graph-canvas">',
      svg_text,
      '</div></div>',
      '</section>',
      '</div>'
    )
  } else {
    paste0(graph_toolbar, '<div class="graph-frame graph-frame--only" id="graphOnlyFrame"><div class="graph-canvas">', svg_text, '</div></div>')
  }

  paste0(
    '<!DOCTYPE html>\n',
    '<html lang="en">\n',
    '<head>\n',
    '  <meta charset="utf-8">\n',
    '  <meta name="viewport" content="width=device-width, initial-scale=1">\n',
    '  <title>', html_escape(paste0("GhostwriteR Graph - ", script_name)), '</title>\n',
    '  <style>\n',
    html_graph_page_css(),
    '  </style>\n',
    '</head>\n',
    '<body>\n',
    '  <div class="graph-page-shell">\n',
    '    <header class="graph-hero">\n',
    '      <div>\n',
    '        <p class="graph-eyebrow">GhostwriteR Graph Only</p>\n',
    '        <h1>', html_escape(script_name), '</h1>\n',
    '        <p class="graph-summary">', html_escape(subtitle), '</p>\n',
    '      </div>\n',
    '      <button class="graph-reset" type="button" id="graphOnlyResetButton">Reset view</button>\n',
    '    </header>\n',
    '    <section class="graph-panel graph-panel--only">\n',
    '      <div class="graph-panel__copy">This is a standalone technical graph. It is self-contained and safe to share without the original script or supporting data files.</div>\n',
    graph_markup,
    '    </section>\n',
    '  </div>\n',
    '  <script>\n',
    html_graph_page_js(),
    '  </script>\n',
    '</body>\n',
    '</html>\n'
  )
}

html_graph_page_css <- function() {
  paste(c(
    ':root {',
    '  color-scheme: light;',
    '  --ink: #18212c;',
    '  --muted: #5e6b76;',
    '  --panel: rgba(255, 255, 255, 0.9);',
    '  --border: rgba(65, 74, 83, 0.12);',
    '  --shadow: 0 24px 60px rgba(41, 52, 64, 0.12);',
    '}',
    '* { box-sizing: border-box; }',
    'body {',
    '  margin: 0;',
    '  font-family: "Avenir Next", "Segoe UI", "Trebuchet MS", sans-serif;',
    '  color: var(--ink);',
    '  background:',
    '    radial-gradient(circle at top left, rgba(244, 223, 155, 0.34), transparent 28%),',
    '    radial-gradient(circle at top right, rgba(217, 232, 245, 0.56), transparent 30%),',
    '    linear-gradient(180deg, #fffdf7 0%, #f7f1e6 100%);',
    '}',
    '.graph-page-shell {',
    '  min-height: 100vh;',
    '  padding: 26px;',
    '}',
    '.graph-hero, .graph-panel--only {',
    '  background: var(--panel);',
    '  border: 1px solid var(--border);',
    '  border-radius: 24px;',
    '  box-shadow: var(--shadow);',
    '  backdrop-filter: blur(10px);',
    '}',
    '.graph-hero {',
    '  padding: 24px 28px;',
    '  display: flex;',
    '  justify-content: space-between;',
    '  gap: 18px;',
    '  align-items: start;',
    '  margin-bottom: 18px;',
    '}',
    '.graph-eyebrow {',
    '  margin: 0 0 10px;',
    '  text-transform: uppercase;',
    '  letter-spacing: 0.14em;',
    '  font-size: 0.72rem;',
    '  color: var(--muted);',
    '}',
    '.graph-hero h1 {',
    '  margin: 0;',
    '  font-family: Georgia, "Times New Roman", serif;',
    '  font-size: clamp(2rem, 4vw, 3rem);',
    '  line-height: 1.05;',
    '}',
    '.graph-summary {',
    '  margin: 14px 0 0;',
    '  color: var(--muted);',
    '  max-width: 52rem;',
    '  line-height: 1.6;',
    '}',
    '.graph-reset {',
    '  border: none;',
    '  border-radius: 999px;',
    '  padding: 11px 16px;',
    '  background: linear-gradient(135deg, #284f6b, #3f799b);',
    '  color: white;',
    '  font-weight: 700;',
    '  cursor: pointer;',
    '}',
    '.graph-panel--only {',
    '  padding: 22px;',
    '}',
    '.graph-toolbar {',
    '  display: flex;',
    '  flex-wrap: wrap;',
    '  align-items: center;',
    '  gap: 10px;',
    '  margin-bottom: 14px;',
    '}',
    '.graph-tool {',
    '  border: 1px solid rgba(35, 75, 104, 0.14);',
    '  border-radius: 999px;',
    '  min-width: 42px;',
    '  padding: 10px 14px;',
    '  background: white;',
    '  color: #1f425c;',
    '  font-weight: 800;',
    '  cursor: pointer;',
    '}',
    '.graph-tool--wide {',
    '  min-width: 64px;',
    '}',
    '.graph-zoom-label {',
    '  margin-left: auto;',
    '  border-radius: 999px;',
    '  padding: 8px 12px;',
    '  background: rgba(35, 75, 104, 0.08);',
    '  color: #1f425c;',
    '  font-weight: 700;',
    '}',
    '.graph-panel__copy {',
    '  color: var(--muted);',
    '  line-height: 1.6;',
    '  margin-bottom: 16px;',
    '}',
    '.graph-frame--only {',
    '  height: min(84vh, 1200px);',
    '  overflow: auto;',
    '  border-radius: 20px;',
    '  background: linear-gradient(180deg, rgba(255,255,255,0.94), rgba(255,255,255,0.72));',
    '  border: 1px solid rgba(35, 75, 104, 0.10);',
    '  padding: 18px;',
    '}',
    '.graph-canvas {',
    '  width: max-content;',
    '  min-width: 100%;',
    '}',
    '.graph-frame--only svg {',
    '  width: auto;',
    '  max-width: none;',
    '  height: auto;',
    '  display: block;',
    '}',
    '.graph-subviews {',
    '  display: grid;',
    '  gap: 16px;',
    '}',
    '.graph-mode-toggle {',
    '  display: inline-flex;',
    '  gap: 10px;',
    '  padding: 6px;',
    '  background: rgba(24, 33, 44, 0.05);',
    '  border-radius: 999px;',
    '  width: fit-content;',
    '}',
    '.graph-mode-button {',
    '  border: none;',
    '  background: transparent;',
    '  color: var(--muted);',
    '  padding: 10px 16px;',
    '  border-radius: 999px;',
    '  font-weight: 700;',
    '  cursor: pointer;',
    '}',
    '.graph-mode-button.is-active {',
    '  background: #284f6b;',
    '  color: white;',
    '  box-shadow: 0 8px 18px rgba(40, 79, 107, 0.22);',
    '}',
    '.graph-subview { display: none; }',
    '.graph-subview.is-visible { display: block; }',
    '.phase-overview {',
    '  display: grid;',
    '  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));',
    '  gap: 14px;',
    '}',
    '.phase-card {',
    '  border-radius: 20px;',
    '  padding: 18px;',
    '  background: linear-gradient(180deg, rgba(255,255,255,0.96), rgba(246,241,231,0.86));',
    '  border: 1px solid rgba(35, 75, 104, 0.10);',
    '}',
    '.phase-card__eyebrow {',
    '  margin: 0 0 8px;',
    '  font-size: 0.72rem;',
    '  text-transform: uppercase;',
    '  letter-spacing: 0.14em;',
    '  color: var(--muted);',
    '}',
    '.phase-card h3 {',
    '  margin: 0;',
    '  font-size: 1.05rem;',
    '}',
    '.phase-card__meta {',
    '  margin: 8px 0 0;',
    '  color: var(--muted);',
    '  line-height: 1.55;',
    '}',
    '.phase-card__list {',
    '  margin: 14px 0 0;',
    '  padding-left: 18px;',
    '  color: var(--ink);',
    '  line-height: 1.55;',
    '}',
    '@media (max-width: 720px) {',
    '  .graph-page-shell { padding: 16px; }',
    '  .graph-hero { flex-direction: column; }',
    '}',
    ''), collapse = "\n")
}


html_graph_page_js <- function() {
  paste(c(
    '(function() {',
    '  var graphFrame = document.getElementById("graphOnlyFrame");',
    '  var resetButton = document.getElementById("graphOnlyResetButton");',
    '  var graphViewButtons = Array.prototype.slice.call(document.querySelectorAll("[data-graph-view-target]"));',
    '  var graphViewPanels = Array.prototype.slice.call(document.querySelectorAll(".graph-subview"));',
    '  function setupGraphZoom(frame, options) {',
    '    if (!frame) return null;',
    '    var svg = frame.querySelector("svg");',
    '    if (!svg) return null;',
    '    var label = options && options.label ? document.getElementById(options.label) : null;',
    '    var width = 0;',
    '    if (svg.viewBox && svg.viewBox.baseVal && svg.viewBox.baseVal.width) width = svg.viewBox.baseVal.width;',
    '    if (!width) width = parseFloat(svg.getAttribute("width")) || svg.getBoundingClientRect().width || 1200;',
    '    var baseWidth = width;',
    '    var state = { scale: 1 };',
    '    function updateLabel() { if (label) label.textContent = Math.round(state.scale * 100) + "%"; }',
    '    function applyScale(scale) {',
    '      state.scale = Math.max(0.35, Math.min(scale, 2.5));',
    '      svg.style.setProperty("width", (baseWidth * state.scale) + "px");',
    '      svg.style.setProperty("max-width", "none");',
    '      svg.style.setProperty("height", "auto");',
    '      updateLabel();',
    '    }',
    '    function fit() {',
    '      var available = Math.max(frame.clientWidth - 36, 320);',
    '      applyScale(available / baseWidth);',
    '      frame.scrollTo({ top: 0, left: 0, behavior: "smooth" });',
    '    }',
    '    applyScale(1);',
    '    if (options && options.fit !== false) fit();',
    '    return {',
    '      zoomIn: function() { applyScale(state.scale * 1.2); },',
    '      zoomOut: function() { applyScale(state.scale / 1.2); },',
    '      actual: function() { applyScale(1); frame.scrollTo({ top: 0, left: 0, behavior: "smooth" }); },',
    '      fit: fit',
    '    };',
    '  }',
    '  function bindToolbar(controller, ids) {',
    '    if (!controller) return;',
    '    var out = document.getElementById(ids.out);',
    '    var inn = document.getElementById(ids.inn);',
    '    var fit = document.getElementById(ids.fit);',
    '    var actual = document.getElementById(ids.actual);',
    '    if (out) out.addEventListener("click", controller.zoomOut);',
    '    if (inn) inn.addEventListener("click", controller.zoomIn);',
    '    if (fit) fit.addEventListener("click", controller.fit);',
    '    if (actual) actual.addEventListener("click", controller.actual);',
    '  }',
    '  function setGraphView(viewName) {',
    '    graphViewButtons.forEach(function(button) {',
    '      var active = button.getAttribute("data-graph-view-target") === viewName;',
    '      button.classList.toggle("is-active", active);',
    '      button.setAttribute("aria-pressed", active ? "true" : "false");',
    '    });',
    '    graphViewPanels.forEach(function(panel) {',
    '      panel.classList.toggle("is-visible", panel.getAttribute("data-graph-view") === viewName);',
    '    });',
    '  }',
    '  var zoomController = setupGraphZoom(graphFrame, { label: "graphOnlyZoomLabel" });',
    '  bindToolbar(zoomController, { out: "graphOnlyZoomOutButton", inn: "graphOnlyZoomInButton", fit: "graphOnlyFitButton", actual: "graphOnlyActualButton" });',
    '  if (resetButton && zoomController) {',
    '    resetButton.addEventListener("click", function() { zoomController.fit(); });',
    '  }',
    '  graphViewButtons.forEach(function(button) {',
    '    button.addEventListener("click", function() {',
    '      setGraphView(button.getAttribute("data-graph-view-target"));',
    '      if (button.getAttribute("data-graph-view-target") === "full" && zoomController) zoomController.fit();',
    '    });',
    '  });',
    '  if (graphViewButtons.length > 0) setGraphView("overview");',
    '})();'
  ), collapse = "\n")
}

workflow_stats <- function(parsed) {
  steps <- parsed$steps
  nodes <- parsed$nodes
  object_counts <- named_object_kind_counts(nodes)
  object_total <- sum(object_counts)
  input_paths <- unique(steps$target_path[steps$kind == "input" & nzchar(steps$target_path)])
  input_nodes <- unique(nodes$key[nodes$kind %in% c("input_file", "source_table")])
  source_count <- length(unique(c(input_paths, input_nodes)))

  list(
    inputs = source_count,
    datasets = object_total,
    object_counts = object_counts,
    object_label = named_object_stat_label(object_counts),
    object_breakdown = named_object_breakdown(object_counts),
    transforms = sum(steps$kind == "transform"),
    analyses = sum(steps$kind == "analysis"),
    outputs = sum(nodes$kind %in% c("output_file", "output_table"))
  )
}


workflow_summary_line <- function(stats) {
  object_phrase <- paste0(
    stats$datasets, " ",
    if (nzchar(stats$object_breakdown)) "named objects" else object_kind_label(primary_named_object_kind(stats$object_counts), stats$datasets, capitalize = FALSE)
  )

  if (nzchar(stats$object_breakdown)) {
    object_phrase <- paste0(object_phrase, " (", stats$object_breakdown, ")")
  }

  paste0(
    "This workflow uses ", stats$inputs, " external source", plural_suffix(stats$inputs),
    ", builds or reuses ", object_phrase,
    ", runs through ", stats$transforms, " transformation", plural_suffix(stats$transforms),
    ", and produces ", stats$outputs, " final deliverable", plural_suffix(stats$outputs), "."
  )
}


build_stat_card <- function(label, value, accent) {
  paste0(
    '<div class="stat-card" style="--accent:', html_escape(accent), ';">',
    '<span class="stat-label">', html_escape(label), '</span>',
    '<strong class="stat-value">', html_escape(as.character(value)), '</strong>',
    '</div>'
  )
}


workflow_display_config <- function(steps, phase_groups = NULL) {
  total_steps <- nrow(steps)
  if (is.null(phase_groups)) {
    phase_groups <- workflow_phases(steps)
  }

  phase_count <- length(phase_groups)
  large <- total_steps >= 12L || phase_count >= 5L

  list(
    total_steps = total_steps,
    phase_count = phase_count,
    large = large,
    timeline_step_limit = if (total_steps >= 18L) 3L else if (large) 4L else total_steps,
    graph_overview = total_steps >= 10L || phase_count >= 5L
  )
}


workflow_phases <- function(steps) {
  if (nrow(steps) == 0) {
    return(list())
  }

  phase_names <- vapply(seq_len(nrow(steps)), function(i) {
    step_phase_label(steps[i, , drop = FALSE])
  }, character(1))

  phase_order <- order(vapply(phase_names, phase_rank, integer(1)), steps$step)
  ordered_phase_names <- unique(phase_names[phase_order])
  stats::setNames(
    lapply(ordered_phase_names, function(name) phase_order[phase_names[phase_order] == name]),
    ordered_phase_names
  )
}


workflow_timeline_groups <- function(steps) {
  if (nrow(steps) == 0) {
    return(list())
  }

  if ("group" %in% names(steps) && any(nzchar(steps$group))) {
    groups <- steps$group
    missing <- !nzchar(groups)
    if (any(missing)) {
      groups[missing] <- vapply(which(missing), function(i) step_phase_label(steps[i, , drop = FALSE]), character(1))
    }
    ordered_names <- unique(groups)
    return(stats::setNames(lapply(ordered_names, function(name) which(groups == name)), ordered_names))
  }

  workflow_phases(steps)
}


workflow_overview_groups <- function(steps, phase_groups = NULL, timeline_groups = NULL) {
  if (is.null(timeline_groups)) {
    timeline_groups <- workflow_timeline_groups(steps)
  }

  if ("group" %in% names(steps) && any(nzchar(steps$group))) {
    return(timeline_groups)
  }

  if (is.null(phase_groups)) {
    phase_groups <- workflow_phases(steps)
  }

  phase_groups
}


timeline_group_caption <- function(group_name, indices, steps) {
  if ("group" %in% names(steps) && any(nzchar(steps$group[indices]))) {
    phase_names <- unique(vapply(indices, function(i) step_phase_label(steps[i, , drop = FALSE]), character(1)))
    return(paste0(length(indices), " step", plural_suffix(length(indices)), ". ", paste(phase_names, collapse = " + ")))
  }

  phase_caption(group_name, length(indices))
}


step_phase_label <- function(step_row) {
  step <- unclass(step_row[1, , drop = TRUE])
  text <- tolower(paste(step$title, step$detail, step$narrative, sep = " "))

  if (grepl("clear workspace|set folder path|set output path|set file path|set runtime date|set runtime timestamp|define helper function|define checked .* reader|define date cutoff|build file path|create reference vector", text, perl = TRUE)) {
    return("Setup")
  }

  if (identical(step$kind, "input")) {
    return("Inputs")
  }

  if (identical(step$kind, "output")) {
    return("Outputs")
  }

  if (identical(step$kind, "analysis")) {
    if (grepl("create chart|arrange charts|set chart defaults", text, perl = TRUE)) {
      return("Visualization")
    }
    return("Analysis")
  }

  if (grepl("discover source data files|load json files|prepare collection|combine imported files", text, perl = TRUE)) {
    return("Inputs")
  }

  if (grepl("fit linear model|fit generalized linear model|train predictive model|train classification model|train model", text, perl = TRUE)) {
    return("Modeling")
  }

  if (grepl("score records with model|generate predicted values|predicted values|forecast", text, perl = TRUE)) {
    return("Scoring")
  }

  if (grepl("create chart|add chart layer|label chart|style chart|set chart defaults|arrange charts", text, perl = TRUE)) {
    return("Visualization")
  }

  if (grepl("join tables|combine with", text, perl = TRUE)) {
    return("Joins")
  }

  if (grepl("group rows|summarize data|count records|calculate summary metrics|average of|row count|number of distinct|return a regular table without grouping", text, perl = TRUE)) {
    return("Aggregation")
  }

  if (grepl("filter grouped results", text, perl = TRUE)) {
    return("Aggregation")
  }

  if (grepl("filter rows|rename columns|sort rows|keep selected columns|remove duplicates|reshape to|trim spaces|convert .* to a date|convert .* to a number|uppercase", text, perl = TRUE)) {
    return("Cleaning")
  }

  if (grepl("add or change columns|create conditional value|bucket values|rule-based", text, perl = TRUE)) {
    return("Feature engineering")
  }

  if (grepl("^run ", tolower(step$title))) {
    return("Analysis")
  }

  if (grepl("display object", text, perl = TRUE)) {
    return("Analysis")
  }

  "Preparation"
}


phase_rank <- function(phase) {
  order_map <- c("Setup", "Inputs", "Cleaning", "Feature engineering", "Joins", "Aggregation", "Modeling", "Scoring", "Visualization", "Analysis", "Preparation", "Outputs")
  match_value <- match(phase, order_map)
  if (is.na(match_value)) 999L else match_value
}


phase_caption <- function(phase, count) {
  descriptions <- c(
    Setup = "Folder paths, runtime dates, helper functions, and other values the script defines before main processing begins.",
    Inputs = "Files or data sources loaded into the workflow.",
    Cleaning = "Filtering, reshaping, sorting, or cleanup before analysis.",
    "Feature engineering" = "New columns, flags, buckets, or business rules added to the data.",
    Joins = "Steps that combine tables or bring in reference data.",
    Aggregation = "Grouping and summary calculations for reporting or rollups.",
    Modeling = "Model fitting steps that estimate patterns from the data.",
    Scoring = "Predictions or model-based scores added back to records.",
    Visualization = "Chart-building steps that shape how results are shown.",
    Analysis = "Other analytical operations that do not fit a simpler bucket.",
    Preparation = "General transformation steps used to prepare the workflow.",
    Outputs = "Saved files, charts, or final deliverables."
  )

  paste0(count, " step", plural_suffix(count), ". ", descriptions[[phase]] %||% "Workflow steps.")
}


build_phase_overview <- function(phase_groups, steps) {
  if (length(phase_groups) == 0) {
    return('<p class="empty-state">No workflow steps were detected.</p>')
  }

  cards <- paste(vapply(names(phase_groups), function(phase) {
    build_phase_overview_card(phase, phase_groups[[phase]], steps)
  }, character(1)), collapse = "\n")

  paste0('<div class="phase-overview">', cards, '</div>')
}


build_phase_overview_card <- function(phase, indices, steps) {
  caption <- if ("group" %in% names(steps) && any(nzchar(steps$group[indices]))) {
    timeline_group_caption(phase, indices, steps)
  } else {
    phase_caption(phase, length(indices))
  }

  paste0(
    '<article class="phase-card">',
    '<p class="phase-card__eyebrow">', html_escape(paste0(length(indices), " step", plural_suffix(length(indices)))), '</p>',
    '<h3>', html_escape(phase), '</h3>',
    '<p class="phase-card__meta">', html_escape(caption), '</p>',
    build_overview_preview(indices, steps),
    '</article>'
  )
}


build_overview_preview <- function(indices, steps, limit = 3L) {
  preview_indices <- utils::head(indices, limit)
  items <- paste(vapply(preview_indices, function(i) {
    paste0('<li>', html_escape(steps$title[[i]]), '</li>')
  }, character(1)), collapse = "")

  extra_count <- max(length(indices) - length(preview_indices), 0L)
  if (extra_count > 0L) {
    items <- paste0(items, '<li>', html_escape(paste0(extra_count, " more step", plural_suffix(extra_count))), '</li>')
  }

  paste0('<ul class="phase-card__list">', items, '</ul>')
}


build_html_timeline_section <- function(group_name, indices, steps, config, open = FALSE) {
  count <- length(indices)
  visible_limit <- if (isTRUE(config$large)) min(count, config$timeline_step_limit) else count
  caption <- timeline_group_caption(group_name, indices, steps)

  buttons <- paste(vapply(seq_along(indices), function(position) {
    i <- indices[[position]]
    build_timeline_step_button(
      steps[i, , drop = FALSE],
      active = identical(as.integer(steps$step[[i]]), 1L),
      hidden = position > visible_limit
    )
  }, character(1)), collapse = "\n")

  control_markup <- if (count > visible_limit) {
    hidden_count <- count - visible_limit
    collapsed_label <- paste0("Show ", hidden_count, " more step", plural_suffix(hidden_count))
    paste0(
      '<button class="phase-expand" type="button" data-phase-expand aria-expanded="false"',
      ' data-collapsed-label="', html_escape(collapsed_label), '"',
      ' data-expanded-label="Show fewer steps">',
      html_escape(collapsed_label),
      '</button>'
    )
  } else {
    ""
  }

  paste0(
    '<details class="phase-group"', if (isTRUE(open)) ' open' else '', '>',
    '<summary><span class="phase-title">', html_escape(group_name), '</span><span class="phase-meta">', html_escape(caption), '</span></summary>',
    '<div class="phase-steps">', buttons, control_markup, '</div>',
    '</details>'
  )
}


build_timeline_step_button <- function(step_row, active = FALSE, hidden = FALSE) {
  step <- unclass(step_row[1, , drop = TRUE])
  phase <- step_phase_label(step_row)
  preview <- step$detail
  if (!nzchar(preview)) {
    preview <- step$narrative
  }
  if (nchar(preview) > 120) {
    preview <- paste0(substr(preview, 1, 117), "...")
  }

  paste0(
    '<button class="timeline-step', if (isTRUE(active)) ' is-active' else '', if (isTRUE(hidden)) ' is-collapsed' else '', '" type="button"',
    ' data-step="', step$step,
    '" data-kind="', html_escape(step$kind),
    '" data-phase="', html_escape(phase),
    '" data-narrative="', html_escape(step$narrative),
    '" data-title="', html_escape(step$title),
    '" data-summary="', html_escape(detail_summary_text(step_row)),
    '" data-detail="', html_escape(step$detail),
    '" data-source="', html_escape(step$source),
    '" data-output="', html_escape(step$output),
    '" data-target="', html_escape(step$target_path),
    '" data-input-columns="', html_escape(step$input_columns),
    '" data-output-columns="', html_escape(step$output_columns),
    '" data-carried-columns="', html_escape(step$carried_columns),
    '" data-created-columns="', html_escape(step$created_columns),
    '" data-superseded-by="', html_escape(step$superseded_by),
    '" data-code="', html_escape(step$code),
    '" data-explanation="', html_escape(step$explanation),
    '">',
    '<span class="timeline-step__number">', step$step, '</span>',
    '<span class="timeline-step__body">',
    '<strong class="timeline-step__title">', html_escape(step$title), '</strong>',
    '<span class="timeline-step__preview">', html_escape(preview), '</span>',
    build_timeline_inventory_meta(step_row),
    '</span>',
    '</button>'
  )
}

build_timeline_inventory_meta <- function(step_row) {
  step <- unclass(step_row[1, , drop = TRUE])
  if (!nzchar(step$input_columns) && !nzchar(step$output_columns)) {
    return("")
  }

  pieces <- character()
  if (nzchar(step$input_columns)) {
    count <- inventory_count(step$input_columns)
    pieces <- c(pieces, paste0(count, " in"))
  }
  if (nzchar(step$output_columns)) {
    count <- inventory_count(step$output_columns)
    pieces <- c(pieces, paste0(count, " out"))
  }

  paste0('<span class="timeline-step__inventory">', html_escape(paste(pieces, collapse = " | ")), '</span>')
}


build_html_detail_panel <- function(steps) {
  if (nrow(steps) == 0) {
    return('<aside class="panel detail-panel"><div class="detail-empty">No workflow steps were detected.</div></aside>')
  }

  step_row <- steps[1, , drop = FALSE]
  step <- unclass(step_row[1, , drop = TRUE])
  phase <- step_phase_label(step_row)
  paste0(
    '<aside class="panel detail-panel">',
    '<div class="detail-sticky">',
    '<p class="section-label">Selected step details</p>',
    '<div class="detail-header">',
    '<span class="detail-chip" id="detailPhase">', html_escape(phase), '</span>',
    '<span class="detail-chip detail-chip--muted" id="detailKind">', html_escape(step$kind), '</span>',
    '</div>',
    '<h2 id="detailTitle">', html_escape(step$title), '</h2>',
    '<p class="detail-narrative" id="detailNarrative">', html_escape(detail_summary_text(step_row)), '</p>',
    '<div class="detail-callout" id="detailCallout">', build_detail_callout_markup(step$detail), '</div>',
    '<div class="detail-explanation" id="detailExplanationBlock">', build_detail_explanation_markup(step$explanation), '</div>',
    '<details class="detail-code" id="detailCodeBlock">',
    '<summary>Show code chunk</summary>',
    '<pre><code id="detailCode">', html_escape(step$code), '</code></pre>',
    '</details>',
    '<dl class="detail-meta">',
    build_detail_meta_row('Source', step$source, 'detailSource', mode = 'comma'),
    build_detail_meta_row('Output', step$output, 'detailOutput'),
    build_detail_meta_row('Saved to', step$target_path, 'detailTarget'),
    build_detail_meta_row('Replaced later', if (nzchar(step$superseded_by)) paste0("Step ", step$superseded_by) else "", 'detailSuperseded'),
    build_detail_meta_row('Input columns used', step$input_columns, 'detailInputColumns', mode = 'comma'),
    build_detail_meta_row('Output columns produced', step$output_columns, 'detailOutputColumns', mode = 'comma'),
    build_detail_meta_row('Columns carried through', step$carried_columns, 'detailCarriedColumns', mode = 'comma'),
    build_detail_meta_row('Columns newly created or changed', step$created_columns, 'detailCreatedColumns', mode = 'comma'),
    '</dl>',
    '<p class="detail-help">Use the timeline on the left to move through the workflow. Keep the full graph for technical review only.</p>',
    '</div>',
    '</aside>'
  )
}


detail_summary_text <- function(step_row) {
  step <- unclass(step_row[1, , drop = TRUE])
  summary <- build_step_narrative(step$kind, step$title, "", step$source, step$output, step$target_path)
  summary <- compact_ws(summary)
  if (!nzchar(summary)) {
    summary <- step$title
  }
  summary
}


split_display_items <- function(text, mode = c("text", "comma", "semicolon", "newline")) {
  mode <- match.arg(mode)
  text <- trim_ws(text)
  if (!nzchar(text)) {
    return(character())
  }

  items <- switch(
    mode,
    text = text,
    comma = split_top_level(text, ","),
    semicolon = unlist(strsplit(text, ";", fixed = TRUE), use.names = FALSE),
    newline = unlist(strsplit(text, "\n", fixed = TRUE), use.names = FALSE)
  )

  items <- trim_ws(items)
  items[nzchar(items)]
}

build_expandable_value_markup <- function(value, mode = c("text", "comma", "semicolon", "newline"), ordered = FALSE, visible = 8L, item_class = "detail-meta__list") {
  mode <- match.arg(mode)
  value <- trim_ws(value)
  if (!nzchar(value)) {
    return("")
  }

  if (identical(mode, "text")) {
    return(paste0('<p class="detail-meta__text">', html_escape(value), '</p>'))
  }

  items <- split_display_items(value, mode)
  if (length(items) <= 1L) {
    return(paste0('<p class="detail-meta__text">', html_escape(value), '</p>'))
  }

  list_tag <- if (isTRUE(ordered)) "ol" else "ul"
  visible_n <- min(length(items), visible)
  visible_items <- paste0('<li>', html_escape(items[seq_len(visible_n)]), '</li>', collapse = "")
  markup <- paste0('<', list_tag, ' class="', item_class, '">', visible_items, '</', list_tag, '>')

  if (length(items) > visible_n) {
    hidden_items <- paste0('<li>', html_escape(items[(visible_n + 1L):length(items)]), '</li>', collapse = "")
    markup <- paste0(
      markup,
      '<details class="detail-more">',
      '<summary>Show ', length(items) - visible_n, ' more item', plural_suffix(length(items) - visible_n), '</summary>',
      '<', list_tag, ' class="', item_class, ' detail-more__list">', hidden_items, '</', list_tag, '>',
      '</details>'
    )
  }

  markup
}


build_detail_callout_markup <- function(detail) {
  detail <- trim_ws(detail)
  if (!nzchar(detail)) {
    return('<p class="detail-callout__text">No extra detail for this step.</p>')
  }

  build_expandable_value_markup(detail, mode = "semicolon", ordered = TRUE, visible = 8L, item_class = "detail-list")
}


build_detail_explanation_markup <- function(explanation) {
  explanation <- trim_ws(explanation)
  if (!nzchar(explanation)) {
    return('<p class="detail-explanation__text">No explanation notes for this step.</p>')
  }

  build_expandable_value_markup(explanation, mode = "newline", ordered = FALSE, visible = 6L, item_class = "detail-explanation__list")
}


build_html_inventory_view <- function(steps, nodes = NULL) {
  named <- named_inventory_indices(steps)
  step_indices <- inventory_step_indices(steps)
  collection_heading <- if (is.null(nodes)) "Named objects and outputs" else inventory_collection_heading(nodes)

  named_markup <- if (length(named) == 0L) {
    '<p class="empty-state">No named table or file inventories are available for this workflow yet.</p>'
  } else {
    paste(vapply(named, function(i) {
      build_inventory_card(steps[i, , drop = FALSE], mode = "named")
    }, character(1)), collapse = "\n")
  }

  step_markup <- if (length(step_indices) == 0L) {
    '<p class="empty-state">No step-level inventories are available for this workflow yet.</p>'
  } else {
    paste(vapply(step_indices, function(i) {
      build_inventory_card(steps[i, , drop = FALSE], mode = "step")
    }, character(1)), collapse = "\n")
  }

  paste0(
    '<section class="view-panel" data-view="inventories">',
    '<div class="inventory-layout">',
    '<section class="panel inventory-panel">',
    '<div class="panel-head panel-head--stacked">',
    '<div>',
    '<p class="section-label">Handoff view</p>',
    '<h2>Column inventories</h2>',
    '</div>',
    '<p class="panel-copy">Use this view when someone needs to rebuild the workflow in another tool. It separates what each major output contains from the step-by-step column changes that feed it.</p>',
    '</div>',
    '<div class="inventory-section">',
    '<div class="inventory-section__head">',
    '<h3>', html_escape(collection_heading), '</h3>',
    '<p>These are the most reusable inventories for handoff work because they describe the columns produced for saved objects and exported files.</p>',
    '</div>',
    '<div class="inventory-grid">', named_markup, '</div>',
    '</div>',
    '<div class="inventory-section">',
    '<div class="inventory-section__head">',
    '<h3>Step-by-step inventory</h3>',
    '<p>Use this when you need to follow how the workflow narrows, joins, or reshapes columns over time.</p>',
    '</div>',
    '<div class="inventory-list">', step_markup, '</div>',
    '</div>',
    '</section>',
    '</div>',
    '</section>'
  )
}


build_inventory_card <- function(step_row, mode = c("named", "step")) {
  mode <- match.arg(mode)
  step <- unclass(step_row[1, , drop = TRUE])
  id_prefix <- if (identical(mode, "named")) "inventoryNamed" else "inventoryStep"
  title <- if (identical(mode, "named")) inventory_subject_label(step_row) else paste0("Step ", step$step, ": ", step$title)
  eyebrow <- if (identical(mode, "named")) inventory_subject_type(step_row) else step_phase_label(step_row)
  meta <- if (identical(mode, "named")) {
    paste0("Step ", step$step, " | ", step$title)
  } else {
    piece <- character()
    if (nzchar(step$output)) {
      piece <- c(piece, paste0("Creates ", step$output))
    }
    if (nzchar(step$target_path)) {
      piece <- c(piece, paste0("Saves to ", pretty_path(step$target_path)))
    }
    if (length(piece) == 0L) paste0("Workflow step ", step$step) else paste(piece, collapse = " | ")
  }

  counts <- character()
  if (nzchar(step$input_columns)) {
    counts <- c(counts, paste0(inventory_count(step$input_columns), " input column", plural_suffix(inventory_count(step$input_columns))))
  }
  if (nzchar(step$output_columns)) {
    counts <- c(counts, paste0(inventory_count(step$output_columns), " output column", plural_suffix(inventory_count(step$output_columns))))
  }
  count_line <- if (length(counts) > 0L) paste(counts, collapse = " | ") else ""

  paste0(
    '<article class="inventory-card">',
    '<p class="inventory-card__eyebrow">', html_escape(eyebrow), '</p>',
    '<h3>', html_escape(title), '</h3>',
    '<p class="inventory-card__meta">', html_escape(meta), '</p>',
    if (nzchar(count_line)) paste0('<p class="inventory-card__counts">', html_escape(count_line), '</p>') else '',
    '<dl class="inventory-card__details">',
    build_detail_meta_row('Input columns used', step$input_columns, paste0(id_prefix, 'Input', step$step), mode = 'comma'),
    build_detail_meta_row('Output columns produced', step$output_columns, paste0(id_prefix, 'Output', step$step), mode = 'comma'),
    build_detail_meta_row('Columns carried through', step$carried_columns, paste0(id_prefix, 'Carried', step$step), mode = 'comma'),
    build_detail_meta_row('Columns newly created or changed', step$created_columns, paste0(id_prefix, 'Created', step$step), mode = 'comma'),
    '</dl>',
    '</article>'
  )
}


build_detail_meta_row <- function(label, value, id, mode = c("text", "comma", "semicolon", "newline")) {
  mode <- match.arg(mode)
  display_style <- if (nzchar(value)) '' else ' style="display:none;"'
  paste0(
    '<div class="detail-meta__row" id="', id, 'Row"', display_style, '>',
    '<dt>', html_escape(label), '</dt>',
    '<dd id="', id, '">', build_expandable_value_markup(value, mode = mode, ordered = FALSE, visible = 8L), '</dd>',
    '</div>'
  )
}


html_page_css <- function() {
  paste(c(
    ':root {',
    '  color-scheme: light;',
    '  --paper: #fbf7ef;',
    '  --ink: #18212c;',
    '  --muted: #5e6b76;',
    '  --panel: rgba(255, 255, 255, 0.84);',
    '  --border: rgba(65, 74, 83, 0.12);',
    '  --shadow: 0 24px 60px rgba(41, 52, 64, 0.12);',
    '  --accent: #234b68;',
    '}',
    '* { box-sizing: border-box; }',
    'body {',
    '  margin: 0;',
    '  font-family: "Avenir Next", "Segoe UI", "Trebuchet MS", sans-serif;',
    '  color: var(--ink);',
    '  background:',
    '    radial-gradient(circle at top left, rgba(244, 223, 155, 0.34), transparent 28%),',
    '    radial-gradient(circle at top right, rgba(217, 232, 245, 0.56), transparent 30%),',
    '    linear-gradient(180deg, #fffdf7 0%, #f7f1e6 100%);',
    '}',
    '.page-shell {',
    '  min-height: 100vh;',
    '  padding: 28px;',
    '}',
    '.hero-card, .panel, .mode-strip {',
    '  background: var(--panel);',
    '  backdrop-filter: blur(10px);',
    '  border: 1px solid var(--border);',
    '  border-radius: 24px;',
    '  box-shadow: var(--shadow);',
    '}',
    '.hero-card {',
    '  padding: 28px 30px;',
    '  display: grid;',
    '  grid-template-columns: minmax(0, 1.4fr) minmax(280px, 0.9fr);',
    '  gap: 24px;',
    '  margin-bottom: 18px;',
    '}',
    '.eyebrow, .section-label {',
    '  margin: 0 0 10px;',
    '  text-transform: uppercase;',
    '  letter-spacing: 0.14em;',
    '  font-size: 0.72rem;',
    '  color: var(--muted);',
    '}',
    '.hero-copy h1, .panel-head h2, .detail-panel h2 {',
    '  margin: 0;',
    '  font-family: Georgia, "Times New Roman", serif;',
    '  font-weight: 700;',
    '  line-height: 1.05;',
    '}',
    '.hero-copy h1 { font-size: clamp(2rem, 4vw, 3.35rem); }',
    '.script-kicker {',
    '  margin: 0 0 8px;',
    '  color: var(--muted);',
    '  font-size: 1rem;',
    '  line-height: 1.5;',
    '}',
    '.hero-date {',
    '  margin: 10px 0 0;',
    '  color: var(--muted);',
    '  font-weight: 700;',
    '}',
    '.hero-summary {',
    '  margin: 16px 0 0;',
    '  max-width: 52rem;',
    '  color: var(--muted);',
    '  font-size: 1rem;',
    '  line-height: 1.65;',
    '}',
    '.stat-grid {',
    '  display: grid;',
    '  grid-template-columns: repeat(2, minmax(0, 1fr));',
    '  gap: 14px;',
    '  align-self: start;',
    '}',
    '.stat-card {',
    '  padding: 16px 18px;',
    '  border-radius: 18px;',
    '  background: linear-gradient(180deg, rgba(255,255,255,0.92), rgba(255,255,255,0.68));',
    '  border: 1px solid rgba(255,255,255,0.8);',
    '  box-shadow: inset 0 0 0 1px rgba(0,0,0,0.03);',
    '  position: relative;',
    '}',
    '.stat-card::before {',
    '  content: "";',
    '  position: absolute;',
    '  inset: 0 auto 0 0;',
    '  width: 6px;',
    '  border-radius: 18px 0 0 18px;',
    '  background: var(--accent);',
    '}',
    '.stat-label {',
    '  display: block;',
    '  font-size: 0.8rem;',
    '  color: var(--muted);',
    '}',
    '.stat-value {',
    '  display: block;',
    '  margin-top: 8px;',
    '  font-size: 1.7rem;',
    '}',
    '.mode-strip {',
    '  padding: 16px 20px;',
    '  display: flex;',
    '  align-items: center;',
    '  justify-content: space-between;',
    '  gap: 18px;',
    '  margin-bottom: 18px;',
    '}',
    '.mode-toggle, .graph-mode-toggle {',
      '  display: inline-flex;',
      '  gap: 10px;',
      '  padding: 6px;',
      '  background: rgba(24, 33, 44, 0.05);',
      '  border-radius: 999px;',
    '}',
    '.mode-button, .graph-mode-button {',
      '  border: none;',
      '  background: transparent;',
      '  color: var(--muted);',
    '  padding: 10px 16px;',
    '  border-radius: 999px;',
    '  font-weight: 700;',
    '  cursor: pointer;',
    '}',
    '.mode-button.is-active, .graph-mode-button.is-active {',
      '  background: #284f6b;',
      '  color: white;',
      '  box-shadow: 0 8px 18px rgba(40, 79, 107, 0.22);',
    '}',
    '.mode-copy {',
    '  margin: 0;',
    '  color: var(--muted);',
    '  max-width: 46rem;',
    '  line-height: 1.55;',
    '}',
    '.view-stack {',
    '  display: grid;',
    '}',
    '.view-panel {',
    '  display: none;',
    '}',
    '.view-panel.is-visible {',
    '  display: block;',
    '}',
    '.timeline-layout {',
    '  display: grid;',
    '  grid-template-columns: minmax(0, 1.15fr) minmax(320px, 0.85fr);',
    '  gap: 22px;',
    '  align-items: start;',
    '}',
    '.panel {',
    '  padding: 24px;',
    '}',
    '.panel-head {',
    '  display: flex;',
    '  justify-content: space-between;',
    '  align-items: center;',
    '  gap: 18px;',
    '}',
    '.panel-head--stacked {',
    '  display: block;',
    '}',
    '.panel-copy {',
    '  margin: 10px 0 0;',
    '  color: var(--muted);',
    '  line-height: 1.65;',
    '}',
    '.phase-list {',
    '  display: grid;',
    '  gap: 14px;',
    '  margin-top: 18px;',
    '}',
    '.phase-group {',
    '  border: 1px solid rgba(35, 75, 104, 0.10);',
    '  border-radius: 20px;',
    '  background: rgba(255,255,255,0.70);',
    '  overflow: hidden;',
    '}',
    '.phase-group summary {',
    '  list-style: none;',
    '  cursor: pointer;',
    '  padding: 16px 18px;',
    '  display: flex;',
    '  justify-content: space-between;',
    '  align-items: center;',
    '  gap: 16px;',
    '  background: linear-gradient(180deg, rgba(255,255,255,0.92), rgba(248,244,235,0.82));',
    '}',
    '.phase-group summary::-webkit-details-marker { display: none; }',
    '.phase-title {',
    '  font-weight: 700;',
    '  font-size: 1rem;',
    '}',
    '.phase-meta {',
    '  color: var(--muted);',
    '  font-size: 0.88rem;',
    '  text-align: right;',
    '}',
    '.phase-steps {',
      '  display: grid;',
      '  gap: 10px;',
      '  padding: 14px;',
    '}',
    '.phase-expand {',
      '  justify-self: start;',
      '  border: none;',
      '  border-radius: 999px;',
      '  padding: 9px 14px;',
      '  background: rgba(40, 79, 107, 0.10);',
      '  color: #1f425c;',
      '  font-weight: 700;',
      '  cursor: pointer;',
    '}',
    '.timeline-step {',
      '  width: 100%;',
      '  text-align: left;',
    '  border: 1px solid rgba(35, 75, 104, 0.10);',
    '  border-radius: 18px;',
    '  background: white;',
    '  padding: 14px 16px;',
    '  display: grid;',
    '  grid-template-columns: 34px minmax(0, 1fr);',
    '  gap: 12px;',
      '  cursor: pointer;',
      '  transition: transform 120ms ease, box-shadow 120ms ease, border-color 120ms ease;',
    '}',
    '.timeline-step.is-collapsed { display: none; }',
    '.phase-group.is-expanded .timeline-step.is-collapsed { display: grid; }',
    '.timeline-step:hover, .timeline-step:focus-visible {',
      '  transform: translateY(-1px);',
      '  box-shadow: 0 14px 26px rgba(35, 75, 104, 0.12);',
    '  border-color: rgba(40, 79, 107, 0.28);',
    '  outline: none;',
    '}',
    '.timeline-step.is-active {',
    '  border-color: rgba(40, 79, 107, 0.42);',
    '  box-shadow: 0 18px 30px rgba(40, 79, 107, 0.14);',
    '  background: linear-gradient(180deg, rgba(255,255,255,1), rgba(244,248,250,0.96));',
    '}',
    '.timeline-step__number {',
    '  width: 34px;',
    '  height: 34px;',
    '  border-radius: 999px;',
    '  background: #284f6b;',
    '  color: white;',
    '  display: inline-flex;',
    '  align-items: center;',
    '  justify-content: center;',
    '  font-weight: 700;',
    '}',
    '.timeline-step__body {',
    '  min-width: 0;',
    '  display: grid;',
    '  gap: 4px;',
    '}',
    '.timeline-step__title {',
    '  display: block;',
    '  font-size: 1rem;',
    '}',
    '.timeline-step__preview {',
    '  color: var(--muted);',
    '  font-size: 0.9rem;',
    '  line-height: 1.45;',
    '}',
    '.timeline-step__inventory {',
    '  display: inline-flex;',
    '  justify-self: start;',
    '  margin-top: 4px;',
    '  border-radius: 999px;',
    '  padding: 5px 9px;',
    '  background: rgba(79, 129, 49, 0.10);',
    '  color: #36511d;',
    '  font-size: 0.76rem;',
    '  font-weight: 700;',
    '}',
    '.detail-panel {',
    '  position: sticky;',
    '  top: 22px;',
    '}',
    '.detail-sticky {',
    '  display: grid;',
    '  gap: 14px;',
    '}',
    '.detail-header {',
    '  display: flex;',
    '  flex-wrap: wrap;',
    '  gap: 8px;',
    '}',
    '.detail-chip {',
    '  display: inline-flex;',
    '  align-items: center;',
    '  border-radius: 999px;',
    '  padding: 7px 12px;',
    '  background: rgba(40, 79, 107, 0.10);',
    '  color: #1f425c;',
    '  font-size: 0.82rem;',
    '  font-weight: 700;',
    '}',
    '.detail-chip--muted {',
    '  background: rgba(24, 33, 44, 0.07);',
    '  color: var(--muted);',
    '}',
    '.detail-narrative {',
    '  margin: 0;',
    '  color: var(--ink);',
    '  line-height: 1.7;',
    '  font-size: 1rem;',
    '  font-weight: 600;',
    '}',
    '.detail-callout {',
    '  padding: 14px 16px;',
    '  border-radius: 18px;',
    '  background: linear-gradient(180deg, rgba(244, 223, 155, 0.22), rgba(255,255,255,0.74));',
    '  border: 1px solid rgba(208, 160, 33, 0.18);',
    '  line-height: 1.65;',
    '}',
    '.detail-callout__text {',
    '  margin: 0;',
    '}',
    '.detail-list {',
    '  margin: 0;',
    '  padding-left: 1.3rem;',
    '  display: grid;',
    '  gap: 0.4rem;',
    '}',
    '.detail-list li {',
    '  padding-left: 0.1rem;',
    '}',
    '.detail-more {',
    '  margin-top: 10px;',
    '}',
    '.detail-more summary {',
    '  cursor: pointer;',
    '  color: #1f425c;',
    '  font-weight: 700;',
    '}',
    '.detail-more__list {',
    '  margin-top: 10px;',
    '}',
    '.detail-explanation {',
    '  padding: 14px 16px;',
    '  border-radius: 18px;',
    '  background: rgba(35, 75, 104, 0.07);',
    '  border: 1px solid rgba(35, 75, 104, 0.12);',
    '  line-height: 1.65;',
    '}',
    '.detail-explanation::before {',
    '  content: "Explanation";',
    '  display: block;',
    '  margin-bottom: 6px;',
    '  color: var(--muted);',
    '  font-size: 0.72rem;',
    '  text-transform: uppercase;',
    '  letter-spacing: 0.12em;',
    '  font-weight: 700;',
    '}',
    '.detail-explanation__text, .detail-explanation__list {',
    '  margin: 0;',
    '}',
    '.detail-explanation__list {',
    '  padding-left: 1.1rem;',
    '  display: grid;',
    '  gap: 0.35rem;',
    '}',
    '.detail-code {',
    '  border-radius: 18px;',
    '  border: 1px solid rgba(24, 33, 44, 0.10);',
    '  background: rgba(255,255,255,0.72);',
    '  overflow: hidden;',
    '}',
    '.detail-code summary {',
    '  cursor: pointer;',
    '  padding: 12px 14px;',
    '  font-weight: 700;',
    '  color: #1f425c;',
    '}',
    '.detail-code pre {',
    '  margin: 0;',
    '  padding: 14px;',
    '  overflow: auto;',
    '  background: #18212c;',
    '  color: #f8f0dc;',
    '  font-size: 0.82rem;',
    '  line-height: 1.5;',
    '}',
    '.detail-code code {',
    '  font-family: "SFMono-Regular", Consolas, "Liberation Mono", monospace;',
    '  white-space: pre;',
    '}',
    '.detail-code.is-empty { display: none; }',
    '.detail-meta {',
    '  margin: 0;',
    '  display: grid;',
    '  gap: 10px;',
    '}',
    '.detail-meta__row {',
    '  display: grid;',
    '  gap: 4px;',
    '  padding: 12px 14px;',
    '  border-radius: 16px;',
    '  background: rgba(24, 33, 44, 0.04);',
    '}',
    '.detail-meta dt {',
    '  color: var(--muted);',
    '  font-size: 0.78rem;',
    '  text-transform: uppercase;',
    '  letter-spacing: 0.12em;',
    '}',
    '.detail-meta dd {',
    '  margin: 0;',
    '  font-weight: 600;',
    '  word-break: break-word;',
    '}',
    '.detail-meta__text {',
    '  margin: 0;',
    '  font-weight: 600;',
    '  line-height: 1.6;',
    '}',
    '.detail-meta__list {',
    '  margin: 0;',
    '  padding-left: 1.2rem;',
    '  display: grid;',
    '  gap: 0.35rem;',
    '  line-height: 1.55;',
    '  font-weight: 600;',
    '}',
    '.detail-help {',
    '  margin: 0;',
    '  color: var(--muted);',
    '  line-height: 1.6;',
    '}',
    '.detail-empty {',
    '  color: var(--muted);',
    '}',
    '.inventory-layout {',
    '  display: grid;',
    '}',
    '.inventory-panel {',
    '  display: grid;',
    '  gap: 24px;',
    '}',
    '.inventory-section {',
    '  display: grid;',
    '  gap: 14px;',
    '}',
    '.inventory-section__head h3 {',
    '  margin: 0;',
    '  font-family: Georgia, "Times New Roman", serif;',
    '  font-size: 1.35rem;',
    '}',
    '.inventory-section__head p {',
    '  margin: 8px 0 0;',
    '  color: var(--muted);',
    '  line-height: 1.6;',
    '}',
    '.inventory-grid {',
    '  display: grid;',
    '  grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));',
    '  gap: 16px;',
    '}',
    '.inventory-list {',
    '  display: grid;',
    '  gap: 14px;',
    '}',
    '.inventory-card {',
    '  border-radius: 20px;',
    '  padding: 18px;',
    '  background: linear-gradient(180deg, rgba(255,255,255,0.96), rgba(246,241,231,0.86));',
    '  border: 1px solid rgba(35, 75, 104, 0.10);',
    '}',
    '.inventory-card__eyebrow {',
    '  margin: 0 0 8px;',
    '  font-size: 0.72rem;',
    '  text-transform: uppercase;',
    '  letter-spacing: 0.14em;',
    '  color: var(--muted);',
    '}',
    '.inventory-card h3 {',
    '  margin: 0;',
    '  font-size: 1.05rem;',
    '}',
    '.inventory-card__meta, .inventory-card__counts {',
    '  margin: 8px 0 0;',
    '  color: var(--muted);',
    '  line-height: 1.55;',
    '}',
    '.inventory-card__counts {',
    '  font-weight: 700;',
    '  color: #36511d;',
    '}',
    '.inventory-card__details {',
    '  margin: 14px 0 0;',
    '  display: grid;',
    '  gap: 10px;',
    '}',
    '.graph-panel { min-height: 78vh; }',
    '.graph-toolbar {',
    '  display: flex;',
    '  flex-wrap: wrap;',
    '  align-items: center;',
    '  gap: 10px;',
    '  margin: 16px 0 12px;',
    '}',
    '.graph-tool {',
    '  border: 1px solid rgba(35, 75, 104, 0.14);',
    '  border-radius: 999px;',
    '  min-width: 42px;',
    '  padding: 10px 14px;',
    '  background: white;',
    '  color: #1f425c;',
    '  font-weight: 800;',
    '  cursor: pointer;',
    '}',
    '.graph-tool--wide {',
    '  min-width: 64px;',
    '}',
    '.graph-zoom-label {',
    '  margin-left: auto;',
    '  border-radius: 999px;',
    '  padding: 8px 12px;',
    '  background: rgba(35, 75, 104, 0.08);',
    '  color: #1f425c;',
    '  font-weight: 700;',
    '}',
    '.focus-button {',
      '  border: none;',
      '  border-radius: 999px;',
    '  padding: 11px 16px;',
    '  background: linear-gradient(135deg, #284f6b, #3f799b);',
      '  color: white;',
      '  font-weight: 700;',
      '  cursor: pointer;',
    '}',
    '.graph-subviews {',
      '  display: grid;',
      '  gap: 16px;',
      '  margin-top: 18px;',
    '}',
    '.graph-subview { display: none; }',
    '.graph-subview.is-visible { display: block; }',
    '.graph-frame {',
      '  height: min(82vh, 1100px);',
      '  overflow: auto;',
      '  border-radius: 20px;',
    '  background: linear-gradient(180deg, rgba(255,255,255,0.94), rgba(255,255,255,0.72));',
    '  border: 1px solid rgba(35, 75, 104, 0.10);',
    '  padding: 18px;',
    '}',
    '.graph-canvas {',
    '  width: max-content;',
    '  min-width: 100%;',
    '}',
    '.graph-frame svg {',
    '  width: auto;',
    '  max-width: none;',
      '  height: auto;',
      '  display: block;',
    '}',
    '.phase-overview {',
      '  display: grid;',
      '  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));',
      '  gap: 14px;',
    '}',
    '.phase-card {',
      '  border-radius: 20px;',
      '  padding: 18px;',
      '  background: linear-gradient(180deg, rgba(255,255,255,0.96), rgba(246,241,231,0.86));',
      '  border: 1px solid rgba(35, 75, 104, 0.10);',
    '}',
    '.phase-card__eyebrow {',
      '  margin: 0 0 8px;',
      '  font-size: 0.72rem;',
      '  text-transform: uppercase;',
      '  letter-spacing: 0.14em;',
      '  color: var(--muted);',
    '}',
    '.phase-card h3 {',
      '  margin: 0;',
      '  font-size: 1.05rem;',
    '}',
    '.phase-card__meta {',
      '  margin: 8px 0 0;',
      '  color: var(--muted);',
      '  line-height: 1.55;',
    '}',
    '.phase-card__list {',
      '  margin: 14px 0 0;',
      '  padding-left: 18px;',
      '  color: var(--ink);',
      '  line-height: 1.55;',
    '}',
    '.empty-state {',
      '  color: var(--muted);',
      '  margin: 0;',
    '}',
    '@media (max-width: 1100px) {',
    '  .hero-card, .timeline-layout { grid-template-columns: 1fr; }',
    '  .detail-panel { position: static; }',
    '  .mode-strip { flex-direction: column; align-items: flex-start; }',
    '}',
    '@media (max-width: 720px) {',
    '  .page-shell { padding: 16px; }',
    '  .hero-card, .panel, .mode-strip { padding: 18px; }',
    '  .stat-grid { grid-template-columns: 1fr 1fr; }',
    '  .mode-toggle { width: 100%; }',
    '  .mode-button { flex: 1 1 auto; }',
    '  .graph-zoom-label { margin-left: 0; }',
    '  .phase-group summary { display: block; }',
    '  .phase-meta { display: block; text-align: left; margin-top: 4px; }',
    '}',
    ''), collapse = "\n")
}


html_page_js <- function() {
  paste(c(
    '(function() {',
    '  var viewButtons = Array.prototype.slice.call(document.querySelectorAll("[data-view-target]"));',
    '  var viewPanels = Array.prototype.slice.call(document.querySelectorAll(".view-panel"));',
    '  var graphFrame = document.getElementById("graphFrame");',
    '  var resetButton = document.getElementById("graphResetButton");',
    '  var stepButtons = Array.prototype.slice.call(document.querySelectorAll(".timeline-step"));',
    '  var phaseExpandButtons = Array.prototype.slice.call(document.querySelectorAll("[data-phase-expand]"));',
    '  var detailMap = {',
    '    phase: document.getElementById("detailPhase"),',
    '    kind: document.getElementById("detailKind"),',
    '    title: document.getElementById("detailTitle"),',
    '    narrative: document.getElementById("detailNarrative"),',
    '    callout: document.getElementById("detailCallout"),',
    '    explanationBlock: document.getElementById("detailExplanationBlock"),',
    '    codeBlock: document.getElementById("detailCodeBlock"),',
    '    code: document.getElementById("detailCode"),',
    '    sourceRow: document.getElementById("detailSourceRow"),',
    '    source: document.getElementById("detailSource"),',
    '    outputRow: document.getElementById("detailOutputRow"),',
    '    output: document.getElementById("detailOutput"),',
    '    targetRow: document.getElementById("detailTargetRow"),',
    '    target: document.getElementById("detailTarget"),',
    '    supersededRow: document.getElementById("detailSupersededRow"),',
    '    superseded: document.getElementById("detailSuperseded"),',
    '    inputColumnsRow: document.getElementById("detailInputColumnsRow"),',
    '    inputColumns: document.getElementById("detailInputColumns"),',
    '    outputColumnsRow: document.getElementById("detailOutputColumnsRow"),',
    '    outputColumns: document.getElementById("detailOutputColumns"),',
    '    carriedColumnsRow: document.getElementById("detailCarriedColumnsRow"),',
    '    carriedColumns: document.getElementById("detailCarriedColumns"),',
    '    createdColumnsRow: document.getElementById("detailCreatedColumnsRow"),',
    '    createdColumns: document.getElementById("detailCreatedColumns")',
    '  };',
    '  function setupGraphZoom(frame, options) {',
    '    if (!frame) return null;',
    '    var svg = frame.querySelector("svg");',
    '    if (!svg) return null;',
    '    var label = options && options.label ? document.getElementById(options.label) : null;',
    '    var width = 0;',
    '    if (svg.viewBox && svg.viewBox.baseVal && svg.viewBox.baseVal.width) width = svg.viewBox.baseVal.width;',
    '    if (!width) width = parseFloat(svg.getAttribute("width")) || svg.getBoundingClientRect().width || 1200;',
    '    var baseWidth = width;',
    '    var state = { scale: 1 };',
    '    function updateLabel() { if (label) label.textContent = Math.round(state.scale * 100) + "%"; }',
    '    function applyScale(scale) {',
    '      state.scale = Math.max(0.35, Math.min(scale, 2.5));',
    '      svg.style.setProperty("width", (baseWidth * state.scale) + "px");',
    '      svg.style.setProperty("max-width", "none");',
    '      svg.style.setProperty("height", "auto");',
    '      updateLabel();',
    '    }',
    '    function fit() {',
    '      var available = Math.max(frame.clientWidth - 36, 320);',
    '      applyScale(available / baseWidth);',
    '      frame.scrollTo({ top: 0, left: 0, behavior: "smooth" });',
    '    }',
    '    applyScale(1);',
    '    if (options && options.fit !== false) fit();',
    '    return {',
    '      zoomIn: function() { applyScale(state.scale * 1.2); },',
    '      zoomOut: function() { applyScale(state.scale / 1.2); },',
    '      actual: function() { applyScale(1); frame.scrollTo({ top: 0, left: 0, behavior: "smooth" }); },',
    '      fit: fit',
    '    };',
    '  }',
    '  function bindToolbar(controller, ids) {',
    '    if (!controller) return;',
    '    var out = document.getElementById(ids.out);',
    '    var inn = document.getElementById(ids.inn);',
    '    var fit = document.getElementById(ids.fit);',
    '    var actual = document.getElementById(ids.actual);',
    '    if (out) out.addEventListener("click", controller.zoomOut);',
    '    if (inn) inn.addEventListener("click", controller.zoomIn);',
    '    if (fit) fit.addEventListener("click", controller.fit);',
    '    if (actual) actual.addEventListener("click", controller.actual);',
    '  }',
    '  function setView(viewName) {',
    '    viewButtons.forEach(function(button) {',
    '      var active = button.getAttribute("data-view-target") === viewName;',
    '      button.classList.toggle("is-active", active);',
    '      button.setAttribute("aria-pressed", active ? "true" : "false");',
    '    });',
    '    viewPanels.forEach(function(panel) {',
    '      panel.classList.toggle("is-visible", panel.getAttribute("data-view") === viewName);',
    '    });',
    '  }',
    '  function clearNode(node) {',
    '    if (!node) return;',
    '    while (node.firstChild) node.removeChild(node.firstChild);',
    '  }',
    '  function appendParagraph(containerNode, className, text) {',
    '    var p = document.createElement("p");',
    '    p.className = className;',
    '    p.textContent = text;',
    '    containerNode.appendChild(p);',
    '  }',
    '  function splitTopLevel(text, delimiter) {',
    '    var parts = [];',
    '    var current = "";',
    '    var depth = 0;',
    '    var quote = "";',
    '    for (var i = 0; i < text.length; i += 1) {',
    '      var ch = text.charAt(i);',
    '      if (quote) {',
    '        current += ch;',
    '        if (ch === quote && text.charAt(i - 1) !== "\\\\") quote = "";',
    '        continue;',
    '      }',
    "      if (ch === \"\\\"\" || ch === \"'\" || ch === \"`\") { quote = ch; current += ch; continue; }",
    '      if (ch === "(" || ch === "[") { depth += 1; current += ch; continue; }',
    '      if ((ch === ")" || ch === "]") && depth > 0) { depth -= 1; current += ch; continue; }',
    '      if (ch === delimiter && depth === 0) { parts.push(current.trim()); current = ""; continue; }',
    '      current += ch;',
    '    }',
    '    if (current.trim()) parts.push(current.trim());',
    '    return parts.filter(Boolean);',
    '  }',
    '  function parseItems(text, mode) {',
    '    var value = (text || "").trim();',
    '    if (!value) return [];',
    '    if (mode === "semicolon") return value.split(";").map(function(part) { return part.trim(); }).filter(Boolean);',
    '    if (mode === "newline") return value.split(/\\n+/).map(function(part) { return part.trim(); }).filter(Boolean);',
    '    if (mode === "comma") return splitTopLevel(value, ",");',
    '    return [value];',
    '  }',
    '  function appendExpandableList(containerNode, items, options) {',
    '    var opts = options || {};',
    '    var listTag = opts.ordered ? "ol" : "ul";',
    '    var className = opts.className || "detail-meta__list";',
    '    var visible = Math.min(items.length, opts.visible || 8);',
    '    var list = document.createElement(listTag);',
    '    list.className = className;',
    '    items.slice(0, visible).forEach(function(item) {',
    '      var li = document.createElement("li");',
    '      li.textContent = item;',
    '      list.appendChild(li);',
    '    });',
    '    containerNode.appendChild(list);',
    '    if (items.length > visible) {',
    '      var details = document.createElement("details");',
    '      details.className = "detail-more";',
    '      var summary = document.createElement("summary");',
    '      var hiddenCount = items.length - visible;',
    '      summary.textContent = "Show " + hiddenCount + " more item" + (hiddenCount === 1 ? "" : "s");',
    '      details.appendChild(summary);',
    '      var hiddenList = document.createElement(listTag);',
    '      hiddenList.className = className + " detail-more__list";',
    '      items.slice(visible).forEach(function(item) {',
    '        var li = document.createElement("li");',
    '        li.textContent = item;',
    '        hiddenList.appendChild(li);',
    '      });',
    '      details.appendChild(hiddenList);',
    '      containerNode.appendChild(details);',
    '    }',
    '  }',
    '  function renderDetailCallout(detail, containerNode) {',
    '    if (!containerNode) return;',
    '    clearNode(containerNode);',
    '    var text = (detail || "").trim();',
    '    if (!text) {',
    '      appendParagraph(containerNode, "detail-callout__text", "No extra detail for this step.");',
    '      return;',
    '    }',
    '    var parts = parseItems(text, "semicolon");',
    '    if (parts.length <= 1) {',
    '      appendParagraph(containerNode, "detail-callout__text", text);',
    '      return;',
    '    }',
    '    appendExpandableList(containerNode, parts, { ordered: true, className: "detail-list", visible: 8 });',
    '  }',
    '  function renderDetailExplanation(explanation, containerNode) {',
    '    if (!containerNode) return;',
    '    clearNode(containerNode);',
    '    var text = (explanation || "").trim();',
    '    if (!text) {',
    '      appendParagraph(containerNode, "detail-explanation__text", "No explanation notes for this step.");',
    '      return;',
    '    }',
    '    var parts = parseItems(text, "newline");',
    '    if (parts.length <= 1) {',
    '      appendParagraph(containerNode, "detail-explanation__text", text);',
    '      return;',
    '    }',
    '    appendExpandableList(containerNode, parts, { ordered: false, className: "detail-explanation__list", visible: 6 });',
    '  }',
    '  function renderMetaValue(value, containerNode, mode) {',
    '    if (!containerNode) return;',
    '    clearNode(containerNode);',
    '    var text = (value || "").trim();',
    '    if (!text) return;',
    '    var parts = parseItems(text, mode || "text");',
    '    if (parts.length <= 1 || mode === "text") {',
    '      appendParagraph(containerNode, "detail-meta__text", text);',
    '      return;',
    '    }',
    '    appendExpandableList(containerNode, parts, { ordered: false, className: "detail-meta__list", visible: 8 });',
    '  }',
    '  function setMeta(rowNode, valueNode, value, mode) {',
    '    if (!rowNode || !valueNode) return;',
    '    if ((value || "").trim()) {',
    '      rowNode.style.display = "grid";',
    '      renderMetaValue(value, valueNode, mode);',
    '    } else {',
    '      rowNode.style.display = "none";',
    '      clearNode(valueNode);',
    '    }',
    '  }',
    '  function setCodeChunk(code) {',
    '    var text = (code || "").trim();',
    '    if (!detailMap.codeBlock || !detailMap.code) return;',
    '    detailMap.codeBlock.classList.toggle("is-empty", !text);',
    '    detailMap.code.textContent = text;',
    '  }',
    '  function activateStep(button) {',
    '    stepButtons.forEach(function(node) { node.classList.remove("is-active"); });',
    '    button.classList.add("is-active");',
    '    detailMap.phase.textContent = button.dataset.phase || "";',
    '    detailMap.kind.textContent = button.dataset.kind || "";',
    '    detailMap.title.textContent = button.dataset.title || "";',
    '    detailMap.narrative.textContent = button.dataset.summary || button.dataset.narrative || "";',
    '    renderDetailCallout(button.dataset.detail || "", detailMap.callout);',
    '    renderDetailExplanation(button.dataset.explanation || "", detailMap.explanationBlock);',
    '    setCodeChunk(button.dataset.code || "");',
    '    setMeta(detailMap.sourceRow, detailMap.source, button.dataset.source || "", "comma");',
    '    setMeta(detailMap.outputRow, detailMap.output, button.dataset.output || "", "text");',
    '    setMeta(detailMap.targetRow, detailMap.target, button.dataset.target || "", "text");',
    '    setMeta(detailMap.supersededRow, detailMap.superseded, button.dataset.supersededBy ? ("Step " + button.dataset.supersededBy) : "", "text");',
    '    setMeta(detailMap.inputColumnsRow, detailMap.inputColumns, button.dataset.inputColumns || "", "comma");',
    '    setMeta(detailMap.outputColumnsRow, detailMap.outputColumns, button.dataset.outputColumns || "", "comma");',
    '    setMeta(detailMap.carriedColumnsRow, detailMap.carriedColumns, button.dataset.carriedColumns || "", "comma");',
    '    setMeta(detailMap.createdColumnsRow, detailMap.createdColumns, button.dataset.createdColumns || "", "comma");',
    '  }',
    '  var zoomController = setupGraphZoom(graphFrame, { label: "graphZoomLabel" });',
    '  bindToolbar(zoomController, { out: "graphZoomOutButton", inn: "graphZoomInButton", fit: "graphFitButton", actual: "graphActualButton" });',
    '  viewButtons.forEach(function(button) {',
    '    button.addEventListener("click", function() {',
    '      setView(button.getAttribute("data-view-target"));',
    '      if (button.getAttribute("data-view-target") === "graph" && zoomController) zoomController.fit();',
    '    });',
    '  });',
    '  phaseExpandButtons.forEach(function(button) {',
    '    button.addEventListener("click", function() {',
    '      var group = button.closest(".phase-group");',
    '      if (!group) return;',
    '      var expanded = group.classList.toggle("is-expanded");',
    '      button.textContent = expanded ? (button.getAttribute("data-expanded-label") || "Show fewer steps") : (button.getAttribute("data-collapsed-label") || "Show more steps");',
    '      button.setAttribute("aria-expanded", expanded ? "true" : "false");',
    '    });',
    '  });',
    '  if (resetButton && zoomController) resetButton.addEventListener("click", function() { zoomController.fit(); });',
    '  stepButtons.forEach(function(button) {',
    '    button.addEventListener("click", function() { activateStep(button); });',
    '  });',
    '  if (stepButtons.length > 0) activateStep(stepButtons[0]);',
    '})();'
  ), collapse = "\n")
}
