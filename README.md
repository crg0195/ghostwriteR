# ghostwriteR

Visualize R and SQL workflow logic as shareable diagrams and handoff documents.

## Install

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("crg0195/ghostwriteR")
```

If you want a stable shared version for colleagues, install from a tagged
release instead of the default branch once a release has been created.

## Usage

```r
library(ghostwriteR)

# Choose a script via dialog
path <- file.choose()

# Write an HTML handoff page
ghostwriteR(path, format = "html", out = "script-flow.html")

# Or a standalone graph
ghostwriteR(path, format = "html_graph", out = "script-graph.html")

# Or create a diagram + report bundle together
ghostwriter_bundle(path, graph_format = "html", report_format = "markdown")
```

## Notes

- PDF/PNG output requires `DiagrammeRsvg` and `rsvg`.
- SVG output requires `DiagrammeRsvg` only.
- HTML output is self-contained and can be shared directly.
- The package parses scripts without executing them, so example input files do
  not need to exist just to generate the workflow map.

## Sharing Safely

- Do not commit proprietary scripts, real data extracts, API keys, passwords,
  or local machine paths.
- Review generated reports before sharing if the source script names sensitive
  files, tables, or business logic.
- For coordinated releases to colleagues, prefer a tagged GitHub release over
  sharing whatever happens to be on the default branch.
