# ghostwriteR

Visualize R and SQL workflow logic as shareable diagrams and handoff documents.

## What It Does

- parses a single R or SQL script without executing it
- builds a dependency graph of sources, transformations, and outputs
- exports self-contained HTML handoff pages that are easy to share
- generates plain-language workflow reports for technical and non-technical readers

For R scripts, ghostwriteR uses parsing via `parse()` rather than `source()`
or `eval()`, so it reads workflow structure without intentionally running the
script itself.

## Install

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("crg0195/ghostwriteR")
```

For a stable shared version, install from a tagged release:

```r
remotes::install_github("crg0195/ghostwriteR", ref = "v0.0.8")
```

## Quick Start

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

## Output Options

- `format = "graph"` returns a `DiagrammeR` graph object in R
- `format = "html"` writes a self-contained interactive handoff page
- `format = "html_graph"` writes a self-contained graph-only HTML page
- `format = "svg"`, `"png"`, and `"pdf"` write exportable diagrams
- `ghostwriter_report()` writes Markdown, text, or Word-style report output
- `ghostwriter_bundle()` writes the graph and report together

## Notes

- PDF/PNG output requires `DiagrammeRsvg` and `rsvg`.
- SVG output requires `DiagrammeRsvg` only.
- HTML output is self-contained and can be shared directly.
- The package parses scripts without executing them, so example input files do
  not need to exist just to generate the workflow map.

## Sharing Safely

- Do not commit proprietary scripts, real data extracts, API keys, passwords,
  or local machine paths.
- Generated HTML and report outputs can include literal file paths, table
  names, and column names that appear in the source script.
- If you want file paths reduced to just filenames in generated output, set:

```r
options(ghostwriteR.path_style = "basename")
```

- Review generated reports before sharing if the source script names sensitive
  files, tables, or business logic.
- For coordinated releases to colleagues, prefer a tagged GitHub release over
  sharing whatever happens to be on the default branch.

## Development Status

This package is currently shared through GitHub rather than CRAN. Install from
the repository or from a tagged release when you want a more stable version.
