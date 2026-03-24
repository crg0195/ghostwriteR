# ghostwriteR

Visualize R and SQL workflow logic as shareable diagrams and handoff documents.

## Install

```r
# install from GitHub
install.packages("remotes")
remotes::install_github("crg0195/ghostwriteR")
```

## Usage

```r
library(ghostwriteR)

# Choose a script via dialog
path <- file.choose()

# Write an HTML handoff page
ghostwriteR(path, format = "html", out = "script-flow.html")

# Or a standalone graph
ghostwriteR(path, format = "html_graph", out = "script-graph.html")
```

## Notes

- PDF/PNG output requires `DiagrammeRsvg` and `rsvg`.
- SVG output requires `DiagrammeRsvg` only.
- HTML output is self-contained and can be shared directly.
