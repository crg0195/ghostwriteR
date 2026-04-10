# ghostwriteR news

## ghostwriteR 0.0.7

- Split the package code into focused modules for parsing, humanization, HTML output, reporting, graph generation, and utilities so future debugging and iteration are easier.
- Improved SQL CTE documentation to summarize working datasets and main queries more clearly, with better stage-scoped code chunks and handling for divider comments such as `--------- MAIN QUERY ---------`.

## ghostwriteR 0.0.6

- Fixed a SQL inventory edge case that could error with `subscript out of bounds` when source references outnumbered available source metadata.

## ghostwriteR 0.0.5

- Added richer HTML step detail panes with an Explanation section and collapsible code chunks.
- Hardened HTML detail rendering by replacing dynamic innerHTML updates with DOM construction and textContent.
- Improved R workflow parsing for JSON ingestion, legacy pipes, chart pipelines, and common data science verbs.
- Improved SQL workflow parsing for grouped filters, joins, unions, inventories, and common dialect patterns.
- Updated the HTML report header to show the script title and generation date.

## ghostwriteR 0.0.3

- Aligned package version metadata with the latest GitHub release tag.
- Added path-display controls for safer sharing of generated output.
- Added clearer README guidance for GitHub installs and public sharing.
- Added a friendlier DOCX export error when a zip utility is unavailable.

## ghostwriteR 0.0.1

- Initial public GitHub release.
- Added support for parsing single-file R and SQL workflows.
- Added graph export for HTML, standalone HTML graph, SVG, PNG, and PDF.
- Added plain-language workflow reports and bundle exports.
- Improved handoff readability with workflow timelines, inventories, and
  object-aware graph labels.
