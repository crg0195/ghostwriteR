# ghostwriteR news

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
