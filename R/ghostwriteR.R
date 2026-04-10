#' Visualize workflow logic in an R or SQL script
#'
#' @param path Path to an R or SQL script file.
#' @param format Output format. Use `"graph"` to return a DiagrammeR graph
#'   object, or `"html"`, `"html_graph"`, `"svg"`, `"png"`, or `"pdf"` to
#'   write a file.
#' @param out Output file path. Required when `format` is not `"graph"`.
#' @return A DiagrammeR grViz object when `format = "graph"`. Otherwise,
#'   invisibly returns the written file path.
#' @export
ghostwriteR <- function(path, format = c("graph", "html", "html_graph", "svg", "png", "pdf"), out = NULL) {
  format <- match.arg(format)
  parsed <- ghostwriter_parse(path)
  graph <- ghostwriter_graph(parsed)

  if (identical(format, "graph")) {
    return(graph)
  }

  out <- resolve_output_path(path, format, out)
  render_graph_file(graph, out, format, parsed = parsed)
  invisible(out)
}


#' Build a written explanation of an R or SQL workflow
#'
#' @param path Path to an R or SQL script file.
#' @param format Output format for the report: `"markdown"`, `"text"`, or
#'   `"docx"`.
#' @param out Optional file path to write the report.
#' @return The report text when `out` is `NULL` and format is text-based.
#'   Otherwise, invisibly returns the written file path.
#' @export
ghostwriter_report <- function(path, format = c("markdown", "text", "docx"), out = NULL) {
  format <- match.arg(format)
  parsed <- ghostwriter_parse(path)

  if (identical(format, "docx")) {
    out <- resolve_report_path(path, format, out)
    write_docx_report(parsed, out)
    return(invisible(out))
  }

  report <- build_script_report(parsed, format)

  if (is.null(out)) {
    return(report)
  }

  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  writeLines(report, con = out, useBytes = TRUE)
  invisible(out)
}


#' Export a diagram and written report together
#'
#' @param path Path to an R or SQL script file.
#' @param dir Output directory for the bundle. Defaults to a folder beside the
#'   script named `<script-name>-ghostwriteR`.
#' @param graph_format File format for the diagram: `"html"`, `"png"`,
#'   `"svg"`, or `"pdf"`.
#' @param report_format Output format for the report: `"docx"`, `"markdown"`,
#'   or `"text"`.
#' @return A named list with paths for the bundle directory, diagram, and report.
#' @export
ghostwriter_bundle <- function(path,
                             dir = NULL,
                             graph_format = c("html", "html_graph", "png", "svg", "pdf"),
                             report_format = c("docx", "markdown", "text")) {
  graph_format <- match.arg(graph_format)
  report_format <- match.arg(report_format)

  dir <- resolve_bundle_dir(path, dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  base <- tools::file_path_sans_ext(basename(path))
  graph_extension <- if (identical(graph_format, "html_graph")) "html" else graph_format
  graph_out <- file.path(dir, paste0(base, "-ghostwriteR.", graph_extension))
  report_out <- file.path(
    dir,
    paste0(base, "-ghostwriteR-report.", report_extension(report_format))
  )

  ghostwriteR(path, format = graph_format, out = graph_out)
  ghostwriter_report(path, format = report_format, out = report_out)

  list(
    directory = dir,
    graph = graph_out,
    report = report_out
  )
}


#' Extract data-flow edges from an R or SQL script
#'
#' @param path Path to an R or SQL script file.
#' @return A data frame with columns `from`, `to`, and `type`.
#' @export
ghostwriter_edges <- function(path) {
  parsed <- ghostwriter_parse(path)
  edges <- parsed$edges
  attr(edges, "nodes") <- parsed$nodes
  attr(edges, "steps") <- parsed$steps
  attr(edges, "script_path") <- parsed$script_path
  edges
}

#' Build a DiagrammeR graph from extracted edges
#'
#' @param x A parsed ghostwriteR object or an edge data frame from `ghostwriter_edges()`.
#' @return A DiagrammeR grViz object.
#' @export
ghostwriter_graph <- function(x) {
  ghostwriter_graph_impl(x)
}



ghostwriter_parse <- function(path) {
  if (!file.exists(path)) {
    stop("Script not found: ", path)
  }

  language <- ghostwriter_detect_language(path)

  if (identical(language, "sql")) {
    return(ghostwriter_parse_sql(path))
  }

  ghostwriter_parse_r(path)
}


resolve_bundle_dir <- function(path, dir = NULL) {
  if (!is.null(dir) && nzchar(trim_ws(dir))) {
    return(dir)
  }

  base <- tools::file_path_sans_ext(basename(path))
  file.path(dirname(path), paste0(base, "-ghostwriteR"))
}


report_extension <- function(format) {
  if (identical(format, "markdown")) {
    return("md")
  }

  if (identical(format, "text")) {
    return("txt")
  }

  "docx"
}


resolve_report_path <- function(path, format, out = NULL) {
  if (!is.null(out) && nzchar(trim_ws(out))) {
    return(out)
  }

  base <- tools::file_path_sans_ext(basename(path))
  file.path(dirname(path), paste0(base, "-ghostwriteR-report.", report_extension(format)))
}


resolve_output_path <- function(path, format, out = NULL) {
  if (!is.null(out) && nzchar(trim_ws(out))) {
    return(out)
  }

  base <- tools::file_path_sans_ext(basename(path))
  extension <- if (identical(format, "html_graph")) "html" else format
  file.path(dirname(path), paste0(base, "-ghostwriteR.", extension))
}


render_graph_file <- function(graph, out, format, parsed = NULL) {
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)

  if (identical(format, "html")) {
    write_self_contained_html(parsed, graph, out)
    return(invisible(out))
  }

  if (identical(format, "html_graph")) {
    write_graph_only_html(parsed, graph, out)
    return(invisible(out))
  }

  svg_text <- export_graph_svg(graph)

  if (identical(format, "svg")) {
    writeLines(svg_text, con = out, useBytes = TRUE)
    return(invisible(out))
  }

  if (!requireNamespace("rsvg", quietly = TRUE)) {
    stop("Package 'rsvg' is required for PNG and PDF export.")
  }

  svg_raw <- charToRaw(svg_text)

  if (identical(format, "png")) {
    rsvg::rsvg_png(svg_raw, file = out)
    return(invisible(out))
  }

  if (identical(format, "pdf")) {
    rsvg::rsvg_pdf(svg_raw, file = out)
    return(invisible(out))
  }

  stop("Unsupported export format: ", format)
}


export_graph_svg <- function(graph) {
  if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) {
    stop("Package 'DiagrammeRsvg' is required for file export.")
  }

  DiagrammeRsvg::export_svg(graph)
}
