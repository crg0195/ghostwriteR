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
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("Package 'DiagrammeR' is required.")
  }

  if (is.list(x) && all(c("nodes", "edges") %in% names(x))) {
    nodes <- x$nodes
    edges <- x$edges
    script_path <- x$script_path
  } else if (is.data.frame(x)) {
    edges <- x
    nodes <- attr(x, "nodes")
    script_path <- attr(x, "script_path")
  } else {
    stop("`x` must be a parsed ghostwriteR object or an edge data frame.")
  }

  if (is.null(nodes) || nrow(nodes) == 0) {
    return(DiagrammeR::grViz("digraph { }"))
  }

  graph_label <- "GhostwriteR Process Map"
  if (!is.null(script_path) && nzchar(script_path)) {
    graph_label <- paste0(graph_label, "\\n", basename(script_path))
  }

  node_lines <- apply(nodes, 1, function(node) {
    style <- node_style(node[["kind"]])
    paste0(
      node[["id"]],
      ' [label="', escape_dot(node[["label"]]),
      '", shape="', style$shape,
      '", fillcolor="', style$fillcolor,
      '", color="', style$color,
      '", fontcolor="', style$fontcolor,
      '", style="', style$style,
      '", penwidth="', style$penwidth,
      '"]'
    )
  })

  edge_lines <- apply(edges, 1, function(edge) {
    style <- edge_style(edge[["type"]])
    paste0(
      edge[["from"]],
      " -> ",
      edge[["to"]],
      ' [color="', style$color,
      '", penwidth="', style$penwidth,
      '", arrowsize="0.8"]'
    )
  })

  dot <- paste(
    "digraph {",
    'graph [rankdir=TB, bgcolor="#fbf7ef", pad="0.35", nodesep="0.55", ranksep="0.9", labelloc="t", labeljust="l", label="',
    graph_label,
    '", fontname="Helvetica", fontsize="22"];',
    'node [fontname="Helvetica", fontsize="11", margin="0.16,0.10"];',
    'edge [fontname="Helvetica", fontsize="9"];',
    paste(node_lines, collapse = "\n"),
    legend_dot(nodes),
    paste(edge_lines, collapse = "\n"),
    "}",
    sep = "\n"
  )

  DiagrammeR::grViz(dot)
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

ghostwriter_detect_language <- function(path) {
  extension <- tolower(tools::file_ext(path))
  if (extension %in% c("sql")) {
    return("sql")
  }

  preview_lines <- readLines(path, warn = FALSE, n = 20L)
  preview_lines <- preview_lines[nzchar(trim_ws(preview_lines))]
  if (length(preview_lines) == 0L) {
    return("r")
  }

  first_line <- tolower(trim_ws(preview_lines[[1]]))
  if (grepl("^(with|select|create|insert|copy|unload)\\b", first_line, perl = TRUE)) {
    return("sql")
  }

  "r"
}

ghostwriter_parse_r <- function(path) {
  if (!file.exists(path)) {
    stop("Script not found: ", path)
  }

  exprs <- parse(file = path, keep.source = TRUE)
  srcrefs <- attr(exprs, "srcref")

  input_funs <- c(
    "read.csv", "readRDS", "readr::read_csv", "readr::read_tsv",
    "readr::read_delim", "readxl::read_excel", "data.table::fread",
    "DBI::dbGetQuery"
  )

  output_funs <- c(
    "write.csv", "saveRDS", "readr::write_csv", "readr::write_tsv",
    "readr::write_delim", "writexl::write_xlsx", "openxlsx::write.xlsx",
    "ggplot2::ggsave", "pdf", "png", "jpeg", "tiff", "svg",
    "svglite::svglite", "grDevices::pdf", "grDevices::png"
  )

  input_funs_base <- sub("^[^:]+::", "", input_funs)
  output_funs_base <- sub("^[^:]+::", "", output_funs)

  nodes <- data.frame(
    id = character(),
    key = character(),
    label = character(),
    kind = character(),
    stringsAsFactors = FALSE
  )

  edges <- data.frame(
    from = character(),
    to = character(),
    type = character(),
    stringsAsFactors = FALSE
  )

  steps <- data.frame(
    step = integer(),
    kind = character(),
    group = character(),
    title = character(),
    detail = character(),
    source = character(),
    output = character(),
    target_path = character(),
    input_columns = character(),
    output_columns = character(),
    carried_columns = character(),
    created_columns = character(),
    narrative = character(),
    stringsAsFactors = FALSE
  )

  node_index <- 0L
  step_index <- 0L
  node_keys <- character()
  node_ids <- character()

  node_kind_rank <- function(kind) {
    switch(
      kind,
      generic_object = 0L,
      dataset = 1L,
      data_table = 1L,
      data_frame = 1L,
      matrix_object = 2L,
      vector_object = 2L,
      model_object = 2L,
      plot_object = 2L,
      input_file = 3L,
      output_file = 3L,
      source_table = 3L,
      output_table = 3L,
      input_step = 4L,
      transform_step = 4L,
      output_step = 4L,
      0L
    )
  }

  add_node <- function(key, label, kind) {
    match_index <- match(key, node_keys)
    if (!is.na(match_index)) {
      current_kind <- nodes$kind[[match_index]]
      if (node_kind_rank(kind) >= node_kind_rank(current_kind)) {
        nodes$label[[match_index]] <<- label
        nodes$kind[[match_index]] <<- kind
      }
      return(node_ids[[match_index]])
    }

    node_index <<- node_index + 1L
    id <- paste0("n", node_index)

    nodes <<- rbind(
      nodes,
      data.frame(id = id, key = key, label = label, kind = kind, stringsAsFactors = FALSE)
    )

    node_keys <<- c(node_keys, key)
    node_ids <<- c(node_ids, id)
    id
  }

  add_edge <- function(from, to, type) {
    if (is.null(from) || is.null(to)) {
      return(invisible(NULL))
    }

    edges <<- rbind(
      edges,
      data.frame(from = from, to = to, type = type, stringsAsFactors = FALSE)
    )
  }

  add_step_record <- function(kind, title, detail = "", source = "", output = "", target_path = "", group = "", input_columns = "", output_columns = "", carried_columns = "", created_columns = "") {
    step_index <<- step_index + 1L
    narrative <- build_step_narrative(kind, title, detail, source, output, target_path)

    steps <<- rbind(
      steps,
      data.frame(
        step = step_index,
        kind = kind,
        group = group,
        title = title,
        detail = detail,
        source = source,
        output = output,
        target_path = target_path,
        input_columns = input_columns,
        output_columns = output_columns,
        carried_columns = carried_columns,
        created_columns = created_columns,
        narrative = narrative,
        stringsAsFactors = FALSE
      )
    )

    add_node(
      paste0("step::", step_index),
      multiline_label(title, detail),
      paste0(kind, "_step")
    )
  }

  object_node <- function(name, kind = "generic_object") {
    add_node(
      paste0("object::", name),
      multiline_label(object_kind_label(kind, capitalize = TRUE), quoted_name(name)),
      kind
    )
  }

  file_node <- function(path_value, kind) {
    role_title <- if (identical(kind, "input_file")) "Source data file" else "Output file"
    detail <- pretty_path(path_value)
    add_node(
      paste0(kind, "::", path_value),
      multiline_label(role_title, detail),
      kind
    )
  }

  source_from_expr <- function(text) {
    text <- trim_ws(text)

    if (!nzchar(text)) {
      return(list(node_id = NULL, source_label = ""))
    }

    if (is_plain_name(text)) {
      return(list(node_id = object_node(text), source_label = text))
    }

    fn <- leading_call(text)
    if (is_known_call(fn, input_funs, input_funs_base)) {
      info <- describe_input(text)
      step_id <- add_step_record(
        kind = "input",
        title = info$title,
        detail = info$detail,
        source = info$path %||% "",
        output = "",
        target_path = info$path %||% ""
      )
      if (!is.null(info$path)) {
        add_edge(file_node(info$path, "input_file"), step_id, "reads")
      }
      return(list(node_id = step_id, source_label = info$title))
    }

    if (!is.null(fn)) {
      info <- describe_transform(text, source_in_call = TRUE)
      step_id <- add_step_record(
        kind = "transform",
        title = info$title,
        detail = info$detail,
        source = info$source,
        output = ""
      )
      return(list(node_id = step_id, source_label = info$title))
    }

    list(node_id = NULL, source_label = "")
  }

  for (index in seq_along(exprs)) {
    statement <- expression_text(exprs[[index]], srcrefs[[index]])
    if (!nzchar(statement)) {
      next
    }

    left_assign <- regexec("^([[:alnum:]_.]+)\\s*(<-|=)\\s*(.+)$", statement)
    left_hit <- regmatches(statement, left_assign)[[1]]

    right_assign <- regexec("^(.+)\\s*(->|->>)\\s*([[:alnum:]_.]+)$", statement)
    right_hit <- regmatches(statement, right_assign)[[1]]

    if (length(left_hit) > 0) {
      lhs_name <- trim_ws(left_hit[2])
      rhs <- trim_ws(left_hit[4])
      lhs_id <- object_node(lhs_name)

      pipe_parts <- split_pipe(rhs)
      if (length(pipe_parts) > 1) {
        source_info <- source_from_expr(pipe_parts[[1]])
        current_id <- source_info$node_id
        current_source <- if (nzchar(source_info$source_label)) source_info$source_label else label_from_expr(pipe_parts[[1]])
        tail_parts <- pipe_parts[-1]

        for (part_index in seq_along(tail_parts)) {
          part <- tail_parts[[part_index]]
          info <- describe_transform(part, source_in_call = FALSE)
          output_name <- if (part_index == length(tail_parts)) lhs_name else ""
          if (nzchar(output_name)) {
            lhs_id <- object_node(lhs_name, info$object_kind)
          }
          step_id <- add_step_record(
            kind = "transform",
            title = info$title,
            detail = info$detail,
            source = current_source,
            output = output_name
          )
          add_edge(current_id, step_id, "flows")
          current_id <- step_id
          current_source <- if (nzchar(output_name)) output_name else "the result from the previous step"
        }

        add_edge(current_id, lhs_id, "creates")
        next
      }

      fn <- leading_call(rhs)
      if (is_known_call(fn, input_funs, input_funs_base)) {
        info <- describe_input(rhs)
        lhs_id <- object_node(lhs_name, info$object_kind)
        step_id <- add_step_record(
          kind = "input",
          title = info$title,
          detail = info$detail,
          source = info$path %||% "",
          output = lhs_name,
          target_path = info$path %||% ""
        )
        if (!is.null(info$path)) {
          add_edge(file_node(info$path, "input_file"), step_id, "reads")
        }
        add_edge(step_id, lhs_id, "creates")
        next
      }

      if (!is.null(fn)) {
        info <- describe_transform(rhs, source_in_call = TRUE)
        source_label <- if (nzchar(info$source)) info$source else first_plain_arg(rhs)
        lhs_id <- object_node(lhs_name, info$object_kind)
        source_id <- if (nzchar(source_label)) object_node(source_label) else NULL
        step_id <- add_step_record(
          kind = "transform",
          title = info$title,
          detail = info$detail,
          source = source_label,
          output = lhs_name
        )
        add_edge(source_id, step_id, "flows")
        add_edge(step_id, lhs_id, "creates")
        next
      }

      if (is_plain_name(rhs)) {
        add_edge(object_node(rhs), lhs_id, "copies")
      }

      next
    }

    if (length(right_hit) > 0) {
      rhs <- trim_ws(right_hit[2])
      lhs_name <- trim_ws(right_hit[4])
      lhs_id <- object_node(lhs_name)
      source_info <- source_from_expr(rhs)
      add_edge(source_info$node_id, lhs_id, "creates")
      next
    }

    fn <- leading_call(statement)
    if (is_known_call(fn, output_funs, output_funs_base)) {
      info <- describe_output(statement)
      source_expr <- info$source_expr %||% ""
      source_label <- if (nzchar(source_expr)) label_from_expr(source_expr) else ""
      source_info <- if (nzchar(source_expr)) source_from_expr(source_expr) else list(node_id = NULL, source_label = "")
      step_id <- add_step_record(
        kind = "output",
        title = info$title,
        detail = info$detail,
        source = source_label,
        output = info$path %||% "",
        target_path = info$path %||% ""
      )
      add_edge(source_info$node_id, step_id, "writes")
      if (!is.null(info$path)) {
        add_edge(step_id, file_node(info$path, "output_file"), "writes")
      }
    }
  }

  edges <- unique(edges)
  list(nodes = nodes, edges = edges, steps = steps, script_path = path, language = "r")
}

ghostwriter_parse_sql <- function(path) {
  sql_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  statements <- sql_statement_list(sql_text)

  nodes <- data.frame(
    id = character(),
    key = character(),
    label = character(),
    kind = character(),
    stringsAsFactors = FALSE
  )

  edges <- data.frame(
    from = character(),
    to = character(),
    type = character(),
    stringsAsFactors = FALSE
  )

  steps <- data.frame(
    step = integer(),
    kind = character(),
    group = character(),
    title = character(),
    detail = character(),
    source = character(),
    output = character(),
    target_path = character(),
    input_columns = character(),
    output_columns = character(),
    carried_columns = character(),
    created_columns = character(),
    narrative = character(),
    stringsAsFactors = FALSE
  )

  node_index <- 0L
  step_index <- 0L
  node_keys <- character()
  node_ids <- character()
  source_step_keys <- character()
  source_step_ids <- character()
  result_keys <- character()
  result_ids <- character()
  result_is_summary <- logical()
  result_group_details <- character()
  result_output_columns <- character()
  result_carried_columns <- character()
  result_created_columns <- character()

  add_node <- function(key, label, kind) {
    match_index <- match(key, node_keys)
    if (!is.na(match_index)) {
      return(node_ids[[match_index]])
    }

    node_index <<- node_index + 1L
    id <- paste0("n", node_index)

    nodes <<- rbind(
      nodes,
      data.frame(id = id, key = key, label = label, kind = kind, stringsAsFactors = FALSE)
    )

    node_keys <<- c(node_keys, key)
    node_ids <<- c(node_ids, id)
    id
  }

  add_edge <- function(from, to, type) {
    if (is.null(from) || is.null(to)) {
      return(invisible(NULL))
    }

    edges <<- rbind(
      edges,
      data.frame(from = from, to = to, type = type, stringsAsFactors = FALSE)
    )
  }

  connect_many <- function(from_ids, to_id, type) {
    if (length(from_ids) == 0L || is.null(to_id)) {
      return(invisible(NULL))
    }

    for (from_id in unique(from_ids)) {
      if (!is.null(from_id) && nzchar(from_id)) {
        add_edge(from_id, to_id, type)
      }
    }
  }

  add_step_record <- function(kind, title, detail = "", source = "", output = "", target_path = "", group = "", input_columns = "", output_columns = "", carried_columns = "", created_columns = "") {
    step_index <<- step_index + 1L
    narrative <- build_step_narrative(kind, title, detail, source, output, target_path)

    steps <<- rbind(
      steps,
      data.frame(
        step = step_index,
        kind = kind,
        group = group,
        title = title,
        detail = detail,
        source = source,
        output = output,
        target_path = target_path,
        input_columns = input_columns,
        output_columns = output_columns,
        carried_columns = carried_columns,
        created_columns = created_columns,
        narrative = narrative,
        stringsAsFactors = FALSE
      )
    )

    add_node(
      paste0("step::", step_index),
      multiline_label(title, detail),
      paste0(kind, "_step")
    )
  }

  table_node <- function(name, kind = c("dataset", "source_table", "output_table")) {
    kind <- match.arg(kind)
    role_title <- switch(
      kind,
      source_table = "Source table",
      output_table = "Output table",
      "Data table"
    )

    add_node(
      paste0(kind, "::", name),
      multiline_label(role_title, quoted_name(name)),
      kind
    )
  }

  file_node <- function(path_value, kind) {
    role_title <- if (identical(kind, "input_file")) "Source data file" else "Output file"
    detail <- pretty_path(path_value)
    add_node(
      paste0(kind, "::", path_value),
      multiline_label(role_title, detail),
      kind
    )
  }

  ensure_source_step <- function(name, group = "") {
    name <- sql_clean_identifier(name)
    match_index <- match(name, source_step_keys)
    if (!is.na(match_index)) {
      return(source_step_ids[[match_index]])
    }

    step_id <- add_step_record(
      kind = "input",
      title = "Use source table",
      detail = name,
      group = group
    )
    add_edge(table_node(name, "source_table"), step_id, "reads")
    source_step_keys <<- c(source_step_keys, name)
    source_step_ids <<- c(source_step_ids, step_id)
    step_id
  }

  remember_result <- function(name, node_id, is_summary = FALSE, group_detail = "", output_columns = "", carried_columns = "", created_columns = "") {
    name <- sql_clean_identifier(name)
    match_index <- match(name, result_keys)
    if (!is.na(match_index)) {
      result_ids[[match_index]] <<- node_id
      result_is_summary[[match_index]] <<- isTRUE(is_summary)
      result_group_details[[match_index]] <<- group_detail
      result_output_columns[[match_index]] <<- output_columns
      result_carried_columns[[match_index]] <<- carried_columns
      result_created_columns[[match_index]] <<- created_columns
      return(invisible(node_id))
    }

    result_keys <<- c(result_keys, name)
    result_ids <<- c(result_ids, node_id)
    result_is_summary <<- c(result_is_summary, isTRUE(is_summary))
    result_group_details <<- c(result_group_details, group_detail)
    result_output_columns <<- c(result_output_columns, output_columns)
    result_carried_columns <<- c(result_carried_columns, carried_columns)
    result_created_columns <<- c(result_created_columns, created_columns)
    invisible(node_id)
  }

  result_metadata <- function(name) {
    name <- sql_clean_identifier(name)
    match_index <- match(name, result_keys)
    if (is.na(match_index)) {
      return(list(node_id = NULL, is_summary = FALSE, group_detail = "", output_columns = "", carried_columns = "", created_columns = ""))
    }

    list(
      node_id = result_ids[[match_index]],
      is_summary = isTRUE(result_is_summary[[match_index]]),
      group_detail = result_group_details[[match_index]],
      output_columns = result_output_columns[[match_index]],
      carried_columns = result_carried_columns[[match_index]],
      created_columns = result_created_columns[[match_index]]
    )
  }

  source_node_id <- function(name, group = "") {
    metadata <- result_metadata(name)
    if (!is.null(metadata$node_id)) {
      return(metadata$node_id)
    }

    ensure_source_step(name, group = group)
  }

  process_query_stage <- function(stage, output_name = "", output_kind = "dataset", group = "") {
    current_ids <- character()
    current_source_label <- ""
    source_metadata <- list()
    resolved_inventory <- sql_resolve_query_inventory(stage, source_metadata)

    if (length(stage$sources) > 0L) {
      source_metadata <- lapply(stage$sources, result_metadata)
      resolved_inventory <- sql_resolve_query_inventory(stage, source_metadata)
      current_ids <- unique(vapply(stage$sources, function(source_name) source_node_id(source_name, group = group), character(1)))
      current_source_label <- paste(stage$sources, collapse = ", ")
    }

    if (length(stage$query_steps) > 0L) {
      for (index in seq_along(stage$query_steps)) {
        step <- stage$query_steps[[index]]
        if (identical(step$title, "Filter rows") && any(vapply(source_metadata, function(x) isTRUE(x$is_summary), logical(1)))) {
          summary_sources <- source_metadata[vapply(source_metadata, function(x) isTRUE(x$is_summary), logical(1))]
          step$title <- "Filter grouped results"
          step$detail <- sql_group_filter_detail(step$detail, summary_sources[[1]]$group_detail)
        }

        step_output <- if (nzchar(output_name) && identical(index, length(stage$query_steps))) output_name else ""
        step_id <- add_step_record(
          kind = "transform",
          title = step$title,
          detail = step$detail,
          source = current_source_label,
          output = step_output,
          group = group,
          input_columns = step$input_columns %||% "",
          output_columns = if (nzchar(step_output)) resolved_inventory$output_columns else step$output_columns %||% "",
          carried_columns = if (nzchar(step_output)) resolved_inventory$carried_columns else step$carried_columns %||% "",
          created_columns = if (nzchar(step_output)) resolved_inventory$created_columns else step$created_columns %||% ""
        )
        connect_many(current_ids, step_id, "flows")
        current_ids <- step_id
        current_source_label <- if (nzchar(step_output)) step_output else "the result from the previous step"
      }
    }

    if (nzchar(output_name)) {
      node_id <- table_node(output_name, output_kind)
      if (length(stage$query_steps) == 0L) {
        step_id <- add_step_record(
          kind = "transform",
          title = "Run SQL query",
          detail = stage$query_detail,
          source = current_source_label,
          output = output_name,
          group = group,
          input_columns = stage$query_input_columns %||% "",
          output_columns = resolved_inventory$output_columns,
          carried_columns = resolved_inventory$carried_columns,
          created_columns = resolved_inventory$created_columns
        )
        connect_many(current_ids, step_id, "flows")
        current_ids <- step_id
      }

      connect_many(current_ids, node_id, "creates")
      summary_steps <- stage$query_steps[vapply(stage$query_steps, function(x) identical(x$title, "Summarize data"), logical(1))]
      summary_detail <- if (length(summary_steps) > 0L) summary_steps[[length(summary_steps)]]$detail else ""
      remember_result(
        output_name,
        node_id,
        is_summary = length(summary_steps) > 0L,
        group_detail = summary_detail,
        output_columns = resolved_inventory$output_columns,
        carried_columns = resolved_inventory$carried_columns,
        created_columns = resolved_inventory$created_columns
      )

      return(list(
        node_ids = node_id,
        source_label = output_name,
        output_columns = resolved_inventory$output_columns,
        carried_columns = resolved_inventory$carried_columns,
        created_columns = resolved_inventory$created_columns
      ))
    }

    if (length(stage$query_steps) == 0L && length(current_ids) > 0L) {
      step_id <- add_step_record(
        kind = "transform",
        title = "Run SQL query",
        detail = stage$query_detail,
        source = current_source_label,
        group = group,
        input_columns = stage$query_input_columns %||% "",
        output_columns = resolved_inventory$output_columns,
        carried_columns = resolved_inventory$carried_columns,
        created_columns = resolved_inventory$created_columns
      )
      connect_many(current_ids, step_id, "flows")
      current_ids <- step_id
      current_source_label <- "the result from the previous step"
    }

    list(
      node_ids = current_ids,
      source_label = current_source_label,
      output_columns = resolved_inventory$output_columns,
      carried_columns = resolved_inventory$carried_columns,
      created_columns = resolved_inventory$created_columns
    )
  }

  for (statement in statements) {
    info <- sql_describe_statement(statement)
    current_ids <- character()
    current_source_label <- ""
    current_output_columns <- ""
    current_carried_columns <- ""
    current_created_columns <- ""

    if (length(info$stages) > 0L) {
      for (stage_index in seq_along(info$stages)) {
        stage <- info$stages[[stage_index]]
        stage_group <- sql_stage_group_label(stage$name, final = identical(stage_index, length(info$stages)))
        stage_result <- process_query_stage(
          stage,
          output_name = stage$name,
          output_kind = "dataset",
          group = stage_group
        )
        current_ids <- stage_result$node_ids
        current_source_label <- stage_result$source_label
        current_output_columns <- stage_result$output_columns %||% ""
        current_carried_columns <- stage_result$carried_columns %||% ""
        current_created_columns <- stage_result$created_columns %||% ""
      }
    } else if (length(info$sources) > 0L) {
      final_group <- sql_stage_group_label("", final = TRUE)
      current_ids <- unique(vapply(info$sources, function(source_name) source_node_id(source_name, group = final_group), character(1)))
      current_source_label <- paste(info$sources, collapse = ", ")
    }

    if (nzchar(info$output_title)) {
      output_source <- if (nzchar(current_source_label)) current_source_label else paste(info$sources, collapse = ", ")
      output_group <- if (nzchar(info$output_path)) "Export" else sql_stage_group_label("", final = TRUE)
      source_inventory <- character()
      if (length(info$sources) > 0L) {
        source_inventory <- vapply(info$sources, function(source_name) {
          result_metadata(source_name)$output_columns
        }, character(1))
      }
      source_inventory <- source_inventory[nzchar(source_inventory)]
      output_inventory <- if (nzchar(current_output_columns)) {
        current_output_columns
      } else if (nzchar(info$query_output_columns %||% "")) {
        info$query_output_columns
      } else if (length(source_inventory) > 0L) {
        source_inventory[[1]]
      } else {
        info$query_output_columns %||% ""
      }
      carried_inventory <- if (nzchar(current_carried_columns)) current_carried_columns else if (length(info$sources) > 0L) {
        source_carried <- vapply(info$sources, function(source_name) {
          result_metadata(source_name)$carried_columns
        }, character(1))
        paste(sql_unique_trimmed(unlist(strsplit(source_carried[nzchar(source_carried)], "\\s*,\\s*", perl = TRUE), use.names = FALSE)), collapse = ", ")
      } else {
        ""
      }
      created_inventory <- if (nzchar(current_created_columns)) current_created_columns else if (length(info$sources) > 0L) {
        source_created <- vapply(info$sources, function(source_name) {
          result_metadata(source_name)$created_columns
        }, character(1))
        paste(sql_unique_trimmed(unlist(strsplit(source_created[nzchar(source_created)], "\\s*,\\s*", perl = TRUE), use.names = FALSE)), collapse = ", ")
      } else {
        ""
      }
      step_id <- add_step_record(
        kind = "output",
        title = info$output_title,
        detail = info$output_detail,
        source = output_source,
        output = info$output_name,
        target_path = info$output_path,
        group = output_group,
        input_columns = output_inventory,
        output_columns = output_inventory,
        carried_columns = carried_inventory,
        created_columns = created_inventory
      )

      connect_many(current_ids, step_id, if (nzchar(info$output_path)) "writes" else "creates")

      if (nzchar(info$output_name)) {
        target_kind <- if (isTRUE(info$output_is_temporary)) "dataset" else "output_table"
        target_id <- table_node(info$output_name, target_kind)
        add_edge(step_id, target_id, "creates")
        remember_result(
          info$output_name,
          target_id,
          output_columns = output_inventory,
          carried_columns = carried_inventory,
          created_columns = created_inventory
        )
      }

      if (nzchar(info$output_path)) {
        add_edge(step_id, file_node(info$output_path, "output_file"), "writes")
      }

      next
    }

    if (length(info$query_steps) == 0L && length(current_ids) > 0L) {
      step_id <- add_step_record(
        kind = "transform",
        title = "Run SQL query",
        detail = info$query_detail,
        source = current_source_label,
        group = sql_stage_group_label("", final = TRUE),
        input_columns = info$query_input_columns %||% "",
        output_columns = current_output_columns %||% info$query_output_columns %||% "",
        carried_columns = current_carried_columns,
        created_columns = current_created_columns
      )
      connect_many(current_ids, step_id, "flows")
    }
  }

  edges <- unique(edges)
  list(nodes = nodes, edges = edges, steps = steps, script_path = path, language = "sql")
}

build_script_report <- function(parsed, format = c("markdown", "text")) {
  format <- match.arg(format)
  steps <- parsed$steps
  script_name <- basename(parsed$script_path)
  has_inventories <- workflow_has_inventories(steps)

  if (identical(format, "text")) {
    lines <- c(
      paste0("GhostwriteR Report: ", script_name),
      "",
      "How to read the diagram:",
      "- Yellow note or cylinder: external source such as a file or database table",
      "- Blue cylinder: intermediate data table used in the workflow",
      "- Green box: transformation step",
      "- Peach box, cylinder, or note: final output step, output table, or output file",
      "",
      "Workflow:"
    )

    if (nrow(steps) == 0) {
      lines <- c(lines, "No workflow steps were detected.")
    } else {
      for (index in seq_len(nrow(steps))) {
        lines <- c(lines, paste0(index, ". ", steps$narrative[[index]]))
        lines <- c(lines, step_inventory_lines(steps[index, , drop = FALSE], prefix = "   "))
      }
    }

    if (isTRUE(has_inventories)) {
      lines <- c(lines, "", "Column inventories:")
      lines <- c(lines, build_inventory_report_lines(steps, nodes = parsed$nodes, format = "text"))
    }

    return(paste(lines, collapse = "\n"))
  }

  lines <- c(
    paste0("# GhostwriteR Report: ", script_name),
    "",
    "## How To Read The Diagram",
    "- Yellow note or cylinder: external source such as a file or database table.",
    "- Blue cylinder: intermediate data table created or reused during processing.",
    "- Green rounded box: transformation or calculation step.",
    "- Peach rounded box, cylinder, or note: final output step, output table, or saved file.",
    "",
    "## Workflow"
  )

  if (nrow(steps) == 0) {
    lines <- c(lines, "No workflow steps were detected.")
  } else {
    for (index in seq_len(nrow(steps))) {
      lines <- c(lines, paste0(index, ". ", steps$narrative[[index]]))
      lines <- c(lines, step_inventory_lines(steps[index, , drop = FALSE], prefix = "   "))
    }
  }

  if (isTRUE(has_inventories)) {
    lines <- c(lines, "", "## Column Inventories")
    lines <- c(lines, build_inventory_report_lines(steps, nodes = parsed$nodes, format = "markdown"))
  }

  paste(lines, collapse = "\n")
}

step_inventory_lines <- function(step_row, prefix = "") {
  step <- unclass(step_row[1, , drop = TRUE])
  lines <- character()

  if (nzchar(step$input_columns)) {
    lines <- c(lines, paste0(prefix, "Input columns used: ", step$input_columns))
  }

  if (nzchar(step$output_columns)) {
    lines <- c(lines, paste0(prefix, "Output columns produced: ", step$output_columns))
  }

  if ("carried_columns" %in% names(step_row) && nzchar(step$carried_columns)) {
    lines <- c(lines, paste0(prefix, "Columns carried through: ", step$carried_columns))
  }

  if ("created_columns" %in% names(step_row) && nzchar(step$created_columns)) {
    lines <- c(lines, paste0(prefix, "Columns newly created or changed: ", step$created_columns))
  }

  lines
}

workflow_has_inventories <- function(steps) {
  if (nrow(steps) == 0L) {
    return(FALSE)
  }

  any(nzchar(steps$input_columns) | nzchar(steps$output_columns))
}

inventory_step_indices <- function(steps) {
  if (nrow(steps) == 0L) {
    return(integer())
  }

  which(nzchar(steps$input_columns) | nzchar(steps$output_columns))
}

named_inventory_indices <- function(steps) {
  indices <- inventory_step_indices(steps)
  if (length(indices) == 0L) {
    return(integer())
  }

  named <- indices[nzchar(steps$output[indices]) | nzchar(steps$target_path[indices])]
  if (length(named) == 0L) {
    return(integer())
  }

  keys <- vapply(named, function(i) {
    step <- unclass(steps[i, , drop = FALSE][1, , drop = TRUE])
    if (nzchar(step$target_path)) {
      return(paste0("target::", step$target_path))
    }
    paste0("output::", step$output)
  }, character(1))

  named[!duplicated(keys)]
}

inventory_subject_label <- function(step_row) {
  step <- unclass(step_row[1, , drop = TRUE])

  if (nzchar(step$target_path)) {
    return(pretty_path(step$target_path))
  }

  if (nzchar(step$output)) {
    return(quoted_name(step$output))
  }

  paste0("Step ", step$step, ": ", step$title)
}

inventory_subject_type <- function(step_row) {
  step <- unclass(step_row[1, , drop = TRUE])

  if (identical(step$kind, "output") && nzchar(step$target_path)) {
    return("Exported file")
  }

  if (identical(step$kind, "output") && nzchar(step$output)) {
    return("Final output")
  }

  if (nzchar(step$output)) {
    return("Named object")
  }

  "Workflow step"
}

inventory_count <- function(text) {
  text <- trim_ws(text)
  if (!nzchar(text)) {
    return(0L)
  }

  length(strsplit(text, "\\s*,\\s*", perl = TRUE)[[1]])
}

build_inventory_report_lines <- function(steps, nodes = NULL, format = c("markdown", "text")) {
  format <- match.arg(format)
  indices <- inventory_step_indices(steps)
  if (length(indices) == 0L) {
    return(if (identical(format, "markdown")) "No column inventories are available for this workflow yet." else "No column inventories are available for this workflow yet.")
  }

  named <- named_inventory_indices(steps)
  lines <- character()
  collection_heading <- if (is.null(nodes)) "Named objects and outputs" else inventory_collection_heading(nodes)

  if (identical(format, "markdown")) {
    if (length(named) > 0L) {
      lines <- c(lines, paste0("### ", collection_heading))
      lines <- c(lines, vapply(named, function(i) {
        step <- steps[i, , drop = FALSE]
        piece <- paste0("- **", inventory_subject_label(step), "**")
        piece <- paste0(piece, " (`", inventory_subject_type(step), "`, step ", step$step[[1]], ")")
        if (nzchar(step$input_columns[[1]])) {
          piece <- paste0(piece, "  \n  Inputs: ", step$input_columns[[1]])
        }
        if (nzchar(step$output_columns[[1]])) {
          piece <- paste0(piece, "  \n  Outputs: ", step$output_columns[[1]])
        }
        if ("carried_columns" %in% names(step) && nzchar(step$carried_columns[[1]])) {
          piece <- paste0(piece, "  \n  Carried through: ", step$carried_columns[[1]])
        }
        if ("created_columns" %in% names(step) && nzchar(step$created_columns[[1]])) {
          piece <- paste0(piece, "  \n  Newly created or changed: ", step$created_columns[[1]])
        }
        piece
      }, character(1)))
      lines <- c(lines, "")
    }

    lines <- c(lines, "### Step-By-Step Inventory")
    lines <- c(lines, vapply(indices, function(i) {
      step <- steps[i, , drop = FALSE]
      piece <- paste0("- **Step ", step$step[[1]], ": ", step$title[[1]], "**")
      piece <- paste0(piece, " (`", step_phase_label(step), "`)")
      if (nzchar(step$input_columns[[1]])) {
        piece <- paste0(piece, "  \n  Inputs: ", step$input_columns[[1]])
      }
      if (nzchar(step$output_columns[[1]])) {
        piece <- paste0(piece, "  \n  Outputs: ", step$output_columns[[1]])
      }
      if ("carried_columns" %in% names(step) && nzchar(step$carried_columns[[1]])) {
        piece <- paste0(piece, "  \n  Carried through: ", step$carried_columns[[1]])
      }
      if ("created_columns" %in% names(step) && nzchar(step$created_columns[[1]])) {
        piece <- paste0(piece, "  \n  Newly created or changed: ", step$created_columns[[1]])
      }
      piece
    }, character(1)))
    return(lines)
  }

  if (length(named) > 0L) {
    lines <- c(lines, paste0(collection_heading, ":"))
    lines <- c(lines, vapply(named, function(i) {
      step <- steps[i, , drop = FALSE]
      piece <- paste0("- ", inventory_subject_label(step), " (", inventory_subject_type(step), ", step ", step$step[[1]], ")")
      if (nzchar(step$input_columns[[1]])) {
        piece <- paste0(piece, "\n  Inputs: ", step$input_columns[[1]])
      }
      if (nzchar(step$output_columns[[1]])) {
        piece <- paste0(piece, "\n  Outputs: ", step$output_columns[[1]])
      }
      if ("carried_columns" %in% names(step) && nzchar(step$carried_columns[[1]])) {
        piece <- paste0(piece, "\n  Carried through: ", step$carried_columns[[1]])
      }
      if ("created_columns" %in% names(step) && nzchar(step$created_columns[[1]])) {
        piece <- paste0(piece, "\n  Newly created or changed: ", step$created_columns[[1]])
      }
      piece
    }, character(1)))
    lines <- c(lines, "")
  }

  lines <- c(lines, "Step-by-step inventory:")
  lines <- c(lines, vapply(indices, function(i) {
    step <- steps[i, , drop = FALSE]
    piece <- paste0("- Step ", step$step[[1]], ": ", step$title[[1]], " (", step_phase_label(step), ")")
    if (nzchar(step$input_columns[[1]])) {
      piece <- paste0(piece, "\n  Inputs: ", step$input_columns[[1]])
    }
    if (nzchar(step$output_columns[[1]])) {
      piece <- paste0(piece, "\n  Outputs: ", step$output_columns[[1]])
    }
    if ("carried_columns" %in% names(step) && nzchar(step$carried_columns[[1]])) {
      piece <- paste0(piece, "\n  Carried through: ", step$carried_columns[[1]])
    }
    if ("created_columns" %in% names(step) && nzchar(step$created_columns[[1]])) {
      piece <- paste0(piece, "\n  Newly created or changed: ", step$created_columns[[1]])
    }
    piece
  }, character(1)))

  lines
}

build_step_narrative <- function(kind, title, detail = "", source = "", output = "", target_path = "") {
  detail_clause <- if (nzchar(detail)) paste0(": ", detail) else ""

  if (identical(kind, "input")) {
    output_clause <- if (nzchar(output)) paste0(" to create data table `", output, "`") else ""
    return(paste0(title, detail_clause, output_clause, "."))
  }

  if (identical(kind, "transform")) {
    source_clause <- if (nzchar(source)) paste0("Using `", source, "`, ") else ""
    output_clause <- if (nzchar(output)) paste0(". This creates `", output, "`") else ""
    return(paste0(source_clause, tolower_first(title), detail_clause, output_clause, "."))
  }

  if (identical(kind, "output")) {
    if (grepl("^Create (table|view)$", title, perl = TRUE) || identical(title, "Insert query results into table")) {
      source_clause <- if (nzchar(source)) paste0("Use `", source, "` to ") else ""
      object_clause <- if (nzchar(detail)) paste0(" `", detail, "`") else ""
      return(paste0(source_clause, tolower_first(title), object_clause, "."))
    }

    if (identical(title, "Return query results")) {
      source_clause <- if (nzchar(source)) paste0("Return query results from `", source, "`") else title
      detail_text <- if (nzchar(detail)) paste0(": ", detail) else ""
      return(paste0(source_clause, detail_text, "."))
    }

    source_clause <- if (nzchar(source)) paste0("Write `", source, "`") else title
    target_clause <- if (nzchar(target_path)) paste0(" to `", target_path, "`") else ""
    detail_text <- if (nzchar(detail) && !nzchar(target_clause)) paste0(": ", detail) else ""
    return(paste0(source_clause, target_clause, detail_text, "."))
  }

  paste0(title, detail_clause, ".")
}

object_kind_label <- function(kind, count = 1L, capitalize = TRUE) {
  base <- switch(
    kind,
    dataset = "data table",
    data_table = "data table",
    data_frame = "data frame",
    model_object = "model",
    plot_object = "chart object",
    matrix_object = "matrix",
    vector_object = "vector",
    generic_object = "named object",
    "named object"
  )

  text <- if (identical(base, "matrix")) {
    if (count == 1L) "matrix" else "matrices"
  } else {
    paste0(base, plural_suffix(count))
  }

  if (isTRUE(capitalize)) {
    paste0(toupper(substr(text, 1L, 1L)), substr(text, 2L, nchar(text)))
  } else {
    text
  }
}

named_object_kind_counts <- function(nodes) {
  object_kinds <- c("dataset", "data_table", "data_frame", "model_object", "plot_object", "matrix_object", "vector_object", "generic_object")
  counts <- stats::setNames(integer(length(object_kinds)), object_kinds)

  if (!("kind" %in% names(nodes)) || nrow(nodes) == 0L) {
    return(counts)
  }

  present <- table(nodes$kind[nodes$kind %in% object_kinds])
  counts[names(present)] <- as.integer(present)
  counts
}

primary_named_object_kind <- function(object_counts) {
  nonzero <- names(object_counts)[object_counts > 0L]
  if (length(nonzero) == 1L) nonzero[[1]] else "generic_object"
}

named_object_stat_label <- function(object_counts) {
  nonzero <- names(object_counts)[object_counts > 0L]
  if (length(nonzero) == 1L) {
    return(object_kind_label(nonzero[[1]], count = 2L, capitalize = TRUE))
  }
  "Named objects"
}

named_object_breakdown <- function(object_counts) {
  nonzero <- names(object_counts)[object_counts > 0L]
  if (length(nonzero) <= 1L) {
    return("")
  }

  paste(vapply(nonzero, function(kind) {
    paste(object_counts[[kind]], object_kind_label(kind, object_counts[[kind]], capitalize = FALSE))
  }, character(1)), collapse = ", ")
}

inventory_collection_heading <- function(nodes) {
  nonzero <- names(named_object_kind_counts(nodes))[named_object_kind_counts(nodes) > 0L]
  if (length(nonzero) == 1L) {
    return(paste0(object_kind_label(nonzero[[1]], count = 2L, capitalize = TRUE), " and outputs"))
  }
  "Named objects and outputs"
}

legend_named_object_kinds <- function(nodes) {
  object_counts <- named_object_kind_counts(nodes)
  legend_counts <- c(
    data_table = object_counts[["dataset"]] + object_counts[["data_table"]],
    data_frame = object_counts[["data_frame"]],
    model_object = object_counts[["model_object"]],
    plot_object = object_counts[["plot_object"]],
    matrix_object = object_counts[["matrix_object"]],
    vector_object = object_counts[["vector_object"]],
    generic_object = object_counts[["generic_object"]]
  )

  kinds <- names(legend_counts)[legend_counts > 0L]
  if (length(kinds) == 0L) {
    "generic_object"
  } else {
    kinds
  }
}

legend_source_kinds <- function(nodes) {
  kinds <- c("input_file", "source_table")
  present <- kinds[kinds %in% nodes$kind]
  if (length(present) == 0L) "input_file" else unique(present)
}

legend_output_kinds <- function(nodes) {
  kinds <- c("output_table", "output_file")
  present <- kinds[kinds %in% nodes$kind]
  if (length(present) == 0L) "output_file" else unique(present)
}

legend_dot <- function(nodes) {
  source_kinds <- legend_source_kinds(nodes)
  object_kinds <- legend_named_object_kinds(nodes)
  output_kinds <- legend_output_kinds(nodes)
  source_node_ids <- paste0("legend_source_", seq_along(source_kinds))
  object_node_ids <- paste0("legend_object_", seq_along(object_kinds))
  output_node_ids <- paste0("legend_output_", seq_along(output_kinds))

  source_nodes <- vapply(seq_along(source_kinds), function(i) {
    kind <- source_kinds[[i]]
    label <- switch(
      kind,
      input_file = "Source data file",
      source_table = "Source table",
      object_kind_label(kind, count = 1L, capitalize = TRUE)
    )
    build_legend_node(source_node_ids[[i]], label, kind)
  }, character(1))

  object_nodes <- vapply(seq_along(object_kinds), function(i) {
    build_legend_node(object_node_ids[[i]], object_kind_label(object_kinds[[i]], count = 1L, capitalize = TRUE), object_kinds[[i]])
  }, character(1))

  output_nodes <- vapply(seq_along(output_kinds), function(i) {
    kind <- output_kinds[[i]]
    label <- switch(
      kind,
      output_table = "Output table",
      output_file = "Output file",
      object_kind_label(kind, count = 1L, capitalize = TRUE)
    )
    build_legend_node(output_node_ids[[i]], label, kind)
  }, character(1))

  legend_nodes <- c(
    source_nodes,
    object_nodes,
    build_legend_node("legend_transform", "Transformation", "transform_step"),
    output_nodes
  )

  legend_chain <- paste(
    c(source_node_ids, object_node_ids, "legend_transform", output_node_ids),
    collapse = " -> "
  )

  paste(
    "subgraph cluster_legend {",
    'label="Legend";',
    'labelloc="t";',
    'labeljust="l";',
    'fontsize="12";',
    'fontname="Helvetica";',
    'color="#d6cfc0";',
    'style="rounded";',
    paste(legend_nodes, collapse = "\n"),
    paste0(legend_chain, ' [style="invis"];'),
    "}",
    sep = "\n"
  )
}

build_legend_node <- function(id, label, kind) {
  style <- node_style(kind)
  paste0(
    id,
    ' [label="', escape_dot(label),
    '", shape="', style$shape,
    '", fillcolor="', style$fillcolor,
    '", color="', style$color,
    '", fontcolor="', style$fontcolor,
    '", style="', style$style,
    '", penwidth="', style$penwidth,
    '"]'
  )
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

  graph_markup <- paste0('<div class="graph-frame" id="graphFrame">', svg_text, '</div>')

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
    '        <p class="eyebrow">GhostwriteR Workflow Bundle</p>\n',
    '        <h1>', html_escape(script_name), '</h1>\n',
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
      '<div class="graph-frame graph-frame--only" id="graphOnlyFrame">',
      svg_text,
      '</div>',
      '</section>',
      '</div>'
    )
  } else {
    paste0('<div class="graph-frame graph-frame--only" id="graphOnlyFrame">', svg_text, '</div>')
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
    '.graph-frame--only svg {',
    '  width: 100% !important;',
    '  height: auto !important;',
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
    '  if (resetButton && graphFrame) {',
    '    resetButton.addEventListener("click", function() {',
    '      graphFrame.scrollTo({ top: 0, left: 0, behavior: "smooth" });',
    '    });',
    '  }',
    '  graphViewButtons.forEach(function(button) {',
    '    button.addEventListener("click", function() {',
    '      setGraphView(button.getAttribute("data-graph-view-target"));',
    '    });',
    '  });',
    '  if (graphViewButtons.length > 0) {',
    '    setGraphView("overview");',
    '  }',
    '})();'
  ), collapse = "\n")
}

workflow_stats <- function(parsed) {
  steps <- parsed$steps
  nodes <- parsed$nodes
  object_counts <- named_object_kind_counts(nodes)
  object_total <- sum(object_counts)

  list(
    inputs = sum(nodes$kind %in% c("input_file", "source_table")),
    datasets = object_total,
    object_counts = object_counts,
    object_label = named_object_stat_label(object_counts),
    object_breakdown = named_object_breakdown(object_counts),
    transforms = sum(steps$kind == "transform"),
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

  if (identical(step$kind, "input")) {
    return("Inputs")
  }

  if (identical(step$kind, "output")) {
    return("Outputs")
  }

  if (grepl("fit linear model|fit generalized linear model|train predictive model|train classification model|train model", text, perl = TRUE)) {
    return("Modeling")
  }

  if (grepl("score records with model|generate predicted values|predicted values|forecast", text, perl = TRUE)) {
    return("Scoring")
  }

  if (grepl("create chart|add chart layer|label chart|style chart", text, perl = TRUE)) {
    return("Visualization")
  }

  if (grepl("join tables|combine with", text, perl = TRUE)) {
    return("Joins")
  }

  if (grepl("group rows|summarize data|calculate summary metrics|average of|row count|number of distinct|return a regular table without grouping", text, perl = TRUE)) {
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

  "Preparation"
}

phase_rank <- function(phase) {
  order_map <- c("Inputs", "Cleaning", "Feature engineering", "Joins", "Aggregation", "Modeling", "Scoring", "Visualization", "Analysis", "Preparation", "Outputs")
  match_value <- match(phase, order_map)
  if (is.na(match_value)) 999L else match_value
}

phase_caption <- function(phase, count) {
  descriptions <- c(
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
    '" data-title="', html_escape(step$title),
    '" data-narrative="', html_escape(step$narrative),
    '" data-detail="', html_escape(step$detail),
    '" data-source="', html_escape(step$source),
    '" data-output="', html_escape(step$output),
    '" data-target="', html_escape(step$target_path),
    '" data-input-columns="', html_escape(step$input_columns),
    '" data-output-columns="', html_escape(step$output_columns),
    '" data-carried-columns="', html_escape(step$carried_columns),
    '" data-created-columns="', html_escape(step$created_columns),
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

  step <- unclass(steps[1, , drop = TRUE])
  phase <- step_phase_label(steps[1, , drop = FALSE])
  paste0(
    '<aside class="panel detail-panel">',
    '<div class="detail-sticky">',
    '<p class="section-label">Selected step details</p>',
    '<div class="detail-header">',
    '<span class="detail-chip" id="detailPhase">', html_escape(phase), '</span>',
    '<span class="detail-chip detail-chip--muted" id="detailKind">', html_escape(step$kind), '</span>',
    '</div>',
    '<h2 id="detailTitle">', html_escape(step$title), '</h2>',
    '<p class="detail-narrative" id="detailNarrative">', html_escape(step$narrative), '</p>',
    '<div class="detail-callout" id="detailCallout">', build_detail_callout_markup(step$detail), '</div>',
    '<dl class="detail-meta">',
    build_detail_meta_row('Source', step$source, 'detailSource'),
    build_detail_meta_row('Output', step$output, 'detailOutput'),
    build_detail_meta_row('Saved to', step$target_path, 'detailTarget'),
    build_detail_meta_row('Input columns used', step$input_columns, 'detailInputColumns'),
    build_detail_meta_row('Output columns produced', step$output_columns, 'detailOutputColumns'),
    build_detail_meta_row('Columns carried through', step$carried_columns, 'detailCarriedColumns'),
    build_detail_meta_row('Columns newly created or changed', step$created_columns, 'detailCreatedColumns'),
    '</dl>',
    '<p class="detail-help">Use the timeline on the left to move through the workflow. Keep the full graph for technical review only.</p>',
    '</div>',
    '</aside>'
  )
}

build_detail_callout_markup <- function(detail) {
  detail <- trim_ws(detail)
  if (!nzchar(detail)) {
    return('<p class="detail-callout__text">No extra detail for this step.</p>')
  }

  parts <- trim_ws(unlist(strsplit(detail, ";", fixed = TRUE), use.names = FALSE))
  parts <- parts[nzchar(parts)]

  if (length(parts) <= 1L) {
    return(paste0('<p class="detail-callout__text">', html_escape(detail), '</p>'))
  }

  items <- paste0('<li>', html_escape(parts), '</li>', collapse = "")
  paste0('<ol class="detail-list">', items, '</ol>')
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
    build_detail_meta_row('Input columns used', step$input_columns, paste0(id_prefix, 'Input', step$step)),
    build_detail_meta_row('Output columns produced', step$output_columns, paste0(id_prefix, 'Output', step$step)),
    build_detail_meta_row('Columns carried through', step$carried_columns, paste0(id_prefix, 'Carried', step$step)),
    build_detail_meta_row('Columns newly created or changed', step$created_columns, paste0(id_prefix, 'Created', step$step)),
    '</dl>',
    '</article>'
  )
}

build_detail_meta_row <- function(label, value, id) {
  display_style <- if (nzchar(value)) '' else ' style="display:none;"'
  paste0(
    '<div class="detail-meta__row" id="', id, 'Row"', display_style, '>',
    '<dt>', html_escape(label), '</dt>',
    '<dd id="', id, '">', html_escape(value), '</dd>',
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
    '.graph-frame svg {',
    '  width: 100% !important;',
      '  height: auto !important;',
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
    '    sourceRow: document.getElementById("detailSourceRow"),',
    '    source: document.getElementById("detailSource"),',
    '    outputRow: document.getElementById("detailOutputRow"),',
    '    output: document.getElementById("detailOutput"),',
    '    targetRow: document.getElementById("detailTargetRow"),',
    '    target: document.getElementById("detailTarget"),',
    '    inputColumnsRow: document.getElementById("detailInputColumnsRow"),',
    '    inputColumns: document.getElementById("detailInputColumns"),',
    '    outputColumnsRow: document.getElementById("detailOutputColumnsRow"),',
    '    outputColumns: document.getElementById("detailOutputColumns"),',
    '    carriedColumnsRow: document.getElementById("detailCarriedColumnsRow"),',
    '    carriedColumns: document.getElementById("detailCarriedColumns"),',
    '    createdColumnsRow: document.getElementById("detailCreatedColumnsRow"),',
    '    createdColumns: document.getElementById("detailCreatedColumns")',
    '  };',
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
    '  function setMeta(rowNode, valueNode, value) {',
      '    if (!rowNode || !valueNode) return;',
      '    if (value) {',
    '      rowNode.style.display = "grid";',
      '      valueNode.textContent = value;',
    '    } else {',
    '      rowNode.style.display = "none";',
      '      valueNode.textContent = "";',
    '    }',
    '  }',
    '  function escapeHtml(value) {',
    '    return String(value)',
    '      .replace(/&/g, "&amp;")',
    '      .replace(/</g, "&lt;")',
    '      .replace(/>/g, "&gt;")',
    '      .replace(/"/g, "&quot;");',
    '  }',
    '  function renderDetailCallout(detail) {',
    '    var text = (detail || "").trim();',
    '    if (!text) {',
    '      return "<p class=\\"detail-callout__text\\">No extra detail for this step.</p>";',
    '    }',
    '    var parts = text.split(";").map(function(part) { return part.trim(); }).filter(Boolean);',
    '    if (parts.length <= 1) {',
    '      return "<p class=\\"detail-callout__text\\">" + escapeHtml(text) + "</p>";',
    '    }',
    '    return "<ol class=\\"detail-list\\">" + parts.map(function(part) {',
    '      return "<li>" + escapeHtml(part) + "</li>";',
    '    }).join("") + "</ol>";',
    '  }',
    '  function activateStep(button) {',
    '    stepButtons.forEach(function(node) { node.classList.remove("is-active"); });',
    '    button.classList.add("is-active");',
    '    detailMap.phase.textContent = button.dataset.phase || "";',
    '    detailMap.kind.textContent = button.dataset.kind || "";',
    '    detailMap.title.textContent = button.dataset.title || "";',
    '    detailMap.narrative.textContent = button.dataset.narrative || "";',
    '    detailMap.callout.innerHTML = renderDetailCallout(button.dataset.detail || "");',
    '    setMeta(detailMap.sourceRow, detailMap.source, button.dataset.source || "");',
    '    setMeta(detailMap.outputRow, detailMap.output, button.dataset.output || "");',
    '    setMeta(detailMap.targetRow, detailMap.target, button.dataset.target || "");',
    '    setMeta(detailMap.inputColumnsRow, detailMap.inputColumns, button.dataset.inputColumns || "");',
    '    setMeta(detailMap.outputColumnsRow, detailMap.outputColumns, button.dataset.outputColumns || "");',
    '    setMeta(detailMap.carriedColumnsRow, detailMap.carriedColumns, button.dataset.carriedColumns || "");',
    '    setMeta(detailMap.createdColumnsRow, detailMap.createdColumns, button.dataset.createdColumns || "");',
    '  }',
    '  viewButtons.forEach(function(button) {',
      '    button.addEventListener("click", function() {',
        '      setView(button.getAttribute("data-view-target"));',
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
    '  if (resetButton && graphFrame) {',
      '    resetButton.addEventListener("click", function() {',
        '      graphFrame.scrollTo({ top: 0, left: 0, behavior: "smooth" });',
    '    });',
    '  }',
    '  stepButtons.forEach(function(button) {',
    '    button.addEventListener("click", function() {',
    '      activateStep(button);',
    '    });',
    '  });',
    '  if (stepButtons.length > 0) {',
      '    activateStep(stepButtons[0]);',
    '  }',
    '})();'
  ), collapse = "\n")
}
write_docx_report <- function(parsed, out) {
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  staging <- file.path(tempdir(), paste0("ghostwriteR-docx-", as.integer(Sys.time()), "-", sample.int(100000L, 1)))
  dir.create(file.path(staging, "_rels"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(staging, "word", "_rels"), recursive = TRUE, showWarnings = FALSE)

  writeLines(docx_content_types_xml(), file.path(staging, "[Content_Types].xml"), useBytes = TRUE)
  writeLines(docx_package_rels_xml(), file.path(staging, "_rels", ".rels"), useBytes = TRUE)
  writeLines(docx_document_xml(parsed), file.path(staging, "word", "document.xml"), useBytes = TRUE)
  writeLines(docx_styles_xml(), file.path(staging, "word", "styles.xml"), useBytes = TRUE)
  writeLines(docx_document_rels_xml(), file.path(staging, "word", "_rels", "document.xml.rels"), useBytes = TRUE)

  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(staging, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  setwd(staging)
  if (file.exists(out)) {
    unlink(out)
  }

  zip_binary <- resolve_zip_binary()

  utils::zip(
    zip = zip_binary,
    zipfile = normalizePath(out, winslash = "/", mustWork = FALSE),
    files = c(
      "[Content_Types].xml",
      "_rels/.rels",
      "word/document.xml",
      "word/styles.xml",
      "word/_rels/document.xml.rels"
    ),
    flags = "-r9Xq"
  )

  invisible(out)
}

resolve_zip_binary <- function() {
  candidates <- unique(c(
    Sys.getenv("R_ZIPCMD", unset = ""),
    unname(Sys.which("zip"))
  ))
  candidates <- candidates[nzchar(candidates)]

  if (length(candidates) == 0L) {
    stop(
      "DOCX export requires a zip utility. Install a zip executable or set R_ZIPCMD to a valid zip command before using format = \"docx\".",
      call. = FALSE
    )
  }

  candidates[[1L]]
}

docx_content_types_xml <- function() {
  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
    '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
    '<Default Extension="xml" ContentType="application/xml"/>',
    '<Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>',
    '<Override PartName="/word/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/>',
    '</Types>'
  )
}

docx_package_rels_xml <- function() {
  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/>',
    '</Relationships>'
  )
}

docx_document_rels_xml <- function() {
  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"/>'
  )
}

docx_styles_xml <- function() {
  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<w:styles xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    '<w:style w:type="paragraph" w:default="1" w:styleId="Normal">',
    '<w:name w:val="Normal"/>',
    '<w:qFormat/>',
    '</w:style>',
    '<w:style w:type="paragraph" w:styleId="Heading1">',
    '<w:name w:val="heading 1"/>',
    '<w:basedOn w:val="Normal"/>',
    '<w:next w:val="Normal"/>',
    '<w:qFormat/>',
    '<w:rPr><w:b/><w:sz w:val="34"/></w:rPr>',
    '</w:style>',
    '<w:style w:type="paragraph" w:styleId="Heading2">',
    '<w:name w:val="heading 2"/>',
    '<w:basedOn w:val="Normal"/>',
    '<w:next w:val="Normal"/>',
    '<w:qFormat/>',
    '<w:rPr><w:b/><w:sz w:val="28"/></w:rPr>',
    '</w:style>',
    '</w:styles>'
  )
}

docx_document_xml <- function(parsed) {
  steps <- parsed$steps
  has_inventories <- workflow_has_inventories(steps)
  body <- c(
    docx_paragraph(paste0("GhostwriteR Report: ", basename(parsed$script_path)), style = "Heading1"),
    docx_paragraph(workflow_summary_line(workflow_stats(parsed))),
    docx_paragraph("How To Read The Diagram", style = "Heading2"),
    docx_paragraph("Yellow note or cylinder: external source such as a file or database table."),
    docx_paragraph("Blue and accent-colored shapes: intermediate named objects created or reused during processing, such as data frames, tables, models, matrices, vectors, or chart objects."),
    docx_paragraph("Green rounded box: transformation or calculation step."),
    docx_paragraph("Peach rounded box, cylinder, or note: final output step, output table, or saved file."),
    docx_paragraph("Workflow", style = "Heading2")
  )

  if (nrow(steps) == 0) {
    body <- c(body, docx_paragraph("No workflow steps were detected."))
  } else {
    for (index in seq_len(nrow(steps))) {
      body <- c(body, docx_paragraph(paste0(index, ". ", steps$narrative[[index]])))
      inventory_lines <- step_inventory_lines(steps[index, , drop = FALSE])
      if (length(inventory_lines) > 0L) {
        body <- c(body, vapply(inventory_lines, docx_paragraph, character(1)))
      }
    }
  }

  if (isTRUE(has_inventories)) {
    body <- c(body, docx_paragraph("Column Inventories", style = "Heading2"))
    body <- c(body, vapply(build_inventory_report_lines(steps, nodes = parsed$nodes, format = "text"), docx_paragraph, character(1)))
  }

  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    '<w:body>',
    paste(body, collapse = ""),
    '<w:sectPr><w:pgSz w:w="12240" w:h="15840"/><w:pgMar w:top="1440" w:right="1440" w:bottom="1440" w:left="1440" w:header="708" w:footer="708" w:gutter="0"/></w:sectPr>',
    '</w:body>',
    '</w:document>'
  )
}

docx_paragraph <- function(text, style = "Normal") {
  paste0(
    '<w:p>',
    '<w:pPr><w:pStyle w:val="', escape_xml(style), '"/></w:pPr>',
    '<w:r><w:t xml:space="preserve">', escape_xml(text), '</w:t></w:r>',
    '</w:p>'
  )
}

expression_text <- function(expr, srcref) {
  if (!is.null(srcref)) {
    return(compact_ws(paste(as.character(srcref), collapse = "\n")))
  }

  compact_ws(paste(deparse(expr), collapse = " "))
}

compact_ws <- function(text) {
  text <- gsub("\\s+", " ", text)
  trim_ws(text)
}

trim_ws <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

label_from_expr <- function(text) {
  text <- trim_ws(text)
  if (is_plain_name(text)) {
    return(text)
  }

  fn <- leading_call(text)
  if (is.null(fn)) {
    return(text)
  }

  plain_fn(fn)
}

is_plain_name <- function(text) {
  grepl("^[[:alnum:]_.]+$", text)
}

leading_call <- function(text) {
  text <- trim_ws(text)
  match <- regexec("^([[:alnum:]_.]+(?:::[[:alnum:]_.]+)?)\\s*\\(", text)
  hit <- regmatches(text, match)[[1]]

  if (length(hit) < 2) {
    return(NULL)
  }

  hit[2]
}

call_args <- function(text) {
  open <- regexpr("\\(", text)
  close <- regexpr("\\)[^()]*$", text)

  if (open[[1]] < 0 || close[[1]] < 0 || close[[1]] <= open[[1]]) {
    return(character())
  }

  inner <- substr(text, open[[1]] + 1L, close[[1]] - 1L)
  parts <- split_top_level(inner, ",")
  trim_ws(parts[nzchar(trim_ws(parts))])
}

split_pipe <- function(text) {
  split_top_level(text, c("%>%", "|>"))
}

split_top_level <- function(text, separators) {
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  pieces <- character()
  current <- character()
  depth <- 0L
  quote_char <- ""
  index <- 1L
  text_length <- length(chars)

  while (index <= text_length) {
    char <- chars[[index]]

    if (nzchar(quote_char)) {
      current <- c(current, char)
      if (char == quote_char && (index == 1L || chars[[index - 1L]] != "\\")) {
        quote_char <- ""
      }
      index <- index + 1L
      next
    }

    if (char %in% c("\"", "'")) {
      quote_char <- char
      current <- c(current, char)
      index <- index + 1L
      next
    }

    if (char %in% c("(", "[", "{")) {
      depth <- depth + 1L
      current <- c(current, char)
      index <- index + 1L
      next
    }

    if (char %in% c(")", "]", "}")) {
      depth <- max(0L, depth - 1L)
      current <- c(current, char)
      index <- index + 1L
      next
    }

    if (depth == 0L) {
      matched <- FALSE

      for (separator in separators) {
        width <- nchar(separator)
        candidate <- paste(chars[index:min(text_length, index + width - 1L)], collapse = "")

        if (candidate == separator) {
          pieces <- c(pieces, paste(current, collapse = ""))
          current <- character()
          index <- index + width
          matched <- TRUE
          break
        }
      }

      if (matched) {
        next
      }
    }

    current <- c(current, char)
    index <- index + 1L
  }

  c(pieces, paste(current, collapse = ""))
}

extract_string_literal <- function(text) {
  match <- regexec("['\"]([^'\"]+)['\"]", text)
  hit <- regmatches(text, match)[[1]]

  if (length(hit) < 2) {
    return(NULL)
  }

  hit[2]
}

first_plain_arg <- function(text) {
  args <- call_args(text)
  if (length(args) == 0) {
    return("")
  }

  first <- args[[1]]
  if (is_plain_name(first)) {
    return(first)
  }

  ""
}

is_known_call <- function(fn, known, known_base) {
  if (is.null(fn)) {
    return(FALSE)
  }

  fn_base <- sub("^[^:]+::", "", fn)
  fn %in% known || fn_base %in% known_base
}

plain_fn <- function(fn) {
  if (is.null(fn) || length(fn) == 0L) {
    return("")
  }

  fn <- trim_ws(fn[[1]])
  if (!nzchar(fn)) {
    return("")
  }

  sub("^[^:]+::", "", fn)
}

multiline_label <- function(title, detail = NULL) {
  title <- trim_ws(title)
  if (is.null(detail) || !nzchar(trim_ws(detail))) {
    return(title)
  }

  wrapped <- strwrap(trim_ws(detail), width = 26)
  paste(c(title, wrapped), collapse = "\n")
}

quoted_name <- function(text) {
  paste0('"', trim_ws(text), '"')
}

pretty_path <- function(path_value) {
  if (is.null(path_value) || !nzchar(trim_ws(path_value))) {
    return("Unknown path")
  }

  path_value <- display_path_value(path_value)
  parent <- dirname(path_value)
  file <- basename(path_value)

  if (identical(parent, ".")) {
    return(file)
  }

  paste(file, paste0("from ", parent), sep = "\n")
}

display_path_value <- function(path_value) {
  if (is.null(path_value) || !nzchar(trim_ws(path_value))) {
    return(path_value)
  }

  style <- getOption("ghostwriteR.path_style", "full")
  style <- match.arg(style, c("full", "basename"))
  path_value <- trim_ws(path_value)

  if (identical(style, "basename")) {
    return(basename(path_value))
  }

  path_value
}

human_action <- function(fn) {
  fn <- plain_fn(fn)

  if (!nzchar(fn)) return("Transform data")

  if (fn %in% c("read.csv", "read_csv")) return("Load CSV file")
  if (fn %in% c("read_tsv")) return("Load tab-delimited file")
  if (fn %in% c("read_delim", "fread")) return("Load data file")
  if (fn %in% c("read_excel")) return("Load Excel file")
  if (fn %in% c("readRDS")) return("Load R data file")
  if (fn %in% c("dbGetQuery")) return("Run database query")
  if (fn %in% c("transform", "mutate")) return("Add or change columns")
  if (fn %in% c("subset", "filter")) return("Filter rows")
  if (fn %in% c("select")) return("Keep selected columns")
  if (fn %in% c("arrange")) return("Sort rows")
  if (fn %in% c("group_by")) return("Group rows")
  if (fn %in% c("summarise", "summarize")) return("Summarize data")
  if (fn %in% c("rename")) return("Rename columns")
  if (fn %in% c("distinct")) return("Remove duplicates")
  if (fn %in% c("pivot_longer")) return("Reshape to long format")
  if (fn %in% c("pivot_wider")) return("Reshape to wide format")
  if (fn %in% c("left_join", "right_join", "inner_join", "full_join", "semi_join", "anti_join")) return("Join tables")
  if (fn %in% c("lm")) return("Fit linear model")
  if (fn %in% c("glm")) return("Fit generalized linear model")
  if (fn %in% c("predict")) return("Score records with model")
  if (fn %in% c("ifelse", "case_when")) return("Create conditional value")
  if (fn %in% c("cut")) return("Bucket values")
  if (fn %in% c("ggplot")) return("Create chart")
  if (grepl("^geom_", fn)) return("Add chart layer")
  if (fn %in% c("labs")) return("Label chart")
  if (grepl("^theme", fn)) return("Style chart")
  if (fn %in% c("write.csv", "write_csv")) return("Write CSV file")
  if (fn %in% c("write_tsv", "write_delim", "write.xlsx", "write_xlsx")) return("Write output file")
  if (fn %in% c("saveRDS", "save")) return("Save R data file")
  if (fn %in% c("ggsave", "pdf", "png", "jpeg", "tiff", "svg", "svglite")) return("Save visual output")

  paste("Run", fn)
}

r_object_kind_from_input <- function(fn) {
  fn <- plain_fn(fn)

  if (fn %in% c("read.csv", "read_csv", "read_tsv", "read_delim", "fread", "read_excel", "dbGetQuery")) {
    return("data_frame")
  }

  if (fn %in% c("readRDS")) {
    return("generic_object")
  }

  "generic_object"
}

r_object_kind_from_transform <- function(text, fn = leading_call(text)) {
  fn <- plain_fn(fn)

  if (grepl("\\bggplot\\s*\\(", text, perl = TRUE) || grepl("\\bgeom_[[:alnum:]_]+\\s*\\(", text, perl = TRUE)) {
    return("plot_object")
  }

  if (fn %in% c("lm", "glm")) {
    return("model_object")
  }

  if (fn %in% c("matrix", "as.matrix", "model.matrix")) {
    return("matrix_object")
  }

  if (fn %in% c("predict", "ifelse", "case_when", "cut")) {
    return("vector_object")
  }

  if (fn %in% c(
    "transform", "mutate", "subset", "filter", "select", "arrange", "group_by",
    "summarise", "summarize", "rename", "distinct", "pivot_longer", "pivot_wider",
    "left_join", "right_join", "inner_join", "full_join", "semi_join", "anti_join"
  )) {
    return("data_frame")
  }

  "generic_object"
}

strip_wrapping_quotes <- function(text) {
  text <- trim_ws(text)
  if (grepl('^".*"$', text) || grepl("^'.*'$", text)) {
    substr(text, 2L, nchar(text) - 1L)
  } else {
    text
  }
}

named_arg_value <- function(args, name) {
  if (length(args) == 0) {
    return("")
  }

  pattern <- paste0("^", name, "\\s*=")
  hit <- args[grepl(pattern, args)]
  if (length(hit) == 0) {
    return("")
  }

  trim_ws(sub(pattern, "", hit[[1]]))
}

model_detail <- function(fn, args) {
  formula_arg <- args[grepl("~", args)][1]
  if (is.na(formula_arg) || !nzchar(formula_arg)) {
    formula_arg <- args[!grepl("^[[:alnum:]_.]+\\s*=", args)][1]
  }
  data_arg <- named_arg_value(args, "data")
  family_arg <- named_arg_value(args, "family")

  detail <- if (!is.na(formula_arg) && nzchar(formula_arg) && grepl("~", formula_arg, fixed = TRUE)) {
    parts <- strsplit(formula_arg, "~", fixed = TRUE)[[1]]
    response <- trim_ws(parts[[1]])
    predictors <- trim_ws(strsplit(parts[[2]], "\\+", perl = TRUE)[[1]])
    paste0("estimate ", response, " from ", paste(predictors, collapse = ", "))
  } else {
    "estimate a model from the supplied formula"
  }

  if (nzchar(data_arg)) {
    detail <- paste0(detail, " using data table ", label_from_expr(data_arg))
  }

  if (identical(plain_fn(fn), "glm") && nzchar(family_arg)) {
    detail <- paste0(detail, " with ", strip_wrapping_quotes(family_arg), " assumptions")
  }

  detail
}

prediction_detail <- function(args) {
  model_name <- if (length(args) > 0) label_from_expr(args[[1]]) else "a model"
  newdata_arg <- named_arg_value(args, "newdata")

  detail <- paste0("generate predicted values from ", model_name)
  if (nzchar(newdata_arg)) {
    detail <- paste0(detail, " using ", label_from_expr(newdata_arg))
  }
  detail
}

chart_detail <- function(args, raw_text = "") {
  data_source <- if (length(args) > 0 && is_plain_name(args[[1]])) label_from_expr(args[[1]]) else ""
  mapping_arg <- ""
  if (nzchar(raw_text)) {
    mapping_match <- regexec("aes\\s*\\(([^)]*)\\)", raw_text, perl = TRUE)
    mapping_hit <- regmatches(raw_text, mapping_match)[[1]]
    if (length(mapping_hit) >= 2L) {
      mapping_arg <- paste0("aes(", mapping_hit[[2]], ")")
    }
  }
  if (!nzchar(mapping_arg)) {
    mapping_candidates <- args[grepl("^aes\\s*\\(", args)]
    if (length(mapping_candidates) > 0L) {
      mapping_arg <- mapping_candidates[[1]]
    }
  }
  mapping_bits <- character()

  if (nzchar(mapping_arg)) {
    aes_args <- call_args(mapping_arg)
    mapping_bits <- vapply(aes_args, function(arg) {
      parts <- strsplit(arg, "=", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        key <- trim_ws(parts[[1]])
        value <- humanize_expression(trim_ws(paste(parts[-1], collapse = "=")))
        if (identical(key, "x")) return(paste0(value, " on the x-axis"))
        if (identical(key, "y")) return(paste0(value, " on the y-axis"))
        if (identical(key, "color")) return(paste0("color by ", value))
        if (identical(key, "fill")) return(paste0("fill by ", value))
        if (identical(key, "group")) return(paste0("group by ", value))
        paste0(key, " uses ", value)
      } else {
        humanize_expression(trim_ws(arg))
      }
    }, character(1))
  }

  detail <- if (nzchar(data_source)) paste0("build the chart from ", data_source) else "build the chart from the current data"
  if (length(mapping_bits) > 0) {
    detail <- paste0(detail, "; ", paste(mapping_bits, collapse = "; "))
  }
  detail
}

humanize_special_call <- function(text) {
  fn <- leading_call(text)
  if (is.null(fn)) {
    return(NULL)
  }

  fn <- plain_fn(fn)
  args <- call_args(text)

  if (fn == "ifelse" && length(args) >= 3) {
    return(paste0(
      "if ", humanize_expression(args[[1]]),
      " then ", humanize_expression(args[[2]]),
      " otherwise ", humanize_expression(args[[3]])
    ))
  }

  if (fn == "format" && length(args) >= 2) {
    return(paste0("format ", humanize_expression(args[[1]]), " as ", strip_wrapping_quotes(args[[2]])))
  }

  if (fn == "predict") {
    return(prediction_detail(args))
  }

  if (fn %in% c("lm", "glm")) {
    return(model_detail(fn, args))
  }

  if (fn == "desc" && length(args) >= 1) {
    return(paste0("descending ", humanize_expression(args[[1]])))
  }

  NULL
}

humanize_expression <- function(text) {
  text <- trim_ws(text)
  if (!nzchar(text)) {
    return(text)
  }

  if (grepl('^".*"$', text) || grepl("^'.*'$", text)) {
    return(strip_wrapping_quotes(text))
  }

  special <- humanize_special_call(text)
  if (!is.null(special)) {
    return(trim_ws(special))
  }

  replacements <- c(
    "!is\\.na\\(([^()]+)\\)" = "\\1 is present",
    "mean\\(([^,()]+),\\s*na\\.rm\\s*=\\s*TRUE\\)" = "average of \\1, ignoring missing values",
    "mean\\(([^()]+)\\)" = "average of \\1",
    "sum\\(([^,()]+),\\s*na\\.rm\\s*=\\s*TRUE\\)" = "total \\1, ignoring missing values",
    "sum\\(([^()]+)\\)" = "total \\1",
    "n_distinct\\(([^()]+)\\)" = "number of distinct \\1",
    "n\\(\\)" = "row count",
    "toupper\\(([^()]+)\\)" = "uppercase \\1",
    "trimws\\(([^()]+)\\)" = "trim spaces from \\1",
    "as\\.Date\\(([^()]+)\\)" = "convert \\1 to a date",
    "as\\.numeric\\(([^()]+)\\)" = "convert \\1 to a number",
    "is\\.na\\(([^()]+)\\)" = "\\1 is missing",
    "\\.groups\\s*=\\s*\"drop\"" = "return a regular table without grouping"
  )

  for (pattern in names(replacements)) {
    text <- gsub(pattern, replacements[[pattern]], text, perl = TRUE)
  }

  text <- gsub("==", " equals ", text, fixed = TRUE)
  text <- gsub("!=", " does not equal ", text, fixed = TRUE)
  text <- gsub(">=", " is at least ", text, fixed = TRUE)
  text <- gsub("<=", " is at most ", text, fixed = TRUE)
  text <- gsub(">", " is greater than ", text, fixed = TRUE)
  text <- gsub("<", " is less than ", text, fixed = TRUE)
  text <- gsub("&", " and ", text, fixed = TRUE)
  text <- gsub("|", " or ", text, fixed = TRUE)
  text <- gsub("\\s+", " ", text)
  trim_ws(text)
}

explain_assignment <- function(arg) {
  parts <- strsplit(arg, "=", fixed = TRUE)[[1]]
  if (length(parts) < 2) {
    return(humanize_expression(arg))
  }

  lhs <- trim_ws(parts[[1]])
  rhs <- trim_ws(paste(parts[-1], collapse = "="))

  if (identical(lhs, ".groups") && identical(gsub('"', "", rhs, fixed = TRUE), "drop")) {
    return("return a regular table without grouping")
  }

  paste0(lhs, " = ", humanize_expression(rhs))
}

explain_grouping <- function(args) {
  paste0("organize records by ", paste(vapply(args, humanize_expression, character(1)), collapse = ", "))
}

explain_arrange <- function(args) {
  paste0("sort rows by ", paste(vapply(args, humanize_expression, character(1)), collapse = ", "))
}

explain_select <- function(args) {
  paste0("keep columns ", paste(vapply(args, humanize_expression, character(1)), collapse = ", "))
}

explain_distinct <- function(args) {
  if (length(args) == 0) {
    return("keep unique rows")
  }
  paste0("keep one row per unique ", paste(vapply(args, humanize_expression, character(1)), collapse = ", "))
}

summarise_detail <- function(args) {
  args <- trim_ws(args)
  metric_args <- args[!grepl("^\\.groups\\s*=", args)]
  group_args <- args[grepl("^\\.groups\\s*=", args)]

  details <- character()
  if (length(metric_args) > 0) {
    details <- c(details, paste0("calculate summary metrics: ", paste(vapply(metric_args, explain_assignment, character(1)), collapse = "; ")))
  }
  if (length(group_args) > 0) {
    details <- c(details, vapply(group_args, explain_assignment, character(1)))
  }

  paste(details, collapse = "; ")
}

transform_detail <- function(fn, args, raw_text = "") {
  fn <- plain_fn(fn)
  args <- trim_ws(args)
  args <- args[nzchar(args)]

  if (length(args) == 0) {
    return("")
  }

  if (fn %in% c("transform", "mutate", "rename")) {
    return(paste(vapply(args, explain_assignment, character(1)), collapse = "; "))
  }

  if (fn %in% c("subset", "filter")) {
    return(paste(vapply(args, humanize_expression, character(1)), collapse = " and "))
  }

  if (fn %in% c("summarise", "summarize")) {
    return(summarise_detail(args))
  }

  if (fn %in% c("group_by")) {
    return(explain_grouping(args))
  }

  if (fn %in% c("arrange")) {
    return(explain_arrange(args))
  }

  if (fn %in% c("select")) {
    return(explain_select(args))
  }

  if (fn %in% c("distinct")) {
    return(explain_distinct(args))
  }

  if (fn %in% c("pivot_longer", "pivot_wider")) {
    return(paste(vapply(args, humanize_expression, character(1)), collapse = ", "))
  }

  if (fn %in% c("left_join", "right_join", "inner_join", "full_join", "semi_join", "anti_join")) {
    table_name <- if (length(args) > 0) label_from_expr(args[[1]]) else "another table"
    join_key <- ""
    by_arg <- args[grepl("^by\\s*=", args)]
    if (length(by_arg) > 0) {
      join_key <- paste0(" using key ", gsub('^by\\s*=\\s*', "", by_arg[[1]]))
    }
    join_type <- toupper(sub("_join$", "", fn))
    keep_note <- switch(
      join_type,
      LEFT = ", keeping all rows from the current data even when matches are missing",
      RIGHT = ", keeping all rows from the joined table even when matches are missing",
      FULL = ", keeping rows even when one side does not match",
      INNER = ", keeping only rows that match on both sides",
      SEMI = ", keeping only rows from the current data that have a match",
      ANTI = ", keeping only rows from the current data that do not have a match",
      ""
    )
    return(paste0("use a ", join_type, " JOIN to bring ", table_name, " into the current data", join_key, keep_note))
  }

  if (fn %in% c("lm", "glm")) {
    return(model_detail(fn, args))
  }

  if (fn == "predict") {
    return(prediction_detail(args))
  }

  if (fn == "ggplot") {
    return(chart_detail(args, raw_text = raw_text))
  }

  if (grepl("^geom_", fn)) {
    return(paste0("add a ", sub("^geom_", "", fn), " layer to the chart"))
  }

  if (fn %in% c("labs")) {
    return(paste(vapply(args, explain_assignment, character(1)), collapse = "; "))
  }

  if (grepl("^theme", fn)) {
    return("adjust the chart styling for readability")
  }

  if (fn %in% c("ifelse", "case_when", "cut")) {
    return(humanize_expression(paste(args, collapse = ", ")))
  }

  paste(vapply(args, humanize_expression, character(1)), collapse = ", ")
}

describe_input <- function(text) {
  fn <- leading_call(text)
  path_value <- display_path_value(extract_string_literal(text))
  list(
    title = human_action(fn),
    detail = pretty_path(path_value),
    path = path_value,
    object_kind = r_object_kind_from_input(fn)
  )
}

describe_output <- function(text) {
  fn <- leading_call(text)
  fn_plain <- plain_fn(fn)
  args <- call_args(text)
  unnamed_args <- args[!grepl("^[[:alnum:]_.]+\\s*=", args)]

  path_value <- if (fn_plain == "ggsave") {
    filename_arg <- named_arg_value(args, "filename")
    file_arg <- named_arg_value(args, "file")
    if (nzchar(filename_arg)) {
      extract_string_literal(filename_arg)
    } else if (nzchar(file_arg)) {
      extract_string_literal(file_arg)
    } else if (length(unnamed_args) > 0) {
      extract_string_literal(unnamed_args[[1]])
    } else {
      extract_string_literal(text)
    }
  } else if (fn_plain %in% c("pdf", "png", "jpeg", "tiff", "svg", "svglite")) {
    if (length(unnamed_args) > 0) extract_string_literal(unnamed_args[[1]]) else extract_string_literal(text)
  } else if (length(args) >= 2) {
    extract_string_literal(args[[2]])
  } else {
    extract_string_literal(text)
  }
  path_value <- display_path_value(path_value)

  source_expr <- if (fn_plain == "ggsave") {
    plot_arg <- named_arg_value(args, "plot")
    if (nzchar(plot_arg)) {
      plot_arg
    } else if (length(unnamed_args) >= 2) {
      unnamed_args[[2]]
    } else {
      ""
    }
  } else if (length(args) > 0) {
    args[[1]]
  } else {
    ""
  }

  list(
    title = human_action(fn),
    detail = pretty_path(path_value),
    path = path_value,
    source_expr = source_expr
  )
}

describe_transform <- function(text, source_in_call = TRUE) {
  fn <- leading_call(text)
  fn_plain <- plain_fn(fn)
  args <- call_args(text)
  source <- ""

  if (isTRUE(source_in_call) && length(args) > 0 && is_plain_name(args[[1]])) {
    source <- args[[1]]
    args <- args[-1]
  } else if (fn_plain %in% c("lm", "glm", "ggplot")) {
    data_arg <- named_arg_value(args, "data")
    if (nzchar(data_arg)) {
      source <- label_from_expr(data_arg)
    }
  }

  list(
    title = human_action(fn),
    detail = transform_detail(fn, args, raw_text = text),
    source = source,
    object_kind = r_object_kind_from_transform(text, fn)
  )
}

sql_statement_list <- function(text) {
  cleaned <- sql_strip_comments(text)
  statements <- split_top_level(cleaned, ";")
  statements <- vapply(statements, compact_ws, character(1))
  statements[nzchar(statements)]
}

sql_strip_comments <- function(text) {
  text <- gsub("(?s)/\\*.*?\\*/", " ", text, perl = TRUE)
  gsub("(?m)--[^\\n]*$", " ", text, perl = TRUE)
}

sql_describe_statement <- function(statement) {
  statement <- compact_ws(statement)
  query_text <- sql_query_text(statement)
  output_info <- sql_output_info(statement)
  copy_table_export <- sql_is_copy_table_export(statement)

  if (copy_table_export) {
    stage_infos <- list()
    copy_sources <- sql_extract_sources(query_text)
    final_stage <- list(
      sources = copy_sources,
      source_refs = list(),
      query_steps = list(),
      query_detail = "",
      query_input_columns = "",
      query_output_columns = "",
      query_carried_columns = "",
      query_created_columns = "",
      select_items = character()
    )
  } else {
    stages <- sql_query_stages(query_text)
    stage_infos <- lapply(stages, function(stage) {
      parsed <- sql_describe_query(stage$query)
      parsed$name <- stage$name
      parsed
    })
    final_stage <- if (length(stage_infos) > 0L) stage_infos[[length(stage_infos)]] else list(sources = character(), query_steps = list(), query_detail = "")
  }


  list(
    stages = stage_infos,
    sources = final_stage$sources,
    query_steps = final_stage$query_steps,
    query_detail = final_stage$query_detail,
    query_input_columns = final_stage$query_input_columns %||% "",
    query_output_columns = final_stage$query_output_columns %||% "",
    query_carried_columns = final_stage$query_carried_columns %||% "",
    query_created_columns = final_stage$query_created_columns %||% "",
    output_title = output_info$title,
    output_detail = output_info$detail,
    output_name = output_info$name,
    output_path = output_info$path,
    output_is_temporary = output_info$is_temporary
  )
}

sql_describe_query <- function(query_text) {
  sources <- sql_extract_sources(query_text)
  source_refs <- sql_extract_source_refs(query_text)
  joins <- sql_extract_join_specs(query_text)
  where_clause <- sql_extract_clause(query_text, "where", c("group by", "having", "order by", "limit", "qualify", "union"))
  group_clause <- sql_extract_clause(query_text, "group by", c("having", "order by", "limit", "qualify", "union"))
  order_clause <- sql_extract_clause(query_text, "order by", c("limit", "qualify", "union"))
  if (!nzchar(order_clause)) {
    order_clause <- sql_extract_order_clause(query_text)
  }
  select_items <- sql_extract_select_items(query_text)
  has_distinct <- grepl("(?is)\\bselect\\s+distinct\\b", query_text, perl = TRUE)
  has_aggregates <- sql_has_aggregates(select_items) || nzchar(group_clause)
  computed_items <- select_items[vapply(select_items, sql_is_feature_select_item, logical(1))]
  feature_detail <- sql_feature_detail(select_items)
  select_inventory <- sql_select_inventory(select_items)
  query_output_columns <- select_inventory$output_columns
  query_input_columns <- sql_query_input_columns(select_items, where_clause, group_clause, order_clause, joins)

  where_clause <- trim_ws(sub("(?is)\\border\\s+by\\b.*$", "", where_clause, perl = TRUE))

  query_steps <- list()

  if (length(sources) > 1L) {
    query_steps[[length(query_steps) + 1L]] <- list(
      title = "Join tables",
      detail = sql_join_detail(sources, joins),
      input_columns = sql_join_input_columns(joins),
      output_columns = "",
      carried_columns = "",
      created_columns = ""
    )
  }

  if (nzchar(where_clause)) {
    query_steps[[length(query_steps) + 1L]] <- list(
      title = "Filter rows",
      detail = humanize_sql_expression(where_clause),
      input_columns = sql_clause_column_inventory(where_clause),
      output_columns = "",
      carried_columns = "",
      created_columns = ""
    )
  }

  if (nzchar(feature_detail)) {
    query_steps[[length(query_steps) + 1L]] <- list(
      title = "Add or change columns",
      detail = feature_detail,
      input_columns = sql_select_input_columns(computed_items),
      output_columns = sql_select_output_columns(computed_items),
      carried_columns = "",
      created_columns = sql_select_output_columns(computed_items)
    )
  }

  if (has_distinct && !has_aggregates) {
    query_steps[[length(query_steps) + 1L]] <- list(
      title = "Remove duplicates",
      detail = sql_distinct_detail(select_items),
      input_columns = query_input_columns,
      output_columns = query_output_columns,
      carried_columns = select_inventory$carried_columns,
      created_columns = select_inventory$created_columns
    )
  }

  if (has_aggregates) {
    query_steps[[length(query_steps) + 1L]] <- list(
      title = "Summarize data",
      detail = sql_summarize_detail(select_items, group_clause),
      input_columns = sql_summarize_input_columns(select_items, group_clause),
      output_columns = query_output_columns,
      carried_columns = select_inventory$carried_columns,
      created_columns = select_inventory$created_columns
    )
  }

  if (nzchar(order_clause)) {
    query_steps[[length(query_steps) + 1L]] <- list(
      title = "Sort rows",
      detail = sql_order_detail(order_clause),
      input_columns = sql_clause_column_inventory(order_clause),
      output_columns = "",
      carried_columns = "",
      created_columns = ""
    )
  }

  if (length(query_steps) == 0L && grepl("(?i)\\bselect\\b", query_text, perl = TRUE)) {
    query_steps[[1L]] <- list(
      title = "Select columns",
      detail = sql_select_detail(select_items),
      input_columns = query_input_columns,
      output_columns = query_output_columns,
      carried_columns = select_inventory$carried_columns,
      created_columns = select_inventory$created_columns
    )
  }

  list(
    sources = sources,
    source_refs = source_refs,
    query_steps = query_steps,
    query_detail = sql_select_detail(select_items),
    query_input_columns = query_input_columns,
    query_output_columns = query_output_columns,
    query_carried_columns = select_inventory$carried_columns,
    query_created_columns = select_inventory$created_columns,
    select_items = select_items
  )
}

sql_extract_order_clause <- function(query_text) {
  hit <- regexec("(?is)\\border\\s+by\\b\\s+(.+?)(?:;|$)", query_text, perl = TRUE)
  capture <- regmatches(query_text, hit)[[1]]

  if (length(capture) < 2L) {
    return("")
  }

  compact_ws(capture[[2]])
}

sql_query_text <- function(statement) {
  statement <- compact_ws(statement)

  if (grepl("(?is)^copy\\s*\\(", statement, perl = TRUE)) {
    inner <- extract_enclosed_text(statement, regexpr("\\(", statement)[[1]])
    return(compact_ws(inner))
  }

  if (grepl("(?is)^unload\\s*\\(", statement, perl = TRUE)) {
    inner <- extract_enclosed_text(statement, regexpr("\\(", statement)[[1]])
    return(compact_ws(strip_wrapping_quotes(inner)))
  }

  create_match <- regexec(
    "(?is)^create\\s+(?:or\\s+replace\\s+)?(?:temp(?:orary)?\\s+)?(?:table|view|materialized\\s+view)\\s+(?:if\\s+not\\s+exists\\s+)?([[:alnum:]_.$\"]+)\\s+as\\s+(.+)$",
    statement,
    perl = TRUE
  )
  create_hit <- regmatches(statement, create_match)[[1]]
  if (length(create_hit) >= 3L) {
    return(compact_ws(create_hit[[3]]))
  }

  insert_match <- regexec(
    "(?is)^insert\\s+into\\s+([[:alnum:]_.$\"]+)\\s+(.+)$",
    statement,
    perl = TRUE
  )
  insert_hit <- regmatches(statement, insert_match)[[1]]
  if (length(insert_hit) >= 3L) {
    return(compact_ws(insert_hit[[3]]))
  }

  copy_table_match <- regexec(
    "(?is)^copy\\s+([[:alnum:]_.$\"]+)\\s+to\\s+['\"][^'\"]+['\"]",
    statement,
    perl = TRUE
  )
  copy_table_hit <- regmatches(statement, copy_table_match)[[1]]
  if (length(copy_table_hit) >= 2L) {
    return(paste("SELECT * FROM", sql_clean_identifier(copy_table_hit[[2]])))
  }

  statement
}

sql_query_stages <- function(query_text) {
  query_text <- compact_ws(query_text)
  if (!grepl("(?is)^with\\b", query_text, perl = TRUE)) {
    return(list(list(name = "", query = query_text)))
  }

  remaining <- sub("(?is)^with\\b\\s*", "", query_text, perl = TRUE)
  stages <- list()

  repeat {
    name_match <- regexec("^([[:alnum:]_.$\"]+)", remaining, perl = TRUE)
    name_hit <- regmatches(remaining, name_match)[[1]]
    if (length(name_hit) < 2L) {
      break
    }

    cte_name <- sql_clean_identifier(name_hit[[2]])
    after_name <- substring(remaining, nchar(name_hit[[1]]) + 1L)

    if (grepl("^\\s*\\(", after_name, perl = TRUE)) {
      stage_cols <- extract_enclosed_segment(after_name, regexpr("\\(", after_name)[[1]])
      after_name <- substring(after_name, stage_cols$end + 1L)
    }

    as_match <- regexec("^\\s*as\\s*\\(", after_name, ignore.case = TRUE, perl = TRUE)
    as_hit <- regmatches(after_name, as_match)[[1]]
    if (length(as_hit) == 0L) {
      break
    }

    open_index <- regexpr("\\(", after_name)[[1]]
    segment <- extract_enclosed_segment(after_name, open_index)
    stages[[length(stages) + 1L]] <- list(name = cte_name, query = compact_ws(segment$inner))

    remaining <- trim_ws(substring(after_name, segment$end + 1L))
    if (startsWith(remaining, ",")) {
      remaining <- trim_ws(substring(remaining, 2L))
      next
    }

    if (nzchar(remaining)) {
      stages[[length(stages) + 1L]] <- list(name = "", query = compact_ws(remaining))
    }
    break
  }

  if (length(stages) == 0L) {
    list(list(name = "", query = query_text))
  } else {
    stages
  }
}

extract_enclosed_text <- function(text, open_index) {
  extract_enclosed_segment(text, open_index)$inner
}

extract_enclosed_segment <- function(text, open_index) {
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  depth <- 0L
  quote_char <- ""
  collected <- character()
  end_index <- length(chars)

  for (index in seq.int(open_index, length(chars))) {
    char <- chars[[index]]

    if (nzchar(quote_char)) {
      if (index > open_index) {
        collected <- c(collected, char)
      }
      if (char == quote_char && chars[[max(index - 1L, 1L)]] != "\\") {
        quote_char <- ""
      }
      next
    }

    if (char %in% c("\"", "'")) {
      quote_char <- char
      if (index > open_index) {
        collected <- c(collected, char)
      }
      next
    }

    if (char == "(") {
      depth <- depth + 1L
      if (depth > 1L) {
        collected <- c(collected, char)
      }
      next
    }

    if (char == ")") {
      depth <- depth - 1L
      if (depth == 0L) {
        end_index <- index
        break
      }
      collected <- c(collected, char)
      next
    }

    if (depth >= 1L) {
      collected <- c(collected, char)
    }
  }

  list(inner = paste(collected, collapse = ""), end = end_index)
}

sql_output_info <- function(statement) {
  statement <- compact_ws(statement)

  create_match <- regexec(
    "(?is)^create\\s+(?:or\\s+replace\\s+)?(temp(?:orary)?\\s+)?(table|view|materialized\\s+view)\\s+(?:if\\s+not\\s+exists\\s+)?([[:alnum:]_.$\"]+)\\s+as\\s+.+$",
    statement,
    perl = TRUE
  )
  create_hit <- regmatches(statement, create_match)[[1]]
  if (length(create_hit) >= 4L) {
    object_type <- if (grepl("view", create_hit[[3]], ignore.case = TRUE)) "Create view" else "Create table"
    return(list(
      title = object_type,
      detail = sql_clean_identifier(create_hit[[4]]),
      name = sql_clean_identifier(create_hit[[4]]),
      path = "",
      is_temporary = nzchar(trim_ws(create_hit[[2]]))
    ))
  }

  insert_match <- regexec(
    "(?is)^insert\\s+into\\s+([[:alnum:]_.$\"]+)\\s+.+$",
    statement,
    perl = TRUE
  )
  insert_hit <- regmatches(statement, insert_match)[[1]]
  if (length(insert_hit) >= 2L) {
    return(list(
      title = "Insert query results into table",
      detail = sql_clean_identifier(insert_hit[[2]]),
      name = sql_clean_identifier(insert_hit[[2]]),
      path = "",
      is_temporary = FALSE
    ))
  }

  copy_match <- regexec(
    "(?is)^copy\\s+(?:\\(.+\\)|[[:alnum:]_.$\"]+)\\s+to\\s+['\"]([^'\"]+)['\"]",
    statement,
    perl = TRUE
  )
  copy_hit <- regmatches(statement, copy_match)[[1]]
  if (length(copy_hit) >= 2L) {
    export_path <- display_path_value(copy_hit[[2]])
    return(list(
      title = "Export query results",
      detail = pretty_path(export_path),
      name = "",
      path = export_path,
      is_temporary = FALSE
    ))
  }

  unload_match <- regexec(
    "(?is)^unload\\s*\\(.+\\)\\s+to\\s+['\"]([^'\"]+)['\"]",
    statement,
    perl = TRUE
  )
  unload_hit <- regmatches(statement, unload_match)[[1]]
  if (length(unload_hit) >= 2L) {
    export_path <- display_path_value(unload_hit[[2]])
    return(list(
      title = "Export query results",
      detail = pretty_path(export_path),
      name = "",
      path = export_path,
      is_temporary = FALSE
    ))
  }

  if (grepl("(?is)^(with|select)\\b", statement, perl = TRUE)) {
    return(list(
      title = "Return query results",
      detail = "return the result set without saving it to a table or file",
      name = "",
      path = "",
      is_temporary = FALSE
    ))
  }

  list(title = "", detail = "", name = "", path = "", is_temporary = FALSE)
}

sql_stage_group_label <- function(name = "", final = FALSE) {
  if (nzchar(trim_ws(name))) {
    return(paste0("Working table: ", sql_clean_identifier(name)))
  }

  if (isTRUE(final)) {
    return("Final query")
  }

  "SQL workflow"
}

sql_is_copy_table_export <- function(statement) {
  grepl(
    "(?is)^copy\\s+(?!\\()([[:alnum:]_.$\"]+)\\s+to\\s+['\"][^'\"]+['\"]",
    compact_ws(statement),
    perl = TRUE
  )
}

sql_extract_sources <- function(query_text) {
  refs <- sql_extract_source_refs(query_text)
  if (length(refs) == 0L) {
    return(character())
  }

  names <- vapply(refs, function(ref) ref$table, character(1))
  cte_names <- sql_cte_names(query_text)
  unique(names[!(tolower(names) %in% tolower(cte_names))])
}

sql_extract_source_refs <- function(query_text) {
  if (!nzchar(trim_ws(query_text))) {
    return(list())
  }

  pattern <- "(?is)\\b(from|join)\\s+((?:\"[^\"]+\"|`[^`]+`|\\[[^\\]]+\\]|[[:alnum:]_.$]+))(?:\\s+(?:as\\s+)?([[:alnum:]_.$]+))?"
  matches <- gregexpr(pattern, query_text, perl = TRUE)
  hits <- regmatches(query_text, matches)[[1]]

  if (length(hits) == 0L) {
    return(list())
  }

  refs <- lapply(hits, function(hit) {
    capture <- regexec(pattern, hit, perl = TRUE)
    parts <- regmatches(hit, capture)[[1]]
    table_name <- if (length(parts) >= 3L) sql_clean_identifier(parts[[3]]) else ""
    alias_name <- if (length(parts) >= 4L) sql_clean_identifier(parts[[4]]) else ""
    list(
      clause = if (length(parts) >= 2L) tolower(trim_ws(parts[[2]])) else "",
      table = table_name,
      alias = alias_name
    )
  })

  cte_names <- sql_cte_names(query_text)
  refs[!(tolower(vapply(refs, function(ref) ref$table, character(1))) %in% tolower(cte_names))]
}

sql_cte_names <- function(query_text) {
  pattern <- "(?i)(?:\\bwith\\b|,)\\s*([[:alnum:]_.$\"]+)\\s+as\\s*\\("
  matches <- gregexpr(pattern, query_text, perl = TRUE)
  hits <- regmatches(query_text, matches)[[1]]

  if (length(hits) == 0L) {
    return(character())
  }

  vapply(hits, function(hit) {
    sub(pattern, "\\1", hit, perl = TRUE)
  }, character(1))
}

sql_extract_join_specs <- function(query_text) {
  pattern <- "(?is)\\b((?:left|right|inner|full|cross)\\s+)?join\\b\\s+((?:\"[^\"]+\"|`[^`]+`|\\[[^\\]]+\\]|[[:alnum:]_.$]+))(?:\\s+(?:as\\s+)?[[:alnum:]_]+)?(?:\\s+on\\s+(.+?))?(?=\\b(?:left|right|inner|full|cross)\\s+join\\b|\\bjoin\\b|\\bwhere\\b|\\bgroup\\s+by\\b|\\bhaving\\b|\\border\\s+by\\b|\\blimit\\b|\\bqualify\\b|\\bunion\\b|$)"
  matches <- gregexpr(pattern, query_text, perl = TRUE)
  hits <- regmatches(query_text, matches)[[1]]

  if (length(hits) == 0L) {
    return(list())
  }

  lapply(hits, function(hit) {
    capture <- regexec(pattern, hit, perl = TRUE)
    parts <- regmatches(hit, capture)[[1]]
    join_type <- if (length(parts) >= 2L && nzchar(trim_ws(parts[[2]]))) toupper(trim_ws(parts[[2]])) else "INNER"
    table_name <- if (length(parts) >= 3L) sql_clean_identifier(parts[[3]]) else ""
    condition <- if (length(parts) >= 4L) compact_ws(parts[[4]]) else ""

    list(type = join_type, table = table_name, condition = condition)
  })
}

sql_extract_clause <- function(query_text, clause, terminators) {
  text <- compact_ws(query_text)
  clause_pattern <- paste0("(?is)\\b", gsub("\\s+", "\\\\s+", trim_ws(clause)), "\\b\\s+")
  start_match <- regexpr(clause_pattern, text, perl = TRUE)
  start <- start_match[[1]]
  if (start < 0L) {
    return("")
  }

  content_start <- start + attr(start_match, "match.length")
  value <- trim_ws(substr(text, content_start, nchar(text)))
  if (!nzchar(value)) {
    return("")
  }

  if (length(terminators) > 0L) {
    end_positions <- vapply(terminators, function(term) {
      term_pattern <- paste0("(?is)\\b", gsub("\\s+", "\\\\s+", trim_ws(term)), "\\b")
      pos <- regexpr(term_pattern, value, perl = TRUE)[[1]]
      if (pos < 0L) Inf else pos
    }, numeric(1))

    first_end <- min(end_positions)
    if (is.finite(first_end)) {
      value <- substr(value, 1L, first_end - 1L)
    }
  }

  compact_ws(value)
}

sql_extract_select_items <- function(query_text) {
  hit <- regexec("(?is)\\bselect\\b\\s+(?:distinct\\s+)?(.+?)\\bfrom\\b", query_text, perl = TRUE)
  capture <- regmatches(query_text, hit)[[1]]

  if (length(capture) < 2L) {
    return(character())
  }

  pieces <- split_top_level(capture[[2]], ",")
  trim_ws(pieces[nzchar(trim_ws(pieces))])
}

sql_query_input_columns <- function(select_items, where_clause = "", group_clause = "", order_clause = "", joins = list()) {
  pieces <- c(
    sql_select_input_column_vector(select_items, keep_wildcards = FALSE),
    sql_clause_columns(where_clause),
    sql_clause_columns(group_clause),
    sql_clause_columns(order_clause),
    sql_clause_columns(paste(vapply(joins, `[[`, character(1), "condition"), collapse = " "))
  )
  sql_column_inventory_text(pieces)
}

sql_select_input_columns <- function(select_items, keep_wildcards = FALSE) {
  sql_column_inventory_text(sql_select_input_column_vector(select_items, keep_wildcards = keep_wildcards))
}

sql_select_input_column_vector <- function(select_items, keep_wildcards = FALSE) {
  if (length(select_items) == 0L) {
    return(character())
  }

  columns <- unlist(lapply(select_items, function(item) {
    sql_select_item_input_columns(item, keep_wildcards = keep_wildcards)
  }), use.names = FALSE)

  sql_unique_trimmed(columns)
}

sql_select_output_columns <- function(select_items) {
  sql_select_inventory(select_items)$output_columns
}

sql_select_output_column_vector <- function(select_items) {
  if (length(select_items) == 0L) {
    return(character())
  }

  outputs <- vapply(select_items, sql_select_item_output_column, character(1))
  sql_unique_trimmed(outputs)
}

sql_select_inventory <- function(select_items, source_column_map = list()) {
  if (length(select_items) == 0L) {
    return(list(output_columns = "", carried_columns = "", created_columns = ""))
  }

  carried <- character()
  created <- character()

  for (item in select_items) {
    item_inventory <- sql_select_item_inventory(item, source_column_map)
    carried <- c(carried, item_inventory$carried)
    created <- c(created, item_inventory$created)
  }

  carried <- sql_unique_trimmed(carried)
  created <- sql_unique_trimmed(created)
  output <- sql_unique_trimmed(c(carried, created))

  list(
    output_columns = sql_column_inventory_text(output),
    carried_columns = sql_column_inventory_text(carried),
    created_columns = sql_column_inventory_text(created)
  )
}

sql_select_item_inventory <- function(item, source_column_map = list()) {
  item <- trim_ws(item)
  if (!nzchar(item)) {
    return(list(carried = character(), created = character()))
  }

  if (identical(item, "*")) {
    expanded <- sql_expand_star_columns(source_column_map)
    if (length(expanded) > 0L) {
      return(list(carried = expanded, created = character()))
    }
    return(list(carried = "all selected columns", created = character()))
  }

  if (grepl("^[[:alnum:]_.$]+\\.\\*$", item, perl = TRUE)) {
    source_key <- sub("\\.\\*$", "", item, perl = TRUE)
    expanded <- sql_expand_star_columns(source_column_map, source_key)
    if (length(expanded) > 0L) {
      return(list(carried = expanded, created = character()))
    }
    return(list(carried = paste0("all columns from ", source_key), created = character()))
  }

  alias_match <- regexec("(?is)^(.+?)\\s+as\\s+([[:alnum:]_.$\"]+)$", item, perl = TRUE)
  alias_hit <- regmatches(item, alias_match)[[1]]
  if (length(alias_hit) >= 3L) {
    expression_text <- trim_ws(alias_hit[[2]])
    alias_name <- sql_clean_identifier(alias_hit[[3]])
    if (sql_is_plain_column_reference(expression_text)) {
      return(list(carried = alias_name, created = character()))
    }
    return(list(carried = character(), created = alias_name))
  }

  if (sql_is_plain_column_reference(item)) {
    return(list(carried = sql_clean_identifier(sub("^.*\\.", "", item, perl = TRUE)), created = character()))
  }

  if (sql_is_feature_select_item(item)) {
    return(list(carried = character(), created = sql_select_item_output_column(item)))
  }

  list(carried = sql_select_item_output_column(item), created = character())
}

sql_is_plain_column_reference <- function(text) {
  grepl("^[[:alnum:]_.$]+$", trim_ws(text), perl = TRUE)
}

sql_expand_star_columns <- function(source_column_map, source_key = NULL) {
  if (length(source_column_map) == 0L) {
    return(character())
  }

  if (is.null(source_key)) {
    return(sql_unique_trimmed(unlist(unname(source_column_map), use.names = FALSE)))
  }

  matched <- source_column_map[[source_key]]
  if (is.null(matched) || length(matched) == 0L) {
    return(character())
  }

  sql_unique_trimmed(matched)
}

sql_resolve_query_inventory <- function(stage, source_metadata = list()) {
  source_map <- sql_source_column_map(stage$source_refs %||% list(), source_metadata)
  select_items <- stage$select_items %||% character()
  resolved <- sql_select_inventory(select_items, source_map)

  if (!nzchar(resolved$output_columns)) {
    resolved$output_columns <- stage$query_output_columns %||% ""
  }
  if (!nzchar(resolved$carried_columns)) {
    resolved$carried_columns <- stage$query_carried_columns %||% ""
  }
  if (!nzchar(resolved$created_columns)) {
    resolved$created_columns <- stage$query_created_columns %||% ""
  }

  resolved
}

sql_source_column_map <- function(source_refs, source_metadata) {
  if (length(source_refs) == 0L || length(source_metadata) == 0L) {
    return(list())
  }

  map <- list()
  for (index in seq_along(source_refs)) {
    ref <- source_refs[[index]]
    metadata <- source_metadata[[index]]
    if (is.null(metadata)) {
      next
    }

    columns <- sql_parse_column_inventory(metadata$output_columns %||% "")
    if (length(columns) == 0L) {
      next
    }

    keys <- unique(c(ref$alias, ref$table, sql_identifier_tail(ref$table)))
    for (key in keys[nzchar(keys)]) {
      existing <- map[[key]]
      map[[key]] <- sql_unique_trimmed(c(existing %||% character(), columns))
    }
  }

  map
}

sql_parse_column_inventory <- function(text) {
  text <- trim_ws(text)
  if (!nzchar(text)) {
    return(character())
  }

  sql_unique_trimmed(strsplit(text, "\\s*,\\s*", perl = TRUE)[[1]])
}

sql_identifier_tail <- function(name) {
  name <- sql_clean_identifier(name)
  sub("^.*\\.", "", name, perl = TRUE)
}

sql_select_item_input_columns <- function(item, keep_wildcards = FALSE) {
  item <- trim_ws(item)
  if (!nzchar(item)) {
    return(character())
  }

  alias_match <- regexec("(?is)^(.+?)\\s+as\\s+([[:alnum:]_.$\"]+)$", item, perl = TRUE)
  alias_hit <- regmatches(item, alias_match)[[1]]
  expression_text <- if (length(alias_hit) >= 3L) trim_ws(alias_hit[[2]]) else item

  sql_extract_expression_columns(expression_text, keep_wildcards = keep_wildcards)
}

sql_select_item_output_column <- function(item) {
  item <- trim_ws(item)
  if (!nzchar(item)) {
    return("")
  }

  if (identical(item, "*")) {
    return("all selected columns")
  }

  if (grepl("\\.\\*$", item, perl = TRUE)) {
    return(paste0("all columns from ", sub("\\.\\*$", "", item, perl = TRUE)))
  }

  alias_match <- regexec("(?is)^(.+?)\\s+as\\s+([[:alnum:]_.$\"]+)$", item, perl = TRUE)
  alias_hit <- regmatches(item, alias_match)[[1]]
  if (length(alias_hit) >= 3L) {
    return(sql_clean_identifier(alias_hit[[3]]))
  }

  if (grepl("^[[:alnum:]_.$]+$", item, perl = TRUE)) {
    return(sql_clean_identifier(sub("^.*\\.", "", item, perl = TRUE)))
  }

  humanize_sql_expression(item)
}

sql_summarize_input_columns <- function(select_items, group_clause = "") {
  pieces <- c(
    sql_clause_columns(group_clause),
    sql_select_input_column_vector(select_items, keep_wildcards = FALSE)
  )
  sql_column_inventory_text(pieces)
}

sql_join_input_columns <- function(joins) {
  if (length(joins) == 0L) {
    return("")
  }

  columns <- unlist(lapply(joins, function(join) {
    sql_clause_columns(join$condition)
  }), use.names = FALSE)

  sql_column_inventory_text(columns)
}

sql_clause_column_inventory <- function(clause_text) {
  sql_column_inventory_text(sql_clause_columns(clause_text))
}

sql_clause_columns <- function(clause_text) {
  if (!nzchar(trim_ws(clause_text))) {
    return(character())
  }

  sql_extract_expression_columns(clause_text, keep_wildcards = FALSE)
}

sql_extract_expression_columns <- function(text, keep_wildcards = FALSE) {
  text <- trim_ws(text)
  if (!nzchar(text)) {
    return(character())
  }

  stripped <- gsub("(?s)'(?:''|[^'])*'", " ", text, perl = TRUE)
  stripped <- gsub('(?s)"(?:""|[^"])*"', " ", stripped, perl = TRUE)

  pattern <- "(?<![[:alnum:]_$.])(?:[[:alpha:]_][[:alnum:]_$]*\\.)?(?:[[:alpha:]_][[:alnum:]_$]*|\\*)(?![[:alnum:]_$.])"
  matches <- gregexpr(pattern, stripped, perl = TRUE)
  hits <- regmatches(stripped, matches)[[1]]

  if (length(hits) == 0L) {
    return(character())
  }

  reserved <- sql_reserved_words()
  columns <- hits[vapply(hits, function(hit) {
    token <- trim_ws(hit)
    if (!nzchar(token)) {
      return(FALSE)
    }

    if (!isTRUE(keep_wildcards) && (identical(token, "*") || grepl("\\.\\*$", token, perl = TRUE))) {
      return(FALSE)
    }

    token_lower <- tolower(token)
    if (token_lower %in% reserved) {
      return(FALSE)
    }

    !grepl(paste0("(?i)\\b", regex_escape(token), "\\s*\\("), stripped, perl = TRUE, useBytes = TRUE)
  }, logical(1))]

  sql_unique_trimmed(columns)
}

sql_reserved_words <- function() {
  c(
    "select", "distinct", "from", "join", "left", "right", "inner", "full", "cross",
    "on", "where", "group", "by", "having", "order", "asc", "desc", "limit", "qualify",
    "union", "all", "as", "and", "or", "not", "in", "is", "null", "case", "when",
    "then", "else", "end", "with", "create", "table", "view", "materialized", "temporary",
    "temp", "insert", "into", "copy", "to", "unload", "over", "partition", "rows",
    "range", "current", "row", "preceding", "following", "true", "false", "date"
  )
}

sql_column_inventory_text <- function(columns) {
  columns <- sql_unique_trimmed(columns)
  if (length(columns) == 0L) {
    return("")
  }

  paste(columns, collapse = ", ")
}

sql_unique_trimmed <- function(values) {
  values <- trim_ws(values)
  values <- values[nzchar(values)]
  if (length(values) == 0L) {
    return(character())
  }

  values[!duplicated(tolower(values))]
}

regex_escape <- function(text) {
  gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", text, perl = TRUE)
}

sql_has_aggregates <- function(select_items) {
  if (length(select_items) == 0L) {
    return(FALSE)
  }

  any(grepl("(?i)\\b(sum|avg|count|min|max|string_agg|array_agg)\\s*\\(", select_items, perl = TRUE))
}

sql_join_detail <- function(sources, joins) {
  if (length(sources) == 0L) {
    return("combine source tables")
  }

  if (length(joins) == 0L) {
    return(paste0("combine ", sources[[1]], " with ", paste(sources[-1], collapse = ", ")))
  }

  left_source <- sources[[1]]
  phrases <- character()

  for (index in seq_along(joins)) {
    join <- joins[[index]]
    keep_note <- sql_join_keep_note(join$type, left_source)
    condition_text <- if (nzchar(join$condition)) paste0(" on ", humanize_sql_expression(join$condition)) else ""

    phrases <- c(
      phrases,
      paste0(
        "use a ",
        join$type,
        " JOIN to bring ",
        join$table,
        " into ",
        left_source,
        condition_text,
        keep_note
      )
    )

    left_source <- "the current result"
  }

  paste(phrases, collapse = "; ")
}

sql_join_keep_note <- function(join_type, left_source) {
  join_type <- toupper(trim_ws(join_type))

  if (identical(join_type, "LEFT")) {
    return(paste0(", keeping all rows from ", left_source, " even when matches are missing"))
  }

  if (identical(join_type, "RIGHT")) {
    return(", keeping all rows from the joined table even when matches are missing")
  }

  if (identical(join_type, "FULL")) {
    return(", keeping rows even when one side does not match")
  }

  if (identical(join_type, "INNER")) {
    return(", keeping only rows that match on both sides")
  }

  if (identical(join_type, "CROSS")) {
    return(", pairing every row from both sides")
  }

  ""
}

sql_distinct_detail <- function(select_items) {
  if (length(select_items) == 0L || any(trim_ws(select_items) == "*")) {
    return("keep unique rows from the query result")
  }

  paste0(
    "keep unique combinations of ",
    paste(vapply(select_items, humanize_sql_expression, character(1)), collapse = ", ")
  )
}

sql_summarize_detail <- function(select_items, group_clause = "") {
  details <- character()

  if (nzchar(group_clause)) {
    group_items <- split_top_level(group_clause, ",")
    group_items <- trim_ws(group_items[nzchar(trim_ws(group_items))])
    if (length(group_items) > 0L) {
      details <- c(
        details,
        paste0(
          "group rows by ",
          paste(vapply(group_items, humanize_sql_expression, character(1)), collapse = ", ")
        )
      )
    }
  }

  metric_items <- select_items[grepl("(?i)\\b(sum|avg|count|min|max|string_agg|array_agg)\\s*\\(", select_items, perl = TRUE)]
  if (length(metric_items) > 0L) {
    details <- c(
      details,
      paste0(
        "calculate summary metrics: ",
        paste(vapply(metric_items, sql_select_item_detail, character(1)), collapse = "; ")
      )
    )
  }

  if (length(details) == 0L) {
    return("summarize the selected records")
  }

  paste(details, collapse = "; ")
}

sql_order_detail <- function(order_clause) {
  items <- split_top_level(order_clause, ",")
  items <- trim_ws(items[nzchar(trim_ws(items))])
  if (length(items) == 0L) {
    return("")
  }

  detail_items <- vapply(items, function(item) {
    clean <- trim_ws(item)
    if (grepl("(?i)\\s+desc$", clean, perl = TRUE)) {
      return(paste0(humanize_sql_expression(sub("(?i)\\s+desc$", "", clean, perl = TRUE)), " in descending order"))
    }
    if (grepl("(?i)\\s+asc$", clean, perl = TRUE)) {
      return(paste0(humanize_sql_expression(sub("(?i)\\s+asc$", "", clean, perl = TRUE)), " in ascending order"))
    }
    humanize_sql_expression(clean)
  }, character(1))

  paste0("sort rows by ", paste(detail_items, collapse = ", "))
}

sql_select_detail <- function(select_items) {
  if (length(select_items) == 0L || any(trim_ws(select_items) == "*")) {
    return("return all columns from the selected source tables")
  }

  preview <- utils::head(select_items, 5L)
  detail <- paste(vapply(preview, sql_select_item_detail, character(1)), collapse = "; ")
  if (length(select_items) > length(preview)) {
    detail <- paste0(detail, "; plus ", length(select_items) - length(preview), " more selected fields")
  }
  detail
}

sql_select_item_detail <- function(item) {
  item <- trim_ws(item)
  alias_match <- regexec("(?is)^(.+?)\\s+as\\s+([[:alnum:]_.$\"]+)$", item, perl = TRUE)
  alias_hit <- regmatches(item, alias_match)[[1]]

  if (length(alias_hit) >= 3L) {
    alias_name <- sql_clean_identifier(alias_hit[[3]])
    expression_text <- trim_ws(alias_hit[[2]])
    return(paste0(alias_name, " = ", sql_humanize_alias_expression(alias_name, expression_text)))
  }

  humanize_sql_expression(item)
}

sql_humanize_alias_expression <- function(alias_name, expression_text) {
  detail <- humanize_sql_expression(expression_text)

  if (
    grepl("^revenue_gap$", alias_name, ignore.case = TRUE) &&
    grepl("-", expression_text, fixed = TRUE) &&
    grepl("target", expression_text, ignore.case = TRUE, perl = TRUE)
  ) {
    return(paste0(detail, " (negative means below target; positive means above target)"))
  }

  detail
}

sql_feature_detail <- function(select_items) {
  if (length(select_items) == 0L) {
    return("")
  }

  computed_items <- select_items[vapply(select_items, sql_is_feature_select_item, logical(1))]
  if (length(computed_items) == 0L) {
    return("")
  }

  paste(vapply(computed_items, sql_select_item_detail, character(1)), collapse = "; ")
}

sql_is_feature_select_item <- function(item) {
  item <- trim_ws(item)
  if (grepl("(?i)\\b(sum|avg|count|min|max|string_agg|array_agg)\\s*\\(", item, perl = TRUE)) {
    return(FALSE)
  }

  alias_match <- regexec("(?is)^(.+?)\\s+as\\s+([[:alnum:]_.$\"]+)$", item, perl = TRUE)
  alias_hit <- regmatches(item, alias_match)[[1]]

  if (length(alias_hit) >= 3L) {
    expr <- trim_ws(alias_hit[[2]])
    return(!grepl("^[[:alnum:]_.$]+$", expr))
  }

  grepl("\\(|\\+|-|\\*|/|\\bcase\\b", item, ignore.case = TRUE, perl = TRUE)
}

sql_group_filter_detail <- function(condition_text, group_detail = "") {
  group_label <- sql_group_dimension_label(group_detail)
  condition_text <- humanize_sql_expression(condition_text)

  if (nzchar(group_label)) {
    return(paste0("keep only ", group_label, " where ", condition_text))
  }

  paste0("keep only grouped results where ", condition_text)
}

sql_group_dimension_label <- function(group_detail) {
  group_detail <- trim_ws(group_detail)
  if (!nzchar(group_detail)) {
    return("")
  }

  match <- regexec("^group rows by\\s+([^;]+)", group_detail, perl = TRUE)
  capture <- regmatches(group_detail, match)[[1]]
  if (length(capture) < 2L) {
    return("")
  }

  paste0(capture[[2]], " groups")
}

humanize_sql_expression <- function(text) {
  text <- trim_ws(text)
  if (!nzchar(text)) {
    return(text)
  }

  text <- gsub(
    "(?i)coalesce\\(\\s*nullif\\(([^,]+),\\s*''\\)\\s*,\\s*'([^']+)'\\s*\\)",
    "if \\1 is blank use \\2",
    text,
    perl = TRUE
  )
  text <- gsub("(?i)date\\s+'([^']+)'", "date \\1", text, perl = TRUE)
  text <- gsub("(?i)date_trunc\\(\\s*'([^']+)'\\s*,\\s*([^)]+)\\)", "truncate \\2 to \\1", text, perl = TRUE)
  text <- gsub("(?i)upper\\(trim\\(([^)]+)\\)\\)", "uppercase and trim spaces from \\1", text, perl = TRUE)
  text <- gsub("(?i)nullif\\(([^,]+),\\s*''\\)", "if \\1 is blank treat it as missing", text, perl = TRUE)
  text <- gsub("(?i)coalesce\\(([^,]+),\\s*'([^']+)'\\)", "if \\1 is missing use \\2", text, perl = TRUE)

  replacements <- c(
    "(?i)count\\(\\s*distinct\\s+([^)]+)\\)" = "number of distinct \\1",
    "(?i)count\\(\\s*\\*\\s*\\)" = "row count",
    "(?i)count\\(([^)]+)\\)" = "count of \\1",
    "(?i)avg\\(([^)]+)\\)" = "average of \\1",
    "(?i)sum\\(([^)]+)\\)" = "total \\1",
    "(?i)min\\(([^)]+)\\)" = "minimum \\1",
    "(?i)max\\(([^)]+)\\)" = "maximum \\1",
    "(?i)coalesce\\(([^,]+),\\s*([^)]+)\\)" = "if \\1 is missing use \\2",
    "(?i)([[:alnum:]_.$]+)\\s+is\\s+not\\s+null" = "\\1 is present",
    "(?i)([[:alnum:]_.$]+)\\s+is\\s+null" = "\\1 is missing"
  )

  for (pattern in names(replacements)) {
    text <- gsub(pattern, replacements[[pattern]], text, perl = TRUE)
  }

  text <- gsub("<>", " does not equal ", text, fixed = TRUE)
  text <- gsub("!=", " does not equal ", text, fixed = TRUE)
  text <- gsub(">=", " is at least ", text, fixed = TRUE)
  text <- gsub("<=", " is at most ", text, fixed = TRUE)
  text <- gsub("=", " equals ", text, fixed = TRUE)
  text <- gsub(">", " is greater than ", text, fixed = TRUE)
  text <- gsub("<", " is less than ", text, fixed = TRUE)
  text <- gsub("(?i)\\band\\b", " and ", text, perl = TRUE)
  text <- gsub("(?i)\\bor\\b", " or ", text, perl = TRUE)
  text <- gsub("(?i)\\bnot\\s+in\\b", " is not in ", text, perl = TRUE)
  text <- gsub("(?i)\\bin\\b", " is in ", text, perl = TRUE)
  text <- gsub("\\s+", " ", text)
  trim_ws(text)
}

sql_clean_identifier <- function(name) {
  name <- trim_ws(name)
  name <- gsub('^"|"$', "", name)
  name <- gsub("^`|`$", "", name)
  name <- gsub("^\\[|\\]$", "", name)
  trim_ws(name)
}

node_style <- function(kind) {
  switch(
    kind,
    input_file = list(shape = "note", fillcolor = "#f4df9b", color = "#b9911f", fontcolor = "#5c4710", style = "filled", penwidth = "1.5"),
    source_table = list(shape = "cylinder", fillcolor = "#f8efc8", color = "#b9911f", fontcolor = "#5c4710", style = "filled", penwidth = "1.5"),
    output_file = list(shape = "note", fillcolor = "#f5d6c6", color = "#b9653d", fontcolor = "#66321b", style = "filled", penwidth = "1.5"),
    dataset = list(shape = "cylinder", fillcolor = "#d9e8f5", color = "#5a88ae", fontcolor = "#24445d", style = "filled", penwidth = "1.5"),
    data_table = list(shape = "cylinder", fillcolor = "#d9e8f5", color = "#5a88ae", fontcolor = "#24445d", style = "filled", penwidth = "1.5"),
    data_frame = list(shape = "cylinder", fillcolor = "#d9e8f5", color = "#5a88ae", fontcolor = "#24445d", style = "filled", penwidth = "1.5"),
    generic_object = list(shape = "box", fillcolor = "#dce7f1", color = "#5a88ae", fontcolor = "#24445d", style = "rounded,filled", penwidth = "1.5"),
    matrix_object = list(shape = "box3d", fillcolor = "#dbe6f7", color = "#5f7fa8", fontcolor = "#24445d", style = "filled", penwidth = "1.5"),
    vector_object = list(shape = "ellipse", fillcolor = "#f5e4c7", color = "#b5883b", fontcolor = "#5b4212", style = "filled", penwidth = "1.5"),
    model_object = list(shape = "hexagon", fillcolor = "#e7dcf6", color = "#7c62a8", fontcolor = "#42305e", style = "filled", penwidth = "1.5"),
    plot_object = list(shape = "ellipse", fillcolor = "#d8efe7", color = "#4f9078", fontcolor = "#1f5041", style = "filled", penwidth = "1.5"),
    output_table = list(shape = "cylinder", fillcolor = "#f3ddcf", color = "#b9653d", fontcolor = "#66321b", style = "filled", penwidth = "1.5"),
    input_step = list(shape = "box", fillcolor = "#f8efc8", color = "#b9911f", fontcolor = "#5c4710", style = "rounded,filled", penwidth = "1.5"),
    output_step = list(shape = "box", fillcolor = "#f3ddcf", color = "#b9653d", fontcolor = "#66321b", style = "rounded,filled", penwidth = "1.5"),
    transform_step = list(shape = "box", fillcolor = "#dbeac8", color = "#72984f", fontcolor = "#2f4d19", style = "rounded,filled", penwidth = "1.5"),
    list(shape = "box", fillcolor = "#e5e7eb", color = "#6b7280", fontcolor = "#111827", style = "filled", penwidth = "1.2")
  )
}

edge_style <- function(type) {
  switch(
    type,
    reads = list(color = "#b9911f", penwidth = "1.5"),
    writes = list(color = "#b9653d", penwidth = "1.5"),
    creates = list(color = "#5a88ae", penwidth = "1.6"),
    copies = list(color = "#7c8aa5", penwidth = "1.2"),
    flows = list(color = "#72984f", penwidth = "1.5"),
    list(color = "#7c8aa5", penwidth = "1.2")
  )
}

plural_suffix <- function(n) {
  if (identical(as.integer(n), 1L)) "" else "s"
}

tolower_first <- function(text) {
  if (!nzchar(text)) {
    return(text)
  }
  paste0(tolower(substr(text, 1, 1)), substring(text, 2))
}

html_escape <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  text
}

escape_xml <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  text <- gsub("'", "&apos;", text, fixed = TRUE)
  text
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

escape_dot <- function(text) {
  text <- gsub("\\\\", "\\\\\\\\", text)
  text <- gsub("'", "`", text, fixed = TRUE)
  text <- gsub('"', '\\\\"', text)
  gsub("\n", "\\\\n", text)
}
