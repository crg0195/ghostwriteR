ghostwriter_graph_impl <- function(x) {
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
    if ("analysis_step" %in% nodes$kind) build_legend_node("legend_analysis", "Analysis", "analysis_step") else character(),
    output_nodes
  )

  legend_chain <- paste(
    c(source_node_ids, object_node_ids, "legend_transform", if ("analysis_step" %in% nodes$kind) "legend_analysis" else character(), output_node_ids),
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
    analysis_step = list(shape = "box", fillcolor = "#e5ebf4", color = "#6f7f96", fontcolor = "#28323f", style = "rounded,filled", penwidth = "1.5"),
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
