
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
  if (grepl("^(with|select|create|insert|copy|unload|delete|update|merge|truncate|drop|alter|begin|declare|execute|call)\\b", first_line, perl = TRUE)) {
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
    "read.csv", "read.csv2", "readRDS", "readr::read_csv", "readr::read_tsv",
    "readr::read_delim", "readr::read_fwf", "readxl::read_excel",
    "openxlsx::read.xlsx", "openxlsx2::read_xlsx",
    "data.table::fread", "DBI::dbGetQuery", "DBI::dbReadTable",
    "jsonlite::fromJSON", "fromJSON",
    "haven::read_sas", "haven::read_spss", "haven::read_sav",
    "haven::read_dta", "haven::read_stata",
    "arrow::read_parquet", "arrow::read_feather", "arrow::read_csv_arrow",
    "vroom::vroom"
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
    superseded_by = character(),
    code = character(),
    explanation = character(),
    narrative = character(),
    stringsAsFactors = FALSE
  )

  node_index <- 0L
  step_index <- 0L
  node_keys <- character()
  node_ids <- character()
  file_collections <- list()
  import_lists <- list()
  current_code_chunk <- ""

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
      analysis_step = 4L,
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

  add_step_record <- function(kind, title, detail = "", source = "", output = "", target_path = "", group = "", input_columns = "", output_columns = "", carried_columns = "", created_columns = "", superseded_by = "", code = current_code_chunk, explanation = "") {
    step_index <<- step_index + 1L
    narrative <- build_step_narrative(kind, title, detail, source, output, target_path)
    if (!nzchar(explanation)) {
      explanation <- build_code_explanation(code, title, detail, language = "r")
    }

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
        superseded_by = superseded_by,
        code = code,
        explanation = explanation,
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

  file_collection_label <- function(pattern) {
    pattern <- strip_wrapping_quotes(pattern %||% "")
    pattern_lower <- tolower(pattern)

    if (!nzchar(pattern_lower)) {
      return("source data files")
    }

    if (grepl("json", pattern_lower, fixed = TRUE)) {
      return("JSON files")
    }

    if (grepl("csv", pattern_lower, fixed = TRUE)) {
      return("CSV files")
    }

    if (grepl("xlsx", pattern_lower, fixed = TRUE) || grepl("xls", pattern_lower, fixed = TRUE)) {
      return("Excel files")
    }

    paste0("files matching ", pattern)
  }

  describe_file_discovery <- function(text) {
    fn <- plain_fn(leading_call(text))
    if (!fn %in% c("dir", "list.files")) {
      return(NULL)
    }

    args <- call_args(text)
    unnamed_args <- args[!grepl("^[[:alnum:]_.]+\\s*=", args)]
    path_arg <- named_arg_value(args, "path")
    if (!nzchar(path_arg) && length(unnamed_args) > 0L) {
      path_arg <- unnamed_args[[1]]
    }

    pattern_arg <- named_arg_value(args, "pattern")
    path_value <- display_path_value(extract_string_literal(path_arg))
    path_label <- if (!is.null(path_value) && nzchar(path_value)) paste0("scan ", path_value) else "scan the selected folder"
    detail <- paste0(path_label, " for ", file_collection_label(pattern_arg))

    if (identical(tolower(strip_wrapping_quotes(named_arg_value(args, "full.names"))), "true")) {
      detail <- paste0(detail, " and keep the full file paths")
    }

    list(
      title = "Discover source data files",
      detail = detail,
      path = path_value %||% "",
      pattern = strip_wrapping_quotes(pattern_arg),
      object_kind = "vector_object"
    )
  }

  describe_import_loop <- function(text) {
    text <- compact_ws(text)
    if (!grepl("^for\\s*\\(", text, perl = TRUE)) {
      return(NULL)
    }

    loop_match <- regexec("^for\\s*\\(\\s*([[:alnum:]_.]+)\\s+in\\s+seq_along\\(([^)]+)\\)\\s*\\)\\s*\\{\\s*(.+)\\s*\\}$", text, perl = TRUE)
    loop_hit <- regmatches(text, loop_match)[[1]]

    if (length(loop_hit) < 4L) {
      loop_match <- regexec("^for\\s*\\(\\s*([[:alnum:]_.]+)\\s+in\\s+([[:alnum:]_.]+)\\s*\\)\\s*\\{\\s*(.+)\\s*\\}$", text, perl = TRUE)
      loop_hit <- regmatches(text, loop_match)[[1]]
      if (length(loop_hit) < 4L) {
        return(NULL)
      }
    }

    index_name <- trim_ws(loop_hit[[2]])
    source_collection <- trim_ws(loop_hit[[3]])
    body <- trim_ws(loop_hit[[4]])

    body_match <- regexec("^([[:alnum:]_.]+)\\s*\\[\\[[^]]+\\]\\]\\s*<-\\s*(.+)$", body, perl = TRUE)
    body_hit <- regmatches(body, body_match)[[1]]
    if (length(body_hit) < 3L) {
      return(NULL)
    }

    output_name <- trim_ws(body_hit[[2]])
    rhs <- trim_ws(body_hit[[3]])
    reader_fn <- plain_fn(leading_call(rhs))
    if (!reader_fn %in% c("fromJSON", "read.csv", "read_csv", "read_tsv", "read_delim", "fread", "read_excel", "readRDS")) {
      return(NULL)
    }

    args <- call_args(rhs)
    if (length(args) == 0L) {
      return(NULL)
    }

    source_arg <- trim_ws(args[[1]])
    indexed_collection <- sub("^([[:alnum:]_.]+)\\s*\\[.*$", "\\1", source_arg, perl = TRUE)
    if (!nzchar(indexed_collection) || identical(indexed_collection, source_arg)) {
      if (identical(source_arg, index_name)) {
        indexed_collection <- source_collection
      } else if (is_plain_name(source_arg)) {
        indexed_collection <- source_arg
      } else {
        indexed_collection <- source_collection
      }
    }

    collection_info <- file_collections[[indexed_collection]]
    collection_label <- if (!is.null(collection_info)) file_collection_label(collection_info$pattern) else "source files"

    detail <- paste0("read the ", collection_label, " listed in `", indexed_collection, "` into a list of imported records")
    if (identical(reader_fn, "fromJSON") && grepl("flatten\\s*=\\s*TRUE", rhs, perl = TRUE)) {
      detail <- paste0(detail, "; flatten nested fields where possible")
    }

    list(
      title = if (identical(reader_fn, "fromJSON")) "Load JSON files" else paste("Load", collection_label),
      detail = detail,
      source = indexed_collection,
      output = output_name,
      object_kind = "generic_object"
    )
  }

  chart_step_titles <- c("Create chart", "Add chart layer", "Label chart", "Style chart", "Set chart defaults", "Arrange charts")
  prep_step_titles <- c(
    "Convert to data frame", "Add or change columns", "Filter rows", "Keep selected columns",
    "Sort rows", "Rename columns", "Remove duplicates", "Reshape to long format",
    "Reshape to wide format"
  )

  is_chart_info <- function(info) {
    identical(info$object_kind, "plot_object") || info$title %in% chart_step_titles
  }

  is_prep_info <- function(info) {
    info$title %in% prep_step_titles
  }

  combine_step_details <- function(infos) {
    details <- unique(trim_ws(vapply(infos, function(info) info$detail %||% "", character(1))))
    details <- details[nzchar(details)]
    paste(details, collapse = "; ")
  }

  describe_chart_pipeline <- function(infos) {
    create_info <- Filter(function(info) identical(info$title, "Create chart"), infos)
    layer_info <- Filter(function(info) identical(info$title, "Add chart layer"), infos)
    label_info <- Filter(function(info) identical(info$title, "Label chart"), infos)

    details <- c(
      if (length(create_info) > 0L) create_info[[1]]$detail else character(),
      if (length(layer_info) > 0L) layer_info[[1]]$detail else character(),
      if (length(label_info) > 0L) label_info[[1]]$detail else character()
    )
    details <- unique(trim_ws(details))
    details <- details[nzchar(details)]

    list(
      title = "Create chart",
      detail = paste(details, collapse = "; "),
      object_kind = "plot_object"
    )
  }

  for (index in seq_along(exprs)) {
    current_code_chunk <- expression_code(exprs[[index]], srcrefs[[index]])
    statement <- expression_text(exprs[[index]], srcrefs[[index]])
    if (!nzchar(statement)) {
      next
    }

    compound_assign <- regexec("^([[:alnum:]_.]+)\\s*%<>%\\s*(.+)$", statement, perl = TRUE)
    compound_hit <- regmatches(statement, compound_assign)[[1]]

    left_assign <- regexec("^([[:alnum:]_.]+)\\s*(<<?-|=)\\s*(.+)$", statement)
    left_hit <- regmatches(statement, left_assign)[[1]]

    right_assign <- regexec("^(.+)\\s*(->|->>)\\s*([[:alnum:]_.]+)$", statement)
    right_hit <- regmatches(statement, right_assign)[[1]]

    if (length(compound_hit) > 0) {
      lhs_name <- trim_ws(compound_hit[2])
      rhs <- trim_ws(compound_hit[3])
      lhs_id <- object_node(lhs_name)
      pipe_parts <- split_pipe(rhs)
      current_id <- lhs_id
      current_source <- lhs_name

      for (part_index in seq_along(pipe_parts)) {
        part <- pipe_parts[[part_index]]
        info <- describe_transform(part, source_in_call = FALSE)
        step_id <- add_step_record(
          kind = "transform",
          title = info$title,
          detail = info$detail,
          source = current_source,
          output = if (part_index == length(pipe_parts)) lhs_name else ""
        )
        add_edge(current_id, step_id, "flows")
        current_id <- step_id
        current_source <- if (part_index == length(pipe_parts)) lhs_name else "the result from the previous step"
      }

      final_kind <- if (length(pipe_parts) > 0L) describe_transform(pipe_parts[[length(pipe_parts)]], source_in_call = FALSE)$object_kind else "generic_object"
      lhs_id <- object_node(lhs_name, final_kind)
      add_edge(current_id, lhs_id, "updates")
      next
    }

    if (length(left_hit) > 0) {
      lhs_name <- trim_ws(left_hit[2])
      rhs <- trim_ws(left_hit[4])
      lhs_id <- object_node(lhs_name)

      discovery_info <- describe_file_discovery(rhs)
      if (!is.null(discovery_info)) {
        lhs_id <- object_node(lhs_name, discovery_info$object_kind)
        step_id <- add_step_record(
          kind = "input",
          title = discovery_info$title,
          detail = discovery_info$detail,
          output = lhs_name,
          target_path = discovery_info$path
        )
        add_edge(step_id, lhs_id, "creates")
        file_collections[[lhs_name]] <- discovery_info
        next
      }

      pipe_parts <- split_pipe(rhs)
      if (length(pipe_parts) > 1) {
        source_info <- source_from_expr(pipe_parts[[1]])
        current_id <- source_info$node_id
        current_source <- if (nzchar(source_info$source_label)) source_info$source_label else label_from_expr(pipe_parts[[1]])
        tail_parts <- pipe_parts[-1]

        tail_infos <- lapply(tail_parts, function(part) describe_transform(part, source_in_call = FALSE))
        chart_start <- which(vapply(tail_infos, is_chart_info, logical(1)))

        if (length(tail_infos) > 1L && length(chart_start) == 0L && all(vapply(tail_infos, is_prep_info, logical(1)))) {
          combined_info <- list(
            title = "Prepare data frame",
            detail = combine_step_details(tail_infos),
            object_kind = tail_infos[[length(tail_infos)]]$object_kind
          )
          lhs_id <- object_node(lhs_name, combined_info$object_kind)
          step_id <- add_step_record(
            kind = "transform",
            title = combined_info$title,
            detail = combined_info$detail,
            source = current_source,
            output = lhs_name
          )
          add_edge(current_id, step_id, "flows")
          add_edge(step_id, lhs_id, "creates")
          next
        }

        chart_index <- if (length(chart_start) > 0L) chart_start[[1]] else NA_integer_
        pre_chart_count <- if (is.na(chart_index)) length(tail_infos) else chart_index - 1L

        if (pre_chart_count > 0L) {
          for (part_index in seq_len(pre_chart_count)) {
            info <- tail_infos[[part_index]]
            output_name <- if (is.na(chart_index) && part_index == length(tail_infos)) lhs_name else ""
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
        }

        if (!is.na(chart_index)) {
          chart_info <- describe_chart_pipeline(tail_infos[chart_index:length(tail_infos)])
          lhs_id <- object_node(lhs_name, chart_info$object_kind)
          step_id <- add_step_record(
            kind = "transform",
            title = chart_info$title,
            detail = chart_info$detail,
            source = current_source,
            output = lhs_name
          )
          add_edge(current_id, step_id, "flows")
          add_edge(step_id, lhs_id, "creates")
          next
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

    loop_info <- describe_import_loop(statement)
    if (!is.null(loop_info)) {
      output_id <- object_node(loop_info$output, loop_info$object_kind)
      source_id <- object_node(loop_info$source, "vector_object")
      step_id <- add_step_record(
        kind = "input",
        title = loop_info$title,
        detail = loop_info$detail,
        source = loop_info$source,
        output = loop_info$output
      )
      add_edge(source_id, step_id, "reads")
      add_edge(step_id, output_id, "creates")
      import_lists[[loop_info$output]] <- loop_info
      next
    }

    pipe_parts <- split_pipe(statement)
    if (length(pipe_parts) > 1L) {
      source_info <- source_from_expr(pipe_parts[[1]])
      current_id <- source_info$node_id
      current_source <- if (nzchar(source_info$source_label)) source_info$source_label else label_from_expr(pipe_parts[[1]])
      tail_parts <- pipe_parts[-1]
      tail_infos <- lapply(tail_parts, function(part) describe_transform(part, source_in_call = FALSE))
      chart_start <- which(vapply(tail_infos, is_chart_info, logical(1)))
      info <- if (length(chart_start) > 0L) {
        describe_chart_pipeline(tail_infos[chart_start[[1]]:length(tail_infos)])
      } else {
        list(
          title = "Inspect intermediate results",
          detail = combine_step_details(tail_infos),
          object_kind = "generic_object"
        )
      }

      step_id <- add_step_record(
        kind = "analysis",
        title = info$title,
        detail = info$detail,
        source = current_source,
        output = ""
      )
      add_edge(current_id, step_id, "flows")
      next
    }

    if (is_plain_name(statement)) {
      source_id <- object_node(statement)
      step_id <- add_step_record(
        kind = "analysis",
        title = "Display object",
        detail = paste0("show `", statement, "` for review"),
        source = statement,
        output = ""
      )
      add_edge(source_id, step_id, "displays")
      next
    }

    fn <- leading_call(statement)
    if (!is.null(fn) && plain_fn(fn) %in% c("library", "require")) {
      next
    }

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
      next
    }

    if (!is.null(fn)) {
      info <- describe_transform(statement, source_in_call = TRUE)
      source_label <- if (nzchar(info$source)) info$source else first_plain_arg(statement)
      source_id <- if (nzchar(source_label)) object_node(source_label) else NULL
      standalone_title <- info$title
      if (identical(plain_fn(fn), "season")) {
        standalone_title <- "Inspect season labels"
      }
      step_id <- add_step_record(
        kind = "analysis",
        title = standalone_title,
        detail = info$detail,
        source = source_label,
        output = ""
      )
      add_edge(source_id, step_id, "flows")
    }
  }

  steps <- annotate_superseded_steps(steps)
  edges <- unique(edges)
  list(nodes = nodes, edges = edges, steps = steps, script_path = path, language = "r")
}


expression_code <- function(expr, srcref) {
  if (!is.null(srcref)) {
    return(paste(as.character(srcref), collapse = "\n"))
  }

  paste(deparse(expr), collapse = "\n")
}


expression_text <- function(expr, srcref) {
  if (!is.null(srcref)) {
    return(compact_ws(strip_inline_comments(paste(as.character(srcref), collapse = "\n"))))
  }

  compact_ws(strip_inline_comments(paste(deparse(expr), collapse = " ")))
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


r_object_kind_from_input <- function(fn) {
  fn <- plain_fn(fn)

  if (fn %in% c(
    "read.csv", "read.csv2", "read_csv", "read_csv_arrow", "read_tsv",
    "read_delim", "read_fwf", "fread", "vroom",
    "read_excel", "read.xlsx", "read_xlsx",
    "dbGetQuery", "dbReadTable", "fromJSON",
    "read_sas", "read_spss", "read_sav", "read_dta", "read_stata",
    "read_parquet", "read_feather"
  )) {
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

  if (fn %in% c("predict", "ifelse", "case_when", "cut", "c")) {
    return("vector_object")
  }

  if (fn %in% c("bind_rows")) {
    return("data_frame")
  }

  if (fn %in% c("vector", "list")) {
    return("generic_object")
  }

  if (fn %in% c("as_tibble", "as.data.frame", "mutate_at", "mutate_if", "mutate_all", "transmute", "transmute_at", "transmute_if", "transmute_all", "count", "tally", "add_count", "add_tally", "slice_head", "slice_tail", "slice", "slice_sample", "slice_min", "slice_max", "ungroup", "rowwise", "rename", "rename_with", "relocate", "fill", "replace_na", "drop_na", "coalesce")) {
    return("data_frame")
  }

  if (fn %in% c("pull")) {
    return("vector_object")
  }

  if (fn %in% c("e_charts", "e_bar", "e_line", "e_scatter", "e_area", "e_pie", "e_title", "e_legend", "e_color", "e_tooltip", "e_labels", "e_common", "e_arrange")) {
    return("plot_object")
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

  if (isTRUE(source_in_call) && length(args) > 0 && is_plain_name(args[[1]]) && !fn_plain %in% c("e_arrange")) {
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


annotate_superseded_steps <- function(steps) {
  if (nrow(steps) == 0L || !("output" %in% names(steps))) {
    return(steps)
  }

  outputs <- trim_ws(steps$output)
  steps$superseded_by[is.na(steps$superseded_by)] <- ""

  repeated_outputs <- unique(outputs[nzchar(outputs) & duplicated(outputs, fromLast = TRUE)])
  if (length(repeated_outputs) == 0L) {
    return(steps)
  }

  for (output_name in repeated_outputs) {
    indices <- which(outputs == output_name)
    if (length(indices) < 2L) {
      next
    }

    for (position in seq_len(length(indices) - 1L)) {
      current_index <- indices[[position]]
      next_index <- indices[[position + 1L]]
      replacement_step <- as.character(steps$step[[next_index]])
      note <- paste0("this version is replaced later at step ", replacement_step)

      steps$superseded_by[[current_index]] <- replacement_step
      steps$detail[[current_index]] <- if (nzchar(trim_ws(steps$detail[[current_index]]))) {
        paste0(steps$detail[[current_index]], "; ", note)
      } else {
        paste0("This version is replaced later at step ", replacement_step)
      }
      steps$narrative[[current_index]] <- paste0(
        sub("\\.$", "", steps$narrative[[current_index]]),
        ". This version is replaced later at step ",
        replacement_step,
        "."
      )
    }
  }

  steps
}
