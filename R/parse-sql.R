
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
  source_step_keys <- character()
  source_step_ids <- character()
  result_keys <- character()
  result_ids <- character()
  result_is_summary <- logical()
  result_group_details <- character()
  result_output_columns <- character()
  result_carried_columns <- character()
  result_created_columns <- character()
  current_sql_chunk <- ""

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

  add_step_record <- function(kind, title, detail = "", source = "", output = "", target_path = "", group = "", input_columns = "", output_columns = "", carried_columns = "", created_columns = "", superseded_by = "", code = current_sql_chunk, explanation = "") {
    step_index <<- step_index + 1L
    narrative <- build_step_narrative(kind, title, detail, source, output, target_path)
    if (!nzchar(explanation)) {
      explanation <- build_code_explanation(code, title, detail, language = "sql")
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

    table_node(sql_clean_identifier(name), "source_table")
  }

  process_query_stage <- function(stage, output_name = "", output_kind = "dataset", group = "", final = FALSE) {
    current_ids <- character()
    current_source_label <- ""
    source_metadata <- list()
    resolved_inventory <- sql_resolve_query_inventory(stage, source_metadata)

    if (length(stage$sources) > 0L) {
      source_metadata <- lapply(stage$sources, result_metadata)
      names(source_metadata) <- stage$sources
      resolved_inventory <- sql_resolve_query_inventory(stage, source_metadata)
      current_ids <- unique(vapply(stage$sources, function(source_name) source_node_id(source_name, group = group), character(1)))
      current_source_label <- paste(stage$sources, collapse = ", ")
    }

    if (isTRUE(final) || nzchar(output_name)) {
      summary_title <- if (nzchar(output_name)) "Create working dataset" else "Run main query"
      summary_detail <- sql_stage_summary_detail(stage, output_name = output_name, final = final)
      summary_explanation <- if (nzchar(output_name)) {
        paste(
          "A working dataset is a temporary SQL result created inside the script.",
          "SQL users often call this a CTE, or common table expression.",
          "It is used later in the query but is not usually saved as a permanent table by itself."
        )
      } else {
        paste(
          "This is the final SELECT statement that returns the main result of the SQL script.",
          "It can reuse earlier working datasets, also called CTEs, before producing the final rows shown or exported."
        )
      }
      summary_step_id <- add_step_record(
        kind = "transform",
        title = summary_title,
        detail = summary_detail,
        source = current_source_label,
        group = group,
        input_columns = stage$query_input_columns %||% "",
        output_columns = resolved_inventory$output_columns,
        carried_columns = resolved_inventory$carried_columns,
        created_columns = resolved_inventory$created_columns,
        code = sql_stage_code(stage$name %||% "", stage$query %||% "", final = final),
        explanation = summary_explanation
      )
      connect_many(current_ids, summary_step_id, "flows")
      current_ids <- summary_step_id
      current_source_label <- if (nzchar(output_name)) paste0("working dataset `", output_name, "`") else "the result from the previous step"
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
          created_columns = if (nzchar(step_output)) resolved_inventory$created_columns else step$created_columns %||% "",
          code = sql_stage_code(stage$name %||% "", stage$query %||% "", final = final)
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
          created_columns = resolved_inventory$created_columns,
          code = sql_stage_code(stage$name %||% "", stage$query %||% "", final = final)
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
        created_columns = resolved_inventory$created_columns,
        code = sql_stage_code(stage$name %||% "", stage$query %||% "", final = final)
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
    current_sql_chunk <- statement
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
          group = stage_group,
          final = identical(stage_index, length(info$stages))
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


sql_statement_list <- function(text) {
  cleaned <- sql_strip_comments(text)
  statements <- split_top_level(cleaned, ";")
  statements <- vapply(statements, compact_ws, character(1))
  statements[nzchar(statements)]
}


sql_strip_comments <- function(text) {
  text <- gsub("(?s)/\\*.*?\\*/", " ", text, perl = TRUE)
  text <- gsub("(?m)--[^\\n]*$", " ", text, perl = TRUE)
  gsub("(?m)^\\s*-{3,}[^\\n]*$", " ", text, perl = TRUE)
}


sql_depth0_text <- function(text) {
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  depth <- 0L
  quote_char <- ""
  result <- chars

  for (i in seq_along(chars)) {
    char <- chars[[i]]

    if (nzchar(quote_char)) {
      result[[i]] <- " "
      if (char == quote_char && (i <= 1L || chars[[i - 1L]] != "\\")) {
        quote_char <- ""
      }
      next
    }

    if (char %in% c("'", '"', "`")) {
      quote_char <- char
      result[[i]] <- " "
      next
    }

    if (char %in% c("(", "[")) {
      depth <- depth + 1L
      result[[i]] <- " "
      next
    }

    if (char %in% c(")", "]")) {
      depth <- max(0L, depth - 1L)
      result[[i]] <- " "
      next
    }

    if (depth > 0L) {
      result[[i]] <- " "
    }
  }

  paste(result, collapse = "")
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
      parsed$query <- stage$query
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
  union_parts <- sql_union_query_parts(query_text)
  has_union <- length(union_parts) > 1L
  first_part <- if (has_union) union_parts[[1]] else query_text
  where_clause <- sql_extract_clause(first_part, "where", c("group by", "having", "order by", "limit", "qualify", "union"))
  group_clause <- sql_extract_clause(first_part, "group by", c("having", "order by", "limit", "qualify", "union"))
  qualify_clause <- sql_extract_clause(query_text, "qualify", c("order by", "limit", "union"))
  order_clause <- sql_extract_clause(query_text, "order by", c("limit", "qualify", "union"))
  if (!nzchar(order_clause)) {
    order_clause <- sql_extract_order_clause(query_text)
  }
  select_items <- sql_extract_select_items(first_part)
  has_distinct <- grepl("(?is)\\bselect\\s+distinct\\b", query_text, perl = TRUE)
  has_aggregates <- sql_has_aggregates(select_items) || nzchar(group_clause)
  computed_items <- select_items[vapply(select_items, sql_is_feature_select_item, logical(1))]
  feature_detail <- sql_feature_detail(select_items)
  select_inventory <- sql_select_inventory(select_items)
  query_output_columns <- select_inventory$output_columns
  query_input_columns <- sql_query_input_columns(select_items, where_clause, group_clause, order_clause, joins)

  where_clause <- trim_ws(sub("(?is)\\border\\s+by\\b.*$", "", where_clause, perl = TRUE))

  query_steps <- list()

  if (has_union) {
    union_type <- sql_union_type_label(query_text)
    all_union_sources <- unique(unlist(lapply(union_parts, function(p) {
      refs <- sql_extract_source_refs(p)
      vapply(refs, function(r) r$table, character(1))
    }), use.names = FALSE))
    query_steps[[length(query_steps) + 1L]] <- list(
      title = paste0(union_type, " query branches"),
      detail = paste0(
        "combine results from ", length(union_parts), " separate SELECT branches",
        if (length(all_union_sources) > 0L) paste0(" drawing from: ", paste(all_union_sources, collapse = ", ")) else ""
      ),
      input_columns = "",
      output_columns = "",
      carried_columns = "",
      created_columns = ""
    )
  }

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

  if (nzchar(qualify_clause)) {
    query_steps[[length(query_steps) + 1L]] <- list(
      title = "Filter grouped results",
      detail = humanize_sql_expression(qualify_clause),
      input_columns = sql_clause_column_inventory(qualify_clause),
      output_columns = "",
      carried_columns = "",
      created_columns = ""
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
  text <- compact_ws(query_text)
  depth0 <- sql_depth0_text(text)
  m <- regexpr("(?is)\\border\\s+by\\b\\s+", depth0, perl = TRUE)
  if (m[[1]] < 0L) {
    return("")
  }
  start <- m[[1]] + attr(m, "match.length")
  value <- substr(text, start, nchar(text))
  value <- sub("(?is)\\s*;.*$", "", value, perl = TRUE)
  compact_ws(value)
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
  query_text <- compact_ws(sql_strip_comments(query_text))
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


sql_stage_code <- function(name = "", query_text = "", final = FALSE) {
  query_text <- trim_ws(query_text)
  if (!nzchar(query_text)) {
    return("")
  }

  pretty <- compact_ws(query_text)
  pretty <- gsub("(?i)\\bselect\\b", "\nSELECT", pretty, perl = TRUE)
  pretty <- gsub("(?i)\\bfrom\\b", "\nFROM", pretty, perl = TRUE)
  pretty <- gsub("(?i)\\b(inner|left|right|full|cross)\\s+join\\b", "\n\\1 JOIN", pretty, perl = TRUE)
  pretty <- gsub("(?i)\\bwhere\\b", "\nWHERE", pretty, perl = TRUE)
  pretty <- gsub("(?i)\\bgroup\\s+by\\b", "\nGROUP BY", pretty, perl = TRUE)
  pretty <- gsub("(?i)\\border\\s+by\\b", "\nORDER BY", pretty, perl = TRUE)
  pretty <- gsub("(?i)\\bhaving\\b", "\nHAVING", pretty, perl = TRUE)
  pretty <- gsub(',\\s+(?=[[:alnum:]_\\[\\]"])', ',\n  ', pretty, perl = TRUE)
  pretty <- trim_ws(pretty)

  if (nzchar(trim_ws(name))) {
    return(paste0(sql_clean_identifier(name), " AS (\n  ", pretty, "\n)"))
  }

  pretty
}


sql_stage_group_label <- function(name = "", final = FALSE) {
  if (nzchar(trim_ws(name))) {
    return(paste0("Working dataset: ", sql_clean_identifier(name)))
  }

  if (isTRUE(final)) {
    return("Main query")
  }

  "SQL workflow"
}


sql_union_type_label <- function(query_text) {
  depth0 <- sql_depth0_text(compact_ws(query_text))
  if (grepl("(?is)\\bunion\\s+all\\b", depth0, perl = TRUE)) return("Combine all")
  if (grepl("(?is)\\bunion\\b", depth0, perl = TRUE)) return("Combine unique")
  if (grepl("(?is)\\bintersect\\b", depth0, perl = TRUE)) return("Intersect")
  if (grepl("(?is)\\bexcept\\b", depth0, perl = TRUE)) return("Subtract")
  "Combine"
}


sql_is_copy_table_export <- function(statement) {
  grepl(
    "(?is)^copy\\s+(?!\\()([[:alnum:]_.$\"]+)\\s+to\\s+['\"][^'\"]+['\"]",
    compact_ws(statement),
    perl = TRUE
  )
}


sql_union_query_parts <- function(query_text) {
  text <- compact_ws(query_text)
  depth0 <- sql_depth0_text(text)
  positions <- gregexpr("(?is)\\b(?:union(?:\\s+all)?|intersect|except)\\b", depth0, perl = TRUE)[[1]]
  if (positions[[1]] < 0L) {
    return(list(text))
  }
  lengths <- attr(positions, "match.length")
  parts <- character()
  prev_end <- 1L
  for (i in seq_along(positions)) {
    parts <- c(parts, trim_ws(substr(text, prev_end, positions[[i]] - 1L)))
    prev_end <- positions[[i]] + lengths[[i]]
  }
  parts <- c(parts, trim_ws(substr(text, prev_end, nchar(text))))
  as.list(parts[nzchar(trim_ws(parts))])
}


sql_extract_sources <- function(query_text) {
  parts <- sql_union_query_parts(query_text)
  cte_names <- sql_cte_names(query_text)
  all_names <- character()
  for (part in parts) {
    refs <- sql_extract_source_refs(part)
    if (length(refs) == 0L) next
    names <- vapply(refs, function(ref) ref$table, character(1))
    all_names <- c(all_names, names[!(tolower(names) %in% tolower(cte_names))])
  }
  unique(all_names)
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
  depth0 <- sql_depth0_text(text)

  clause_pattern <- paste0("(?is)\\b", gsub("\\s+", "\\\\s+", trim_ws(clause)), "\\b\\s+")
  start_match <- regexpr(clause_pattern, depth0, perl = TRUE)
  start <- start_match[[1]]
  if (start < 0L) {
    return("")
  }

  content_start <- start + attr(start_match, "match.length")
  value <- trim_ws(substr(text, content_start, nchar(text)))
  depth0_value <- trim_ws(substr(depth0, content_start, nchar(depth0)))
  if (!nzchar(value)) {
    return("")
  }

  if (length(terminators) > 0L) {
    end_positions <- vapply(terminators, function(term) {
      term_pattern <- paste0("(?is)\\b", gsub("\\s+", "\\\\s+", trim_ws(term)), "\\b")
      pos <- regexpr(term_pattern, depth0_value, perl = TRUE)[[1]]
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

  tsql <- sql_tsql_alias_parts(item)
  if (!is.null(tsql)) {
    if (sql_is_plain_column_reference(tsql$expression)) {
      return(list(carried = tsql$alias, created = character()))
    }
    return(list(carried = character(), created = tsql$alias))
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

  metadata_names <- names(source_metadata) %||% character()
  map <- list()
  for (index in seq_along(source_refs)) {
    ref <- source_refs[[index]]
    metadata <- NULL

    if (length(metadata_names) > 0L && !is.null(ref$table) && nzchar(ref$table)) {
      name_match <- match(tolower(ref$table), tolower(metadata_names))
      if (!is.na(name_match)) {
        metadata <- source_metadata[[name_match]]
      }
    }

    if (is.null(metadata) && index <= length(source_metadata)) {
      metadata <- source_metadata[[index]]
    }

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
  tsql <- sql_tsql_alias_parts(item)
  if (!is.null(tsql)) {
    expression_text <- tsql$expression
  }

  sql_extract_expression_columns(expression_text, keep_wildcards = keep_wildcards)
}


sql_select_item_output_column <- function(item) {
  item <- trim_ws(item)
  if (!nzchar(item)) return("")
  if (identical(item, "*")) return("all selected columns")
  if (grepl("\\.\\*$", item, perl = TRUE)) return(paste0("all columns from ", sub("\\.\\*$", "", item, perl = TRUE)))

  alias_match <- regexec("(?is)^(.+?)\\s+as\\s+([[:alnum:]_.$\"]+)$", item, perl = TRUE)
  alias_hit <- regmatches(item, alias_match)[[1]]
  if (length(alias_hit) >= 3L) return(sql_clean_identifier(alias_hit[[3]]))

  tsql <- sql_tsql_alias_parts(item)
  if (!is.null(tsql)) return(tsql$alias)

  if (grepl("^[[:alnum:]_.$]+$", item, perl = TRUE)) return(sql_clean_identifier(sub("^.*\\.", "", item, perl = TRUE)))

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
    "range", "current", "row", "preceding", "following", "true", "false", "date",
    "between", "like", "ilike", "rlike", "similar", "exists", "any", "some",
    "intersect", "except", "lateral", "natural", "using",
    "delete", "update", "merge", "set", "values", "truncate", "drop", "alter",
    "begin", "declare", "execute", "call", "returns", "return",
    "interval", "extract", "cast", "convert", "trim", "replace", "escape",
    "window", "recursive", "volatile", "stable", "immutable", "if", "else",
    "for", "while", "loop", "exit", "continue", "raise", "notice", "exception"
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


sql_aggregate_pattern <- function() {
  "(?i)\\b(sum|avg|count|min|max|string_agg|array_agg|median|stddev|stddev_pop|stddev_samp|variance|var_pop|var_samp|percentile_cont|percentile_disc|approx_count_distinct|approx_percentile|listagg|wm_concat|group_concat|first_value|last_value|nth_value|mode|kurtosis|skewness)\\s*\\("
}


sql_has_aggregates <- function(select_items) {
  if (length(select_items) == 0L) {
    return(FALSE)
  }

  any(grepl(sql_aggregate_pattern(), select_items, perl = TRUE))
}


sql_is_feature_select_item <- function(item) {
  item <- trim_ws(item)
  if (grepl(sql_aggregate_pattern(), item, perl = TRUE)) {
    return(FALSE)
  }

  alias_match <- regexec("(?is)^(.+?)\\s+as\\s+([[:alnum:]_.$\"]+)$", item, perl = TRUE)
  alias_hit <- regmatches(item, alias_match)[[1]]

  if (length(alias_hit) >= 3L) {
    expr <- trim_ws(alias_hit[[2]])
    return(!grepl("^[[:alnum:]_.$]+$", expr))
  }

  tsql <- sql_tsql_alias_parts(item)
  if (!is.null(tsql)) {
    return(!grepl("^[[:alnum:]_.$]+$", tsql$expression))
  }

  grepl("\\(|\\+|-|\\*|/|\\bcase\\b", item, ignore.case = TRUE, perl = TRUE)
}


sql_tsql_alias_parts <- function(item) {
  item <- trim_ws(item)
  m <- regexec("^([[:alpha:]][[:alnum:]_]*)\\s*=\\s*(.+)$", item, perl = TRUE)
  hit <- regmatches(item, m)[[1]]
  if (length(hit) < 3L) return(NULL)
  alias <- trim_ws(hit[[2]])
  expr <- trim_ws(hit[[3]])
  if (grepl("^[[:digit:]]", expr) || grepl("^['\"]", expr)) return(NULL)
  list(alias = alias, expression = expr)
}


sql_clean_identifier <- function(name) {
  name <- trim_ws(name)
  name <- gsub('^"|"$', "", name)
  name <- gsub("^`|`$", "", name)
  name <- gsub("^\\[|\\]$", "", name)
  trim_ws(name)
}
