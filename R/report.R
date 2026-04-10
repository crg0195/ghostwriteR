
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
    output_clause <- if (nzchar(output)) paste0(" to create `", output, "`") else ""
    return(paste0(title, detail_clause, output_clause, "."))
  }

  if (identical(kind, "transform")) {
    source_clause <- if (nzchar(source)) paste0("Using `", source, "`, ") else ""
    output_clause <- if (nzchar(output)) paste0(". This creates `", output, "`") else ""
    return(paste0(source_clause, tolower_first(title), detail_clause, output_clause, "."))
  }

  if (identical(kind, "analysis")) {
    source_clause <- if (nzchar(source)) paste0("Using `", source, "`, ") else ""
    return(paste0(source_clause, tolower_first(title), detail_clause, "."))
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
