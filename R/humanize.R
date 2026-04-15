
human_action <- function(fn) {
  fn <- plain_fn(fn)

  if (!nzchar(fn)) return("Transform data")

  if (fn %in% c("read.csv", "read.csv2", "read_csv")) return("Load CSV file")
  if (fn %in% c("read_tsv")) return("Load tab-delimited file")
  if (fn %in% c("read_delim", "read_fwf", "fread", "vroom")) return("Load data file")
  if (fn %in% c("read_excel", "read.xlsx", "read_xlsx")) return("Load Excel file")
  if (fn %in% c("readRDS")) return("Load R data file")
  if (fn %in% c("load")) return("Load R workspace file")
  if (fn %in% c("data")) return("Load packaged dataset")
  if (fn %in% c("fromJSON")) return("Load JSON file")
  if (fn %in% c("dbGetQuery")) return("Run database query")
  if (fn %in% c("dbReadTable")) return("Load database table")
  if (fn %in% c("read_sas")) return("Load SAS data file")
  if (fn %in% c("read_spss", "read_sav")) return("Load SPSS data file")
  if (fn %in% c("read_dta", "read_stata")) return("Load Stata data file")
  if (fn %in% c("read_parquet")) return("Load Parquet file")
  if (fn %in% c("read_feather")) return("Load Feather file")
  if (fn %in% c("read_csv_arrow")) return("Load CSV file")
  if (fn %in% c("Load10X_Spatial")) return("Load spatial transcriptomics data")
  if (fn %in% c("c")) return("Create reference vector")
  if (fn %in% c("dir", "list.files")) return("Discover source data files")
  if (fn %in% c("vector")) return("Prepare collection")
  if (fn %in% c("bind_rows")) return("Combine imported files")
  if (fn %in% c("as_tibble", "as.data.frame")) return("Convert to data frame")
  if (fn %in% c("transform", "mutate")) return("Add or change columns")
  if (fn %in% c("mutate_at", "mutate_if", "mutate_all", "transmute", "transmute_at", "transmute_if", "transmute_all")) return("Add or change columns")
  if (fn %in% c("subset", "filter")) return("Filter rows")
  if (fn %in% c("select")) return("Keep selected columns")
  if (fn %in% c("arrange")) return("Sort rows")
  if (fn %in% c("group_by")) return("Group rows")
  if (fn %in% c("summarise", "summarize")) return("Summarize data")
  if (fn %in% c("count")) return("Count records")
  if (fn %in% c("slice_head")) return("Keep first rows")
  if (fn %in% c("slice_tail")) return("Keep last rows")
  if (fn %in% c("slice")) return("Keep selected rows by position")
  if (fn %in% c("slice_sample")) return("Take a random sample of rows")
  if (fn %in% c("slice_min")) return("Keep rows with the smallest values")
  if (fn %in% c("slice_max")) return("Keep rows with the largest values")
  if (fn %in% c("rename", "rename_with")) return("Rename columns")
  if (fn %in% c("relocate")) return("Reorder columns")
  if (fn %in% c("distinct")) return("Remove duplicates")
  if (fn %in% c("ungroup")) return("Remove row grouping")
  if (fn %in% c("rowwise")) return("Switch to row-by-row processing mode")
  if (fn %in% c("pull")) return("Extract column as a vector")
  if (fn %in% c("add_count", "add_tally")) return("Add a count column")
  if (fn %in% c("tally")) return("Count records")
  if (fn %in% c("fill")) return("Fill missing values from adjacent rows")
  if (fn %in% c("replace_na")) return("Replace missing values")
  if (fn %in% c("drop_na")) return("Remove rows with missing values")
  if (fn %in% c("coalesce")) return("Replace missing values with fallback")
  if (fn %in% c("across")) return("Apply transformation across multiple columns")
  if (fn %in% c("starts_with", "ends_with", "contains", "matches", "everything", "last_col")) return("Select columns by name pattern")
  if (fn %in% c("pivot_longer")) return("Reshape to long format")
  if (fn %in% c("pivot_wider")) return("Reshape to wide format")
  if (fn %in% c("left_join", "right_join", "inner_join", "full_join", "semi_join", "anti_join")) return("Join tables")
  if (fn %in% c("lm")) return("Fit linear model")
  if (fn %in% c("glm")) return("Fit generalized linear model")
  if (fn %in% c("predict")) return("Score records with model")
  if (fn %in% c("ifelse", "case_when")) return("Create conditional value")
  if (fn %in% c("cut")) return("Bucket values")
  if (fn %in% c("PercentageFeatureSet")) return("Calculate feature percentages")
  if (fn %in% c("AddMetaData")) return("Add metadata columns")
  if (fn %in% c("GetTissueCoordinates")) return("Extract tissue coordinates")
  if (fn %in% c("SCTransform")) return("Normalize spatial counts")
  if (fn %in% c("ggplot")) return("Create chart")
  if (fn %in% c("SpatialFeaturePlot")) return("Create spatial feature plot")
  if (fn %in% c("SpatialDimPlot")) return("Create spatial tissue plot")
  if (fn %in% c("VlnPlot")) return("Create violin plot")
  if (fn %in% c("e_charts")) return("Create chart")
  if (fn %in% c("e_bar", "e_line", "e_scatter", "e_area", "e_pie")) return("Add chart layer")
  if (fn %in% c("e_title")) return("Label chart")
  if (fn %in% c("e_legend", "e_color", "e_tooltip", "e_labels")) return("Style chart")
  if (fn %in% c("e_common")) return("Set chart defaults")
  if (fn %in% c("e_arrange")) return("Arrange charts")
  if (grepl("^geom_", fn)) return("Add chart layer")
  if (fn %in% c("labs")) return("Label chart")
  if (grepl("^theme", fn)) return("Style chart")
  if (fn %in% c("write.csv", "write_csv")) return("Write CSV file")
  if (fn %in% c("write_tsv", "write_delim", "write.xlsx", "write_xlsx")) return("Write output file")
  if (fn %in% c("saveRDS", "save")) return("Save R data file")
  if (fn %in% c("ggsave", "pdf", "png", "jpeg", "tiff", "svg", "svglite")) return("Save visual output")

  paste("Run", fn)
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


build_code_explanation <- function(code, title = "", detail = "", language = c("r", "sql")) {
  language <- match.arg(language)
  code <- trim_ws(code)
  if (!nzchar(code)) {
    return("")
  }

  if (identical(language, "sql")) {
    return(sql_code_explanation(code, title, detail))
  }

  r_code_explanation(code, title, detail)
}


r_code_explanation <- function(code, title = "", detail = "") {
  functions <- unique(plain_fn(unlist(regmatches(code, gregexpr("[[:alnum:]_.]+(?:::[[:alnum:]_.]+)?\\s*(?=\\()", code, perl = TRUE)), use.names = FALSE)))
  functions <- functions[nzchar(functions)]
  if (length(functions) == 0L) {
    return("")
  }

  notes <- vapply(functions, r_function_note, character(1), USE.NAMES = FALSE)
  notes <- unique(notes[nzchar(notes)])

  if (grepl("case_when\\s*\\(", code, perl = TRUE) && grepl("DJF|MAM|JJA|SON", code, perl = TRUE)) {
    notes <- c(notes, "This step recodes compact season codes: DJF = Winter, MAM = Spring, JJA = Summer, and SON = Fall.")
  }

  paste(unique(notes), collapse = "\n")
}


r_function_note <- function(fn) {
  switch(
    fn,
    dir = "dir() searches a folder and returns file names or file paths that match the requested pattern.",
    list.files = "list.files() searches a folder and returns file names or file paths that match the requested pattern.",
    fromJSON = "fromJSON() reads a JSON file and converts it into R data; flatten = TRUE spreads nested JSON fields into regular columns where possible.",
    load = "load() reads one or more R objects from an `.RData` or `.rda` file into the current session.",
    data = "data() loads an example or packaged dataset that ships with an installed R package.",
    Load10X_Spatial = "Load10X_Spatial() reads a 10x Visium/Space Ranger output folder into a Seurat spatial object.",
    vector = "vector() creates an empty container; here it is used to prepare a list that will hold imported files.",
    bind_rows = "bind_rows() stacks many imported tables into one combined data frame.",
    c = "c() combines values into a vector, often used as a reusable list of labels or filter values.",
    filter = "filter() keeps only rows where the stated condition is true.",
    mutate = "mutate() adds new columns or changes existing columns.",
    mutate_at = "mutate_at() applies the same conversion or transformation to selected columns.",
    case_when = "case_when() is a multi-condition recode: it checks conditions in order and returns the matching label/value.",
    season = "season() converts date values into season labels. In this workflow, those labels are used to group listening history by time of year.",
    ymd_hms = "ymd_hms() parses text timestamps into date-time values ordered as year-month-day hour-minute-second.",
    hours = "hours() creates a time offset in hours; subtracting it shifts timestamps to a different time zone or reference time.",
    floor_date = "floor_date() rounds dates down to the start of a period, such as the day or month.",
    as_date = "as_date() converts a date-time value into a date without the time-of-day portion.",
    group_by = "group_by() defines the groups for later summaries, similar to choosing rows/columns in a pivot table.",
    summarise = "summarise() collapses each group into summary metrics, such as totals, averages, or counts.",
    summarize = "summarize() collapses each group into summary metrics, such as totals, averages, or counts.",
    arrange = "arrange() sorts the rows by one or more columns.",
    desc = "desc() means sort descending, with the largest values first.",
    count = "count() counts how many records fall into each group.",
    slice_head = "slice_head() keeps the first rows after any grouping or sorting has been applied.",
    e_common = "e_common() sets default echarts4r styling options that later interactive charts inherit.",
    e_charts = "e_charts() starts an interactive echarts4r chart and chooses the main category axis.",
    e_bar = "e_bar() adds a bar series to an interactive chart.",
    e_line = "e_line() adds a line series to an interactive chart.",
    e_title = "e_title() sets the chart title and optional subtitle.",
    e_legend = "e_legend() controls whether and where the chart legend appears.",
    e_color = "e_color() sets chart colors and background styling.",
    e_tooltip = "e_tooltip() controls the hover tooltip shown in the interactive chart.",
    e_arrange = "e_arrange() places multiple echarts4r charts into one combined display.",
    ggplot = "ggplot() starts a ggplot chart by choosing the data and visual mappings.",
    aes = "aes() maps data columns to visual roles such as x-axis, y-axis, color, or grouping.",
    labs = "labs() sets human-readable chart titles, subtitles, axis labels, or legend labels.",
    lm = "lm() fits a linear model that estimates one outcome from one or more predictors.",
    glm = "glm() fits a generalized linear model for outcomes that need non-normal assumptions, such as binary or count outcomes.",
    predict = "predict() applies a fitted model to data to generate predicted values.",
    PercentageFeatureSet = "PercentageFeatureSet() calculates the percentage of counts or features that match a pattern for each cell or spot.",
    AddMetaData = "AddMetaData() attaches a new metadata column to the main object so it can be grouped, filtered, or plotted later.",
    GetTissueCoordinates = "GetTissueCoordinates() extracts the x/y spot coordinates from a spatial object.",
    SCTransform = "SCTransform() normalizes expression data and models technical variation so downstream comparisons are more stable.",
    SpatialFeaturePlot = "SpatialFeaturePlot() shows where selected genes or metrics appear across the tissue image.",
    SpatialDimPlot = "SpatialDimPlot() shows the spatial layout of spots or cells, optionally colored by groups or annotations.",
    VlnPlot = "VlnPlot() compares the distribution of one or more metrics across groups with violin plots.",
    ""
  )
}


sql_code_explanation <- function(code, title = "", detail = "") {
  notes <- character()
  lowered <- tolower(code)
  if (grepl("\\bleft\\s+join\\b", lowered, perl = TRUE)) {
    notes <- c(notes, "LEFT JOIN keeps every row from the left/current table even when the joined table has no match.")
  }
  if (grepl("\\bgroup\\s+by\\b", lowered, perl = TRUE)) {
    notes <- c(notes, "GROUP BY defines the slices used for summary metrics, similar to grouping fields in a pivot table.")
  }
  if (grepl("\\bhaving\\b|\\bqualify\\b", lowered, perl = TRUE)) {
    notes <- c(notes, "This filter is applied after summary/window calculations, not directly to raw source rows.")
  }
  paste(unique(notes), collapse = "\n")
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

  if (fn == "season" && length(args) >= 1) {
    return(paste0("convert ", humanize_expression(args[[1]]), " into a season label"))
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

  text <- gsub("%<>%", " then update ", text, fixed = TRUE)
  text <- gsub("%>%", " then ", text, fixed = TRUE)
  text <- gsub("|>", " then ", text, fixed = TRUE)

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

  text <- gsub("!([[:alnum:]_.]+)\\s+%in%\\s+([[:alnum:]_.]+)", "\\1 is not in \\2", text, perl = TRUE)
  text <- gsub("([[:alnum:]_.]+)\\s+%in%\\s+([[:alnum:]_.]+)", "\\1 is in \\2", text, perl = TRUE)
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
  neg_args <- args[grepl("^-", trim_ws(args))]
  pos_args <- args[!grepl("^-", trim_ws(args))]

  if (length(neg_args) > 0L && length(pos_args) == 0L) {
    dropped <- vapply(sub("^-", "", trim_ws(neg_args)), humanize_expression, character(1))
    return(paste0("drop columns ", paste(dropped, collapse = ", ")))
  }

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

  if (fn %in% c("mutate_at", "mutate_if", "mutate_all", "transmute", "transmute_at", "transmute_if", "transmute_all")) {
    if (fn == "mutate_at" && length(args) >= 2L) {
      cols <- humanize_expression(args[[1]])
      fun <- humanize_expression(args[[2]])
      return(paste0("update ", cols, " using ", fun))
    }
    return(paste(vapply(args, humanize_expression, character(1)), collapse = "; "))
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

  if (fn %in% c("count")) {
    group_args <- args[!grepl("^(sort|wt|name)\\s*=", args)]
    detail <- if (length(group_args) > 0L) {
      paste0("count records by ", paste(vapply(group_args, humanize_expression, character(1)), collapse = ", "))
    } else {
      "count records"
    }
    if (any(grepl("^sort\\s*=\\s*TRUE$", args))) {
      detail <- paste0(detail, " and sort by the largest counts")
    }
    detail
  }

  if (fn %in% c("arrange")) {
    return(explain_arrange(args))
  }

  if (fn %in% c("slice_head", "slice_tail")) {
    n_arg <- named_arg_value(args, "n")
    count_text <- if (nzchar(n_arg)) strip_wrapping_quotes(n_arg) else "a subset of"
    direction <- if (fn == "slice_head") "first" else "last"
    return(paste0("keep the ", direction, " ", count_text, " rows"))
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

  if (fn %in% c("as_tibble", "as.data.frame")) {
    return("convert the current data into a tidy data frame")
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

  if (fn %in% c("Load10X_Spatial")) {
    dir_arg <- named_arg_value(args, "data.dir")
    file_arg <- named_arg_value(args, "filename")
    slice_arg <- named_arg_value(args, "slice")
    pieces <- c()
    if (nzchar(dir_arg)) {
      pieces <- c(pieces, paste0("read spatial data from ", humanize_expression(dir_arg)))
    } else {
      pieces <- c(pieces, "read spatial data from the provided folder")
    }
    if (nzchar(file_arg)) {
      pieces <- c(pieces, paste0("use file ", humanize_expression(file_arg)))
    }
    if (nzchar(slice_arg)) {
      pieces <- c(pieces, paste0("label the slice as ", humanize_expression(slice_arg)))
    }
    return(paste(pieces, collapse = "; "))
  }

  if (fn %in% c("PercentageFeatureSet")) {
    pattern_arg <- if (length(args) > 1L) args[[2]] else ""
    name_arg <- named_arg_value(args, "col.name")
    detail <- "calculate a per-cell or per-spot percentage metric"
    if (nzchar(pattern_arg)) {
      detail <- paste0(detail, " for features matching ", humanize_expression(pattern_arg))
    }
    if (nzchar(name_arg)) {
      detail <- paste0(detail, " and store it as ", humanize_expression(name_arg))
    }
    return(detail)
  }

  if (fn %in% c("AddMetaData")) {
    col_arg <- named_arg_value(args, "col.name")
    if (nzchar(col_arg)) {
      return(paste0("attach a metadata column named ", humanize_expression(col_arg), " to the current object"))
    }
    return("attach additional metadata to the current object")
  }

  if (fn %in% c("GetTissueCoordinates")) {
    scale_arg <- named_arg_value(args, "scale")
    if (nzchar(scale_arg)) {
      return(paste0("extract tissue coordinates at the ", humanize_expression(scale_arg), " image scale"))
    }
    return("extract tissue coordinates from the current spatial object")
  }

  if (fn %in% c("SCTransform")) {
    assay_arg <- named_arg_value(args, "assay")
    method_arg <- named_arg_value(args, "method")
    detail <- "normalize the current expression data"
    if (nzchar(assay_arg)) {
      detail <- paste0(detail, " using the ", humanize_expression(assay_arg), " assay")
    }
    if (nzchar(method_arg)) {
      detail <- paste0(detail, " with the ", humanize_expression(method_arg), " method")
    }
    return(detail)
  }

  if (fn %in% c("SpatialFeaturePlot", "SpatialDimPlot", "VlnPlot")) {
    feature_arg <- named_arg_value(args, "features")
    group_arg <- named_arg_value(args, "group.by")
    detail <- switch(
      fn,
      SpatialFeaturePlot = "show selected genes or metrics across the tissue image",
      SpatialDimPlot = "show the tissue layout with spots colored by grouping information",
      VlnPlot = "compare the distribution of selected metrics across groups"
    )
    if (nzchar(feature_arg)) {
      detail <- paste0(detail, "; features = ", humanize_expression(feature_arg))
    }
    if (nzchar(group_arg)) {
      detail <- paste0(detail, "; group by ", humanize_expression(group_arg))
    }
    return(detail)
  }

  if (fn %in% c("bind_rows")) {
    return("stack the imported records into one data frame")
  }

  if (fn %in% c("c")) {
    return("store these values as a reusable vector")
  }

  if (fn %in% c("vector") && length(args) > 0L) {
    collection_type <- strip_wrapping_quotes(args[[1]])
    if (identical(collection_type, "list")) {
      return("create an empty list to hold imported files")
    }
    return(paste0("create an empty ", collection_type, " collection"))
  }

  if (fn == "predict") {
    return(prediction_detail(args))
  }

  if (fn == "ggplot") {
    return(chart_detail(args, raw_text = raw_text))
  }

  if (fn == "e_charts") {
    if (length(args) > 0L) {
      return(paste0("start an interactive chart using ", humanize_expression(args[[1]]), " as the main category axis"))
    }
    return("start an interactive chart from the current data")
  }

  if (fn %in% c("e_bar", "e_line", "e_scatter", "e_area", "e_pie")) {
    layer <- sub("^e_", "", fn)
    series_arg <- if (length(args) > 0L) humanize_expression(args[[1]]) else "the selected measure"
    return(paste0("add a ", layer, " layer showing ", series_arg))
  }

  if (fn == "e_title") {
    title_arg <- if (length(args) > 0L) strip_wrapping_quotes(args[[1]]) else "the chart"
    subtext_arg <- named_arg_value(args, "subtext")
    detail <- paste0("set the chart title to ", title_arg)
    if (nzchar(subtext_arg)) {
      detail <- paste0(detail, " with subtitle ", strip_wrapping_quotes(subtext_arg))
    }
    return(detail)
  }

  if (fn == "e_legend") {
    return("configure how the chart legend is displayed")
  }

  if (fn == "e_color") {
    return("set the chart colors and background")
  }

  if (fn == "e_tooltip") {
    return("configure the chart tooltip behavior")
  }

  if (fn == "e_common") {
    return("set default chart fonts and theme options for later charts")
  }

  if (fn == "e_arrange") {
    chart_names <- if (length(args) > 0L) paste(vapply(args, humanize_expression, character(1)), collapse = ", ") else "the selected charts"
    return(paste0("arrange ", chart_names, " into a shared display"))
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


sql_stage_field_summary <- function(select_items) {
  if (length(select_items) == 0L) {
    return("")
  }

  item_text <- trim_ws(select_items)
  wildcard_items <- item_text[grepl("(^\\*$|\\.\\*$)", item_text, perl = TRUE)]
  alias_items <- item_text[grepl("(?is)\\s+as\\s+", item_text, perl = TRUE)]

  pieces <- character()
  if (length(wildcard_items) > 0L) {
    wildcard_bits <- vapply(utils::head(wildcard_items, 3L), function(item) {
      if (identical(item, "*")) {
        return("include all selected columns")
      }
      paste0("include all columns from `", sub("\\.\\*$", "", item, perl = TRUE), "`")
    }, character(1))
    pieces <- c(pieces, paste(wildcard_bits, collapse = "; "))
  }

  if (length(alias_items) > 0L) {
    alias_bits <- vapply(utils::head(alias_items, 4L), sql_alias_summary, character(1))
    alias_bits <- alias_bits[nzchar(alias_bits)]
    if (length(alias_bits) > 0L) {
      pieces <- c(pieces, paste0("rename selected fields such as ", paste(alias_bits, collapse = "; ")))
    }
  }

  paste(pieces[nzchar(pieces)], collapse = "; ")
}


sql_alias_summary <- function(item) {
  item <- trim_ws(item)
  alias_match <- regexec("(?is)^(.+?)\\s+as\\s+(.+)$", item, perl = TRUE)
  alias_hit <- regmatches(item, alias_match)[[1]]
  if (length(alias_hit) < 3L) {
    return("")
  }

  source_text <- trim_ws(alias_hit[[2]])
  if (!sql_is_plain_column_reference(source_text)) {
    return("")
  }
  alias_name <- sql_clean_identifier(alias_hit[[3]])
  source_name <- sql_clean_identifier(sub("^.*\\.", "", source_text, perl = TRUE))
  if (!nzchar(source_name)) {
    source_name <- source_text
  }
  paste0("`", source_name, "` -> `", alias_name, "`")
}


sql_stage_summary_detail <- function(stage, output_name = "", final = FALSE) {
  sources <- stage$sources %||% character()
  select_items <- stage$select_items %||% character()
  text <- stage$query %||% ""
  pieces <- character()

  if (nzchar(output_name)) {
    pieces <- c(pieces, paste0("create a temporary SQL result named `", output_name, "`"))
  } else if (isTRUE(final)) {
    pieces <- c(pieces, "return the main query result")
  }

  if (length(sources) == 1L) {
    pieces <- c(pieces, paste0("start from `", sources[[1]], "`"))
  } else if (length(sources) > 1L) {
    pieces <- c(pieces, paste0("combine data from ", paste(paste0("`", sources, "`"), collapse = ", ")))
  }

  field_summary <- sql_stage_field_summary(select_items)
  if (nzchar(field_summary)) {
    pieces <- c(pieces, field_summary)
  }

  if (grepl("(?i)\\bcase\\b", text, perl = TRUE)) {
    pieces <- c(pieces, "apply CASE WHEN rules to create readable conditional fields")
  }

  if (grepl("(?is)\\bselect\\s+distinct\\b", text, perl = TRUE)) {
    pieces <- c(pieces, "keep unique result rows with SELECT DISTINCT")
  }

  paste(pieces[nzchar(pieces)], collapse = "; ")
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

  metric_items <- select_items[grepl(sql_aggregate_pattern(), select_items, perl = TRUE)]
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

  tsql <- sql_tsql_alias_parts(item)
  if (!is.null(tsql)) {
    return(paste0(tsql$alias, " = ", sql_humanize_alias_expression(tsql$alias, tsql$expression)))
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
