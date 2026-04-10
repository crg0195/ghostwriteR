
compact_ws <- function(text) {
  text <- gsub("\\s+", " ", text)
  trim_ws(text)
}


strip_inline_comments <- function(text) {
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  pieces <- character()
  quote_char <- ""
  in_comment <- FALSE

  for (index in seq_along(chars)) {
    char <- chars[[index]]

    if (in_comment) {
      if (char == "\n") {
        in_comment <- FALSE
        pieces <- c(pieces, char)
      }
      next
    }

    if (nzchar(quote_char)) {
      pieces <- c(pieces, char)
      if (char == quote_char && (index == 1L || chars[[index - 1L]] != "\\")) {
        quote_char <- ""
      }
      next
    }

    if (char %in% c("\"", "'")) {
      quote_char <- char
      pieces <- c(pieces, char)
      next
    }

    if (char == "#") {
      in_comment <- TRUE
      next
    }

    pieces <- c(pieces, char)
  }

  paste(pieces, collapse = "")
}


trim_ws <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
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


regex_escape <- function(text) {
  gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", text, perl = TRUE)
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
