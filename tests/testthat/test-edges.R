test_that("ghostwriter_edges keeps readable nodes and process flow", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'sales <- read.csv("sales.csv")',
    'sales_clean <- sales |> transform(total = amount * 1.1)',
    'sales_filtered <- sales_clean %>% subset(total > 100)',
    'write.csv(sales_filtered, "filtered_sales.csv")'
  ), tmp)

  edges <- ghostwriteR::ghostwriter_edges(tmp)
  nodes <- attr(edges, "nodes")

  expect_true(any(nodes$kind == "input_file" & grepl("sales.csv", nodes$label, fixed = TRUE)))
  expect_true(any(nodes$kind == "input_file" & grepl("Source data file", nodes$label, fixed = TRUE)))
  expect_true(any(nodes$kind == "data_frame" & grepl('"sales_filtered"', nodes$label, fixed = TRUE)))
  expect_true(any(nodes$kind == "transform_step" & grepl("Add or change columns", nodes$label, fixed = TRUE)))
  expect_true(any(nodes$kind == "transform_step" & grepl("Filter rows", nodes$label, fixed = TRUE)))
  expect_true(any(nodes$kind == "transform_step" & grepl("total is greater than 100", nodes$label, fixed = TRUE)))
  expect_true(any(nodes$kind == "output_file" & grepl("filtered_sales.csv", nodes$label, fixed = TRUE)))

  expect_true(any(edges$type == "reads"))
  expect_true(any(edges$type == "flows"))
  expect_true(any(edges$type == "creates"))
  expect_true(any(edges$type == "writes"))
})

test_that("R parser step metadata stays aligned with SQL parser fields", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'sales <- read.csv("sales.csv")',
    'sales_filtered <- sales %>% subset(total > 100)',
    'write.csv(sales_filtered, "filtered_sales.csv")'
  ), tmp)

  parsed <- ghostwriteR:::ghostwriter_parse(tmp)
  steps <- parsed$steps

  expect_true(all(c("group", "input_columns", "output_columns") %in% names(steps)))
  expect_true(all(steps$group == ""))
  expect_true(all(steps$input_columns == ""))
  expect_true(all(steps$output_columns == ""))
})
