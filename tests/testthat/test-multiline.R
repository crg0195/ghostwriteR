test_that("ghostwriteR handles multiline pipelines and wrapped writes", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'sales <- read.csv("sales.csv")',
    'sales_clean <- sales |>',
    '  transform(',
    '    total = amount * 1.1,',
    '    region = toupper(region)',
    '  )',
    'sales_filtered <- sales_clean %>%',
    '  subset(',
    '    total > 100',
    '  )',
    'write.csv(',
    '  sales_filtered,',
    '  "filtered_sales.csv"',
    ')'
  ), tmp)

  edges <- ghostwriteR::ghostwriter_edges(tmp)
  nodes <- attr(edges, "nodes")

  expect_true(any(nodes$kind == "transform_step" & grepl("Add or change columns", nodes$label, fixed = TRUE)))
  expect_true(any(nodes$kind == "transform_step" & grepl("region = uppercase region", nodes$label, fixed = TRUE)))
  expect_true(any(nodes$kind == "transform_step" & grepl("Filter rows", nodes$label, fixed = TRUE)))
  expect_true(any(nodes$kind == "output_step" & grepl("Write CSV file", nodes$label, fixed = TRUE)))
  expect_true(any(nodes$kind == "output_file" & grepl("filtered_sales.csv", nodes$label, fixed = TRUE)))

  expect_true(any(edges$type == "flows"))
  expect_true(any(edges$type == "writes"))
})
