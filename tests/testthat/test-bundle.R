test_that("ghostwriter_bundle writes a self-contained html page and a Word report by default", {
  skip_if_not_installed("DiagrammeRsvg")

  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'sales <- read.csv("sales.csv")',
    'sales_filtered <- sales %>% subset(total > 100)',
    'write.csv(sales_filtered, "filtered_sales.csv")'
  ), tmp)

  out_dir <- tempfile(pattern = "ghostwriteR-bundle-")
  bundle <- ghostwriteR::ghostwriter_bundle(tmp, dir = out_dir)

  expect_equal(bundle$directory, out_dir)
  expect_true(file.exists(bundle$graph))
  expect_true(file.exists(bundle$report))
  expect_match(bundle$graph, "-ghostwriteR\\.html$")
  expect_match(bundle$report, "-ghostwriteR-report\\.docx$")
  expect_gt(file.info(bundle$graph)$size, 0)
  expect_gt(file.info(bundle$report)$size, 0)
})
