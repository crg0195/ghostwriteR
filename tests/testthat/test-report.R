test_that("ghostwriter_report explains workflow in plain English", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'sales <- read.csv("sales.csv")',
    'summary_tbl <- sales %>%',
    '  group_by(region) %>%',
    '  summarise(avg_order_value = mean(net_revenue, na.rm = TRUE), .groups = "drop")',
    'write.csv(summary_tbl, "summary.csv")'
  ), tmp)

  report <- ghostwriteR::ghostwriter_report(tmp)

  expect_match(report, "How To Read The Diagram", fixed = TRUE)
  expect_match(report, "average of net_revenue, ignoring missing values", fixed = TRUE)
  expect_match(report, "return a regular table without grouping", fixed = TRUE)
  expect_match(report, "summary.csv", fixed = TRUE)
})

test_that("ghostwriter_report can write a Word document", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'sales <- read.csv("sales.csv")',
    'write.csv(sales, "sales_out.csv")'
  ), tmp)

  out <- tempfile(fileext = ".docx")
  expect_equal(ghostwriteR::ghostwriter_report(tmp, format = "docx", out = out), out)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)

  zipped <- utils::unzip(out, list = TRUE)$Name
  expect_true("word/document.xml" %in% zipped)

  doc_xml <- paste(readLines(unz(out, "word/document.xml"), warn = FALSE), collapse = "")
  expect_match(doc_xml, "GhostwriteR Report", fixed = TRUE)
  expect_match(doc_xml, "sales_out.csv", fixed = TRUE)
})

test_that("ghostwriter_report translates modeling and scoring steps for lay readers", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'training <- read.csv("training.csv")',
    'model <- lm(amount ~ quantity + discount, data = training)',
    'scored <- training |> mutate(predicted_amount = predict(model, newdata = training), score_band = ifelse(predicted_amount > 500, "High", "Standard"))',
    'summary_tbl <- scored %>% group_by(score_band) %>% summarise(avg_amount = mean(predicted_amount, na.rm = TRUE), customers = n_distinct(customer_id), .groups = "drop")'
  ), tmp)

  report <- ghostwriteR::ghostwriter_report(tmp)

  expect_match(report, "estimate amount from quantity, discount using data table training", fixed = TRUE)
  expect_match(report, "generate predicted values from model using training", fixed = TRUE)
  expect_match(report, "if predicted_amount is greater than 500 then High otherwise Standard", fixed = TRUE)
  expect_match(report, "calculate summary metrics", fixed = TRUE)
  expect_match(report, "number of distinct customer_id", fixed = TRUE)
})
