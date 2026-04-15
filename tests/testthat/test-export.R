test_that("ghostwriteR exports a self-contained html page with timeline and graph views", {
  skip_if_not_installed("DiagrammeRsvg")

  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'sales <- read.csv("sales.csv")',
    'sales_clean <- sales %>% subset(total > 100)',
    'summary_tbl <- sales_clean %>% group_by(region) %>% summarise(avg_order_value = mean(total, na.rm = TRUE), .groups = "drop")',
    'write.csv(summary_tbl, "filtered_sales.csv")'
  ), tmp)

  html_out <- tempfile(fileext = ".html")
  expect_equal(ghostwriteR::ghostwriteR(tmp, format = "html", out = html_out), html_out)
  expect_true(file.exists(html_out))
  expect_gt(file.info(html_out)$size, 0)

  html <- paste(readLines(html_out, warn = FALSE), collapse = "\n")
  expect_match(html, '<meta name="viewport"', fixed = TRUE)
  expect_match(html, 'Workflow timeline', fixed = TRUE)
  expect_match(html, 'Full dependency graph', fixed = TRUE)
  expect_match(html, 'Data frames', fixed = TRUE)
  expect_match(html, 'phase-group', fixed = TRUE)
  expect_match(html, 'Selected step details', fixed = TRUE)
  expect_match(html, 'renderDetailCallout', fixed = TRUE)
  expect_match(html, 'detail-list', fixed = TRUE)
  expect_match(html, 'detail-more', fixed = TRUE)
  expect_false(grepl('ch === """', html, fixed = TRUE))
  expect_match(html, 'graphZoomInButton', fixed = TRUE)
  expect_match(html, 'graphFitButton', fixed = TRUE)
  expect_match(html, 'filtered_sales.csv', fixed = TRUE)
  expect_false(grepl('<script[^>]+src=', html, perl = TRUE))
  expect_false(grepl('<link[^>]+href=', html, perl = TRUE))
})

test_that("ghostwriteR exports svg, png, and pdf files", {
  skip_if_not_installed("DiagrammeRsvg")
  skip_if_not_installed("rsvg")

  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'sales <- read.csv("sales.csv")',
    'sales_filtered <- sales %>% subset(total > 100)',
    'write.csv(sales_filtered, "filtered_sales.csv")'
  ), tmp)

  svg_out <- tempfile(fileext = ".svg")
  png_out <- tempfile(fileext = ".png")
  pdf_out <- tempfile(fileext = ".pdf")

  expect_equal(ghostwriteR::ghostwriteR(tmp, format = "svg", out = svg_out), svg_out)
  expect_equal(ghostwriteR::ghostwriteR(tmp, format = "png", out = png_out), png_out)
  expect_equal(ghostwriteR::ghostwriteR(tmp, format = "pdf", out = pdf_out), pdf_out)

  expect_true(file.exists(svg_out))
  expect_true(file.exists(png_out))
  expect_true(file.exists(pdf_out))
  expect_gt(file.info(svg_out)$size, 0)
  expect_gt(file.info(png_out)$size, 0)
  expect_gt(file.info(pdf_out)$size, 0)
})

test_that("ghostwriteR exports a standalone self-contained html graph", {
  skip_if_not_installed("DiagrammeRsvg")

  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    "sales <- read.csv(\"sales.csv\")",
    "sales_clean <- sales |> transform(total = amount * 1.1)",
    "sales_filtered <- sales_clean %>% subset(total > 100)",
    "write.csv(sales_filtered, \"filtered_sales.csv\")"
  ), tmp)

  html_out <- tempfile(fileext = ".html")
  expect_equal(ghostwriteR::ghostwriteR(tmp, format = "html_graph", out = html_out), html_out)
  expect_true(file.exists(html_out))
  expect_gt(file.info(html_out)$size, 0)

  html <- paste(readLines(html_out, warn = FALSE), collapse = "\n")
  expect_match(html, "GhostwriteR Graph Only", fixed = TRUE)
  expect_match(html, "graphOnlyFrame", fixed = TRUE)
  expect_match(html, "Reset view", fixed = TRUE)
  expect_match(html, "graphOnlyZoomInButton", fixed = TRUE)
  expect_match(html, "graphOnlyFitButton", fixed = TRUE)
  expect_match(html, "filtered_sales.csv", fixed = TRUE)
  expect_false(grepl("Workflow timeline", html, fixed = TRUE))
  expect_false(grepl("Selected step details", html, fixed = TRUE))
  expect_false(grepl("<script[^>]+src=", html, perl = TRUE))
  expect_false(grepl("<link[^>]+href=", html, perl = TRUE))
})

test_that("large workflows export direct graph html with expandable phases", {
  skip_if_not_installed("DiagrammeRsvg")

  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'sales <- read.csv("sales.csv")',
    'customers <- read.csv("customers.csv")',
    'products <- read.csv("products.csv")',
    'sales_clean <- sales |> transform(region = toupper(region), month = format(order_date, "%Y-%m"))',
    'sales_filtered <- sales_clean %>% subset(amount > 100)',
    'sales_typed <- sales_filtered |> transform(discount_num = as.numeric(discount))',
    'sales_sorted <- sales_typed %>% arrange(desc(amount))',
    'sales_unique <- sales_sorted %>% distinct(customer_id)',
    'sales_enriched <- sales_unique %>% left_join(customers, by = "customer_id")',
    'sales_scored <- sales_enriched |> mutate(score_band = ifelse(amount > 500, "High", "Standard"))',
    'summary_tbl <- sales_scored %>% group_by(month, region) %>% summarise(avg_amount = mean(amount, na.rm = TRUE), customers = n_distinct(customer_id), .groups = "drop")',
    'revenue_model <- lm(amount ~ quantity + discount, data = sales_scored)',
    'scored_output <- sales_scored |> mutate(predicted_amount = predict(revenue_model, newdata = sales_scored))',
    'chart_obj <- scored_output %>% ggplot(aes(x = month, y = predicted_amount, color = region))',
    'ggsave("chart.png", plot = chart_obj)',
    'write.csv(summary_tbl, "summary.csv")'
  ), tmp)

  html_out <- tempfile(fileext = ".html")
  expect_equal(ghostwriteR::ghostwriteR(tmp, format = "html", out = html_out), html_out)

  html <- paste(readLines(html_out, warn = FALSE), collapse = "\n")
  expect_match(html, "Full dependency graph", fixed = TRUE)
  expect_match(html, "complete technical dependency map", fixed = TRUE)
  expect_false(grepl('data-graph-view-target="overview"', html, fixed = TRUE))
  expect_match(html, "data-phase-expand", fixed = TRUE)
  expect_match(html, "Feature engineering", fixed = TRUE)
  expect_match(html, "Modeling", fixed = TRUE)
  expect_match(html, "Scoring", fixed = TRUE)
})
