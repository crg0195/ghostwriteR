test_that("ghostwriteR returns a DiagrammeR graph with readable labels and a legend", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'sales <- read.csv("sales.csv")',
    'sales_filtered <- sales %>% subset(total > 100)',
    'write.csv(sales_filtered, "filtered_sales.csv")'
  ), tmp)

  graph <- ghostwriteR::ghostwriteR(tmp)

  expect_true(inherits(graph, "grViz"))
  expect_match(graph$x$diagram, "GhostwriteR Process Map", fixed = TRUE)
  expect_match(graph$x$diagram, "Filter rows", fixed = TRUE)
  expect_match(graph$x$diagram, "Source data file", fixed = TRUE)
  expect_match(graph$x$diagram, "Output file", fixed = TRUE)
  expect_match(graph$x$diagram, "Legend", fixed = TRUE)
  expect_match(graph$x$diagram, "Data frame", fixed = TRUE)
  expect_match(graph$x$diagram, '\\"sales_filtered\\"', fixed = TRUE)
})

test_that("R parser handles realistic workflow details for handoff users", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'sales <- read.csv("data/sales_transactions.csv")',
    'customers <- read.csv("data/customers.csv")',
    'products <- read.csv("data/products.csv")',
    'targets <- read.csv("data/monthly_targets.csv")',
    'sales_clean <- sales |> mutate(order_date = as.Date(order_date), month = format(order_date, "%Y-%m"), region = toupper(trimws(region)), channel = ifelse(channel == "", "UNKNOWN", channel), units = as.numeric(units), unit_price = as.numeric(unit_price), gross_revenue = units * unit_price, net_revenue = gross_revenue * (1 - discount_pct)) |> subset(!is.na(customer_id) & !is.na(product_id) & units > 0 & unit_price > 0)',
    'sales_enriched <- sales_clean %>% left_join(customers, by = "customer_id") %>% left_join(products, by = "product_id")',
    'regional_monthly_summary <- sales_enriched %>% group_by(month, region, channel) %>% summarise(orders = n(), revenue = sum(net_revenue, na.rm = TRUE), avg_order_value = mean(net_revenue, na.rm = TRUE), .groups = "drop") %>% left_join(targets, by = c("month", "region"))',
    'revenue_model <- lm(net_revenue ~ units + unit_price + discount_pct, data = sales_enriched)',
    'scored_sales <- sales_enriched |> mutate(predicted_revenue = predict(revenue_model, newdata = sales_enriched))',
    'top_regions_plot <- regional_monthly_summary %>% ggplot(aes(x = month, y = revenue, color = region, group = region)) + geom_line() + geom_point()',
    'write.csv(regional_monthly_summary, "output/regional_monthly_summary.csv")',
    'write.csv(scored_sales, "output/scored_sales.csv")',
    'write.csv(sales_enriched, "output/sales_enriched.csv")',
    'saveRDS(revenue_model, "output/revenue_model.rds")',
    'ggsave("output/top_regions_plot.png", plot = top_regions_plot, width = 10, height = 6)'
  ), tmp)

  parsed <- ghostwriteR:::ghostwriter_parse(tmp)
  steps <- parsed$steps
  stats <- ghostwriteR:::workflow_stats(parsed)

  filter_step <- steps[steps$title == "Filter rows", , drop = FALSE]
  expect_true(any(grepl("customer_id is present", filter_step$detail, fixed = TRUE)))
  expect_false(any(grepl("!customer_id is missing", filter_step$detail, fixed = TRUE)))

  model_step <- steps[steps$title == "Fit linear model", , drop = FALSE]
  expect_true(any(model_step$source == "sales_enriched"))

  join_step <- steps[steps$title == "Join tables", , drop = FALSE]
  expect_true(any(grepl("LEFT JOIN", join_step$detail, fixed = TRUE)))

  chart_step <- steps[steps$title == "Create chart", , drop = FALSE]
  expect_true(any(grepl("month on the x-axis", chart_step$detail, fixed = TRUE)))
  expect_true(any(grepl("revenue on the y-axis", chart_step$detail, fixed = TRUE)))
  expect_true(any(grepl("color by region", chart_step$detail, fixed = TRUE)))

  save_plot_step <- steps[steps$title == "Save visual output", , drop = FALSE]
  expect_true(any(save_plot_step$source == "top_regions_plot"))
  expect_true(any(save_plot_step$target_path == "output/top_regions_plot.png"))
  expect_true(any(grepl("top_regions_plot.png", save_plot_step$detail, fixed = TRUE)))

  expect_equal(stats$outputs, 5)
  expect_equal(stats$object_label, "Named objects")
  expect_true(stats$object_counts[["data_frame"]] >= 1L)
  expect_true(stats$object_counts[["model_object"]] >= 1L)
  expect_true(stats$object_counts[["plot_object"]] >= 1L)
  expect_true(any(parsed$nodes$kind == "model_object" & grepl("revenue_model", parsed$nodes$label, fixed = TRUE)))
  expect_true(any(parsed$nodes$kind == "plot_object" & grepl("top_regions_plot", parsed$nodes$label, fixed = TRUE)))
  expect_true(any(parsed$nodes$kind == "data_frame" & grepl('"sales_enriched"', parsed$nodes$label, fixed = TRUE)))
})

test_that("mixed R workflows show distinct object labels in the graph legend", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'sales <- read.csv("sales.csv")',
    'summary_tbl <- sales %>% group_by(region) %>% summarise(avg_amount = mean(amount, na.rm = TRUE), .groups = "drop")',
    'revenue_model <- lm(amount ~ quantity + discount, data = summary_tbl)',
    'chart_obj <- summary_tbl %>% ggplot(aes(x = region, y = avg_amount, color = region))'
  ), tmp)

  graph <- ghostwriteR::ghostwriteR(tmp)

  expect_match(graph$x$diagram, "Data frame", fixed = TRUE)
  expect_match(graph$x$diagram, "Model", fixed = TRUE)
  expect_match(graph$x$diagram, "Chart object", fixed = TRUE)
})

test_that("R workflow phases respect phase_rank order instead of alphabetical order", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'sales <- read.csv("sales.csv")',
    'sales_clean <- sales %>% subset(amount > 100)',
    'sales_flagged <- sales_clean |> mutate(flag = ifelse(amount > 500, "High", "Standard"))',
    'sales_joined <- sales_flagged %>% left_join(customers, by = "customer_id")',
    'summary_tbl <- sales_joined %>% group_by(region) %>% summarise(avg_amount = mean(amount, na.rm = TRUE), .groups = "drop")',
    'model <- lm(amount ~ quantity + discount, data = sales_joined)',
    'scored <- sales_joined |> mutate(predicted_amount = predict(model, newdata = sales_joined))',
    'chart_obj <- summary_tbl %>% ggplot(aes(x = region, y = avg_amount, color = region))',
    'write.csv(summary_tbl, "summary.csv")'
  ), tmp)

  parsed <- ghostwriteR:::ghostwriter_parse(tmp)
  groups <- ghostwriteR:::workflow_timeline_groups(parsed$steps)

  expect_equal(
    names(groups),
    c("Inputs", "Cleaning", "Feature engineering", "Joins", "Aggregation", "Modeling", "Scoring", "Visualization", "Outputs")
  )
})

test_that("basename path style reduces full file paths in parsed output", {
  old_options <- options(ghostwriteR.path_style = "basename")
  on.exit(options(old_options), add = TRUE)

  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'sales <- read.csv("/Users/example/private/sales.csv")',
    'write.csv(sales, "/Users/example/private/output/final.csv")'
  ), tmp)

  parsed <- ghostwriteR:::ghostwriter_parse(tmp)
  steps <- parsed$steps

  expect_true(any(steps$target_path == "sales.csv"))
  expect_true(any(steps$target_path == "final.csv"))
  expect_false(any(grepl("/Users/example/private", steps$target_path, fixed = TRUE)))
})
