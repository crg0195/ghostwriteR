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
  stats <- ghostwriteR:::workflow_stats(parsed)

  expect_true(any(steps$target_path == "sales.csv"))
  expect_true(any(steps$target_path == "final.csv"))
  expect_false(any(grepl("/Users/example/private", steps$target_path, fixed = TRUE)))
})

test_that("unnamed callable transforms do not crash parsing", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'sales <- read.csv("sales.csv")',
    'result <- (function(d) d)(sales)'
  ), tmp)

  expect_no_error(ghostwriteR:::ghostwriter_parse(tmp))
})

test_that("R parser recognizes discovered JSON files and list-based imports", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'files <- dir("data/spotify", pattern = "\\\\.json", full.names = TRUE)',
    'df_list <- vector("list", length(files))',
    'for (i in seq_along(files)) {',
    '  df_list[[i]] <- fromJSON(files[[i]], flatten = TRUE)',
    '}',
    'streamingData1 <- bind_rows(df_list)'
  ), tmp)

  parsed <- ghostwriteR:::ghostwriter_parse(tmp)
  steps <- parsed$steps
  stats <- ghostwriteR:::workflow_stats(parsed)

  discover_step <- steps[steps$title == "Discover source data files", , drop = FALSE]
  import_step <- steps[steps$title == "Load JSON files", , drop = FALSE]
  combine_step <- steps[steps$title == "Combine imported files", , drop = FALSE]

  expect_equal(nrow(discover_step), 1)
  expect_equal(nrow(import_step), 1)
  expect_equal(nrow(combine_step), 1)
  expect_true(discover_step$step[[1]] < import_step$step[[1]])
  expect_true(import_step$step[[1]] < combine_step$step[[1]])
  expect_match(discover_step$detail[[1]], "JSON files", fixed = TRUE)
  expect_match(import_step$detail[[1]], "read the JSON files listed in `files`", fixed = TRUE)
  expect_equal(import_step$output[[1]], "df_list")
  expect_equal(combine_step$source[[1]], "df_list")
  expect_equal(combine_step$output[[1]], "streamingData1")
  expect_equal(stats$inputs, 1)
})

test_that("R parser handles magrittr pipes and older tidyverse verbs cleanly", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'library(jsonlite)',
    'files <- dir("data/spotify", pattern = "\\\\.json", full.names = TRUE)',
    'df_list <- vector("list", length(files))',
    'for (i in seq_along(files)) {',
    '  df_list[[i]] <- fromJSON(files[[i]], flatten = TRUE)',
    '}',
    'streamingData1 <- bind_rows(df_list)',
    'streamingData1 %<>% filter(!artist %in% blocked_artists)',
    'streamingData <- streamingData1 %>% as_tibble() %>% mutate_at("ts", ymd_hms) %>% mutate(date = floor_date(ts, "day") %>% as_date)'
  ), tmp)

  parsed <- ghostwriteR:::ghostwriter_parse(tmp)
  steps <- parsed$steps

  expect_true(any(steps$title == "Filter rows" & steps$output == "streamingData1"))
  expect_true(any(steps$title == "Convert to data frame"))
  expect_true(any(steps$title == "Add or change columns" & grepl("update ts using ymd_hms", steps$detail, fixed = TRUE)))
  expect_true(any(grepl('date = floor_date\\(ts, "day"\\) then as_date', steps$detail)))
  expect_true(any(grepl("artist is not in blocked_artists", steps$detail, fixed = TRUE)))
})

test_that("R parser ignores commented-out pipe fragments and captures standalone workflow calls", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'library(echarts4r)',
    'season <- data.frame(season = c("Winter", "Spring"), minutes = c(10, 20), songs = c(1, 2))',
    'e_common(font_family = "Sans", theme = NULL)',
    'songs <- season %>% filter(minutes >= 10) %>%',
    '  group_by(season) %>%',
    '  # group_by(date = floor_date(date, "month")) %>%',
    '  summarise(songs = n()) %>%',
    '  arrange(desc(songs)) %>%',
    '  e_charts(season) |>',
    '  e_bar(songs, name = "Songs") |>',
    '  # e_step(minutes, name = "Broken|>") |>',
    '  e_title("Total Song Listening per Season", subtext = "From 2014-2023")',
    'songs',
    'mins <- season %>% count(season, sort = TRUE)',
    'e_arrange(mins, songs)',
    'season %>% count(minutes, sort = TRUE)',
    'season %>% count(songs, sort = TRUE)'
  ), tmp)

  parsed <- ghostwriteR:::ghostwriter_parse(tmp)
  steps <- parsed$steps

  expect_false(any(grepl("Broken", steps$detail, fixed = TRUE)))
  expect_false(any(grepl("floor_date\\(date, \"month\"\\)", steps$detail)))
  expect_true(any(steps$title == "Set chart defaults"))
  expect_true(any(steps$title == "Arrange charts"))
  expect_true(sum(steps$title == "Count records") >= 3)
  expect_true(any(steps$title == "Display object" & steps$source == "songs"))
})

test_that("R parser marks overwritten objects as replaced later", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    'season <- streamingData %>% mutate(season = season(date))',
    'season <- streamingData %>% mutate(season = case_when(season(date) == "DJF" ~ "Winter", .default = "UNK"))',
    'season_counts <- season %>% count(master_metadata_album_artist_name, sort = TRUE)',
    'season_counts <- season %>% group_by(master_metadata_album_artist_name) %>% summarise(sum = sum(minutes))'
  ), tmp)

  parsed <- ghostwriteR:::ghostwriter_parse(tmp)
  steps <- parsed$steps

  season_steps <- steps[steps$output == "season", , drop = FALSE]
  counts_steps <- steps[steps$output == "season_counts", , drop = FALSE]

  expect_equal(nrow(season_steps), 2)
  expect_equal(nrow(counts_steps), 2)
  expect_true(nzchar(season_steps$superseded_by[[1]]))
  expect_equal(season_steps$superseded_by[[1]], as.character(season_steps$step[[2]]))
  expect_true(grepl("replaced later at step", season_steps$detail[[1]], fixed = TRUE))
  expect_true(nzchar(counts_steps$superseded_by[[1]]))
  expect_equal(counts_steps$superseded_by[[1]], as.character(counts_steps$step[[2]]))
})
