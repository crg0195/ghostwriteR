test_that("ghostwriteR parses SQL source tables, transformations, and output tables", {
  tmp <- tempfile(fileext = ".sql")
  writeLines(c(
    "CREATE TABLE analytics.monthly_sales AS",
    "SELECT",
    "  s.region,",
    "  DATE_TRUNC('month', s.order_date) AS sales_month,",
    "  COUNT(DISTINCT s.customer_id) AS customers,",
    "  SUM(s.net_revenue) AS total_revenue,",
    "  AVG(s.net_revenue) AS avg_revenue",
    "FROM raw.sales s",
    "JOIN raw.customers c ON s.customer_id = c.customer_id",
    "WHERE s.net_revenue > 100 AND c.is_active = 1",
    "GROUP BY s.region, DATE_TRUNC('month', s.order_date)",
    "ORDER BY sales_month DESC;",
    "",
    "COPY analytics.monthly_sales TO 'output/monthly_sales.csv';"
  ), tmp)

  edges <- ghostwriteR::ghostwriter_edges(tmp)
  nodes <- attr(edges, "nodes")
  steps <- attr(edges, "steps")

  expect_true(any(nodes$kind == "source_table" & grepl("raw.sales", nodes$label, fixed = TRUE)))
  expect_true(any(nodes$kind == "source_table" & grepl("raw.customers", nodes$label, fixed = TRUE)))
  expect_true(any(nodes$kind == "output_table" & grepl("analytics.monthly_sales", nodes$label, fixed = TRUE)))
  expect_true(any(nodes$kind == "output_file" & grepl("monthly_sales.csv", nodes$label, fixed = TRUE)))

  expect_true(any(steps$title == "Use source table"))
  expect_true(any(steps$title == "Join tables"))
  expect_true(any(steps$title == "Filter rows"))
  expect_true(any(steps$title == "Summarize data"))
  expect_true(any(steps$title == "Create table"))
  expect_true(any(steps$title == "Export query results"))
  expect_true(any(grepl("INNER JOIN", steps$detail, fixed = TRUE)))

  expect_true(any(edges$type == "reads"))
  expect_true(any(edges$type == "flows"))
  expect_true(any(edges$type == "creates"))
  expect_true(any(edges$type == "writes"))
})

test_that("ghostwriter_report explains SQL workflows in plain English", {
  tmp <- tempfile(fileext = ".sql")
  writeLines(c(
    "CREATE VIEW analytics.active_customers AS",
    "SELECT",
    "  region,",
    "  COUNT(DISTINCT customer_id) AS customers,",
    "  AVG(net_revenue) AS avg_revenue",
    "FROM raw.sales",
    "WHERE net_revenue >= 100 AND customer_id IS NOT NULL",
    "GROUP BY region;"
  ), tmp)

  report <- ghostwriteR::ghostwriter_report(tmp)

  expect_match(report, "external source such as a file or database table", fixed = TRUE)
  expect_match(report, "number of distinct customer_id", fixed = TRUE)
  expect_match(report, "average of net_revenue", fixed = TRUE)
  expect_match(report, "net_revenue is at least 100 and customer_id is present", fixed = TRUE)
  expect_match(report, "create view `analytics.active_customers`", fixed = TRUE)
  expect_match(report, "group rows by region", fixed = TRUE)
  expect_match(report, "## Column Inventories", fixed = TRUE)
  expect_match(report, "Step-By-Step Inventory", fixed = TRUE)
  expect_match(report, "Outputs: region, customers, avg_revenue", fixed = TRUE)
})

test_that("sql parser avoids duplicating aggregate metrics as feature-engineering steps", {
  tmp <- tempfile(fileext = ".sql")
  writeLines(c(
    "WITH regional_summary AS (",
    "    SELECT region, COUNT(DISTINCT customer_id) AS customers, AVG(net_revenue) AS avg_revenue",
    "    FROM raw.sales",
    "    GROUP BY region",
    ")",
    "SELECT * FROM regional_summary;"
  ), tmp)

  parsed <- ghostwriteR:::ghostwriter_parse(tmp)
  steps <- parsed$steps

  expect_true(any(steps$title == "Summarize data"))
  expect_false(any(
    steps$title == "Add or change columns" &
      grepl("customers = number of distinct customer_id", steps$detail, fixed = TRUE)
  ))
  expect_true(any(
    steps$title == "Summarize data" &
      grepl("group rows by region", steps$detail, fixed = TRUE)
  ))
})

test_that("sql parser humanizes coalesce nullif logic without duplicated words", {
  tmp <- tempfile(fileext = ".sql")
  writeLines(c(
    "SELECT",
    "  COALESCE(NULLIF(channel, ''), 'UNKNOWN') AS channel_normalized",
    "FROM raw.sales;"
  ), tmp)

  parsed <- ghostwriteR:::ghostwriter_parse(tmp)
  steps <- parsed$steps

  expect_true(any(
    steps$title == "Add or change columns" &
      grepl("channel_normalized = if channel is blank use UNKNOWN", steps$detail, fixed = TRUE)
  ))
})

test_that("ghostwriteR exports SQL workflows to self-contained html", {
  skip_if_not_installed("DiagrammeRsvg")

  tmp <- tempfile(fileext = ".sql")
  writeLines(c(
    "CREATE TABLE analytics.region_sales AS",
    "SELECT region, SUM(net_revenue) AS total_revenue",
    "FROM raw.sales",
    "GROUP BY region;"
  ), tmp)

  html_out <- tempfile(fileext = ".html")
  expect_equal(ghostwriteR::ghostwriteR(tmp, format = "html_graph", out = html_out), html_out)
  expect_true(file.exists(html_out))

  html <- paste(readLines(html_out, warn = FALSE), collapse = "\n")
  expect_match(html, "GhostwriteR Graph Only", fixed = TRUE)
  expect_match(html, "analytics.region_sales", fixed = TRUE)
  expect_match(html, "raw.sales", fixed = TRUE)
})

test_that("ghostwriteR handles CTE-heavy SQL with date filters in html export", {
  skip_if_not_installed("DiagrammeRsvg")

  tmp <- tempfile(fileext = ".sql")
  writeLines(c(
    "CREATE TABLE analytics.monthly_sales_kpis AS",
    "WITH filtered_sales AS (",
    "    SELECT DATE_TRUNC('month', order_date) AS sales_month, order_date, customer_id, product_id, region, units, unit_price, units * unit_price AS gross_revenue",
    "    FROM raw.sales",
    "    WHERE order_date >= DATE '2025-01-01'",
    "      AND order_date < DATE '2026-01-01'",
    "      AND customer_id IS NOT NULL",
    "),",
    "regional_summary AS (",
    "    SELECT region, COUNT(DISTINCT customer_id) AS customers, AVG(gross_revenue) AS avg_revenue",
    "    FROM filtered_sales",
    "    GROUP BY region",
    ")",
    "SELECT r.region, r.customers, r.avg_revenue, t.target_revenue, r.avg_revenue - t.target_revenue AS revenue_gap",
    "FROM regional_summary r",
    "LEFT JOIN raw.monthly_targets t ON r.region = t.region",
    "WHERE r.avg_revenue >= 100",
    "ORDER BY r.region DESC;",
    "",
    "COPY analytics.monthly_sales_kpis TO 'output/monthly_sales_kpis.csv';"
  ), tmp)

  html_out <- tempfile(fileext = ".html")
  expect_equal(ghostwriteR::ghostwriteR(tmp, format = "html", out = html_out), html_out)
  expect_true(file.exists(html_out))

  parsed <- ghostwriteR:::ghostwriter_parse(tmp)
  steps <- parsed$steps
  expect_false(any(steps$title == "Select columns"))
  expect_true(any(steps$title == "Create table"))
  expect_true(any(steps$title == "Export query results" & steps$source == "analytics.monthly_sales_kpis"))
  expect_true(any(steps$title == "Sort rows"))
  expect_true(any(steps$title == "Add or change columns" & grepl("truncate order_date to month", steps$detail, fixed = TRUE)))
  expect_true(any(
    steps$title == "Summarize data" &
      grepl("group rows by", steps$detail, fixed = TRUE) &
      grepl("region", steps$detail, fixed = TRUE)
  ))
  expect_true(any(
    steps$title == "Join tables" &
      grepl("LEFT JOIN", steps$detail, fixed = TRUE)
  ))
  expect_true(any(
    steps$title == "Filter grouped results" &
      grepl("region groups", steps$detail, fixed = TRUE)
  ))
  expect_true(any(
    steps$title == "Add or change columns" &
      grepl("revenue_gap = r.avg_revenue - t.target_revenue", steps$detail, fixed = TRUE) &
      grepl("negative means below target; positive means above target", steps$detail, fixed = TRUE)
  ))
  expect_false(any(steps$title == "Create CTE result"))

  html <- paste(readLines(html_out, warn = FALSE), collapse = "\n")
  expect_match(html, "filtered_sales", fixed = TRUE)
  expect_match(html, "regional_summary", fixed = TRUE)
  expect_match(html, "monthly_sales_kpis.csv", fixed = TRUE)
  expect_match(html, "Full dependency graph", fixed = TRUE)
  expect_false(grepl('data-graph-view-target="overview"', html, fixed = TRUE))
  expect_match(html, "Column inventories", fixed = TRUE)
  expect_match(html, "Data tables and outputs", fixed = TRUE)
  expect_match(html, "Step-by-step inventory", fixed = TRUE)
  expect_match(html, "Input columns used", fixed = TRUE)
  expect_match(html, "Output columns produced", fixed = TRUE)
  expect_match(html, "inventoryNamedInput", fixed = TRUE)
  expect_match(html, "inventoryStepInput", fixed = TRUE)
  expect_match(html, "1 in", fixed = TRUE)
  expect_lt(regexpr("Working table: filtered_sales", html, fixed = TRUE)[[1]], regexpr("Working table: regional_summary", html, fixed = TRUE)[[1]])
  expect_lt(regexpr("Working table: regional_summary", html, fixed = TRUE)[[1]], regexpr("Final query", html, fixed = TRUE)[[1]])
  expect_lt(regexpr("Final query", html, fixed = TRUE)[[1]], regexpr("Export", html, fixed = TRUE)[[1]])
})

test_that("sql inventories capture columns used and produced for major steps", {
  tmp <- tempfile(fileext = ".sql")
  writeLines(c(
    "CREATE TABLE analytics.monthly_sales_kpis AS",
    "WITH filtered_sales AS (",
    "    SELECT",
    "        DATE_TRUNC('month', s.order_date) AS sales_month,",
    "        s.customer_id,",
    "        s.region,",
    "        s.channel,",
    "        s.units,",
    "        s.unit_price,",
    "        s.units * s.unit_price AS gross_revenue",
    "    FROM raw.sales s",
    "    WHERE s.units > 0",
    "),",
    "regional_summary AS (",
    "    SELECT",
    "        sales_month,",
    "        region,",
    "        channel,",
    "        COUNT(DISTINCT customer_id) AS customers,",
    "        SUM(gross_revenue) AS total_revenue",
    "    FROM filtered_sales",
    "    GROUP BY sales_month, region, channel",
    ")",
    "SELECT",
    "    r.sales_month,",
    "    r.region,",
    "    r.channel,",
    "    r.customers,",
    "    r.total_revenue,",
    "    t.target_revenue,",
    "    r.total_revenue - t.target_revenue AS revenue_gap",
    "FROM regional_summary r",
    "LEFT JOIN raw.monthly_targets t ON r.sales_month = t.sales_month AND r.region = t.region",
    "WHERE r.total_revenue >= 1000",
    "ORDER BY r.sales_month DESC;",
    "",
    "COPY analytics.monthly_sales_kpis TO 'output/monthly_sales_kpis.csv';"
  ), tmp)

  parsed <- ghostwriteR:::ghostwriter_parse(tmp)
  steps <- parsed$steps

  summarize_step <- steps[steps$title == "Summarize data", , drop = FALSE]
  expect_true(nrow(summarize_step) >= 1L)
  expect_match(summarize_step$input_columns[[1]], "sales_month", fixed = TRUE)
  expect_match(summarize_step$input_columns[[1]], "customer_id", fixed = TRUE)
  expect_match(summarize_step$output_columns[[1]], "customers", fixed = TRUE)
  expect_match(summarize_step$output_columns[[1]], "total_revenue", fixed = TRUE)

  join_step <- steps[steps$title == "Join tables" & grepl("monthly_targets", steps$detail, fixed = TRUE), , drop = FALSE]
  expect_true(nrow(join_step) >= 1L)
  expect_match(join_step$input_columns[[1]], "r.sales_month", fixed = TRUE)
  expect_match(join_step$input_columns[[1]], "t.region", fixed = TRUE)

  export_step <- steps[steps$title == "Export query results", , drop = FALSE]
  expect_true(nrow(export_step) >= 1L)
  expect_match(export_step$input_columns[[1]], "revenue_gap", fixed = TRUE)
  expect_match(export_step$output_columns[[1]], "target_revenue", fixed = TRUE)

  report <- ghostwriteR::ghostwriter_report(tmp)
  expect_match(report, "Input columns used:", fixed = TRUE)
  expect_match(report, "Output columns produced:", fixed = TRUE)
  expect_match(report, "revenue_gap", fixed = TRUE)
  expect_match(report, "negative means below target; positive means above target", fixed = TRUE)
})

test_that("sql named-table inventories keep pass-through and created columns separate", {
  tmp <- tempfile(fileext = ".sql")
  writeLines(c(
    "WITH filtered_sales AS (",
    "  SELECT sales_month, customer_id, product_id, region, channel, units, net_revenue",
    "  FROM raw.sales",
    "),",
    "enriched_sales AS (",
    "  SELECT",
    "    f.sales_month,",
    "    f.customer_id,",
    "    f.product_id,",
    "    UPPER(TRIM(f.region)) AS region,",
    "    COALESCE(NULLIF(f.channel, ''), 'UNKNOWN') AS channel,",
    "    f.units,",
    "    f.net_revenue,",
    "    c.customer_name,",
    "    c.segment,",
    "    p.product_name,",
    "    p.category",
    "  FROM filtered_sales f",
    "  LEFT JOIN raw.customers c ON f.customer_id = c.customer_id",
    "  LEFT JOIN raw.products p ON f.product_id = p.product_id",
    ")",
    "SELECT * FROM enriched_sales;"
  ), tmp)

  parsed <- ghostwriteR:::ghostwriter_parse(tmp)
  steps <- parsed$steps
  enriched_step <- steps[steps$output == "enriched_sales", , drop = FALSE]

  expect_equal(nrow(enriched_step), 1L)
  expect_match(enriched_step$output_columns[[1]], "customer_name", fixed = TRUE)
  expect_match(enriched_step$output_columns[[1]], "product_name", fixed = TRUE)
  expect_match(enriched_step$carried_columns[[1]], "sales_month", fixed = TRUE)
  expect_match(enriched_step$carried_columns[[1]], "customer_name", fixed = TRUE)
  expect_match(enriched_step$created_columns[[1]], "region", fixed = TRUE)
  expect_match(enriched_step$created_columns[[1]], "channel", fixed = TRUE)

  report <- ghostwriteR::ghostwriter_report(tmp)
  expect_match(report, "Carried through:", fixed = TRUE)
  expect_match(report, "Newly created or changed:", fixed = TRUE)
})

test_that("sql source steps are deduplicated when the same raw table is reused", {
  tmp <- tempfile(fileext = ".sql")
  writeLines(c(
    "WITH first_pass AS (",
    "  SELECT customer_id, region FROM raw.sales",
    "),",
    "second_pass AS (",
    "  SELECT customer_id, region FROM raw.sales",
    ")",
    "SELECT *",
    "FROM first_pass",
    "JOIN second_pass ON first_pass.customer_id = second_pass.customer_id;"
  ), tmp)

  parsed <- ghostwriteR:::ghostwriter_parse(tmp)
  source_steps <- parsed$steps[parsed$steps$title == "Use source table" & parsed$steps$detail == "raw.sales", , drop = FALSE]

  expect_equal(nrow(source_steps), 1L)
})

test_that("sql clause extraction tolerates flexible whitespace", {
  tmp <- tempfile(fileext = ".sql")
  writeLines(c(
    "SELECT region, COUNT(*) AS orders",
    "FROM raw.sales",
    "GROUP  BY region",
    "ORDER",
    "BY region DESC;"
  ), tmp)

  parsed <- ghostwriteR:::ghostwriter_parse(tmp)
  steps <- parsed$steps

  expect_true(any(steps$title == "Summarize data" & grepl("group rows by region", steps$detail, fixed = TRUE)))
  expect_true(any(steps$title == "Sort rows" & grepl("region in descending order", steps$detail, fixed = TRUE)))
})

test_that("filter grouped results is classified as aggregation in the timeline", {
  step_row <- data.frame(
    step = 1L,
    kind = "transform",
    group = "Final query",
    title = "Filter grouped results",
    detail = "keep only region groups where total_revenue is at least 1000",
    source = "regional_summary",
    output = "",
    target_path = "",
    input_columns = "",
    output_columns = "",
    narrative = "Using `regional_summary`, filter grouped results: keep only region groups where total_revenue is at least 1000.",
    stringsAsFactors = FALSE
  )

  expect_equal(ghostwriteR:::step_phase_label(step_row), "Aggregation")
})
