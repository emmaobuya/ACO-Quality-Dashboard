# ==============================================================================
# Acclaim -- Emergency Department Quality & Safety Dashboard (LIVE)
# ==============================================================================
# Connects directly to KoboToolbox -- no export file needed. Polls the API
# every REFRESH_SECONDS (set in kobo_connect.R) and refreshes automatically.
#
# BEFORE RUNNING: open kobo_connect.R and set KOBO_TOKEN to your own API
# token (Kobo -> Account Settings -> API). KOBO_SERVER and ASSET_UID are
# already filled in for this form.
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(lubridate)

source("kobo_connect.R")   # config, fetch, parse -- see that file for details

# ------------------------------------------------------------------------------
# Metric definitions for Form 1 (used by Overview + Facility Trends)
# ------------------------------------------------------------------------------
metrics_list <- list(
  submissions        = list(label = "Number of submissions", type = "count"),
  patients_seen      = list(label = "Total patients seen", type = "sum"),
  total_deaths       = list(label = "Total deaths", type = "sum"),
  death_rate         = list(label = "Death rate (%)", type = "rate",
                             numerator = "total_deaths", denominator = "patients_seen"),
  lwbs_rate          = list(label = "LWBS rate (%)", type = "rate",
                             numerator = "lwbs", denominator = "patients_seen"),
  sepsis_bundle_rate = list(label = "Sepsis bundle compliance (%)", type = "rate",
                             numerator = "sepsis_bundle_completed", denominator = "sepsis_cases"),
  red_delay_60min    = list(label = "Red-triage >60min delays", type = "sum"),
  yellow_delay_2h    = list(label = "Yellow-triage >2h delays", type = "sum"),
  equipment_failures = list(label = "Equipment failures", type = "sum"),
  preventable_count  = list(label = "Preventable-problem cases", type = "sum")
)
metric_choices <- setNames(names(metrics_list), sapply(metrics_list, `[[`, "label"))

compute_metric <- function(df, group_vars, metric_key) {
  m <- metrics_list[[metric_key]]
  if (m$type == "count") {
    df %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(value = n(), .groups = "drop")
  } else if (m$type == "sum") {
    df %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(value = sum(.data[[metric_key]], na.rm = TRUE), .groups = "drop")
  } else {
    df %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        value = ifelse(sum(.data[[m$denominator]], na.rm = TRUE) == 0, NA,
                        100 * sum(.data[[m$numerator]], na.rm = TRUE) /
                              sum(.data[[m$denominator]], na.rm = TRUE)),
        .groups = "drop"
      )
  }
}

# Facility/shift choices come from the form definition, not the data, so the
# UI can be built before the first API call completes.
facility_choices <- unname(facility_lookup)
shift_choices <- unname(shift_lookup)

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- page_navbar(
  title = "Acclaim -- ED Quality Dashboard (Live)",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = TRUE,

  sidebar = sidebar(
    width = 280,
    h5("Filters"),
    selectInput("f_facility", "Health Facility",
                choices = c("All Facilities", facility_choices),
                selected = "All Facilities"),
    checkboxGroupInput("f_shift", "Shift", choices = shift_choices,
                        selected = shift_choices),
    dateRangeInput("f_dates", "Date range",
                    start = Sys.Date() - 90, end = Sys.Date()),
    hr(),
    textOutput("connection_status"),
    p(class = "text-muted small",
      paste0("Auto-refreshes every ", REFRESH_SECONDS, " seconds. ",
             "Case-level narratives are intentionally excluded -- only categories and counts are shown."))
  ),

  nav_panel(
    "Overview",
    layout_columns(
      col_widths = c(2, 3, 2, 2, 3),
      value_box(title = "Total submissions", value = textOutput("kpi_total_submissions"), showcase = icon("clipboard-list")),
      value_box(title = "Patients seen", value = textOutput("kpi_patients"), showcase = icon("users")),
      value_box(title = "Death rate", value = textOutput("kpi_death_rate"), showcase = icon("heart-pulse")),
      value_box(title = "LWBS rate", value = textOutput("kpi_lwbs_rate"), showcase = icon("person-walking-arrow-right")),
      value_box(title = "Sepsis bundle compliance", value = textOutput("kpi_sepsis_rate"), showcase = icon("syringe"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(
          div(class = "d-flex justify-content-between align-items-center",
              span("Compare facilities"),
              selectInput("overview_metric_facility", NULL, choices = metric_choices, width = "260px"))
        ),
        plotlyOutput("facility_compare_chart", height = "320px")
      ),
      card(
        card_header(
          div(class = "d-flex justify-content-between align-items-center",
              span("Trend over time"),
              selectInput("overview_metric_trend", NULL, choices = metric_choices, width = "260px"))
        ),
        plotlyOutput("trend_chart", height = "320px")
      )
    )
  ),

  nav_panel(
    "Facility Trends",
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Focus facility"),
        selectInput("focus_facility", NULL, choices = facility_choices, selected = facility_choices[1]),
        p(class = "text-muted small",
          "Shows this facility's own KPI trend against the filters selected in the sidebar.")
      ),
      card(
        card_header("Shifts logged"),
        plotlyOutput("focus_shift_count", height = "150px")
      )
    ),
    card(
      card_header("KPI trend for selected facility"),
      selectInput("focus_metric", NULL, choices = metric_choices, width = "300px"),
      plotlyOutput("focus_trend_chart", height = "320px")
    )
  ),

  nav_panel(
    "Event Log (Form 2)",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Why cases were logged (trigger reasons)"),
        plotlyOutput("trigger_chart", height = "320px")
      ),
      card(
        card_header("Cases by category and triage color"),
        plotlyOutput("category_triage_chart", height = "320px")
      )
    ),
    card(
      card_header("Logged cases (summary -- narratives not shown)"),
      DTOutput("event_log_table")
    )
  ),

  nav_panel(
    "Patient Flow (Form 3)",
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(title = "Avg. time to triage", value = textOutput("kpi_time_triage"), showcase = icon("stopwatch")),
      value_box(title = "Avg. time to clinician", value = textOutput("kpi_time_clinician"), showcase = icon("user-doctor")),
      value_box(title = "Avg. length of stay", value = textOutput("kpi_los"), showcase = icon("hourglass-half"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Outcome at 24 hours"),
        plotlyOutput("outcome_chart", height = "300px")
      ),
      card(
        card_header("Main diagnosis"),
        plotlyOutput("diagnosis_chart", height = "300px")
      )
    ),
    card(
      card_header("Patient flow records"),
      DTOutput("flow_table")
    )
  )
)

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Poll KoboToolbox on a timer ----
  live_data <- reactivePoll(
    intervalMillis = REFRESH_SECONDS * 1000,
    session = session,
    checkFunc = function() Sys.time(),   # always re-check; valueFunc does the real fetch
    valueFunc = function() load_live_data()
  )

  output$connection_status <- renderText({
    d <- live_data()
    paste0("Last synced: ", format(Sys.time(), "%H:%M:%S"),
           " -- ", nrow(d$form1), " shift record(s) loaded")
  })

  # One-time: once real data arrives, widen the date filter to cover it
  observeEvent(live_data(), {
    df <- live_data()$form1
    if (nrow(df) > 0) {
      updateDateRangeInput(session, "f_dates",
                            start = min(df$date, na.rm = TRUE),
                            end = max(df$date, na.rm = TRUE))
    }
  }, once = TRUE)

  # ---- Filtered datasets, reactive to sidebar + live data ----
  f1_filtered <- reactive({
    df <- live_data()$form1
    if (nrow(df) == 0) return(df)
    if (input$f_facility != "All Facilities") df <- df %>% filter(facility == input$f_facility)
    df %>%
      filter(
        shift %in% input$f_shift,
        date >= input$f_dates[1], date <= input$f_dates[2]
      )
  })

  f2_filtered <- reactive({
    df <- live_data()$form2
    if (nrow(df) == 0) return(df)
    if (input$f_facility != "All Facilities") df <- df %>% filter(facility == input$f_facility)
    df %>%
      filter(
        event_date >= input$f_dates[1], event_date <= input$f_dates[2]
      )
  })

  f3_filtered <- reactive({
    df <- live_data()$form3
    if (nrow(df) == 0) return(df)
    if (input$f_facility != "All Facilities") df <- df %>% filter(facility == input$f_facility)
    df %>%
      filter(
        shift %in% input$f_shift,
        record_date >= input$f_dates[1], record_date <= input$f_dates[2]
      )
  })

  # ---- Overview KPIs ----
  output$kpi_total_submissions <- renderText({
    format(nrow(f1_filtered()), big.mark = ",")
  })

  output$kpi_patients <- renderText({
    df <- f1_filtered()
    if (nrow(df) == 0) return("--")
    format(sum(df$patients_seen, na.rm = TRUE), big.mark = ",")
  })

  output$kpi_death_rate <- renderText({
    df <- f1_filtered()
    if (nrow(df) == 0) return("--")
    tot <- sum(df$patients_seen, na.rm = TRUE)
    if (tot == 0) return("--")
    paste0(round(100 * sum(df$total_deaths, na.rm = TRUE) / tot, 1), "%")
  })

  output$kpi_lwbs_rate <- renderText({
    df <- f1_filtered()
    if (nrow(df) == 0) return("--")
    tot <- sum(df$patients_seen, na.rm = TRUE)
    if (tot == 0) return("--")
    paste0(round(100 * sum(df$lwbs, na.rm = TRUE) / tot, 1), "%")
  })

  output$kpi_sepsis_rate <- renderText({
    df <- f1_filtered()
    if (nrow(df) == 0) return("--")
    tot <- sum(df$sepsis_cases, na.rm = TRUE)
    if (tot == 0) return("--")
    paste0(round(100 * sum(df$sepsis_bundle_completed, na.rm = TRUE) / tot, 1), "%")
  })

  # ---- Overview: facility comparison chart ----
  output$facility_compare_chart <- renderPlotly({
    req(input$overview_metric_facility)
    df <- f1_filtered()
    if (nrow(df) == 0) return(plotly_empty(type = "bar") |> layout(title = "No data for current filters"))
    m <- compute_metric(df, "facility", input$overview_metric_facility)
    lbl <- metrics_list[[input$overview_metric_facility]]$label
    plot_ly(m, x = ~facility, y = ~value, type = "bar") |>
      layout(xaxis = list(title = ""), yaxis = list(title = lbl))
  })

  # ---- Overview: trend chart (all facilities, colored) ----
  output$trend_chart <- renderPlotly({
    req(input$overview_metric_trend)
    df <- f1_filtered()
    if (nrow(df) == 0) return(plotly_empty(type = "scatter") |> layout(title = "No data for current filters"))
    m <- compute_metric(df, c("date", "facility"), input$overview_metric_trend)
    lbl <- metrics_list[[input$overview_metric_trend]]$label
    plot_ly(m, x = ~date, y = ~value, color = ~facility, type = "scatter", mode = "lines+markers") |>
      layout(xaxis = list(title = ""), yaxis = list(title = lbl))
  })

  # ---- Facility Trends tab ----
  output$focus_shift_count <- renderPlotly({
    df <- f1_filtered() %>% filter(facility == input$focus_facility)
    if (nrow(df) == 0) return(plotly_empty(type = "bar") |> layout(title = "No data"))
    df <- df %>% count(shift)
    plot_ly(df, x = ~shift, y = ~n, type = "bar") |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Shifts logged"))
  })

  output$focus_trend_chart <- renderPlotly({
    req(input$focus_metric)
    df <- f1_filtered() %>% filter(facility == input$focus_facility)
    if (nrow(df) == 0) return(plotly_empty(type = "scatter") |> layout(title = "No data"))
    m <- compute_metric(df, "date", input$focus_metric)
    lbl <- metrics_list[[input$focus_metric]]$label
    plot_ly(m, x = ~date, y = ~value, type = "scatter", mode = "lines+markers") |>
      layout(xaxis = list(title = ""), yaxis = list(title = lbl))
  })

  # ---- Event Log tab ----
  output$trigger_chart <- renderPlotly({
    df <- f2_filtered()
    if (nrow(df) == 0) return(plotly_empty(type = "bar") |> layout(title = "No triggered cases for current filters"))
    counts <- df %>%
      tidyr::separate_rows(trigger_combined, sep = ",\\s*") %>%
      filter(trigger_combined != "") %>%
      count(trigger_combined, sort = TRUE)
    plot_ly(counts, x = ~n, y = ~reorder(trigger_combined, n), type = "bar", orientation = "h") |>
      layout(xaxis = list(title = "Cases"), yaxis = list(title = ""))
  })

  output$category_triage_chart <- renderPlotly({
    df <- f2_filtered()
    if (nrow(df) == 0) return(plotly_empty(type = "bar") |> layout(title = "No triggered cases for current filters"))
    df <- df %>% count(category, triage_category)
    plot_ly(df, x = ~category, y = ~n, color = ~triage_category, type = "bar",
            colors = c("Red" = "#d62728", "Yellow" = "#f0ad4e", "Green" = "#5cb85c")) |>
      layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "Cases"))
  })

  output$event_log_table <- renderDT({
    df <- f2_filtered()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No triggered cases for current filters"), rownames = FALSE))
    }
    df <- df %>%
      select(event_date, facility, category, triage_category, age, sex, trigger_combined) %>%
      arrange(desc(event_date))
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE,
              colnames = c("Date", "Facility", "Category", "Triage", "Age", "Sex", "Trigger reason(s)"))
  })

  # ---- Patient Flow tab ----
  output$kpi_time_triage <- renderText({
    v <- mean(f3_filtered()$time_to_triage_min, na.rm = TRUE)
    if (is.nan(v) || is.na(v)) return("--")
    paste0(round(v, 0), " min")
  })

  output$kpi_time_clinician <- renderText({
    v <- mean(f3_filtered()$time_to_clinician_min, na.rm = TRUE)
    if (is.nan(v) || is.na(v)) return("--")
    paste0(round(v, 0), " min")
  })

  output$kpi_los <- renderText({
    v <- mean(f3_filtered()$length_of_stay_min, na.rm = TRUE)
    if (is.nan(v) || is.na(v)) return("--")
    paste0(round(v / 60, 1), " hrs")
  })

  output$outcome_chart <- renderPlotly({
    df <- f3_filtered()
    if (nrow(df) == 0) return(plotly_empty(type = "pie") |> layout(title = "No patient-flow records for current filters"))
    df <- df %>% count(outcome_24h)
    plot_ly(df, labels = ~outcome_24h, values = ~n, type = "pie")
  })

  output$diagnosis_chart <- renderPlotly({
    df <- f3_filtered()
    if (nrow(df) == 0) return(plotly_empty(type = "bar") |> layout(title = "No patient-flow records for current filters"))
    df <- df %>% count(diagnosis) %>% arrange(desc(n))
    plot_ly(df, x = ~n, y = ~reorder(diagnosis, n), type = "bar", orientation = "h") |>
      layout(xaxis = list(title = "Patients"), yaxis = list(title = ""))
  })

  output$flow_table <- renderDT({
    df <- f3_filtered()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No patient-flow records for current filters"), rownames = FALSE))
    }
    df <- df %>%
      select(record_date, facility, shift, ward, triage_category, diagnosis,
             sepsis_bundle, outcome_24h, time_to_triage_min, time_to_clinician_min, length_of_stay_min) %>%
      arrange(desc(record_date))
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE,
              colnames = c("Date", "Facility", "Shift", "Ward", "Triage", "Diagnosis",
                           "Sepsis bundle", "Outcome (24h)", "Time to triage (min)",
                           "Time to clinician (min)", "Length of stay (min)"))
  })
}

shinyApp(ui, server)
