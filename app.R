# ==============================================================================
# ACO -- Emergency Department Quality & Safety Dashboard (LIVE)
# ==============================================================================
# Connects directly to KoboToolbox -- no export file needed. Polls the API
# every REFRESH_SECONDS (set in kobo_connect.R) and refreshes automatically.
#
# BEFORE RUNNING: open kobo_connect.R and set KOBO_TOKEN to your own API
# token (Kobo -> Account Settings -> API). KOBO_SERVER and ASSET_UID are
# already filled in for this form.
#
# Changes in this version (2026-09-28):
#   1. FIXED deaths under-count: the "Total deaths" card now sums Form 1
#      Q8 (_8_a_How_many_patinets_died), which every shift submission
#      answers. It used to count Form 3 rows with outcome "Died", but the
#      Form 3 repeat is NOT shown when deaths are the only thing reported
#      (its relevance rule doesn't include Q8), so most deaths never
#      reached Form 3.
#   2. FIXED stale date filter: the sidebar end date used to be set once at
#      start-up, so anything submitted afterwards (or dated after the
#      latest record at start-up) was silently filtered out. The end date
#      now follows new data automatically unless you change it yourself.
#   3. Weekly Signals now uses Sunday-Saturday weeks and opens on the
#      previous complete week by default (e.g. on Mon 28 Sep 2026 it shows
#      20-26 Sep 2026).
#   4. Weekly Signals: "Death rate (%)" replaced by the number of deaths
#      recorded that week (sum of Form 1 Q8, _8_a_How_many_patinets_died).
#   5. Overview: "Still in A&E" card removed; the "Trend over time" chart is
#      replaced by a table of Form 1 Q14 (main blockage this shift) with its
#      own Health Facility filter. Needs the updated kobo_connect.R.
#   6. Q14 table gets a week filter (defaults to the current running week);
#      every bar chart now shows its value on top of / inside each bar.
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
# Metric definitions for Form 1 (used by Overview + Facility Trends + Weekly Signals)
# ------------------------------------------------------------------------------
metrics_list <- list(
  submissions        = list(label = "Number of submissions", type = "count"),
  patients_seen      = list(label = "Total patients seen", type = "sum"),
  total_deaths       = list(label = "Deaths recorded (Form 1 Q8)", type = "sum"),
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

# Weekly Signals shows this fixed set of key KPIs side by side, one column
# per metric, one row per week -- easier to scan for a sudden shift than
# picking through metrics one at a time.
WEEKLY_SIGNAL_METRICS <- c("submissions", "patients_seen", "total_deaths",
                            "lwbs_rate", "sepsis_bundle_rate", "red_delay_60min",
                            "yellow_delay_2h", "equipment_failures", "preventable_count")

# Icon + direction metadata for the Weekly Signals placards.
# direction controls how a week-over-week change is colored:
#   "down_is_good" -- a rise is bad news (danger), a fall is good news (success)
#   "up_is_good"   -- the reverse (e.g. compliance rates)
#   "neutral"      -- volume metrics; shown as info, no good/bad judgement
WEEKLY_SIGNAL_META <- list(
  submissions         = list(icon = "clipboard-list",              direction = "neutral"),
  patients_seen       = list(icon = "users",                       direction = "neutral"),
  total_deaths        = list(icon = "heart-pulse",                 direction = "down_is_good"),
  lwbs_rate           = list(icon = "person-walking-arrow-right",  direction = "down_is_good"),
  sepsis_bundle_rate  = list(icon = "syringe",                     direction = "up_is_good"),
  red_delay_60min     = list(icon = "hourglass-half",              direction = "down_is_good"),
  yellow_delay_2h     = list(icon = "clock",                       direction = "down_is_good"),
  equipment_failures  = list(icon = "screwdriver-wrench",          direction = "down_is_good"),
  preventable_count   = list(icon = "triangle-exclamation",        direction = "down_is_good")
)

# Fixed colour bands, used when a week has no earlier week to compare with.
# Same cut-offs as the Overview cards; edit the numbers here if needed.
#   green  = "success", amber = "warning", red = "danger"
band_theme <- function(mkey, v) {
  if (is.na(v)) return("secondary")
  switch(mkey,
    total_deaths       = if (v == 0) "success" else "danger",
    lwbs_rate          = if (v < 5)  "success" else if (v < 10) "warning" else "danger",
    sepsis_bundle_rate = if (v >= 90) "success" else if (v >= 70) "warning" else "danger",
    # counts of delays / failures / preventable cases: none = green,
    # a few = amber, more than 5 in a week = red
    if (v == 0) "success" else if (v <= 5) "warning" else "danger"
  )
}

# Number shown on top of each bar: whole numbers as-is, decimals to 1 place.
bar_label <- function(v) {
  ifelse(is.na(v), "",
         ifelse(v == round(v),
                formatC(round(v), format = "d", big.mark = ","),
                formatC(v, format = "f", digits = 1, big.mark = ",")))
}

# Weeks run Sunday -> Saturday (lubridate: 7 = Sunday).
WEEK_START_DAY <- 7

week_of <- function(d) floor_date(d, "week", week_start = WEEK_START_DAY)

# The previous complete week, e.g. on Mon 28 Sep 2026 -> Sun 20 Sep 2026.
previous_week_start <- function(today = Sys.Date()) week_of(today) - 7

week_label <- function(ws) {
  paste0(format(ws, "%d %b"), " – ", format(ws + 6, "%d %b %Y"))
}

# Shift ordering within a day, used to determine "the previous shift"
# chronologically (a plain date sort alone can't tell Morning from Night).
SHIFT_ORDER <- c("Morning" = 1, "Afternoon" = 2, "Night" = 3)

# Facility/shift choices come from the form definition, not the data, so the
# UI can be built before the first API call completes.
facility_choices <- unname(facility_lookup)
shift_choices <- unname(shift_lookup)

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- page_navbar(
  title = "ACO Dashboard (Live)",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  # FALSE so cards keep their natural height -- with TRUE, the Weekly
  # Signals tab squashed the week selector, trend chart and table into
  # thin empty bars once the placards were added.
  fillable = FALSE,

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
      paste0("Auto-refreshes every ", REFRESH_SECONDS, " seconds."))
  ),

  nav_panel(
    "Overview",
    uiOutput("overview_kpis"),
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
              span("Main blockage this shift (Form 1 Q14)"),
              div(class = "d-flex gap-2",
                  selectInput("blockage_week", NULL, choices = NULL, width = "210px"),
                  selectInput("blockage_facility", NULL,
                              choices = c("All Facilities", facility_choices),
                              selected = "All Facilities", width = "200px")))
        ),
        DTOutput("blockage_table")
      )
    )
  ),

  nav_panel(
    "Weekly Signals",
    p(class = "text-muted small",
      "Key KPIs rolled up by week (Sunday to Saturday), using the sidebar's Facility/Shift/Date filters. Opens on the previous complete week; pick another week to see its placards. Each placard shows the change from the week before."),
    layout_columns(
      col_widths = c(3, 9),
      card(
        card_header("Select Week"),
        selectInput("weekly_week", NULL, choices = NULL, width = "100%"),
        p(class = "text-muted small",
          "Placard colors: green = improved vs last week, red = worsened, gray = no meaningful change. Submissions and patients seen are volume only, shown in blue."),
        hr(),
        selectInput("weekly_metric", "Trend chart metric", choices = metric_choices, width = "100%")
      ),
      card(
        card_header("Weekly Trend"),
        plotlyOutput("weekly_trend_chart", height = "300px")
      )
    ),
    uiOutput("weekly_placards"),
    card(
      card_header("Weekly Signals Table"),
      DTOutput("weekly_signals_table")
    )
  ),

  nav_panel(
    "Facility Trends",
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Focus facility"),
        selectInput("focus_facility", NULL,
                    choices = c("All Facilities", facility_choices),
                    selected = "All Facilities"),
        p(class = "text-muted small",
          "Shows the KPI trend for all facilities combined, or pick one facility to see its own trend. Uses the Shift/Date filters in the sidebar.")
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
      card_header("Logged cases, with narrative"),
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

  # ---- Keep the sidebar date range in step with the data ----
  # Previously this ran once at start-up, so the end date froze at whatever
  # the latest record was then, and every later submission fell outside the
  # filter. Now, on every refresh, the range is widened to cover new data --
  # unless the user has set their own dates (we only move an edge that is
  # still sitting where the dashboard last put it).
  auto_range <- reactiveVal(NULL)

  observeEvent(live_data(), {
    df <- live_data()$form1
    if (nrow(df) == 0 || all(is.na(df$date))) return()

    new_start <- min(df$date, na.rm = TRUE)
    new_end   <- max(Sys.Date(), max(df$date, na.rm = TRUE))
    prev      <- auto_range()
    cur       <- input$f_dates

    if (is.null(prev)) {
      updateDateRangeInput(session, "f_dates", start = new_start, end = new_end)
    } else {
      start_untouched <- !is.null(cur) && !is.na(cur[1]) && cur[1] == prev[1]
      end_untouched   <- !is.null(cur) && !is.na(cur[2]) && cur[2] == prev[2]
      updateDateRangeInput(
        session, "f_dates",
        start = if (start_untouched) new_start else cur[1],
        end   = if (end_untouched)   new_end   else cur[2]
      )
    }
    auto_range(c(new_start, new_end))
  })

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
  # Total submissions, patients seen and TOTAL DEATHS come from Form 1 (the
  # per-shift tally that every submission fills in). Deaths used to be
  # counted from Form 3 outcomes, but Form 3 only opens when some other
  # trigger is > 0 -- a shift reporting deaths alone never reaches Form 3 --
  # so that count was far too low.
  # LWBS still comes from Form 3 (one row per patient).
  output$overview_kpis <- renderUI({
    df <- f1_filtered()
    f3 <- f3_filtered()

    total_submissions <- nrow(df)
    patients <- if (nrow(df) == 0) NA_real_ else sum(df$patients_seen, na.rm = TRUE)
    died_n   <- if (nrow(df) == 0) NA_real_ else sum(df$total_deaths, na.rm = TRUE)
    death_rate <- if (is.na(patients) || patients == 0) NA_real_ else 100 * died_n / patients

    f3_total <- nrow(f3)
    outcome_n   <- function(label) if (f3_total == 0) NA_real_ else sum(f3$outcome_24h == label, na.rm = TRUE)
    outcome_pct <- function(n) if (f3_total == 0 || is.na(n)) NA_real_ else 100 * n / f3_total

    lwbs_n   <- outcome_n("LWBS")
    lwbs_pct <- outcome_pct(lwbs_n)

    sepsis_tot  <- if (nrow(df) == 0) NA_real_ else sum(df$sepsis_cases, na.rm = TRUE)
    sepsis_rate <- if (is.na(sepsis_tot) || sepsis_tot == 0) NA_real_ else
      100 * sum(df$sepsis_bundle_completed, na.rm = TRUE) / sepsis_tot

    fmt_int <- function(v) if (is.na(v)) "--" else format(round(v), big.mark = ",")
    fmt_pct <- function(v) if (is.na(v)) "--" else paste0(round(v, 1), "%")
    subtitle <- function(pct, label) {
      if (is.na(pct)) p(class = "text-muted small", paste0("No ", label, " data")) else
        p(class = "text-muted small", paste0(round(pct, 1), "% of Form 3 patients"))
    }
    death_subtitle <- if (is.na(death_rate)) {
      p(class = "text-muted small", "No patients-seen data")
    } else {
      p(class = "text-muted small", paste0(round(death_rate, 1), "% of patients seen (Form 1)"))
    }

    # Threshold-based coloring
    deaths_theme   <- if (is.na(died_n)) "secondary" else if (died_n == 0) "success" else "danger"
    lwbs_theme     <- if (is.na(lwbs_pct)) "secondary" else if (lwbs_pct >= 10) "danger" else if (lwbs_pct >= 5) "warning" else "success"
    sepsis_theme   <- if (is.na(sepsis_rate)) "secondary" else if (sepsis_rate >= 90) "success" else if (sepsis_rate >= 70) "warning" else "danger"

    layout_column_wrap(
      width = 1/5,
      value_box(title = "Total submissions", value = fmt_int(total_submissions),
                showcase = icon("clipboard-list"), theme = "primary"),
      value_box(title = "Patients seen", value = fmt_int(patients),
                showcase = icon("users"), theme = "info"),
      value_box(title = "Total deaths", value = fmt_int(died_n),
                showcase = icon("heart-pulse"), theme = deaths_theme,
                death_subtitle),
      value_box(title = "LWBS (24h)", value = fmt_int(lwbs_n),
                showcase = icon("person-walking-arrow-right"), theme = lwbs_theme,
                subtitle(lwbs_pct, "outcome")),
      value_box(title = "Sepsis bundle compliance", value = fmt_pct(sepsis_rate),
                showcase = icon("syringe"), theme = sepsis_theme)
    )
  })

  # ---- Overview: facility comparison chart ----
  output$facility_compare_chart <- renderPlotly({
    req(input$overview_metric_facility)
    df <- f1_filtered()
    if (nrow(df) == 0) return(plotly_empty(type = "bar") |> layout(title = "No data for current filters"))
    m <- compute_metric(df, "facility", input$overview_metric_facility)
    lbl <- metrics_list[[input$overview_metric_facility]]$label
    plot_ly(m, x = ~facility, y = ~value, type = "bar",
            text = ~bar_label(value), textposition = "outside", cliponaxis = FALSE) |>
      layout(xaxis = list(title = ""), yaxis = list(title = lbl))
  })

  # ---- Overview: week picker for the Q14 table ----
  # Defaults to the CURRENT running week (Sunday to today). The user's pick
  # is kept across refreshes; if they're on the default, it moves forward
  # when a new week starts.
  last_blockage_default <- reactiveVal(NULL)

  observeEvent(f1_filtered(), {
    df <- f1_filtered()
    this_wk <- week_of(Sys.Date())
    data_wks <- if (nrow(df) > 0) unique(week_of(df$date[!is.na(df$date)])) else as.Date(character(0))
    weeks <- sort(unique(c(this_wk, data_wks)), decreasing = TRUE)
    labels <- ifelse(weeks == this_wk, paste0("This week (", week_label(weeks), ")"), week_label(weeks))
    choices <- c(setNames(as.character(weeks), labels), "All weeks" = "all")

    default_wk <- as.character(this_wk)
    current    <- input$blockage_week
    on_default <- is.null(current) || current == "" || identical(current, last_blockage_default())
    selected   <- if (!on_default && current %in% choices) current else default_wk

    updateSelectInput(session, "blockage_week", choices = choices, selected = selected)
    last_blockage_default(default_wk)
  })

  # ---- Overview: Form 1 Q14 "main blockage" free-text table ----
  # One row per shift submission that wrote something in Q14. Uses the
  # sidebar Shift/Date filters, plus its own facility dropdown in the card.
  output$blockage_table <- renderDT({
    df <- f1_filtered()
    empty_msg <- datatable(data.frame(Message = "No blockage notes for this week / facility yet"),
                           rownames = FALSE, options = list(dom = "t"))
    if (nrow(df) == 0 || !"main_blockage" %in% names(df)) return(empty_msg)

    if (!is.null(input$blockage_facility) && input$blockage_facility != "All Facilities") {
      df <- df %>% filter(facility == input$blockage_facility)
    }
    if (!is.null(input$blockage_week) && input$blockage_week != "" &&
        input$blockage_week != "all") {
      wk <- as_date(input$blockage_week)
      df <- df %>% filter(date >= wk, date <= wk + 6)
    }
    df <- df %>%
      mutate(main_blockage = trimws(main_blockage),
             shift = factor(shift, levels = names(SHIFT_ORDER))) %>%
      filter(!is.na(main_blockage), main_blockage != "") %>%
      arrange(desc(date), facility, desc(shift)) %>%
      select(date, facility, shift, main_blockage)
    if (nrow(df) == 0) return(empty_msg)

    datatable(df, rownames = FALSE,
              colnames = c("Date", "Facility", "Shift", "Main blockage"),
              options = list(pageLength = 6, scrollX = TRUE, scrollY = "260px",
                             dom = "ftip", ordering = FALSE))
  })

  # ---- Weekly Signals tab ----
  weekly_data <- reactive({
    df <- f1_filtered()
    if (nrow(df) == 0) return(df)
    df %>% mutate(week_start = week_of(date))
  })

  # All-metric summary table, one row per week -- computed once and reused
  # by the placards and the table, so they always agree. Covers every week
  # from the first with data up to the previous complete week, so the
  # default week always exists even if it had no submissions (counts show 0).
  weekly_summary <- reactive({
    df <- weekly_data()
    if (nrow(df) == 0) return(NULL)
    first_wk <- min(df$week_start, na.rm = TRUE)
    last_wk  <- max(df$week_start, na.rm = TRUE)
    # Include the previous complete week even if nothing was submitted in
    # it -- but only when it falls inside the sidebar date range.
    prev_wk <- previous_week_start()
    if (prev_wk + 6 >= input$f_dates[1] && prev_wk <= input$f_dates[2]) {
      last_wk <- max(last_wk, prev_wk)
    }
    weeks <- seq(first_wk, last_wk, by = "week")
    out <- data.frame(week_start = weeks)
    for (mkey in WEEKLY_SIGNAL_METRICS) {
      m <- compute_metric(df, "week_start", mkey)
      vals <- m$value[match(weeks, m$week_start)]
      if (metrics_list[[mkey]]$type != "rate") vals[is.na(vals)] <- 0
      out[[mkey]] <- vals
    }
    out
  })

  # Week selector: defaults to the previous complete Sunday-Saturday week.
  # If the user picks another week we keep it across refreshes; if they are
  # still on the default, we move them to the new default when the week
  # rolls over.
  last_default_week <- reactiveVal(NULL)

  observeEvent(weekly_summary(), {
    ws <- weekly_summary()
    if (is.null(ws) || nrow(ws) == 0) return()
    weeks <- sort(ws$week_start, decreasing = TRUE)
    choices <- setNames(as.character(weeks), week_label(weeks))

    default_wk <- as.character(previous_week_start())
    current    <- input$weekly_week
    on_default <- is.null(current) || current == "" ||
                  identical(current, last_default_week())

    selected <- if (!on_default && current %in% choices) current
                else if (default_wk %in% choices) default_wk
                else unname(choices[[1]])
    updateSelectInput(session, "weekly_week", choices = choices, selected = selected)
    last_default_week(default_wk)
  })

  output$weekly_trend_chart <- renderPlotly({
    req(input$weekly_metric)
    ws <- weekly_summary()
    if (is.null(ws)) return(plotly_empty(type = "scatter") |> layout(title = "No data for current filters"))
    df <- weekly_data()
    m <- compute_metric(df, "week_start", input$weekly_metric)
    lbl <- metrics_list[[input$weekly_metric]]$label
    plot_ly(m, x = ~week_start, y = ~value, type = "scatter", mode = "lines+markers") |>
      layout(xaxis = list(title = "Week starting (Sunday)"), yaxis = list(title = lbl))
  })

  # ---- Weekly Signals: colored KPI placards for the selected week ----
  output$weekly_placards <- renderUI({
    ws <- weekly_summary()
    if (is.null(ws) || nrow(ws) == 0 || is.null(input$weekly_week) || input$weekly_week == "") {
      return(card(class = "mt-3", card_body("No data for current filters.")))
    }
    sel_week <- as_date(input$weekly_week)
    ws <- ws %>% arrange(week_start)
    row_idx <- match(sel_week, ws$week_start)
    if (is.na(row_idx)) return(NULL)
    prev_idx <- row_idx - 1  # 0 if this is the first week on record

    boxes <- lapply(WEEKLY_SIGNAL_METRICS, function(mkey) {
      meta   <- WEEKLY_SIGNAL_META[[mkey]]
      lbl    <- metrics_list[[mkey]]$label
      is_pct <- metrics_list[[mkey]]$type == "rate"
      cur    <- ws[[mkey]][row_idx]
      prev   <- if (prev_idx >= 1) ws[[mkey]][prev_idx] else NA

      fmt <- function(v) {
        if (is.na(v)) return("--")
        if (is_pct) paste0(round(v, 1), "%") else format(round(v, 1), big.mark = ",")
      }

      # Decide color + change text
      if (is.na(cur)) {
        theme_color <- "secondary"
        change_text <- "No data this week"
      } else if (is.na(prev)) {
        # No earlier week to compare with -> fall back to fixed colour bands
        theme_color <- if (meta$direction == "neutral") "info" else band_theme(mkey, cur)
        change_text <- "No prior week to compare"
      } else {
        delta <- cur - prev
        if (meta$direction == "neutral") {
          theme_color <- "info"
          arrow <- if (delta > 0) "arrow-up" else if (delta < 0) "arrow-down" else "minus"
          change_text <- paste0(icon(arrow), " ", fmt(abs(delta)), " vs last week")
        } else {
          improved <- if (meta$direction == "down_is_good") delta < 0 else delta > 0
          worsened <- if (meta$direction == "down_is_good") delta > 0 else delta < 0
          theme_color <- if (delta == 0) "secondary" else if (improved) "success" else "danger"
          arrow <- if (delta == 0) "minus" else if (delta > 0) "arrow-up" else "arrow-down"
          change_text <- paste0(icon(arrow), " ", fmt(abs(delta)), " vs last week")
        }
      }

      value_box(
        title = lbl,
        value = fmt(cur),
        showcase = icon(meta$icon),
        theme = theme_color,
        p(HTML(change_text))
      )
    })

    tagList(
      h6(class = "mt-3 text-muted", paste0("Placards for ", week_label(sel_week))),
      do.call(layout_columns, c(list(col_widths = rep(4, length(boxes))), boxes))
    )
  })

  output$weekly_signals_table <- renderDT({
    ws <- weekly_summary()
    if (is.null(ws) || nrow(ws) == 0) {
      return(datatable(data.frame(Message = "No data for current filters"), rownames = FALSE))
    }
    ws <- ws %>% arrange(desc(week_start))
    out <- data.frame(Week = week_label(ws$week_start))
    for (mkey in WEEKLY_SIGNAL_METRICS) {
      lbl  <- metrics_list[[mkey]]$label
      vals <- ws[[mkey]]
      out[[lbl]] <- if (metrics_list[[mkey]]$type == "rate") round(vals, 1) else vals
    }
    datatable(out, options = list(pageLength = 15, scrollX = TRUE, ordering = FALSE),
              rownames = FALSE)
  })

  # ---- Facility Trends tab ----
  # "All Facilities" (the default) pools every facility; otherwise filter
  # to the one chosen.
  focus_data <- reactive({
    req(input$focus_facility)
    df <- f1_filtered()
    if (nrow(df) == 0 || input$focus_facility == "All Facilities") return(df)
    df %>% filter(facility == input$focus_facility)
  })

  output$focus_shift_count <- renderPlotly({
    df <- focus_data()
    if (nrow(df) == 0) return(plotly_empty(type = "bar") |> layout(title = "No data"))
    df <- df %>% count(shift)
    plot_ly(df, x = ~shift, y = ~n, type = "bar",
            text = ~n, textposition = "outside", cliponaxis = FALSE) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Shifts logged"))
  })

  output$focus_trend_chart <- renderPlotly({
    req(input$focus_metric)
    df <- focus_data()
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
    plot_ly(counts, x = ~n, y = ~reorder(trigger_combined, n), type = "bar", orientation = "h",
            text = ~n, textposition = "outside", cliponaxis = FALSE) |>
      layout(xaxis = list(title = "Cases"), yaxis = list(title = ""))
  })

  output$category_triage_chart <- renderPlotly({
    df <- f2_filtered()
    if (nrow(df) == 0) return(plotly_empty(type = "bar") |> layout(title = "No triggered cases for current filters"))
    df <- df %>% count(category, triage_category)
    totals <- df %>% group_by(category) %>% summarise(n = sum(n), .groups = "drop")
    plot_ly(df, x = ~category, y = ~n, color = ~triage_category, type = "bar",
            text = ~n, textposition = "inside", insidetextanchor = "middle",
            colors = c("Red" = "#d62728", "Yellow" = "#f0ad4e", "Green" = "#5cb85c")) |>
      add_trace(data = totals, x = ~category, y = ~n, type = "scatter", mode = "text",
                text = ~paste0("<b>", n, "</b>"), textposition = "top center",
                showlegend = FALSE, inherit = FALSE, hoverinfo = "skip") |>
      layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "Cases"))
  })

  output$event_log_table <- renderDT({
    df <- f2_filtered()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No triggered cases for current filters"), rownames = FALSE))
    }
    df <- df %>%
      select(event_date, facility, category, triage_category, age, sex, trigger_combined, narrative) %>%
      arrange(desc(event_date))
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE,
              colnames = c("Date", "Facility", "Category", "Triage", "Age", "Sex", "Trigger reason(s)", "Narrative"))
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
    plot_ly(df, x = ~n, y = ~reorder(diagnosis, n), type = "bar", orientation = "h",
            text = ~n, textposition = "outside", cliponaxis = FALSE) |>
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
