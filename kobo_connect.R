# ==============================================================================
# Acclaim -- Live KoboToolbox connection
# ==============================================================================
# Fetches submissions straight from the KoboToolbox API and reshapes them
# into the same form1 / form2 / form3 structure app.R expects (previously
# built from a static export in data_prep.R). This is the "live" data source.
#
# WHY THE DEFENSIVE FIELD LOOKUP: the API returns each submission's fields
# using the form's internal question *names*, not the labels you see in
# Kobo's UI. Depending on form/server version, fields inside a `begin_group`
# sometimes come back prefixed with "GroupName/field_name" and sometimes
# don't. find_field() below matches on the exact name OR anything ending in
# "/<name>", so it works either way without needing to know in advance.
# ==============================================================================

library(curl)
library(jsonlite)
library(dplyr)
library(lubridate)
library(hms)
library(stringr)

# ------------------------------------------------------------------------------
# CONFIG -- fill these in (or set as env vars / .Renviron, recommended)
# ------------------------------------------------------------------------------
KOBO_SERVER <- Sys.getenv("KOBO_SERVER", unset = "https://kf.kobotoolbox.org")
KOBO_TOKEN  <- Sys.getenv("KOBO_TOKEN",  unset = "97cf6251f4e77d891d960454a54f30918916fc24")
ASSET_UID   <- Sys.getenv("ASSET_UID",   unset = "a2uByyb4iqMzEXp8cBZfBh")

REFRESH_SECONDS <- 300  # how often the dashboard re-polls Kobo

# ------------------------------------------------------------------------------
# Choice lists (from the XLSForm "choices" sheet) -- the API returns these as
# raw codes, so we decode them back to labels ourselves.
# ------------------------------------------------------------------------------
facility_lookup <- c(
  "1" = "Mulago NRH", "2" = "Kiruddu NRH", "3" = "Mbarara RRH", "4" = "Gulu RRH",
  "5" = "Mbale RRH", "6" = "Arua RRH", "7" = "Katakwi GH", "8" = "Iganga GH",
  "9" = "Kiryandongo GH", "10" = "Yumbe RRH"
)
shift_lookup    <- c("1" = "Morning", "2" = "Afternoon", "3" = "Night")
yesno_lookup    <- c("0" = "No", "1" = "Yes")
category_lookup <- c("1" = "Medical Emergency", "2" = "Surgical Emergency", "3" = "Pediatric Emergency")
sex_lookup      <- c("1" = "M (Male)", "2" = "F (Female)")
triage_lookup   <- c("1" = "Red", "2" = "Yellow", "3" = "Green")
trigger_lookup  <- c(
  "1" = "Death <24h", "2" = "Death in >24h", "3" = "Red delay >60min",
  "4" = "Yellow delay >2h", "5" = "No oxygen/resus >30min", "6" = "No antibiotics in 1h",
  "7" = "Deteriorated while waiting", "8" = "Equipment failure", "9" = "Patient lost",
  "10" = "Something felt wrong", "88" = "Other specify"
)
diagnosis_lookup <- c(
  "1" = "Sepsis", "2" = "Trauma", "3" = "Chest pain", "4" = "MI",
  "5" = "Respiratory distress", "6" = "Stroke", "7" = "Surgical emergency",
  "8" = "Obstetric emergency", "88" = "Other"
)
sepsis_bundle_lookup <- c("1" = "Yes", "2" = "No", "3" = "N/A")
outcome_lookup <- c(
  "1" = "Alive and well", "2" = "Alive - deteriorated", "3" = "Died",
  "4" = "Still in A&E", "5" = "LWBS"
)

decode_choice <- function(code, lookup) {
  if (is.null(code) || length(code) == 0 || is.na(code)) return(NA_character_)
  out <- lookup[as.character(code)]
  ifelse(is.na(out), as.character(code), unname(out))  # fall back to raw code if unmapped
}

decode_multiselect <- function(codes_string, lookup) {
  if (is.null(codes_string) || length(codes_string) == 0 || is.na(codes_string) || codes_string == "") {
    return(character(0))
  }
  codes <- str_split(codes_string, "\\s+")[[1]]
  vapply(codes, function(c) decode_choice(c, lookup), character(1))
}

# ------------------------------------------------------------------------------
# Defensive field lookup helpers -- see note at top of file
# ------------------------------------------------------------------------------
find_field <- function(record, field_name) {
  keys <- names(record)
  hit <- keys[keys == field_name | endsWith(keys, paste0("/", field_name))]
  if (length(hit) == 0) return(NA)
  val <- record[[hit[1]]]
  if (is.null(val)) return(NA)
  val
}

find_repeat <- function(record, group_name) {
  keys <- names(record)
  hit <- keys[keys == group_name | endsWith(keys, paste0("/", group_name))]
  if (length(hit) == 0) return(list())
  record[[hit[1]]]
}

safe_num  <- function(x) suppressWarnings(as.numeric(x))
safe_date <- function(x) suppressWarnings(as_date(x))
safe_time <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(as_hms(NA))
  as_hms(str_extract(as.character(x), "^\\d{2}:\\d{2}:\\d{2}"))
}

# ------------------------------------------------------------------------------
# Fetch ALL submissions from the API, paging through as needed
# ------------------------------------------------------------------------------
# KoboToolbox recently changed this endpoint to only return a single page
# of results (around 100) per request unless you explicitly page through
# them -- previously it returned everything in one call. This loops,
# requesting PAGE_LIMIT records at a time and advancing `start`, until
# we've collected as many results as Kobo's own reported `count`, so this
# keeps working correctly regardless of how large the dataset grows. This
# is the same "keep requesting the next batch until nothing more comes
# back" pattern others have hit the same issue with on Kobo's forum.
# ------------------------------------------------------------------------------
kobo_fetch_raw <- function() {
  base_url <- paste0(KOBO_SERVER, "/api/v2/assets/", ASSET_UID, "/data.json")
  page_limit <- 1000  # Kobo's current max page size
  
  h <- new_handle()
  handle_setheaders(h, "Authorization" = paste("Token", KOBO_TOKEN))
  
  all_results <- list()
  start <- 0
  total_count <- NA_integer_
  
  repeat {
    url <- paste0(base_url, "?limit=", page_limit, "&start=", start)
    
    resp <- tryCatch(curl_fetch_memory(url, handle = h), error = function(e) {
      warning("Kobo API connection failed: ", e$message)
      NULL
    })
    if (is.null(resp)) return(NULL)
    
    if (resp$status_code >= 400) {
      warning("Kobo API request failed with status ", resp$status_code, ": ",
              rawToChar(resp$content))
      return(NULL)
    }
    
    page <- fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
    if (is.na(total_count)) total_count <- page$count
    
    all_results <- c(all_results, page$results)
    start <- start + page_limit
    
    # Stop once we've collected everything Kobo says exists, or a page
    # comes back empty (safety net against an infinite loop if `count`
    # is ever wrong).
    if (length(page$results) == 0 || start >= total_count) break
  }
  
  list(count = total_count, results = all_results)
}

# ------------------------------------------------------------------------------
# Parse raw JSON into the three form tables (mirrors data_prep.R's output)
# ------------------------------------------------------------------------------
parse_kobo_data <- function(raw) {
  empty <- list(
    form1 = data.frame(),
    form2 = data.frame(),
    form3 = data.frame()
  )
  if (is.null(raw) || length(raw$results) == 0) return(empty)
  
  form1_rows <- list()
  form2_rows <- list()
  form3_rows <- list()
  
  for (rec in raw$results) {
    
    root_uuid       <- find_field(rec, "_uuid")
    submission_time <- find_field(rec, "_submission_time")
    facility_code   <- find_field(rec, "_1_What_is_the_Name_of_your_Facility")
    facility_name   <- decode_choice(facility_code, facility_lookup)
    
    # ---- Form 1: shift-level pulse-check ----
    patients_seen   <- safe_num(find_field(rec, "_4_How_many_total_pa_were_seen_this_shift"))
    admitted        <- safe_num(find_field(rec, "_5_How_many_patients_admitted_this_shift"))
    deaths_24h      <- safe_num(find_field(rec, "_6_How_many_deaths_o_24_hours_of_arrival"))
    deaths_after24h <- safe_num(find_field(rec, "_7_How_many_deaths_o_r_more_after_arrival"))
    lwbs            <- safe_num(find_field(rec, "_8_How_many_patients_out_being_seen_LWBS"))
    sepsis_cases    <- safe_num(find_field(rec, "_10_How_many_sepsis_were_seen_this_shift"))
    sepsis_bundle_completed <- safe_num(find_field(rec, "_15_How_many_sepsis_bundle_within_1_hour"))
    preventable_flag_raw    <- find_field(rec, "_22_Were_there_any_cases_this_")
    
    form1_rows[[length(form1_rows) + 1]] <- data.frame(
      uuid = as.character(root_uuid),
      submission_time = as_datetime(as.character(submission_time)),
      facility = facility_name,
      date = safe_date(find_field(rec, "_2_What_is_today_s_date")),
      shift = decode_choice(find_field(rec, "_3_Which_shift_is_this_for"), shift_lookup),
      patients_seen = patients_seen,
      admitted = admitted,
      deaths_24h = deaths_24h,
      deaths_after24h = deaths_after24h,
      total_deaths = sum(deaths_24h, deaths_after24h, na.rm = TRUE),
      lwbs = lwbs,
      red_delay_60min = safe_num(find_field(rec, "_9_How_many_Red_tria_more_than_60_minutes")),
      sepsis_cases = sepsis_cases,
      sepsis_bundle_completed = sepsis_bundle_completed,
      deteriorated_waiting = safe_num(find_field(rec, "_17_How_many_patient_ned_shock_developed")),
      equipment_failures = safe_num(find_field(rec, "_18_How_many_critica_lator_oxygen_supply")),
      patients_lost = safe_num(find_field(rec, "_19_How_many_triaged_efore_receiving_care")),
      yellow_delay_2h = safe_num(find_field(rec, "_20_How_many_Yellow_seen_by_a_clinician")),
      sepsis_no_antibiotics = safe_num(find_field(rec, "_21_How_many_sepsis_iotics_within_1_hour")),
      preventable_flag = identical(decode_choice(preventable_flag_raw, yesno_lookup), "Yes"),
      preventable_count = safe_num(find_field(rec, "_23_If_Yes_how_many_such_cases")),
      stringsAsFactors = FALSE
    )
    
    # ---- Form 2: case event log (repeat) ----
    for (item in find_repeat(rec, "group_form2")) {
      trigger_codes <- find_field(item, "_8_Why_was_this_case_logged_t")
      trigger_labels <- decode_multiselect(trigger_codes, trigger_lookup)
      
      form2_rows[[length(form2_rows) + 1]] <- data.frame(
        parent_uuid = as.character(root_uuid),
        facility = facility_name,
        event_date = safe_date(find_field(item, "_1_What_is_the_date_of_this_event")),
        triage_id = as.character(find_field(item, "_3_What_is_the_patie_triage_number_or_ID")),
        category = decode_choice(find_field(item, "_4_Which_Category_is_this_for"), category_lookup),
        age = safe_num(find_field(item, "_5_What_is_the_patient_s_age_in_years")),
        sex = decode_choice(find_field(item, "_6_What_is_the_patient_s_sex"), sex_lookup),
        triage_category = decode_choice(find_field(item, "_7_What_was_the_pati_nt_s_triage_category"), triage_lookup),
        trigger_combined = paste(trigger_labels, collapse = ", "),
        narrative = as.character(find_field(item, "_9_In_one_line_what_happened_facts_only")),
        stringsAsFactors = FALSE
      )
    }
    
    # ---- Form 3: patient flow (repeat) ----
    for (item in find_repeat(rec, "group_form3")) {
      t_arrival   <- safe_time(find_field(item, "_5_What_time_did_the_patient_arrive"))
      t_triaged   <- safe_time(find_field(item, "_6_What_time_was_the_patient_triaged"))
      t_clinician <- safe_time(find_field(item, "_7_What_time_was_the_seen_by_a_clinician"))
      t_dispo     <- safe_time(find_field(item, "_8_What_time_was_the_sition_decision_made"))
      
      wrap_forward <- function(mins) ifelse(!is.na(mins) & mins < 0, mins + 1440, mins)
      
      form3_rows[[length(form3_rows) + 1]] <- data.frame(
        parent_uuid = as.character(root_uuid),
        facility = facility_name,
        record_date = safe_date(find_field(item, "_1_What_is_today_s_date")),
        shift = decode_choice(find_field(item, "_2_Which_shift_is_this_for"), shift_lookup),
        ward = decode_choice(find_field(item, "_3_Which_ward_is_this_for"), category_lookup),
        triage_id = as.character(find_field(item, "_4_What_is_the_patie_number_patient_ID")),
        triage_category = decode_choice(find_field(item, "_9_What_was_the_pati_nt_s_triage_category"), triage_lookup),
        diagnosis = decode_choice(find_field(item, "_10_What_was_the_main_diagnosis"), diagnosis_lookup),
        sepsis_bundle = decode_choice(find_field(item, "_11_Was_the_sepsis_b_ted_for_this_patient"), sepsis_bundle_lookup),
        outcome_24h = decode_choice(find_field(item, "_12_What_was_the_pat_outcome_at_24_hours"), outcome_lookup),
        time_to_triage_min    = wrap_forward(as.numeric(t_triaged - t_arrival) / 60),
        time_to_clinician_min = wrap_forward(as.numeric(t_clinician - t_arrival) / 60),
        length_of_stay_min    = wrap_forward(as.numeric(t_dispo - t_arrival) / 60),
        stringsAsFactors = FALSE
      )
    }
  }
  
  list(
    form1 = bind_rows(form1_rows),
    form2 = bind_rows(form2_rows),
    form3 = bind_rows(form3_rows)
  )
}

# ------------------------------------------------------------------------------
# Single entry point app.R calls
# ------------------------------------------------------------------------------
load_live_data <- function() {
  raw <- tryCatch(kobo_fetch_raw(), error = function(e) {
    warning("Failed to fetch from Kobo: ", e$message)
    NULL
  })
  parse_kobo_data(raw)
}


