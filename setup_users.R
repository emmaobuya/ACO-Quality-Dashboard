# ==============================================================================
# ACO Dashboard -- create the user accounts (run on your own computer)
# ==============================================================================
# Run by the ACO admin. Creates users.sqlite (encrypted) with:
#   * acoadmin  -- national view + admin panel
#   * acoteam   -- national view, no admin panel
#   * one login per hospital, no underscores:
#       Arua RRH  -> username aruarrh,  password aruaaco2026
#       Gulu RRH  -> username gulurrh,  password guluaco2026  ... and so on
#
# The full list is written to initial_passwords.csv so you can hand the
# details out -- then DELETE that file. Never upload it with the app.
#
# HOW TO RUN (close Excel first)
#   setwd("C:/Users/Personal/Desktop/Work/kobo dashboard")
#   readRenviron(".Renviron")
#   source("setup_users.R")
#
# Running it again REPLACES users.sqlite. Afterwards upload the new
# users.sqlite to GitHub and redeploy.
# ==============================================================================

library(shinymanager)
source("kobo_connect.R")   # only for facility_lookup (the facility names)

passphrase <- Sys.getenv("SHINY_DB_PASSPHRASE")
if (passphrase == "") stop("Run readRenviron(\".Renviron\") first -- SHINY_DB_PASSPHRASE is empty.")

# ---- 1. National account passwords --------------------------------------------
# Type your own between the quotes. Leave as "" to get a random password
# (it will be shown in initial_passwords.csv).
ADMIN_PASSWORD <- "adminaco26"
TEAM_PASSWORD  <- "acoteam26"

# ---- 2. Which hospitals get an account ----------------------------------------
# All facilities by default. To set up only some, list them instead, e.g.
#   HOSPITALS <- c("Arua RRH", "Gulu RRH")
HOSPITALS <- unname(facility_lookup)

# ---- 3. Password pattern for hospitals -----------------------------------------
# First word of the facility name + this ending: "Arua RRH" -> aruaaco2026
HOSPITAL_PASSWORD_ENDING <- "aco26"

# ------------------------------------------------------------------------------
make_random_password <- function(n = 10) {
  chars <- c(LETTERS[-c(9, 15)], letters[-c(12, 15)], 2:9)
  paste(sample(chars, n, replace = TRUE), collapse = "")
}
or_random <- function(p) if (nzchar(p)) p else make_random_password()

# "Arua RRH" -> "aruarrh"
make_username <- function(facility) tolower(gsub("[^A-Za-z0-9]", "", facility))
# "Arua RRH" -> "aruaaco26"
make_hospital_password <- function(facility) {
  paste0(tolower(gsub("[^A-Za-z0-9]", "", strsplit(trimws(facility), "\\s+")[[1]][1])),
         HOSPITAL_PASSWORD_ENDING)
}

bad <- setdiff(HOSPITALS, unname(facility_lookup))
if (length(bad)) stop("Not a facility name in the form: ", paste(bad, collapse = ", "))

national <- data.frame(
  user     = c("acoadmin", "acoteam"),
  password = c(or_random(ADMIN_PASSWORD), or_random(TEAM_PASSWORD)),
  facility = c("ALL", "ALL"),
  admin    = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)

hospitals <- data.frame(
  user     = make_username(HOSPITALS),
  password = vapply(HOSPITALS, make_hospital_password, character(1), USE.NAMES = FALSE),
  facility = HOSPITALS,
  admin    = FALSE,
  stringsAsFactors = FALSE
)

credentials <- rbind(national, hospitals)
if (anyDuplicated(credentials$user)) stop("Two accounts ended up with the same username.")

write.csv(credentials[, c("user", "password", "facility")],
          "initial_passwords.csv", row.names = FALSE)

create_db(
  credentials_data = credentials[, c("user", "password", "admin", "facility")],
  sqlite_path      = "users.sqlite",
  passphrase       = passphrase
)

message("Created users.sqlite with ", nrow(credentials), " accounts.\n",
        "Usernames and passwords are in initial_passwords.csv -- share them, then delete that file.\n",
        "Now upload users.sqlite to GitHub and redeploy.")
