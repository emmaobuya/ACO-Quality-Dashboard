# ==============================================================================
# ACO Dashboard -- create the user accounts (run ONCE, on your own computer)
# ==============================================================================
# Creates users.sqlite (encrypted) with:
#   * national_admin  -- national view + admin panel (add users, reset passwords)
#   * national_team   -- national view, no admin panel
#   * one login per facility, e.g. arua_rrh -> sees Arua RRH only
#
# Every account gets a random starting password. They are written to
# initial_passwords.csv so you can hand them out -- then DELETE that file.
# Never upload initial_passwords.csv with the app.
#
# HOW TO RUN
#   1. install.packages("shinymanager")
#   2. Put this line in a file called .Renviron next to app.R (pick your own
#      long phrase), then restart R:
#        SHINY_DB_PASSPHRASE=some-long-secret-phrase-only-you-know
#   3. In R, with the working directory set to the app folder:
#        source("setup_users.R")
#
# Running it again REPLACES users.sqlite (all passwords change). To add or
# change a single user later, use the admin panel inside the dashboard, or
# edit the `extra_users` table below and re-run.
# ==============================================================================

library(shinymanager)
source("kobo_connect.R")   # only for facility_lookup (the facility names)

passphrase <- Sys.getenv("SHINY_DB_PASSPHRASE")
if (passphrase == "") stop("Set SHINY_DB_PASSPHRASE in .Renviron first (see step 2 above).")

make_password <- function(n = 10) {
  # No 0/O, 1/l/I -- easier to read out over the phone
  chars <- c(LETTERS[-c(9, 15)], letters[-c(12, 15)], 2:9)
  paste(sample(chars, n, replace = TRUE), collapse = "")
}
make_username <- function(facility) {
  tolower(gsub("[^A-Za-z0-9]+", "_", trimws(facility)))   # "Arua RRH" -> "arua_rrh"
}

facilities <- unname(facility_lookup)

# National accounts. facility = "ALL" means "see every facility".
national <- data.frame(
  user     = c("national_admin", "national_team"),
  facility = c("ALL", "ALL"),
  admin    = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)

# One account per facility. facility must match the name in facility_lookup
# EXACTLY (e.g. "Arua RRH"), otherwise the account sees no data at all.
hospitals <- data.frame(
  user     = make_username(facilities),
  facility = facilities,
  admin    = FALSE,
  stringsAsFactors = FALSE
)

# Add more people here if a facility needs a second login, e.g.:
#   data.frame(user = "arua_rrh_2", facility = "Arua RRH", admin = FALSE)
extra_users <- data.frame(user = character(0), facility = character(0),
                          admin = logical(0), stringsAsFactors = FALSE)

credentials <- rbind(national, hospitals, extra_users)
set.seed(NULL)
credentials$password <- vapply(seq_len(nrow(credentials)),
                               function(i) make_password(), character(1))

if (anyDuplicated(credentials$user)) stop("Duplicate usernames: fix extra_users.")
bad <- setdiff(credentials$facility, c("ALL", facilities))
if (length(bad)) stop("Unknown facility name(s): ", paste(bad, collapse = ", "))

# Hand-out list (plain text!) -- distribute, then delete.
write.csv(credentials[, c("user", "password", "facility")],
          "initial_passwords.csv", row.names = FALSE)

create_db(
  credentials_data = credentials[, c("user", "password", "admin", "facility")],
  sqlite_path      = "users.sqlite",
  passphrase       = passphrase
)

message("Created users.sqlite with ", nrow(credentials), " accounts.\n",
        "Starting passwords are in initial_passwords.csv -- share them, then delete that file.")
