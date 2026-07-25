# Acclaim ED Quality Dashboard — Setup Guide (Live Version)

## Files

- `kobo_connect.R` — connects to the KoboToolbox API, decodes choice codes
  into labels, and reshapes submissions into the three form tables the
  dashboard uses. **Edit `KOBO_TOKEN` here before running.**
- `app.R` — the live Shiny dashboard. Sources `kobo_connect.R` and polls it
  every 30 seconds.
- `data_prep.R` / `acclaim_export.xlsx` — the earlier offline version, kept
  in case you want to design against a static export again. Not used by the
  live `app.R`.

## 1. Install required R packages

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "tidyr", "DT", "plotly",
  "lubridate", "hms", "stringr", "curl", "jsonlite"
))
```

If you previously hit an `rlang`-version error from `httr2`, that's no longer
a concern — the connection now uses the lightweight `curl` package instead,
which doesn't pull in the same strict `rlang` requirement.

## 2. Configure your token

Open `kobo_connect.R` and set:

```r
KOBO_TOKEN <- "your_regenerated_api_token"
```

`KOBO_SERVER` and `ASSET_UID` are already filled in for this form
(`https://kf.kobotoolbox.org` / `a2uByyb4iqMzEXp8cBZfBh`).

**Don't paste your token into chat with me or commit it to Git.** Better
long-term: set it as an environment variable instead, e.g. in a local
`.Renviron` file in this folder:
```
KOBO_TOKEN=your_regenerated_api_token
```
`kobo_connect.R` already reads from the environment first via `Sys.getenv()`.

## 3. Run it

```r
setwd("path/to/kobo_dashboard")
shiny::runApp("app.R")
```

The sidebar shows "Last synced" so you can see it's actually polling. It
refreshes automatically every 30 seconds (change `REFRESH_SECONDS` in
`kobo_connect.R` to adjust) — no manual reload needed when a new submission
comes in from the field.

## 4. If something looks wrong

The API returns raw choice *codes* (e.g. `"4"`) rather than labels, and
whether fields inside a group get a `GroupName/` prefix can vary by form
version — `kobo_connect.R` is written to handle both cases automatically,
but if a chart or table looks empty or mislabeled, the most likely cause is
a field-name mismatch. Run this to see the real field names Kobo is sending:

```r
setwd("path/to/kobo_dashboard")
source("inspect_kobo_data.R")
```

and paste the output back to me — I'll adjust the field names in
`kobo_connect.R` (search for `find_field(rec, "...")` — those quoted names
are the only things that would need to change).

## 5. Sharing with colleagues

Once the data looks right, host it somewhere persistent (shinyapps.io, Posit
Connect, or Shiny Server) so it's not just running on your laptop. Set
`KOBO_TOKEN` as an environment variable in that hosting platform's settings,
not in the code. Since this touches patient mortality/safety data, add
authentication at the hosting layer rather than leaving the link fully
public.
