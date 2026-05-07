#!/usr/bin/env Rscript
# R/fetch_real_data.R
# Direct Transfermarkt scraping — confirmed working with browser headers

suppressPackageStartupMessages({
  library(httr)
  library(rvest)
  library(dplyr)
  library(stringr)
  library(purrr)
})

HEADERS <- c(
  "User-Agent"      = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
  "Accept-Language" = "en-US,en;q=0.9",
  "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Referer"         = "https://www.transfermarkt.com/"
)

safe_get <- function(url, pause = 5) {
  Sys.sleep(pause)
  tryCatch(
    GET(url, add_headers(.headers = HEADERS), timeout(30)),
    error = function(e) { message("GET error: ", e$message); NULL }
  )
}

# ── 1. League tables ───────────────────────────────────────────────────────────
fetch_standings <- function(season_start) {
  url  <- sprintf("https://www.transfermarkt.com/super-lig/tabelle/wettbewerb/TR1/saison_id/%d", season_start)
  resp <- safe_get(url)
  if (is.null(resp) || status_code(resp) != 200) {
    message("standings fetch failed for season ", season_start); return(NULL)
  }
  page   <- read_html(content(resp, "text"))
  tables <- html_table(page, fill = TRUE)

  # The main table has columns: Rank, -, Team, -, MP, W, D, L, Goals, +/-, Pts
  tbl <- NULL
  for (t in tables) {
    if (ncol(t) >= 9 && any(str_detect(names(t), regex("pts|punkte|pkt|sp", ignore_case = TRUE)) |
                              (nrow(t) >= 15 && is.numeric(t[[ncol(t)]])))) {
      tbl <- t; break
    }
    # try heuristic: last col all numeric ints >=0
    last <- t[[ncol(t)]]
    if (nrow(t) >= 15 && is.numeric(last) && all(!is.na(last)) && all(last >= 0)) {
      tbl <- t; break
    }
  }

  if (is.null(tbl)) {
    # fallback: largest table
    tbl <- tables[[which.max(sapply(tables, nrow))]]
  }

  message("standings table for ", season_start, ": ", nrow(tbl), " rows, ", ncol(tbl), " cols")
  message("  col names: ", paste(names(tbl), collapse = " | "))

  # Print head for debugging
  print(head(tbl, 5))

  tbl
}

standings_raw <- list()
for (yr in c(2023, 2024, 2025)) {
  message("\n=== Fetching standings ", yr, "-", yr+1, " ===")
  standings_raw[[as.character(yr)]] <- fetch_standings(yr)
}

# ── 2. Squad market values ─────────────────────────────────────────────────────
fetch_squad_values <- function(season_start) {
  url  <- sprintf("https://www.transfermarkt.com/super-lig/startseite/wettbewerb/TR1/plus/?saison_id=%d", season_start)
  resp <- safe_get(url)
  if (is.null(resp) || status_code(resp) != 200) {
    message("squad values fetch failed for season ", season_start); return(NULL)
  }
  page   <- read_html(content(resp, "text"))
  tables <- html_table(page, fill = TRUE)

  # Find table with squad value column
  tbl <- NULL
  for (t in tables) {
    col_names <- tolower(paste(names(t), collapse = " "))
    if (str_detect(col_names, "squad|market|value|squad size|kader")) {
      if (nrow(t) >= 15) { tbl <- t; break }
    }
  }
  if (is.null(tbl)) {
    tbl <- tables[[which.max(sapply(tables, nrow))]]
  }

  message("squad values for ", season_start, ": ", nrow(tbl), " rows, ", ncol(tbl), " cols")
  message("  col names: ", paste(names(tbl), collapse = " | "))
  print(head(tbl, 5))
  tbl
}

squad_raw <- list()
for (yr in c(2023, 2024, 2025)) {
  message("\n=== Fetching squad values ", yr, "-", yr+1, " ===")
  squad_raw[[as.character(yr)]] <- fetch_squad_values(yr)
}

# ── 3. Team URLs for player stats ──────────────────────────────────────────────
# Get team IDs and slugs from the squad page links
fetch_team_links <- function(season_start) {
  url  <- sprintf("https://www.transfermarkt.com/super-lig/startseite/wettbewerb/TR1/plus/?saison_id=%d", season_start)
  resp <- safe_get(url, pause = 3)
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  page <- read_html(content(resp, "text"))

  # All links to team pages (startseite)
  links <- page |> html_elements("a") |> html_attr("href")
  # Filter: /TEAMSLUG/startseite/verein/ID/saison_id/YEAR
  team_links <- links[str_detect(links, "/startseite/verein/\\d+/saison_id/")]
  team_links <- unique(team_links)
  team_links <- str_replace(team_links, "^//www\\.transfermarkt\\.com", "")

  # Extract team slug and ID
  df <- data.frame(href = team_links, stringsAsFactors = FALSE) |>
    mutate(
      slug    = str_extract(href, "^/[^/]+") |> str_remove("^/"),
      team_id = str_extract(href, "verein/(\\d+)", group = 1)
    ) |>
    filter(!is.na(team_id)) |>
    distinct(team_id, .keep_all = TRUE)

  message("Found ", nrow(df), " teams for season ", season_start)
  df
}

team_links_2023 <- fetch_team_links(2023)
team_links_2024 <- fetch_team_links(2024)
team_links_2025 <- fetch_team_links(2025)

print(team_links_2023)

# ── 4. Player stats per team ───────────────────────────────────────────────────
fetch_player_stats <- function(slug, team_id, season_start) {
  url <- sprintf(
    "https://www.transfermarkt.com/%s/leistungsdaten/verein/%s/reldata/TR1%%%d/plus/1",
    slug, team_id, season_start
  )
  # Note: reldata uses %26 encoding — try both
  url2 <- sprintf(
    "https://www.transfermarkt.com/%s/leistungsdaten/verein/%s/reldata/TR1&%d/plus/1",
    slug, team_id, season_start
  )

  message("  Fetching player stats: ", slug, " (", season_start, ")")
  resp <- safe_get(url, pause = 4)
  if (is.null(resp) || status_code(resp) != 200) {
    resp <- safe_get(url2, pause = 4)
  }
  if (is.null(resp) || status_code(resp) != 200) {
    message("  FAILED: ", slug); return(NULL)
  }

  page   <- read_html(content(resp, "text"))
  tables <- html_table(page, fill = TRUE)

  # Find the player stats table: has many rows (>15), has numeric columns for goals
  # Typically the main stats table has 20+ rows including header
  candidate <- NULL
  for (t in tables) {
    if (nrow(t) >= 10 && ncol(t) >= 8) {
      # Check if there's a column with small integers (goals)
      int_cols <- sapply(t, function(x) is.numeric(x) || all(str_detect(as.character(x), "^\\d+$"), na.rm = TRUE))
      if (sum(int_cols) >= 4) { candidate <- t; break }
    }
  }

  if (is.null(candidate) && length(tables) > 0) {
    sizes <- sapply(tables, nrow)
    candidate <- tables[[which.max(sizes)]]
  }

  if (is.null(candidate)) { message("  No table found: ", slug); return(NULL) }

  message("  Got table: ", nrow(candidate), " rows x ", ncol(candidate), " cols")
  candidate$team_slug <- slug
  candidate$season_start <- season_start
  candidate
}

# Test with Galatasaray first
message("\n=== Test: Galatasaray 2023 player stats ===")
gala_2023 <- fetch_player_stats("galatasaray-sk", "141", 2023)
if (!is.null(gala_2023)) {
  message("Columns: ", paste(names(gala_2023), collapse = " | "))
  print(head(gala_2023, 10))
}

message("\n=== Test: Fenerbahçe 2023 player stats ===")
fener_2023 <- fetch_player_stats("fenerbahce-sk", "36", 2023)
if (!is.null(fener_2023)) {
  message("Columns: ", paste(names(fener_2023), collapse = " | "))
  print(head(fener_2023, 10))
}

message("\nDone — inspect output above to determine correct column structure")
