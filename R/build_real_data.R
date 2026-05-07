#!/usr/bin/env Rscript
# R/build_real_data.R  — Build superlig_final.RData from live Transfermarkt scraping
suppressPackageStartupMessages({
  library(httr); library(rvest); library(dplyr); library(stringr); library(purrr)
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
    error = function(e) { message("  GET error: ", e$message); NULL }
  )
}

parse_eur <- function(x) {
  # "€255.65m" → 255650000, "€5.56m" → 5560000, "€500k" → 500000
  x <- str_trim(x)
  mult <- ifelse(str_detect(x, "(?i)m"), 1e6, ifelse(str_detect(x, "(?i)k"), 1e3, 1))
  as.numeric(str_replace_all(x, "[^0-9.]", "")) * mult
}

parse_min <- function(x) {
  # "2.858'" or "3.300'" → integer minutes (dots are thousands separators in German)
  x <- str_trim(x)
  x <- str_remove(x, "'$")
  x <- str_remove_all(x, "\\.")
  suppressWarnings(as.integer(x))
}

# ─── 1. STANDINGS ──────────────────────────────────────────────────────────────
fetch_standings <- function(season_start) {
  url  <- sprintf("https://www.transfermarkt.com/super-lig/tabelle/wettbewerb/TR1/saison_id/%d", season_start)
  resp <- safe_get(url, pause = 6)
  if (is.null(resp) || status_code(resp) != 200) return(NULL)

  page <- read_html(content(resp, "text"))
  # The main table has cols: # | Club(img) | Club(name) | MP | W | D | L | Goals | +/- | Pts
  tbls <- html_table(page, fill = TRUE)
  tbl  <- NULL
  for (t in tbls) {
    if (ncol(t) == 10 && "Pts" %in% names(t) && nrow(t) >= 15) { tbl <- t; break }
  }
  if (is.null(tbl)) {
    for (t in tbls) {
      if (nrow(t) >= 15 && ncol(t) >= 9) { tbl <- t; break }
    }
  }
  if (is.null(tbl)) { message("standings: no table for ", season_start); return(NULL) }

  # Columns: [1]rank [2]club_img [3]club_name [4]MP [5]W [6]D [7]L [8]Goals [9]GD [10]Pts
  # Use positional renaming to avoid empty column name issues
  names(tbl) <- c("rank","club_img","team","MP","W","D","L","goals_str","GD","Pts")
  df <- tbl |>
    filter(!is.na(team), team != "") |>
    mutate(
      season  = paste0(season_start, "-", substr(season_start + 1, 3, 4)),
      MP      = as.integer(MP),
      W       = as.integer(W),
      D       = as.integer(D),
      L       = as.integer(L),
      GF      = as.integer(str_extract(goals_str, "^\\d+")),
      GA      = as.integer(str_extract(goals_str, "\\d+$")),
      GD      = as.integer(GD),
      Pts     = as.integer(Pts)
    ) |>
    select(team, season, MP, W, D, L, GF, GA, GD, Pts)

  message("Standings ", season_start, ": ", nrow(df), " clubs")
  df
}

# ─── 2. SQUAD MARKET VALUES ────────────────────────────────────────────────────
fetch_squad_values <- function(season_start) {
  url  <- sprintf("https://www.transfermarkt.com/super-lig/startseite/wettbewerb/TR1/plus/?saison_id=%d", season_start)
  resp <- safe_get(url, pause = 6)
  if (is.null(resp) || status_code(resp) != 200) return(NULL)

  page <- read_html(content(resp, "text"))
  tbls <- html_table(page, fill = TRUE)
  # Table: Club(img) | Club(name) | name? | Squad | ø age | Foreigners | ø market value | Total market value
  tbl  <- NULL
  for (t in tbls) {
    nms <- tolower(paste(names(t), collapse=" "))
    if (str_detect(nms, "market") && nrow(t) >= 15) { tbl <- t; break }
  }
  if (is.null(tbl)) return(NULL)

  # Col structure: [1]img [2]name [3]? [4]squad [5]age [6]foreigners [7]avg_val [8]total_val
  # Actual column layout (verified by inspection):
  # col1=img(NA), col2=team_name, col3=squad_size(int), col4=avg_age,
  # col5=foreigners, col6=avg_market_value_str, col7=total_market_value_str, col8=extra?
  core_names <- c("club_img","team","squad_size","avg_age","foreigners",
                  "avg_val_str","total_val_str")
  names(tbl) <- if (ncol(tbl) >= 8) c(core_names, rep("extra", ncol(tbl)-7)) else core_names
  df <- tbl |>
    filter(!is.na(team), team != "", !str_detect(team, "^\\d")) |>
    mutate(
      season                 = paste0(season_start, "-", substr(season_start + 1, 3, 4)),
      squad_size             = suppressWarnings(as.integer(squad_size)),
      squad_market_value_eur = parse_eur(total_val_str),
      avg_player_value_eur   = parse_eur(avg_val_str)
    ) |>
    filter(!is.na(squad_market_value_eur), squad_market_value_eur > 0) |>
    select(team, season, squad_size, squad_market_value_eur, avg_player_value_eur)

  message("Squad values ", season_start, ": ", nrow(df), " clubs")
  df
}

# ─── 3. TEAM LINKS ─────────────────────────────────────────────────────────────
fetch_team_links <- function(season_start) {
  url  <- sprintf("https://www.transfermarkt.com/super-lig/startseite/wettbewerb/TR1/plus/?saison_id=%d", season_start)
  resp <- safe_get(url, pause = 4)
  if (is.null(resp) || status_code(resp) != 200) return(NULL)

  page <- read_html(content(resp, "text"))
  links <- page |> html_elements("a") |> html_attr("href")
  team_links <- unique(links[str_detect(links, "/startseite/verein/\\d+/saison_id/")])

  data.frame(href = team_links, stringsAsFactors = FALSE) |>
    mutate(
      slug    = str_extract(href, "^/([^/]+)/startseite", group = 1),
      team_id = str_extract(href, "verein/(\\d+)", group = 1)
    ) |>
    filter(!is.na(team_id), !is.na(slug)) |>
    distinct(team_id, .keep_all = TRUE)
}

# ─── 4. PLAYER STATS PER TEAM ──────────────────────────────────────────────────
# Cell structure (1-based) from html_elements("td"):
#  1=jersey, 2=name+pos combined, 3=flag blank, 4=full name, 5=position, 6=age,
#  7=blank, 8=total apps in squad, 9=league apps (or "Not used…"), 10=Goals,
#  11=Assists, 12=YellowCards, 13=2ndYellow, 14=RedCard, 15=misc, 16=misc,
#  17=PPG, 18=minutes
fetch_player_stats <- function(slug, team_id, season_start) {
  # %%26 in sprintf → literal %26 in the URL (URL-encoded &)
  url  <- sprintf(
    "https://www.transfermarkt.com/%s/leistungsdaten/verein/%s/reldata/TR1%%26%d/plus/1",
    slug, team_id, season_start
  )
  # fallback: literal & (httr will encode it)
  url2 <- sprintf(
    "https://www.transfermarkt.com/%s/leistungsdaten/verein/%s/reldata/TR1&%d/plus/1",
    slug, team_id, season_start
  )

  resp <- safe_get(url, pause = 5)
  if (is.null(resp) || status_code(resp) != 200) resp <- safe_get(url2, pause = 5)
  if (is.null(resp) || status_code(resp) != 200) {
    message("  FAILED player stats: ", slug, " (", season_start, ")"); return(NULL)
  }

  page <- read_html(content(resp, "text"))
  tbl  <- page |> html_element("table.items")
  if (is.null(tbl)) {
    message("  No table.items: ", slug); return(NULL)
  }

  rows <- tbl |> html_elements("tbody tr")
  records <- list()

  for (row in rows) {
    cells <- row |> html_elements("td")
    if (length(cells) < 10) next  # separator/header row

    # Clean player name: use td.hauptlink link text
    name_el <- row |> html_element("td.hauptlink a")
    player   <- if (!is.null(name_el)) html_text(name_el, trim = TRUE) else NA_character_
    if (is.na(player) || player == "") next

    pos_raw <- html_text(cells[[5]], trim = TRUE)
    if (pos_raw == "") next  # skip blank rows

    apps_str <- html_text(cells[[9]], trim = TRUE)  # league apps or "Not used…"
    if (str_detect(apps_str, regex("not used|not in squad|keine", ignore_case = TRUE))) next

    goals_str   <- html_text(cells[[10]], trim = TRUE)
    assists_str <- html_text(cells[[11]], trim = TRUE)
    min_str     <- if (length(cells) >= 18) html_text(cells[[18]], trim = TRUE) else NA_character_

    goals   <- suppressWarnings(as.integer(goals_str))
    assists <- suppressWarnings(as.integer(assists_str))
    minutes <- parse_min(min_str)

    if (is.na(goals))   goals   <- 0L
    if (is.na(assists)) assists <- 0L

    # Normalise position (use full TM names only — no 2-letter codes that cause false matches)
    position <- case_when(
      str_detect(pos_raw, regex("goalkeeper|keeper",                  ignore_case = TRUE)) ~ "GK",
      str_detect(pos_raw, regex("centre.back|central defender",       ignore_case = TRUE)) ~ "CB",
      str_detect(pos_raw, regex("left.back",                          ignore_case = TRUE)) ~ "LB",
      str_detect(pos_raw, regex("right.back",                         ignore_case = TRUE)) ~ "RB",
      str_detect(pos_raw, regex("defensive midfield",                 ignore_case = TRUE)) ~ "DM",
      str_detect(pos_raw, regex("central midfield",                   ignore_case = TRUE)) ~ "CM",
      str_detect(pos_raw, regex("attacking midfield|second striker",  ignore_case = TRUE)) ~ "CAM",
      str_detect(pos_raw, regex("left winger?",                       ignore_case = TRUE)) ~ "LW",
      str_detect(pos_raw, regex("right winger?",                      ignore_case = TRUE)) ~ "RW",
      str_detect(pos_raw, regex("centre.forward|striker",             ignore_case = TRUE)) ~ "ST",
      TRUE ~ str_sub(pos_raw, 1, 5)
    )

    records[[length(records) + 1]] <- data.frame(
      player   = player,
      position = position,
      goals    = goals,
      assists  = assists,
      minutes  = if (is.na(minutes)) 0L else minutes,
      stringsAsFactors = FALSE
    )
  }

  if (length(records) == 0) return(NULL)
  bind_rows(records) |>
    mutate(team = slug, season_start = season_start) |>
    distinct(player, .keep_all = TRUE)
}

# ─── 5. TEAM NAME NORMALISATION ────────────────────────────────────────────────
normalize_team <- function(x) {
  dplyr::case_when(
    x %in% c("Galatasaray","galatasaray-istanbul","Galatasaray-Istanbul") ~ "Galatasaray",
    x %in% c("Fenerbahce","Fenerbahce-Istanbul","fenerbahce-istanbul","Fenerbahçe") ~ "Fenerbahçe",
    x %in% c("Besiktas","Besiktas JK","Beşiktaş","besiktas-istanbul") ~ "Beşiktaş",
    x %in% c("Trabzonspor","trabzonspor") ~ "Trabzonspor",
    x %in% c("Basaksehir","Basaksehir FK","Istanbul Basaksehir","Istanbul Basaksehir FK",
              "istanbul-basaksehir-fk","Başakşehir") ~ "Başakşehir",
    x %in% c("Adana Demirspor","adana-demirspor") ~ "Adana Demirspor",
    x %in% c("Samsunspor","samsunspor") ~ "Samsunspor",
    x %in% c("C. Rizespor","Caykur Rizespor","Çaykur Rizespor","caykur-rizespor","Rizespor") ~ "Çaykur Rizespor",
    x %in% c("Kayserispor","kayserispor") ~ "Kayserispor",
    x %in% c("Ankaragücü","Ankaragucu","MKE Ankaragucu","MKE Ankaragücü","mke-ankaragucu") ~ "Ankaragücü",
    x %in% c("Alanyaspor","alanyaspor") ~ "Alanyaspor",
    x %in% c("Karagümrük","Fatih Karagumruk","Fatih Karagümrük","fatih-karagumruk") ~ "Fatih Karagümrük",
    x %in% c("Konyaspor","konyaspor") ~ "Konyaspor",
    x %in% c("Antalyaspor","antalyaspor") ~ "Antalyaspor",
    x %in% c("Sivasspor","sivasspor") ~ "Sivasspor",
    x %in% c("Kasimpaşa","Kasimpasa","kasimpasa","Kasımpaşa") ~ "Kasımpaşa",
    x %in% c("Hatayspor","hatayspor") ~ "Hatayspor",
    x %in% c("Gaziantep FK","gaziantep-fk","Gaziantep") ~ "Gaziantep FK",
    x %in% c("Pendikspor","pendikspor") ~ "Pendikspor",
    x %in% c("Istanbulspor","istanbulspor","İstanbulspor") ~ "İstanbulspor",
    x %in% c("Eyupspor","eyupspor","Eyüpspor") ~ "Eyüpspor",
    x %in% c("Goztepe","goztepe","Göztepe") ~ "Göztepe",
    x %in% c("Bodrumspor","bodrumspor","Bodrum FK") ~ "Bodrum FK",
    x %in% c("Genclerbirligi","genclerbirligi","Genclerbirligi Ankara","genclerbirligi-ankara","Gençlerbirliği") ~ "Genclerbirliği",
    x %in% c("Kocaelispor","kocaelispor") ~ "Kocaelispor",
    TRUE ~ x
  )
}

# ─── MAIN: COLLECT ALL DATA ────────────────────────────────────────────────────
message("\n========== PHASE 1: STANDINGS ==========")
standings_list <- list()
for (yr in c(2023, 2024, 2025)) {
  message("Standings ", yr)
  s <- fetch_standings(yr)
  if (!is.null(s)) standings_list[[as.character(yr)]] <- s
}
all_standings <- bind_rows(standings_list)
all_standings$team <- normalize_team(all_standings$team)
message("Total standing rows: ", nrow(all_standings))

message("\n========== PHASE 2: SQUAD VALUES ==========")
sq_list <- list()
for (yr in c(2023, 2024, 2025)) {
  message("Squad values ", yr)
  s <- fetch_squad_values(yr)
  if (!is.null(s)) sq_list[[as.character(yr)]] <- s
}
all_squad <- bind_rows(sq_list)
all_squad$team <- normalize_team(all_squad$team)
message("Total squad rows: ", nrow(all_squad))

message("\n========== PHASE 3: TEAM LINKS ==========")
links_list <- list()
for (yr in c(2023, 2024, 2025)) {
  message("Team links ", yr)
  lnk <- fetch_team_links(yr)
  if (!is.null(lnk)) {
    lnk$season_start <- yr
    links_list[[as.character(yr)]] <- lnk
  }
}
# Combine: use union of all teams, prefer earlier season for IDs
all_links <- bind_rows(links_list) |>
  arrange(season_start) |>
  distinct(slug, .keep_all = TRUE)
message("Total unique team slugs: ", nrow(all_links))

# For each season, build the slug→season mapping
season_team_map <- bind_rows(links_list) |>
  select(slug, team_id, season_start) |>
  distinct()

message("\n========== PHASE 4: PLAYER STATS ==========")
player_list <- list()
for (i in seq_len(nrow(season_team_map))) {
  row   <- season_team_map[i, ]
  slug  <- row$slug
  tid   <- row$team_id
  yr    <- row$season_start
  message("  [", i, "/", nrow(season_team_map), "] ", slug, " (", yr, ")")
  p <- fetch_player_stats(slug, tid, yr)
  if (!is.null(p)) player_list[[length(player_list) + 1]] <- p
}

player_raw <- bind_rows(player_list)
message("Total player rows (raw): ", nrow(player_raw))

# Attach canonical team name and season label
# Note: player_raw$team = slug string, season_start = integer year
slug_to_team <- season_team_map |>
  mutate(
    canonical_team = normalize_team(slug),
    season         = paste0(season_start, "-", substr(season_start + 1, 3, 4))
  ) |>
  select(slug, season_start, canonical_team, season)

player_full <- player_raw |>
  rename(slug = team) |>
  left_join(slug_to_team, by = c("slug", "season_start")) |>
  mutate(
    team   = coalesce(canonical_team, normalize_team(slug)),
    season = coalesce(season, paste0(season_start, "-", substr(season_start + 1, 3, 4)))
  ) |>
  select(player, team, season, position, goals, assists, minutes) |>
  filter(!is.na(player), player != "") |>
  distinct(player, team, season, .keep_all = TRUE) |>
  mutate(
    goals            = as.integer(goals),
    assists          = as.integer(assists),
    minutes          = as.integer(minutes),
    market_value_eur = NA_real_
  )

message("player_full rows: ", nrow(player_full))

# ─── 6. BUILD team_season ──────────────────────────────────────────────────────
team_season <- all_standings |>
  left_join(all_squad, by = c("team", "season")) |>
  mutate(
    median_player_value_eur = avg_player_value_eur * 0.6,
    total_wages_eur         = squad_market_value_eur * 0.28   # approx wage ratio
  ) |>
  arrange(season, desc(Pts))

message("\nteam_season rows: ", nrow(team_season))
print(team_season |> select(team, season, W, D, L, Pts, squad_market_value_eur) |> head(10))

# ─── 7. GK STATS ───────────────────────────────────────────────────────────────
gk_clean <- player_full |>
  filter(position == "GK", minutes > 0) |>
  mutate(
    save_pct   = NA_real_,  # TM doesn't show save% — leave NA; real_players.csv used for Best XI
    gk_minutes = minutes
  ) |>
  select(player, team, season, save_pct, gk_minutes)

# ─── 8. SQUAD VALUES TABLE ────────────────────────────────────────────────────
squad_values <- all_squad |>
  mutate(median_player_value_eur = avg_player_value_eur * 0.6)

# ─── 9. SAVE ──────────────────────────────────────────────────────────────────
dir.create("data", showWarnings = FALSE)
save(team_season, player_full, gk_clean, squad_values,
     file = "data/superlig_final.RData")
message("\n✓ Saved data/superlig_final.RData")
message("  team_season : ", nrow(team_season), " rows")
message("  player_full : ", nrow(player_full), " rows")
message("  gk_clean    : ", nrow(gk_clean),    " rows")
message("  squad_values: ", nrow(squad_values), " rows")
