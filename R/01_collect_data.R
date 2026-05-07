# ============================================================================
# R/01_collect_data.R
# Türk Süper Ligi — Ham Veri Toplama
# Kaynaklar: Transfermarkt (worldfootballR), FBref (worldfootballR), Capology
# Sezonlar : 2023-24, 2024-25, 2025-26
#
# ÇALIŞTIRMA TALİMATLARI:
#   Bu script Quarto render edilmeden önce bir kez çalıştırılmalıdır.
#   data/ klasörüne 3 adet .RData dosyası kaydedilir.
#   Rate limiting nedeniyle toplam süre ~20-40 dakika sürebilir.
# ============================================================================

library(worldfootballR)
library(dplyr)
library(purrr)
library(stringr)

dir.create("data", showWarnings = FALSE)

seasons <- list(
  list(start = 2023, end = 2024, label = "2023-24"),
  list(start = 2024, end = 2025, label = "2024-25"),
  list(start = 2025, end = 2026, label = "2025-26")
)

# ── Yardımcı fonksiyon: güvenli HTTP çağrısı ─────────────────────────────────
safe_call <- function(expr, label = "") {
  tryCatch({
    result <- eval(expr)
    message("  OK: ", label, " — ", nrow(result), " satır")
    result
  }, error = function(e) {
    message("  HATA (", label, "): ", e$message)
    NULL
  })
}

# =============================================================================
# BÖLÜM 1: Transfermarkt — Kadro Piyasa Değerleri
# =============================================================================
message("\n=== TRANSFERMARKT: Kadro Piyasa Değerleri ===")

tm_list <- map(seasons, function(s) {
  message("Çekiliyor: ", s$label)
  Sys.sleep(8)  # Transfermarkt bot korumasına karşı uzun bekleme

  df <- safe_call(
    quote(tm_player_market_values(country_name = "Turkey", start_year = s$start)),
    label = paste("TM", s$label)
  )

  if (!is.null(df)) {
    df$season <- s$label
  }
  df
})

raw_tm <- bind_rows(compact(tm_list))

if (nrow(raw_tm) > 0) {
  save(raw_tm, file = "data/raw_tm_values.RData")
  message("KAYDEDILDI: data/raw_tm_values.RData — ", nrow(raw_tm), " satır, ",
          n_distinct(raw_tm$squad_name), " takım")
} else {
  warning("Transfermarkt verisi BOŞ — internet bağlantısını ve paket sürümünü kontrol et.")
}

Sys.sleep(10)

# =============================================================================
# BÖLÜM 2: FBref — Lig Tablosu (Takım Bazlı)
# =============================================================================
message("\n=== FBREF: Lig Tablosu ===")

standings_list <- map(seasons, function(s) {
  message("Çekiliyor standings: ", s$label)
  Sys.sleep(5)

  df <- safe_call(
    quote(fb_season_team_stats(
      country         = "TUR",
      gender          = "M",
      season_end_year = s$end,
      tier            = "1st",
      stat_type       = "standard"
    )),
    label = paste("FBref standings", s$label)
  )

  if (!is.null(df)) df$season <- s$label
  df
})

raw_standings <- bind_rows(compact(standings_list))
message("Standings toplam: ", nrow(raw_standings), " satır")

# =============================================================================
# BÖLÜM 3: FBref — Oyuncu İstatistikleri
# =============================================================================
message("\n=== FBREF: Oyuncu İstatistikleri ===")

collect_player_season <- function(season_end_year, season_label) {
  message("  League URL alınıyor: ", season_label)
  Sys.sleep(4)

  league_url <- tryCatch(
    fb_league_urls(country = "TUR", gender = "M",
                   season_end_year = season_end_year, tier = "1st"),
    error = function(e) { message("  league_url hatası: ", e$message); NULL }
  )

  if (is.null(league_url) || length(league_url) == 0) return(list(std = NULL, gk = NULL))

  Sys.sleep(4)
  message("  Team URLs alınıyor")
  team_urls <- tryCatch(
    fb_teams_urls(league_url[1]),
    error = function(e) { message("  team_urls hatası: ", e$message); NULL }
  )

  if (is.null(team_urls)) return(list(std = NULL, gk = NULL))
  message("  ", length(team_urls), " takım bulundu")

  # Standart oyuncu istatistikleri
  std_dfs <- map(team_urls, function(url) {
    Sys.sleep(2.5)
    tryCatch(
      fb_team_player_stats(team_urls = url, stat_type = "standard"),
      error = function(e) NULL
    )
  })

  # Kaleci istatistikleri
  gk_dfs <- map(team_urls, function(url) {
    Sys.sleep(2.5)
    tryCatch(
      fb_team_player_stats(team_urls = url, stat_type = "keeper"),
      error = function(e) NULL
    )
  })

  std <- bind_rows(compact(std_dfs))
  gk  <- bind_rows(compact(gk_dfs))

  if (nrow(std) > 0) std$season <- season_label
  if (nrow(gk)  > 0) gk$season  <- season_label

  message("  STD: ", nrow(std), " | GK: ", nrow(gk))
  list(std = std, gk = gk)
}

player_2324 <- collect_player_season(2024, "2023-24")
Sys.sleep(8)
player_2425 <- collect_player_season(2025, "2024-25")
Sys.sleep(8)
player_2526 <- collect_player_season(2026, "2025-26")

raw_player_std <- bind_rows(
  compact(list(player_2324$std, player_2425$std, player_2526$std))
)
raw_player_gk <- bind_rows(
  compact(list(player_2324$gk, player_2425$gk, player_2526$gk))
)

save(raw_standings, raw_player_std, raw_player_gk, file = "data/raw_fbref.RData")
message("KAYDEDILDI: data/raw_fbref.RData")
message("  Standings : ", nrow(raw_standings))
message("  Player STD: ", nrow(raw_player_std))
message("  Player GK : ", nrow(raw_player_gk))

Sys.sleep(10)

# =============================================================================
# BÖLÜM 4: Capology — Kulüp Maaş Verileri
# =============================================================================
message("\n=== CAPOLOGY: Kulüp Maaş Verileri ===")

# worldfootballR::capology_club_wage_data() kullanır
# country_name için "turkey" veya "super-lig" deneyebilirsiniz
collect_wages_season <- function(season_end_year, season_label) {
  message("  Capology çekiliyor: ", season_label)
  Sys.sleep(5)

  # Önce "turkey" dene, hata alırsan "super-lig" dene
  df <- tryCatch(
    capology_club_wage_data(country_name = "turkey",
                            season_end_year = season_end_year),
    error = function(e1) {
      message("    'turkey' hata verdi, 'super-lig' deneniyor...")
      Sys.sleep(3)
      tryCatch(
        capology_club_wage_data(country_name = "super-lig",
                                season_end_year = season_end_year),
        error = function(e2) {
          message("    Capology erişilemiyor (", season_label, "): ", e2$message)
          NULL
        }
      )
    }
  )

  if (!is.null(df) && nrow(df) > 0) {
    df$season <- season_label
    message("  OK: ", nrow(df), " kulüp kaydı")
  }
  df
}

wages_2324 <- collect_wages_season(2024, "2023-24")
wages_2425 <- collect_wages_season(2025, "2024-25")
wages_2526 <- collect_wages_season(2026, "2025-26")

raw_wages <- bind_rows(compact(list(wages_2324, wages_2425, wages_2526)))

if (nrow(raw_wages) > 0) {
  save(raw_wages, file = "data/raw_wages.RData")
  message("KAYDEDILDI: data/raw_wages.RData — ", nrow(raw_wages), " satır")
} else {
  message("UYARI: Capology verisi alınamadı. Maaş sütunları NA olacak.")
  raw_wages <- data.frame()
  save(raw_wages, file = "data/raw_wages.RData")
}

message("\n=== TÜM VERİLER TOPLANDILDI ===")
message("Bir sonraki adım: R/02_clean_merge.R çalıştır")
