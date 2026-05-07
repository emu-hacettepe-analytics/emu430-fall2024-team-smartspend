# ============================================================================
# R/02_clean_merge.R
# Türk Süper Ligi — Veri Temizleme ve Birleştirme
#
# Girdi:  data/raw_tm_values.RData, data/raw_fbref.RData, data/raw_wages.RData
# Çıktı:  data/superlig_final.RData
#   - team_season : takım × sezon özet tablosu
#   - player_full : oyuncu istatistikleri
#   - gk_clean    : kaleci kurtarış istatistikleri
#   - squad_values: kadro değeri özeti
# ============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# =============================================================================
# 0. Ham verileri yükle
# =============================================================================
if (!file.exists("data/raw_tm_values.RData") ||
    !file.exists("data/raw_fbref.RData")) {
  stop("Ham veri dosyaları bulunamadı. Önce R/01_collect_data.R çalıştırın.")
}

load("data/raw_tm_values.RData")   # raw_tm
load("data/raw_fbref.RData")       # raw_standings, raw_player_std, raw_player_gk
load("data/raw_wages.RData")       # raw_wages

message("Ham veriler yüklendi:")
message("  raw_tm        : ", nrow(raw_tm), " satır")
message("  raw_standings : ", nrow(raw_standings), " satır")
message("  raw_player_std: ", nrow(raw_player_std), " satır")
message("  raw_player_gk : ", nrow(raw_player_gk), " satır")
message("  raw_wages     : ", nrow(raw_wages), " satır")

# =============================================================================
# 1. Takım adı standardizasyonu
# =============================================================================
# Transfermarkt, FBref ve Capology farklı isim formatları kullanıyor.
# Bu lookup tablosu, yaygın tutarsızlıkları gideriyor.

team_name_map <- c(
  "Galatasaray"             = "Galatasaray",
  "Fenerbahce"              = "Fenerbahçe",
  "Fenerbahçe"              = "Fenerbahçe",
  "Besiktas"                = "Beşiktaş",
  "Beşiktaş"                = "Beşiktaş",
  "Trabzonspor"             = "Trabzonspor",
  "Istanbul Basaksehir"     = "Başakşehir",
  "Istanbul Başakşehir"     = "Başakşehir",
  "İstanbul Başakşehir"     = "Başakşehir",
  "Basaksehir FK"           = "Başakşehir",
  "Basaksehir"              = "Başakşehir",
  "Başakşehir"              = "Başakşehir",
  "Alanyaspor"              = "Alanyaspor",
  "Antalyaspor"             = "Antalyaspor",
  "Kayserispor"             = "Kayserispor",
  "Kasimpasa"               = "Kasımpaşa",
  "Kasımpaşa"               = "Kasımpaşa",
  "Konyaspor"               = "Konyaspor",
  "Sivasspor"               = "Sivasspor",
  "Samsunspor"              = "Samsunspor",
  "Rizespor"                = "Çaykur Rizespor",
  "Caykur Rizespor"         = "Çaykur Rizespor",
  "Çaykur Rizespor"         = "Çaykur Rizespor",
  "Rize"                    = "Çaykur Rizespor",
  "Ankaragücü"              = "Ankaragücü",
  "MKE Ankaragücü"          = "Ankaragücü",
  "Ankaragucu"              = "Ankaragücü",
  "Hatayspor"               = "Hatayspor",
  "Pendikspor"              = "Pendikspor",
  "Gaziantep FK"            = "Gaziantep FK",
  "Gaziantep"               = "Gaziantep FK",
  "Fatih Karagümrük"        = "Fatih Karagümrük",
  "Karagümrük"              = "Fatih Karagümrük",
  "Karagümruk"              = "Fatih Karagümrük",
  "Adana Demirspor"         = "Adana Demirspor",
  "Eyüpspor"                = "Eyüpspor",
  "Eyupspor"                = "Eyüpspor",
  "Bodrum FK"               = "Bodrum FK",
  "Göztepe"                 = "Göztepe",
  "Goztepe"                 = "Göztepe",
  "Çorum FK"                = "Çorum FK",
  "Corum FK"                = "Çorum FK",
  "Bandirmaspor"            = "Bandırmaspor",
  "Bandırmaspor"            = "Bandırmaspor"
)

standardize_team <- function(name) {
  clean <- str_trim(as.character(name))
  result <- team_name_map[clean]
  # Lookup'ta yoksa orijinal adı döndür (uyarı ver)
  unmapped <- is.na(result)
  if (any(unmapped)) {
    unique_unmapped <- unique(clean[unmapped])
    message("  Eşleştirilemeyen takım adları: ", paste(unique_unmapped, collapse = ", "))
  }
  ifelse(unmapped, clean, result)
}

# =============================================================================
# 2. Transfermarkt — Kadro Piyasa Değeri Özeti
# =============================================================================
message("\n--- Transfermarkt işleniyor ---")

# FBref'te oyuncu yokken Transfermarkt verisinden tahmin etmek için sütun adlarını kontrol et
tm_cols <- names(raw_tm)
message("  TM sütunları: ", paste(tm_cols, collapse = ", "))

# worldfootballR tm_player_market_values() çıktı sütunları genellikle:
# squad_name, player_name, player_age, player_position, player_market_value_euro
# Farklı versiyonlarda sütun adları değişebilir — aşağıda her iki olasılık ele alınıyor

squad_values <- raw_tm |>
  mutate(
    team = standardize_team(
      coalesce(
        if ("squad_name" %in% tm_cols) squad_name else NA_character_,
        if ("club"       %in% tm_cols) club       else NA_character_
      )
    ),
    market_value_eur = coalesce(
      if ("player_market_value_euro" %in% tm_cols) player_market_value_euro else NA_real_,
      if ("market_value_euro"        %in% tm_cols) market_value_euro        else NA_real_
    )
  ) |>
  filter(!is.na(team), !is.na(market_value_eur)) |>
  group_by(team, season) |>
  summarise(
    squad_market_value_eur = sum(market_value_eur, na.rm = TRUE),
    squad_size             = n(),
    avg_player_value_eur   = mean(market_value_eur, na.rm = TRUE),
    median_player_value_eur = median(market_value_eur, na.rm = TRUE),
    .groups = "drop"
  )

message("  squad_values: ", nrow(squad_values), " satır, ",
        n_distinct(squad_values$team), " takım")

# =============================================================================
# 3. FBref Standings — Lig Tablosu
# =============================================================================
message("\n--- FBref standings işleniyor ---")

stn_cols <- names(raw_standings)
message("  Standings sütunları: ", paste(head(stn_cols, 20), collapse = ", "))

# FBref sütun adları: Squad, MP, W, D, L, GF, GA, GD, Pts (veya Rk, Squad, ...)
standings_clean <- raw_standings |>
  mutate(
    team = standardize_team(
      coalesce(
        if ("Squad" %in% stn_cols) Squad else NA_character_,
        if ("squad" %in% stn_cols) squad else NA_character_
      )
    )
  ) |>
  select(
    team,
    season,
    MP  = any_of(c("MP",  "Matches")),
    W   = any_of(c("W",   "Wins")),
    D   = any_of(c("D",   "Draws")),
    L   = any_of(c("L",   "Losses")),
    GF  = any_of(c("GF",  "Goals")),
    GA  = any_of(c("GA",  "GoalsAgainst")),
    GD  = any_of(c("GD",  "GoalDiff")),
    Pts = any_of(c("Pts", "Points"))
  ) |>
  mutate(across(c(W, D, L, GF, GA, GD, Pts, MP), as.integer)) |>
  filter(!is.na(Pts), !is.na(team))

message("  standings_clean: ", nrow(standings_clean), " satır")

# =============================================================================
# 4. FBref Oyuncu İstatistikleri
# =============================================================================
message("\n--- FBref oyuncu istatistikleri işleniyor ---")

std_cols <- names(raw_player_std)
message("  STD sütunları: ", paste(head(std_cols, 25), collapse = ", "))

# Yaygın FBref oyuncu sütunları: Player, Squad, Pos, Age, Nation, Min, Gls, Ast
player_clean <- raw_player_std |>
  mutate(
    team = standardize_team(
      coalesce(
        if ("Squad" %in% std_cols) Squad else NA_character_,
        if ("squad" %in% std_cols) squad else NA_character_
      )
    ),
    player   = coalesce(
      if ("Player" %in% std_cols) Player else NA_character_,
      if ("player" %in% std_cols) player else NA_character_
    ),
    position = str_extract(
      coalesce(if ("Pos" %in% std_cols) Pos else NA_character_,
               if ("pos" %in% std_cols) pos else ""),
      "^[A-Z]+"
    ),
    minutes  = as.integer(str_remove_all(
      coalesce(if ("Min" %in% std_cols) Min else NA_character_, "0"), ","
    )),
    goals    = as.integer(
      coalesce(if ("Gls" %in% std_cols) Gls else NA_character_, "0")
    ),
    assists  = as.integer(
      coalesce(if ("Ast" %in% std_cols) Ast else NA_character_, "0")
    )
  ) |>
  filter(!is.na(player), !is.na(team), !is.na(minutes), minutes > 0) |>
  select(player, team, season, position, minutes, goals, assists) |>
  distinct(player, team, season, .keep_all = TRUE)

message("  player_clean: ", nrow(player_clean), " satır")

# Kaleci
gk_cols <- names(raw_player_gk)
message("  GK sütunları: ", paste(head(gk_cols, 20), collapse = ", "))

gk_clean <- raw_player_gk |>
  mutate(
    team   = standardize_team(
      coalesce(if ("Squad" %in% gk_cols) Squad else NA_character_,
               if ("squad" %in% gk_cols) squad else NA_character_)
    ),
    player = coalesce(if ("Player" %in% gk_cols) Player else NA_character_,
                      if ("player" %in% gk_cols) player else NA_character_),
    save_pct   = as.numeric(
      coalesce(if ("Save%" %in% gk_cols) .data[["Save%"]] else NA_character_,
               if ("save_pct" %in% gk_cols) save_pct else NA_character_)
    ),
    gk_minutes = as.integer(str_remove_all(
      coalesce(if ("Min" %in% gk_cols) Min else NA_character_, "0"), ","
    ))
  ) |>
  filter(!is.na(player), !is.na(save_pct), gk_minutes > 0) |>
  select(player, team, season, save_pct, gk_minutes) |>
  distinct(player, team, season, .keep_all = TRUE)

message("  gk_clean: ", nrow(gk_clean), " satır")

# =============================================================================
# 5. Capology Maaş Verisi
# =============================================================================
message("\n--- Capology maaş verisi işleniyor ---")

if (nrow(raw_wages) > 0) {
  wage_cols <- names(raw_wages)
  message("  Wages sütunları: ", paste(wage_cols, collapse = ", "))

  # Capology sütunları değişken olabilir; olası adlar deneniyor
  wages_clean <- raw_wages |>
    mutate(
      team = standardize_team(
        coalesce(
          if ("club"       %in% wage_cols) club       else NA_character_,
          if ("Club"       %in% wage_cols) Club       else NA_character_,
          if ("squad_name" %in% wage_cols) squad_name else NA_character_
        )
      ),
      total_wages_eur = as.numeric(str_remove_all(
        coalesce(
          if ("annual_wages"  %in% wage_cols) as.character(annual_wages)  else NA_character_,
          if ("total_wages"   %in% wage_cols) as.character(total_wages)   else NA_character_,
          if ("weekly_wages"  %in% wage_cols) as.character(weekly_wages)  else "0"
        ), "[^0-9.]"
      )) * if ("weekly_wages" %in% wage_cols && !("annual_wages" %in% wage_cols)) 52 else 1
    ) |>
    filter(!is.na(team), !is.na(total_wages_eur)) |>
    select(team, season, total_wages_eur) |>
    group_by(team, season) |>
    summarise(total_wages_eur = sum(total_wages_eur, na.rm = TRUE), .groups = "drop")

  message("  wages_clean: ", nrow(wages_clean), " satır")
} else {
  message("  Capology verisi yok — maaş sütunu NA olacak")
  wages_clean <- tibble(team = character(), season = character(),
                        total_wages_eur = numeric())
}

# =============================================================================
# 6. Birleştirme — Takım Bazlı Ana Tablo
# =============================================================================
message("\n--- Takım bazlı veri birleştiriliyor ---")

team_season <- standings_clean |>
  left_join(squad_values, by = c("team", "season")) |>
  left_join(wages_clean,  by = c("team", "season")) |>
  filter(!is.na(squad_market_value_eur))

message("  team_season: ", nrow(team_season), " satır, ",
        n_distinct(team_season$team), " takım, ",
        n_distinct(team_season$season), " sezon")

# =============================================================================
# 7. Birleştirme — Oyuncu Tam Veri Seti
# =============================================================================
message("\n--- Oyuncu veri seti oluşturuluyor ---")

# Transfermarkt oyuncu bazlı piyasa değeri ekle
tm_player_lookup <- raw_tm |>
  mutate(
    player = coalesce(
      if ("player_name" %in% names(raw_tm)) player_name else NA_character_,
      if ("Player"      %in% names(raw_tm)) Player      else NA_character_
    ),
    team = standardize_team(
      coalesce(
        if ("squad_name" %in% names(raw_tm)) squad_name else NA_character_,
        if ("club"       %in% names(raw_tm)) club       else NA_character_
      )
    ),
    market_value_eur = coalesce(
      if ("player_market_value_euro" %in% names(raw_tm)) player_market_value_euro else NA_real_,
      if ("market_value_euro"        %in% names(raw_tm)) market_value_euro        else NA_real_
    )
  ) |>
  select(player, team, season, market_value_eur) |>
  filter(!is.na(player), !is.na(market_value_eur))

player_full <- player_clean |>
  left_join(tm_player_lookup, by = c("player", "team", "season")) |>
  # market_value eksikse takım ortalamasını kullan
  left_join(squad_values |> select(team, season, avg_player_value_eur),
            by = c("team", "season")) |>
  mutate(
    market_value_eur = coalesce(market_value_eur, avg_player_value_eur)
  ) |>
  select(-avg_player_value_eur)

message("  player_full: ", nrow(player_full), " satır")

# =============================================================================
# 8. Veri Kalite Kontrolü
# =============================================================================
message("\n--- Kalite kontrolü ---")

# Temel istatistikler
cat("\n team_season özeti:\n")
print(summary(team_season |> select(squad_market_value_eur, Pts, total_wages_eur)))

# Eksik değer raporu
na_report <- team_season |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "sütun", values_to = "NA_sayısı") |>
  filter(NA_sayısı > 0)

if (nrow(na_report) > 0) {
  message("  Eksik değer bulunan sütunlar:")
  print(na_report)
}

# =============================================================================
# 9. Kayıt
# =============================================================================
message("\n--- Kaydediliyor ---")

save(team_season, player_full, gk_clean, squad_values,
     file = "data/superlig_final.RData")

message("TAMAMLANDI: data/superlig_final.RData kaydedildi")
message("  team_season : ", nrow(team_season), " satır")
message("  player_full : ", nrow(player_full), " satır")
message("  gk_clean    : ", nrow(gk_clean), " satır")
message("  squad_values: ", nrow(squad_values), " satır")
message("\nQuarto site'yi render etmek için: quarto render")
