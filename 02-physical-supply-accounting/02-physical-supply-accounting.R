# =============================================================
# 2026 Iran Conflict: Agricultural and Food Security Consequences
# A Physical Supply Balance Approach
# Phase 2: Global Fertilizer Supply Disruption
#
# Measures the physical reduction in global nitrogen and
# phosphate fertilizer supply attributable to the 2026
# Iran conflict and Strait of Hormuz closure.
#
# Data sources:
#   FAO FAOSTAT: Fertilizers by Nutrient (production, export)
#   FAO FAOSTAT: Fertilizers by Product (import quantity by country)
#   WITS / UN Comtrade: bilateral nitrogen imports by source (HS 3102)
#
# Author: Erik Gandara
# Date: 2026-07-14
# =============================================================

rm(list = ls())

library(tidyverse)
library(here)

# --- Load FAO Fertilizer-by-Nutrient Data (production, export) ---
fao_fert_raw <- read_csv(
  here("data/raw/Inputs_FertilizersNutrient_E_All_Data_NOFLAG.csv")
)

head(fao_fert_raw)
tail(fao_fert_raw)


# --- Filter to relevant elements and nutrients ---
fao_fert <- fao_fert_raw |>
  filter(
    Element %in% c("Production", "Export quantity"),
    Item %in% c("Nutrient nitrogen N (total)",
                "Nutrient phosphate P2O5 (total)")
  ) |>
  select(Area, Element, Item, Y2018:Y2023) |>
  pivot_longer(
    cols = Y2018:Y2023,
    names_to = "year",
    values_to = "value"
  ) |>
  mutate(year = as.integer(str_remove(year, "Y")))

head(fao_fert)
nrow(fao_fert)


# --- Load FAO Fertilizer-by-Product Import Data ---
# NOTE: This is total imports by reporting country only,
# NOT bilateral by source country. Bilateral source data
# comes from WITS below (wits_nitrogen). Named fao_imports
# to avoid confusion with the WITS bilateral table.
# File: FAOSTAT_fertilizer_imports.csv
#   Domain:  Fertilizers by Product
#   Element: Import quantity
#   Years:   2021, 2022, 2023
#   192 countries, 10 nitrogen + phosphate products
fao_imports_raw <- read_csv(
  here("data/raw/FAOSTAT_fertilizer_imports.csv"),
  col_types = cols(
    Value = col_double(),
    Year  = col_integer(),
    .default = col_character()
  )
)

fao_imports <- fao_imports_raw |>
  select(country = Area, product = Item,
         year = Year, tonnes = Value) |>
  filter(!is.na(tonnes)) |>
  # Reconcile FAO country names to the keys used in wits_nitrogen /
  # wits_phosphate so downstream joins match. FAO uses long-form
  # names (e.g. "United Republic of Tanzania") that otherwise fail
  # to join against "Tanzania" and leave NA tonnage.
  mutate(country = recode(country,
                          "United Republic of Tanzania" = "Tanzania",
                          "United States of America"    = "United States of America",
                          "Iran (Islamic Republic of)"  = "Iran (Islamic Republic of)"
  ))

head(fao_imports)
nrow(fao_imports)


# --- Define Gulf conflict-affected countries ---
gulf_countries <- c("Qatar", "Iran (Islamic Republic of)",
                    "Saudi Arabia", "United Arab Emirates",
                    "Bahrain", "Iraq", "Kuwait", "Oman")

# --- Gulf production and export baseline ---
gulf_baseline <- fao_fert |>
  filter(Area %in% gulf_countries) |>
  group_by(Area, Element, Item) |>
  summarise(avg_2018_2023 = mean(value, na.rm = TRUE),
            .groups = "drop")

gulf_baseline |> print(n = 30)


# --- Total Gulf baseline ---
gulf_total <- gulf_baseline |>
  group_by(Element) |>
  summarise(gulf_total = sum(avg_2018_2023, na.rm = TRUE),
            .groups = "drop")

gulf_total


# --- Total fertilizer imports by country (tonnes) ---
# Sum all products within each country-year to get total
# annual tonnes, then average across 2021-2023 for a stable
# baseline. This average-annual-tonnes figure is the physical
# quantity that dependency percentages are applied to.
imports_by_country <- fao_imports |>
  group_by(country, year) |>
  summarise(annual_tonnes = sum(tonnes, na.rm = TRUE),
            .groups = "drop") |>
  group_by(country) |>
  summarise(avg_imports_tonnes = mean(annual_tonnes, na.rm = TRUE),
            .groups = "drop") |>
  arrange(desc(avg_imports_tonnes))

head(imports_by_country, 15)


# --- Define nitrogen and phosphate products ---
nitrogen_products <- c("Ammonia, anhydrous",
                       "Ammonium nitrate (AN)",
                       "Ammonium sulphate",
                       "Urea",
                       "Urea and ammonium nitrate solutions (UAN)")

phosphate_products <- c("Diammonium phosphate (DAP)",
                        "Monoammonium phosphate (MAP)",
                        "Phosphate rock",
                        "Superphosphates above 35%",
                        "Superphosphates, other")

# --- Total imports by nutrient type (tonnes), 2021-2023 avg ---
# Same logic as imports_by_country: sum the products in each
# nutrient group per country-year, then average across years.
nitrogen_imports <- fao_imports |>
  filter(product %in% nitrogen_products) |>
  group_by(country, year) |>
  summarise(annual_n = sum(tonnes, na.rm = TRUE), .groups = "drop") |>
  group_by(country) |>
  summarise(avg_n_imports = mean(annual_n, na.rm = TRUE),
            .groups = "drop")

phosphate_imports <- fao_imports |>
  filter(product %in% phosphate_products) |>
  group_by(country, year) |>
  summarise(annual_p = sum(tonnes, na.rm = TRUE), .groups = "drop") |>
  group_by(country) |>
  summarise(avg_p_imports = mean(annual_p, na.rm = TRUE),
            .groups = "drop")


# --- WTO Gulf dependency percentages by nutrient (2024) ---
# Source: WTO Data Blog, July 10 2026
# https://www.wto.org/english/blogs_e/data_blog_e/blog_dta_10jul26_451_e.htm
# Figure 4: Major fertilizer importers' world shares by nutrient group, 2024
# Single canonical definition (previously duplicated).
gulf_dependency <- tibble(
  country = c("India", "Thailand", "Morocco",
              "Brazil", "United States of America",
              "Australia"),
  gulf_pct_n = c(0.637, 0.470, 0.383, 0.213, 0.164, NA),
  gulf_pct_p = c(0.221, NA,    NA,    0.090, 0.298, 0.043)
)

# --- Join and calculate tonnes at risk (nutrient-split) ---
exposure <- nitrogen_imports |>
  full_join(phosphate_imports, by = "country") |>
  inner_join(gulf_dependency, by = "country") |>
  mutate(
    n_at_risk = avg_n_imports * gulf_pct_n,
    p_at_risk = avg_p_imports * gulf_pct_p,
    total_at_risk = coalesce(n_at_risk, 0) +
      coalesce(p_at_risk, 0)
  ) |>
  arrange(desc(total_at_risk))

exposure


# --- WITS Bilateral Nitrogen Import Data (HS 3102, 2022) ---
# Source: WITS World Bank / UN Comtrade
# Gulf countries: Qatar, Saudi Arabia, UAE, Iran, Kuwait,
#                 Bahrain, Oman
wits_nitrogen <- tibble(
  country = c("Kenya", "Malawi", "Tanzania", "Uganda",
              "Mozambique", "Rwanda", "Zimbabwe",
              "India", "Brazil", "United States of America",
              "Thailand", "Morocco", "Australia"),
  
  total_n_imports_kg = c(
    197555000, 141002000, 360046000, 22437300,
    159976000, 11089500, 1651280000,
    NA, NA, NA, NA, NA, NA
  ),
  
  gulf_n_kg = c(
    70633482, 53873500, 106943500, 5986090,
    27336630, 10612300, 28181000,
    NA, NA, NA, NA, NA, NA
  ),
  
  gulf_pct_n = c(
    0.357, 0.382, 0.297, 0.267,
    0.171, 0.957, 0.017,
    0.637, 0.213, 0.164,
    0.470, 0.383, NA
  ),
  
  russia_n_kg = c(
    0, 0, 36389100, 8324800,
    30011700, 0, 23539800,
    NA, NA, NA, NA, NA, NA
  ),
  
  source = c(
    rep("WITS/Comtrade 2022", 7),
    rep("WTO Data Blog 2024", 6)
  )
) |>
  mutate(
    gulf_pct_n = ifelse(is.na(gulf_pct_n) & !is.na(gulf_n_kg),
                        gulf_n_kg / total_n_imports_kg,
                        gulf_pct_n),
    russia_pct_n = russia_n_kg / total_n_imports_kg
  )

wits_nitrogen |>
  select(country, gulf_pct_n, russia_pct_n) |>
  print(n = 13)


# --- Combined disruption: Gulf + Russia ---
# Use FAO nitrogen_imports (total N tonnes, 2021-2023 avg) as the
# denominator for ALL countries, so nitrogen rests on the SAME
# FAO product-weight basis as phosphate. This replaces the earlier
# WITS total_n_imports_kg, which was populated only for the 7
# African countries (leaving India/US/Brazil/etc. with NA nitrogen
# bars). The Gulf/Russia shares from wits_nitrogen are applied to
# the FAO tonnage. Result is in tonnes (not kg).
disruption_table <- wits_nitrogen |>
  select(country, gulf_pct_n, russia_pct_n, source) |>
  left_join(nitrogen_imports, by = "country") |>  # FAO total N tonnes (avg_n_imports)
  mutate(
    combined_pct = case_when(
      !is.na(gulf_pct_n) & !is.na(russia_pct_n) ~
        gulf_pct_n + russia_pct_n,
      !is.na(gulf_pct_n) ~ gulf_pct_n,
      TRUE ~ NA_real_
    ),
    gulf_disruption_t     = avg_n_imports * gulf_pct_n,
    russia_disruption_t   = avg_n_imports * russia_pct_n,
    combined_disruption_t = avg_n_imports * combined_pct
  ) |>
  arrange(desc(combined_pct))

disruption_table |>
  select(country, avg_n_imports, gulf_pct_n, russia_pct_n,
         combined_pct, combined_disruption_t) |>
  print(n = 13)


# =============================================================
# PHOSPHATE Gulf dependency share (DAP proxy)
# =============================================================
# Streamlined approach to keep Phase 2 scoped:
#   Denominator = FAO total phosphate imports (phosphate_imports,
#                 already loaded, 2021-2023 avg tonnes).
#   Gulf share  = single WITS query per country at HS 310530
#                 (DAP), Gulf partners / World, net weight kg.
#
# DAP dominates traded phosphate fertilizer, so the Gulf share
# of DAP is used as a proxy for the Gulf share of TOTAL
# phosphate. State this approximation in the writeup.
#
# Gulf partners: Qatar, Saudi Arabia, UAE, Iran, Kuwait,
#                Bahrain, Oman.
#
# gulf_pct_p is entered directly (Gulf DAP kg / World DAP kg
# from WITS). Tonnes at risk are computed downstream by
# applying this share to the FAO total-phosphate tonnage.
#
# COLLECTED SO FAR (WITS 310530, 2022):
#   Kenya:  World 161,979,000 kg; Gulf (Saudi Arabia) 68,300,000 kg
#           -> 68,300,000 / 161,979,000 = 0.4217
#   Malawi: World 12,819,400 kg; Gulf (Saudi 11,982,000 + Oman 830,000)
#           = 12,812,000 -> 12,812,000 / 12,819,400 = 0.9994
#   Tanzania: World 156,891,000 kg; Gulf (Saudi Arabia) 28,356,100 kg
#           -> 28,356,100 / 156,891,000 = 0.1807 (Russia 22M excluded, Gulf-only)
#   Uganda: World 4,355,680 kg; Gulf (Saudi Arabia) 3,003,000 kg
#           -> 3,003,000 / 4,355,680 = 0.6895
#   Mozambique: World 17,000,000 kg; Gulf (Saudi Arabia) 17,000,000 kg
#           -> 17,000,000 / 17,000,000 = 1.0000 (single-sourced Saudi)
#   Rwanda: World 15,591,000 kg; Gulf (Saudi Arabia) 9,187,000 kg
#           -> 9,187,000 / 15,591,000 = 0.5893 (N Gulf was 95.7%; P diverges)
#   Zimbabwe: World 4,513,900 kg; Gulf 0 (Mauritius 4,511,900 transship + SA 2,000)
#           -> 0 / 4,513,900 = 0.0000 (Mauritius = hidden-origin transship)
#   India: World 6,779,300,000 kg; Gulf (Saudi 1,976,540,000 + UAE 4,215,000)
#           = 1,980,755,000 -> 1,980,755,000 / 6,779,300,000 = 0.2922
#           (weight-based; WTO value-based was 22.1% - tonnage differs)
#   Brazil: World 215,451,000 kg; Gulf (Saudi Arabia) 40,990,000 kg
#           -> 40,990,000 / 215,451,000 = 0.1903
#           (weight-based; WTO value-based was 9.0%)
#   USA: World 592,201,000 kg; Gulf (Saudi Arabia) 406,278,000 kg
#           -> 406,278,000 / 592,201,000 = 0.6860
#           (weight-based; WTO value-based was 29.8% - large basis divergence, flag)
#   Thailand: World 7,436,210 kg; Gulf (Saudi Arabia) 5 kg
#           -> 5 / 7,436,210 = 0.0000 (98.5% China; N Gulf was 47.0%, P diverges)
#   Morocco: World 162 kg (negligible - Morocco is a PRODUCER, not importer)
#           -> 0 / 162 = 0.0000 (import-based method N/A; not a true exposure zero)
#   Australia: World 121,174,000 kg; Gulf (Saudi Arabia) 23,976,900 kg
#           -> 23,976,900 / 121,174,000 = 0.1979
#           (WTO's 4.3% was world-share not Gulf; confirmed earlier suspicion)
# COLLECTION COMPLETE — all 13 countries.
wits_phosphate <- tibble(
  country = c("Kenya", "Malawi", "Tanzania", "Uganda",
              "Mozambique", "Rwanda", "Zimbabwe",
              "India", "Brazil", "United States of America",
              "Thailand", "Morocco", "Australia"),
  
  # Gulf share of DAP imports (Gulf kg / World kg, WITS 310530 2022)
  gulf_pct_p = c(
    0.4217, 0.9994, 0.1807, 0.6895,
    1.0000, 0.5893, 0.0000,
    0.2922, 0.1903, 0.6860, 0.0000, 0.0000, 0.1979
  ),
  
  source = rep("WITS/Comtrade 2022 (DAP 310530, net weight)", 13)
)

wits_phosphate |>
  select(country, gulf_pct_p) |>
  print(n = 13)


# =============================================================
# Combined nitrogen + phosphate fertilizer at risk (Gulf-only)
# =============================================================
# Nutrient columns are kept SEPARATE (not summed into a single
# raw tonnage) because product-weight urea and product-weight
# DAP are not nutritionally comparable, and Phase 3 is a
# nitrogen deficit model. The stacked bar visualizes the
# segments side by side rather than collapsing them.
#
# Phosphate tonnes at risk = FAO total phosphate tonnage
# (phosphate_imports$avg_p_imports) x Gulf DAP share.
combined_risk <- disruption_table |>
  select(country, avg_n_imports,
         gulf_pct_n, russia_pct_n,
         gulf_disruption_t, russia_disruption_t) |>
  left_join(
    wits_phosphate |> select(country, gulf_pct_p),
    by = "country"
  ) |>
  left_join(
    phosphate_imports,  # FAO total phosphate tonnes (avg_p_imports)
    by = "country"
  ) |>
  mutate(
    # Nitrogen disruption already in tonnes (FAO basis)
    gulf_n_t   = gulf_disruption_t,
    russia_n_t = russia_disruption_t,
    # Phosphate at risk in tonnes (FAO tonnes x Gulf DAP share)
    gulf_p_t   = avg_p_imports * gulf_pct_p
  ) |>
  arrange(desc(coalesce(gulf_n_t, 0) +
                 coalesce(russia_n_t, 0) +
                 coalesce(gulf_p_t, 0)))

combined_risk |>
  select(country, gulf_n_t, russia_n_t, gulf_p_t) |>
  print(n = 13)


# =============================================================
# Phase 2 Visualization: tonnes of fertilizer at risk by country
# Stacked by Gulf nitrogen, Russia nitrogen, Gulf phosphate
# =============================================================
plot_data <- combined_risk |>
  select(country, gulf_n_t, russia_n_t, gulf_p_t) |>
  pivot_longer(
    cols = c(gulf_n_t, russia_n_t, gulf_p_t),
    names_to = "source",
    values_to = "tonnes"
  ) |>
  filter(!is.na(tonnes) & tonnes > 0) |>
  mutate(
    source = factor(
      source,
      levels = c("gulf_n_t", "russia_n_t", "gulf_p_t"),
      labels = c("Gulf nitrogen", "Russia nitrogen",
                 "Gulf phosphate")
    )
  )

# Order countries by total tonnes at risk
country_order <- plot_data |>
  group_by(country) |>
  summarise(total = sum(tonnes, na.rm = TRUE), .groups = "drop") |>
  arrange(total) |>
  pull(country)

plot_data <- plot_data |>
  mutate(country = factor(country, levels = country_order))

p_risk <- ggplot(plot_data,
                 aes(x = country, y = tonnes, fill = source)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(
    "Gulf nitrogen"   = "#B22222",  # firebrick — Gulf N
    "Russia nitrogen" = "#4682B4",  # steel blue — Russia N
    "Gulf phosphate"  = "#DAA520"   # goldenrod — Gulf P
  )) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Fertilizer Import Tonnage at Risk from 2026 Conflict Disruption",
    subtitle = "Gulf and Russia-sourced nitrogen and Gulf-sourced phosphate, by importing country",
    x = NULL,
    y = "Tonnes at risk (product weight)",
    fill = "Disruption source",
    caption = "Source: WITS/UN Comtrade 2022 bilateral trade (net weight). Nutrient segments not summed."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

p_risk

# --- Save output ---
ggsave(
  here("02-physical-supply-accounting/fertilizer_at_risk_by_country.png"),
  plot = p_risk,
  width = 10, height = 6, dpi = 300
)


# =============================================================
# Phase 2 Table: dependency + tonnage at risk by country
# Sorted by combined nitrogen dependency (vulnerability view).
# Complements the bar chart, which ranks by absolute tonnage.
#
# Exported as CSV (no rendering dependencies). The presentation
# table image is produced separately; this CSV holds the exact
# underlying numbers.
# =============================================================
table_data <- combined_risk |>
  mutate(
    combined_pct = case_when(
      !is.na(gulf_pct_n) & !is.na(russia_pct_n) ~ gulf_pct_n + russia_pct_n,
      !is.na(gulf_pct_n) ~ gulf_pct_n,
      TRUE ~ NA_real_
    )
  ) |>
  select(
    country,
    total_n_imports_t = avg_n_imports,
    gulf_pct_n,
    russia_pct_n,
    combined_pct,
    gulf_n_at_risk_t  = gulf_n_t,
    gulf_pct_p,
    gulf_p_at_risk_t  = gulf_p_t
  ) |>
  arrange(desc(combined_pct))

print(table_data, n = 13)

write_csv(
  table_data,
  here("02-physical-supply-accounting/fertilizer_risk_table.csv")
)