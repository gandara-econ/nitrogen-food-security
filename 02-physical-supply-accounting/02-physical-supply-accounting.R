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
#   IFA: Fertilizer consumption by nutrient, country
#
# Author: Erik Gandara
# Date: 2026-07-14
# =============================================================

rm(list = ls())

library(tidyverse)
library(here)

# --- Load FAO Fertilizer Data ---
fao_fert_raw <- read_csv(here("data/raw/Inputs_FertilizersNutrient_E_All_Data_NOFLAG.csv"))

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


# --- Define Gulf conflict-affected countries ---
gulf_countries <- c("Qatar", "Iran (Islamic Republic of)",
                    "Saudi Arabia", "United Arab Emirates",
                    "Bahrain", "Iraq", "Kuwait")

# --- Gulf production and export baseline ---
gulf_baseline <- fao_fert |>
  filter(Area %in% gulf_countries) |>
  group_by(Area, Element, Item) |>
  summarise(avg_2018_2023 = mean(value, na.rm = TRUE)) |>
  ungroup()

gulf_baseline |> print(n = 30)


# --- Total Gulf baseline ---
gulf_total <- gulf_baseline |>
  group_by(Element) |>
  summarise(gulf_total = sum(avg_2018_2023, na.rm = TRUE)) |>
  ungroup()

gulf_total