# =============================================================
# 2026 Iran Conflict: Agricultural and Food Security Consequences
# A Physical Supply Balance Approach
# Phase 1: Energy, Fertilizer, and Food Price Visualization
#
# Series:
#   - World Bank Fossil Fuel Energy Index (composite)
#   - World Bank Composite Fertilizer Index (urea + phosphate)
#   - FAO Cereal Price Index (2014-2016 = 100)
#
# All series indexed to January 2000 = 100 for comparability
#
# Data sources:
#   World Bank Pink Sheet: CMO-Historical-Data-Monthly.xlsx
#   FAO Cereal Price Index: ffpi-data-2026-07.xlsx
#
# Author: Erik Gandara
# Date: 2026-07-13
# =============================================================

library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(here)
library(ggrepel)

# --- Load World Bank Pink Sheet (urea and phosphate) ---
pink_sheet_raw <- read_excel(
  here("data/raw/CMO-Historical-Data-Monthly.xlsx"),
  sheet = "Monthly Prices",
  skip = 5,
  col_names = FALSE
)

pink_sheet <- pink_sheet_raw |>
  select(date = 1, phosphate = 58, urea = 61) |>
  slice(-1) |>
  filter(!is.na(date)) |>
  mutate(
    date      = paste0(substr(date, 1, 4), "-",
                       substr(date, 6, 7), "-01"),
    date      = as.Date(date),
    phosphate = as.numeric(phosphate),
    urea      = as.numeric(urea)
  ) |>
  filter(!is.na(phosphate))

# --- Load World Bank Fossil Fuel Energy Index ---
energy_raw <- read_excel(
  here("data/raw/CMO-Historical-Data-Monthly.xlsx"),
  sheet = "Monthly Indices",
  skip = 5,
  col_names = FALSE
)

energy_index <- energy_raw |>
  select(date = 1, energy_index = 3) |>
  filter(!is.na(date), !is.na(energy_index)) |>
  mutate(
    date         = paste0(substr(date, 1, 4), "-",
                          substr(date, 6, 7), "-01"),
    date         = as.Date(date),
    energy_index = as.numeric(energy_index)
  )

# --- Load FAO Cereal Price Index ---
fao_raw <- read_excel(
  here("data/raw/ffpi-data-2026-07.xlsx"),
  sheet = "Indices_Monthly",
  skip = 2,
  col_names = FALSE
)

fao_cereal <- fao_raw |>
  select(date = 1, fao_cereal = 5) |>
  filter(!is.na(date), !is.na(fao_cereal)) |>
  mutate(
    date       = as.Date(as.numeric(date), origin = "1899-12-30"),
    fao_cereal = as.numeric(fao_cereal)
  ) |>
  filter(!is.na(date))

# --- Join series on date ---
combined <- pink_sheet |>
  inner_join(energy_index, by = "date") |>
  inner_join(fao_cereal,   by = "date") |>
  filter(!is.na(energy_index), !is.na(fao_cereal))

# --- Annual averages ---
combined_annual <- combined |>
  mutate(year = year(date)) |>
  group_by(year) |>
  summarise(
    energy_index = mean(energy_index, na.rm = TRUE),
    phosphate    = mean(phosphate,    na.rm = TRUE),
    urea         = mean(urea,         na.rm = TRUE),
    fao_cereal   = mean(fao_cereal,   na.rm = TRUE)
  ) |>
  mutate(date = as.Date(paste0(year, "-01-01")))

# --- Normalize to January 2000 = 100 ---
base_date <- as.Date("2000-01-01")

base_values <- combined_annual |>
  filter(date == base_date) |>
  select(energy_index, phosphate, urea, fao_cereal)

combined_indexed <- combined_annual |>
  filter(date >= base_date) |>
  mutate(
    energy_index = energy_index / as.numeric(base_values$energy_index) * 100,
    phosphate    = phosphate    / as.numeric(base_values$phosphate)    * 100,
    urea         = urea         / as.numeric(base_values$urea)         * 100,
    fao_cereal   = fao_cereal   / as.numeric(base_values$fao_cereal)   * 100
  )

# --- Composite fertilizer index ---
combined_indexed <- combined_indexed |>
  mutate(fertilizer = (urea + phosphate) / 2) |>
  select(date, energy_index, fertilizer, fao_cereal)

# --- Reshape for plotting ---
combined_long <- combined_indexed |>
  pivot_longer(
    cols      = c(energy_index, fertilizer, fao_cereal),
    names_to  = "series",
    values_to = "index"
  ) |>
  mutate(series = recode(series,
                         "energy_index" = "Energy",
                         "fertilizer"   = "Fertilizer",
                         "fao_cereal"   = "Food Price"
  ))

# --- Shaded event regions ---
regions <- data.frame(
  xmin  = as.Date(c("2007-01-01", "2021-01-01", "2026-01-01")),
  xmax  = as.Date(c("2009-01-01", "2023-01-01", "2027-01-01")),
  label = c("2007-09\nFood & energy\nprice crisis",
            "2021-23\nUkraine\ninvasion",
            "2026-\nIran conflict\nHormuz closure")
)

# --- Plot ---
ggplot(combined_long, aes(x = date, y = index, color = series)) +
  geom_rect(
    data = regions,
    aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 700),
    fill = "gray90", alpha = 0.4,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = regions,
    aes(x = xmin, y = 660, label = label),
    inherit.aes = FALSE,
    size = 2.5, color = "gray30", hjust = -0.05,
    lineheight = 0.9
  ) +
  geom_hline(yintercept = 100, color = "gray40", linewidth = 0.5) +
  geom_line(linewidth = 1.0) +
  scale_color_manual(
    values = c(
      "Energy"     = "#2980b9",
      "Fertilizer" = "#e67e22",
      "Food Price" = "#c0392b"
    ),
    labels = c(
      "Energy"     = "Energy: World Bank Fossil Fuel Index (coal, oil, gas)",
      "Fertilizer" = "Fertilizer: World Bank composite (urea + phosphate)",
      "Food Price" = "Food Price: FAO Cereal Price Index"
    )
  ) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
  scale_x_date(
    limits      = c(as.Date("2000-01-01"), as.Date("2033-01-01")),
    date_breaks = "5 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(limits = c(0, 700)) +
  labs(
    title    = "Energy, Fertilizer, and Food Prices: Global Indices",
    subtitle = "Annual averages, indexed to January 2000 = 100",
    x        = NULL,
    y        = "Index (Jan 2000 = 100)",
    color    = NULL
  ) +
  theme_classic() +
  theme(
    legend.position  = "bottom",
    legend.text      = element_text(size = 8),
    legend.key.width = unit(1, "cm"),
    plot.title       = element_text(size = 13, face = "bold"),
    plot.subtitle    = element_text(size = 10, color = "gray40"),
    axis.text        = element_text(size = 9),
    axis.title.y     = element_text(size = 9, color = "gray40"),
    plot.margin      = margin(10, 20, 10, 10)
  )

# --- Save ---
ggsave(
  here("01-energy-fertilizer-food-visualization/energy_fertilizer_food.png"),
  width = 12, height = 6, dpi = 150
)