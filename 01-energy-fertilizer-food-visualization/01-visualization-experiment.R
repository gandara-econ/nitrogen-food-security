# Visualization experiment — Phase 1
# Source the main script to load all data objects
source(here::here("01-energy-fertilizer-food-visualization/01-energy-fertilizer-food-visualization.R"))

# --- Reindex to 2000 = 100 ---
base_date_2000 <- as.Date("2000-01-01")

base_values_2000 <- combined_annual |>
  filter(date == base_date_2000) |>
  select(energy_index, phosphate, urea, fao_cereal)

combined_indexed_2000 <- combined_annual |>
  filter(date >= base_date_2000) |>
  mutate(
    energy_index = energy_index / as.numeric(base_values_2000$energy_index) * 100,
    phosphate    = phosphate    / as.numeric(base_values_2000$phosphate)    * 100,
    urea         = urea         / as.numeric(base_values_2000$urea)         * 100,
    fao_cereal   = fao_cereal   / as.numeric(base_values_2000$fao_cereal)   * 100
  ) |>
  mutate(fertilizer = (urea + phosphate) / 2) |>
  select(date, energy_index, fertilizer, fao_cereal)

combined_long_2000 <- combined_indexed_2000 |>
  pivot_longer(
    cols      = c(energy_index, fertilizer, fao_cereal),
    names_to  = "series",
    values_to = "index"
  ) |>
  mutate(series = recode(series,
                         "energy_index" = "Fossil Fuel Energy Index",
                         "fertilizer"   = "Fertilizer Price Index",
                         "fao_cereal"   = "FAO Cereal Index"
  ))

# --- Annotations ---
events <- data.frame(
  date  = as.Date(c("2008-01-01", "2022-01-01", "2026-01-01")),
  label = c("Food & energy\nprice crisis",
            "Ukraine\ninvasion",
            "Iran conflict\nHormuz closure"),
  y_pos = c(480, 480, 480),
  hjust = c(-0.1, -0.1, -0.1)
)

install.packages("ggrepel")
library(ggrepel)

# --- Direct labels at end of each series ---
labels_end <- combined_long_2000 |>
  group_by(series) |>
  filter(date == max(date))

# --- Shaded event regions ---
regions <- data.frame(
  xmin  = as.Date(c("2007-01-01", "2021-01-01", "2026-01-01")),
  xmax  = as.Date(c("2009-01-01", "2023-01-01", "2027-01-01")),
  label = c("2007-09\nFood & energy\nprice crisis",
            "2021-23\nUkraine\ninvasion",
            "2026-\nIran conflict\nHormuz closure")
)

# --- Plot ---
ggplot(combined_long_2000, aes(x = date, y = index, color = series)) +
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
  geom_text(
    data = labels_end,
    aes(x = date, y = index, label = series, color = series),
    hjust = -0.1, size = 3, fontface = "bold",
    inherit.aes = FALSE
  ) +
  annotate("text", x = as.Date("2026-01-01"), y = 190,
           label = "Cereal response\nlagged",
           size = 2.8, color = "#c0392b", hjust = -0.1) +
  scale_color_manual(values = c(
    "Fossil Fuel Energy Index" = "#2980b9",
    "Fertilizer Price Index"   = "#e67e22",
    "FAO Cereal Index"         = "#c0392b"
  )) +
  scale_x_date(
    limits      = c(as.Date("2000-01-01"), as.Date("2031-01-01")),
    date_breaks = "5 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(limits = c(0, 700)) +
  labs(
    title   = "Energy, Fertilizer, and Food Prices: Global Indices",
    subtitle = "Fossil fuel energy, composite fertilizer, and FAO cereal index (Jan 2000 = 100)",
    x       = NULL,
    y       = "Index (Jan 2000 = 100)",
    caption = "Sources: World Bank Pink Sheet, FAO Cereal Price Index"
  ) +
  theme_classic() +
  theme(
    legend.position  = "none",
    plot.title       = element_text(size = 13, face = "bold"),
    plot.subtitle    = element_text(size = 10, color = "gray40"),
    plot.caption     = element_text(size = 8, color = "gray50", hjust = 1),
    axis.text        = element_text(size = 9),
    axis.title.y     = element_text(size = 9, color = "gray40"),
    plot.margin      = margin(10, 80, 10, 10)
  )

ggsave(
  here("01-energy-fertilizer-food-visualization/energy_fertilizer_food_experiment.png"),
  width = 12, height = 6, dpi = 150
)