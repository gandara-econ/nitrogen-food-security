# =============================================================
# 2026 Iran Conflict: Agricultural and Food Security Consequences
# A Physical Supply Balance Approach
# Phase 1: Energy, Fertilizer, and Food Price Visualization
#
# Series:
#   - World Bank Natural Gas Index (composite global benchmark)
#   - World Bank Urea Price ($/MT)
#   - World Bank Phosphate Price ($/MT)
#   - FAO Food Price Index (composite, 2014-2016 = 100)
#
# All series indexed to January 2020 = 100 for comparability
#
# Data sources:
#   World Bank Pink Sheet: CMO-Historical-Data-Monthly.xlsx
#   FAO Food Price Index:  ffpi-data-2026-07.xlsx
#
# Author: Erik Gandara
# Date: 2026-07-13
# =============================================================

library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(here)

