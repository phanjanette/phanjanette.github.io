# =========================
# DeSoto ISD: ACS 2023 Raw Variables
# =========================

library(tidycensus)
library(tigris)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

options(tigris_use_cache = TRUE)
census_api_key("991adc56800ae3c63abc4d1f74bc78711191233d", install = FALSE)

# ---- Parameters ----
state_abbr <- "TX"
geo_level  <- "school district (unified)"
year_acs   <- 2023
survey     <- "acs5"

# ---- Find DeSoto ISD by GEOID ----
tx_unsd <- tigris::school_districts(state_abbr, type = "unified", year = year_acs)
desoto_row <- tx_unsd |> filter(str_detect(NAME, regex("DeSoto", ignore_case = TRUE)))
if (nrow(desoto_row) == 0) stop("No DeSoto ISD found.")
desoto_geoid <- desoto_row$GEOID[1]

# ---- Helper to pull one table and filter by GEOID ----
get_table <- function(tbl) {
  get_acs(
    geography = geo_level,
    table = tbl,
    state = state_abbr,
    year = year_acs,
    survey = survey,
    geometry = TRUE
  ) |>
    filter(GEOID == desoto_geoid)
}

# ---- Pull tables (include both estimates & MOEs) ----
tables <- list(
  poverty = "B17001",
  mobility = "B07003",
  race = "B03002",
  sexage = "B01001",
  median_age = "B01002"
)

acs_tables <- lapply(tables, get_table)

# ---- Pivot wider, keeping both estimates and MOEs ----
pivot_wide <- function(df, keep_geom = FALSE) {
  out <- df |>
    select(GEOID, NAME, geometry, variable, estimate, moe) |>
    pivot_wider(
      id_cols = c(GEOID, NAME, geometry),
      names_from = variable,
      values_from = c(estimate, moe)
    )
  if (!keep_geom) out <- st_drop_geometry(out)
  out
}

# Join all tables
w1 <- pivot_wide(acs_tables$poverty, keep_geom = TRUE)
w2 <- pivot_wide(acs_tables$mobility)
w3 <- pivot_wide(acs_tables$race)
w4 <- pivot_wide(acs_tables$sexage)
w5 <- pivot_wide(acs_tables$median_age)

desoto_full <- w1 |>
  left_join(w2, by = c("GEOID", "NAME")) |>
  left_join(w3, by = c("GEOID", "NAME")) |>
  left_join(w4, by = c("GEOID", "NAME")) |>
  left_join(w5, by = c("GEOID", "NAME")) |>
  mutate(.year = year_acs)

# ---- Keep only desired variables ----
desired_vars <- c(
  "NAME", ".year",
  "estimate_B17001_001","estimate_B17001_002","estimate_B17001_031",
  "moe_B17001_001","moe_B17001_002","moe_B17001_031",
  "estimate_B07003_001","estimate_B07003_002","estimate_B07003_003",
  "estimate_B07003_004","estimate_B07003_005","estimate_B07003_006",
  "moe_B07003_001","moe_B07003_002","moe_B07003_003",
  "moe_B07003_004","moe_B07003_005","moe_B07003_006",
  "estimate_B03002_001","estimate_B03002_002","estimate_B03002_003",
  "estimate_B03002_004","estimate_B03002_005","estimate_B03002_006",
  "estimate_B03002_007","estimate_B03002_008","estimate_B03002_009",
  "estimate_B03002_012","moe_B03002_001","moe_B03002_002",
  "moe_B03002_003","moe_B03002_004","moe_B03002_005",
  "moe_B03002_006","moe_B03002_007","moe_B03002_008",
  "moe_B03002_009","moe_B03002_012",
  "estimate_B01001_001","estimate_B01001_002","estimate_B01001_026",
  "moe_B01001_001","moe_B01001_002","moe_B01001_026",
  "estimate_B01002_001","estimate_B01002_002","estimate_B01002_003",
  "moe_B01002_001","moe_B01002_002","moe_B01002_003"
)

desoto_final <- desoto_full |> 
  select(any_of(desired_vars))

# ---- Save output ----
output_path <- "~/Desktop/desoto_isd_raw_variables_2023.csv"
readr::write_csv(desoto_final, output_path)
message("✅ Saved raw variable file to: ", output_path)
