# =========================
# Dallas-area ISDs: ACS 2023 (+ attempt 2024)
# =========================

# ---- Packages ----
library(tidycensus)
library(tigris)
library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)
library(purrr)

options(tigris_use_cache = TRUE)

# ---- API Key (edit if needed) ----
# census_api_key("YOUR_KEY_HERE", install = FALSE)
census_api_key("991adc56800ae3c63abc4d1f74bc78711191233d", install = FALSE)

# ---- Helpers ----
`%||%` <- function(a, b) if (!is.null(a)) a else b  # safe coalesce for NULLs

# ---- Parameters ----
state_abbr <- "TX"
geo_level  <- "school district (unified)"  # ISDs
years_to_try <- c(2023, 2024)              # 2024 will be skipped gracefully if unavailable
survey_for <- function(y) "acs5"           # School districts come from 5-year ACS

# ---- Define ISDs explicitly (with exact Census naming conventions) ----
isd_full_names <- c(
  "Carrollton-Farmers Branch Independent School District, Texas",
  "Cedar Hill Independent School District, Texas",
  "Dallas Independent School District, Texas",
  "Desoto Independent School District, Texas",  # fixed capitalization per Census
  "Duncanville Independent School District, Texas",
  "Garland Independent School District, Texas",
  "Grand Prairie Independent School District, Texas",
  "Highland Park Independent School District (Dallas County), Texas",  # exact name
  "Irving Independent School District, Texas",
  "Lancaster Independent School District, Texas",
  "Mesquite Independent School District, Texas",
  "Richardson Independent School District, Texas",
  "Sunnyvale Independent School District, Texas",
  "Coppell Independent School District, Texas",
  "Aldine Independent School District, Texas",
  "Alief Independent School District, Texas",
  "Arlington Independent School District, Texas",
  "Birdville Independent School District, Texas",
  "Burleson Independent School District, Texas",
  "Carroll Independent School District, Texas",
  "Castleberry Independent School District, Texas",
  "Channelview Independent School District, Texas",
  "Clear Creek Independent School District, Texas",
  "Crosby Independent School District, Texas",
  "Crowley Independent School District, Texas",
  "Cypress-Fairbanks Independent School District, Texas",
  "Deer Park Independent School District, Texas",
  "Eagle Mountain-Saginaw Independent School District, Texas",
  "Everman Independent School District, Texas",
  "Fort Worth Independent School District, Texas",
  "Galena Park Independent School District, Texas",
  "Goose Creek Consolidated Independent School District, Texas",
  "Grapevine-Colleyville Independent School District, Texas",
  "Houston Independent School District, Texas",
  "Huffman Independent School District, Texas",
  "Humble Independent School District, Texas",
  "Hurst-Euless-Bedford Independent School District, Texas",
  "Katy Independent School District, Texas",
  "Keller Independent School District, Texas",
  "Kennedale Independent School District, Texas",
  "Klein Independent School District, Texas",
  "La Porte Independent School District, Texas",
  "Lake Worth Independent School District, Texas",
  "Mansfield Independent School District, Texas",
  "Pasadena Independent School District, Texas",
  "Sheldon Independent School District, Texas",
  "Spring Branch Independent School District, Texas",
  "Spring Independent School District, Texas",
  "Tomball Independent School District, Texas",
  "White Settlement Independent School District, Texas"
)


# Tables to fetch
tbls <- c(
  poverty  = "B17001", # Poverty universe + below poverty
  mobility = "B07003", # Residence 1 year ago
  race     = "B03002", # Hispanic origin by race
  sexage   = "B01001", # Sex by age
  age_med  = "B01002"  # Median age
)

# ---- Fetch one table for one year (safe) ----
get_table_year <- function(tbl, year) {
  surv <- survey_for(year)
  msg_tag <- paste0(tbl, " / ", year, " / ", surv)
  out <- try(
    get_acs(
      geography = geo_level,
      table = tbl,
      state = state_abbr,
      year = year,
      survey = surv,
      geometry = TRUE
    ),
    silent = TRUE
  )
  if (inherits(out, "try-error") || is.null(out) || nrow(out) == 0) {
    message("Skipping (unavailable): ", msg_tag)
    return(NULL)
  }
  out |>
    filter(NAME %in% isd_full_names) |>
    mutate(.year = year, .survey = surv)
}

# ---- Widen helper (option to drop geometry on RHS tables) ----
widen <- function(df, keep_geom = TRUE) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  out <- df |>
    select(GEOID, NAME, geometry, variable, estimate, moe, .year, .survey) |>
    tidyr::pivot_wider(
      id_cols   = c(GEOID, NAME, geometry, .year, .survey),
      names_from = variable,
      values_from = c(estimate, moe)
    )
  if (!keep_geom) out <- sf::st_drop_geometry(out)
  out
}

# ---- Build per-year SF data frame (geometry only on base poverty table) ----
pull_all_tables <- function(year) {
  res <- lapply(tbls, get_table_year, year = year)
  names(res) <- names(tbls)
  if (all(vapply(res, is.null, logical(1)))) return(NULL)
  
  w_poverty  <- widen(res$poverty,  keep_geom = TRUE)   # sf (will carry geometry)
  if (is.null(w_poverty)) return(NULL)                  # poverty is the base
  
  w_mobility <- if (!is.null(res$mobility)) widen(res$mobility, keep_geom = FALSE) else NULL
  w_race     <- if (!is.null(res$race))     widen(res$race,     keep_geom = FALSE) else NULL
  w_sexage   <- if (!is.null(res$sexage))   widen(res$sexage,   keep_geom = FALSE) else NULL
  w_age_med  <- if (!is.null(res$age_med))  widen(res$age_med,  keep_geom = FALSE) else NULL
  
  out <- w_poverty
  # Join by keys; RHS has no geometry, so this is a regular attribute join
  if (!is.null(w_mobility)) out <- left_join(out, w_mobility, by = c("GEOID","NAME",".year",".survey"))
  if (!is.null(w_race))     out <- left_join(out, w_race,     by = c("GEOID","NAME",".year",".survey"))
  if (!is.null(w_sexage))   out <- left_join(out, w_sexage,   by = c("GEOID","NAME",".year",".survey"))
  if (!is.null(w_age_med))  out <- left_join(out, w_age_med,  by = c("GEOID","NAME",".year",".survey"))
  
  out  # sf
}

# ---- Pull all years and stack with st_rbind ----
acs_all_list <- lapply(years_to_try, pull_all_tables)
acs_all_list <- acs_all_list[!vapply(acs_all_list, is.null, logical(1))]

if (length(acs_all_list) == 0) {
  stop("No data returned. 2024 may be unavailable for school-district geography; try year = 2023 only.")
}

acs_all <- do.call(rbind, acs_all_list)

# =========================
# Derived metrics
# =========================

# Poverty
acs_all <- acs_all |>
  mutate(
    poverty_universe = .data[["estimate_B17001_001"]],
    poverty_count    = .data[["estimate_B17001_002"]],
    poverty_rate     = 100 * poverty_count / poverty_universe
  )

# Mobility shares from B07003 via labels (avoid hardcoding positions)
# Use 2023 labels (same structure for 2024 when available)
var_labels <- load_variables(2023, "acs5", cache = TRUE) |>
  select(variable = name, label)

# Re-pull mobility in long form just to construct category totals robustly
mobility_raw <- map_dfr(years_to_try, ~ get_table_year("B07003", .x))  # may include 0 rows (then skipped)
if (!is.null(mobility_raw) && nrow(mobility_raw) > 0) {
  mob_long <- mobility_raw |>
    left_join(var_labels, by = c("variable" = "variable")) |>
    mutate(cat = case_when(
      str_detect(label, "^Total:") ~ "total",
      str_detect(label, "Same house 1 year ago") ~ "same_house",
      str_detect(label, "Moved within same county") ~ "within_county",
      str_detect(label, "Moved from different county within same state") ~ "diff_county_same_state",
      str_detect(label, "Moved from different state") ~ "diff_state",
      str_detect(label, "Moved from abroad") ~ "abroad",
      TRUE ~ NA_character_
    )) |>
    filter(!is.na(cat)) |>
    group_by(GEOID, NAME, geometry, .year, .survey, cat) |>
    summarise(estimate = sum(estimate, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = cat, values_from = estimate)
  
  # Join those mobility buckets back (keys include year/survey)
  # ---- Mobility (robust, no label dependence) ----
  # Table B07003 codes:
  #  _001 = Total (pop 1 year and over)
  #  _002 = Same house 1 year ago
  #  _003 = Moved within same county
  #  _004 = Moved from different county within same state
  #  _005 = Moved from different state
  #  _006 = Moved from abroad
  
  acs_all <- acs_all |>
    mutate(
      mob_total       = .data[["estimate_B07003_001"]],
      same_house      = .data[["estimate_B07003_002"]],
      within_county   = .data[["estimate_B07003_003"]],
      diff_cnty_state = .data[["estimate_B07003_004"]],
      diff_state      = .data[["estimate_B07003_005"]],
      abroad          = .data[["estimate_B07003_006"]],
      # percentages (guard against missing or zero totals)
      pct_within_cnty = ifelse(is.na(mob_total) | mob_total == 0, NA_real_, 100 * within_county   / mob_total),
      pct_diff_cnty   = ifelse(is.na(mob_total) | mob_total == 0, NA_real_, 100 * diff_cnty_state / mob_total),
      pct_diff_state  = ifelse(is.na(mob_total) | mob_total == 0, NA_real_, 100 * diff_state      / mob_total),
      pct_abroad      = ifelse(is.na(mob_total) | mob_total == 0, NA_real_, 100 * abroad          / mob_total)
    )
  
}

# Race / Ethnicity (B03002)
acs_all <- acs_all |>
  mutate(
    pop_total   = .data[["estimate_B03002_001"]],
    nh_white    = .data[["estimate_B03002_003"]],
    nh_black    = .data[["estimate_B03002_004"]],
    nh_aian     = .data[["estimate_B03002_005"]],
    nh_asian    = .data[["estimate_B03002_006"]],
    nh_nhopi    = .data[["estimate_B03002_007"]],
    nh_other    = .data[["estimate_B03002_008"]],
    nh_two_plus = .data[["estimate_B03002_009"]],
    hispanic    = .data[["estimate_B03002_012"]],
    sh_white      = 100 * nh_white    / pop_total,
    sh_black      = 100 * nh_black    / pop_total,
    sh_aian       = 100 * nh_aian     / pop_total,
    sh_asian      = 100 * nh_asian    / pop_total,
    sh_nhopi      = 100 * nh_nhopi    / pop_total,
    sh_other      = 100 * nh_other    / pop_total,
    sh_two_plus   = 100 * nh_two_plus / pop_total,
    sh_hispanic   = 100 * hispanic    / pop_total
  )

# Ethnic heterogeneity (fractionalization: 1 - sum(p_i^2))
frac_index <- function(...) {
  v <- c(...)
  v <- v / sum(v, na.rm = TRUE)
  if (!is.finite(sum(v)) || sum(v) == 0) return(NA_real_)
  1 - sum(v^2)
}
acs_all <- acs_all |>
  rowwise() |>
  mutate(ethnic_heterogeneity = frac_index(nh_white, nh_black, nh_aian, nh_asian, nh_nhopi, nh_other, nh_two_plus, hispanic)) |>
  ungroup()

# Sex totals & median age
acs_all <- acs_all |>
  mutate(
    male_total   = .data[["estimate_B01001_002"]],
    female_total = .data[["estimate_B01001_026"]],
    pct_male     = 100 * male_total / (male_total + female_total),
    median_age   = .data[["estimate_B01002_001"]]
  )

# =========================
# Outputs
# =========================

# Latest year available in the stacked result
latest_year <- max(acs_all$.year, na.rm = TRUE)

# Map: Poverty rate, latest year
ggplot(filter(acs_all, .year == latest_year)) +
  geom_sf(aes(fill = poverty_rate), color = NA) +
  scale_fill_viridis_c(name = "% Below Poverty") +
  labs(
    title   = paste0("ACS ", latest_year, " 5-year: Poverty Rate — Selected Dallas-area ISDs"),
    caption = "Source: U.S. Census Bureau via tidycensus"
  ) +
  theme_minimal()
# Summary table
summary_tbl <- acs_all |>
  st_drop_geometry() |>
  transmute(
    Year = .year,
    ISD = NAME,
    Population = pop_total,
    `Poverty rate %` = round(poverty_rate, 1),
    `Movers within county %` = round(pct_within_cnty, 1),
    `Movers diff county (same state) %` = round(pct_diff_cnty, 1),
    `Movers different state %` = round(pct_diff_state, 1),
    `Movers from abroad %` = round(pct_abroad, 1),
    `Median age` = median_age,
    `% Male` = round(pct_male, 1),
    `% Hispanic` = round(sh_hispanic, 1),
    `% NH White` = round(sh_white, 1),
    `% NH Black` = round(sh_black, 1),
    `% NH Asian` = round(sh_asian, 1),
    `Ethnic heterogeneity (0-1)` = round(ethnic_heterogeneity, 3)
  ) |>
  arrange(Year, desc(`Poverty rate %`))

summary_tbl

# Save flat data (no geometry)
readr::write_csv(
  st_drop_geometry(acs_all),
  "~/Desktop/updated_variables_50.csv"
)
