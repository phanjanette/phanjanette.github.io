# Packages
library(tidycensus)
library(tigris)
library(sf)
library(dplyr)
library(ggplot2)
library(readr)

options(tigris_use_cache = TRUE)

# 1) API key (uncomment and paste your key)
census_api_key("991adc56800ae3c63abc4d1f74bc78711191233d", install = FALSE)

# 2) Explore variables
vars <- load_variables(2023, "acs5", cache = TRUE)
# View a few example codes
vars |> dplyr::filter(grepl("^B19", name)) |> dplyr::slice_head(n = 10)

# 3) Parameters (EDIT ME)
state_abbr <- "TX"
geo_level  <- "county"   # options: state, county, tract, block group
my_vars    <- c(income = "B19013_001", poverty = "B17001_002")
year_acs   <- 2022
survey     <- "acs5"

# 4) Download
acs <- get_acs(
  geography = geo_level,
  variables = my_vars,
  state = state_abbr,
  year = year_acs,
  survey = survey,
  geometry = TRUE
)

# 5) Wide format for convenience
acs_wide <- acs |>
  tidyr::pivot_wider(
    id_cols = c(GEOID, NAME, geometry),
    names_from = variable,
    values_from = c(estimate, moe)
  )

# 6) Map (edit titles/theme)
ggplot(acs_wide) +
  geom_sf(aes(fill = estimate_income), color = NA) +
  scale_fill_viridis_c(name = "Median HH Income") +
  labs(title = paste0("ACS ", year_acs, " 5-year: Median Income — ", state_abbr, " (", geo_level, ")"),
       caption = "Source: U.S. Census Bureau via tidycensus") + #important to include source in bottom right corner
  theme_minimal()

# 7) Table (top/bottom by poverty count)
top10 <- acs_wide |>
  arrange(desc(estimate_poverty)) |>
  select(NAME, estimate_poverty, moe_poverty) |>
  slice_head(n = 10)

bottom10 <- acs_wide |>
  arrange(estimate_poverty) |>
  select(NAME, estimate_poverty, moe_poverty) |>
  slice_head(n = 10)

top10
bottom10

# 8) Save outputs (optional)
readr::write_csv(st_drop_geometry(acs_wide), "acs_data.csv")
