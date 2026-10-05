library(tidyverse)

# 1. Read the four World Bank datasets
matmor <- read.csv("data/original/maternal_mortality.csv")
infmor <- read.csv("data/original/infant_mortality.csv")
neomor <- read.csv("data/original/neonatal_mortality.csv")
un5mor <- read.csv("data/original/under5_mortality.csv")

# 2. Create a function to change wide data to long data
prepare_wb <- function(data, outcome_name) {
  data |>
    select(iso, X2000:X2019) |>
    pivot_longer(
      cols = X2000:X2019,
      names_to = "year",
      names_prefix = "X",
      values_to = outcome_name
    ) |>
    mutate(year = as.integer(year)) |>
    arrange(iso, year)
}

# 3. Apply the function to each dataset
matmor_long <- prepare_wb(matmor, "maternal_mortality")
infmor_long <- prepare_wb(infmor, "infant_mortality")
neomor_long <- prepare_wb(neomor, "neonatal_mortality")
un5mor_long <- prepare_wb(un5mor, "under5_mortality")

# 4. Combine the four datasets by country and year
wb_data <- list(
  matmor_long,
  infmor_long,
  neomor_long,
  un5mor_long
) |>
  reduce(full_join, by = c("iso", "year")) |>
  arrange(iso, year)

# 5. Inspect the combined data
head(wb_data)
glimpse(wb_data)

# 6. Save the prepared World Bank data
write.csv(
  wb_data,
  "data/processed/wb_mortality.csv",
  row.names = FALSE
)

# 7. Read and clean disaster data
disaster <- read.csv("data/original/disaster.csv") |>
  janitor::clean_names()

# 8. Keep earthquakes and droughts in 2000–2019
# Summarise multiple events into one row per country and year
disaster_events <- disaster |>
  filter(
    between(year, 2000, 2019),
    disaster_type %in% c("Earthquake", "Drought")
  ) |>
  select(year, iso, disaster_type) |>
  group_by(iso, year) |>
  summarise(
    earthquake = as.integer(any(disaster_type == "Earthquake")),
    drought = as.integer(any(disaster_type == "Drought")),
    .groups = "drop"
  )

# 9. Include country-years with no recorded earthquake or drought
disaster_data <- wb_data |>
  distinct(iso, year) |>
  left_join(disaster_events, by = c("iso", "year")) |>
  mutate(
    earthquake = replace_na(earthquake, 0L),
    drought = replace_na(drought, 0L)
  ) |>
  select(year, iso, earthquake, drought) |>
  arrange(iso, year)

# 10. Inspect the result
head(disaster_data)
count(disaster_data, earthquake, drought)

# 11. Read conflict data
conflict <- read.csv("data/original/conflict.csv")

# 12. Sum deaths for each conflict within each country and year
conflict_totals <- conflict |>
  filter(!is.na(conflict_id)) |>
  group_by(iso, year, conflict_id) |>
  summarise(
    deaths = sum(best),
    .groups = "drop"
  )

# 13. Create one binary conflict indicator per country and year
conflict_year <- conflict_totals |>
  group_by(iso, year) |>
  summarise(
    armed_conflict = as.integer(any(deaths >= 25)),
    .groups = "drop"
  )

# 14. Lag conflict by one year
# Conflict in 1999 is matched to mortality in 2000
conflict_lagged <- conflict_year |>
  mutate(year = year + 1L) |>
  rename(armed_conflict_lag1 = armed_conflict)

# 15. Include country-years with no recorded conflict
conflict_data <- wb_data |>
  distinct(iso, year) |>
  left_join(conflict_lagged, by = c("iso", "year")) |>
  mutate(
    armed_conflict_lag1 = replace_na(armed_conflict_lag1, 0L)
  )

# 16. Read the remaining covariates
covariates <- read.csv("data/original/covariates.csv")

# Check that covariates have one row per country and year
stopifnot(!anyDuplicated(covariates[c("iso", "year")]))

# 17. Merge all prepared datasets
final_data <- wb_data |>
  left_join(disaster_data, by = c("iso", "year")) |>
  left_join(conflict_data, by = c("iso", "year")) |>
  left_join(covariates, by = c("iso", "year")) |>
  arrange(iso, year)

# 18. Check the final dataset
stopifnot(
  nrow(final_data) == nrow(wb_data),
  !anyDuplicated(final_data[c("iso", "year")])
)

glimpse(final_data)
count(final_data, armed_conflict_lag1)

# 19. Save the final dataset
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

write.csv(
  final_data,
  "data/processed/final_data.csv",
  row.names = FALSE
)