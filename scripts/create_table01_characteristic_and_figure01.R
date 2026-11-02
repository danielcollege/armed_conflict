# the table (final data) has 17 columns, 3720 rows
# library(flextable)
# data = read.csv("data/processed/final_data.csv")
#preview_table = flextable(head(data, 10))


library(dplyr)
library(gtsummary)
library(gt)

# Assignment 1
dat = read.csv("data/processed/final_data.csv",
              na.strings = c("", "NA"))

dim(dat)
names(dat)

range(dat$year)
length(unique(dat$iso))
table(dat$armed_conflict_lag1, useNA = "ifany")
colSums(is.na(dat))

dat <- dat |>
  mutate(
    armed_conflict_lag1 = factor(
      armed_conflict_lag1,
      levels = c(0, 1),
      labels = c(
        "No conflict in previous year",
        "Conflict in previous year"
      )
    ),
    across(
      c(earthquake, drought, oecd),
      ~ factor(.x, levels = c(0, 1), labels = c("No", "Yes"))
    )
  )

table_vars <- c(
  "maternal_mortality",
  "infant_mortality",
  "neonatal_mortality",
  "under5_mortality",
  "gdp_1000",
  "oecd",
  "pop_dens",
  "urban",
  "age_dep",
  "male_edu",
  "earthquake",
  "drought",
  "temp",
  "rainfall_1000"
)

tab1 <- dat |>
  tbl_summary(
    by = armed_conflict_lag1,
    include = all_of(table_vars),

    label = list(
      maternal_mortality ~ "Maternal mortality",
      infant_mortality ~ "Infant mortality",
      neonatal_mortality ~ "Neonatal mortality",
      under5_mortality ~ "Under-five mortality",
      gdp_1000 ~ "GDP",
      oecd ~ "OECD membership",
      pop_dens ~ "Population density",
      urban ~ "Urban population",
      age_dep ~ "Age dependency ratio",
      male_edu ~ "Male education",
      earthquake ~ "Earthquake",
      drought ~ "Drought",
      temp ~ "Temperature",
      rainfall_1000 ~ "Rainfall"
    ),

    type = list(
      c(earthquake, drought, oecd) ~ "categorical"
    ),

    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),

    digits = all_continuous() ~ 1,

    missing = "always",
    missing_text = "Missing, n",
    missing_stat = "{N_miss}"
  ) |>
  add_overall() |>
  modify_header(label = "**Characteristic**") |>
  modify_caption(
    "**Table 1. Country-year characteristics by previous-year armed conflict**"
  ) |>
  bold_labels()

tab1

tab1 |>
  as_gt() |>
  gtsave(filename = "table01_characteristics_by_armed_conflict.pdf")

getwd()


# Assignment 2

library(dplyr)
library(ggplot2)

dat <- read.csv(
  "data/processed/final_data.csv",
  na.strings = c("", "NA")
)

endpoint_comparison <- dat |>
  filter(year %in% c(2000, 2017)) |>
  group_by(iso) |>
  summarise(
    mortality_2000 = maternal_mortality[year == 2000],
    mortality_2017 = maternal_mortality[year == 2017],
    .groups = "drop"
  ) |>
  mutate(
    change = mortality_2017 - mortality_2000
  )

endpoint_comparison

increasing_countries <- endpoint_comparison |>
  filter(
    !is.na(mortality_2000),
    !is.na(mortality_2017),
    change > 0
  )

increasing_countries
nrow(increasing_countries)

plot_data <- dat |>
  semi_join(increasing_countries, by = "iso") |>
  filter(between(year, 2000, 2017)) |>
  arrange(iso, year)

fig1 <- ggplot(
  plot_data,
  aes(
    x = year,
    y = maternal_mortality,
    color = iso,
    group = iso
  )
) +
  geom_line(linewidth = 0.9) +
  scale_color_viridis_d(option = "turbo", end = 0.9) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2017)
  ) +
  labs(
    title = "Maternal mortality trends, 2000–2017",
    subtitle = "Countries with higher maternal mortality in 2017 than in 2000",
    x = "Year",
    y = "Maternal mortality",
    color = "Country (ISO3)",
    caption = paste(
      "Countries were selected by comparing observed values in 2000 and 2017.",
      "Maternal mortality data are unavailable for 2018–2019 in this dataset.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    plot.caption = element_text(hjust = 0)
  )

fig1

dir.create("outputs", showWarnings = FALSE)

ggsave(
  filename = "outputs/fig01_maternal_mortality_trends.png",
  plot = fig1,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white",
  create.dir = TRUE
)
