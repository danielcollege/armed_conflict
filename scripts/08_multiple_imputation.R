# Run once if needed:
install.packages(c("mice", "lme4", "dplyr", "broom"))

library(mice)
library(dplyr)
library(broom)

finaldata <- read.csv(
  "/Users/danielg/Desktop/2026/CHL5233/armed_conflict/data/processed/final_data.csv"
)

finaldata <- finaldata |>
  mutate(loggdp = log(gdp_1000))

# 1. Convert country codes to numeric IDs
midata <- finaldata |>
  mutate(iso_num = as.numeric(as.factor(iso))) |>
  select(-iso)

# 2. Initialize without performing imputation
mice0 <- mice(
  midata, seed = 100, m = 5,
  maxit = 0, printFlag = FALSE
)

# 3. Specify variables to impute
vars <- c(
  "urban", "male_edu", "temp", "rainfall_1000",
  "loggdp", "pop_dens", "maternal_mortality",
  "infant_mortality", "neonatal_mortality",
  "under5_mortality"
)

meth <- mice0$method
meth[vars] <- "2l.lmer"

# Identify country as the grouping variable
pred <- mice0$predictorMatrix
pred[vars, "iso_num"] <- -2

mice.multi.out <- mice(
  midata,
  seed = 100,
  m = 10,
  maxit = 20,
  method = meth,
  predictorMatrix = pred
)

mice.multi.out$m       # Should be 10
mice.multi.out$iteration # Should be 20
plot(mice.multi.out)   # Inspect convergence

# Fit and pool the four MI model
fit.mi.matmor <- with(mice.multi.out,
  lm(maternal_mortality ~ -1 +
       armed_conflict_lag1 + loggdp + oecd + pop_dens + urban +
       age_dep + male_edu + temp + rainfall_1000 +
       earthquake + drought + factor(iso_num) + factor(year))
)

fit.mi.under5 <- with(mice.multi.out,
  lm(under5_mortality ~ -1 +
       armed_conflict_lag1 + loggdp + oecd + pop_dens + urban +
       age_dep + male_edu + temp + rainfall_1000 +
       earthquake + drought + factor(iso_num) + factor(year))
)

fit.mi.infant <- with(mice.multi.out,
  lm(infant_mortality ~ -1 +
       armed_conflict_lag1 + loggdp + oecd + pop_dens + urban +
       age_dep + male_edu + temp + rainfall_1000 +
       earthquake + drought + factor(iso_num) + factor(year))
)

fit.mi.neonatal <- with(mice.multi.out,
  lm(neonatal_mortality ~ -1 +
       armed_conflict_lag1 + loggdp + oecd + pop_dens + urban +
       age_dep + male_edu + temp + rainfall_1000 +
       earthquake + drought + factor(iso_num) + factor(year))
)

mi_models <- list(
  Maternal = fit.mi.matmor,
  Under5 = fit.mi.under5,
  Infant = fit.mi.infant,
  Neonatal = fit.mi.neonatal
)

mi_results <- lapply(mi_models, function(model) {
  summary(pool(model), conf.int = TRUE)
})

outcomes <- c(
  "maternal_mortality", "under5_mortality",
  "infant_mortality", "neonatal_mortality"
)

mi_models <- list(
  fit.mi.matmor, fit.mi.under5,
  fit.mi.infant, fit.mi.neonatal
)

table_final <- bind_rows(lapply(1:4, function(i) {

  # Complete-case model: same formula as the MI model
  cc <- lm(
    formula(mi_models[[i]]$analyses[[1]]),
    data = mice.multi.out$data,
    na.action = na.omit
  )

  cc_table <- tidy(cc, conf.int = TRUE) |>
    transmute(
      term,
      CC = sprintf("%.2f (%.2f, %.2f)",
                   estimate, conf.low, conf.high)
    )

  mi_table <- summary(pool(mi_models[[i]]), conf.int = TRUE) |>
    transmute(
      term,
      MI = sprintf("%.2f (%.2f, %.2f)",
                   estimate, `2.5 %`, `97.5 %`)
    )

  left_join(cc_table, mi_table, by = "term") |>
    filter(!grepl("factor|Intercept", term)) |>
    mutate(Outcome = outcomes[i], .before = 1)
}))

View(table_final)

dir.create("output", showWarnings = FALSE)
write.csv(table_final, "output/CC_MI_comparison.csv",
          row.names = FALSE)

