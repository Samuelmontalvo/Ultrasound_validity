library(readxl)
df <- read_excel("US_SF_.xlsx", sheet = "validation")
View(df)
attach(df)

library(dplyr)
df <-df %>%
  mutate(Sex = ifelse(Sex == 1, "Male", "Female"))

##Validity
library(bmbstats)
bmbstats::plot_pair_OLP(
  predictor = df$Triceps_C,
  outcome = df$Triceps_US1,
  predictor_label = "Skinfold",
  outcome_label = "Ultrasound",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)

library(ggplot2)
ggsave("ForcevsOptojump.png")

olp_method <- function(data=df,
                       criterion=Triceps_C,
                       practical=Triceps_US1,
                       SESOI_lower = 0,
                       SESOI_upper = 0,
                       na.rm = FALSE) {
  practical_obs <- data[[practical]]
  criterion_obs <- data[[criterion]]
  SESOI_range <- SESOI_upper - SESOI_lower

  olp_model <- bmbstats::OLP_regression(
    outcome = criterion_obs,
    predictor = practical_obs,
    na.rm = na.rm)

  n_obs <- length(criterion_obs)

  intercept <- olp_model$intercept
  slope <- olp_model$slope
  rse <- olp_model$rse

  PPER <- stats::pt((SESOI_upper) / rse, df = n_obs - 1) -
    stats::pt((SESOI_lower) / rse, df = n_obs - 1)

  c(
    "Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER
  )
}


olp_method(
  data = df,
  criterion = "Triceps_C",
  practical = "Triceps_US1",
  SESOI_lower = -0.25,
  SESOI_upper = 0.25)

olp_validity <- bmbstats::validity_analysis(
  data = df,
  criterion = "Triceps_C",
  practical = "Triceps_US1",
  SESOI_lower = -0.25,
  SESOI_upper = .25,
  estimator_function = olp_method,
  control = model_control(seed = 1667))
olp_validity


library(psych)
icc_triceps <-
ICC(Triceps_C,Triceps_US1)
