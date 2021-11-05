library(readxl)
df <- read_excel("~/R/Ultrasound_validity/US_SF.xlsx",
                    sheet = "validation")
View(df)
attach(df)

## Required libraries
library(dplyr)
library(BlandAltmanLeh)
library(ggplot2)
library(bmbstats)
library(irr)

## recode data
df <-df %>%
  mutate(Sex = ifelse(Sex == 1, "Male", "Female"))

## filter by gender
Male <- df %>% filter(Sex == "Male")
Female <- df %>% filter(Sex == "Female")

####################TRICEPS 1####################################

## BLAND ALTMAN basic
BA_triceps_1 <- bland.altman.plot(Triceps_US1, Triceps_C,
                                  graph.sys="ggplot2") + theme_classic()
BA_triceps_1
ggsave("Triceps_BA_Basic_1.png")

## Bland atlman difference + Linear Regression
BA_LR_triceps_1 <- plot_pair_BA(
  predictor = df$Triceps_C,
  outcome = df$Triceps_US1,
  predictor_label = "True Score",
  outcome_label = "Criterion Score",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
BA_LR_triceps_1
ggsave("Triceps_BA_LR_1.png")

## OLP Validity

#Visual Plot
OLP_plot_triceps_1 <- plot_pair_OLP(
  predictor = df$Triceps_C,
  outcome = df$Triceps_US1,
  predictor_label = "Skinfold Triceps",
  outcome_label = "Ultrasound Triceps",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
OLP_plot_triceps_1
ggsave("US_SF_Triceps.png")

olp_method_triceps_1 <- function(data=df,
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

  c("Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER,
    SDC = rse * 1.96)
}


olp_validity_triceps_1 <- validity_analysis(
  data = df,
  criterion = "Triceps_US1",
  practical = "Triceps_C",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5,
  estimator_function = olp_method_triceps_1,
  control = model_control(boot_samples = 1000,seed = 1667))
olp_validity_triceps_1


## Intra class correlation
icc_triceps_1 <- df %>% select(c("Triceps_C","Triceps_US1"))
icc(icc_triceps_1, model = "twoway", type = "agreement", unit = "average")


###############TRICEPS 2 ########################

## BLAND ALTMAN basic
BA_Triceps_2 <- bland.altman.plot(Triceps_US2, Triceps_C,
                                  graph.sys="ggplot2") + theme_classic()
BA_Triceps_2
ggsave("Triceps_BA_Basic_2.png")

## Bland atlman difference + Linear Regression
BA_LR_triceps_2 <- plot_pair_BA(
  predictor = df$Triceps_C,
  outcome = df$Triceps_US2,
  predictor_label = "True Score",
  outcome_label = "Criterion Score",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
BA_LR_triceps_2
ggsave("Triceps_BA_LR_2.png")

## OLP Validity

#Visual Plot
OLP_plot_triceps_2 <- plot_pair_OLP(
  predictor = df$Triceps_C,
  outcome = df$Triceps_US2,
  predictor_label = "Skinfold Triceps 2",
  outcome_label = "Ultrasound Triceps",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
OLP_plot_triceps_2
ggsave("US_SF_Triceps2.png")

olp_method_triceps_2 <- function(data=df,
                                 criterion=Triceps_C,
                                 practical=Triceps_US2,
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

  c("Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER,
    SDC = rse * 1.96)
}


olp_validity_triceps_2 <- validity_analysis(
  data = df,
  criterion = "Triceps_US2",
  practical = "Triceps_C",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5,
  estimator_function = olp_method_triceps_2,
  control = model_control(boot_samples = 1000,seed = 1667))
olp_validity_triceps_2


## Intra class correlation
icc_triceps_2 <- df %>% select(c("Triceps_C","Triceps_US2"))
icc(icc_triceps_2, model = "twoway", type = "agreement", unit = "average")



###############Subscapular 1 ########################

## BLAND ALTMAN basic
BA_subscapular_1 <- bland.altman.plot(Subscapular_US1, Subscapular_C,
                                  graph.sys="ggplot2") + theme_classic()
BA_subscapular_1
ggsave("BA_subscapular_1.png")

## Bland atlman difference + Linear Regression
BA_LR_subscapular_1 <- plot_pair_BA(
  predictor = df$Subscapular_C,
  outcome = df$Subscapular_US1,
  predictor_label = "True Score",
  outcome_label = "Criterion Score",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
BA_LR_subscapular_1
ggsave("Subscapular_BA_LR_1.png")

## OLP Validity

#Visual Plot
OLP_plot_Subscapular_1 <- plot_pair_OLP(
  predictor = df$Subscapular_C,
  outcome = df$Subscapular_US1,
  predictor_label = "Skinfold Subscapular 1",
  outcome_label = "Ultrasound Subscapular",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
OLP_plot_Subscapular_1
ggsave("US_SF_Subscapular.png")

OLP_Subscapular_1 <- function(data=df,
                                 criterion=Subscapular_C,
                                 practical=Subscapular_US1,
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

  c("Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER,
    SDC = rse * 1.96)
}


olp_validity_subscapular_1 <- validity_analysis(
  data = df,
  criterion = "Subscapular_US1",
  practical = "Subscapular_C",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5,
  estimator_function = OLP_Subscapular_1,
  control = model_control(boot_samples = 1000,seed = 1667))
olp_validity_subscapular_1


## Intra class correlation
icc_subscapular_1 <- df %>% select(c("Subscapular_C","Subscapular_US1"))
icc(icc_subscapular_1, model = "twoway", type = "agreement", unit = "average")


###############Subscapular 2 ########################

## BLAND ALTMAN basic
BA_subscapular_2 <- bland.altman.plot(Subscapular_US2, Subscapular_C,
                                      graph.sys="ggplot2") + theme_classic()
BA_subscapular_2
ggsave("BA_subscapular_2.png")

## Bland atlman difference + Linear Regression
BA_LR_subscapular_1 <- plot_pair_BA(
  predictor = df$Subscapular_C,
  outcome = df$Subscapular_US2,
  predictor_label = "True Score",
  outcome_label = "Criterion Score",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
BA_LR_subscapular_1
ggsave("Subscapular_BA_LR_2.png")

## OLP Validity

#Visual Plot
OLP_plot_Subscapular_2 <- plot_pair_OLP(
  predictor = df$Subscapular_C,
  outcome = df$Subscapular_US1,
  predictor_label = "Skinfold Subscapular 2",
  outcome_label = "Ultrasound Subscapular",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
OLP_plot_Subscapular_2
ggsave("US_SF_Subscapular2.png")

OLP_Subscapular_2 <- function(data=df,
                              criterion=Subscapular_C,
                              practical=Subscapular_US2,
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

  c("Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER,
    SDC = rse * 1.96)
}


olp_validity_subscapular_2 <- validity_analysis(
  data = df,
  criterion = "Subscapular_US2",
  practical = "Subscapular_C",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5,
  estimator_function = OLP_Subscapular_2,
  control = model_control(boot_samples = 1000,seed = 1667))
olp_validity_subscapular_2


## Intra class correlation
icc_subscapular_2 <- df %>% select(c("Subscapular_C","Subscapular_US2"))
icc(icc_subscapular_2, model = "twoway", type = "agreement", unit = "average")





###############Bicep 1 ########################

## BLAND ALTMAN basic
BA_bicep_1 <- bland.altman.plot(Bicep_US1, Bicep_C,
                                      graph.sys="ggplot2") + theme_classic()
BA_bicep_1
ggsave("BA_bicep_1.png")

## Bland atlman difference + Linear Regression
BA_LR_bicep_1 <- plot_pair_BA(
  predictor = df$Bicep_C,
  outcome = df$Bicep_US1,
  predictor_label = "True Score",
  outcome_label = "Criterion Score",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
BA_LR_bicep_1
ggsave("Bicep_BA_LR_1.png")

## OLP Validity

#Visual Plot
OLP_plot_bicep_1 <- plot_pair_OLP(
  predictor = df$Bicep_C,
  outcome = df$Bicep_US1,
  predictor_label = "Skinfold Bicep",
  outcome_label = "Ultrasound Bicep",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
OLP_plot_Subscapular_1
ggsave("US_SF_bicep.png")

OLP_bicep_1 <- function(data=df,
                              criterion=Bicep_C,
                              practical=Bicep_US1,
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

  c("Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER,
    SDC = rse * 1.96)
}


olp_validity_bicep_1 <- validity_analysis(
  data = df,
  criterion = "Bicep_US1",
  practical = "Bicep_C",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5,
  estimator_function = OLP_bicep_1,
  control = model_control(boot_samples = 1000,seed = 1667))
olp_validity_bicep_1


## Intra class correlation
icc_bicep_1 <- df %>% select(c("Bicep_C","Bicep_US1"))
icc(icc_bicep_1, model = "twoway", type = "agreement", unit = "average")


#####################BICEP 2 ###############################
## BLAND ALTMAN basic
BA_bicep_2 <- bland.altman.plot(Bicep_US2, Bicep_C,
                                graph.sys="ggplot2") + theme_classic()
BA_bicep_2
ggsave("BA_bicep_2.png")

## Bland atlman difference + Linear Regression
BA_LR_bicep_2 <- plot_pair_BA(
  predictor = df$Bicep_C,
  outcome = df$Bicep_US2,
  predictor_label = "True Score",
  outcome_label = "Criterion Score",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
BA_LR_bicep_2
ggsave("Bicep_BA_LR_2.png")

## OLP Validity

#Visual Plot
OLP_plot_bicep_2 <- plot_pair_OLP(
  predictor = df$Bicep_C,
  outcome = df$Bicep_US2,
  predictor_label = "Skinfold Bicep 2",
  outcome_label = "Ultrasound Bicep",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
OLP_plot_bicep_2
ggsave("US_SF_bicep_2.png")

OLP_bicep_2 <- function(data=df,
                        criterion=Bicep_C,
                        practical=Bicep_US2,
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

  c("Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER,
    SDC = rse * 1.96)
}


olp_validity_bicep_2 <- validity_analysis(
  data = df,
  criterion = "Bicep_US1",
  practical = "Bicep_C",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5,
  estimator_function = OLP_bicep_2,
  control = model_control(boot_samples = 1000,seed = 1667))
olp_validity_bicep_2


## Intra class correlation
icc_bicep_2 <- df %>% select(c("Bicep_C","Bicep_US2"))
icc(icc_bicep_2, model = "twoway", type = "agreement", unit = "average")


###############iliac crest 1 ########################

## BLAND ALTMAN basic
BA_iliac_1 <- bland.altman.plot(iliac_Crest_US1, iliac_Crest_C,
                                graph.sys="ggplot2") + theme_classic()
BA_iliac_1
ggsave("BA_iliac_1.png")

## Bland atlman difference + Linear Regression
BA_LR_iliac_1 <- plot_pair_BA(
  predictor = df$iliac_Crest_C,
  outcome = df$iliac_Crest_US1,
  predictor_label = "True Score",
  outcome_label = "Criterion Score",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
BA_LR_iliac_1
ggsave("Iliac_BA_LR_1.png")

## OLP Validity

#Visual Plot
OLP_plot_iliac_1 <- plot_pair_OLP(
  predictor = df$iliac_Crest_C,
  outcome = df$iliac_Crest_US1,
  predictor_label = "Skinfold iliac crest",
  outcome_label = "Ultrasound iliac crest",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
OLP_plot_iliac_1
ggsave("US_SF_iliac.png")

OLP_iliac_1 <- function(data=df,
                        criterion=iliac_Crest_C,
                        practical=iliac_Crest_US1,
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

  c("Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER,
    SDC = rse * 1.96)
}


olp_validity_iliac_1 <- validity_analysis(
  data = df,
  criterion = "iliac_Crest_C",
  practical = "iliac_Crest_US1",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5,
  estimator_function = OLP_iliac_1,
  control = model_control(boot_samples = 1000,seed = 1667))
olp_validity_iliac_1


## Intra class correlation
icc_iliac_1 <- df %>% select(c("iliac_Crest_C","iliac_Crest_US1"))
icc(icc_iliac_1, model = "twoway", type = "agreement", unit = "average")


###############iliac crest 2 ########################

## BLAND ALTMAN basic
BA_iliac_2 <- bland.altman.plot(iliac_Crest_US2, iliac_Crest_C,
                                graph.sys="ggplot2") + theme_classic()
BA_iliac_2
ggsave("BA_iliac_2.png")

## Bland atlman difference + Linear Regression
BA_LR_iliac_2 <- plot_pair_BA(
  predictor = df$iliac_Crest_C,
  outcome = df$iliac_Crest_US2,
  predictor_label = "True Score",
  outcome_label = "Criterion Score",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
BA_LR_iliac_2
ggsave("Iliac_BA_LR_2.png")

## OLP Validity

#Visual Plot
OLP_plot_iliac_2 <- plot_pair_OLP(
  predictor = df$iliac_Crest_C,
  outcome = df$iliac_Crest_US2,
  predictor_label = "Skinfold iliac crest 2",
  outcome_label = "Ultrasound iliac crest",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
OLP_plot_iliac_1
ggsave("US_SF_iliac_2.png")

OLP_iliac_2 <- function(data=df,
                        criterion=iliac_Crest_C,
                        practical=iliac_Crest_US2,
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

  c("Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER,
    SDC = rse * 1.96)
}


olp_validity_iliac_2 <- validity_analysis(
  data = df,
  criterion = "iliac_Crest_C",
  practical = "iliac_Crest_US2",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5,
  estimator_function = OLP_iliac_2,
  control = model_control(boot_samples = 1000,seed = 1667))
olp_validity_iliac_2


## Intra class correlation
icc_iliac_2 <- df %>% select(c("iliac_Crest_C","iliac_Crest_US2"))
icc(icc_iliac_2, model = "twoway", type = "agreement", unit = "average")



###############supraspinale 1 ########################

## BLAND ALTMAN basic
BA_supra_1 <- bland.altman.plot(Supraspinale_US1, Supraspinale_C,
                                graph.sys="ggplot2") + theme_classic()
BA_supra_1
ggsave("BA_supra_1.png")

## Bland atlman difference + Linear Regression
BA_LR_supra_1 <- plot_pair_BA(
  predictor = df$Supraspinale_C,
  outcome = df$Supraspinale_US1,
  predictor_label = "True Score",
  outcome_label = "Criterion Score",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
BA_LR_supra_1
ggsave("Supra_BA_LR_1.png")

## OLP Validity

#Visual Plot
OLP_plot_supra_1 <- plot_pair_OLP(
  predictor = df$Supraspinale_C,
  outcome = df$Supraspinale_US1,
  predictor_label = "Skinfold iliac crest",
  outcome_label = "Ultrasound iliac crest",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
OLP_plot_supra_1
ggsave("US_SF_supra.png")

OLP_supra_1 <- function(data=df,
                        criterion=Supraspinale_C,
                        practical=Supraspinale_US1,
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

  c("Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER,
    SDC = rse * 1.96)
}


olp_validity_iliac_1 <- validity_analysis(
  data = df,
  criterion = "Supraspinale_C",
  practical = "Supraspinale_US1",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5,
  estimator_function = OLP_supra_1,
  control = model_control(boot_samples = 1000,seed = 1667))
olp_validity_iliac_1


## Intra class correlation
icc_supra_1 <- df %>% select(c("Supraspinale_C","Supraspinale_US1"))
icc(icc_supra_1, model = "twoway", type = "agreement", unit = "average")

###############supraspinale 2 ########################

## BLAND ALTMAN basic
BA_supra_2 <- bland.altman.plot(Supraspinale_US2, Supraspinale_C,
                                graph.sys="ggplot2") + theme_classic()
BA_supra_2
ggsave("BA_supra_2.png")

## Bland atlman difference + Linear Regression
BA_LR_supra_2 <- plot_pair_BA(
  predictor = df$Supraspinale_C,
  outcome = df$Supraspinale_US2,
  predictor_label = "True Score",
  outcome_label = "Criterion Score",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
BA_LR_supra_2
ggsave("Supra_BA_LR_2.png")

## OLP Validity
#Visual Plot
OLP_plot_supra_2 <- plot_pair_OLP(
  predictor = df$Supraspinale_C,
  outcome = df$Supraspinale_US2,
  predictor_label = "Skinfold iliac crest 2",
  outcome_label = "Ultrasound iliac crest",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
OLP_plot_supra_2
ggsave("US_SF_supra_2.png")

OLP_supra_2 <- function(data=df,
                        criterion=Supraspinale_C,
                        practical=Supraspinale_US2,
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

  c("Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER,
    SDC = rse * 1.96)
}


olp_validity_iliac_2 <- validity_analysis(
  data = df,
  criterion = "Supraspinale_C",
  practical = "Supraspinale_US2",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5,
  estimator_function = OLP_supra_1,
  control = model_control(boot_samples = 1000,seed = 1667))
olp_validity_iliac_2


## Intra class correlation
icc_supra_2 <- df %>% select(c("Supraspinale_C","Supraspinale_US2"))
icc(icc_supra_2, model = "twoway", type = "agreement", unit = "average")


###############Abdominal 1 ########################

## BLAND ALTMAN basic
BA_abdominal_1 <- bland.altman.plot(Abdominal_US1, Abdominal_C,
                                graph.sys="ggplot2") + theme_classic()
BA_abdominal_1
ggsave("BA_abdominal_1.png")

## Bland atlman difference + Linear Regression
BA_LR_abdominal_1 <- plot_pair_BA(
  predictor = df$Abdominal_C,
  outcome = df$Abdominal_US1,
  predictor_label = "True Score",
  outcome_label = "Criterion Score",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
BA_LR_abdominal_1
ggsave("Sabdominal_BA_LR_1.png")

## OLP Validity

#Visual Plot
OLP_plot_abdominal_1 <- plot_pair_OLP(
  predictor = df$Abdominal_C,
  outcome = df$Abdominal_US1,
  predictor_label = "Skinfold Abdominal",
  outcome_label = "Ultrasound Bbdominal",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
OLP_plot_abdominal_1
ggsave("US_SF_abdominal.png")

OLP_abdominal_1 <- function(data=df,
                        criterion=Abdominal_C,
                        practical=Abdominal_US1,
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

  c("Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER,
    SDC = rse * 1.96)
}


olp_validity_abdominal_1 <- validity_analysis(
  data = df,
  criterion = "Abdominal_C",
  practical = "Abdominal_US1",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5,
  estimator_function = OLP_abdominal_1,
  control = model_control(boot_samples = 1000,seed = 1667))
olp_validity_abdominal_1

## Intra class correlation
icc_abdominal_1 <- df %>% select(c("Abdominal_C","Abdominal_US1"))
icc(icc_abdominal_1, model = "twoway", type = "agreement", unit = "average")

###############Abdominal 2 ########################

## BLAND ALTMAN basic
BA_abdominal_2 <- bland.altman.plot(Abdominal_US2, Abdominal_C,
                                    graph.sys="ggplot2") + theme_classic()
BA_abdominal_2
ggsave("BA_abdominal_2.png")

## Bland atlman difference + Linear Regression
BA_LR_abdominal_2 <- plot_pair_BA(
  predictor = df$Abdominal_C,
  outcome = df$Abdominal_US2,
  predictor_label = "True Score",
  outcome_label = "Criterion Score",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
BA_LR_abdominal_2
ggsave("Sabdominal_BA_LR_2.png")

## OLP Validity

#Visual Plot
OLP_plot_abdominal_2 <- plot_pair_OLP(
  predictor = df$Abdominal_C,
  outcome = df$Abdominal_US1,
  predictor_label = "Skinfold Abdominal 2",
  outcome_label = "Ultrasound Bbdominal",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
OLP_plot_abdominal_2
ggsave("US_SF_abdominal_2.png")

OLP_abdominal_2 <- function(data=df,
                            criterion=Abdominal_C,
                            practical=Abdominal_US2,
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

  c("Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER,
    SDC = rse * 1.96)
}


olp_validity_abdominal_2 <- validity_analysis(
  data = df,
  criterion = "Abdominal_C",
  practical = "Abdominal_US2",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5,
  estimator_function = OLP_abdominal_2,
  control = model_control(boot_samples = 1000,seed = 1667))
olp_validity_abdominal_2

## Intra class correlation
icc_abdominal_2 <- df %>% select(c("Abdominal_C","Abdominal_US2"))
icc(icc_abdominal_2, model = "twoway", type = "agreement", unit = "average")


####################Front Thigh 1####################################

## BLAND ALTMAN basic
BA_FT_1 <- bland.altman.plot(Front_Thigh_US1, Front_Thigh_C,
                                  graph.sys="ggplot2") + theme_classic()
BA_FT_1
ggsave("FT_BA_Basic_1.png")

## Bland atlman difference + Linear Regression
BA_LR_FT_1 <- plot_pair_BA(
  predictor = df$Front_Thigh_C,
  outcome = df$Front_Thigh_US1,
  predictor_label = "True Score",
  outcome_label = "Criterion Score",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
BA_LR_FT_1
ggsave("FT_BA_LR_1.png")

## OLP Validity

#Visual Plot
OLP_plot_FT_1 <- plot_pair_OLP(
  predictor = df$Front_Thigh_C,
  outcome = df$Front_Thigh_US1,
  predictor_label = "Skinfold Triceps",
  outcome_label = "Ultrasound Triceps",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
OLP_plot_FT_1
ggsave("US_SF_FT.png")

olp_method_FT_1 <- function(data=df,
                                 criterion=Front_Thigh_C,
                                 practical=Front_Thigh_US1,
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

  c("Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER,
    SDC = rse * 1.96)
}


olp_validity_FT_1 <- validity_analysis(
  data = df,
  criterion = "Front_Thigh_US1",
  practical = "Front_Thigh_C",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5,
  estimator_function = olp_method_FT_1,
  control = model_control(boot_samples = 1000,seed = 1667))
olp_validity_FT_1


## Intra class correlation
icc_FT_1 <- df %>% select(c("Front_Thigh_C","Front_Thigh_US1"))
icc(icc_FT_1, model = "twoway", type = "agreement", unit = "average")

####################Front Thigh 2####################################

## BLAND ALTMAN basic
BA_FT_2 <- bland.altman.plot(Front_Thigh_US2, Front_Thigh_C,
                             graph.sys="ggplot2") + theme_classic()
BA_FT_2
ggsave("FT_BA_Basic_2.png")

## Bland atlman difference + Linear Regression
BA_LR_FT_2 <- plot_pair_BA(
  predictor = df$Front_Thigh_C,
  outcome = df$Front_Thigh_US2,
  predictor_label = "True Score",
  outcome_label = "Criterion Score",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
BA_LR_FT_2
ggsave("FT_BA_LR_2.png")

## OLP Validity

#Visual Plot
OLP_plot_FT_2 <- plot_pair_OLP(
  predictor = df$Front_Thigh_C,
  outcome = df$Front_Thigh_US2,
  predictor_label = "Skinfold Triceps",
  outcome_label = "Ultrasound Triceps",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
OLP_plot_FT_2
ggsave("US_SF_FT_2.png")

olp_method_FT_2 <- function(data=df,
                            criterion=Front_Thigh_C,
                            practical=Front_Thigh_US2,
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

  c("Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER,
    SDC = rse * 1.96)
}


olp_validity_FT_2 <- validity_analysis(
  data = df,
  criterion = "Front_Thigh_US1",
  practical = "Front_Thigh_C",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5,
  estimator_function = olp_method_FT_2,
  control = model_control(boot_samples = 1000,seed = 1667))
olp_validity_FT_2


## Intra class correlation
icc_FT_2 <- df %>% select(c("Front_Thigh_C","Front_Thigh_US2"))
icc(icc_FT_2, model = "twoway", type = "agreement", unit = "average")

####################Medial Calf 1####################################

## BLAND ALTMAN basic
BA_calf_1 <- bland.altman.plot(Medial_Calf_US1, Medial_Calf_C,
                             graph.sys="ggplot2") + theme_classic()
BA_calf_1
ggsave("Calf_BA_Basic_1.png")

## Bland atlman difference + Linear Regression
BA_LR_calf_1 <- plot_pair_BA(
  predictor = df$Medial_Calf_C,
  outcome = df$Medial_Calf_US1,
  predictor_label = "True Score",
  outcome_label = "Criterion Score",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
BA_LR_calf_1
ggsave("Calf_BA_LR_1.png")

## OLP Validity

#Visual Plot
OLP_plot_calf_1 <- plot_pair_OLP(
  predictor = df$Medial_Calf_C,
  outcome = df$Medial_Calf_US1,
  predictor_label = "Skinfold Calf 1",
  outcome_label = "Ultrasound Calf",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
OLP_plot_calf_1
ggsave("US_SF_calf.png")

olp_method_calf_1 <- function(data=df,
                            criterion=Medial_Calf_C,
                            practical=Medial_Calf_US1,
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

  c("Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER,
    SDC = rse * 1.96)
}


olp_validity_calf_1 <- validity_analysis(
  data = df,
  criterion = "Front_Thigh_US1",
  practical = "Medial_Calf_US1",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5,
  estimator_function = olp_method_calf_1,
  control = model_control(boot_samples = 1000,seed = 1667))
olp_validity_calf_1


## Intra class correlation
icc_calf_1 <- df %>% select(c("Medial_Calf_C","Medial_Calf_US1"))
icc(icc_calf_1, model = "twoway", type = "agreement", unit = "average")


####################Medial Calf 2####################################

## BLAND ALTMAN basic
BA_calf_2 <- bland.altman.plot(Medial_Calf_US2, Medial_Calf_C,
                               graph.sys="ggplot2") + theme_classic()
BA_calf_2
ggsave("Calf_BA_Basic_2.png")

## Bland atlman difference + Linear Regression
BA_LR_calf_2 <- plot_pair_BA(
  predictor = df$Medial_Calf_C,
  outcome = df$Medial_Calf_US2,
  predictor_label = "True Score",
  outcome_label = "Criterion Score",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
BA_LR_calf_2
ggsave("Calf_BA_LR_2.png")

## OLP Validity

#Visual Plot
OLP_plot_calf_2 <- plot_pair_OLP(
  predictor = df$Medial_Calf_C,
  outcome = df$Medial_Calf_US1,
  predictor_label = "Skinfold Calf 2",
  outcome_label = "Ultrasound Calf",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5)
OLP_plot_calf_2
ggsave("US_SF_calf_2.png")

olp_method_calf_2 <- function(data=df,
                              criterion=Medial_Calf_C,
                              practical=Medial_Calf_US2,
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

  c("Intercept" = intercept,
    "Slope" = slope,
    "RSE" = rse,
    PPER = PPER,
    SDC = rse * 1.96)
}


olp_validity_calf_2 <- validity_analysis(
  data = df,
  criterion = "Medial_Calf_C",
  practical = "Medial_Calf_US2",
  SESOI_lower = -2.5,
  SESOI_upper = 2.5,
  estimator_function = olp_method_calf_2,
  control = model_control(boot_samples = 1000,seed = 1667))
olp_validity_calf_2


## Intra class correlation
icc_calf_2 <- df %>% select(c("Medial_Calf_C","Medial_Calf_US2"))
icc(icc_calf_2, model = "twoway", type = "agreement", unit = "average")
