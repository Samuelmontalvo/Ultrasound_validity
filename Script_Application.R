library(readxl)
df <- read_excel("~/R/Ultrasound_validity/US_SF.xlsx",
                 sheet = "validation")
View(df)
attach(df)

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

####################TRICEPS ####################################

Triceps_lm = lm(Triceps_C~ Triceps_US2, data = df)
Triceps_summary  <- summary(Triceps_lm)
Triceps_summary

Triceps_reg <- (Triceps_US2*Triceps_summary$coefficients[1,1]) +
  Triceps_summary$coefficients[1,1]

cor(Triceps_C,Triceps_reg)

icc_Triceps_application <- data_frame(Triceps_reg,Triceps_C)
icc(icc_Triceps_application, model = "twoway", type = "agreement", unit = "average")
