library(readxl)
df <- read_excel("~/R/Ultrasound_validity/US_SF_Data.xlsx",
                 sheet = "validation")
View(df)
attach(df)

## Required libraries
library(dplyr)
library(BlandAltmanLeh)
library(blandr)
library(ggplot2)
library(irr)
library(ggpubr)
library(stats)
library(psych)

## recode data
df <-df %>%
  mutate(Sex = ifelse(Sex == 1, "Male", "Female"))
## filter by gender
Male <- df %>% filter(Sex == "Male")
Female <- df %>% filter(Sex == "Female")

Df_descriptives <- df %>% select("Age", "Height", "Weight",
                                 "BMI", "Sex")
describe(Df_descriptives)
describeBy(Df_descriptives ~ Sex)

##################TRICEPS ##################################

## Pearsons, ICC, BA
# Pearsons
cor.test(df$Triceps_C,df$Triceps_US2,
                          method = "pearson",alternative = "two.sided")
#ICC
ICC_triceps_all <- df %>%  select(c("Triceps_C", "Triceps_US2"))

icc(ICC_triceps_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Triceps_C,df$Triceps_US2, sig.level=0.95)

BA_triceps_all <- blandr.draw(df$Triceps_C,df$Triceps_US2, sig.level = 0.95,
                                   plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Triceps_C,Male$Triceps_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_triceps_male <- Male %>%  select(c("Triceps_C", "Triceps_US2"))

icc(ICC_triceps_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Triceps_C,Male$Triceps_US2, sig.level=0.95)

BA_triceps_males <- blandr.draw(Male$Triceps_C,Male$Triceps_US2, sig.level = 0.95,
                              plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Triceps_C,Female$Triceps_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_triceps_female <- Female %>%  select(c("Triceps_C", "Triceps_US2"))

icc(ICC_triceps_female, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Triceps_C,Female$Triceps_US2, sig.level=0.95)

BA_triceps_females <- blandr.draw(Female$Triceps_C,Female$Triceps_US2, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()

## Triceps Linear Method
#create new data from LM
Triceps_lm = lm(Triceps_C ~ Triceps_US2, data = df)
Triceps_summary  <- summary(Triceps_lm)
Triceps_summary

Triceps_predicted <- Triceps_summary$coefficients[1,1] +
  (df$Triceps_US2*Triceps_summary$coefficients[2,1])

df <- df %>% mutate(Triceps_predicted = Triceps_predicted)

## filter by gender
Male <- df %>% filter(Sex == "Male")
Female <- df %>% filter(Sex == "Female")
# Pearsons
cor.test(df$Triceps_C,df$Triceps_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_tricepsLM_all <- df %>%  select(c("Triceps_C", "Triceps_predicted"))

icc(ICC_tricepsLM_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Triceps_C,df$Triceps_predicted, sig.level=0.95)

BA_tricepsLM_all <- blandr.draw(df$Triceps_C,df$Triceps_predicted, sig.level = 0.95,
                              plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Triceps_C,Male$Triceps_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_tricepsLM_male <- Male %>%  select(c("Triceps_C", "Triceps_predicted"))

icc(ICC_tricepsLM_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Triceps_C,Male$Triceps_predicted, sig.level=0.95)

BA_tricepsLM_males <- blandr.draw(Male$Triceps_C,Male$Triceps_predicted, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Triceps_C,Female$Triceps_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_tricepsLM_females <- Female %>%  select(c("Triceps_C", "Triceps_predicted"))

icc(ICC_tricepsLM_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Triceps_C,Female$Triceps_predicted, sig.level=0.95)

BA_tricepsLM_females <- blandr.draw(Female$Triceps_C,Female$Triceps_predicted, sig.level = 0.95,
                                  plotTitle="",ciDisplay = FALSE) +theme_bw()

## Multiplication Method
# Pearsons
cor.test(df$Triceps_C,df$Triceps_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_triceps2_all <- df %>%  select(c("Triceps_C", "Triceps_US2_2"))

icc(ICC_triceps2_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Triceps_C,df$Triceps_US2_2, sig.level=0.95)

BA_triceps2_all <- blandr.draw(df$Triceps_C,df$Triceps_US2_2, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Triceps_C,Male$Triceps_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_triceps2_male <- Male %>%  select(c("Triceps_C", "Triceps_US2_2"))

icc(ICC_triceps2_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Triceps_C,Male$Triceps_US2_2, sig.level=0.95)

BA_triceps2_males <- blandr.draw(Male$Triceps_C,Male$Triceps_US2_2, sig.level = 0.95,
                                  plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Triceps_C,Female$Triceps_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_triceps2_females <- Female %>%  select(c("Triceps_C", "Triceps_US2_2"))

icc(ICC_triceps2_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Triceps_C,Female$Triceps_US2_2, sig.level=0.95)

BA_triceps2_females <- blandr.draw(Female$Triceps_C,Female$Triceps_US2_2, sig.level = 0.95,
                                    plotTitle="",ciDisplay = FALSE) +theme_bw()

#####################SUBSCAPULAR################################
## Pearsons, ICC, BA
# Pearsons
cor.test(df$Subscapular_C,df$Subscapular_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_subscapular_all <- df %>%  select(c("Subscapular_C", "Subscapular_US2"))

icc(ICC_subscapular_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Subscapular_C,df$Subscapular_US2, sig.level=0.95)

BA_subscapular_all <- blandr.draw(df$Subscapular_C,df$Subscapular_US2, sig.level = 0.95,
                              plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Subscapular_C,Male$Subscapular_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_subscapular_male <- Male %>%  select(c("Subscapular_C", "Subscapular_US2"))

icc(ICC_subscapular_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Subscapular_C,Male$Subscapular_US2, sig.level=0.95)

BA_subscapular_males <- blandr.draw(Male$Subscapular_C,Male$Subscapular_US2, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Subscapular_C,Female$Subscapular_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_subscapular_female <- Female %>%  select(c("Subscapular_C", "Subscapular_US2"))

icc(ICC_subscapular_female, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Subscapular_C,Female$Subscapular_US2, sig.level=0.95)

BA_subscapular_females <- blandr.draw(Female$Subscapular_C,Female$Subscapular_US2, sig.level = 0.95,
                                  plotTitle="",ciDisplay = FALSE) +theme_bw()

## Subscapular Linear Method
#create new data from LM
Subscapular_lm = lm(Subscapular_C ~ Subscapular_US2, data = df)
Subscapular_summary  <- summary(Subscapular_lm)
Subscapular_summary

Subscapular_predicted <- Subscapular_summary$coefficients[1,1] +
  (df$Subscapular_US2*Subscapular_summary$coefficients[2,1])

df <- df %>% mutate(Subscapular_predicted = Subscapular_predicted)

## filter by gender
Male <- df %>% filter(Sex == "Male")
Female <- df %>% filter(Sex == "Female")
# Pearsons
cor.test(df$Subscapular_C,df$Subscapular_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_subscapularLM_all <- df %>%  select(c("Subscapular_C", "Subscapular_predicted"))

icc(ICC_subscapularLM_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Subscapular_C,df$Subscapular_predicted, sig.level=0.95)

BA_subscapularLM_all <- blandr.draw(df$Subscapular_C,df$Subscapular_predicted, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Subscapular_C,Male$Subscapular_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_subscapularLM_male <- Male %>%  select(c("Subscapular_C", "Subscapular_predicted"))

icc(ICC_subscapularLM_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Subscapular_C,Male$Subscapular_predicted, sig.level=0.95)

BA_subscapularLM_males <- blandr.draw(Male$Subscapular_C,Male$Subscapular_predicted, sig.level = 0.95,
                                  plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Subscapular_C,Female$Subscapular_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_subscapularLM_females <- Female %>%  select(c("Subscapular_C", "Subscapular_predicted"))

icc(ICC_subscapularLM_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Subscapular_C,Female$Subscapular_predicted, sig.level=0.95)

BA_subscapularLM_females <- blandr.draw(Female$Subscapular_C,Female$Subscapular_predicted, sig.level = 0.95,
                                        plotTitle="",ciDisplay = FALSE) +theme_bw()

## Multiplication Method
# Pearsons
cor.test(df$Subscapular_C,df$Subscapular_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_subscapular2_all <- df %>%  select(c("Subscapular_C", "Subscapular_US2_2"))

icc(ICC_subscapular2_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Subscapular_C,df$Subscapular_US2_2, sig.level=0.95)

BA_subscapular2_all <- blandr.draw(df$Subscapular_C,df$Subscapular_US2_2, sig.level = 0.95,
                               plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Subscapular_C,Male$Subscapular_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_subscapular2_male <- Male %>%  select(c("Subscapular_C", "Subscapular_US2_2"))

icc(ICC_subscapular2_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Subscapular_C,Male$Subscapular_US2_2, sig.level=0.95)

BA_subscapular2_males <- blandr.draw(Male$Subscapular_C,Male$Subscapular_US2_2, sig.level = 0.95,
                                 plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Subscapular_C,Female$Subscapular_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_subscapular2_females <- Female %>%  select(c("Subscapular_C", "Subscapular_*2"))

icc(ICC_subscapular2_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Subscapular_C,Female$Subscapular_US2_2, sig.level=0.95)

BA_subscapular2_females <- blandr.draw(Female$Subscapular_C,Female$Subscapular_US2_2, sig.level = 0.95,
                                   plotTitle="",ciDisplay = FALSE) +theme_bw()

####################Biceps ################################
## Pearsons, ICC, BA
# Pearsons
cor.test(df$Bicep_C,df$Bicep_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_bicep_all <- df %>%  select(c("Bicep_C", "Bicep_US2"))

icc(ICC_bicep_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Bicep_C,df$Bicep_US2, sig.level=0.95)

BA_bicep_all <- blandr.draw(df$Bicep_C,df$Bicep_US2, sig.level = 0.95,
                            plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Bicep_C,Male$Bicep_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_bicep_male <- Male %>%  select(c("Bicep_C", "Bicep_US2"))

icc(ICC_bicep_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Bicep_C,Male$Bicep_US2, sig.level=0.95)

BA_bicep_males <- blandr.draw(Male$Bicep_C,Male$Bicep_US2, sig.level = 0.95,
                              plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Bicep_C,Female$Bicep_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_bicep_female <- Female %>%  select(c("Bicep_C", "Bicep_US2"))

icc(ICC_bicep_female, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Bicep_C,Female$Bicep_US2, sig.level=0.95)

BA_bicep_females <- blandr.draw(Female$Bicep_C,Female$Bicep_US2, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()

## bicep Linear Method
#create new data from LM
Biceps_lm = lm(Bicep_C ~ Bicep_US2, data = df)
Biceps_summary  <- summary(Biceps_lm)
Biceps_summary

Biceps_predicted <- Biceps_summary$coefficients[1,1] +
  (df$Bicep_US2*Biceps_summary$coefficients[2,1])

df <- df %>% mutate(Biceps_predicted = Biceps_predicted)

## filter by gender
Male <- df %>% filter(Sex == "Male")
Female <- df %>% filter(Sex == "Female")
# Pearsons
cor.test(df$Bicep_C,df$Biceps_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_bicepLM_all <- df %>%  select(c("Bicep_C", "Biceps_predicted"))

icc(ICC_bicepLM_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Bicep_C,df$Biceps_predicted, sig.level=0.95)

BA_bicepLM_all <- blandr.draw(df$Bicep_C,df$Biceps_predicted, sig.level = 0.95,
                              plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Bicep_C,Male$Biceps_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_bicepLM_male <- Male %>%  select(c("Bicep_C", "Biceps_predicted"))

icc(ICC_bicepLM_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Bicep_C,Male$Biceps_predicted, sig.level=0.95)

BA_bicepLM_males <- blandr.draw(Male$Bicep_C,Male$Biceps_predicted, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Bicep_C,Female$Biceps_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_bicepLM_females <- Female %>%  select(c("Bicep_C", "Biceps_predicted"))

icc(ICC_bicepLM_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Bicep_C,Female$Biceps_predicted, sig.level=0.95)

BA_bicepLM_females <- blandr.draw(Female$Bicep_C,Female$Biceps_predicted, sig.level = 0.95,
                                  plotTitle="",ciDisplay = FALSE) +theme_bw()

## Multiplication Method
# Pearsons
cor.test(df$Bicep_C,df$Bicep_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_bicep2_all <- df %>%  select(c("Bicep_C", "Bicep_US2_2"))

icc(ICC_bicep2_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Bicep_C,df$Bicep_US2_2, sig.level=0.95)

BA_bicep2_all <- blandr.draw(df$Bicep_C,df$Bicep_US2_2, sig.level = 0.95,
                             plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Bicep_C,Male$Bicep_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_bicep2_male <- Male %>%  select(c("Bicep_C", "Bicep_US2_2"))

icc(ICC_bicep2_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Bicep_C,Male$Bicep_US2_2, sig.level=0.95)

BA_bicep2_males <- blandr.draw(Male$Bicep_C,Male$Bicep_US2_2, sig.level = 0.95,
                               plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Bicep_C,Female$Bicep_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_bicep2_females <- Female %>%  select(c("Bicep_C", "Bicep_US2_2"))

icc(ICC_bicep2_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Bicep_C,Female$Bicep_US2_2, sig.level=0.95)

BA_bicep2_females <- blandr.draw(Female$Bicep_C,Female$Bicep_US2_2, sig.level = 0.95,
                                 plotTitle="",ciDisplay = FALSE) +theme_bw()

####################iliac ################################
## Pearsons, ICC, BA
# Pearsons
cor.test(df$iliac_Crest_C,df$iliac_Crest_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_iliac_all <- df %>%  select(c("iliac_Crest_C", "iliac_Crest_US2"))

icc(ICC_iliac_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$iliac_Crest_C,df$iliac_Crest_US2, sig.level=0.95)

BA_iliac_all <- blandr.draw(df$iliac_Crest_C,df$iliac_Crest_US2, sig.level = 0.95,
                            plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$iliac_Crest_C,Male$iliac_Crest_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_iliac_male <- Male %>%  select(c("iliac_Crest_C", "iliac_Crest_US2"))

icc(ICC_iliac_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$iliac_Crest_C,Male$iliac_Crest_US2, sig.level=0.95)

BA_iliac_males <- blandr.draw(Male$iliac_Crest_C,Male$iliac_Crest_US2, sig.level = 0.95,
                              plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$iliac_Crest_C,Female$iliac_Crest_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_iliac_female <- Female %>%  select(c("iliac_Crest_C", "iliac_Crest_US2"))

icc(ICC_iliac_female, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$iliac_Crest_C,Female$iliac_Crest_US2, sig.level=0.95)

BA_iliac_females <- blandr.draw(Female$iliac_Crest_C,Female$iliac_Crest_US2, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()

## iliac Linear Method
#create new data from LM
iliac_lm = lm(iliac_Crest_C ~ iliac_Crest_US2, data = df)
iliac_summary  <- summary(iliac_lm)
iliac_summary

iliac_predicted <- iliac_summary$coefficients[1,1] +
  (df$iliac_Crest_US2*iliac_summary$coefficients[2,1])

df <- df %>% mutate(iliac_predicted = iliac_predicted)

## filter by gender
Male <- df %>% filter(Sex == "Male")
Female <- df %>% filter(Sex == "Female")
# Pearsons
cor.test(df$iliac_Crest_C,df$iliac_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_iliacLM_all <- df %>%  select(c("iliac_Crest_C", "iliac_predicted"))

icc(ICC_iliacLM_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$iliac_Crest_C,df$iliac_predicted, sig.level=0.95)

BA_iliacLM_all <- blandr.draw(df$iliac_Crest_C,df$iliac_predicted, sig.level = 0.95,
                              plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$iliac_Crest_C,Male$iliac_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_iliacLM_male <- Male %>%  select(c("iliac_Crest_C", "iliac_predicted"))

icc(ICC_iliacLM_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$iliac_Crest_C,Male$iliac_predicted, sig.level=0.95)

BA_iliacLM_males <- blandr.draw(Male$iliac_Crest_C,Male$iliac_predicted, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$iliac_Crest_C,Female$iliac_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_iliacLM_females <- Female %>%  select(c("iliac_Crest_C", "iliac_predicted"))

icc(ICC_iliacLM_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$iliac_Crest_C,Female$iliac_predicted, sig.level=0.95)

BA_iliacLM_females <- blandr.draw(Female$iliac_Crest_C,Female$iliac_predicted, sig.level = 0.95,
                                  plotTitle="",ciDisplay = FALSE) +theme_bw()

## Multiplication Method
# Pearsons
cor.test(df$iliac_Crest_C,df$iliac_Crest_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_iliac2_all <- df %>%  select(c("iliac_Crest_C", "iliac_Crest_US2_2"))

icc(ICC_iliac2_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$iliac_Crest_C,df$iliac_Crest_US2_2, sig.level=0.95)

BA_iliac2_all <- blandr.draw(df$iliac_Crest_C,df$iliac_Crest_US2_2, sig.level = 0.95,
                             plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$iliac_Crest_C,Male$iliac_Crest_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_iliac2_male <- Male %>%  select(c("iliac_Crest_C", "iliac_Crest_US2_2"))

icc(ICC_iliac2_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$iliac_Crest_C,Male$iliac_Crest_US2_2, sig.level=0.95)

BA_iliac2_males <- blandr.draw(Male$iliac_Crest_C,Male$iliac_Crest_US2_2, sig.level = 0.95,
                               plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$iliac_Crest_C,Female$iliac_Crest_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_iliac2_females <- Female %>%  select(c("iliac_Crest_C", "iliac_Crest_US2_2"))

icc(ICC_iliac2_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$iliac_Crest_C,Female$iliac_Crest_US2_2, sig.level=0.95)

BA_iliac2_females <- blandr.draw(Female$iliac_Crest_C,Female$iliac_Crest_US2_2, sig.level = 0.95,
                                 plotTitle="",ciDisplay = FALSE) +theme_bw()

####################Supraspinale ################################
## Pearsons, ICC, BA
# Pearsons
cor.test(df$Supraspinale_C,df$Supraspinale_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_supra_all <- df %>%  select(c("Supraspinale_C", "Supraspinale_US2"))

icc(ICC_supra_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Supraspinale_C,df$Supraspinale_US2, sig.level=0.95)

BA_supra_all <- blandr.draw(df$Supraspinale_C,df$Supraspinale_US2, sig.level = 0.95,
                            plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Supraspinale_C,Male$Supraspinale_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_supra_male <- Male %>%  select(c("Supraspinale_C", "Supraspinale_US2"))

icc(ICC_supra_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Supraspinale_C,Male$Supraspinale_US2, sig.level=0.95)

BA_supra_males <- blandr.draw(Male$Supraspinale_C,Male$Supraspinale_US2, sig.level = 0.95,
                              plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Supraspinale_C,Female$Supraspinale_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_supra_female <- Female %>%  select(c("Supraspinale_C", "Supraspinale_US2"))

icc(ICC_supra_female, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Supraspinale_C,Female$Supraspinale_US2, sig.level=0.95)

BA_supra_females <- blandr.draw(Female$Supraspinale_C,Female$Supraspinale_US2, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()

## supra Linear Method
#create new data from LM
supra_lm = lm(Supraspinale_C ~ Supraspinale_US2, data = df)
supra_summary  <- summary(supra_lm)
supra_summary

supra_predicted <- supra_summary$coefficients[1,1] +
  (df$Supraspinale_US2*supra_summary$coefficients[2,1])

df <- df %>% mutate(supra_predicted = supra_predicted)

## filter by gender
Male <- df %>% filter(Sex == "Male")
Female <- df %>% filter(Sex == "Female")
# Pearsons
cor.test(df$Supraspinale_C,df$supra_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_supraLM_all <- df %>%  select(c("Supraspinale_C", "supra_predicted"))

icc(ICC_supraLM_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Supraspinale_C,df$supra_predicted, sig.level=0.95)

BA_supraLM_all <- blandr.draw(df$Supraspinale_C,df$supra_predicted, sig.level = 0.95,
                              plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Supraspinale_C,Male$supra_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_supraLM_male <- Male %>%  select(c("Supraspinale_C", "supra_predicted"))

icc(ICC_supraLM_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Supraspinale_C,Male$supra_predicted, sig.level=0.95)

BA_supraLM_males <- blandr.draw(Male$Supraspinale_C,Male$supra_predicted, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Supraspinale_C,Female$supra_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_supraLM_females <- Female %>%  select(c("Supraspinale_C", "supra_predicted"))

icc(ICC_supraLM_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Supraspinale_C,Female$supra_predicted, sig.level=0.95)

BA_supraLM_females <- blandr.draw(Female$Supraspinale_C,Female$supra_predicted, sig.level = 0.95,
                                  plotTitle="",ciDisplay = FALSE) +theme_bw()

## Multiplication Method
# Pearsons
cor.test(df$Supraspinale_C,df$Supraspinale_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_supra2_all <- df %>%  select(c("Supraspinale_C", "Supraspinale_US2_2"))

icc(ICC_supra2_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Supraspinale_C,df$Supraspinale_US2_2, sig.level=0.95)

BA_supra2_all <- blandr.draw(df$Supraspinale_C,df$Supraspinale_US2_2, sig.level = 0.95,
                             plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Supraspinale_C,Male$Supraspinale_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_supra2_male <- Male %>%  select(c("Supraspinale_C", "Supraspinale_US2_2"))

icc(ICC_supra2_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Supraspinale_C,Male$Supraspinale_US2_2, sig.level=0.95)

BA_supra2_males <- blandr.draw(Male$Supraspinale_C,Male$Supraspinale_US2_2, sig.level = 0.95,
                               plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Supraspinale_C,Female$Supraspinale_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_supra2_females <- Female %>%  select(c("Supraspinale_C", "Supraspinale_US2_2"))

icc(ICC_supra2_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Supraspinale_C,Female$Supraspinale_US2_2, sig.level=0.95)

BA_supra2_females <- blandr.draw(Female$Supraspinale_C,Female$Supraspinale_US2_2, sig.level = 0.95,
                                 plotTitle="",ciDisplay = FALSE) +theme_bw()


####################Abdominal ################################
## Pearsons, ICC, BA
# Pearsons
cor.test(df$Abdominal_C,df$Abdominal_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_abdominal_all <- df %>%  select(c("Abdominal_C", "Abdominal_US2"))

icc(ICC_abdominal_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Abdominal_C,df$Abdominal_US2, sig.level=0.95)

BA_abdominal_all <- blandr.draw(df$Abdominal_C,df$Abdominal_US2, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Abdominal_C,Male$Abdominal_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_abdominal_male <- Male %>%  select(c("Abdominal_C", "Abdominal_US2"))

icc(ICC_abdominal_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Abdominal_C,Male$Abdominal_US2, sig.level=0.95)

BA_abdominal_males <- blandr.draw(Male$Abdominal_C,Male$Abdominal_US2, sig.level = 0.95,
                                  plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Abdominal_C,Female$Abdominal_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_abdominal_female <- Female %>%  select(c("Abdominal_C", "Abdominal_US2"))

icc(ICC_abdominal_female, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Abdominal_C,Female$Abdominal_US2, sig.level=0.95)

BA_abdominal_females <- blandr.draw(Female$Abdominal_C,Female$Abdominal_US2, sig.level = 0.95,
                                    plotTitle="",ciDisplay = FALSE) +theme_bw()

## abdominal Linear Method
#create new data from LM
abdominal_lm = lm(Abdominal_C ~ Abdominal_US2, data = df)
abdominal_summary  <- summary(abdominal_lm)
abdominal_summary

abdominal_predicted <- abdominal_summary$coefficients[1,1] +
  (df$Abdominal_US2*abdominal_summary$coefficients[2,1])

df <- df %>% mutate(abdominal_predicted = abdominal_predicted)

## filter by gender
Male <- df %>% filter(Sex == "Male")
Female <- df %>% filter(Sex == "Female")
# Pearsons
cor.test(df$Abdominal_C,df$abdominal_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_abdominalLM_all <- df %>%  select(c("Abdominal_C", "abdominal_predicted"))

icc(ICC_abdominalLM_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Abdominal_C,df$abdominal_predicted, sig.level=0.95)

BA_abdominalLM_all <- blandr.draw(df$Abdominal_C,df$abdominal_predicted, sig.level = 0.95,
                                  plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Abdominal_C,Male$abdominal_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_abdominalLM_male <- Male %>%  select(c("Abdominal_C", "abdominal_predicted"))

icc(ICC_abdominalLM_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Abdominal_C,Male$abdominal_predicted, sig.level=0.95)

BA_abdominalLM_males <- blandr.draw(Male$Abdominal_C,Male$abdominal_predicted, sig.level = 0.95,
                                    plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Abdominal_C,Female$abdominal_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_abdominalLM_females <- Female %>%  select(c("Abdominal_C", "abdominal_predicted"))

icc(ICC_abdominalLM_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Abdominal_C,Female$abdominal_predicted, sig.level=0.95)

BA_abdominalLM_females <- blandr.draw(Female$Abdominal_C,Female$abdominal_predicted, sig.level = 0.95,
                                      plotTitle="",ciDisplay = FALSE) +theme_bw()

## Multiplication Method
# Pearsons
cor.test(df$Abdominal_C,df$Abdominal_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_abdominal2_all <- df %>%  select(c("Abdominal_C", "Abdominal_US2_2"))

icc(ICC_abdominal2_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Abdominal_C,df$Abdominal_US2_2, sig.level=0.95)

BA_abdominal2_all <- blandr.draw(df$Abdominal_C,df$Abdominal_US2_2, sig.level = 0.95,
                                 plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Abdominal_C,Male$Abdominal_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_abdominal2_male <- Male %>%  select(c("Abdominal_C", "Abdominal_US2_2"))

icc(ICC_abdominal2_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Abdominal_C,Male$Abdominal_US2_2, sig.level=0.95)

BA_abdominal2_males <- blandr.draw(Male$Abdominal_C,Male$Abdominal_US2_2, sig.level = 0.95,
                                   plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Abdominal_C,Female$Abdominal_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_abdominal2_females <- Female %>%  select(c("Abdominal_C", "Abdominal_US2_2"))

icc(ICC_abdominal2_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Abdominal_C,Female$Abdominal_US2_2, sig.level=0.95)

BA_abdominal2_females <- blandr.draw(Female$Abdominal_C,Female$Abdominal_US2_2, sig.level = 0.95,
                                     plotTitle="",ciDisplay = FALSE) +theme_bw()

####################Thight ################################
## Pearsons, ICC, BA
# Pearsons
cor.test(df$Front_Thigh_C,df$Front_Thigh_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_FT_all <- df %>%  select(c("Front_Thigh_C", "Front_Thigh_US2"))

icc(ICC_FT_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Front_Thigh_C,df$Front_Thigh_US2, sig.level=0.95)

BA_FT_all <- blandr.draw(df$Front_Thigh_C,df$Front_Thigh_US2, sig.level = 0.95,
                         plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Front_Thigh_C,Male$Front_Thigh_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_FT_male <- Male %>%  select(c("Front_Thigh_C", "Front_Thigh_US2"))

icc(ICC_FT_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Front_Thigh_C,Male$Front_Thigh_US2, sig.level=0.95)

BA_FT_males <- blandr.draw(Male$Front_Thigh_C,Male$Front_Thigh_US2, sig.level = 0.95,
                           plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Front_Thigh_C,Female$Front_Thigh_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_FT_female <- Female %>%  select(c("Front_Thigh_C", "Front_Thigh_US2"))

icc(ICC_FT_female, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Front_Thigh_C,Female$Front_Thigh_US2, sig.level=0.95)

BA_FT_females <- blandr.draw(Female$Front_Thigh_C,Female$Front_Thigh_US2, sig.level = 0.95,
                             plotTitle="",ciDisplay = FALSE) +theme_bw()

## FT Linear Method
#create new data from LM
FT_lm = lm(Front_Thigh_C ~ Front_Thigh_US2, data = df)
FT_summary  <- summary(FT_lm)
FT_summary

FT_predicted <- FT_summary$coefficients[1,1] +
  (df$Front_Thigh_US2*FT_summary$coefficients[2,1])

df <- df %>% mutate(FT_predicted = FT_predicted)

## filter by gender
Male <- df %>% filter(Sex == "Male")
Female <- df %>% filter(Sex == "Female")
# Pearsons
cor.test(df$Front_Thigh_C,df$FT_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_FTLM_all <- df %>%  select(c("Front_Thigh_C", "FT_predicted"))

icc(ICC_FTLM_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Front_Thigh_C,df$FT_predicted, sig.level=0.95)

BA_FTLM_all <- blandr.draw(df$Front_Thigh_C,df$FT_predicted, sig.level = 0.95,
                           plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Front_Thigh_C,Male$FT_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_FTLM_male <- Male %>%  select(c("Front_Thigh_C", "FT_predicted"))

icc(ICC_FTLM_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Front_Thigh_C,Male$FT_predicted, sig.level=0.95)

BA_FTLM_males <- blandr.draw(Male$Front_Thigh_C,Male$FT_predicted, sig.level = 0.95,
                             plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Front_Thigh_C,Female$FT_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_FTLM_females <- Female %>%  select(c("Front_Thigh_C", "FT_predicted"))

icc(ICC_FTLM_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Front_Thigh_C,Female$FT_predicted, sig.level=0.95)

BA_FTLM_females <- blandr.draw(Female$Front_Thigh_C,Female$FT_predicted, sig.level = 0.95,
                               plotTitle="",ciDisplay = FALSE) +theme_bw()

## Multiplication Method
# Pearsons
cor.test(df$Front_Thigh_C,df$Front_Thigh_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_FT2_all <- df %>%  select(c("Front_Thigh_C", "Front_Thigh_US2_2"))

icc(ICC_FT2_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Front_Thigh_C,df$Front_Thigh_US2_2, sig.level=0.95)

BA_FT2_all <- blandr.draw(df$Front_Thigh_C,df$Front_Thigh_US2_2, sig.level = 0.95,
                          plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Front_Thigh_C,Male$Front_Thigh_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_FT2_male <- Male %>%  select(c("Front_Thigh_C", "Front_Thigh_US2_2"))

icc(ICC_FT2_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Front_Thigh_C,Male$Front_Thigh_US2_2, sig.level=0.95)

BA_FT2_males <- blandr.draw(Male$Front_Thigh_C,Male$Front_Thigh_US2_2, sig.level = 0.95,
                            plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Front_Thigh_C,Female$Front_Thigh_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_FT2_females <- Female %>%  select(c("Front_Thigh_C", "Front_Thigh_US2_2"))

icc(ICC_FT2_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Front_Thigh_C,Female$Front_Thigh_US2_2, sig.level=0.95)

BA_FT2_females <- blandr.draw(Female$Front_Thigh_C,Female$Front_Thigh_US2_2, sig.level = 0.95,
                              plotTitle="",ciDisplay = FALSE) +theme_bw()

####################Calf ################################
## Pearsons, ICC, BA
# Pearsons
cor.test(df$Medial_Calf_C,df$Medial_Calf_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_calf_all <- df %>%  select(c("Medial_Calf_C", "Medial_Calf_US2"))

icc(ICC_calf_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Medial_Calf_C,df$Medial_Calf_US2, sig.level=0.95)

BA_calf_all <- blandr.draw(df$Medial_Calf_C,df$Medial_Calf_US2, sig.level = 0.95,
                           plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Medial_Calf_C,Male$Medial_Calf_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_calf_male <- Male %>%  select(c("Medial_Calf_C", "Medial_Calf_US2"))

icc(ICC_calf_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Medial_Calf_C,Male$Medial_Calf_US2, sig.level=0.95)

BA_calf_males <- blandr.draw(Male$Medial_Calf_C,Male$Medial_Calf_US2, sig.level = 0.95,
                             plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Medial_Calf_C,Female$Medial_Calf_US2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_calf_female <- Female %>%  select(c("Medial_Calf_C", "Medial_Calf_US2"))

icc(ICC_calf_female, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Medial_Calf_C,Female$Medial_Calf_US2, sig.level=0.95)

BA_calf_females <- blandr.draw(Female$Medial_Calf_C,Female$Medial_Calf_US2, sig.level = 0.95,
                               plotTitle="",ciDisplay = FALSE) +theme_bw()

## calf Linear Method
#create new data from LM
calf_lm = lm(Medial_Calf_C ~ Medial_Calf_US2, data = df)
calf_summary  <- summary(calf_lm)
calf_summary

calf_predicted <- calf_summary$coefficients[1,1] +
  (df$Medial_Calf_US2*calf_summary$coefficients[2,1])

df <- df %>% mutate(calf_predicted = calf_predicted)

## filter by gender
Male <- df %>% filter(Sex == "Male")
Female <- df %>% filter(Sex == "Female")
# Pearsons
cor.test(df$Medial_Calf_C,df$calf_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_calfLM_all <- df %>%  select(c("Medial_Calf_C", "calf_predicted"))

icc(ICC_calfLM_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Medial_Calf_C,df$calf_predicted, sig.level=0.95)

BA_calfLM_all <- blandr.draw(df$Medial_Calf_C,df$calf_predicted, sig.level = 0.95,
                             plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Medial_Calf_C,Male$calf_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_calfLM_male <- Male %>%  select(c("Medial_Calf_C", "calf_predicted"))

icc(ICC_calfLM_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Medial_Calf_C,Male$calf_predicted, sig.level=0.95)

BA_calfLM_males <- blandr.draw(Male$Medial_Calf_C,Male$calf_predicted, sig.level = 0.95,
                               plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Medial_Calf_C,Female$calf_predicted,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_calfLM_females <- Female %>%  select(c("Medial_Calf_C", "calf_predicted"))

icc(ICC_calfLM_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Medial_Calf_C,Female$calf_predicted, sig.level=0.95)

BA_calfLM_females <- blandr.draw(Female$Medial_Calf_C,Female$calf_predicted, sig.level = 0.95,
                                 plotTitle="",ciDisplay = FALSE) +theme_bw()

## Multiplication Method
# Pearsons
cor.test(df$Medial_Calf_C,df$Medial_Calf_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_calf2_all <- df %>%  select(c("Medial_Calf_C", "Medial_Calf_US2_2"))

icc(ICC_calf2_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Medial_Calf_C,df$Medial_Calf_US2_2, sig.level=0.95)

BA_calf2_all <- blandr.draw(df$Medial_Calf_C,df$Medial_Calf_US2_2, sig.level = 0.95,
                            plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Medial_Calf_C,Male$Medial_Calf_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_calf2_male <- Male %>%  select(c("Medial_Calf_C", "Medial_Calf_US2_2"))

icc(ICC_calf2_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Medial_Calf_C,Male$Medial_Calf_US2_2, sig.level=0.95)

BA_calf2_males <- blandr.draw(Male$Medial_Calf_C,Male$Medial_Calf_US2_2, sig.level = 0.95,
                              plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Medial_Calf_C,Female$Medial_Calf_US2_2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_calf2_females <- Female %>%  select(c("Medial_Calf_C", "Medial_Calf_US2_2"))

icc(ICC_calf2_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Medial_Calf_C,Female$Medial_Calf_US2_2, sig.level=0.95)

BA_calf2_females <- blandr.draw(Female$Medial_Calf_C,Female$Medial_Calf_US2_2, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()


###################FIGURES ####################
##Raw data
ggarrange(BA_triceps_all,BA_subscapular_all,
          BA_bicep_all,BA_iliac_all,BA_supra_all,BA_abdominal_all,BA_FT_all,
          BA_calf_all, ncol = 2, nrow = 4,
          labels = c("A","B", "C", "D","E", "F", "G", "H"))
ggsave("BA_all.png")

ggarrange(BA_triceps_males,BA_subscapular_males,
          BA_bicep_males,BA_iliac_males,
          BA_supra_males,BA_abdominal_males,BA_FT_males,
          BA_calf_males, ncol = 2, nrow = 4,
          labels = c("A","B", "C", "D","E", "F", "G", "H"))
ggsave("BA_males.png")


ggarrange(BA_triceps_females,BA_subscapular_females,
          BA_bicep_females,BA_iliac_females,
          BA_supra_females,BA_abdominal_females,BA_FT_females,
          BA_calf_females, ncol = 2, nrow = 4,
          labels = c("A","B", "C", "D","E", "F", "G", "H"))
ggsave("BA_females.png")

ggarrange(BA_tricepsLM_all,BA_subscapularLM_all,
          BA_bicepLM_all,BA_iliacLM_all,BA_supraLM_all,BA_abdominalLM_all,
          BA_FTLM_all,BA_calfLM_all, ncol = 2, nrow = 4,
          labels = c("A","B", "C", "D","E", "F", "G", "H"))
ggsave("BA_LM_all.png")

## LM Data
ggarrange(BA_tricepsLM_males,BA_subscapularLM_males,
          BA_bicepLM_males,BA_iliacLM_males,
          BA_supraLM_males,BA_abdominalLM_males,BA_FTLM_males,
          BA_calfLM_males, ncol = 2, nrow = 4,
          labels = c("A","B", "C", "D","E", "F", "G", "H"))
ggsave("BA_LM_males.png")

ggarrange(BA_tricepsLM_females,BA_subscapularLM_females,
          BA_bicepLM_females,BA_iliacLM_females,
          BA_supraLM_females,BA_abdominalLM_females,BA_FTLM_females,
          BA_calfLM_females, ncol = 2, nrow = 4,
          labels = c("A","B", "C", "D","E", "F", "G", "H"))
ggsave("BA_LM_females.png")

## Multiplication data
ggarrange(BA_triceps2_all,BA_subscapular2_all,
          BA_bicep2_all,BA_iliac2_all,BA_supra2_all,BA_abdominal2_all,
          BA_FT2_all,BA_calf2_all, ncol = 2, nrow = 4,
          labels = c("A","B", "C", "D","E", "F", "G", "H"))
ggsave("BA_2_all.png")

ggarrange(BA_triceps2_males,BA_subscapular2_males,
          BA_bicep2_males,BA_iliac2_males,
          BA_supra2_males,BA_abdominal2_males,BA_FT2_males,
          BA_calf2_males, ncol = 2, nrow = 4,
          labels = c("A","B", "C", "D","E", "F", "G", "H"))
ggsave("BA_2_males.png")

ggarrange(BA_triceps2_females,BA_subscapular2_females,
          BA_bicep2_females,BA_iliac2_females,
          BA_supra2_females,BA_abdominal2_females,BA_FT2_females,
          BA_calf2_females, ncol = 2, nrow = 4,
          labels = c("A","B", "C", "D","E", "F", "G", "H"))
ggsave("BA_2_females.png")


############################### DK #####################

library(readxl)
df <- read_excel("~/R/Ultrasound_validity/US_SF_Data.xlsx",
                 sheet = "Regression_Data")
View(df)
attach(df)

## Required libraries
library(dplyr)
library(BlandAltmanLeh)
library(blandr)
library(ggplot2)
library(irr)
library(ggpubr)
library(stats)
## recode data
df <-df %>%
  mutate(Sex = ifelse(Sex == 1, "Male", "Female"))
## filter by gender
Male <- df %>% filter(Sex == "Male")
Female <- df %>% filter(Sex == "Female")

################## adipose
## LM method
#Pearsons
cor.test(df$Adipose_Mass,df$Adipose_Mass_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_adiposeLM_all <- df %>%  select(c("Adipose_Mass", "Adipose_Mass_LM"))

icc(ICC_adiposeLM_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Adipose_Mass,df$Adipose_Mass_LM, sig.level=0.95)

BA_adiposeLM_all <- blandr.draw(df$Adipose_Mass,df$Adipose_Mass_LM, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Adipose_Mass,Male$Adipose_Mass_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_adiposeLM_male <- Male %>%  select(c("Adipose_Mass", "Adipose_Mass_LM"))

icc(ICC_adiposeLM_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Adipose_Mass,Male$Adipose_Mass_LM, sig.level=0.95)

BA_adiposeLM_males <- blandr.draw(Male$Adipose_Mass,Male$Adipose_Mass_LM, sig.level = 0.95,
                                  plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Adipose_Mass,Female$Adipose_Mass_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_adiposeLM_females <- Female %>%  select(c("Adipose_Mass", "Adipose_Mass_LM"))

icc(ICC_adiposeLM_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Adipose_Mass,Female$Adipose_Mass_LM, sig.level=0.95)

BA_adiposeLM_females <- blandr.draw(Female$Adipose_Mass,Female$Adipose_Mass_LM, sig.level = 0.95,
                                    plotTitle="",ciDisplay = FALSE) +theme_bw()

## Multiplication Method
# Pearsons
cor.test(df$Adipose_Mass,df$Adipose_Mass2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_adipose2_all <- df %>%  select(c("Adipose_Mass", "Adipose_Mass2"))

icc(ICC_adipose2_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Adipose_Mass,df$Adipose_Mass2, sig.level=0.95)

BA_adipose2_all <- blandr.draw(df$Adipose_Mass,df$Adipose_Mass2, sig.level = 0.95,
                               plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Adipose_Mass,Male$Adipose_Mass2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_adipose2_male <- Male %>%  select(c("Adipose_Mass", "Adipose_Mass2"))

icc(ICC_adipose2_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Adipose_Mass,Male$Adipose_Mass2, sig.level=0.95)

BA_adipose2_males <- blandr.draw(Male$Adipose_Mass,Male$Adipose_Mass2, sig.level = 0.95,
                                 plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Adipose_Mass,Female$Adipose_Mass2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_adipose2_females <- Female %>%  select(c("Adipose_Mass", "Adipose_Mass2"))

icc(ICC_adipose2_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Adipose_Mass,Female$Adipose_Mass2, sig.level=0.95)

BA_adipose2_females <- blandr.draw(Female$Adipose_Mass,Female$Adipose_Mass2, sig.level = 0.95,
                                   plotTitle="",ciDisplay = FALSE) +theme_bw()

################## adipose %
## LM method
#Pearsons
cor.test(df$Adipose_Mass_Percentage,df$Adipose_Mass_Percentage_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_adipose_percentage_LM_all <- df %>%  select(c("Adipose_Mass_Percentage", "Adipose_Mass_Percentage_LM"))

icc(ICC_adipose_percentage_LM_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Adipose_Mass_Percentage,df$Adipose_Mass_Percentage_LM, sig.level=0.95)

BA_adipose_percentage_LM_all <- blandr.draw(df$Adipose_Mass_Percentage,df$Adipose_Mass_Percentage_LM, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()

## MALES
cor.test(Male$Adipose_Mass_Percentage,Male$Adipose_Mass_Percentage_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_adipose_percentage_male <- Male %>%  select(c("Adipose_Mass_Percentage", "Adipose_Mass_Percentage_LM"))

icc(ICC_adipose_percentage_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Adipose_Mass_Percentage,Male$Adipose_Mass_Percentage_LM, sig.level=0.95)

BA_adipose_percentage_LM_males <- blandr.draw(Male$Adipose_Mass_Percentage,Male$Adipose_Mass_Percentage_LM, sig.level = 0.95,
                                 plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Adipose_Mass_Percentage,Female$Adipose_Mass_Percentage_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_adipose_percentage_females <- Female %>%  select(c("Adipose_Mass_Percentage", "Adipose_Mass_Percentage_LM"))

icc(ICC_adipose_percentage_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Adipose_Mass_Percentage,Female$Adipose_Mass_Percentage_LM, sig.level=0.95)

BA_adipose_percentage_LM_females <- blandr.draw(Female$Adipose_Mass_Percentage,Female$Adipose_Mass_Percentage_LM, sig.level = 0.95,
                                   plotTitle="",ciDisplay = FALSE) +theme_bw()


## multiplication method
#Pearsons
cor.test(df$Adipose_Mass_Percentage,df$Adipose_Mass_Percentage2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_adipose2_percentage_all <- df %>%  select(c("Adipose_Mass_Percentage", "Adipose_Mass_Percentage2"))

icc(ICC_adipose2_percentage_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Adipose_Mass_Percentage,df$Adipose_Mass_Percentage2, sig.level=0.95)

BA_adipose2_percentage_all <- blandr.draw(df$Adipose_Mass_Percentage,df$Adipose_Mass_Percentage2, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()

## MALES
cor.test(Male$Adipose_Mass_Percentage,Male$Adipose_Mass_Percentage2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_adipose2_percentage_male <- Male %>%  select(c("Adipose_Mass_Percentage", "Adipose_Mass_Percentage2"))

icc(ICC_adipose2_percentage_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Adipose_Mass_Percentage,Male$Adipose_Mass_Percentage2, sig.level=0.95)

BA_adipose2_percentage_males <- blandr.draw(Male$Adipose_Mass_Percentage,Male$Adipose_Mass_Percentage2, sig.level = 0.95,
                                           plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Adipose_Mass_Percentage,Female$Adipose_Mass_Percentage2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_adipose2_percentage_females <- Female %>%  select(c("Adipose_Mass_Percentage", "Adipose_Mass_Percentage2"))

icc(ICC_adipose2_percentage_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Adipose_Mass_Percentage,Female$Adipose_Mass_Percentage2, sig.level=0.95)

BA_adipose2_percentage_females <- blandr.draw(Female$Adipose_Mass_Percentage,Female$Adipose_Mass_Percentage2, sig.level = 0.95,
                                             plotTitle="",ciDisplay = FALSE) +theme_bw()

############FIGURES DK

ggarrange(BA_adiposeLM_all, BA_adiposeLM_males, BA_adiposeLM_females,
          BA_adipose2_all, BA_adipose2_males, BA_adipose2_females,
          ncol = 3, nrow = 2, labels = c("A","B", "C", "D","E", "F"))
ggsave("BA_adipose.png")


ggarrange(BA_adipose_percentage_LM_all, BA_adipose_percentage_LM_males,
          BA_adipose_percentage_LM_females,BA_adipose2_percentage_all,
          BA_adipose2_percentage_males, BA_adipose2_percentage_females,
          ncol = 3, nrow = 2, labels = c("A","B", "C", "D","E", "F"))
ggsave("BA_adipose_percentage.png")




################## Muscle Mass
## LM method
#Pearsons
cor.test(df$Muscle_Mass,df$Muscle_Mass_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_muscleLM_all <- df %>%  select(c("Muscle_Mass", "Muscle_Mass_LM"))

icc(ICC_muscleLM_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Muscle_Mass,df$Muscle_Mass_LM, sig.level=0.95)

BA_muscleLM_all <- blandr.draw(df$Muscle_Mass,df$Muscle_Mass_LM, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Muscle_Mass,Male$Muscle_Mass_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_muscleLM_male <- Male %>%  select(c("Muscle_Mass", "Muscle_Mass_LM"))

icc(ICC_muscleLM_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Muscle_Mass,Male$Muscle_Mass_LM, sig.level=0.95)

BA_muscleLM_males <- blandr.draw(Male$Muscle_Mass,Male$Muscle_Mass_LM, sig.level = 0.95,
                                  plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Muscle_Mass,Female$Muscle_Mass_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_muscleLM_females <- Female %>%  select(c("Muscle_Mass", "Muscle_Mass_LM"))

icc(ICC_muscleLM_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Muscle_Mass,Female$Muscle_Mass_LM, sig.level=0.95)

BA_muscleLM_females <- blandr.draw(Female$Muscle_Mass,Female$Muscle_Mass_LM, sig.level = 0.95,
                                    plotTitle="",ciDisplay = FALSE) +theme_bw()

## Multiplication Method
# Pearsons
cor.test(df$Muscle_Mass,df$Muscle_Mass2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_muscle2_all <- df %>%  select(c("Muscle_Mass", "Muscle_Mass2"))

icc(ICC_muscle2_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Muscle_Mass,df$Muscle_Mass2, sig.level=0.95)

BA_muscle2_all <- blandr.draw(df$Muscle_Mass,df$Muscle_Mass2, sig.level = 0.95,
                               plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Muscle_Mass,Male$Muscle_Mass2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_muscle2_male <- Male %>%  select(c("Muscle_Mass", "Muscle_Mass2"))

icc(ICC_muscle2_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Muscle_Mass,Male$Muscle_Mass2, sig.level=0.95)

BA_muscle2_males <- blandr.draw(Male$Muscle_Mass,Male$Muscle_Mass2, sig.level = 0.95,
                                 plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Muscle_Mass,Female$Muscle_Mass2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_muscle2_females <- Female %>%  select(c("Muscle_Mass", "Muscle_Mass2"))

icc(ICC_muscle2_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Muscle_Mass,Female$Muscle_Mass2, sig.level=0.95)

BA_muscle2_females <- blandr.draw(Female$Muscle_Mass,Female$Muscle_Mass2, sig.level = 0.95,
                                   plotTitle="",ciDisplay = FALSE) +theme_bw()

################## muscle %
## LM method
#Pearsons
cor.test(df$Muscle_Mass_Percentage,df$Muscle_Mass_Percentage_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_muscle_percentage_LM_all <- df %>%  select(c("Muscle_Mass_Percentage", "Muscle_Mass_Percentage_LM"))

icc(ICC_muscle_percentage_LM_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Muscle_Mass_Percentage,df$Muscle_Mass_Percentage_LM, sig.level=0.95)

BA_muscle_percentage_LM_all <- blandr.draw(df$Muscle_Mass_Percentage,df$Muscle_Mass_Percentage_LM, sig.level = 0.95,
                                            plotTitle="",ciDisplay = FALSE) +theme_bw()

## MALES
cor.test(Male$Muscle_Mass_Percentage,Male$Muscle_Mass_Percentage_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_muscle_percentage_male <- Male %>%  select(c("Muscle_Mass_Percentage", "Muscle_Mass_Percentage_LM"))

icc(ICC_adipose_percentage_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Muscle_Mass_Percentage,Male$Muscle_Mass_Percentage_LM, sig.level=0.95)

BA_muscle_percentage_LM_males <- blandr.draw(Male$Muscle_Mass_Percentage,Male$Muscle_Mass_Percentage_LM, sig.level = 0.95,
                                              plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Muscle_Mass_Percentage,Female$Muscle_Mass_Percentage_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_muscle_percentage_females <- Female %>%  select(c("Muscle_Mass_Percentage", "Muscle_Mass_Percentage_LM"))

icc(ICC_muscle_percentage_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Muscle_Mass_Percentage,Female$Muscle_Mass_Percentage_LM, sig.level=0.95)

BA_muscle_percentage_LM_females <- blandr.draw(Female$Muscle_Mass_Percentage,Female$Muscle_Mass_Percentage_LM, sig.level = 0.95,
                                                plotTitle="",ciDisplay = FALSE) +theme_bw()


## multiplication method
#Pearsons
cor.test(df$Muscle_Mass_Percentage,df$Muscle_Mass_Percentage2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_muscle2_percentage_all <- df %>%  select(c("Muscle_Mass_Percentage", "Muscle_Mass_Percentage2"))

icc(ICC_muscle2_percentage_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Muscle_Mass_Percentage,df$Muscle_Mass_Percentage2, sig.level=0.95)

BA_muscle2_percentage_all <- blandr.draw(df$Muscle_Mass_Percentage,df$Muscle_Mass_Percentage2, sig.level = 0.95,
                                          plotTitle="",ciDisplay = FALSE) +theme_bw()

## MALES
cor.test(Male$Muscle_Mass_Percentage,Male$Muscle_Mass_Percentage2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_muscle2_percentage_male <- Male %>%  select(c("Muscle_Mass_Percentage", "Muscle_Mass_Percentage2"))

icc(ICC_muscle2_percentage_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Muscle_Mass_Percentage,Male$Muscle_Mass_Percentage2, sig.level=0.95)

BA_muscle2_percentage_males <- blandr.draw(Male$Muscle_Mass_Percentage,Male$Muscle_Mass_Percentage2, sig.level = 0.95,
                                            plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Muscle_Mass_Percentage,Female$Muscle_Mass_Percentage2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_muscle2_percentage_females <- Female %>%  select(c("Muscle_Mass_Percentage", "Muscle_Mass_Percentage2"))

icc(ICC_muscle2_percentage_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Muscle_Mass_Percentage,Female$Muscle_Mass_Percentage2, sig.level=0.95)

BA_muscle2_percentage_females <- blandr.draw(Female$Muscle_Mass_Percentage,Female$Muscle_Mass_Percentage2, sig.level = 0.95,
                                              plotTitle="",ciDisplay = FALSE) +theme_bw()

############FIGURES DK

ggarrange(BA_muscleLM_all, BA_muscleLM_males, BA_muscleLM_females,
          BA_muscle2_all, BA_muscle2_males, BA_muscle2_females,
          ncol = 3, nrow = 2, labels = c("A","B", "C", "D","E", "F"))
ggsave("BA_muscle.png")


ggarrange(BA_muscle_percentage_LM_all, BA_muscle_percentage_LM_males,
          BA_muscle_percentage_LM_females,BA_muscle2_percentage_all,
          BA_muscle2_percentage_males, BA_muscle2_percentage_females,
          ncol = 3, nrow = 2, labels = c("A","B", "C", "D","E", "F"))
ggsave("BA_muscle_percentage.png")


################## Residual Mass
## LM method
#Pearsons
cor.test(df$Residual_Mass,df$Residual_Mass_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_residualLM_all <- df %>%  select(c("Residual_Mass", "Residual_Mass_LM"))

icc(ICC_residualLM_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Residual_Mass,df$Residual_Mass_LM, sig.level=0.95)

BA_residualLM_all <- blandr.draw(df$Residual_Mass,df$Residual_Mass_LM, sig.level = 0.95,
                               plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Residual_Mass,Male$Residual_Mass_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_residualLM_male <- Male %>%  select(c("Residual_Mass", "Residual_Mass_LM"))

icc(ICC_residualLM_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Residual_Mass,Male$Residual_Mass_LM, sig.level=0.95)

BA_residualLM_males <- blandr.draw(Male$Residual_Mass,Male$Residual_Mass_LM, sig.level = 0.95,
                                 plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Residual_Mass,Female$Residual_Mass_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_residualLM_females <- Female %>%  select(c("Residual_Mass", "Residual_Mass_LM"))

icc(ICC_residualLM_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Residual_Mass,Female$Residual_Mass_LM, sig.level=0.95)

BA_residualLM_females <- blandr.draw(Female$Residual_Mass,Female$Residual_Mass_LM, sig.level = 0.95,
                                   plotTitle="",ciDisplay = FALSE) +theme_bw()

## Multiplication Method Residual Mass
# Pearsons
cor.test(df$Residual_Mass,df$Residual_Mass2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_residual2_all <- df %>%  select(c("Residual_Mass", "Residual_Mass2"))

icc(ICC_residual2_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Residual_Mass,df$Residual_Mass2, sig.level=0.95)

BA_residual2_all <- blandr.draw(df$Residual_Mass,df$Residual_Mass2, sig.level = 0.95,
                              plotTitle="",ciDisplay = FALSE) +theme_bw()
## MALES
cor.test(Male$Residual_Mass,Male$Residual_Mass2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_residual2_male <- Male %>%  select(c("Residual_Mass", "Residual_Mass2"))

icc(ICC_residual2_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Residual_Mass,Male$Residual_Mass2, sig.level=0.95)

BA_residual2_males <- blandr.draw(Male$Residual_Mass,Male$Residual_Mass2, sig.level = 0.95,
                                plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Residual_Mass,Female$Residual_Mass2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_residual2_females <- Female %>%  select(c("Residual_Mass", "Residual_Mass2"))

icc(ICC_residual2_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Residual_Mass,Female$Residual_Mass2, sig.level=0.95)

BA_residual2_females <- blandr.draw(Female$Residual_Mass,Female$Residual_Mass2, sig.level = 0.95,
                                  plotTitle="",ciDisplay = FALSE) +theme_bw()

################## Residual Mass %
## LM method
#Pearsons
cor.test(df$Residual_Mass_Percentage,df$Residual_Mass_Percentage_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_residual_percentage_LM_all <- df %>%  select(c("Residual_Mass_Percentage", "Residual_Mass_Percentage_LM"))

icc(ICC_residual_percentage_LM_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Residual_Mass_Percentage,df$Residual_Mass_Percentage_LM, sig.level=0.95)

BA_residual_percentage_LM_all <- blandr.draw(df$Residual_Mass_Percentage,df$Residual_Mass_Percentage_LM, sig.level = 0.95,
                                           plotTitle="",ciDisplay = FALSE) +theme_bw()

## MALES
cor.test(Male$Residual_Mass_Percentage,Male$Residual_Mass_Percentage_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_residual_percentage_male <- Male %>%  select(c("Residual_Mass_Percentage", "Residual_Mass_Percentage_LM"))

icc(ICC_residual_percentage_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Residual_Mass_Percentage,Male$Residual_Mass_Percentage_LM, sig.level=0.95)

BA_residual_percentage_LM_males <- blandr.draw(Male$Residual_Mass_Percentage,Male$Residual_Mass_Percentage_LM, sig.level = 0.95,
                                             plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Residual_Mass_Percentage,Female$Residual_Mass_Percentage_LM,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_residual_percentage_females <- Female %>%  select(c("Residual_Mass_Percentage", "Residual_Mass_Percentage_LM"))

icc(ICC_residual_percentage_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Residual_Mass_Percentage,Female$Residual_Mass_Percentage_LM, sig.level=0.95)

BA_residual_percentage_LM_females <- blandr.draw(Female$Residual_Mass_Percentage,Female$Residual_Mass_Percentage_LM, sig.level = 0.95,
                                               plotTitle="",ciDisplay = FALSE) +theme_bw()


## multiplication method
#Pearsons
cor.test(df$Residual_Mass_Percentage,df$Residual_Mass_Percentage2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_residual2_percentage_all <- df %>%  select(c("Residual_Mass_Percentage", "Residual_Mass_Percentage2"))

icc(ICC_residual2_percentage_all, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(df$Residual_Mass_Percentage,df$Residual_Mass_Percentage2, sig.level=0.95)

BA_residual2_percentage_all <- blandr.draw(df$Residual_Mass_Percentage,df$Residual_Mass_Percentage2, sig.level = 0.95,
                                         plotTitle="",ciDisplay = FALSE) +theme_bw()

## MALES
cor.test(Male$Residual_Mass_Percentage,Male$Residual_Mass_Percentage2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_residual2_percentage_male <- Male %>%  select(c("Residual_Mass_Percentage", "Residual_Mass_Percentage2"))

icc(ICC_residual2_percentage_male, model = "twoway", type = "agreement", unit = "average")
# Bland altman
blandr.statistics(Male$Residual_Mass_Percentage,Male$Residual_Mass_Percentage2, sig.level=0.95)

BA_residual2_percentage_males <- blandr.draw(Male$Residual_Mass_Percentage,Male$Residual_Mass_Percentage2, sig.level = 0.95,
                                           plotTitle="",ciDisplay = FALSE) +theme_bw()

## FEMALES
cor.test(Female$Residual_Mass_Percentage,Female$Residual_Mass_Percentage2,
         method = "pearson",alternative = "two.sided")
#ICC
ICC_residual2_percentage_females <- Female %>%  select(c("Residual_Mass_Percentage", "Residual_Mass_Percentage2"))

icc(ICC_residual2_percentage_females, model = "twoway", type = "agreement", unit = "average")
# Bland Altman
blandr.statistics(Female$Residual_Mass_Percentage,Female$Residual_Mass_Percentage2, sig.level=0.95)

BA_residual2_percentage_females <- blandr.draw(Female$Residual_Mass_Percentage,Female$Residual_Mass_Percentage2, sig.level = 0.95,
                                             plotTitle="",ciDisplay = FALSE) +theme_bw()

############FIGURES DK

ggarrange(BA_residualLM_all, BA_residualLM_males, BA_residualLM_females,
          BA_residual2_all, BA_residual2_males, BA_residual2_females,
          ncol = 3, nrow = 2, labels = c("A","B", "C", "D","E", "F"))
ggsave("BA_residual.png")


ggarrange(BA_residual_percentage_LM_all, BA_residual_percentage_LM_males,
          BA_residual_percentage_LM_females,BA_residual2_percentage_all,
          BA_residual2_percentage_males, BA_residual2_percentage_females,
          ncol = 3, nrow = 2, labels = c("A","B", "C", "D","E", "F"))
ggsave("BA_residual_percentage.png")




## Figure  7 combined adipose and muscle mass LM

ggarrange(BA_adiposeLM_all, BA_adipose_percentage_LM_all,
          BA_adipose2_all, BA_adipose2_percentage_all,
          ncol = 2, nrow = 2, labels = c("A","B", "C", "D","E", "F"))
ggsave("BA_adipose_all.png")

ggarrange(BA_adiposeLM_males, BA_adipose_percentage_LM_males,
          BA_adipose2_males, BA_adipose2_percentage_males,
          ncol = 2, nrow = 2, labels = c("A","B", "C", "D","E", "F"))
ggsave("BA_adipose_males.png")

ggarrange(BA_adiposeLM_females, BA_adipose_percentage_LM_females,
          BA_adipose2_females, BA_adipose2_percentage_females,
          ncol = 2, nrow = 2, labels = c("A","B", "C", "D","E", "F"))
ggsave("BA_adipose_females.png")
