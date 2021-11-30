library(readxl)
df <- read_excel("~/R/Ultrasound_validity/US_SF.xlsx",
                 sheet = "Regressions")
View(df)
attach(df)

View(df)
attach(df)

## Required libraries
library(dplyr)
library(blandr)
library(ggplot2)
library(irr)

## recode data
df <-df %>%
  mutate(Sex = ifelse(Sex == 1, "Male", "Female"))

## filter by gender
Male <- df %>% filter(Sex == "Male")
Female <- df %>% filter(Sex == "Female")

####################TRICEPS ####################################

Triceps_lm = lm(Triceps_C ~ Triceps_US2, data = df)
Triceps_summary  <- summary(Triceps_lm)
Triceps_summary

Triceps_predicted <- Triceps_summary$coefficients[1,1] +
  (Triceps_US2*Triceps_summary$coefficients[2,1])

df <- df %>% mutate(Triceps_predicted = Triceps_predicted)

cor(Triceps_C,Triceps_predicted)

icc_Triceps_application <- data_frame(Triceps_predicted,Triceps_C)
icc(icc_Triceps_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_triceps_application <- blandr.statistics(Triceps_predicted,
                                Triceps_C, sig.level=0.95 )
summary(BA_stats_triceps_application)

BA_plot_triceps_application <- blandr.draw(Triceps_predicted,Triceps_C, sig.level = 0.95,
                               plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_triceps_application

############################SUBSCAPULAR#################################

Subscapular_lm = lm(Subscapular_C ~ Subscapular_US2, data = df)
Subscapular_summary  <- summary(Subscapular_lm)
Subscapular_summary

Subscapular_predicted <- Subscapular_summary$coefficients[1,1] +
  (Subscapular_US2*Subscapular_summary$coefficients[2,1])

df <- df %>% mutate(Subscapular_predicted = Subscapular_predicted)

cor(Subscapular_C,Subscapular_predicted)

icc_subscapular_application <- data_frame(Subscapular_predicted,Subscapular_C)
icc(icc_subscapular_application, model = "twoway", type = "agreement", unit = "average")


BA_stats_subscapular_application <- blandr.statistics(Subscapular_predicted,
                                                  Subscapular_C, sig.level=0.95 )
summary(BA_stats_subscapular_application)

BA_plot_subscapular_application <- blandr.draw(Subscapular_predicted,Subscapular_C, sig.level = 0.95,
                                           plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_subscapular_application

#############################BICEPS #######################################

Biceps_lm = lm(Bicep_C ~ Bicep_US2, data = df)
Biceps_summary  <- summary(Biceps_lm)
Biceps_summary

Biceps_predicted <- Biceps_summary$coefficients[1,1] +
  (Bicep_US2*Biceps_summary$coefficients[2,1])

df <- df %>% mutate(Biceps_predicted = Biceps_predicted)

cor(Bicep_C,Biceps_predicted)

icc_biceps_application <- data_frame(Biceps_predicted,Bicep_C)
icc(icc_biceps_application, model = "twoway", type = "agreement", unit = "average")


BA_stats_subscapular_application <- blandr.statistics(Biceps_predicted,
                                                      Bicep_C, sig.level=0.95 )
summary(BA_stats_subscapular_application)

BA_plot_subscapular_application <- blandr.draw(Biceps_predicted,Bicep_C, sig.level = 0.95,
                                               plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_subscapular_application

############################Iliac crest#################################

Iliac_lm = lm(iliac_Crest_C ~ iliac_Crest_US2, data = df)
Iliac_summary  <- summary(Iliac_lm)
Iliac_summary

Iliac_predicted <- Iliac_summary$coefficients[1,1] +
  (iliac_Crest_US2*Iliac_summary$coefficients[2,1])

df <- df %>% mutate(Iliac_predicted = Iliac_predicted)

cor(iliac_Crest_C,Iliac_predicted)

icc_biceps_application <- data_frame(Iliac_predicted,iliac_Crest_C)
icc(icc_biceps_application, model = "twoway", type = "agreement", unit = "average")


BA_stats_iliac_application <- blandr.statistics(Iliac_predicted,
                                                      iliac_Crest_C, sig.level=0.95 )
summary(BA_stats_iliac_application)

BA_plot_iliac_application <- blandr.draw(Iliac_predicted,iliac_Crest_C, sig.level = 0.95,
                                               plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_iliac_application

###########################Supraspinale ####################################

supra_lm = lm(Supraspinale_C ~ Supraspinale_US2, data = df)
supra_summary  <- summary(supra_lm)
supra_summary

supra_predicted <- supra_summary$coefficients[1,1] +
  (Supraspinale_US2*supra_summary$coefficients[2,1])

df <- df %>% mutate(supra_predicted = supra_predicted)

cor(Supraspinale_C,supra_predicted)

icc_biceps_application <- data_frame(supra_predicted,Supraspinale_C)
icc(icc_biceps_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_iliac_application <- blandr.statistics(supra_predicted,
                                                Supraspinale_C, sig.level=0.95 )
summary(BA_stats_iliac_application)

BA_plot_iliac_application <- blandr.draw(supra_predicted,Supraspinale_C, sig.level = 0.95,
                                         plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_iliac_application

#######################ABDOMINAL #####################################
abdominal_lm = lm(Abdominal_C~Abdominal_US2, data = df)
abdominal_summary  <- summary(abdominal_lm)
abdominal_summary

abdominal_predicted <- abdominal_summary$coefficients[1,1] +
  (Abdominal_C*abdominal_summary$coefficients[2,1])

df <- df %>% mutate(abdominal_predicted = abdominal_predicted)

cor(Abdominal_C,abdominal_predicted)

icc_abdominal_application <- data_frame(Abdominal_Formula,Abdominal_C)
icc(icc_abdominal_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_iliac_application <- blandr.statistics(abdominal_predicted,
                                                Abdominal_C, sig.level=0.95 )
summary(BA_stats_iliac_application)

BA_plot_abdominal_application <- blandr.draw(abdominal_predicted,Abdominal_C, sig.level = 0.95,
                                         plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_abdominal_application

######################### Front Thigh ################################

FT_lm = lm(Front_Thigh_C ~ Front_Thigh_US2, data = df)
FT_summary  <- summary(FT_lm)
FT_summary

FT_predicted <- FT_summary$coefficients[1,1] +
  (Front_Thigh_US2*FT_summary$coefficients[2,1])

df <- df %>% mutate(FT_predicted = FT_predicted)

cor(Front_Thigh_C,FT_predicted)

icc_abdominal_application <- data_frame(FT_predicted,Front_Thigh_C)
icc(icc_abdominal_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_FT_application <- blandr.statistics(FT_predicted,
                                                Front_Thigh_C, sig.level=0.95 )
summary(BA_stats_FT_application)

BA_plot_FT_application <- blandr.draw(FT_predicted,Front_Thigh_C, sig.level = 0.95,
                                         plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_FT_application


##########################CALF ######################################

calf_lm = lm(Medial_Calf_C ~ Medial_Calf_US2, data = df)
calf_summary  <- summary(calf_lm)
calf_summary

calf_predicted <- calf_summary$coefficients[1,1] +
  (Medial_Calf_US2*calf_summary$coefficients[2,1])

df <- df %>% mutate(calf_predicted = calf_predicted)

cor(Medial_Calf_C,calf_predicted)

icc_calf_application <- data_frame(calf_predicted,Medial_Calf_C)
icc(icc_calf_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_calf_application <- blandr.statistics(calf_predicted,
                                             Medial_Calf_C, sig.level=0.95 )
summary(BA_stats_calf_application)

BA_plot_calf_application <- blandr.draw(calf_predicted,Medial_Calf_C, sig.level = 0.95,
                                      plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_calf_application
