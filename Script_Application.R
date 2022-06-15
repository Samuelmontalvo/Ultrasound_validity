library(readxl)
df <- read_excel("~/R/Ultrasound_validity/US_SF.xlsx",
                 sheet = "Regressions")
View(df)
attach(df)

## Required libraries
library(dplyr)
library(blandr)
library(ggplot2)
library(irr)
library(rstatix)
library(ggpubr)

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

Triceps_predicted_alt <- Triceps_summary$coefficients[1,1] +
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


BA_stats_bicep_application <- blandr.statistics(Biceps_predicted,
                                                      Bicep_C, sig.level=0.95 )
summary(BA_stats_bicep_application)

BA_plot_bicep_application <- blandr.draw(Biceps_predicted,Bicep_C, sig.level = 0.95,
                                               plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_bicep_application

############################Iliac crest#################################

Iliac_lm = lm(iliac_Crest_C ~ iliac_Crest_US2, data = df)
Iliac_summary  <- summary(Iliac_lm)
Iliac_summary

Iliac_predicted <- Iliac_summary$coefficients[1,1] +
  (iliac_Crest_US2*Iliac_summary$coefficients[2,1])

df <- df %>% mutate(Iliac_predicted = Iliac_predicted)

cor(iliac_Crest_C,Iliac_predicted)

icc_iliac_application <- data_frame(Iliac_predicted,iliac_Crest_C)
icc(icc_iliac_application, model = "twoway", type = "agreement", unit = "average")


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

icc_supra_application <- data_frame(supra_predicted,Supraspinale_C)
icc(icc_supra_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_supra_application <- blandr.statistics(supra_predicted,
                                                Supraspinale_C, sig.level=0.95 )
summary(BA_stats_iliac_application)

BA_plot__supra_application <- blandr.draw(supra_predicted,Supraspinale_C, sig.level = 0.95,
                                         plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot__supra_application

#######################ABDOMINAL #####################################
abdominal_lm = lm(Abdominal_C~Abdominal_US2, data = df)
abdominal_summary  <- summary(abdominal_lm)
abdominal_summary

abdominal_predicted <- abdominal_summary$coefficients[1,1] +
  (Abdominal_US2*abdominal_summary$coefficients[2,1])

df <- df %>% mutate(abdominal_predicted = abdominal_predicted)

cor(Abdominal_C,abdominal_predicted)

icc_abdominal_application <- data_frame(abdominal_predicted,Abdominal_C)
icc(icc_abdominal_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_abdominal_application <- blandr.statistics(abdominal_predicted,
                                                Abdominal_C, sig.level=0.95 )
summary(BA_stats_abdominal_application)

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

######################## * 2 ###########################################
####################TRICEPS *2 ####################################

triceps2_lm = lm(Triceps_C ~ `Triceps_*2`, data = df)
triceps2_summary  <- summary(triceps2_lm)
triceps2_summary

cor(Triceps_C,`Triceps_*2`)

icc_Triceps2_application <- data_frame(`Triceps_*2`,Triceps_C)
icc(icc_Triceps2_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_triceps2_application <- blandr.statistics(`Triceps_*2`,
                                                  Triceps_C, sig.level=0.95 )
summary(BA_stats_triceps2_application)

BA_plot_triceps2_application <- blandr.draw(`Triceps_*2`,Triceps_C, sig.level = 0.95,
                                           plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_triceps2_application

############################SUBSCAPULAR 2#################################

Subscapular2_lm = lm(Subscapular_C ~ `Subscapular_*2`, data = df)
Subscapular2_summary  <- summary(Subscapular2_lm)
Subscapular2_summary

cor(Subscapular_C,`Subscapular_*2`)

icc_subscapular_application <- data_frame(`Subscapular_*2`,Subscapular_C)
icc(icc_subscapular_application, model = "twoway", type = "agreement", unit = "average")


BA_stats_subscapular2_application <- blandr.statistics(`Subscapular_*2`,
                                                      Subscapular_C, sig.level=0.95 )
summary(BA_stats_subscapular2_application)

BA_plot_subscapular2_application <- blandr.draw(`Subscapular_*2`,Subscapular_C, sig.level = 0.95,
                                               plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_subscapular2_application

#############################BICEPS 2 #######################################

Biceps2_lm = lm(Bicep_C ~ `Bicep_*2`, data = df)
Biceps2_summary  <- summary(Biceps_lm)
Biceps2_summary

cor(Bicep_C,`Bicep_*2`)

icc_biceps2_application <- data_frame(`Bicep_*2`,Bicep_C)
icc(icc_biceps2_application, model = "twoway", type = "agreement", unit = "average")


BA_stats_biceps2_application <- blandr.statistics(`Bicep_*2`,
                                                      Bicep_C, sig.level=0.95 )
summary(BA_stats_biceps2_application)

BA_plot_biceps2_application <- blandr.draw(`Bicep_*2`,Bicep_C, sig.level = 0.95,
                                               plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_biceps2_application

############################Iliac crest 2#################################

Iliac2_lm = lm(iliac_Crest_C ~ `Iliac_Crest_*2`, data = df)
Iliac2_summary  <- summary(Iliac2_lm)
Iliac2_summary

cor(iliac_Crest_C,`Iliac_Crest_*2`)

icc_iliac2_application <- data_frame(`Iliac_Crest_*2`,iliac_Crest_C)
icc(icc_iliac2_application, model = "twoway", type = "agreement", unit = "average")


BA_stats_iliac2_application <- blandr.statistics(`Iliac_Crest_*2`,
                                                iliac_Crest_C, sig.level=0.95 )
summary(BA_stats_iliac2_application)

BA_plot_iliac2_application <- blandr.draw(`Iliac_Crest_*2`,iliac_Crest_C, sig.level = 0.95,
                                         plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_iliac2_application

###########################Supraspinale 2####################################

supra2_lm = lm(Supraspinale_C ~ `Supraspinale_*2`, data = df)
supra2_summary  <- summary(supra2_lm)
supra2_summary

cor(Supraspinale_C,`Supraspinale_*2`)

icc_supra2_application <- data_frame(`Supraspinale_*2`,Supraspinale_C)
icc(icc_supra2_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_supra2_application <- blandr.statistics(`Supraspinale_*2`,
                                                Supraspinale_C, sig.level=0.95 )
summary(BA_stats_supra2_application)

BA_plot_supra2_application <- blandr.draw(`Supraspinale_*2`,Supraspinale_C, sig.level = 0.95,
                                         plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_supra2_application

#######################ABDOMINAL 2 #####################################
abdominal2_lm = lm(Abdominal_C~`Abdominal_*2`, data = df)
abdominal2_summary  <- summary(abdominal2_lm)
abdominal2_summary

cor(Abdominal_C,`Abdominal_*2`)

icc_abdominal2_application <- data_frame(`Abdominal_*2`,Abdominal_C)
icc(icc_abdominal2_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_abdominal2_application <- blandr.statistics(`Abdominal_*2`,
                                                Abdominal_C, sig.level=0.95 )
summary(BA_stats_abdominal2_application)

BA_plot_abdominal2_application <- blandr.draw(`Abdominal_*2`,Abdominal_C, sig.level = 0.95,
                                             plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_abdominal2_application

######################### Front Thigh 2 ################################

FT2_lm = lm(Front_Thigh_C ~ `Front_Thigh_*2`, data = df)
FT2_summary  <- summary(FT2_lm)
FT2_summary

cor(Front_Thigh_C,`Front_Thigh_*2`)

icc_FT2_application <- data_frame(`Front_Thigh_*2`,Front_Thigh_C)
icc(icc_abdominal_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_FT2_application <- blandr.statistics(`Front_Thigh_*2`,
                                             Front_Thigh_C, sig.level=0.95 )
summary(BA_stats_FT2_application)

BA_plot_FT2_application <- blandr.draw(`Front_Thigh_*2`,Front_Thigh_C, sig.level = 0.95,
                                      plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_FT2_application


##########################CALF 2 ######################################

calf2_lm = lm(Medial_Calf_C ~ `Medial Calf_*2`, data = df)
calf2_summary  <- summary(calf2_lm)
calf2_summary

cor(Medial_Calf_C,`Medial Calf_*2`)

icc_calf2_application <- data_frame(`Medial Calf_*2`,Medial_Calf_C)
icc(icc_calf2_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_calf2_application <- blandr.statistics(`Medial Calf_*2`,
                                               Medial_Calf_C, sig.level=0.95 )
summary(BA_stats_calf2_application)

BA_plot_calf2_application <- blandr.draw(`Medial Calf_*2`,Medial_Calf_C, sig.level = 0.95,
                                        plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_calf2_application


## PLOT OF BLAND ALTMAN of All
library(ggpubr)
BA_ALL_APPLICATION <- ggarrange(BA_plot_triceps_application,BA_plot_triceps2_application,
                    BA_plot_subscapular_application,BA_plot_subscapular_application,
                    BA_plot_bicep_application, BA_plot_biceps2_application,
                    BA_plot_iliac_application,BA_plot_iliac2_application,
                    BA_plot_supra_application,BA_plot_supra2_application,
                    BA_plot_abdominal_application,BA_plot_abdominal2_application,
                    BA_plot_FT_application,BA_plot_FT2_application,
                    BA_plot_calf_application,BA_plot_calf2_application,
                    ncol=4,nrow=4,
                    labels = c("A","B", "C", "D","E","F","G","H","I","J",
                               "K","L","M","N","O","P"),
                    label.y = 1.03)
ggsave("BA_ALL_APPLICATION.png")



#####################VALIDATION#######################################

library("openxlsx")
write.xlsx(df, file = "US_SF_new.xlsx",
           sheetName = "Regressions", append = FALSE)

#############ADIPOSE #######################################
library(readxl)
df <- read_excel("~/R/Ultrasound_validity/US_SF.xlsx",
                 sheet = "Application")
View(df)
attach(df)

Adipose_lm = lm(Adipose_Mass ~ Adipose_Mass_LM_Formula, data = df)
Adipose_summary  <- summary(Adipose_lm)
Adipose_summary

cor(Adipose_Mass,Adipose_Mass_LM_Formula)

icc_adipose_application <- data_frame(Adipose_Mass_LM_Formula,Adipose_Mass)
icc(icc_adipose_application, model = "twoway", type = "agreement", unit = "average")


BA_stats_adipose_application <- blandr.statistics(Adipose_Mass_LM_Formula,
                                                  Adipose_Mass, sig.level=0.95 )
summary(BA_stats_adipose_application)

BA_plot_adipose_application <- blandr.draw(Adipose_Mass,Adipose_Mass_LM_Formula, sig.level = 0.95,
                                           plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_adipose_application

## Adipose mass *2

Adipose2 = lm(Adipose_Mass ~ `Adipose_Mass*2`, data = df)
Adipose2_summary  <- summary(Adipose2)
Adipose2_summary

cor(Adipose_Mass,`Adipose_Mass*2`)

icc_adipose2_application <- data_frame(`Adipose_Mass*2`,Adipose_Mass)
icc(icc_adipose2_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_adipose2_application <- blandr.statistics(`Adipose_Mass*2`,
                                                  Adipose_Mass, sig.level=0.95 )
summary(BA_stats_adipose2_application)

BA_plot_adipose2_application <- blandr.draw(Adipose_Mass,`Adipose_Mass*2`, sig.level = 0.95,
                                           plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_adipose2_application

## Muscle Mass
Muscle_Mass_lm = lm(Muscle_Mass ~ Muscle_Mass_LM_Formula, data = df)
Muscle_Mass_lm_summary  <- summary(Muscle_Mass_lm)
Muscle_Mass_lm_summary

cor(Muscle_Mass,Muscle_Mass_LM_Formula)

icc_muscle_application <- data_frame(Muscle_Mass_LM_Formula,Muscle_Mass)
icc(icc_muscle_application, model = "twoway", type = "agreement", unit = "average")


BA_stats_muscle_application <- blandr.statistics(Muscle_Mass_LM_Formula,
                                                  Muscle_Mass, sig.level=0.95 )
summary(BA_stats_muscle_application)

BA_plot_muscle_application <- blandr.draw(Muscle_Mass,Muscle_Mass_LM_Formula, sig.level = 0.95,
                                           plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_muscle_application

##Muscle mass * 2
Muscle_Mass2 = lm(Muscle_Mass~ `Muscle_Mass*2`, data = df)
Muscle_Mass2_summary  <- summary(Muscle_Mass2)
Muscle_Mass2_summary

cor(Muscle_Mass,`Muscle_Mass*2`)

icc_muscle2_application <- data_frame(`Muscle_Mass*2`,Muscle_Mass)
icc(icc_muscle2_application, model = "twoway", type = "agreement", unit = "average")


BA_stats_muscle2_application <- blandr.statistics(`Muscle_Mass*2`,
                                                  Muscle_Mass, sig.level=0.95 )
summary(BA_stats_muscle2_application)

BA_plot_muscle2_application <- blandr.draw(Muscle_Mass,`Muscle_Mass*2`, sig.level = 0.95,
                                           plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_muscle2_application


