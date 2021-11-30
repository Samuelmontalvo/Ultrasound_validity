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

######################### Front Thigh ################################

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


##########################CALF ######################################

calf2_lm = lm(Medial_Calf_C ~ `Medial Calf_*2`, data = df)
calf2_summary  <- summary(calf2_lm)
calf2_summary

cor(Medial_Calf_C,`Medial Calf_*2`)

icc_calf2_application <- data_frame(`Medial Calf_*2`,Medial_Calf_C)
icc(icc_calf2_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_calf2_application <- blandr.statistics(`Medial Calf_*2`,
                                               Medial_Calf_C, sig.level=0.95 )
summary(BA_stats_calf2_application)

BA_plot_calf_application <- blandr.draw(`Medial Calf_*2`,Medial_Calf_C, sig.level = 0.95,
                                        plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_calf_application


####################TRICEPS PT FORMULA####################################
triceps_pt_lm = lm(Triceps_US2 ~ Triceps_C, data = df)
triceps_pt_summary  <- summary(triceps_pt_lm)
triceps_pt_summary

Triceps_pt_predicted <- (Triceps_US2/triceps_pt_summary$coefficients[2,1]) -
  triceps_pt_summary$coefficients[1,1]

cor(Triceps_C,Triceps_pt_predicted)

icc_Triceps_application <- data_frame(Triceps_pt_predicted,Triceps_C)
icc(icc_Triceps_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_pt_triceps_application <- blandr.statistics(Triceps_pt_predicted,
                                                  Triceps_C, sig.level=0.95 )
summary(BA_stats_pt_triceps_application)

BA_plot_pt_triceps_application <- blandr.draw(Triceps_pt_predicted,Triceps_C, sig.level = 0.95,
                                           plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_pt_triceps_application

############################SUBSCAPULAR PT FORMULA#################################

Subscapular_pt_lm = lm(Subscapular_US2 ~ Subscapular_C, data = df)
Subscapular_pt_summary  <- summary(Subscapular_pt_lm)
Subscapular_pt_summary

Subscapular_pt_predicted <- (Subscapular_US2/Subscapular_pt_summary$coefficients[2,1]) -
  Subscapular_pt_summary$coefficients[1,1]

cor(Subscapular_C,Subscapular_pt_predicted)

icc_subscapular_pt_application <- data_frame(Subscapular_pt_predicted,Subscapular_C)
icc(icc_subscapular_pt_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_subscapular_pt_application <- blandr.statistics(Subscapular_pt_predicted,
                                                      Subscapular_C, sig.level=0.95 )
summary(BA_stats_subscapular_pt_application)

BA_plot_subscapular_pt_application <- blandr.draw(Subscapular_pt_predicted,Subscapular_C, sig.level = 0.95,
                                               plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_subscapular_pt_application

#############################BICEPS PT FORMULA #######################################

Biceps_pt_lm = lm(Bicep_US2 ~ Bicep_C, data = df)
Biceps_pt_summary  <- summary(Biceps_pt_lm)
Biceps_pt_summary

Biceps_pt_predicted <- (Bicep_US2/Biceps_pt_summary$coefficients[2,1]) -
  Biceps_pt_summary$coefficients[1,1]

cor(Bicep_C,Biceps_pt_predicted)

icc_biceps_pt_application <- data_frame(Biceps_pt_predicted,Bicep_C)
icc(icc_biceps_pt_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_pt_biceps_application <- blandr.statistics(Biceps_pt_predicted,
                                                      Bicep_C, sig.level=0.95 )
summary(BA_stats_pt_subscapular_application)

BA_plot_pt_biceps_application <- blandr.draw(Biceps_pt_predicted,Bicep_C, sig.level = 0.95,
                                               plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_pt_biceps_application

############################Iliac crest PT FORMULA#################################

Iliac_pt_lm = lm(iliac_Crest_US2 ~ iliac_Crest_C, data = df)
Iliac_pt_summary  <- summary(Iliac_pt_lm)
Iliac_pt_summary

Iliac_pt_predicted <- (iliac_Crest_US2/Iliac_pt_summary$coefficients[2,1]) -
  Iliac_pt_summary$coefficients[1,1]

cor(iliac_Crest_C,Iliac_pt_predicted)

icc_iliac_application <- data_frame(Iliac_pt_predicted,iliac_Crest_C)
icc(icc_iliac_application, model = "twoway", type = "agreement", unit = "average")


BA_stats_pt_iliac_application <- blandr.statistics(Iliac_pt_predicted,
                                                iliac_Crest_C, sig.level=0.95 )
summary(BA_stats_pt_iliac_application)

BA_plot_pt_iliac_application <- blandr.draw(Iliac_pt_predicted,iliac_Crest_C, sig.level = 0.95,
                                         plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_pt_iliac_application

###########################Supraspinale PT FORMULA ####################################

supra_pt_lm = lm(Supraspinale_US2 ~ Supraspinale_C, data = df)
supra_pt_summary  <- summary(supra_pt_lm)
supra_pt_summary

supra_pt_predicted <- (Supraspinale_US2/supra_pt_summary$coefficients[2,1]) -
  supra_pt_summary$coefficients[1,1]

Iliac_pt_predicted <- (iliac_Crest_US2/Iliac_summary$coefficients[2,1]) -
  Iliac_summary$coefficients[1,1]

cor(Supraspinale_C,supra_pt_predicted)

icc_supra_application <- data_frame(supra_pt_predicted,Supraspinale_C)
icc(icc_supra_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_supra_application <- blandr.statistics(supra_pt_predicted,
                                                Supraspinale_C, sig.level=0.95 )
summary(BA_stats_supra_application)

BA_plot_supra_application <- blandr.draw(supra_pt_predicted,Supraspinale_C, sig.level = 0.95,
                                         plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_supra_application

#######################ABDOMINAL PT FORMULA #####################################
abdominal_pt_lm = lm(Abdominal_US2~Abdominal_C, data = df)
abdominal_pt_summary  <- summary(abdominal_pt_lm)
abdominal_pt_summary

abdominal_pt_predicted <-  (Abdominal_US2/abdominal_summary$coefficients[2,1]) -
  abdominal_summary$coefficients[1,1]

cor(Abdominal_C,abdominal_pt_predicted)

icc_abdominal_application <- data_frame(abdominal_pt_predicted,Abdominal_C)
icc(icc_abdominal_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_abdominal_pt_application <- blandr.statistics(abdominal_pt_predicted,
                                                Abdominal_C, sig.level=0.95 )
summary(BA_stats_abdominal_pt_application)

BA_plot_pt_abdominal_application <- blandr.draw(abdominal_pt_predicted,Abdominal_C, sig.level = 0.95,
                                             plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_pt_abdominal_application

######################### Front Thigh PT FORMULA ################################

FT_pt_lm = lm(Front_Thigh_US2 ~ Front_Thigh_C, data = df)
FT_pt_summary  <- summary(FT_pt_lm)
FT_pt_summary

FT_pt_predicted <- (Front_Thigh_US2/FT_pt_summary$coefficients[2,1]) -
  FT_pt_summary$coefficients[1,1]

cor(Front_Thigh_C,FT_pt_predicted)

icc_FT_pt_application <- data_frame(FT_pt_predicted,Front_Thigh_C)
icc(icc_FT_pt_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_FT_application <- blandr.statistics(FT_pt_predicted,
                                             Front_Thigh_C, sig.level=0.95 )
summary(BA_stats_FT_application)

BA_plot_FT_application <- blandr.draw(FT_pt_predicted,Front_Thigh_C, sig.level = 0.95,
                                      plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_FT_application

##########################CALF PT FORMULA ######################################

calf_pt_lm = lm(Medial_Calf_US2 ~ Medial_Calf_C, data = df)
calf_pt_summary  <- summary(calf_pt_lm)
calf_pt_summary

calf_pt_predicted <- (Medial_Calf_US2/calf_pt_summary$coefficients[2,1]) -
  calf_pt_summary$coefficients[1,1]

cor(Medial_Calf_C,calf_pt_predicted)

icc_calf_pt_application <- data_frame(calf_pt_predicted,Medial_Calf_C)
icc(icc_calf_pt_application, model = "twoway", type = "agreement", unit = "average")

BA_stats_calf_pt_application <- blandr.statistics(calf_pt_predicted,
                                               Medial_Calf_C, sig.level=0.95 )
summary(BA_stats_calf_pt_application)

BA_plot_calf_pt_application <- blandr.draw(calf_pt_predicted,Medial_Calf_C, sig.level = 0.95,
                                        plotTitle="",ciDisplay = FALSE) +theme_bw()
BA_plot_calf_pt_application



library("openxlsx")
write.xlsx(df, file = "US_SF_new.xlsx",
           sheetName = "Regressions", append = FALSE)
