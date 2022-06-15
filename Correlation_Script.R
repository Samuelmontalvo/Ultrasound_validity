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

##################TRICEPS ##################################

## Pearsons Correlation
library(stats)
cor_triceps_1 <- cor.test(df$Triceps_C,df$Triceps_US1,
                          method = "pearson",alternative = "two.sided")
cor_triceps_1

cor_triceps_2 <- cor.test(df$Triceps_C,df$Triceps_US2,
                          method = "pearson",alternative = "two.sided")
cor_triceps_2

cor_triceps_3 <- cor.test(df$Triceps_C,df$Triceps_US1_2,
                          method = "pearson",alternative = "two.sided")
cor_triceps_3

cor_triceps_4 <- cor.test(df$Triceps_C,df$Triceps_US2_2,
                          method = "pearson",alternative = "two.sided")
cor_triceps_4

#MALES
cor_male_triceps_1 <- cor.test(Male$Triceps_C,Male$Triceps_US1,
                          method = "pearson",alternative = "two.sided")
cor_male_triceps_1


cor_male_triceps_2 <- cor.test(Male$Triceps_C,Male$Triceps_US2,
                          method = "pearson",alternative = "two.sided")
cor_male_triceps_2

cor_male_triceps_3 <- cor.test(Male$Triceps_C,Male$Triceps_US1_2,
                               method = "pearson",alternative = "two.sided")
cor_male_triceps_3

cor_male_triceps_4 <- cor.test(Male$Triceps_C,Male$Triceps_US2_2,
                               method = "pearson",alternative = "two.sided")
cor_male_triceps_4

#FEMALES
cor_female_triceps_1 <- cor.test(Female$Triceps_C,Female$Triceps_US1,
                          method = "pearson",alternative = "two.sided")
cor_female_triceps_1

cor_female_triceps_2 <- cor.test(Female$Triceps_C,Female$Triceps_US2,
                          method = "pearson",alternative = "two.sided")
cor_female_triceps_2

cor_female_triceps_3 <- cor.test(Female$Triceps_C,Female$Triceps_US1_2,
                                 method = "pearson",alternative = "two.sided")
cor_female_triceps_3

cor_female_triceps_4 <- cor.test(Female$Triceps_C,Female$Triceps_US2_2,
                                 method = "pearson",alternative = "two.sided")
cor_female_triceps_4

###############Subscapular ########################
cor_subscapular_1 <- cor.test(df$Subscapular_C,df$Subscapular_US1,
                              method = "pearson",alternative = "two.sided")
cor_subscapular_1

cor_subscapular_2 <- cor.test(df$Subscapular_C,df$Subscapular_US2,
                              method = "pearson",alternative = "two.sided")
cor_subscapular_2

cor_subscapular_3 <- cor.test(df$Subscapular_C,df$Subscapular_US1_2,
                              method = "pearson",alternative = "two.sided")
cor_subscapular_3

cor_subscapular_4 <- cor.test(df$Subscapular_C,df$Subscapular_US2_2,
                              method = "pearson",alternative = "two.sided")
cor_subscapular_4

#MALES
cor_male_subscapular_1 <- cor.test(Male$Subscapular_C,Male$Subscapular_US1,
                              method = "pearson",alternative = "two.sided")
cor_male_subscapular_1

cor_male_subscapular_2 <- cor.test(Male$Subscapular_C,Male$Subscapular_US2,
                              method = "pearson",alternative = "two.sided")
cor_male_subscapular_2

cor_male_subscapular_3 <- cor.test(Male$Subscapular_C,Male$Subscapular_US1_2,
                                   method = "pearson",alternative = "two.sided")
cor_male_subscapular_3

cor_male_subscapular_4 <- cor.test(Male$Subscapular_C,Male$Subscapular_US2_2,
                                   method = "pearson",alternative = "two.sided")
cor_male_subscapular_4

#FEMALES
cor_female_subscapular_1 <- cor.test(Female$Subscapular_C,Female$Subscapular_US1,
                                   method = "pearson",alternative = "two.sided")
cor_female_subscapular_1

cor_female_subscapular_2 <- cor.test(Female$Subscapular_C,Female$Subscapular_US2,
                                   method = "pearson",alternative = "two.sided")
cor_female_subscapular_2

cor_female_subscapular_3 <- cor.test(Female$Subscapular_C,Female$Subscapular_US1_2,
                                     method = "pearson",alternative = "two.sided")
cor_female_subscapular_3

cor_female_subscapular_4 <- cor.test(Female$Subscapular_C,Female$Subscapular_US2_2,
                                     method = "pearson",alternative = "two.sided")
cor_female_subscapular_4

###############Bicep ########################
cor_bicep_1 <- cor.test(df$Bicep_C,df$Bicep_US1,
                        method = "pearson",alternative = "two.sided")
cor_bicep_1

cor_bicep_2 <- cor.test(df$Bicep_C,df$Bicep_US2,
                        method = "pearson",alternative = "two.sided")
cor_bicep_2

#MALE
cor_male_bicep_1 <- cor.test(Male$Bicep_C,Male$Bicep_US1,
                        method = "pearson",alternative = "two.sided")
cor_male_bicep_1

cor_male_bicep_2 <- cor.test(Male$Bicep_C,Male$Bicep_US2,
                        method = "pearson",alternative = "two.sided")
cor_male_bicep_2

cor_male_bicep_3 <- cor.test(Male$Bicep_C,Male$Bicep_US1_2,
                             method = "pearson",alternative = "two.sided")
cor_male_bicep_3

cor_male_bicep_4 <- cor.test(Male$Bicep_C,Male$Bicep_US2_2,
                             method = "pearson",alternative = "two.sided")
cor_male_bicep_4

#FEMALE
cor_female_bicep_1 <- cor.test(Female$Bicep_C,Female$Bicep_US1,
                             method = "pearson",alternative = "two.sided")
cor_female_bicep_1

cor_female_bicep_2 <- cor.test(Female$Bicep_C,Female$Bicep_US2,
                             method = "pearson",alternative = "two.sided")
cor_female_bicep_2

cor_female_bicep_3 <- cor.test(Female$Bicep_C,Female$Bicep_US1_2,
                               method = "pearson",alternative = "two.sided")
cor_female_bicep_3

cor_female_bicep_4 <- cor.test(Female$Bicep_C,Female$Bicep_US2_2,
                               method = "pearson",alternative = "two.sided")
cor_female_bicep_4


###############iliac crest 1 ########################
cor_iliac_1 <- cor.test(df$iliac_Crest_C,df$iliac_Crest_US1,
                        method = "pearson",alternative = "two.sided")
cor_iliac_1

cor_iliac_2 <- cor.test(df$iliac_Crest_C,df$iliac_Crest_US2,
                        method = "pearson",alternative = "two.sided")
cor_iliac_2

cor_iliac_3 <- cor.test(df$iliac_Crest_C,df$iliac_Crest_US1_2,
                        method = "pearson",alternative = "two.sided")
cor_iliac_3

cor_iliac_4 <- cor.test(df$iliac_Crest_C,df$iliac_Crest_US2_2,
                        method = "pearson",alternative = "two.sided")
cor_iliac_4

#MALE
cor_male_iliac_1 <- cor.test(Male$iliac_Crest_C,Male$iliac_Crest_US1,
                        method = "pearson",alternative = "two.sided")
cor_male_iliac_1

cor_male_iliac_2 <- cor.test(Male$iliac_Crest_C,Male$iliac_Crest_US2,
                        method = "pearson",alternative = "two.sided")
cor_male_iliac_2

cor_male_iliac_3 <- cor.test(Male$iliac_Crest_C,Male$iliac_Crest_US1_2,
                        method = "pearson",alternative = "two.sided")
cor_male_iliac_3

cor_male_iliac_4 <- cor.test(Male$iliac_Crest_C,Male$iliac_Crest_US2_2,
                        method = "pearson",alternative = "two.sided")
cor_male_iliac_4

#FEMALE
cor_female_iliac_1 <- cor.test(Female$iliac_Crest_C,Female$iliac_Crest_US1,
                             method = "pearson",alternative = "two.sided")
cor_female_iliac_1

cor_female_iliac_2 <- cor.test(Female$iliac_Crest_C,Female$iliac_Crest_US2,
                             method = "pearson",alternative = "two.sided")
cor_female_iliac_2

cor_female_iliac_3 <- cor.test(Female$iliac_Crest_C,Female$iliac_Crest_US1_2,
                             method = "pearson",alternative = "two.sided")
cor_female_iliac_3

cor_female_iliac_4 <- cor.test(Female$iliac_Crest_C,Female$iliac_Crest_US2_2,
                             method = "pearson",alternative = "two.sided")
cor_female_iliac_4

###############supraspinale ########################
cor_supra_1 <- cor.test(df$Supraspinale_C,df$Supraspinale_US1,
                        method = "pearson",alternative = "two.sided")
cor_supra_1

cor_supra_2 <- cor.test(df$Supraspinale_C,df$Supraspinale_US2,
                        method = "pearson",alternative = "two.sided")
cor_supra_2

cor_supra_3 <- cor.test(df$Supraspinale_C,df$Supraspinale_US1_2,
                        method = "pearson",alternative = "two.sided")
cor_supra_3

cor_supra_4 <- cor.test(df$Supraspinale_C,df$Supraspinale_US2_2,
                        method = "pearson",alternative = "two.sided")
cor_supra_4

#MALE
cor_male_supra_1 <- cor.test(Male$Supraspinale_C,Male$Supraspinale_US1,
                        method = "pearson",alternative = "two.sided")
cor_male_supra_1

cor_male_supra_2 <- cor.test(Male$Supraspinale_C,Male$Supraspinale_US2,
                        method = "pearson",alternative = "two.sided")
cor_male_supra_2

cor_male_supra_3 <- cor.test(Male$Supraspinale_C,Male$Supraspinale_US1_2,
                        method = "pearson",alternative = "two.sided")
cor_male_supra_3

cor_male_supra_4 <- cor.test(Male$Supraspinale_C,Male$Supraspinale_US2_2,
                        method = "pearson",alternative = "two.sided")
cor_male_supra_4

#FEMALE
cor_female_supra_1 <- cor.test(Female$Supraspinale_C,Female$Supraspinale_US1,
                             method = "pearson",alternative = "two.sided")
cor_female_supra_1

cor_female_supra_2 <- cor.test(Female$Supraspinale_C,Female$Supraspinale_US2,
                             method = "pearson",alternative = "two.sided")
cor_female_supra_2

cor_female_supra_3 <- cor.test(Female$Supraspinale_C,Female$Supraspinale_US1_2,
                             method = "pearson",alternative = "two.sided")
cor_female_supra_3

cor_female_supra_4 <- cor.test(Female$Supraspinale_C,Female$Supraspinale_US2_2,
                             method = "pearson",alternative = "two.sided")
cor_female_supra_4

###############Abdominal ########################
cor_abdominal_1 <- cor.test(df$Abdominal_C,df$Abdominal_US1,
                            method = "pearson",alternative = "two.sided")
cor_abdominal_1

cor_abdominal_2 <- cor.test(df$Abdominal_C,df$Abdominal_US2,
                            method = "pearson",alternative = "two.sided")
cor_abdominal_2

cor_abdominal_3 <- cor.test(df$Abdominal_C,df$Abdominal_US1_2,
                            method = "pearson",alternative = "two.sided")
cor_abdominal_3
cor_abdominal_4 <- cor.test(df$Abdominal_C,df$Abdominal_US2_2,
                            method = "pearson",alternative = "two.sided")
cor_abdominal_4

#MALE
cor_male_abdominal_1 <- cor.test(Male$Abdominal_C,Male$Abdominal_US1,
                            method = "pearson",alternative = "two.sided")
cor_male_abdominal_1

cor_male_abdominal_2 <- cor.test(Male$Abdominal_C,Male$Abdominal_US2,
                            method = "pearson",alternative = "two.sided")
cor_male_abdominal_2

cor_male_abdominal_3 <- cor.test(Male$Abdominal_C,Male$Abdominal_US1_2,
                            method = "pearson",alternative = "two.sided")
cor_male_abdominal_3
cor_male_abdominal_4 <- cor.test(Male$Abdominal_C,Male$Abdominal_US2_2,
                            method = "pearson",alternative = "two.sided")
cor_male_abdominal_4

#FEMALE
cor_female_abdominal_1 <- cor.test(Female$Abdominal_C,Female$Abdominal_US1,
                                 method = "pearson",alternative = "two.sided")
cor_female_abdominal_1

cor_female_abdominal_2 <- cor.test(Female$Abdominal_C,Female$Abdominal_US2,
                                 method = "pearson",alternative = "two.sided")
cor_female_abdominal_2

cor_female_abdominal_3 <- cor.test(Female$Abdominal_C,Female$Abdominal_US1_2,
                                 method = "pearson",alternative = "two.sided")
cor_female_abdominal_3
cor_female_abdominal_4 <- cor.test(Female$Abdominal_C,Female$Abdominal_US2_2,
                                 method = "pearson",alternative = "two.sided")
cor_female_abdominal_4

####################Front Thigh ####################################
cor_FT_1 <- cor.test(df$Front_Thigh_C,df$Front_Thigh_US1,
                     method = "pearson",alternative = "two.sided")
cor_FT_1

cor_FT_2 <- cor.test(df$Front_Thigh_C,df$Front_Thigh_US2,
                     method = "pearson",alternative = "two.sided")
cor_FT_2
cor_FT_3 <- cor.test(df$Front_Thigh_C,df$Front_Thigh_US1_2,
                     method = "pearson",alternative = "two.sided")
cor_FT_3

cor_FT_4 <- cor.test(df$Front_Thigh_C,df$Front_Thigh_US2_2,
                     method = "pearson",alternative = "two.sided")
cor_FT_4

#MALE
cor_male_FT_1 <- cor.test(Male$Front_Thigh_C,Male$Front_Thigh_US1,
                     method = "pearson",alternative = "two.sided")
cor_male_FT_1

cor_male_FT_2 <- cor.test(Male$Front_Thigh_C,Male$Front_Thigh_US2,
                     method = "pearson",alternative = "two.sided")
cor_male_FT_2
cor_male_FT_3 <- cor.test(Male$Front_Thigh_C,Male$Front_Thigh_US1_2,
                     method = "pearson",alternative = "two.sided")
cor_male_FT_3

cor_male_FT_4 <- cor.test(Male$Front_Thigh_C,Male$Front_Thigh_US2_2,
                     method = "pearson",alternative = "two.sided")
cor_male_FT_4

#FEMALE
cor_female_FT_1 <- cor.test(Female$Front_Thigh_C,Female$Front_Thigh_US1,
                          method = "pearson",alternative = "two.sided")
cor_female_FT_1

cor_female_FT_2 <- cor.test(Female$Front_Thigh_C,Female$Front_Thigh_US2,
                          method = "pearson",alternative = "two.sided")
cor_female_FT_2
cor_female_FT_3 <- cor.test(Female$Front_Thigh_C,Female$Front_Thigh_US1_2,
                          method = "pearson",alternative = "two.sided")
cor_female_FT_3

cor_female_FT_4 <- cor.test(Female$Front_Thigh_C,Female$Front_Thigh_US2_2,
                          method = "pearson",alternative = "two.sided")
cor_female_FT_4

####################Medial Calf ####################################
cor_calf_1 <- cor.test(df$Medial_Calf_C,df$Medial_Calf_US1,
                       method = "pearson",alternative = "two.sided")
cor_calf_1
cor_calf_2 <- cor.test(df$Medial_Calf_C,df$Medial_Calf_US2,
                       method = "pearson",alternative = "two.sided")
cor_calf_2
cor_calf_3 <- cor.test(df$Medial_Calf_C,df$Medial_Calf_US1_2,
                       method = "pearson",alternative = "two.sided")
cor_calf_3
cor_calf_4 <- cor.test(df$Medial_Calf_C,df$Medial_Calf_US2_2,
                       method = "pearson",alternative = "two.sided")
cor_calf_4

#Male
cor_male_calf_1 <- cor.test(Male$Medial_Calf_C,Male$Medial_Calf_US1,
                       method = "pearson",alternative = "two.sided")
cor_male_calf_1
cor_male_calf_2 <- cor.test(Male$Medial_Calf_C,Male$Medial_Calf_US2,
                       method = "pearson",alternative = "two.sided")
cor_male_calf_2
cor_male_calf_3 <- cor.test(Male$Medial_Calf_C,Male$Medial_Calf_US1_2,
                       method = "pearson",alternative = "two.sided")
cor_male_calf_3
cor_male_calf_4 <- cor.test(Male$Medial_Calf_C,Male$Medial_Calf_US2_2,
                       method = "pearson",alternative = "two.sided")
cor_male_calf_4

#Female
cor_female_calf_1 <- cor.test(Female$Medial_Calf_C,Female$Medial_Calf_US1,
                            method = "pearson",alternative = "two.sided")
cor_female_calf_1
cor_female_calf_2 <- cor.test(Female$Medial_Calf_C,Female$Medial_Calf_US2,
                            method = "pearson",alternative = "two.sided")
cor_female_calf_2
cor_female_calf_3 <- cor.test(Female$Medial_Calf_C,Female$Medial_Calf_US1_2,
                            method = "pearson",alternative = "two.sided")
cor_female_calf_3
cor_female_calf_4 <- cor.test(Female$Medial_Calf_C,Female$Medial_Calf_US2_2,
                            method = "pearson",alternative = "two.sided")
cor_female_calf_4


