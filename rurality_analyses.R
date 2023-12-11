# This script was written by Alaina Pearce in Spring 2023
# to analyze the rurality data from the Salad Bar Study
#
#     Copyright (C) 2023 Alaina L Pearce
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.

############ Basic Data Load/Setup ############
# need to uncomment if running independently - not needed if compiling with 2022-01-27_PSU_CBBCsem.Rmd

# library(GLMMadaptive)
# library(lme4)
# library(lmerTest)

# source('setup.R')
# source('functions.R')

## reduced to usable data ####

salad_bar_dat_use <- salad_bar_dat[!is.na(salad_bar_dat[['fv_pre']]) & !is.na(salad_bar_dat[['fv_post']]), ]

#reduce to complete data
salad_bar_dat_use <- salad_bar_dat_use[!is.na(salad_bar_dat_use[['gender']]) & !is.na(salad_bar_dat_use[["race_ethnicity"]]) & !is.na(salad_bar_dat_use[["grade"]]) & !is.na(salad_bar_dat_use[["paid_free_reduced"]]) & !is.na(salad_bar_dat_use[["lunch_dur"]]), ]

## Demo ###

## gender
gender_chi <- chisq.test(xtabs(~rurality + gender, data = salad_bar_dat_use))

## age
age_mod <- lm(age~rurality, data = salad_bar_dat_use)
age_anova <- Anova(age_mod, type = 3, test.statistic = 'F')
age_emmeans <- emmeans(age_mod, pairwise ~ rurality)

## grade
grade_mod <- lm(grade~rurality, data = salad_bar_dat_use)
grade_anova <- Anova(grade_mod, type = 3, test.statistic = 'F')
grade_emmeans <- emmeans(grade_mod, pairwise ~ rurality)

## race/ethnicity
raceethnicity_chi <- chisq.test(xtabs(~rurality + race_ethnicity, data = salad_bar_dat_use))

## frl
frl_chi <- chisq.test(xtabs(~rurality + paid_free_reduced, data = salad_bar_dat_use))

## fv select
fv_selected_chi <- chisq.test(xtabs(~rurality + fv_selected, data = salad_bar_dat_use))

## school level
school_chi <- chisq.test(xtabs(~rurality + school_type, data = salad_bar_dat_use))


## lunch duration
lunch_dur_mod <- lmer(lunch_dur~rurality + (1|school_name), data = salad_bar_dat_use)
lunch_dur_anova <- anova(lunch_dur_mod, test.statistic = 'F')
lunch_dur_emmeans <- emmeans(lunch_dur_mod, pairwise ~ rurality)

## fv selected g
fv_pre_mod <- lmer(fv_pre~rurality + (1|school_name), data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ])
fv_pre_anova <- Anova(fv_pre_mod, type = 3, test.statistic = 'F')
fv_pre_emmeans <- emmeans(fv_pre_mod, pairwise ~ rurality)

## fv consumed g
fv_consumed_mod <- lmer(fv_consumed~rurality + (1|school_name), data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ])
fv_consumed_anova <- Anova(fv_consumed_mod, type = 3, test.statistic = 'F')
fv_consumed_emmeans <- emmeans(fv_consumed_mod, pairwise ~ rurality)

## fv waste g
fv_waste_mod <- lmer(fv_post~rurality + (1|school_name), data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ])
fv_waste_anova <- Anova(fv_waste_mod, type = 3, test.statistic = 'F')
fv_waste_emmeans <- emmeans(fv_waste_mod, pairwise ~ rurality)

## fv waste prop
fv_prop_waste_mod <- lmer(fv_prop_waste~rurality + (1|school_name), data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ])
fv_prop_waste_anova <- Anova(fv_prop_waste_mod, type = 3, test.statistic = 'F')
fv_prop_waste_emmeans <- emmeans(fv_prop_waste_mod, pairwise ~ rurality)


## Primary values

salad_bar_dat_use[['rurality']] <- factor(salad_bar_dat_use[["rurality"]])

salad_bar_dat_use$school_type <- factor(salad_bar_dat_use$school_type, levels = c('Elementary', 'Middle School', 'High School'))

## Fruit/Vegetable Selected ####

# Test Interaction
fv_served_int_model <- mixed_model(fv_pre ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality, zi_random = ~ 1 | school_name, iter_EM = 0)

fv_served_int_sum <- summary(fv_served_int_model)


# All
fv_served_model <- mixed_model(fv_pre ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, zi_random = ~ 1 | school_name)

fv_served_sum <- summary(fv_served_model)

fv_served_gender_model <- mixed_model(fv_pre ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, zi_random = ~ 1 | school_name)

fv_served_gender_sum <- summary(fv_served_gender_model)


fv_served_gender_fixed_emm <-emmeans(fv_served_gender_model,  ~gender | rurality, mode = 'fixed-effects', type = 'response')

# fv_served_gender_fixed_posthoc <-pairs(fv_served_gender_fixed_emm, infer = FALSE)
# fv_served_gender_zi_contrasts <- contrast(emmeans(fv_served_gender_model, ~gender | rurality, mode = 'zero_part', type = 'response'), 'revpairwise', infer = FALSE)

# fv_served_grade_model <- mixed_model(fv_pre ~ gender + race_ethnicity + paid_free_reduced + lunch_dur + grade*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ gender + race_ethnicity + paid_free_reduced + lunch_dur + grade*rurality, zi_random = ~ 1 | school_name)
# 
# fv_served_grade_sum <- summary(fv_served_grade_model)
# 
# fv_served_grade_fixed_emm <- emmeans(fv_served_grade_model,  ~rurality|grade, var = 'grade', mode = 'fixed-effects', at = list(grade = c(1, 5, 7, 9, 12)), type = 'response')
# 
# fv_served_grade_fixed_gcon <- pairs(emmeans(fv_served_grade_model,  ~grade|rurality, at = list(grade = c(12, 1)), mode = 'fixed-effects', type = 'response', infer = TRUE))
# 
# fv_served_grade_fixed_rcon <- pairs(emmeans(fv_served_grade_model,  ~rurality|grade, at = list(grade = c(1, 12)), mode = 'fixed-effects', type = 'response', infer = TRUE))

## Fruit/Vegetable Consumed ####

# Test Interaction
fv_consumed_int_model <- mixed_model(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality, zi_random = ~ 1 | school_name, iter_EM = 0)

fv_consumed_int_sum <- summary(fv_consumed_int_model)

# All
fv_consumed_model <- mixed_model(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, zi_random = ~ 1 | school_name)

fv_consumed_sum <- summary(fv_consumed_model)

fv_consumed_gender_model <- mixed_model(fv_consumed ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ], family = zi.negative.binomial(), zi_fixed = ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, zi_random = ~ 1 | school_name)

fv_consumed_gender_sum <- summary(fv_consumed_gender_model)

fv_consumed_grade_model <- mixed_model(fv_consumed ~ gender + race_ethnicity + paid_free_reduced + lunch_dur + grade*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ], family = zi.negative.binomial(), zi_fixed = ~ gender + race_ethnicity + paid_free_reduced + lunch_dur + grade*rurality, zi_random = ~ 1 | school_name)

# fv_consumed_grade_sum <- summary(fv_consumed_grade_model)
# 
# fv_consumed_grade_fixed_emm <- emmeans(fv_consumed_grade_model,  ~rurality|grade, var = 'grade', mode = 'fixed-effects', at = list(grade = c(1, 5, 7, 9, 12)), type = 'response')
# 
# fv_consumed_grade_fixed_gcon <- pairs(emmeans(fv_consumed_grade_model,  ~grade|rurality, at = list(grade = c(12, 1)), mode = 'fixed-effects', type = 'response', infer = TRUE))
# 
# fv_consumed_grade_fixed_rcon <- pairs(emmeans(fv_consumed_grade_model,  ~rurality|grade, at = list(grade = c(1, 12)), mode = 'fixed-effects', type = 'response', infer = TRUE))

## Fruit/Vegetable Waste Proportion ####

# Test Interaction
fv_prop_waste_int_model <- mixed_model(fv_prop_waste ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality, zi_random = ~ 1 | school_name, iter_EM = 0)

fv_prop_waste_int_sum <- summary(fv_prop_waste_int_model)

# All
fv_prop_waste_model <- mixed_model(fv_prop_waste ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, zi_random = ~ 1 | school_name)

fv_prop_waste_sum <- summary(fv_prop_waste_model)

fv_prop_waste_gender_model <- mixed_model(fv_prop_waste ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, zi_random = ~ 1 | school_name)

fv_prop_waste_gender_sum <- summary(fv_prop_waste_gender_model)

fv_prop_waste_grade_model <- mixed_model(fv_prop_waste ~ gender + race_ethnicity + paid_free_reduced + lunch_dur + grade*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ gender + race_ethnicity + paid_free_reduced + lunch_dur + grade*rurality, zi_random = ~ 1 | school_name)

fv_prop_waste_grade_sum <- summary(fv_prop_waste_grade_model)

## Supplement by school level ####
# 
# ## Fruit/Vegetable Selected ####
# 
# # Elementary
# 
# ## had to remove race_ethnicity from zero model
# 
# fv_served_ES_model <- mixed_model(fv_pre ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Elementary', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + paid_free_reduced + lunch_dur + rurality, zi_random = ~ 1 | school_name)
# 
# fv_served_ES_sum <- summary(fv_served_ES_model)
# 
# fv_served_gender_ES_model <- mixed_model(fv_pre ~ grade + paid_free_reduced + lunch_dur + gender*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Elementary', ], family = zi.negative.binomial(), zi_fixed = ~ grade + paid_free_reduced + lunch_dur + gender*rurality, zi_random = ~ 1 | school_name)
# 
# fv_served_gender_ES_sum <- summary(fv_served_gender_ES_model)
# 
# # Middle School
# fv_served_MS_model <- mixed_model(fv_pre ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Middle School', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, zi_random = ~ 1 | school_name)
# 
# fv_served_MS_sum <- summary(fv_served_MS_model)
# 
# fv_served_gender_MS_model <- mixed_model(fv_pre ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Middle School', ], family = zi.negative.binomial(), zi_fixed = ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, zi_random = ~ 1 | school_name)
# 
# fv_served_gender_MS_sum <- summary(fv_served_gender_MS_model)
# 
# # High School
# fv_served_HS_model <- mixed_model(fv_pre ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'High School', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, zi_random = ~ 1 | school_name)
# 
# fv_served_HS_sum <- summary(fv_served_HS_model)
# 
# fv_served_gender_HS_model <- mixed_model(fv_pre ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'High School', ], family = zi.negative.binomial(), zi_fixed = ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, zi_random = ~ 1 | school_name)
# 
# fv_served_gender_HS_sum <- summary(fv_served_gender_HS_model)
# 
# ## Fruit/Vegetable Consumed ####
# 
# # Elementary
# fv_consumed_ES_model <- mixed_model(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$school_type == 'Elementary', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, zi_random = ~ 1 | school_name)
# 
# fv_consumed_ES_sum <- summary(fv_consumed_ES_model)
# 
# fv_consumed_gender_ES_model <- mixed_model(fv_consumed ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$school_type == 'Elementary', ], family = zi.negative.binomial(), zi_fixed = ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, zi_random = ~ 1 | school_name)
# 
# fv_consumed_gender_ES_sum <- summary(fv_consumed_gender_ES_model)
# 
# # Middle School
# fv_consumed_MS_model <- mixed_model(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$school_type == 'Middle School', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, zi_random = ~ 1 | school_name)
# 
# fv_consumed_MS_sum <- summary(fv_consumed_MS_model)
# 
# fv_consumed_gender_MS_model <- mixed_model(fv_consumed ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$school_type == 'Middle School', ], family = zi.negative.binomial(), zi_fixed = ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, zi_random = ~ 1 | school_name)
# 
# fv_consumed_gender_MS_sum <- summary(fv_consumed_gender_MS_model)
# 
# # High School
# fv_consumed_HS_model <- mixed_model(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$school_type == 'High School', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, zi_random = ~ 1 | school_name)
# 
# fv_consumed_HS_sum <- summary(fv_consumed_HS_model)
# 
# fv_consumed_gender_HS_model <- mixed_model(fv_consumed ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$school_type == 'High School', ], family = zi.negative.binomial(), zi_fixed = ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, zi_random = ~ 1 | school_name)
# 
# fv_consumed_gender_HS_sum <- summary(fv_consumed_gender_HS_model)
# 
# ## Fruit/Vegetable Waste Proportion ####
# 
# # Elementary
# ## 0 rural children had 0 percent waste
# 
# # fv_prop_waste_ES_model <- mixed_model(fv_prop_waste ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Elementary' & salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + paid_free_reduced + lunch_dur + rurality, zi_random = ~ 1 | school_name)
# 
# fv_prop_waste_ES_model <- lmer(fv_prop_waste ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality + (1|school_name), data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Elementary' & salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ])
# 
# fv_prop_waste_ES_sum <- summary(fv_prop_waste_ES_model)
# 
# # fv_prop_waste_gender_ES_model <- mixed_model(fv_prop_waste ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Elementary' & salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, zi_random = ~ 1 | school_name)
# 
# fv_prop_waste_gender_ES_model <- lmer(fv_prop_waste ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality + (1|school_name), data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Elementary' & salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ])
# 
# fv_prop_waste_gender_ES_sum <- summary(fv_prop_waste_gender_ES_model)
# 
# # Middle School
# fv_prop_waste_MS_model <- mixed_model(fv_prop_waste ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Middle School' & salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, zi_random = ~ 1 | school_name)
# 
# fv_prop_waste_MS_sum <- summary(fv_prop_waste_MS_model)
# 
# fv_prop_waste_gender_MS_model <- mixed_model(fv_prop_waste ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Middle School' & salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, zi_random = ~ 1 | school_name)
# 
# fv_prop_waste_gender_MS_sum <- summary(fv_prop_waste_gender_MS_model)
# 
# # High School
# fv_prop_waste_HS_model <- mixed_model(fv_prop_waste ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'High School' & salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + rurality, zi_random = ~ 1 | school_name)
# 
# fv_prop_waste_HS_sum <- summary(fv_prop_waste_HS_model)
# 
# fv_prop_waste_gender_HS_model <- mixed_model(fv_prop_waste ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'High School' & salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ grade + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, zi_random = ~ 1 | school_name)
# 
# fv_prop_waste_gender_HS_sum <- summary(fv_prop_waste_gender_HS_model)
