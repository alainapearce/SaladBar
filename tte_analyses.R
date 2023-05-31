# This script was written by Alaina Pearce in Spring 2023
# to set up the data from the Salad Bar Study
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

## Demo ###

## gender
gender_chi <- chisq.test(xtabs(~school_type + gender, data = salad_bar_dat_use))

## age
age_mod <- lm(age~school_type, data = salad_bar_dat_use)
age_anova <- Anova(age_mod, type = 3, test.statistic = 'F')
age_emmeans <- emmeans(age_mod, pairwise ~ school_type)

## race/ethnicity
raceethnicity_chi <- chisq.test(xtabs(~school_type + race_ethnicity, data = salad_bar_dat_use))

## frl
frl_chi <- chisq.test(xtabs(~school_type + paid_free_reduced, data = salad_bar_dat_use))

## fv select
fv_selected_chi <- chisq.test(xtabs(~school_type + fv_selected, data = salad_bar_dat_use))

## fv selected g
fv_pre_mod <- lm(fv_pre~school_type, data = salad_bar_dat_use)
fv_pre_anova <- Anova(fv_pre_mod, type = 3, test.statistic = 'F')
fv_pre_emmeans <- emmeans(fv_pre_mod, pairwise ~ school_type)

## fv consumed g
fv_consumed_mod <- lm(fv_consumed~school_type, data = salad_bar_dat_use)
fv_consumed_anova <- Anova(fv_consumed_mod, type = 3, test.statistic = 'F')
fv_consumed_emmeans <- emmeans(fv_consumed_mod, pairwise ~ school_type)

## fv waste g
fv_post_ttest <- t.test(fv_post ~ tte_dat, data = salad_bar_dat[salad_bar_dat$fv_selected == 'Y', ])

fv_post_mod <- lm(fv_post~school_type, data = salad_bar_dat_use)
fv_post_anova <- Anova(fv_post_mod, type = 3, test.statistic = 'F')
fv_post_emmeans <- emmeans(fv_post_mod, pairwise ~ school_type)

## fv waste prop
fv_prop_waste_mod <- lm(fv_prop_waste~school_type, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ])
fv_prop_waste_anova <- Anova(fv_prop_waste_mod, type = 3, test.statistic = 'F')
fv_prop_waste_emmeans <- emmeans(fv_prop_waste_mod, pairwise ~ school_type)

## Zero-Inflated Negative Binomial Models ####

## Fruit/Vegetable Selected ####

# All
fv_served_model <- mixed_model(fv_pre ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_served_sum <- summary(fv_served_model)

fv_served_coef <- as.data.frame(round(fv_served_sum$coef_table, 3))
fv_served_coef$irr <- exp(fv_served_coef$Estimate)
fv_served_coef$irr_se <- exp(fv_served_coef$Std.Err)
fv_served_coef <- fv_served_coef[c(5:6, 4)]
names(fv_served_coef) <- c('irr', 'se_irr', 'p')

fv_served_coef_z <- as.data.frame(round(fv_served_sum$coef_table_zi, 3))
fv_served_coef_z$or <- exp(fv_served_coef_z$Estimate)
fv_served_coef_z$irr_se <- exp(fv_served_coef_z$Std.Err)
fv_served_coef_z <- fv_served_coef_z[c(5:6, 4)]
names(fv_served_coef_z) <- c('or', 'se_or', 'p')

# Elementary
fv_served_ES_model <- mixed_model(fv_pre ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Elementary', ], family = zi.negative.binomial(), iter_EM = 0, max_coef_value = 50, zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_served_ES_sum <- summary(fv_served_ES_model)
fv_served_ES_coef <- as.data.frame(round(fv_served_ES_sum$coef_table, 3))

fv_served_ES_coef$irr <- exp(fv_served_ES_coef$Estimate)
fv_served_ES_coef$irr_se <- exp(fv_served_ES_coef$Std.Err)
fv_served_ES_coef <- fv_served_ES_coef[c(5:6, 4)]
names(fv_served_ES_coef) <- c('irr', 'se_irr', 'p')

fv_served_ES_coef_z <- as.data.frame(round(fv_served_ES_sum$coef_table_zi, 3))
fv_served_ES_coef_z$or <- exp(fv_served_ES_coef_z$Estimate)
fv_served_ES_coef_z$or_se <- exp(fv_served_ES_coef_z$Std.Err)
fv_served_ES_coef_z <- fv_served_ES_coef_z[c(5:6, 4)]
names(fv_served_ES_coef_z) <- c('or', 'se_or', 'p')

# Middle School
fv_served_MS_model <- mixed_model(fv_pre ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Middle School', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_served_MS_sum <- summary(fv_served_MS_model)
fv_served_MS_coef <- as.data.frame(round(fv_served_MS_sum$coef_table, 3))

fv_served_MS_coef$irr <- exp(fv_served_MS_coef$Estimate)
fv_served_MS_coef$irr_se <- exp(fv_served_MS_coef$Std.Err)
fv_served_MS_coef <- fv_served_MS_coef[c(5:6, 4)]
names(fv_served_MS_coef) <- c('irr', 'se_irr', 'p')

fv_served_MS_coef_z <- as.data.frame(round(fv_served_MS_sum$coef_table_zi, 3))
fv_served_MS_coef_z$or <- exp(fv_served_MS_coef_z$Estimate)
fv_served_MS_coef_z$or_se <- exp(fv_served_MS_coef_z$Std.Err)
fv_served_MS_coef_z <- fv_served_MS_coef_z[c(5:6, 4)]
names(fv_served_MS_coef_z) <- c('or', 'se_or', 'p')

# High School
fv_served_HS_model <- mixed_model(fv_pre ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'High School', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_served_HS_sum <- summary(fv_served_HS_model)
fv_served_HS_coef <- as.data.frame(round(fv_served_HS_sum$coef_table, 3))

fv_served_HS_coef$irr <- exp(fv_served_HS_coef$Estimate)
fv_served_HS_coef$irr_se <- exp(fv_served_HS_coef$Std.Err)
fv_served_HS_coef <- fv_served_HS_coef[c(5:6, 4)]
names(fv_served_HS_coef) <- c('irr', 'se_irr', 'p')

fv_served_HS_coef_z <- as.data.frame(round(fv_served_HS_sum$coef_table_zi, 3))
fv_served_HS_coef_z$or <- exp(fv_served_HS_coef_z$Estimate)
fv_served_HS_coef_z$or_se <- exp(fv_served_HS_coef_z$Std.Err)
fv_served_HS_coef_z <- fv_served_HS_coef_z[c(5:6, 4)]
names(fv_served_HS_coef_z) <- c('or', 'se_or', 'p')



## Fruit/Vegetable Consumed ####

# All
fv_consumed_model <- mixed_model(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_consumed_sum <- summary(fv_consumed_model)

fv_consumed_coef <- as.data.frame(round(fv_consumed_sum$coef_table, 3))
fv_consumed_coef$irr <- exp(fv_consumed_coef$Estimate)
fv_consumed_coef$irr_se <- exp(fv_consumed_coef$Std.Err)
fv_consumed_coef <- fv_consumed_coef[c(5:6, 4)]
names(fv_consumed_coef) <- c('irr', 'se_irr', 'p')

fv_consumed_coef_z <- as.data.frame(round(fv_consumed_sum$coef_table_zi, 3))
fv_consumed_coef_z$or <- exp(fv_consumed_coef_z$Estimate)
fv_consumed_coef_z$irr_se <- exp(fv_consumed_coef_z$Std.Err)
fv_consumed_coef_z <- fv_consumed_coef_z[c(5:6, 4)]
names(fv_consumed_coef_z) <- c('or', 'se_or', 'p')

# Elementary
fv_consumed_ES_model <- mixed_model(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Elementary', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_consumed_ES_sum <- summary(fv_consumed_ES_model)
fv_consumed_ES_coef <- as.data.frame(round(fv_consumed_ES_sum$coef_table, 3))

fv_consumed_ES_coef$irr <- exp(fv_consumed_ES_coef$Estimate)
fv_consumed_ES_coef$irr_se <- exp(fv_consumed_ES_coef$Std.Err)
fv_consumed_ES_coef <- fv_consumed_ES_coef[c(5:6, 4)]
names(fv_consumed_ES_coef) <- c('irr', 'se_irr', 'p')

fv_consumed_ES_coef_z <- as.data.frame(round(fv_consumed_ES_sum$coef_table_zi, 3))
fv_consumed_ES_coef_z$or <- exp(fv_consumed_ES_coef_z$Estimate)
fv_consumed_ES_coef_z$or_se <- exp(fv_consumed_ES_coef_z$Std.Err)
fv_consumed_ES_coef_z <- fv_consumed_ES_coef_z[c(5:6, 4)]
names(fv_consumed_ES_coef_z) <- c('or', 'se_or', 'p')

# Middle School
fv_consumed_MS_model <- mixed_model(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Middle School', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_consumed_MS_sum <- summary(fv_consumed_MS_model)
fv_consumed_MS_coef <- as.data.frame(round(fv_consumed_MS_sum$coef_table, 3))

fv_consumed_MS_coef$irr <- exp(fv_consumed_MS_coef$Estimate)
fv_consumed_MS_coef$irr_se <- exp(fv_consumed_MS_coef$Std.Err)
fv_consumed_MS_coef <- fv_consumed_MS_coef[c(5:6, 4)]
names(fv_consumed_MS_coef) <- c('irr', 'se_irr', 'p')

fv_consumed_MS_coef_z <- as.data.frame(round(fv_consumed_MS_sum$coef_table_zi, 3))
fv_consumed_MS_coef_z$or <- exp(fv_consumed_MS_coef_z$Estimate)
fv_consumed_MS_coef_z$or_se <- exp(fv_consumed_MS_coef_z$Std.Err)
fv_consumed_MS_coef_z <- fv_consumed_MS_coef_z[c(5:6, 4)]
names(fv_consumed_MS_coef_z) <- c('or', 'se_or', 'p')

# High School
fv_consumed_HS_model <- mixed_model(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'High School', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_consumed_HS_sum <- summary(fv_consumed_HS_model)
fv_consumed_HS_coef <- as.data.frame(round(fv_consumed_HS_sum$coef_table, 3))

fv_consumed_HS_coef$irr <- exp(fv_consumed_HS_coef$Estimate)
fv_consumed_HS_coef$irr_se <- exp(fv_consumed_HS_coef$Std.Err)
fv_consumed_HS_coef <- fv_consumed_HS_coef[c(5:6, 4)]
names(fv_consumed_HS_coef) <- c('irr', 'se_irr', 'p')

fv_consumed_HS_coef_z <- as.data.frame(round(fv_consumed_HS_sum$coef_table_zi, 3))
fv_consumed_HS_coef_z$or <- exp(fv_consumed_HS_coef_z$Estimate)
fv_consumed_HS_coef_z$or_se <- exp(fv_consumed_HS_coef_z$Std.Err)
fv_consumed_HS_coef_z <- fv_consumed_HS_coef_z[c(5:6, 4)]
names(fv_consumed_HS_coef_z) <- c('or', 'se_or', 'p')

## Fruit/Vegetable Waste ####

# All
fv_waste_model <- mixed_model(fv_post ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_waste_sum <- summary(fv_waste_model)

fv_waste_coef <- as.data.frame(round(fv_waste_sum$coef_table, 3))
fv_waste_coef$irr <- exp(fv_waste_coef$Estimate)
fv_waste_coef$irr_se <- exp(fv_waste_coef$Std.Err)
fv_waste_coef <- fv_waste_coef[c(5:6, 4)]
names(fv_waste_coef) <- c('irr', 'se_irr', 'p')

fv_waste_coef_z <- as.data.frame(round(fv_waste_sum$coef_table_zi, 3))
fv_waste_coef_z$or <- exp(fv_waste_coef_z$Estimate)
fv_waste_coef_z$irr_se <- exp(fv_waste_coef_z$Std.Err)
fv_waste_coef_z <- fv_waste_coef_z[c(5:6, 4)]
names(fv_waste_coef_z) <- c('or', 'se_or', 'p')

# Elementary
fv_waste_ES_model <- mixed_model(fv_post ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Elementary', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_waste_ES_sum <- summary(fv_waste_ES_model)
fv_waste_ES_coef <- as.data.frame(round(fv_waste_ES_sum$coef_table, 3))

fv_waste_ES_coef$irr <- exp(fv_waste_ES_coef$Estimate)
fv_waste_ES_coef$irr_se <- exp(fv_waste_ES_coef$Std.Err)
fv_waste_ES_coef <- fv_waste_ES_coef[c(5:6, 4)]
names(fv_waste_ES_coef) <- c('irr', 'se_irr', 'p')

fv_waste_ES_coef_z <- as.data.frame(round(fv_waste_ES_sum$coef_table_zi, 3))
fv_waste_ES_coef_z$or <- exp(fv_waste_ES_coef_z$Estimate)
fv_waste_ES_coef_z$or_se <- exp(fv_waste_ES_coef_z$Std.Err)
fv_waste_ES_coef_z <- fv_waste_ES_coef_z[c(5:6, 4)]
names(fv_waste_ES_coef_z) <- c('or', 'se_or', 'p')

# Middle School
fv_waste_MS_model <- mixed_model(fv_post ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Middle School', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_waste_MS_sum <- summary(fv_waste_MS_model)
fv_waste_MS_coef <- as.data.frame(round(fv_waste_MS_sum$coef_table, 3))

fv_waste_MS_coef$irr <- exp(fv_waste_MS_coef$Estimate)
fv_waste_MS_coef$irr_se <- exp(fv_waste_MS_coef$Std.Err)
fv_waste_MS_coef <- fv_waste_MS_coef[c(5:6, 4)]
names(fv_waste_MS_coef) <- c('irr', 'se_irr', 'p')

fv_waste_MS_coef_z <- as.data.frame(round(fv_waste_MS_sum$coef_table_zi, 3))
fv_waste_MS_coef_z$or <- exp(fv_waste_MS_coef_z$Estimate)
fv_waste_MS_coef_z$or_se <- exp(fv_waste_MS_coef_z$Std.Err)
fv_waste_MS_coef_z <- fv_waste_MS_coef_z[c(5:6, 4)]
names(fv_waste_MS_coef_z) <- c('or', 'se_or', 'p')

# High School
fv_waste_HS_model <- mixed_model(fv_post ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'High School', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_waste_HS_sum <- summary(fv_waste_HS_model)
fv_waste_HS_coef <- as.data.frame(round(fv_waste_HS_sum$coef_table, 3))

fv_waste_HS_coef$irr <- exp(fv_waste_HS_coef$Estimate)
fv_waste_HS_coef$irr_se <- exp(fv_waste_HS_coef$Std.Err)
fv_waste_HS_coef <- fv_waste_HS_coef[c(5:6, 4)]
names(fv_waste_HS_coef) <- c('irr', 'se_irr', 'p')

fv_waste_HS_coef_z <- as.data.frame(round(fv_waste_HS_sum$coef_table_zi, 3))
fv_waste_HS_coef_z$or <- exp(fv_waste_HS_coef_z$Estimate)
fv_waste_HS_coef_z$or_se <- exp(fv_waste_HS_coef_z$Std.Err)
fv_waste_HS_coef_z <- fv_waste_HS_coef_z[c(5:6, 4)]
names(fv_waste_HS_coef_z) <- c('or', 'se_or', 'p')

## Fruit/Vegetable Waste Proportion ####
# All
fv_prop_waste_model <- mixed_model(fv_prop_waste ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_prop_waste_sum <- summary(fv_prop_waste_model)

fv_prop_waste_coef <- as.data.frame(round(fv_prop_waste_sum$coef_table, 3))
fv_prop_waste_coef$irr <- exp(fv_prop_waste_coef$Estimate)
fv_prop_waste_coef$irr_se <- exp(fv_prop_waste_coef$Std.Err)
fv_prop_waste_coef <- fv_prop_waste_coef[c(5:6, 4)]
names(fv_prop_waste_coef) <- c('irr', 'se_irr', 'p')

fv_prop_waste_coef_z <- as.data.frame(round(fv_prop_waste_sum$coef_table_zi, 3))
fv_prop_waste_coef_z$or <- exp(fv_prop_waste_coef_z$Estimate)
fv_prop_waste_coef_z$irr_se <- exp(fv_prop_waste_coef_z$Std.Err)
fv_prop_waste_coef_z <- fv_prop_waste_coef_z[c(5:6, 4)]
names(fv_prop_waste_coef_z) <- c('or', 'se_or', 'p')

# Elementary
fv_prop_waste_ES_model <- mixed_model(fv_prop_waste ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Elementary' & salad_bar_dat_use$fv_selected == 'Y', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_prop_waste_ES_sum <- summary(fv_prop_waste_ES_model)
fv_prop_waste_ES_coef <- as.data.frame(round(fv_prop_waste_ES_sum$coef_table, 3))

fv_prop_waste_ES_coef$irr <- exp(fv_prop_waste_ES_coef$Estimate)
fv_prop_waste_ES_coef$irr_se <- exp(fv_prop_waste_ES_coef$Std.Err)
fv_prop_waste_ES_coef <- fv_prop_waste_ES_coef[c(5:6, 4)]
names(fv_prop_waste_ES_coef) <- c('irr', 'se_irr', 'p')

fv_prop_waste_ES_coef_z <- as.data.frame(round(fv_prop_waste_ES_sum$coef_table_zi, 3))
fv_prop_waste_ES_coef_z$or <- exp(fv_prop_waste_ES_coef_z$Estimate)
fv_prop_waste_ES_coef_z$or_se <- exp(fv_prop_waste_ES_coef_z$Std.Err)
fv_prop_waste_ES_coef_z <- fv_prop_waste_ES_coef_z[c(5:6, 4)]
names(fv_prop_waste_ES_coef_z) <- c('or', 'se_or', 'p')

# Middle School
fv_prop_waste_MS_model <- mixed_model(fv_prop_waste ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Middle School' & salad_bar_dat_use$fv_selected == 'Y', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_prop_waste_MS_sum <- summary(fv_prop_waste_MS_model)
fv_prop_waste_MS_coef <- as.data.frame(round(fv_prop_waste_MS_sum$coef_table, 3))

fv_prop_waste_MS_coef$irr <- exp(fv_prop_waste_MS_coef$Estimate)
fv_prop_waste_MS_coef$irr_se <- exp(fv_prop_waste_MS_coef$Std.Err)
fv_prop_waste_MS_coef <- fv_prop_waste_MS_coef[c(5:6, 4)]
names(fv_prop_waste_MS_coef) <- c('irr', 'se_irr', 'p')

fv_prop_waste_MS_coef_z <- as.data.frame(round(fv_prop_waste_MS_sum$coef_table_zi, 3))
fv_prop_waste_MS_coef_z$or <- exp(fv_prop_waste_MS_coef_z$Estimate)
fv_prop_waste_MS_coef_z$or_se <- exp(fv_prop_waste_MS_coef_z$Std.Err)
fv_prop_waste_MS_coef_z <- fv_prop_waste_MS_coef_z[c(5:6, 4)]
names(fv_prop_waste_MS_coef_z) <- c('or', 'se_or', 'p')

# High School
fv_prop_waste_HS_model <- mixed_model(fv_prop_waste ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'High School' & salad_bar_dat_use$fv_selected == 'Y', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_prop_waste_HS_sum <- summary(fv_prop_waste_HS_model)
fv_prop_waste_HS_coef <- as.data.frame(round(fv_prop_waste_HS_sum$coef_table, 3))

fv_prop_waste_HS_coef$irr <- exp(fv_prop_waste_HS_coef$Estimate)
fv_prop_waste_HS_coef$irr_se <- exp(fv_prop_waste_HS_coef$Std.Err)
fv_prop_waste_HS_coef <- fv_prop_waste_HS_coef[c(5:6, 4)]
names(fv_prop_waste_HS_coef) <- c('irr', 'se_irr', 'p')

fv_prop_waste_HS_coef_z <- as.data.frame(round(fv_prop_waste_HS_sum$coef_table_zi, 3))
fv_prop_waste_HS_coef_z$or <- exp(fv_prop_waste_HS_coef_z$Estimate)
fv_prop_waste_HS_coef_z$or_se <- exp(fv_prop_waste_HS_coef_z$Std.Err)
fv_prop_waste_HS_coef_z <- fv_prop_waste_HS_coef_z[c(5:6, 4)]
names(fv_prop_waste_HS_coef_z) <- c('or', 'se_or', 'p')