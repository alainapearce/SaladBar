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
gender_chi <- chisq.test(xtabs(~rurality + gender, data = salad_bar_dat))

## age
age_ttest <- t.test(age~rurality, data = salad_bar_dat)

## grade
grade_ttest <- t.test(grade ~ rurality, data = salad_bar_dat)

## race
race_chi <- chisq.test(xtabs(~rurality + race, data = salad_bar_dat))

## ethnicity
ethnicity_chi <- chisq.test(xtabs(~rurality + ethnicity, data = salad_bar_dat))

## frl
frl_chi <- chisq.test(xtabs(~rurality + paid_free_reduced, data = salad_bar_dat))

## fv select
fv_select_chi <- chisq.test(xtabs(~rurality + fv_selected, data = salad_bar_dat))

## fv selected g
fv_pre_ttest <- t.test(fv_pre ~ rurality, data = salad_bar_dat[salad_bar_dat[['fv_selected']] & salad_bar_dat[['fv_exclude']] == 'N', ])

## fv consumed g
fv_consumed_ttest <- t.test(fv_consumed ~ rurality, data = salad_bar_dat[salad_bar_dat[['fv_selected']] & salad_bar_dat[['fv_exclude']] == 'N', ])

## fv waste g
fv_post_ttest <- t.test(fv_post ~ rurality, data = salad_bar_dat[salad_bar_dat[['fv_selected']] & salad_bar_dat[['fv_exclude']] == 'N', ])

## fv waste prop
fv_post_prop_ttest <- t.test(fv_prop_waste ~ rurality, data = salad_bar_dat[salad_bar_dat[['fv_selected']] & salad_bar_dat[['fv_exclude']] == 'N', ])

## Fruit/Vegetable Selection ####

# Y/N_binomial
# salad_bar_dat[['fv_selected_dummy']] <- ifelse(salad_bar_dat[['fv_selected']] == 'Y', 1, 0)
# 
# fv_selected_model <- mixed_model(fv_selected_dummy ~ grade + gender + race_ethnicity + paid_free_reduced + rurality, random = ~ 1 | school_name, iter_EM = 0, data = salad_bar_dat[salad_bar_dat[['fv_exclude']] == 'N', ], family = binomial(link = 'logit'))

#center grade at 1st grade
salad_bar_dat[['grade_c1']] <- salad_bar_dat[['grade']] - 1

fv_selected_model <- mixed_model(fv_pre ~ gender + race_ethnicity + paid_free_reduced + rurality*grade_c1, random = ~ 1 | school_name, data = salad_bar_dat[salad_bar_dat[['fv_exclude']] == 'N', ], family = zi.negative.binomial(), zi_fixed = ~ gender + race_ethnicity + paid_free_reduced + rurality*grade_c1, zi_random = ~ 1 | school_name)

fv_selected_sum <- summary(fv_selected_model)

fv_selected_coef <- as.data.frame(round(fv_selected_sum$coef_table, 3))
fv_selected_coef$irr <- exp(fv_selected_coef$Estimate)
fv_selected_coef$irr_se <- exp(fv_selected_coef$Std.Err)
fv_selected_coef <- fv_selected_coef[c(1:2, 5:6, 3:4)]
names(fv_selected_coef) <- c('beta', 'se', 'irr', 'se_irr', 'z', 'p')

fv_selected_coef_zi <- as.data.frame(round(fv_selected_sum$coef_table_zi, 3))
fv_selected_coef_zi$or <- exp(fv_selected_coef_zi$Estimate)
fv_selected_coef_zi$irr_se <- exp(fv_selected_coef_zi$Std.Err)
fv_selected_coef_zi <- fv_selected_coef_zi[c(1:2, 5:6, 3:4)]
names(fv_selected_coef_zi) <- c('beta', 'se', 'or', 'se_or', 'z', 'p')

# determine range where rurality is significant
fv_selected_emmeans <- emmeans(fv_selected_model, pairwise ~ rurality | grade_c1, at = list(grade_c1 = 0:11), adjust = 'none')


## Fruit/Vegetable Consumed ####
fv_consumed_model <- mixed_model(fv_consumed ~ gender + race_ethnicity + paid_free_reduced + rurality*grade_c1, random = ~ 1 | school_name, data = salad_bar_dat[salad_bar_dat[['fv_exclude']] == 'N', ], family = zi.negative.binomial(), zi_fixed = ~ gender + race_ethnicity + paid_free_reduced + rurality*grade_c1, zi_random = ~ 1 | school_name)

fv_consumed_sum <- summary(fv_consumed_model)

fv_consumed_coef <- as.data.frame(round(fv_consumed_sum$coef_table, 3))
fv_consumed_coef$irr <- exp(fv_consumed_coef$Estimate)
fv_consumed_coef$irr_se <- exp(fv_consumed_coef$Std.Err)
fv_consumed_coef <- fv_consumed_coef[c(1:2, 5:6, 3:4)]
names(fv_consumed_coef) <- c('beta', 'se', 'irr', 'se_irr', 'z', 'p')

fv_consumed_coef_zi <- as.data.frame(round(fv_consumed_sum$coef_table_zi, 3))
fv_consumed_coef_zi$or <- exp(fv_consumed_coef_zi$Estimate)
fv_consumed_coef_zi$irr_se <- exp(fv_consumed_coef_zi$Std.Err)
fv_consumed_coef_zi <- fv_consumed_coef_zi[c(1:2, 5:6, 3:4)]
names(fv_consumed_coef_zi) <- c('beta', 'se', 'or', 'se_or', 'z', 'p')

# determine range where rurality is significant
fv_consumed_emmeans <- emmeans(fv_consumed_model, pairwise ~ rurality | grade_c1, at = list(grade_c1 = 0:11), adjust = 'none')

## Fruit/Vegetable Waste ####
fv_waste_model <- mixed_model(fv_post ~ gender + race_ethnicity + paid_free_reduced + rurality*grade_c1, random = ~ 1 | school_name, data = salad_bar_dat[salad_bar_dat[['fv_selected']] == 'Y' & salad_bar_dat[['fv_exclude']] == 'N', ], family = zi.negative.binomial(), zi_fixed = ~ gender + race_ethnicity + paid_free_reduced + rurality*grade_c1, zi_random = ~ 1 | school_name)

fv_waste_sum <- summary(fv_waste_model)

fv_waste_coef <- as.data.frame(round(fv_waste_sum$coef_table, 3))
fv_waste_coef$irr <- exp(fv_waste_coef$Estimate)
fv_waste_coef$irr_se <- exp(fv_waste_coef$Std.Err)
fv_waste_coef <- fv_waste_coef[c(1:2, 5:6, 3:4)]
names(fv_waste_coef) <- c('beta', 'se', 'irr', 'se_irr', 'z', 'p')

fv_waste_coef_zi <- as.data.frame(round(fv_waste_sum$coef_table_zi, 3))
fv_waste_coef_zi$or <- exp(fv_waste_coef_zi$Estimate)
fv_waste_coef_zi$irr_se <- exp(fv_waste_coef_zi$Std.Err)
fv_waste_coef_zi <- fv_waste_coef_zi[c(1:2, 5:6, 3:4)]
names(fv_waste_coef_zi) <- c('beta', 'se', 'or', 'se_or', 'z', 'p')


## Fruit/Vegetable Proportion Waste ####
fv_prop_waste_model <- mixed_model(fv_prop_waste ~ gender + race_ethnicity + paid_free_reduced + rurality*grade_c1, random = ~ 1 | school_name, data = salad_bar_dat[salad_bar_dat[['fv_selected']] == 'Y' & salad_bar_dat[['fv_exclude']] == 'N', ], family = zi.negative.binomial(), zi_fixed = ~ gender + race_ethnicity + paid_free_reduced + rurality*grade_c1, zi_random = ~ 1 | school_name)

fv_prop_waste_sum <- summary(fv_prop_waste_model)

fv_prop_waste_coef <- as.data.frame(round(fv_prop_waste_sum$coef_table, 3))
fv_prop_waste_coef$irr <- exp(fv_prop_waste_coef$Estimate)
fv_prop_waste_coef$irr_se <- exp(fv_prop_waste_coef$Std.Err)
fv_prop_waste_coef <- fv_prop_waste_coef[c(1:2, 5:6, 3:4)]
names(fv_prop_waste_coef) <- c('beta', 'se', 'irr', 'se_irr', 'z', 'p')

fv_prop_waste_coef_zi <- as.data.frame(round(fv_prop_waste_sum$coef_table_zi, 3))
fv_prop_waste_coef_zi$or <- exp(fv_prop_waste_coef_zi$Estimate)
fv_prop_waste_coef_zi$irr_se <- exp(fv_prop_waste_coef_zi$Std.Err)
fv_prop_waste_coef_zi <- fv_prop_waste_coef_zi[c(1:2, 5:6, 3:4)]
names(fv_prop_waste_coef_zi) <- c('beta', 'se', 'or', 'se_or', 'z', 'p')

# determine range where rurality is significant
fv_prop_waste_emmeans <- emmeans(fv_prop_waste_model, pairwise ~ rurality | grade_c1, at = list(grade_c1 = 0:11), adjust = 'none')
