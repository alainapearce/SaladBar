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
fv_pre_ttest <- t.test(fv_pre ~ rurality, data = salad_bar_dat[salad_bar_dat$fv_selected == 'Y', ])

## fv consumed g
fv_consumed_ttest <- t.test(fv_consumed ~ rurality, data = salad_bar_dat[salad_bar_dat$fv_selected == 'Y', ])

## fv waste g
fv_post_ttest <- t.test(fv_post ~ rurality, data = salad_bar_dat[salad_bar_dat$fv_selected == 'Y', ])

## fv waste prop
fv_post_prop_ttest <- t.test(fv_prop_waste ~ rurality, data = salad_bar_dat[salad_bar_dat$fv_selected == 'Y', ])

## Fruit/Vegetable Selection ####
salad_bar_dat_noout <- salad_bar_dat[salad_bar_dat[['fv_exclude']] == 'N' & salad_bar_dat[['fv_pre_out']] == 'N' & salad_bar_dat[['fv_consumed_out']] == 'N' & salad_bar_dat[['fv_post_out']] == 'N', ]

salad_bar_dat_noout[['fv_selected']] <- factor(salad_bar_dat_noout[['fv_selected']], levels = c('N', 'Y'))
salad_bar_dat_noout[['school_name']] <- factor(salad_bar_dat_noout[['school_name']])

fv_selected_model <- mixed_model(fv_selected ~ scale(age) + gender + race_ethnicity + paid_free_reduced + rurality, random = ~ 1 | school_name, iter_EM = 0, data = salad_bar_dat_noout[salad_bar_dat_noout[['suburb']] == 'N', ], family = binomial(link = 'logit'))

## Fruit/Vegetable Self-Serve ####

fv_selected_model <- lmer(fv_pre ~ grade + gender + race + ethnicity + paid_free_reduced + rurality + (1|school_name), data = salad_bar_dat_noout[salad_bar_dat_noout[['fv_selected']] == 'Y' & salad_bar_dat_noout[['suburb']] == 'N', ])
fv_selected_sum <- summary(fv_selected_model)

## Fruit/Vegetable Consumed ####

fv_consumed_model <- mixed_model(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + rurality, random = ~ 1 | school_name, data = salad_bar_dat_noout[salad_bar_dat_noout[['fv_selected']] == 'Y' & salad_bar_dat_noout[['suburb']] == 'N', ], family = zi.negative.binomial(), zi_fixed = ~ 1)

summary(zeroinfl(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + rurality | school_name, data = salad_bar_dat_noout[salad_bar_dat_noout[['fv_selected']] == 'Y' & salad_bar_dat_noout[['suburb']] == 'N', ]))

fv_consumed_model <- mixed_model(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + rurality, random = ~ 1 | school_name, data = salad_bar_dat_noout[salad_bar_dat_noout[['fv_selected']] == 'Y' & salad_bar_dat_noout[['suburb']] == 'N', ], family = zi.negative.binomial(), zi_fixed = ~ 1)

fv_consumed_model_zirandom <- mixed_model(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + rurality, random = ~ 1 | school_name, data = salad_bar_dat_noout[salad_bar_dat_noout[['fv_selected']] == 'Y' & salad_bar_dat_noout[['suburb']] == 'N', ], family = zi.negative.binomial(), zi_fixed = ~ 1, zi_random = ~ 1 | school_name)

anova(fv_consumed_model, fv_consumed_model_zirandom)

## Fruit/Vegetable Self-Serve ####
fv_waste_model <- mixed_model(fv_post ~ grade + gender + race_ethnicity + paid_free_reduced + rurality, random = ~ 1 | school_name, data = salad_bar_dat_noout[salad_bar_dat_noout[['fv_selected']] == 'Y' & salad_bar_dat_noout[['suburb']] == 'N', ], family = zi.negative.binomial(), zi_fixed = ~ 1)

fv_waste_model_zirandom <- mixed_model(fv_post ~ grade + gender + race_ethnicity + paid_free_reduced + rurality, random = ~ 1 | school_name, data = salad_bar_dat_noout[salad_bar_dat_noout[['fv_selected']] == 'Y' & salad_bar_dat_noout[['suburb']] == 'N', ], family = zi.negative.binomial(), zi_fixed = ~ 1, zi_random = ~ 1 | school_name)

anova(fv_waste_model, fv_waste_model_zirandom)
