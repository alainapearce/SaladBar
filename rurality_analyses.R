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

# Primary Variables
salad_bar_dat_use[['rurality']] <- factor(salad_bar_dat_use[["rurality"]])

salad_bar_dat_use$rurality2 <- ifelse(salad_bar_dat_use$rurality == 'Urban' | salad_bar_dat_use$rurality == 'Suburb', 'Urban', 'non-Urban')

salad_bar_dat_use$school_type <- factor(salad_bar_dat_use$school_type, levels = c('Elementary', 'Middle School', 'High School'))


## Demo ###

## gender
gender_chi <- chisq.test(xtabs(~rurality2 + gender, data = salad_bar_dat_use))

## age
age_ttest <- t.test(age~rurality2, data = salad_bar_dat_use)

## grade
grade_ttest <- t.test(grade~rurality2, data = salad_bar_dat_use)

## race/ethnicity
raceethnicity_chi <- chisq.test(xtabs(~rurality2 + race_ethnicity, data = salad_bar_dat_use))

## frl
frl_chi <- chisq.test(xtabs(~rurality2 + paid_free_reduced, data = salad_bar_dat_use))

## fv select
fv_selected_chi <- chisq.test(xtabs(~rurality2 + fv_selected, data = salad_bar_dat_use))

## school level
school_chi <- chisq.test(xtabs(~rurality2 + school_type, data = salad_bar_dat_use))

## lunch duration
lunch_dur_mod <- lmer(lunch_dur~rurality2 + (1|school_name), data = salad_bar_dat_use)
lunch_dur_anova <- anova(lunch_dur_mod, test.statistic = 'F')
lunch_dur_emmeans <- emmeans(lunch_dur_mod, pairwise ~ rurality2)

## eating duration
linewait_mod <- lmer(time_line~rurality2 + (1|school_name), data = salad_bar_dat_use)
linewait_anova <- anova(linewait_mod, test.statistic = 'F')
linewait_emmeans <- emmeans(linewait_mod, pairwise ~ rurality2)

## fv selected g
fv_pre_mod <- lmer(fv_pre~rurality2 + (1|school_name), data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ])
fv_pre_anova <- Anova(fv_pre_mod, type = 3, test.statistic = 'F')
fv_pre_emmeans <- emmeans(fv_pre_mod, pairwise ~ rurality2)

## fv consumed g
fv_consumed_mod <- lmer(fv_consumed~rurality2 + (1|school_name), data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ])
fv_consumed_anova <- Anova(fv_consumed_mod, type = 3, test.statistic = 'F')
fv_consumed_emmeans <- emmeans(fv_consumed_mod, pairwise ~ rurality2)

## fv waste g
fv_waste_mod <- lmer(fv_post~rurality2 + (1|school_name), data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ])
fv_waste_anova <- Anova(fv_waste_mod, type = 3, test.statistic = 'F')
fv_waste_emmeans <- emmeans(fv_waste_mod, pairwise ~ rurality2)

## fv waste prop
fv_prop_waste_mod <- lmer(fv_prop_waste~rurality2 + (1|school_name), data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ])
fv_prop_waste_anova <- Anova(fv_prop_waste_mod, type = 3, test.statistic = 'F')
fv_prop_waste_emmeans <- emmeans(fv_prop_waste_mod, pairwise ~ rurality2)

## Fruit/Vegetable Selected - 2 category ####

# All
fv_served_model2 <- mixed_model(fv_pre ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + rurality2, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + rurality2, zi_random = ~ 1 | school_name)

fv_served_sum2 <- summary(fv_served_model2)


# Test Interaction with school level
# fv_served_int_model2 <- mixed_model(fv_pre ~ race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality2, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality2, zi_random = ~ 1 | school_name)
# 
# fv_served_int_sum2 <- summary(fv_served_int_model2)

# Test Interaction with gender
fv_served_gender_model2 <- mixed_model(fv_pre ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality2, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality2, zi_random = ~ 1 | school_name)

fv_served_gender_sum2 <- summary(fv_served_gender_model2)

fv_served_gender_fixed_emt2 <- emmeans(fv_served_gender_model2, pairwise ~gender | rurality2, mode = 'zero_part')

# Test Interaction with time to eat
salad_bar_dat_use$time_to_eat_c <- salad_bar_dat_use$time_to_eat - mean(salad_bar_dat_use$time_to_eat, na.rm = TRUE)

fv_served_tte_model2 <- mixed_model(fv_pre ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat_c*rurality2, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat_c*rurality2, zi_random = ~ 1 | school_name)

fv_served_tte_sum2 <- summary(fv_served_tte_model2)

fv_served_tte_zi_emt2 <- test(emtrends(fv_served_tte_model2,  pairwise ~ rurality2, var = 'time_to_eat_c', mode = 'zero_part', type = 'response'))


## Fruit/Vegetable Consumed ####


# All
fv_consumed_model2 <- mixed_model(fv_consumed ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + rurality2, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ], family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + rurality2, zi_random = ~ 1 | school_name)

fv_consumed_sum2 <- summary(fv_consumed_model2)


# Test Interaction with school level
# fv_consumed_int_model2 <- mixed_model(fv_consumed ~ race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality2, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ], family = zi.negative.binomial(), zi_fixed = ~ race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality2, zi_random = ~ 1 | school_name)
# 
# fv_consumed_int_sum2 <- summary(fv_consumed_int_model2)

# Test Interaction with gender
fv_consumed_gender_model2 <- mixed_model(fv_consumed ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality2, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ], family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality2, zi_random = ~ 1 | school_name)

fv_consumed_gender_sum2 <- summary(fv_consumed_gender_model2)


# Test Interaction with time to eat
fv_consumed_tte_model2 <- mixed_model(fv_consumed ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat_c*rurality2, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ], family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat_c*rurality2, zi_random = ~ 1 | school_name)

fv_consumed_tte_sum2 <- summary(fv_consumed_tte_model2)

fv_consumed_tte_zi_emt2 <- test(emtrends(fv_consumed_tte_model2,  pairwise ~ rurality2, var = 'time_to_eat_c', mode = 'zero_part', type = 'response'))


## Fruit/Vegetable Waste Proportion ####

# All
fv_prop_waste_model2 <- mixed_model(fv_prop_waste ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + rurality2, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + rurality2, zi_random = ~ 1 | school_name)

fv_prop_waste_sum2 <- summary(fv_prop_waste_model2)


# Test Interaction with school level
# fv_prop_waste_int_model2 <- mixed_model(fv_prop_waste ~  race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality2, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality2, zi_random = ~ 1 | school_name)
# 
# fv_prop_waste_int_sum2 <- summary(fv_prop_waste_int_model2)

# Test Interaction with gender
fv_prop_waste_gender_model2 <- mixed_model(fv_prop_waste ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality2, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality2, zi_random = ~ 1 | school_name)

fv_prop_waste_gender_sum2 <- summary(fv_prop_waste_gender_model2)

# Test Interaction with time to eat
fv_prop_waste_tte_model2 <- mixed_model(fv_prop_waste ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat_c*rurality2, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat_c*rurality2, zi_random = ~ 1 | school_name)

fv_prop_waste_tte_sum2 <- summary(fv_prop_waste_tte_model2)

fv_prop_waste_tte_fixed_emm2 <- test(emtrends(fv_prop_waste_tte_model2,  pairwise ~ rurality2, var = 'time_to_eat_c', mode = 'fixed-effects', type = 'response'))

## Fruit/Vegetable Selected - 3 category####
# 
# 
# # All
# fv_served_model <- mixed_model(fv_pre ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + rurality, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + rurality, zi_random = ~ 1 | school_name)
# 
# fv_served_sum <- summary(fv_served_model)
# 
# 
# # Test Interaction with school level
# # fv_served_int_model <- mixed_model(fv_pre ~ race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality, zi_random = ~ 1 | school_name)
# # 
# # fv_served_int_sum <- summary(fv_served_int_model)
# 
# # Test Interaction with gender
# fv_served_gender_model <- mixed_model(fv_pre ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, zi_random = ~ 1 | school_name)
# 
# fv_served_gender_sum <- summary(fv_served_gender_model)
# 
# fv_served_gender_fixed_emm <-emmeans(fv_served_gender_model,  ~gender | rurality, mode = 'fixed-effects', type = 'response')
# 
# # Test Interaction with time to eat
# fv_served_tte_model <- mixed_model(fv_pre ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat_c*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use, family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat_c*rurality, zi_random = ~ 1 | school_name)
# 
# fv_served_tte_sum <- summary(fv_served_tte_model)
# 
# fv_served_tte_zi_emt <- test(emtrends(fv_served_tte_model,  pairwise ~ rurality, var = 'time_to_eat_c', mode = 'zero_part', type = 'response'))
# 
# 
# ## Fruit/Vegetable Consumed ####
# 
# 
# # All
# fv_consumed_model <- mixed_model(fv_consumed ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ], family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + rurality, zi_random = ~ 1 | school_name)
# 
# fv_consumed_sum <- summary(fv_consumed_model)
# 
# 
# # Test Interaction with school level
# # fv_consumed_int_model <- mixed_model(fv_consumed ~ race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ], family = zi.negative.binomial(), zi_fixed = ~ race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality, zi_random = ~ 1 | school_name)
# # 
# # fv_consumed_int_sum <- summary(fv_consumed_int_model)
# 
# # Test Interaction with gender
# fv_consumed_gender_model <- mixed_model(fv_consumed ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ], family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, zi_random = ~ 1 | school_name)
# 
# fv_consumed_gender_sum <- summary(fv_consumed_gender_model)
# 
# 
# # Test Interaction with time to eat
# fv_consumed_tte_model <- mixed_model(fv_consumed ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat_c*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ], family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat_c*rurality, zi_random = ~ 1 | school_name)
# 
# fv_consumed_tte_sum <- summary(fv_consumed_tte_model)
# 
# fv_consumed_tte_zi_emt <- test(emtrends(fv_consumed_tte_model,  pairwise ~ rurality, var = 'time_to_eat_c', mode = 'zero_part', type = 'response'))
# 
# 
# ## Fruit/Vegetable Waste Proportion ####
# 
# # All
# fv_prop_waste_model <- mixed_model(fv_prop_waste ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + rurality, zi_random = ~ 1 | school_name)
# 
# fv_prop_waste_sum <- summary(fv_prop_waste_model)
# 
# 
# # Test Interaction with school level
# # fv_prop_waste_int_model <- mixed_model(fv_prop_waste ~ race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ race_ethnicity + paid_free_reduced + lunch_dur + school_type*rurality, zi_random = ~ 1 | school_name)
# # 
# # fv_prop_waste_int_sum <- summary(fv_prop_waste_int_model)
# 
# # Test Interaction with gender
# fv_prop_waste_gender_model <- mixed_model(fv_prop_waste ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + gender*rurality, zi_random = ~ 1 | school_name)
# 
# fv_prop_waste_gender_sum <- summary(fv_prop_waste_gender_model)
# 
# # Test Interaction with time to eat
# fv_prop_waste_tte_model <- mixed_model(fv_prop_waste ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat_c*rurality, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ school_type + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat_c*rurality, zi_random = ~ 1 | school_name)
# 
# fv_prop_waste_tte_sum <- summary(fv_prop_waste_tte_model)
# 
# fv_prop_waste_tte_fixed_emm <- test(emtrends(fv_prop_waste_tte_model,  pairwise ~ rurality, var = 'time_to_eat_c', mode = 'fixed-effects', type = 'response'))
