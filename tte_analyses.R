# This script was written by Alaina Pearce in Spring 2023
# to analyze the eating duration data from the Salad Bar Study
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

# check tte
salad_bar_dat$tte_extra <- salad_bar_dat[['time_to_eat']] - salad_bar_dat[['lunch_dur']]

# Quality Checks:
salad_bar_dat$assent_dif <- time_length(salad_bar_dat$lunch_start - salad_bar_dat$assent_time, unit = 'min')
salad_bar_dat$exit_dif <- time_length(salad_bar_dat$lunch_end - salad_bar_dat$exit_time, unit = 'min')
salad_bar_dat$scan_dif <- time_length(salad_bar_dat$exit_time - salad_bar_dat$assent_time, unit = 'min')
salad_bar_dat$assent_pict_dif <- time_length(salad_bar_dat$pre_photo - salad_bar_dat$assent_time, unit = 'min')
salad_bar_dat$pict_exit_dif <- time_length(salad_bar_dat$exit_time - salad_bar_dat$pre_photo, unit = 'min')


## reduce to complete data
salad_bar_dat_use <- salad_bar_dat[!is.na(salad_bar_dat[['fv_pre']]) & !is.na(salad_bar_dat[['fv_post']]) & salad_bar_dat[['time_to_eat']] >= 0, ]

salad_bar_dat_use <- salad_bar_dat_use[!is.na(salad_bar_dat_use[['gender']]) & !is.na(salad_bar_dat_use[["race_ethnicity"]]) & !is.na(salad_bar_dat_use[["grade"]]) & !is.na(salad_bar_dat_use[["paid_free_reduced"]]) & !is.na(salad_bar_dat_use[["lunch_dur"]]), ]

nrow(salad_bar_dat_use[salad_bar_dat_use$time_to_eat > salad_bar_dat_use$lunch_dur, ])
#47

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

## lunch duration
lunch_dur_mod <- lmer(lunch_dur~school_type + (1|school_name), data = salad_bar_dat_use)
lunch_dur_anova <- anova(lunch_dur_mod, test.statistic = 'F')
lunch_dur_emmeans <- emmeans(lunch_dur_mod, pairwise ~ school_type)

## time to eat
tte_mod <- lmer(time_to_eat~school_type + (1|school_name), data = salad_bar_dat_use)
tte_anova <- Anova(tte_mod, type = 3, test.statistic = 'F')
tte_emmeans <- emmeans(tte_mod, pairwise ~ school_type)

## fv selected g
fv_pre_mod <- lmer(fv_pre~school_type + (1|school_name), data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ])
fv_pre_anova <- Anova(fv_pre_mod, type = 3, test.statistic = 'F')
fv_pre_emmeans <- emmeans(fv_pre_mod, pairwise ~ school_type)

## fv consumed g
fv_consumed_mod <- lmer(fv_consumed~school_type + (1|school_name), data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ])
fv_consumed_anova <- Anova(fv_consumed_mod, type = 3, test.statistic = 'F')
fv_consumed_emmeans <- emmeans(fv_consumed_mod, pairwise ~ school_type)

## fv waste g
fv_post_mod <- lmer(fv_post~school_type + (1|school_name), data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ])
fv_post_anova <- Anova(fv_post_mod, type = 3, test.statistic = 'F')
fv_post_emmeans <- emmeans(fv_post_mod, pairwise ~ school_type)

## fv waste prop
fv_prop_waste_mod <- lmer(fv_prop_waste~school_type + (1|school_name), data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ])
fv_prop_waste_anova <- Anova(fv_prop_waste_mod, type = 3, test.statistic = 'F')
fv_prop_waste_emmeans <- emmeans(fv_prop_waste_mod, pairwise ~ school_type)

## Figures ####
salad_bar_dat_use[["school_type"]] <- factor(salad_bar_dat_use[["school_type"]], levels = c('Elementary', 'Middle School', 'High School'))
tte_means <- means.function.na(salad_bar_dat_use, salad_bar_dat_use[["time_to_eat"]], salad_bar_dat_use[["school_type"]])
tte_se <- se.function.na(salad_bar_dat_use, salad_bar_dat_use[["time_to_eat"]], salad_bar_dat_use[["school_type"]])
# 
# bar_graph.se(tte_means, tte_se, xlab = 'School', ylab = 'Time To Eat, min', ymax = 15, ymin = 0, group = 0)

tte_boxplot <- ggplot(salad_bar_dat_use, aes(x=school_type, y=time_to_eat)) + 
  geom_boxplot(color="cornflowerblue", fill = 'cornflowerblue', alpha=0.2, outlier.colour="black", outlier.fill="black", outlier.size=3
  )

tte_boxplot_nooutline <- ggplot(salad_bar_dat_use, aes(x=school_type, y=time_to_eat)) + 
  geom_boxplot(color="cornflowerblue", fill = 'cornflowerblue', alpha=0.2, outlier.shape = NA) + scale_y_continuous(limits = c(0, 20))

## Primary values

salad_bar_dat_use[['school_type']] <- factor(salad_bar_dat_use[["school_type"]])

## Fruit/Vegetable Selected ####

# All
fv_served_lmer <- lmer(time_to_eat ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + fv_pre + (1|school_name), data = salad_bar_dat_use)

fv_served_sum <- summary(fv_served_lmer)

fv_served_emmeans_sex <- emmeans(fv_served_lmer, pairwise ~ gender)
fv_served_emmeans_race <- emmeans(fv_served_lmer, pairwise ~ race_ethnicity)

# Elementary
fv_served_ES_lmer <- lmer(time_to_eat ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + fv_pre + (1 | school_name), data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Elementary', ])

fv_served_ES_sum <- summary(fv_served_ES_lmer)

# Middle School
fv_served_MS_lmer <- lmer(time_to_eat ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + fv_pre + (1 | school_name), data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Middle School', ])

fv_served_MS_sum <- summary(fv_served_MS_lmer)

# High School
fv_served_HS_lmer <- lmer(time_to_eat ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + fv_pre + (1 | school_name), data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'High School', ])

fv_served_HS_sum <- summary(fv_served_HS_lmer)


## Fruit/Vegetable Consumed ####

# All
fv_consumed_model <- mixed_model(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_consumed_sum <- summary(fv_consumed_model)

fv_consumed_mean <- mean(salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', 'time_to_eat'])
fv_consumed_sd <- sd(salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y', 'time_to_eat'])

fv_consumed_fixed_emm <- emtrends(fv_consumed_model, ~time_to_eat, var = 'time_to_eat', mode = 'fixed-effects', type = 'response', at = list(time_to_eat = c(5, 10, 15)))

fv_consumed_zi_emm <- emmeans(fv_consumed_model, ~time_to_eat, mode = 'zero_part', type = 'response', at = list(time_to_eat = c(5, 10, 15)))

# Elementary
fv_consumed_ES_model <- mixed_model(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$school_type == 'Elementary', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_consumed_ES_sum <- summary(fv_consumed_ES_model)

# Middle School
fv_consumed_MS_model <- mixed_model(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$school_type == 'Middle School', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_consumed_MS_sum <- summary(fv_consumed_MS_model)

# High School
fv_consumed_HS_model <- mixed_model(fv_consumed ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$school_type == 'High School', ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_consumed_HS_sum <- summary(fv_consumed_HS_model)

## Fruit/Vegetable Waste Proportion ####

# All

fv_prop_waste_model <- mixed_model(fv_prop_waste ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_prop_waste_sum <- summary(fv_prop_waste_model)

# Elementary School

fv_prop_waste_ES_model <- mixed_model(fv_prop_waste ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Elementary' & salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_prop_waste_ES_sum <- summary(fv_prop_waste_ES_model)

# Middle School

fv_prop_waste_MS_model <- mixed_model(fv_prop_waste ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'Middle School' & salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_prop_waste_MS_sum <- summary(fv_prop_waste_MS_model)


# High School

fv_prop_waste_HS_model <- mixed_model(fv_prop_waste ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, random = ~ 1 | school_name, data = salad_bar_dat_use[salad_bar_dat_use$school_type == 'High School' & salad_bar_dat_use$fv_selected == 'Y' & salad_bar_dat_use$fv_prop_waste < 100, ], family = zi.negative.binomial(), zi_fixed = ~ grade + gender + race_ethnicity + paid_free_reduced + lunch_dur + time_to_eat, zi_random = ~ 1 | school_name)

fv_prop_waste_HS_sum <- summary(fv_prop_waste_HS_model)
