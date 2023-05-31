# This script was written by Alaina Pearce in Spring 2023
# to set up the tables from the Salad Bar Study
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

# library(gtsummary)
# theme_gtsummary_compact()

# source('setup.R')

## participant table
partab_data <- salad_bar_dat_use[c(55:57, 64, 60, 61)]

partab_all <-
  tbl_summary(
    data = partab_data,
    value = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected"),
    label = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected"),
    type = list(gender ~ "categorical", grade ~ "continuous", age ~ "continuous", race_ethnicity ~ 'categorical', paid_free_reduced ~ "categorical", fv_selected ~ "categorical"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  modify_header(all_stat_cols() ~ "**Overall**") %>%
  add_n()
  
partab_intake_data <- salad_bar_dat_use[salad_bar_dat_use[['fv_selected']] == 'Y', c(16, 18, 17, 62, 68, 67)]

partab_intake_all <-
  tbl_summary(
    data = partab_intake_data,
    value = list(fv_pre ~ "F/V Self-Served, g", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %', lunch_dur ~ "Lunch Period", time_to_eat ~ "Eating Duration"),
    label = list(fv_pre ~ "F/V Self-Served, g", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %', lunch_dur ~ "Lunch Period", time_to_eat ~ "Eating Duration"),
    type = list(fv_pre ~ "continuous", fv_consumed ~ 'continuous', fv_post ~ 'continuous', fv_prop_waste ~ 'continuous', lunch_dur ~ 'continuous', time_to_eat ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  modify_header(all_stat_cols() ~ "**Overall**") %>%
  add_n()

partab_school_data <- salad_bar_dat_use[c(4, 55:57, 64, 60, 61)]

partab_all_school <-
  tbl_summary(
    data = partab_school_data,
    by = 'school_type',
    value = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected"),
    label = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected"),
    type = list(gender ~ "categorical", grade ~ "continuous", age ~ "continuous", race_ethnicity ~ 'categorical', paid_free_reduced ~ "categorical", fv_selected ~ "categorical"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_n() %>%
  add_p()

partab_intake_school_data <- salad_bar_dat_use[salad_bar_dat_use[['fv_selected']] == 'Y', c(4, 16, 18, 17, 62, 68, 67)]

partab_intake_school <-
  tbl_summary(
    data = partab_intake_school_data,
    by = 'school_type',
    value = list(fv_pre ~ "F/V Self-Served, g", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %', lunch_dur ~ "Lunch Period", time_to_eat ~ "Eating Duration"),
    label = list(fv_pre ~ "F/V Self-Served, g", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %', lunch_dur ~ "Lunch Period", time_to_eat ~ "Eating Duration"),
    type = list(fv_pre ~ "continuous", fv_consumed ~ 'continuous', fv_post ~ 'continuous', fv_prop_waste ~ 'continuous', lunch_dur ~ 'continuous', time_to_eat ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_n() %>%
  add_p()

partab_merge <-
  tbl_merge(
    tbls = list(partab_all, partab_all_school),
    tab_spanner = c("**Full Sample**", "**School Type**")
  )

partab_intake_merge <-
  tbl_merge(
    tbls = list(partab_intake_all, partab_intake_school),
    tab_spanner = c("**Full Sample**", "**School Type**")
  )

overall_table <-
  tbl_stack(list(partab_merge, partab_intake_merge), group_header = c("", ""))

## Model Tables ###

# served 
fv_served <- cbind.data.frame(fv_served_coef, fv_served_ES_coef, fv_served_MS_coef, fv_served_HS_coef)
names(fv_served) <- c('all_irr', 'all_se', 'all_p', 'E_irr', 'E_se', 'E_p', 'M_irr', 'M_se', 'M_p', 'H_irr', 'H_se', 'H_p')
rownames(fv_served) <- c('(intercept)', 'grade', 'gender, M', 'white', 'other', 'Black/AA', 'Paid Lunch', 'Lunch Dur', 'Time to Eat')

fv_served_z <- cbind.data.frame(fv_served_coef_z, fv_served_ES_coef_z, fv_served_MS_coef_z, fv_served_HS_coef_z)
names(fv_served_z) <- c('all_or', 'all_se', 'all_p', 'E_or', 'E_se', 'E_p', 'M_irr', 'M_se', 'M_p', 'H_or', 'H_se', 'H_p')
rownames(fv_served_z) <- c('(intercept)', 'grade', 'gender, M', 'white', 'other', 'Black/AA', 'Paid Lunch', 'Lunch Dur', 'Time to Eat')

# consumed
fv_consumed <- cbind.data.frame(fv_consumed_coef, fv_consumed_ES_coef, fv_consumed_MS_coef, fv_consumed_HS_coef)
names(fv_consumed) <- c('all_irr', 'all_se', 'all_p', 'E_irr', 'E_se', 'E_p', 'M_irr', 'M_se', 'M_p', 'H_irr', 'H_se', 'H_p')
rownames(fv_consumed) <- c('(intercept)', 'grade', 'gender, M', 'white', 'other', 'Black/AA', 'Paid Lunch', 'Lunch Dur', 'Time to Eat')

fv_consumed_z <- cbind.data.frame(fv_consumed_coef_z, fv_consumed_ES_coef_z, fv_consumed_MS_coef_z, fv_consumed_HS_coef_z)
names(fv_consumed_z) <- c('all_or', 'all_se', 'all_p', 'E_or', 'E_se', 'E_p', 'M_irr', 'M_se', 'M_p', 'H_or', 'H_se', 'H_p')
rownames(fv_consumed_z) <- c('(intercept)', 'grade', 'gender, M', 'white', 'other', 'Black/AA', 'Paid Lunch', 'Lunch Dur', 'Time to Eat')

# waste
fv_waste <- cbind.data.frame(fv_waste_coef, fv_waste_ES_coef, fv_waste_MS_coef, fv_waste_HS_coef)
names(fv_waste) <- c('all_irr', 'all_se', 'all_p', 'E_irr', 'E_se', 'E_p', 'M_irr', 'M_se', 'M_p', 'H_irr', 'H_se', 'H_p')
rownames(fv_waste) <- c('(intercept)', 'grade', 'gender, M', 'white', 'other', 'Black/AA', 'Paid Lunch', 'Lunch Dur', 'Time to Eat')

fv_waste_z <- cbind.data.frame(fv_waste_coef_z, fv_waste_ES_coef_z, fv_waste_MS_coef_z, fv_waste_HS_coef_z)
names(fv_waste_z) <- c('all_or', 'all_se', 'all_p', 'E_or', 'E_se', 'E_p', 'M_irr', 'M_se', 'M_p', 'H_or', 'H_se', 'H_p')
rownames(fv_waste_z) <- c('(intercept)', 'grade', 'gender, M', 'white', 'other', 'Black/AA', 'Paid Lunch', 'Lunch Dur', 'Time to Eat')

# percent waste
fv_prop_waste <- cbind.data.frame(fv_prop_waste_coef, fv_prop_waste_ES_coef, fv_prop_waste_MS_coef, fv_prop_waste_HS_coef)
names(fv_prop_waste) <- c('all_irr', 'all_se', 'all_p', 'E_irr', 'E_se', 'E_p', 'M_irr', 'M_se', 'M_p', 'H_irr', 'H_se', 'H_p')
rownames(fv_prop_waste) <- c('(intercept)', 'grade', 'gender, M', 'white', 'other', 'Black/AA', 'Paid Lunch', 'Lunch Dur', 'Time to Eat')

fv_prop_waste_z <- cbind.data.frame(fv_prop_waste_coef_z, fv_prop_waste_ES_coef_z, fv_prop_waste_MS_coef_z, fv_prop_waste_HS_coef_z)
names(fv_prop_waste_z) <- c('all_or', 'all_se', 'all_p', 'E_or', 'E_se', 'E_p', 'M_irr', 'M_se', 'M_p', 'H_or', 'H_se', 'H_p')
rownames(fv_prop_waste_z) <- c('(intercept)', 'grade', 'gender, M', 'white', 'other', 'Black/AA', 'Paid Lunch', 'Lunch Dur', 'Time to Eat')


## Complete vs Missing ####
salad_bar_dat_use_includemissing <- salad_bar_dat[salad_bar_dat[['fv_exclude']] == 'N' & !is.na(salad_bar_dat[['fv_pre']]) & !is.na(salad_bar_dat[['fv_post']]), ]


partab_tte_data <- salad_bar_dat_use_includemissing[c(69, 55:57, 64, 60, 61)]

partab_all_tte <-
  tbl_summary(
    data = partab_tte_data,
    by = 'tte_dat',
    value = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected"),
    label = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected"),
    type = list(gender ~ "categorical", grade ~ "continuous", age ~ "continuous", race_ethnicity ~ 'categorical', paid_free_reduced ~ "categorical", fv_selected ~ "categorical"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_n() %>%
  add_p()

partab_intake_tte_data <- salad_bar_dat_use_includemissing[salad_bar_dat_use_includemissing[['fv_selected']] == 'Y', c(69, 16, 18, 17, 62, 68)]

partab_intake_tte <-
  tbl_summary(
    data = partab_intake_tte_data,
    by = 'tte_dat',
    value = list(fv_pre ~ "F/V Self-Served, g", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %', lunch_dur ~ "Lunch Period"),
    label = list(fv_pre ~ "F/V Self-Served, g", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %', lunch_dur ~ "Lunch Period"),
    type = list(fv_pre ~ "continuous", fv_consumed ~ 'continuous', fv_post ~ 'continuous', fv_prop_waste ~ 'continuous', lunch_dur ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_n() %>%
  add_p()

partab_merge_tte <-
  tbl_merge(
    tbls = list(partab_all, partab_all_tte),
    tab_spanner = c("**Full Sample**", "**Time to Eat Data**")
  )

partab_intake_merge_tte <-
  tbl_merge(
    tbls = list(partab_intake_all, partab_intake_tte),
    tab_spanner = c("**Full Sample**", "**Time to Eat Data**")
  )

overall_table_tte <-
  tbl_stack(list(partab_merge_tte, partab_intake_merge_tte), group_header = c("", ""))
