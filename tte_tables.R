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

# salad_bar_dat_use <- salad_bar_dat_use[!is.na(salad_bar_dat_use[["gender"]]) & !is.na(salad_bar_dat_use[["grade"]]) & !is.na(salad_bar_dat_use[["race_ethnicity"]]) & !is.na(salad_bar_dat_use[["paid_free_reduced"]]) & !is.na(salad_bar_dat_use[["lunch_dur"]]), ]

## participant table
salad_bar_dat_use$school_type <- factor(salad_bar_dat_use$school_type, levels = c('Elementary', 'Middle School', 'High School'))
partab_data <- salad_bar_dat_use[c(55:57, 63, 60, 61, 69, 72)]

partab_all <-
  tbl_summary(
    data = partab_data,
    value = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected", lunch_dur ~ "Lunch Period", time_line ~ "Time in Line"),
    label = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected",lunch_dur ~ "Lunch Period", time_line ~ "Time in Line"),
    type = list(gender ~ "categorical", grade ~ "continuous", age ~ "continuous", race_ethnicity ~ 'categorical', paid_free_reduced ~ "categorical", fv_selected ~ "categorical", lunch_dur ~ "continuous", time_line ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  modify_header(all_stat_cols() ~ "**Overall**") %>%
  add_n()
  
partab_select_data <- salad_bar_dat_use[salad_bar_dat_use[['fv_selected']] == 'Y', c(75, 16, 69, 72, 73, 18, 17, 62)]

partab_select <-
  tbl_summary(
    data = partab_select_data,
    value = list(fv_consumed_cat ~ "F/V Consumed Any", fv_pre ~ "F/V Self-Served, g", lunch_dur ~ "Lunch Period", time_line ~ "Time in Line", time_to_eat ~ "Eating Duration", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %'),
    label = list(fv_consumed_cat ~ "F/V Consumed Any", fv_pre ~ "F/V Self-Served, g", lunch_dur ~ "Lunch Period", time_line ~ "Time in Line", time_to_eat ~ "Eating Duration", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %'),
    type = list(fv_consumed_cat ~ "categorical", fv_pre ~ "continuous", lunch_dur ~ 'continuous', time_line ~ "continuous", time_to_eat ~ "continuous", fv_consumed ~ 'continuous', fv_post ~ 'continuous', fv_prop_waste ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  add_n()

partab_school_data <- salad_bar_dat_use[c(4, 55:57, 63, 60, 61, 69, 72)]

partab_all_school <-
  tbl_summary(
    data = partab_school_data,
    by = 'school_type',
    value = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected", lunch_dur ~ "Lunch Period", time_line ~ "Time in Line"),
    label = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected", lunch_dur ~ "Lunch Period", time_line ~ "Time in Line"),
    type = list(gender ~ "categorical", grade ~ "continuous", age ~ "continuous", race_ethnicity ~ 'categorical', paid_free_reduced ~ "categorical", fv_selected ~ "categorical", lunch_dur ~ "continuous", time_line ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_n() %>%
  add_p(pvalue_fun = function(x) style_number(x, digits = 3))

partab_select_school_data <- salad_bar_dat_use[salad_bar_dat_use[['fv_selected']] == 'Y', c(4, 75, 16, 69, 72, 73, 18, 17, 62)]


partab_select_school <-
  tbl_summary(
    data = partab_select_school_data,
    by = 'school_type',
    value = list(fv_consumed_cat ~ "F/V Consumed Any", fv_pre ~ "F/V Self-Served, g", lunch_dur ~ "Lunch Period", time_line ~ "Time in Line", time_to_eat ~ "Eating Duration", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %'),
    label = list(fv_consumed_cat ~ "F/V Consumed Any", fv_pre ~ "F/V Self-Served, g", lunch_dur ~ "Lunch Period", time_line ~ "Time in Line", time_to_eat ~ "Eating Duration", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %'),
    type = list(fv_consumed_cat ~ "categorical", fv_pre ~ "continuous", lunch_dur ~ 'continuous', time_line ~ "continuous", time_to_eat ~ "continuous", fv_consumed ~ 'continuous', fv_post ~ 'continuous', fv_prop_waste ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_n() %>%
  add_p(pvalue_fun = function(x) style_number(x, digits = 3))


partab_merge <-
  tbl_merge(
    tbls = list(partab_all, partab_all_school),
    tab_spanner = c("**Full Sample**", "**School Type**")
  )

partab_select_merge <-
  tbl_merge(
    tbls = list(partab_select, partab_select_school),
    tab_spanner = c("**Full Sample**", "**School Type**")
  )


overall_table <-
  tbl_stack(list(partab_merge, partab_select_merge), group_header = c("", ""))


partab_select_data <- salad_bar_dat_use[c(55:57, 63, 60, 61, 69, 72)]

partab_all_byselect <-
  tbl_summary(
    data = partab_select_data,
    by = 'fv_selected',
    value = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch",  lunch_dur ~ "Lunch Period", time_line ~ "Time in Line"),
    label = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", lunch_dur ~ "Lunch Period", time_line ~ "Time in Line"),
    type = list(gender ~ "categorical", grade ~ "continuous", age ~ "continuous", race_ethnicity ~ 'categorical', paid_free_reduced ~ "categorical", lunch_dur ~ "continuous", time_line ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_n() %>%
  add_p(pvalue_fun = function(x) style_number(x, digits = 3))


## Complete vs Missing for TTE (after ensuring all other covars are present) ####
salad_bar_dat_use_includemissing <- salad_bar_dat[!is.na(salad_bar_dat[['fv_pre']]) & !is.na(salad_bar_dat[['fv_post']]) & !is.na(salad_bar_dat[['gender']]) & !is.na(salad_bar_dat[["race_ethnicity"]]) & !is.na(salad_bar_dat[["grade"]]) & !is.na(salad_bar_dat[["paid_free_reduced"]]) & !is.na(salad_bar_dat[["lunch_dur"]]), ]

# set tte < 0 as missing
salad_bar_dat_use_includemissing[['tte_dat']] <- ifelse(is.na(salad_bar_dat_use_includemissing[['time_to_eat']]) | salad_bar_dat_use_includemissing[['time_to_eat']] < 0, 'Missing', as.character(salad_bar_dat_use_includemissing[['tte_dat']]))

partab_tte_alldata <- salad_bar_dat_use_includemissing[c(55:57, 63, 60, 61, 69)]

partab_alldata_tte <-
  tbl_summary(
    data = partab_tte_alldata,
    value = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected", lunch_dur ~ "Lunch duration"),
    label = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected", lunch_dur ~ "Lunch duration"),
    type = list(gender ~ "categorical", grade ~ "continuous", age ~ "continuous", race_ethnicity ~ 'categorical', paid_free_reduced ~ "categorical", fv_selected ~ "categorical", lunch_dur ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  add_n()

partab_intake_tte_alldata <- salad_bar_dat_use_includemissing[salad_bar_dat_use_includemissing[['fv_selected']] == 'Y', c(75, 16, 69, 72, 73, 18, 17, 62)]

partab_intake_alldata_tte <-
  tbl_summary(
    data = partab_intake_tte_alldata,
    value = list(fv_consumed_cat ~ "F/V Consumed Any", fv_pre ~ "F/V Self-Served, g", lunch_dur ~ "Lunch Period", time_line ~ "Time in Line", time_to_eat ~ "Eating Duration", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %'),
    label = list(fv_consumed_cat ~ "F/V Consumed Any", fv_pre ~ "F/V Self-Served, g", lunch_dur ~ "Lunch Period", time_line ~ "Time in Line", time_to_eat ~ "Eating Duration", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %'),
    type = list(fv_consumed_cat ~ "categorical", fv_pre ~ "continuous", lunch_dur ~ 'continuous', time_line ~ "continuous", time_to_eat ~ "continuous", fv_consumed ~ 'continuous', fv_post ~ 'continuous', fv_prop_waste ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  add_n()

# by TTE status
partab_tte_data <- salad_bar_dat_use_includemissing[c(74, 55:57, 63, 60, 61, 69)]

partab_all_tte <-
  tbl_summary(
    data = partab_tte_data,
    by = 'tte_dat',
    value = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected", lunch_dur ~ "Lunch duration"),
    label = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected", lunch_dur ~ "Lunch duration"),
    type = list(gender ~ "categorical", grade ~ "continuous", age ~ "continuous", race_ethnicity ~ 'categorical', paid_free_reduced ~ "categorical", fv_selected ~ "categorical", lunch_dur ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_n() %>%
  add_p(pvalue_fun = function(x) style_number(x, digits = 3))

partab_intake_tte_data <- salad_bar_dat_use_includemissing[salad_bar_dat_use_includemissing[['fv_selected']] == 'Y', c(74, 75, 16, 69, 72, 73, 18, 17, 62)]

partab_intake_tte <-
  tbl_summary(
    data = partab_intake_tte_data,
    by = 'tte_dat',
    value = list(fv_consumed_cat ~ "F/V Consumed Any", fv_pre ~ "F/V Self-Served, g", lunch_dur ~ "Lunch Period", time_line ~ "Time in Line", time_to_eat ~ "Eating Duration", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %'),
    label = list(fv_consumed_cat ~ "F/V Consumed Any", fv_pre ~ "F/V Self-Served, g", lunch_dur ~ "Lunch Period", time_line ~ "Time in Line", time_to_eat ~ "Eating Duration", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %'),
    type = list(fv_consumed_cat ~ "categorical", fv_pre ~ "continuous", lunch_dur ~ 'continuous', time_line ~ "continuous", time_to_eat ~ "continuous", fv_consumed ~ 'continuous', fv_post ~ 'continuous', fv_prop_waste ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_n() %>%
  add_p(pvalue_fun = function(x) style_number(x, digits = 3))

# merge 
partab_merge_tte <-
  tbl_merge(
    tbls = list(partab_alldata_tte, partab_all_tte),
    tab_spanner = c("**Full Sample**", "**Time to Eat Data**")
  )

partab_intake_merge_tte <-
  tbl_merge(
    tbls = list(partab_intake_alldata_tte, partab_intake_tte),
    tab_spanner = c("**Full Sample**", "**Time to Eat Data**")
  )

overall_table_tte <-
  tbl_stack(list(partab_merge_tte, partab_intake_merge_tte), group_header = c("", ""))
