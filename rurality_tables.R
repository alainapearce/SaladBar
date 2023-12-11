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
salad_bar_dat_use$school_type <- factor(salad_bar_dat_use$school_type, levels = c('Elementary', 'Middle School', 'High School'))
partab_data <- salad_bar_dat_use[c(55:57, 63, 60, 61, 64, 65, 67)]

partab_all <-
  tbl_summary(
    data = partab_data,
    value = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected", rurality ~ 'Ruality, 2 class', p_rural ~ 'Percent Rural, county', lunch_dur ~ 'Lunch Period'),
    label = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected", rurality ~ 'Rurality', p_rural ~ 'Percent Rural, county', lunch_dur ~ 'Lunch Period'),
    type = list(gender ~ "categorical", grade ~ "continuous", age ~ "continuous", race_ethnicity ~ 'categorical', paid_free_reduced ~ "categorical", fv_selected ~ "categorical", rurality ~ 'categorical', p_rural ~ 'continuous', lunch_dur ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  modify_header(all_stat_cols() ~ "**Overall**") %>%
  add_n()

partab_select_data <- salad_bar_dat_use[salad_bar_dat_use[['fv_selected']] == 'Y', c(16, 69, 66, 18, 17, 62)]

partab_select <-
  tbl_summary(
    data = partab_select_data,
    value = list(fv_consumed_cat ~ "F/V Consumed Any", fv_pre ~ "F/V Self-Served, g", time_to_eat ~ "Eating Duration", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %'),
    label = list(fv_consumed_cat ~ "F/V Consumed Any", fv_pre ~ "F/V Self-Served, g", time_to_eat ~ "Eating Duration", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %'),
    type = list(fv_consumed_cat ~ "categorical", fv_pre ~ "continuous", time_to_eat ~ "continuous", fv_consumed ~ 'continuous', fv_post ~ 'continuous', fv_prop_waste ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  modify_header(all_stat_cols() ~ "**Overall**") %>%
  add_n()

partab_school_data <- salad_bar_dat_use[c(4, 55:57, 63, 60, 61, 64, 65, 67)]

partab_all_school <-
  tbl_summary(
    data = partab_school_data,
    by = 'school_type',
    value = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected", rurality ~ 'Ruality, 2 class', p_rural ~ 'Percent Rural, county', lunch_dur ~ 'Lunch Period'),
    label = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ 'Race/Ethnicity', paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected", rurality ~ 'Ruality, 2 class', p_rural ~ 'Percent Rural, county', lunch_dur ~ 'Lunch Period'),
    type = list(gender ~ "categorical", grade ~ "continuous", age ~ "continuous", race_ethnicity ~ 'categorical', paid_free_reduced ~ "categorical", fv_selected ~ "categorical", rurality ~ 'categorical', p_rural ~ 'continuous', lunch_dur ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_n() %>%
  add_p()

partab_select_school_data <- salad_bar_dat_use[salad_bar_dat_use[['fv_selected']] == 'Y', c(4, 16, 66, 69, 18, 17, 62)]


partab_select_school <-
  tbl_summary(
    data = partab_select_school_data,
    by = 'school_type',
    value = list(fv_consumed_cat ~ "F/V Consumed Any", fv_pre ~ "F/V Self-Served, g",  time_to_eat ~ "Eating Duration", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %'),
    label = list(fv_consumed_cat ~ "F/V Consumed Any", fv_pre ~ "F/V Self-Served, g", time_to_eat ~ "Eating Duration", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %'),
    type = list(fv_consumed_cat ~ "categorical", fv_pre ~ "continuous", time_to_eat ~ "continuous", fv_consumed ~ 'continuous', fv_post ~ 'continuous', fv_prop_waste ~ 'continuous'),
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

partab_select_merge <-
  tbl_merge(
    tbls = list(partab_select, partab_select_school),
    tab_spanner = c("**Full Sample**", "**School Type**")
  )


overall_table <-
  tbl_stack(list(partab_merge, partab_select_merge), group_header = c("", ""))

## by rural ####

partab_rural_data <- salad_bar_dat_use[c(64, 55:57, 63, 60:61, 65, 67)]

partab_rural <-
  tbl_summary(
    data = partab_rural_data,
    by = 'rurality',
    value = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ "Race/Ethnicity", paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected", p_rural ~ 'Percent Rural, county', lunch_dur ~ 'Lunch Period'),
    label = list(gender ~ "Gender", grade ~ "Grade", age ~ "Age, yr", race_ethnicity ~ "Race/Ethnicity", paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "F/V Selected", p_rural ~ 'Percent Rural, county', lunch_dur ~ 'Lunch Period'),
    type = list(gender ~ "categorical", grade ~ "continuous", age ~ "continuous", race_ethnicity ~ "categorical", paid_free_reduced ~ "categorical", fv_selected ~ "categorical", p_rural ~ 'continuous', lunch_dur ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
add_p()

partab_select_rural_data <- salad_bar_dat_use[salad_bar_dat_use[['fv_selected']] == 'Y', c(64, 16, 66, 69, 18, 17, 62)]

partab_select_rural <-
  tbl_summary(
    data = partab_select_rural_data,
    by = 'rurality',
    value = list(fv_consumed_cat ~ "F/V Consumed Any", fv_pre ~ "F/V Self-Served, g", time_to_eat ~ "Eating Duration", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %'),
    label = list(fv_consumed_cat ~ "F/V Consumed Any", fv_pre ~ "F/V Self-Served, g", time_to_eat ~ "Eating Duration", fv_consumed ~ 'F/V Consumed, g', fv_post ~ 'F/V Waste, g', fv_prop_waste ~ 'F/V Percent Waste (post/pre), %'),
    type = list(fv_consumed_cat ~ "categorical", fv_pre ~ "continuous", time_to_eat ~ "continuous", fv_consumed ~ 'continuous', fv_post ~ 'continuous', fv_prop_waste ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_n() %>%
  add_p()

partab_merge_rural <-
  tbl_merge(
    tbls = list(partab_all, partab_rural),
    tab_spanner = c("**Full Sample**", "**Urban vs Rural**")
  )

partab_select_merge_rural <-
  tbl_merge(
    tbls = list(partab_select, partab_select_rural),
    tab_spanner = c("**Full Sample**", "**Urban vs Rural**")
  )

overall_table_rural <-
  tbl_stack(list(partab_merge_rural, partab_select_merge_rural), group_header = c("", ""))

