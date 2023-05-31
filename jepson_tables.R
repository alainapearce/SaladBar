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
partab_data <- salad_bar_dat[c(56:62, 68)]

partab_all <-
  tbl_summary(
    data = partab_data,
    value = list(gender ~ "Gender", grade ~ "grade", age ~ "Age, yr", race ~ "Race", ethnicity ~ "Ethnicity", paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "Fruit/Veg Selected", race_ethnicity ~ 'Race/Ethnicity'),
    label = list(gender ~ "Gender", grade ~ "grade", age ~ "Age, yr", race ~ "Race", ethnicity ~ "Ethnicity", paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "Fruit/Veg Selected", race_ethnicity ~ 'Race/Ethnicity'),
    type = list(gender ~ "categorical", grade ~ "continuous", age ~ "continuous", race ~ "categorical", ethnicity ~ "categorical", paid_free_reduced ~ "categorical", fv_selected ~ "categorical", race_ethnicity ~ 'categorical'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1)

partab_rural_data <- salad_bar_dat[c(69, 56:62, 68)]

partab_rural <-
  tbl_summary(
    data = partab_rural_data,
    by = 'rurality',
    value = list(gender ~ "Gender", grade ~ "grade", age ~ "Age, yr", race ~ "Race", ethnicity ~ "Ethnicity", paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "Fruit/Veg Selected", race_ethnicity ~ 'Race/Ethnicity'),
    label = list(gender ~ "Gender", grade ~ "grade", age ~ "Age, yr", race ~ "Race", ethnicity ~ "Ethnicity", paid_free_reduced ~ "Free-Reduced Lunch", fv_selected ~ "Fruit/Veg Selected", race_ethnicity ~ 'Race/Ethnicity'),
    type = list(gender ~ "categorical", grade ~ "continuous", age ~ "continuous", race ~ "categorical", ethnicity ~ "categorical", paid_free_reduced ~ "categorical", fv_selected ~ "categorical", race_ethnicity ~ 'categorical'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
add_p()

partab_merge <-
  tbl_merge(
    tbls = list(partab_all, partab_rural),
    tab_spanner = c("**Full Sample**", "**Urban v Rural**")
  )

## intake tables ####
fvtab_data <- salad_bar_dat[salad_bar_dat[['fv_selected']] == 'Y', c(17, 19, 18, 63:67)]

fvtab_all <-
  tbl_summary(
    data = fvtab_data,
    value = list(fv_pre ~ "Fruit/Veg Served, g", fv_consumed ~ 'Fruit/Veg Consumed, g', fv_post ~ 'Fruit/Veg Waste, g', fv_prop_waste ~ 'Fruit/Veg Prop Waste (post/pre)', fv_exclude ~ 'Excluded Intake', fv_pre_out ~ 'Fruit/Veg Served Outlier', fv_consumed_out ~ 'Fruit/Veg Consumed Outlier', fv_post_out ~ 'Fruit/Veg Waste Outlier'),
    label = list(fv_pre ~ "Fruit/Veg Served, g", fv_consumed ~ 'Fruit/Veg Consumed, g', fv_post ~ 'Fruit/Veg Waste, g', fv_prop_waste ~ 'Fruit/Veg Prop Waste (post/pre)', fv_exclude ~ 'Excluded Intake', fv_pre_out ~ 'Fruit/Veg Served Outlier', fv_consumed_out ~ 'Fruit/Veg Consumed Outlier', fv_post_out ~ 'Fruit/Veg Waste Outlier'),
    type = list(fv_pre ~ "continuous", fv_consumed ~ 'continuous', fv_post ~ 'continuous', fv_prop_waste ~ 'continuous', fv_exclude ~ 'categorical', fv_pre_out ~ 'categorical', fv_consumed_out ~ 'categorical', fv_post_out ~ 'categorical'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  add_n()

fvtab_rural_data <- salad_bar_dat[salad_bar_dat[['fv_selected']] == 'Y', c(69, 17, 19, 18, 63:67)]

fvtab_rural <-
  tbl_summary(
    data = fvtab_rural_data,
    by = 'rurality',
    value = list(fv_pre ~ "Fruit/Veg Selected, g", fv_consumed ~ 'Fruit/Veg Consumed, g', fv_post ~ 'Fruit/Veg Waste, g', fv_prop_waste ~ 'Fruit/Veg Prop Waste (post/pre)', fv_exclude ~ 'Excluded Intake', fv_pre_out ~ 'Fruit/Veg Served Outlier', fv_consumed_out ~ 'Fruit/Veg Consumed Outlier', fv_post_out ~ 'Fruit/Veg Waste Outlier'),
    label = list(fv_pre ~ "Fruit/Veg Selected, g", fv_consumed ~ 'Fruit/Veg Consumed, g', fv_post ~ 'Fruit/Veg Waste, g', fv_prop_waste ~ 'Fruit/Veg Prop Waste (post/pre)', fv_exclude ~ 'Excluded Intake', fv_pre_out ~ 'Fruit/Veg Served Outlier', fv_consumed_out ~ 'Fruit/Veg Consumed Outlier', fv_post_out ~ 'Fruit/Veg Waste Outlier'),
    type = list(fv_pre ~ "continuous", fv_consumed ~ 'continuous', fv_post ~ 'continuous', fv_prop_waste ~ 'continuous', fv_exclude ~ 'categorical', fv_pre_out ~ 'categorical', fv_consumed_out ~ 'categorical', fv_post_out ~ 'categorical'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
  add_n() %>%
  add_p()

fvtab_merge <-
  tbl_merge(
    tbls = list(fvtab_all, fvtab_rural),
    tab_spanner = c("**Full Sample**", "**Urban v Rural**")
  )

