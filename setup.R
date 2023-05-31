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
# need to uncomment if running independently - not needed if compiling with TaskEF-Risk_paper.Rmd
#
# source('functions.R')
# library(haven)
# library(sjlabelled)
# library(reshape2)
# library(psycho)

#### Demographics ####

## Load Data ####
salad_bar_dat <- as.data.frame(read_spss("data/BL_elem_middle_high_wave1_wide_complete_cases_clean_2023-03-03.sav"))

names(salad_bar_dat)[16:18] <- c('fv_pre', 'fv_post', 'fv_consumed')

# make slected variable
salad_bar_dat[['fv_selected']] <- ifelse(salad_bar_dat[['fv_pre']] > 0, 'Y', 'N')
salad_bar_dat[['fv_prop_waste']] <- (salad_bar_dat[['fv_post']]/salad_bar_dat[['fv_pre']])*100

# ensure all exclusion for weights are made
salad_bar_dat[['fv_exclude']] <- ifelse(salad_bar_dat[['fv_pre']] < salad_bar_dat[['fv_post']], 'Y', 'N')

# fix NAs
salad_bar_dat[['gender']] <- ifelse(salad_bar_dat[['gender']] == 'NULL', NA, as.character(salad_bar_dat[['gender']]))

salad_bar_dat[['paid_free_reduced']] <- ifelse(salad_bar_dat[['paid_free_reduced']] == '-999', NA, as.character(salad_bar_dat[['paid_free_reduced']]))

salad_bar_dat[['race']] <- ifelse(salad_bar_dat[['race']] == '-999', NA, as.character(salad_bar_dat[['race']]))

salad_bar_dat[['ethnicity']] <- ifelse(salad_bar_dat[['ethnicity']] == '-999', NA, as.character(salad_bar_dat[['ethnicity']]))

salad_bar_dat[['age']] <- ifelse(salad_bar_dat[['age']] == '-999', NA, as.numeric(salad_bar_dat[['age']]))

# fix race categories
salad_bar_dat[['race']] <- ifelse(salad_bar_dat[['race']] == 'Asian' | salad_bar_dat[['race']] == 'Asian or Pacific Islander' | salad_bar_dat[['race']] == 'Native Hawaiian or Other Pacific Islande', 'APPI', as.character(salad_bar_dat[['race']]))

# fix free-reduced lunch categories
salad_bar_dat[['paid_free_reduced']] <- ifelse(salad_bar_dat[['paid_free_reduced']] == 'F' | salad_bar_dat[['paid_free_reduced']] == 'R' | salad_bar_dat[['paid_free_reduced']] == 'C', 'Free/Reduced', ifelse(salad_bar_dat[['paid_free_reduced']] == 'P', 'Paid', as.character(salad_bar_dat[['paid_free_reduced']])))

# race/ethnicity
salad_bar_dat[['race_ethnicity']] <- ifelse(is.na(salad_bar_dat[['ethnicity']]) | salad_bar_dat[['ethnicity']] == 'Not Hispanic or Latino', ifelse(salad_bar_dat[['race']] == 'Two or More Races' | salad_bar_dat[['race']] == 'American Indian or Alaska Native' | salad_bar_dat[['race']] == 'APPI', 'Other', ifelse(salad_bar_dat[['race']] == 'Prefer not to answer', NA, as.character(salad_bar_dat[['race']]))), salad_bar_dat[['ethnicity']])

salad_bar_dat[['race_ethnicity']] <- factor(salad_bar_dat[['race_ethnicity']], levels = c('Hispanic or Latino', 'White', 'Other', 'Black or African American'))

## Load in rurality table ####
rural_dat <- read.csv('data/Rural-Urban School Coding.csv')
names(rural_dat)[c(1, 7, 6)] <- c('school_name', 'rurality', 'suburb') 

# merge classification
salad_bar_dat <- merge(salad_bar_dat, rural_dat[c(1, 7, 6)], by = 'school_name', all.x = TRUE)

salad_bar_dat[['rurality']] <- factor(salad_bar_dat[['rurality']], levels = c('Urban', 'Rural'))

salad_bar_dat[['suburb']] <- ifelse(salad_bar_dat[['suburb']] == 'Suburb, Small' | salad_bar_dat[['suburb']] == 'Suburb, Large', 'Y', 'N')

# Time to Eat
salad_bar_tte <- as.data.frame(read_spss("data/BL_elem_middle_high_wave1_wide_complete_cases_clean_2023-03-03_deidentified_TTE.sav"))

names(salad_bar_tte)[76] <- 'time_to_eat'

salad_bar_tte$LunchPeriodStart_parsed <- parse_date_time(salad_bar_tte$LunchPeriodStart, '%I:%M:%S %p')
salad_bar_tte$LunchPeriodEnd_parsed <- parse_date_time(salad_bar_tte$LunchPeriodEnd, '%I:%M:%S %p')

salad_bar_tte$lunch_dur <- time_length(salad_bar_tte$LunchPeriodEnd_parsed - salad_bar_tte$LunchPeriodStart_parsed, unit = 'min')

salad_bar_tte$time_to_eat <- time_length(hms(salad_bar_tte$time_to_eat), unit = 'min')

# merge tte
salad_bar_dat <- merge(salad_bar_dat, salad_bar_tte[c(11, 76, 80)], by = 'randomized_student_id', all.x = TRUE)

salad_bar_dat$tte_dat <- ifelse(is.na(salad_bar_dat$time_to_eat), 'Missing', 'Complete')

salad_bar_dat_use <- salad_bar_dat[salad_bar_dat[['fv_exclude']] == 'N' & salad_bar_dat[['tte_dat']] == 'Complete' & !is.na(salad_bar_dat[['fv_pre']]) & !is.na(salad_bar_dat[['fv_post']]), ]
