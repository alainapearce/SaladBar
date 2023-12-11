#This script was written by Alaina Pearce in Fall 2023
# to correct lunch timing for the Salad Bar study
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
# library(lubridate)

#uncomment if running independently
#salad_bar_tte <- read.csv("data/BL_elem_middle_high_wave1_wide_complete_cases_clean_2023-03-03_deidentified_TTE.csv", na.strings = '#N/A')

# inital assent time processing to set lunch periods
salad_bar_tte$assent_time <- parse_date_time(salad_bar_tte$A_AssentTime_fixed, '%H:%M:%S')

# fix reported lunch duration

## 1 - Madison Meadows Middle School - end set equal to start so need to update all lunch end times
# still need to figure out meal start times
salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 1,
                                       ifelse(salad_bar_tte$grade == 6, '12:50:00 PM',
                                              ifelse(salad_bar_tte$grade == 7, '11:44:00 AM', '12:05:00 PM')), salad_bar_tte$LunchPeriodEnd)

## 2 - Camelback Academy

# fix NA values
salad_bar_tte$LunchPeriodStart <- ifelse(salad_bar_tte$school_id == 2,
                                         ifelse(salad_bar_tte$grade == 2, 
                                                ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 11:00:00 UTC'), unit = 'min') > 0, '11:10:00 AM',
                                                       ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 10:40:00 UTC'), unit = 'min') > 0, '10:45:00 AM', salad_bar_tte$LunchPeriodStart)),
                                                ifelse(salad_bar_tte$grade == 3 | salad_bar_tte$grade == 4, 
                                                       ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 12:35:00 UTC'), unit = 'min') > 0, '12:40:00 PM', salad_bar_tte$LunchPeriodStart), salad_bar_tte$LunchPeriodStart)), salad_bar_tte$LunchPeriodStart)

salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 2,
                                       ifelse(salad_bar_tte$grade == 2, 
                                              ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 11:00:00 UTC'), unit = 'min') > 0, '11:30:00 AM',
                                                     ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 10:40:00 UTC'), unit = 'min') > 0, '11:05:00 AM', salad_bar_tte$LunchPeriodEnd)),
                                              ifelse(salad_bar_tte$grade == 3 | salad_bar_tte$grade == 4, 
                                                     ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 12:35:00 UTC'), unit = 'min') > 0, '01:00:00 PM', salad_bar_tte$LunchPeriodEnd), salad_bar_tte$LunchPeriodEnd)), salad_bar_tte$LunchPeriodEnd)

#re-sort 5th graders
salad_bar_tte$LunchPeriodStart <- ifelse(salad_bar_tte$school_id == 2 & salad_bar_tte$grade == 5, 
                                         ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 12:35:00 UTC'), unit = 'min') > 0, '12:40:00 PM',
                                                ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 11:50:00 UTC'), unit = 'min') > 0, '11:55:00 AM', salad_bar_tte$LunchPeriodStart)), salad_bar_tte$LunchPeriodStart)

salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 2 & salad_bar_tte$grade == 5, 
                                       ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 12:40:00 UTC'), unit = 'min') > 0, '01:00:00 PM',
                                              ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 11:50:00 UTC'), unit = 'min') > 0, '12:15:00 PM', salad_bar_tte$LunchPeriodEnd)), salad_bar_tte$LunchPeriodEnd)

## 3 - Paideia Academy
salad_bar_tte$LunchPeriodStart <- ifelse(salad_bar_tte$school_id == 3 & salad_bar_tte$grade == 1, 
                                         ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 12:15:00 UTC'), unit = 'min') > 0, '12:15:00 PM', salad_bar_tte$LunchPeriodStart), salad_bar_tte$LunchPeriodStart)

salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 3 & salad_bar_tte$grade == 1, 
                                       ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 12:15:00 UTC'), unit = 'min') > 0, '12:45:00 PM', salad_bar_tte$LunchPeriodEnd), salad_bar_tte$LunchPeriodEnd)

## missing school_id 4??

## 5 - Eloy Junior High

### still need to figure out 7th grade
salad_bar_tte$LunchPeriodStart <- ifelse(salad_bar_tte$school_id == 5, 
                                         ifelse(salad_bar_tte$grade == 6, '11:30:00 AM', 
                                                ifelse(salad_bar_tte$grade == 7, '12:00:00 PM', 
                                                       ifelse(salad_bar_tte$grade == 8, '12:00:00 PM', salad_bar_tte$LunchPeriodStart))), salad_bar_tte$LunchPeriodStart)

salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 5, 
                                       ifelse(salad_bar_tte$grade == 6, '12:00:00 PM', 
                                              ifelse(salad_bar_tte$grade == 7, '12:30:00 PM',
                                                     ifelse(salad_bar_tte$grade == 8, '12:30:00 PM', salad_bar_tte$LunchPeriodEnd))), salad_bar_tte$LunchPeriodEnd)

## 6 - Aguilar Elementary
salad_bar_tte$LunchPeriodStart <- ifelse(salad_bar_tte$school_id == 6, 
                                         ifelse(salad_bar_tte$grade == 1, '11:25:00 AM',
                                                ifelse(salad_bar_tte$grade == 2, '11:00:00 AM',
                                                       ifelse(salad_bar_tte$grade == 3, '11:45:00 AM',
                                                              ifelse(salad_bar_tte$grade == 4, '11:55:00 AM', '12:25:00 PM')))), salad_bar_tte$LunchPeriodStart)

salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 6, 
                                       ifelse(salad_bar_tte$grade == 1, '11:45:00 AM',
                                              ifelse(salad_bar_tte$grade == 2, '11:20:00 AM',
                                                     ifelse(salad_bar_tte$grade == 3, '12:05:00 PM',
                                                            ifelse(salad_bar_tte$grade == 4, '12:15:00 PM', '12:45:00 PM')))), salad_bar_tte$LunchPeriodEnd)


## 7 - Coronado High school - solution unclear

## 8 - Reyes Maria Ruiz Leadership Academy - end set equal to start so need to update all lunch end times
# still need to figure out meal start times
salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 8,
                                       ifelse(salad_bar_tte$grade == 1, '11:25:00 AM',
                                              ifelse(salad_bar_tte$grade == 2, '11:50:00 AM', 
                                                     ifelse(salad_bar_tte$grade == 2, '12:00:00 PM',   
                                                            ifelse(salad_bar_tte$grade == 2, '12:15:00 PM', '12:30:00 PM')))), salad_bar_tte$LunchPeriodEnd)

## 9 - Arizona City Elementary School - end set equal to start so need to update all lunch end times
# still need to figure out meal start times
salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 9,
                                       ifelse(salad_bar_tte$grade == 1, '11:35:00 AM',
                                              ifelse(salad_bar_tte$grade == 2, '11:45:00 AM', '12:45:00 PM')), salad_bar_tte$LunchPeriodEnd)

## 10 - Saguaro High School - end set equal to start so need to update all lunch end times
salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 10, '12:26:00 PM', salad_bar_tte$LunchPeriodEnd)

## 11 - Queen Creek Middle School - solution unclear

## 12 - Cactus Shadow - end set equal to start so need to update all lunch end times
salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 12, '12:09:00 PM', salad_bar_tte$LunchPeriodEnd)

## 13 - Franklin Phonetic School - end set equal to start so need to update all lunch end times
salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 13,
                                       ifelse(salad_bar_tte$grade == 1, '11:15:00 AM',
                                              ifelse(salad_bar_tte$grade == 2, '11:30:00 AM', 
                                                     ifelse(salad_bar_tte$grade == 2, '11:45:00 AM', 
                                                            ifelse(salad_bar_tte$grade == 2, '12:00:00 PM','12:15:00 PM')))), salad_bar_tte$LunchPeriodEnd)

## 14 - Pan-American Elementary School
salad_bar_tte$LunchPeriodStart <- ifelse(salad_bar_tte$school_id == 14,
                                         ifelse(salad_bar_tte$grade == 3, '11:00:00 AM',
                                                ifelse(salad_bar_tte$grade == 4, '11:10:00 AM','11:20:00 AM')), salad_bar_tte$LunchPeriodStart)

salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 14,
                                       ifelse(salad_bar_tte$grade == 3, '11:45:00 AM',
                                              ifelse(salad_bar_tte$grade == 4, '11:55:00 AM','12:05:00 PM')), salad_bar_tte$LunchPeriodEnd)


## 15 - Pan-American High School - solution not clear


## 16 - Queen Creek High School
salad_bar_tte$LunchPeriodStart <- ifelse(salad_bar_tte$school_id == 16, 
                                         ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 11:25:00 UTC'), unit = 'min') > 0, '11:30:00 AM', salad_bar_tte$LunchPeriodStart), salad_bar_tte$LunchPeriodStart)

salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 16, 
                                       ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 11:25:00 UTC'), unit = 'min') > 0, '12:00:00 PM', salad_bar_tte$LunchPeriodEnd), salad_bar_tte$LunchPeriodEnd)


## 17 - Leading Edge Maricopa - Correct!!!

## 18 - St. Vincent de Paul - matches lunch schedule, most are 5 min late on exit scan

## 19 - EAGLE Prep - fixed below bc don't have schedule (set lunch dur by exit scans)

## 20 - Solano Elementary School - matches lunch schedule, most are 5 min late on exit scan

## 21 - Broadmor Elementary School - Correct!!!

## 22 - Academia del Pueblo - solution not clear

## 23 - Black Mountain Elementary - Correct!!!

## 24 - Beaver Creek Elementary - fixed below bc don't have better lunch schedule for 5th grade

## 25 - Southgate Academy High School - solution not clear

## 26 - Isaac Middle School - group decision
salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 26 & salad_bar_tte$grade == 6, '11:26:00 AM', salad_bar_tte$LunchPeriodEnd)

## 27 - High Desert Middle School - solution unclear

## 28 - Pensar Academy
salad_bar_tte$LunchPeriodStart <- ifelse(salad_bar_tte$school_id == 28, 
                                         ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 12:40:00 UTC'), unit = 'min') > 0, '12:45:00 PM', 
                                                ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 12:25:00 UTC'), unit = 'min') > 0, '12:30:00 PM', 
                                                       ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 12:10:00 UTC'), unit = 'min') > 0, '12:15:00 PM', 
                                                              ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 11:55:00 UTC'), unit = 'min') > 0, '12:00:00 PM',
                                                                     ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 11:40:00 UTC'), unit = 'min') > 0, '11:45:00 AM', '11:30:00 AM'))))), salad_bar_tte$LunchPeriodStart)

salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 28, 
                                       ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 12:40:00 UTC'), unit = 'min') > 0, '01:00:00 PM', 
                                              ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 12:25:00 UTC'), unit = 'min') > 0, '12:45:00 PM', 
                                                     ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 12:10:00 UTC'), unit = 'min') > 0, '12:30:00 PM', 
                                                            ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 11:55:00 UTC'), unit = 'min') > 0, '12:15:00 PM',
                                                                   ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 11:40:00 UTC'), unit = 'min') > 0, '12:00:00 PM', '11:45:00 AM'))))), salad_bar_tte$LunchPeriodEnd)

## 29 - Gila Ridge High School
salad_bar_tte$LunchPeriodStart <- ifelse(salad_bar_tte$school_id == 29, 
                                         ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 11:55:00 UTC'), unit = 'min') > 0, '12:05:00 PM', 
                                                ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 10:55:00 UTC'), unit = 'min') > 0, '11:00:00 AM', salad_bar_tte$LunchPeriodStart)), salad_bar_tte$LunchPeriodStart)

salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 29, 
                                       ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 11:55:00 UTC'), unit = 'min') > 0, '12:35:00 PM', 
                                              ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 10:55:00 UTC'), unit = 'min') > 0, '11:30:00 AM', salad_bar_tte$LunchPeriodEnd)), salad_bar_tte$LunchPeriodEnd)


## 30 - Yuma High School
salad_bar_tte$LunchPeriodStart <- ifelse(salad_bar_tte$school_id == 30, 
                                         ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 11:55:00 UTC'), unit = 'min') > 0, '12:03:00 PM', '10:57:00 AM'), salad_bar_tte$LunchPeriodStart)

salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 30, 
                                       ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 11:55:00 UTC'), unit = 'min') > 0, '12:33:00 PM', '11:27:00 AM'), salad_bar_tte$LunchPeriodEnd)

## 31 - Cesar Chavez High School - Correct !!!

## 32 - no school_id with 32

## 33 - San Manuel High School
#note - different Wednesday schedules
salad_bar_tte$LunchPeriodStart <- ifelse(salad_bar_tte$school_id == 33, '11:20:00 AM', salad_bar_tte$LunchPeriodStart)

salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 33, '11:45:00 AM', salad_bar_tte$LunchPeriodEnd)

## 34 - Magma Ranch Middle School - still have 1 to address
salad_bar_tte$LunchPeriodStart <- ifelse(salad_bar_tte$school_id == 34 & salad_bar_tte$grade == 8, 
                                         ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 01:00:00 UTC'), unit = 'min') < 0, '01:06:00 PM', salad_bar_tte$LunchPeriodStart), salad_bar_tte$LunchPeriodStart)

salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 34 & salad_bar_tte$grade == 8, 
                                       ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 01:00:00 UTC'), unit = 'min') < 0, '01:26:00 PM', salad_bar_tte$LunchPeriodEnd), salad_bar_tte$LunchPeriodEnd)

## 35 - Toltecalli High School - Correct !!!

## 36 - GateWay Early College High School - Correct !!!

## 37 - Academy of Math and Science(Flower) Middle School
#note - different Friday schedules
salad_bar_tte$LunchPeriodStart <- ifelse(salad_bar_tte$school_id == 37, '12:23:00 PM', salad_bar_tte$LunchPeriodStart)

salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 37, '12:48:00 PM', salad_bar_tte$LunchPeriodEnd)

## 38 - Payne Jr. High School
salad_bar_tte$LunchPeriodStart <- ifelse(salad_bar_tte$school_id == 38, 
                                         ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 12:20:00 UTC'), unit = 'min') > 0, '12:24:00 PM', 
                                                ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 11:45:00 UTC'), unit = 'min') > 0, '11:47:00 AM', salad_bar_tte$LunchPeriodStart)), salad_bar_tte$LunchPeriodStart)

salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 38, 
                                       ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 12:20:00 UTC'), unit = 'min') > 0, '12:57:00 PM', 
                                              ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 11:45:00 UTC'), unit = 'min') > 0, '12:20:00 PM', salad_bar_tte$LunchPeriodEnd)), salad_bar_tte$LunchPeriodEnd)

## 39 - Alice Vail - 1 student attended different lunch period
salad_bar_tte$LunchPeriodStart <- ifelse(salad_bar_tte$school_id == 39 & salad_bar_tte$grade == 6, 
                                         ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 12:00:00 UTC'), unit = 'min') > 0, '12:04:00 PM', salad_bar_tte$LunchPeriodStart), salad_bar_tte$LunchPeriodStart)

salad_bar_tte$LunchPeriodEnd <- ifelse(salad_bar_tte$school_id == 39 & salad_bar_tte$grade == 6, 
                                       ifelse(time_length(salad_bar_tte$assent_time - as_datetime('0000-01-01 12:00:00 UTC'), unit = 'min') > 0, '12:39:00 PM', salad_bar_tte$LunchPeriodEnd), salad_bar_tte$LunchPeriodEnd)

## covert times
salad_bar_tte$lunch_start <- parse_date_time(salad_bar_tte$LunchPeriodStart, '%H:%M:%S %p')
salad_bar_tte$lunch_end <- parse_date_time(salad_bar_tte$LunchPeriodEnd, '%H:%M:%S %p')

salad_bar_tte$lunch_dur <- time_length(salad_bar_tte$lunch_end - salad_bar_tte$lunch_start, unit = 'min')

# lunch duration - these we can fill in based on other entries that match school and grade level
salad_bar_tte[['lunch_dur']] <- ifelse(is.na(salad_bar_tte[['lunch_dur']]) & salad_bar_tte[['school_id']] == 2, 20, ifelse(
  is.na(salad_bar_tte[['lunch_dur']]) & salad_bar_tte[['school_id']] == 12, 40, ifelse(
    is.na(salad_bar_tte[['lunch_dur']]) & salad_bar_tte[['school_id']] == 38, 33, ifelse(
      is.na(salad_bar_tte[['lunch_dur']]) & salad_bar_tte[['school_id']] == 27 & salad_bar_tte[['grade']] == 6, 34, ifelse(
        is.na(salad_bar_tte[['lunch_dur']]) & salad_bar_tte[['school_id']] == 26 & salad_bar_tte[['grade']] == 6, 35, salad_bar_tte[['lunch_dur']])))))


## Beaver Creak - group decision
salad_bar_tte[['lunch_dur']] <- ifelse(salad_bar_tte[['school_id']] == 24 & salad_bar_tte[['grade']] == 5, 15, salad_bar_tte[['lunch_dur']])

## EAGLE Prep - group decision = max time to eat (max = 24.567, set lunch_dur to 25 min)
salad_bar_tte[['lunch_dur']] <- ifelse(salad_bar_tte[['school_id']] == 19,
                                       ifelse(salad_bar_tte[['grade']] <= 2, 20, 
                                              ifelse(salad_bar_tte[['grade']] < 5, 25, 20)), salad_bar_tte[['lunch_dur']])

#make assent time military time - only one that wasn't in military time from scanner
salad_bar_tte$A_AssentTime_fixed <- ifelse(grepl('^1:', salad_bar_tte$A_AssentTime_fixed), sub('1:', '13:', salad_bar_tte$A_AssentTime_fixed), salad_bar_tte$A_AssentTime_fixed)

# format times
salad_bar_tte$assent_time <- parse_date_time(salad_bar_tte$A_AssentTime_fixed, '%H:%M:%S')
salad_bar_tte$pre_photo <- parse_date_time(salad_bar_tte$B_TimeofLatestPrelunchPhoto_fixed, '%H:%M:%S')
salad_bar_tte$exit_time <- parse_date_time(salad_bar_tte$D_ExitScanner_fixed, '%H:%M:%S')

#convert to minutes
salad_bar_tte$time_line <- time_length(salad_bar_tte$pre_photo - salad_bar_tte$assent_time, unit = 'min')
salad_bar_tte$time_to_eat <- time_length(salad_bar_tte$exit_time - salad_bar_tte$pre_photo, unit = 'min')
