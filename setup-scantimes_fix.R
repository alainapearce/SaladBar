#This script was written by Alaina Pearce in Spring 2024
# to correct individual scan times for the Salad Bar study
#
#     Copyright (C) 2024 Alaina L Pearce
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

# fix individual cases ####

# wrong id matching
salad_bar_tte$randomized_student_id <- ifelse(salad_bar_tte$randomized_student_id == 1080, 13007, salad_bar_tte$randomized_student_id)

# remove duplicate ID
salad_bar_tte <- salad_bar_tte[salad_bar_tte$tray_id != 8880549, ]
salad_bar_tte <- salad_bar_tte[salad_bar_tte$tray_id != 8881879, ]


# fix assent times
salad_bar_tte[salad_bar_tte$randomized_student_id == 13007, 'A_AssentTime'] <- '11:08:31 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 13007, 'A_AssentTime_fixed'] <- '11:08:31'

salad_bar_tte[salad_bar_tte$randomized_student_id == 623, 'A_AssentTime'] <- '11:53:44 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 623, 'A_AssentTime_fixed'] <- '11:53:44'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2706, 'A_AssentTime'] <- '11:57:04 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2706, 'A_AssentTime_fixed'] <- '11:57:04'

salad_bar_tte[salad_bar_tte$randomized_student_id == 33979, 'A_AssentTime'] <- '11:57:04 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 33979, 'A_AssentTime_fixed'] <- '11:57:04'

# last latest lunch photo updates
salad_bar_tte[salad_bar_tte$randomized_student_id == 623, 'B_TimeofLatestPrelunchPhoto'] <- '11:55:21'
salad_bar_tte[salad_bar_tte$randomized_student_id == 623, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:55:21'

salad_bar_tte[salad_bar_tte$randomized_student_id == 26031, 'B_TimeofLatestPrelunchPhoto'] <- '12:10:22 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 26031, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:10:22'

salad_bar_tte[salad_bar_tte$randomized_student_id == 509, 'B_TimeofLatestPrelunchPhoto'] <- '10:53:00 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 509, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '10:53:00'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23422, 'B_TimeofLatestPrelunchPhoto'] <- '11:01:32 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23422, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:01:32'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23463, 'B_TimeofLatestPrelunchPhoto'] <- '11:02:07 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23463, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:02:07'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23453, 'B_TimeofLatestPrelunchPhoto'] <- '11:00:32 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23453, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:00:32'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23488, 'B_TimeofLatestPrelunchPhoto'] <- '11:24:22 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23488, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:24:22'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23511, 'B_TimeofLatestPrelunchPhoto'] <- '11:23:11 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23511, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:23:11'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23496, 'B_TimeofLatestPrelunchPhoto'] <- '11:23:57 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23496, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:23:57'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23485, 'B_TimeofLatestPrelunchPhoto'] <- '11:19:04 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23485, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:19:04'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23471, 'B_TimeofLatestPrelunchPhoto'] <- '11:21:46 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23471, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:21:46'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23480, 'B_TimeofLatestPrelunchPhoto'] <- '11:20:44 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23480, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:20:44'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23477, 'B_TimeofLatestPrelunchPhoto'] <- '11:22:59 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23477, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:22:59'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23545, 'B_TimeofLatestPrelunchPhoto'] <- '11:33:28 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23545, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:33:28'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23549, 'B_TimeofLatestPrelunchPhoto'] <- '11:32:50 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23549, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:32:50'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23538, 'B_TimeofLatestPrelunchPhoto'] <- '11:36:00 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23538, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:36:00'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23556, 'B_TimeofLatestPrelunchPhoto'] <- '11:35:22 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23556, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:35:22'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23553, 'B_TimeofLatestPrelunchPhoto'] <- '11:35:50 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23553, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:35:50'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23551, 'B_TimeofLatestPrelunchPhoto'] <- '11:31:59 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23551, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:31:59'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23571, 'B_TimeofLatestPrelunchPhoto'] <- '11:52:55 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23571, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:52:55'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23552, 'B_TimeofLatestPrelunchPhoto'] <- '11:35:01 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23552, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:35:01'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23564, 'B_TimeofLatestPrelunchPhoto'] <- '11:55:34 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23564, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:55:34'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23577, 'B_TimeofLatestPrelunchPhoto'] <- '11:52:26 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23577, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:52:26'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23594, 'B_TimeofLatestPrelunchPhoto'] <- '11:51:15 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23594, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:51:15'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23574, 'B_TimeofLatestPrelunchPhoto'] <- '11:51:45 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23574, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:51:45'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23597, 'B_TimeofLatestPrelunchPhoto'] <- '11:50:41 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23597, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:50:41'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23581, 'B_TimeofLatestPrelunchPhoto'] <- '11:53:35 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23581, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:53:35'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23646, 'B_TimeofLatestPrelunchPhoto'] <- '12:11:06 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23646, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:11:06'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23650, 'B_TimeofLatestPrelunchPhoto'] <- '12:06:46 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23650, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:06:46'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23651, 'B_TimeofLatestPrelunchPhoto'] <- '12:09:11 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23651, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:09:11'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23632, 'B_TimeofLatestPrelunchPhoto'] <- '12:09:48 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23632, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:09:48'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23622, 'B_TimeofLatestPrelunchPhoto'] <- '12:08:16 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23622, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:08:16'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23640, 'B_TimeofLatestPrelunchPhoto'] <- '12:07:35 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23640, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:07:35'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23620, 'B_TimeofLatestPrelunchPhoto'] <- '12:08:03 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23620, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:08:03'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23617, 'B_TimeofLatestPrelunchPhoto'] <- '12:10:03 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23617, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:10:03'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23652, 'B_TimeofLatestPrelunchPhoto'] <- '12:10:43 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23652, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:10:43'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23655, 'B_TimeofLatestPrelunchPhoto'] <- '12:08:56 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23655, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:08:56'

salad_bar_tte[salad_bar_tte$randomized_student_id == 23443, 'B_TimeofLatestPrelunchPhoto'] <- '10:59:35 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 23443, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '10:59:35'

salad_bar_tte[salad_bar_tte$randomized_student_id == 1184, 'B_TimeofLatestPrelunchPhoto'] <- '11:48:49 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 1184, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:48:49'

salad_bar_tte[salad_bar_tte$randomized_student_id == 30, 'B_TimeofLatestPrelunchPhoto'] <- '12:32:12 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 30, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:32:12'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2513, 'B_TimeofLatestPrelunchPhoto'] <- '11:38:00 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2513, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:38:00'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2736, 'B_TimeofLatestPrelunchPhoto'] <- '11:58:47 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2736, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:58:47'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2786, 'B_TimeofLatestPrelunchPhoto'] <- '12:18:16 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2786, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:18:16'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2812, 'B_TimeofLatestPrelunchPhoto'] <- '12:19:59 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2812, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:19:59'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2772, 'B_TimeofLatestPrelunchPhoto'] <- '12:22:03 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2772, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:22:03'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2813, 'B_TimeofLatestPrelunchPhoto'] <- '12:22:50 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2813, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:22:50'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2751, 'B_TimeofLatestPrelunchPhoto'] <- '12:21:25 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2751, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:21:25'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2770, 'B_TimeofLatestPrelunchPhoto'] <- '12:26:57 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2770, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:26:57'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2817, 'B_TimeofLatestPrelunchPhoto'] <- '12:22:13 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2817, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:22:13'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2817, 'B_TimeofLatestPrelunchPhoto'] <- '12:22:13 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2817, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:22:13'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2706, 'B_TimeofLatestPrelunchPhoto'] <- '12:06:13 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2706, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:06:13'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2848, 'B_TimeofLatestPrelunchPhoto'] <- '12:26:41 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2848, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:26:41'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2840, 'B_TimeofLatestPrelunchPhoto'] <- '12:21:36 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2840, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:21:36'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2749, 'B_TimeofLatestPrelunchPhoto'] <- '12:18:47 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2749, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:18:47'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2769, 'B_TimeofLatestPrelunchPhoto'] <- '12:22:09 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2769, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:22:09'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2794, 'B_TimeofLatestPrelunchPhoto'] <- '12:20:47 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2794, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:20:47'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2846, 'B_TimeofLatestPrelunchPhoto'] <- '12:24:26 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2846, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:24:26'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2754, 'B_TimeofLatestPrelunchPhoto'] <- '12:27:15 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2754, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:27:15'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2806, 'B_TimeofLatestPrelunchPhoto'] <- '12:26:34 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2806, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:26:34'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2760, 'B_TimeofLatestPrelunchPhoto'] <- '12:26:11 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2760, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '12:26:11'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35679, 'B_TimeofLatestPrelunchPhoto'] <- '11:02:28 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35679, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:02:28'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35501, 'B_TimeofLatestPrelunchPhoto'] <- '11:06:31 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35501, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:06:31'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35166, 'B_TimeofLatestPrelunchPhoto'] <- '11:03:49 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35166, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:03:49'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35323, 'B_TimeofLatestPrelunchPhoto'] <- '11:07:01 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35323, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:07:01'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35203, 'B_TimeofLatestPrelunchPhoto'] <- '11:02:43 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35203, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:02:43'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35241, 'B_TimeofLatestPrelunchPhoto'] <- '11:11:26 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35241, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:11:26'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35531, 'B_TimeofLatestPrelunchPhoto'] <- '11:12:13 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35531, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:12:13'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35231, 'B_TimeofLatestPrelunchPhoto'] <- '11:12:13 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35231, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:12:13'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35224, 'B_TimeofLatestPrelunchPhoto'] <- '11:04:48 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35224, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:04:48'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35437, 'B_TimeofLatestPrelunchPhoto'] <- '11:12:04 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35437, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:12:04'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35478, 'B_TimeofLatestPrelunchPhoto'] <- '11:11:38 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35478, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:11:38'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35352, 'B_TimeofLatestPrelunchPhoto'] <- '11:04:30 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35352, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:04:30'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35221, 'B_TimeofLatestPrelunchPhoto'] <- '11:05:27 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35221, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:05:27'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35367, 'B_TimeofLatestPrelunchPhoto'] <- '11:08:38 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35367, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:08:38'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35406, 'B_TimeofLatestPrelunchPhoto'] <- '11:05:05 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35406, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:05:05'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35294, 'B_TimeofLatestPrelunchPhoto'] <- '11:09:19 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35294, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:09:19'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35395, 'B_TimeofLatestPrelunchPhoto'] <- '11:06:35 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35395, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:06:35'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35424, 'B_TimeofLatestPrelunchPhoto'] <- '11:10:04 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35424, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:10:04'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35212, 'B_TimeofLatestPrelunchPhoto'] <- '11:12:28 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35212, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:12:28'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35503, 'B_TimeofLatestPrelunchPhoto'] <- '11:06:56 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35503, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:06:56'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35256, 'B_TimeofLatestPrelunchPhoto'] <- '11:11:43 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35256, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:11:43'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35392, 'B_TimeofLatestPrelunchPhoto'] <- '11:08:55 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35392, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:08:55'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35345, 'B_TimeofLatestPrelunchPhoto'] <- '11:04:02 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35345, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:04:02'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35379, 'B_TimeofLatestPrelunchPhoto'] <- '11:09:44 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35379, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:09:44'

salad_bar_tte[salad_bar_tte$randomized_student_id == 35178, 'B_TimeofLatestPrelunchPhoto'] <- '11:12:33 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 35178, 'B_TimeofLatestPrelunchPhoto_fixed'] <- '11:12:33'

# fix exit scanner
salad_bar_tte[salad_bar_tte$randomized_student_id == 498, 'D_ExitScanner'] <- '10:42:29 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 498, 'D_ExitScanner_fixed'] <- '10:42:29'

salad_bar_tte[salad_bar_tte$randomized_student_id == 26349, 'D_ExitScanner'] <- '11:59:08 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 26349, 'D_ExitScanner_fixed'] <- '11:59:08'

salad_bar_tte[salad_bar_tte$randomized_student_id == 26314, 'D_ExitScanner'] <- '11:57:12 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 26314, 'D_ExitScanner_fixed'] <- '11:57:12'

salad_bar_tte[salad_bar_tte$randomized_student_id == 25166, 'D_ExitScanner'] <- '12:12:34 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 25166, 'D_ExitScanner_fixed'] <- '12:12:34'

salad_bar_tte[salad_bar_tte$randomized_student_id == 2971, 'D_ExitScanner'] <- '11:56:13 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 2971, 'D_ExitScanner_fixed'] <- '11:56:13'

salad_bar_tte[salad_bar_tte$randomized_student_id == 3335, 'D_ExitScanner'] <- '11:47:06 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 3335, 'D_ExitScanner_fixed'] <- '11:47:06'

salad_bar_tte[salad_bar_tte$randomized_student_id == 3002, 'D_ExitScanner'] <- '11:53:04 AM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 3002, 'D_ExitScanner_fixed'] <- '11:53:04'

salad_bar_tte[salad_bar_tte$randomized_student_id == 25166, 'D_ExitScanner'] <- '12:12:34 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 25166, 'D_ExitScanner_fixed'] <- '12:12:34'

salad_bar_tte[salad_bar_tte$randomized_student_id == 37463, 'D_ExitScanner'] <- '12:01:20 PM'
salad_bar_tte[salad_bar_tte$randomized_student_id == 37463, 'D_ExitScanner_fixed'] <- '12:01:20'

# non-resolvable times
salad_bar_tte[salad_bar_tte$randomized_student_id == 482, 'D_ExitScanner'] <- NA
salad_bar_tte[salad_bar_tte$randomized_student_id == 482, 'D_ExitScanner_fixed'] <- NA