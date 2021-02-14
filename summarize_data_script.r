######################################################
########## NFL Big Data Bowl Running Script ##########
######################################################
#
#
#
#
#


########## Section 1 - Set Up ##########

# Load packages
library(matlib)
library(tidyverse)
library(xgboost)
library(ggplot2)
library(data.table)

# Load Functions
source("nfl_function_file2.r")

# Load players
players <- read.csv("players.csv")
# Load plays
plays <- read.csv("plays.csv")
# Load games
games <- read.csv("games.csv")


########## Section 2 - Process Data ##########

###### 2-1 Week 1 ######

# Load week 1 plays
week <- read.csv("week1.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                      'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                      'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 1 plays
rm(week)

# Calculate statistics
week_1_stats <- stat_calc(pbf, plays)


###### 2-2 Week 2 ######

# Load week 2 plays
week <- read.csv("week2.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                  'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                  'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 2 plays
rm(week)

# Calculate statistics
week_2_stats <- stat_calc(pbf, plays)



###### 2-3 Week 3 ######

# Load week 3 plays
week <- read.csv("week3.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                  'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                  'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 3 plays
rm(week)

# Calculate statistics
week_3_stats <- stat_calc(pbf, plays)


###### 2-4 Week 4 ######

# Load week 4 plays
week <- read.csv("week4.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                  'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                  'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 4 plays
rm(week)

# Calculate statistics
week_4_stats <- stat_calc(pbf, plays)


###### 2-5 Week 5 ######

# Load week 5 plays
week <- read.csv("week5.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                  'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                  'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 5 plays
rm(week)

# Calculate statistics
week_5_stats <- stat_calc(pbf, plays)


###### 2-6 Week 6 ######

# Load week 6 plays
week <- read.csv("week6.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                  'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                  'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 6 plays
rm(week)

# Calculate statistics
week_6_stats <- stat_calc(pbf, plays)


###### 2-7 Week 7 ######

# Load week 7 plays
week <- read.csv("week4.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                  'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                  'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 7 plays
rm(week)

# Calculate statistics
week_7_stats <- stat_calc(pbf, plays)


###### 2-8 Week 8 ######

# Load week 8 plays
week <- read.csv("week8.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                  'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                  'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 8 plays
rm(week)

# Calculate statistics
week_8_stats <- stat_calc(pbf, plays)


###### 2-9 Week 9 ######

# Load week 9 plays
week <- read.csv("week9.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                  'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                  'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 9 plays
rm(week)

# Calculate statistics
week_9_stats <- stat_calc(pbf, plays)


###### 2-10 Week 10 ######

# Load week 10 plays
week <- read.csv("week10.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                  'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                  'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 10 plays
rm(week)

# Calculate statistics
week_10_stats <- stat_calc(pbf, plays)


###### 2-11 Week 11 ######

# Load week 11 plays
week <- read.csv("week11.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                  'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                  'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 11 plays
rm(week)

# Calculate statistics
week_11_stats <- stat_calc(pbf, plays)


###### 2-12 Week 12 ######

# Load week 12 plays
week <- read.csv("week12.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                  'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                  'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 12 plays
rm(week)

# Calculate statistics
week_12_stats <- stat_calc(pbf, plays)


###### 2-13 Week 13 ######

# Load week 13 plays
week <- read.csv("week13.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                  'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                  'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 13 plays
rm(week)

# Calculate statistics
week_13_stats <- stat_calc(pbf, plays)


###### 2-14 Week 14 ######

# Load week 14 plays
week <- read.csv("week14.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                  'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                  'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 14 plays
rm(week)

# Calculate statistics
week_14_stats <- stat_calc(pbf, plays)


###### 2-15 Week 15 ######

# Load week 15 plays
week <- read.csv("week15.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                  'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                  'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 15 plays
rm(week)

# Calculate statistics
week_15_stats <- stat_calc(pbf, plays)


###### 2-16 Week 16 ######

# Load week 16 plays
week <- read.csv("week16.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                  'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                  'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 16 plays
rm(week)

# Calculate statistics
week_16_stats <- stat_calc(pbf, plays)


###### 2-17 Week 17 ######

# Load week 17 plays
week <- read.csv("week17.csv")

#' find the ball for tracking purposes
week$off_def <- rep('Ball', nrow(week))
#' find defenders for tracking purposes
week$off_def[week$position %in% c('CB','DB','DE','DT','ILB','LB','MLB',
                                  'NT','OLB','FS','SS','S')] <- "Defense"
#' find offensive players for tracking purposes
week$off_def[week$position %in% c('HB','K','P','QB','RB','TE','WR',
                                  'LS','FB')] <- "Offense"

# Fix coordinates
pbf <- fix_coords(week, plays, games)

# Remove week 17 plays
rm(week)

# Calculate statistics
week_17_stats <- stat_calc(pbf, plays)


########## Section 3 - Join Data ##########

stat_1 <- rbindlist(week_1_stats)
stat_2 <- rbindlist(week_2_stats)
stat_3 <- rbindlist(week_3_stats)
stat_4 <- rbindlist(week_4_stats)
stat_5 <- rbindlist(week_5_stats)
stat_6 <- rbindlist(week_6_stats)
stat_7 <- rbindlist(week_7_stats)
stat_8 <- rbindlist(week_8_stats)
stat_9 <- rbindlist(week_9_stats)
stat_10 <- rbindlist(week_10_stats)
stat_11 <- rbindlist(week_11_stats)
stat_12 <- rbindlist(week_12_stats)
stat_13 <- rbindlist(week_13_stats)
stat_14 <- rbindlist(week_14_stats)
stat_15 <- rbindlist(week_15_stats)
stat_16 <- rbindlist(week_16_stats)
stat_17 <- rbindlist(week_17_stats)

comb_data <- rbind.data.frame(stat_1, 
                              stat_2,
                              stat_3,
                              stat_4,
                              stat_5,
                              stat_6,
                              stat_7,
                              stat_8,
                              stat_9,
                              stat_10,
                              stat_11,
                              stat_12,
                              stat_13,
                              stat_14,
                              stat_15,
                              stat_16,
                              stat_17)

save(comb_data, file = "combined_data.rda")






































































