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

stat_1$week <- 1
stat_2$week <- 2
stat_3$week <- 3
stat_4$week <- 4
stat_5$week <- 5
stat_6$week <- 6
stat_7$week <- 7
stat_8$week <- 8
stat_9$week <- 9
stat_10$week <- 10
stat_11$week <- 11
stat_12$week <- 12
stat_13$week <- 13
stat_14$week <- 14
stat_15$week <- 15
stat_16$week <- 16
stat_17$week <- 17


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


playing_teams <- as.data.frame(matrix(NA, nrow = length(games$gameId), 
                                      ncol = 3))

for (i in 1:length(games$gameId)){
  playing_teams[i,] <- games[i,c('gameId','homeTeamAbbr', 'visitorTeamAbbr')]
}

def_team <- rep(NA, length(comb_data$week))

for (i in 1:length(comb_data$gameId)){
  team_1 <- as.vector(playing_teams[playing_teams$V1 == comb_data$gameId[i],2:3])
  def_team[i] <- team_1[!team_1 %in% comb_data$possessionTeam[i]]
}

comb_data$def_team <- unlist(def_team)

########## Predict PI ##########

library(xgboost)

target_on_a <- comb_data[comb_data$target_rec == 1, c(1:15,31:63)]
target_on_e <- comb_data[comb_data$target_rec_e == 1, c(16:63)]

names(target_on_e) <- names(target_on_a)

use_dat <- rbind.data.frame(target_on_a, target_on_e)

type <- c(rep(0, nrow(target_on_a)), rep(1, nrow(target_on_e)))

use_dat$type <- type

dtrain <- xgb.DMatrix(data = as.matrix(use_dat[use_dat$week <= 15,c("avg_dist", "dist_at_start", "dist_at_throw", "min_dist",
                                                   "max_dist", "dist_at_catch", "min_angle_5", "min_angle_10", 
                                                   "max_angle_5", "max_angle_10", "avg_angle_5", "avg_angle_10",
                                                   "type")]),
                      label = use_dat$isDefensivePI[use_dat$week <= 15])


set.seed(111111)
bst_1 <- xgboost(data = dtrain, # Set training data
                 
                 nrounds = 100, # Set number of rounds
                 
                 verbose = 1, # 1 - Prints out fit
                 print_every_n = 20, # Prints out result every 20th iteration
                 
                 objective = "binary:logistic", # Set objective
                 eval_metric = "auc",
                 eval_metric = "error") # Set evaluation metric to use

library(tidyverse)
# Calculate SHAP importance
shap_result <- shap.score.rank(xgb_model = bst_1, 
                               X_train = as.matrix(use_dat[,c("avg_dist", "dist_at_start", "dist_at_throw", "min_dist",
                                                              "max_dist", "dist_at_catch", "min_angle_5", "min_angle_10", 
                                                              "max_angle_5", "max_angle_10", "avg_angle_5", "avg_angle_10",
                                                              "type")]),
                               shap_approx = F)
# Plot SHAP importance
var_importance(shap_result, top_n=10)

shap_long = shap.prep(shap = shap_result,
                      X_train = as.matrix(use_dat[,c("avg_dist", "dist_at_start", "dist_at_throw", "min_dist",
                                                     "max_dist", "dist_at_catch", "min_angle_5", "min_angle_10", 
                                                     "max_angle_5", "max_angle_10", "avg_angle_5", "avg_angle_10",
                                                     "type")]), 
                      top_n = 10)


plot.shap.summary(data_long = shap_long)


########## Predict Play Success ##########

play_res <- rep(0, nrow(use_dat))
play_res[use_dat$passResult == "C"] <- 1
use_dat$play_res <- play_res

dtrain2 <- xgb.DMatrix(data = as.matrix(use_dat[use_dat$week <= 15,c("avg_dist", "dist_at_start", "dist_at_throw", "min_dist",
                                                                    "max_dist", "dist_at_catch", "min_angle_5", "min_angle_10", 
                                                                    "max_angle_5", "max_angle_10", "avg_angle_5", "avg_angle_10",
                                                                    "type", "rec_depth_catch", "rec_depth_final",       
                                                                    "rec_depth_catch_e", "rec_depth_final_e")]),
                      label = use_dat$play_res[use_dat$week <= 15])


set.seed(111111)
bst_2 <- xgboost(data = dtrain2, # Set training data
                 
                 nrounds = 100, # Set number of rounds
                 
                 verbose = 1, # 1 - Prints out fit
                 print_every_n = 20, # Prints out result every 20th iteration
                 
                 objective = "binary:logistic", # Set objective
                 eval_metric = "auc",
                 eval_metric = "error") # Set evaluation metric to use


# Calculate SHAP importance
shap_result2 <- shap.score.rank(xgb_model = bst_2, 
                               X_train = as.matrix(use_dat[use_dat$week <= 15,c("avg_dist", "dist_at_start", "dist_at_throw", "min_dist",
                                                                                "max_dist", "dist_at_catch", "min_angle_5", "min_angle_10", 
                                                                                "max_angle_5", "max_angle_10", "avg_angle_5", "avg_angle_10",
                                                                                "type", "rec_depth_catch", "rec_depth_final",       
                                                                                "rec_depth_catch_e", "rec_depth_final_e")]),
                               shap_approx = F)
# Plot SHAP importance
var_importance(shap_result2, top_n=10)

shap_long2 = shap.prep(shap = shap_result2,
                      X_train = as.matrix(use_dat[use_dat$week <= 15,c("avg_dist", "dist_at_start", "dist_at_throw", "min_dist",
                                                                                 "max_dist", "dist_at_catch", "min_angle_5", "min_angle_10", 
                                                                                 "max_angle_5", "max_angle_10", "avg_angle_5", "avg_angle_10",
                                                                                 "type", "rec_depth_catch", "rec_depth_final",       
                                                                                 "rec_depth_catch_e", "rec_depth_final_e")]), 
                      top_n = 10)


plot.shap.summary(data_long = shap_long2)


########## Section 5 - Predictions for players ##########

pi_data <- xgb.DMatrix(data = as.matrix(use_dat[,c("avg_dist", "dist_at_start", "dist_at_throw", "min_dist",
                                                                     "max_dist", "dist_at_catch", "min_angle_5", "min_angle_10", 
                                                                     "max_angle_5", "max_angle_10", "avg_angle_5", "avg_angle_10",
                                                                     "type")]))

pass_data <- xgb.DMatrix(data = as.matrix(use_dat[,c("avg_dist", "dist_at_start", "dist_at_throw", "min_dist",
                                                                                  "max_dist", "dist_at_catch", "min_angle_5", "min_angle_10", 
                                                                                  "max_angle_5", "max_angle_10", "avg_angle_5", "avg_angle_10",
                                                                                  "type", "rec_depth_catch", "rec_depth_final",       
                                                                                  "rec_depth_catch_e", "rec_depth_final_e")]))

pi_preds <- predict(bst_1, pi_data)
pass_preds <- predict(bst_2, pass_data)

plot_data <- cbind.data.frame(use_dat$isDefensivePI, as.factor(use_dat$play_res), pi_preds, pass_preds)
names(plot_data) <- c("defensive_pi", "pass_result", "pi_pred", "pass_pred")


library(ggplot2)
library(ggdark)
library(ggExtra)
g_1 <- ggplot(plot_data, aes(x = pass_pred, y = pi_pred, color = pass_result, shape = defensive_pi)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("1" = "Red", "0" = "blue"),
                     labels = c("1" = "Complete", "0" = "Incomplete")) +
  dark_theme_bw() +
  labs(x = "Pass Success Probability", y = "PI Probability",
       color = "Pass Result", shape = "Defensive Pass\n Interference",
       title = "Probability of Pass Success vs Pass Interference") 
g_1


########## Section 7 - Player Ratings ##########
  
players_id <- unique(use_dat[,c("play_defenders", "def_team")])
targets <- pi <- success <- pred_pi <- pred_success <- player_names <- rep(NA, nrow(players_id ))
for(i in 1:nrow(players_id )){
  targets[i] <- sum(use_dat$play_defenders == players_id$play_defenders[i] & use_dat$def_team == players_id$def_team[i])
  pi[i] <- sum(use_dat$isDefensivePI[use_dat$play_defenders == players_id$play_defenders[i] & use_dat$def_team == players_id$def_team[i]] == 1)
  success[i] <- sum(use_dat$play_res[use_dat$play_defenders == players_id$play_defenders[i] & use_dat$def_team == players_id$def_team[i]] == 1)
  pred_pi[i] <- sum(pi_preds[use_dat$play_defenders == players_id$play_defenders[i] & use_dat$def_team == players_id$def_team[i]])
  pred_success[i] <- sum(pass_preds[use_dat$play_defenders == players_id$play_defenders[i] & use_dat$def_team == players_id$def_team[i]])
  player_names[i] <- players$displayName[players$nflId == players_id$play_defenders[i]]
}

res_table <- cbind.data.frame(players_id, targets, pi, success, pred_pi, pred_success, player_names)
head(res_table[order(res_table$targets, decreasing = TRUE),])

summary(as.factor(res_table$def_team))

# Calculate deviation from success
res_table$dev_success <- res_table$success - res_table$pred_success
# Calculate deviation from PI
res_table$dev_pi <- res_table$pi - res_table$pred_pi

res_table$dev_s_pp <- res_table$dev_success/res_table$targets
res_table$dev_pi_pp <- res_table$dev_pi/res_table$targets

res_table_2 <- res_table[res_table$targets > 50, ]

res_table_3 <- res_table_2[(res_table_2$dev_s_pp < -0.05 &
                              res_table_2$dev_pi_pp < -0.005) | 
                             (res_table_2$dev_s_pp < -0.1 ) |
                             (res_table_2$dev_pi_pp > 0.02), ]


library(ggrepel)
g_2 <- ggplot(res_table_2, aes(x = dev_s_pp, y= dev_pi_pp, color = targets)) +
  geom_point() +
  dark_theme_bw() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  annotate("text", x = 0.1, y = 0.02, label = "More Completions\nMore PI", color = "blue") +
  annotate("text", x = 0.1, y = -0.01, label = "More Completions\nLess PI", color = "purple") +
  annotate("text", x = -0.1, y = 0.02, label = "Less Completions\nMore PI", color = "purple") +
  annotate("text", x = -0.1, y = -0.01, label = "Less Completions\nLess PI", color = "red") +
  xlim(-0.2, 0.2) +
  labs(x = "Deviation from Expected Pass Success",
       y = "Deviation from Expected Pass Interference",
       color = "Targets",
       title = "Deviation from Expected Values of Pass Success and\nPass Interference Per Target") +
  geom_text_repel(data = res_table_3, aes(x = dev_s_pp, y= dev_pi_pp, label = player_names))
g_2


less_pi_less_success <- res_table[res_table$dev_s_pp < -.05 &
                                    res_table$dev_pi_pp < 0,]

top_dawgs <- less_pi_less_success[less_pi_less_success$targets > 75,]

summary(as.factor(top_dawgs$def_team))

summary(as.factor(less_pi_less_success$def_team))









































