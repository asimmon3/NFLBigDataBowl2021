######################################################
########## NFL Big Data Bowl Function File ###########
######################################################
#
#
#
#
#

fix_coords <- function(pbf, plays, games){
  #'
  #' This function fixes the coordinates so that home and away plays
  #' are on the same scale
  #'
  #' @param pbf The frame by frame data
  #' @param plays The play level data
  #' @param games The game level data
  #'
  #' @return A data frame with the corrected coordinates
  #'
  # Create new x and y
  new_x <- pbf$x 
  new_y <- pbf$y
  
  # Create home and away indicator
  plays$home_away <- rep(NA, nrow(plays))
  # Create indicator for frame by frame
  frame_ind <- rep(NA, nrow(pbf))
  
  # Loop through and correct data
  # For each play
  for(i in 1:nrow(plays)){
    # Extract game data
    game_dat <- games[games$gameId == plays$gameId[i],]
    # If possession is home team
    if(plays$possessionTeam[i] == game_dat$homeTeamAbbr){
      # Assign home to play data frame
      plays$home_away[i] <- "home"
      # Assign home to relevant frames
      frame_ind[pbf$playId == plays$playId[i] &
                  pbf$gameId == plays$gameId[i]] <- "home"
      # If possession is away team
    } else {
      # Assign away to play data frame
      plays$home_away[i] <- "away"
      # Assign away to relevant data frame
      frame_ind[pbf$playId == plays$playId[i] &
                  pbf$gameId == plays$gameId[i]] <- "away"
    }
  }
  
  # Calculate new x and y
  new_x[frame_ind == "away"] <- 120 - pbf$x[frame_ind == "away"]
  new_y[frame_ind == "away"] <- 53.3 - pbf$y[frame_ind == "away"]
  # Assign new x and y
  pbf$x <- new_x
  pbf$y <- new_y
  # Return corrected data
  return(pbf)
}

calculate_frame_distance <- function(play_db, frames){
  #'
  #' This function calculates the distance between each of the players
  #' and the ball at each frame
  #' 
  #' @param play_db The frame by frame data for a single play
  #' @param frames The unique frames for the play
  #'
  #'
  #' @return A list which contains a distance matrix for each frame
  #' 
  #' 
  
  # Create list to store distances
  frame_dist <- vector(mode = "list", length = length(frames))
  
  # For each frame
  for(j in 1:length(frames)){
    ### Calculate distances ###
    # Extract info for single frame
    t_1 <- play_db[play_db$frameId == j,]
    # Calculate distance matrix
    dist_mat <- as.matrix(dist(t_1[, c("x", "y") ]))
    # Assign names to distance matrix
    colnames(dist_mat) <- rownames(dist_mat) <- t_1$nflId
    # Store distance matrix
    frame_dist[[j]] <- dist_mat
  }
  
  # Return distances at each frame
  return(frame_dist)
}

calculate_release_point <- function(players, frames, frame_dist){
  #'
  #' This function uses the distance from the QB to the ball
  #' at each frame to identify a pseudo-release point. This is estimated as
  #' the point where the QB is closest to the ball. 
  #' 
  #' @param players The data frame with the information for the players
  #'  involved in the play
  #' @param frames The unique frames in the play
  #' @param frame_dist The distance between each player and the ball at each frame. 
  #' Calculated using the 'calculate_frame_dist' function
  #'  
  #' @return the estimated release point 
  #' 
  #'  
  
  # Identify qb
  qb <- players$nflId[players$position == "QB"][1]
  # Create vector to store release point
  qb_to_ball_out <- rep(NA,length(frames))
  # For each frame
  for(j in 1:length(frames)){
    # Check QB is in the frame
    if(qb %in% rownames(frame_dist[[j]]) &
       "Football" %in% colnames(frame_dist[[j]])){
      # Extract distance from QB to ball
      qb_to_ball_out[j] <- frame_dist[[j]][rownames(frame_dist[[j]]) == qb,
                                           colnames(frame_dist[[j]]) =="Football"]
    }
    
  }
  # Identify closest frame
  release_point <- which.min(qb_to_ball_out)
  # Return results
  return(release_point)
}


summary_stat_calc_1 <- function(frames, play_recivers,
                                play_defenders, frame_dist,
                                play_db){
  #'
  #' This function calculates the closest defender distance, 
  #' reciver depth, distance from reciver to the ball, and 
  #' the difference in angle the reciever is facing to the ball
  #'
  #' @param frames The unique frames for the play
  #' @param play_recivers The recivers on the play
  #' @param play_defenders The defenders on the play
  #' @param frame_dist The list with distance between each of the players
  #' for each frame
  #' @param play_db The data frame with the frame by frame data
  #'
  #'
  #'
  
  # Create data frames to store clearance, depth and ball distance
  rec_clearance <- rec_depth <- ball_distance <- ball_angle_diff <- 
    as.data.frame(matrix(NA, nrow = length(frames),ncol=length(play_recivers)))
  # Name columns
  names(rec_clearance) <- names(rec_depth) <- names(ball_distance) <-  names(ball_angle_diff) <- 
    play_recivers
  # For each frame
  for(x in 1:nrow(rec_clearance)){
    # For each player
    for(j in 1:ncol(rec_clearance)){
      # Check reciver and football in each frame
      if(names(rec_clearance)[j] %in% rownames(frame_dist[[x]]) &
         "Football" %in% colnames(frame_dist[[x]])){
        # Calculate closest defender distance
        rec_clearance[x,j] <- 
          min(frame_dist[[x]][rownames(frame_dist[[x]]) == names(rec_clearance)[j],
                              colnames(frame_dist[[x]]) %in% c(play_defenders)])
        # Calculate depth
        rec_depth[x,j] <- play_db$x[play_db$nflId == names(rec_depth)[j] &
                                      play_db$frameId == x &
                                      play_db$off_def == "Offense"]
        # Calculate distance to ball
        ball_distance[x,j] <- 
          frame_dist[[x]][rownames(frame_dist[[x]]) == names(rec_clearance)[j],
                          colnames(frame_dist[[x]]) == "Football"]
        
        ## Calculate angle to ball
        # Calculate y distance frome player to ball
        a <- play_db$y[play_db$nflId == names(rec_depth)[j] &
                         play_db$frameId == x] - play_db$y[play_db$nflId == "Football" &
                                                             play_db$frameId == x] 
        # Calculate angle to ball
        angle <- asin(a/ball_distance[x,j]) * (180/pi)
        # Calculate angle facing away from ball
        ball_angle_diff[x, j] <- min(abs(play_db$o[play_db$nflId == names(rec_depth)[j] &
                                                     play_db$frameId == x] - angle),
                                     abs(play_db$o[play_db$nflId == names(rec_depth)[j] &
                                                     play_db$frameId == x] + angle))
        
      }
    }
  }
  
  # Return results
  return(list(rec_clearance, rec_depth, ball_distance, ball_angle_diff))
}

summary_stat_calc_2 <- function(play_defenders, play_recivers, frame_dist, 
                                release_point, rec_point, play_db, frames,
                                target_reciever){
  #'
  #' This function calculates summary statistics for the defenders and the
  #' reciever they are closest to at the release point
  #'
  #' @param play_defenders The defenders on the play
  #' @param play_recievers The recievers on the play
  #' @param frame_dist The distance between each player and ball at each frame
  #' @param release_point The estimated release point for the ball
  #' @param rec_point The point of the reception
  #' @param play_db The frame by frame data for the play
  #' @param frames The unique frames for the play
  #' @param target_reciever The target reciever on the play
  #' 
  #' @return This function returns a data frame with the following columns:
  #' 
  #' play_defenders - The defender on the play
  #' def_a - Defensive assignment for the defender based on distance at release
  #' avg_dist - Average distance from defender to assignment
  #' dist_at_start - Distance to assignment at start of play
  #' dist_at_throw - Distance to assignment at release
  #' min_dist - Minimum distance to defensive assignment
  #' max_dist - Maximum distance to defensive assignment
  #' dist_at_catch - Distance to assignment at catch
  #' min_angle_5 - Minimum angle to ball in five frames prior to catch
  #' min_angle_10 - Minimum angle to ball in ten frames prior to catch
  #' max_angle_5 - Maximum angle to ball in five frames prior to catch
  #' max_angle_10 - Maximum angle to ball in ten frames prior to catch
  #' avg_angle_5 - Average angle to ball in five frames prior to catch
  #' avg_angle_10 - Average angle to ball in ten frames prior to catch
  #' target_reciever - Binary indicator of if defensive assignment is target reciever
  
  # Find defensive assignment
  def_a <- rep(NA, length(play_defenders))
  # Create vectors to store results
  avg_dist <- dist_at_start <- dist_at_throw <- min_dist <-
    max_dist <- dist_at_catch <- min_angle_5 <- min_angle_10 <-
    max_angle_5 <- max_angle_10 <- avg_angle_5 <- avg_angle_10 <- rep(NA, length(play_defenders))
  # Create matrix to store ball angle at each frame for each player
  def_ball_angle <-  as.data.frame(matrix(NA, nrow = length(frames),ncol=length(play_defenders)))
  # Name columns of ball angle
  names(def_ball_angle) <- play_defenders
  # For each defensive player
  for(j in 1:length(play_defenders)){
    if(play_defenders[j] %in% colnames(frame_dist[[release_point]])){
      # Identify closest reciever at release point
      def_a[j] <- play_recivers[which.min(frame_dist[[release_point]][play_defenders[j],
                                                                      play_recivers[play_recivers %in%
                                                                                      colnames(frame_dist[[release_point]])]])]
      # Create vector to store distances
      dist_vec <- rep(NA, length(frame_dist))
      # For each frame
      for(x in 1:length(frame_dist)){
        # Check reciver and assignment are in frame
        if(play_defenders[j] %in% rownames(frame_dist[[x]]) &
           def_a[j] %in% rownames(frame_dist[[x]])){
          # Calculate distance to defensive assignment at each frame
          dist_vec[x] <- frame_dist[[x]][play_defenders[j], def_a[j]]
          
          if("Football" %in% colnames(frame_dist[[x]])){
            # Calculate distance to ball
            def_ball_distance <- 
              frame_dist[[x]][rownames(frame_dist[[x]]) == play_defenders[j],
                              colnames(frame_dist[[x]]) == "Football"]
            
            ## Calculate angle to ball
            # Extract y distance for players to ball
            a <- play_db$y[play_db$nflId == play_defenders[j] &
                             play_db$frameId == x] - play_db$y[play_db$nflId == "Football" &
                                                                 play_db$frameId == x] 
            # Calculate angle to ball
            angle <- asin(a/def_ball_distance) * (180/pi)
            # Calculate difference in direction and angle to ball
            def_ball_angle[x, j] <- min(abs(play_db$o[play_db$nflId == play_defenders[j] &
                                                        play_db$frameId == x] - angle), 
                                        abs(play_db$o[play_db$nflId ==play_defenders[j] &
                                                        play_db$frameId == x] + angle))
          }
        } 
      }
      # Calculate average distance to closest reciever at release
      avg_dist[j] <- mean(dist_vec, na.rm = T)
      # Calculate distance to reciver at play start
      dist_at_start[j] <- dist_vec[1]
      # Calculate distance to reciver at release point
      dist_at_throw[j] <- dist_vec[release_point]
      # Calculate minimum distance to reciever
      min_dist[j] <- min(dist_vec, na.rm = T)
      # Calculate maximum distance to reciever
      max_dist[j] <- max(dist_vec, na.rm = T)
      # Calculate distance at catch point
      dist_at_catch[j] <- dist_vec[rec_point]
      
      # Calculate minimum angle within 5 frames
      min_angle_5[j] <- min(def_ball_angle[max(1,(rec_point - 5)):rec_point,j], na.rm = T)
      # Calculate minimum angle within 10 frames
      min_angle_10[j] <- min(def_ball_angle[max(1,(rec_point - 10)):rec_point,j], na.rm = T)
      
      # Calculate maximum angle within 5 frames
      max_angle_5[j] <- max(def_ball_angle[max(1,(rec_point - 5)):rec_point,j], na.rm = T)
      # Calculate maximum angle within 10 frames
      max_angle_10[j] <- max(def_ball_angle[max(1,(rec_point - 10)):rec_point,j], na.rm = T)       
      
      # Calculate average angle within 5 frames
      avg_angle_5[j] <- mean(def_ball_angle[max(1,(rec_point - 5)):rec_point,j], na.rm = T)
      # Calculate average angle within 10 frames
      avg_angle_10[j] <- mean(def_ball_angle[max(1,(rec_point - 10)):rec_point,j], na.rm = T)    
    }
  }
  
  # Join results together
  res <- cbind.data.frame(play_defenders, def_a,  avg_dist, dist_at_start,
                          dist_at_throw, min_dist, max_dist, dist_at_catch,
                          min_angle_5, min_angle_10, max_angle_5, max_angle_10, 
                          avg_angle_5, avg_angle_10)
  # Create binary indicator of target reciever
  target_rec <- rep(0, nrow(res))
  # Assign 1 if reciever is target
  target_rec[def_a == target_reciever] <- 1
  # Join vector to result data frame
  res$target_rec <- target_rec
  
  # Return results
  return(res)
}


summary_stat_calc_3 <- function(play_defenders, play_recivers, frame_dist, 
                                release_point, rec_point, play_db, frames, 
                                target_reciever){
  #'
  #' This function calculates summary statistics for the defenders and the
  #' reciever they are closest to at the release point
  #'
  #' @param play_defenders The defenders on the play
  #' @param play_recievers The recievers on the play
  #' @param frame_dist The distance between each player and ball at each frame
  #' @param release_point The estimated release point for the ball
  #' @param rec_point The point of the reception
  #' @param play_db The frame by frame data for the play
  #' @param frames The unique frames for the play
  #' @param target_reciever The target reciever on the play
  #' 
  #' @return This function returns a data frame with the following columns:
  #' 
  #' play_defenders - The defender on the play
  #' def_a - Defensive assignment for the defender based on distance at release
  #' avg_dist - Average distance from defender to assignment
  #' dist_at_start - Distance to assignment at start of play
  #' dist_at_throw - Distance to assignment at release
  #' min_dist - Minimum distance to defensive assignment
  #' max_dist - Maximum distance to defensive assignment
  #' dist_at_catch - Distance to assignment at catch
  #' min_angle_5 - Minimum angle to ball in five frames prior to catch
  #' min_angle_10 - Minimum angle to ball in ten frames prior to catch
  #' max_angle_5 - Maximum angle to ball in five frames prior to catch
  #' max_angle_10 - Maximum angle to ball in ten frames prior to catch
  #' avg_angle_5 - Average angle to ball in five frames prior to catch
  #' avg_angle_10 - Average angle to ball in ten frames prior to catch
  #' target_reciever - Binary indicator of if defensive assignment is target reciever
  
  # Find defensive assignment
  def_a <- rep(NA, length(play_defenders))
  # Create vectors to store results
  avg_dist <- dist_at_start <- dist_at_throw <- min_dist <-
    max_dist <- dist_at_catch <- min_angle_5 <- min_angle_10 <-
    max_angle_5 <- max_angle_10 <- avg_angle_5 <- avg_angle_10 <- rep(NA, length(play_defenders))
  # Create matrix to store ball angle at each frame for each player
  def_ball_angle <-  as.data.frame(matrix(NA, nrow = length(frames),ncol=length(play_defenders)))
  # Name columns of ball angle
  names(def_ball_angle) <- play_defenders
  # For each defensive player
  for(j in 1:length(play_defenders)){
    if(play_defenders[j] %in% colnames(frame_dist[[rec_point]])){
      # Identify closest reciever at reception point
      def_a[j] <- play_recivers[which.min(frame_dist[[rec_point]][play_defenders[j],
                                                                  play_recivers[play_recivers %in% 
                                                                                  colnames(frame_dist[[rec_point]])]])]
      # Create vector to store distances
      dist_vec <- rep(NA, length(frame_dist))
      # For each frame
      for(x in 1:length(frame_dist)){
        # Check reciver and assignment are in frame
        if(play_defenders[j] %in% rownames(frame_dist[[x]]) &
           def_a[j] %in% rownames(frame_dist[[x]])){
          # Calculate distance to defensive assignment at each frame
          dist_vec[x] <- frame_dist[[x]][play_defenders[j], def_a[j]]
          
          if("Football" %in% colnames(frame_dist[[x]])){
            # Calculate distance to ball
            def_ball_distance <- 
              frame_dist[[x]][rownames(frame_dist[[x]]) == play_defenders[j],
                              colnames(frame_dist[[x]]) == "Football"]
            
            ## Calculate angle to ball
            # Extract y distance for players to ball
            a <- play_db$y[play_db$nflId == play_defenders[j] &
                             play_db$frameId == x] - play_db$y[play_db$nflId == "Football" &
                                                                 play_db$frameId == x] 
            # Calculate angle to ball
            angle <- asin(a/def_ball_distance) * (180/pi)
            # Calculate difference in direction and angle to ball
            def_ball_angle[x, j] <- min(abs(play_db$o[play_db$nflId == play_defenders[j] &
                                                        play_db$frameId == x] - angle), 
                                        abs(play_db$o[play_db$nflId ==play_defenders[j] &
                                                        play_db$frameId == x] + angle))
          }
        } 
      }
      # Calculate average distance to closest reciever at release
      avg_dist[j] <- mean(dist_vec, na.rm = T)
      # Calculate distance to reciver at play start
      dist_at_start[j] <- dist_vec[1]
      # Calculate distance to reciver at release point
      dist_at_throw[j] <- dist_vec[release_point]
      # Calculate minimum distance to reciever
      min_dist[j] <- min(dist_vec, na.rm = T)
      # Calculate maximum distance to reciever
      max_dist[j] <- max(dist_vec, na.rm = T)
      # Calculate distance at catch point
      dist_at_catch[j] <- dist_vec[rec_point]
      
      # Calculate minimum angle within 5 frames
      min_angle_5[j] <- min(def_ball_angle[max(1,(rec_point - 5)):rec_point,j], na.rm = T)
      # Calculate minimum angle within 10 frames
      min_angle_10[j] <- min(def_ball_angle[max(1,(rec_point - 10)):rec_point,j], na.rm = T)
      
      # Calculate maximum angle within 5 frames
      max_angle_5[j] <- max(def_ball_angle[max(1,(rec_point - 5)):rec_point,j], na.rm = T)
      # Calculate maximum angle within 10 frames
      max_angle_10[j] <- max(def_ball_angle[max(1,(rec_point - 10)):rec_point,j], na.rm = T)       
      
      # Calculate average angle within 5 frames
      avg_angle_5[j] <- mean(def_ball_angle[max(1,(rec_point - 5)):rec_point,j], na.rm = T)
      # Calculate average angle within 10 frames
      avg_angle_10[j] <- mean(def_ball_angle[max(1,(rec_point - 10)):rec_point,j], na.rm = T)  
    }
  }
  
  # Join results together
  res <- cbind.data.frame(play_defenders, def_a,  avg_dist, dist_at_start,
                          dist_at_throw, min_dist, max_dist, dist_at_catch,
                          min_angle_5, min_angle_10, max_angle_5, max_angle_10, 
                          avg_angle_5, avg_angle_10)
  # Create binary indicator of target reciever
  target_rec <- rep(0, nrow(res))
  # Assign 1 if reciever is target
  target_rec[def_a == target_reciever] <- 1
  # Join vector to result data frame
  res$target_rec <- target_rec
  
  # Change names to separate from assignment
  names(res)[2:ncol(res)] <- paste(names(res)[2:ncol(res)], "_e", sep = "")
  
  # Return results
  return(res)
}


depth_calc <- function(res_final, rec_point, rec_depth){
  #'
  #' This function calculates the reciever depth for each defensive 
  #' assignment at catch and end of play
  #' 
  #' @param res_final The previously calculated summary statistics
  #' @param rec_point The point of the reception
  #' @param rec_depth A data frame with reciever depth for each play
  #'
  #' @return A data frame with the defensive assignment depth at catch and end
  #' of play
  #' 
  
  # Create vectors to store results
  rec_depth_catch <- rec_depth_final <- rec_depth_catch_e <- rec_depth_final_e <- rep(NA, nrow(res_final))
  # For each row of results
  for(j in 1:nrow(res_final)){
    if(!is.na(res_final$def_a[j])){
      # Calculate reciever depth at catch for closest reciver at release
      rec_depth_catch[j] <- rec_depth[rec_point,names(rec_depth) == res_final$def_a[j]]
      # Calculate reciever final depth for closest reciver at release
      rec_depth_final[j] <- rec_depth[nrow(rec_depth),names(rec_depth) == res_final$def_a[j]]
    }
    
    if(!is.na(res_final$def_a_e[j])){
      # Calculate reciever depth at catch for closest reciver at catch
      rec_depth_catch_e[j] <- rec_depth[rec_point,names(rec_depth) == res_final$def_a_e[j]]
      # Calculate reciever final depth for closest reciver at catch
      rec_depth_final_e[j] <- rec_depth[nrow(rec_depth),names(rec_depth) == res_final$def_a_e[j]]
    }
    
  }
  # Join results together
  rec_res <- cbind.data.frame(rec_depth_catch, rec_depth_final, 
                              rec_depth_catch_e, rec_depth_final_e)
  # Return results
  return(rec_res)
}


stat_calc <- function(pbf, plays){
  #'
  #' This function calculates summary statistics for each defender for each
  #' play
  #' 
  #' @param pbf The frame-by-frame data
  #' @param plays The play level data frame
  #'
  #' @return A data frame with calculated summary statistics for each defender
  #' on each play
  
  # Keep only unique rows
  pbf <- unique(pbf)
  # Convert IDs to characters
  pbf$nflId <- as.character(pbf$nflId)
  pbf$nflId[is.na(pbf$nflId)] <- "Football"
  # Extract plays in play by play dataset
  pbf_plays <- unique(pbf[, c("gameId", "playId")])
  
  # Create list to store play stats
  play_res <- vector(mode = "list", length = nrow(pbf_plays))
  
  # For each play
  for(i in 1:nrow(pbf_plays)){
    print(i)
    # Extract frames for the given play
    play_db <- pbf[pbf$gameId == pbf_plays$gameId[i] &
                     pbf$playId == pbf_plays$playId[i],]
    
    # Extract players involved in play
    players <- unique(play_db[, c("displayName", "nflId", "position")])
    
    # Extract recievers
    play_recivers <- players$nflId[players$position %in% 
                                           c("WR", "TE", "RB", "FB", "HB")]
    # Extract defenders
    play_defenders <- players$nflId[players$position %in% 
                                            c("CB", "DB", "FS", "SS","S")]
    if(length(play_defenders) > 0 &
       length(play_recivers) > 0){
      # Extract frames
      frames <- unique(play_db$frameId)
      # Calculate distance between each player and ball at each frame
      frame_dist <- calculate_frame_distance(play_db, frames)
      
      ## Calculate release point
      release_point <- calculate_release_point(players, frames, frame_dist)
      if(length(release_point) > 0){
        ## Calculate summary stats 1
        sum_stats <- summary_stat_calc_1(frames, play_recivers,play_defenders,
                                         frame_dist,play_db)
        # Extract summary stats results
        rec_clearance <- sum_stats[[1]]
        rec_depth <- sum_stats[[2]]
        ball_distance <- sum_stats[[3]]
        ball_angle_diff <- sum_stats[[4]]
        
        # Drop empty rows
        #ball_distance <- na.omit(ball_distance)
        
        # Identify target reciver as reciever who gets closest to ball
        target_reciever <- play_recivers[which.min(apply(ball_distance,2, min, na.rm = T))]
        
        #' Identify reception point as position where target reciever gets within 1 
        #' yard of ball
        rec_point <- which(ball_distance[,target_reciever] < 1)[1]
        # If no reception point within a foot then choose closest
        if(is.na(rec_point)){
          rec_point <- which.min(ball_distance[,target_reciever]) 
        }
        
        # 2 Calculate defensive summary statistics 
        # - Reciever who defender is closest to at release point is assumed defensive assignment
        res <- summary_stat_calc_2(play_defenders, play_recivers, frame_dist, 
                                   release_point, rec_point, play_db, frames,
                                   target_reciever)
        
        ## Calculating stats for closest reciver at reception
        res_2 <- summary_stat_calc_3(play_defenders, play_recivers, frame_dist, 
                                     release_point, rec_point, play_db, frames,
                                     target_reciever)
        
        # Join results for closest reciver at catch and release
        res_all <-cbind.data.frame(res, res_2)
        
        ## 3 - Extract per play stats
        # Extract statistics for play
        play_stats <- plays[plays$gameId == pbf_plays$gameId[i] &
                              plays$playId == pbf_plays$playId[i],]
        # Create exmpty data frame to store replicated rows
        res_play <- as.data.frame(matrix(NA, nrow = nrow(res_all), ncol = ncol(play_stats)))
        # Assign result statistics
        res_play[,] <- play_stats
        # Add names to play statistics
        names(res_play) <- names(play_stats)
        # Join results
        res_final <- cbind.data.frame(res_all, res_play)
        
        ## Calculate reciver depth at catch and end of play
        rec_res <- depth_calc(res_final, rec_point, rec_depth)
        
        # Join results together
        res_final_2 <- cbind.data.frame(res_final, rec_res)
        # Store results for play
        play_res[[i]] <- res_final_2
      }
    }
  }
  # Return calculated results
  return(play_res)
}


# Note: The functions shap.score.rank, shap_long_hd and plot.shap.summary were 
# originally published at https://liuyanguu.github.io/post/2018/10/14/shap-visualization-for-xgboost/
# All the credits to the author.


## functions for plot
# return matrix of shap score and mean ranked score list
shap.score.rank <- function(xgb_model = xgb_mod, shap_approx = TRUE, 
                            X_train = mydata$train_mm){
  require(xgboost)
  require(data.table)
  shap_contrib <- predict(xgb_model, X_train,
                          predcontrib = TRUE, approxcontrib = shap_approx)
  shap_contrib <- as.data.table(shap_contrib)
  shap_contrib[,BIAS:=NULL]
  cat('make SHAP score by decreasing order\n\n')
  mean_shap_score <- colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)), decreasing = T)]
  return(list(shap_score = shap_contrib,
              mean_shap_score = (mean_shap_score)))
}


# a function to standardize feature values into same range
std1 <- function(x){
  return ((x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T)))
}


# prep shap data
shap.prep <- function(shap  = shap_result, X_train = mydata$train_mm, top_n){
  require(ggforce)
  # descending order
  if (missing(top_n)) top_n <- dim(X_train)[2] # by default, use all features
  if (!top_n%in%c(1:dim(X_train)[2])) stop('supply correct top_n')
  require(data.table)
  shap_score_sub <- as.data.table(shap$shap_score)
  shap_score_sub <- shap_score_sub[, names(shap$mean_shap_score)[1:top_n], with = F]
  shap_score_long <- melt.data.table(shap_score_sub, measure.vars = colnames(shap_score_sub))
  
  # feature values: the values in the original dataset
  fv_sub <- as.data.table(X_train)[, names(shap$mean_shap_score)[1:top_n], with = F]
  # standardize feature values
  fv_sub_long <- melt.data.table(fv_sub, measure.vars = colnames(fv_sub))
  fv_sub_long[, stdfvalue := std1(value), by = "variable"]
  # SHAP value: value
  # raw feature value: rfvalue; 
  # standarized: stdfvalue
  names(fv_sub_long) <- c("variable", "rfvalue", "stdfvalue" )
  shap_long2 <- cbind(shap_score_long, fv_sub_long[,c('rfvalue','stdfvalue')])
  shap_long2[, mean_value := mean(abs(value)), by = variable]
  setkey(shap_long2, variable)
  return(shap_long2) 
}


plot.shap.summary <- function(data_long){
  x_bound <- max(abs(data_long$value))
  require('ggforce') # for `geom_sina`
  plot1 <- ggplot(data = data_long)+
    coord_flip() + 
    # sina plot: 
    geom_sina(aes(x = variable, y = value, color = stdfvalue)) +
    # print the mean absolute value: 
    geom_text(data = unique(data_long[, c("variable", "mean_value"), with = F]),
              aes(x = variable, y=-Inf, label = sprintf("%.3f", mean_value)),
              size = 3, alpha = 0.7,
              hjust = -0.2, 
              fontface = "bold") + # bold
    # # add a "SHAP" bar notation
    # annotate("text", x = -Inf, y = -Inf, vjust = -0.2, hjust = 0, size = 3,
    #          label = expression(group("|", bar(SHAP), "|"))) + 
    scale_color_gradient(low="#FFCC33", high="#6600CC", 
                         breaks=c(0,1), labels=c("Low","High")) +
    theme_bw() + 
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), # remove axis line
          legend.position="bottom") + 
    geom_hline(yintercept = 0) + # the vertical line
    scale_y_continuous(limits = c(-x_bound, x_bound)) +
    # reverse the order of features
    scale_x_discrete(limits = rev(levels(data_long$variable)) 
    ) + 
    labs(y = "SHAP value (impact on model output)", x = "", color = "Feature value") 
  return(plot1)
}


var_importance <- function(shap_result, top_n=10)
{
  var_importance=tibble(var=names(shap_result$mean_shap_score), importance=shap_result$mean_shap_score)
  
  var_importance=var_importance[1:top_n,]
  
  ggplot(var_importance, aes(x=reorder(var,importance), y=importance)) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    theme_light() + 
    theme(axis.title.y=element_blank()) 
}














































