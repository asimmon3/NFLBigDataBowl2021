# NFLBigDataBowl2021
The goal of the competition was to analyze pass defense in the NFL


Predicting Pass Interference & Defensive Grades

Defensive coverage in the NFL is often a trade-off between the quality of the coverage and the risk of conceding a penalty for pass interference. For this analysis we develop several summary statistics to characterize the coverage of a defender on a target reciever. Using these summary statistics we develop two models one to predict pass interference being called and a second to determine the liklihood of play success. Using these two models we can rate a player based on their deviation from expected number of succesful passes against them and the expected number of pass interference calls against them. Defenders who give up less plays than expected and reciever less pass interference calls than expected given the coverage are more likely to be successful.

Preliminaries

First we need to define several functions and pre-process the data to calculate summary statistics for the model.

Process Data

For the next step we are ready to process the data. Here take the following steps:

Add each players position to the frame by frame data.
Convert the coordinates for the away team to be on the same orientation as the home team.
Extract unique rows from the frame by frame data.
Calculate the distance between each player and ball for each frame.
Calculate summary statistics for recievers.
Calculate summary statistics for defenders.
For the analysis we had to make several assumptions using the data, these are:

Identifying QB Release point - In order to identify the frame where the ball is released we use the frame where the ball is closest to the QB on the play. Initial analysis revealed that this is generally a reasonable assumption and should give us a close approximation of ball release.
Identifying Target Reciever and reception point - We identify the target reciever as the offensive player (excluding the QB) who is closest to the ball on the play. We also define the reception point as the frame where this player is closest to the ball. We calcuulate this reception point regardless of whether the play was complete or incomplete for analysis purposes.
Defensive Assignment - In order to account for differing coverage types and multiple players covering each reciever we created two separte classifications of defensive assignment. These are the reciever who the defener is closest to at the release of the ball by the QB and the reciever who the defender is closest to at the recption point of the ball.
For each of the two types of defensive assignment we define a series of summary statistics for the coverage. The angle metrics relate to deviation from the direction the reciever is facing to the location of the ball:

Average distance from the defender to their defensive assignment (avg_dist)
Distance to defensive assignment at the start of the play (dist_at_start)
Distance to defenive assignment at the release point (dist_at_throw)
Minimum distance to defensive assignment (min_dist)
Maximum distance to defensive assignment (max_dist)
Distance to defensive assignment at catch/reception point (dist_at_catch)
Minimum angle from the direction the defender is facing to the location of the ball within five frames of the catch (min_angle_5)
Minimum angle from the direction the defender is facing to the location of the ball within ten frames of the catch (min_angle_10)
Maximum angle from the direction the defender is facing to the location of the ball within five frames of the catch (max_angle_5)
Maximum angle from the direction the defender is facing to the location of the ball within ten frames of the catch (max_angle_10)
Average angle from the direction the defender is facing to the location of the ball within five frames of the catch (avg_angle_5)
Average angle from the direction the defender is facing to the location of the ball within ten frames of the catch (avg_angle_10)

Once we have calculated the statistics for each player for each week we need to join the results together into a single data frame.

We next want to add add the team of each defender to the result data frame.

Modelling

The next step in our analysis is to fit two models to the data we have created. For this we only consider data where the defender is marking the target reciever on the play at either release or reception point. The two XGBoost models are purposely left untuned and underfit for analysis purposes. For deployment several seasons of data would be used to develop more finely tuned models which could then be used to predict the current seasons data.

The two models developed are:

PI Model - This model uses the calculated statistics to model pass interference being called.
Pass Completion Model - This model uses the calculated statistics to model the pass being successful
We first fit these models and then extract significant variables using SHAP importance. We then apply these models to the data to develop an expected value for pass interference and pass completion.

For the pass interference model the most important variables are displayed in the SHAP plots created.

We next fit the model to predict if a pass is complete or incomplete.

From the two models we see considerable overlap in the important variables. In particular we see the following variables appearing in the top 10 variables for both models:

dist_at_catch - The distance at the catch point is the most important variable in the pass interference model and the second most important variable in the pass success model.
min_dist - The minimum distance between the defender and reciever is the second most important variable in the pass interference model and most important variable in the pass success model.
avg_dist - The average distance is the third most important variable in the pass interference model and fourth most important variable in the pass success model.
dist_at_start - The average distance is the fourth most important variable in the pass interference model and fifth most important variable in the pass success model.
dist_at_throw - The distance at throw is the fifth most important variable in the pass interference model and the third most important variable in the pass success model.
min_angle_5 - The minimum angle within five frames of the catch is the sixth most important in the pass interference model and sixth most important variable in the pass success model
Given the significant overlap between the important variable for both models it appears that there is a trade-off between commiting pass interference and succesfully defending a pass play. A top quality reciever is one who can both provide quality coverage and minimise pass interference calls.

To quantify player quality we apply the models to the data to develop an expected value for pass interference and pass success for each play

Visualizing the expected values it can be seen that as the expected vaue of pass interference increases the expected value of pass success decreases

Player Analysis

For each defender we then sum up the following statistics:

Targets - Number of targets against the defender.
PI - Number of pass interference calls against the defnder.
Success - Number of successful pass plays against the defender.
Expected PI - Total expected PI value for the defender across the targets.
Expected Success - Total expected succesful pass plays against defender across the targets.
We then calculate two metrics measuring the deviation from expected pass interference and pass success for each defender and average these values per target.

A positive deviation for pass success per target from expected success (dev_s_pt) means that when targeting that player passes were completed at a higher rate than would be expected given the coverage they are carrying out and a negative deviation indicates that passes were completed at a lower rate than would be expected.

A positive deviation for PI per target from expected PI (dev_pi_pt) means that the player was called for pass interference at a higher rate than was expected and a negative deviation means they are called for pass interference at a lower rate than would be expected.

A top quality defender would be those who have negative values for both metrics.

To analyse the indiviudal players we only consider those players who faced fifty or more targets during the season. From this plot we can see that Darius Slay was highly effective at both defending passes and avoiding pass interference calls. Joe Haden was effective at defending passes but gave up more pass interference calls than expected. Prince Amukamara was both less effective at defending passes and avoid pass interference calls.

When we looked at the top_dawgs table, which included defenders whose dev_s_pt was less than -0.5, dev_pi_pt was less than 0, and targets were greater than 75, only 12 defenders made the cut. Of those 12, the only team with more than 1 who made the list were the New England Patriots, potential further proof of their defensive dominance during the 2018 season. To look at this at a deeper level, we looked at the parent table for top_dawgs, less_pi_less_success. Again, the New England Patriots had the highest representation, with 4 defenders making the list. It is important to note that one of the important filters added to the less_pi_less_success table to get to top_dawgs was the number of targets, where many defenders in less_pi_less_success had 15 or less targets on the season. The table [top_dawgs] was in line with at least some of our preconceived notions, and included top-level defensive backs like Desmond King, Jason McCourty, Pierre Desir and Darius Slay. These defenders performed, on average, significantly better than the model had predicted them to.

Breaking top_dawgs down by category, the top performer in dev_success was Pierre Desir, the top performer in dev_pi was Darius Slay, the top performer in dev_s_pt was Taron Johnson, the top performer in dev_pi_pt was Darius Slay. These categories can be used to help grade players, week by week, track player improvement over time, as well as isolate potential in players that can be harder to track with the naked eye. For example, Taron Johnson stood out as one of the best defenders in our analysis. Yet, until this season, he has not been seen as a 'top-level' defensive back in the NFL. With this type of modeling technique, expectations can be more clearly defined, and players who are performing better than expectations can be easily isolated.

Some next steps:

A potential next step would be to include physical attributes of each player as features in the predictive model and see if and how the results differ with the knowledge of player heights and weights. Another future addition would be to factor wpa and epa into the model to better weight one PI, completion, incompletion, etc over another result. This would help build a more complex understanding of which defenders are making or allowing "big" plays.
