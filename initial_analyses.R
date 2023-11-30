### This script runs the initial analysis
### This outputs the data isn't csvs that are used in the app, as well as in clustering
library(dplyr); library(ggplot2); library(xgboost); library(caret)
seconds <- 3.5 # the maximum seconds of evalution
seconds_first_step <- 1.5 # not really used, but the first-step seconds

# reading in the data, which can be found 
# https://www.kaggle.com/c/nfl-big-data-bowl-2023
setwd("C:/Users/eric/Dropbox/PC (2)/Documents")
players <- read.csv("players.csv") %>% select(nflId, officialPosition, displayName)
scouting <- read.csv("pffScoutingData.csv") %>%
  filter(pff_role == "Pass Block", pff_positionLinedUp %in% c("LT", "LG", "C", "RG", "RT")) %>%
  select(gameId, playId, nflId, pff_hit, pff_hurry, pff_sack, pff_beatenByDefender, pff_positionLinedUp)
plays <- read.csv("plays.csv"); games <- read.csv("games.csv")

pbp <- rbind(read.csv("week8.csv"), read.csv("week7.csv"),
               read.csv("week6.csv"), read.csv("week5.csv"),
               read.csv("week4.csv"),  read.csv("week3.csv"), 
               read.csv("week2.csv"),  read.csv("week1.csv")) %>%
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y))
ball_snap_Id <- pbp %>% filter(event %in% c("ball_snap")) %>% 
  select(gameId, playId, nflId, ball_snap_frameId = frameId) %>% unique()
ball_snap_coord <- pbp %>% filter(event %in% c("ball_snap")) %>% 
  select(gameId, playId, nflId, ball_snap_x = x, ball_snap_y = y)
pbp_next_coord <- pbp %>% mutate(x_prev = x, y_prev = y, s_prev = s, frameId = frameId + 1) %>% 
  select(gameId, playId, nflId, frameId, x_prev, y_prev, s_prev)

# joining the data
pbp <- pbp %>%  
  inner_join(ball_snap_Id, by = c("gameId", "playId", "nflId")) %>% 
  inner_join(ball_snap_coord, by = c("gameId", "playId", "nflId")) %>% 
  inner_join(players, by = "nflId") %>% 
  inner_join(scouting, by = c("gameId", "playId", "nflId")) %>% 
  inner_join(plays, by = c("gameId", "playId")) %>% 
  inner_join(games, by = "gameId") %>% 
  left_join(pbp_next_coord, by = c("gameId", "playId", "nflId", "frameId")) %>%
  mutate(is_home = ifelse(homeTeamAbbr == team, 1, 0)) %>%
  mutate(s_delta = s - s_prev) %>%
  select(-s_prev)

# creating first-step analysis
pbp$s_delta[is.na(pbp$s_delta)] <- 0
pbp$pff_beatenByDefender[is.na(pbp$pff_beatenByDefender)] <- 0

first_min_s <- pbp %>% filter(frameId > ball_snap_frameId, s_delta < 0) %>% group_by(gameId, playId, nflId) %>%
  arrange(frameId) %>% dplyr::slice(1:1) %>%
  select(gameId, playId, nflId, decel_frameId = frameId)

pbp <- pbp %>% left_join(first_min_s, by = c("gameId", "playId", "nflId"))

first_step_Id <- pbp %>% filter(frameId == ball_snap_frameId + 10*seconds_first_step) %>%
  select(gameId, playId, nflId, first_step_frameId = frameId) %>% unique()
first_step_coord <- pbp %>% filter(frameId == ball_snap_frameId + 10*seconds_first_step) %>%
  select(gameId, playId, nflId, first_step_x = x, first_step_y = y)

pbp <- pbp %>%
  left_join(first_step_Id, by = c("gameId", "playId", "nflId")) %>%
  left_join(first_step_coord, by = c("gameId", "playId", "nflId")) %>%
  mutate(set = case_when(first_step_x >= ball_snap_x ~ "Other", 
                         pff_positionLinedUp %in% c("LT", "LG") & first_step_x < ball_snap_x & first_step_y < ball_snap_y ~ "Vertical Set", 
                         pff_positionLinedUp %in% c("LT", "LG") & first_step_x < ball_snap_x & first_step_y >= ball_snap_y ~ "Down Block", 
                         pff_positionLinedUp %in% c("RT", "RG") & first_step_x < ball_snap_x & first_step_y > ball_snap_y ~ "Vertical Set", 
                         pff_positionLinedUp %in% c("RT", "RG") & first_step_x < ball_snap_x & first_step_y <= ball_snap_y ~ "Down Block", 
                         pff_positionLinedUp %in% c("C") & first_step_x < ball_snap_x & first_step_y < ball_snap_y ~ "Left Guard", 
                         pff_positionLinedUp %in% c("C") & first_step_x < ball_snap_x & first_step_y >= ball_snap_y ~ "Right Guard",
                         TRUE ~ "Other"))

# looking at when the pass is thrown
pass_thrown_Id <- pbp %>% filter(event %in% c("pass_forward", "qb_sack", "qb_strip_sack", "run")) %>%
  unique() %>%
  mutate(time_to_throw = (frameId - ball_snap_frameId + 1)/10) %>%
select(gameId, playId, time_to_throw, pass_frame = frameId)

pass_thrown_Id <- pass_thrown_Id %>% group_by(gameId, playId) %>%
  summarize(time_to_throw = min(time_to_throw), pass_frame = min(pass_frame))

max_TTT <- max(filter(pass_thrown_Id, !is.na(time_to_throw))$time_to_throw)

pbp <- pbp %>% left_join(pass_thrown_Id, by = c("gameId", "playId")) %>%
  mutate(time_to_throw = ifelse(is.na(time_to_throw), max_TTT, time_to_throw), 
         pass_frame = ifelse(is.na(pass_frame), 1000, pass_frame)) %>%
  mutate(timer_frame = 10*seconds + ball_snap_frameId) %>%
  mutate(stop_frame = pmin(timer_frame, pass_frame)) %>%
  mutate(pass_away = ifelse(pass_frame <= decel_frameId, "yes", "no"))
# could be timer_frame

# looking at the max depth across the offensive line
max_depth <- pbp %>% filter(frameId <= stop_frame) %>%
  group_by(gameId, playId) %>% 
  summarize(max_depth = min(x - ball_snap_x))

# looking at time of throw or 3.5 seconds, for expected depth
normalize <- pbp %>% filter(frameId == decel_frameId) %>%
  mutate(vert_depth = abs(x - ball_snap_x), 
         depth = sqrt((x - ball_snap_x)^2 + (y - ball_snap_y)^2), 
         score_diff = ifelse(is_home == 1, preSnapHomeScore - preSnapVisitorScore, -preSnapHomeScore + preSnapVisitorScore), 
         field_pos = ifelse(possessionTeam == yardlineSide, 50 + yardlineNumber, yardlineNumber)) %>%
  left_join(max_depth, by = c("gameId", "playId"))
normalize[is.na(normalize)] <- 0

# hyperparamters
ControlParamteres <- trainControl(method = "cv", number = 5,
                                  savePredictions = TRUE, classProbs = FALSE,
                                  verboseIter = TRUE,
                                  allowParallel = TRUE)

parametersGrid <-  expand.grid(eta = c(0.1), colsample_bytree = c(0.25, 0.5, 0.75),
                               max_depth = c(2, 4, 6, 8), nrounds = c(200),
                               gamma = 1, min_child_weight = 1, subsample = 1)

# fitting actual model
fit <- train(vert_depth ~ down + yardsToGo + field_pos + is_home + 
               pass_away + time_to_throw + set + max_depth + 
               ball_snap_x + ball_snap_y + pff_positionLinedUp + score_diff + 
               defendersInBox + pff_playAction + pff_passCoverageType + 
               dropBackType + offenseFormation, 
               data = normalize, method = "xgbTree", trControl = ControlParamteres,
             tuneGrid = parametersGrid)
sqrt(mean((normalize$vert_depth - predict(fit, normalize))^2))
varImp(fit)

# fitting the null model for comparison
fit_null <- train(vert_depth ~ down + yardsToGo + field_pos + is_home + score_diff, 
                  data = normalize, method = "xgbTree", trControl = ControlParamteres,
                  tuneGrid = parametersGrid)

sqrt(mean((normalize$vert_depth - predict(fit_null, normalize))^2))
normalize[, "exp_depth"] <- predict(fit, normalize) 

# writting expected depth data into a local folder
write.csv(normalize, "C:/Users/eric/Dropbox/ol_maps/expected_depth.csv", row.names = FALSE)

# doing some evaluation
how_good <- data.frame()
for (j in 1:8) {
  temp <- group_by(normalize %>% filter(week < j), nflId) %>% summarize(n = n(), beaten = mean(pff_beatenByDefender))
  how_good <- rbind(how_good, temp)
}

average_beaten <- mean(normalize$pff_beatenByDefender)
how_good <- how_good %>% mutate(beaten = pmax(0, (1 - n/36))*average_beaten + 
                                  pmin(1, n/36)*beaten)
normalize <- normalize %>% left_join(how_good)

# seeing if getting beaten on a play is dependent on depth after
# accounting for other things
summary(glm(pff_beatenByDefender ~ as.factor(over_under) + beaten +
              (as.factor(down) + yardsToGo)^2 + field_pos + 
              as.factor(is_home) + pass_away + time_to_throw + set + max_depth + 
              ball_snap_x + ball_snap_y + pff_positionLinedUp + score_diff + 
              defendersInBox + pff_playAction + pff_passCoverageType + 
              dropBackType + offenseFormation, data = normalize %>%
              mutate(over_under = ifelse(vert_depth > exp_depth, 1, 0))))
normalize <- normalize %>% select(gameId, playId, nflId, vert_depth, exp_depth)

# looking at some team-level metrics from nflfastR
epa <- nflfastR::load_pbp(2021) %>%
  select(gameId = old_game_id, playId = play_id, epa, success) %>%
  mutate(gameId = as.integer(gameId), playId = as.integer(playId))

# more adjustments
pbp <- pbp %>% left_join(normalize, by = c("gameId", "playId", "nflId")) %>%
  mutate(over_under = ifelse(vert_depth > exp_depth, 
                             "yes", "no")) %>% 
  mutate(score_diff = ifelse(is_home == 1, preSnapHomeScore - preSnapVisitorScore, -preSnapHomeScore + preSnapVisitorScore), 
                      field_pos = ifelse(possessionTeam == yardlineSide, 50 + yardlineNumber, yardlineNumber)) %>%
  mutate(beaten = ifelse(pff_beatenByDefender == 1, "yes", "no")) %>%
  filter(frameId >= ball_snap_frameId) %>%
  mutate(new_frameId = frameId - ball_snap_frameId + 1) %>%
  mutate(x_prev = ifelse(is.na(x_prev), ball_snap_x, x_prev), 
         y_prev = ifelse(is.na(y_prev), ball_snap_y, y_prev)) 

pbp <- pbp %>% left_join(epa)

# visualzing model results
ggplot(normalize, aes(exp_depth, vert_depth)) + geom_point() + geom_smooth(method = "lm") + geom_abline()

# writing slimmed down data into a local file
pbp_slim <- pbp %>% select(season, week, gameId, playId, x,y, ball_snap_x, ball_snap_y, 
                           beaten, over_under, pff_positionLinedUp, pff_playAction, 
                           nflId, displayName, team, time_to_throw, pass_away, new_frameId, set, 
                           decel_frameId, epa, success) %>%
  filter(new_frameId <= decel_frameId) # could be different
write.csv(pbp_slim, "C:/Users/eric/Dropbox/ol_maps/sumer_ol/wrangled_pbp.csv", row.names = FALSE)
write.csv(pbp_slim %>% select(displayName) %>% unique(), "C:/Users/eric/Dropbox/ol_maps/sumer_ol/ol_names.csv", row.names = FALSE)
