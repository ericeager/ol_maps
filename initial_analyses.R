library(dplyr); library(ggplot2); library(xgboost); library(caret)
seconds <- 3.5
seconds_first_step <- 1.5

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

#instead of frameId == stop_frame
normalize <- pbp %>% filter(frameId == decel_frameId) %>%
  mutate(vert_depth = abs(x - ball_snap_x), 
         depth = sqrt((x - ball_snap_x)^2 + (y - ball_snap_y)^2), 
         score_diff = ifelse(is_home == 1, preSnapHomeScore - preSnapVisitorScore, -preSnapHomeScore + preSnapVisitorScore), 
         field_pos = ifelse(possessionTeam == yardlineSide, 50 + yardlineNumber, yardlineNumber))


# think about nearest offensive player - splits
# think about time left on the clock

normalize[is.na(normalize)] <- 0

ControlParamteres <- trainControl(method = "cv", number = 5,
                                  savePredictions = TRUE, classProbs = FALSE,
                                  verboseIter = TRUE,
                                  allowParallel = TRUE)

parametersGrid <-  expand.grid(eta = c(0.1), colsample_bytree = c(0.25, 0.5, 0.75),
                               max_depth = c(2, 4, 6, 8), nrounds = c(200),
                               gamma = 1, min_child_weight = 1, subsample = 1)

fit <- train(vert_depth ~ down + yardsToGo + field_pos + is_home + pass_away + time_to_throw + set + 
               ball_snap_x + ball_snap_y + pff_positionLinedUp + score_diff + 
               defendersInBox + pff_playAction + pff_passCoverageType + 
               dropBackType + offenseFormation, 
               data = normalize, method = "xgbTree", trControl = ControlParamteres,
             tuneGrid = parametersGrid)

normalize[, "exp_depth"] <- predict(fit, normalize) 
normalize <- normalize %>% select(gameId, playId, nflId, vert_depth, exp_depth)

pbp[is.na(pbp)] <- 0

ControlParamteres <- trainControl(method = "cv", number = 5,
                                  savePredictions = TRUE, classProbs = TRUE,
                                  verboseIter = TRUE,
                                  allowParallel = TRUE)

parametersGrid <-  expand.grid(eta = c(0.1), colsample_bytree = c(0.25, 0.5, 0.75),
                               max_depth = c(2, 4, 6, 8), nrounds = c(200),
                               gamma = 1, min_child_weight = 1, subsample = 1)

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

fit_opti <- train(beaten ~ down + yardsToGo + field_pos + is_home + pass_away + time_to_throw + set + 
                    ball_snap_x + ball_snap_y + 
                    x_prev + y_prev + 
                    x + y + 
                    pff_positionLinedUp + score_diff + 
                    defendersInBox + pff_playAction + pff_passCoverageType + 
                    dropBackType + offenseFormation, 
                  data = pbp, method = "xgbTree", trControl = ControlParamteres,
                  tuneGrid = parametersGrid)

varImp(fit)
varImp(fit_opti)

ggplot(normalize, aes(exp_depth, vert_depth)) + geom_point() + geom_smooth(method = "lm") + geom_abline()

pbp_slim <- pbp %>% select(season, week, gameId, playId, x,y, ball_snap_x, ball_snap_y, 
                           beaten, over_under, pff_positionLinedUp, pff_playAction, 
                           nflId, displayName, team, time_to_throw, pass_away, new_frameId, set, 
                           decel_frameId) %>%
  filter(new_frameId <= decel_frameId) # could be different
write.csv(pbp_slim, "C:/Users/eric/Dropbox/ol_maps/sumer_ol/wrangled_pbp.csv", row.names = FALSE)
#write.csv(averages, "ol_averages.csv", row.names = FALSE)
write.csv(pbp_slim %>% select(displayName) %>% unique(), "C:/Users/eric/Dropbox/ol_maps/sumer_ol/ol_names.csv", row.names = FALSE)
