library(dplyr); library(ggplot2); library(xgboost); library(caret)
seconds <- 3.5

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

pbp <- pbp %>%  
  inner_join(ball_snap_Id, by = c("gameId", "playId", "nflId")) %>% 
  inner_join(ball_snap_coord, by = c("gameId", "playId", "nflId")) %>% 
  inner_join(players, by = "nflId") %>% 
  inner_join(scouting, by = c("gameId", "playId", "nflId")) %>% 
  inner_join(plays, by = c("gameId", "playId")) %>% 
  inner_join(games, by = "gameId") %>% 
  mutate(is_home = ifelse(homeTeamAbbr == team, 1, 0))

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
  mutate(pass_away = ifelse(pass_frame <= timer_frame, "yes", "no"))


normalize <- pbp %>% filter(frameId == stop_frame) %>%
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

fit <- train(vert_depth ~ down + yardsToGo + field_pos + is_home + pass_away + time_to_throw + 
               ball_snap_x + ball_snap_y + pff_positionLinedUp + score_diff + 
               defendersInBox + pff_playAction + pff_passCoverageType + 
               dropBackType + offenseFormation, 
               data = normalize, method = "xgbTree", trControl = ControlParamteres,
             tuneGrid = parametersGrid)

normalize[, "exp_depth"] <- predict(fit, normalize) 
normalize <- normalize %>% select(gameId, playId, nflId, vert_depth, exp_depth)

ggplot(normalize, aes(exp_depth, vert_depth)) + geom_point() + geom_smooth(method = "lm") + geom_abline()

pbp <- pbp %>% left_join(normalize, by = c("gameId", "playId", "nflId")) %>%
  mutate(over_under = ifelse(vert_depth > exp_depth, 
                             "yes", "no")) %>%
  mutate(beaten = ifelse(pff_beatenByDefender == 1, "yes", "no")) %>%
  filter(frameId >= ball_snap_frameId) %>%
  mutate(new_frameId = frameId - ball_snap_frameId + 1)

pbp_slim <- pbp %>% select(gameId, playId, x,y, ball_snap_x, ball_snap_y, 
                           beaten, over_under, pff_positionLinedUp, pff_playAction, 
                           nflId, displayName, time_to_throw, pass_away, new_frameId) %>%
  filter(new_frameId <= seconds*10)
write.csv(pbp_slim, "wrangled_pbp.csv", row.names = FALSE)
#write.csv(averages, "ol_averages.csv", row.names = FALSE)
write.csv(pbp_slim %>% select(displayName) %>% unique(), "ol_names.csv", row.names = FALSE)

example <- pbp_slim %>% filter(#gameId == 2021090900, 
  displayName == "Jawaan Taylor") 

NAME <- example$displayName[1]
ggplot(example, aes(y - ball_snap_y, x - ball_snap_x, group = as.factor(paste(gameId, playId)), color = as.factor(over_under), 
                    linetype = as.factor(beaten)), size = 2) + geom_smooth(se = FALSE) + 
  geom_point(aes(y_mean, x_mean), color = "red", size = 5) + 
  scale_colour_manual(values = c("gold", "black")) + 
  theme_minimal() + labs(x = "vertical distance from position at snap", y = "vertical distance from position at snap", 
                         color = "Did He Reach Depth?", 
                         linetype = "Was He Beaten (PFF)?",
                         title = paste("Pass Blocking Map for", NAME)) #+ theme(legend.position="none")

group_by(pbp %>% filter(pff_positionLinedUp == "LT"), nflId, displayName) %>% 
  filter(!is.na(over_under)) %>% 
  summarize(n = n(), over_under = mean(over_under == "yes"), beaten = mean(beaten == "yes")) %>% 
  filter(n >= 2500) %>% arrange(-over_under) %>% 
  ggplot(aes(over_under, beaten, label = displayName)) + geom_text() + geom_smooth()

group_by(pbp %>% filter(new_frameId <= 30, pff_positionLinedUp == "RT"), team, nflId, displayName, gameId, playId) %>% 
  summarize(diff = sum(sqrt((y - ball_snap_y - y_mean)^2 + (x - ball_snap_x - x_mean)^2)), beaten = mean(pff_beatenByDefender)) %>% 
  group_by(team, nflId, displayName) %>% 
  summarize(n = n(), diff = mean(diff), beaten = mean(beaten)) %>% arrange(-diff) %>% filter(n >= 100) %>% as.data.frame() %>%
  group_by(team) %>% summarize(diff = mean(diff)) %>% arrange(-diff)

